
MODULE module_sbm_mosaic_ikj

USE module_peg_util

CONTAINS

!-----------------------------------------------------------------
!----------------------------------------------------------------------
        subroutine  p_activation      (         &
             ww, qc_m, qc_n, rho, xl,tt,            &
             supsat, chem_n, chem_m, chem_sbm)

        use module_configure, only: grid_config_rec_type
	use module_data_mosaic_asect
        use module_data_mosaic_other
	use module_mixactivate, only: mixactivate

	implicit none

! variables description
! chem_n,        aerosol number from MOSAIC (#/kg)
! chem_m,        aerosol mass from MOSAIC (ug/kg)
! chem_sbm,      cloud droplets from SBM microphysics
! ww,            vertical air velocity (m/s)
! qc_m,          cloud water mixing ration (kg/kg)
! qc_n,          cloud droplet number concentration (#/kg)
! rho,           air density (g/cm^3)
! xl,            mass of each droplet size (g)
! tt,            temperature (K)
! supsat,        supersaturation 
! precr,         rain precipitation rate at all levels (kg/m^2/s)

! rdroplo_sbm,   radius at lower boundary of the droplets bins in SBM (cm)
! rdrophi_sbm,   radius at upper boundary of the droplets bins in SBM (cm)
! rgn_aer,       geometric-mean radius of the lognormal number distribution (cm)
! rgn_mass_aer,  mass-mean radius of the lognormal number distribution (cm)
! scgn_aer,      the corresponding critical supersaturation at size of radius=rgn_aer
! scgn_mass_aer, the corresponding critical supersaturation at size of radius=rgn_mass_aer
! racn_sbm,      central radius of the assumed 33 bin CCN with composition (NH4)2SO4 (cm)
! ralo_sbm,      radius at lower boundary of the assumed 33 bin CCN (cm)
! rahi_sbm,      radius at upper boundary of the assumed 33 bin CCN (cm)
! schi_sbm,      supersaturation of upper boundary with the assumed 33 bin CCN
! sclo_sbm,      supersaturation of lower boundary with the assumed 33 bin CCN
! rlo,           the lower radius of 33 bins CCN according to aerosol composition from MOSAIC
! rhi,           the upper radius of 33 bins CCN according to aerosol composition from MOSAIC
! rcn,           the central radius of 33 bins CCN according to aerosol composition from MOSAIC
! scmean_mass,   log(scgn_mass_aer)
! sigmag_aer     standard deviation of the lognormal distribution for aerosol number (=1.492)

! parameters of the lognormal distribution:
! sc = log(supersaturation(r))
! scmean = log(scgn_aer)
! dn/dsc = [number_aer/(sqrt(2*pai)*scwidth)]*exp(-0.5*((sc-scmean)/scwidth)**2)

! f_map2na(m,l),      fraction of the number in MOSAIC bin m mapped to assumed CCN bin l
! f_map2na_mass(m,l), fraction of the mass in MOSAIC bin m mapped to assumed CCN bin l
! ract_sbm(m,l)       radius of activated AP from MOSAIC (m bin) -> SBM CCN (l bin)  (cm)
! nact_sbm(m,l)       the corrsponding number concentration (#/kg)
! f_act_mass_l(m,n,l) the fraction of mass that be activated from MOSAIC m-bin to CCN l-bin (%)
! f_act_mass(m,n)     the fraction of mass that be activated in MOSAIC bin m (%)
! ccn_sbm(m),         activated AP number in MOSAIC bin m  (#/kg)
! nact_0,             the total number of activated AP goes into the droplet bin in SBM
!                     in terms of the radius of activated AP (#/kg)
! nact_track,         the tracking of number from MOSAIC interstitial AP to SBM
! mass_act_track,     the trackong of the corrsponding mass

        real, parameter    :: pai=3.14159265, col=0.23105 
        integer, parameter :: nccnbin=33, nkr=33, krdrop=18
        character*200 msg
        integer i,j,k,l,lc,m,n,nn,nb
        integer itype, nhi, lnumb, lmass, cblnumb, cblmass
        real hygro(maxd_asize, maxd_atype) 
        real, dimension(maxd_asize, maxd_atype) :: &
             rgn_aer,rgn_mass_aer,scgn_mass_aer,scmean_mass
        real sumhygro, sumvol
        real tmp, tmp_r, rn, k_rn, coef1, X0CCN, dum
        real KLA, KLB, KLA1, KLB1, A1
        real racn_sbm(nccnbin), ralo_sbm(nccnbin), rahi_sbm(nccnbin)
        real sclo_sbm(nccnbin), schi_sbm(nccnbin)
        real scgn_aer(maxd_asize, maxd_atype)
        real scmean(maxd_asize, maxd_atype)
        real, dimension(maxd_asize, maxd_atype) ::  scwidth
        real normsc1, normsc2, erfc

        real, dimension(maxd_asize, maxd_atype, nccnbin) ::   &  
             rlo, rhi, rcn, f_map2na, f_map2na_mass, ract_sbm, nact_sbm, &
             f_act_mass_l       
        real, dimension(maxd_asize, maxd_atype) :: f_act_mass

        real ccn_sbm(maxd_asize, maxd_atype)
        real tn1,tn2,tn3,tn4
        real,optional,intent(in) :: xl(nccnbin) 
        real, dimension(krdrop) :: rdropcn_sbm, rdroplo_sbm, rdrophi_sbm, nact_0
        real qc_m,qc_n,qcl, qncl, ww, rho, qrl
        integer krr,kr
        double precision supsat,tt

        real chem_n(2,maxd_asize,maxd_atype), chem_m(2,maxd_asize,maxd_atype,8),chem_sbm(nkr)
        real nact_track(maxd_asize,nccnbin,krdrop), mass_act_track(maxd_asize,8,nccnbin,krdrop)
        real frac_cc, frac_frz, frac_resu
        real ntot_droplets, ntot_cld_bor, mult_k
        real ntot_inters, ntot_1,ntot_2
        logical total_resu
!-----------------------------------------------------------
! the assumed 33 bins CCN
!
        X0CCN =0.3351e-10/2.**32                        ! g, the mass of cloud droplet with radius 2-um
        KLA1=3.3e-05/288.15                             ! Koehler equation A parameter, temperature 15C
        KLB1=0.5                                        ! Koehler equation B parameter, composition (NH4)2SO4
        A1= (4.*KLA1**3/27./KLB1)**0.5
        DO n=1,nccnbin
           racn_sbm(n)=(3.*X0CCN*2.**(n-1)/4./pai/1.0)**(1./3)     ! central radius, 1.77 is the density of (NH4)2SO4
           ralo_sbm(n)= racn_sbm(n)/2.**(1./6)                     ! cm, radius at lower boundary
           rahi_sbm(n)= racn_sbm(n)*2.**(1./6)                     ! cm, radius at upper boundary
           if(n.eq.nccnbin) rahi_sbm(n)= 1.e-3                     ! cm, the upper boundary of bin 33

           schi_sbm(n)= A1/ralo_sbm(n)**1.5                    ! supersaturation of upper boundary 
           sclo_sbm(n)= A1/rahi_sbm(n)**1.5                    ! supersaturation of lower boundary 
        ENDDO

!------------------------------------------------------------
! the radius of 17 droplet bins in SBM
!
        DO n=1, krdrop-1
           rdropcn_sbm(n)= (3.*0.3351e-10*2.**(n-1)/4./pai/1.0)**(1./3)     !cm
           rdroplo_sbm(n)= rdropcn_sbm(n)/2.**(1./6)                     
           rdrophi_sbm(n)= rdropcn_sbm(n)*2.**(1./6)                     
        ENDDO

!----------------------------------------------------------
! the bin size of interstitial AP in MOSAIC
!
        do itype = 1, ntype_aer

            nhi = nsize_aer(itype)
            dlo_sect(  1,itype) = 3.90625e-6            ! cm, minimal dry diameter
            dhi_sect(nhi,itype) = 10.0e-4               ! cm, maximal dry diameter

            dum = log( dhi_sect(nhi,itype)/dlo_sect(1,itype))/nhi
            do n = 2, nhi
                dlo_sect( n, itype) = dlo_sect(1,itype) * exp( (n-1)*dum )
                dhi_sect(n-1,itype) = dlo_sect(n,itype)
            end do

            do n = 1, nhi
!                dcen_sect(n,itype) = sqrt( dlo_sect(n,itype)*dhi_sect(n,itype) )
                sigmag_aer(n,itype) = (dhi_sect(n,itype)/dlo_sect(n,itype))**0.289     ! standard deviation 
                scwidth(n,itype)  = log(sigmag_aer(n,itype)**1.5)
            end do
        end do

!---------------------------------------------------------
!---------------------------------------------------------
! the properties of MOSAIC interstitial AP
!
           do n=1,ntype_aer
           do m=1,nsize_aer(n)

! put in these values initially so the scgn and scmean will always have something in them
! Note that with these large scgn values, the mosaic bin will not activate

               scgn_aer(m,n) = 1.0e10
               scmean(m,n) = log(scgn_aer(m,n))
               scgn_mass_aer(m,n) = 1.0e10
               scmean_mass(m,n) = log(scgn_mass_aer(m,n))
               hygro(m,n) = 0.0
! volume-averaged bulk hygroscopicity 

               sumhygro=0.
               sumvol=0.

               do l=1,ncomp_aer(n)
                  sumhygro = sumhygro+hygro_aer(l,n)*                  &
                             max(chem_m(1,m,n,l)*1.0e-6/dens_aer(l,n),0.0)
                  sumvol = sumvol+max(chem_m(1,m,n,l)*1.0e-6/dens_aer(l,n),0.0)    ! cm^3
               end do

!             if(sumvol.gt.0.) hygro(m,n) = sumhygro/sumvol
             if(sumvol.le.1.0e-35) cycle  ! mosaic bin is effectively empty
             hygro(m,n) = sumhygro/sumvol
             if(hygro(m,n).le.1.0e-35) cycle  ! mosaic bin has hygro ~= 0 and will not activate

! cm, geometric-mean radius

!             if(chem(i,k,j,lnumb).gt.0.) rgn_aer(m,n) = (3*sumvol/4./pai/chem(i,k,j,lnumb))**(1/3.)      &
!                                   /exp(1.5*(log(sigmag_aer(m,n)))**2)        
             if(chem_n(1,m,n).le.1.0e-35) cycle  ! mosaic bin is effectively empty
             tmp_r = (3*sumvol/4./pai/chem_n(1,m,n))**(1/3.)
! force volume mean diameter (=2.0*tmp_r) to be between dlo_sect*0.5 and dhi_sect*2.0
             tmp_r = max( tmp_r, dlo_sect(m,n)*0.25 )
             tmp_r = min( tmp_r, dhi_sect(m,n) )
             rgn_aer(m,n) = tmp_r/exp(1.5*(log(sigmag_aer(m,n)))**2)

! cm, mass-mean radius

             rgn_mass_aer(m,n) = rgn_aer(m,n)*exp(3.*(log(sigmag_aer(m,n)))**2) 
!
! the lognormal distribution of interstitial AP in each MOSAIC bin
! in terms of the partical critical supersaturation. 
! scgn_aer is the supersaturation when AP dry radius is rgn_aer 
! Koehler parameter is from MOSAIC
!
             KLA = 3.3*1.0e-5/tt
             KLB = hygro(m,n)
             if(rgn_aer(m,n).gt.0.) scgn_aer(m,n) = (4.*KLA**3/27./KLB)**0.5/(rgn_aer(m,n))**1.5
             if(scgn_aer(m,n).gt.0.) scmean(m,n) = log(scgn_aer(m,n))

             if(rgn_mass_aer(m,n).gt.0.) scgn_mass_aer(m,n) = (4.*KLA**3/27./KLB)**0.5/(rgn_mass_aer(m,n))**1.5
             if(scgn_mass_aer(m,n).gt.0.) scmean_mass(m,n) = log(scgn_mass_aer(m,n))

           end do 
           end do 

!--------------------------------------------------------------
! mapping aerosol in MOSAIC to the assumed 33 bins CCN in terms of supersaturation
!       
        f_map2na=0.
        f_map2na_mass=0.

            KLA = 3.3*1.0e-5/tt

            do n=1,ntype_aer
            do m=1,nsize_aer(n)

             do l= 1,nccnbin

               rcn(m,n,l)=racn_sbm(l)  ! put in this default value
               if(scgn_aer(m,n).ge.0.99e10) cycle  ! this happens when mosaic bin is essentially empty
               if(hygro(m,n).le.1.0e-35) cycle  ! mosaic bin has hygro ~= 0 and will not activate

! f_map2na, fraction of the number in MOSAIC bin m mapped to assumed CCN bin l

               normsc1 = (log(sclo_sbm(l)) - scmean(m,n))/scwidth(m,n)
               normsc2 = (log(schi_sbm(l)) - scmean(m,n))/scwidth(m,n)
               f_map2na(m,n,l) = 0.5*(erfc(normsc1/sqrt(2.)) - erfc(normsc2/sqrt(2.)))

! f_map2na_mass, fraction of the mass in MOSAIC bin m mapped to assumed CCN bin l
! same value for different aerosol composition

               normsc1 = (log(sclo_sbm(l)) - scmean_mass(m,n))/scwidth(m,n)
               normsc2 = (log(schi_sbm(l)) - scmean_mass(m,n))/scwidth(m,n)
               f_map2na_mass(m,n,l) = 0.5*(erfc(normsc1/sqrt(2.)) - erfc(normsc2/sqrt(2.)))

! the corresponding radius of assumed 33 bins CCN according to the aerosol composition from MOSAIC

               if(hygro(m,n).gt.0..and.sclo_sbm(l).gt.0.) rhi(m,n,l)=KLA/3.*(4/hygro(m,n))**(1/3.)/sclo_sbm(l)**(2/3.)
               if(hygro(m,n).gt.0..and.schi_sbm(l).gt.0.) rlo(m,n,l)=KLA/3.*(4/hygro(m,n))**(1/3.)/schi_sbm(l)**(2/3.)
               rcn(m,n,l)=sqrt(rlo(m,n,l)*rhi(m,n,l))
             enddo
            end do
            end do

!-----------------------------------------------------------
! the aerosol activation process
!
         qrl=0.
         qcl=0.
         qncl=0.
         DO krr= 1, krdrop-1
            qcl=qcl+chem_sbm(krr)*col*xl(krr)*xl(krr)*3/rho
            qncl=qncl+chem_sbm(krr)*col*xl(krr)*3/rho*1000. 
         END DO
         DO krr= krdrop, 33
            qrl=qrl+chem_sbm(krr)*col*xl(krr)*xl(krr)*3/rho
         END DO

         ntot_cld_bor=0.
         ntot_inters=0.
         do m=1,nsize_aer(1)
             ntot_cld_bor = ntot_cld_bor+chem_n(2,m,1)
             ntot_inters = ntot_inters+chem_n(1,m,1)
         enddo

         if(qncl.gt.(ntot_cld_bor+ntot_inters).and.(ntot_cld_bor+ntot_inters).gt.1e8) then
          do krr= 1,33
            if(krr.lt.krdrop) then
             chem_sbm(krr)=chem_sbm(krr)*(ntot_cld_bor+ntot_inters)/qncl 
            elseif(krr.ge.krdrop.and.qrl.gt.1e-5) then
              chem_sbm(krr)=chem_sbm(krr)*(1.+qcl*(1.-(ntot_cld_bor+ntot_inters)/qncl)/qrl)
            endif
          enddo
         endif 


        ract_sbm = 0.
        nact_sbm = 0.

! large-scale conditions for aerosol activation

         if(supsat/=supsat) supsat=0.0d0

         if(supsat.ge.sclo_sbm(nccnbin).and.ww.gt.0.001.and.tt.gt.243.15) then

! rn (um->cm): when dry AP radius>rn, the size of activation droplet will mulity by a factor of k_rn,
! w: vertical velocity in m/s (KOGAN, JAS, 1991) 

            rn = 0.09/ww**0.16/1.e4

! the index (nn) of CCN bin that the supersaturation lies in

            nn=0
            do l= nccnbin,1,-1
              if(supsat.ge.sclo_sbm(l)) then
                 nn=nn+1 
              else
                goto 110
              endif
            enddo
         
110        continue

! ract_sbm(m,n,l), the radius of activated AP from MOSAIC (m bin) -> assumed CCN (l bin)
! nact_sbm(m,n,l), the corrsponding number concentration 

            f_act_mass_l = 0.0

            do n=1,ntype_aer
            do m=1,nsize_aer(n)

! complete activation in the bin > nn

              do l= nccnbin,nccnbin+2-nn,-1

                if(rcn(m,n,l).gt.rn) then
                  k_rn = 5.8/ww**0.12/(rcn(m,n,l)*1.e4)**0.214
                  if(k_rn.ge.10.) k_rn = 10.
                  ract_sbm(m,n,l) = max(k_rn*rcn(m,n,l),2.0e-4)         
                  nact_sbm(m,n,l) = f_map2na(m,n,l)*chem_n(1,m,n)   

                  f_act_mass_l(m,n,l) = 1.0*f_map2na_mass(m,n,l)

                else
                  ract_sbm(m,n,l) = 2.0e-4                       ! cm, minimal radius of droplet in SBM
                  nact_sbm(m,n,l) = f_map2na(m,n,l)*chem_n(1,m,n)

                  f_act_mass_l(m,n,l) = 1.0*f_map2na_mass(m,n,l)

                endif

              enddo

! partial activation in the bin nn

                coef1=(dlog(supsat)-log(sclo_sbm(nccnbin+1-nn)))            &
                      /(log(schi_sbm(nccnbin+1-nn))-log(sclo_sbm(nccnbin+1-nn)))

                if(rcn(m,n,nccnbin+1-nn).gt.rn) then
                  k_rn = 5.8/ww**0.12/(rcn(m,n,nccnbin+1-nn)*1.e4)**0.214
                  if(k_rn.ge.10.) k_rn = 10.
                  ract_sbm(m,n,nccnbin+1-nn) = max(k_rn*rcn(m,n,nccnbin+1-nn),2.0e-4)
                  nact_sbm(m,n,nccnbin+1-nn) = coef1*f_map2na(m,n,nccnbin+1-nn)*chem_n(1,m,n)

                  f_act_mass_l(m,n,nccnbin+1-nn) = coef1*f_map2na_mass(m,n,nccnbin+1-nn)

                 else
                  ract_sbm(m,n,nccnbin+1-nn) = 2.0e-4     !cm
                  nact_sbm(m,n,nccnbin+1-nn) = coef1*f_map2na(m,n,nccnbin+1-nn)*chem_n(1,m,n)

                  f_act_mass_l(m,n,nccnbin+1-nn) = coef1*f_map2na_mass(m,n,nccnbin+1-nn)

                endif

! the activated AP number from MOSAIC bin m

                ccn_sbm(m,n)=0.

                do l= nccnbin,nccnbin+1-nn,-1
                    ccn_sbm(m,n)= ccn_sbm(m,n)+nact_sbm(m,n,l)
                end do

! the activated AP mass for each composition from MOSAIC bin m

                  f_act_mass(m,n) = 0.

                  do l= nccnbin,nccnbin+1-nn,-1
                    f_act_mass(m,n) = f_act_mass(m,n) + f_act_mass_l(m,n,l)
                  end do

                  f_act_mass(m,n) = max(0., min(1., f_act_mass(m,n)))

            end do
            end do

!-----------------------------------------------------------
! check the cloud-borne AP in MOSAIC and cloud droplets in SBM
! adjust the number and mass of cloud-borne AP in MOSAIC

!----------------------------------------------------------
! According to the radius of activated aerosol,
! the number of activated aerosol in MOSAIC is added to ithe droplets in SBM,
! update the number concentration of droplet bins in SBM
!
         do nb= 1,krdrop-1
 
           nact_0(nb) = 0.

            do n=1,ntype_aer
            do m=1,nsize_aer(n)

              do l=1,nccnbin
                 if(ract_sbm(m,n,l).ge.rdroplo_sbm(nb).and.ract_sbm(m,n,l).lt.rdrophi_sbm(nb)) then
                   nact_0(nb) = nact_0(nb)+nact_sbm(m,n,l)
                   nact_track(m,l,nb) = nact_sbm(m,n,l)

                   do lc=1,ncomp_aer(n)
                     mass_act_track(m,lc,l,nb) = f_act_mass_l(m,n,l)*chem_m(1,m,n,lc)
                   end do

                  endif                        
              end do
            end do
            end do

              chem_sbm(nb)=chem_sbm(nb)+nact_0(nb)*rho/col/xl(nb)/3./1000.
         enddo

         ntot_1=0.
         do nb= 1,krdrop-1
           ntot_1=ntot_1+nact_0(nb)
         enddo
     
         ntot_2=0.
            do n=1,ntype_aer
            do m=1,nsize_aer(n)
              do l=1,nccnbin
                   ntot_2 = ntot_2+nact_sbm(m,n,l)
              enddo
            enddo
            enddo
! change the MOSAIC interstitial and cloud-borne AP
! ccn_sbm(m,n)     the number of activated AP in MOSAIC bin m
! f_act_mass(m,n)  the fraction of activated AP mass for each composition
tn1=0.
tn2=0.
tn3=0.
tn4=0.

do i=1,17
tn1=tn1++nact_0(i)
enddo

do m=1,4
tn2= tn2+ccn_sbm(m,1)
tn4= tn4+chem_n(1,m,1)
enddo

           do n=1,ntype_aer
           do m=1,nsize_aer(n)

              if(ccn_sbm(m,n).gt.1.) then
                lnumb = numptr_aer(m,n,ai_phase)
                cblnumb = numptr_aer(m,n,cw_phase)

                call wrf_message('Activation update:')
                write(msg,1188) lnumb, cblnumb 
                call peg_message( lunerr, msg)
                write(msg,1177) chem_n(1,m,n), chem_n(2,m,n) 
                call peg_message( lunerr, msg)

! tmp is the change in number - apply limiters to it before using it
                tmp = max( 0., min(chem_n(1,m,n), ccn_sbm(m,n)))
                chem_n(1,m,n) = chem_n(1,m,n)-tmp
                chem_n(2,m,n) = chem_n(2,m,n)+tmp

tn3=tn3+tmp
                do lc=1,ncomp_aer(n)

! tmp is the change in mass - apply limiters to it before using it
                  tmp = max(0., chem_m(1,m,n,lc)*f_act_mass(m,n))
                  chem_m(1,m,n,lc) = chem_m(1,m,n,lc)-tmp
                  chem_m(2,m,n,lc) = chem_m(2,m,n,lc)+tmp

                end do

                write(msg,1177) chem_n(1,m,n), chem_n(2,m,n), ccn_sbm(m,n)
                call peg_message( lunerr, msg)

              endif
           enddo
           enddo
if(tn2.gt.1e2) then
                call wrf_message('number comp:')
                write(msg,1177) tn1,tn2,tn3,tn4
                call peg_message( lunerr, msg)
                write(msg,1177) qc_n,qncl,ntot_cld_bor,ntot_1,ntot_2,ntot_inters
                call peg_message( lunerr, msg)
         endif
endif
if(ntot_1.gt.ntot_inters) then
                call wrf_message('num_error:')
                write(msg,1177) ntot_1,ntot_2,ntot_inters
                call peg_message( lunerr, msg)
endif

1166    format( 12e13.5)
1177    format(  6e20.12)
1188    format( 12i13)

      end subroutine p_activation

!----------------------------------------------------------------------
!----------------------------------------------------------------------
        subroutine resusp_wet_remov   (         &
             qc_m, qc_n,             &
             dropdel,frac_proc, chem_n, chem_m  )

!        use module_configure, only: grid_config_rec_type
        use module_data_mosaic_asect
        use module_data_mosaic_other
!        use module_mixactivate, only: mixactivate

        implicit none

! dropdel(1:3),   the change of droplet number after and befor the processes (#/kg)
!                 1: evaporation, 2: coagulation, 3: freezing. They are from SBM.
! frac_resu,      the fraction of total droplets number in SBM that is lost due to
!                 the evaporation in one step time.
! frac_cc,        the fraction of total droplets number in SBM droplet bins
!                 that is lost due to the collected by rain,snow,graupel,and hail
! frac_frz,       the fraction of total droplets number in SBM droplet bins
!                 that is lost due to the droplet freezing.

        character*200 msg
        integer lc,m,n
        real, dimension(4) :: dropdel,frac_proc
        real chem_n(2,maxd_asize,maxd_atype), chem_m(2,maxd_asize,maxd_atype,8)
        integer lnumb, lmass, cblnumb, cblmass
        real qc_m,qc_n
        real frac_cc, frac_frz, frac_resu
        real ntot_droplets, ntot_cld_bor, mult_k
        logical total_resu

! Resuspension, incease in the MOSAIC interstitial AP number and mass
!               decrease in the MOSAIC cloud-born AP number and mass

! tendencies of droplet evaporayion
        frac_resu=frac_proc(1)
        frac_resu= max( -1.0, min( 0.0, frac_resu))

! if no cloud is present, then resuspend all cloud-borne aerosol
        if ( qc_n.lt.1.0 .and. qc_m.lt.1.0e-6 ) then
           total_resu = .true.
           frac_resu = -1.0
         else
           total_resu = .false.
        end if

!          if((frac_resu.lt.0..and.dropdel(1).lt.-5.e1) .or. total_resu) then
          if(frac_resu.lt.0..and.dropdel(1).lt.-1.e-5) then

!             write(msg,1166) frac_resu, dropdel(1)
!             call peg_message( lunerr, msg)

             do n=1,ntype_aer
             do m=1,nsize_aer(n)
                lnumb = numptr_aer(m,n,ai_phase)
                cblnumb = numptr_aer(m,n,cw_phase)

!if(m.eq.2) then
!                call wrf_message('Resuspension update:')
!                write(msg,1188) lnumb, cblnumb
!                call peg_message( lunerr, msg)
!                write(msg,1177) chem_n(1,m,n), chem_n(2,m,n)
!                call peg_message( lunerr, msg)
!endif
                chem_n(1,m,n)= chem_n(1,m,n)-frac_resu*chem_n(2,m,n)
                chem_n(2,m,n)=(1.+frac_resu)*chem_n(2,m,n)

                do lc=1,ncomp_aer(n)
                  chem_m(1,m,n,lc)=chem_m(1,m,n,lc)-frac_resu*chem_m(2,m,n,lc)
                  chem_m(2,m,n,lc)=(1.+frac_resu)*chem_m(2,m,n,lc)
                end do

!if(m.eq.2) then
!                write(msg,1177) chem_n(1,m,n), chem_n(2,m,n)
!                call peg_message( lunerr, msg)
!endif

             enddo
             enddo

          endif

!---------------------------------------------------
! In-cloud wet removal, only decrease MOSAIC cloud-borne AP number and mass

! tendencies of droplet collected by rain,snow,graupel,and hail
        frac_cc=frac_proc(2)
        frac_cc= max( -1.0, min( 0.0, frac_cc))

! tendencies of droplet by freezing
        frac_frz=frac_proc(3)
        frac_frz= max( -1.0, min( 0.0, frac_frz))

          if((frac_cc.lt.0..and.dropdel(2).lt.-1.e-5).or.         &
             (frac_frz.lt.0..and.dropdel(3).lt.-1.e-5)) then

!             write(msg,1166) frac_cc,dropdel(2),frac_frz,dropdel(3)
!             call peg_message( lunerr, msg)
             do n=1,ntype_aer
             do m=1,nsize_aer(n)

!if(m.eq.2) then
!                call wrf_message('In-cloud wet removal update:')
!                cblnumb = numptr_aer(m,n,cw_phase)
!                write(msg,1177) cblnumb*1., chem_n(2,m,n)
!                call peg_message( lunerr, msg)
!endif

                chem_n(2,m,n)=(1.+frac_cc)*(1.+frac_frz)*chem_n(2,m,n)

                do lc=1,ncomp_aer(n)
                  chem_m(2,m,n,lc)=(1.+frac_cc)*(1.+frac_frz)*chem_m(2,m,n,lc)
                end do

!if(m.eq.2) then
!                write(msg,1177) chem_n(2,m,n)
!                call peg_message( lunerr, msg)
!endif

             enddo
             enddo

          endif

!---------------------------------------------------------
! sub-cloud wet removal, decrease in the MOSAIC interstitial AP number and mass
! precr,  kg/m2/s,       sedimentation fluxs of rain
! the variables are in the SBM scheme


!---------------------------------------------------------

1166    format( 12e13.5)
1177    format(  6e20.12)
1188    format( 12i13)

      end subroutine resusp_wet_remov

END MODULE module_sbm_mosaic_ikj

