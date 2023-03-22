!---YZ0220:Add for considering the heat flux of plume released into atmosphere by Yuwei Zhang

 Module module_plumeheat_driver
 use module_model_constants, only: cp
        
 CONTAINS
 subroutine plumeheat_driver(DX,firesize_agtf,firesize_agef,firesize_agsv,  &
             firesize_aggr,plume_hgt_agtf,plume_hgt_agef,plume_hgt_agsv,    &
             plume_hgt_aggr,ht,z_at_w,dz8w,rho,pi_phy,rthwften,tten_wf,       &
             itimestep,IDS,IDE, JDS,JDE, KDS,KDE,IMS,IME, JMS,JME, KMS,KME,  &
             i_start,i_end,j_start,j_end,kts, kte,num_tiles)
 IMPLICIT NONE
    INTEGER,      INTENT(IN   )    ::  ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                                         kts,kte, &
                                       num_tiles
   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                       &
                i_start,i_end,j_start,j_end
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(IN ) ::                   dz8w,pi_phy,rho,z_at_w 
   INTEGER, INTENT(IN  ) ::  itimestep
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(IN)  :: ht
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(INOUT)  ::                              rthwften,tten_wf
   real, dimension(ims:ime,jms:jme),optional,intent(in)::  &
                 firesize_agtf,firesize_agef,firesize_agsv,firesize_aggr,  &
                 plume_hgt_agtf,plume_hgt_agef,plume_hgt_agsv,plume_hgt_aggr
   real, intent(in) :: DX
   
   INTEGER :: i,j,k,its,ite,jts,jte,ij
   
   DO ij = 1 , num_tiles
         its = i_start(ij)
         ite = i_end(ij)
         jts = j_start(ij)
         jte = j_end(ij)
   ENDDO

     DO j=jts,jte
     DO k=kts,kte+1
     DO i=its,ite
        rthwften(I,K,J)=0.
        tten_wf(I,K,J)=0.
     ENDDO
     ENDDO
     ENDDO

!chemistry is called after dynamics-physics, so the heat of fire plume is calcuated from second timestep 
   IF (itimestep .gt.1)THEN 
  
   CALL calc_plumeheat_ten(DX,firesize_agtf,firesize_agef,firesize_agsv,  &
             firesize_aggr,plume_hgt_agtf,plume_hgt_agef,plume_hgt_agsv,      &
             plume_hgt_aggr,ht,z_at_w,dz8w,rho,pi_phy,rthwften,tten_wf,         &
             ims,ime, jms,jme, kms,kme,                                       &
             its,ite, jts,jte, kts,kte                                        )

   ENDIF
 end subroutine plumeheat_driver

 subroutine calc_plumeheat_ten(DX,firesize_agtf,firesize_agef,firesize_agsv,  &
             firesize_aggr,plume_hgt_agtf,plume_hgt_agef,plume_hgt_agsv,      &
             plume_hgt_aggr,ht,z_at_w,dz8w,rho,pi_phy,rthwften,tten_wf,         &
             ims,ime, jms,jme, kms,kme,                                       &
             its,ite, jts,jte, kts,kte                                        )
 implicit none
 integer, parameter :: nveg_agreg      = 4
 integer, parameter :: tropical_forest = 1
 integer, parameter :: boreal_forest   = 2
 integer, parameter :: savannah        = 3
 integer, parameter :: grassland       = 4
 integer, intent(in) :: ims,ime, jms,jme, kms,kme, &
                        its,ite, jts,jte, kts,kte
 real, intent(in) :: DX  
 real, dimension(ims:ime,jms:jme),optional,intent(in)::  &
                 firesize_agtf,firesize_agef,firesize_agsv,firesize_aggr,  &
                 plume_hgt_agtf,plume_hgt_agef,plume_hgt_agsv,plume_hgt_aggr
 real, dimension(ims:ime,jms:jme),intent(in):: ht
 real, dimension(ims:ime,kms:kme,jms:jme),intent(in):: z_at_w,dz8w,rho,pi_phy
 real, dimension(ims:ime,kms:kme,jms:jme),intent(out):: rthwften,tten_wf      !unit: K/s

! --- local variables
 integer :: i,j,k
 integer :: iveg_ag
 real, dimension(nveg_agreg) :: firesize,plume_hgt,heat
!upper bounds for heat flux of Tropical Forest,Boreal forest,cerrado/woody
!savanna,Grassland/cropland
 real,    dimension(nveg_agreg) :: heat_flux
 data heat_flux /80.0,80.0,23.0,3.3/ !unit:kW/m2
 real :: del_x,z_w,fact_z,z_ext
 real, dimension(kms:kme):: t_ten,hfx

 del_x=DX*1.0
 z_ext=50. !The default value in WRF-SFIRE. Dependent on fire property (i.e., fuel properties and flame length).

!distribute over different land index
 do j=jts,jte
 do i=its,ite
   
    !set zero for each grid
    do k=kts,kte
    rthwften(i,k,j)=0.
    tten_wf(i,k,j)=0.
    enddo

    firesize(1)=firesize_agtf(i,j)
    firesize(2)=firesize_agef(i,j)
    firesize(3)=firesize_agsv(i,j)
    firesize(4)=firesize_aggr(i,j)
    plume_hgt(1)=plume_hgt_agtf(i,j)
    plume_hgt(2)=plume_hgt_agef(i,j)
    plume_hgt(3)=plume_hgt_agsv(i,j)
    plume_hgt(4)=plume_hgt_aggr(i,j)

    !loop over each land category

    do iveg_ag=1,nveg_agreg

       if(firesize(iveg_ag).lt.1.e-6) cycle
    
    !get heat flux for model grid box in kW/m2

       heat(iveg_ag)=heat_flux(iveg_ag)*firesize(iveg_ag)/del_x/del_x

    !vertically distribute the heat flux 
    if(plume_hgt(iveg_ag).gt.0)then
       do k=kts,kte

       !set z (in meters above ground)

       z_w=z_at_w(i,k,j) - ht(i,j) 

       !set fact_z by exponential decreasing 

       fact_z=EXP(-z_w/z_ext)
       
       !vertically distribute heat flux
      
       hfx(k)=heat(iveg_ag)*fact_z  !kW/m2
       
       enddo
     else
       hfx(kts)=heat(iveg_ag)
       hfx(kts+1:kte)=0.
     endif

       !set as zero for different land categories
       do k=kts,kte
       t_ten(k)=0.
       enddo

       !add flux divergence to tendencies
       do k=kts,kte-1

       t_ten(k)=-(hfx(k+1)-hfx(k))*1000./rho(i,k,j)/dz8w(i,k,j)/cp

       rthwften(i,k,j)=rthwften(i,k,j)+t_ten(k)/pi_phy(i,k,j)
       tten_wf(i,k,j)=tten_wf(i,k,j)+t_ten(k)

       enddo 
    enddo !end iveg_ag
 enddo !end i
 enddo !end j

 end subroutine calc_plumeheat_ten

 END Module module_plumeheat_driver
