!     =======================================================================
!                 THIS IS THE INTERFACE FOR THE SS-MOSAIC MODEL
!     =======================================================================

      MODULE mod_SSMOSAIC

!     MODULE INTERFACE:            
!     =======================================================================
      USE mod_SSMOSAIC_PARAMS, &
      ONLY: nCOMP, nBINS, num_chem, p_HO, SOAMASS3d_00, SOAMASS3d_01
      
      USE mod_STEP_SOACOND, ONLY: STEP_SOACOND
      USE mod_STEP_OLIG,    ONLY: STEP_OLIG
      USE mod_STEP_HETCHEM, ONLY: STEP_HETCHEM
      USE mod_STEP_GASCHEM, ONLY: STEP_GASCHEM
      USE mod_MAP_INOUT,    ONLY: MAP_INOUT
      
      CONTAINS
      
!     =======================================================================
!                          THIS IS THE DRIVER FOR MOSAIC
!     =======================================================================

      SUBROUTINE drive_SSMOSAIC(CHEM, iD1,iD2, jD1,jD2, kD1,kD2, &
                                      iM1,iM2, jM1,jM2, kM1,kM2, & 
                                      iT1,iT2, jT1,jT2, kT1,kT2, &
                                      DIFFb, kfOLIG, krOLIG, gammaOH, pHETFRAG, TEMP3d, mdt)
      
      IMPLICIT NONE

      INTEGER,INTENT(IN) :: iD1, iD2, jD1, jD2, kD1, kD2
      INTEGER,INTENT(IN) :: iM1, iM2, jM1, jM2, kM1, kM2
      INTEGER,INTENT(IN) :: iT1, iT2, jT1, jT2, kT1, kT2

!     WRF ARRAY FOR TRACER SPECIES 
!     FOR GAS [ppm] AND FOR AEROSOL SPECIES [ug per kg-air]:
      REAL,INTENT(INOUT), &
      DIMENSION( iM1:iM2, kM1:kM2, jM1:jM2, 1:num_chem ) :: CHEM
      
!     TEMPERATURE FILED [K]:
      REAL,INTENT(IN), &
      DIMENSION( iM1:iM2, kM1:kM2, jM1:jM2) :: TEMP3d

!     MODEL TIMESTEP,
!     INTERNAL CLOCK, AND 
!     INTEGRATION TIMESTEP [s]:
      REAL,INTENT(IN) :: mdt
      REAL(8) :: clock
      REAL(8) :: mmdt
      

      REAL(8) :: DIFFb


!     MOSAIC GAS [ug m-3], 
!     AEROSOL [ug m-3] AND OLIGOMER [ug m-3] ARRAYS:
      REAL(8) :: moGAS(nCOMP)
      REAL(8) :: moAERO(nCOMP,nBINS)
      REAL(8) :: moOLIG(nCOMP,nBINS)

!     AEROSOl SIZE BIN DIAMETERS [nm]:
      REAL(8),DIMENSION(nBINS) :: &
      DIAM = (/78.0, 312.2, 1249.2, 4999.0/)

!     AEROSOL NUMBER CONC. [cm-3]:
      REAL(8),DIMENSION(nBINS) :: NUM
      
!     AEROSOL NUMBER CONC. [cm-3]:
!      REAL(8),DIMENSION(nBINS) :: &
!      NUMxx = (/3000.,200.,2.,1./)

!     OLIGOMERIZATION FORWARD [cm3 s-1] AND
!     REVERSE RATES [s-1]:
      REAL(8) :: kfOLIG
      REAL(8) :: krOLIG

!     HET. OH UPTAKE COEFFICIENT:
      REAL(8) :: gammaOH

!     HET. FRAGMENTATION PROBABILITY:
      REAL(8) :: pHETFRAG

!     OH CONCENTRATION:
      REAL(8) :: OH

!     TEMPERATUER [K]:
      REAL(8) :: Temp

!     COUNTERS:
      INTEGER :: i,j,k,ii,jj,kk

!     CHECK ALLOCATED:
!     =======================================================================
      IF (.NOT.ALLOCATED(SOAMASS3d_00)) THEN
         ALLOCATE(SOAMASS3d_00(iM1:iM2, kM1:kM2, jM1:jM2, nBINS))
      END IF
      
      IF (.NOT.ALLOCATED(SOAMASS3d_01)) THEN
         ALLOCATE(SOAMASS3d_01(iM1:iM2, kM1:kM2, jM1:jM2, nBINS))
      END IF



!     LOOP OVER TILES:
!     =======================================================================
      LOOP1: DO i = iT1,iT2
      LOOP2: DO j = jT1,jT2
      LOOP3: DO k = kT1,kT2

!     STEP FOR GAS-PHASE CHEMISTRY:
!     =======================================================================
!      OH = CHEM(i,k,j,p_HO)*1e-6*(101325./8.314/298.)*6.022e23*1e-6

!      mmdt = mdt/1.0

!      CALL STEP_GASCHEM(i,k,j,CHEM,iM1,iM2, jM1,jM2, kM1,kM2, num_chem, OH, mmdt)

!     FLUSH ARRAYS:
!     =======================================================================
      moGAS(:) = 0.0
      
      moAERO(:,:) = 0.0
      moOLIG(:,:) = 0.0

!     MAPPING IN:
!     =======================================================================
      CALL MAP_INOUT(i,k,j,0,CHEM, &
                     iM1,iM2, jM1,jM2, kM1,kM2, num_chem, &
                     moGAS, moAERO, moOLIG, NUM, nCOMP, nBINS)

!     SAVE MASS VALUEs:
!     =======================================================================
      DO kk = 1,nBINS
         SOAMASS3d_00(i,k,j,kk) = SUM(moAERO(:,kk)) + SUM(moOLIG(:,kk))
      END DO

!     LOOP OVER TIMESTEPS FOR PARTITIONING:
!     =======================================================================
!     ZERO CLOCK:
      clock = 0.0

      LOOP6: DO WHILE (clock < DBLE(mdt))

!     CALCULATE INTEGRATION TIMESTEP (ASTEM):
!      mmdt = DBLE(mdt)/1.0; clock = clock + mmdt
      mmdt = 10.; clock = clock + mmdt
      
!     STEP FOR SOA CONDENSATION:
!     =======================================================================
!     > GET TEMPERATURE:
      Temp = TEMP3d(i,k,j)

      CALL STEP_SOACOND(moGAS, moAERO, moOLIG, nCOMP, nBINS, DIAM, NUM, DIFFb, Temp, mmdt)
      
!     STEP FOR OLIGOMERIZATION:
!     =======================================================================
      CALL STEP_OLIG(moAERO, moOLIG, nCOMP, nBINS, DIAM, NUM, kfOLIG, krOLIG, mmdt)

!     STEP FOR HETEROGENEOUS OXIDATION:
!     =======================================================================
      OH = CHEM(i,k,j,p_HO)*1e-6*(101325./8.314/298.)*6.022e23*1e-6

      CALL STEP_HETCHEM(moAERO, nCOMP, nBINS, DIAM, NUM, gammaOH, pHETFRAG, OH , mmdt)

!     MAPPING OUT:
!     =======================================================================
      CALL MAP_INOUT(i,k,j,1,CHEM, &
                     iM1,iM2, jM1,jM2, kM1,kM2, num_chem, &
                     moGAS, moAERO, moOLIG, NUM, nCOMP, nBINS)
      
      END DO LOOP6

!     SAVE MASS VALUEs:
!     =======================================================================
      DO kk = 1,nBINS
         SOAMASS3d_01(i,k,j,kk) = SUM(moAERO(:,kk)) + SUM(moOLIG(:,kk))
      END DO

      END DO LOOP3
      END DO LOOP2
      END DO LOOP1      

      RETURN
      END SUBROUTINE
      
      END MODULE mod_SSMOSAIC
