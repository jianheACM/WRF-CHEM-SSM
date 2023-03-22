!     =======================================================================
!             THIS SUBROUINTE STEPS FORWARD FOR GAS-PHASE OXIDATION
!     =======================================================================

      MODULE mod_STEP_GASCHEM
      
!     MODULE INTERFACE:
!     =======================================================================
      USE mod_SSMOSAIC_PARAMS, ONLY: p_NUME, p_DEN, &
      p_TERP, p_ARO1, p_ARO2, p_SESQ, p_ISOPRENE, p_PCG7_F_C, &

      p_TERP_CN3_g, p_TERP_CN2_g, p_TERP_CN1_g, p_TERP_C00_g, p_TERP_C01_g, p_TERP_C02_g, &
      p_TERP_C03_g, p_TERP_C04_g, p_TERP_C05_g, p_TERP_C06_g, p_TERP_C07_g, p_TERP_C08_g, &
      p_TERP_C09_g, &

      p_ARO1_CN3_g, p_ARO1_CN2_g, p_ARO1_CN1_g, p_ARO1_C00_g, p_ARO1_C01_g, p_ARO1_C02_g, &
      p_ARO1_C03_g, p_ARO1_C04_g, p_ARO1_C05_g, p_ARO1_C06_g, p_ARO1_C07_g, p_ARO1_C08_g, &
      p_ARO1_C09_g, &

      p_ARO2_CN3_g, p_ARO2_CN2_g, p_ARO2_CN1_g, p_ARO2_C00_g, p_ARO2_C01_g, p_ARO2_C02_g, &
      p_ARO2_C03_g, p_ARO2_C04_g, p_ARO2_C05_g, p_ARO2_C06_g, p_ARO2_C07_g, p_ARO2_C08_g, &
      p_ARO2_C09_g, &

      p_SESQ_CN3_g, p_SESQ_CN2_g, p_SESQ_CN1_g, p_SESQ_C00_g, p_SESQ_C01_g, p_SESQ_C02_g, &
      p_SESQ_C03_g, p_SESQ_C04_g, p_SESQ_C05_g, p_SESQ_C06_g, p_SESQ_C07_g, p_SESQ_C08_g, &
      p_SESQ_C09_g, &

      p_ISPR_CN3_g, p_ISPR_CN2_g, p_ISPR_CN1_g, p_ISPR_C00_g, p_ISPR_C01_g, p_ISPR_C02_g, &
      p_ISPR_C03_g, p_ISPR_C04_g, p_ISPR_C05_g, p_ISPR_C06_g, p_ISPR_C07_g, p_ISPR_C08_g, &
      p_ISPR_C09_g, &

      p_IVO1_CN3_g, p_IVO1_CN2_g, p_IVO1_CN1_g, p_IVO1_C00_g, p_IVO1_C01_g, p_IVO1_C02_g, &
      p_IVO1_C03_g, p_IVO1_C04_g, p_IVO1_C05_g, p_IVO1_C06_g, p_IVO1_C07_g, p_IVO1_C08_g, &
      p_IVO1_C09_g

      CONTAINS
      
!     =======================================================================
!           THIS SUBROUINTE STEPS FORWARD FOR HETEROGENEOUS OXIDATION
!     =======================================================================
      
      SUBROUTINE STEP_GASCHEM(iNOW, kNOW, jNOW, & 
                              CHEM, iM1,iM2, jM1,jM2, kM1,kM2, nCHEM, &
                              OH, mmdt)
      
!     DECLARATIONS:
!     =======================================================================
      IMPLICIT NONE
      
!     CURRENT TILE COORDINATES:
      INTEGER,INTENT(IN) :: iNOW, jNOW, kNOW

!     CHEM ARRAY DIMENSIONS:
      INTEGER,INTENT(IN) :: iM1,iM2
      INTEGER,INTENT(IN) :: jM1,jM2
      INTEGER,INTENT(IN) :: kM1,kM2
      INTEGER,INTENT(IN) :: nCHEM

!     WRF ARRAY FOR TRACER SPECIES 
!     FOR GAS [ppm] AND FOR AEROSOL SPECIES [ug per kg-air]:
      REAL,INTENT(INOUT), &
      DIMENSION(iM1:iM2, kM1:kM2, jM1:jM2, 1:nCHEM) :: CHEM

!     OH CONCENTRATION [cm-3]:
      REAL(8),INTENT(IN) :: OH

!     INTEGRATION TIMESTEP:
      REAL(8),INTENT(IN) :: mmdt

!     HIGH- AND LOW-NOx
!     BRANCHING RATIOS:
      REAL(8) :: brHHH
      REAL(8) :: brLLL

!     COUNTERS:
      INTEGER :: i,j,k

!     REACTED AMOUNTs:
      REAL(8) :: ddTERP
      REAL(8) :: ddARO1
      REAL(8) :: ddARO2
      REAL(8) :: ddSESQ
      REAL(8) :: ddISPR
      REAL(8) :: ddIVO1
      
!     REACTED AMOUNTs:
      REAL(8) :: ddTERP_CN3_g, ddTERP_CN2_g, ddTERP_CN1_g, ddTERP_C00_g, &
                 ddTERP_C01_g, ddTERP_C02_g, ddTERP_C03_g, ddTERP_C04_g, &
                 ddTERP_C05_g, ddTERP_C06_g, ddTERP_C07_g, ddTERP_C08_g, &
                 ddTERP_C09_g

      REAL(8) :: ddARO1_CN3_g, ddARO1_CN2_g, ddARO1_CN1_g, ddARO1_C00_g, &
                 ddARO1_C01_g, ddARO1_C02_g, ddARO1_C03_g, ddARO1_C04_g, &
                 ddARO1_C05_g, ddARO1_C06_g, ddARO1_C07_g, ddARO1_C08_g, &
                 ddARO1_C09_g

      REAL(8) :: ddARO2_CN3_g, ddARO2_CN2_g, ddARO2_CN1_g, ddARO2_C00_g, &
                 ddARO2_C01_g, ddARO2_C02_g, ddARO2_C03_g, ddARO2_C04_g, &
                 ddARO2_C05_g, ddARO2_C06_g, ddARO2_C07_g, ddARO2_C08_g, &
                 ddARO2_C09_g

      REAL(8) :: ddSESQ_CN3_g, ddSESQ_CN2_g, ddSESQ_CN1_g, ddSESQ_C00_g, &
                 ddSESQ_C01_g, ddSESQ_C02_g, ddSESQ_C03_g, ddSESQ_C04_g, &
                 ddSESQ_C05_g, ddSESQ_C06_g, ddSESQ_C07_g, ddSESQ_C08_g, &
                 ddSESQ_C09_g

      REAL(8) :: ddISPR_CN3_g, ddISPR_CN2_g, ddISPR_CN1_g, ddISPR_C00_g, &
                 ddISPR_C01_g, ddISPR_C02_g, ddISPR_C03_g, ddISPR_C04_g, &
                 ddISPR_C05_g, ddISPR_C06_g, ddISPR_C07_g, ddISPR_C08_g, &
                 ddISPR_C09_g

      REAL(8) :: ddIVO1_CN3_g, ddIVO1_CN2_g, ddIVO1_CN1_g, ddIVO1_C00_g, &
                 ddIVO1_C01_g, ddIVO1_C02_g, ddIVO1_C03_g, ddIVO1_C04_g, &
                 ddIVO1_C05_g, ddIVO1_C06_g, ddIVO1_C07_g, ddIVO1_C08_g, &
                 ddIVO1_C09_g

!     PRODUCED AMOUNTs FOR LOW-NOx:
      REAL(8) :: ppllTERP_CN3_g, ppllTERP_CN2_g, ppllTERP_CN1_g, ppllTERP_C00_g, &
                 ppllTERP_C01_g, ppllTERP_C02_g, ppllTERP_C03_g, ppllTERP_C04_g, &
                 ppllTERP_C05_g, ppllTERP_C06_g, ppllTERP_C07_g, ppllTERP_C08_g, &
                 ppllTERP_C09_g

      REAL(8) :: ppllARO1_CN3_g, ppllARO1_CN2_g, ppllARO1_CN1_g, ppllARO1_C00_g, &
                 ppllARO1_C01_g, ppllARO1_C02_g, ppllARO1_C03_g, ppllARO1_C04_g, &
                 ppllARO1_C05_g, ppllARO1_C06_g, ppllARO1_C07_g, ppllARO1_C08_g, &
                 ppllARO1_C09_g

      REAL(8) :: ppllARO2_CN3_g, ppllARO2_CN2_g, ppllARO2_CN1_g, ppllARO2_C00_g, &
                 ppllARO2_C01_g, ppllARO2_C02_g, ppllARO2_C03_g, ppllARO2_C04_g, &
                 ppllARO2_C05_g, ppllARO2_C06_g, ppllARO2_C07_g, ppllARO2_C08_g, &
                 ppllARO2_C09_g

      REAL(8) :: ppllSESQ_CN3_g, ppllSESQ_CN2_g, ppllSESQ_CN1_g, ppllSESQ_C00_g, &
                 ppllSESQ_C01_g, ppllSESQ_C02_g, ppllSESQ_C03_g, ppllSESQ_C04_g, &
                 ppllSESQ_C05_g, ppllSESQ_C06_g, ppllSESQ_C07_g, ppllSESQ_C08_g, &
                 ppllSESQ_C09_g

      REAL(8) :: ppllISPR_CN3_g, ppllISPR_CN2_g, ppllISPR_CN1_g, ppllISPR_C00_g, &
                 ppllISPR_C01_g, ppllISPR_C02_g, ppllISPR_C03_g, ppllISPR_C04_g, &
                 ppllISPR_C05_g, ppllISPR_C06_g, ppllISPR_C07_g, ppllISPR_C08_g, &
                 ppllISPR_C09_g

      REAL(8) :: ppllIVO1_CN3_g, ppllIVO1_CN2_g, ppllIVO1_CN1_g, ppllIVO1_C00_g, &
                 ppllIVO1_C01_g, ppllIVO1_C02_g, ppllIVO1_C03_g, ppllIVO1_C04_g, &
                 ppllIVO1_C05_g, ppllIVO1_C06_g, ppllIVO1_C07_g, ppllIVO1_C08_g, &
                 ppllIVO1_C09_g

!     PRODUCED AMOUNTs FOR HIGH-NOx:
      REAL(8) :: pphhTERP_CN3_g, pphhTERP_CN2_g, pphhTERP_CN1_g, pphhTERP_C00_g, &
                 pphhTERP_C01_g, pphhTERP_C02_g, pphhTERP_C03_g, pphhTERP_C04_g, &
                 pphhTERP_C05_g, pphhTERP_C06_g, pphhTERP_C07_g, pphhTERP_C08_g, &
                 pphhTERP_C09_g

      REAL(8) :: pphhARO1_CN3_g, pphhARO1_CN2_g, pphhARO1_CN1_g, pphhARO1_C00_g, &
                 pphhARO1_C01_g, pphhARO1_C02_g, pphhARO1_C03_g, pphhARO1_C04_g, &
                 pphhARO1_C05_g, pphhARO1_C06_g, pphhARO1_C07_g, pphhARO1_C08_g, &
                 pphhARO1_C09_g

      REAL(8) :: pphhARO2_CN3_g, pphhARO2_CN2_g, pphhARO2_CN1_g, pphhARO2_C00_g, &
                 pphhARO2_C01_g, pphhARO2_C02_g, pphhARO2_C03_g, pphhARO2_C04_g, &
                 pphhARO2_C05_g, pphhARO2_C06_g, pphhARO2_C07_g, pphhARO2_C08_g, &
                 pphhARO2_C09_g

      REAL(8) :: pphhSESQ_CN3_g, pphhSESQ_CN2_g, pphhSESQ_CN1_g, pphhSESQ_C00_g, &
                 pphhSESQ_C01_g, pphhSESQ_C02_g, pphhSESQ_C03_g, pphhSESQ_C04_g, &
                 pphhSESQ_C05_g, pphhSESQ_C06_g, pphhSESQ_C07_g, pphhSESQ_C08_g, &
                 pphhSESQ_C09_g

      REAL(8) :: pphhISPR_CN3_g, pphhISPR_CN2_g, pphhISPR_CN1_g, pphhISPR_C00_g, &
                 pphhISPR_C01_g, pphhISPR_C02_g, pphhISPR_C03_g, pphhISPR_C04_g, &
                 pphhISPR_C05_g, pphhISPR_C06_g, pphhISPR_C07_g, pphhISPR_C08_g, &
                 pphhISPR_C09_g

      REAL(8) :: pphhIVO1_CN3_g, pphhIVO1_CN2_g, pphhIVO1_CN1_g, pphhIVO1_C00_g, &
                 pphhIVO1_C01_g, pphhIVO1_C02_g, pphhIVO1_C03_g, pphhIVO1_C04_g, &
                 pphhIVO1_C05_g, pphhIVO1_C06_g, pphhIVO1_C07_g, pphhIVO1_C08_g, &
                 pphhIVO1_C09_g

!     CALCULATE BRANCHING RATIOS:
!     =======================================================================
      brHHH = CHEM(iNOW,kNOW,jNOW,p_NUME)/ &
             (CHEM(iNOW,kNOW,jNOW,p_NUME) + CHEM(iNOW,kNOW,jNOW,p_DEN))

      IF (brHHH.GT.1.0) brHHH = 1.0
      IF (brHHH.LT.0.0) brHHH = 0.0

!!!   MARK
      brHHH = 0.0

      brLLL = 1.0 - brHHH

!     CALCULATE REACTED AMOUNTS:
!     =======================================================================
!     PRECURSORS:
      ddTERP = 5.23e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP))*mmdt
      ddARO1 = 5.63e-12*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1))*mmdt
      ddARO2 = 2.31e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2))*mmdt
      ddSESQ = 1.97e-10*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ))*mmdt
      ddISPR = 1.00e-10*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ISOPRENE))*mmdt
      ddIVO1 = 1.32e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_PCG7_F_C))*mmdt

!     SIMPLESOM SPECIES:
      ddTERP_C09_g = 1.18e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C09_g))*mmdt
      ddTERP_C08_g = 1.81e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C08_g))*mmdt
      ddTERP_C07_g = 2.40e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C07_g))*mmdt
      ddTERP_C06_g = 2.93e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C06_g))*mmdt
      ddTERP_C05_g = 3.41e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C05_g))*mmdt
      ddTERP_C04_g = 3.83e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C04_g))*mmdt
      ddTERP_C03_g = 4.21e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C03_g))*mmdt
      ddTERP_C02_g = 4.54e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C02_g))*mmdt
      ddTERP_C01_g = 4.81e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C01_g))*mmdt
      ddTERP_C00_g = 5.03e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C00_g))*mmdt
      ddTERP_CN1_g = 5.20e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_g))*mmdt
      ddTERP_CN2_g = 5.32e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_g))*mmdt
      ddTERP_CN3_g = 5.38e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_g))*mmdt

      ddARO1_C09_g = 1.17e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_g))*mmdt
      ddARO1_C08_g = 1.80e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_g))*mmdt
      ddARO1_C07_g = 2.38e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_g))*mmdt
      ddARO1_C06_g = 2.91e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_g))*mmdt
      ddARO1_C05_g = 3.39e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_g))*mmdt
      ddARO1_C04_g = 3.82e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_g))*mmdt
      ddARO1_C03_g = 4.19e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_g))*mmdt
      ddARO1_C02_g = 4.52e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_g))*mmdt
      ddARO1_C01_g = 4.79e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_g))*mmdt
      ddARO1_C00_g = 5.01e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_g))*mmdt
      ddARO1_CN1_g = 5.18e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_g))*mmdt
      ddARO1_CN2_g = 5.30e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_g))*mmdt
      ddARO1_CN3_g = 5.37e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_g))*mmdt

      ddARO2_C09_g = 1.23e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_g))*mmdt
      ddARO2_C08_g = 1.92e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_g))*mmdt
      ddARO2_C07_g = 2.54e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_g))*mmdt
      ddARO2_C06_g = 3.11e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_g))*mmdt
      ddARO2_C05_g = 3.62e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_g))*mmdt
      ddARO2_C04_g = 4.06e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_g))*mmdt
      ddARO2_C03_g = 4.45e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_g))*mmdt
      ddARO2_C02_g = 4.77e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_g))*mmdt
      ddARO2_C01_g = 5.04e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_g))*mmdt
      ddARO2_C00_g = 5.25e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_g))*mmdt
      ddARO2_CN1_g = 5.39e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_g))*mmdt
      ddARO2_CN2_g = 5.48e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_g))*mmdt
      ddARO2_CN3_g = 5.51e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_g))*mmdt

      ddSESQ_C09_g = 1.45e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_g))*mmdt
      ddSESQ_C08_g = 2.35e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_g))*mmdt
      ddSESQ_C07_g = 3.16e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_g))*mmdt
      ddSESQ_C06_g = 3.87e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_g))*mmdt
      ddSESQ_C05_g = 4.49e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_g))*mmdt
      ddSESQ_C04_g = 5.01e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_g))*mmdt
      ddSESQ_C03_g = 5.44e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_g))*mmdt
      ddSESQ_C02_g = 5.77e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_g))*mmdt
      ddSESQ_C01_g = 6.01e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_g))*mmdt
      ddSESQ_C00_g = 6.16e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_g))*mmdt
      ddSESQ_CN1_g = 6.21e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_g))*mmdt
      ddSESQ_CN2_g = 6.16e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_g))*mmdt
      ddSESQ_CN3_g = 6.02e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_g))*mmdt

      ddISPR_C09_g = 1.12e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_g))*mmdt
      ddISPR_C08_g = 1.69e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_g))*mmdt
      ddISPR_C07_g = 2.23e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_g))*mmdt
      ddISPR_C06_g = 2.72e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_g))*mmdt
      ddISPR_C05_g = 1.00e-10*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_g))*mmdt
      ddISPR_C04_g = 1.00e-10*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_g))*mmdt
      ddISPR_C03_g = 1.00e-10*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_g))*mmdt
      ddISPR_C02_g = 1.00e-10*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_g))*mmdt
      ddISPR_C01_g = 1.00e-10*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_g))*mmdt
      ddISPR_C00_g = 1.00e-10*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_g))*mmdt
      ddISPR_CN1_g = 1.00e-10*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_g))*mmdt
      ddISPR_CN2_g = 1.00e-10*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_g))*mmdt
      ddISPR_CN3_g = 1.00e-10*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_g))*mmdt

      ddIVO1_C09_g = 1.22e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_g))*mmdt
      ddIVO1_C08_g = 1.89e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_g))*mmdt
      ddIVO1_C07_g = 2.50e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_g))*mmdt
      ddIVO1_C06_g = 3.06e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_g))*mmdt
      ddIVO1_C05_g = 3.56e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_g))*mmdt
      ddIVO1_C04_g = 4.00e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_g))*mmdt
      ddIVO1_C03_g = 4.38e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_g))*mmdt
      ddIVO1_C02_g = 4.71e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_g))*mmdt
      ddIVO1_C01_g = 4.98e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_g))*mmdt
      ddIVO1_C00_g = 5.19e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_g))*mmdt
      ddIVO1_CN1_g = 5.34e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_g))*mmdt
      ddIVO1_CN2_g = 5.44e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_g))*mmdt
      ddIVO1_CN3_g = 5.47e-11*OH*DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_g))*mmdt

!     CALCULATE PRODUCED AMOUNTS FOR LOW-NOx:
!     =======================================================================
!     SIMPLESOM SPECIES:
      ppllTERP_C09_g = &
      0.000*ddTERP_C09_g + 0.001*ddTERP_C08_g + 0.002*ddTERP_C07_g + 0.000*ddTERP_C06_g + &
      0.000*ddTERP_C05_g + 0.000*ddTERP_C04_g + 0.000*ddTERP_C03_g + 0.000*ddTERP_C02_g + &
      0.000*ddTERP_C01_g + 0.000*ddTERP_C00_g + 0.000*ddTERP_CN1_g + 0.000*ddTERP_CN2_g + &
      0.000*ddTERP_CN3_g + 0.001*ddTERP

      ppllTERP_C08_g = &
      0.001*ddTERP_C09_g + 0.000*ddTERP_C08_g + 0.002*ddTERP_C07_g + 0.003*ddTERP_C06_g + &
      0.000*ddTERP_C05_g + 0.000*ddTERP_C04_g + 0.000*ddTERP_C03_g + 0.000*ddTERP_C02_g + &
      0.000*ddTERP_C01_g + 0.000*ddTERP_C00_g + 0.000*ddTERP_CN1_g + 0.000*ddTERP_CN2_g + &
      0.000*ddTERP_CN3_g + 0.003*ddTERP

      ppllTERP_C07_g = &
      0.016*ddTERP_C09_g + 0.000*ddTERP_C08_g + 0.000*ddTERP_C07_g + 0.003*ddTERP_C06_g + &
      0.004*ddTERP_C05_g + 0.000*ddTERP_C04_g + 0.000*ddTERP_C03_g + 0.000*ddTERP_C02_g + &
      0.000*ddTERP_C01_g + 0.000*ddTERP_C00_g + 0.000*ddTERP_CN1_g + 0.000*ddTERP_CN2_g + &
      0.000*ddTERP_CN3_g + 0.002*ddTERP

      ppllTERP_C06_g = &
      0.231*ddTERP_C09_g + 0.012*ddTERP_C08_g + 0.000*ddTERP_C07_g + 0.000*ddTERP_C06_g + &
      0.004*ddTERP_C05_g + 0.004*ddTERP_C04_g + 0.000*ddTERP_C03_g + 0.000*ddTERP_C02_g + &
      0.000*ddTERP_C01_g + 0.000*ddTERP_C00_g + 0.000*ddTERP_CN1_g + 0.000*ddTERP_CN2_g + &
      0.000*ddTERP_CN3_g + 0.000*ddTERP

      ppllTERP_C05_g = &
      0.509*ddTERP_C09_g + 0.171*ddTERP_C08_g + 0.009*ddTERP_C07_g + 0.000*ddTERP_C06_g + &
      0.000*ddTERP_C05_g + 0.004*ddTERP_C04_g + 0.004*ddTERP_C03_g + 0.000*ddTERP_C02_g + &
      0.000*ddTERP_C01_g + 0.000*ddTERP_C00_g + 0.000*ddTERP_CN1_g + 0.000*ddTERP_CN2_g + &
      0.000*ddTERP_CN3_g + 0.004*ddTERP

      ppllTERP_C04_g = &
      0.078*ddTERP_C09_g + 0.376*ddTERP_C08_g + 0.126*ddTERP_C07_g + 0.006*ddTERP_C06_g + &
      0.000*ddTERP_C05_g + 0.000*ddTERP_C04_g + 0.004*ddTERP_C03_g + 0.004*ddTERP_C02_g + &
      0.000*ddTERP_C01_g + 0.000*ddTERP_C00_g + 0.000*ddTERP_CN1_g + 0.000*ddTERP_CN2_g + &
      0.000*ddTERP_CN3_g + 0.054*ddTERP

      ppllTERP_C03_g = &
      0.153*ddTERP_C09_g + 0.058*ddTERP_C08_g + 0.278*ddTERP_C07_g + 0.093*ddTERP_C06_g + &
      0.005*ddTERP_C05_g + 0.000*ddTERP_C04_g + 0.000*ddTERP_C03_g + 0.004*ddTERP_C02_g + &
      0.005*ddTERP_C01_g + 0.000*ddTERP_C00_g + 0.000*ddTERP_CN1_g + 0.000*ddTERP_CN2_g + &
      0.000*ddTERP_CN3_g + 0.165*ddTERP

      ppllTERP_C02_g = &
      0.000*ddTERP_C09_g + 0.113*ddTERP_C08_g + 0.043*ddTERP_C07_g + 0.206*ddTERP_C06_g + &
      0.069*ddTERP_C05_g + 0.003*ddTERP_C04_g + 0.000*ddTERP_C03_g + 0.000*ddTERP_C02_g + &
      0.005*ddTERP_C01_g + 0.005*ddTERP_C00_g + 0.000*ddTERP_CN1_g + 0.000*ddTERP_CN2_g + &
      0.000*ddTERP_CN3_g + 0.139*ddTERP

      ppllTERP_C01_g = &
      0.001*ddTERP_C09_g + 0.000*ddTERP_C08_g + 0.083*ddTERP_C07_g + 0.032*ddTERP_C06_g + &
      0.152*ddTERP_C05_g + 0.051*ddTERP_C04_g + 0.003*ddTERP_C03_g + 0.000*ddTERP_C02_g + &
      0.000*ddTERP_C01_g + 0.005*ddTERP_C00_g + 0.005*ddTERP_CN1_g + 0.000*ddTERP_CN2_g + &
      0.000*ddTERP_CN3_g + 0.052*ddTERP

      ppllTERP_C00_g = &
      0.000*ddTERP_C09_g + 0.000*ddTERP_C08_g + 0.000*ddTERP_C07_g + 0.062*ddTERP_C06_g + &
      0.023*ddTERP_C05_g + 0.112*ddTERP_C04_g + 0.038*ddTERP_C03_g + 0.002*ddTERP_C02_g + &
      0.000*ddTERP_C01_g + 0.000*ddTERP_C00_g + 0.005*ddTERP_CN1_g + 0.005*ddTERP_CN2_g + &
      0.000*ddTERP_CN3_g + 0.037*ddTERP

      ppllTERP_CN1_g = &
      0.000*ddTERP_C09_g + 0.000*ddTERP_C08_g + 0.000*ddTERP_C07_g + 0.000*ddTERP_C06_g + &
      0.046*ddTERP_C05_g + 0.017*ddTERP_C04_g + 0.083*ddTERP_C03_g + 0.028*ddTERP_C02_g + &
      0.001*ddTERP_C01_g + 0.000*ddTERP_C00_g + 0.000*ddTERP_CN1_g + 0.005*ddTERP_CN2_g + &
      0.005*ddTERP_CN3_g + 0.000*ddTERP

      ppllTERP_CN2_g = &
      0.000*ddTERP_C09_g + 0.000*ddTERP_C08_g + 0.000*ddTERP_C07_g + 0.000*ddTERP_C06_g + &
      0.000*ddTERP_C05_g + 0.034*ddTERP_C04_g + 0.013*ddTERP_C03_g + 0.061*ddTERP_C02_g + &
      0.021*ddTERP_C01_g + 0.001*ddTERP_C00_g + 0.000*ddTERP_CN1_g + 0.000*ddTERP_CN2_g + &
      0.005*ddTERP_CN3_g + 0.000*ddTERP

      ppllTERP_CN3_g = &
      0.012*ddTERP_C09_g + 0.009*ddTERP_C08_g + 0.007*ddTERP_C07_g + 0.005*ddTERP_C06_g + &
      0.004*ddTERP_C05_g + 0.003*ddTERP_C04_g + 0.027*ddTERP_C03_g + 0.029*ddTERP_C02_g + &
      0.067*ddTERP_C01_g + 0.065*ddTERP_C00_g + 0.049*ddTERP_CN1_g + 0.036*ddTERP_CN2_g + &
      0.027*ddTERP_CN3_g + 0.017*ddTERP

!     SIMPLESOM SPECIES:
      ppllARO1_C09_g = &
      0.000*ddARO1_C09_g + 0.077*ddARO1_C08_g + 0.131*ddARO1_C07_g + 0.000*ddARO1_C06_g + &
      0.000*ddARO1_C05_g + 0.000*ddARO1_C04_g + 0.000*ddARO1_C03_g + 0.000*ddARO1_C02_g + &
      0.000*ddARO1_C01_g + 0.000*ddARO1_C00_g + 0.000*ddARO1_CN1_g + 0.000*ddARO1_CN2_g + &
      0.000*ddARO1_CN3_g + 0.126*ddARO1

      ppllARO1_C08_g = &
      0.072*ddARO1_C09_g + 0.000*ddARO1_C08_g + 0.131*ddARO1_C07_g + 0.169*ddARO1_C06_g + &
      0.000*ddARO1_C05_g + 0.000*ddARO1_C04_g + 0.000*ddARO1_C03_g + 0.000*ddARO1_C02_g + &
      0.000*ddARO1_C01_g + 0.000*ddARO1_C00_g + 0.000*ddARO1_CN1_g + 0.000*ddARO1_CN2_g + &
      0.000*ddARO1_CN3_g + 0.118*ddARO1

      ppllARO1_C07_g = &
      0.188*ddARO1_C09_g + 0.050*ddARO1_C08_g + 0.000*ddARO1_C07_g + 0.169*ddARO1_C06_g + &
      0.195*ddARO1_C05_g + 0.000*ddARO1_C04_g + 0.000*ddARO1_C03_g + 0.000*ddARO1_C02_g + &
      0.000*ddARO1_C01_g + 0.000*ddARO1_C00_g + 0.000*ddARO1_CN1_g + 0.000*ddARO1_CN2_g + &
      0.000*ddARO1_CN3_g + 0.005*ddARO1

      ppllARO1_C06_g = &
      0.078*ddARO1_C09_g + 0.131*ddARO1_C08_g + 0.035*ddARO1_C07_g + 0.000*ddARO1_C06_g + &
      0.195*ddARO1_C05_g + 0.213*ddARO1_C04_g + 0.000*ddARO1_C03_g + 0.000*ddARO1_C02_g + &
      0.000*ddARO1_C01_g + 0.000*ddARO1_C00_g + 0.000*ddARO1_CN1_g + 0.000*ddARO1_CN2_g + &
      0.000*ddARO1_CN3_g + 0.045*ddARO1

      ppllARO1_C05_g = &
      0.191*ddARO1_C09_g + 0.054*ddARO1_C08_g + 0.091*ddARO1_C07_g + 0.024*ddARO1_C06_g + &
      0.000*ddARO1_C05_g + 0.213*ddARO1_C04_g + 0.226*ddARO1_C03_g + 0.000*ddARO1_C02_g + &
      0.000*ddARO1_C01_g + 0.000*ddARO1_C00_g + 0.000*ddARO1_CN1_g + 0.000*ddARO1_CN2_g + &
      0.000*ddARO1_CN3_g + 0.088*ddARO1

      ppllARO1_C04_g = &
      0.093*ddARO1_C09_g + 0.133*ddARO1_C08_g + 0.038*ddARO1_C07_g + 0.064*ddARO1_C06_g + &
      0.017*ddARO1_C05_g + 0.000*ddARO1_C04_g + 0.226*ddARO1_C03_g + 0.235*ddARO1_C02_g + &
      0.000*ddARO1_C01_g + 0.000*ddARO1_C00_g + 0.000*ddARO1_CN1_g + 0.000*ddARO1_CN2_g + &
      0.000*ddARO1_CN3_g + 0.047*ddARO1

      ppllARO1_C03_g = &
      0.211*ddARO1_C09_g + 0.065*ddARO1_C08_g + 0.093*ddARO1_C07_g + 0.026*ddARO1_C06_g + &
      0.044*ddARO1_C05_g + 0.012*ddARO1_C04_g + 0.000*ddARO1_C03_g + 0.235*ddARO1_C02_g + &
      0.241*ddARO1_C01_g + 0.000*ddARO1_C00_g + 0.000*ddARO1_CN1_g + 0.000*ddARO1_CN2_g + &
      0.000*ddARO1_CN3_g + 0.090*ddARO1

      ppllARO1_C02_g = &
      0.054*ddARO1_C09_g + 0.147*ddARO1_C08_g + 0.045*ddARO1_C07_g + 0.065*ddARO1_C06_g + &
      0.018*ddARO1_C05_g + 0.031*ddARO1_C04_g + 0.008*ddARO1_C03_g + 0.000*ddARO1_C02_g + &
      0.241*ddARO1_C01_g + 0.246*ddARO1_C00_g + 0.000*ddARO1_CN1_g + 0.000*ddARO1_CN2_g + &
      0.000*ddARO1_CN3_g + 0.055*ddARO1

      ppllARO1_C01_g = &
      0.113*ddARO1_C09_g + 0.038*ddARO1_C08_g + 0.102*ddARO1_C07_g + 0.032*ddARO1_C06_g + &
      0.045*ddARO1_C05_g + 0.013*ddARO1_C04_g + 0.022*ddARO1_C03_g + 0.006*ddARO1_C02_g + &
      0.000*ddARO1_C01_g + 0.246*ddARO1_C00_g + 0.249*ddARO1_CN1_g + 0.000*ddARO1_CN2_g + &
      0.000*ddARO1_CN3_g + 0.096*ddARO1

      ppllARO1_C00_g = &
      0.000*ddARO1_C09_g + 0.078*ddARO1_C08_g + 0.026*ddARO1_C07_g + 0.071*ddARO1_C06_g + &
      0.022*ddARO1_C05_g + 0.031*ddARO1_C04_g + 0.009*ddARO1_C03_g + 0.015*ddARO1_C02_g + &
      0.004*ddARO1_C01_g + 0.000*ddARO1_C00_g + 0.249*ddARO1_CN1_g + 0.251*ddARO1_CN2_g + &
      0.000*ddARO1_CN3_g + 0.031*ddARO1

      ppllARO1_CN1_g = &
      0.000*ddARO1_C09_g + 0.000*ddARO1_C08_g + 0.055*ddARO1_C07_g + 0.018*ddARO1_C06_g + &
      0.050*ddARO1_C05_g + 0.015*ddARO1_C04_g + 0.022*ddARO1_C03_g + 0.006*ddARO1_C02_g + &
      0.011*ddARO1_C01_g + 0.003*ddARO1_C00_g + 0.000*ddARO1_CN1_g + 0.251*ddARO1_CN2_g + &
      0.252*ddARO1_CN3_g + 0.049*ddARO1

      ppllARO1_CN2_g = &
      0.000*ddARO1_C09_g + 0.000*ddARO1_C08_g + 0.000*ddARO1_C07_g + 0.038*ddARO1_C06_g + &
      0.013*ddARO1_C05_g + 0.035*ddARO1_C04_g + 0.011*ddARO1_C03_g + 0.015*ddARO1_C02_g + &
      0.004*ddARO1_C01_g + 0.007*ddARO1_C00_g + 0.002*ddARO1_CN1_g + 0.000*ddARO1_CN2_g + &
      0.252*ddARO1_CN3_g + 0.000*ddARO1

      ppllARO1_CN3_g = &
      0.001*ddARO1_C09_g + 0.001*ddARO1_C08_g + 0.000*ddARO1_C07_g + 0.000*ddARO1_C06_g + &
      0.027*ddARO1_C05_g + 0.028*ddARO1_C04_g + 0.043*ddARO1_C03_g + 0.038*ddARO1_C02_g + &
      0.037*ddARO1_C01_g + 0.029*ddARO1_C00_g + 0.025*ddARO1_CN1_g + 0.019*ddARO1_CN2_g + &
      0.013*ddARO1_CN3_g + 0.002*ddARO1

!     SIMPLESOM SPECIES:
      ppllARO2_C09_g = &
      0.000*ddARO2_C09_g + 0.048*ddARO2_C08_g + 0.082*ddARO2_C07_g + 0.000*ddARO2_C06_g + &
      0.000*ddARO2_C05_g + 0.000*ddARO2_C04_g + 0.000*ddARO2_C03_g + 0.000*ddARO2_C02_g + &
      0.000*ddARO2_C01_g + 0.000*ddARO2_C00_g + 0.000*ddARO2_CN1_g + 0.000*ddARO2_CN2_g + &
      0.000*ddARO2_CN3_g + 0.061*ddARO2

      ppllARO2_C08_g = &
      0.007*ddARO2_C09_g + 0.000*ddARO2_C08_g + 0.082*ddARO2_C07_g + 0.108*ddARO2_C06_g + &
      0.000*ddARO2_C05_g + 0.000*ddARO2_C04_g + 0.000*ddARO2_C03_g + 0.000*ddARO2_C02_g + &
      0.000*ddARO2_C01_g + 0.000*ddARO2_C00_g + 0.000*ddARO2_CN1_g + 0.000*ddARO2_CN2_g + &
      0.000*ddARO2_CN3_g + 0.033*ddARO2

      ppllARO2_C07_g = &
      0.037*ddARO2_C09_g + 0.005*ddARO2_C08_g + 0.000*ddARO2_C07_g + 0.108*ddARO2_C06_g + &
      0.126*ddARO2_C05_g + 0.000*ddARO2_C04_g + 0.000*ddARO2_C03_g + 0.000*ddARO2_C02_g + &
      0.000*ddARO2_C01_g + 0.000*ddARO2_C00_g + 0.000*ddARO2_CN1_g + 0.000*ddARO2_CN2_g + &
      0.000*ddARO2_CN3_g + 0.003*ddARO2

      ppllARO2_C06_g = &
      0.150*ddARO2_C09_g + 0.027*ddARO2_C08_g + 0.004*ddARO2_C07_g + 0.000*ddARO2_C06_g + &
      0.126*ddARO2_C05_g + 0.140*ddARO2_C04_g + 0.000*ddARO2_C03_g + 0.000*ddARO2_C02_g + &
      0.000*ddARO2_C01_g + 0.000*ddARO2_C00_g + 0.000*ddARO2_CN1_g + 0.000*ddARO2_CN2_g + &
      0.000*ddARO2_CN3_g + 0.018*ddARO2

      ppllARO2_C05_g = &
      0.134*ddARO2_C09_g + 0.110*ddARO2_C08_g + 0.020*ddARO2_C07_g + 0.003*ddARO2_C06_g + &
      0.000*ddARO2_C05_g + 0.140*ddARO2_C04_g + 0.150*ddARO2_C03_g + 0.000*ddARO2_C02_g + &
      0.000*ddARO2_C01_g + 0.000*ddARO2_C00_g + 0.000*ddARO2_CN1_g + 0.000*ddARO2_CN2_g + &
      0.000*ddARO2_CN3_g + 0.074*ddARO2

      ppllARO2_C04_g = &
      0.079*ddARO2_C09_g + 0.098*ddARO2_C08_g + 0.080*ddARO2_C07_g + 0.014*ddARO2_C06_g + &
      0.002*ddARO2_C05_g + 0.000*ddARO2_C04_g + 0.150*ddARO2_C03_g + 0.157*ddARO2_C02_g + &
      0.000*ddARO2_C01_g + 0.000*ddARO2_C00_g + 0.000*ddARO2_CN1_g + 0.000*ddARO2_CN2_g + &
      0.000*ddARO2_CN3_g + 0.091*ddARO2

      ppllARO2_C03_g = &
      0.222*ddARO2_C09_g + 0.058*ddARO2_C08_g + 0.072*ddARO2_C07_g + 0.059*ddARO2_C06_g + &
      0.011*ddARO2_C05_g + 0.001*ddARO2_C04_g + 0.000*ddARO2_C03_g + 0.157*ddARO2_C02_g + &
      0.163*ddARO2_C01_g + 0.000*ddARO2_C00_g + 0.000*ddARO2_CN1_g + 0.000*ddARO2_CN2_g + &
      0.000*ddARO2_CN3_g + 0.063*ddARO2

      ppllARO2_C02_g = &
      0.307*ddARO2_C09_g + 0.163*ddARO2_C08_g + 0.042*ddARO2_C07_g + 0.052*ddARO2_C06_g + &
      0.043*ddARO2_C05_g + 0.008*ddARO2_C04_g + 0.001*ddARO2_C03_g + 0.000*ddARO2_C02_g + &
      0.163*ddARO2_C01_g + 0.167*ddARO2_C00_g + 0.000*ddARO2_CN1_g + 0.000*ddARO2_CN2_g + &
      0.000*ddARO2_CN3_g + 0.114*ddARO2

      ppllARO2_C01_g = &
      0.064*ddARO2_C09_g + 0.225*ddARO2_C08_g + 0.119*ddARO2_C07_g + 0.031*ddARO2_C06_g + &
      0.038*ddARO2_C05_g + 0.032*ddARO2_C04_g + 0.006*ddARO2_C03_g + 0.001*ddARO2_C02_g + &
      0.000*ddARO2_C01_g + 0.167*ddARO2_C00_g + 0.170*ddARO2_CN1_g + 0.000*ddARO2_CN2_g + &
      0.000*ddARO2_CN3_g + 0.182*ddARO2

      ppllARO2_C00_g = &
      0.000*ddARO2_C09_g + 0.047*ddARO2_C08_g + 0.164*ddARO2_C07_g + 0.087*ddARO2_C06_g + &
      0.023*ddARO2_C05_g + 0.028*ddARO2_C04_g + 0.023*ddARO2_C03_g + 0.004*ddARO2_C02_g + &
      0.001*ddARO2_C01_g + 0.000*ddARO2_C00_g + 0.170*ddARO2_CN1_g + 0.172*ddARO2_CN2_g + &
      0.000*ddARO2_CN3_g + 0.094*ddARO2

      ppllARO2_CN1_g = &
      0.000*ddARO2_C09_g + 0.000*ddARO2_C08_g + 0.035*ddARO2_C07_g + 0.120*ddARO2_C06_g + &
      0.064*ddARO2_C05_g + 0.017*ddARO2_C04_g + 0.021*ddARO2_C03_g + 0.017*ddARO2_C02_g + &
      0.003*ddARO2_C01_g + 0.000*ddARO2_C00_g + 0.000*ddARO2_CN1_g + 0.172*ddARO2_CN2_g + &
      0.173*ddARO2_CN3_g + 0.014*ddARO2

      ppllARO2_CN2_g = &
      0.000*ddARO2_C09_g + 0.000*ddARO2_C08_g + 0.000*ddARO2_C07_g + 0.025*ddARO2_C06_g + &
      0.088*ddARO2_C05_g + 0.047*ddARO2_C04_g + 0.012*ddARO2_C03_g + 0.015*ddARO2_C02_g + &
      0.012*ddARO2_C01_g + 0.002*ddARO2_C00_g + 0.000*ddARO2_CN1_g + 0.000*ddARO2_CN2_g + &
      0.173*ddARO2_CN3_g + 0.000*ddARO2

      ppllARO2_CN3_g = &
      0.001*ddARO2_C09_g + 0.001*ddARO2_C08_g + 0.001*ddARO2_C07_g + 0.000*ddARO2_C06_g + &
      0.019*ddARO2_C05_g + 0.078*ddARO2_C04_g + 0.092*ddARO2_C03_g + 0.076*ddARO2_C02_g + &
      0.067*ddARO2_C01_g + 0.058*ddARO2_C00_g + 0.044*ddARO2_CN1_g + 0.032*ddARO2_CN2_g + &
      0.024*ddARO2_CN3_g + 0.002*ddARO2

!     SIMPLESOM SPECIES:
      ppllSESQ_C09_g = &
      0.000*ddSESQ_C09_g + 0.041*ddSESQ_C08_g + 0.078*ddSESQ_C07_g + 0.000*ddSESQ_C06_g + &
      0.000*ddSESQ_C05_g + 0.000*ddSESQ_C04_g + 0.000*ddSESQ_C03_g + 0.000*ddSESQ_C02_g + &
      0.000*ddSESQ_C01_g + 0.000*ddSESQ_C00_g + 0.000*ddSESQ_CN1_g + 0.000*ddSESQ_CN2_g + &
      0.000*ddSESQ_CN3_g + 0.000*ddSESQ

      ppllSESQ_C08_g = &
      0.673*ddSESQ_C09_g + 0.000*ddSESQ_C08_g + 0.078*ddSESQ_C07_g + 0.110*ddSESQ_C06_g + &
      0.000*ddSESQ_C05_g + 0.000*ddSESQ_C04_g + 0.000*ddSESQ_C03_g + 0.000*ddSESQ_C02_g + &
      0.000*ddSESQ_C01_g + 0.000*ddSESQ_C00_g + 0.000*ddSESQ_CN1_g + 0.000*ddSESQ_CN2_g + &
      0.000*ddSESQ_CN3_g + 0.000*ddSESQ

      ppllSESQ_C07_g = &
      0.281*ddSESQ_C09_g + 0.593*ddSESQ_C08_g + 0.000*ddSESQ_C07_g + 0.110*ddSESQ_C06_g + &
      0.138*ddSESQ_C05_g + 0.000*ddSESQ_C04_g + 0.000*ddSESQ_C03_g + 0.000*ddSESQ_C02_g + &
      0.000*ddSESQ_C01_g + 0.000*ddSESQ_C00_g + 0.000*ddSESQ_CN1_g + 0.000*ddSESQ_CN2_g + &
      0.000*ddSESQ_CN3_g + 0.014*ddSESQ

      ppllSESQ_C06_g = &
      0.021*ddSESQ_C09_g + 0.248*ddSESQ_C08_g + 0.522*ddSESQ_C07_g + 0.000*ddSESQ_C06_g + &
      0.138*ddSESQ_C05_g + 0.163*ddSESQ_C04_g + 0.000*ddSESQ_C03_g + 0.000*ddSESQ_C02_g + &
      0.000*ddSESQ_C01_g + 0.000*ddSESQ_C00_g + 0.000*ddSESQ_CN1_g + 0.000*ddSESQ_CN2_g + &
      0.000*ddSESQ_CN3_g + 0.158*ddSESQ

      ppllSESQ_C05_g = &
      0.006*ddSESQ_C09_g + 0.019*ddSESQ_C08_g + 0.218*ddSESQ_C07_g + 0.460*ddSESQ_C06_g + &
      0.000*ddSESQ_C05_g + 0.163*ddSESQ_C04_g + 0.185*ddSESQ_C03_g + 0.000*ddSESQ_C02_g + &
      0.000*ddSESQ_C01_g + 0.000*ddSESQ_C00_g + 0.000*ddSESQ_CN1_g + 0.000*ddSESQ_CN2_g + &
      0.000*ddSESQ_CN3_g + 0.144*ddSESQ

      ppllSESQ_C04_g = &
      0.000*ddSESQ_C09_g + 0.005*ddSESQ_C08_g + 0.017*ddSESQ_C07_g + 0.192*ddSESQ_C06_g + &
      0.405*ddSESQ_C05_g + 0.000*ddSESQ_C04_g + 0.185*ddSESQ_C03_g + 0.205*ddSESQ_C02_g + &
      0.000*ddSESQ_C01_g + 0.000*ddSESQ_C00_g + 0.000*ddSESQ_CN1_g + 0.000*ddSESQ_CN2_g + &
      0.000*ddSESQ_CN3_g + 0.040*ddSESQ

      ppllSESQ_C03_g = &
      0.000*ddSESQ_C09_g + 0.000*ddSESQ_C08_g + 0.005*ddSESQ_C07_g + 0.015*ddSESQ_C06_g + &
      0.169*ddSESQ_C05_g + 0.357*ddSESQ_C04_g + 0.000*ddSESQ_C03_g + 0.205*ddSESQ_C02_g + &
      0.222*ddSESQ_C01_g + 0.000*ddSESQ_C00_g + 0.000*ddSESQ_CN1_g + 0.000*ddSESQ_CN2_g + &
      0.000*ddSESQ_CN3_g + 0.332*ddSESQ

      ppllSESQ_C02_g = &
      0.000*ddSESQ_C09_g + 0.000*ddSESQ_C08_g + 0.000*ddSESQ_C07_g + 0.004*ddSESQ_C06_g + &
      0.013*ddSESQ_C05_g + 0.149*ddSESQ_C04_g + 0.314*ddSESQ_C03_g + 0.000*ddSESQ_C02_g + &
      0.222*ddSESQ_C01_g + 0.237*ddSESQ_C00_g + 0.000*ddSESQ_CN1_g + 0.000*ddSESQ_CN2_g + &
      0.000*ddSESQ_CN3_g + 0.133*ddSESQ

      ppllSESQ_C01_g = &
      0.000*ddSESQ_C09_g + 0.000*ddSESQ_C08_g + 0.000*ddSESQ_C07_g + 0.000*ddSESQ_C06_g + &
      0.004*ddSESQ_C05_g + 0.011*ddSESQ_C04_g + 0.131*ddSESQ_C03_g + 0.277*ddSESQ_C02_g + &
      0.000*ddSESQ_C01_g + 0.237*ddSESQ_C00_g + 0.250*ddSESQ_CN1_g + 0.000*ddSESQ_CN2_g + &
      0.000*ddSESQ_CN3_g + 0.010*ddSESQ

      ppllSESQ_C00_g = &
      0.000*ddSESQ_C09_g + 0.000*ddSESQ_C08_g + 0.000*ddSESQ_C07_g + 0.000*ddSESQ_C06_g + &
      0.000*ddSESQ_C05_g + 0.003*ddSESQ_C04_g + 0.010*ddSESQ_C03_g + 0.116*ddSESQ_C02_g + &
      0.244*ddSESQ_C01_g + 0.000*ddSESQ_C00_g + 0.250*ddSESQ_CN1_g + 0.262*ddSESQ_CN2_g + &
      0.000*ddSESQ_CN3_g + 0.003*ddSESQ

      ppllSESQ_CN1_g = &
      0.000*ddSESQ_C09_g + 0.000*ddSESQ_C08_g + 0.000*ddSESQ_C07_g + 0.000*ddSESQ_C06_g + &
      0.000*ddSESQ_C05_g + 0.000*ddSESQ_C04_g + 0.003*ddSESQ_C03_g + 0.009*ddSESQ_C02_g + &
      0.102*ddSESQ_C01_g + 0.215*ddSESQ_C00_g + 0.000*ddSESQ_CN1_g + 0.262*ddSESQ_CN2_g + &
      0.272*ddSESQ_CN3_g + 0.000*ddSESQ

      ppllSESQ_CN2_g = &
      0.000*ddSESQ_C09_g + 0.000*ddSESQ_C08_g + 0.000*ddSESQ_C07_g + 0.000*ddSESQ_C06_g + &
      0.000*ddSESQ_C05_g + 0.000*ddSESQ_C04_g + 0.000*ddSESQ_C03_g + 0.003*ddSESQ_C02_g + &
      0.008*ddSESQ_C01_g + 0.090*ddSESQ_C00_g + 0.189*ddSESQ_CN1_g + 0.000*ddSESQ_CN2_g + &
      0.272*ddSESQ_CN3_g + 0.000*ddSESQ

      ppllSESQ_CN3_g = &
      0.018*ddSESQ_C09_g + 0.016*ddSESQ_C08_g + 0.014*ddSESQ_C07_g + 0.012*ddSESQ_C06_g + &
      0.011*ddSESQ_C05_g + 0.010*ddSESQ_C04_g + 0.008*ddSESQ_C03_g + 0.007*ddSESQ_C02_g + &
      0.009*ddSESQ_C01_g + 0.015*ddSESQ_C00_g + 0.092*ddSESQ_CN1_g + 0.247*ddSESQ_CN2_g + &
      0.218*ddSESQ_CN3_g + 0.027*ddSESQ

!     SIMPLESOM SPECIES:
      ppllISPR_C09_g = &
      0.000*ddISPR_C09_g + 0.019*ddISPR_C08_g + 0.030*ddISPR_C07_g + 0.000*ddISPR_C06_g + &
      0.000*ddISPR_C05_g + 0.000*ddISPR_C04_g + 0.000*ddISPR_C03_g + 0.000*ddISPR_C02_g + &
      0.000*ddISPR_C01_g + 0.000*ddISPR_C00_g + 0.000*ddISPR_CN1_g + 0.000*ddISPR_CN2_g + &
      0.000*ddISPR_CN3_g + 0.013*ddISPR

      ppllISPR_C08_g = &
      0.030*ddISPR_C09_g + 0.000*ddISPR_C08_g + 0.030*ddISPR_C07_g + 0.037*ddISPR_C06_g + &
      0.000*ddISPR_C05_g + 0.000*ddISPR_C04_g + 0.000*ddISPR_C03_g + 0.000*ddISPR_C02_g + &
      0.000*ddISPR_C01_g + 0.000*ddISPR_C00_g + 0.000*ddISPR_CN1_g + 0.000*ddISPR_CN2_g + &
      0.000*ddISPR_CN3_g + 0.009*ddISPR

      ppllISPR_C07_g = &
      0.136*ddISPR_C09_g + 0.018*ddISPR_C08_g + 0.000*ddISPR_C07_g + 0.037*ddISPR_C06_g + &
      0.041*ddISPR_C05_g + 0.000*ddISPR_C04_g + 0.000*ddISPR_C03_g + 0.000*ddISPR_C02_g + &
      0.000*ddISPR_C01_g + 0.000*ddISPR_C00_g + 0.000*ddISPR_CN1_g + 0.000*ddISPR_CN2_g + &
      0.000*ddISPR_CN3_g + 0.054*ddISPR

      ppllISPR_C06_g = &
      0.061*ddISPR_C09_g + 0.083*ddISPR_C08_g + 0.011*ddISPR_C07_g + 0.000*ddISPR_C06_g + &
      0.041*ddISPR_C05_g + 0.044*ddISPR_C04_g + 0.000*ddISPR_C03_g + 0.000*ddISPR_C02_g + &
      0.000*ddISPR_C01_g + 0.000*ddISPR_C00_g + 0.000*ddISPR_CN1_g + 0.000*ddISPR_CN2_g + &
      0.000*ddISPR_CN3_g + 0.076*ddISPR

      ppllISPR_C05_g = &
      0.454*ddISPR_C09_g + 0.037*ddISPR_C08_g + 0.050*ddISPR_C07_g + 0.007*ddISPR_C06_g + &
      0.000*ddISPR_C05_g + 0.044*ddISPR_C04_g + 0.045*ddISPR_C03_g + 0.000*ddISPR_C02_g + &
      0.000*ddISPR_C01_g + 0.000*ddISPR_C00_g + 0.000*ddISPR_CN1_g + 0.000*ddISPR_CN2_g + &
      0.000*ddISPR_CN3_g + 0.162*ddISPR

      ppllISPR_C04_g = &
      0.023*ddISPR_C09_g + 0.275*ddISPR_C08_g + 0.023*ddISPR_C07_g + 0.030*ddISPR_C06_g + &
      0.004*ddISPR_C05_g + 0.000*ddISPR_C04_g + 0.045*ddISPR_C03_g + 0.046*ddISPR_C02_g + &
      0.000*ddISPR_C01_g + 0.000*ddISPR_C00_g + 0.000*ddISPR_CN1_g + 0.000*ddISPR_CN2_g + &
      0.000*ddISPR_CN3_g + 0.199*ddISPR

      ppllISPR_C03_g = &
      0.278*ddISPR_C09_g + 0.014*ddISPR_C08_g + 0.167*ddISPR_C07_g + 0.014*ddISPR_C06_g + &
      0.018*ddISPR_C05_g + 0.002*ddISPR_C04_g + 0.000*ddISPR_C03_g + 0.046*ddISPR_C02_g + &
      0.047*ddISPR_C01_g + 0.000*ddISPR_C00_g + 0.000*ddISPR_CN1_g + 0.000*ddISPR_CN2_g + &
      0.000*ddISPR_CN3_g + 0.093*ddISPR

      ppllISPR_C02_g = &
      0.001*ddISPR_C09_g + 0.169*ddISPR_C08_g + 0.008*ddISPR_C07_g + 0.101*ddISPR_C06_g + &
      0.008*ddISPR_C05_g + 0.011*ddISPR_C04_g + 0.001*ddISPR_C03_g + 0.000*ddISPR_C02_g + &
      0.047*ddISPR_C01_g + 0.047*ddISPR_C00_g + 0.000*ddISPR_CN1_g + 0.000*ddISPR_CN2_g + &
      0.000*ddISPR_CN3_g + 0.118*ddISPR

      ppllISPR_C01_g = &
      0.017*ddISPR_C09_g + 0.001*ddISPR_C08_g + 0.102*ddISPR_C07_g + 0.005*ddISPR_C06_g + &
      0.061*ddISPR_C05_g + 0.005*ddISPR_C04_g + 0.007*ddISPR_C03_g + 0.001*ddISPR_C02_g + &
      0.000*ddISPR_C01_g + 0.047*ddISPR_C00_g + 0.047*ddISPR_CN1_g + 0.000*ddISPR_CN2_g + &
      0.000*ddISPR_CN3_g + 0.006*ddISPR

      ppllISPR_C00_g = &
      0.000*ddISPR_C09_g + 0.010*ddISPR_C08_g + 0.000*ddISPR_C07_g + 0.062*ddISPR_C06_g + &
      0.003*ddISPR_C05_g + 0.037*ddISPR_C04_g + 0.003*ddISPR_C03_g + 0.004*ddISPR_C02_g + &
      0.001*ddISPR_C01_g + 0.000*ddISPR_C00_g + 0.047*ddISPR_CN1_g + 0.047*ddISPR_CN2_g + &
      0.000*ddISPR_CN3_g + 0.007*ddISPR

      ppllISPR_CN1_g = &
      0.000*ddISPR_C09_g + 0.000*ddISPR_C08_g + 0.006*ddISPR_C07_g + 0.000*ddISPR_C06_g + &
      0.038*ddISPR_C05_g + 0.002*ddISPR_C04_g + 0.023*ddISPR_C03_g + 0.002*ddISPR_C02_g + &
      0.002*ddISPR_C01_g + 0.000*ddISPR_C00_g + 0.000*ddISPR_CN1_g + 0.047*ddISPR_CN2_g + &
      0.047*ddISPR_CN3_g + 0.000*ddISPR

      ppllISPR_CN2_g = &
      0.000*ddISPR_C09_g + 0.000*ddISPR_C08_g + 0.000*ddISPR_C07_g + 0.004*ddISPR_C06_g + &
      0.000*ddISPR_C05_g + 0.023*ddISPR_C04_g + 0.001*ddISPR_C03_g + 0.014*ddISPR_C02_g + &
      0.001*ddISPR_C01_g + 0.002*ddISPR_C00_g + 0.000*ddISPR_CN1_g + 0.000*ddISPR_CN2_g + &
      0.047*ddISPR_CN3_g + 0.000*ddISPR

      ppllISPR_CN3_g = &
      0.000*ddISPR_C09_g + 0.000*ddISPR_C08_g + 0.000*ddISPR_C07_g + 0.000*ddISPR_C06_g + &
      0.002*ddISPR_C05_g + 0.001*ddISPR_C04_g + 0.015*ddISPR_C03_g + 0.010*ddISPR_C02_g + &
      0.014*ddISPR_C01_g + 0.009*ddISPR_C00_g + 0.007*ddISPR_CN1_g + 0.004*ddISPR_CN2_g + &
      0.002*ddISPR_CN3_g + 0.000*ddISPR

!     SIMPLESOM SPECIES:
      ppllIVO1_C09_g = &
      0.000*ddIVO1_C09_g + 0.024*ddIVO1_C08_g + 0.046*ddIVO1_C07_g + 0.000*ddIVO1_C06_g + &
      0.000*ddIVO1_C05_g + 0.000*ddIVO1_C04_g + 0.000*ddIVO1_C03_g + 0.000*ddIVO1_C02_g + &
      0.000*ddIVO1_C01_g + 0.000*ddIVO1_C00_g + 0.000*ddIVO1_CN1_g + 0.000*ddIVO1_CN2_g + &
      0.000*ddIVO1_CN3_g + 0.000*ddIVO1

      ppllIVO1_C08_g = &
      0.375*ddIVO1_C09_g + 0.000*ddIVO1_C08_g + 0.046*ddIVO1_C07_g + 0.066*ddIVO1_C06_g + &
      0.000*ddIVO1_C05_g + 0.000*ddIVO1_C04_g + 0.000*ddIVO1_C03_g + 0.000*ddIVO1_C02_g + &
      0.000*ddIVO1_C01_g + 0.000*ddIVO1_C00_g + 0.000*ddIVO1_CN1_g + 0.000*ddIVO1_CN2_g + &
      0.000*ddIVO1_CN3_g + 0.020*ddIVO1

      ppllIVO1_C07_g = &
      0.620*ddIVO1_C09_g + 0.343*ddIVO1_C08_g + 0.000*ddIVO1_C07_g + 0.066*ddIVO1_C06_g + &
      0.084*ddIVO1_C05_g + 0.000*ddIVO1_C04_g + 0.000*ddIVO1_C03_g + 0.000*ddIVO1_C02_g + &
      0.000*ddIVO1_C01_g + 0.000*ddIVO1_C00_g + 0.000*ddIVO1_CN1_g + 0.000*ddIVO1_CN2_g + &
      0.000*ddIVO1_CN3_g + 0.079*ddIVO1

      ppllIVO1_C06_g = &
      0.000*ddIVO1_C09_g + 0.566*ddIVO1_C08_g + 0.313*ddIVO1_C07_g + 0.000*ddIVO1_C06_g + &
      0.084*ddIVO1_C05_g + 0.101*ddIVO1_C04_g + 0.000*ddIVO1_C03_g + 0.000*ddIVO1_C02_g + &
      0.000*ddIVO1_C01_g + 0.000*ddIVO1_C00_g + 0.000*ddIVO1_CN1_g + 0.000*ddIVO1_CN2_g + &
      0.000*ddIVO1_CN3_g + 0.059*ddIVO1

      ppllIVO1_C05_g = &
      0.001*ddIVO1_C09_g + 0.000*ddIVO1_C08_g + 0.517*ddIVO1_C07_g + 0.286*ddIVO1_C06_g + &
      0.000*ddIVO1_C05_g + 0.101*ddIVO1_C04_g + 0.116*ddIVO1_C03_g + 0.000*ddIVO1_C02_g + &
      0.000*ddIVO1_C01_g + 0.000*ddIVO1_C00_g + 0.000*ddIVO1_CN1_g + 0.000*ddIVO1_CN2_g + &
      0.000*ddIVO1_CN3_g + 0.086*ddIVO1

      ppllIVO1_C04_g = &
      0.001*ddIVO1_C09_g + 0.001*ddIVO1_C08_g + 0.000*ddIVO1_C07_g + 0.473*ddIVO1_C06_g + &
      0.261*ddIVO1_C05_g + 0.000*ddIVO1_C04_g + 0.116*ddIVO1_C03_g + 0.130*ddIVO1_C02_g + &
      0.000*ddIVO1_C01_g + 0.000*ddIVO1_C00_g + 0.000*ddIVO1_CN1_g + 0.000*ddIVO1_CN2_g + &
      0.000*ddIVO1_CN3_g + 0.325*ddIVO1

      ppllIVO1_C03_g = &
      0.001*ddIVO1_C09_g + 0.001*ddIVO1_C08_g + 0.001*ddIVO1_C07_g + 0.000*ddIVO1_C06_g + &
      0.432*ddIVO1_C05_g + 0.239*ddIVO1_C04_g + 0.000*ddIVO1_C03_g + 0.130*ddIVO1_C02_g + &
      0.143*ddIVO1_C01_g + 0.000*ddIVO1_C00_g + 0.000*ddIVO1_CN1_g + 0.000*ddIVO1_CN2_g + &
      0.000*ddIVO1_CN3_g + 0.302*ddIVO1

      ppllIVO1_C02_g = &
      0.001*ddIVO1_C09_g + 0.001*ddIVO1_C08_g + 0.001*ddIVO1_C07_g + 0.001*ddIVO1_C06_g + &
      0.000*ddIVO1_C05_g + 0.394*ddIVO1_C04_g + 0.218*ddIVO1_C03_g + 0.000*ddIVO1_C02_g + &
      0.143*ddIVO1_C01_g + 0.155*ddIVO1_C00_g + 0.000*ddIVO1_CN1_g + 0.000*ddIVO1_CN2_g + &
      0.000*ddIVO1_CN3_g + 0.000*ddIVO1

      ppllIVO1_C01_g = &
      0.000*ddIVO1_C09_g + 0.001*ddIVO1_C08_g + 0.001*ddIVO1_C07_g + 0.001*ddIVO1_C06_g + &
      0.000*ddIVO1_C05_g + 0.000*ddIVO1_C04_g + 0.360*ddIVO1_C03_g + 0.199*ddIVO1_C02_g + &
      0.000*ddIVO1_C01_g + 0.155*ddIVO1_C00_g + 0.165*ddIVO1_CN1_g + 0.000*ddIVO1_CN2_g + &
      0.000*ddIVO1_CN3_g + 0.001*ddIVO1

      ppllIVO1_C00_g = &
      0.000*ddIVO1_C09_g + 0.000*ddIVO1_C08_g + 0.001*ddIVO1_C07_g + 0.001*ddIVO1_C06_g + &
      0.001*ddIVO1_C05_g + 0.000*ddIVO1_C04_g + 0.000*ddIVO1_C03_g + 0.329*ddIVO1_C02_g + &
      0.182*ddIVO1_C01_g + 0.000*ddIVO1_C00_g + 0.165*ddIVO1_CN1_g + 0.175*ddIVO1_CN2_g + &
      0.000*ddIVO1_CN3_g + 0.001*ddIVO1

      ppllIVO1_CN1_g = &
      0.000*ddIVO1_C09_g + 0.000*ddIVO1_C08_g + 0.000*ddIVO1_C07_g + 0.001*ddIVO1_C06_g + &
      0.001*ddIVO1_C05_g + 0.001*ddIVO1_C04_g + 0.000*ddIVO1_C03_g + 0.000*ddIVO1_C02_g + &
      0.301*ddIVO1_C01_g + 0.166*ddIVO1_C00_g + 0.000*ddIVO1_CN1_g + 0.175*ddIVO1_CN2_g + &
      0.184*ddIVO1_CN3_g + 0.001*ddIVO1

      ppllIVO1_CN2_g = &
      0.000*ddIVO1_C09_g + 0.000*ddIVO1_C08_g + 0.000*ddIVO1_C07_g + 0.000*ddIVO1_C06_g + &
      0.001*ddIVO1_C05_g + 0.001*ddIVO1_C04_g + 0.001*ddIVO1_C03_g + 0.000*ddIVO1_C02_g + &
      0.000*ddIVO1_C01_g + 0.275*ddIVO1_C00_g + 0.152*ddIVO1_CN1_g + 0.000*ddIVO1_CN2_g + &
      0.184*ddIVO1_CN3_g + 0.001*ddIVO1

      ppllIVO1_CN3_g = &
      0.000*ddIVO1_C09_g + 0.000*ddIVO1_C08_g + 0.000*ddIVO1_C07_g + 0.000*ddIVO1_C06_g + &
      0.000*ddIVO1_C05_g + 0.001*ddIVO1_C04_g + 0.002*ddIVO1_C03_g + 0.002*ddIVO1_C02_g + &
      0.002*ddIVO1_C01_g + 0.002*ddIVO1_C00_g + 0.253*ddIVO1_CN1_g + 0.370*ddIVO1_CN2_g + &
      0.338*ddIVO1_CN3_g + 0.000*ddIVO1

!     CALCULATE PRODUCED AMOUNTS FOR HIGH-NOx:
!     =======================================================================
!     SIMPLESOM SPECIES:
      pphhTERP_C09_g = &
      0.000*ddTERP_C09_g + 0.172*ddTERP_C08_g + 0.284*ddTERP_C07_g + 0.000*ddTERP_C06_g + &
      0.000*ddTERP_C05_g + 0.000*ddTERP_C04_g + 0.000*ddTERP_C03_g + 0.000*ddTERP_C02_g + &
      0.000*ddTERP_C01_g + 0.000*ddTERP_C00_g + 0.000*ddTERP_CN1_g + 0.000*ddTERP_CN2_g + &
      0.000*ddTERP_CN3_g + 0.114*ddTERP

      pphhTERP_C08_g = &
      0.221*ddTERP_C09_g + 0.000*ddTERP_C08_g + 0.284*ddTERP_C07_g + 0.357*ddTERP_C06_g + &
      0.000*ddTERP_C05_g + 0.000*ddTERP_C04_g + 0.000*ddTERP_C03_g + 0.000*ddTERP_C02_g + &
      0.000*ddTERP_C01_g + 0.000*ddTERP_C00_g + 0.000*ddTERP_CN1_g + 0.000*ddTERP_CN2_g + &
      0.000*ddTERP_CN3_g + 0.328*ddTERP

      pphhTERP_C07_g = &
      0.272*ddTERP_C09_g + 0.143*ddTERP_C08_g + 0.000*ddTERP_C07_g + 0.357*ddTERP_C06_g + &
      0.404*ddTERP_C05_g + 0.000*ddTERP_C04_g + 0.000*ddTERP_C03_g + 0.000*ddTERP_C02_g + &
      0.000*ddTERP_C01_g + 0.000*ddTERP_C00_g + 0.000*ddTERP_CN1_g + 0.000*ddTERP_CN2_g + &
      0.000*ddTERP_CN3_g + 0.214*ddTERP

      pphhTERP_C06_g = &
      0.138*ddTERP_C09_g + 0.177*ddTERP_C08_g + 0.093*ddTERP_C07_g + 0.000*ddTERP_C06_g + &
      0.404*ddTERP_C05_g + 0.435*ddTERP_C04_g + 0.000*ddTERP_C03_g + 0.000*ddTERP_C02_g + &
      0.000*ddTERP_C01_g + 0.000*ddTERP_C00_g + 0.000*ddTERP_CN1_g + 0.000*ddTERP_CN2_g + &
      0.000*ddTERP_CN3_g + 0.037*ddTERP

      pphhTERP_C05_g = &
      0.064*ddTERP_C09_g + 0.090*ddTERP_C08_g + 0.115*ddTERP_C07_g + 0.060*ddTERP_C06_g + &
      0.000*ddTERP_C05_g + 0.435*ddTERP_C04_g + 0.455*ddTERP_C03_g + 0.000*ddTERP_C02_g + &
      0.000*ddTERP_C01_g + 0.000*ddTERP_C00_g + 0.000*ddTERP_CN1_g + 0.000*ddTERP_CN2_g + &
      0.000*ddTERP_CN3_g + 0.082*ddTERP

      pphhTERP_C04_g = &
      0.070*ddTERP_C09_g + 0.041*ddTERP_C08_g + 0.058*ddTERP_C07_g + 0.075*ddTERP_C06_g + &
      0.039*ddTERP_C05_g + 0.000*ddTERP_C04_g + 0.455*ddTERP_C03_g + 0.468*ddTERP_C02_g + &
      0.000*ddTERP_C01_g + 0.000*ddTERP_C00_g + 0.000*ddTERP_CN1_g + 0.000*ddTERP_CN2_g + &
      0.000*ddTERP_CN3_g + 0.068*ddTERP

      pphhTERP_C03_g = &
      0.169*ddTERP_C09_g + 0.045*ddTERP_C08_g + 0.027*ddTERP_C07_g + 0.038*ddTERP_C06_g + &
      0.048*ddTERP_C05_g + 0.026*ddTERP_C04_g + 0.000*ddTERP_C03_g + 0.468*ddTERP_C02_g + &
      0.476*ddTERP_C01_g + 0.000*ddTERP_C00_g + 0.000*ddTERP_CN1_g + 0.000*ddTERP_CN2_g + &
      0.000*ddTERP_CN3_g + 0.033*ddTERP

      pphhTERP_C02_g = &
      0.063*ddTERP_C09_g + 0.110*ddTERP_C08_g + 0.030*ddTERP_C07_g + 0.017*ddTERP_C06_g + &
      0.025*ddTERP_C05_g + 0.031*ddTERP_C04_g + 0.017*ddTERP_C03_g + 0.000*ddTERP_C02_g + &
      0.476*ddTERP_C01_g + 0.481*ddTERP_C00_g + 0.000*ddTERP_CN1_g + 0.000*ddTERP_CN2_g + &
      0.000*ddTERP_CN3_g + 0.022*ddTERP

      pphhTERP_C01_g = &
      0.003*ddTERP_C09_g + 0.041*ddTERP_C08_g + 0.071*ddTERP_C07_g + 0.019*ddTERP_C06_g + &
      0.011*ddTERP_C05_g + 0.016*ddTERP_C04_g + 0.020*ddTERP_C03_g + 0.011*ddTERP_C02_g + &
      0.000*ddTERP_C01_g + 0.481*ddTERP_C00_g + 0.485*ddTERP_CN1_g + 0.000*ddTERP_CN2_g + &
      0.000*ddTERP_CN3_g + 0.040*ddTERP

      pphhTERP_C00_g = &
      0.000*ddTERP_C09_g + 0.002*ddTERP_C08_g + 0.027*ddTERP_C07_g + 0.046*ddTERP_C06_g + &
      0.012*ddTERP_C05_g + 0.007*ddTERP_C04_g + 0.010*ddTERP_C03_g + 0.013*ddTERP_C02_g + &
      0.007*ddTERP_C01_g + 0.000*ddTERP_C00_g + 0.485*ddTERP_CN1_g + 0.487*ddTERP_CN2_g + &
      0.000*ddTERP_CN3_g + 0.038*ddTERP

      pphhTERP_CN1_g = &
      0.000*ddTERP_C09_g + 0.000*ddTERP_C08_g + 0.001*ddTERP_C07_g + 0.017*ddTERP_C06_g + &
      0.030*ddTERP_C05_g + 0.008*ddTERP_C04_g + 0.005*ddTERP_C03_g + 0.007*ddTERP_C02_g + &
      0.009*ddTERP_C01_g + 0.005*ddTERP_C00_g + 0.000*ddTERP_CN1_g + 0.487*ddTERP_CN2_g + &
      0.489*ddTERP_CN3_g + 0.011*ddTERP

      pphhTERP_CN2_g = &
      0.000*ddTERP_C09_g + 0.000*ddTERP_C08_g + 0.000*ddTERP_C07_g + 0.001*ddTERP_C06_g + &
      0.011*ddTERP_C05_g + 0.020*ddTERP_C04_g + 0.005*ddTERP_C03_g + 0.003*ddTERP_C02_g + &
      0.004*ddTERP_C01_g + 0.006*ddTERP_C00_g + 0.003*ddTERP_CN1_g + 0.000*ddTERP_CN2_g + &
      0.489*ddTERP_CN3_g + 0.001*ddTERP

      pphhTERP_CN3_g = &
      0.000*ddTERP_C09_g + 0.000*ddTERP_C08_g + 0.000*ddTERP_C07_g + 0.000*ddTERP_C06_g + &
      0.001*ddTERP_C05_g + 0.008*ddTERP_C04_g + 0.018*ddTERP_C03_g + 0.015*ddTERP_C02_g + &
      0.012*ddTERP_C01_g + 0.010*ddTERP_C00_g + 0.010*ddTERP_CN1_g + 0.009*ddTERP_CN2_g + &
      0.006*ddTERP_CN3_g + 0.000*ddTERP

!     SIMPLESOM SPECIES:
      pphhARO1_C09_g = &
      0.000*ddARO1_C09_g + 0.010*ddARO1_C08_g + 0.019*ddARO1_C07_g + 0.000*ddARO1_C06_g + &
      0.000*ddARO1_C05_g + 0.000*ddARO1_C04_g + 0.000*ddARO1_C03_g + 0.000*ddARO1_C02_g + &
      0.000*ddARO1_C01_g + 0.000*ddARO1_C00_g + 0.000*ddARO1_CN1_g + 0.000*ddARO1_CN2_g + &
      0.000*ddARO1_CN3_g + 0.018*ddARO1

      pphhARO1_C08_g = &
      0.441*ddARO1_C09_g + 0.000*ddARO1_C08_g + 0.019*ddARO1_C07_g + 0.026*ddARO1_C06_g + &
      0.000*ddARO1_C05_g + 0.000*ddARO1_C04_g + 0.000*ddARO1_C03_g + 0.000*ddARO1_C02_g + &
      0.000*ddARO1_C01_g + 0.000*ddARO1_C00_g + 0.000*ddARO1_CN1_g + 0.000*ddARO1_CN2_g + &
      0.000*ddARO1_CN3_g + 0.017*ddARO1

      pphhARO1_C07_g = &
      0.406*ddARO1_C09_g + 0.357*ddARO1_C08_g + 0.000*ddARO1_C07_g + 0.026*ddARO1_C06_g + &
      0.031*ddARO1_C05_g + 0.000*ddARO1_C04_g + 0.000*ddARO1_C03_g + 0.000*ddARO1_C02_g + &
      0.000*ddARO1_C01_g + 0.000*ddARO1_C00_g + 0.000*ddARO1_CN1_g + 0.000*ddARO1_CN2_g + &
      0.000*ddARO1_CN3_g + 0.036*ddARO1

      pphhARO1_C06_g = &
      0.002*ddARO1_C09_g + 0.328*ddARO1_C08_g + 0.288*ddARO1_C07_g + 0.000*ddARO1_C06_g + &
      0.031*ddARO1_C05_g + 0.036*ddARO1_C04_g + 0.000*ddARO1_C03_g + 0.000*ddARO1_C02_g + &
      0.000*ddARO1_C01_g + 0.000*ddARO1_C00_g + 0.000*ddARO1_CN1_g + 0.000*ddARO1_CN2_g + &
      0.000*ddARO1_CN3_g + 0.292*ddARO1

      pphhARO1_C05_g = &
      0.005*ddARO1_C09_g + 0.002*ddARO1_C08_g + 0.265*ddARO1_C07_g + 0.233*ddARO1_C06_g + &
      0.000*ddARO1_C05_g + 0.036*ddARO1_C04_g + 0.039*ddARO1_C03_g + 0.000*ddARO1_C02_g + &
      0.000*ddARO1_C01_g + 0.000*ddARO1_C00_g + 0.000*ddARO1_CN1_g + 0.000*ddARO1_CN2_g + &
      0.000*ddARO1_CN3_g + 0.239*ddARO1

      pphhARO1_C04_g = &
      0.043*ddARO1_C09_g + 0.004*ddARO1_C08_g + 0.001*ddARO1_C07_g + 0.214*ddARO1_C06_g + &
      0.188*ddARO1_C05_g + 0.000*ddARO1_C04_g + 0.039*ddARO1_C03_g + 0.042*ddARO1_C02_g + &
      0.000*ddARO1_C01_g + 0.000*ddARO1_C00_g + 0.000*ddARO1_CN1_g + 0.000*ddARO1_CN2_g + &
      0.000*ddARO1_CN3_g + 0.002*ddARO1

      pphhARO1_C03_g = &
      0.081*ddARO1_C09_g + 0.035*ddARO1_C08_g + 0.003*ddARO1_C07_g + 0.001*ddARO1_C06_g + &
      0.173*ddARO1_C05_g + 0.152*ddARO1_C04_g + 0.000*ddARO1_C03_g + 0.042*ddARO1_C02_g + &
      0.045*ddARO1_C01_g + 0.000*ddARO1_C00_g + 0.000*ddARO1_CN1_g + 0.000*ddARO1_CN2_g + &
      0.000*ddARO1_CN3_g + 0.006*ddARO1

      pphhARO1_C02_g = &
      0.021*ddARO1_C09_g + 0.066*ddARO1_C08_g + 0.028*ddARO1_C07_g + 0.003*ddARO1_C06_g + &
      0.001*ddARO1_C05_g + 0.140*ddARO1_C04_g + 0.123*ddARO1_C03_g + 0.000*ddARO1_C02_g + &
      0.045*ddARO1_C01_g + 0.046*ddARO1_C00_g + 0.000*ddARO1_CN1_g + 0.000*ddARO1_CN2_g + &
      0.000*ddARO1_CN3_g + 0.032*ddARO1

      pphhARO1_C01_g = &
      0.001*ddARO1_C09_g + 0.017*ddARO1_C08_g + 0.053*ddARO1_C07_g + 0.023*ddARO1_C06_g + &
      0.002*ddARO1_C05_g + 0.001*ddARO1_C04_g + 0.113*ddARO1_C03_g + 0.099*ddARO1_C02_g + &
      0.000*ddARO1_C01_g + 0.046*ddARO1_C00_g + 0.048*ddARO1_CN1_g + 0.000*ddARO1_CN2_g + &
      0.000*ddARO1_CN3_g + 0.049*ddARO1

      pphhARO1_C00_g = &
      0.000*ddARO1_C09_g + 0.001*ddARO1_C08_g + 0.014*ddARO1_C07_g + 0.043*ddARO1_C06_g + &
      0.018*ddARO1_C05_g + 0.002*ddARO1_C04_g + 0.001*ddARO1_C03_g + 0.091*ddARO1_C02_g + &
      0.080*ddARO1_C01_g + 0.000*ddARO1_C00_g + 0.048*ddARO1_CN1_g + 0.049*ddARO1_CN2_g + &
      0.000*ddARO1_CN3_g + 0.012*ddARO1

      pphhARO1_CN1_g = &
      0.000*ddARO1_C09_g + 0.000*ddARO1_C08_g + 0.000*ddARO1_C07_g + 0.011*ddARO1_C06_g + &
      0.035*ddARO1_C05_g + 0.015*ddARO1_C04_g + 0.001*ddARO1_C03_g + 0.000*ddARO1_C02_g + &
      0.074*ddARO1_C01_g + 0.065*ddARO1_C00_g + 0.000*ddARO1_CN1_g + 0.049*ddARO1_CN2_g + &
      0.050*ddARO1_CN3_g + 0.000*ddARO1

      pphhARO1_CN2_g = &
      0.000*ddARO1_C09_g + 0.000*ddARO1_C08_g + 0.000*ddARO1_C07_g + 0.000*ddARO1_C06_g + &
      0.009*ddARO1_C05_g + 0.028*ddARO1_C04_g + 0.012*ddARO1_C03_g + 0.001*ddARO1_C02_g + &
      0.000*ddARO1_C01_g + 0.060*ddARO1_C00_g + 0.052*ddARO1_CN1_g + 0.000*ddARO1_CN2_g + &
      0.050*ddARO1_CN3_g + 0.000*ddARO1

      pphhARO1_CN3_g = &
      0.000*ddARO1_C09_g + 0.000*ddARO1_C08_g + 0.000*ddARO1_C07_g + 0.000*ddARO1_C06_g + &
      0.000*ddARO1_C05_g + 0.008*ddARO1_C04_g + 0.029*ddARO1_C03_g + 0.033*ddARO1_C02_g + &
      0.028*ddARO1_C01_g + 0.023*ddARO1_C00_g + 0.066*ddARO1_CN1_g + 0.096*ddARO1_CN2_g + &
      0.078*ddARO1_CN3_g + 0.000*ddARO1

!     SIMPLESOM SPECIES:
      pphhARO2_C09_g = &
      0.000*ddARO2_C09_g + 0.048*ddARO2_C08_g + 0.077*ddARO2_C07_g + 0.000*ddARO2_C06_g + &
      0.000*ddARO2_C05_g + 0.000*ddARO2_C04_g + 0.000*ddARO2_C03_g + 0.000*ddARO2_C02_g + &
      0.000*ddARO2_C01_g + 0.000*ddARO2_C00_g + 0.000*ddARO2_CN1_g + 0.000*ddARO2_CN2_g + &
      0.000*ddARO2_CN3_g + 0.059*ddARO2

      pphhARO2_C08_g = &
      0.041*ddARO2_C09_g + 0.000*ddARO2_C08_g + 0.077*ddARO2_C07_g + 0.095*ddARO2_C06_g + &
      0.000*ddARO2_C05_g + 0.000*ddARO2_C04_g + 0.000*ddARO2_C03_g + 0.000*ddARO2_C02_g + &
      0.000*ddARO2_C01_g + 0.000*ddARO2_C00_g + 0.000*ddARO2_CN1_g + 0.000*ddARO2_CN2_g + &
      0.000*ddARO2_CN3_g + 0.031*ddARO2

      pphhARO2_C07_g = &
      0.115*ddARO2_C09_g + 0.025*ddARO2_C08_g + 0.000*ddARO2_C07_g + 0.095*ddARO2_C06_g + &
      0.106*ddARO2_C05_g + 0.000*ddARO2_C04_g + 0.000*ddARO2_C03_g + 0.000*ddARO2_C02_g + &
      0.000*ddARO2_C01_g + 0.000*ddARO2_C00_g + 0.000*ddARO2_CN1_g + 0.000*ddARO2_CN2_g + &
      0.000*ddARO2_CN3_g + 0.015*ddARO2

      pphhARO2_C06_g = &
      0.236*ddARO2_C09_g + 0.071*ddARO2_C08_g + 0.016*ddARO2_C07_g + 0.000*ddARO2_C06_g + &
      0.106*ddARO2_C05_g + 0.113*ddARO2_C04_g + 0.000*ddARO2_C03_g + 0.000*ddARO2_C02_g + &
      0.000*ddARO2_C01_g + 0.000*ddARO2_C00_g + 0.000*ddARO2_CN1_g + 0.000*ddARO2_CN2_g + &
      0.000*ddARO2_CN3_g + 0.049*ddARO2

      pphhARO2_C05_g = &
      0.259*ddARO2_C09_g + 0.145*ddARO2_C08_g + 0.044*ddARO2_C07_g + 0.010*ddARO2_C06_g + &
      0.000*ddARO2_C05_g + 0.113*ddARO2_C04_g + 0.117*ddARO2_C03_g + 0.000*ddARO2_C02_g + &
      0.000*ddARO2_C01_g + 0.000*ddARO2_C00_g + 0.000*ddARO2_CN1_g + 0.000*ddARO2_CN2_g + &
      0.000*ddARO2_CN3_g + 0.104*ddARO2

      pphhARO2_C04_g = &
      0.218*ddARO2_C09_g + 0.159*ddARO2_C08_g + 0.089*ddARO2_C07_g + 0.027*ddARO2_C06_g + &
      0.006*ddARO2_C05_g + 0.000*ddARO2_C04_g + 0.117*ddARO2_C03_g + 0.119*ddARO2_C02_g + &
      0.000*ddARO2_C01_g + 0.000*ddARO2_C00_g + 0.000*ddARO2_CN1_g + 0.000*ddARO2_CN2_g + &
      0.000*ddARO2_CN3_g + 0.131*ddARO2

      pphhARO2_C03_g = &
      0.098*ddARO2_C09_g + 0.134*ddARO2_C08_g + 0.098*ddARO2_C07_g + 0.055*ddARO2_C06_g + &
      0.017*ddARO2_C05_g + 0.004*ddARO2_C04_g + 0.000*ddARO2_C03_g + 0.119*ddARO2_C02_g + &
      0.121*ddARO2_C01_g + 0.000*ddARO2_C00_g + 0.000*ddARO2_CN1_g + 0.000*ddARO2_CN2_g + &
      0.000*ddARO2_CN3_g + 0.120*ddARO2

      pphhARO2_C02_g = &
      0.030*ddARO2_C09_g + 0.060*ddARO2_C08_g + 0.083*ddARO2_C07_g + 0.060*ddARO2_C06_g + &
      0.034*ddARO2_C05_g + 0.010*ddARO2_C04_g + 0.002*ddARO2_C03_g + 0.000*ddARO2_C02_g + &
      0.121*ddARO2_C01_g + 0.122*ddARO2_C00_g + 0.000*ddARO2_CN1_g + 0.000*ddARO2_CN2_g + &
      0.000*ddARO2_CN3_g + 0.069*ddARO2

      pphhARO2_C01_g = &
      0.002*ddARO2_C09_g + 0.019*ddARO2_C08_g + 0.037*ddARO2_C07_g + 0.051*ddARO2_C06_g + &
      0.037*ddARO2_C05_g + 0.021*ddARO2_C04_g + 0.006*ddARO2_C03_g + 0.001*ddARO2_C02_g + &
      0.000*ddARO2_C01_g + 0.122*ddARO2_C00_g + 0.123*ddARO2_CN1_g + 0.000*ddARO2_CN2_g + &
      0.000*ddARO2_CN3_g + 0.026*ddARO2

      pphhARO2_C00_g = &
      0.000*ddARO2_C09_g + 0.001*ddARO2_C08_g + 0.011*ddARO2_C07_g + 0.023*ddARO2_C06_g + &
      0.031*ddARO2_C05_g + 0.023*ddARO2_C04_g + 0.013*ddARO2_C03_g + 0.004*ddARO2_C02_g + &
      0.001*ddARO2_C01_g + 0.000*ddARO2_C00_g + 0.123*ddARO2_CN1_g + 0.123*ddARO2_CN2_g + &
      0.000*ddARO2_CN3_g + 0.005*ddARO2

      pphhARO2_CN1_g = &
      0.000*ddARO2_C09_g + 0.000*ddARO2_C08_g + 0.001*ddARO2_C07_g + 0.007*ddARO2_C06_g + &
      0.014*ddARO2_C05_g + 0.019*ddARO2_C04_g + 0.014*ddARO2_C03_g + 0.008*ddARO2_C02_g + &
      0.002*ddARO2_C01_g + 0.001*ddARO2_C00_g + 0.000*ddARO2_CN1_g + 0.123*ddARO2_CN2_g + &
      0.123*ddARO2_CN3_g + 0.000*ddARO2

      pphhARO2_CN2_g = &
      0.000*ddARO2_C09_g + 0.000*ddARO2_C08_g + 0.000*ddARO2_C07_g + 0.000*ddARO2_C06_g + &
      0.004*ddARO2_C05_g + 0.009*ddARO2_C04_g + 0.012*ddARO2_C03_g + 0.009*ddARO2_C02_g + &
      0.005*ddARO2_C01_g + 0.001*ddARO2_C00_g + 0.000*ddARO2_CN1_g + 0.000*ddARO2_CN2_g + &
      0.123*ddARO2_CN3_g + 0.000*ddARO2

      pphhARO2_CN3_g = &
      0.000*ddARO2_C09_g + 0.000*ddARO2_C08_g + 0.000*ddARO2_C07_g + 0.000*ddARO2_C06_g + &
      0.000*ddARO2_C05_g + 0.003*ddARO2_C04_g + 0.007*ddARO2_C03_g + 0.012*ddARO2_C02_g + &
      0.012*ddARO2_C01_g + 0.011*ddARO2_C00_g + 0.007*ddARO2_CN1_g + 0.005*ddARO2_CN2_g + &
      0.003*ddARO2_CN3_g + 0.000*ddARO2

!     SIMPLESOM SPECIES:
      pphhSESQ_C09_g = &
      0.000*ddSESQ_C09_g + 0.031*ddSESQ_C08_g + 0.060*ddSESQ_C07_g + 0.000*ddSESQ_C06_g + &
      0.000*ddSESQ_C05_g + 0.000*ddSESQ_C04_g + 0.000*ddSESQ_C03_g + 0.000*ddSESQ_C02_g + &
      0.000*ddSESQ_C01_g + 0.000*ddSESQ_C00_g + 0.000*ddSESQ_CN1_g + 0.000*ddSESQ_CN2_g + &
      0.000*ddSESQ_CN3_g + 0.000*ddSESQ

      pphhSESQ_C08_g = &
      0.237*ddSESQ_C09_g + 0.000*ddSESQ_C08_g + 0.060*ddSESQ_C07_g + 0.086*ddSESQ_C06_g + &
      0.000*ddSESQ_C05_g + 0.000*ddSESQ_C04_g + 0.000*ddSESQ_C03_g + 0.000*ddSESQ_C02_g + &
      0.000*ddSESQ_C01_g + 0.000*ddSESQ_C00_g + 0.000*ddSESQ_CN1_g + 0.000*ddSESQ_CN2_g + &
      0.000*ddSESQ_CN3_g + 0.000*ddSESQ

      pphhSESQ_C07_g = &
      0.480*ddSESQ_C09_g + 0.215*ddSESQ_C08_g + 0.000*ddSESQ_C07_g + 0.086*ddSESQ_C06_g + &
      0.110*ddSESQ_C05_g + 0.000*ddSESQ_C04_g + 0.000*ddSESQ_C03_g + 0.000*ddSESQ_C02_g + &
      0.000*ddSESQ_C01_g + 0.000*ddSESQ_C00_g + 0.000*ddSESQ_CN1_g + 0.000*ddSESQ_CN2_g + &
      0.000*ddSESQ_CN3_g + 0.011*ddSESQ

      pphhSESQ_C06_g = &
      0.034*ddSESQ_C09_g + 0.437*ddSESQ_C08_g + 0.195*ddSESQ_C07_g + 0.000*ddSESQ_C06_g + &
      0.110*ddSESQ_C05_g + 0.131*ddSESQ_C04_g + 0.000*ddSESQ_C03_g + 0.000*ddSESQ_C02_g + &
      0.000*ddSESQ_C01_g + 0.000*ddSESQ_C00_g + 0.000*ddSESQ_CN1_g + 0.000*ddSESQ_CN2_g + &
      0.000*ddSESQ_CN3_g + 0.129*ddSESQ

      pphhSESQ_C05_g = &
      0.056*ddSESQ_C09_g + 0.031*ddSESQ_C08_g + 0.397*ddSESQ_C07_g + 0.177*ddSESQ_C06_g + &
      0.000*ddSESQ_C05_g + 0.131*ddSESQ_C04_g + 0.151*ddSESQ_C03_g + 0.000*ddSESQ_C02_g + &
      0.000*ddSESQ_C01_g + 0.000*ddSESQ_C00_g + 0.000*ddSESQ_CN1_g + 0.000*ddSESQ_CN2_g + &
      0.000*ddSESQ_CN3_g + 0.118*ddSESQ

      pphhSESQ_C04_g = &
      0.049*ddSESQ_C09_g + 0.051*ddSESQ_C08_g + 0.028*ddSESQ_C07_g + 0.360*ddSESQ_C06_g + &
      0.161*ddSESQ_C05_g + 0.000*ddSESQ_C04_g + 0.151*ddSESQ_C03_g + 0.168*ddSESQ_C02_g + &
      0.000*ddSESQ_C01_g + 0.000*ddSESQ_C00_g + 0.000*ddSESQ_CN1_g + 0.000*ddSESQ_CN2_g + &
      0.000*ddSESQ_CN3_g + 0.016*ddSESQ

      pphhSESQ_C03_g = &
      0.062*ddSESQ_C09_g + 0.045*ddSESQ_C08_g + 0.047*ddSESQ_C07_g + 0.025*ddSESQ_C06_g + &
      0.327*ddSESQ_C05_g + 0.146*ddSESQ_C04_g + 0.000*ddSESQ_C03_g + 0.168*ddSESQ_C02_g + &
      0.185*ddSESQ_C01_g + 0.000*ddSESQ_C00_g + 0.000*ddSESQ_CN1_g + 0.000*ddSESQ_CN2_g + &
      0.000*ddSESQ_CN3_g + 0.165*ddSESQ

      pphhSESQ_C02_g = &
      0.045*ddSESQ_C09_g + 0.056*ddSESQ_C08_g + 0.041*ddSESQ_C07_g + 0.042*ddSESQ_C06_g + &
      0.023*ddSESQ_C05_g + 0.297*ddSESQ_C04_g + 0.133*ddSESQ_C03_g + 0.000*ddSESQ_C02_g + &
      0.185*ddSESQ_C01_g + 0.199*ddSESQ_C00_g + 0.000*ddSESQ_CN1_g + 0.000*ddSESQ_CN2_g + &
      0.000*ddSESQ_CN3_g + 0.270*ddSESQ

      pphhSESQ_C01_g = &
      0.037*ddSESQ_C09_g + 0.041*ddSESQ_C08_g + 0.051*ddSESQ_C07_g + 0.037*ddSESQ_C06_g + &
      0.038*ddSESQ_C05_g + 0.021*ddSESQ_C04_g + 0.270*ddSESQ_C03_g + 0.121*ddSESQ_C02_g + &
      0.000*ddSESQ_C01_g + 0.199*ddSESQ_C00_g + 0.212*ddSESQ_CN1_g + 0.000*ddSESQ_CN2_g + &
      0.000*ddSESQ_CN3_g + 0.023*ddSESQ

      pphhSESQ_C00_g = &
      0.000*ddSESQ_C09_g + 0.033*ddSESQ_C08_g + 0.037*ddSESQ_C07_g + 0.047*ddSESQ_C06_g + &
      0.034*ddSESQ_C05_g + 0.035*ddSESQ_C04_g + 0.019*ddSESQ_C03_g + 0.246*ddSESQ_C02_g + &
      0.110*ddSESQ_C01_g + 0.000*ddSESQ_C00_g + 0.212*ddSESQ_CN1_g + 0.225*ddSESQ_CN2_g + &
      0.000*ddSESQ_CN3_g + 0.035*ddSESQ

      pphhSESQ_CN1_g = &
      0.000*ddSESQ_C09_g + 0.000*ddSESQ_C08_g + 0.030*ddSESQ_C07_g + 0.033*ddSESQ_C06_g + &
      0.042*ddSESQ_C05_g + 0.031*ddSESQ_C04_g + 0.032*ddSESQ_C03_g + 0.017*ddSESQ_C02_g + &
      0.223*ddSESQ_C01_g + 0.100*ddSESQ_C00_g + 0.000*ddSESQ_CN1_g + 0.225*ddSESQ_CN2_g + &
      0.235*ddSESQ_CN3_g + 0.032*ddSESQ

      pphhSESQ_CN2_g = &
      0.000*ddSESQ_C09_g + 0.000*ddSESQ_C08_g + 0.000*ddSESQ_C07_g + 0.028*ddSESQ_C06_g + &
      0.030*ddSESQ_C05_g + 0.038*ddSESQ_C04_g + 0.028*ddSESQ_C03_g + 0.029*ddSESQ_C02_g + &
      0.016*ddSESQ_C01_g + 0.203*ddSESQ_C00_g + 0.091*ddSESQ_CN1_g + 0.000*ddSESQ_CN2_g + &
      0.235*ddSESQ_CN3_g + 0.038*ddSESQ

      pphhSESQ_CN3_g = &
      0.000*ddSESQ_C09_g + 0.000*ddSESQ_C08_g + 0.000*ddSESQ_C07_g + 0.000*ddSESQ_C06_g + &
      0.025*ddSESQ_C05_g + 0.050*ddSESQ_C04_g + 0.081*ddSESQ_C03_g + 0.099*ddSESQ_C02_g + &
      0.116*ddSESQ_C01_g + 0.119*ddSESQ_C00_g + 0.293*ddSESQ_CN1_g + 0.348*ddSESQ_CN2_g + &
      0.316*ddSESQ_CN3_g + 0.048*ddSESQ

!     SIMPLESOM SPECIES:
      pphhISPR_C09_g = &
      0.000*ddISPR_C09_g + 0.106*ddISPR_C08_g + 0.186*ddISPR_C07_g + 0.000*ddISPR_C06_g + &
      0.000*ddISPR_C05_g + 0.000*ddISPR_C04_g + 0.000*ddISPR_C03_g + 0.000*ddISPR_C02_g + &
      0.000*ddISPR_C01_g + 0.000*ddISPR_C00_g + 0.000*ddISPR_CN1_g + 0.000*ddISPR_CN2_g + &
      0.000*ddISPR_CN3_g + 0.074*ddISPR

      pphhISPR_C08_g = &
      0.332*ddISPR_C09_g + 0.000*ddISPR_C08_g + 0.186*ddISPR_C07_g + 0.247*ddISPR_C06_g + &
      0.000*ddISPR_C05_g + 0.000*ddISPR_C04_g + 0.000*ddISPR_C03_g + 0.000*ddISPR_C02_g + &
      0.000*ddISPR_C01_g + 0.000*ddISPR_C00_g + 0.000*ddISPR_CN1_g + 0.000*ddISPR_CN2_g + &
      0.000*ddISPR_CN3_g + 0.100*ddISPR

      pphhISPR_C07_g = &
      0.585*ddISPR_C09_g + 0.252*ddISPR_C08_g + 0.000*ddISPR_C07_g + 0.247*ddISPR_C06_g + &
      0.293*ddISPR_C05_g + 0.000*ddISPR_C04_g + 0.000*ddISPR_C03_g + 0.000*ddISPR_C02_g + &
      0.000*ddISPR_C01_g + 0.000*ddISPR_C00_g + 0.000*ddISPR_CN1_g + 0.000*ddISPR_CN2_g + &
      0.000*ddISPR_CN3_g + 0.352*ddISPR

      pphhISPR_C06_g = &
      0.029*ddISPR_C09_g + 0.443*ddISPR_C08_g + 0.191*ddISPR_C07_g + 0.000*ddISPR_C06_g + &
      0.293*ddISPR_C05_g + 0.328*ddISPR_C04_g + 0.000*ddISPR_C03_g + 0.000*ddISPR_C02_g + &
      0.000*ddISPR_C01_g + 0.000*ddISPR_C00_g + 0.000*ddISPR_CN1_g + 0.000*ddISPR_CN2_g + &
      0.000*ddISPR_CN3_g + 0.319*ddISPR

      pphhISPR_C05_g = &
      0.034*ddISPR_C09_g + 0.022*ddISPR_C08_g + 0.336*ddISPR_C07_g + 0.144*ddISPR_C06_g + &
      0.000*ddISPR_C05_g + 0.328*ddISPR_C04_g + 0.354*ddISPR_C03_g + 0.000*ddISPR_C02_g + &
      0.000*ddISPR_C01_g + 0.000*ddISPR_C00_g + 0.000*ddISPR_CN1_g + 0.000*ddISPR_CN2_g + &
      0.000*ddISPR_CN3_g + 0.026*ddISPR

      pphhISPR_C04_g = &
      0.005*ddISPR_C09_g + 0.026*ddISPR_C08_g + 0.017*ddISPR_C07_g + 0.254*ddISPR_C06_g + &
      0.109*ddISPR_C05_g + 0.000*ddISPR_C04_g + 0.354*ddISPR_C03_g + 0.374*ddISPR_C02_g + &
      0.000*ddISPR_C01_g + 0.000*ddISPR_C00_g + 0.000*ddISPR_CN1_g + 0.000*ddISPR_CN2_g + &
      0.000*ddISPR_CN3_g + 0.020*ddISPR

      pphhISPR_C03_g = &
      0.005*ddISPR_C09_g + 0.004*ddISPR_C08_g + 0.020*ddISPR_C07_g + 0.013*ddISPR_C06_g + &
      0.193*ddISPR_C05_g + 0.083*ddISPR_C04_g + 0.000*ddISPR_C03_g + 0.374*ddISPR_C02_g + &
      0.390*ddISPR_C01_g + 0.000*ddISPR_C00_g + 0.000*ddISPR_CN1_g + 0.000*ddISPR_CN2_g + &
      0.000*ddISPR_CN3_g + 0.004*ddISPR

      pphhISPR_C02_g = &
      0.006*ddISPR_C09_g + 0.004*ddISPR_C08_g + 0.003*ddISPR_C07_g + 0.015*ddISPR_C06_g + &
      0.010*ddISPR_C05_g + 0.146*ddISPR_C04_g + 0.063*ddISPR_C03_g + 0.000*ddISPR_C02_g + &
      0.390*ddISPR_C01_g + 0.401*ddISPR_C00_g + 0.000*ddISPR_CN1_g + 0.000*ddISPR_CN2_g + &
      0.000*ddISPR_CN3_g + 0.005*ddISPR

      pphhISPR_C01_g = &
      0.003*ddISPR_C09_g + 0.004*ddISPR_C08_g + 0.003*ddISPR_C07_g + 0.002*ddISPR_C06_g + &
      0.011*ddISPR_C05_g + 0.007*ddISPR_C04_g + 0.111*ddISPR_C03_g + 0.048*ddISPR_C02_g + &
      0.000*ddISPR_C01_g + 0.401*ddISPR_C00_g + 0.410*ddISPR_CN1_g + 0.000*ddISPR_CN2_g + &
      0.000*ddISPR_CN3_g + 0.004*ddISPR

      pphhISPR_C00_g = &
      0.000*ddISPR_C09_g + 0.002*ddISPR_C08_g + 0.003*ddISPR_C07_g + 0.002*ddISPR_C06_g + &
      0.002*ddISPR_C05_g + 0.008*ddISPR_C04_g + 0.006*ddISPR_C03_g + 0.084*ddISPR_C02_g + &
      0.036*ddISPR_C01_g + 0.000*ddISPR_C00_g + 0.410*ddISPR_CN1_g + 0.416*ddISPR_CN2_g + &
      0.000*ddISPR_CN3_g + 0.001*ddISPR

      pphhISPR_CN1_g = &
      0.000*ddISPR_C09_g + 0.000*ddISPR_C08_g + 0.002*ddISPR_C07_g + 0.002*ddISPR_C06_g + &
      0.002*ddISPR_C05_g + 0.001*ddISPR_C04_g + 0.006*ddISPR_C03_g + 0.004*ddISPR_C02_g + &
      0.063*ddISPR_C01_g + 0.027*ddISPR_C00_g + 0.000*ddISPR_CN1_g + 0.416*ddISPR_CN2_g + &
      0.421*ddISPR_CN3_g + 0.000*ddISPR

      pphhISPR_CN2_g = &
      0.000*ddISPR_C09_g + 0.000*ddISPR_C08_g + 0.000*ddISPR_C07_g + 0.001*ddISPR_C06_g + &
      0.002*ddISPR_C05_g + 0.001*ddISPR_C04_g + 0.001*ddISPR_C03_g + 0.005*ddISPR_C02_g + &
      0.003*ddISPR_C01_g + 0.048*ddISPR_C00_g + 0.021*ddISPR_CN1_g + 0.000*ddISPR_CN2_g + &
      0.421*ddISPR_CN3_g + 0.000*ddISPR

      pphhISPR_CN3_g = &
      0.000*ddISPR_C09_g + 0.000*ddISPR_C08_g + 0.000*ddISPR_C07_g + 0.000*ddISPR_C06_g + &
      0.001*ddISPR_C05_g + 0.002*ddISPR_C04_g + 0.003*ddISPR_C03_g + 0.003*ddISPR_C02_g + &
      0.006*ddISPR_C01_g + 0.007*ddISPR_C00_g + 0.042*ddISPR_CN1_g + 0.047*ddISPR_CN2_g + &
      0.036*ddISPR_CN3_g + 0.000*ddISPR

!     SIMPLESOM SPECIES:
      pphhIVO1_C09_g = &
      0.000*ddIVO1_C09_g + 0.065*ddIVO1_C08_g + 0.115*ddIVO1_C07_g + 0.000*ddIVO1_C06_g + &
      0.000*ddIVO1_C05_g + 0.000*ddIVO1_C04_g + 0.000*ddIVO1_C03_g + 0.000*ddIVO1_C02_g + &
      0.000*ddIVO1_C01_g + 0.000*ddIVO1_C00_g + 0.000*ddIVO1_CN1_g + 0.000*ddIVO1_CN2_g + &
      0.000*ddIVO1_CN3_g + 0.000*ddIVO1

      pphhIVO1_C08_g = &
      0.459*ddIVO1_C09_g + 0.000*ddIVO1_C08_g + 0.115*ddIVO1_C07_g + 0.155*ddIVO1_C06_g + &
      0.000*ddIVO1_C05_g + 0.000*ddIVO1_C04_g + 0.000*ddIVO1_C03_g + 0.000*ddIVO1_C02_g + &
      0.000*ddIVO1_C01_g + 0.000*ddIVO1_C00_g + 0.000*ddIVO1_CN1_g + 0.000*ddIVO1_CN2_g + &
      0.000*ddIVO1_CN3_g + 0.047*ddIVO1

      pphhIVO1_C07_g = &
      0.434*ddIVO1_C09_g + 0.360*ddIVO1_C08_g + 0.000*ddIVO1_C07_g + 0.155*ddIVO1_C06_g + &
      0.186*ddIVO1_C05_g + 0.000*ddIVO1_C04_g + 0.000*ddIVO1_C03_g + 0.000*ddIVO1_C02_g + &
      0.000*ddIVO1_C01_g + 0.000*ddIVO1_C00_g + 0.000*ddIVO1_CN1_g + 0.000*ddIVO1_CN2_g + &
      0.000*ddIVO1_CN3_g + 0.177*ddIVO1

      pphhIVO1_C06_g = &
      0.012*ddIVO1_C09_g + 0.341*ddIVO1_C08_g + 0.283*ddIVO1_C07_g + 0.000*ddIVO1_C06_g + &
      0.186*ddIVO1_C05_g + 0.211*ddIVO1_C04_g + 0.000*ddIVO1_C03_g + 0.000*ddIVO1_C02_g + &
      0.000*ddIVO1_C01_g + 0.000*ddIVO1_C00_g + 0.000*ddIVO1_CN1_g + 0.000*ddIVO1_CN2_g + &
      0.000*ddIVO1_CN3_g + 0.130*ddIVO1

      pphhIVO1_C05_g = &
      0.024*ddIVO1_C09_g + 0.010*ddIVO1_C08_g + 0.268*ddIVO1_C07_g + 0.222*ddIVO1_C06_g + &
      0.000*ddIVO1_C05_g + 0.211*ddIVO1_C04_g + 0.230*ddIVO1_C03_g + 0.000*ddIVO1_C02_g + &
      0.000*ddIVO1_C01_g + 0.000*ddIVO1_C00_g + 0.000*ddIVO1_CN1_g + 0.000*ddIVO1_CN2_g + &
      0.000*ddIVO1_CN3_g + 0.067*ddIVO1

      pphhIVO1_C04_g = &
      0.031*ddIVO1_C09_g + 0.019*ddIVO1_C08_g + 0.008*ddIVO1_C07_g + 0.210*ddIVO1_C06_g + &
      0.174*ddIVO1_C05_g + 0.000*ddIVO1_C04_g + 0.230*ddIVO1_C03_g + 0.245*ddIVO1_C02_g + &
      0.000*ddIVO1_C01_g + 0.000*ddIVO1_C00_g + 0.000*ddIVO1_CN1_g + 0.000*ddIVO1_CN2_g + &
      0.000*ddIVO1_CN3_g + 0.185*ddIVO1

      pphhIVO1_C03_g = &
      0.032*ddIVO1_C09_g + 0.024*ddIVO1_C08_g + 0.015*ddIVO1_C07_g + 0.006*ddIVO1_C06_g + &
      0.165*ddIVO1_C05_g + 0.137*ddIVO1_C04_g + 0.000*ddIVO1_C03_g + 0.245*ddIVO1_C02_g + &
      0.257*ddIVO1_C01_g + 0.000*ddIVO1_C00_g + 0.000*ddIVO1_CN1_g + 0.000*ddIVO1_CN2_g + &
      0.000*ddIVO1_CN3_g + 0.117*ddIVO1

      pphhIVO1_C02_g = &
      0.009*ddIVO1_C09_g + 0.025*ddIVO1_C08_g + 0.019*ddIVO1_C07_g + 0.011*ddIVO1_C06_g + &
      0.005*ddIVO1_C05_g + 0.129*ddIVO1_C04_g + 0.107*ddIVO1_C03_g + 0.000*ddIVO1_C02_g + &
      0.257*ddIVO1_C01_g + 0.266*ddIVO1_C00_g + 0.000*ddIVO1_CN1_g + 0.000*ddIVO1_CN2_g + &
      0.000*ddIVO1_CN3_g + 0.007*ddIVO1

      pphhIVO1_C01_g = &
      0.000*ddIVO1_C09_g + 0.007*ddIVO1_C08_g + 0.020*ddIVO1_C07_g + 0.015*ddIVO1_C06_g + &
      0.009*ddIVO1_C05_g + 0.004*ddIVO1_C04_g + 0.102*ddIVO1_C03_g + 0.084*ddIVO1_C02_g + &
      0.000*ddIVO1_C01_g + 0.266*ddIVO1_C00_g + 0.274*ddIVO1_CN1_g + 0.000*ddIVO1_CN2_g + &
      0.000*ddIVO1_CN3_g + 0.011*ddIVO1

      pphhIVO1_C00_g = &
      0.000*ddIVO1_C09_g + 0.000*ddIVO1_C08_g + 0.005*ddIVO1_C07_g + 0.015*ddIVO1_C06_g + &
      0.012*ddIVO1_C05_g + 0.007*ddIVO1_C04_g + 0.003*ddIVO1_C03_g + 0.080*ddIVO1_C02_g + &
      0.066*ddIVO1_C01_g + 0.000*ddIVO1_C00_g + 0.274*ddIVO1_CN1_g + 0.280*ddIVO1_CN2_g + &
      0.000*ddIVO1_CN3_g + 0.013*ddIVO1

      pphhIVO1_CN1_g = &
      0.000*ddIVO1_C09_g + 0.000*ddIVO1_C08_g + 0.000*ddIVO1_C07_g + 0.004*ddIVO1_C06_g + &
      0.012*ddIVO1_C05_g + 0.009*ddIVO1_C04_g + 0.006*ddIVO1_C03_g + 0.002*ddIVO1_C02_g + &
      0.063*ddIVO1_C01_g + 0.052*ddIVO1_C00_g + 0.000*ddIVO1_CN1_g + 0.280*ddIVO1_CN2_g + &
      0.284*ddIVO1_CN3_g + 0.010*ddIVO1

      pphhIVO1_CN2_g = &
      0.000*ddIVO1_C09_g + 0.000*ddIVO1_C08_g + 0.000*ddIVO1_C07_g + 0.000*ddIVO1_C06_g + &
      0.003*ddIVO1_C05_g + 0.009*ddIVO1_C04_g + 0.007*ddIVO1_C03_g + 0.004*ddIVO1_C02_g + &
      0.002*ddIVO1_C01_g + 0.049*ddIVO1_C00_g + 0.041*ddIVO1_CN1_g + 0.000*ddIVO1_CN2_g + &
      0.284*ddIVO1_CN3_g + 0.002*ddIVO1

      pphhIVO1_CN3_g = &
      0.000*ddIVO1_C09_g + 0.000*ddIVO1_C08_g + 0.000*ddIVO1_C07_g + 0.000*ddIVO1_C06_g + &
      0.000*ddIVO1_C05_g + 0.003*ddIVO1_C04_g + 0.010*ddIVO1_C03_g + 0.013*ddIVO1_C02_g + &
      0.014*ddIVO1_C01_g + 0.012*ddIVO1_C00_g + 0.048*ddIVO1_CN1_g + 0.070*ddIVO1_CN2_g + &
      0.055*ddIVO1_CN3_g + 0.000*ddIVO1

!     SUM REACTION AND PRODUCTION:
!     =======================================================================
!     FOR SIMPLESOM SPECIES:
      CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_g) = &
      CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_g) - REAL(ddTERP_CN3_g + brLLL*ppllTERP_CN3_g + brHHH*pphhTERP_CN3_g)

      CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_g) = &
      CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_g) - REAL(ddTERP_CN2_g + brLLL*ppllTERP_CN2_g + brHHH*pphhTERP_CN2_g)

      CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_g) = &
      CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_g) - REAL(ddTERP_CN1_g + brLLL*ppllTERP_CN1_g + brHHH*pphhTERP_CN1_g)

      CHEM(iNOW,kNOW,jNOW,p_TERP_C00_g) = &
      CHEM(iNOW,kNOW,jNOW,p_TERP_C00_g) - REAL(ddTERP_C00_g + brLLL*ppllTERP_C00_g + brHHH*pphhTERP_C00_g)

      CHEM(iNOW,kNOW,jNOW,p_TERP_C01_g) = &
      CHEM(iNOW,kNOW,jNOW,p_TERP_C01_g) - REAL(ddTERP_C01_g + brLLL*ppllTERP_C01_g + brHHH*pphhTERP_C01_g)

      CHEM(iNOW,kNOW,jNOW,p_TERP_C02_g) = &
      CHEM(iNOW,kNOW,jNOW,p_TERP_C02_g) - REAL(ddTERP_C02_g + brLLL*ppllTERP_C02_g + brHHH*pphhTERP_C02_g)

      CHEM(iNOW,kNOW,jNOW,p_TERP_C03_g) = &
      CHEM(iNOW,kNOW,jNOW,p_TERP_C03_g) - REAL(ddTERP_C03_g + brLLL*ppllTERP_C03_g + brHHH*pphhTERP_C03_g)

      CHEM(iNOW,kNOW,jNOW,p_TERP_C04_g) = &
      CHEM(iNOW,kNOW,jNOW,p_TERP_C04_g) - REAL(ddTERP_C04_g + brLLL*ppllTERP_C04_g + brHHH*pphhTERP_C04_g)

      CHEM(iNOW,kNOW,jNOW,p_TERP_C05_g) = &
      CHEM(iNOW,kNOW,jNOW,p_TERP_C05_g) - REAL(ddTERP_C05_g + brLLL*ppllTERP_C05_g + brHHH*pphhTERP_C05_g)

      CHEM(iNOW,kNOW,jNOW,p_TERP_C06_g) = &
      CHEM(iNOW,kNOW,jNOW,p_TERP_C06_g) - REAL(ddTERP_C06_g + brLLL*ppllTERP_C06_g + brHHH*pphhTERP_C06_g)

      CHEM(iNOW,kNOW,jNOW,p_TERP_C07_g) = &
      CHEM(iNOW,kNOW,jNOW,p_TERP_C07_g) - REAL(ddTERP_C07_g + brLLL*ppllTERP_C07_g + brHHH*pphhTERP_C07_g)

      CHEM(iNOW,kNOW,jNOW,p_TERP_C08_g) = &
      CHEM(iNOW,kNOW,jNOW,p_TERP_C08_g) - REAL(ddTERP_C08_g + brLLL*ppllTERP_C08_g + brHHH*pphhTERP_C08_g)

      CHEM(iNOW,kNOW,jNOW,p_TERP_C09_g) = &
      CHEM(iNOW,kNOW,jNOW,p_TERP_C09_g) - REAL(ddTERP_C09_g + brLLL*ppllTERP_C09_g + brHHH*pphhTERP_C09_g)

!     FOR SIMPLESOM SPECIES:
      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_g) - REAL(ddARO1_CN3_g + brLLL*ppllARO1_CN3_g + brHHH*pphhARO1_CN3_g)

      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_g) - REAL(ddARO1_CN2_g + brLLL*ppllARO1_CN2_g + brHHH*pphhARO1_CN2_g)

      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_g) - REAL(ddARO1_CN1_g + brLLL*ppllARO1_CN1_g + brHHH*pphhARO1_CN1_g)

      CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_g) - REAL(ddARO1_C00_g + brLLL*ppllARO1_C00_g + brHHH*pphhARO1_C00_g)

      CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_g) - REAL(ddARO1_C01_g + brLLL*ppllARO1_C01_g + brHHH*pphhARO1_C01_g)

      CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_g) - REAL(ddARO1_C02_g + brLLL*ppllARO1_C02_g + brHHH*pphhARO1_C02_g)

      CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_g) - REAL(ddARO1_C03_g + brLLL*ppllARO1_C03_g + brHHH*pphhARO1_C03_g)

      CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_g) - REAL(ddARO1_C04_g + brLLL*ppllARO1_C04_g + brHHH*pphhARO1_C04_g)

      CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_g) - REAL(ddARO1_C05_g + brLLL*ppllARO1_C05_g + brHHH*pphhARO1_C05_g)

      CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_g) - REAL(ddARO1_C06_g + brLLL*ppllARO1_C06_g + brHHH*pphhARO1_C06_g)

      CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_g) - REAL(ddARO1_C07_g + brLLL*ppllARO1_C07_g + brHHH*pphhARO1_C07_g)

      CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_g) - REAL(ddARO1_C08_g + brLLL*ppllARO1_C08_g + brHHH*pphhARO1_C08_g)

      CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_g) - REAL(ddARO1_C09_g + brLLL*ppllARO1_C09_g + brHHH*pphhARO1_C09_g)

!     FOR SIMPLESOM SPECIES:
      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_g) - REAL(ddARO2_CN3_g + brLLL*ppllARO2_CN3_g + brHHH*pphhARO2_CN3_g)

      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_g) - REAL(ddARO2_CN2_g + brLLL*ppllARO2_CN2_g + brHHH*pphhARO2_CN2_g)

      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_g) - REAL(ddARO2_CN1_g + brLLL*ppllARO2_CN1_g + brHHH*pphhARO2_CN1_g)

      CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_g) - REAL(ddARO2_C00_g + brLLL*ppllARO2_C00_g + brHHH*pphhARO2_C00_g)

      CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_g) - REAL(ddARO2_C01_g + brLLL*ppllARO2_C01_g + brHHH*pphhARO2_C01_g)

      CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_g) - REAL(ddARO2_C02_g + brLLL*ppllARO2_C02_g + brHHH*pphhARO2_C02_g)

      CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_g) - REAL(ddARO2_C03_g + brLLL*ppllARO2_C03_g + brHHH*pphhARO2_C03_g)

      CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_g) - REAL(ddARO2_C04_g + brLLL*ppllARO2_C04_g + brHHH*pphhARO2_C04_g)

      CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_g) - REAL(ddARO2_C05_g + brLLL*ppllARO2_C05_g + brHHH*pphhARO2_C05_g)

      CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_g) - REAL(ddARO2_C06_g + brLLL*ppllARO2_C06_g + brHHH*pphhARO2_C06_g)

      CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_g) - REAL(ddARO2_C07_g + brLLL*ppllARO2_C07_g + brHHH*pphhARO2_C07_g)

      CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_g) - REAL(ddARO2_C08_g + brLLL*ppllARO2_C08_g + brHHH*pphhARO2_C08_g)

      CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_g) - REAL(ddARO2_C09_g + brLLL*ppllARO2_C09_g + brHHH*pphhARO2_C09_g)

!     FOR SIMPLESOM SPECIES:
      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_g) = &
      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_g) - REAL(ddSESQ_CN3_g + brLLL*ppllSESQ_CN3_g + brHHH*pphhSESQ_CN3_g)

      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_g) = &
      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_g) - REAL(ddSESQ_CN2_g + brLLL*ppllSESQ_CN2_g + brHHH*pphhSESQ_CN2_g)

      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_g) = &
      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_g) - REAL(ddSESQ_CN1_g + brLLL*ppllSESQ_CN1_g + brHHH*pphhSESQ_CN1_g)

      CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_g) = &
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_g) - REAL(ddSESQ_C00_g + brLLL*ppllSESQ_C00_g + brHHH*pphhSESQ_C00_g)

      CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_g) = &
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_g) - REAL(ddSESQ_C01_g + brLLL*ppllSESQ_C01_g + brHHH*pphhSESQ_C01_g)

      CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_g) = &
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_g) - REAL(ddSESQ_C02_g + brLLL*ppllSESQ_C02_g + brHHH*pphhSESQ_C02_g)

      CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_g) = &
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_g) - REAL(ddSESQ_C03_g + brLLL*ppllSESQ_C03_g + brHHH*pphhSESQ_C03_g)

      CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_g) = &
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_g) - REAL(ddSESQ_C04_g + brLLL*ppllSESQ_C04_g + brHHH*pphhSESQ_C04_g)

      CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_g) = &
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_g) - REAL(ddSESQ_C05_g + brLLL*ppllSESQ_C05_g + brHHH*pphhSESQ_C05_g)

      CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_g) = &
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_g) - REAL(ddSESQ_C06_g + brLLL*ppllSESQ_C06_g + brHHH*pphhSESQ_C06_g)

      CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_g) = &
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_g) - REAL(ddSESQ_C07_g + brLLL*ppllSESQ_C07_g + brHHH*pphhSESQ_C07_g)

      CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_g) = &
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_g) - REAL(ddSESQ_C08_g + brLLL*ppllSESQ_C08_g + brHHH*pphhSESQ_C08_g)

      CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_g) = &
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_g) - REAL(ddSESQ_C09_g + brLLL*ppllSESQ_C09_g + brHHH*pphhSESQ_C09_g)

!     FOR SIMPLESOM SPECIES:
      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_g) - REAL(ddISPR_CN3_g + brLLL*ppllISPR_CN3_g + brHHH*pphhISPR_CN3_g)

      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_g) - REAL(ddISPR_CN2_g + brLLL*ppllISPR_CN2_g + brHHH*pphhISPR_CN2_g)

      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_g) - REAL(ddISPR_CN1_g + brLLL*ppllISPR_CN1_g + brHHH*pphhISPR_CN1_g)

      CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_g) - REAL(ddISPR_C00_g + brLLL*ppllISPR_C00_g + brHHH*pphhISPR_C00_g)

      CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_g) - REAL(ddISPR_C01_g + brLLL*ppllISPR_C01_g + brHHH*pphhISPR_C01_g)

      CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_g) - REAL(ddISPR_C02_g + brLLL*ppllISPR_C02_g + brHHH*pphhISPR_C02_g)

      CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_g) - REAL(ddISPR_C03_g + brLLL*ppllISPR_C03_g + brHHH*pphhISPR_C03_g)

      CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_g) - REAL(ddISPR_C04_g + brLLL*ppllISPR_C04_g + brHHH*pphhISPR_C04_g)

      CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_g) - REAL(ddISPR_C05_g + brLLL*ppllISPR_C05_g + brHHH*pphhISPR_C05_g)

      CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_g) - REAL(ddISPR_C06_g + brLLL*ppllISPR_C06_g + brHHH*pphhISPR_C06_g)

      CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_g) - REAL(ddISPR_C07_g + brLLL*ppllISPR_C07_g + brHHH*pphhISPR_C07_g)

      CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_g) - REAL(ddISPR_C08_g + brLLL*ppllISPR_C08_g + brHHH*pphhISPR_C08_g)

      CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_g) = &
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_g) - REAL(ddISPR_C09_g + brLLL*ppllISPR_C09_g + brHHH*pphhISPR_C09_g)

!     FOR SIMPLESOM SPECIES:
      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_g) = &
      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_g) - REAL(ddIVO1_CN3_g + brLLL*ppllIVO1_CN3_g + brHHH*pphhIVO1_CN3_g)

      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_g) = &
      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_g) - REAL(ddIVO1_CN2_g + brLLL*ppllIVO1_CN2_g + brHHH*pphhIVO1_CN2_g)

      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_g) = &
      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_g) - REAL(ddIVO1_CN1_g + brLLL*ppllIVO1_CN1_g + brHHH*pphhIVO1_CN1_g)

      CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_g) = &
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_g) - REAL(ddIVO1_C00_g + brLLL*ppllIVO1_C00_g + brHHH*pphhIVO1_C00_g)

      CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_g) = &
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_g) - REAL(ddIVO1_C01_g + brLLL*ppllIVO1_C01_g + brHHH*pphhIVO1_C01_g)

      CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_g) = &
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_g) - REAL(ddIVO1_C02_g + brLLL*ppllIVO1_C02_g + brHHH*pphhIVO1_C02_g)

      CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_g) = &
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_g) - REAL(ddIVO1_C03_g + brLLL*ppllIVO1_C03_g + brHHH*pphhIVO1_C03_g)

      CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_g) = &
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_g) - REAL(ddIVO1_C04_g + brLLL*ppllIVO1_C04_g + brHHH*pphhIVO1_C04_g)

      CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_g) = &
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_g) - REAL(ddIVO1_C05_g + brLLL*ppllIVO1_C05_g + brHHH*pphhIVO1_C05_g)

      CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_g) = &
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_g) - REAL(ddIVO1_C06_g + brLLL*ppllIVO1_C06_g + brHHH*pphhIVO1_C06_g)

      CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_g) = &
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_g) - REAL(ddIVO1_C07_g + brLLL*ppllIVO1_C07_g + brHHH*pphhIVO1_C07_g)

      CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_g) = &
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_g) - REAL(ddIVO1_C08_g + brLLL*ppllIVO1_C08_g + brHHH*pphhIVO1_C08_g)

      CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_g) = &
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_g) - REAL(ddIVO1_C09_g + brLLL*ppllIVO1_C09_g + brHHH*pphhIVO1_C09_g)

      RETURN
      END SUBROUTINE STEP_GASCHEM
      
      END MODULE mod_STEP_GASCHEM
