!     =======================================================================
!              THIS SUBROUINTE MAPS BETWEEN SS-MOSAIC AND WRF-CHEM
!     =======================================================================

      MODULE mod_MAP_INOUT
      
!     MODULE INTERFACE:
!     =======================================================================      
!     POINTERS FOR CHEM IN WRF:
      USE mod_SSMOSAIC_PARAMS, ONLY: MW_SS, & 

      p_TERP_CN3_g, p_TERP_CN2_g, p_TERP_CN1_g, p_TERP_C00_g, p_TERP_C01_g, &
      p_TERP_C02_g, p_TERP_C03_g, p_TERP_C04_g, p_TERP_C05_g, p_TERP_C06_g, &
      p_TERP_C07_g, p_TERP_C08_g, p_TERP_C09_g, &
      p_TERP_CN3_a01, p_TERP_CN3_a02, p_TERP_CN3_a03, p_TERP_CN3_a04, &
      p_TERP_CN2_a01, p_TERP_CN2_a02, p_TERP_CN2_a03, p_TERP_CN2_a04, &
      p_TERP_CN1_a01, p_TERP_CN1_a02, p_TERP_CN1_a03, p_TERP_CN1_a04, &
      p_TERP_C00_a01, p_TERP_C00_a02, p_TERP_C00_a03, p_TERP_C00_a04, &
      p_TERP_C01_a01, p_TERP_C01_a02, p_TERP_C01_a03, p_TERP_C01_a04, &
      p_TERP_C02_a01, p_TERP_C02_a02, p_TERP_C02_a03, p_TERP_C02_a04, &
      p_TERP_C03_a01, p_TERP_C03_a02, p_TERP_C03_a03, p_TERP_C03_a04, &
      p_TERP_C04_a01, p_TERP_C04_a02, p_TERP_C04_a03, p_TERP_C04_a04, &
      p_TERP_C05_a01, p_TERP_C05_a02, p_TERP_C05_a03, p_TERP_C05_a04, &
      p_TERP_C06_a01, p_TERP_C06_a02, p_TERP_C06_a03, p_TERP_C06_a04, &
      p_TERP_C07_a01, p_TERP_C07_a02, p_TERP_C07_a03, p_TERP_C07_a04, &
      p_TERP_C08_a01, p_TERP_C08_a02, p_TERP_C08_a03, p_TERP_C08_a04, &
      p_TERP_C09_a01, p_TERP_C09_a02, p_TERP_C09_a03, p_TERP_C09_a04, &      
      p_TERP_CN3_d01, p_TERP_CN3_d02, p_TERP_CN3_d03, p_TERP_CN3_d04, &
      p_TERP_CN2_d01, p_TERP_CN2_d02, p_TERP_CN2_d03, p_TERP_CN2_d04, &
      p_TERP_CN1_d01, p_TERP_CN1_d02, p_TERP_CN1_d03, p_TERP_CN1_d04, &
      p_TERP_C00_d01, p_TERP_C00_d02, p_TERP_C00_d03, p_TERP_C00_d04, &
      p_TERP_C01_d01, p_TERP_C01_d02, p_TERP_C01_d03, p_TERP_C01_d04, &
      p_TERP_C02_d01, p_TERP_C02_d02, p_TERP_C02_d03, p_TERP_C02_d04, &
      p_TERP_C03_d01, p_TERP_C03_d02, p_TERP_C03_d03, p_TERP_C03_d04, &
      p_TERP_C04_d01, p_TERP_C04_d02, p_TERP_C04_d03, p_TERP_C04_d04, &
      p_TERP_C05_d01, p_TERP_C05_d02, p_TERP_C05_d03, p_TERP_C05_d04, &
      p_TERP_C06_d01, p_TERP_C06_d02, p_TERP_C06_d03, p_TERP_C06_d04, &
      p_TERP_C07_d01, p_TERP_C07_d02, p_TERP_C07_d03, p_TERP_C07_d04, &
      p_TERP_C08_d01, p_TERP_C08_d02, p_TERP_C08_d03, p_TERP_C08_d04, &
      p_TERP_C09_d01, p_TERP_C09_d02, p_TERP_C09_d03, p_TERP_C09_d04, &

!      p_TERP_CN3_a05, p_TERP_CN3_a06, p_TERP_CN3_a07, p_TERP_CN3_a08, &
!      p_TERP_CN2_a05, p_TERP_CN2_a06, p_TERP_CN2_a07, p_TERP_CN2_a08, &
!      p_TERP_CN1_a05, p_TERP_CN1_a06, p_TERP_CN1_a07, p_TERP_CN1_a08, &
!      p_TERP_C00_a05, p_TERP_C00_a06, p_TERP_C00_a07, p_TERP_C00_a08, &
!      p_TERP_C01_a05, p_TERP_C01_a06, p_TERP_C01_a07, p_TERP_C01_a08, &
!      p_TERP_C02_a05, p_TERP_C02_a06, p_TERP_C02_a07, p_TERP_C02_a08, &
!      p_TERP_C03_a05, p_TERP_C03_a06, p_TERP_C03_a07, p_TERP_C03_a08, &
!      p_TERP_C04_a05, p_TERP_C04_a06, p_TERP_C04_a07, p_TERP_C04_a08, &
!      p_TERP_C05_a05, p_TERP_C05_a06, p_TERP_C05_a07, p_TERP_C05_a08, &
!      p_TERP_C06_a05, p_TERP_C06_a06, p_TERP_C06_a07, p_TERP_C06_a08, &
!      p_TERP_C07_a05, p_TERP_C07_a06, p_TERP_C07_a07, p_TERP_C07_a08, &
!      p_TERP_C08_a05, p_TERP_C08_a06, p_TERP_C08_a07, p_TERP_C08_a08, &
!      p_TERP_C09_a05, p_TERP_C09_a06, p_TERP_C09_a07, p_TERP_C09_a08, &      
!      p_TERP_CN3_d05, p_TERP_CN3_d06, p_TERP_CN3_d07, p_TERP_CN3_d08, &
!      p_TERP_CN2_d05, p_TERP_CN2_d06, p_TERP_CN2_d07, p_TERP_CN2_d08, &
!      p_TERP_CN1_d05, p_TERP_CN1_d06, p_TERP_CN1_d07, p_TERP_CN1_d08, &
!      p_TERP_C00_d05, p_TERP_C00_d06, p_TERP_C00_d07, p_TERP_C00_d08, &
!      p_TERP_C01_d05, p_TERP_C01_d06, p_TERP_C01_d07, p_TERP_C01_d08, &
!      p_TERP_C02_d05, p_TERP_C02_d06, p_TERP_C02_d07, p_TERP_C02_d08, &
!      p_TERP_C03_d05, p_TERP_C03_d06, p_TERP_C03_d07, p_TERP_C03_d08, &
!      p_TERP_C04_d05, p_TERP_C04_d06, p_TERP_C04_d07, p_TERP_C04_d08, &
!      p_TERP_C05_d05, p_TERP_C05_d06, p_TERP_C05_d07, p_TERP_C05_d08, &
!      p_TERP_C06_d05, p_TERP_C06_d06, p_TERP_C06_d07, p_TERP_C06_d08, &
!      p_TERP_C07_d05, p_TERP_C07_d06, p_TERP_C07_d07, p_TERP_C07_d08, &
!      p_TERP_C08_d05, p_TERP_C08_d06, p_TERP_C08_d07, p_TERP_C08_d08, &
!      p_TERP_C09_d05, p_TERP_C09_d06, p_TERP_C09_d07, p_TERP_C09_d08, &

      p_ARO1_CN3_g, p_ARO1_CN2_g, p_ARO1_CN1_g, p_ARO1_C00_g, p_ARO1_C01_g, &
      p_ARO1_C02_g, p_ARO1_C03_g, p_ARO1_C04_g, p_ARO1_C05_g, p_ARO1_C06_g, &
      p_ARO1_C07_g, p_ARO1_C08_g, p_ARO1_C09_g, &
      p_ARO1_CN3_a01, p_ARO1_CN3_a02, p_ARO1_CN3_a03, p_ARO1_CN3_a04, &
      p_ARO1_CN2_a01, p_ARO1_CN2_a02, p_ARO1_CN2_a03, p_ARO1_CN2_a04, &
      p_ARO1_CN1_a01, p_ARO1_CN1_a02, p_ARO1_CN1_a03, p_ARO1_CN1_a04, &
      p_ARO1_C00_a01, p_ARO1_C00_a02, p_ARO1_C00_a03, p_ARO1_C00_a04, &
      p_ARO1_C01_a01, p_ARO1_C01_a02, p_ARO1_C01_a03, p_ARO1_C01_a04, &
      p_ARO1_C02_a01, p_ARO1_C02_a02, p_ARO1_C02_a03, p_ARO1_C02_a04, &
      p_ARO1_C03_a01, p_ARO1_C03_a02, p_ARO1_C03_a03, p_ARO1_C03_a04, &
      p_ARO1_C04_a01, p_ARO1_C04_a02, p_ARO1_C04_a03, p_ARO1_C04_a04, &
      p_ARO1_C05_a01, p_ARO1_C05_a02, p_ARO1_C05_a03, p_ARO1_C05_a04, &
      p_ARO1_C06_a01, p_ARO1_C06_a02, p_ARO1_C06_a03, p_ARO1_C06_a04, &
      p_ARO1_C07_a01, p_ARO1_C07_a02, p_ARO1_C07_a03, p_ARO1_C07_a04, &
      p_ARO1_C08_a01, p_ARO1_C08_a02, p_ARO1_C08_a03, p_ARO1_C08_a04, &
      p_ARO1_C09_a01, p_ARO1_C09_a02, p_ARO1_C09_a03, p_ARO1_C09_a04, &      
      p_ARO1_CN3_d01, p_ARO1_CN3_d02, p_ARO1_CN3_d03, p_ARO1_CN3_d04, &
      p_ARO1_CN2_d01, p_ARO1_CN2_d02, p_ARO1_CN2_d03, p_ARO1_CN2_d04, &
      p_ARO1_CN1_d01, p_ARO1_CN1_d02, p_ARO1_CN1_d03, p_ARO1_CN1_d04, &
      p_ARO1_C00_d01, p_ARO1_C00_d02, p_ARO1_C00_d03, p_ARO1_C00_d04, &
      p_ARO1_C01_d01, p_ARO1_C01_d02, p_ARO1_C01_d03, p_ARO1_C01_d04, &
      p_ARO1_C02_d01, p_ARO1_C02_d02, p_ARO1_C02_d03, p_ARO1_C02_d04, &
      p_ARO1_C03_d01, p_ARO1_C03_d02, p_ARO1_C03_d03, p_ARO1_C03_d04, &
      p_ARO1_C04_d01, p_ARO1_C04_d02, p_ARO1_C04_d03, p_ARO1_C04_d04, &
      p_ARO1_C05_d01, p_ARO1_C05_d02, p_ARO1_C05_d03, p_ARO1_C05_d04, &
      p_ARO1_C06_d01, p_ARO1_C06_d02, p_ARO1_C06_d03, p_ARO1_C06_d04, &
      p_ARO1_C07_d01, p_ARO1_C07_d02, p_ARO1_C07_d03, p_ARO1_C07_d04, &
      p_ARO1_C08_d01, p_ARO1_C08_d02, p_ARO1_C08_d03, p_ARO1_C08_d04, &
      p_ARO1_C09_d01, p_ARO1_C09_d02, p_ARO1_C09_d03, p_ARO1_C09_d04, &

!      p_ARO1_CN3_a05, p_ARO1_CN3_a06, p_ARO1_CN3_a07, p_ARO1_CN3_a08, &
!      p_ARO1_CN2_a05, p_ARO1_CN2_a06, p_ARO1_CN2_a07, p_ARO1_CN2_a08, &
!      p_ARO1_CN1_a05, p_ARO1_CN1_a06, p_ARO1_CN1_a07, p_ARO1_CN1_a08, &
!      p_ARO1_C00_a05, p_ARO1_C00_a06, p_ARO1_C00_a07, p_ARO1_C00_a08, &
!      p_ARO1_C01_a05, p_ARO1_C01_a06, p_ARO1_C01_a07, p_ARO1_C01_a08, &
!      p_ARO1_C02_a05, p_ARO1_C02_a06, p_ARO1_C02_a07, p_ARO1_C02_a08, &
!      p_ARO1_C03_a05, p_ARO1_C03_a06, p_ARO1_C03_a07, p_ARO1_C03_a08, &
!      p_ARO1_C04_a05, p_ARO1_C04_a06, p_ARO1_C04_a07, p_ARO1_C04_a08, &
!      p_ARO1_C05_a05, p_ARO1_C05_a06, p_ARO1_C05_a07, p_ARO1_C05_a08, &
!      p_ARO1_C06_a05, p_ARO1_C06_a06, p_ARO1_C06_a07, p_ARO1_C06_a08, &
!      p_ARO1_C07_a05, p_ARO1_C07_a06, p_ARO1_C07_a07, p_ARO1_C07_a08, &
!      p_ARO1_C08_a05, p_ARO1_C08_a06, p_ARO1_C08_a07, p_ARO1_C08_a08, &
!      p_ARO1_C09_a05, p_ARO1_C09_a06, p_ARO1_C09_a07, p_ARO1_C09_a08, &      
!      p_ARO1_CN3_d05, p_ARO1_CN3_d06, p_ARO1_CN3_d07, p_ARO1_CN3_d08, &
!      p_ARO1_CN2_d05, p_ARO1_CN2_d06, p_ARO1_CN2_d07, p_ARO1_CN2_d08, &
!      p_ARO1_CN1_d05, p_ARO1_CN1_d06, p_ARO1_CN1_d07, p_ARO1_CN1_d08, &
!      p_ARO1_C00_d05, p_ARO1_C00_d06, p_ARO1_C00_d07, p_ARO1_C00_d08, &
!      p_ARO1_C01_d05, p_ARO1_C01_d06, p_ARO1_C01_d07, p_ARO1_C01_d08, &
!      p_ARO1_C02_d05, p_ARO1_C02_d06, p_ARO1_C02_d07, p_ARO1_C02_d08, &
!      p_ARO1_C03_d05, p_ARO1_C03_d06, p_ARO1_C03_d07, p_ARO1_C03_d08, &
!      p_ARO1_C04_d05, p_ARO1_C04_d06, p_ARO1_C04_d07, p_ARO1_C04_d08, &
!      p_ARO1_C05_d05, p_ARO1_C05_d06, p_ARO1_C05_d07, p_ARO1_C05_d08, &
!      p_ARO1_C06_d05, p_ARO1_C06_d06, p_ARO1_C06_d07, p_ARO1_C06_d08, &
!      p_ARO1_C07_d05, p_ARO1_C07_d06, p_ARO1_C07_d07, p_ARO1_C07_d08, &
!      p_ARO1_C08_d05, p_ARO1_C08_d06, p_ARO1_C08_d07, p_ARO1_C08_d08, &
!      p_ARO1_C09_d05, p_ARO1_C09_d06, p_ARO1_C09_d07, p_ARO1_C09_d08, &

      p_ARO2_CN3_g, p_ARO2_CN2_g, p_ARO2_CN1_g, p_ARO2_C00_g, p_ARO2_C01_g, &
      p_ARO2_C02_g, p_ARO2_C03_g, p_ARO2_C04_g, p_ARO2_C05_g, p_ARO2_C06_g, &
      p_ARO2_C07_g, p_ARO2_C08_g, p_ARO2_C09_g, &
      p_ARO2_CN3_a01, p_ARO2_CN3_a02, p_ARO2_CN3_a03, p_ARO2_CN3_a04, &
      p_ARO2_CN2_a01, p_ARO2_CN2_a02, p_ARO2_CN2_a03, p_ARO2_CN2_a04, &
      p_ARO2_CN1_a01, p_ARO2_CN1_a02, p_ARO2_CN1_a03, p_ARO2_CN1_a04, &
      p_ARO2_C00_a01, p_ARO2_C00_a02, p_ARO2_C00_a03, p_ARO2_C00_a04, &
      p_ARO2_C01_a01, p_ARO2_C01_a02, p_ARO2_C01_a03, p_ARO2_C01_a04, &
      p_ARO2_C02_a01, p_ARO2_C02_a02, p_ARO2_C02_a03, p_ARO2_C02_a04, &
      p_ARO2_C03_a01, p_ARO2_C03_a02, p_ARO2_C03_a03, p_ARO2_C03_a04, &
      p_ARO2_C04_a01, p_ARO2_C04_a02, p_ARO2_C04_a03, p_ARO2_C04_a04, &
      p_ARO2_C05_a01, p_ARO2_C05_a02, p_ARO2_C05_a03, p_ARO2_C05_a04, &
      p_ARO2_C06_a01, p_ARO2_C06_a02, p_ARO2_C06_a03, p_ARO2_C06_a04, &
      p_ARO2_C07_a01, p_ARO2_C07_a02, p_ARO2_C07_a03, p_ARO2_C07_a04, &
      p_ARO2_C08_a01, p_ARO2_C08_a02, p_ARO2_C08_a03, p_ARO2_C08_a04, &
      p_ARO2_C09_a01, p_ARO2_C09_a02, p_ARO2_C09_a03, p_ARO2_C09_a04, &      
      p_ARO2_CN3_d01, p_ARO2_CN3_d02, p_ARO2_CN3_d03, p_ARO2_CN3_d04, &
      p_ARO2_CN2_d01, p_ARO2_CN2_d02, p_ARO2_CN2_d03, p_ARO2_CN2_d04, &
      p_ARO2_CN1_d01, p_ARO2_CN1_d02, p_ARO2_CN1_d03, p_ARO2_CN1_d04, &
      p_ARO2_C00_d01, p_ARO2_C00_d02, p_ARO2_C00_d03, p_ARO2_C00_d04, &
      p_ARO2_C01_d01, p_ARO2_C01_d02, p_ARO2_C01_d03, p_ARO2_C01_d04, &
      p_ARO2_C02_d01, p_ARO2_C02_d02, p_ARO2_C02_d03, p_ARO2_C02_d04, &
      p_ARO2_C03_d01, p_ARO2_C03_d02, p_ARO2_C03_d03, p_ARO2_C03_d04, &
      p_ARO2_C04_d01, p_ARO2_C04_d02, p_ARO2_C04_d03, p_ARO2_C04_d04, &
      p_ARO2_C05_d01, p_ARO2_C05_d02, p_ARO2_C05_d03, p_ARO2_C05_d04, &
      p_ARO2_C06_d01, p_ARO2_C06_d02, p_ARO2_C06_d03, p_ARO2_C06_d04, &
      p_ARO2_C07_d01, p_ARO2_C07_d02, p_ARO2_C07_d03, p_ARO2_C07_d04, &
      p_ARO2_C08_d01, p_ARO2_C08_d02, p_ARO2_C08_d03, p_ARO2_C08_d04, &
      p_ARO2_C09_d01, p_ARO2_C09_d02, p_ARO2_C09_d03, p_ARO2_C09_d04, &

!      p_ARO2_CN3_a05, p_ARO2_CN3_a06, p_ARO2_CN3_a07, p_ARO2_CN3_a08, &
!      p_ARO2_CN2_a05, p_ARO2_CN2_a06, p_ARO2_CN2_a07, p_ARO2_CN2_a08, &
!      p_ARO2_CN1_a05, p_ARO2_CN1_a06, p_ARO2_CN1_a07, p_ARO2_CN1_a08, &
!      p_ARO2_C00_a05, p_ARO2_C00_a06, p_ARO2_C00_a07, p_ARO2_C00_a08, &
!      p_ARO2_C01_a05, p_ARO2_C01_a06, p_ARO2_C01_a07, p_ARO2_C01_a08, &
!      p_ARO2_C02_a05, p_ARO2_C02_a06, p_ARO2_C02_a07, p_ARO2_C02_a08, &
!      p_ARO2_C03_a05, p_ARO2_C03_a06, p_ARO2_C03_a07, p_ARO2_C03_a08, &
!      p_ARO2_C04_a05, p_ARO2_C04_a06, p_ARO2_C04_a07, p_ARO2_C04_a08, &
!      p_ARO2_C05_a05, p_ARO2_C05_a06, p_ARO2_C05_a07, p_ARO2_C05_a08, &
!      p_ARO2_C06_a05, p_ARO2_C06_a06, p_ARO2_C06_a07, p_ARO2_C06_a08, &
!      p_ARO2_C07_a05, p_ARO2_C07_a06, p_ARO2_C07_a07, p_ARO2_C07_a08, &
!      p_ARO2_C08_a05, p_ARO2_C08_a06, p_ARO2_C08_a07, p_ARO2_C08_a08, &
!      p_ARO2_C09_a05, p_ARO2_C09_a06, p_ARO2_C09_a07, p_ARO2_C09_a08, &      
!      p_ARO2_CN3_d05, p_ARO2_CN3_d06, p_ARO2_CN3_d07, p_ARO2_CN3_d08, &
!      p_ARO2_CN2_d05, p_ARO2_CN2_d06, p_ARO2_CN2_d07, p_ARO2_CN2_d08, &
!      p_ARO2_CN1_d05, p_ARO2_CN1_d06, p_ARO2_CN1_d07, p_ARO2_CN1_d08, &
!      p_ARO2_C00_d05, p_ARO2_C00_d06, p_ARO2_C00_d07, p_ARO2_C00_d08, &
!      p_ARO2_C01_d05, p_ARO2_C01_d06, p_ARO2_C01_d07, p_ARO2_C01_d08, &
!      p_ARO2_C02_d05, p_ARO2_C02_d06, p_ARO2_C02_d07, p_ARO2_C02_d08, &
!      p_ARO2_C03_d05, p_ARO2_C03_d06, p_ARO2_C03_d07, p_ARO2_C03_d08, &
!      p_ARO2_C04_d05, p_ARO2_C04_d06, p_ARO2_C04_d07, p_ARO2_C04_d08, &
!      p_ARO2_C05_d05, p_ARO2_C05_d06, p_ARO2_C05_d07, p_ARO2_C05_d08, &
!      p_ARO2_C06_d05, p_ARO2_C06_d06, p_ARO2_C06_d07, p_ARO2_C06_d08, &
!      p_ARO2_C07_d05, p_ARO2_C07_d06, p_ARO2_C07_d07, p_ARO2_C07_d08, &
!      p_ARO2_C08_d05, p_ARO2_C08_d06, p_ARO2_C08_d07, p_ARO2_C08_d08, &
!      p_ARO2_C09_d05, p_ARO2_C09_d06, p_ARO2_C09_d07, p_ARO2_C09_d08, &

      p_ISPR_CN3_g, p_ISPR_CN2_g, p_ISPR_CN1_g, p_ISPR_C00_g, p_ISPR_C01_g, &
      p_ISPR_C02_g, p_ISPR_C03_g, p_ISPR_C04_g, p_ISPR_C05_g, p_ISPR_C06_g, &
      p_ISPR_C07_g, p_ISPR_C08_g, p_ISPR_C09_g, &
      p_ISPR_CN3_a01, p_ISPR_CN3_a02, p_ISPR_CN3_a03, p_ISPR_CN3_a04, &
      p_ISPR_CN2_a01, p_ISPR_CN2_a02, p_ISPR_CN2_a03, p_ISPR_CN2_a04, &
      p_ISPR_CN1_a01, p_ISPR_CN1_a02, p_ISPR_CN1_a03, p_ISPR_CN1_a04, &
      p_ISPR_C00_a01, p_ISPR_C00_a02, p_ISPR_C00_a03, p_ISPR_C00_a04, &
      p_ISPR_C01_a01, p_ISPR_C01_a02, p_ISPR_C01_a03, p_ISPR_C01_a04, &
      p_ISPR_C02_a01, p_ISPR_C02_a02, p_ISPR_C02_a03, p_ISPR_C02_a04, &
      p_ISPR_C03_a01, p_ISPR_C03_a02, p_ISPR_C03_a03, p_ISPR_C03_a04, &
      p_ISPR_C04_a01, p_ISPR_C04_a02, p_ISPR_C04_a03, p_ISPR_C04_a04, &
      p_ISPR_C05_a01, p_ISPR_C05_a02, p_ISPR_C05_a03, p_ISPR_C05_a04, &
      p_ISPR_C06_a01, p_ISPR_C06_a02, p_ISPR_C06_a03, p_ISPR_C06_a04, &
      p_ISPR_C07_a01, p_ISPR_C07_a02, p_ISPR_C07_a03, p_ISPR_C07_a04, &
      p_ISPR_C08_a01, p_ISPR_C08_a02, p_ISPR_C08_a03, p_ISPR_C08_a04, &
      p_ISPR_C09_a01, p_ISPR_C09_a02, p_ISPR_C09_a03, p_ISPR_C09_a04, &      
      p_ISPR_CN3_d01, p_ISPR_CN3_d02, p_ISPR_CN3_d03, p_ISPR_CN3_d04, &
      p_ISPR_CN2_d01, p_ISPR_CN2_d02, p_ISPR_CN2_d03, p_ISPR_CN2_d04, &
      p_ISPR_CN1_d01, p_ISPR_CN1_d02, p_ISPR_CN1_d03, p_ISPR_CN1_d04, &
      p_ISPR_C00_d01, p_ISPR_C00_d02, p_ISPR_C00_d03, p_ISPR_C00_d04, &
      p_ISPR_C01_d01, p_ISPR_C01_d02, p_ISPR_C01_d03, p_ISPR_C01_d04, &
      p_ISPR_C02_d01, p_ISPR_C02_d02, p_ISPR_C02_d03, p_ISPR_C02_d04, &
      p_ISPR_C03_d01, p_ISPR_C03_d02, p_ISPR_C03_d03, p_ISPR_C03_d04, &
      p_ISPR_C04_d01, p_ISPR_C04_d02, p_ISPR_C04_d03, p_ISPR_C04_d04, &
      p_ISPR_C05_d01, p_ISPR_C05_d02, p_ISPR_C05_d03, p_ISPR_C05_d04, &
      p_ISPR_C06_d01, p_ISPR_C06_d02, p_ISPR_C06_d03, p_ISPR_C06_d04, &
      p_ISPR_C07_d01, p_ISPR_C07_d02, p_ISPR_C07_d03, p_ISPR_C07_d04, &
      p_ISPR_C08_d01, p_ISPR_C08_d02, p_ISPR_C08_d03, p_ISPR_C08_d04, &
      p_ISPR_C09_d01, p_ISPR_C09_d02, p_ISPR_C09_d03, p_ISPR_C09_d04, &

!      p_ISPR_CN3_a05, p_ISPR_CN3_a06, p_ISPR_CN3_a07, p_ISPR_CN3_a08, &
!      p_ISPR_CN2_a05, p_ISPR_CN2_a06, p_ISPR_CN2_a07, p_ISPR_CN2_a08, &
!      p_ISPR_CN1_a05, p_ISPR_CN1_a06, p_ISPR_CN1_a07, p_ISPR_CN1_a08, &
!      p_ISPR_C00_a05, p_ISPR_C00_a06, p_ISPR_C00_a07, p_ISPR_C00_a08, &
!      p_ISPR_C01_a05, p_ISPR_C01_a06, p_ISPR_C01_a07, p_ISPR_C01_a08, &
!      p_ISPR_C02_a05, p_ISPR_C02_a06, p_ISPR_C02_a07, p_ISPR_C02_a08, &
!      p_ISPR_C03_a05, p_ISPR_C03_a06, p_ISPR_C03_a07, p_ISPR_C03_a08, &
!      p_ISPR_C04_a05, p_ISPR_C04_a06, p_ISPR_C04_a07, p_ISPR_C04_a08, &
!      p_ISPR_C05_a05, p_ISPR_C05_a06, p_ISPR_C05_a07, p_ISPR_C05_a08, &
!      p_ISPR_C06_a05, p_ISPR_C06_a06, p_ISPR_C06_a07, p_ISPR_C06_a08, &
!      p_ISPR_C07_a05, p_ISPR_C07_a06, p_ISPR_C07_a07, p_ISPR_C07_a08, &
!      p_ISPR_C08_a05, p_ISPR_C08_a06, p_ISPR_C08_a07, p_ISPR_C08_a08, &
!      p_ISPR_C09_a05, p_ISPR_C09_a06, p_ISPR_C09_a07, p_ISPR_C09_a08, &      
!      p_ISPR_CN3_d05, p_ISPR_CN3_d06, p_ISPR_CN3_d07, p_ISPR_CN3_d08, &
!      p_ISPR_CN2_d05, p_ISPR_CN2_d06, p_ISPR_CN2_d07, p_ISPR_CN2_d08, &
!      p_ISPR_CN1_d05, p_ISPR_CN1_d06, p_ISPR_CN1_d07, p_ISPR_CN1_d08, &
!      p_ISPR_C00_d05, p_ISPR_C00_d06, p_ISPR_C00_d07, p_ISPR_C00_d08, &
!      p_ISPR_C01_d05, p_ISPR_C01_d06, p_ISPR_C01_d07, p_ISPR_C01_d08, &
!      p_ISPR_C02_d05, p_ISPR_C02_d06, p_ISPR_C02_d07, p_ISPR_C02_d08, &
!      p_ISPR_C03_d05, p_ISPR_C03_d06, p_ISPR_C03_d07, p_ISPR_C03_d08, &
!      p_ISPR_C04_d05, p_ISPR_C04_d06, p_ISPR_C04_d07, p_ISPR_C04_d08, &
!      p_ISPR_C05_d05, p_ISPR_C05_d06, p_ISPR_C05_d07, p_ISPR_C05_d08, &
!      p_ISPR_C06_d05, p_ISPR_C06_d06, p_ISPR_C06_d07, p_ISPR_C06_d08, &
!      p_ISPR_C07_d05, p_ISPR_C07_d06, p_ISPR_C07_d07, p_ISPR_C07_d08, &
!      p_ISPR_C08_d05, p_ISPR_C08_d06, p_ISPR_C08_d07, p_ISPR_C08_d08, &
!      p_ISPR_C09_d05, p_ISPR_C09_d06, p_ISPR_C09_d07, p_ISPR_C09_d08, &

      p_SESQ_CN3_g, p_SESQ_CN2_g, p_SESQ_CN1_g, p_SESQ_C00_g, p_SESQ_C01_g, &
      p_SESQ_C02_g, p_SESQ_C03_g, p_SESQ_C04_g, p_SESQ_C05_g, p_SESQ_C06_g, &
      p_SESQ_C07_g, p_SESQ_C08_g, p_SESQ_C09_g, &
      p_SESQ_CN3_a01, p_SESQ_CN3_a02, p_SESQ_CN3_a03, p_SESQ_CN3_a04, &
      p_SESQ_CN2_a01, p_SESQ_CN2_a02, p_SESQ_CN2_a03, p_SESQ_CN2_a04, &
      p_SESQ_CN1_a01, p_SESQ_CN1_a02, p_SESQ_CN1_a03, p_SESQ_CN1_a04, &
      p_SESQ_C00_a01, p_SESQ_C00_a02, p_SESQ_C00_a03, p_SESQ_C00_a04, &
      p_SESQ_C01_a01, p_SESQ_C01_a02, p_SESQ_C01_a03, p_SESQ_C01_a04, &
      p_SESQ_C02_a01, p_SESQ_C02_a02, p_SESQ_C02_a03, p_SESQ_C02_a04, &
      p_SESQ_C03_a01, p_SESQ_C03_a02, p_SESQ_C03_a03, p_SESQ_C03_a04, &
      p_SESQ_C04_a01, p_SESQ_C04_a02, p_SESQ_C04_a03, p_SESQ_C04_a04, &
      p_SESQ_C05_a01, p_SESQ_C05_a02, p_SESQ_C05_a03, p_SESQ_C05_a04, &
      p_SESQ_C06_a01, p_SESQ_C06_a02, p_SESQ_C06_a03, p_SESQ_C06_a04, &
      p_SESQ_C07_a01, p_SESQ_C07_a02, p_SESQ_C07_a03, p_SESQ_C07_a04, &
      p_SESQ_C08_a01, p_SESQ_C08_a02, p_SESQ_C08_a03, p_SESQ_C08_a04, &
      p_SESQ_C09_a01, p_SESQ_C09_a02, p_SESQ_C09_a03, p_SESQ_C09_a04, &      
      p_SESQ_CN3_d01, p_SESQ_CN3_d02, p_SESQ_CN3_d03, p_SESQ_CN3_d04, &
      p_SESQ_CN2_d01, p_SESQ_CN2_d02, p_SESQ_CN2_d03, p_SESQ_CN2_d04, &
      p_SESQ_CN1_d01, p_SESQ_CN1_d02, p_SESQ_CN1_d03, p_SESQ_CN1_d04, &
      p_SESQ_C00_d01, p_SESQ_C00_d02, p_SESQ_C00_d03, p_SESQ_C00_d04, &
      p_SESQ_C01_d01, p_SESQ_C01_d02, p_SESQ_C01_d03, p_SESQ_C01_d04, &
      p_SESQ_C02_d01, p_SESQ_C02_d02, p_SESQ_C02_d03, p_SESQ_C02_d04, &
      p_SESQ_C03_d01, p_SESQ_C03_d02, p_SESQ_C03_d03, p_SESQ_C03_d04, &
      p_SESQ_C04_d01, p_SESQ_C04_d02, p_SESQ_C04_d03, p_SESQ_C04_d04, &
      p_SESQ_C05_d01, p_SESQ_C05_d02, p_SESQ_C05_d03, p_SESQ_C05_d04, &
      p_SESQ_C06_d01, p_SESQ_C06_d02, p_SESQ_C06_d03, p_SESQ_C06_d04, &
      p_SESQ_C07_d01, p_SESQ_C07_d02, p_SESQ_C07_d03, p_SESQ_C07_d04, &
      p_SESQ_C08_d01, p_SESQ_C08_d02, p_SESQ_C08_d03, p_SESQ_C08_d04, &
      p_SESQ_C09_d01, p_SESQ_C09_d02, p_SESQ_C09_d03, p_SESQ_C09_d04, &

!      p_SESQ_CN3_a05, p_SESQ_CN3_a06, p_SESQ_CN3_a07, p_SESQ_CN3_a08, &
!      p_SESQ_CN2_a05, p_SESQ_CN2_a06, p_SESQ_CN2_a07, p_SESQ_CN2_a08, &
!      p_SESQ_CN1_a05, p_SESQ_CN1_a06, p_SESQ_CN1_a07, p_SESQ_CN1_a08, &
!      p_SESQ_C00_a05, p_SESQ_C00_a06, p_SESQ_C00_a07, p_SESQ_C00_a08, &
!      p_SESQ_C01_a05, p_SESQ_C01_a06, p_SESQ_C01_a07, p_SESQ_C01_a08, &
!      p_SESQ_C02_a05, p_SESQ_C02_a06, p_SESQ_C02_a07, p_SESQ_C02_a08, &
!      p_SESQ_C03_a05, p_SESQ_C03_a06, p_SESQ_C03_a07, p_SESQ_C03_a08, &
!      p_SESQ_C04_a05, p_SESQ_C04_a06, p_SESQ_C04_a07, p_SESQ_C04_a08, &
!      p_SESQ_C05_a05, p_SESQ_C05_a06, p_SESQ_C05_a07, p_SESQ_C05_a08, &
!      p_SESQ_C06_a05, p_SESQ_C06_a06, p_SESQ_C06_a07, p_SESQ_C06_a08, &
!      p_SESQ_C07_a05, p_SESQ_C07_a06, p_SESQ_C07_a07, p_SESQ_C07_a08, &
!      p_SESQ_C08_a05, p_SESQ_C08_a06, p_SESQ_C08_a07, p_SESQ_C08_a08, &
!      p_SESQ_C09_a05, p_SESQ_C09_a06, p_SESQ_C09_a07, p_SESQ_C09_a08, &      
!      p_SESQ_CN3_d05, p_SESQ_CN3_d06, p_SESQ_CN3_d07, p_SESQ_CN3_d08, &
!      p_SESQ_CN2_d05, p_SESQ_CN2_d06, p_SESQ_CN2_d07, p_SESQ_CN2_d08, &
!      p_SESQ_CN1_d05, p_SESQ_CN1_d06, p_SESQ_CN1_d07, p_SESQ_CN1_d08, &
!      p_SESQ_C00_d05, p_SESQ_C00_d06, p_SESQ_C00_d07, p_SESQ_C00_d08, &
!      p_SESQ_C01_d05, p_SESQ_C01_d06, p_SESQ_C01_d07, p_SESQ_C01_d08, &
!      p_SESQ_C02_d05, p_SESQ_C02_d06, p_SESQ_C02_d07, p_SESQ_C02_d08, &
!      p_SESQ_C03_d05, p_SESQ_C03_d06, p_SESQ_C03_d07, p_SESQ_C03_d08, &
!      p_SESQ_C04_d05, p_SESQ_C04_d06, p_SESQ_C04_d07, p_SESQ_C04_d08, &
!      p_SESQ_C05_d05, p_SESQ_C05_d06, p_SESQ_C05_d07, p_SESQ_C05_d08, &
!      p_SESQ_C06_d05, p_SESQ_C06_d06, p_SESQ_C06_d07, p_SESQ_C06_d08, &
!      p_SESQ_C07_d05, p_SESQ_C07_d06, p_SESQ_C07_d07, p_SESQ_C07_d08, &
!      p_SESQ_C08_d05, p_SESQ_C08_d06, p_SESQ_C08_d07, p_SESQ_C08_d08, &
!      p_SESQ_C09_d05, p_SESQ_C09_d06, p_SESQ_C09_d07, p_SESQ_C09_d08, &

      p_IVO1_CN3_g, p_IVO1_CN2_g, p_IVO1_CN1_g, p_IVO1_C00_g, p_IVO1_C01_g, &
      p_IVO1_C02_g, p_IVO1_C03_g, p_IVO1_C04_g, p_IVO1_C05_g, p_IVO1_C06_g, &
      p_IVO1_C07_g, p_IVO1_C08_g, p_IVO1_C09_g, &
      p_IVO1_CN3_a01, p_IVO1_CN3_a02, p_IVO1_CN3_a03, p_IVO1_CN3_a04, &
      p_IVO1_CN2_a01, p_IVO1_CN2_a02, p_IVO1_CN2_a03, p_IVO1_CN2_a04, &
      p_IVO1_CN1_a01, p_IVO1_CN1_a02, p_IVO1_CN1_a03, p_IVO1_CN1_a04, &
      p_IVO1_C00_a01, p_IVO1_C00_a02, p_IVO1_C00_a03, p_IVO1_C00_a04, &
      p_IVO1_C01_a01, p_IVO1_C01_a02, p_IVO1_C01_a03, p_IVO1_C01_a04, &
      p_IVO1_C02_a01, p_IVO1_C02_a02, p_IVO1_C02_a03, p_IVO1_C02_a04, &
      p_IVO1_C03_a01, p_IVO1_C03_a02, p_IVO1_C03_a03, p_IVO1_C03_a04, &
      p_IVO1_C04_a01, p_IVO1_C04_a02, p_IVO1_C04_a03, p_IVO1_C04_a04, &
      p_IVO1_C05_a01, p_IVO1_C05_a02, p_IVO1_C05_a03, p_IVO1_C05_a04, &
      p_IVO1_C06_a01, p_IVO1_C06_a02, p_IVO1_C06_a03, p_IVO1_C06_a04, &
      p_IVO1_C07_a01, p_IVO1_C07_a02, p_IVO1_C07_a03, p_IVO1_C07_a04, &
      p_IVO1_C08_a01, p_IVO1_C08_a02, p_IVO1_C08_a03, p_IVO1_C08_a04, &
      p_IVO1_C09_a01, p_IVO1_C09_a02, p_IVO1_C09_a03, p_IVO1_C09_a04, &      
      p_IVO1_CN3_d01, p_IVO1_CN3_d02, p_IVO1_CN3_d03, p_IVO1_CN3_d04, &
      p_IVO1_CN2_d01, p_IVO1_CN2_d02, p_IVO1_CN2_d03, p_IVO1_CN2_d04, &
      p_IVO1_CN1_d01, p_IVO1_CN1_d02, p_IVO1_CN1_d03, p_IVO1_CN1_d04, &
      p_IVO1_C00_d01, p_IVO1_C00_d02, p_IVO1_C00_d03, p_IVO1_C00_d04, &
      p_IVO1_C01_d01, p_IVO1_C01_d02, p_IVO1_C01_d03, p_IVO1_C01_d04, &
      p_IVO1_C02_d01, p_IVO1_C02_d02, p_IVO1_C02_d03, p_IVO1_C02_d04, &
      p_IVO1_C03_d01, p_IVO1_C03_d02, p_IVO1_C03_d03, p_IVO1_C03_d04, &
      p_IVO1_C04_d01, p_IVO1_C04_d02, p_IVO1_C04_d03, p_IVO1_C04_d04, &
      p_IVO1_C05_d01, p_IVO1_C05_d02, p_IVO1_C05_d03, p_IVO1_C05_d04, &
      p_IVO1_C06_d01, p_IVO1_C06_d02, p_IVO1_C06_d03, p_IVO1_C06_d04, &
      p_IVO1_C07_d01, p_IVO1_C07_d02, p_IVO1_C07_d03, p_IVO1_C07_d04, &
      p_IVO1_C08_d01, p_IVO1_C08_d02, p_IVO1_C08_d03, p_IVO1_C08_d04, &
      p_IVO1_C09_d01, p_IVO1_C09_d02, p_IVO1_C09_d03, p_IVO1_C09_d04, &

!      p_IVO1_CN3_a05, p_IVO1_CN3_a06, p_IVO1_CN3_a07, p_IVO1_CN3_a08, &
!      p_IVO1_CN2_a05, p_IVO1_CN2_a06, p_IVO1_CN2_a07, p_IVO1_CN2_a08, &
!      p_IVO1_CN1_a05, p_IVO1_CN1_a06, p_IVO1_CN1_a07, p_IVO1_CN1_a08, &
!      p_IVO1_C00_a05, p_IVO1_C00_a06, p_IVO1_C00_a07, p_IVO1_C00_a08, &
!      p_IVO1_C01_a05, p_IVO1_C01_a06, p_IVO1_C01_a07, p_IVO1_C01_a08, &
!      p_IVO1_C02_a05, p_IVO1_C02_a06, p_IVO1_C02_a07, p_IVO1_C02_a08, &
!      p_IVO1_C03_a05, p_IVO1_C03_a06, p_IVO1_C03_a07, p_IVO1_C03_a08, &
!      p_IVO1_C04_a05, p_IVO1_C04_a06, p_IVO1_C04_a07, p_IVO1_C04_a08, &
!      p_IVO1_C05_a05, p_IVO1_C05_a06, p_IVO1_C05_a07, p_IVO1_C05_a08, &
!      p_IVO1_C06_a05, p_IVO1_C06_a06, p_IVO1_C06_a07, p_IVO1_C06_a08, &
!      p_IVO1_C07_a05, p_IVO1_C07_a06, p_IVO1_C07_a07, p_IVO1_C07_a08, &
!      p_IVO1_C08_a05, p_IVO1_C08_a06, p_IVO1_C08_a07, p_IVO1_C08_a08, &
!      p_IVO1_C09_a05, p_IVO1_C09_a06, p_IVO1_C09_a07, p_IVO1_C09_a08, &      
!      p_IVO1_CN3_d05, p_IVO1_CN3_d06, p_IVO1_CN3_d07, p_IVO1_CN3_d08, &
!      p_IVO1_CN2_d05, p_IVO1_CN2_d06, p_IVO1_CN2_d07, p_IVO1_CN2_d08, &
!      p_IVO1_CN1_d05, p_IVO1_CN1_d06, p_IVO1_CN1_d07, p_IVO1_CN1_d08, &
!      p_IVO1_C00_d05, p_IVO1_C00_d06, p_IVO1_C00_d07, p_IVO1_C00_d08, &
!      p_IVO1_C01_d05, p_IVO1_C01_d06, p_IVO1_C01_d07, p_IVO1_C01_d08, &
!      p_IVO1_C02_d05, p_IVO1_C02_d06, p_IVO1_C02_d07, p_IVO1_C02_d08, &
!      p_IVO1_C03_d05, p_IVO1_C03_d06, p_IVO1_C03_d07, p_IVO1_C03_d08, &
!      p_IVO1_C04_d05, p_IVO1_C04_d06, p_IVO1_C04_d07, p_IVO1_C04_d08, &
!      p_IVO1_C05_d05, p_IVO1_C05_d06, p_IVO1_C05_d07, p_IVO1_C05_d08, &
!      p_IVO1_C06_d05, p_IVO1_C06_d06, p_IVO1_C06_d07, p_IVO1_C06_d08, &
!      p_IVO1_C07_d05, p_IVO1_C07_d06, p_IVO1_C07_d07, p_IVO1_C07_d08, &
!      p_IVO1_C08_d05, p_IVO1_C08_d06, p_IVO1_C08_d07, p_IVO1_C08_d08, &
!      p_IVO1_C09_d05, p_IVO1_C09_d06, p_IVO1_C09_d07, p_IVO1_C09_d08, &      

      p_HO, & 
      p_NUM_a01, p_NUM_a02, p_NUM_a03, p_NUM_a04, &
!      p_NUM_a05, p_NUM_a06, p_NUM_a07, p_NUM_a08, &
      p_PCG1_F_C, p_PCG1_F_C_a01, p_PCG1_F_C_a02, p_PCG1_F_C_a03, p_PCG1_F_C_a04
      
      CONTAINS
      
!     =======================================================================
!              THIS SUBROUINTE MAPS BETWEEN SS-MOSAIC AND WRF-CHEM
!     =======================================================================

      SUBROUTINE MAP_INOUT(iNOW,kNOW,jNOW,CTRL, &
                           CHEM, iM1,iM2, jM1,jM2, kM1,kM2, nCHEM, &
                           moGAS, moAERO, moOLIG, NUM, nCOMP, nBINS)
      
!     DECLARATIONS:
!     =======================================================================
      IMPLICIT NONE
      
!     CURRENT TILE COORDINATES:
      INTEGER,INTENT(IN) :: iNOW, jNOW, kNOW

!     MAP CONTROL (IN or OUT):
      INTEGER,INTENT(IN) :: CTRL

!     CHEM ARRAY DIMENSIONS:
      INTEGER,INTENT(IN) :: iM1,iM2
      INTEGER,INTENT(IN) :: jM1,jM2
      INTEGER,INTENT(IN) :: kM1,kM2
      INTEGER,INTENT(IN) :: nCHEM
      
!     WRF ARRAY FOR TRACER SPECIES 
!     FOR GAS [ppm] AND FOR AEROSOL SPECIES [ug per kg-air]:
      REAL,INTENT(INOUT), &
      DIMENSION(iM1:iM2, kM1:kM2, jM1:jM2, 1:nCHEM) :: CHEM
      
!     NUMBER OF SPECIES 
!     AND AEROSOL BINS:
      INTEGER,INTENT(IN) :: nCOMP
      INTEGER,INTENT(IN) :: nBINS

!     SS-MOSAIC GAS,
!     AEROSOL, AND OLIGOMER ARRAYS:
      REAL(8),INTENT(INOUT) :: moGAS(nCOMP)
      REAL(8),INTENT(INOUT) :: moAERO(nCOMP,nBINS)
      REAL(8),INTENT(INOUT) :: moOLIG(nCOMP,nBINS)

!     PARTICLE NUM. ARRAY:
      REAL(8),INTENT(INOUT) :: NUM(nBINS)

!     CONVERSION FACTORS:
      REAL(8) :: CONV1(nCOMP)
      REAL(8) :: CONV2
      REAL(8) :: CONV3
      
!     UNIT CONVERSION FACTORS:
!     =======================================================================
!     >> FOR GAS FROM [ppm] TO [ug m-3]:
!     && NOTE: THIS IS SPECIES-DEPENDENT.
!     && NOTE: ACCOUNT FOR cAIRCLM.
      CONV1 = 1e-6*(101325./8.314/298.*1e-6)*1e6*MW_SS*1e6

!     >> FOR AEROSOL FROM [ug kg@air-1] to [ug m-3]:
!     && NOTE: ACCOUNT FOR cAIRCLM.
      CONV2 = 1./(1000./28.966)*(101325./8.314/298.*1e-6)*1e6

!     >> FOR AEROSOL NUMBER 
!     && FROM [# kg@air-1] to [# cm-3]:
!     && NOTE: ACCOUNT FOR cAIRCLM. 
      CONV3 = 1./(1000./28.966)*(101325./8.314/298.)*1e-6

!     MAP IN OR OUT:
!     =======================================================================
      IF (CTRL.EQ.0) GOTO 100
      IF (CTRL.EQ.1) GOTO 200
      
      STOP "ERROR in SS-MOSAIC MAP INOUT"

!     MAPPING IN:
!     =======================================================================
100   CONTINUE

!     FOR NUM.:
      NUM(1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_NUM_a01))*CONV3
      NUM(2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_NUM_a02))*CONV3
      NUM(3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_NUM_a03))*CONV3      
      NUM(4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_NUM_a04))*CONV3
!      NUM(5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_NUM_a05))*CONV3
!      NUM(6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_NUM_a06))*CONV3
!      NUM(7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_NUM_a07))*CONV3      
!      NUM(8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_NUM_a08))*CONV3

!     FOR GAS:
      moGAS(1)  = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_g))*CONV1(1)
      moGAS(2)  = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_g))*CONV1(2)
      moGAS(3)  = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_g))*CONV1(3)
      moGAS(4)  = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C00_g))*CONV1(4)
      moGAS(5)  = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C01_g))*CONV1(5)
      moGAS(6)  = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C02_g))*CONV1(6)
      moGAS(7)  = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C03_g))*CONV1(7)
      moGAS(8)  = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C04_g))*CONV1(8)
      moGAS(9)  = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C05_g))*CONV1(9)
      moGAS(10) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C06_g))*CONV1(10)
      moGAS(11) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C07_g))*CONV1(11)
      moGAS(12) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C08_g))*CONV1(12)
      moGAS(13) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C09_g))*CONV1(13)

      moGAS(14) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_g))*CONV1(14)
      moGAS(15) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_g))*CONV1(15)
      moGAS(16) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_g))*CONV1(16)
      moGAS(17) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_g))*CONV1(17)
      moGAS(18) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_g))*CONV1(18)
      moGAS(19) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_g))*CONV1(19)
      moGAS(20) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_g))*CONV1(20)
      moGAS(21) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_g))*CONV1(21)
      moGAS(22) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_g))*CONV1(22)
      moGAS(23) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_g))*CONV1(23)
      moGAS(24) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_g))*CONV1(24)
      moGAS(25) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_g))*CONV1(25)
      moGAS(26) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_g))*CONV1(26)

      moGAS(27) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_g))*CONV1(27)
      moGAS(28) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_g))*CONV1(28)
      moGAS(29) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_g))*CONV1(29)
      moGAS(30) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_g))*CONV1(30)
      moGAS(31) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_g))*CONV1(31)
      moGAS(32) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_g))*CONV1(32)
      moGAS(33) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_g))*CONV1(33)
      moGAS(34) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_g))*CONV1(34)
      moGAS(35) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_g))*CONV1(35)
      moGAS(36) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_g))*CONV1(36)
      moGAS(37) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_g))*CONV1(37)
      moGAS(38) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_g))*CONV1(38)
      moGAS(39) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_g))*CONV1(39)

      moGAS(40) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_g))*CONV1(40)
      moGAS(41) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_g))*CONV1(41)
      moGAS(42) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_g))*CONV1(42)
      moGAS(43) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_g))*CONV1(43)
      moGAS(44) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_g))*CONV1(44)
      moGAS(45) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_g))*CONV1(45)
      moGAS(46) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_g))*CONV1(46)
      moGAS(47) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_g))*CONV1(47)
      moGAS(48) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_g))*CONV1(48)
      moGAS(49) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_g))*CONV1(49)
      moGAS(50) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_g))*CONV1(50)
      moGAS(51) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_g))*CONV1(51)
      moGAS(52) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_g))*CONV1(52)

      moGAS(53) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_g))*CONV1(53)
      moGAS(54) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_g))*CONV1(54)
      moGAS(55) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_g))*CONV1(55)
      moGAS(56) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_g))*CONV1(56)
      moGAS(57) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_g))*CONV1(57)
      moGAS(58) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_g))*CONV1(58)
      moGAS(59) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_g))*CONV1(59)
      moGAS(60) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_g))*CONV1(60)
      moGAS(61) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_g))*CONV1(61)
      moGAS(62) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_g))*CONV1(62)
      moGAS(63) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_g))*CONV1(63)
      moGAS(64) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_g))*CONV1(64)
      moGAS(65) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_g))*CONV1(65)

      moGAS(66) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_g))*CONV1(66)
      moGAS(67) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_g))*CONV1(67)
      moGAS(68) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_g))*CONV1(68)
      moGAS(69) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_g))*CONV1(69)
      moGAS(70) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_g))*CONV1(70)
      moGAS(71) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_g))*CONV1(71)
      moGAS(72) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_g))*CONV1(72)
      moGAS(73) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_g))*CONV1(73)
      moGAS(74) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_g))*CONV1(74)
      moGAS(75) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_g))*CONV1(75)
      moGAS(76) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_g))*CONV1(76)
      moGAS(77) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_g))*CONV1(77)
      moGAS(78) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_g))*CONV1(78)

      moGAS(79) = DBLE(CHEM(iNOW,kNOW,jNOW,p_PCG1_F_C))*CONV1(79)

!     FOR AERO:
      moAERO(1,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_a01))*CONV2
      moAERO(1,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_a02))*CONV2
      moAERO(1,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_a03))*CONV2
      moAERO(1,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_a04))*CONV2
!      moAERO(1,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_a05))*CONV2
!      moAERO(1,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_a06))*CONV2
!      moAERO(1,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_a07))*CONV2
!      moAERO(1,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_a08))*CONV2

      moAERO(2,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_a01))*CONV2
      moAERO(2,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_a02))*CONV2
      moAERO(2,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_a03))*CONV2
      moAERO(2,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_a04))*CONV2
!      moAERO(2,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_a05))*CONV2
!      moAERO(2,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_a06))*CONV2
!      moAERO(2,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_a07))*CONV2
!      moAERO(2,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_a08))*CONV2

      moAERO(3,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_a01))*CONV2
      moAERO(3,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_a02))*CONV2
      moAERO(3,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_a03))*CONV2
      moAERO(3,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_a04))*CONV2
!      moAERO(3,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_a05))*CONV2
!      moAERO(3,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_a06))*CONV2
!      moAERO(3,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_a07))*CONV2
!      moAERO(3,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_a08))*CONV2

      moAERO(4,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C00_a01))*CONV2
      moAERO(4,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C00_a02))*CONV2
      moAERO(4,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C00_a03))*CONV2
      moAERO(4,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C00_a04))*CONV2
!      moAERO(4,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C00_a05))*CONV2
!      moAERO(4,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C00_a06))*CONV2
!      moAERO(4,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C00_a07))*CONV2
!      moAERO(4,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C00_a08))*CONV2

      moAERO(5,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C01_a01))*CONV2
      moAERO(5,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C01_a02))*CONV2
      moAERO(5,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C01_a03))*CONV2
      moAERO(5,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C01_a04))*CONV2
!      moAERO(5,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C01_a05))*CONV2
!      moAERO(5,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C01_a06))*CONV2
!      moAERO(5,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C01_a07))*CONV2
!      moAERO(5,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C01_a08))*CONV2

      moAERO(6,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C02_a01))*CONV2
      moAERO(6,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C02_a02))*CONV2
      moAERO(6,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C02_a03))*CONV2
      moAERO(6,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C02_a04))*CONV2
!      moAERO(6,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C02_a05))*CONV2
!      moAERO(6,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C02_a06))*CONV2
!      moAERO(6,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C02_a07))*CONV2
!      moAERO(6,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C02_a08))*CONV2

      moAERO(7,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C03_a01))*CONV2
      moAERO(7,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C03_a02))*CONV2
      moAERO(7,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C03_a03))*CONV2
      moAERO(7,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C03_a04))*CONV2
!      moAERO(7,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C03_a05))*CONV2
!      moAERO(7,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C03_a06))*CONV2
!      moAERO(7,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C03_a07))*CONV2
!      moAERO(7,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C03_a08))*CONV2

      moAERO(8,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C04_a01))*CONV2
      moAERO(8,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C04_a02))*CONV2
      moAERO(8,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C04_a03))*CONV2
      moAERO(8,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C04_a04))*CONV2
!      moAERO(8,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C04_a05))*CONV2
!      moAERO(8,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C04_a06))*CONV2
!      moAERO(8,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C04_a07))*CONV2
!      moAERO(8,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C04_a08))*CONV2

      moAERO(9,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C05_a01))*CONV2
      moAERO(9,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C05_a02))*CONV2
      moAERO(9,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C05_a03))*CONV2
      moAERO(9,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C05_a04))*CONV2
!      moAERO(9,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C05_a05))*CONV2
!      moAERO(9,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C05_a06))*CONV2
!      moAERO(9,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C05_a07))*CONV2
!      moAERO(9,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C05_a08))*CONV2

      moAERO(10,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C06_a01))*CONV2
      moAERO(10,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C06_a02))*CONV2
      moAERO(10,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C06_a03))*CONV2
      moAERO(10,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C06_a04))*CONV2
!      moAERO(10,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C06_a05))*CONV2
!      moAERO(10,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C06_a06))*CONV2
!      moAERO(10,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C06_a07))*CONV2
!      moAERO(10,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C06_a08))*CONV2
      
      moAERO(11,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C07_a01))*CONV2
      moAERO(11,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C07_a02))*CONV2
      moAERO(11,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C07_a03))*CONV2
      moAERO(11,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C07_a04))*CONV2
!      moAERO(11,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C07_a05))*CONV2
!      moAERO(11,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C07_a06))*CONV2
!      moAERO(11,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C07_a07))*CONV2
!      moAERO(11,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C07_a08))*CONV2

      moAERO(12,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C08_a01))*CONV2
      moAERO(12,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C08_a02))*CONV2
      moAERO(12,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C08_a03))*CONV2
      moAERO(12,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C08_a04))*CONV2
!      moAERO(12,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C08_a05))*CONV2
!      moAERO(12,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C08_a06))*CONV2
!      moAERO(12,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C08_a07))*CONV2
!      moAERO(12,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C08_a08))*CONV2

      moAERO(13,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C09_a01))*CONV2
      moAERO(13,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C09_a02))*CONV2
      moAERO(13,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C09_a03))*CONV2
      moAERO(13,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C09_a04))*CONV2
!      moAERO(13,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C09_a05))*CONV2
!      moAERO(13,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C09_a06))*CONV2
!      moAERO(13,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C09_a07))*CONV2
!      moAERO(13,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C09_a08))*CONV2
      
!     FOR AERO:
      moAERO(14,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_a01))*CONV2
      moAERO(14,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_a02))*CONV2
      moAERO(14,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_a03))*CONV2
      moAERO(14,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_a04))*CONV2
!      moAERO(14,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_a05))*CONV2
!      moAERO(14,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_a06))*CONV2
!      moAERO(14,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_a07))*CONV2
!      moAERO(14,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_a08))*CONV2

      moAERO(15,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_a01))*CONV2
      moAERO(15,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_a02))*CONV2
      moAERO(15,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_a03))*CONV2
      moAERO(15,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_a04))*CONV2
!      moAERO(15,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_a05))*CONV2
!      moAERO(15,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_a06))*CONV2
!      moAERO(15,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_a07))*CONV2
!      moAERO(15,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_a08))*CONV2

      moAERO(16,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_a01))*CONV2
      moAERO(16,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_a02))*CONV2
      moAERO(16,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_a03))*CONV2
      moAERO(16,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_a04))*CONV2
!      moAERO(16,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_a05))*CONV2
!      moAERO(16,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_a06))*CONV2
!      moAERO(16,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_a07))*CONV2
!      moAERO(16,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_a08))*CONV2

      moAERO(17,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_a01))*CONV2
      moAERO(17,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_a02))*CONV2
      moAERO(17,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_a03))*CONV2
      moAERO(17,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_a04))*CONV2
!      moAERO(17,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_a05))*CONV2
!      moAERO(17,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_a06))*CONV2
!      moAERO(17,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_a07))*CONV2
!      moAERO(17,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_a08))*CONV2

      moAERO(18,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_a01))*CONV2
      moAERO(18,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_a02))*CONV2
      moAERO(18,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_a03))*CONV2
      moAERO(18,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_a04))*CONV2
!      moAERO(18,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_a05))*CONV2
!      moAERO(18,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_a06))*CONV2
!      moAERO(18,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_a07))*CONV2
!      moAERO(18,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_a08))*CONV2

      moAERO(19,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_a01))*CONV2
      moAERO(19,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_a02))*CONV2
      moAERO(19,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_a03))*CONV2
      moAERO(19,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_a04))*CONV2
!      moAERO(19,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_a05))*CONV2
!      moAERO(19,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_a06))*CONV2
!      moAERO(19,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_a07))*CONV2
!      moAERO(19,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_a08))*CONV2

      moAERO(20,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_a01))*CONV2
      moAERO(20,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_a02))*CONV2
      moAERO(20,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_a03))*CONV2
      moAERO(20,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_a04))*CONV2
!      moAERO(20,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_a05))*CONV2
!      moAERO(20,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_a06))*CONV2
!      moAERO(20,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_a07))*CONV2
!      moAERO(20,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_a08))*CONV2

      moAERO(21,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_a01))*CONV2
      moAERO(21,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_a02))*CONV2
      moAERO(21,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_a03))*CONV2
      moAERO(21,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_a04))*CONV2
!      moAERO(21,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_a05))*CONV2
!      moAERO(21,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_a06))*CONV2
!      moAERO(21,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_a07))*CONV2
!      moAERO(21,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_a08))*CONV2

      moAERO(22,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_a01))*CONV2
      moAERO(22,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_a02))*CONV2
      moAERO(22,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_a03))*CONV2
      moAERO(22,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_a04))*CONV2
!      moAERO(22,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_a05))*CONV2
!      moAERO(22,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_a06))*CONV2
!      moAERO(22,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_a07))*CONV2
!      moAERO(22,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_a08))*CONV2

      moAERO(23,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_a01))*CONV2
      moAERO(23,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_a02))*CONV2
      moAERO(23,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_a03))*CONV2
      moAERO(23,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_a04))*CONV2
!      moAERO(23,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_a05))*CONV2
!      moAERO(23,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_a06))*CONV2
!      moAERO(23,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_a07))*CONV2
!      moAERO(23,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_a08))*CONV2
      
      moAERO(24,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_a01))*CONV2
      moAERO(24,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_a02))*CONV2
      moAERO(24,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_a03))*CONV2
      moAERO(24,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_a04))*CONV2
!      moAERO(24,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_a05))*CONV2
!      moAERO(24,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_a06))*CONV2
!      moAERO(24,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_a07))*CONV2
!      moAERO(24,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_a08))*CONV2

      moAERO(25,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_a01))*CONV2
      moAERO(25,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_a02))*CONV2
      moAERO(25,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_a03))*CONV2
      moAERO(25,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_a04))*CONV2
!      moAERO(25,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_a05))*CONV2
!      moAERO(25,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_a06))*CONV2
!      moAERO(25,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_a07))*CONV2
!      moAERO(25,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_a08))*CONV2

      moAERO(26,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_a01))*CONV2
      moAERO(26,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_a02))*CONV2
      moAERO(26,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_a03))*CONV2
      moAERO(26,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_a04))*CONV2
 !     moAERO(26,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_a05))*CONV2
 !     moAERO(26,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_a06))*CONV2
 !     moAERO(26,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_a07))*CONV2
 !     moAERO(26,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_a08))*CONV2

!     FOR AERO:
      moAERO(27,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_a01))*CONV2
      moAERO(27,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_a02))*CONV2
      moAERO(27,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_a03))*CONV2
      moAERO(27,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_a04))*CONV2
!      moAERO(27,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_a05))*CONV2
!      moAERO(27,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_a06))*CONV2
!      moAERO(27,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_a07))*CONV2
!      moAERO(27,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_a08))*CONV2

      moAERO(28,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_a01))*CONV2
      moAERO(28,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_a02))*CONV2
      moAERO(28,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_a03))*CONV2
      moAERO(28,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_a04))*CONV2
!      moAERO(28,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_a05))*CONV2
!      moAERO(28,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_a06))*CONV2
!      moAERO(28,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_a07))*CONV2
!      moAERO(28,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_a08))*CONV2

      moAERO(29,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_a01))*CONV2
      moAERO(29,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_a02))*CONV2
      moAERO(29,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_a03))*CONV2
      moAERO(29,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_a04))*CONV2
!      moAERO(29,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_a05))*CONV2
!      moAERO(29,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_a06))*CONV2
!      moAERO(29,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_a07))*CONV2
!      moAERO(29,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_a08))*CONV2

      moAERO(30,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_a01))*CONV2
      moAERO(30,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_a02))*CONV2
      moAERO(30,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_a03))*CONV2
      moAERO(30,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_a04))*CONV2
!      moAERO(30,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_a05))*CONV2
!      moAERO(30,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_a06))*CONV2
!      moAERO(30,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_a07))*CONV2
!      moAERO(30,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_a08))*CONV2

      moAERO(31,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_a01))*CONV2
      moAERO(31,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_a02))*CONV2
      moAERO(31,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_a03))*CONV2
      moAERO(31,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_a04))*CONV2
!      moAERO(31,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_a05))*CONV2
!      moAERO(31,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_a06))*CONV2
!      moAERO(31,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_a07))*CONV2
!      moAERO(31,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_a08))*CONV2

      moAERO(32,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_a01))*CONV2
      moAERO(32,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_a02))*CONV2
      moAERO(32,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_a03))*CONV2
      moAERO(32,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_a04))*CONV2
!      moAERO(32,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_a05))*CONV2
!      moAERO(32,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_a06))*CONV2
!      moAERO(32,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_a07))*CONV2
!      moAERO(32,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_a08))*CONV2

      moAERO(33,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_a01))*CONV2
      moAERO(33,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_a02))*CONV2
      moAERO(33,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_a03))*CONV2
      moAERO(33,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_a04))*CONV2
!      moAERO(33,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_a05))*CONV2
!      moAERO(33,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_a06))*CONV2
!      moAERO(33,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_a07))*CONV2
!      moAERO(33,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_a08))*CONV2

      moAERO(34,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_a01))*CONV2
      moAERO(34,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_a02))*CONV2
      moAERO(34,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_a03))*CONV2
      moAERO(34,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_a04))*CONV2
!      moAERO(34,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_a05))*CONV2
!      moAERO(34,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_a06))*CONV2
!      moAERO(34,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_a07))*CONV2
!      moAERO(34,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_a08))*CONV2

      moAERO(35,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_a01))*CONV2
      moAERO(35,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_a02))*CONV2
      moAERO(35,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_a03))*CONV2
      moAERO(35,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_a04))*CONV2
!      moAERO(35,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_a05))*CONV2
!      moAERO(35,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_a06))*CONV2
!      moAERO(35,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_a07))*CONV2
!      moAERO(35,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_a08))*CONV2

      moAERO(36,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_a01))*CONV2
      moAERO(36,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_a02))*CONV2
      moAERO(36,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_a03))*CONV2
      moAERO(36,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_a04))*CONV2
!      moAERO(36,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_a05))*CONV2
!      moAERO(36,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_a06))*CONV2
!      moAERO(36,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_a07))*CONV2
!      moAERO(36,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_a08))*CONV2
      
      moAERO(37,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_a01))*CONV2
      moAERO(37,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_a02))*CONV2
      moAERO(37,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_a03))*CONV2
      moAERO(37,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_a04))*CONV2
!      moAERO(37,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_a05))*CONV2
!      moAERO(37,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_a06))*CONV2
!      moAERO(37,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_a07))*CONV2
!      moAERO(37,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_a08))*CONV2

      moAERO(38,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_a01))*CONV2
      moAERO(38,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_a02))*CONV2
      moAERO(38,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_a03))*CONV2
      moAERO(38,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_a04))*CONV2
!      moAERO(38,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_a05))*CONV2
!      moAERO(38,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_a06))*CONV2
!      moAERO(38,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_a07))*CONV2
!      moAERO(38,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_a08))*CONV2

      moAERO(39,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_a01))*CONV2
      moAERO(39,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_a02))*CONV2
      moAERO(39,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_a03))*CONV2
      moAERO(39,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_a04))*CONV2
!      moAERO(39,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_a05))*CONV2
!      moAERO(39,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_a06))*CONV2
!      moAERO(39,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_a07))*CONV2
!      moAERO(39,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_a08))*CONV2

!     FOR AERO:
      moAERO(40,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_a01))*CONV2
      moAERO(40,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_a02))*CONV2
      moAERO(40,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_a03))*CONV2
      moAERO(40,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_a04))*CONV2
!      moAERO(40,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_a05))*CONV2
!      moAERO(40,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_a06))*CONV2
!      moAERO(40,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_a07))*CONV2
!      moAERO(40,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_a08))*CONV2

      moAERO(41,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_a01))*CONV2
      moAERO(41,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_a02))*CONV2
      moAERO(41,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_a03))*CONV2
      moAERO(41,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_a04))*CONV2
!      moAERO(41,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_a05))*CONV2
!      moAERO(41,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_a06))*CONV2
!      moAERO(41,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_a07))*CONV2
!      moAERO(41,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_a08))*CONV2

      moAERO(42,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_a01))*CONV2
      moAERO(42,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_a02))*CONV2
      moAERO(42,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_a03))*CONV2
      moAERO(42,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_a04))*CONV2
!      moAERO(42,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_a05))*CONV2
!      moAERO(42,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_a06))*CONV2
!      moAERO(42,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_a07))*CONV2
!      moAERO(42,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_a08))*CONV2

      moAERO(43,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_a01))*CONV2
      moAERO(43,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_a02))*CONV2
      moAERO(43,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_a03))*CONV2
      moAERO(43,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_a04))*CONV2
!      moAERO(43,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_a05))*CONV2
!      moAERO(43,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_a06))*CONV2
!      moAERO(43,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_a07))*CONV2
!      moAERO(43,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_a08))*CONV2

      moAERO(44,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_a01))*CONV2
      moAERO(44,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_a02))*CONV2
      moAERO(44,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_a03))*CONV2
      moAERO(44,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_a04))*CONV2
!      moAERO(44,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_a05))*CONV2
!      moAERO(44,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_a06))*CONV2
!      moAERO(44,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_a07))*CONV2
!      moAERO(44,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_a08))*CONV2

      moAERO(45,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_a01))*CONV2
      moAERO(45,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_a02))*CONV2
      moAERO(45,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_a03))*CONV2
      moAERO(45,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_a04))*CONV2
!      moAERO(45,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_a05))*CONV2
!      moAERO(45,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_a06))*CONV2
!      moAERO(45,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_a07))*CONV2
!      moAERO(45,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_a08))*CONV2

      moAERO(46,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_a01))*CONV2
      moAERO(46,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_a02))*CONV2
      moAERO(46,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_a03))*CONV2
      moAERO(46,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_a04))*CONV2
!      moAERO(46,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_a05))*CONV2
!      moAERO(46,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_a06))*CONV2
!      moAERO(46,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_a07))*CONV2
!      moAERO(46,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_a08))*CONV2

      moAERO(47,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_a01))*CONV2
      moAERO(47,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_a02))*CONV2
      moAERO(47,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_a03))*CONV2
      moAERO(47,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_a04))*CONV2
!      moAERO(47,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_a05))*CONV2
!      moAERO(47,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_a06))*CONV2
!      moAERO(47,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_a07))*CONV2
!      moAERO(47,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_a08))*CONV2

      moAERO(48,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_a01))*CONV2
      moAERO(48,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_a02))*CONV2
      moAERO(48,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_a03))*CONV2
      moAERO(48,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_a04))*CONV2
!      moAERO(48,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_a05))*CONV2
!      moAERO(48,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_a06))*CONV2
!      moAERO(48,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_a07))*CONV2
!      moAERO(48,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_a08))*CONV2

      moAERO(49,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_a01))*CONV2
      moAERO(49,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_a02))*CONV2
      moAERO(49,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_a03))*CONV2
      moAERO(49,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_a04))*CONV2
!      moAERO(49,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_a05))*CONV2
!      moAERO(49,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_a06))*CONV2
!      moAERO(49,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_a07))*CONV2
!      moAERO(49,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_a08))*CONV2
      
      moAERO(50,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_a01))*CONV2
      moAERO(50,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_a02))*CONV2
      moAERO(50,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_a03))*CONV2
      moAERO(50,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_a04))*CONV2
!      moAERO(50,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_a05))*CONV2
!      moAERO(50,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_a06))*CONV2
!      moAERO(50,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_a07))*CONV2
!      moAERO(50,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_a08))*CONV2

      moAERO(51,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_a01))*CONV2
      moAERO(51,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_a02))*CONV2
      moAERO(51,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_a03))*CONV2
      moAERO(51,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_a04))*CONV2
!      moAERO(51,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_a05))*CONV2
!      moAERO(51,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_a06))*CONV2
!      moAERO(51,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_a07))*CONV2
!      moAERO(51,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_a08))*CONV2

      moAERO(52,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_a01))*CONV2
      moAERO(52,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_a02))*CONV2
      moAERO(52,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_a03))*CONV2
      moAERO(52,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_a04))*CONV2
!      moAERO(52,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_a05))*CONV2
!      moAERO(52,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_a06))*CONV2
!      moAERO(52,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_a07))*CONV2
!      moAERO(52,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_a08))*CONV2

!     FOR AERO:
      moAERO(53,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_a01))*CONV2
      moAERO(53,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_a02))*CONV2
      moAERO(53,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_a03))*CONV2
      moAERO(53,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_a04))*CONV2
!      moAERO(53,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_a05))*CONV2
!      moAERO(53,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_a06))*CONV2
!      moAERO(53,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_a07))*CONV2
!      moAERO(53,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_a08))*CONV2

      moAERO(54,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_a01))*CONV2
      moAERO(54,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_a02))*CONV2
      moAERO(54,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_a03))*CONV2
      moAERO(54,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_a04))*CONV2
!      moAERO(54,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_a05))*CONV2
!      moAERO(54,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_a06))*CONV2
!      moAERO(54,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_a07))*CONV2
!      moAERO(54,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_a08))*CONV2

      moAERO(55,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_a01))*CONV2
      moAERO(55,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_a02))*CONV2
      moAERO(55,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_a03))*CONV2
      moAERO(55,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_a04))*CONV2
!      moAERO(55,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_a05))*CONV2
!      moAERO(55,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_a06))*CONV2
!      moAERO(55,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_a07))*CONV2
!      moAERO(55,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_a08))*CONV2

      moAERO(56,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_a01))*CONV2
      moAERO(56,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_a02))*CONV2
      moAERO(56,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_a03))*CONV2
      moAERO(56,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_a04))*CONV2
!      moAERO(56,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_a05))*CONV2
!      moAERO(56,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_a06))*CONV2
!      moAERO(56,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_a07))*CONV2
!      moAERO(56,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_a08))*CONV2

      moAERO(57,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_a01))*CONV2
      moAERO(57,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_a02))*CONV2
      moAERO(57,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_a03))*CONV2
      moAERO(57,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_a04))*CONV2
!      moAERO(57,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_a05))*CONV2
!      moAERO(57,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_a06))*CONV2
!      moAERO(57,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_a07))*CONV2
!      moAERO(57,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_a08))*CONV2

      moAERO(58,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_a01))*CONV2
      moAERO(58,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_a02))*CONV2
      moAERO(58,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_a03))*CONV2
      moAERO(58,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_a04))*CONV2
!      moAERO(58,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_a05))*CONV2
!      moAERO(58,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_a06))*CONV2
!      moAERO(58,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_a07))*CONV2
!      moAERO(58,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_a08))*CONV2

      moAERO(59,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_a01))*CONV2
      moAERO(59,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_a02))*CONV2
      moAERO(59,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_a03))*CONV2
      moAERO(59,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_a04))*CONV2
!      moAERO(59,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_a05))*CONV2
!      moAERO(59,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_a06))*CONV2
!      moAERO(59,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_a07))*CONV2
!      moAERO(59,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_a08))*CONV2

      moAERO(60,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_a01))*CONV2
      moAERO(60,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_a02))*CONV2
      moAERO(60,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_a03))*CONV2
      moAERO(60,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_a04))*CONV2
!      moAERO(60,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_a05))*CONV2
!      moAERO(60,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_a06))*CONV2
!      moAERO(60,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_a07))*CONV2
!      moAERO(60,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_a08))*CONV2

      moAERO(61,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_a01))*CONV2
      moAERO(61,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_a02))*CONV2
      moAERO(61,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_a03))*CONV2
      moAERO(61,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_a04))*CONV2
!      moAERO(61,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_a05))*CONV2
!      moAERO(61,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_a06))*CONV2
!      moAERO(61,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_a07))*CONV2
!      moAERO(61,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_a08))*CONV2

      moAERO(62,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_a01))*CONV2
      moAERO(62,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_a02))*CONV2
      moAERO(62,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_a03))*CONV2
      moAERO(62,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_a04))*CONV2
!      moAERO(62,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_a05))*CONV2
!      moAERO(62,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_a06))*CONV2
!      moAERO(62,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_a07))*CONV2
!      moAERO(62,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_a08))*CONV2
      
      moAERO(63,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_a01))*CONV2
      moAERO(63,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_a02))*CONV2
      moAERO(63,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_a03))*CONV2
      moAERO(63,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_a04))*CONV2
!      moAERO(63,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_a05))*CONV2
!      moAERO(63,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_a06))*CONV2
!      moAERO(63,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_a07))*CONV2
!      moAERO(63,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_a08))*CONV2

      moAERO(64,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_a01))*CONV2
      moAERO(64,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_a02))*CONV2
      moAERO(64,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_a03))*CONV2
      moAERO(64,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_a04))*CONV2
!      moAERO(64,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_a05))*CONV2
!      moAERO(64,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_a06))*CONV2
!      moAERO(64,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_a07))*CONV2
!      moAERO(64,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_a08))*CONV2

      moAERO(65,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_a01))*CONV2
      moAERO(65,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_a02))*CONV2
      moAERO(65,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_a03))*CONV2
      moAERO(65,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_a04))*CONV2
!      moAERO(65,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_a05))*CONV2
!      moAERO(65,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_a06))*CONV2
!      moAERO(65,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_a07))*CONV2
!      moAERO(65,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_a08))*CONV2

!     FOR AERO:
      moAERO(66,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_a01))*CONV2
      moAERO(66,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_a02))*CONV2
      moAERO(66,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_a03))*CONV2
      moAERO(66,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_a04))*CONV2
!      moAERO(66,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_a05))*CONV2
!      moAERO(66,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_a06))*CONV2
!      moAERO(66,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_a07))*CONV2
!      moAERO(66,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_a08))*CONV2

      moAERO(67,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_a01))*CONV2
      moAERO(67,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_a02))*CONV2
      moAERO(67,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_a03))*CONV2
      moAERO(67,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_a04))*CONV2
!      moAERO(67,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_a05))*CONV2
!      moAERO(67,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_a06))*CONV2
!      moAERO(67,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_a07))*CONV2
!      moAERO(67,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_a08))*CONV2

      moAERO(68,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_a01))*CONV2
      moAERO(68,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_a02))*CONV2
      moAERO(68,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_a03))*CONV2
      moAERO(68,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_a04))*CONV2
!      moAERO(68,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_a05))*CONV2
!      moAERO(68,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_a06))*CONV2
!      moAERO(68,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_a07))*CONV2
!      moAERO(68,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_a08))*CONV2

      moAERO(69,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_a01))*CONV2
      moAERO(69,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_a02))*CONV2
      moAERO(69,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_a03))*CONV2
      moAERO(69,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_a04))*CONV2
!      moAERO(69,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_a05))*CONV2
!      moAERO(69,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_a06))*CONV2
!      moAERO(69,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_a07))*CONV2
!      moAERO(69,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_a08))*CONV2

      moAERO(70,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_a01))*CONV2
      moAERO(70,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_a02))*CONV2
      moAERO(70,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_a03))*CONV2
      moAERO(70,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_a04))*CONV2
!      moAERO(70,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_a05))*CONV2
!      moAERO(70,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_a06))*CONV2
!      moAERO(70,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_a07))*CONV2
!      moAERO(70,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_a08))*CONV2

      moAERO(71,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_a01))*CONV2
      moAERO(71,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_a02))*CONV2
      moAERO(71,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_a03))*CONV2
      moAERO(71,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_a04))*CONV2
!      moAERO(71,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_a05))*CONV2
!      moAERO(71,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_a06))*CONV2
!      moAERO(71,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_a07))*CONV2
!      moAERO(71,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_a08))*CONV2

      moAERO(72,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_a01))*CONV2
      moAERO(72,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_a02))*CONV2
      moAERO(72,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_a03))*CONV2
      moAERO(72,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_a04))*CONV2
!      moAERO(72,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_a05))*CONV2
!      moAERO(72,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_a06))*CONV2
!      moAERO(72,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_a07))*CONV2
!      moAERO(72,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_a08))*CONV2

      moAERO(73,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_a01))*CONV2
      moAERO(73,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_a02))*CONV2
      moAERO(73,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_a03))*CONV2
      moAERO(73,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_a04))*CONV2
!      moAERO(73,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_a05))*CONV2
!      moAERO(73,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_a06))*CONV2
!      moAERO(73,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_a07))*CONV2
!      moAERO(73,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_a08))*CONV2

      moAERO(74,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_a01))*CONV2
      moAERO(74,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_a02))*CONV2
      moAERO(74,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_a03))*CONV2
      moAERO(74,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_a04))*CONV2
!      moAERO(74,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_a05))*CONV2
!      moAERO(74,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_a06))*CONV2
!      moAERO(74,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_a07))*CONV2
!      moAERO(74,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_a08))*CONV2

      moAERO(75,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_a01))*CONV2
      moAERO(75,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_a02))*CONV2
      moAERO(75,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_a03))*CONV2
      moAERO(75,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_a04))*CONV2
 !     moAERO(75,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_a05))*CONV2
 !     moAERO(75,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_a06))*CONV2
 !     moAERO(75,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_a07))*CONV2
 !     moAERO(75,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_a08))*CONV2
      
      moAERO(76,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_a01))*CONV2
      moAERO(76,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_a02))*CONV2
      moAERO(76,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_a03))*CONV2
      moAERO(76,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_a04))*CONV2
 !     moAERO(76,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_a05))*CONV2
 !     moAERO(76,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_a06))*CONV2
 !     moAERO(76,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_a07))*CONV2
 !     moAERO(76,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_a08))*CONV2

      moAERO(77,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_a01))*CONV2
      moAERO(77,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_a02))*CONV2
      moAERO(77,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_a03))*CONV2
      moAERO(77,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_a04))*CONV2
 !     moAERO(77,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_a05))*CONV2
 !     moAERO(77,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_a06))*CONV2
 !     moAERO(77,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_a07))*CONV2
 !     moAERO(77,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_a08))*CONV2

      moAERO(78,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_a01))*CONV2
      moAERO(78,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_a02))*CONV2
      moAERO(78,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_a03))*CONV2
      moAERO(78,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_a04))*CONV2
 !     moAERO(78,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_a05))*CONV2
 !     moAERO(78,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_a06))*CONV2
 !     moAERO(78,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_a07))*CONV2
 !     moAERO(78,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_a08))*CONV2

!     FOR AERO:
      moAERO(79,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_PCG1_F_C_a01))*CONV2
      moAERO(79,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_PCG1_F_C_a02))*CONV2
      moAERO(79,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_PCG1_F_C_a03))*CONV2
      moAERO(79,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_PCG1_F_C_a04))*CONV2

!     FOR OLIG:
      moOLIG(1,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_d01))*CONV2
      moOLIG(1,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_d02))*CONV2
      moOLIG(1,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_d03))*CONV2
      moOLIG(1,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_d04))*CONV2
!      moOLIG(1,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_d05))*CONV2
!      moOLIG(1,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_d06))*CONV2
!      moOLIG(1,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_d07))*CONV2
!      moOLIG(1,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_d08))*CONV2

      moOLIG(2,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_d01))*CONV2
      moOLIG(2,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_d02))*CONV2
      moOLIG(2,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_d03))*CONV2
      moOLIG(2,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_d04))*CONV2
!      moOLIG(2,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_d05))*CONV2
!      moOLIG(2,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_d06))*CONV2
!      moOLIG(2,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_d07))*CONV2
!      moOLIG(2,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_d08))*CONV2

      moOLIG(3,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_d01))*CONV2
      moOLIG(3,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_d02))*CONV2
      moOLIG(3,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_d03))*CONV2
      moOLIG(3,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_d04))*CONV2
!      moOLIG(3,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_d05))*CONV2
!      moOLIG(3,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_d06))*CONV2
!      moOLIG(3,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_d07))*CONV2
!      moOLIG(3,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_d08))*CONV2

      moOLIG(4,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C00_d01))*CONV2
      moOLIG(4,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C00_d02))*CONV2
      moOLIG(4,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C00_d03))*CONV2
      moOLIG(4,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C00_d04))*CONV2
!      moOLIG(4,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C00_d05))*CONV2
!      moOLIG(4,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C00_d06))*CONV2
!      moOLIG(4,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C00_d07))*CONV2
!      moOLIG(4,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C00_d08))*CONV2

      moOLIG(5,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C01_d01))*CONV2
      moOLIG(5,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C01_d02))*CONV2
      moOLIG(5,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C01_d03))*CONV2
      moOLIG(5,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C01_d04))*CONV2
!      moOLIG(5,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C01_d05))*CONV2
!      moOLIG(5,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C01_d06))*CONV2
!      moOLIG(5,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C01_d07))*CONV2
!      moOLIG(5,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C01_d08))*CONV2

      moOLIG(6,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C02_d01))*CONV2
      moOLIG(6,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C02_d02))*CONV2
      moOLIG(6,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C02_d03))*CONV2
      moOLIG(6,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C02_d04))*CONV2
!      moOLIG(6,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C02_d05))*CONV2
!      moOLIG(6,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C02_d06))*CONV2
!      moOLIG(6,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C02_d07))*CONV2
!      moOLIG(6,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C02_d08))*CONV2

      moOLIG(7,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C03_d01))*CONV2
      moOLIG(7,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C03_d02))*CONV2
      moOLIG(7,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C03_d03))*CONV2
      moOLIG(7,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C03_d04))*CONV2
!      moOLIG(7,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C03_d05))*CONV2
!      moOLIG(7,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C03_d06))*CONV2
!      moOLIG(7,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C03_d07))*CONV2
!      moOLIG(7,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C03_d08))*CONV2

      moOLIG(8,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C04_d01))*CONV2
      moOLIG(8,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C04_d02))*CONV2
      moOLIG(8,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C04_d03))*CONV2
      moOLIG(8,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C04_d04))*CONV2
!      moOLIG(8,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C04_d05))*CONV2
!      moOLIG(8,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C04_d06))*CONV2
!      moOLIG(8,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C04_d07))*CONV2
!      moOLIG(8,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C04_d08))*CONV2

      moOLIG(9,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C05_d01))*CONV2
      moOLIG(9,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C05_d02))*CONV2
      moOLIG(9,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C05_d03))*CONV2
      moOLIG(9,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C05_d04))*CONV2
!      moOLIG(9,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C05_d05))*CONV2
!      moOLIG(9,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C05_d06))*CONV2
!      moOLIG(9,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C05_d07))*CONV2
!      moOLIG(9,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C05_d08))*CONV2

      moOLIG(10,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C06_d01))*CONV2
      moOLIG(10,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C06_d02))*CONV2
      moOLIG(10,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C06_d03))*CONV2
      moOLIG(10,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C06_d04))*CONV2
!      moOLIG(10,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C06_d05))*CONV2
!      moOLIG(10,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C06_d06))*CONV2
!      moOLIG(10,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C06_d07))*CONV2
!      moOLIG(10,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C06_d08))*CONV2
      
      moOLIG(11,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C07_d01))*CONV2
      moOLIG(11,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C07_d02))*CONV2
      moOLIG(11,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C07_d03))*CONV2
      moOLIG(11,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C07_d04))*CONV2
!      moOLIG(11,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C07_d05))*CONV2
!      moOLIG(11,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C07_d06))*CONV2
!      moOLIG(11,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C07_d07))*CONV2
!      moOLIG(11,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C07_d08))*CONV2

      moOLIG(12,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C08_d01))*CONV2
      moOLIG(12,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C08_d02))*CONV2
      moOLIG(12,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C08_d03))*CONV2
      moOLIG(12,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C08_d04))*CONV2
!      moOLIG(12,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C08_d05))*CONV2
!      moOLIG(12,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C08_d06))*CONV2
!      moOLIG(12,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C08_d07))*CONV2
!      moOLIG(12,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C08_d08))*CONV2

      moOLIG(13,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C09_d01))*CONV2
      moOLIG(13,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C09_d02))*CONV2
      moOLIG(13,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C09_d03))*CONV2
      moOLIG(13,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C09_d04))*CONV2
!      moOLIG(13,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C09_d05))*CONV2
!      moOLIG(13,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C09_d06))*CONV2
!      moOLIG(13,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C09_d07))*CONV2
!      moOLIG(13,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_TERP_C09_d08))*CONV2
      
!     FOR OLIG:
!      moOLIG(14,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_d01))*CONV2
!      moOLIG(14,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_d02))*CONV2
!      moOLIG(14,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_d03))*CONV2
!      moOLIG(14,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_d04))*CONV2
!      moOLIG(14,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_d05))*CONV2
!      moOLIG(14,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_d06))*CONV2
!      moOLIG(14,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_d07))*CONV2
!      moOLIG(14,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_d08))*CONV2

!      moOLIG(15,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_d01))*CONV2
!      moOLIG(15,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_d02))*CONV2
!      moOLIG(15,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_d03))*CONV2
!      moOLIG(15,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_d04))*CONV2
!      moOLIG(15,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_d05))*CONV2
!      moOLIG(15,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_d06))*CONV2
!      moOLIG(15,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_d07))*CONV2
!      moOLIG(15,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_d08))*CONV2

!      moOLIG(16,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_d01))*CONV2
!      moOLIG(16,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_d02))*CONV2
!      moOLIG(16,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_d03))*CONV2
!      moOLIG(16,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_d04))*CONV2
!      moOLIG(16,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_d05))*CONV2
!      moOLIG(16,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_d06))*CONV2
!      moOLIG(16,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_d07))*CONV2
!      moOLIG(16,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_d08))*CONV2

!      moOLIG(17,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_d01))*CONV2
!      moOLIG(17,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_d02))*CONV2
!      moOLIG(17,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_d03))*CONV2
!      moOLIG(17,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_d04))*CONV2
!      moOLIG(17,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_d05))*CONV2
!      moOLIG(17,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_d06))*CONV2
!      moOLIG(17,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_d07))*CONV2
!      moOLIG(17,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_d08))*CONV2

!      moOLIG(18,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_d01))*CONV2
!      moOLIG(18,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_d02))*CONV2
!      moOLIG(18,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_d03))*CONV2
!      moOLIG(18,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_d04))*CONV2
!      moOLIG(18,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_d05))*CONV2
!      moOLIG(18,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_d06))*CONV2
!      moOLIG(18,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_d07))*CONV2
!      moOLIG(18,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_d08))*CONV2

!      moOLIG(19,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_d01))*CONV2
!      moOLIG(19,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_d02))*CONV2
!      moOLIG(19,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_d03))*CONV2
!      moOLIG(19,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_d04))*CONV2
!      moOLIG(19,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_d05))*CONV2
!      moOLIG(19,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_d06))*CONV2
!      moOLIG(19,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_d07))*CONV2
!      moOLIG(19,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_d08))*CONV2

!      moOLIG(20,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_d01))*CONV2
!      moOLIG(20,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_d02))*CONV2
!      moOLIG(20,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_d03))*CONV2
!      moOLIG(20,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_d04))*CONV2
!      moOLIG(20,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_d05))*CONV2
!      moOLIG(20,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_d06))*CONV2
!      moOLIG(20,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_d07))*CONV2
!      moOLIG(20,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_d08))*CONV2

!      moOLIG(21,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_d01))*CONV2
!      moOLIG(21,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_d02))*CONV2
!      moOLIG(21,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_d03))*CONV2
!      moOLIG(21,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_d04))*CONV2
!      moOLIG(21,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_d05))*CONV2
!      moOLIG(21,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_d06))*CONV2
!      moOLIG(21,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_d07))*CONV2
!      moOLIG(21,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_d08))*CONV2

!      moOLIG(22,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_d01))*CONV2
!      moOLIG(22,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_d02))*CONV2
!      moOLIG(22,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_d03))*CONV2
!      moOLIG(22,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_d04))*CONV2
!      moOLIG(22,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_d05))*CONV2
!      moOLIG(22,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_d06))*CONV2
!      moOLIG(22,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_d07))*CONV2
!      moOLIG(22,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_d08))*CONV2

!      moOLIG(23,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_d01))*CONV2
!      moOLIG(23,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_d02))*CONV2
!      moOLIG(23,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_d03))*CONV2
!      moOLIG(23,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_d04))*CONV2
!      moOLIG(23,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_d05))*CONV2
!      moOLIG(23,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_d06))*CONV2
!      moOLIG(23,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_d07))*CONV2
!      moOLIG(23,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_d08))*CONV2
      
!      moOLIG(24,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_d01))*CONV2
!      moOLIG(24,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_d02))*CONV2
!      moOLIG(24,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_d03))*CONV2
!      moOLIG(24,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_d04))*CONV2
!      moOLIG(25,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_d05))*CONV2
!      moOLIG(24,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_d06))*CONV2
!      moOLIG(24,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_d07))*CONV2
!      moOLIG(24,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_d08))*CONV2

!      moOLIG(25,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_d01))*CONV2
!      moOLIG(25,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_d02))*CONV2
!      moOLIG(25,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_d03))*CONV2
!      moOLIG(25,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_d04))*CONV2
!      moOLIG(25,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_d05))*CONV2
!      moOLIG(25,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_d06))*CONV2
!      moOLIG(25,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_d07))*CONV2
!      moOLIG(25,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_d08))*CONV2

!      moOLIG(26,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_d01))*CONV2
!      moOLIG(26,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_d02))*CONV2
!      moOLIG(26,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_d03))*CONV2
!      moOLIG(26,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_d04))*CONV2
!      moOLIG(26,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_d05))*CONV2
!      moOLIG(26,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_d06))*CONV2
!      moOLIG(26,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_d07))*CONV2
!      moOLIG(26,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_d08))*CONV2

!     FOR OLIG:
!      moOLIG(27,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_d01))*CONV2
!      moOLIG(27,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_d02))*CONV2
!      moOLIG(27,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_d03))*CONV2
!      moOLIG(27,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_d04))*CONV2
!      moOLIG(27,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_d05))*CONV2
!      moOLIG(27,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_d06))*CONV2
!      moOLIG(27,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_d07))*CONV2
!      moOLIG(27,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_d08))*CONV2

!      moOLIG(28,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_d01))*CONV2
!      moOLIG(28,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_d02))*CONV2
!      moOLIG(28,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_d03))*CONV2
!      moOLIG(28,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_d04))*CONV2
!      moOLIG(28,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_d05))*CONV2
!      moOLIG(28,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_d06))*CONV2
!      moOLIG(28,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_d07))*CONV2
!      moOLIG(28,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_d08))*CONV2

!      moOLIG(29,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_d01))*CONV2
!      moOLIG(29,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_d02))*CONV2
!      moOLIG(29,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_d03))*CONV2
!      moOLIG(29,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_d04))*CONV2
!      moOLIG(29,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_d05))*CONV2
!      moOLIG(29,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_d06))*CONV2
!      moOLIG(29,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_d07))*CONV2
!      moOLIG(29,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_d08))*CONV2

!      moOLIG(30,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_d01))*CONV2
!      moOLIG(30,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_d02))*CONV2
!      moOLIG(30,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_d03))*CONV2
!      moOLIG(30,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_d04))*CONV2
!      moOLIG(30,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_d05))*CONV2
!      moOLIG(30,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_d06))*CONV2
!      moOLIG(30,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_d07))*CONV2
!      moOLIG(30,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_d08))*CONV2

!      moOLIG(31,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_d01))*CONV2
!      moOLIG(31,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_d02))*CONV2
!      moOLIG(31,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_d03))*CONV2
!      moOLIG(31,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_d04))*CONV2
!      moOLIG(31,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_d05))*CONV2
!      moOLIG(31,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_d06))*CONV2
!      moOLIG(31,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_d07))*CONV2
!      moOLIG(31,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_d08))*CONV2

!      moOLIG(32,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_d01))*CONV2
!      moOLIG(32,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_d02))*CONV2
!      moOLIG(32,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_d03))*CONV2
!      moOLIG(32,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_d04))*CONV2
!      moOLIG(32,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_d05))*CONV2
!      moOLIG(32,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_d06))*CONV2
!      moOLIG(32,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_d07))*CONV2
!      moOLIG(32,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_d08))*CONV2

!      moOLIG(33,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_d01))*CONV2
!      moOLIG(33,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_d02))*CONV2
!      moOLIG(33,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_d03))*CONV2
!      moOLIG(33,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_d04))*CONV2
!      moOLIG(33,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_d05))*CONV2
!      moOLIG(33,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_d06))*CONV2
!      moOLIG(33,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_d07))*CONV2
!      moOLIG(33,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_d08))*CONV2

!      moOLIG(34,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_d01))*CONV2
!      moOLIG(34,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_d02))*CONV2
!      moOLIG(34,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_d03))*CONV2
!      moOLIG(34,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_d04))*CONV2
!      moOLIG(34,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_d05))*CONV2
!      moOLIG(34,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_d06))*CONV2
!      moOLIG(34,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_d07))*CONV2
!      moOLIG(34,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_d08))*CONV2

!      moOLIG(35,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_d01))*CONV2
!      moOLIG(35,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_d02))*CONV2
!      moOLIG(35,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_d03))*CONV2
!      moOLIG(35,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_d04))*CONV2
!      moOLIG(35,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_d05))*CONV2
!      moOLIG(35,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_d06))*CONV2
!      moOLIG(35,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_d07))*CONV2
!      moOLIG(35,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_d08))*CONV2

!      moOLIG(36,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_d01))*CONV2
!      moOLIG(36,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_d02))*CONV2
!      moOLIG(36,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_d03))*CONV2
!      moOLIG(36,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_d04))*CONV2
!      moOLIG(36,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_d05))*CONV2
!      moOLIG(36,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_d06))*CONV2
!      moOLIG(36,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_d07))*CONV2
!      moOLIG(36,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_d08))*CONV2
      
!      moOLIG(37,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_d01))*CONV2
!      moOLIG(37,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_d02))*CONV2
!      moOLIG(37,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_d03))*CONV2
!      moOLIG(37,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_d04))*CONV2
!      moOLIG(37,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_d05))*CONV2
!      moOLIG(37,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_d06))*CONV2
!      moOLIG(37,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_d07))*CONV2
!      moOLIG(37,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_d08))*CONV2

!      moOLIG(38,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_d01))*CONV2
!      moOLIG(38,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_d02))*CONV2
!      moOLIG(38,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_d03))*CONV2
!      moOLIG(38,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_d04))*CONV2
!      moOLIG(38,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_d05))*CONV2
!      moOLIG(38,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_d06))*CONV2
!      moOLIG(38,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_d07))*CONV2
!      moOLIG(38,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_d08))*CONV2

!      moOLIG(39,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_d01))*CONV2
!      moOLIG(39,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_d02))*CONV2
!      moOLIG(39,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_d03))*CONV2
!      moOLIG(39,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_d04))*CONV2
!      moOLIG(39,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_d05))*CONV2
!      moOLIG(39,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_d06))*CONV2
!      moOLIG(39,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_d07))*CONV2
!      moOLIG(39,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_d08))*CONV2

!     FOR OLIG:
!      moOLIG(40,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_d01))*CONV2
!      moOLIG(40,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_d02))*CONV2
!      moOLIG(40,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_d03))*CONV2
!      moOLIG(40,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_d04))*CONV2
!      moOLIG(40,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_d05))*CONV2
!      moOLIG(40,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_d06))*CONV2
!      moOLIG(40,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_d07))*CONV2
!      moOLIG(40,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_d08))*CONV2

!      moOLIG(41,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_d01))*CONV2
!      moOLIG(41,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_d02))*CONV2
!      moOLIG(41,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_d03))*CONV2
!      moOLIG(41,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_d04))*CONV2
!      moOLIG(41,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_d05))*CONV2
!      moOLIG(41,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_d06))*CONV2
!      moOLIG(41,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_d07))*CONV2
!      moOLIG(41,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_d08))*CONV2

!      moOLIG(42,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_d01))*CONV2
!      moOLIG(42,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_d02))*CONV2
!      moOLIG(42,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_d03))*CONV2
!      moOLIG(42,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_d04))*CONV2
!      moOLIG(42,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_d05))*CONV2
!      moOLIG(42,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_d06))*CONV2
!      moOLIG(42,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_d07))*CONV2
!      moOLIG(42,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_d08))*CONV2

!      moOLIG(43,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_d01))*CONV2
!      moOLIG(43,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_d02))*CONV2
!      moOLIG(43,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_d03))*CONV2
!      moOLIG(43,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_d04))*CONV2
!      moOLIG(43,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_d05))*CONV2
!      moOLIG(43,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_d06))*CONV2
!      moOLIG(43,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_d07))*CONV2
!      moOLIG(43,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_d08))*CONV2

!      moOLIG(44,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_d01))*CONV2
!      moOLIG(44,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_d02))*CONV2
!      moOLIG(44,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_d03))*CONV2
!      moOLIG(44,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_d04))*CONV2
!      moOLIG(44,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_d05))*CONV2
!      moOLIG(44,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_d06))*CONV2
!      moOLIG(44,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_d07))*CONV2
!      moOLIG(44,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_d08))*CONV2

!      moOLIG(45,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_d01))*CONV2
!      moOLIG(45,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_d02))*CONV2
!      moOLIG(45,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_d03))*CONV2
!      moOLIG(45,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_d04))*CONV2
!      moOLIG(45,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_d05))*CONV2
!      moOLIG(45,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_d06))*CONV2
!      moOLIG(45,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_d07))*CONV2
!      moOLIG(45,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_d08))*CONV2

!      moOLIG(46,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_d01))*CONV2
!      moOLIG(46,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_d02))*CONV2
!      moOLIG(46,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_d03))*CONV2
!      moOLIG(46,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_d04))*CONV2
!      moOLIG(46,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_d05))*CONV2
!      moOLIG(46,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_d06))*CONV2
!      moOLIG(46,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_d07))*CONV2
!      moOLIG(46,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_d08))*CONV2

!      moOLIG(47,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_d01))*CONV2
!      moOLIG(47,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_d02))*CONV2
!      moOLIG(47,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_d03))*CONV2
!      moOLIG(47,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_d04))*CONV2
!      moOLIG(47,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_d05))*CONV2
!      moOLIG(47,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_d06))*CONV2
!      moOLIG(47,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_d07))*CONV2
!      moOLIG(47,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_d08))*CONV2

!      moOLIG(48,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_d01))*CONV2
!      moOLIG(48,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_d02))*CONV2
!      moOLIG(48,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_d03))*CONV2
!      moOLIG(48,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_d04))*CONV2
!      moOLIG(48,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_d05))*CONV2
!      moOLIG(48,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_d06))*CONV2
!      moOLIG(48,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_d07))*CONV2
!      moOLIG(48,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_d08))*CONV2

!      moOLIG(49,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_d01))*CONV2
!      moOLIG(49,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_d02))*CONV2
!      moOLIG(49,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_d03))*CONV2
!      moOLIG(49,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_d04))*CONV2
!      moOLIG(49,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_d05))*CONV2
!      moOLIG(49,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_d06))*CONV2
!      moOLIG(49,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_d07))*CONV2
!      moOLIG(49,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_d08))*CONV2
      
!      moOLIG(50,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_d01))*CONV2
!      moOLIG(50,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_d02))*CONV2
!      moOLIG(50,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_d03))*CONV2
!      moOLIG(50,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_d04))*CONV2
!      moOLIG(50,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_d05))*CONV2
!      moOLIG(50,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_d06))*CONV2
!      moOLIG(50,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_d07))*CONV2
!      moOLIG(50,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_d08))*CONV2

!      moOLIG(51,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_d01))*CONV2
!      moOLIG(51,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_d02))*CONV2
!      moOLIG(51,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_d03))*CONV2
!      moOLIG(51,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_d04))*CONV2
!      moOLIG(51,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_d05))*CONV2
!      moOLIG(51,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_d06))*CONV2
!      moOLIG(51,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_d07))*CONV2
!      moOLIG(51,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_d08))*CONV2

!      moOLIG(52,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_d01))*CONV2
!      moOLIG(52,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_d02))*CONV2
!      moOLIG(52,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_d03))*CONV2
!      moOLIG(52,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_d04))*CONV2
!      moOLIG(52,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_d05))*CONV2
!      moOLIG(52,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_d06))*CONV2
!      moOLIG(52,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_d07))*CONV2
!      moOLIG(52,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_d08))*CONV2

!     FOR OLIG:
!      moOLIG(53,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_d01))*CONV2
!      moOLIG(53,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_d02))*CONV2
!      moOLIG(53,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_d03))*CONV2
!      moOLIG(53,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_d04))*CONV2
!      moOLIG(53,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_d05))*CONV2
!      moOLIG(53,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_d06))*CONV2
!      moOLIG(53,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_d07))*CONV2
!      moOLIG(53,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_d08))*CONV2

!      moOLIG(54,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_d01))*CONV2
!      moOLIG(54,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_d02))*CONV2
!      moOLIG(54,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_d03))*CONV2
!      moOLIG(54,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_d04))*CONV2
!      moOLIG(54,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_d05))*CONV2
!      moOLIG(54,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_d06))*CONV2
!      moOLIG(54,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_d07))*CONV2
!      moOLIG(54,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_d08))*CONV2

!      moOLIG(55,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_d01))*CONV2
!      moOLIG(55,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_d02))*CONV2
!      moOLIG(55,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_d03))*CONV2
!      moOLIG(55,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_d04))*CONV2
!      moOLIG(55,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_d05))*CONV2
!      moOLIG(55,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_d06))*CONV2
!      moOLIG(55,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_d07))*CONV2
!      moOLIG(55,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_d08))*CONV2

!      moOLIG(56,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_d01))*CONV2
!      moOLIG(56,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_d02))*CONV2
!      moOLIG(56,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_d03))*CONV2
!      moOLIG(56,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_d04))*CONV2
!      moOLIG(56,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_d05))*CONV2
!      moOLIG(56,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_d06))*CONV2
!      moOLIG(56,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_d07))*CONV2
!      moOLIG(56,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_d08))*CONV2

!      moOLIG(57,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_d01))*CONV2
!      moOLIG(57,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_d02))*CONV2
!      moOLIG(57,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_d03))*CONV2
!      moOLIG(57,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_d04))*CONV2
!      moOLIG(57,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_d05))*CONV2
!      moOLIG(57,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_d06))*CONV2
!      moOLIG(57,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_d07))*CONV2
!      moOLIG(57,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_d08))*CONV2

!      moOLIG(58,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_d01))*CONV2
!      moOLIG(58,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_d02))*CONV2
!      moOLIG(58,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_d03))*CONV2
!      moOLIG(58,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_d04))*CONV2
!      moOLIG(58,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_d05))*CONV2
!      moOLIG(58,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_d06))*CONV2
!      moOLIG(58,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_d07))*CONV2
!      moOLIG(58,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_d08))*CONV2

!      moOLIG(59,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_d01))*CONV2
!      moOLIG(59,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_d02))*CONV2
!      moOLIG(59,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_d03))*CONV2
!      moOLIG(59,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_d04))*CONV2
!      moOLIG(59,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_d05))*CONV2
!      moOLIG(59,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_d06))*CONV2
!      moOLIG(59,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_d07))*CONV2
!      moOLIG(59,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_d08))*CONV2

!      moOLIG(60,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_d01))*CONV2
!      moOLIG(60,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_d02))*CONV2
!      moOLIG(60,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_d03))*CONV2
!      moOLIG(60,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_d04))*CONV2
!      moOLIG(60,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_d05))*CONV2
!      moOLIG(60,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_d06))*CONV2
!      moOLIG(60,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_d07))*CONV2
!      moOLIG(60,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_d08))*CONV2

!      moOLIG(61,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_d01))*CONV2
!      moOLIG(61,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_d02))*CONV2
!      moOLIG(61,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_d03))*CONV2
!      moOLIG(61,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_d04))*CONV2
!      moOLIG(61,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_d05))*CONV2
!      moOLIG(61,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_d06))*CONV2
!      moOLIG(61,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_d07))*CONV2
!      moOLIG(61,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_d08))*CONV2

!      moOLIG(62,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_d01))*CONV2
!      moOLIG(62,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_d02))*CONV2
!      moOLIG(62,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_d03))*CONV2
!      moOLIG(62,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_d04))*CONV2
!      moOLIG(62,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_d05))*CONV2
!      moOLIG(62,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_d06))*CONV2
!      moOLIG(62,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_d07))*CONV2
!      moOLIG(62,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_d08))*CONV2
      
!      moOLIG(63,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_d01))*CONV2
!      moOLIG(63,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_d02))*CONV2
!      moOLIG(63,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_d03))*CONV2
!      moOLIG(63,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_d04))*CONV2
!      moOLIG(63,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_d05))*CONV2
!      moOLIG(63,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_d06))*CONV2
!      moOLIG(63,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_d07))*CONV2
!      moOLIG(63,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_d08))*CONV2

!      moOLIG(64,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_d01))*CONV2
!      moOLIG(64,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_d02))*CONV2
!      moOLIG(64,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_d03))*CONV2
!      moOLIG(64,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_d04))*CONV2
!      moOLIG(64,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_d05))*CONV2
!      moOLIG(64,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_d06))*CONV2
!      moOLIG(64,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_d07))*CONV2
!      moOLIG(64,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_d08))*CONV2

!      moOLIG(65,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_d01))*CONV2
!      moOLIG(65,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_d02))*CONV2
!      moOLIG(65,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_d03))*CONV2
!      moOLIG(65,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_d04))*CONV2
!      moOLIG(65,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_d05))*CONV2
!      moOLIG(65,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_d06))*CONV2
!      moOLIG(65,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_d07))*CONV2
!      moOLIG(65,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_d08))*CONV2

!     FOR OLIG:
!      moOLIG(66,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_d01))*CONV2
!      moOLIG(66,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_d02))*CONV2
!      moOLIG(66,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_d03))*CONV2
!      moOLIG(66,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_d04))*CONV2
!      moOLIG(66,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_d05))*CONV2
!      moOLIG(66,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_d06))*CONV2
!      moOLIG(66,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_d07))*CONV2
!      moOLIG(66,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_d08))*CONV2

!      moOLIG(67,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_d01))*CONV2
!      moOLIG(67,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_d02))*CONV2
!      moOLIG(67,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_d03))*CONV2
!      moOLIG(67,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_d04))*CONV2
!      moOLIG(67,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_d05))*CONV2
!      moOLIG(67,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_d06))*CONV2
!      moOLIG(67,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_d07))*CONV2
!      moOLIG(67,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_d08))*CONV2

!      moOLIG(68,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_d01))*CONV2
!      moOLIG(68,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_d02))*CONV2
!      moOLIG(68,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_d03))*CONV2
!      moOLIG(68,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_d04))*CONV2
!      moOLIG(68,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_d05))*CONV2
!      moOLIG(68,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_d06))*CONV2
!      moOLIG(68,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_d07))*CONV2
!      moOLIG(68,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_d08))*CONV2

!      moOLIG(69,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_d01))*CONV2
!      moOLIG(69,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_d02))*CONV2
!      moOLIG(69,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_d03))*CONV2
!      moOLIG(69,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_d04))*CONV2
!      moOLIG(69,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_d05))*CONV2
!      moOLIG(69,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_d06))*CONV2
!      moOLIG(69,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_d07))*CONV2
!      moOLIG(69,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_d08))*CONV2

!      moOLIG(70,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_d01))*CONV2
!      moOLIG(70,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_d02))*CONV2
!      moOLIG(70,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_d03))*CONV2
!      moOLIG(70,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_d04))*CONV2
!      moOLIG(70,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_d05))*CONV2
!      moOLIG(70,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_d06))*CONV2
!      moOLIG(70,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_d07))*CONV2
!      moOLIG(70,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_d08))*CONV2

!      moOLIG(71,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_d01))*CONV2
!      moOLIG(71,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_d02))*CONV2
!      moOLIG(71,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_d03))*CONV2
!      moOLIG(71,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_d04))*CONV2
!      moOLIG(71,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_d05))*CONV2
!      moOLIG(71,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_d06))*CONV2
!      moOLIG(71,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_d07))*CONV2
!      moOLIG(71,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_d08))*CONV2

!      moOLIG(72,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_d01))*CONV2
!      moOLIG(72,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_d02))*CONV2
!      moOLIG(72,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_d03))*CONV2
!      moOLIG(72,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_d04))*CONV2
!      moOLIG(72,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_d05))*CONV2
!      moOLIG(72,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_d06))*CONV2
!      moOLIG(72,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_d07))*CONV2
!      moOLIG(72,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_d08))*CONV2

!      moOLIG(73,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_d01))*CONV2
!      moOLIG(73,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_d02))*CONV2
!      moOLIG(73,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_d03))*CONV2
!      moOLIG(73,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_d04))*CONV2
!      moOLIG(73,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_d05))*CONV2
!      moOLIG(73,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_d06))*CONV2
!      moOLIG(73,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_d07))*CONV2
!      moOLIG(73,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_d08))*CONV2

!      moOLIG(74,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_d01))*CONV2
!      moOLIG(74,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_d02))*CONV2
!      moOLIG(74,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_d03))*CONV2
!      moOLIG(74,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_d04))*CONV2
!      moOLIG(74,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_d05))*CONV2
!      moOLIG(74,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_d06))*CONV2
!      moOLIG(74,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_d07))*CONV2
!      moOLIG(74,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_d08))*CONV2

!      moOLIG(75,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_d01))*CONV2
!      moOLIG(75,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_d02))*CONV2
!      moOLIG(75,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_d03))*CONV2
!      moOLIG(75,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_d04))*CONV2
!      moOLIG(75,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_d05))*CONV2
!      moOLIG(75,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_d06))*CONV2
!      moOLIG(75,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_d07))*CONV2
!      moOLIG(75,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_d08))*CONV2
      
!      moOLIG(76,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_d01))*CONV2
!      moOLIG(76,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_d02))*CONV2
!      moOLIG(76,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_d03))*CONV2
!      moOLIG(76,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_d04))*CONV2
!      moOLIG(76,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_d05))*CONV2
!      moOLIG(76,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_d06))*CONV2
!      moOLIG(76,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_d07))*CONV2
!      moOLIG(76,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_d08))*CONV2

!      moOLIG(77,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_d01))*CONV2
!      moOLIG(77,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_d02))*CONV2
!      moOLIG(77,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_d03))*CONV2
!      moOLIG(77,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_d04))*CONV2
!      moOLIG(77,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_d05))*CONV2
!      moOLIG(77,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_d06))*CONV2
!      moOLIG(77,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_d07))*CONV2
!      moOLIG(77,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_d08))*CONV2

!      moOLIG(78,1) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_d01))*CONV2
!      moOLIG(78,2) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_d02))*CONV2
!      moOLIG(78,3) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_d03))*CONV2
!      moOLIG(78,4) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_d04))*CONV2
!      moOLIG(78,5) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_d05))*CONV2
!      moOLIG(78,6) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_d06))*CONV2
!      moOLIG(78,7) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_d07))*CONV2
!      moOLIG(78,8) = DBLE(CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_d08))*CONV2

      RETURN

!     MAPPING OUT:
!     =======================================================================
200   CONTINUE

!     FOR NUM.:
      CHEM(iNOW,kNOW,jNOW,p_NUM_a01) = REAL(NUM(1))/CONV3
      CHEM(iNOW,kNOW,jNOW,p_NUM_a02) = REAL(NUM(2))/CONV3
      CHEM(iNOW,kNOW,jNOW,p_NUM_a03) = REAL(NUM(3))/CONV3
      CHEM(iNOW,kNOW,jNOW,p_NUM_a04) = REAL(NUM(4))/CONV3
!      CHEM(iNOW,kNOW,jNOW,p_NUM_a05) = REAL(NUM(5)/CONV3)
!      CHEM(iNOW,kNOW,jNOW,p_NUM_a06) = REAL(NUM(6)/CONV3)
!      CHEM(iNOW,kNOW,jNOW,p_NUM_a07) = REAL(NUM(7)/CONV3)
!      CHEM(iNOW,kNOW,jNOW,p_NUM_a08) = REAL(NUM(8)/CONV3)
      
!     FOR GAS:
      CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_g) = REAL(moGAS(1))/CONV1(1)
      CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_g) = REAL(moGAS(2))/CONV1(2)
      CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_g) = REAL(moGAS(3))/CONV1(3)
      CHEM(iNOW,kNOW,jNOW,p_TERP_C00_g) = REAL(moGAS(4))/CONV1(4)
      CHEM(iNOW,kNOW,jNOW,p_TERP_C01_g) = REAL(moGAS(5))/CONV1(5)
      CHEM(iNOW,kNOW,jNOW,p_TERP_C02_g) = REAL(moGAS(6))/CONV1(6)
      CHEM(iNOW,kNOW,jNOW,p_TERP_C03_g) = REAL(moGAS(7))/CONV1(7)
      CHEM(iNOW,kNOW,jNOW,p_TERP_C04_g) = REAL(moGAS(8))/CONV1(8)
      CHEM(iNOW,kNOW,jNOW,p_TERP_C05_g) = REAL(moGAS(9))/CONV1(9)
      CHEM(iNOW,kNOW,jNOW,p_TERP_C06_g) = REAL(moGAS(10))/CONV1(10)
      CHEM(iNOW,kNOW,jNOW,p_TERP_C07_g) = REAL(moGAS(11))/CONV1(11)
      CHEM(iNOW,kNOW,jNOW,p_TERP_C08_g) = REAL(moGAS(12))/CONV1(12)
      CHEM(iNOW,kNOW,jNOW,p_TERP_C09_g) = REAL(moGAS(13))/CONV1(13)
      
      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_g) = REAL(moGAS(14))/CONV1(14)
      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_g) = REAL(moGAS(15))/CONV1(15)
      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_g) = REAL(moGAS(16))/CONV1(16)
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_g) = REAL(moGAS(17))/CONV1(17)
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_g) = REAL(moGAS(18))/CONV1(18)
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_g) = REAL(moGAS(19))/CONV1(19)
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_g) = REAL(moGAS(20))/CONV1(20)
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_g) = REAL(moGAS(21))/CONV1(21)
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_g) = REAL(moGAS(22))/CONV1(22)
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_g) = REAL(moGAS(23))/CONV1(23)
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_g) = REAL(moGAS(24))/CONV1(24)
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_g) = REAL(moGAS(25))/CONV1(25)
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_g) = REAL(moGAS(26))/CONV1(26)

      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_g) = REAL(moGAS(27))/CONV1(27)
      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_g) = REAL(moGAS(28))/CONV1(28)
      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_g) = REAL(moGAS(29))/CONV1(29)
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_g) = REAL(moGAS(30))/CONV1(30)
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_g) = REAL(moGAS(31))/CONV1(31)
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_g) = REAL(moGAS(32))/CONV1(32)
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_g) = REAL(moGAS(33))/CONV1(33)
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_g) = REAL(moGAS(34))/CONV1(34)
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_g) = REAL(moGAS(35))/CONV1(35)
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_g) = REAL(moGAS(36))/CONV1(36)
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_g) = REAL(moGAS(37))/CONV1(37)
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_g) = REAL(moGAS(38))/CONV1(38)
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_g) = REAL(moGAS(39))/CONV1(39)

      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_g) = REAL(moGAS(40))/CONV1(40)
      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_g) = REAL(moGAS(41))/CONV1(41)
      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_g) = REAL(moGAS(42))/CONV1(42)
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_g) = REAL(moGAS(43))/CONV1(43)
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_g) = REAL(moGAS(44))/CONV1(44)
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_g) = REAL(moGAS(45))/CONV1(45)
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_g) = REAL(moGAS(45))/CONV1(46)
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_g) = REAL(moGAS(47))/CONV1(47)
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_g) = REAL(moGAS(48))/CONV1(48)
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_g) = REAL(moGAS(49))/CONV1(49)
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_g) = REAL(moGAS(50))/CONV1(50)
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_g) = REAL(moGAS(51))/CONV1(51)
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_g) = REAL(moGAS(52))/CONV1(52)

      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_g) = REAL(moGAS(53))/CONV1(53)
      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_g) = REAL(moGAS(54))/CONV1(54)
      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_g) = REAL(moGAS(55))/CONV1(55)
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_g) = REAL(moGAS(56))/CONV1(56)
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_g) = REAL(moGAS(57))/CONV1(57)
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_g) = REAL(moGAS(58))/CONV1(58)
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_g) = REAL(moGAS(59))/CONV1(59)
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_g) = REAL(moGAS(60))/CONV1(60)
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_g) = REAL(moGAS(61))/CONV1(61)
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_g) = REAL(moGAS(62))/CONV1(62)
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_g) = REAL(moGAS(63))/CONV1(63)
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_g) = REAL(moGAS(64))/CONV1(64)
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_g) = REAL(moGAS(65))/CONV1(65)

      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_g) = REAL(moGAS(66))/CONV1(66)
      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_g) = REAL(moGAS(67))/CONV1(67)
      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_g) = REAL(moGAS(68))/CONV1(68)
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_g) = REAL(moGAS(69))/CONV1(69)
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_g) = REAL(moGAS(70))/CONV1(70)
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_g) = REAL(moGAS(71))/CONV1(71)
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_g) = REAL(moGAS(72))/CONV1(72)
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_g) = REAL(moGAS(73))/CONV1(73)
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_g) = REAL(moGAS(74))/CONV1(74)
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_g) = REAL(moGAS(75))/CONV1(75)
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_g) = REAL(moGAS(76))/CONV1(76)
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_g) = REAL(moGAS(77))/CONV1(77)
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_g) = REAL(moGAS(78))/CONV1(78)

      CHEM(iNOW,kNOW,jNOW,p_PCG1_F_C) = REAL(moGAS(79))/CONV1(79)

!     FOR AERO:
      CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_a01) = REAL(moAERO(1,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_a02) = REAL(moAERO(1,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_a03) = REAL(moAERO(1,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_a04) = REAL(moAERO(1,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_a05) = REAL(moAERO(1,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_a06) = REAL(moAERO(1,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_a07) = REAL(moAERO(1,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_a08) = REAL(moAERO(1,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_a01) = REAL(moAERO(2,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_a02) = REAL(moAERO(2,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_a03) = REAL(moAERO(2,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_a04) = REAL(moAERO(2,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_a05) = REAL(moAERO(2,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_a06) = REAL(moAERO(2,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_a07) = REAL(moAERO(2,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_a08) = REAL(moAERO(2,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_a01) = REAL(moAERO(3,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_a02) = REAL(moAERO(3,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_a03) = REAL(moAERO(3,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_a04) = REAL(moAERO(3,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_a05) = REAL(moAERO(3,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_a06) = REAL(moAERO(3,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_a07) = REAL(moAERO(3,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_a08) = REAL(moAERO(3,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_TERP_C00_a01) = REAL(moAERO(4,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C00_a02) = REAL(moAERO(4,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C00_a03) = REAL(moAERO(4,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C00_a04) = REAL(moAERO(4,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C00_a05) = REAL(moAERO(4,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C00_a06) = REAL(moAERO(4,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C00_a07) = REAL(moAERO(4,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C00_a08) = REAL(moAERO(4,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_TERP_C01_a01) = REAL(moAERO(5,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C01_a02) = REAL(moAERO(5,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C01_a03) = REAL(moAERO(5,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C01_a04) = REAL(moAERO(5,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C01_a05) = REAL(moAERO(5,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C01_a06) = REAL(moAERO(5,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C01_a07) = REAL(moAERO(5,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C01_a08) = REAL(moAERO(5,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_TERP_C02_a01) = REAL(moAERO(6,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C02_a02) = REAL(moAERO(6,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C02_a03) = REAL(moAERO(6,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C02_a04) = REAL(moAERO(6,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C02_a05) = REAL(moAERO(6,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C02_a06) = REAL(moAERO(6,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C02_a07) = REAL(moAERO(6,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C02_a08) = REAL(moAERO(6,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_TERP_C03_a01) = REAL(moAERO(7,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C03_a02) = REAL(moAERO(7,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C03_a03) = REAL(moAERO(7,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C03_a04) = REAL(moAERO(7,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C03_a05) = REAL(moAERO(7,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C03_a06) = REAL(moAERO(7,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C03_a07) = REAL(moAERO(7,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C03_a08) = REAL(moAERO(7,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_TERP_C04_a01) = REAL(moAERO(8,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C04_a02) = REAL(moAERO(8,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C04_a03) = REAL(moAERO(8,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C04_a04) = REAL(moAERO(8,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C04_a05) = REAL(moAERO(8,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C04_a06) = REAL(moAERO(8,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C04_a07) = REAL(moAERO(8,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C04_a08) = REAL(moAERO(8,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_TERP_C05_a01) = REAL(moAERO(9,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C05_a02) = REAL(moAERO(9,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C05_a03) = REAL(moAERO(9,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C05_a04) = REAL(moAERO(9,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C05_a05) = REAL(moAERO(9,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C05_a06) = REAL(moAERO(9,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C05_a07) = REAL(moAERO(9,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C05_a08) = REAL(moAERO(9,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_TERP_C06_a01) = REAL(moAERO(10,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C06_a02) = REAL(moAERO(10,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C06_a03) = REAL(moAERO(10,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C06_a04) = REAL(moAERO(10,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C06_a05) = REAL(moAERO(10,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C06_a06) = REAL(moAERO(10,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C06_a07) = REAL(moAERO(10,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C06_a08) = REAL(moAERO(10,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_TERP_C07_a01) = REAL(moAERO(11,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C07_a02) = REAL(moAERO(11,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C07_a03) = REAL(moAERO(11,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C07_a04) = REAL(moAERO(11,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C07_a05) = REAL(moAERO(11,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C07_a06) = REAL(moAERO(11,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C07_a07) = REAL(moAERO(11,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C07_a08) = REAL(moAERO(11,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_TERP_C08_a01) = REAL(moAERO(12,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C08_a02) = REAL(moAERO(12,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C08_a03) = REAL(moAERO(12,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C08_a04) = REAL(moAERO(12,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C08_a05) = REAL(moAERO(12,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C08_a06) = REAL(moAERO(12,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C08_a07) = REAL(moAERO(12,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C08_a08) = REAL(moAERO(12,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_TERP_C09_a01) = REAL(moAERO(13,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C09_a02) = REAL(moAERO(13,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C09_a03) = REAL(moAERO(13,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C09_a04) = REAL(moAERO(13,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C09_a05) = REAL(moAERO(13,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C09_a06) = REAL(moAERO(13,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C09_a07) = REAL(moAERO(13,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C09_a08) = REAL(moAERO(13,8))/CONV2
      
!     FOR AERO:
      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_a01) = REAL(moAERO(14,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_a02) = REAL(moAERO(14,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_a03) = REAL(moAERO(14,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_a04) = REAL(moAERO(14,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_a05) = REAL(moAERO(14,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_a06) = REAL(moAERO(14,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_a07) = REAL(moAERO(14,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_a08) = REAL(moAERO(14,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_a01) = REAL(moAERO(15,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_a02) = REAL(moAERO(15,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_a03) = REAL(moAERO(15,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_a04) = REAL(moAERO(15,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_a05) = REAL(moAERO(15,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_a06) = REAL(moAERO(15,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_a07) = REAL(moAERO(15,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_a08) = REAL(moAERO(15,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_a01) = REAL(moAERO(16,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_a02) = REAL(moAERO(16,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_a03) = REAL(moAERO(16,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_a04) = REAL(moAERO(16,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_a05) = REAL(moAERO(16,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_a06) = REAL(moAERO(16,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_a07) = REAL(moAERO(16,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_a08) = REAL(moAERO(16,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_a01) = REAL(moAERO(17,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_a02) = REAL(moAERO(17,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_a03) = REAL(moAERO(17,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_a04) = REAL(moAERO(17,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_a05) = REAL(moAERO(17,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_a06) = REAL(moAERO(17,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_a07) = REAL(moAERO(17,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_a08) = REAL(moAERO(17,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_a01) = REAL(moAERO(18,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_a02) = REAL(moAERO(18,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_a03) = REAL(moAERO(18,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_a04) = REAL(moAERO(18,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_a05) = REAL(moAERO(18,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_a06) = REAL(moAERO(18,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_a07) = REAL(moAERO(18,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_a08) = REAL(moAERO(18,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_a01) = REAL(moAERO(19,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_a02) = REAL(moAERO(19,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_a03) = REAL(moAERO(19,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_a04) = REAL(moAERO(19,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_a05) = REAL(moAERO(19,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_a06) = REAL(moAERO(19,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_a07) = REAL(moAERO(19,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_a08) = REAL(moAERO(19,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_a01) = REAL(moAERO(20,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_a02) = REAL(moAERO(20,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_a03) = REAL(moAERO(20,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_a04) = REAL(moAERO(20,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_a05) = REAL(moAERO(20,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_a06) = REAL(moAERO(20,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_a07) = REAL(moAERO(20,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_a08) = REAL(moAERO(20,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_a01) = REAL(moAERO(21,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_a02) = REAL(moAERO(21,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_a03) = REAL(moAERO(21,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_a04) = REAL(moAERO(21,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_a05) = REAL(moAERO(21,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_a06) = REAL(moAERO(21,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_a07) = REAL(moAERO(21,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_a08) = REAL(moAERO(21,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_a01) = REAL(moAERO(22,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_a02) = REAL(moAERO(22,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_a03) = REAL(moAERO(22,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_a04) = REAL(moAERO(22,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_a05) = REAL(moAERO(22,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_a06) = REAL(moAERO(22,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_a07) = REAL(moAERO(22,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_a08) = REAL(moAERO(22,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_a01) = REAL(moAERO(23,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_a02) = REAL(moAERO(23,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_a03) = REAL(moAERO(23,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_a04) = REAL(moAERO(23,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_a05) = REAL(moAERO(23,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_a06) = REAL(moAERO(23,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_a07) = REAL(moAERO(23,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_a08) = REAL(moAERO(23,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_a01) = REAL(moAERO(24,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_a02) = REAL(moAERO(24,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_a03) = REAL(moAERO(24,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_a04) = REAL(moAERO(24,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_a05) = REAL(moAERO(24,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_a06) = REAL(moAERO(24,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_a07) = REAL(moAERO(24,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_a08) = REAL(moAERO(24,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_a01) = REAL(moAERO(25,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_a02) = REAL(moAERO(25,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_a03) = REAL(moAERO(25,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_a04) = REAL(moAERO(25,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_a05) = REAL(moAERO(25,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_a06) = REAL(moAERO(25,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_a07) = REAL(moAERO(25,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_a08) = REAL(moAERO(25,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_a01) = REAL(moAERO(26,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_a02) = REAL(moAERO(26,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_a03) = REAL(moAERO(26,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_a04) = REAL(moAERO(26,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_a05) = REAL(moAERO(26,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_a06) = REAL(moAERO(26,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_a07) = REAL(moAERO(26,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_a08) = REAL(moAERO(26,8))/CONV2
      
!     FOR AERO:
      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_a01) = REAL(moAERO(27,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_a02) = REAL(moAERO(27,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_a03) = REAL(moAERO(27,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_a04) = REAL(moAERO(27,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_a05) = REAL(moAERO(27,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_a06) = REAL(moAERO(27,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_a07) = REAL(moAERO(27,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_a08) = REAL(moAERO(27,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_a01) = REAL(moAERO(28,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_a02) = REAL(moAERO(28,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_a03) = REAL(moAERO(28,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_a04) = REAL(moAERO(28,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_a05) = REAL(moAERO(28,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_a06) = REAL(moAERO(28,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_a07) = REAL(moAERO(28,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_a08) = REAL(moAERO(28,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_a01) = REAL(moAERO(29,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_a02) = REAL(moAERO(29,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_a03) = REAL(moAERO(29,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_a04) = REAL(moAERO(29,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_a05) = REAL(moAERO(29,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_a06) = REAL(moAERO(29,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_a07) = REAL(moAERO(29,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_a08) = REAL(moAERO(29,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_a01) = REAL(moAERO(30,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_a02) = REAL(moAERO(30,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_a03) = REAL(moAERO(30,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_a04) = REAL(moAERO(30,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_a05) = REAL(moAERO(30,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_a06) = REAL(moAERO(30,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_a07) = REAL(moAERO(30,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_a08) = REAL(moAERO(30,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_a01) = REAL(moAERO(31,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_a02) = REAL(moAERO(31,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_a03) = REAL(moAERO(31,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_a04) = REAL(moAERO(31,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_a05) = REAL(moAERO(31,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_a06) = REAL(moAERO(31,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_a07) = REAL(moAERO(31,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_a08) = REAL(moAERO(31,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_a01) = REAL(moAERO(32,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_a02) = REAL(moAERO(32,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_a03) = REAL(moAERO(32,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_a04) = REAL(moAERO(32,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_a05) = REAL(moAERO(32,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_a06) = REAL(moAERO(32,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_a07) = REAL(moAERO(32,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_a08) = REAL(moAERO(32,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_a01) = REAL(moAERO(33,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_a02) = REAL(moAERO(33,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_a03) = REAL(moAERO(33,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_a04) = REAL(moAERO(33,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_a05) = REAL(moAERO(33,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_a06) = REAL(moAERO(33,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_a07) = REAL(moAERO(33,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_a08) = REAL(moAERO(33,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_a01) = REAL(moAERO(34,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_a02) = REAL(moAERO(34,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_a03) = REAL(moAERO(34,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_a04) = REAL(moAERO(34,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_a05) = REAL(moAERO(34,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_a06) = REAL(moAERO(34,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_a07) = REAL(moAERO(34,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_a08) = REAL(moAERO(34,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_a01) = REAL(moAERO(35,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_a02) = REAL(moAERO(35,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_a03) = REAL(moAERO(35,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_a04) = REAL(moAERO(35,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_a05) = REAL(moAERO(35,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_a06) = REAL(moAERO(35,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_a07) = REAL(moAERO(35,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_a08) = REAL(moAERO(35,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_a01) = REAL(moAERO(36,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_a02) = REAL(moAERO(36,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_a03) = REAL(moAERO(36,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_a04) = REAL(moAERO(36,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_a05) = REAL(moAERO(36,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_a06) = REAL(moAERO(36,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_a07) = REAL(moAERO(36,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_a08) = REAL(moAERO(36,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_a01) = REAL(moAERO(37,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_a02) = REAL(moAERO(37,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_a03) = REAL(moAERO(37,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_a04) = REAL(moAERO(37,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_a05) = REAL(moAERO(37,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_a06) = REAL(moAERO(37,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_a07) = REAL(moAERO(37,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_a08) = REAL(moAERO(37,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_a01) = REAL(moAERO(38,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_a02) = REAL(moAERO(38,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_a03) = REAL(moAERO(38,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_a04) = REAL(moAERO(38,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_a05) = REAL(moAERO(38,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_a06) = REAL(moAERO(38,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_a07) = REAL(moAERO(38,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_a08) = REAL(moAERO(38,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_a01) = REAL(moAERO(39,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_a02) = REAL(moAERO(39,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_a03) = REAL(moAERO(39,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_a04) = REAL(moAERO(39,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_a05) = REAL(moAERO(39,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_a06) = REAL(moAERO(39,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_a07) = REAL(moAERO(39,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_a08) = REAL(moAERO(39,8))/CONV2

!     FOR AERO:
      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_a01) = REAL(moAERO(40,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_a02) = REAL(moAERO(40,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_a03) = REAL(moAERO(40,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_a04) = REAL(moAERO(40,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_a05) = REAL(moAERO(40,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_a06) = REAL(moAERO(40,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_a07) = REAL(moAERO(40,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_a08) = REAL(moAERO(40,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_a01) = REAL(moAERO(41,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_a02) = REAL(moAERO(41,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_a03) = REAL(moAERO(41,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_a04) = REAL(moAERO(41,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_a05) = REAL(moAERO(41,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_a06) = REAL(moAERO(41,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_a07) = REAL(moAERO(41,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_a08) = REAL(moAERO(41,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_a01) = REAL(moAERO(42,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_a02) = REAL(moAERO(42,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_a03) = REAL(moAERO(42,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_a04) = REAL(moAERO(42,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_a05) = REAL(moAERO(42,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_a06) = REAL(moAERO(42,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_a07) = REAL(moAERO(42,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_a08) = REAL(moAERO(42,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_a01) = REAL(moAERO(43,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_a02) = REAL(moAERO(43,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_a03) = REAL(moAERO(43,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_a04) = REAL(moAERO(43,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_a05) = REAL(moAERO(43,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_a06) = REAL(moAERO(43,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_a07) = REAL(moAERO(43,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_a08) = REAL(moAERO(43,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_a01) = REAL(moAERO(44,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_a02) = REAL(moAERO(44,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_a03) = REAL(moAERO(44,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_a04) = REAL(moAERO(44,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_a05) = REAL(moAERO(44,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_a06) = REAL(moAERO(44,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_a07) = REAL(moAERO(44,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_a08) = REAL(moAERO(44,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_a01) = REAL(moAERO(45,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_a02) = REAL(moAERO(45,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_a03) = REAL(moAERO(45,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_a04) = REAL(moAERO(45,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_a05) = REAL(moAERO(45,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_a06) = REAL(moAERO(45,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_a07) = REAL(moAERO(45,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_a08) = REAL(moAERO(45,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_a01) = REAL(moAERO(46,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_a02) = REAL(moAERO(46,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_a03) = REAL(moAERO(46,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_a04) = REAL(moAERO(46,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_a05) = REAL(moAERO(46,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_a06) = REAL(moAERO(46,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_a07) = REAL(moAERO(46,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_a08) = REAL(moAERO(46,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_a01) = REAL(moAERO(47,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_a02) = REAL(moAERO(47,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_a03) = REAL(moAERO(47,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_a04) = REAL(moAERO(47,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_a05) = REAL(moAERO(47,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_a06) = REAL(moAERO(47,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_a07) = REAL(moAERO(47,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_a08) = REAL(moAERO(47,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_a01) = REAL(moAERO(48,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_a02) = REAL(moAERO(48,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_a03) = REAL(moAERO(48,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_a04) = REAL(moAERO(48,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_a05) = REAL(moAERO(48,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_a06) = REAL(moAERO(48,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_a07) = REAL(moAERO(48,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_a08) = REAL(moAERO(48,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_a01) = REAL(moAERO(49,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_a02) = REAL(moAERO(49,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_a03) = REAL(moAERO(49,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_a04) = REAL(moAERO(49,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_a05) = REAL(moAERO(49,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_a06) = REAL(moAERO(49,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_a07) = REAL(moAERO(49,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_a08) = REAL(moAERO(49,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_a01) = REAL(moAERO(50,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_a02) = REAL(moAERO(50,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_a03) = REAL(moAERO(50,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_a04) = REAL(moAERO(50,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_a05) = REAL(moAERO(50,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_a06) = REAL(moAERO(50,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_a07) = REAL(moAERO(50,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_a08) = REAL(moAERO(50,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_a01) = REAL(moAERO(51,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_a02) = REAL(moAERO(51,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_a03) = REAL(moAERO(51,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_a04) = REAL(moAERO(51,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_a05) = REAL(moAERO(51,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_a06) = REAL(moAERO(51,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_a07) = REAL(moAERO(51,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_a08) = REAL(moAERO(51,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_a01) = REAL(moAERO(52,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_a02) = REAL(moAERO(52,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_a03) = REAL(moAERO(52,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_a04) = REAL(moAERO(52,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_a05) = REAL(moAERO(52,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_a06) = REAL(moAERO(52,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_a07) = REAL(moAERO(52,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_a08) = REAL(moAERO(52,8))/CONV2

!     FOR AERO:
      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_a01) = REAL(moAERO(53,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_a02) = REAL(moAERO(53,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_a03) = REAL(moAERO(53,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_a04) = REAL(moAERO(53,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_a05) = REAL(moAERO(53,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_a06) = REAL(moAERO(53,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_a07) = REAL(moAERO(53,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_a08) = REAL(moAERO(53,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_a01) = REAL(moAERO(54,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_a02) = REAL(moAERO(54,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_a03) = REAL(moAERO(54,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_a04) = REAL(moAERO(54,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_a05) = REAL(moAERO(54,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_a06) = REAL(moAERO(54,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_a07) = REAL(moAERO(54,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_a08) = REAL(moAERO(54,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_a01) = REAL(moAERO(55,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_a02) = REAL(moAERO(55,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_a03) = REAL(moAERO(55,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_a04) = REAL(moAERO(55,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_a05) = REAL(moAERO(55,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_a06) = REAL(moAERO(55,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_a07) = REAL(moAERO(55,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_a08) = REAL(moAERO(55,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_a01) = REAL(moAERO(56,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_a02) = REAL(moAERO(56,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_a03) = REAL(moAERO(56,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_a04) = REAL(moAERO(56,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_a05) = REAL(moAERO(56,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_a06) = REAL(moAERO(56,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_a07) = REAL(moAERO(56,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_a08) = REAL(moAERO(56,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_a01) = REAL(moAERO(57,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_a02) = REAL(moAERO(57,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_a03) = REAL(moAERO(57,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_a04) = REAL(moAERO(57,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_a05) = REAL(moAERO(57,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_a06) = REAL(moAERO(57,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_a07) = REAL(moAERO(57,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_a08) = REAL(moAERO(57,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_a01) = REAL(moAERO(58,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_a02) = REAL(moAERO(58,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_a03) = REAL(moAERO(58,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_a04) = REAL(moAERO(58,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_a05) = REAL(moAERO(58,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_a06) = REAL(moAERO(58,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_a07) = REAL(moAERO(58,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_a08) = REAL(moAERO(58,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_a01) = REAL(moAERO(59,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_a02) = REAL(moAERO(59,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_a03) = REAL(moAERO(59,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_a04) = REAL(moAERO(59,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_a05) = REAL(moAERO(59,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_a06) = REAL(moAERO(59,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_a07) = REAL(moAERO(59,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_a08) = REAL(moAERO(59,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_a01) = REAL(moAERO(60,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_a02) = REAL(moAERO(60,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_a03) = REAL(moAERO(60,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_a04) = REAL(moAERO(60,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_a05) = REAL(moAERO(60,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_a06) = REAL(moAERO(60,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_a07) = REAL(moAERO(60,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_a08) = REAL(moAERO(60,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_a01) = REAL(moAERO(61,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_a02) = REAL(moAERO(61,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_a03) = REAL(moAERO(61,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_a04) = REAL(moAERO(61,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_a05) = REAL(moAERO(61,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_a06) = REAL(moAERO(61,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_a07) = REAL(moAERO(61,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_a08) = REAL(moAERO(61,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_a01) = REAL(moAERO(62,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_a02) = REAL(moAERO(62,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_a03) = REAL(moAERO(62,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_a04) = REAL(moAERO(62,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_a05) = REAL(moAERO(62,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_a06) = REAL(moAERO(62,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_a07) = REAL(moAERO(62,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_a08) = REAL(moAERO(62,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_a01) = REAL(moAERO(63,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_a02) = REAL(moAERO(63,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_a03) = REAL(moAERO(63,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_a04) = REAL(moAERO(63,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_a05) = REAL(moAERO(63,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_a06) = REAL(moAERO(63,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_a07) = REAL(moAERO(63,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_a08) = REAL(moAERO(63,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_a01) = REAL(moAERO(64,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_a02) = REAL(moAERO(64,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_a03) = REAL(moAERO(64,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_a04) = REAL(moAERO(64,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_a05) = REAL(moAERO(64,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_a06) = REAL(moAERO(64,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_a07) = REAL(moAERO(64,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_a08) = REAL(moAERO(64,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_a01) = REAL(moAERO(65,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_a02) = REAL(moAERO(65,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_a03) = REAL(moAERO(65,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_a04) = REAL(moAERO(65,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_a05) = REAL(moAERO(65,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_a06) = REAL(moAERO(65,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_a07) = REAL(moAERO(65,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_a08) = REAL(moAERO(65,8))/CONV2

!     FOR AERO:
      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_a01) = REAL(moAERO(66,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_a02) = REAL(moAERO(66,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_a03) = REAL(moAERO(66,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_a04) = REAL(moAERO(66,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_a05) = REAL(moAERO(66,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_a06) = REAL(moAERO(66,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_a07) = REAL(moAERO(66,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_a08) = REAL(moAERO(66,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_a01) = REAL(moAERO(67,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_a02) = REAL(moAERO(67,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_a03) = REAL(moAERO(67,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_a04) = REAL(moAERO(67,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_a05) = REAL(moAERO(67,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_a06) = REAL(moAERO(67,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_a07) = REAL(moAERO(67,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_a08) = REAL(moAERO(67,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_a01) = REAL(moAERO(68,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_a02) = REAL(moAERO(68,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_a03) = REAL(moAERO(68,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_a04) = REAL(moAERO(68,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_a05) = REAL(moAERO(68,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_a06) = REAL(moAERO(68,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_a07) = REAL(moAERO(68,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_a08) = REAL(moAERO(68,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_a01) = REAL(moAERO(69,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_a02) = REAL(moAERO(69,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_a03) = REAL(moAERO(69,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_a04) = REAL(moAERO(69,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_a05) = REAL(moAERO(69,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_a06) = REAL(moAERO(69,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_a07) = REAL(moAERO(69,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_a08) = REAL(moAERO(69,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_a01) = REAL(moAERO(70,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_a02) = REAL(moAERO(70,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_a03) = REAL(moAERO(70,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_a04) = REAL(moAERO(70,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_a05) = REAL(moAERO(70,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_a06) = REAL(moAERO(70,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_a07) = REAL(moAERO(70,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_a08) = REAL(moAERO(70,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_a01) = REAL(moAERO(71,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_a02) = REAL(moAERO(71,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_a03) = REAL(moAERO(71,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_a04) = REAL(moAERO(71,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_a05) = REAL(moAERO(71,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_a06) = REAL(moAERO(71,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_a07) = REAL(moAERO(71,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_a08) = REAL(moAERO(71,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_a01) = REAL(moAERO(72,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_a02) = REAL(moAERO(72,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_a03) = REAL(moAERO(72,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_a04) = REAL(moAERO(72,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_a05) = REAL(moAERO(72,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_a06) = REAL(moAERO(72,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_a07) = REAL(moAERO(72,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_a08) = REAL(moAERO(72,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_a01) = REAL(moAERO(73,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_a02) = REAL(moAERO(73,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_a03) = REAL(moAERO(73,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_a04) = REAL(moAERO(73,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_a05) = REAL(moAERO(73,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_a06) = REAL(moAERO(73,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_a07) = REAL(moAERO(73,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_a08) = REAL(moAERO(73,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_a01) = REAL(moAERO(74,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_a02) = REAL(moAERO(74,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_a03) = REAL(moAERO(74,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_a04) = REAL(moAERO(74,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_a05) = REAL(moAERO(74,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_a06) = REAL(moAERO(74,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_a07) = REAL(moAERO(74,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_a08) = REAL(moAERO(74,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_a01) = REAL(moAERO(75,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_a02) = REAL(moAERO(75,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_a03) = REAL(moAERO(75,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_a04) = REAL(moAERO(75,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_a05) = REAL(moAERO(75,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_a06) = REAL(moAERO(75,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_a07) = REAL(moAERO(75,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_a08) = REAL(moAERO(75,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_a01) = REAL(moAERO(76,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_a02) = REAL(moAERO(76,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_a03) = REAL(moAERO(76,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_a04) = REAL(moAERO(76,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_a05) = REAL(moAERO(76,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_a06) = REAL(moAERO(76,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_a07) = REAL(moAERO(76,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_a08) = REAL(moAERO(76,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_a01) = REAL(moAERO(77,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_a02) = REAL(moAERO(77,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_a03) = REAL(moAERO(77,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_a04) = REAL(moAERO(77,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_a05) = REAL(moAERO(77,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_a06) = REAL(moAERO(77,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_a07) = REAL(moAERO(77,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_a08) = REAL(moAERO(77,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_a01) = REAL(moAERO(78,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_a02) = REAL(moAERO(78,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_a03) = REAL(moAERO(78,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_a04) = REAL(moAERO(78,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_a05) = REAL(moAERO(78,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_a06) = REAL(moAERO(78,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_a07) = REAL(moAERO(78,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_a08) = REAL(moAERO(78,8))/CONV2

!     FOR AERO:
      CHEM(iNOW,kNOW,jNOW,p_PCG1_F_C_a01) = REAL(moAERO(79,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_PCG1_F_C_a02) = REAL(moAERO(79,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_PCG1_F_C_a03) = REAL(moAERO(79,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_PCG1_F_C_a04) = REAL(moAERO(79,4))/CONV2

!     FOR OLIG:
      CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_d01) = REAL(moOLIG(1,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_d02) = REAL(moOLIG(1,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_d03) = REAL(moOLIG(1,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_d04) = REAL(moOLIG(1,4))/CONV2
 !     CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_d05) = REAL(moOLIG(1,5))/CONV2
 !     CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_d06) = REAL(moOLIG(1,6))/CONV2
 !     CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_d07) = REAL(moOLIG(1,7))/CONV2
 !     CHEM(iNOW,kNOW,jNOW,p_TERP_CN3_d08) = REAL(moOLIG(1,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_d01) = REAL(moOLIG(2,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_d02) = REAL(moOLIG(2,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_d03) = REAL(moOLIG(2,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_d04) = REAL(moOLIG(2,4))/CONV2
 !     CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_d05) = REAL(moOLIG(2,5))/CONV2
 !     CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_d06) = REAL(moOLIG(2,6))/CONV2
 !     CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_d07) = REAL(moOLIG(2,7))/CONV2
 !     CHEM(iNOW,kNOW,jNOW,p_TERP_CN2_d08) = REAL(moOLIG(2,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_d01) = REAL(moOLIG(3,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_d02) = REAL(moOLIG(3,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_d03) = REAL(moOLIG(3,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_d04) = REAL(moOLIG(3,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_d05) = REAL(moOLIG(3,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_d06) = REAL(moOLIG(3,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_d07) = REAL(moOLIG(3,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_CN1_d08) = REAL(moOLIG(3,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_TERP_C00_d01) = REAL(moOLIG(4,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C00_d02) = REAL(moOLIG(4,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C00_d03) = REAL(moOLIG(4,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C00_d04) = REAL(moOLIG(4,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C00_d05) = REAL(moOLIG(4,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C00_d06) = REAL(moOLIG(4,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C00_d07) = REAL(moOLIG(4,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C00_d08) = REAL(moOLIG(4,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_TERP_C01_d01) = REAL(moOLIG(5,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C01_d02) = REAL(moOLIG(5,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C01_d03) = REAL(moOLIG(5,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C01_d04) = REAL(moOLIG(5,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C01_d05) = REAL(moOLIG(5,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C01_d06) = REAL(moOLIG(5,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C01_d07) = REAL(moOLIG(5,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C01_d08) = REAL(moOLIG(5,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_TERP_C02_d01) = REAL(moOLIG(6,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C02_d02) = REAL(moOLIG(6,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C02_d03) = REAL(moOLIG(6,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C02_d04) = REAL(moOLIG(6,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C02_d05) = REAL(moOLIG(6,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C02_d06) = REAL(moOLIG(6,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C02_d07) = REAL(moOLIG(6,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C02_d08) = REAL(moOLIG(6,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_TERP_C03_d01) = REAL(moOLIG(7,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C03_d02) = REAL(moOLIG(7,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C03_d03) = REAL(moOLIG(7,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C03_d04) = REAL(moOLIG(7,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C03_d05) = REAL(moOLIG(7,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C03_d06) = REAL(moOLIG(7,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C03_d07) = REAL(moOLIG(7,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C03_d08) = REAL(moOLIG(7,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_TERP_C04_d01) = REAL(moOLIG(8,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C04_d02) = REAL(moOLIG(8,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C04_d03) = REAL(moOLIG(8,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C04_d04) = REAL(moOLIG(8,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C04_d05) = REAL(moOLIG(8,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C04_d06) = REAL(moOLIG(8,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C04_d07) = REAL(moOLIG(8,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C04_d08) = REAL(moOLIG(8,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_TERP_C05_d01) = REAL(moOLIG(9,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C05_d02) = REAL(moOLIG(9,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C05_d03) = REAL(moOLIG(9,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C05_d04) = REAL(moOLIG(9,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C05_d05) = REAL(moOLIG(9,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C05_d06) = REAL(moOLIG(9,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C05_d07) = REAL(moOLIG(9,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C05_d08) = REAL(moOLIG(9,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_TERP_C06_d01) = REAL(moOLIG(10,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C06_d02) = REAL(moOLIG(10,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C06_d03) = REAL(moOLIG(10,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C06_d04) = REAL(moOLIG(10,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C06_d05) = REAL(moOLIG(10,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C06_d06) = REAL(moOLIG(10,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C06_d07) = REAL(moOLIG(10,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C06_d08) = REAL(moOLIG(10,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_TERP_C07_d01) = REAL(moOLIG(11,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C07_d02) = REAL(moOLIG(11,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C07_d03) = REAL(moOLIG(11,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C07_d04) = REAL(moOLIG(11,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C07_d05) = REAL(moOLIG(11,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C07_d06) = REAL(moOLIG(11,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C07_d07) = REAL(moOLIG(11,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C07_d08) = REAL(moOLIG(11,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_TERP_C08_d01) = REAL(moOLIG(12,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C08_d02) = REAL(moOLIG(12,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C08_d03) = REAL(moOLIG(12,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C08_d04) = REAL(moOLIG(12,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C08_d05) = REAL(moOLIG(12,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C08_d06) = REAL(moOLIG(12,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C08_d07) = REAL(moOLIG(12,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C08_d08) = REAL(moOLIG(12,8))/CONV2

      CHEM(iNOW,kNOW,jNOW,p_TERP_C09_d01) = REAL(moOLIG(13,1))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C09_d02) = REAL(moOLIG(13,2))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C09_d03) = REAL(moOLIG(13,3))/CONV2
      CHEM(iNOW,kNOW,jNOW,p_TERP_C09_d04) = REAL(moOLIG(13,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C09_d05) = REAL(moOLIG(13,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C09_d06) = REAL(moOLIG(13,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C09_d07) = REAL(moOLIG(13,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_TERP_C09_d08) = REAL(moOLIG(13,8))/CONV2
      
!     FOR OLIG:
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_d01) = REAL(moOLIG(14,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_d02) = REAL(moOLIG(14,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_d03) = REAL(moOLIG(14,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_d04) = REAL(moOLIG(14,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_d05) = REAL(moOLIG(14,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_d06) = REAL(moOLIG(14,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_d07) = REAL(moOLIG(14,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN3_d08) = REAL(moOLIG(14,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_d01) = REAL(moOLIG(15,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_d02) = REAL(moOLIG(15,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_d03) = REAL(moOLIG(15,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_d04) = REAL(moOLIG(15,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_d05) = REAL(moOLIG(15,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_d06) = REAL(moOLIG(15,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_d07) = REAL(moOLIG(15,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN2_d08) = REAL(moOLIG(15,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_d01) = REAL(moOLIG(16,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_d02) = REAL(moOLIG(16,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_d03) = REAL(moOLIG(16,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_d04) = REAL(moOLIG(16,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_d05) = REAL(moOLIG(16,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_d06) = REAL(moOLIG(16,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_d07) = REAL(moOLIG(16,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_CN1_d08) = REAL(moOLIG(16,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_d01) = REAL(moOLIG(17,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_d02) = REAL(moOLIG(17,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_d03) = REAL(moOLIG(17,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_d04) = REAL(moOLIG(17,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_d05) = REAL(moOLIG(17,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_d06) = REAL(moOLIG(17,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_d07) = REAL(moOLIG(17,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C00_d08) = REAL(moOLIG(17,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_d01) = REAL(moOLIG(18,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_d02) = REAL(moOLIG(18,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_d03) = REAL(moOLIG(18,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_d04) = REAL(moOLIG(18,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_d05) = REAL(moOLIG(18,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_d06) = REAL(moOLIG(18,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_d07) = REAL(moOLIG(18,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C01_d08) = REAL(moOLIG(18,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_d01) = REAL(moOLIG(19,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_d02) = REAL(moOLIG(19,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_d03) = REAL(moOLIG(19,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_d04) = REAL(moOLIG(19,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_d05) = REAL(moOLIG(19,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_d06) = REAL(moOLIG(19,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_d07) = REAL(moOLIG(19,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C02_d08) = REAL(moOLIG(19,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_d01) = REAL(moOLIG(20,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_d02) = REAL(moOLIG(20,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_d03) = REAL(moOLIG(20,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_d04) = REAL(moOLIG(20,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_d05) = REAL(moOLIG(20,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_d06) = REAL(moOLIG(20,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_d07) = REAL(moOLIG(20,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C03_d08) = REAL(moOLIG(20,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_d01) = REAL(moOLIG(21,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_d02) = REAL(moOLIG(21,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_d03) = REAL(moOLIG(21,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_d04) = REAL(moOLIG(21,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_d05) = REAL(moOLIG(21,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_d06) = REAL(moOLIG(21,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_d07) = REAL(moOLIG(21,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C04_d08) = REAL(moOLIG(21,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_d01) = REAL(moOLIG(22,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_d02) = REAL(moOLIG(22,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_d03) = REAL(moOLIG(22,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_d04) = REAL(moOLIG(22,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_d05) = REAL(moOLIG(22,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_d06) = REAL(moOLIG(22,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_d07) = REAL(moOLIG(22,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C05_d08) = REAL(moOLIG(22,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_d01) = REAL(moOLIG(23,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_d02) = REAL(moOLIG(23,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_d03) = REAL(moOLIG(23,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_d04) = REAL(moOLIG(23,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_d05) = REAL(moOLIG(23,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_d06) = REAL(moOLIG(23,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_d07) = REAL(moOLIG(23,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C06_d08) = REAL(moOLIG(23,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_d01) = REAL(moOLIG(24,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_d02) = REAL(moOLIG(24,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_d03) = REAL(moOLIG(24,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_d04) = REAL(moOLIG(24,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_d05) = REAL(moOLIG(24,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_d06) = REAL(moOLIG(24,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_d07) = REAL(moOLIG(24,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C07_d08) = REAL(moOLIG(24,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_d01) = REAL(moOLIG(25,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_d02) = REAL(moOLIG(25,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_d03) = REAL(moOLIG(25,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_d04) = REAL(moOLIG(25,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_d05) = REAL(moOLIG(25,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_d06) = REAL(moOLIG(25,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_d07) = REAL(moOLIG(25,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C08_d08) = REAL(moOLIG(25,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_d01) = REAL(moOLIG(26,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_d02) = REAL(moOLIG(26,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_d03) = REAL(moOLIG(26,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_d04) = REAL(moOLIG(26,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_d05) = REAL(moOLIG(26,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_d06) = REAL(moOLIG(26,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_d07) = REAL(moOLIG(26,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO1_C09_d08) = REAL(moOLIG(26,8))/CONV2
      
!     FOR OLIG:
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_d01) = REAL(moOLIG(27,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_d02) = REAL(moOLIG(27,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_d03) = REAL(moOLIG(27,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_d04) = REAL(moOLIG(27,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_d05) = REAL(moOLIG(27,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_d06) = REAL(moOLIG(27,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_d07) = REAL(moOLIG(27,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN3_d08) = REAL(moOLIG(27,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_d01) = REAL(moOLIG(28,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_d02) = REAL(moOLIG(28,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_d03) = REAL(moOLIG(28,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_d04) = REAL(moOLIG(28,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_d05) = REAL(moOLIG(28,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_d06) = REAL(moOLIG(28,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_d07) = REAL(moOLIG(28,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN2_d08) = REAL(moOLIG(28,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_d01) = REAL(moOLIG(29,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_d02) = REAL(moOLIG(29,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_d03) = REAL(moOLIG(29,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_d04) = REAL(moOLIG(29,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_d05) = REAL(moOLIG(29,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_d06) = REAL(moOLIG(29,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_d07) = REAL(moOLIG(29,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_CN1_d08) = REAL(moOLIG(29,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_d01) = REAL(moOLIG(30,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_d02) = REAL(moOLIG(30,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_d03) = REAL(moOLIG(30,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_d04) = REAL(moOLIG(30,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_d05) = REAL(moOLIG(30,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_d06) = REAL(moOLIG(30,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_d07) = REAL(moOLIG(30,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C00_d08) = REAL(moOLIG(30,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_d01) = REAL(moOLIG(31,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_d02) = REAL(moOLIG(31,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_d03) = REAL(moOLIG(31,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_d04) = REAL(moOLIG(31,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_d05) = REAL(moOLIG(31,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_d06) = REAL(moOLIG(31,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_d07) = REAL(moOLIG(31,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C01_d08) = REAL(moOLIG(31,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_d01) = REAL(moOLIG(32,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_d02) = REAL(moOLIG(32,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_d03) = REAL(moOLIG(32,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_d04) = REAL(moOLIG(32,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_d05) = REAL(moOLIG(32,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_d06) = REAL(moOLIG(32,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_d07) = REAL(moOLIG(32,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C02_d08) = REAL(moOLIG(32,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_d01) = REAL(moOLIG(33,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_d02) = REAL(moOLIG(33,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_d03) = REAL(moOLIG(33,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_d04) = REAL(moOLIG(33,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_d05) = REAL(moOLIG(33,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_d06) = REAL(moOLIG(33,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_d07) = REAL(moOLIG(33,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C03_d08) = REAL(moOLIG(33,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_d01) = REAL(moOLIG(34,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_d02) = REAL(moOLIG(34,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_d03) = REAL(moOLIG(34,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_d04) = REAL(moOLIG(34,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_d05) = REAL(moOLIG(34,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_d06) = REAL(moOLIG(34,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_d07) = REAL(moOLIG(34,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C04_d08) = REAL(moOLIG(34,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_d01) = REAL(moOLIG(35,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_d02) = REAL(moOLIG(35,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_d03) = REAL(moOLIG(35,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_d04) = REAL(moOLIG(35,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_d05) = REAL(moOLIG(35,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_d06) = REAL(moOLIG(35,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_d07) = REAL(moOLIG(35,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C05_d08) = REAL(moOLIG(35,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_d01) = REAL(moOLIG(36,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_d02) = REAL(moOLIG(36,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_d03) = REAL(moOLIG(36,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_d04) = REAL(moOLIG(36,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_d05) = REAL(moOLIG(36,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_d06) = REAL(moOLIG(36,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_d07) = REAL(moOLIG(36,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C06_d08) = REAL(moOLIG(36,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_d01) = REAL(moOLIG(37,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_d02) = REAL(moOLIG(37,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_d03) = REAL(moOLIG(37,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_d04) = REAL(moOLIG(37,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_d05) = REAL(moOLIG(37,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_d06) = REAL(moOLIG(37,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_d07) = REAL(moOLIG(37,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C07_d08) = REAL(moOLIG(37,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_d01) = REAL(moOLIG(38,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_d02) = REAL(moOLIG(38,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_d03) = REAL(moOLIG(38,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_d04) = REAL(moOLIG(38,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_d05) = REAL(moOLIG(38,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_d06) = REAL(moOLIG(38,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_d07) = REAL(moOLIG(38,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C08_d08) = REAL(moOLIG(38,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_d01) = REAL(moOLIG(39,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_d02) = REAL(moOLIG(39,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_d03) = REAL(moOLIG(39,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_d04) = REAL(moOLIG(39,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_d05) = REAL(moOLIG(39,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_d06) = REAL(moOLIG(39,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_d07) = REAL(moOLIG(39,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ARO2_C09_d08) = REAL(moOLIG(39,8))/CONV2

!     FOR OLIG:
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_d01) = REAL(moOLIG(40,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_d02) = REAL(moOLIG(40,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_d03) = REAL(moOLIG(40,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_d04) = REAL(moOLIG(40,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_d05) = REAL(moOLIG(40,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_d06) = REAL(moOLIG(40,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_d07) = REAL(moOLIG(40,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN3_d08) = REAL(moOLIG(40,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_d01) = REAL(moOLIG(41,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_d02) = REAL(moOLIG(41,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_d03) = REAL(moOLIG(41,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_d04) = REAL(moOLIG(41,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_d05) = REAL(moOLIG(41,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_d06) = REAL(moOLIG(41,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_d07) = REAL(moOLIG(41,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN2_d08) = REAL(moOLIG(41,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_d01) = REAL(moOLIG(42,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_d02) = REAL(moOLIG(42,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_d03) = REAL(moOLIG(42,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_d04) = REAL(moOLIG(42,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_d05) = REAL(moOLIG(42,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_d06) = REAL(moOLIG(42,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_d07) = REAL(moOLIG(42,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_CN1_d08) = REAL(moOLIG(42,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_d01) = REAL(moOLIG(43,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_d02) = REAL(moOLIG(43,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_d03) = REAL(moOLIG(43,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_d04) = REAL(moOLIG(43,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_d05) = REAL(moOLIG(43,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_d06) = REAL(moOLIG(43,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_d07) = REAL(moOLIG(43,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C00_d08) = REAL(moOLIG(43,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_d01) = REAL(moOLIG(44,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_d02) = REAL(moOLIG(44,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_d03) = REAL(moOLIG(44,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_d04) = REAL(moOLIG(44,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_d05) = REAL(moOLIG(44,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_d06) = REAL(moOLIG(44,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_d07) = REAL(moOLIG(44,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C01_d08) = REAL(moOLIG(44,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_d01) = REAL(moOLIG(45,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_d02) = REAL(moOLIG(45,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_d03) = REAL(moOLIG(45,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_d04) = REAL(moOLIG(45,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_d05) = REAL(moOLIG(45,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_d06) = REAL(moOLIG(45,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_d07) = REAL(moOLIG(45,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C02_d08) = REAL(moOLIG(45,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_d01) = REAL(moOLIG(46,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_d02) = REAL(moOLIG(46,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_d03) = REAL(moOLIG(46,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_d04) = REAL(moOLIG(46,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_d05) = REAL(moOLIG(46,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_d06) = REAL(moOLIG(46,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_d07) = REAL(moOLIG(46,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C03_d08) = REAL(moOLIG(46,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_d01) = REAL(moOLIG(47,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_d02) = REAL(moOLIG(47,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_d03) = REAL(moOLIG(47,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_d04) = REAL(moOLIG(47,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_d05) = REAL(moOLIG(47,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_d06) = REAL(moOLIG(47,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_d07) = REAL(moOLIG(47,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C04_d08) = REAL(moOLIG(47,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_d01) = REAL(moOLIG(48,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_d02) = REAL(moOLIG(48,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_d03) = REAL(moOLIG(48,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_d04) = REAL(moOLIG(48,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_d05) = REAL(moOLIG(48,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_d06) = REAL(moOLIG(48,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_d07) = REAL(moOLIG(48,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C05_d08) = REAL(moOLIG(48,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_d01) = REAL(moOLIG(49,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_d02) = REAL(moOLIG(49,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_d03) = REAL(moOLIG(49,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_d04) = REAL(moOLIG(49,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_d05) = REAL(moOLIG(49,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_d06) = REAL(moOLIG(49,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_d07) = REAL(moOLIG(49,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C06_d08) = REAL(moOLIG(49,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_d01) = REAL(moOLIG(50,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_d02) = REAL(moOLIG(50,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_d03) = REAL(moOLIG(50,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_d04) = REAL(moOLIG(50,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_d05) = REAL(moOLIG(50,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_d06) = REAL(moOLIG(50,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_d07) = REAL(moOLIG(50,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C07_d08) = REAL(moOLIG(50,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_d01) = REAL(moOLIG(51,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_d02) = REAL(moOLIG(51,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_d03) = REAL(moOLIG(51,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_d04) = REAL(moOLIG(51,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_d05) = REAL(moOLIG(51,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_d06) = REAL(moOLIG(51,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_d07) = REAL(moOLIG(51,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C08_d08) = REAL(moOLIG(51,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_d01) = REAL(moOLIG(52,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_d02) = REAL(moOLIG(52,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_d03) = REAL(moOLIG(52,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_d04) = REAL(moOLIG(52,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_d05) = REAL(moOLIG(52,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_d06) = REAL(moOLIG(52,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_d07) = REAL(moOLIG(52,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_ISPR_C09_d08) = REAL(moOLIG(52,8))/CONV2

!     FOR OLIG:
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_d01) = REAL(moOLIG(53,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_d02) = REAL(moOLIG(53,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_d03) = REAL(moOLIG(53,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_d04) = REAL(moOLIG(53,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_d05) = REAL(moOLIG(53,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_d06) = REAL(moOLIG(53,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_d07) = REAL(moOLIG(53,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN3_d08) = REAL(moOLIG(53,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_d01) = REAL(moOLIG(54,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_d02) = REAL(moOLIG(54,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_d03) = REAL(moOLIG(54,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_d04) = REAL(moOLIG(54,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_d05) = REAL(moOLIG(54,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_d06) = REAL(moOLIG(54,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_d07) = REAL(moOLIG(54,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN2_d08) = REAL(moOLIG(54,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_d01) = REAL(moOLIG(55,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_d02) = REAL(moOLIG(55,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_d03) = REAL(moOLIG(55,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_d04) = REAL(moOLIG(55,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_d05) = REAL(moOLIG(55,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_d06) = REAL(moOLIG(55,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_d07) = REAL(moOLIG(55,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_CN1_d08) = REAL(moOLIG(55,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_d01) = REAL(moOLIG(56,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_d02) = REAL(moOLIG(56,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_d03) = REAL(moOLIG(56,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_d04) = REAL(moOLIG(56,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_d05) = REAL(moOLIG(56,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_d06) = REAL(moOLIG(56,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_d07) = REAL(moOLIG(56,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C00_d08) = REAL(moOLIG(56,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_d01) = REAL(moOLIG(57,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_d02) = REAL(moOLIG(57,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_d03) = REAL(moOLIG(57,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_d04) = REAL(moOLIG(57,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_d05) = REAL(moOLIG(57,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_d06) = REAL(moOLIG(57,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_d07) = REAL(moOLIG(57,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C01_d08) = REAL(moOLIG(57,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_d01) = REAL(moOLIG(58,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_d02) = REAL(moOLIG(58,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_d03) = REAL(moOLIG(58,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_d04) = REAL(moOLIG(58,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_d05) = REAL(moOLIG(58,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_d06) = REAL(moOLIG(58,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_d07) = REAL(moOLIG(58,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C02_d08) = REAL(moOLIG(58,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_d01) = REAL(moOLIG(59,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_d02) = REAL(moOLIG(59,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_d03) = REAL(moOLIG(59,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_d04) = REAL(moOLIG(59,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_d05) = REAL(moOLIG(59,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_d06) = REAL(moOLIG(59,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_d07) = REAL(moOLIG(59,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C03_d08) = REAL(moOLIG(59,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_d01) = REAL(moOLIG(60,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_d02) = REAL(moOLIG(60,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_d03) = REAL(moOLIG(60,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_d04) = REAL(moOLIG(60,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_d05) = REAL(moOLIG(60,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_d06) = REAL(moOLIG(60,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_d07) = REAL(moOLIG(60,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C04_d08) = REAL(moOLIG(60,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_d01) = REAL(moOLIG(61,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_d02) = REAL(moOLIG(61,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_d03) = REAL(moOLIG(61,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_d04) = REAL(moOLIG(61,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_d05) = REAL(moOLIG(61,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_d06) = REAL(moOLIG(61,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_d07) = REAL(moOLIG(61,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C05_d08) = REAL(moOLIG(61,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_d01) = REAL(moOLIG(62,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_d02) = REAL(moOLIG(62,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_d03) = REAL(moOLIG(62,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_d04) = REAL(moOLIG(62,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_d05) = REAL(moOLIG(62,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_d06) = REAL(moOLIG(62,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_d07) = REAL(moOLIG(62,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C06_d08) = REAL(moOLIG(62,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_d01) = REAL(moOLIG(63,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_d02) = REAL(moOLIG(63,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_d03) = REAL(moOLIG(63,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_d04) = REAL(moOLIG(63,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_d05) = REAL(moOLIG(63,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_d06) = REAL(moOLIG(63,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_d07) = REAL(moOLIG(63,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C07_d08) = REAL(moOLIG(63,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_d01) = REAL(moOLIG(64,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_d02) = REAL(moOLIG(64,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_d03) = REAL(moOLIG(64,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_d04) = REAL(moOLIG(64,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_d05) = REAL(moOLIG(64,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_d06) = REAL(moOLIG(64,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_d07) = REAL(moOLIG(64,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C08_d08) = REAL(moOLIG(64,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_d01) = REAL(moOLIG(65,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_d02) = REAL(moOLIG(65,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_d03) = REAL(moOLIG(65,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_d04) = REAL(moOLIG(65,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_d05) = REAL(moOLIG(65,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_d06) = REAL(moOLIG(65,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_d07) = REAL(moOLIG(65,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_SESQ_C09_d08) = REAL(moOLIG(65,8))/CONV2

!     FOR OLIG:
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_d01) = REAL(moOLIG(66,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_d02) = REAL(moOLIG(66,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_d03) = REAL(moOLIG(66,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_d04) = REAL(moOLIG(66,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_d05) = REAL(moOLIG(66,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_d06) = REAL(moOLIG(66,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_d07) = REAL(moOLIG(66,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN3_d08) = REAL(moOLIG(66,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_d01) = REAL(moOLIG(67,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_d02) = REAL(moOLIG(67,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_d03) = REAL(moOLIG(67,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_d04) = REAL(moOLIG(67,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_d05) = REAL(moOLIG(67,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_d06) = REAL(moOLIG(67,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_d07) = REAL(moOLIG(67,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN2_d08) = REAL(moOLIG(67,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_d01) = REAL(moOLIG(68,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_d02) = REAL(moOLIG(68,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_d03) = REAL(moOLIG(68,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_d04) = REAL(moOLIG(68,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_d05) = REAL(moOLIG(68,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_d06) = REAL(moOLIG(68,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_d07) = REAL(moOLIG(68,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_CN1_d08) = REAL(moOLIG(68,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_d01) = REAL(moOLIG(69,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_d02) = REAL(moOLIG(69,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_d03) = REAL(moOLIG(69,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_d04) = REAL(moOLIG(69,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_d05) = REAL(moOLIG(69,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_d06) = REAL(moOLIG(69,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_d07) = REAL(moOLIG(69,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C00_d08) = REAL(moOLIG(69,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_d01) = REAL(moOLIG(70,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_d02) = REAL(moOLIG(70,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_d03) = REAL(moOLIG(70,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_d04) = REAL(moOLIG(70,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_d05) = REAL(moOLIG(70,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_d06) = REAL(moOLIG(70,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_d07) = REAL(moOLIG(70,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C01_d08) = REAL(moOLIG(70,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_d01) = REAL(moOLIG(71,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_d02) = REAL(moOLIG(71,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_d03) = REAL(moOLIG(71,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_d04) = REAL(moOLIG(71,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_d05) = REAL(moOLIG(71,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_d06) = REAL(moOLIG(71,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_d07) = REAL(moOLIG(71,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C02_d08) = REAL(moOLIG(71,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_d01) = REAL(moOLIG(72,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_d02) = REAL(moOLIG(72,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_d03) = REAL(moOLIG(72,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_d04) = REAL(moOLIG(72,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_d05) = REAL(moOLIG(72,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_d06) = REAL(moOLIG(72,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_d07) = REAL(moOLIG(72,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C03_d08) = REAL(moOLIG(72,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_d01) = REAL(moOLIG(73,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_d02) = REAL(moOLIG(73,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_d03) = REAL(moOLIG(73,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_d04) = REAL(moOLIG(73,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_d05) = REAL(moOLIG(73,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_d06) = REAL(moOLIG(73,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_d07) = REAL(moOLIG(73,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C04_d08) = REAL(moOLIG(73,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_d01) = REAL(moOLIG(74,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_d02) = REAL(moOLIG(74,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_d03) = REAL(moOLIG(74,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_d04) = REAL(moOLIG(74,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_d05) = REAL(moOLIG(74,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_d06) = REAL(moOLIG(74,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_d07) = REAL(moOLIG(74,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C05_d08) = REAL(moOLIG(74,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_d01) = REAL(moOLIG(75,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_d02) = REAL(moOLIG(75,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_d03) = REAL(moOLIG(75,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_d04) = REAL(moOLIG(75,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_d05) = REAL(moOLIG(75,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_d06) = REAL(moOLIG(75,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_d07) = REAL(moOLIG(75,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C06_d08) = REAL(moOLIG(75,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_d01) = REAL(moOLIG(76,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_d02) = REAL(moOLIG(76,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_d03) = REAL(moOLIG(76,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_d04) = REAL(moOLIG(76,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_d05) = REAL(moOLIG(76,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_d06) = REAL(moOLIG(76,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_d07) = REAL(moOLIG(76,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C07_d08) = REAL(moOLIG(76,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_d01) = REAL(moOLIG(77,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_d02) = REAL(moOLIG(77,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_d03) = REAL(moOLIG(77,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_d04) = REAL(moOLIG(77,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_d05) = REAL(moOLIG(77,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_d06) = REAL(moOLIG(77,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_d07) = REAL(moOLIG(77,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C08_d08) = REAL(moOLIG(77,8))/CONV2

!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_d01) = REAL(moOLIG(78,1))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_d02) = REAL(moOLIG(78,2))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_d03) = REAL(moOLIG(78,3))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_d04) = REAL(moOLIG(78,4))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_d05) = REAL(moOLIG(78,5))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_d06) = REAL(moOLIG(78,6))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_d07) = REAL(moOLIG(78,7))/CONV2
!      CHEM(iNOW,kNOW,jNOW,p_IVO1_C09_d08) = REAL(moOLIG(78,8))/CONV2

      RETURN
      END SUBROUTINE
      
      END MODULE mod_MAP_INOUT
