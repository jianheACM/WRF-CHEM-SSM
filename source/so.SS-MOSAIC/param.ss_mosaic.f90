!     =======================================================================
!            THIS MODULE CONTAINS THE GLOBAL PARAMETERS FOR SS-MOSAIC
!     =======================================================================

      MODULE mod_SSMOSAIC_PARAMS
      
!     MODULE INTERFACE:
!     =======================================================================
      USE MODULE_STATE_DESCRIPTION, ONLY: num_moist, num_chem
      
!     ACCESS THE POINTERS FOR CHEM IN WRF:
      USE MODULE_CONFIGURE, ONLY: & 
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
 !     p_NUM_a05, p_NUM_a06, p_NUM_a07, p_NUM_a08, &
      p_PCG1_F_C, p_PCG1_F_C_a01, p_PCG1_F_C_a02, p_PCG1_F_C_a03, p_PCG1_F_C_a04, &

      p_NUME,p_DEN, &

      p_TERP,p_ARO1,p_ARO2,p_SESQ,p_ISOPRENE,p_PCG7_F_C

!     DECLARATIONS:
!     =======================================================================
!     NUMBER OF ACTIVE SPECIES:
      INTEGER,PARAMETER :: nCOMP = 13*6 + 1

!     NUMBER OF AEROSOL SIZE BINS:
      INTEGER,PARAMETER :: nBINS = 4

!     MOLECULAR WEIGHT OF 
!     SIMPLE-SOM SPECIES [g mol-1]:
!     NOTE: THESE MUST BE CONSISTENT WITH THE BOXMODEL FITS.
!     NOTE: ORDER IS TERP, ISPR, ARO1, ARO2, SESQ, IVO1.
      REAL(8),PARAMETER,DIMENSION(nCOMP) :: &
      MW_SS = (/281.80, 267.80, 253.80, 239.80, 225.80, 211.80, 197.80, &
                183.80, 169.80, 155.80, 141.80, 127.80, 113.80, &
                247.50, 233.50, 219.50, 205.50, 191.50, 177.50, 163.50, &
                149.50, 135.50, 121.50, 107.50, 93.50, 79.50, &
                254.60, 240.60, 226.60, 212.60, 198.60, 184.60, 170.60, &
                156.60, 142.60, 128.60, 114.60, 100.60, 86.60, &
                240.30, 226.30, 212.30, 198.30, 184.30, 170.30, 156.30, &
                142.30, 128.30, 114.30, 100.30, 86.30, 72.30, &
                317.80, 303.80, 289.80, 275.80, 261.80, 247.80, 233.80, &
                219.80, 205.80, 191.80, 177.80, 163.80, 149.80, &
                318.66, 304.66, 290.66, 276.66, 262.66, 248.66, 234.66, &
                220.66, 206.66, 192.66, 178.66, 164.66, 150.66, &
                250.00/)
      
!     SATURATION CONC. [ug m-3]:
      REAL(8),PARAMETER,DIMENSION(nCOMP) :: &
      CSAT = (/1e-3,1e-2,1e-1,1e0,1e1,1e2,1e3,1e4,1e5,1e6,1e7,1e8,1e9, &
               1e-3,1e-2,1e-1,1e0,1e1,1e2,1e3,1e4,1e5,1e6,1e7,1e8,1e9, & 
               1e-3,1e-2,1e-1,1e0,1e1,1e2,1e3,1e4,1e5,1e6,1e7,1e8,1e9, & 
               1e-3,1e-2,1e-1,1e0,1e1,1e2,1e3,1e4,1e5,1e6,1e7,1e8,1e9, &
               1e-3,1e-2,1e-1,1e0,1e1,1e2,1e3,1e4,1e5,1e6,1e7,1e8,1e9, &
               1e-3,1e-2,1e-1,1e0,1e1,1e2,1e3,1e4,1e5,1e6,1e7,1e8,1e9,1e-6/)
      
!     PI VALUE:
      REAL(8),PARAMETER :: PI = 3.1415926

!     AVOGADRO'S NUMBER:
      REAL(8),PARAMETER :: nAVO = 6.022e23

!     VOLUME OF SIMPLE-SOM SOA:
      REAL(8),SAVE,ALLOCATABLE :: SOAMASS3d_00(:,:,:,:)
      REAL(8),SAVE,ALLOCATABLE :: SOAMASS3d_01(:,:,:,:)      

      END MODULE mod_SSMOSAIC_PARAMS
