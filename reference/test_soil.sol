**********************************************************************
* Template soil data file for PADDY soil water balance model.        *
**********************************************************************
* Soil        : test_soil - texture classes:c("Lo", "Lo", "Lo")
* File name        : test_soil.sol
* Sampling date      : 2014-04-10
* Additional info  : Create with https://github.com/jrodriguez88
*--------------------------------------------------------------------*

SCODE = 'PADDY'

*---------------------------------------------------------------*
* 1. Various soil and management parameters
*---------------------------------------------------------------*
WL0MX = 100.   ! Bund height (mm)
NL = 3        ! Number of soil layers (maximum is 10) (-)
TKL = 0.20, 0.20, 0.20   ! Thickness of each soil layer (m)
ZRTMS = 0.5   ! Maximum rooting depth in the soil (m)

*---------------------------------------------------------------*
* 2. Puddling switch: 1=PUDDLED or 0=NON PUDDLED
*---------------------------------------------------------------*
SWITPD = 0  !Non puddled
NLPUD = 1
WCSTRP = 0.36, 0.44, 0.36
PFCR = 6.0
DPLOWPAN = 0.6

*---------------------------------------------------------------*
* 3. Groundwater switch: 0=DEEP (i.e., not in profile), 1=DATA
* (supplied), 2=CALCULATE
*---------------------------------------------------------------*
SWITGW = 0
ZWTB =   1.,200.,
       366.,200.

ZWTBI = 100. ! Initial groundwater table depth (cm)
MINGW = 100. ! Minimum groundwater table depth (cm)
MAXGW = 100. ! Maximum groundwater table depth (cm)
ZWA   = 1.0  ! Receding rate of groundwater with no recharge (cm d-1)
ZWB   = 0.5  ! Sensitivity factor of groundwater recharge (-)

*---------------------------------------------------------------*
* 4. Percolation switch
* Value for SWITVP cannot be 1 (CALCULATE) for non-puddled soil
*---------------------------------------------------------------*
SWITVP = -1 ! Fixed percolation rate
FIXPERC = 2.472660134

PTABLE =
  1., 1.0,
 50., 1.0,
100., 20.0,
366., 20.0

*---------------------------------------------------------------*
* 5. Conductivity switch: 0=NO DATA, 1=VAN GENUCHTEN or 2=POWER
*  OR 3= SPAW  function used
*---------------------------------------------------------------*
SWITKH = 0 ! No data

*---------------------------------------------------------------*
* 6. Water retention switch: 0=DATA; 1=VAN GENUCHTEN. When DATA, data
* have to be supplied for saturation, field capacity,
* wilting point and at air dryness
*---------------------------------------------------------------*
SWITPF = 0  ! Data

*---------------------------------------------------------------*
* 7.Soil physical properties, these parameters will be used when model
* runs under actual water or nitrogen condition, or even both. Otherwise
* these parameters will not be used.
*---------------------------------------------------------------*
CLAYX = 0.13, 0.18, 0.16
SANDX = 0.41, 0.45, 0.44
BD = 1.79, 1.80, 1.70

SOC = 27791.27, 13620.38, 29290.51
SON = 2485.50, 5683.53, 2026.29
SNH4X = 11.21, 14.28, 19.10
SNO3X = 10.68, 47.51, 13.39

*-----------------------------------------------------------------*
* 8. Soil hydrological properties. Required type of data input    *
* according to setting of conductivity and water retention switch *
*-----------------------------------------------------------------*
KST = 28.68, 21.94, 24.73
WCST = 0.36, 0.44, 0.36
WCFC = 0.27, 0.28, 0.32
WCWP = 0.20, 0.22, 0.28
WCAD = 0.05, 0.07, 0.07

*---------------------------------------------------------------*
* 9. Initialization conditions, and re-initialization
*---------------------------------------------------------------*
WL0I = 0.
WCLI = 0.27, 0.28, 0.32
RIWCLI = 'NO'

*---------------------------------------------------------------*
* 10. Initialization of soil thermal conditions
*---------------------------------------------------------------*
SATAV = 20.
SOILT = 20., 18., 17.

*---------------------------------------------------------------*
* 11. Observations/measurements
*    Switches to force observed water content in water balance
*---------------------------------------------------------------*
WCLINT = 1,1,1,
         2,2,2,
         3,3,3
