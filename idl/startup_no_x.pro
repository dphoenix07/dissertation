COMPILE_OPT IDL2																								;Set compile options

!MORE       =  0																								;Turn off 'more' for HELP output

!path = !path + $																								;Define IDL search path
	':' + EXPAND_PATH('+~/idl') + $
	':' + EXPAND_PATH('+/chomeyer/idl/') + $
	':' + EXPAND_PATH('+/data1/idl_Shared') + $
	':' + EXPAND_PATH('+/data3/dphoenix') + $
	':' + EXPAND_PATH('+/data3/data1_copy/idl_Shared')

;Location of WRF output
DEFSYSV, '!WRF_DIRECTORY'   , '/data3/dphoenix/wrf/'
;DEFSYSV, '!WRF_DIRECTORY'   , '/data1/dphoenix/WRF/'
DEFSYSV, '!DC3_DATA'	    , '/data1/field_projects/dc3/'
DEFSYSV, '!NEXRAD_DIRECTORY', '/data1/field_projects/dc3/nexrad/'
DEFSYSV, '!NEXRAD_LEVEL3_DIRECTORY', '/data1/NEXRAD/level2/composite/'

;Newline
DEFSYSV, '!NEW_LINE', STRING(10B)

;Mathematical constants
DEFSYSV, '!DDTOR',   !DPI/180.0D0, 1																	;Double precision degrees to radians (read only)
DEFSYSV, '!DRADEG',  180.0D0/!DPI, 1																	;Matches !RADEG name better (read only)
DEFSYSV, '!DRADDEG', 180.0D0/!DPI, 1																	;Double precision radians to degrees (read only)

;Universal physical constants
DEFSYSV, '!Speed_of_light',      299792458.0D0, 1													;Speed of light in vacuum (m s^-1)
DEFSYSV, '!Planck',             6.62606876D-34, 1													;Planck constant (J s)
DEFSYSV, '!Universal_gas',         8314.4720D0, 1													;Universal gas constant (J K^-1 kmol^-1)
DEFSYSV, '!Stefan_Boltzmann',     5.670400D-08, 1													;Stefan-Boltzman constant (W m^-2 K^-4)
DEFSYSV, '!Avogadro',           6.02214199D+23, 1													;Avogadro's number (molecules mol^-1)
DEFSYSV, '!Gravitation',             6.673D-11, 1													;Gravitational constant (J K^-1)
DEFSYSV, '!Boltzmann',           1.3806503D-23, 1													;Boltzmann's constant (N m^2 kg^-2)

;Terrestrial physical constants
DEFSYSV, '!a0',                      6371220.0, 1													;Mean radius of the Earth (m) (read only)
DEFSYSV, '!g',                            9.81, 1													;Acceleration of gravity at sea level (m s^-2) (read only)
DEFSYSV, '!tauDSolar',                   86400, 1													;Length of mean solar day (s) (read only)
DEFSYSV, '!tauDSidereal',              86164.0, 1													;Length of sidereal day (s) (read only)
DEFSYSV, '!tauYSolar',              31556925.0, 1													;Length of mean solar year (s) (read only)
DEFSYSV, '!S0',                         1367.0, 1													;Solar constant (W m^-2) (read only)
DEFSYSV, '!Omega',      2.0*!PI /!tauDSidereal, 1													;Rotation rate of the Earth (rad s^-1) (read only)
DEFSYSV, '!DOmega',   2.0D0*!DPI/!tauDSidereal, 1													;Rotation rate of the Earth (rad s^-1) (read only)

;Air
DEFSYSV, '!Rair',                        287.0, 1													;Gas constant for dry air (J K^-1 kmol^-1) (read only)
DEFSYSV, '!Cp',                         1004.0, 1													;Specific heat of dry air at constant pressure (J K^-1 kg^-1) (read only)
DEFSYSV, '!Cv',                          717.0, 1													;Specific heat of dry air at constant volume (J K^-1 kg^-1) (read only)
DEFSYSV, '!Kair',                        0.023, 1													;Thermal Conductivity of dry air (J s^-1 m^-1 K^-1)

;Water
DEFSYSV, '!Rvapor',                      461.5, 1													;Gas constant for water vapor
DEFSYSV, '!Lv',                         2.5E06, 1													;Latent heat of vaporization (J kg^-1) (read only)
DEFSYSV, '!Lf',                        3.34E05, 1													;Latent heat of fusion (J kg^-1) (read only)
DEFSYSV, '!Ls',                        2.83E06, 1													;Latent heat of sublimation (J kg^-1) (read only)
DEFSYSV, '!Cw',                         4218.0, 1													;Specific heat of liquid water at 0 C (J kg^-1) (read only)
DEFSYSV, '!Rho_w',                      1000.0, 1													;Density of water (kg m^-3) (read only)
DEFSYSV, '!Dv',                         2.5E-5, 1													;Diffusivity of water vapor (m^2 s^-1) (read only)

;Conversions
DEFSYSV, '!CtoK',                       273.15, 1													;Temperature conversion from Celcius to Kelvin
DEFSYSV, '!KtoC',                      -273.15, 1													;Temperatuer conversion from Kelvin to Celcius

;For satellite meteorology (ATMO 489)
DEFSYSV, '!Earth', $
  {name          : 'Earth parameters',             		$                                                                                            ;Name
        aE            : 6.371009D+06,                   $                                                                                               ;Mean radius of the Earth (m)
        aEe           : 6.378137D+06,                   $                                                                                               ;Equatorial radius of the Earth (m)
        aEp           : 6.356752D+06,                   $                                                                                               ;Polar radius of the Earth (m)
        ME            : 5.97370D+24,                    $                                                                                               ;Mass of Earth (kg)
        GM            : 3.986005D+14,                   $                                                                                               ;G x mass of Earth (m^3 s^-2)
        J2            : 1.08263D-03,                    $                                                                                               ;Magnitude of J2 term in gravitational potential (dimensionless)
        tropical_year : 3.15569259747D+07,              $                                                                                               ;Length of tropical year (s)
        omega         : 7.292115922D-05,                $                                                                                               ;Sidereal rotation rate of Earth (rad s^-1)
        omegaD        : !DRADEG*7.292115922D-05,        $                                                                                               ;Sidereal rotation rate of Earth (degrees s^-1)
        alpha0D       : 100.38641,                      $                                                                                               ;Right ascension of Greenwich at t0 (degrees)
        alpha0        : !DDTOR*100.38641,               $                                                                                               ;Right ascension of Greenwich at t0 (radians)
        t0            : MAKE_DATE(1990, 1, 1, 0, 0, 0), $                                                                                               ;Initial time for earth rotation calculations
        tau_Solar     : 86400.0D0,                      $                                                                                               ;Length of mean solar day (s)
        tau_Sidereal  : 2.0D0*!DPI/7.292115922D-05},    $                                                                                               ;Length of sidereal day (s)
        1

DEVICE, RETAIN = 2																							;Have IDL do backing store
