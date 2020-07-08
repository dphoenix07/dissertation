PRO AUTO_WRF_PLOT, event, scheme, start_date, end_date, $
	REFL     	  = refl, $
	WRF_NEX_REFL  = wrf_nex_refl, $
	HYDROMETEOR   = hydrometeor, $
	ECHO     	  = echo, $
	SFC_O3        = sfc_o3, $
	THETA_GRAD    = theta_grad, $
	VERT_WIND     = vert_wind, $
	SFC_NOX       = sfc_nox, $
	SFC_WIND      = sfc_wind, $
	SFC_PRESS     = sfc_press, $
	SECTION1      = section1, $
	SECTION2      = section2, $
	SECTION3      = section3, $
	SECTION4      = section4, $
	SECTIONW      = sectionw, $
	SECTION_CO    = section_co, $
	SECTION_WIND  = section_wind, $
	SECTION_H2O   = section_h2o, $
	SECTION_TEMP  = section_temp, $
	SECTION_THETA = section_theta, $
	SECTION_TRACER = section_tracer, $
	SECTION_PRESS  = section_press, $
	LEV		      = lev, $
	DOMAIN		  = domain, $
	PNG           = png, $
	ANIMATE       = animate


;+
; Name:
;               AUTO_WRF_PLOT
; Purpose:
;               This is a procedure to produce a desired number of WRF plots. Best
;				used for creating plots to combined in a GIF loop.
; Calling sequence:
;               AUTO_WRF_PLOT, event, scheme, variable, date, end_hour
; Inputs:
;               event      : String variable of run name. (e.g., '20120519')
;				scheme     : String variable of microphysics scheme (e.g., 'morrison')
;				start_date : String variable of the start date requested (e.g., '20120519T2300Z')
;				end_date   : String variable of the end date requested (e.g., '20120520T0300Z')
; Output:
;               A set of plots and an animated GIF of those plots.
; Keywords:
;				REFL		: If set, plots reflectivity.
;				TROPO3		: If set, plots ozone concentrations at tropopause.
;				ANIMATE	    : If set, calls WRITE_ANIMATED_GIF to create animation.
;				PNG			: Set if output to PNG desired.
; Author and history:
;               Daniel B. Phoenix	2016-01-21.
;-

COMPILE_OPT IDL2																				;Set compile options

IF (N_ELEMENTS(event	 ) EQ 0) THEN event 	 = '20120519'									;Set defaults
IF (N_ELEMENTS(scheme    ) EQ 0) THEN scheme	 = 'morrison'
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'
IF (N_ELEMENTS(lev		 ) EQ 0) THEN lev 		 = 0
IF (N_ELEMENTS(png		 ) EQ 0) THEN png		 = 0
IF (N_ELEMENTS(domain	 ) EQ 0) THEN domain     = 1

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(event, scheme, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(event, scheme, start_date, end_date, /DATE)	

IF KEYWORD_SET(animate) THEN png = 1

IF KEYWORD_SET(refl) THEN BEGIN																	;Plot reflectivity at selected times
	key = 'refl'
	FOREACH d, date_arr DO MAP_WRF_REFLECTIVITY, d, STRING(event), STRING(scheme), DOMAIN=dom, PNG=png
ENDIF

IF KEYWORD_SET(wrf_nex_refl) THEN BEGIN																	;Plot reflectivity at selected times
	key = 'wrf_nex_refl'
	FOREACH d, date_arr DO MAP_WRF_NEXRAD_REFLECTIVITY, d, STRING(event), STRING(scheme), DOMAIN=dom, PNG=png
ENDIF

IF KEYWORD_SET(hydrometeor) THEN BEGIN
	key = 'hydrometeor'
	FOREACH d, date_arr DO PLOT_WRF_HYDROMETEOR_RALT, d, STRING(event), STRING(scheme),/BINNED,/IN_CLOUD
ENDIF

IF KEYWORD_SET(echo) THEN BEGIN																	;Plot echo tops at selected times
	key = 'echo'
	FOREACH d, date_arr DO MAP_WRF_ECHO_TOP, d, STRING(event), STRING(scheme), ECHO=5, DOMAIN=dom, PNG=png
ENDIF

IF KEYWORD_SET(sfc_o3) THEN BEGIN																;Plot tropopause o3 at selected times
	key = 'sfc_o3'
	nox = 0
	FOREACH d, date_arr DO MAP_SFC_OZONE, d, STRING(event), STRING(scheme), LEV = 49, DOMAIN=dom, PNG=png	
ENDIF

IF KEYWORD_SET(theta_grad) THEN BEGIN																;Plot tropopause o3 at selected times
	key = 'theta_grad'
	FOREACH d, date_arr DO MAP_THETA_GRAD, d, STRING(event), STRING(scheme), LEV = 49, DOMAIN=dom, PNG=png	
ENDIF

IF KEYWORD_SET(vert_wind) THEN BEGIN																;Plot tropopause o3 at selected times
	key = 'vert_wind'
	FOREACH d, date_arr DO MAP_VERT_WIND, d, STRING(event), STRING(scheme), LEV = 49, DOMAIN=dom, PNG=png	
ENDIF

IF KEYWORD_SET(sfc_nox) THEN BEGIN																;Plot tropopause o3 at selected times
	key = 'sfc_nox'
	FOREACH d, date_arr DO MAP_SFC_OZONE, d, STRING(event), STRING(scheme), LEV = lev, /PLT_NOX, PNG=png
ENDIF

IF KEYWORD_SET(sfc_wind) THEN BEGIN																;Plot tropopause o3 at selected times
	key = 'sfc_wind'
;	FOREACH d, date_arr DO MAP_SFC_WIND, d, STRING(event), STRING(scheme), LEV = 49, region=[1000,50,1500,350], PNG=png
	FOREACH d, date_arr DO MAP_SFC_WIND, d, STRING(event), STRING(scheme), LEV = 10250, PNG=png
ENDIF

IF KEYWORD_SET(sfc_press) THEN BEGIN																;Plot tropopause o3 at selected times
	key = 'sfc_press'
;	FOREACH d, date_arr DO MAP_SFC_WIND, d, STRING(event), STRING(scheme), LEV = 49, region=[1000,50,1500,350], PNG=png
	FOREACH d, date_arr DO MAP_PRESSURE, d, STRING(event), STRING(scheme), LEV = 11500, PNG=png
ENDIF

IF KEYWORD_SET(section1) THEN BEGIN
	key = 'O3'
	FOREACH d, date_arr DO PLOT_WRF_SECTION_CRH, STRING(event), STRING(scheme),300,780,2019,300, d, dom=1,map_type=2,section_type=2,chemical='O3_tracer',KEY=key, PNG=1
ENDIF

IF KEYWORD_SET(section2) THEN BEGIN
	key = 'O3'
	FOREACH d, date_arr DO PLOT_WRF_SECTION_CRH, STRING(event), STRING(scheme),75,500,1919,1, d, dom=1,map_type=2,section_type=2,chemical='O3_tracer',KEY=key, PNG=1
ENDIF

IF KEYWORD_SET(section3) THEN BEGIN
	key = 'O3'
	FOREACH d, date_arr DO PLOT_WRF_SECTION_CRH, STRING(event), STRING(scheme),300,780,1600,1, d, dom=1,map_type=2,section_type=2,chemical='O3_tracer',KEY=key, PNG=1
ENDIF

IF KEYWORD_SET(section4) THEN BEGIN
	key = 'O3'
;	FOREACH d, date_arr DO PLOT_WRF_SECTION_DBP, STRING(event), STRING(scheme),1000,350,1519,100, d, dom=1,map_type=2,section_type=2,chemical='O3_tracer',KEY=key, PNG=1
	FOREACH d, date_arr DO PLOT_WRF_SECTION_DBP, STRING(event), STRING(scheme),325,700,700,400, d, dom=1,map_type=2,section_type=2,chemical='w',KEY=key, zrange=[8,13], PNG=1
ENDIF

IF KEYWORD_SET(section_co) THEN BEGIN
	key = 'CO'
	FOREACH d, date_arr DO PLOT_WRF_SECTION_DBP, STRING(event), STRING(scheme),75,700,2019,100, d, dom=1,map_type=2,section_type=2,chemical='CO_tracer',zrange=[7,18],KEY=key, PNG=1
ENDIF

IF KEYWORD_SET(section_tracer) THEN BEGIN
	key = 'CO'
	FOREACH d, date_arr DO PLOT_WRF_SECTION_DBP, STRING(event), STRING(scheme),75,700,2019,100, d, dom=1,map_type=2,section_type=2,chemical='ST_tracer',zrange=[7,18],KEY=key, PNG=1
ENDIF

IF KEYWORD_SET(sectionw) THEN BEGIN
	key = 'O3'
;	FOREACH d, date_arr DO PLOT_WRF_SECTION_DBP, STRING(event), STRING(scheme),75,700,2019,100, d, dom=1,map_type=2,section_type=2,chemical='w',zrange=[7,15],KEY=key, PNG=1
;	FOREACH d, date_arr DO PLOT_WRF_SECTION_DBP, STRING(event), STRING(scheme),325,700,700,400, d, dom=1,map_type=2,section_type=2,chemical='w',KEY=key, zrange=[8,13], PNG=1
;	FOREACH d, date_arr DO PLOT_WRF_SECTION_DBP, STRING(event), STRING(scheme),1000,350,1519,100, d, dom=1,map_type=2,section_type=2,chemical='w',KEY=key, zrange=[7,13], PNG=1
;	FOREACH d, date_arr DO PLOT_WRF_SECTION_DBP, STRING(event), STRING(scheme),800,475,1919,100, d, dom=1,map_type=2,section_type=2,chemical='w',zrange=[8,13],KEY=key, PNG=1

;	FOREACH d, date_arr DO PLOT_WRF_SECTION_AVERAGE, STRING(event), STRING(scheme),75,700,2019,100, d, dom=1,map_type=2,section_type=2,chemical='w',zrange=[7,15],KEY=key, PNG=1
	FOREACH d, date_arr DO PLOT_WRF_SECTION_AVERAGE, STRING(event), STRING(scheme),325,700,700,400, d, dom=1,map_type=2,section_type=2,chemical='w',KEY=key, zrange=[8,13], PNG=1

ENDIF

IF KEYWORD_SET(section_wind) THEN BEGIN
	key = 'hwind'
;	FOREACH d, date_arr DO PLOT_WRF_SECTION_DBP, STRING(event), STRING(scheme),75,700,2019,100, d, dom=1,map_type=2,section_type=3,chemical='O3_tracer',zrange=[7,15],KEY=key, PNG=1
;	FOREACH d, date_arr DO PLOT_WRF_SECTION_DBP, STRING(event), STRING(scheme),1000,350,1519,100, d, dom=1,map_type=2,section_type=3,chemical='O3_tracer',zrange=[7,13],KEY=key, PNG=1
;	FOREACH d, date_arr DO PLOT_WRF_SECTION_DBP, STRING(event), STRING(scheme),325,700,700,400, d, dom=1,map_type=2,section_type=3,chemical='w',KEY=key, zrange=[8,13], PNG=1
;	FOREACH d, date_arr DO PLOT_WRF_SECTION_DBP, STRING(event), STRING(scheme),325,700,750,500, d, dom=1,map_type=2,section_type=3,chemical='w',KEY=key, zrange=[8,13], PNG=1
;	FOREACH d, date_arr DO PLOT_WRF_SECTION_DBP, STRING(event), STRING(scheme),800,475,1919,100, d, dom=1,map_type=2,section_type=3,chemical='w',KEY=key, zrange=[8,13], PNG=1
;	FOREACH d, date_arr DO PLOT_WRF_SECTION_DBP, STRING(event), STRING(scheme),090,730,1934,130, d, dom=1,map_type=2,section_type=3,chemical='w',KEY=key, zrange=[8,13], PNG=1
;	FOREACH d, date_arr DO PLOT_WRF_SECTION_DBP, STRING(event), STRING(scheme),060,670,1904,070, d, dom=1,map_type=2,section_type=3,chemical='w',KEY=key, zrange=[8,13], PNG=1

	FOREACH d, date_arr DO PLOT_WRF_SECTION_AVERAGE, STRING(event), STRING(scheme),75,700,2019,100, d, dom=1,map_type=2,section_type=3,zrange=[7,15],KEY=key, PNG=1


ENDIF

IF KEYWORD_SET(section_theta) THEN BEGIN
	key = 'dthdz'
;	FOREACH d, date_arr DO PLOT_WRF_SECTION_DBP, STRING(event), STRING(scheme),75,700,2019,100, d, dom=1,map_type=2,section_type=4,zrange=[7,15],KEY=key, PNG=1
	FOREACH d, date_arr DO PLOT_WRF_SECTION_DBP, STRING(event), STRING(scheme),800,475,1919,100, d, dom=1,map_type=2,section_type=4,zrange=[8,13],KEY=key, PNG=1
ENDIF

IF KEYWORD_SET(section_h2o) THEN BEGIN
	key = 'H2O'
	FOREACH d, date_arr DO PLOT_WRF_SECTION_DBP, STRING(event), STRING(scheme),75,700,2019,100, d, dom=1,map_type=2,section_type=2,chemical='H2O',zrange=[7,18],KEY=key, PNG=1
;	FOREACH d, date DO PLOT_WRF_SECTION_CRH, d, STRING(event), STRING(scheme), 1,175,380,175,map_type=1,section_type=2,chemical='O3', KEY=key, PNG=1
;	
;	key = 'BL_tracer'
;	FOREACH d, date DO PLOT_WRF_SECTION_CRH, d, STRING(event), STRING(scheme), 1,175,380,175,map_type=1,section_type=2,chemical='BL_tracer', KEY=key, PNG=1
;
;	key = 'TR_tracer'
;	FOREACH d, date DO PLOT_WRF_SECTION_CRH, d, STRING(event), STRING(scheme), 1,175,380,175,map_type=1,section_type=2,chemical='TR_tracer', KEY=key, PNG=1
;
;	key = 'STnoconv_tracer'
;	FOREACH d, date DO PLOT_WRF_SECTION_CRH, d, STRING(event), STRING(scheme), 1,175,380,175,map_type=1,section_type=2,chemical='STnoconv_tracer', KEY=key, PNG=1
ENDIF

IF KEYWORD_SET(section_temp) THEN BEGIN
	key = 'T'
	FOREACH d, date_arr DO PLOT_WRF_SECTION_DBP, STRING(event), STRING(scheme),75,700,2019,100, d, dom=1,map_type=2,section_type=2,chemical='T',zrange=[7,18],KEY=key, PNG=1
ENDIF

IF KEYWORD_SET(section_press) THEN BEGIN
	date_arr = [MAKE_DATE(2012,5,30,22),MAKE_DATE(2012,5,30,23),MAKE_DATE(2012,5,31,0),MAKE_DATE(2012,5,31,1),MAKE_DATE(2012,5,31,2),MAKE_DATE(2012,5,31,3)]
	key = 'T'
	FOREACH d, date_arr DO PLOT_WRF_SECTION_DBP, STRING(event), STRING(scheme),75,700,2019,100, d, dom=1,map_type=2,section_type=6,zrange=[7,18],KEY=key, PNG=1
ENDIF

IF KEYWORD_SET(animate) THEN BEGIN 																;Create animated gif
	PRINT, 'Writing GIF'
	WRITE_ANIMATED_GIF, STRING(event), STRING(scheme), STRING(key)
ENDIF


END