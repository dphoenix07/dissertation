PRO PLOT_WRF_SECTION_AVERAGE, experiment, state, x0, y0, x1, y1, date, $
	DOMAIN       = domain, $
	MAP_TYPE     = map_type, $
	SECTION_TYPE = section_type, $
	CHEMICAL     = chemical, $
	COORDINATES  = coordinates, $
	REGION		 = region, $
	ZRANGE       = zrange, $
	POTTEMP      = pottemp, $
	KEY			 = key, $
	EPS          = eps, $
	PNG          = png


;+
; Name:
;		PLOT_WRF_SECTION
; Purpose:
;		This is a procedure to plot a vertical cross-section of WRF output.
; Calling sequence:
;		PLOT_WRF_SECTION, date, experiment, state
; Input:
;		date       : Analysis date
;		experiment : Simulation name. (e.g., 'APR04')
;		state      : Simulation state name. (e.g., 'era')
; Output:
;		A vertical section plot.
; Keywords:
;		DOMAIN       : Domain name. Default is 'd02'.
;		MAP_TYPE     : An integer value specifying the map type. e.g., 0
;		SECTION_TYPE : An integer value specifying the cross-section type. e.g., 0
;		CHEMICAL     : String name of chemical variable for cross-section. Default is 'O3'
;		COORDINATES  : If set, cross-section end points are longitude-latitude values.
;							Otherwise, grid indices are expected.
;		ZRANGE       : Optional keyword to specify the altitude range of the cross-section in km.
;							Default is [0,20]. 
;		EPS          : If set, output to PostScript.
;		PNG          : If set, write PNG image.
; Author and history:
;		Daniel B. Phoenix	2018-10-22.	Currently only works with section_type=3
;-

COMPILE_OPT IDL2																									;Set compile options

IF (N_ELEMENTS(date      ) EQ 0) THEN date       = MAKE_DATE(2013, 5, 17, 20, 50)			;Set default input variables
IF (N_ELEMENTS(experiment) EQ 0) THEN experiment = '17MAY2013'
IF (N_ELEMENTS(state     ) EQ 0) THEN state      = 'CHEMISTRY'
IF (N_ELEMENTS(x0        ) EQ 0) THEN x0         = 0
IF (N_ELEMENTS(y0        ) EQ 0) THEN y0         = 0
IF (N_ELEMENTS(x1        ) EQ 0) THEN x1         = 1
IF (N_ELEMENTS(y1        ) EQ 0) THEN y1         = 1

IF (N_ELEMENTS(domain      ) EQ 0) THEN domain       = 2										;Set default keyword variables
IF (N_ELEMENTS(map_type    ) EQ 0) THEN map_type     = 0
IF (N_ELEMENTS(section_type) EQ 0) THEN section_type = 0
IF (N_ELEMENTS(chemical    ) EQ 0) THEN chemical     = 'O3'
IF (N_ELEMENTS(zrange      ) EQ 0) THEN zrange       = [0.0, 20.0]
IF (N_ELEMENTS(key		   ) EQ 0) THEN key 		 = chemical
date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Set date string

x     = WRF_READ_VAR('Longitude',       date, experiment, state, DOMAIN = domain)			;Read variables
y     = WRF_READ_VAR('Latitude',        date, experiment, state, DOMAIN = domain)
cloud = WRF_READ_VAR('CLOUD_MIX_TOTAL', date, experiment, state, DOMAIN = domain)
;cloud = WRF_READ_VAR('CLOUD', date, experiment, state, DOMAIN = domain)
trop  = WRF_READ_VAR('Z_trop',          date, experiment, state, DOMAIN = domain)
R 	  = WRF_READ_VAR('REFL', 		    date, experiment, state, DOMAIN = domain)

IF (KEYWORD_SET(pottemp)) THEN BEGIN
	z = WRF_READ_VAR('T', date, experiment, state, DOMAIN = domain)						;Read temperature variable from WRF output
	z.values = ((1000.0/(WRF_READ_VAR('P', date, experiment, state, $						;Compute potential temperature
	 						DOMAIN = domain)).values)^(!Rair/!Cp))*(z.values)
	theta = z.values
	zrange = [300, 400]
ENDIF ELSE BEGIN
	z = WRF_READ_VAR('Z', date, experiment, state, DOMAIN = domain)
ENDELSE 
		
ztrop1 = MEDIAN(trop.values, 30)
dim 	 = SIZE(z.values, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
xyz_trop = REBIN(ztrop1, dim[0], dim[1], dim[2], /SAMPLE)

IF (SIZE(cloud, /TNAME) NE 'STRUCT') THEN $
	cloud = WRF_READ_VAR('CLOUD', date, experiment, state, DOMAIN = domain)						;If cloud mixing ratio not available, read number concentration

IF KEYWORD_SET(coordinates) THEN BEGIN
	PRINT, 'coordinates called'
	region = [x0,y0,x1,y1]
	x.values      = (x.values + 360.0) MOD 360.0															;Ensure longitudes on 0-360 grid
	region[[0,2]] = (region[[0,2]] + 360.0) MOD 360.0
		x0=region[0]
		x1=region[2]
		y0=region[1]
		y1=region[3]
	ij = WHERE(((x.values GE x0) AND (y.values GE y0) AND $											;Find all grid points between desired endpoints of cross-section
					(x.values LE x1) AND (y.values LE y1)), nxy)
					
	IF (nxy GT 0) THEN BEGIN
		ij = ARRAY_INDICES(x.values, ij)																		;Expand indices of search to 2-D indices of grid
		x0 = MIN(ij[0,*])																							;Reset cross-section end points to grid indices
		y0 = MIN(ij[1,*])
		x1 = MAX(ij[0,*])
		y1 = MAX(ij[1,*])
	ENDIF
ENDIF

dim   = SIZE(z.values, /DIMENSIONS)																			;Get grid lengths
nx    = dim[0]
ny    = dim[1]
nz    = dim[2]

nsect = 2*LONG(SQRT((x1-x0)^2 + (y1-y0)^2))																;Compute number of points needed for cross-section
isect = MAKEN(x0, x1, nsect)																					;Set cross-section index values
jsect = MAKEN(y0, y1, nsect)
ksect = FINDGEN(nz)

iisect = REBIN(       isect,         nsect, nz, /SAMPLE)												;Expand cross-section index values to 2 dimensions
jjsect = REBIN(       jsect,         nsect, nz, /SAMPLE)
kksect = REBIN(REFORM(ksect, 1, nz), nsect, nz, /SAMPLE)

zsect0 = INTERPOLATE(z.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)					;Interpolate altitudes to cross-section path

IF (KEYWORD_SET(pottemp)) THEN BEGIN
    dz     = 0.5																					;Set altitude resolution of cross-section
    nzsect = LONG((zrange[1]-zrange[0])/dz) + 1												;Compute number of levels for cross-section based on altitude range
    zsect  = zrange[0] + dz*FINDGEN(nzsect)													;Set altitude array for plotting
ENDIF ELSE BEGIN	
    dz     = 100.0																					;Set altitude resolution of cross-section
    nzsect = LONG(1000.0*(zrange[1]-zrange[0])/dz) + 1												;Compute number of levels for cross-section based on altitude range
    zsect  = 1000.0*zrange[0] + dz*FINDGEN(nzsect)													;Set altitude array for plotting
ENDELSE

iisect = REBIN(                 isect,             nsect, nzsect, /SAMPLE)						;Compute new interpolation indices
jjsect = REBIN(                 jsect,             nsect, nzsect, /SAMPLE)
kksect = REBIN(REFORM(FINDGEN(nzsect), 1, nzsect), nsect, nzsect, /SAMPLE)

FOR i = 0, nsect-1 DO kksect[i,*] = INTERPOL(FINDGEN(nz), zsect0[i,*], zsect)

xsect = INTERPOLATE(x.values, isect, jsect)
ysect = INTERPOLATE(y.values, isect, jsect)


;PRINT, xsect[0], xsect[-1], ysect[0], ysect[-1]

CASE map_type OF
	0 : BEGIN
		 var           = WRF_READ_VAR('REFL', date, experiment, state, DOMAIN = domain)		;Read radar reflectivity
		 map_plot      = MAX(var.values, DIM = 3, /NAN)													;Compute column-maximum map
		 map_bar_title = 'Reflectivity (dBZ)'																;Set color bar title
		 map_bar_min   = 0.0																						;Set echo top minimum
		 map_bar_max   = 75.0																					;Set echo top maximum
		 map_bar_ticks = 5																						;Set number of color bar ticks
		 map_table     = [COLOR_24(230, 230, 230),VISUALIZE_88D_COLOR(0)]							;Set color table
		 map_levels    = [-100.0, 5.0*FINDGEN(N_ELEMENTS(map_table))]								;Set contour levels
		 END
	1 : BEGIN
		 z = WRF_READ_VAR('Z', date, experiment, state, DOMAIN = domain)
		 map_plot      = 0.001*(MAX((cloud.values GE 1.0E-5)*Z.values, DIM = 3, /NAN))		;Set map variable to cloud top altitude
; 		 map_plot	   = MEDIAN(map_plot1,25)
		 map_bar_title = 'Cloud Top Altitude (km)'														;Set color bar title
		 map_bar_min   = 5.0																						;Set echo top minimum
		 map_bar_max   = 20.0																					;Set echo top maximum
		 map_bar_ticks = 3																						;Set number of color bar ticks
		 map_table     = [COLOR_24(230, 230, 230),VISUALIZE_88D_COLOR(1)]							;Set color table
		 map_levels    = [0.0, 5.0 + FINDGEN(N_ELEMENTS(map_table))]								;Set contour levels
		 END
	2 : BEGIN
		 z = WRF_READ_VAR('Z', date, experiment, state, DOMAIN = domain)
		 map_plot      = 0.001*(MAX((cloud.values GE 1.0E-5)*z.values, DIM = 3, /NAN) - xyz_trop)		;Set map variable to cloud top altitude
		 map_bar_title = 'Cloud Top Relative Altitude (km)'														;Set color bar title
		 map_bar_min   = -2.5																					;Set echo top minimum
		 map_bar_max   = 2.5																					;Set echo top maximum
         map_bar_ticks = 2																						;Set number of color bar ticks
         map_table     = [COLOR_24('gray90'), BLUE_RED_24(30,0.0)]							          					    ;Set color table
         map_levels    = [-100.0, MAKEN(-2.5,2.5,30)]  									              ;Set contour levels																					;Set number of color bar ticks
		 END
	3 : BEGIN
		IF(chemical EQ 'tracers') THEN BEGIN
			 bl    =  WRF_READ_VAR('BL_tracer'	, date, experiment, state, DOMAIN = domain)					;Read trace gas from WRF output
			 tr    =  WRF_READ_VAR('TR_tracer'	, date, experiment, state, DOMAIN = domain)
			 utls  =  WRF_READ_VAR('UTLS_tracer', date, experiment, state, DOMAIN = domain)
			 st    =  WRF_READ_VAR('ST_tracer'  , date, experiment, state, DOMAIN = domain)
			 o3    = (WRF_READ_VAR('O3'			, date, experiment, state, DOMAIN = domain)).values * 1.0E3
	
			 var   = (st.values + bl.values + tr.values + utls.values)*1.0E3
			 var =  ((var-o3)/o3)*100
        	 map_plot      = (var[*,*,0])																	;Compute max tracer concentration
        	 map_bar_title = STRING(chemical)																;Set color bar title
        	 map_bar_min   = 0.0																			;Set tracer minimum
        	 map_bar_max   = 100.0																			;Set tracer maximum
        	 map_bar_ticks = 4																				;Set number of color bar ticks
        	 map_table     = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])							;Set color table
        	 map_levels    = ((map_bar_max-map_bar_min)/20.0)*FINDGEN(20)								;Set contour levels
	    ENDIF ELSE BEGIN
    		 var           = WRF_READ_VAR(chemical, date, experiment, state, DOMAIN = domain)		;Read tracer
;    		 map_plot      = (var.values[*,*,50]*1.0E06)				;for NO, ISO						;Compute max tracer concentration
;    		 map_plot      = (var.values[*,*,50]*1.0E11)					;for OH							;Compute max tracer concentration
 			 map_plot	   = (var.values[*,*,0]*1.0E3)
    		 map_bar_title = STRING(chemical)																;Set color bar title
    		 map_bar_min   = 0.0																			;Set tracer minimum
    		 map_bar_max   = 200.0																			;Set tracer maximum
    ;		 map_bar_max   = 5000.0																			;Set tracer maximum
    		 map_bar_ticks = 4																				;Set number of color bar ticks
    		 map_table     = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])							;Set color table
    		 map_levels    = ((map_bar_max-map_bar_min)/20.0)*FINDGEN(20)								;Set contour levels
    
    ;		PRINT, 'max value = ' + STRING(MAX(map_plot))
		 ENDELSE
		 END
	4 : BEGIN
		 map_plot      = (WRF_READ_VAR('Wrapping_Depth', date, experiment, state, DOMAIN = domain, WRAPPING = 1)).values		;Set map variable to cloud top altitude
		 map_bar_title = 'Anvil Wrapping Depth (km)'														;Set color bar title
		 map_bar_min   = 0.0																						;Set echo top minimum
		 map_bar_max   = 4.0																					;Set echo top maximum
		 map_bar_ticks = 3																						;Set number of color bar ticks
         map_table     = [COLOR_24(230, 230, 230),VISUALIZE_88D_COLOR(3)]							;Set color table
         map_levels    = [MAKEN(0,4.0,N_ELEMENTS(map_table))]								;Set contour levels

	     ztrop   = (WRF_READ_VAR('Z_trop', date, experiment, state, DOMAIN = domain)).values	
         dim 	 = SIZE(z.values, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
    	 ztrop    = MEDIAN(ztrop,100)
         xyz_trop = REBIN(ztrop, dim[0], dim[1], dim[2], /SAMPLE)
         
         map_plot1      = 0.001*((MAX((cloud.values GE 1.0E-5)*z.values, DIM = 3, /NAN)) - xyz_trop)															;Set map variable to cloud top altitude
         map_table1     = COLOR_24('gray50')																;Set contour cross-section specs						;Set color table
         map_levels1    = 0.0	
         cont_sect1_thick  = 1
		 END

	ELSE : MESSAGE, 'Requested map type does not exist!'
ENDCASE

CASE section_type OF
	0 : BEGIN
		 IF (map_type EQ 0) THEN $
		 	fill_sect = INTERPOLATE(var.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN) $	;If reflectivity read, then interpolate to color-fill cross-section
		 ELSE BEGIN
		 	var       = WRF_READ_VAR('REFL', date, experiment, state, DOMAIN = domain)				;Read reflectivity
		 	fill_sect = INTERPOLATE(var.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)	;Interpolate reflectivity to color-fill cross-section
		 ENDELSE
		 cont_sect = INTERPOLATE(cloud.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)	;Interpolate cloud concentration to contour cross-section
		 
		 fill_sect_table  = VISUALIZE_88D_COLOR(0)														;Set color-fill cross-section specs
		 fill_sect_levels = 5.0*FINDGEN(N_ELEMENTS(fill_sect_table))
		 sect_bar_min     = 0.0	
		 sect_bar_max     = 75.0
		 sect_bar_ticks   = 5	
		 sect_bar_title   = 'Reflectivity (dBZ)'
		 
		 cont_sect_color  = COLOR_24('gray50')																;Set contour cross-section specs
		 cont_sect_levels = 1.0E-5
		 cont_sect_thick  = 3
		 END
	1 : BEGIN
		 var = WRF_READ_VAR('T', date, experiment, state, DOMAIN = domain)						;Read temperature variable from WRF output
		 var.values = ((1000.0/(WRF_READ_VAR('P', date, experiment, state, $						;Compute potential temperature
		 						DOMAIN = domain)).values)^(!Rair/!Cp))*(var.values)
	
		 fill_sect = INTERPOLATE(cloud.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)	;Interpolate cloud concentration to color-fill cross-section
		 cont_sect = INTERPOLATE(var.values,   iisect, jjsect, kksect, MISSING = !Values.F_NaN)	;Interpolate potential temperature to contour cross-section
		 
		 fill_sect_table  = [COLOR_24('white'), HCL_COLOR_TABLE(7, $								;Set color-fill cross-section specs
		 								HUE_RANGE = [180.0, 260.0], SAT_RANGE = [0.2,0.6])]
		 fill_sect_levels = [0.0, 1.0E-9, 1.0E-8, 1.0E-7, 1.0E-6, 1.0E-5, 1.0E-4, 1.0E-3]
		 sect_bar_min     = 0.0	
		 sect_bar_max     = 1.0
		 sect_bar_ticks   = 8	
		 sect_bar_title   = 'Cloud Particle Concentration (kg kg^-1)'
		 sect_tick_name   = ['0.0', '1e-9', '1e-8', '1e-7', '1e-6', '1e-5', '1e-4', '1e-3', '1e-2']
		 
		 cont_sect_color  = COLOR_24('black')																;Set contour cross-section specs
		 cont_sect_levels = 200.0 + 2.0*FINDGEN(310)
		 cont_sect_thick  = 1
		 END
	2 : BEGIN
		IF(chemical EQ 'tracers') THEN BEGIN
			 bl    =  WRF_READ_VAR('BL_tracer'	, date, experiment, state, DOMAIN = domain)					;Read trace gas from WRF output
			 tr    =  WRF_READ_VAR('TR_tracer'	, date, experiment, state, DOMAIN = domain)
			 utls  =  WRF_READ_VAR('UTLS_tracer', date, experiment, state, DOMAIN = domain)
			 st    =  WRF_READ_VAR('ST_tracer'  , date, experiment, state, DOMAIN = domain)
			 o3    = (WRF_READ_VAR('O3'			, date, experiment, state, DOMAIN = domain)).values * 1.0E3
	
			 var   = (st.values + bl.values + tr.values + utls.values)
			 var   = (utls.values)*1.0E3 
			 var   = (var/o3)*100.0
			 fill_sect = INTERPOLATE(var, iisect, jjsect, kksect, MISSING = !Values.F_NaN)			;Interpolate trace gas concentration to color-fill cross-section

	    ENDIF 
		IF (chemical NE 'tracers') THEN BEGIN
		 	 var = WRF_READ_VAR(chemical, date, experiment, state, DOMAIN = domain)					;Read trace gas from WRF output
	 		 IF (chemical NE 'O3_tracer') THEN ozone = (WRF_READ_VAR('O3_tracer', date, experiment, state, DOMAIN = domain)).values * 1.0E3
	 		 ij = [x0, y0, x1, y1]	 
             nsect = 2*LONG(SQRT((x1-x0)^2 + (y1-y0)^2))																;Compute number of points needed for cross-section
             
             npath1 = 5
             xysect = MAP_2POINTS((x.values)[ij[0],ij[1]],(y.values)[ij[0],ij[1]],$
             							(x.values)[ij[2],ij[3]],(y.values)[ij[2],ij[3]], NPATH = npath1)
             
             parmsect = MAP_2POINTS((x.values)[ij[0],ij[1]],(y.values)[ij[0],ij[1]],$
             							(x.values)[ij[2],ij[3]],(y.values)[ij[2],ij[3]], /RHUMB)
             							
             perp = parmsect[1]*!DDTOR																								;Find angle of cross section path
             
             xy02 = LL_ARC_DISTANCE([((x.values)[ij[0],ij[1]]),((y.values)[ij[0],ij[1]])]*!DDTOR, 20.0/6371.0,perp+(!PI*0.5))		;Find point 20km from starting point perpendicular to section
             xy03 = LL_ARC_DISTANCE([((x.values)[ij[0],ij[1]]),((y.values)[ij[0],ij[1]])]*!DDTOR, 20.0/6371.0,perp-(!PI*0.5))
             
             xy04 = LL_ARC_DISTANCE([((x.values)[ij[2],ij[3]]),((y.values)[ij[2],ij[3]])]*!DDTOR, 20.0/6371.0,perp+(!PI*0.5))
             xy05 = LL_ARC_DISTANCE([((x.values)[ij[2],ij[3]]),((y.values)[ij[2],ij[3]])]*!DDTOR, 20.0/6371.0,perp-(!PI*0.5))
             
             xysect2 = MAP_2POINTS((x.values)[ij[0],ij[1]]*!DDTOR,(y.values)[ij[0],ij[1]]*!DDTOR,$
             							xy02[0],xy02[1], NPATH = npath1, /RADIANS)
             
             xysect3 = MAP_2POINTS((x.values)[ij[0],ij[1]]*!DDTOR,(y.values)[ij[0],ij[1]]*!DDTOR,$
             							xy03[0],xy03[1], NPATH = npath1, /RADIANS)
             
             xysect4 = MAP_2POINTS((x.values)[ij[2],ij[3]]*!DDTOR,(y.values)[ij[2],ij[3]]*!DDTOR,$
             							xy04[0],xy04[1], NPATH = npath1, /RADIANS)
             
             xysect5 = MAP_2POINTS((x.values)[ij[2],ij[3]]*!DDTOR,(y.values)[ij[2],ij[3]]*!DDTOR,$
             							xy05[0],xy05[1], NPATH = npath1, /RADIANS)
             
             ;; Do northern path
             section_array = [ ]
             cont_array    = [ ]
             cont1_array   = [ ]
             ozone_array   = [ ]
             FOR ii = 0, npath1-1 DO BEGIN
            	 region = [xysect3[0:1,ii],xysect5[0:1,ii]]*!DRADDEG
            	 x.values      = (x.values + 360.0) MOD 360.0												;Ensure longitudes on 0-360 grid
            	 region[[0,2]] = (region[[0,2]] + 360.0) MOD 360.0
            	 x0=region[0]
            	 x1=region[2]
            	 y0=region[1]
            	 y1=region[3]
            	 
            	 ;;Find index of nearest start point
            	 i0 = WHERE((ABS(x.values - x0) LT 0.01) AND (ABS(y.values - y0) LT 0.01))
            	 IF (i0[0] EQ -1) THEN i1 = WHERE((ABS(x.values - x0) LT 0.1) AND (ABS(y.values - y0) LT 0.1))
             
            	 store_arr = []
            	 FOR xy = 0, N_ELEMENTS(i0)-1 DO BEGIN
            	 	store = SQRT((x.values[i0[xy]] - x0)^2 + (y.values[i0[xy]] - y0)^2)
            	 	store_arr = [store_arr, store]
            	 ENDFOR
            	 
            	 ij_start = WHERE(MIN(store_arr,/NAN) EQ store_arr)
            	 ij_start = ARRAY_INDICES(x.values, i0[ij_start])
             
            	 ;;Find index of nearest end point
            	 i1 = WHERE((ABS(x.values - x1) LT 0.01) AND (ABS(y.values - y1) LT 0.01))	
            	 IF (i1[0] EQ -1) THEN i1 = WHERE((ABS(x.values - x1) LT 0.1) AND (ABS(y.values - y1) LT 0.1))
            	 
            	 store_arr = []
            	 FOR xy = 0, N_ELEMENTS(i1)-1 DO BEGIN
            	 	store = SQRT((x.values[i1[xy]] - x0)^2 + (y.values[i1[xy]] - y0)^2)
            	 	store_arr = [store_arr, store]
            	 ENDFOR
            	 
            	 ij_end = WHERE(MIN(store_arr,/NAN) EQ store_arr)
            	 ij_end = ARRAY_INDICES(x.values, i1[ij_end])
            	 		
            	 region_new = [ij_start[0],ij_start[1],ij_end[0],ij_end[1]]
            	 x0 = ij_start[0]
            	 y0 = ij_start[1]
            	 x1 = ij_end[0]
            	 y1 = ij_end[1]
             
            	 ;; Now with new indices, create section
                 isect = MAKEN(x0, x1, nsect)																					;Set cross-section index values
                 jsect = MAKEN(y0, y1, nsect)
                 ksect = FINDGEN(nz)
                 
                 iisect = REBIN(       isect,         nsect, nz, /SAMPLE)												;Expand cross-section index values to 2 dimensions
                 jjsect = REBIN(       jsect,         nsect, nz, /SAMPLE)
                 kksect = REBIN(REFORM(ksect, 1, nz), nsect, nz, /SAMPLE)
                 
                 zsect0 = INTERPOLATE(z.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)					;Interpolate altitudes to cross-section path
                 
                 IF (KEYWORD_SET(pottemp)) THEN BEGIN
                     dz     = 0.5																					;Set altitude resolution of cross-section
                     nzsect = LONG((zrange[1]-zrange[0])/dz) + 1												;Compute number of levels for cross-section based on altitude range
                     zsect  = zrange[0] + dz*FINDGEN(nzsect)													;Set altitude array for plotting
                 ENDIF ELSE BEGIN	
                     dz     = 100.0																					;Set altitude resolution of cross-section
                     nzsect = LONG(1000.0*(zrange[1]-zrange[0])/dz) + 1												;Compute number of levels for cross-section based on altitude range
                     zsect  = 1000.0*zrange[0] + dz*FINDGEN(nzsect)													;Set altitude array for plotting
                 ENDELSE
                 
                 iisect = REBIN(                 isect,             nsect, nzsect, /SAMPLE)						;Compute new interpolation indices
                 jjsect = REBIN(                 jsect,             nsect, nzsect, /SAMPLE)
                 kksect = REBIN(REFORM(FINDGEN(nzsect), 1, nzsect), nsect, nzsect, /SAMPLE)
                 
                 FOR i = 0, nsect-1 DO kksect[i,*] = INTERPOL(FINDGEN(nz), zsect0[i,*], zsect)
                 
                 xsect = INTERPOLATE(x.values, isect, jsect)
                 ysect = INTERPOLATE(y.values, isect, jsect)
                 
                 section = INTERPOLATE(var.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)				;Interpolate wind speeds to contour cross-section		 
	        	 
                 dim = SIZE(var_sect,/DIMENSIONS)
	        	 section_array = [[[section_array]], [[section]]]
         		 IF (chemical NE 'O3_tracer') THEN BEGIN
         		 	ozone_sec   = INTERPOLATE(ozone, iisect, jjsect, kksect, MISSING = !Values.F_NaN)
         		 	ozone_array = [[[ozone_array]], [[ozone_sec]]]
         		 ENDIF
         		  
                 IF (~KEYWORD_SET(pottemp)) THEN BEGIN
                     theta = WRF_READ_VAR('T', date, experiment, state, DOMAIN = domain)						;Read temperature variable from WRF output
                     theta.values = ((1000.0/(WRF_READ_VAR('P', date, experiment, state, $						;Compute potential temperature
                     						DOMAIN = domain)).values)^(!Rair/!Cp))*(theta.values)
                     cont_sect  = INTERPOLATE(theta.values,   iisect, jjsect, kksect, MISSING = !Values.F_NaN)	;Interpolate potential temperature to contour cross-section
                 ENDIF
              
	             cont_sect1 = INTERPOLATE(cloud.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)	;Interpolate cloud concentration to contour cross-section
              
	             cont_array  = [[[cont_array ]], [[cont_sect ]]]
	             cont1_array = [[[cont1_array]], [[cont_sect1]]]

			ENDFOR
	        section_ave_north = MEAN(section_array, DIM = 3, /NAN)
	        cont_ave_north    = MEAN(cont_array,    DIM = 3, /NAN)
	        cont1_ave_north   = MEAN(cont1_array,   DIM = 3, /NAN)
	        ozone_ave_north   = MEAN(ozone_array,   DIM = 3, /NAN)
        
            ;; Do southern path
            section_array = [ ]
            cont_array    = [ ]
            cont1_array   = [ ]
            ozone_array   = [ ]
            FOR ii = 0, npath1-1 DO BEGIN
            	region = [xysect2[0:1,ii],xysect4[0:1,ii]]*!DRADDEG
            	x.values      = (x.values + 360.0) MOD 360.0												;Ensure longitudes on 0-360 grid
            	region[[0,2]] = (region[[0,2]] + 360.0) MOD 360.0
            	x0=region[0]
            	x1=region[2]
            	y0=region[1]
            	y1=region[3]
            	
            	;;Find index of nearest start point
            	i0 = WHERE((ABS(x.values - x0) LT 0.01) AND (ABS(y.values - y0) LT 0.01))
            	IF (i0[0] EQ -1) THEN i0 = WHERE((ABS(x.values - x0) LT 0.1) AND (ABS(y.values - y0) LT 0.1))
            	
            	store_arr = []
            	FOR xy = 0, N_ELEMENTS(i0)-1 DO BEGIN
            		store = SQRT((x.values[i0[xy]] - x0)^2 + (y.values[i0[xy]] - y0)^2)
            		store_arr = [store_arr, store]
            	ENDFOR
            	
            	ij_start = WHERE(MIN(store_arr,/NAN) EQ store_arr)
            	ij_start = ARRAY_INDICES(x.values, i0[ij_start])
            
            	;;Find index of nearest end point
            	i1 = WHERE((ABS(x.values - x1) LT 0.01) AND (ABS(y.values - y1) LT 0.01))
            	IF (i1[0] EQ -1) THEN i1 = WHERE((ABS(x.values - x1) LT 0.1) AND (ABS(y.values - y1) LT 0.1))
            
            	store_arr = []
            	FOR xy = 0, N_ELEMENTS(i1)-1 DO BEGIN
            		store = SQRT((x.values[i1[xy]] - x0)^2 + (y.values[i1[xy]] - y0)^2)
            		store_arr = [store_arr, store]
            	ENDFOR
            	
            	ij_end = WHERE(MIN(store_arr,/NAN) EQ store_arr)
            	ij_end = ARRAY_INDICES(x.values, i1[ij_end])
            			
            	region_new = [ij_start[0],ij_start[1],ij_end[0],ij_end[1]]
            	x0 = ij_start[0]
            	y0 = ij_start[1]
            	x1 = ij_end[0]
            	y1 = ij_end[1]
            
            	;; Now with new indices, create section 
                isect = MAKEN(x0, x1, nsect)																					;Set cross-section index values
                jsect = MAKEN(y0, y1, nsect)
                ksect = FINDGEN(nz)
                
                iisect = REBIN(       isect,         nsect, nz, /SAMPLE)												;Expand cross-section index values to 2 dimensions
                jjsect = REBIN(       jsect,         nsect, nz, /SAMPLE)
                kksect = REBIN(REFORM(ksect, 1, nz), nsect, nz, /SAMPLE)
                
                zsect0 = INTERPOLATE(z.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)					;Interpolate altitudes to cross-section path
                
                IF (KEYWORD_SET(pottemp)) THEN BEGIN
                    dz     = 0.5																					;Set altitude resolution of cross-section
                    nzsect = LONG((zrange[1]-zrange[0])/dz) + 1												;Compute number of levels for cross-section based on altitude range
                    zsect  = zrange[0] + dz*FINDGEN(nzsect)													;Set altitude array for plotting
                ENDIF ELSE BEGIN	
                    dz     = 100.0																					;Set altitude resolution of cross-section
                    nzsect = LONG(1000.0*(zrange[1]-zrange[0])/dz) + 1												;Compute number of levels for cross-section based on altitude range
                    zsect  = 1000.0*zrange[0] + dz*FINDGEN(nzsect)													;Set altitude array for plotting
                ENDELSE
                
                iisect = REBIN(                 isect,             nsect, nzsect, /SAMPLE)						;Compute new interpolation indices
                jjsect = REBIN(                 jsect,             nsect, nzsect, /SAMPLE)
                kksect = REBIN(REFORM(FINDGEN(nzsect), 1, nzsect), nsect, nzsect, /SAMPLE)
                
                FOR i = 0, nsect-1 DO kksect[i,*] = INTERPOL(FINDGEN(nz), zsect0[i,*], zsect)
                
                xsect = INTERPOLATE(x.values, isect, jsect)
                ysect = INTERPOLATE(y.values, isect, jsect)
	         
                section = INTERPOLATE(var.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)				;Interpolate wind speeds to contour cross-section		 	        	
	        	section_array = [[[section_array]], [[section]]]
                IF (chemical NE 'O3_tracer') THEN BEGIN
         		 	ozone_sec   = INTERPOLATE(ozone, iisect, jjsect, kksect, MISSING = !Values.F_NaN)
         		 	ozone_array = [[[ozone_array]], [[ozone_sec]]]
         		ENDIF

                IF (~KEYWORD_SET(pottemp)) THEN BEGIN
                    theta = WRF_READ_VAR('T', date, experiment, state, DOMAIN = domain)						;Read temperature variable from WRF output
                    theta.values = ((1000.0/(WRF_READ_VAR('P', date, experiment, state, $						;Compute potential temperature
                    						DOMAIN = domain)).values)^(!Rair/!Cp))*(theta.values)
                    cont_sect  = INTERPOLATE(theta.values,   iisect, jjsect, kksect, MISSING = !Values.F_NaN)	;Interpolate potential temperature to contour cross-section
                ENDIF
             
	            cont_sect1 = INTERPOLATE(cloud.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)	;Interpolate cloud concentration to contour cross-section
             
	            cont_array  = [[[cont_array ]], [[cont_sect ]]]
	            cont1_array = [[[cont1_array]], [[cont_sect1]]]
	        ENDFOR

            section_ave_south = MEAN(section_array, DIM = 3, /NAN)
            cont_ave_south    = MEAN(cont_array,    DIM = 3, /NAN)
            cont1_ave_south   = MEAN(cont1_array,   DIM = 3, /NAN)
            ozone_ave_south   = MEAN(ozone_array,   DIM = 3, /NAN)
            	 
            section_ave = [[[section_ave_north]], [[section_ave_south]]] 
            cont_ave    = [[[cont_ave_north   ]], [[cont_ave_south   ]]]
            cont1_ave   = [[[cont1_ave_north  ]], [[cont1_ave_south  ]]]
            ozone_ave   = [[[ozone_ave_north  ]], [[ozone_ave_south  ]]]
            
            fill_sect  = MEAN(section_ave, DIM = 3, /NAN) 
            cont_sect  = MEAN(cont_ave,    DIM = 3, /NAN)
            cont_sect1 = MEAN(cont1_ave,   DIM = 3, /NAN)
            ozone_sect = MEAN(ozone_ave,   DIM = 3, /NAN)	
		ENDIF

		 CASE chemical OF
		 	'O3' : BEGIN 
		 			 fill_sect        = (1.0E3)*fill_sect													;Convert to ppbv
					 fill_sect_table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])			;Set color-fill cross-section specs
					 sect_bar_min     = 0.0	
					 sect_bar_max     = 400.0
					 fill_sect_levels = ((sect_bar_max-sect_bar_min)/20.0)*FINDGEN(20)
					 sect_bar_ticks   = 4
					 sect_bar_title   = 'Ozone Concentration (ppbv)'
					 END
		 	'O3_tracer' : BEGIN 
		 			 fill_sect        = (1.0E3)*fill_sect													;Convert to ppbv
					 fill_sect_table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])			;Set color-fill cross-section specs
					 sect_bar_min     = 0.0	
					 sect_bar_max     = 400.0
					 fill_sect_levels = ((sect_bar_max-sect_bar_min)/20.0)*FINDGEN(20)
					 sect_bar_ticks   = 4
					 sect_bar_title   = 'Ozone Concentration (ppbv)'
					 END
		 	'CO' : BEGIN 
		 			 fill_sect        = (1.0E3)*fill_sect													;Convert to ppbv
					 fill_sect_table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])			;Set color-fill cross-section specs
					 sect_bar_min     = 0.0	
					 sect_bar_max     = 200.0
					 fill_sect_levels = ((sect_bar_max-sect_bar_min)/20.0)*FINDGEN(20)
					 sect_bar_ticks   = 4
					 sect_bar_title   = 'Carbon Monoxide Concentration (ppbv)'
					 END
		 	'CO_tracer' : BEGIN 
		 			 fill_sect        = (1.0E3)*fill_sect													;Convert to ppbv
					 fill_sect_table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])			;Set color-fill cross-section specs
					 sect_bar_min     = 0.0	
					 sect_bar_max     = 200.0
					 fill_sect_levels = ((sect_bar_max-sect_bar_min)/20.0)*FINDGEN(20)
					 sect_bar_ticks   = 4
					 sect_bar_title   = 'Carbon Monoxide Concentration (ppbv)'
					 END
		 	'H2O' : BEGIN 
		 			 fill_sect        = (1.0E6)*fill_sect													;Convert to ppbv
					 fill_sect_table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])			;Set color-fill cross-section specs
					 sect_bar_min     = 0.0	
					 sect_bar_max     = 200.0
					 fill_sect_levels = ((sect_bar_max-sect_bar_min)/20.0)*FINDGEN(20)
					 sect_bar_ticks   = 4
					 sect_bar_title   = 'Water Vapor Concentration (ppmv)'
					 END
		 	'NO2' : BEGIN 
		 			 fill_sect        = (1.0E6)*fill_sect													;Convert to ppbv
					 fill_sect_table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])			;Set color-fill cross-section specs
					 sect_bar_min     = 0.0	
					 sect_bar_max     = 80.0
					 fill_sect_levels = ((sect_bar_max-sect_bar_min)/20.0)*FINDGEN(20)
					 sect_bar_ticks   = 4
					 sect_bar_title   = 'NOx Concentration (pptv)'
					 END
		 	'HO' : BEGIN 
		 			 fill_sect        = (1.0E11)*fill_sect	
					 fill_sect_table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])			;Set color-fill cross-section specs
					 sect_bar_min     = 0.0	
					 sect_bar_max     = 100.0
					 fill_sect_levels = ((sect_bar_max-sect_bar_min)/20.0)*FINDGEN(20)
					 sect_bar_ticks   = 4
					 sect_bar_title   = 'OH Concentration (pptv)'
					 END
		 	'HO2' : BEGIN 
		 			 fill_sect        = (1.0E6)*fill_sect													;Convert to ppbv
					 fill_sect_table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])			;Set color-fill cross-section specs
					 sect_bar_min     = 0.0	
					 sect_bar_max     = 10.0
					 fill_sect_levels = ((sect_bar_max-sect_bar_min)/20.0)*FINDGEN(20)
					 sect_bar_ticks   = 4
					 sect_bar_title   = 'HO2 Concentration (pptv)'
					 END
		 	'H2' : BEGIN 
		 			 fill_sect        = (1.0E6)*fill_sect													;Convert to ppbv
					 fill_sect_table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])			;Set color-fill cross-section specs
					 sect_bar_min     = 0.0	
					 sect_bar_max     = 10.0
					 fill_sect_levels = ((sect_bar_max-sect_bar_min)/20.0)*FINDGEN(20)
					 sect_bar_ticks   = 4
					 sect_bar_title   = 'H2 Concentration (pptv)'
					 END
		 	'NO' : BEGIN 
		 			 fill_sect        = (1.0E9)*fill_sect													;Convert to ppbv
					 fill_sect_table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])			;Set color-fill cross-section specs
					 sect_bar_min     = 0.0	
					 sect_bar_max     = 5000.0
					 fill_sect_levels = ((sect_bar_max-sect_bar_min)/20.0)*FINDGEN(20)
					 sect_bar_ticks   = 4
					 sect_bar_title   = 'NO Concentration (pptv)'
					 END
		 	'ISO' : BEGIN 
		 			 fill_sect        = (1.0E6)*fill_sect													;Convert to ppbv
					 fill_sect_table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])			;Set color-fill cross-section specs
					 sect_bar_min     = 0.0	
					 sect_bar_max     = 100.0
					 fill_sect_levels = ((sect_bar_max-sect_bar_min)/20.0)*FINDGEN(20)
					 sect_bar_ticks   = 4
					 sect_bar_title   = 'Isoprene Concentration (pptv)'
					 END
			'ASOA1J' : BEGIN
		 			 fill_sect        = (1.0E3)*fill_sect													;Convert to ppbv
					 PRINT, fill_sect*1.0E3
					 fill_sect_table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])			;Set color-fill cross-section specs
					 sect_bar_min     = 0.0	
					 sect_bar_max     = 500.0
					 fill_sect_levels = ((sect_bar_max-sect_bar_min)/20.0)*FINDGEN(20)
					 sect_bar_ticks   = 4
					 sect_bar_title   = 'ASOA1J Concentration (ug/kg-dryair)'
					 END					
			'ASOA1I' : BEGIN
		 			 fill_sect        = (1.0E3)*fill_sect													;Convert to ppbv
					 PRINT, fill_sect*1.0E3
					 fill_sect_table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])			;Set color-fill cross-section specs
					 sect_bar_min     = 0.0	
					 sect_bar_max     = 500.0
					 fill_sect_levels = ((sect_bar_max-sect_bar_min)/20.0)*FINDGEN(20)
					 sect_bar_ticks   = 4
					 sect_bar_title   = 'ASOA1I Concentration (ug/kg-dryair)'
					 END					
			'BSOA1J' : BEGIN
		 			 fill_sect        = (1.0E7)*fill_sect													;Convert to ppbv
					 PRINT, fill_sect*1.0E7
					 fill_sect_table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])			;Set color-fill cross-section specs
					 sect_bar_min     = 0.0	
					 sect_bar_max     = 500.0
					 fill_sect_levels = ((sect_bar_max-sect_bar_min)/20.0)*FINDGEN(20)
					 sect_bar_ticks   = 4
					 sect_bar_title   = 'BSOA1J Concentration (ug/kg-dryair)'
					 END					

			'BSOA1I' : BEGIN
		 			 fill_sect        = (1.0E7)*fill_sect													;Convert to ppbv
					 PRINT, fill_sect*1.0E7
					 fill_sect_table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])			;Set color-fill cross-section specs
					 sect_bar_min     = 0.0	
					 sect_bar_max     = 500.0
					 fill_sect_levels = ((sect_bar_max-sect_bar_min)/20.0)*FINDGEN(20)
					 sect_bar_ticks   = 4
					 sect_bar_title   = 'BSOA1I Concentration (ug/kg-dryair)'
					 END					

			'BL_tracer' : BEGIN
		 			 fill_sect        = (1000.0)*fill_sect													;Convert to ppbv
					 fill_sect_table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])			;Set color-fill cross-section specs
					 sect_bar_min     = 0.0	
					 sect_bar_max     = 100.0
					 fill_sect_levels = ((sect_bar_max-sect_bar_min)/20.0)*FINDGEN(20)
					 sect_bar_ticks   = 4
					 sect_bar_title   = 'BL_tracer'
					 END

			'TR_tracer' : BEGIN
		 			 fill_sect        = (1000.0)*fill_sect													;Convert to ppbv
					 fill_sect_table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])			;Set color-fill cross-section specs
					 sect_bar_min     = 0.0	
					 sect_bar_max     = 300.0
					 fill_sect_levels = ((sect_bar_max-sect_bar_min)/20.0)*FINDGEN(20)
					 sect_bar_ticks   = 4
					 sect_bar_title   = 'TR_tracer'
					 END
			'ST_tracer' : BEGIN
		 			 fill_sect        = (1000.0)*fill_sect													;Convert to ppbv
					 fill_sect_table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])			;Set color-fill cross-section specs
					 sect_bar_min     = 0.0	
					 sect_bar_max     = 400.0
					 fill_sect_levels = ((sect_bar_max-sect_bar_min)/20.0)*FINDGEN(20)
					 sect_bar_ticks   = 4
					 sect_bar_title   = 'ST_tracer'
					 END
			'UTLS_tracer' : BEGIN
		 			 fill_sect        = (1000.0)*fill_sect													;Convert to ppbv
					 fill_sect_table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])			;Set color-fill cross-section specs
					 sect_bar_min     = 0.0	
					 sect_bar_max     = 200.0
					 fill_sect_levels = ((sect_bar_max-sect_bar_min)/20.0)*FINDGEN(20)
					 sect_bar_ticks   = 4
					 sect_bar_title   = 'UTLS_tracer'
					 END

			'Updraft_tracer' : BEGIN
		 			 fill_sect        = (1.0)*fill_sect													;Convert to ppbv
					 fill_sect_table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])			;Set color-fill cross-section specs
					 sect_bar_min     = 0.0
					 sect_bar_max     = 1.0
					 fill_sect_levels = ((sect_bar_max-sect_bar_min)/20.0)*FINDGEN(20)
					 sect_bar_ticks   = 4
					 sect_bar_title   = 'Updraft_tracer'
					 END

			'Cloud_tracer' : BEGIN
		 			 fill_sect        = (100.0)*fill_sect													;Convert to ppbv
					 fill_sect_table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])			;Set color-fill cross-section specs
					 sect_bar_min     = 0.0	
					 sect_bar_max     = 100.0
					 fill_sect_levels = ((sect_bar_max-sect_bar_min)/20.0)*FINDGEN(20)
					 sect_bar_ticks   = 4
					 sect_bar_title   = 'Cloud_tracer'
					 END

			'w' : BEGIN
		 			 fill_sect        = fill_sect													;Convert to ppbv
					 sect_bar_min     = -5.0	
					 sect_bar_max     = 5.0
					 fill_sect_levels = (((sect_bar_max-sect_bar_min)/20.0)*FINDGEN(20) + sect_bar_min)
					 fill_sect_table  = BLUE_RED_24(N_ELEMENTS(fill_sect_levels))
					 sect_bar_ticks   = 4
					 sect_bar_title   = 'Vertical Velocity (m/s)'
					 END

			'T' : BEGIN
		 			 fill_sect        = fill_sect													
					 sect_bar_min     = 200.0
					 sect_bar_max     = 235.0
					 fill_sect_levels = (((sect_bar_max-sect_bar_min)/50.0)*FINDGEN(50) + sect_bar_min)
;					 fill_sect_table  = BLUE_RED_24(N_ELEMENTS(fill_sect_levels))
					 fill_sect_table  = HCL_COLOR_TABLE(N_ELEMENTS(fill_sect_levels), HUE_RANGE = [100.0, 300.0])			;Set color-fill cross-section specs
					 sect_bar_ticks   = 4
					 sect_bar_title   = 'Temperature (K)'
					 END

			'tracers'	: BEGIN
		 			 bl    =  WRF_READ_VAR('BL_tracer'	, date, experiment, state, DOMAIN = domain)					;Read trace gas from WRF output
					 tr    =  WRF_READ_VAR('TR_tracer'	, date, experiment, state, DOMAIN = domain)
					 utls  =  WRF_READ_VAR('UTLS_tracer', date, experiment, state, DOMAIN = domain)
					 st    =  WRF_READ_VAR('ST_tracer'  , date, experiment, state, DOMAIN = domain)
					 o3    = (WRF_READ_VAR('O3'			, date, experiment, state, DOMAIN = domain)).values * 1.0E3
	
;					 var   = (st.values + bl.values + tr.values + utls.values) * 1.0E3
					 var   = (tr.values)*1.0E3 
;					 var =  (var - o3)
					 var = ((o3-var)/o3)*100.0
					 
					 fill_sect = INTERPOLATE(var,   iisect, jjsect, kksect, MISSING = !Values.F_NaN)			;Interpolate trace gas concentration to color-fill cross-section
					 fill_sect_table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])			;Set color-fill cross-section specs
					 sect_bar_min     = 0.0	
					 sect_bar_max     = 100.0
					 fill_sect_levels = ((sect_bar_max-sect_bar_min)/20.0)*FINDGEN(20)
					 sect_bar_ticks   = 4
					 sect_bar_title   = '% o3 chemically produced'

					 END
			 ELSE : MESSAGE, 'Requested chemical does not exist!'										;Message error

		     ENDCASE
		     END
	3 : BEGIN
		 storm_motion = 15.0
         u = (WRF_READ_VAR('u', date, experiment, state, DOMAIN = domain)).values						;Read temperature variable from WRF output
         v = (WRF_READ_VAR('v', date, experiment, state, DOMAIN = domain)).values	
         w = (WRF_READ_VAR('w', date, experiment, state, DOMAIN = domain)).values
  
 		 ij = [x0, y0, x1, y1]	 
     nsect = 2*LONG(SQRT((x1-x0)^2 + (y1-y0)^2))																;Compute number of points needed for cross-section
     
     npath1 = 5
     xysect = MAP_2POINTS((x.values)[ij[0],ij[1]],(y.values)[ij[0],ij[1]],$
     							(x.values)[ij[2],ij[3]],(y.values)[ij[2],ij[3]], NPATH = npath1)
     
     parmsect = MAP_2POINTS((x.values)[ij[0],ij[1]],(y.values)[ij[0],ij[1]],$
     							(x.values)[ij[2],ij[3]],(y.values)[ij[2],ij[3]], /RHUMB)
     							
     perp = parmsect[1]*!DDTOR																								;Find angle of cross section path
     
     xy02 = LL_ARC_DISTANCE([((x.values)[ij[0],ij[1]]),((y.values)[ij[0],ij[1]])]*!DDTOR, 20.0/6371.0,perp+(!PI*0.5))		;Find point 20km from starting point perpendicular to section
     xy03 = LL_ARC_DISTANCE([((x.values)[ij[0],ij[1]]),((y.values)[ij[0],ij[1]])]*!DDTOR, 20.0/6371.0,perp-(!PI*0.5))
     
     xy04 = LL_ARC_DISTANCE([((x.values)[ij[2],ij[3]]),((y.values)[ij[2],ij[3]])]*!DDTOR, 20.0/6371.0,perp+(!PI*0.5))
     xy05 = LL_ARC_DISTANCE([((x.values)[ij[2],ij[3]]),((y.values)[ij[2],ij[3]])]*!DDTOR, 20.0/6371.0,perp-(!PI*0.5))
     
     xysect2 = MAP_2POINTS((x.values)[ij[0],ij[1]]*!DDTOR,(y.values)[ij[0],ij[1]]*!DDTOR,$
     							xy02[0],xy02[1], NPATH = npath1, /RADIANS)
     
     xysect3 = MAP_2POINTS((x.values)[ij[0],ij[1]]*!DDTOR,(y.values)[ij[0],ij[1]]*!DDTOR,$
     							xy03[0],xy03[1], NPATH = npath1, /RADIANS)
     
     xysect4 = MAP_2POINTS((x.values)[ij[2],ij[3]]*!DDTOR,(y.values)[ij[2],ij[3]]*!DDTOR,$
     							xy04[0],xy04[1], NPATH = npath1, /RADIANS)
     
     xysect5 = MAP_2POINTS((x.values)[ij[2],ij[3]]*!DDTOR,(y.values)[ij[2],ij[3]]*!DDTOR,$
     							xy05[0],xy05[1], NPATH = npath1, /RADIANS)
     
     ;; Do northern path
     section_array = [ ]
     cont_array    = [ ]
     cont1_array   = [ ]
     
     FOR ii = 0, npath1-1 DO BEGIN
    	 region = [xysect3[0:1,ii],xysect5[0:1,ii]]*!DRADDEG
    	 x.values      = (x.values + 360.0) MOD 360.0												;Ensure longitudes on 0-360 grid
    	 region[[0,2]] = (region[[0,2]] + 360.0) MOD 360.0
    	 x0=region[0]
    	 x1=region[2]
    	 y0=region[1]
    	 y1=region[3]
    	 
    	 ;;Find index of nearest start point
    	 i0 = WHERE((ABS(x.values - x0) LT 0.01) AND (ABS(y.values - y0) LT 0.01))
    	 IF (i0[0] EQ -1) THEN i1 = WHERE((ABS(x.values - x0) LT 0.1) AND (ABS(y.values - y0) LT 0.1))
     
    	 store_arr = []
    	 FOR xy = 0, N_ELEMENTS(i0)-1 DO BEGIN
    	 	store = SQRT((x.values[i0[xy]] - x0)^2 + (y.values[i0[xy]] - y0)^2)
    	 	store_arr = [store_arr, store]
    	 ENDFOR
    	 
    	 ij_start = WHERE(MIN(store_arr,/NAN) EQ store_arr)
    	 ij_start = ARRAY_INDICES(x.values, i0[ij_start])
     
    	 ;;Find index of nearest end point
    	 i1 = WHERE((ABS(x.values - x1) LT 0.01) AND (ABS(y.values - y1) LT 0.01))	
    	 IF (i1[0] EQ -1) THEN i1 = WHERE((ABS(x.values - x1) LT 0.1) AND (ABS(y.values - y1) LT 0.1))
    	 
    	 store_arr = []
    	 FOR xy = 0, N_ELEMENTS(i1)-1 DO BEGIN
    	 	store = SQRT((x.values[i1[xy]] - x0)^2 + (y.values[i1[xy]] - y0)^2)
    	 	store_arr = [store_arr, store]
    	 ENDFOR
    	 
    	 ij_end = WHERE(MIN(store_arr,/NAN) EQ store_arr)
    	 ij_end = ARRAY_INDICES(x.values, i1[ij_end])
    	 		
    	 region_new = [ij_start[0],ij_start[1],ij_end[0],ij_end[1]]
    	 x0 = ij_start[0]
    	 y0 = ij_start[1]
    	 x1 = ij_end[0]
    	 y1 = ij_end[1]
     
    	 ;; Now with new indices, create section
        isect = MAKEN(x0, x1, nsect)																					;Set cross-section index values
        jsect = MAKEN(y0, y1, nsect)
        ksect = FINDGEN(nz)
        
        iisect = REBIN(       isect,         nsect, nz, /SAMPLE)												;Expand cross-section index values to 2 dimensions
        jjsect = REBIN(       jsect,         nsect, nz, /SAMPLE)
        kksect = REBIN(REFORM(ksect, 1, nz), nsect, nz, /SAMPLE)
        
        zsect0 = INTERPOLATE(z.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)					;Interpolate altitudes to cross-section path
        
        IF (KEYWORD_SET(pottemp)) THEN BEGIN
            dz     = 0.5																					;Set altitude resolution of cross-section
            nzsect = LONG((zrange[1]-zrange[0])/dz) + 1												;Compute number of levels for cross-section based on altitude range
            zsect  = zrange[0] + dz*FINDGEN(nzsect)													;Set altitude array for plotting
        ENDIF ELSE BEGIN	
            dz     = 100.0																					;Set altitude resolution of cross-section
            nzsect = LONG(1000.0*(zrange[1]-zrange[0])/dz) + 1												;Compute number of levels for cross-section based on altitude range
            zsect  = 1000.0*zrange[0] + dz*FINDGEN(nzsect)													;Set altitude array for plotting
        ENDELSE
        
        iisect = REBIN(                 isect,             nsect, nzsect, /SAMPLE)						;Compute new interpolation indices
        jjsect = REBIN(                 jsect,             nsect, nzsect, /SAMPLE)
        kksect = REBIN(REFORM(FINDGEN(nzsect), 1, nzsect), nsect, nzsect, /SAMPLE)
        
        FOR i = 0, nsect-1 DO kksect[i,*] = INTERPOL(FINDGEN(nz), zsect0[i,*], zsect)
        
         xsect = INTERPOLATE(x.values, isect, jsect)
         ysect = INTERPOLATE(y.values, isect, jsect)
        
         u_sect = INTERPOLATE(u, iisect, jjsect, kksect, MISSING = !Values.F_NaN)				;Interpolate wind speeds to contour cross-section		 
         v_sect = INTERPOLATE(v, iisect, jjsect, kksect, MISSING = !Values.F_NaN)				;Interpolate wind speeds to contour cross-section		 
         w_sect = INTERPOLATE(w, iisect, jjsect, kksect, MISSING = !Values.F_NaN)
		 
         dim = SIZE(u_sect,/DIMENSIONS)
         angle_sect = ATAN((x1-x0)/(y1-y0))*!RADEG + 180.0
         angle_wind = FLTARR(dim[0],dim[1])
         angle_new  = FLTARR(dim[0],dim[1])
         delta_angle = angle_sect - 90.0
         
	     FOR i = 0, dim[0]-1 DO BEGIN
         	FOR j = 0, dim[1]-1 DO BEGIN
             	 IF ((u_sect[i,j] GT 0.0) AND (v_sect[i,j] GT 0.0)) THEN angle_wind[i,j] = ATAN(u_sect[i,j]/v_sect[i,j])*!RADEG
                 IF ((u_sect[i,j] LT 0.0) AND (v_sect[i,j] GT 0.0)) THEN angle_wind[i,j] = ATAN(u_sect[i,j]/v_sect[i,j])*!RADEG + 360.0
                 IF ((u_sect[i,j] LT 0.0) AND (v_sect[i,j] LT 0.0)) THEN angle_wind[i,j] = ATAN(u_sect[i,j]/v_sect[i,j])*!RADEG + 180.0
                 IF ((u_sect[i,j] GT 0.0) AND (v_sect[i,j] LT 0.0)) THEN angle_wind[i,j] = ATAN(u_sect[i,j]/v_sect[i,j])*!RADEG + 180.0
         
         		angle_new[i,j] = angle_wind[i,j] - angle_sect
         		IF (angle_new[i,j] LT 0.0) THEN angle_new[i,j] = 360.0 + angle_new[i,j]
         	ENDFOR
         ENDFOR

		 spd = SQRT(u_sect^2 + v_sect^2 + w_sect^2)
		 section_wind = spd * SIN(angle_new*!DDTOR) - storm_motion
		 section_array = [[[section_array]], [[section_wind]]]

         IF (~KEYWORD_SET(pottemp)) THEN BEGIN
             theta = WRF_READ_VAR('T', date, experiment, state, DOMAIN = domain)						;Read temperature variable from WRF output
             theta.values = ((1000.0/(WRF_READ_VAR('P', date, experiment, state, $						;Compute potential temperature
             						DOMAIN = domain)).values)^(!Rair/!Cp))*(theta.values)
             cont_sect  = INTERPOLATE(theta.values,   iisect, jjsect, kksect, MISSING = !Values.F_NaN)	;Interpolate potential temperature to contour cross-section
         ENDIF
     
	      cont_sect1 = INTERPOLATE(cloud.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)	;Interpolate cloud concentration to contour cross-section
     
	      cont_array  = [[[cont_array ]], [[cont_sect ]]]
	      cont1_array = [[[cont1_array]], [[cont_sect1]]]

	ENDFOR
	section_ave_north = MEAN(section_array, DIM = 3, /NAN)
	cont_ave_north    = MEAN(cont_array,    DIM = 3, /NAN)
	cont1_ave_north   = MEAN(cont1_array,   DIM = 3, /NAN)

    ;; Do southern path
    section_array = [ ]
    cont_array    = [ ]
    cont1_array   = [ ]
    FOR ii = 0, npath1-1 DO BEGIN
    	region = [xysect2[0:1,ii],xysect4[0:1,ii]]*!DRADDEG
    	x.values      = (x.values + 360.0) MOD 360.0												;Ensure longitudes on 0-360 grid
    	region[[0,2]] = (region[[0,2]] + 360.0) MOD 360.0
    	x0=region[0]
    	x1=region[2]
    	y0=region[1]
    	y1=region[3]
    	
    	;;Find index of nearest start point
    	i0 = WHERE((ABS(x.values - x0) LT 0.01) AND (ABS(y.values - y0) LT 0.01))
    	IF (i0[0] EQ -1) THEN i0 = WHERE((ABS(x.values - x0) LT 0.1) AND (ABS(y.values - y0) LT 0.1))
    	
    	store_arr = []
    	FOR xy = 0, N_ELEMENTS(i0)-1 DO BEGIN
    		store = SQRT((x.values[i0[xy]] - x0)^2 + (y.values[i0[xy]] - y0)^2)
    		store_arr = [store_arr, store]
    	ENDFOR
    	
    	ij_start = WHERE(MIN(store_arr,/NAN) EQ store_arr)
    	ij_start = ARRAY_INDICES(x.values, i0[ij_start])
    
    	;;Find index of nearest end point
    	i1 = WHERE((ABS(x.values - x1) LT 0.01) AND (ABS(y.values - y1) LT 0.01))
    	IF (i1[0] EQ -1) THEN i1 = WHERE((ABS(x.values - x1) LT 0.1) AND (ABS(y.values - y1) LT 0.1))
    
    	store_arr = []
    	FOR xy = 0, N_ELEMENTS(i1)-1 DO BEGIN
    		store = SQRT((x.values[i1[xy]] - x0)^2 + (y.values[i1[xy]] - y0)^2)
    		store_arr = [store_arr, store]
    	ENDFOR
    	
    	ij_end = WHERE(MIN(store_arr,/NAN) EQ store_arr)
    	ij_end = ARRAY_INDICES(x.values, i1[ij_end])
    			
    	region_new = [ij_start[0],ij_start[1],ij_end[0],ij_end[1]]
    	x0 = ij_start[0]
    	y0 = ij_start[1]
    	x1 = ij_end[0]
    	y1 = ij_end[1]
    
    	;; Now with new indices, create section 
        isect = MAKEN(x0, x1, nsect)																					;Set cross-section index values
        jsect = MAKEN(y0, y1, nsect)
        ksect = FINDGEN(nz)
        
        iisect = REBIN(       isect,         nsect, nz, /SAMPLE)												;Expand cross-section index values to 2 dimensions
        jjsect = REBIN(       jsect,         nsect, nz, /SAMPLE)
        kksect = REBIN(REFORM(ksect, 1, nz), nsect, nz, /SAMPLE)
        
        zsect0 = INTERPOLATE(z.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)					;Interpolate altitudes to cross-section path
        
        IF (KEYWORD_SET(pottemp)) THEN BEGIN
            dz     = 0.5																					;Set altitude resolution of cross-section
            nzsect = LONG((zrange[1]-zrange[0])/dz) + 1												;Compute number of levels for cross-section based on altitude range
            zsect  = zrange[0] + dz*FINDGEN(nzsect)													;Set altitude array for plotting
        ENDIF ELSE BEGIN	
            dz     = 100.0																					;Set altitude resolution of cross-section
            nzsect = LONG(1000.0*(zrange[1]-zrange[0])/dz) + 1												;Compute number of levels for cross-section based on altitude range
            zsect  = 1000.0*zrange[0] + dz*FINDGEN(nzsect)													;Set altitude array for plotting
        ENDELSE
        
        iisect = REBIN(                 isect,             nsect, nzsect, /SAMPLE)						;Compute new interpolation indices
        jjsect = REBIN(                 jsect,             nsect, nzsect, /SAMPLE)
        kksect = REBIN(REFORM(FINDGEN(nzsect), 1, nzsect), nsect, nzsect, /SAMPLE)
        
        FOR i = 0, nsect-1 DO kksect[i,*] = INTERPOL(FINDGEN(nz), zsect0[i,*], zsect)
        
        xsect = INTERPOLATE(x.values, isect, jsect)
        ysect = INTERPOLATE(y.values, isect, jsect)
	 
         u_sect = INTERPOLATE(u, iisect, jjsect, kksect, MISSING = !Values.F_NaN)				;Interpolate wind speeds to contour cross-section		 
         v_sect = INTERPOLATE(v, iisect, jjsect, kksect, MISSING = !Values.F_NaN)				;Interpolate wind speeds to contour cross-section		 
         w_sect = INTERPOLATE(w, iisect, jjsect, kksect, MISSING = !Values.F_NaN)
		 
         dim = SIZE(u_sect,/DIMENSIONS)
         angle_sect = ATAN((x1-x0)/(y1-y0))*!RADEG + 180.0
         angle_wind = FLTARR(dim[0],dim[1])
         angle_new  = FLTARR(dim[0],dim[1])
         delta_angle = angle_sect - 90.0
         
	     FOR i = 0, dim[0]-1 DO BEGIN
         	FOR j = 0, dim[1]-1 DO BEGIN
             	 IF ((u_sect[i,j] GT 0.0) AND (v_sect[i,j] GT 0.0)) THEN angle_wind[i,j] = ATAN(u_sect[i,j]/v_sect[i,j])*!RADEG
                 IF ((u_sect[i,j] LT 0.0) AND (v_sect[i,j] GT 0.0)) THEN angle_wind[i,j] = ATAN(u_sect[i,j]/v_sect[i,j])*!RADEG + 360.0
                 IF ((u_sect[i,j] LT 0.0) AND (v_sect[i,j] LT 0.0)) THEN angle_wind[i,j] = ATAN(u_sect[i,j]/v_sect[i,j])*!RADEG + 180.0
                 IF ((u_sect[i,j] GT 0.0) AND (v_sect[i,j] LT 0.0)) THEN angle_wind[i,j] = ATAN(u_sect[i,j]/v_sect[i,j])*!RADEG + 180.0
         
         		angle_new[i,j] = angle_wind[i,j] - angle_sect
         		IF (angle_new[i,j] LT 0.0) THEN angle_new[i,j] = 360.0 + angle_new[i,j]
         	ENDFOR
         ENDFOR

		 spd = SQRT(u_sect^2 + v_sect^2 + w_sect^2)
		 section_wind = spd * SIN(angle_new*!DDTOR) - storm_motion
		 section_array = [[[section_array]], [[section_wind]]]

         IF (~KEYWORD_SET(pottemp)) THEN BEGIN
             theta = WRF_READ_VAR('T', date, experiment, state, DOMAIN = domain)						;Read temperature variable from WRF output
             theta.values = ((1000.0/(WRF_READ_VAR('P', date, experiment, state, $						;Compute potential temperature
             						DOMAIN = domain)).values)^(!Rair/!Cp))*(theta.values)
             cont_sect  = INTERPOLATE(theta.values,   iisect, jjsect, kksect, MISSING = !Values.F_NaN)	;Interpolate potential temperature to contour cross-section
         ENDIF
     
	      cont_sect1 = INTERPOLATE(cloud.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)	;Interpolate cloud concentration to contour cross-section
     
	      cont_array  = [[[cont_array ]], [[cont_sect ]]]
	      cont1_array = [[[cont1_array]], [[cont_sect1]]]

		ENDFOR

         section_ave_south = MEAN(section_array, DIM = 3, /NAN)
         cont_ave_south    = MEAN(cont_array,    DIM = 3, /NAN)
         cont1_ave_south   = MEAN(cont1_array,   DIM = 3, /NAN)
         	 
         section_ave = [[[section_ave_north]], [[section_ave_south]]] 
         cont_ave    = [[[cont_ave_north   ]], [[cont_ave_south   ]]]
         cont1_ave   = [[[cont1_ave_north  ]], [[cont1_ave_south  ]]]
         
         fill_sect  = MEAN(section_ave, DIM = 3, /NAN) 
         cont_sect  = MEAN(cont_ave,    DIM = 3, /NAN)
         cont_sect1 = MEAN(cont1_ave,   DIM = 3, /NAN)

		 sect_bar_min     = -50.0	
		 sect_bar_max     = 50.0
		 fill_sect_levels = (((sect_bar_max-sect_bar_min)/40.0)*FINDGEN(40) + sect_bar_min)
		 fill_sect_table  = BLUE_RED_24(N_ELEMENTS(fill_sect_levels))
		 sect_bar_ticks   = 4
		 sect_bar_title   = 'Horizontal Wind Speed (m/s)'		 
		END	
			 
	4 : BEGIN
		 storm_motion = 15.0
    	 z 	   =(WRF_READ_VAR('Z', date, experiment, state, DOMAIN = domain)).values						;Read z variable from WRF output
    	 theta = WRF_READ_VAR('T', date, experiment, state, DOMAIN = domain)								;Read temperature variable from WRF output
   	     theta.values = ((1000.0/(WRF_READ_VAR('P', date, experiment, state, $								;Compute potential temperature
    						DOMAIN = domain)).values)^(!Rair/!Cp))*(theta.values)

		 z_sect = INTERPOLATE(z, iisect, jjsect, kksect, MISSING = !Values.F_NaN)
		 theta_sect = INTERPOLATE(theta.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)
		
		 dim = SIZE(z_sect,/DIMENSIONS)        
   		 dth_dz = FLTARR(dim[0],dim[1])*!Values.F_NaN
	     FOR i = 0, dim[0]-1 DO BEGIN
         	FOR j = 0, dim[1]-1 DO BEGIN
		     	IF (j LT dim[1]-2) THEN dth_dz[i,j] = (theta_sect[i,j+1] - theta_sect[i,j-1]) / (z_sect[i,j+1] - z_sect[i,j-1])
         	ENDFOR
         ENDFOR

		 fill_sect        = dth_dz
		 sect_bar_min     = -0.02
		 sect_bar_max     =  0.02
		 fill_sect_table  = BLUE_RED_24(40)
		 fill_sect_levels = 0.001 * FINDGEN(N_ELEMENTS(fill_sect_table)) - 0.02															;Set reflectivity contour levels
		 sect_bar_title   = 'Potential Temperature Gradient (K/km)'		 
		END		 

	5 : BEGIN
         state    = ['d02_30km','d03_30km']
			         
         x_parent = (WRF_READ_VAR('Longitude'      , date, experiment, state[0], DOMAIN = domain, INDICES = region)).values					;Read variables
         y_parent = (WRF_READ_VAR('Latitude'       , date, experiment, state[0], DOMAIN = domain, INDICES = region)).values
         p_parent = (WRF_READ_VAR('P' 	           , date, experiment, state[0], DOMAIN = domain, INDICES = region)).values
         z_parent = (WRF_READ_VAR('Z'              , date, experiment, state[0], DOMAIN = domain, INDICES = region)).values
               
         x_nest   = (WRF_READ_VAR('Longitude'      , date, experiment, state[1], DOMAIN = domain, INDICES = region)).values					;Read variables
         y_nest   = (WRF_READ_VAR('Latitude'       , date, experiment, state[1], DOMAIN = domain, INDICES = region)).values
         p_nest   = (WRF_READ_VAR('P' 	           , date, experiment, state[1], DOMAIN = domain, INDICES = region)).values
         z_nest   = (WRF_READ_VAR('Z'              , date, experiment, state[1], DOMAIN = domain, INDICES = region)).values

    	 theta = WRF_READ_VAR('T', date, experiment, state[1], DOMAIN = domain)								                                ;Read temperature variable from WRF output
   	     theta.values = ((1000.0/(WRF_READ_VAR('P', date, experiment, state[1], $								                                ;Compute potential temperature
    						DOMAIN = domain)).values)^(!Rair/!Cp))*(theta.values)

		 dim = SIZE(z_nest,/DIMENSIONS)
		 press_p2n = CONGRID(p_parent[120:524,115:272,*], dim[0], dim[1], dim[2], /INTERP, CUBIC=-0.5)
		 press_diff = p_nest - press_p2n
		
		 p_sect     = INTERPOLATE(press_diff  , iisect, jjsect, kksect, MISSING = !Values.F_NaN)
		 theta_sect = INTERPOLATE(theta.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)

		 dim = SIZE(p_sect,/DIMENSIONS)        
		 PRINT, MAX(p_sect,/NAN)
		 PRINT, MIN(p_sect,/NAN)
		 fill_sect        = p_sect
		 
		 fill_sect_table  = BLUE_RED_24(16)
		 fill_sect_levels = -2.0 + 0.25*FINDGEN(N_ELEMENTS(fill_sect_table))													;Set reflectivity contour levels
		 sect_bar_title   = 'Pressure'		 
	 	 sect_bar_min     = -2.0
	 	 sect_bar_max     =  2.0

  		state = 'd03_30km'

		END		
	ELSE : MESSAGE, 'Requested cross-section type does not exist!'									;Message error
ENDCASE


;; START AVERAGING
;ij = [x0, y0, x1, y1]	 
;nsect = 2*LONG(SQRT((x1-x0)^2 + (y1-y0)^2))																;Compute number of points needed for cross-section
;
;npath1 = 5
;xysect = MAP_2POINTS((x.values)[ij[0],ij[1]],(y.values)[ij[0],ij[1]],$
;							(x.values)[ij[2],ij[3]],(y.values)[ij[2],ij[3]], NPATH = npath1)
;
;parmsect = MAP_2POINTS((x.values)[ij[0],ij[1]],(y.values)[ij[0],ij[1]],$
;							(x.values)[ij[2],ij[3]],(y.values)[ij[2],ij[3]], /RHUMB)
;							
;perp = parmsect[1]*!DDTOR																								;Find angle of cross section path
;
;xy02 = LL_ARC_DISTANCE([((x.values)[ij[0],ij[1]]),((y.values)[ij[0],ij[1]])]*!DDTOR, 20.0/6371.0,perp+(!PI*0.5))		;Find point 20km from starting point perpendicular to section
;xy03 = LL_ARC_DISTANCE([((x.values)[ij[0],ij[1]]),((y.values)[ij[0],ij[1]])]*!DDTOR, 20.0/6371.0,perp-(!PI*0.5))
;
;xy04 = LL_ARC_DISTANCE([((x.values)[ij[2],ij[3]]),((y.values)[ij[2],ij[3]])]*!DDTOR, 20.0/6371.0,perp+(!PI*0.5))
;xy05 = LL_ARC_DISTANCE([((x.values)[ij[2],ij[3]]),((y.values)[ij[2],ij[3]])]*!DDTOR, 20.0/6371.0,perp-(!PI*0.5))
;
;xysect2 = MAP_2POINTS((x.values)[ij[0],ij[1]]*!DDTOR,(y.values)[ij[0],ij[1]]*!DDTOR,$
;							xy02[0],xy02[1], NPATH = npath1, /RADIANS)
;
;xysect3 = MAP_2POINTS((x.values)[ij[0],ij[1]]*!DDTOR,(y.values)[ij[0],ij[1]]*!DDTOR,$
;							xy03[0],xy03[1], NPATH = npath1, /RADIANS)
;
;xysect4 = MAP_2POINTS((x.values)[ij[2],ij[3]]*!DDTOR,(y.values)[ij[2],ij[3]]*!DDTOR,$
;							xy04[0],xy04[1], NPATH = npath1, /RADIANS)
;
;xysect5 = MAP_2POINTS((x.values)[ij[2],ij[3]]*!DDTOR,(y.values)[ij[2],ij[3]]*!DDTOR,$
;							xy05[0],xy05[1], NPATH = npath1, /RADIANS)
;
;;; Do northern path
;section_array = [ ]
;cont_array    = [ ]
;cont1_array   = [ ]
;
;FOR ii = 0, npath1-1 DO BEGIN
;	region = [xysect3[0:1,ii],xysect5[0:1,ii]]*!DRADDEG
;	x.values      = (x.values + 360.0) MOD 360.0												;Ensure longitudes on 0-360 grid
;	region[[0,2]] = (region[[0,2]] + 360.0) MOD 360.0
;	x0=region[0]
;	x1=region[2]
;	y0=region[1]
;	y1=region[3]
;	
;	;;Find index of nearest start point
;	i0 = WHERE((ABS(x.values - x0) LT 0.01) AND (ABS(y.values - y0) LT 0.01))
;	IF (i0[0] EQ -1) THEN i1 = WHERE((ABS(x.values - x0) LT 0.1) AND (ABS(y.values - y0) LT 0.1))
;
;	store_arr = []
;	FOR xy = 0, N_ELEMENTS(i0)-1 DO BEGIN
;		store = SQRT((x.values[i0[xy]] - x0)^2 + (y.values[i0[xy]] - y0)^2)
;		store_arr = [store_arr, store]
;	ENDFOR
;	
;	ij_start = WHERE(MIN(store_arr,/NAN) EQ store_arr)
;	ij_start = ARRAY_INDICES(x.values, i0[ij_start])
;
;	;;Find index of nearest end point
;	i1 = WHERE((ABS(x.values - x1) LT 0.01) AND (ABS(y.values - y1) LT 0.01))	
;	IF (i1[0] EQ -1) THEN i1 = WHERE((ABS(x.values - x1) LT 0.1) AND (ABS(y.values - y1) LT 0.1))
;	
;	store_arr = []
;	FOR xy = 0, N_ELEMENTS(i1)-1 DO BEGIN
;		store = SQRT((x.values[i1[xy]] - x0)^2 + (y.values[i1[xy]] - y0)^2)
;		store_arr = [store_arr, store]
;	ENDFOR
;	
;	ij_end = WHERE(MIN(store_arr,/NAN) EQ store_arr)
;	ij_end = ARRAY_INDICES(x.values, i1[ij_end])
;			
;	region_new = [ij_start[0],ij_start[1],ij_end[0],ij_end[1]]
;	x0 = ij_start[0]
;	y0 = ij_start[1]
;	x1 = ij_end[0]
;	y1 = ij_end[1]
;
;	;; Now with new indices, create section
;    isect = MAKEN(x0, x1, nsect)																					;Set cross-section index values
;    jsect = MAKEN(y0, y1, nsect)
;    ksect = FINDGEN(nz)
;    
;    iisect = REBIN(       isect,         nsect, nz, /SAMPLE)												;Expand cross-section index values to 2 dimensions
;    jjsect = REBIN(       jsect,         nsect, nz, /SAMPLE)
;    kksect = REBIN(REFORM(ksect, 1, nz), nsect, nz, /SAMPLE)
;    
;    zsect0 = INTERPOLATE(z.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)					;Interpolate altitudes to cross-section path
;    
;    IF (KEYWORD_SET(pottemp)) THEN BEGIN
;        dz     = 0.5																					;Set altitude resolution of cross-section
;        nzsect = LONG((zrange[1]-zrange[0])/dz) + 1												;Compute number of levels for cross-section based on altitude range
;        zsect  = zrange[0] + dz*FINDGEN(nzsect)													;Set altitude array for plotting
;    ENDIF ELSE BEGIN	
;        dz     = 100.0																					;Set altitude resolution of cross-section
;        nzsect = LONG(1000.0*(zrange[1]-zrange[0])/dz) + 1												;Compute number of levels for cross-section based on altitude range
;        zsect  = 1000.0*zrange[0] + dz*FINDGEN(nzsect)													;Set altitude array for plotting
;    ENDELSE
;    
;    iisect = REBIN(                 isect,             nsect, nzsect, /SAMPLE)						;Compute new interpolation indices
;    jjsect = REBIN(                 jsect,             nsect, nzsect, /SAMPLE)
;    kksect = REBIN(REFORM(FINDGEN(nzsect), 1, nzsect), nsect, nzsect, /SAMPLE)
;    
;    FOR i = 0, nsect-1 DO kksect[i,*] = INTERPOL(FINDGEN(nz), zsect0[i,*], zsect)
;    
;    xsect = INTERPOLATE(x.values, isect, jsect)
;    ysect = INTERPOLATE(y.values, isect, jsect)
;	     	 
;	section = INTERPOLATE(var.values,   iisect, jjsect, kksect, MISSING = !Values.F_NaN)		;Interpolate trace gas concentration to color-fill cross-section
;	section_array = [[[section_array]], [[section]]]
;
;    IF (~KEYWORD_SET(pottemp)) THEN BEGIN
;        theta = WRF_READ_VAR('T', date, experiment, state, DOMAIN = domain)						;Read temperature variable from WRF output
;        theta.values = ((1000.0/(WRF_READ_VAR('P', date, experiment, state, $						;Compute potential temperature
;        						DOMAIN = domain)).values)^(!Rair/!Cp))*(theta.values)
;        cont_sect  = INTERPOLATE(theta.values,   iisect, jjsect, kksect, MISSING = !Values.F_NaN)	;Interpolate potential temperature to contour cross-section
;    ENDIF
;
;	 cont_sect1 = INTERPOLATE(cloud.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)	;Interpolate cloud concentration to contour cross-section
;
;	 cont_array  = [[[cont_array ]], [[cont_sect ]]]
;	 cont1_array = [[[cont1_array]], [[cont_sect1]]]
;
;ENDFOR
;section_ave_north = MEAN(section_array, DIM = 3, /NAN)
;cont_ave_north    = MEAN(cont_array,    DIM = 3, /NAN)
;cont1_ave_north   = MEAN(cont1_array,   DIM = 3, /NAN)
;
;;; Do southern path
;section_array = [ ]
;cont_array    = [ ]
;cont1_array   = [ ]
;FOR ii = 0, npath1-1 DO BEGIN
;	region = [xysect2[0:1,ii],xysect4[0:1,ii]]*!DRADDEG
;	x.values      = (x.values + 360.0) MOD 360.0												;Ensure longitudes on 0-360 grid
;	region[[0,2]] = (region[[0,2]] + 360.0) MOD 360.0
;	x0=region[0]
;	x1=region[2]
;	y0=region[1]
;	y1=region[3]
;	
;	;;Find index of nearest start point
;	i0 = WHERE((ABS(x.values - x0) LT 0.01) AND (ABS(y.values - y0) LT 0.01))
;	IF (i0[0] EQ -1) THEN i0 = WHERE((ABS(x.values - x0) LT 0.1) AND (ABS(y.values - y0) LT 0.1))
;	
;	store_arr = []
;	FOR xy = 0, N_ELEMENTS(i0)-1 DO BEGIN
;		store = SQRT((x.values[i0[xy]] - x0)^2 + (y.values[i0[xy]] - y0)^2)
;		store_arr = [store_arr, store]
;	ENDFOR
;	
;	ij_start = WHERE(MIN(store_arr,/NAN) EQ store_arr)
;	ij_start = ARRAY_INDICES(x.values, i0[ij_start])
;
;	;;Find index of nearest end point
;	i1 = WHERE((ABS(x.values - x1) LT 0.01) AND (ABS(y.values - y1) LT 0.01))
;	IF (i1[0] EQ -1) THEN i1 = WHERE((ABS(x.values - x1) LT 0.1) AND (ABS(y.values - y1) LT 0.1))
;
;	store_arr = []
;	FOR xy = 0, N_ELEMENTS(i1)-1 DO BEGIN
;		store = SQRT((x.values[i1[xy]] - x0)^2 + (y.values[i1[xy]] - y0)^2)
;		store_arr = [store_arr, store]
;	ENDFOR
;	
;	ij_end = WHERE(MIN(store_arr,/NAN) EQ store_arr)
;	ij_end = ARRAY_INDICES(x.values, i1[ij_end])
;			
;	region_new = [ij_start[0],ij_start[1],ij_end[0],ij_end[1]]
;	x0 = ij_start[0]
;	y0 = ij_start[1]
;	x1 = ij_end[0]
;	y1 = ij_end[1]
;
;	;; Now with new indices, create section 
;	;nsect = 2*LONG(SQRT((x1-x0)^2 + (y1-y0)^2))																;Compute number of points needed for cross-section
;   isect = MAKEN(x0, x1, nsect)																					;Set cross-section index values
;   jsect = MAKEN(y0, y1, nsect)
;   ksect = FINDGEN(nz)
;    
;    iisect = REBIN(       isect,         nsect, nz, /SAMPLE)												;Expand cross-section index values to 2 dimensions
;    jjsect = REBIN(       jsect,         nsect, nz, /SAMPLE)
;    kksect = REBIN(REFORM(ksect, 1, nz), nsect, nz, /SAMPLE)
;    
;    zsect0 = INTERPOLATE(z.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)					;Interpolate altitudes to cross-section path
;    
;    IF (KEYWORD_SET(pottemp)) THEN BEGIN
;        dz     = 0.5																					;Set altitude resolution of cross-section
;        nzsect = LONG((zrange[1]-zrange[0])/dz) + 1												;Compute number of levels for cross-section based on altitude range
;        zsect  = zrange[0] + dz*FINDGEN(nzsect)													;Set altitude array for plotting
;    ENDIF ELSE BEGIN	
;        dz     = 100.0																					;Set altitude resolution of cross-section
;        nzsect = LONG(1000.0*(zrange[1]-zrange[0])/dz) + 1												;Compute number of levels for cross-section based on altitude range
;        zsect  = 1000.0*zrange[0] + dz*FINDGEN(nzsect)													;Set altitude array for plotting
;    ENDELSE
;    
;    iisect = REBIN(                 isect,             nsect, nzsect, /SAMPLE)						;Compute new interpolation indices
;    jjsect = REBIN(                 jsect,             nsect, nzsect, /SAMPLE)
;    kksect = REBIN(REFORM(FINDGEN(nzsect), 1, nzsect), nsect, nzsect, /SAMPLE)
;    
;    FOR i = 0, nsect-1 DO kksect[i,*] = INTERPOL(FINDGEN(nz), zsect0[i,*], zsect)
;    
;    xsect = INTERPOLATE(x.values, isect, jsect)
;    ysect = INTERPOLATE(y.values, isect, jsect)
;	 
;	 section = INTERPOLATE(var.values,   iisect, jjsect, kksect, MISSING = !Values.F_NaN)	;Interpolate trace gas concentration to color-fill cross-section
;	 section_array = [[[section_array]], [[section]]]
;    
;    IF (~KEYWORD_SET(pottemp)) THEN BEGIN
;        theta = WRF_READ_VAR('T', date, experiment, state, DOMAIN = domain)						;Read temperature variable from WRF output
;        theta.values = ((1000.0/(WRF_READ_VAR('P', date, experiment, state, $						;Compute potential temperature
;        						DOMAIN = domain)).values)^(!Rair/!Cp))*(theta.values)
;        cont_sect  = INTERPOLATE(theta.values,   iisect, jjsect, kksect, MISSING = !Values.F_NaN)	;Interpolate potential temperature to contour cross-section
;    ENDIF
;
;	 cont_sect1 = INTERPOLATE(cloud.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)	;Interpolate cloud concentration to contour cross-section
;
;	 cont_array  = [[[cont_array ]], [[cont_sect ]]]
;	 cont1_array = [[[cont1_array]], [[cont_sect1]]]
;
;ENDFOR
;
;section_ave_south = MEAN(section_array, DIM = 3, /NAN)
;cont_ave_south    = MEAN(cont_array,    DIM = 3, /NAN)
;cont1_ave_south   = MEAN(cont1_array,   DIM = 3, /NAN)
;	 
;section_ave = [[[section_ave_north]], [[section_ave_south]]] 
;cont_ave    = [[[cont_ave_north   ]], [[cont_ave_south   ]]]
;cont1_ave   = [[[cont1_ave_north  ]], [[cont1_ave_south  ]]]
;
;fill_sect  = MEAN(section_ave, DIM = 3, /NAN) 
;cont_sect  = MEAN(cont_ave,    DIM = 3, /NAN)
;cont_sect1 = MEAN(cont1_ave,   DIM = 3, /NAN)
;
;IF (chemical EQ 'O3_tracer') THEN fill_sect = fill_sect*1.0E3


;; END AVERAGING
IF (~KEYWORD_SET(pottemp)) THEN BEGIN
    IF (~KEYWORD_SET(ave_section)) THEN BEGIN
    	var = WRF_READ_VAR('T', date, experiment, state, DOMAIN = domain)						;Read temperature variable from WRF output
    	var.values = ((1000.0/(WRF_READ_VAR('P', date, experiment, state, $						;Compute potential temperature
    						DOMAIN = domain)).values)^(!Rair/!Cp))*(var.values)
    	cont_sect  = INTERPOLATE(var.values,   iisect, jjsect, kksect, MISSING = !Values.F_NaN)	;Interpolate potential temperature to contour cross-section
    ENDIF
    cont_sect_color  = COLOR_24('black')																;Set contour cross-section specs
    cont_sect_levels = 200.0 + 4.0*FINDGEN(250)
    cont_sect_thick  = 1
ENDIF

IF (~KEYWORD_SET(ave_section)) THEN cont_sect1 = INTERPOLATE(cloud.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)	;Interpolate cloud concentration to contour cross-section
cont_sect1_color  = COLOR_24('gray50')																;Set contour cross-section specs
cont_sect1_levels = 1.0E-5
cont_sect1_thick  = 3

u = (WRF_READ_VAR('u', date, experiment, state, DOMAIN = domain)).values						;Read temperature variable from WRF output
v = (WRF_READ_VAR('v', date, experiment, state, DOMAIN = domain)).values	
w = (WRF_READ_VAR('w', date, experiment, state, DOMAIN = domain)).values
u_sect = INTERPOLATE(u, iisect, jjsect, kksect, MISSING = !Values.F_NaN)	;Interpolate wind speeds to contour cross-section		 
v_sect = INTERPOLATE(v, iisect, jjsect, kksect, MISSING = !Values.F_NaN)	;Interpolate wind speeds to contour cross-section		 
w_sect = INTERPOLATE(w, iisect, jjsect, kksect, MISSING = !Values.F_NaN)

IF (~KEYWORD_SET(pottemp)) THEN BEGIN
	time0 = SYSTIME(/SECONDS)
    trop.values = MEDIAN(trop.values,30)
    time_trop = SYSTIME(/SECONDS) - time0
	trop_sect = INTERPOLATE(trop.values, isect , jsect)
ENDIF ELSE BEGIN
    theta_trop = FLTARR(dim[0],dim[1])
    FOR ii = 0, dim[0]-1 DO BEGIN
    	FOR jj = 0, dim[1]-1 DO BEGIN
    		aaa = [INDEX_OF_NEAREST_CRH(z.values[ii,jj,*],xyz_trop[ii,jj,*])]
    		theta_trop[ii,jj] = theta[ii,jj,aaa]
    	ENDFOR
    ENDFOR
	theta_trop = MEDIAN(theta_trop, 30)
	theta_sect = INTERPOLATE(theta_trop, isect, jsect)
ENDELSE

outdir  = !WRF_DIRECTORY + experiment + '/paper/plots/wrapping_section5_w_ave/' 
epsfile = outdir + state + '_' + date_string + '.eps'											;EPS filename
pdffile = outdir + state + '_' + date_string + '.pdf'											;PDF filename
pngfile = outdir + state + '_' + date_string + '.png'											;PNG filename

FILE_MKDIR, outdir																								;Create output directory, if necessary

IF KEYWORD_SET(eps) THEN BEGIN	
	PS_ON, FILENAME = epsfile, PAGE_SIZE = [10.0, 5.0], MARGIN = 0.0, /INCHES					;Switch to Postscript device
	DEVICE, /ENCAPSULATED
	!P.FONT     = 0																								;Hardware fonts
	!P.CHARSIZE = 1.25
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS																							;Load basic color definitions
	thick_scale = 3
ENDIF ELSE BEGIN
	SET_PLOT, 'X'
	WINDOW, XSIZE = 1200, YSIZE = 600																		;Open graphics window
	!P.COLOR      = COLOR_24('black')																		;Foreground color
	!P.BACKGROUND = COLOR_24('white')																		;Background color
	!P.CHARSIZE   = 2.0		
	!P.FONT       = -1																							;Use Hershey fonts
	thick_scale   = 1
ENDELSE

!P.MULTI = [0, 2, 1]																								;Set multiple plots

map_pos      = [0.025, 0.22, 0.375, 0.92]																	;Set map position
map_bar_pos  = [0.055, 0.11, 0.345, 0.13]																	;Set map color bar position
xsn_pos      = [0.450, 0.22, 0.975, 0.92]																	;Set cross-section position
xsn_bar_pos  = [0.500, 0.11, 0.925, 0.13]																	;Set cross-section color bar position

yll = y.values[   0,   0]																						;Set domain boundary points
yul = y.values[   0,ny-1]
yur = y.values[nx-1,ny-1]
ylr = y.values[nx-1,   0]
xll = x.values[   0,   0]
xul = x.values[   0,ny-1]
xur = x.values[nx-1,ny-1]
xlr = x.values[nx-1,   0]

xc = INTERPOLATE(x.values, 0.5*(nx-1), 0.5*(ny-1))														;Get central grid point
yc = INTERPOLATE(y.values, 0.5*(nx-1), 0.5*(ny-1))

IF (xc EQ 0.0) THEN BEGIN
	x.values   = REBIN(       FINDGEN(nx),         nx, ny)											;Set coordinates to grid indices
	y.values   = REBIN(REFORM(FINDGEN(ny), 1, ny), nx, ny)
	map_pos[0] = 0.075																							;Adjust map position
	map_pos[1] = 0.300
ENDIF

IF (xc NE 0.0) THEN $
	MAP_SET, yc, xc, 0, CONIC = 1, $																			;Draw map
		LIMIT     = [yll,xll,yul,xul,yur,xur,ylr,xlr], $
		ISOTROPIC = 1, $
		TITLE     = date_string, $
		POSITION  = map_pos

CONTOUR, map_plot, x.values, y.values, $																	;Contour values
	OVERPLOT  = (xc NE 0.0), $
	FILL      = 1, $
	LEVELS    = map_levels, $
	C_COLOR   = map_table, $
	TITLE     = date_string, $
	POSITION  = map_pos

IF (map_type EQ 3) THEN BEGIN
 	CONTOUR, map_plot1, x.values, y.values, $														;Overplot contour cross-section
       OVERPLOT  = 1, $
       FOLLOW    = 1, $
       LEVELS    = map_levels1, $
       C_COLOR   = map_table1, $
       C_THICK   = cont_sect1_thick*thick_scale

	table   = COLOR_24('red')												;Set reflectivity color table
	rlevels = 30.0																					;Set reflectivity contour levels

    CONTOUR, MAX(R.values, DIM=3), x.values, y.values, $															;Contour reflectivity values
    	OVERPLOT  = 1, $
    	FILL      = 1, $
    	LEVELS    = rlevels, $
    	C_COLOR   = table, $
        POSITION  = map_pos
ENDIF

IF (xc NE 0.0) THEN BEGIN
	MAP_CONTINENTS, /CONT, /USA, THICK = thick_scale													;Draw continental & state outlines
	OPLOT, xsect, ysect, THICK = 2*thick_scale															;Superimpose cross-section path

	XYOUTS, xsect[0], ysect[0] + 0.025*[ysect[nsect-1] - ysect[0]], 'A', $						;Label endpoints of cross-section on map
		ALIGN = 0.5, CHARSIZE = 1.5*!P.CHARSIZE, CHARTHICK = 2.0*thick_scale

	XYOUTS, xsect[nsect-1], ysect[nsect-1] + 0.025*[ysect[nsect-1] - ysect[0]], 'B', $
		ALIGN = 0.5, CHARSIZE = 1.5*!P.CHARSIZE, CHARTHICK = 2.0*thick_scale
ENDIF ELSE BEGIN
	OPLOT, isect, jsect, THICK = 2*thick_scale
	
	XYOUTS, isect[0], jsect[0] + 0.025*[jsect[nsect-1] - jsect[0]], 'A', $						;Label endpoints of cross-section on map
		ALIGN = 0.5, CHARSIZE = 1.5*!P.CHARSIZE, CHARTHICK = 2.0*thick_scale

	XYOUTS, isect[nsect-1], jsect[nsect-1] + 0.025*[jsect[nsect-1] - jsect[0]], 'B', $
		ALIGN = 0.5, CHARSIZE = 1.5*!P.CHARSIZE, CHARTHICK = 2.0*thick_scale
ENDELSE

COLOR_BAR_24_KPB, map_table[1:*], OVER = map_table[-1], UNDER = map_table[0], $						;Draw map color bar
	TICKS = map_bar_ticks, $
	RANGE = [map_bar_min, map_bar_max], $
	TITLE = map_bar_title, $
	POSIT = map_bar_pos

!P.POSITION = 0

storm_motion = 15.0
x_freq = 10.0
n_nsect = nsect/x_freq

IF (~KEYWORD_SET(pottemp)) THEN BEGIN
    CONTOUR, fill_sect, FINDGEN(nsect), 0.001*zsect, $														;Draw cross-section with color-filled variable
    	CELL_FILL = 1, $
    	C_COLOR   = fill_sect_table, $
    	LEVELS    = fill_sect_levels, $
    	XRANGE    = [0, nsect -1], $
    	XTICKS    = 1, $
    	XTICKN    = ['A', 'B'], $
    	XSTYLE    = 1, $
    	YRANGE    = zrange, $
 ;   	YLOG	  = 1, $
    	YSTYLE    = 1, $
    	YTITLE    = 'Altitude (km)', $
    	TITLE     = 'Vertical Section', $
    	POSITION  = xsn_pos

ENDIF ELSE BEGIN
    CONTOUR, fill_sect, FINDGEN(nsect), zsect, $														;Draw cross-section with color-filled variable
    	CELL_FILL = 1, $
    	C_COLOR   = fill_sect_table, $
    	LEVELS    = fill_sect_levels, $
    	XRANGE    = [0, nsect -1], $
    	XTICKS    = 1, $
    	XTICKN    = ['A', 'B'], $
    	XSTYLE    = 1, $
    	YRANGE    = zrange, $
    	YSTYLE    = 1, $
    	YTITLE    = 'Potential Temperature (K)', $
    	TITLE     = 'Vertical Section', $
    	POSITION  = xsn_pos
ENDELSE

;dim = SIZE(u_sect,/DIMENSIONS)
;angle_sect = ATAN((x1-x0)/(y1-y0))*!RADEG + 180.0
;angle_wind = FLTARR(dim[0],dim[1])
;angle_new  = FLTARR(dim[0],dim[1])
;delta_angle = angle_sect - 90.0
;
;FOR i = 0, dim[0]-1 DO BEGIN
;	FOR j = 0, dim[1]-1 DO BEGIN
;    	IF ((u_sect[i,j] GT 0.0) AND (v_sect[i,j] GT 0.0)) THEN angle_wind[i,j] = ATAN(u_sect[i,j]/v_sect[i,j])*!RADEG
;        IF ((u_sect[i,j] LT 0.0) AND (v_sect[i,j] GT 0.0)) THEN angle_wind[i,j] = ATAN(u_sect[i,j]/v_sect[i,j])*!RADEG + 360.0
;        IF ((u_sect[i,j] LT 0.0) AND (v_sect[i,j] LT 0.0)) THEN angle_wind[i,j] = ATAN(u_sect[i,j]/v_sect[i,j])*!RADEG + 180.0
;        IF ((u_sect[i,j] GT 0.0) AND (v_sect[i,j] LT 0.0)) THEN angle_wind[i,j] = ATAN(u_sect[i,j]/v_sect[i,j])*!RADEG + 180.0
;
;		angle_new[i,j] = angle_wind[i,j] - angle_sect
;		IF (angle_new[i,j] LT 0.0) THEN angle_new[i,j] = 360.0 + angle_new[i,j]
;	ENDFOR
;ENDFOR
;
;spd = SQRT(u_sect^2 + v_sect^2 + w_sect^2)
;section_wind = spd * SIN(angle_new*!DDTOR) - storm_motion
;
;h_sample = FLTARR(4068,201)*!Values.F_NaN
;h_sample[0:4067:x_freq, 0:200:10]= section_wind [0:4067:x_freq, 0:200:10]
;
;w_sample = FLTARR(4068,201)*!Values.F_NaN
;w_sample[0:4067:x_freq, 0:200:10]= w_sect[0:4067:x_freq, 0:200:10]
;
;IF (section_type EQ 2) THEN VELOVECT, h_sample, w_sample, FINDGEN(nsect), 0.001*zsect, /OVERPLOT, LENGTH= 350


;;Commented out theta lines for manuscript

IF (~KEYWORD_SET(pottemp)) THEN BEGIN
    CONTOUR, cont_sect, FINDGEN(nsect), 0.001*zsect, $														;Overplot contour cross-section
    	OVERPLOT = 1, $
    	FOLLOW	 = 1, $
    	LEVELS   = cont_sect_levels, $
    	C_COLOR  = cont_sect_color, $
    	C_THICK  = cont_sect_thick*thick_scale

	IF (~(section_type EQ 0) OR (section_type EQ 1)) THEN BEGIN
        CONTOUR, cont_sect1, FINDGEN(nsect), 0.001*zsect, $														;Overplot contour cross-section
        	OVERPLOT = 1, $
        	LEVELS   = cont_sect1_levels, $
        	C_COLOR  = cont_sect1_color, $
        	C_THICK  = cont_sect1_thick*thick_scale
        	 		
 		cont_sect  = ozone_sect																				;Interpolate ozone to contour cross-section
 		cont_sect_color  = COLOR_24('black')																;Set contour cross-section specs
 		cont_sect_levels = 25.0*FINDGEN(40)
 		cont_sect_thick  = 1

	    CONTOUR, cont_sect, FINDGEN(nsect),  0.001*zsect, $															;Overplot contour cross-section
        	OVERPLOT = 1, $
        	LEVELS   = [100.0, 125.0, 150.0], $
        	C_COLOR  = cont_sect_color, $
        	C_THICK  = cont_sect_thick*thick_scale*3.0
	ENDIF
ENDIF ELSE BEGIN
	IF ((section_type EQ 2) OR (section_type EQ 3)) THEN BEGIN
        CONTOUR, cont_sect1, FINDGEN(nsect), zsect, $														;Overplot contour cross-section
        	OVERPLOT = 1, $
        	LEVELS   = cont_sect1_levels, $
        	C_COLOR  = cont_sect1_color, $
        	C_THICK  = cont_sect1_thick*thick_scale
	ENDIF
ENDELSE

USERSYM_CIRCLE, /FILL																						;Load circle plotting symbol

IF (~KEYWORD_SET(pottemp)) THEN BEGIN
    OPLOT, 10*FINDGEN(nsect/10), 0.001*trop_sect[0:*:10], $													;Draw tropopause altitudes as circles
    	PSYM = 8, SYMSIZE = 2.5 - KEYWORD_SET(eps)
ENDIF ELSE BEGIN
    OPLOT, 10*FINDGEN(nsect/10), theta_sect[0:*:10], $														;Draw tropopause altitudes as circles
    	PSYM = 8, SYMSIZE = 2.5 - KEYWORD_SET(eps)
ENDELSE


;OPLOT, 10*FINDGEN(nsect/10), 0.001*zsect0[*,43], $															;Draw tropopause altitudes as circles
;	PSYM = 2, SYMSIZE = 2.5 - KEYWORD_SET(eps)


COLOR_BAR_24_KPB, fill_sect_table, UNDER = fill_sect_table[0], OVER = fill_sect_table[-1], $				;Draw cross-section color bar
	TICKS  = sect_bar_ticks, $
	RANGE  = [sect_bar_min, sect_bar_max], $
	TITLE  = sect_bar_title, $
	POSIT  = xsn_bar_pos, $
	XTICKN = sect_tick_name

!P.MULTI = 0																										;Reset multiple plots
!P.POSITION = 0

IF KEYWORD_SET(eps) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file

END
