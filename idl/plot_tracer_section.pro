PRO PLOT_TRACER_SECTION, experiment, state, x0, y0, x1, y1, date, $
	DOMAIN       = domain, $
	MAP_TYPE     = map_type, $
	SECTION_TYPE = section_type, $
	CHEMICAL     = chemical, $
	COORDINATES  = coordinates, $
	ZRANGE       = zrange, $
	KEY			 = key, $
	EPS          = eps, $
	PNG          = png


;+
; Name:
;		PLOT_TRACER_SECTION
; Purpose:
;		This is a procedure to plot a vertical cross-section of WRF output.
; Calling sequence:
;		PLOT_WTRACER_SECTION, date, experiment, state
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
;		Cameron R. Homeyer  2012-11-28.
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
z     = WRF_READ_VAR('Z',               date, experiment, state, DOMAIN = domain)
cloud = WRF_READ_VAR('CLOUD_MIX_TOTAL', date, experiment, state, DOMAIN = domain)
trop  = WRF_READ_VAR('Z_trop',          date, experiment, state, DOMAIN = domain)

IF (SIZE(cloud, /TNAME) NE 'STRUCT') THEN $
	cloud = WRF_READ_VAR('CLOUD', date, experiment, state, DOMAIN = domain)						;If cloud mixing ratio not available, read number concentration

IF KEYWORD_SET(coordinates) THEN BEGIN
	x.values      = (x.values + 360.0) MOD 360.0															;Ensure longitudes on 0-360 grid
	region[[0,2]] = (region[[0,2]] + 360.0) MOD 360.0

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

dim   = SIZE(Z.values, /DIMENSIONS)																			;Get grid lengths
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

zsect0 = INTERPOLATE(z.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)				;Interpolate altitudes to cross-section path

dz     = 100.0																										;Set altitude resolution of cross-section
nzsect = LONG(1000.0*(zrange[1]-zrange[0])/dz) + 1														;Compute number of levels for cross-section based on altitude range
zsect  = 1000.0*zrange[0] + dz*FINDGEN(nzsect)															;Set altitude array for plotting
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
		 map_plot      = 0.001*(MAX((cloud.values GE 1.0E-9)*Z.values, DIM = 3, /NAN))		;Set map variable to cloud top altitude
		 map_bar_title = 'Cloud Top Altitude (km)'														;Set color bar title
		 map_bar_min   = 5.0																						;Set echo top minimum
		 map_bar_max   = 20.0																					;Set echo top maximum
		 map_bar_ticks = 3																						;Set number of color bar ticks
		 map_table     = [COLOR_24(230, 230, 230),VISUALIZE_88D_COLOR(1)]							;Set color table
		 map_levels    = [0.0, 5.0 + FINDGEN(N_ELEMENTS(map_table))]								;Set contour levels
		 END
	2 : BEGIN
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
		 cont_sect_levels = 1.0E-9
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
		 cont_sect_levels = 200.0 + 2.0*FINDGEN(200)
		 cont_sect_thick  = 1
		 END
	2 : BEGIN
		
		IF(chemical EQ 'tracers') THEN BEGIN
			 bl    = (WRF_READ_VAR('BL_tracer'	, date, experiment, state, DOMAIN = domain)).values * 1.0E3					;Read trace gas from WRF output
			 tr    = (WRF_READ_VAR('TR_tracer'	, date, experiment, state, DOMAIN = domain)).values * 1.0E3
			 utls  = (WRF_READ_VAR('UTLS_tracer', date, experiment, state, DOMAIN = domain)).values * 1.0E3
			 st    = (WRF_READ_VAR('ST_tracer'  , date, experiment, state, DOMAIN = domain)).values * 1.0E3
			 o3    = (WRF_READ_VAR('O3'			, date, experiment, state, DOMAIN = domain)).values * 1.0E3
	
			 bl_rel   =  ((bl-o3  )/o3)*100.0
			 tr_rel   =  ((tr-o3  )/o3)*100.0
			 utls_rel =  ((utls-o3)/o3)*100.0
			 st_rel   =  ((st-o3  )/o3)*100.0

			 bl_fill_sect   = INTERPOLATE(bl_rel  , iisect, jjsect, kksect, MISSING = !Values.F_NaN)			;Interpolate trace gas concentration to color-fill cross-section
			 tr_fill_sect   = INTERPOLATE(tr_rel  , iisect, jjsect, kksect, MISSING = !Values.F_NaN)			;Interpolate trace gas concentration to color-fill cross-section
			 utls_fill_sect = INTERPOLATE(utls_rel, iisect, jjsect, kksect, MISSING = !Values.F_NaN)			;Interpolate trace gas concentration to color-fill cross-section
			 st_fill_sect   = INTERPOLATE(st_rel  , iisect, jjsect, kksect, MISSING = !Values.F_NaN)			;Interpolate trace gas concentration to color-fill cross-section

	    ENDIF 
		IF (chemical NE 'tracers') THEN BEGIN
		 	var = WRF_READ_VAR(chemical, date, experiment, state, DOMAIN = domain)					;Read trace gas from WRF output
			 fill_sect = INTERPOLATE(var.values,   iisect, jjsect, kksect, MISSING = !Values.F_NaN)	;Interpolate trace gas concentration to color-fill cross-section
		ENDIF
		 cont_sect = INTERPOLATE(cloud.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)	;Interpolate cloud concentration to contour cross-section

		 CASE chemical OF
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
					 sect_bar_max     = 100.0
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
					 sect_bar_max     = 400.0
					 fill_sect_levels = ((sect_bar_max-sect_bar_min)/20.0)*FINDGEN(20)
					 sect_bar_ticks   = 4
					 sect_bar_title   = 'UTLS_tracer'
					 END

			'Updraft_tracer' : BEGIN
		 			 fill_sect        = (100.0)*fill_sect													;Convert to ppbv
					 fill_sect_table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])			;Set color-fill cross-section specs
					 sect_bar_min     = 0.0	
					 sect_bar_max     = 100.0
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

			'tracers'	: BEGIN
        			 bl    = (WRF_READ_VAR('BL_tracer'	, date, experiment, state, DOMAIN = domain)).values * 1.0E3					;Read trace gas from WRF output
        			 tr    = (WRF_READ_VAR('TR_tracer'	, date, experiment, state, DOMAIN = domain)).values * 1.0E3
        			 utls  = (WRF_READ_VAR('UTLS_tracer', date, experiment, state, DOMAIN = domain)).values * 1.0E3
        			 st    = (WRF_READ_VAR('ST_tracer'  , date, experiment, state, DOMAIN = domain)).values * 1.0E3
        			 o3    = (WRF_READ_VAR('O3'			, date, experiment, state, DOMAIN = domain)).values * 1.0E3
        	
        			 bl_rel   =  ((bl-o3  )/o3)*100.0
        			 tr_rel   =  ((tr-o3  )/o3)*100.0
        			 utls_rel =  ((utls-o3)/o3)*100.0
        			 st_rel   =  ((st-o3  )/o3)*100.0
        
        			 bl_fill_sect   = INTERPOLATE(bl_rel  , iisect, jjsect, kksect, MISSING = !Values.F_NaN)			;Interpolate trace gas concentration to color-fill cross-section
        			 tr_fill_sect   = INTERPOLATE(tr_rel  , iisect, jjsect, kksect, MISSING = !Values.F_NaN)			;Interpolate trace gas concentration to color-fill cross-section
        			 utls_fill_sect = INTERPOLATE(utls_rel, iisect, jjsect, kksect, MISSING = !Values.F_NaN)			;Interpolate trace gas concentration to color-fill cross-section
        			 st_fill_sect   = INTERPOLATE(st_rel  , iisect, jjsect, kksect, MISSING = !Values.F_NaN)			;Interpolate trace gas concentration to color-fill cross-section
					 
					 bl_fill_sect_table    = WHITE_BLUE_24   (20,0.25,0.75)			;Set color-fill cross-section specs
					 tr_fill_sect_table    = WHITE_RED_24    (20,0.25,0.75)			;Set color-fill cross-section specs
					 utls_fill_sect_table  = WHITE_MAGENTA_24(20,0.25,0.75)			;Set color-fill cross-section specs
					 st_fill_sect_table    = GRAYSCALE_24    (20,0.25,0.75)			;Set color-fill cross-section specs

					 sect_bar_min     = 25.0	
					 sect_bar_max     = 75.0
					 fill_sect_levels = ((sect_bar_max-sect_bar_min)/20.0)*FINDGEN(20)
					 sect_bar_ticks   = 4
					 sect_bar_title   = 'relative contributions of tracers'

					 END

			 ELSE : MESSAGE, 'Requested chemical does not exist!'										;Message error
		 ENDCASE			 
		 
		 cont_sect_color  = COLOR_24('gray50')																;Set contour cross-section specs
		 cont_sect_levels = 1.0E-5
		 cont_sect_thick  = 3
		 END
	ELSE : MESSAGE, 'Requested cross-section type does not exist!'									;Message error
ENDCASE

trop_sect = INTERPOLATE(trop.values, isect, jsect)

outdir  = !WRF_DIRECTORY + experiment + '/paper/plots/' 
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

CONTOUR, bl_fill_sect, FINDGEN(nsect), 0.001*zsect, $														;Draw cross-section with color-filled variable
	CELL_FILL = 1, $
	C_COLOR   = bl_fill_sect_table, $
	LEVELS    = fill_sect_levels, $
	XRANGE    = [0, nsect -1], $
	XTICKS    = 1, $
	XTICKN    = ['A', 'B'], $
	XSTYLE    = 1, $
	YRANGE    = zrange, $
	YSTYLE    = 1, $
	YTITLE    = 'Altitude (km)', $
	TITLE     = 'Vertical Section', $
	POSITION  = xsn_pos

CONTOUR, tr_fill_sect, FINDGEN(nsect), 0.001*zsect, $														;Draw cross-section with color-filled variable
	OVERPLOT  = 1, $
	CELL_FILL = 1, $
	C_COLOR   = tr_fill_sect_table

CONTOUR, bl_fill_sect, FINDGEN(nsect), 0.001*zsect, $														;Draw cross-section with color-filled variable
	OVERPLOT  = 1, $
	CELL_FILL = 1, $
	C_COLOR   = bl_fill_sect_table

CONTOUR, utls_fill_sect, FINDGEN(nsect), 0.001*zsect, $														;Draw cross-section with color-filled variable
	OVERPLOT  = 1, $
	CELL_FILL = 1, $
	C_COLOR   = utls_fill_sect_table

CONTOUR, st_fill_sect, FINDGEN(nsect), 0.001*zsect, $														;Draw cross-section with color-filled variable
	OVERPLOT  = 1, $
	CELL_FILL = 1, $
	C_COLOR   = st_fill_sect_table


;;Commented out theta lines for manuscript
;CONTOUR, cont_sect, FINDGEN(nsect), 0.001*zsect, $														;Overplot contour cross-section
;	OVERPLOT = 1, $
;	LEVELS   = cont_sect_levels, $
;	C_COLOR  = cont_sect_color, $
;	C_THICK  = cont_sect_thick*thick_scale

USERSYM_CIRCLE, /FILL																							;Load circle plotting symbol
OPLOT, 10*FINDGEN(nsect/10), 0.001*trop_sect[0:*:10], $												;Draw tropopause altitudes as circles
	PSYM = 8, SYMSIZE = 2.5 - KEYWORD_SET(eps)

COLOR_BAR_24_KPB, st_fill_sect_table, UNDER = st_fill_sect_table[0], OVER = st_fill_sect_table[-1], $	;Draw cross-section color bar
	TICKS  = sect_bar_ticks, $
	RANGE  = [sect_bar_max, sect_bar_min], $
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
