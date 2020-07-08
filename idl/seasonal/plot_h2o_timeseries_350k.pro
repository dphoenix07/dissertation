PRO PLOT_H2O_TIMESERIES_350K, event, scheme, start_date, end_date, $
	DOMAIN   = domain, $
	REGION   = region, $
	TROP_REL = trop_rel, $
	REFL	 = refl, $
	PNG	     = png, $
	Z_BUFF   = z_buff, $
	EPS   	 = eps


;+
; Name:
;		PLOT_H2O_TIMESERIES_PLOTS
; Purpose:
;		Computes fraction of domain where updraft_tracer = 1
; Calling sequence:
;		PLOT_H2O_TIMESERIES_PLOTS, run, scheme, start_date, end_date
; Input:
;		event      : Model simulation name. (e.g., '20120519')
;		scheme 	   : Model initial state. Typically 'morrison' or 'nssl'.
;		start_date : String variable of the start date requested (e.g., '20120519T2300Z')
;		end_date   : String variable of the end date requested (e.g., '20120520T0300Z')
; Output:
;		Timeseries of fraction of convection in domain and chemical signature 
; Keywords:
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		VERBOSE : If set, message writing process.
; Author and history:
;		Daniel B. Phoenix	    2017-07-18. 
;								2017-09-09. Same as CONVECTIVE_FRACTION but with map plots
;											and track the location on the timeseries
;								2018-03-15. Filtered locations where tropopause exceeded
;											15 km. 
;								2018-06-11. Filtered locations where 350-380K qualified
;											layer were below the lapse rate tropopause.
;											Added in convective intensity (depth of 
;											overshoot), mean tropopause temperature, and
;											fractional amount of stratospheric air.
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1											;Set default domain

outdir  = !WRF_DIRECTORY + event + '/paper/plots/timeseries_new_intensity/'
FILE_MKDIR, outdir

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(event, scheme, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(event, scheme, start_date, end_date, /DATE)	

;; calculate first date 
ave_h2o_bg	   = (WRF_READ_VAR('Stratospheric_H2O'     , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region, H2O=1)).values
conv_fraction  = (WRF_READ_VAR('Convective_Fraction'   , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region, H2O=1)).values					
intensity1     = (WRF_READ_VAR('Convective_Intensity'  , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region, H2O=1)).values					
strat_fraction = (WRF_READ_VAR('Stratospheric_Fraction', date_arr[0], event, scheme, DOMAIN=domain, INDICES=region, H2O=1)).values
trop_temp1	   = (WRF_READ_VAR('Tropopause_Temperature', date_arr[0], event, scheme, DOMAIN=domain, INDICES=region, H2O=1)).values							;Read temperature variable from WRF output
cloud 		   =  WRF_READ_VAR('Cloud_Mixing_Ratio'    , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region, H2O=1)
y              = (WRF_READ_VAR('Latitude'              , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region, H2O=1)).values
x              = (WRF_READ_VAR('Longitude'             , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region, H2O=1)).values
z              = (WRF_READ_VAR('Z'		               , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region, H2O=1)).values
xyz_trop       = (WRF_READ_VAR('Tropopause_Height'	   , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region, H2O=1)).values	
cloud_slice    = (WRF_READ_VAR('Cloud_slice'           , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region, H2O=1)).values
h2o_slice	   = (WRF_READ_VAR('H2O_slice'             , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region, H2O=1)).values							;Read temperature variable from WRF output

dim = SIZE(x, /DIMENSIONS)

offset = 0
y0 = y[          offset ,          offset ]														;Set domain boundary points
y1 = y[          offset ,dim[1]-(1+offset)]
y2 = y[dim[0]-(1+offset),dim[1]-(1+offset)]
y3 = y[dim[0]-(1+offset),          offset ]
x0 = x[          offset ,          offset ]
x1 = x[          offset ,dim[1]-(1+offset)]
x2 = x[dim[0]-(1+offset),dim[1]-(1+offset)]
x3 = x[dim[0]-(1+offset),          offset ]

xc = INTERPOLATE(x, 0.5*(dim[0]-1), 0.5*(dim[1]-1))											;Get central grid point
yc = INTERPOLATE(y, 0.5*(dim[0]-1), 0.5*(dim[1]-1))

table   = [COLOR_24(200, 200, 200),VISUALIZE_88D_COLOR()]												;Set reflectivity color table
rlevels = [-100.0, 5.0*FINDGEN(N_ELEMENTS(table))]													;Set reflectivity contour levels
wfactor = 400.0/(dim[0]) + 400.0/(dim[1])																	;Set map factor

dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string
date_string = MAKE_ISO_DATE_STRING(date_arr[0], PREC='MINUTE', /COMPACT, /UTC)							;Create date string

map_pos = [0.05, 0.15, 0.95, 0.95]																			;Set map position
bar_pos = [0.25, 0.10, 0.75, 0.12]																			;Set color bar position

map_pos1 = [0.02, 0.10, 0.32, 0.50]																			;Set map position
bar_pos1 = [0.04, 0.06, 0.30, 0.08]																			;Set color bar position

map_pos2 = [0.35, 0.10, 0.65, 0.50]																			;Set map position
bar_pos2 = [0.37, 0.06, 0.63, 0.08]																			;Set color bar position

map_pos3 = [0.68, 0.10, 0.98, 0.50]																			;Set map position
bar_pos3 = [0.70, 0.06, 0.96, 0.08]																			;Set color bar position


IF KEYWORD_SET(z_buff) THEN BEGIN
	SET_PLOT, 'Z'																									;Output to Z buffer
	DEVICE, SET_PIXEL_DEPTH = 24, SET_RESOLUTION = [wfactor*(dim[0]), wfactor*(dim[1])], $	;Set device resolution and bit depth
		SET_CHARACTER_SIZE = [12, 20]
	!P.COLOR      = COLOR_24('black')																		;Foreground color
	!P.BACKGROUND = COLOR_24('white')																		;Background color
	!P.CHARSIZE   = 1.5																							;Set character size
	!P.FONT       = -1
ENDIF ELSE BEGIN
	IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN	
		PS_ON, FILENAME = epsfile, PAGE_SIZE = [4.0,4.0], MARGIN = 0.0, /INCHES;PAGE_SIZE = 0.001*dim*wfactor			;Switch to Postscript device
		DEVICE, /ENCAPSULATED
		!P.FONT     = 0																								;Hardware fonts
		!P.CHARSIZE = 0.75	
		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
			LOAD_BASIC_COLORS																							;Load basic color definitions
	ENDIF ELSE BEGIN
		SET_PLOT, 'X'
		WINDOW, XSIZE = 1.5*wfactor*(dim[0]), YSIZE = wfactor*(dim[1])										;Open graphics window
		!P.COLOR      = COLOR_24('black')																		;Foreground color
		!P.BACKGROUND = COLOR_24('white')																		;Background color
		!P.CHARSIZE   = 2.0		
		!P.FONT       = -1		
		thick_scale   = 1
	ENDELSE
ENDELSE

!P.MULTI = [0, 1, 3]

;; now calculate remaining dates
date_arr = date_arr[1:*]
i = 0
FOREACH date, date_arr DO BEGIN
	PRINT, 'Processing: ', date
    ave_h2o_bg0	    = (WRF_READ_VAR('Stratospheric_H2O'     , date, event, scheme, DOMAIN=domain, INDICES=region, H2O=1)).values
    trop_temp10	    = (WRF_READ_VAR('Tropopause_Temperature', date, event, scheme, DOMAIN=domain, INDICES=region, H2O=1)).values							;Read temperature variable from WRF output
    conv_fraction0  = (WRF_READ_VAR('Convective_Fraction'   , date, event, scheme, DOMAIN=domain, INDICES=region, H2O=1)).values					
    intensity10     = (WRF_READ_VAR('Convective_Intensity'  , date, event, scheme, DOMAIN=domain, INDICES=region, H2O=1)).values					
    strat_fraction0 = (WRF_READ_VAR('Stratospheric_Fraction', date, event, scheme, DOMAIN=domain, INDICES=region, H2O=1)).values
    y               = (WRF_READ_VAR('Latitude'              , date, event, scheme, DOMAIN=domain, INDICES=region, H2O=1)).values
    x               = (WRF_READ_VAR('Longitude'             , date, event, scheme, DOMAIN=domain, INDICES=region, H2O=1)).values
    z               = (WRF_READ_VAR('Z'		               , date, event, scheme, DOMAIN=domain, INDICES=region, H2O=1)).values
    xyz_trop        = (WRF_READ_VAR('Tropopause_Height'	   , date, event, scheme, DOMAIN=domain, INDICES=region, H2O=1)).values	
    cloud 		    =  WRF_READ_VAR('Cloud_Mixing_Ratio'    , date, event, scheme, DOMAIN=domain, INDICES=region, H2O=1)
    cloud_slice     = (WRF_READ_VAR('Cloud_slice'           , date, event, scheme, DOMAIN=domain, INDICES=region, H2O=1)).values
    h2o_slice	    = (WRF_READ_VAR('H2O_slice'             , date, event, scheme, DOMAIN=domain, INDICES=region, H2O=1)).values							;Read temperature variable from WRF output
	
    conv_fraction  = [conv_fraction ,  conv_fraction0]
    intensity1     = [intensity1    ,     intensity10]
    strat_fraction = [strat_fraction, strat_fraction0]
    trop_temp1     = [trop_temp1    ,     trop_temp10]
    ave_h2o_bg     = [ave_h2o_bg    ,     ave_h2o_bg0]

	offset = 0
    y0 = y[          offset ,          offset ]														;Set domain boundary points
    y1 = y[          offset ,dim[1]-(1+offset)]
    y2 = y[dim[0]-(1+offset),dim[1]-(1+offset)]
    y3 = y[dim[0]-(1+offset),          offset ]
    x0 = x[          offset ,          offset ]
    x1 = x[          offset ,dim[1]-(1+offset)]
    x2 = x[dim[0]-(1+offset),dim[1]-(1+offset)]
    x3 = x[dim[0]-(1+offset),          offset ]
    
    xc = INTERPOLATE(x, 0.5*(dim[0]-1), 0.5*(dim[1]-1))											;Get central grid point
    yc = INTERPOLATE(y, 0.5*(dim[0]-1), 0.5*(dim[1]-1))
    
    table   = [COLOR_24(200, 200, 200),VISUALIZE_88D_COLOR()]												;Set reflectivity color table
    rlevels = [-100.0, 5.0*FINDGEN(N_ELEMENTS(table))]													;Set reflectivity contour levels
    ;rlevels = [-100.0, 5.0 + 5.0*FINDGEN(N_ELEMENTS(table))]											;***for comparison with t-matrix
    wfactor = 400.0/(dim[0]) + 400.0/(dim[1])																	;Set map factor
    
    dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string
    date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Create date string
    
    map_pos = [0.05, 0.15, 0.95, 0.95]																			;Set map position
    bar_pos = [0.25, 0.10, 0.75, 0.12]																			;Set color bar position
    
    map_pos1 = [0.02, 0.10, 0.32, 0.50]																			;Set map position
    bar_pos1 = [0.04, 0.06, 0.30, 0.08]																			;Set color bar position
    
    map_pos2 = [0.35, 0.10, 0.65, 0.50]																			;Set map position
    bar_pos2 = [0.37, 0.06, 0.63, 0.08]																			;Set color bar position
    
    map_pos3 = [0.68, 0.10, 0.98, 0.50]																			;Set map position
    bar_pos3 = [0.70, 0.06, 0.96, 0.08]																			;Set color bar position
    
    
    IF KEYWORD_SET(z_buff) THEN BEGIN
    	SET_PLOT, 'Z'																									;Output to Z buffer
    	DEVICE, SET_PIXEL_DEPTH = 24, SET_RESOLUTION = [wfactor*(dim[0]), wfactor*(dim[1])], $	;Set device resolution and bit depth
    		SET_CHARACTER_SIZE = [12, 20]
    	!P.COLOR      = COLOR_24('black')																		;Foreground color
    	!P.BACKGROUND = COLOR_24('white')																		;Background color
    	!P.CHARSIZE   = 1.5																							;Set character size
    	!P.FONT       = -1
    ENDIF ELSE BEGIN
    	IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN	
    		PS_ON, FILENAME = epsfile, PAGE_SIZE = [4.0,4.0], MARGIN = 0.0, /INCHES;PAGE_SIZE = 0.001*dim*wfactor			;Switch to Postscript device
    		DEVICE, /ENCAPSULATED
    		!P.FONT     = 0																								;Hardware fonts
    		!P.CHARSIZE = 0.75	
    		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
    			LOAD_BASIC_COLORS																							;Load basic color definitions
    	ENDIF ELSE BEGIN
    		SET_PLOT, 'X'
    		WINDOW, XSIZE = 1.5*wfactor*(dim[0]), YSIZE = wfactor*(dim[1])										;Open graphics window
    		!P.COLOR      = COLOR_24('black')																		;Foreground color
    		!P.BACKGROUND = COLOR_24('white')																		;Background color
    		!P.CHARSIZE   = 2.0		
    		!P.FONT       = -1		
    		thick_scale   = 1
    	ENDELSE
    ENDELSE
    
    !P.MULTI = [0, 1, 3]
    
    color0 = COLOR_24('black')
    color1 = COLOR_24('blue' )
    color2 = COLOR_24('red'	 )
    color3 = COLOR_24('darkgreen')
    
    time = FINDGEN(N_ELEMENTS(date_arr))
    
    PLOT, time, time, /NODATA, $																							;Set up plot
    	THICK     = 2, $
    	XRANGE    = [0, N_ELEMENTS(date_arr)-1], $
    	XSTYLE    = 1, $
    	YRANGE    = [0, 1], $
    	YTICKS    = 1, $
    	YTICKN    = [' ', ' '], $
    	YSTYLE    = 1, $
    	CHARSIZE  = 3.0, $
    	POSITION  = [0.1, 0.55, 0.9, 0.95], $
    	TITLE     = 'Convective Fraction vs Mean H2O Concentration (350K - 380K)'
    
    AXIS, YAXIS = 0, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE   = [0.0, 1.0], $
    	YTITLE   = 'Convective Fraction (%)', $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
    	CHARSIZE = 3, $
    	COLOR  	 = color0
    
;    OPLOT, time, intensity1   , COLOR = color0, THICK = 3, LINESTYLE = 1												;Plot temperature measurments
    OPLOT, time, conv_fraction, COLOR = color0, THICK = 3													;Plot temperature measurments
        
;    AXIS, YAXIS = 0, $																								;Draw altitude axis
;    	SAVE     = 1, $
;    	YRANGE   = [3.0, 10.0], $
;    	YTITLE   = 'Stratospheric Fraction (%)', $
;    	YTICKS   = 1, $
;    	YSTYLE   = 1, $
;    	CHARSIZE = 3, $
;    	COLOR  	 = color3
;    
;    OPLOT, time, strat_fraction, COLOR = color3, THICK = 3													;Plot temperature measurments
    
    AXIS, YAXIS = 1, $																								;Draw temperature axis
    	SAVE     = 1, $
    	YRANGE   = [0, 20], $
    	YTITLE   = 'H2O concentration (ppmv)', $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
    	CHARSIZE = 3, $
    	COLOR    = color1
    
    OPLOT, time, ave_h2o_bg, COLOR = color1, THICK = 3, LINESTYLE = 0 
    
;    AXIS, YAXIS = 1, $																								;Draw temperature axis
;    	SAVE     = 1, $
;    	YRANGE   = [217, 220], $
;    	YTITLE   = 'Tropopause Temperature (K)', $
;    	YTICKS   = 1, $
;    	YSTYLE   = 1, $
;    	CHARSIZE = 3, $
;    	COLOR    = color2
;    
;    OPLOT, time, trop_temp1, COLOR = color2, THICK = 3, LINESTYLE = 0 
    ;;-end timeseries
    
;    ;;+begin plot maps    
;    MAP_SET, yc, xc, 0, CONIC = 1, /NOBORDER, $																;Draw map
;    	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
;    	ISOTROPIC = 1, $
;    	TITLE     = 'H2O at Bottom LMS', $
;    	LATLAB	  = 1, $
;    	LONLAB 	  = 1, $
;    	LABEL	  = 1, $
;    	NOERASE   = 1, $
;    	CHARSIZE  = 2.5, $
;    	POSITION  = map_pos1
;    
;    IF KEYWORD_SET(image) THEN BEGIN
;    	ij0 = CONVERT_COORD([(!X.WINDOW)[0],(!Y.WINDOW)[0]], /NORMAL, /TO_DEVICE)
;    	ij1 = CONVERT_COORD([(!X.WINDOW)[1],(!Y.WINDOW)[1]], /NORMAL, /TO_DEVICE)
;    
;    	xsize = LONG(ij1[0] - ij0[0])
;    	ysize = LONG(ij1[1] - ij0[1])
;    
;    	image0 = (MAX(R.values, DIM=3))[offset:(dim[0]-(1+offset)), offset:(dim[1]-(1+offset))]
;    	dim    = dim - (2*offset)
;    	
;    	IF KEYWORD_SET(eps) THEN $
;    		image0 = INTERPOLATE(image0, MAKEN(0, dim[0]-1, xsize/10), MAKEN(0, dim[1]-1, ysize/10), /GRID) ELSE $
;    		image0 = INTERPOLATE(image0, MAKEN(0, dim[0]-1, xsize   ), MAKEN(0, dim[1]-1, ysize   ), /GRID)
;    
;    	image0 = IMAGE_24(COLOR_LOOKUP_24((image0 < 75.0), table[1:*], MIN = 0.0, MAX = 75.0, $
;    				MISSING = COLOR_24(200, 200, 200), /NAN))
;    	TV, image0, ij0[0], ij0[1], TRUE = 3, XSIZE = xsize, YSIZE = ysize, /DEVICE
;    ENDIF ELSE $	
;    
;    	 map_plot 	   = h2o_slice
;    	 map_bar_title = 'H2O Concentration (ppmv)'														;Set color bar title
;    	 map_bar_min   = 10.0																						;Set echo top minimum
;    	 map_bar_max   = 205.0																					;Set echo top maximum
;    	 map_bar_ticks = 7																						;Set number of color bar ticks
;    	 map_table     = [VISUALIZE_88D_COLOR(3)]							;Set color table
;    	 map_levels    = [10.0 + 10.0 * FINDGEN(N_ELEMENTS(map_table))]								;Set contour levels
;        	
;    CONTOUR, map_plot, x, y, $																	;Contour values
;            OVERPLOT  = 1, $
;            FILL      = 1, $
;            LEVELS    = map_levels, $
;            C_COLOR   = map_table, $
;            TITLE     = date_string, $
;            POSITION  = map_pos1
;    
;    MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines
;    
;    MAP_SET, yc, xc, 0, CONIC = 1, $																				;Redraw map boundaries
;    	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
;    	ISOTROPIC = 1, $
;    	NOERASE   = 1, $
;    	LATLAB	  = 1, $
;    	LONLAB 	  = 1, $
;    	LABEL	  = 1, $
;    	CHARSIZE  = 2.5, $
;    	POSITION  = map_pos1
;    
;    COLOR_BAR_24_KPB, map_table[1:*], OVER = map_table[-1], UNDER = map_table[0], $						;Draw map color bar
;    	TICKS     = map_bar_ticks, $
;    	RANGE     = [map_bar_min, map_bar_max], $
;    	TITLE     = map_bar_title, $
;    	POSIT     = bar_pos1, $
;    	CHARSIZE  = 1.75, $
;    	XCHARSIZE = 1.75
;    
;    ;; end h2o plot ;;
;    ;; begin cloud tops ;;
;       
;    MAP_SET, yc, xc, 0, CONIC = 1, /NOBORDER, $																;Draw map
;    	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
;    	ISOTROPIC = 1, $
;    	TITLE     = 'Trop. Rel Cloud Top', $
;    	LATLAB	  = 1, $
;    	LONLAB 	  = 1, $
;    	LABEL	  = 1, $
;    	NOERASE   = 1, $
;    	CHARSIZE  = 2.5, $
;    	POSITION  = map_pos2
;    
;    map_plot      = 0.001*((MAX((cloud.values GE 1.0E-5)*z, DIM = 3, /NAN)) - xyz_trop)						;Set map variable to cloud top altitude
;    	
;    bad = WHERE(FINITE(map_plot,/NAN), bad_count, COMPLEMENT=good, NCOMPLEMENT=good_count)
;    IF (bad_count GT 0) THEN BEGIN
;    	map_plot[bad ] = -100.0
;    	map_plot[good] = map_plot[good]
;    ENDIF
;    
;    map_bar_title = 'Cloud Top Relative-Altitude (km)'														;Set color bar title
;    map_bar_min   = -2.5																						;Set echo top minimum
;    map_bar_max   = 2.5																				;Set echo top maximum
;    map_bar_ticks = 2																						;Set number of color bar ticks
;    map_table     = [COLOR_24('gray90'), BLUE_RED_24(30,0.0)]							          					    ;Set color table
;    map_levels    = [-100.0, MAKEN(-2.5,2.5,30)]  									              ;Set contour levels																					;Set number of color bar ticks
;    
;    CONTOUR, map_plot, x, y, $																	;Contour values
;         OVERPLOT  = 1, $
;         FILL      = 1, $
;         LEVELS    = map_levels, $
;         C_COLOR   = map_table, $
;         TITLE     = date_string, $
;         XLOG	  = 1, $
;         POSITION  = map_pos2
;    
;    MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines
;     
;    MAP_SET, yc, xc, 0, CONIC = 1, $																				;Redraw map boundaries
;    	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
;    	ISOTROPIC = 1, $
;    	NOERASE   = 1, $
;    	CHARSIZE  = 2.5, $
;    	POSITION  = map_pos2
;    	
;    COLOR_BAR_24_KPB, map_table[1:*], OVER = map_table[-1], UNDER = map_table[0], $						;Draw map color bar
;    	TICKS     = map_bar_ticks, $
;    	RANGE     = [map_bar_min, map_bar_max], $
;    	TITLE     = map_bar_title, $
;    	POSIT     = bar_pos2, $
;    	CHARSIZE  = 1.75, $
;    	XCHARSIZE = 1.75
;
;    ;; end cloud tops ;;
;    ;; start cloud slice
;    MAP_SET, yc, xc, 0, CONIC = 1, /NOBORDER, $																;Draw map
;    	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
;    	ISOTROPIC = 1, $
;    	TITLE     = 'Cloud at Bottom LMS', $
;    	LATLAB	  = 1, $
;    	LONLAB 	  = 1, $
;    	LABEL	  = 1, $
;    	NOERASE   = 1, $
;    	CHARSIZE  = 2.5, $
;    	POSITION  = map_pos3
;    
;     map_plot	   = cloud_slice
;     map_bar_title = 'Cloud Concentration (g/kg)'														;Set color bar title
;     map_bar_min   = 0.0 																						;Set echo top minimum
;     map_bar_max   = 1.0E-3 																					;Set echo top maximum
;     map_bar_ticks = 3																						;Set number of color bar ticks
;     map_table     = [COLOR_24(230, 230, 230),VISUALIZE_88D_COLOR(3)]							;Set color table
;     map_levels    = [0.0, 1.0E-9, 1.0E-8, 1.0E-7, 1.0E-6, 1.0E-5, 1.0E-4, 1.0E-3]								;Set contour levels
;    
;    CONTOUR, map_plot, x, y, $																	;Contour values
;        OVERPLOT  = 1, $
;        FILL      = 1, $
;        LEVELS    = map_levels, $
;        C_COLOR   = map_table, $
;        TITLE     = date_string, $
;        XLOG	  = 1, $
;        POSITION  = map_pos3
;    
;  	MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines
;    
;    MAP_SET, yc, xc, 0, CONIC = 1, $																				;Redraw map boundaries
;    	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
;    	ISOTROPIC = 1, $
;    	NOERASE   = 1, $
;    	CHARSIZE  = 2.5, $
;    	POSITION  = map_pos3
;    	
;    COLOR_BAR_24_KPB, map_table[1:*], OVER = map_table[-1], UNDER = map_table[0], $						;Draw map color bar
;    	TICKS     = map_bar_ticks, $
;    	RANGE     = [map_bar_min, map_bar_max], $
;    	TITLE     = map_bar_title, $
;    	POSIT     = bar_pos3, $
;    	CHARSIZE  = 1.75, $
;    	XCHARSIZE = 1.75
;    
;    ;;end cloud slice
    
    date1   = MAKE_ISO_DATE_STRING(date,/COMPACT,/UTC)
    
    epsfile = outdir +  date1  + '.eps'						;EPS filename
    pdffile = outdir +  date1  + '.pdf'						;PDF filename
    pngfile = outdir +  date1  + '.png'						;PNG filename
    
    
    IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
    	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
    		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
    	PS_OFF																											;Turn PS off
    	
    	IF KEYWORD_SET(pdf) THEN PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS						;Convert to PDF
    ENDIF ELSE IF KEYWORD_SET(png) THEN $
    	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file
    
ENDFOREACH

END