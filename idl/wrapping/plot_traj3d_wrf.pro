PRO PLOT_TRAJ3D_WRF, run, state, $
	PLOT      = plot, $
	DIRECTORY = directory, $
	PNG		  = png, $
	CLOBBER   = clobber

;+
;NAME:
;     TRAJ3D_RAP_P
;PURPOSE:
;     This copies variables from ERA-Interim analysis into a single
;     file in pressure coordinates for use in TRAJ3D.
;     W at the surface is set to zero.  One pressure level is added at 
;     the top of the domain (p = 0), where w is also set to zero.
;CATEGORY:
;     Data handling utility.
;CALLING SEQUENCE:
;     TRAJ3D_RAP_P, date0, outfile
;INPUT:
;		flight_name : RAF flight name (e.g., 'rf01')
;		direction   : 'forward' or 'backward'
;		ndays       : Length of trajectory run in days.  Default is 5.
;KEYWORDS:
;     PLOT      : If set, plot sample maps.
;     DIRECTORY : Output directory for wind file.
;		CLOBBER   : If set, overwrite existing file. This is the default.
;OUTPUT:
;     Netcdf file.
;MODIFICATION HISTORY:
;		C. Homeyer:       2015-06-22.
;-

COMPILE_OPT IDL2																									;Set compile options

IF (N_ELEMENTS(run       ) EQ 0) THEN run        = '20120530_ncar'
IF (N_ELEMENTS(experiment) EQ 0) THEN experiment = 'd03_30km_icloud'
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1
IF (N_ELEMENTS(offset    ) EQ 0) THEN offset     = 0 
IF (N_ELEMENTS(offset    ) EQ 0) THEN offset     = 0 

indir  = '/data3/dphoenix/wrf/20120530_ncar/d03_30km_icloud/traj3d/20120530_ncar.ncd'															;Set input directory

r = TRAJ3D_READ_FILE_3(indir)

;;+Spaghetti plots of altitude change from updraft air in storm center
;zone = WHERE((r.x[*,0] GT 258.89899) AND (r.x[*,0] LT 259.22501) AND (r.y[*,0] GT 39.0077) $
;		AND (r.y[*,0] LT 39.2001))

;;+Spaghetti plots of altitude change from wrapped air in leading anvil
;zone = WHERE((r.x[*,0] GT 265.38599) AND (r.x[*,0] LT 266.69348) AND (r.y[*,0] GT 37.1736) $
;		AND (r.y[*,0] LT 37.7208))
		
;;+Spaghetti plots of altitude change from wrapped air in trailing anvil
;zone = WHERE((r.x[*,0] GT 258.26801) AND (r.x[*,0] LT 259.55701) AND (r.y[*,0] GT 38.5166) $
;		AND (r.y[*,0] LT 38.9871))

;;+Spaghetti plots of altitude change from wrapped air in leading anvil south
;zone = WHERE((r.x[*,0] GT 264.73682) AND (r.x[*,0] LT 266.04089) AND (r.y[*,0] GT 36.7029) $
;		AND (r.y[*,0] LT 37.2432))

;;+Spaghetti plots of altitude change from wrapped air in leading anvil south (smaller)
;zone = WHERE((r.x[*,0] GT 265.34760) AND (r.x[*,0] LT 266.04089) AND (r.y[*,0] GT 36.7029) $
;		AND (r.y[*,0] LT 36.9762))

;;+Spaghetti plots of altitude change from clear air outside of cloud
;zone = WHERE((r.x[*,0] GT 267.2666) AND (r.x[*,0] LT 268.55899) AND (r.y[*,0] GT 37.0776) $
;		AND (r.y[*,0] LT 37.6449))

;;+Spaghetti plots of altitude change from small updraft region
zone = WHERE((r.x[*,0] GT 259.15799) AND (r.x[*,0] LT 259.164) AND (r.y[*,0] GT 39.552) $
		AND (r.y[*,0] LT 39.557))
		

alt1   = WHERE((r.z[zone,0] GE 5000.0) AND (r.z[zone,0] LE 9000.0))
;alt1  = alt[0:N_ELEMENTS(alt)-1:100] 
colors = HCL_COLOR_TABLE(N_ELEMENTS(alt1), HUE_RANGE = [200.0, 400.0])								;Set color table

PLOT, r.z[0,*], COLOR = COLOR_24('black'), YRANGE = [4000.0, 10000.0], /NODATA

FOR ii = 0, N_ELEMENTS(alt1)-1 DO BEGIN
	OPLOT, r.z[zone[alt1[ii]],*], COLOR = colors[ii]
ENDFOR
;;-end

date = MAKE_DATE(2012,5,30,22)
FOR ss = 0, 3 DO BEGIN
    dt = -1800

	indir  = '/data3/dphoenix/wrf/20120530_ncar/d03_30km_icloud/traj3d/20120530_ncar.ncd'															;Set input directory
	r = TRAJ3D_READ_FILE_3(indir)
    lon_pos = [r.x[zone[alt1],(-dt/300)*ss]]
    lat_pos = [r.y[zone[alt1],(-dt/300)*ss]]
    hgt_pos = [r.z[zone[alt1],(-dt/300)*ss]]

;	num = N_ELEMENTS(lon_pos)
;	int = num/12
;
;	lon_pos = lon_pos[0:num-1:int]
;	lat_pos = lat_pos[0:num-1:int]
;	hgt_pos = hgt_pos[0:num-1:int]

	STOP
 	
	ij_start = [ ]
	pos = N_ELEMENTS(lon_pos)
	FOR s = 0, pos-1 DO BEGIN
 	    PRINT, date
 	    PRINT, s
        x  = WRF_READ_VAR('Longitude', date, run, state, DOMAIN = domain, INDICES = region)			;Read variables
        y  = WRF_READ_VAR('Latitude',  date, run, state, DOMAIN = domain, INDICES = region)
        z  = WRF_READ_VAR('Z', 		   date, run, state, DOMAIN = domain, INDICES = region)
        R  = WRF_READ_VAR('REFL',      date, run, state, DOMAIN = domain, INDICES = region)
        
        PRINT, MIN(x.values)+360.0, MAX(x.values)+360.0
        PRINT, MIN(y.values), MAX(y.values)
        
        bad = WHERE (R.values EQ 0.0, bad_count, COMPLEMENT = good, NCOMPLEMENT = good_count)
        R.values [bad ] = -35.0000
        R.values [good] = R.values [good]
        
        dim = SIZE(x.values, /DIMENSIONS)																			;Get dimension sizes
                
        ;;Find index of nearest start point
        i0 = WHERE((ABS(x.values - (lon_pos[s]-360.0)) LT 0.01) AND (ABS(y.values - lat_pos[s]) LT 0.01))
        IF (i0[0] EQ -1) THEN i1 = WHERE((ABS(x.values - (lon_pos[s]-360.0)) LT 0.1) AND (ABS(y.values - lat_pos[s]) LT 0.1))
        IF (i0[0] EQ -1) THEN i1 = WHERE((ABS(x.values - (lon_pos[s]-360.0)) LT 0.5) AND (ABS(y.values - lat_pos[s]) LT 0.5))
    
        ;;Of all locations found, find closest to specified location
        ;store_arr = [ ]
        ;FOR xy = 0, N_ELEMENTS(i0)-1 DO BEGIN
        ;	store = SQRT((x.values[i0[xy]] - lon_pos[s])^2 + (y.values[i0[xy]] - lat_pos[s])^2)
        ;	store_arr = [store_arr, store]
        ;ENDFOR
        ;
        ;ij_start = WHERE(MIN(store_arr,/NAN) EQ store_arr)
        ;ij_start = ARRAY_INDICES(x.values, i0[ij_start])
         
        ij_start = [ij_start, i0] 
    ENDFOR
                 
    y0 = y.values[          offset ,          offset ]														;Set domain boundary points
    y1 = y.values[          offset ,dim[1]-(1+offset)]
    y2 = y.values[dim[0]-(1+offset),dim[1]-(1+offset)]
    y3 = y.values[dim[0]-(1+offset),          offset ]
    x0 = x.values[          offset ,          offset ]
    x1 = x.values[          offset ,dim[1]-(1+offset)]
    x2 = x.values[dim[0]-(1+offset),dim[1]-(1+offset)]
    x3 = x.values[dim[0]-(1+offset),          offset ]
    
    xc = INTERPOLATE(x.values, 0.5*(dim[0]-1), 0.5*(dim[1]-1))											;Get central grid point
    yc = INTERPOLATE(y.values, 0.5*(dim[0]-1), 0.5*(dim[1]-1))
        
    table   = [COLOR_24(200, 200, 200),VISUALIZE_88D_COLOR()]												;Set reflectivity color table
    rlevels = [-100.0, 5.0*FINDGEN(N_ELEMENTS(table))]													;Set reflectivity contour levels
    ;rlevels = [-100.0, 5.0 + 5.0*FINDGEN(N_ELEMENTS(table))]											;***for comparison with t-matrix
    wfactor = 400.0/(dim[0]) + 400.0/(dim[1])																	;Set map factor
    
    dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string
    date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Create date string
    
    outdir  = !WRF_DIRECTORY + run + '/' + experiment + '/plots/trajectory_refl/'
    
    
    epsfile = outdir + experiment + '_' + STRMID(STRING(ss+5),11) + '_' + date_string + '.eps'						;EPS filename
    pdffile = outdir + experiment + '_' + STRMID(STRING(ss+5),11) + '_' + date_string + '.pdf'						;PDF filename
    pngfile = outdir + experiment + '_' + STRMID(STRING(ss+5),11) + '_' + date_string + '.png'						;PNG filename
    
    FILE_MKDIR, outdir																								;Create output directory, if necessary
    
    map_pos = [0.05, 0.15, 0.95, 0.95]																			;Set map position
    bar_pos = [0.25, 0.10, 0.75, 0.12]																			;Set color bar position
    
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
    		WINDOW, XSIZE = wfactor*(dim[0]), YSIZE = wfactor*(dim[1])										;Open graphics window
    		!P.COLOR      = COLOR_24('black')																		;Foreground color
    		!P.BACKGROUND = COLOR_24('white')																		;Background color
    		!P.CHARSIZE   = 2.0		
    		!P.FONT       = -1																							;Use Hershey fonts
    	ENDELSE
    ENDELSE
    
    MAP_SET, yc, xc, 0, CONIC = 1, /NOBORDER, $																;Draw map
    	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
    	ISOTROPIC = 1, $
    	TITLE     = 'WRF-' + experiment + ' ' + dom_string + ' valid ' + date_string, $
    	POSITION  = map_pos
    
    IF KEYWORD_SET(image) THEN BEGIN
    	ij0 = CONVERT_COORD([(!X.WINDOW)[0],(!Y.WINDOW)[0]], /NORMAL, /TO_DEVICE)
    	ij1 = CONVERT_COORD([(!X.WINDOW)[1],(!Y.WINDOW)[1]], /NORMAL, /TO_DEVICE)
    
    	xsize = LONG(ij1[0] - ij0[0])
    	ysize = LONG(ij1[1] - ij0[1])
    
    	image0 = (MAX(R.values, DIM=3))[offset:(dim[0]-(1+offset)), offset:(dim[1]-(1+offset))]
    	dim    = dim - (2*offset)
    	
    	IF KEYWORD_SET(eps) THEN $
    		image0 = INTERPOLATE(image0, MAKEN(0, dim[0]-1, xsize/10), MAKEN(0, dim[1]-1, ysize/10), /GRID) ELSE $
    		image0 = INTERPOLATE(image0, MAKEN(0, dim[0]-1, xsize   ), MAKEN(0, dim[1]-1, ysize   ), /GRID)
    
    	image0 = IMAGE_24(COLOR_LOOKUP_24((image0 < 75.0), table[1:*], MIN = 0.0, MAX = 75.0, $
    				MISSING = COLOR_24(200, 200, 200), /NAN))
    	TV, image0, ij0[0], ij0[1], TRUE = 3, XSIZE = xsize, YSIZE = ysize, /DEVICE
    ENDIF ELSE $	
    	CONTOUR, MAX(R.values, DIM=3), x.values, y.values, $												;Contour reflectivity values
    ;	CONTOUR, R.values[*,*,37], x.values, y.values, $													;For T-Matrix comparison
    		OVERPLOT  = 1, $
    		FILL      = 1, $
    		LEVELS    = rlevels, $
    		C_COLOR   = table
    
    
    USERSYM_STAR, /FILL																					;Load plane symbol at flight path orientation
    
    color_array = [COLOR_24('red'), COLOR_24('blue'), COLOR_24('green'), COLOR_24('black'), $
    				COLOR_24('white'), COLOR_24('purple')]
    FOR xx = 0, N_ELEMENTS(ij_start)-1 DO BEGIN
    	PLOTS, (x.values)[ij_start[xx]], (y.values)[ij_start[xx]], $																		;Overplot plane symbol
    		PSYM    = 8, $
    		SYMSIZE = 4 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
    		NOCLIP  = 0, $
    		COLOR   = color_array[ss]
    ENDFOR
    
    MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines
    
    MAP_SET, yc, xc, 0, CONIC = 1, $																				;Redraw map boundaries
    	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
    	ISOTROPIC = 1, $
    	NOERASE   = 1, $
    	POSITION  = map_pos
    	
    COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
    	RANGE = [0, 75], $
    	TICKS = 5, $
    	TITLE = 'Reflectivity (dBZ)', $
    	POSIT = bar_pos
    
    IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
    	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
    		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
    	PS_OFF																											;Turn PS off
    	
    	IF KEYWORD_SET(pdf) THEN PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS						;Convert to PDF
    ENDIF ELSE IF KEYWORD_SET(png) THEN $
    	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file
    
    date = TIME_INC(date, dt)
ENDFOR
    
STOP
END
