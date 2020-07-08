PRO PLOT_RIP_WRF_TRAJ5, run, state, $
	PLOT      = plot, $
	DIRECTORY = directory, $
	PNG		  = png, $
	EPS		  = eps, $
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
;	  CLOBBER   : If set, overwrite existing file. This is the default.
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

;Plots trajectory 5
lat = [39.48, $
39.44, $
39.41, $
39.38, $
39.35, $
39.32, $
39.28, $
39.25, $
39.22, $
39.18, $
39.14, $
39.11, $
39.07, $
39.03, $
38.98, $
38.94, $
38.90, $
38.86, $
38.81, $
38.77, $
38.73, $
38.68, $
38.63, $
38.59, $
38.55, $
38.50, $
38.46, $
38.42, $
38.37, $
38.33, $
38.28, $
38.24, $
38.19, $
38.15, $
38.10, $
38.06, $
38.02, $
37.97, $
37.93, $
37.88, $
37.84, $
37.80, $
37.76, $
37.72, $
37.68, $
37.64, $
37.60, $
37.56, $
37.52, $
37.49, $
37.45, $
37.42, $
37.39, $
37.36, $
37.33, $
37.30, $
37.27, $
37.25, $
37.22, $
37.20, $
37.18, $
37.16, $
37.14]

lon = [-100.48, $
-100.40, $
-100.33, $
-100.25, $
-100.17, $
-100.09, $
-100.01, $
 -99.93, $
 -99.85, $
 -99.77, $
 -99.68, $
 -99.59, $
 -99.50, $
 -99.41, $
 -99.32, $
 -99.23, $
 -99.14, $
 -99.05, $
 -98.96, $
 -98.87, $
 -98.78, $
 -98.69, $
 -98.60, $
 -98.52, $
 -98.43, $
 -98.35, $
 -98.27, $
 -98.19, $
 -98.11, $
 -98.03, $
 -97.95, $
 -97.87, $
 -97.79, $
 -97.72, $
 -97.64, $
 -97.57, $
 -97.49, $
 -97.42, $
 -97.34, $
 -97.26, $
 -97.18, $
 -97.10, $
 -97.02, $
 -96.94, $
 -96.86, $
 -96.78, $
 -96.70, $
 -96.62, $
 -96.55, $
 -96.48, $
 -96.40, $
 -96.33, $
 -96.26, $
 -96.18, $
 -96.11, $
 -96.03, $
 -95.95, $
 -95.88, $
 -95.80, $
 -95.73, $
 -95.65, $
 -95.58, $
 -95.50] 

o3 = [0.231,$
0.230,$
0.230,$
0.230,$
0.230,$
0.230,$
0.229,$
0.229,$
0.229,$
0.229,$
0.229,$
0.229,$
0.227,$
0.228,$
0.227,$
0.230,$
0.231,$
0.235,$
0.233,$
0.231,$
0.230,$
0.230,$
0.230,$
0.230,$
0.233,$
0.233,$
0.237,$
0.239,$
0.246,$
0.249,$
0.243,$
0.242,$
0.242,$
0.244,$
0.245,$
0.247,$
0.243,$
0.247,$
0.245,$
0.243,$
0.244,$
0.242,$
0.244,$
0.245,$
0.241,$
0.247,$
0.247,$
0.246,$
0.246,$
0.245,$
0.245,$
0.246,$
0.242,$
0.243,$
0.243,$
0.242,$
0.240,$
0.241,$
0.238,$
0.242,$
0.243,$
0.240,$
0.238,$
0.242]*1.0E3


hgt=[12001.0, $
12001.2, $
11999.8, $
11995.6, $
11993.1, $
12004.6, $
11999.5, $
11991.0, $
12008.1, $
12006.1, $
12009.8, $
12027.7, $
11962.3, $
12029.7, $
12048.5, $
12047.6, $
12052.9, $
12085.1, $
12101.5, $
12106.4, $
12120.3, $
12144.3, $
12164.4, $
12186.3, $
12180.4, $
12158.3, $
12160.5, $
12119.7, $
12097.2, $
12078.1, $
12048.9, $
12045.4, $
12051.9, $
12043.3, $
12031.0, $
12041.8, $
12026.7, $
12018.8, $
11990.7, $
11968.6, $
11959.6, $
11947.1, $
11961.5, $
11970.9, $
11966.0, $
11993.6, $
11984.0, $
11981.6, $
11968.7, $
11953.8, $
11960.1, $
11981.6, $
11970.7, $
11954.3, $
11973.1, $
12012.3, $
12029.0, $
12050.8, $
12041.0, $
12078.5, $
12096.8, $
12068.0, $
12061.2, $
12097.2]*1.0E-3

s=0
;dt=300
dt =3600
date = MAKE_DATE(2012,5,30,21,20)
o3_line = [ ]
FOR xy = 0, 52 DO BEGIN
    PRINT, xy
    x  	   = (WRF_READ_VAR('Longitude', date, run, experiment, DOMAIN = domain, INDICES = region)).values			;Read variables
    y      = (WRF_READ_VAR('Latitude',  date, run, experiment, DOMAIN = domain, INDICES = region)).values	

    i0 = WHERE((ABS(x - (lon[xy])) LT 0.01) AND (ABS(y - lat[xy]) LT 0.01))
    IF (i0[0] EQ -1) THEN i1 = WHERE((ABS(x - (lon[xy])) LT 0.1) AND (ABS(y - lat[xy]) LT 0.1))
    IF (i0[0] EQ -1) THEN i1 = WHERE((ABS(x - (lon[xy])) LT 0.5) AND (ABS(y - lat[xy]) LT 0.5))
    
    ;Of all locations found, find closest to specified location
    store_arr = [ ]
    HELP, i0
    FOR xx = 0, N_ELEMENTS(i0)-1 DO BEGIN
    	store = SQRT((x[i0[xx]] - lon[xy])^2 + (y[i0[xx]] - lat[xy])^2)
    	store_arr = [store_arr, store]
    ENDFOR
    
    ij_start = WHERE(MIN(store_arr,/NAN) EQ store_arr)
    ;ij_start = ARRAY_INDICES(x, i0[ij_start])

	o3_line = [o3_line, i0[ij_start]]
ENDFOR

FOR s = 0, 52 DO BEGIN
    x  	   = (WRF_READ_VAR('Longitude',       date, run, experiment, DOMAIN = domain, INDICES = region)).values			;Read variables
    y      = (WRF_READ_VAR('Latitude',        date, run, experiment, DOMAIN = domain, INDICES = region)).values	
    z      = (WRF_READ_VAR('Z', 		      date, run, experiment, DOMAIN = domain, INDICES = region)).values	
    ztrop  = (WRF_READ_VAR('Z_trop', 		  date, run, experiment, DOMAIN = domain, INDICES = region)).values	
    cloud  = (WRF_READ_VAR('CLOUD_MIX_TOTAL', date, run, experiment, DOMAIN = domain, INDICES = region)).values			;Read variables
                     
    ztrop    = MEDIAN(ztrop, 5)
    dim 	 = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
    xyz_trop = REBIN(ztrop, dim[0], dim[1], dim[2], /SAMPLE)
        
    cloud_relz = 0.001*((MAX((cloud GE 1.0E-5)*z, DIM = 3, /NAN)) - xyz_trop)    
    
    i0 = WHERE((ABS(x - (lon[s])) LT 0.01) AND (ABS(y - lat[s]) LT 0.01))
    IF (i0[0] EQ -1) THEN i0 = WHERE((ABS(x - (lon[s])) LT 0.1) AND (ABS(y - lat[s]) LT 0.1))
    IF (i0[0] EQ -1) THEN i0 = WHERE((ABS(x - (lon[s])) LT 0.5) AND (ABS(y - lat[s]) LT 0.5))
    
    ;Of all locations found, find closest to specified location
    store_arr = [ ]
    FOR xy = 0, N_ELEMENTS(i0)-1 DO BEGIN
    	store = SQRT((x[i0[xy]] - lon[s])^2 + (y[i0[xy]] - lat[s])^2)
    	store_arr = [store_arr, store]
    ENDFOR
    
    ij_start = WHERE(MIN(store_arr,/NAN) EQ store_arr)
    ij_start = ARRAY_INDICES(x, i0[ij_start])
    
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
        
    wfactor = 400.0/(dim[0]) + 400.0/(dim[1])																	;Set map factor
    
    dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string
    date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Create date string
    
    outdir  = !WRF_DIRECTORY + run + '/' + experiment + '/plots/rip_wrf_traj5_paperfig/'
    
    epsfile = outdir + experiment + '_' + date_string + '.eps'						        ;EPS filename
    pdffile = outdir + experiment + '_' + date_string + '.pdf'						        ;PDF filename
    pngfile = outdir + experiment + '_' + date_string + '.png'						        ;PNG filename
    
    FILE_MKDIR, outdir																								;Create output directory, if necessary
 	
 	!P.MULTI = [0, 2, 1]																								;Set multiple plots
   
    ;map_pos = [0.05, 0.15, 0.95, 0.95]																			;Set map position
    ;bar_pos = [0.25, 0.10, 0.75, 0.12]																			;Set color bar position
    ;tsr_pos = [0.60, 0.55, 0.85, 0.85]																	;Set cross-section position
    
    map_pos = [0.05, 0.15, 0.55, 0.95]																			;Set map position
    bar_pos = [0.15, 0.10, 0.45, 0.12]																			;Set color bar position
    tsr_pos = [0.60, 0.22, 0.95, 0.78]																	;Set cross-section position

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
    		PS_ON, FILENAME = epsfile, PAGE_SIZE = [8.0,4.0], MARGIN = 0.0, /INCHES;PAGE_SIZE = 0.001*dim*wfactor			;Switch to Postscript device
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

	table = HCL_COLOR_TABLE(160, HUE_RANGE = [100.0, 300.0])
    o3levels = 90.0+FINDGEN(160)
    io3 = WHERE(o3levels EQ o3[s])

    CONTOUR, cloud_relz, x, y, /NODATA, $												;Contour reflectivity values
    	OVERPLOT  = 1, $
    	FILL      = 0, $
    	LEVELS    = 0.0, $
    	C_COLOR   = table[io3]
    
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
	    
    	CONTOUR, cloud_relz, x, y, $												;Contour reflectivity values
    		OVERPLOT  = 1, $
    		FILL      = 0, $
    		LEVELS    = 0.0, $
    		C_COLOR   = table[io3]
    
    USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation    

    FOR yy = 0, N_ELEMENTS(o3_line)-1 DO BEGIN
    	iio3 = WHERE(o3levels EQ o3[yy])		
		PLOTS, (x)[o3_line[yy]], (y)[o3_line[yy]], $																		;Overplot plane symbol
    		PSYM    = 8, $
    		SYMSIZE = 4 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
    		NOCLIP  = 0, $
    		COLOR   = table[iio3]
    ENDFOR

    USERSYM_STAR, /FILL																					;Load plane symbol at flight path orientation
    positions = i0[ij_start]    
    FOR xx = 0, N_ELEMENTS(ij_start)-1 DO BEGIN
    	PLOTS, (x)[positions[xx]], (y)[positions[xx]], $																		;Overplot plane symbol
    		PSYM     = 8, $
    		SYMSIZE  = 6 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
    		NOCLIP   = 0, $
    		CONTINUE = 1, $
			COLOR    = table[io3]
    ENDFOR
    
    MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines
 
    PLOT, hgt, /NODATA, $
    	TITLE    = 'Trajectory Height', $
   		XRANGE   = [0,52], $
   		YRANGE   = [6, 13], $
   		XTITLE   = 'Time', $
   		YTITLE   = 'Altitude (km)', $
   		BACKGROUND = 255, $
   		POSITION = tsr_pos

	AXIS, YAXIS = 0, $																								;Draw altitude axis
	    SAVE     = 1, $
	    YRANGE   = [6, 13], $
	    XRANGE   = [0,52], $
	    YTITLE   = 'Altitude (km)'
   
    OPLOT, hgt
    XYOUTS, s, hgt[s], 'z', /DATA
   
   	AXIS, YAXIS = 1, $																								;Draw altitude axis
	    SAVE     = 1, $
	    YRANGE   = [225.0, 255.0], $
	    XRANGE   = [0,52], $
	    YTITLE   = 'Ozone Concentration'
   		
    OPLOT, o3
    XYOUTS, s, o3[s], 'o3', /DATA

    MAP_SET, yc, xc, 0, CONIC = 1, $																				;Redraw map boundaries
    	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
    	ISOTROPIC = 1, $
    	NOERASE   = 1, $
    	POSITION  = map_pos
    
    COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
    	RANGE = [90,250], $
    	TICKS = 5, $
    	TITLE = 'O3 Concentration (ppb)', $
    	POSIT = bar_pos
      
    IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
    	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
    		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
    	PS_OFF																											;Turn PS off
    	
    	IF KEYWORD_SET(pdf) THEN PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS						;Convert to PDF
    ENDIF ELSE IF KEYWORD_SET(png) THEN $
    	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file
  
    date = TIME_INC(date, dt)
    s = s + (dt/300)
ENDFOR

END
