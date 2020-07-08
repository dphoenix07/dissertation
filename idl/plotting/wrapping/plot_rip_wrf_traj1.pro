PRO PLOT_RIP_WRF_TRAJ1, run, state, $
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

;Plots trajectory 1
lat = [39.48, $
39.42, $
39.37, $
39.32, $
39.27, $
39.22, $
39.16, $
39.11, $
39.06, $
39.00, $
38.95, $
38.89, $
38.83, $
38.78, $
38.72, $
38.66, $
38.60, $
38.54, $
38.48, $
38.42, $
38.36, $
38.30, $
38.24, $
38.18, $
38.12, $
38.06, $
38.00, $
37.95, $
37.89, $
37.83, $
37.77, $
37.72, $
37.66, $
37.61, $
37.56, $
37.50, $
37.45, $
37.40, $
37.35, $
37.29, $
37.24, $
37.19, $
37.14, $
37.10, $
37.05, $
37.00, $
36.96, $
36.92, $
36.88, $
36.84, $
36.81, $
36.78, $
36.75, $
36.72, $
36.70, $
36.68, $
36.65, $
36.64, $
36.62, $
36.60, $
36.59, $
36.57, $
36.55, $
36.54, $
36.53, $
36.51]

lon = [-100.48, $
-100.42, $
-100.36, $
-100.30, $
-100.24, $
-100.18, $
-100.12, $
-100.06, $
-100.01, $
 -99.95, $
 -99.89, $
 -99.83, $
 -99.78, $
 -99.72, $
 -99.66, $
 -99.60, $
 -99.55, $
 -99.49, $
 -99.44, $
 -99.38, $
 -99.33, $
 -99.29, $
 -99.24, $
 -99.20, $
 -99.16, $
 -99.12, $
 -99.08, $
 -99.04, $
 -99.01, $
 -98.97, $
 -98.94, $
 -98.91, $
 -98.89, $
 -98.86, $
 -98.84, $
 -98.82, $
 -98.80, $
 -98.78, $
 -98.76, $
 -98.74, $
 -98.73, $
 -98.71, $
 -98.69, $
 -98.68, $
 -98.67, $
 -98.65, $
 -98.64, $
 -98.63, $
 -98.62, $
 -98.61, $
 -98.60, $
 -98.59, $
 -98.58, $
 -98.57, $
 -98.56, $
 -98.55, $
 -98.54, $
 -98.53, $
 -98.51, $
 -98.49, $
 -98.47, $
 -98.44, $ 
 -98.42, $
 -98.40, $
 -98.37, $
 -98.34]

o3 = [0.096,$
0.096,$
0.096,$
0.096,$
0.096,$
0.096,$
0.085,$
0.079,$
0.084,$
0.088,$
0.093,$
0.095,$
0.095,$
0.096,$
0.096,$
0.096,$
0.095,$
0.095,$
0.095,$
0.095,$
0.095,$
0.095,$
0.095,$
0.095,$
0.095,$
0.094,$
0.094,$
0.095,$
0.094,$
0.094,$
0.095,$
0.095,$
0.095,$
0.095,$
0.095,$
0.096,$
0.096,$
0.096,$
0.097,$
0.097,$
0.097,$
0.098,$
0.098,$
0.098,$
0.099,$
0.099,$
0.099,$
0.099,$
0.099,$
0.099,$
0.099,$
0.099,$
0.099,$
0.099,$
0.100,$
0.099,$
0.099,$
0.099,$
0.099,$
0.099,$
0.096,$
0.092,$
0.093,$
0.094,$
0.092,$
0.091]*1.0E3

hgt=[10000.2, $
 9987.8, $
 9969.7, $
 9942.0, $
 9933.6, $
 9833.2, $
 9846.1, $
 9841.2, $
 9733.5, $
 9691.2, $
 9596.9, $
 9554.2, $
 9488.0, $
 9435.5, $
 9381.0, $
 9337.6, $
 9301.1, $
 9257.4, $
 9209.6, $
 9165.8, $
 9131.9, $
 9116.1, $
 9116.0, $
 9118.5, $
 9105.1, $
 9086.7, $
 9040.5, $
 8999.1, $
 8902.4, $
 8794.3, $
 8715.0, $
 8722.3, $
 8782.0, $
 8813.3, $
 8779.0, $
 8732.3, $
 8712.2, $
 8720.1, $
 8698.8, $
 8638.4, $
 8493.8, $
 8381.4, $
 8315.2, $
 8280.9, $
 8201.2, $
 8158.9, $
 8125.7, $
 8077.8, $
 8015.5, $
 8000.0, $
 7953.2, $
 7915.5, $
 7928.8, $
 7843.2, $
 7776.2, $
 7709.5, $
 7672.2, $
 7572.8, $
 7468.9, $
 7287.9, $
 7290.9, $
 7512.4, $
 7666.8, $
 7615.7, $
 7393.7, $
 7268.0]*1.0E-3

s=0
;dt=300
dt =3600
date = MAKE_DATE(2012,5,30,22,20)
o3_line = [ ]
FOR xy = 0, 52 DO BEGIN
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

;FOR s = 0, 52 DO BEGIN
FOR s = 12, 72 DO BEGIN
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
    
    outdir  = !WRF_DIRECTORY + run + '/' + experiment + '/plots/rip_wrf_traj1_paperfig_REVISION/'
    
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
	    YRANGE   = [75.0, 105.0], $
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
