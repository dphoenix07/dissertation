PRO PLOT_RIP_WRF, run, state, $
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

;Plots trajectory 3
lat = [39.48, $
39.42, $
39.37, $
39.32, $
39.26, $
39.21, $
39.16, $
39.11, $
39.05, $
39.00, $
38.94, $
38.89, $
38.83, $
38.77, $
38.71, $
38.65, $
38.59, $
38.53, $
38.46, $
38.40, $
38.33, $
38.26, $
38.20, $
38.13, $
38.06, $
37.99, $
37.92, $
37.85, $
37.78, $
37.71, $
37.64, $
37.57, $
37.50, $
37.43, $
37.36, $
37.29, $
37.22, $
37.15, $
37.08, $
37.02, $
36.95, $
36.88, $
36.82, $
36.76, $
36.70, $
36.65, $
36.59, $
36.54, $
36.49, $
36.45, $
36.40, $
36.35, $
36.31]

lon = [-100.48, $
-100.42, $
-100.37, $
-100.31, $
-100.26, $
-100.20, $
-100.15, $
-100.10, $
-100.04, $
-99.98, $
-99.93, $
-99.87, $
-99.81, $
-99.75, $
-99.68, $
-99.62, $
-99.56, $
-99.50, $
-99.44, $
-99.38, $
-99.32, $
-99.26, $
-99.20, $
-99.14, $
-99.09, $
-99.03, $
-98.98, $
-98.92, $
-98.87, $
-98.82, $
-98.77, $
-98.73, $
-98.68, $
-98.63, $
-98.59, $
-98.55, $
-98.51, $
-98.47, $
-98.43, $
-98.39, $
-98.36, $
-98.33, $
-98.29, $
-98.26, $
-98.23, $
-98.21, $
-98.18, $
-98.16, $
-98.13, $
-98.11, $
-98.09, $
-98.06, $
-98.04]

o3 = [0.151, $
0.151,$
0.151,$
0.152,$
0.151,$
0.154,$
0.152,$
0.154,$
0.152,$
0.151,$
0.151,$
0.151,$
0.151,$
0.151,$
0.152,$
0.151,$
0.150,$
0.149,$
0.149,$
0.149,$
0.148,$
0.148,$
0.147,$
0.146,$
0.145,$
0.144,$
0.144,$
0.145,$
0.145,$
0.145,$
0.144,$
0.144,$
0.144,$
0.144,$
0.144,$
0.145,$
0.146,$
0.146,$
0.146,$
0.146,$
0.147,$
0.146,$
0.146,$
0.145,$
0.143,$
0.141,$
0.140,$
0.141,$
0.140,$
0.138,$
0.136,$
0.135,$
0.135]*1.0E3

hgt=[11000.6, 10986.7, 10971.2, 10947.8, 10946.0, 10909.3, 10904.0, $
 10903.4, 10830.2, 10897.6, 10785.8, 10835.4, 10858.0, 10832.8, 10800.2, 10781.5, 10765.8, 10731.5, $
 10693.8, 10651.2, 10608.5, 10578.3, 10557.8, 10529.4, 10506.5, 10490.9, 10462.3, 10417.6, 10364.5, $
 10317.5, 10253.6, 10170.2, 10080.6, 10000.3, 9928.7, 9869.3, 9837.3, 9814.8, 9795.6, 9762.9, 9703.3, $
  9628.7, 9559.3, 9490.9, 9421.9, 9368.5, 9285.8, 9173.7, 9053.7, 8943.9, 8894.6, 8866.4, 8829.8]*1.0E-3

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
    
    outdir  = !WRF_DIRECTORY + run + '/' + experiment + '/plots/rip_wrf_traj3_paperfig/'
    
    epsfile = outdir + experiment + '_' + date_string + '.eps'						        ;EPS filename
    pdffile = outdir + experiment + '_' + date_string + '.pdf'						        ;PDF filename
    pngfile = outdir + experiment + '_' + date_string + '.png'						        ;PNG filename
    
    FILE_MKDIR, outdir																								;Create output directory, if necessary
 	
 	!P.MULTI = [0, 2, 1]																								;Set multiple plots
   
    map_pos = [0.05, 0.15, 0.95, 0.95]																			;Set map position
    bar_pos = [0.25, 0.10, 0.75, 0.12]																			;Set color bar position
    tsr_pos = [0.60, 0.55, 0.85, 0.85]																	;Set cross-section position
    
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

	;table = HCL_COLOR_TABLE(25, HUE_RANGE = [100.0, 300.0])
    ;o3levels = FINDGEN(25)+130.0
    ;io3 = WHERE(o3levels EQ o3[s])

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
 
    PLOT, hgt, $
    	TITLE    = 'Trajectory Height', $
   		XRANGE   = [0,52], $
   		YRANGE   = [6, 13], $
   		XTITLE   = 'Time', $
   		YTITLE   = 'Altitude (km)', $
   		BACKGROUND = 255, $
   		POSITION = tsr_pos
   
    XYOUTS, s, hgt[s], 'o', /DATA
   
    MAP_SET, yc, xc, 0, CONIC = 1, $																				;Redraw map boundaries
    	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
    	ISOTROPIC = 1, $
    	NOERASE   = 1, $
    	POSITION  = map_pos
    
    COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
    	;RANGE = [130,155], $
    	RANGE = [90, 250], $
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
