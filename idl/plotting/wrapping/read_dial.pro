PRO READ_DIAL,  $
	PNG		   = png, $
	EPS		   = eps, $
	CLOBBER    = clobber

;+
;NAME:
;     READ_DIAL
;PURPOSE:
;     Compare the number of times the daily 8-hr mean o3 concentration exceeds
;		different thresholds (55, 65, 75, 80, 90, 100 ppb) 
;CATEGORY:
;     Data handling utility.
;CALLING SEQUENCE:
;     READ_DIAL, date0, outfile
;INPUT:
;		start_date : user specified start date to plot in yyyymmdd format (e.g., '20170101')
;		end_date   : user specified end date to plot in yyyymmdd format (e.g., '20180101')
;KEYWORDS:
;     PLOT      : If set, plot sample maps.
;     DIRECTORY : Output directory for wind file.
;	  CLOBBER   : If set, overwrite existing file. This is the default.
;OUTPUT:
;     Netcdf file.
;MODIFICATION HISTORY:
;		D. Phoenix:       2019-02-10.	PDF of O3 for TS vs Non-TS: July1-Dec1
;							2019-02-13. ***The PDFs of Gulf/Atl/TD/TS/HU only include
;										the excluded storms in the "No Storms" population.
;										Make changes to do "Storm Type vs No Storms"
;										Also make script (maybe box plot script) to compare
;										different storms and gulf vs atlantic storms.
;-

COMPILE_OPT IDL2																									;Set compile options

date = MAKE_DATE(2012,5,31,2)
run = '20120530_ncar'
experiment = 'd03_30km_icloud'
domain = 1

infile = !WRF_DIRECTORY + '20120530_ncar/dial/dial_data.h5'																;Set input file path
;data = H5_BROWSER(infile)
o3_dial = H5_GETDATA(infile, '/Data_Products/O3_prfl')
z_dial  = H5_GETDATA(infile, '/Nav_Data/gps_alt')
x_dial  = H5_GETDATA(infile, '/Nav_Data/gps_lon')
y_dial  = H5_GETDATA(infile, '/Nav_Data/gps_lat')
alt_dial= H5_GETDATA(infile, '/Nav_Data/Altitudes')

x     = (WRF_READ_VAR('Longitude', date, run, experiment, DOMAIN = domain, INDICES = region)).values		;Read variables
y     = (WRF_READ_VAR('Latitude' , date, run, experiment, DOMAIN = domain, INDICES = region)).values
z     = (WRF_READ_VAR('Z' 	     , date, run, experiment, DOMAIN = domain, INDICES = region)).values
ztrop = (WRF_READ_VAR('Z_trop'   , date, run, experiment, DOMAIN = domain, INDICES = region)).values
IF ((experiment EQ 'd03_30km') OR (experiment EQ 'd03_30km_icloud')) THEN $
	o = (WRF_READ_VAR('O3_tracer', date, run, experiment, DOMAIN = domain, INDICES = region)).values * 1.0E3
IF (experiment EQ 'd02_30km') THEN $
	o = (WRF_READ_VAR('O3', date, run, experiment, DOMAIN = domain, INDICES = region)).values * 1.0E3

dim = SIZE(z, /DIMENSION)
xx = REBIN(x, dim[0],dim[1],dim[2],/SAMPLE)
yy = REBIN(y, dim[0],dim[1],dim[2],/SAMPLE)

o3_line = []
FOR xy = 0, N_ELEMENTS(x_dial)-1, 100 DO BEGIN
    PRINT, x_dial[xy], y_dial[xy], z_dial[xy]
    PRINT, xy
    
    i0 = WHERE((ABS(xx - (x_dial[xy])) LT 0.01) AND (ABS(yy - y_dial[xy]) LT 0.01) AND (ABS(z-z_dial[xy]) LT 100.0))
    IF (i0[0] EQ -1) THEN i1 = WHERE((ABS(xx - (x_dial[xy])) LT 0.1) AND (ABS(yy - y_dial[xy]) LT 0.1) AND (ABS(z-z_dial[xy]) LT 200.0))
    IF (i0[0] EQ -1) THEN i1 = WHERE((ABS(xx - (x_dial[xy])) LT 0.5) AND (ABS(yy - y_dial[xy]) LT 0.5) AND (ABS(z-z_dial[xy]) LT 300.0))
    
    ;Of all locations found, find closest to specified location
    store_arr = [ ]
    HELP, i0
    FOR xyz = 0, N_ELEMENTS(i0)-1 DO BEGIN
    	store = SQRT((xx[i0[xyz]] - x_dial[xy])^2 + (yy[i0[xyz]] - y_dial[xy])^2 + (z[i0[xyz]] - z_dial[xy])^2)
    	store_arr = [store_arr, store]
    ENDFOR
    
    ij_start = WHERE(MIN(store_arr,/NAN) EQ store_arr)
    ;ij_start = ARRAY_INDICES(x, i0[ij_start])

	o3_line = [o3_line, i0[ij_start]]
ENDFOR

map_pos = [0.05, 0.15, 0.95, 0.95]																			;Set map position
bar_pos = [0.25, 0.10, 0.75, 0.12]																			;Set color bar position

;outdir  = !WRF_DIRECTORY + run + '/' + 'paper/plots/ozone_maps_wind/' 
;epsfile = outdir + STRMID(STRING(kk),10) + '_' + date_string + '.eps'											;EPS filename
;pdffile = outdir + STRMID(STRING(kk),10) + '_' + date_string + '.pdf'											;PDF filename
;pngfile = outdir + STRMID(STRING(kk),10) + '_' + date_string + '.png'											;PNG filename

;FILE_MKDIR, outdir																								;Create output directory, if necessary



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
		PS_ON, FILENAME = epsfile, PAGE_SIZE = [4.0,3.0], MARGIN = 0.0, /INCHES;PAGE_SIZE = 0.001*dim*wfactor			;Switch to Postscript device
		DEVICE, /ENCAPSULATED
		!P.FONT     = 0																								;Hardware fonts
		!P.CHARSIZE = 0.75	
		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
			LOAD_BASIC_COLORS																							;Load basic color definitions
	ENDIF ELSE BEGIN
		SET_PLOT, 'X'
		WINDOW, XSIZE = 800, YSIZE = 600										;Open graphics window
		!P.COLOR      = COLOR_24('black')																		;Foreground color
		!P.BACKGROUND = COLOR_24('white')																		;Background color
		!P.CHARSIZE   = 2.0		
		!P.FONT       = -1																							;Use Hershey fonts
	ENDELSE
ENDELSE


dz = SIZE(alt_dial,/DIMENSIONS)
dt = SIZE(z_dial,/DIMENSIONS)

zz   = REBIN(z_dial,dt,dz[1],/SAMPLE)
alt2 = REBIN(alt_dial, dt, dz[1],/SAMPLE) 

fill_sect_table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])		
sect_bar_min     = 0.0	
sect_bar_max     = 200.0
fill_sect_levels = ((sect_bar_max-sect_bar_min)/20.0)*FINDGEN(20)
sect_bar_ticks   = 4
sect_bar_title   = 'Ozone Concentration (ppbv)'

CONTOUR, o3_dial[2150:2390,*], x_dial[2150:2390], alt_dial[0,*]*0.001, $
	CELL_FILL   = 1, $
	LEVELS      = fill_sect_levels, $
	C_COLOR     = fill_sect_table, $
	YRANGE      = [0, 20]
	 
	
OPLOT, [-94, -87], [11.5, 11.5]

STOP
COLOR_BAR_24_KPB, fill_sect_table, UNDER = fill_sect_table[0], OVER = fill_sect_table[-1], $				;Draw cross-section color bar
	TICKS  = sect_bar_ticks, $
	RANGE  = [sect_bar_min, sect_bar_max], $
	TITLE  = sect_bar_title, $
	POSIT  = xsn_bar_pos, $
	XTICKN = sect_tick_name


IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
	
	IF KEYWORD_SET(pdf) THEN PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS						;Convert to PDF
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file


STOP           
END
