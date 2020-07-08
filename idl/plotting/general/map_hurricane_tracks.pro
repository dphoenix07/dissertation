PRO MAP_HURRICANE_TRACKS, start_date, end_date, $
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
;		start_date : user specified start date to plot in yyyymmdd format (e.g., '20170101')
;		end_date   : user specified end date to plot in yyyymmdd format (e.g., '20180101')
;KEYWORDS:
;     PLOT      : If set, plot sample maps.
;     DIRECTORY : Output directory for wind file.
;	  CLOBBER   : If set, overwrite existing file. This is the default.
;OUTPUT:
;     Netcdf file.
;MODIFICATION HISTORY:
;		D. Phoenix:       2019-02-08.
;-

COMPILE_OPT IDL2																									;Set compile options

IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = 20170101
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = start_date+10000

indir = !WRF_DIRECTORY + 'general/'
infile = indir + 'text_wrangle_hurr_chrono.txt'

hurr_data = READ_ASCII(infile)
ts_data = READ_HURDAT2()

imonth = WHERE(((ts_data.date.year GE '1980') AND (ts_data.date.year LT '2017')) AND $
	((ts_data.date.month GE 8) AND (ts_data.date.month LT 9)))

extr_storms = WHERE(((ts_data.class[imonth] EQ 'HU')), istorms); OR (ts_data.class[imonth] EQ 'TS') OR $
   ; (ts_data.class[imonth] EQ 'HU')), istorms)
iperiod = imonth[extr_storms]

lat   = ts_data.y[iperiod]
lon   = ts_data.x[iperiod]
press = ts_data.p[iperiod]

;inan  = WHERE(FLOAT(press) LT -1)
;REMOVE, inan, press

;dates  =  hurr_data.field01[1,*]  
;period = where((dates GE start_date) AND (dates LT end_date)) 
;
;lat   = hurr_data.field01[5,*] 
;lon   = ((-1)*hurr_data.field01[6,*]) 
;press = hurr_data.field01[8,*] 
;
;inan  = WHERE(press LT -1)
;press[inan] = !Values.F_NaN
;
;period_0 = min(period)
;period_1 = max(period)  
;
;lon   = lon[period_0:period_1] 
;lat   = lat[period_0:period_1] 
;press = press[period_0:period_1]

table  = REVERSE(HCL_COLOR_TABLE(145, HUE_RANGE = [100.0, 300.0]))
levels = 880.0+FINDGEN(145)

y0 = 5.0
y1 = 65.0
x0 = -110.0
x1 = -20.0

yc = 40.0
xc = -80.0

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
		WINDOW, XSIZE = 1200, YSIZE = 800										;Open graphics window
		!P.COLOR      = COLOR_24('black')																		;Foreground color
		!P.BACKGROUND = COLOR_24('white')																		;Background color
		!P.CHARSIZE   = 2.0		
		!P.FONT       = -1																							;Use Hershey fonts
	ENDELSE
ENDELSE

MAP_SET, yc, xc, 0, CONIC = 1, $																;Draw map
	LIMIT     = [y0,x0,y1,x1], $
	ISOTROPIC = 1, $
	;TITLE     = 'Tropical Storm Tracks from: ' + STRING(start_date) + ' to ' + STRING(end_date), $
	TITLE     = 'Tropical Depression Tracks: Aug 1980-2017', $
	POSITION  = map_pos

;PLOTS, ((-1)*hurr_data.field01[6,*]),hurr_data.field01[5,*], $
;	PSYM   = 0

FOR ii=0,N_ELEMENTS(lon)-1 DO BEGIN
	icolor = WHERE(levels EQ press[ii])
	PLOTS, lon[ii], lat[ii], $
		PSYM 	 = 0, $
		NOCLIP   = 0, $
		CONTINUE = 1, $
		THICK	 = 3, $
		COLOR 	 = table[icolor]
ENDFOR
	
MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines

COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
	RANGE = [880, 1024], $
	TICKS = 5, $
	TITLE = 'Pressure (hPa)', $
	POSIT = bar_pos


STOP

END
