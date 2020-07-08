PRO MAP_OZONE_SITES, $
	PNG		  = png, $
	EPS		  = eps, $
	CLOBBER   = clobber

;+
;NAME:
;     MAP_OZONE_SITES
;PURPOSE:
;     This copies variables from ERA-Interim analysis into a single
;     file in pressure coordinates for use in TRAJ3D.
;     W at the surface is set to zero.  One pressure level is added at 
;     the top of the domain (p = 0), where w is also set to zero.
;CATEGORY:
;     Data handling utility.
;CALLING SEQUENCE:
;     MAP_OZONE_SITES, date0, outfile
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
;						  2019-02-11. Updated to include ERA-Interim Reanalysis data
;									  of 950 hPa geopotential heights and markers of 
;									  the O3 Concentration at Dallas
;-

COMPILE_OPT IDL2																									;Set compile options

outdir = '/data3/dphoenix/wrf/general/'
FILE_MKDIR, outdir
epsfile = outdir + 'o3_sites.eps' 

dallas_x = (360.0-96.78)
dallas_y = 32.78

x = FINDGEN(360)
y = FINDGEN(181)-90.0

yc = 32.85
xc = 360.0-96.86

x0 = 360.0-97.5
y0 = 32.3
x1 = 360.0-96.0
y1 = 33.3

ix0 = WHERE(ROUND(x0) EQ x)
ix1 = WHERE(ROUND(x1) EQ x) 
iy0 = WHERE(ROUND(y0) EQ y)
iy1 = WHERE(ROUND(y1) EQ y) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Start Site locations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

s44_x = (360.0-96.863889)
s44_y = 32.825833

s45_x = (360.0-96.808056)
s45_y = 32.919722

s50_x = (360.0-96.797686)
s50_y = 32.774262

s52_x = (360.0-96.891114)
s52_y = 32.719576

s53_x = (360.0-96.797222)
s53_y = 32.783333

s55_x = (360.0-96.756944)
s55_y = 32.616389

s57_x = (360.0-96.873056)
s57_y = 32.778889

s69_x = (360.0-96.860117)
s69_y = 32.820061

s86_x = (360.0-96.762221)
s86_y = 32.986222

s87_x = (360.0-96.87206)
s87_y = 32.676451

s75_x = (360.0-96.808498)
s75_y = 32.919206

s1006_x = (360.0-96.669167)
s1006_y = 32.910556

s1047_x = (360.0-96.553046)
s1047_y = 32.773739

s3003_x = (360.0-96.546299)
s3003_y = 32.769379

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End Site locations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

map_pos = [0.05, 0.15, 0.95, 0.95]																			;Set map position
bar_pos = [0.25, 0.10, 0.75, 0.12]																			;Set color bar position
    
table   = [COLOR_24(200, 200, 200),VISUALIZE_88D_COLOR(0)]												;Set reflectivity color table
levels = FINDGEN(N_ELEMENTS(table))
;levels = levels*30 + 450
levels = levels*30 + 2250

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
	TITLE     = 'Ozone Sites', $
	POSITION  = map_pos

nxy = 60
xreg = MAKEN(MIN(x[ix0:ix1]), MAX(x[ix0:ix1]), nxy)
yreg = MAKEN(MIN(y[iy0:iy1]), MAX(y[iy0:iy1]), nxy)

USERSYM_STAR, /FILL																					;Load plane symbol at flight path orientation

PLOTS, s44_x, s44_y, $																		;Overplot plane symbol
	PSYM     = 8, $
	SYMSIZE  = 6 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
	NOCLIP   = 0, $
	CONTINUE = 1, $
	COLOR    = COLOR_24('red')

PLOTS, s45_x, s45_y, $																		;Overplot plane symbol
	PSYM     = 8, $
	SYMSIZE  = 6 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
	NOCLIP   = 0, $
	CONTINUE = 1, $
	COLOR    = COLOR_24('cyan')

PLOTS, s52_x, s52_y, $																		;Overplot plane symbol
	PSYM     = 8, $
	SYMSIZE  = 6 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
	NOCLIP   = 0, $
	CONTINUE = 1, $
	COLOR    = COLOR_24('darkgreen')

PLOTS, s1047_x, s1047_y, $																		;Overplot plane symbol
	PSYM     = 8, $
	SYMSIZE  = 6 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
	NOCLIP   = 0, $
	CONTINUE = 1, $
	COLOR    = COLOR_24('blue')

PLOTS, s55_x, s55_y, $																		;Overplot plane symbol
	PSYM     = 8, $
	SYMSIZE  = 6 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
	NOCLIP   = 0, $
	CONTINUE = 1, $
	COLOR    = COLOR_24('yellow')

PLOTS, s69_x, s69_y, $																		;Overplot plane symbol
	PSYM     = 8, $
	SYMSIZE  = 6 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
	NOCLIP   = 0, $
	CONTINUE = 1, $
	COLOR    = COLOR_24('orange')

PLOTS, s86_x, s86_y, $																		;Overplot plane symbol
	PSYM     = 8, $
	SYMSIZE  = 6 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
	NOCLIP   = 0, $
	CONTINUE = 1, $
	COLOR    = COLOR_24('green')

PLOTS, s87_x, s87_y, $																		;Overplot plane symbol
	PSYM     = 8, $
	SYMSIZE  = 6 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
	NOCLIP   = 0, $
	CONTINUE = 1, $
	COLOR    = COLOR_24('brown')

PLOTS, s75_x, s75_y, $																		;Overplot plane symbol
	PSYM     = 8, $
	SYMSIZE  = 6 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
	NOCLIP   = 0, $
	CONTINUE = 1, $
	COLOR    = COLOR_24('purple')

PLOTS, s3003_x, s3003_y, $																		;Overplot plane symbol
	PSYM     = 8, $
	SYMSIZE  = 6 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
	NOCLIP   = 0, $
	CONTINUE = 1, $
	COLOR    = COLOR_24('magenta')

PLOTS, dallas_x, dallas_y, $																		;Overplot plane symbol
	PSYM     = 8, $
	SYMSIZE  = 6 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
	NOCLIP   = 0, $
	CONTINUE = 1, $
	COLOR    = COLOR_24('black')

USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation
PLOTS, s50_x, s50_y, $																		;Overplot plane symbol
	PSYM     = 8, $
	SYMSIZE  = 6 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
	NOCLIP   = 0, $
	CONTINUE = 1, $
	COLOR    = COLOR_24('blue')

PLOTS, s53_x, s53_y, $																		;Overplot plane symbol
	PSYM     = 8, $
	SYMSIZE  = 6 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
	NOCLIP   = 0, $
	CONTINUE = 1, $
	COLOR    = COLOR_24('red')

PLOTS, s57_x, s57_y, $																		;Overplot plane symbol
	PSYM     = 8, $
	SYMSIZE  = 6 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
	NOCLIP   = 0, $
	CONTINUE = 1, $
	COLOR    = COLOR_24('darkgreen')

PLOTS, s1006_x, s1006_y, $																		;Overplot plane symbol
	PSYM     = 8, $
	SYMSIZE  = 6 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
	NOCLIP   = 0, $
	CONTINUE = 1, $
	COLOR    = COLOR_24('magenta')


MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines
MAP_COUNTIES

XYOUTS, 1075, 770, 'Dallas', COLOR=COLOR_24('black'),/DEVICE
XYOUTS, 1075, 750, 'Site44', COLOR=COLOR_24('red'),/DEVICE
XYOUTS, 1075, 730, 'Site45', COLOR=COLOR_24('cyan'),/DEVICE
XYOUTS, 1075, 710, 'Site52', COLOR=COLOR_24('darkgreen'),/DEVICE
XYOUTS, 1075, 690, 'Site55', COLOR=COLOR_24('yellow'),/DEVICE
XYOUTS, 1075, 670, 'Site69', COLOR=COLOR_24('orange'),/DEVICE
XYOUTS, 1075, 650, 'Site86', COLOR=COLOR_24('green'),/DEVICE
XYOUTS, 1075, 630, 'Site87', COLOR=COLOR_24('brown'),/DEVICE
XYOUTS, 1075, 610, 'Site75', COLOR=COLOR_24('purple'),/DEVICE
XYOUTS, 1075, 590, 'Site3003', COLOR=COLOR_24('magenta'),/DEVICE
XYOUTS, 1075, 570, 'Site1047', COLOR=COLOR_24('blue'),/DEVICE
XYOUTS, 1075, 550, 'o Site50', COLOR=COLOR_24('blue'),/DEVICE
XYOUTS, 1075, 530, 'o Site53', COLOR=COLOR_24('red'),/DEVICE
XYOUTS, 1075, 510, 'o Site57', COLOR=COLOR_24('darkgreen'),/DEVICE
XYOUTS, 1075, 490, 'o Site1006', COLOR=COLOR_24('magenta'),/DEVICE

IF ~KEYWORD_SET(nowindow) THEN BEGIN
	IF KEYWORD_SET(eps) THEN BEGIN
		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
			LOAD_BASIC_COLORS, /RESET															;Reset color table to linear ramp
		PS_OFF																					;Turn PS off
	ENDIF ELSE IF KEYWORD_SET(png) THEN $
		WRITE_PNG, pngfile, TVRD(TRUE=1)														;Write PNG image
ENDIF

STOP
END
