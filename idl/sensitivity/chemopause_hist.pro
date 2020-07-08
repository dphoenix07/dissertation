PRO CHEMOPAUSE_HIST, hour, $
	DOM		  = dom, $
	EPS 	  = eps, $
	PNG 	  = png

;+
; Name:
;		CHEMOPAUSE_HIST
; Purpose:
;		This is a procedure to plot a frequency distribution of the altitude 
;		corresponding to the chemopause defined by O3 > 100 ppbv and CO < 50 ppbv
; Calling sequence:
;		CHEMOPAUSE_HIST, event, date
; Input:
;		event  : case date (e.g., '20120519'
;		scheme : case name (e.g., 'tracer' or 'nssl_ysu')
;		date   : Analysis date {CDATE}.
; Output:
;		A frequency distribution of altitudes corresponding to the chemopause for O3
;		and CO.
; Keywords:
;		EPS  : If set, output to PostScript.
;		PNG  : If set, write PNG image.
; Author and history:
;		D.B. Phoenix  2016-11-07.
;-

COMPILE_OPT IDL2																															;Set compile options

cmin   = 50.0																																	;Set altitude histogram parameters
cmax   = 150.0
dc     =  10.0
cbin = LONG((cmax-cmin)/dc)
czbin = cmin + 0.5*dc + dc*FINDGEN(cbin)

czbin = [0,40]

event  = ['20120519','20120529','20120530','20120601']
scheme = ['nssl_ysu','nssl'    ,'tracer'  ,'nssl'    ]
date   = [MAKE_DATE(2012,5,19,hour),MAKE_DATE(2012,5,29,hour),MAKE_DATE(2012,5,30,hour),MAKE_DATE(2012,6,1,hour)]


title  = event
epsfile = !WRF_DIRECTORY + event + '/histograms/wrf_chemopause.eps'
pngfile = !WRF_DIRECTORY + event + '/histograms/wrf_chemopause.png'

IF KEYWORD_SET(eps) THEN BEGIN	
	PS_ON, FILENAME = epsfile, PAGE_SIZE = [6.45, 8.25], MARGIN = 0.0, /INCHES											;Switch to Postscript device
	DEVICE, /ENCAPSULATED
	!P.FONT     = 0																														;Hardware fonts
	!P.CHARSIZE = 1.25	
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS																													;Load basic color definitions
ENDIF ELSE BEGIN
	SET_PLOT, 'X'
	WINDOW, XSIZE = 645, YSIZE = 825																								;Open graphics window
	!P.COLOR      = COLOR_24('black')																								;Foreground color
	!P.BACKGROUND = COLOR_24('white')																								;Background color
	!P.CHARSIZE   = 1.8		
	!P.FONT       = -1																													;Use Hershey fonts
ENDELSE
!P.MULTI = [0,2,4]

color = [COLOR_24('red'), COLOR_24('gray50'), COLOR_24('blue')]

o3_trop = 0.0
co_trop = 0.0
FOR i = 0, N_ELEMENTS(event)-1 DO BEGIN
	hist_o3 = LONARR(40, 3)
	hist_co = LONARR(20, 3)
		Zwrfecho_mean  = 0.0
		Zwrfecho_max   = 0.0
		Zwrfecho_count = 0		
		Zwrfcld_mean   = 0.0
		Zwrfcld_max    = 0.0
		Zwrfcld_count  = 0		

	o3    = (WRF_READ_VAR('O3', 	date[i], event[i], scheme[i], $
				 DOMAIN = dom)).values * 1.0E3
	co    = (WRF_READ_VAR('CO', 	date[i], event[i], scheme[i], $
				 DOMAIN = dom)).values * 1.0E3
	Z     = (WRF_READ_VAR('Z',  	date[i], event[i], scheme[i], $
				 DOMAIN = dom)).values * 1.0E-3
	Ztrop = (WRF_READ_VAR('Z_trop', date[i], event[i], scheme[i], $
				 DOMAIN = dom)).values * 1.0E-3
	
	dim = SIZE(Z,/DIMENSION)
	Ztrop = REBIN(Ztrop, dim[0],dim[1],dim[2],/SAMPLE)																											;Convert WRF geopotential heights to km

	itrop = WHERE(ABS(Ztrop - Z) LT 1.0, count)
	IF (count GT 0) THEN BEGIN
		o3_trop = o3[itrop]
		co_trop = co[itrop]
	ENDIF 

	hist_o3 = HISTOGRAM(o3_trop, MIN = 75.0, NBIN = 40, BINSIZE = 5.0)
	hist_co = HISTOGRAM(co_trop, MIN = 25.0, NBIN = 20, BINSIZE = 5.0)

	freq_o3 = FLOAT(hist_o3/TOTAL(hist_o3))*100.0
	freq_co = FLOAT(hist_co/TOTAL(hist_co))*100.0
	
	PRINT, STRING(event[i])
	
	io3 = WHERE(MAX(freq_o3) EQ freq_o3)
	PRINT, "Most frequent O3 concentration at tropopause =" 
	PRINT, (75.0 + io3*5.0)
	PRINT, "Max"
	PRINT, MAX(o3_trop)
	PRINT, "Min"
	PRINT, MIN(o3_trop)
	
	
	ico = WHERE(MAX(freq_co) EQ freq_co)
	PRINT, "Most frequent CO concentration at tropopause =" 
	PRINT, (25.0 + ico*5.0)
	PRINT, "Max"
	PRINT, MAX(co_trop)
	PRINT, "Min"
	PRINT, MIN(co_trop)
	
	PLOT, freq_o3, /NODATA, $
		XRANGE = [0,40], $
		XSTYLE = 1, $
		XTITLE = 'O3 Concentration', $
		XTICKS = 5, $
		XTICKNAME = [100, 150, 200, 250, 300], $
		YRANGE = [0, 10], $
		YSTYLE = 1, $
		YTITLE = 'Frequency (%)', $
		TITLE  = 'O3 ' + title[i]


	OPLOT, freq_o3
;	OPLOT, czbin, 100.0*(FLOAT(hist_o3)/TOTAL(hist_o3)), THICK = 4, PSYM = 10

	PLOT, freq_co, /NODATA, $
;		XRANGE = [0, 20], $
		XSTYLE = 1, $
		XTITLE = 'CO Concentration (km)', $
		XTICKS = 5, $
		XTICKNAME = [35, 55, 75, 95, 115], $
		YRANGE = [0, 35], $
		YSTYLE = 1, $
		YTITLE = 'Frequency (%)', $
		TITLE  = 'CO ' + title[i]

	OPLOT, freq_co
;	OPLOT, czbin, 100.0*(FLOAT(hist_co[*,0])/TOTAL(hist_co[*,0])), THICK = 4, PSYM = 10

;	IF (i EQ 0) THEN BEGIN
;		XYOUTS, 7.0, 75.0, 'NEXRAD Echo Top', /DATA
;		XYOUTS, 7.0, 67.0, 'dx = 2 km',     COLOR = color[0], /DATA
;	ENDIF
ENDFOR


!P.MULTI = 0

IF (N_ELEMENTS(pngfile) NE 0) THEN print, pngfile

IF KEYWORD_SET(eps) THEN BEGIN
    IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
	      LOAD_BASIC_COLORS, /RESET                                           												;Reset color table to linear ramp
   	   PS_OFF                                                                											;Turn PS off
ENDIF ELSE IF KEYWORD_SET(png) THEN $
         WRITE_PNG, pngfile, TVRD(TRUE = 1)                                    											;Write PNG file


END
