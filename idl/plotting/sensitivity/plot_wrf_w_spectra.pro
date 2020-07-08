PRO PLOT_WRF_W_SPECTRA, date, date1, $
		W_TROP = w_trop, $
		PNG	   = png, $
		EPS    = eps

;+
; Name:
;		PLOT_WRF_W_SPECTRA
; Purpose:
;		This is a procedure to plot updraft speed spectra for resolution
;		testing experiments. 
; Calling sequence:
;		PLOT_WRF_W_SPECTRA, date
; Input:
;		date  : Analysis date {CDATE}.
;		date1 : Optional input variable to supply analysis end date {CDATE}. If given,
;					spectra are combined for all output files within the time window.
; Output:
;		A plot of column-maximum updraft speed spectra. 
; Keywords:
;		PNG : If set, write PNG image.
;		EPS : If set, output to PostScript.
; Author and history:
;		Cameron R. Homeyer  2014-08-01.
;		Daniel B. Phoenix	2016-02-23. 	Added option to plot w spectra at tropopause.
;-

COMPILE_OPT IDL2																				;Set compile options

;exper   = ['schooner_test', '15-3km', 'nssl']	  														;Array of experiments
exper   = ['nssl_ysu','nssl_cbmz_new','mozcart_new']
color   = [COLOR_24('red'), COLOR_24('gray50'), COLOR_24('blue')]									;Corresponding colors to use
lines   = [  2,  1,   0]																			;Line type
;indices = [70, 150, 180, 90]
indices = [70, 115, 180, 180] 
;i0    = [ 50, 75,   0]																			;User specified indices
;i1    = [203,535,1379]
;j0    = [ 52, 80,   0]
;j1    = [159,400, 959]
;k     = [ 20, 39,  74]																			

;epsfile = '~/data/WRF/res_tests/plots/wrfrt_w_spectra.eps'
;pngfile = '~/data/WRF/res_tests/plots/wrfrt_w_spectra.png'

IF KEYWORD_SET(eps) THEN BEGIN	
	PS_ON, FILENAME = epsfile, PAGE_SIZE = [8.0, 6.0], MARGIN = 0.0, /INCHES					;Switch to Postscript device
	DEVICE, /ENCAPSULATED
	!P.FONT     = 0																				;Hardware fonts
	!P.CHARSIZE = 1.25	
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS																		;Load basic color definitions
ENDIF ELSE BEGIN
	SET_PLOT, 'X'
	WINDOW, XSIZE = 800, YSIZE = 600															;Open graphics window
	!P.COLOR      = COLOR_24('black')															;Foreground color
	!P.BACKGROUND = COLOR_24('white')															;Background color
	!P.CHARSIZE   = 2.0		
	!P.FONT       = -1																			;Use Hershey fonts
ENDELSE

w0   =  3.0																						;Parameters for binning
w1   = 60.0
dw   =  1.0
nw   = LONG((w1-w0)/dw)
xbin = w0 + 0.5*dw + dw*FINDGEN(nw)

																		
	FOR a = 0, 2 DO BEGIN
		IF (N_ELEMENTS(date1) GT 0) THEN BEGIN													;If end date given
			dt = TIME_DIFF(date1,date)															;Compute dt in seconds
			nt = dt/1800																		;Convert dt to 30 minute 
			FOR s = 0, nt DO BEGIN																;For every 30 minutes 	
				w = (WRF_READ_VAR('w', TIME_INC(date,s*1800), '20120519', exper[a], $			;Read w for given indices
						DOMAIN = 2)).values
				z = (WRF_READ_VAR('Z', TIME_INC(date,s*1800), '20120519', exper[a], $
						DOMAIN = 2)).values
				trop = (WRF_READ_VAR('Z_trop', TIME_INC(date,s*1800), '20120519', exper[a], $
						DOMAIN = 2)).values
				cld  = (WRF_READ_VAR('CLOUD_MIX', TIME_INC(date,s*1800), '20120519', exper[a], $	;test
						DOMAIN = 2)).values * 1.0E3													;test
				IF KEYWORD_SET(w_trop) THEN BEGIN
					dim = SIZE(w, /DIMENSIONS)
					nx  = dim[0]
					ny  = dim[1]
					nz  = dim[2]
					i   = REBIN(FINDGEN(nx), nx, ny)
					j   = REBIN(REFORM(FINDGEN(ny), 1, ny), nx, ny)
					k   = REBIN(REFORM(FINDGEN(nz), 1, 1, nz), nx, ny, nz)
					k   = MAX((ABS(z - REBIN(trop,nx,ny,nz, /SAMPLE) LE 1.0))*k, DIM = 3)
					wmax  = INTERPOLATE(w, i, j, k)
					igood = WHERE((wmax GE w0), ngood)	
				ENDIF ELSE BEGIN
;					w_good = WHERE(cld GE 0.1, ngood, COMPLEMENT = w_bad)						;test
;					IF N_ELEMENTS(ngood GT 0) THEN BEGIN										;test
;						w [w_good] = w [w_good]													;test
;						w [w_bad ] = -999999													;test
;					ENDIF
					wmax = MAX(w, DIM = 3, /NAN)												;Compute max w in vertical dimension			
					igood = WHERE(((wmax GE w0) AND (wmax LE w1)), ngood)						;Only consider wmax values between 56 m/s and 3 m/s
				ENDELSE

				PRINT, exper[a], 100.0*FLOAT(ngood)/N_ELEMENTS(wmax)					    	;Print % of values in above range

				IF (s EQ 0) THEN bin =       LONG((wmax[igood] - w0)/dw) $						;Bin good values
								ELSE bin = [bin, LONG((wmax[igood] - w0)/dw)]
			ENDFOR
		ENDIF ELSE BEGIN																		;If no end date given, compute wmax for one time step
			w = (WRF_READ_VAR('w', TIME_INC(date,s*1800), '20120519', exper[a], $				;Read w for given indices
						DOMAIN = 2)).values
			z = (WRF_READ_VAR('Z', TIME_INC(date,s*1800), '20120519', exper[a], $
						DOMAIN = 2)).values
			trop = (WRF_READ_VAR('Z_trop', TIME_INC(date,s*1800), '20120519', exper[a], $
						DOMAIN = 2)).values
				IF KEYWORD_SET(w_trop) THEN BEGIN
					dim = SIZE(w, /DIMENSIONS)
					nx  = dim[0]
					ny  = dim[1]
					nz  = dim[2]
					i   = REBIN(FINDGEN(nx), nx, ny)
					j   = REBIN(REFORM(FINDGEN(ny), 1, ny), nx, ny)
					k   = REBIN(REFORM(FINDGEN(nz), 1, 1, nz), nx, ny, nz)
					k   = MAX((ABS(z - REBIN(trop,nx,ny,nz, /SAMPLE) LE 1.0))*k, DIM = 3)
					wmax  = INTERPOLATE(w, i, j, k)
					igood = WHERE((wmax GE w0), ngood)	
				ENDIF ELSE BEGIN				
					wmax = MAX(w, DIM = 3, /NAN)
		 			igood = WHERE(((wmax GE w0) AND (wmax LE w1)), ngood)
		 		ENDELSE
					bin   = LONG((wmax[igood] - w0)/dw)
		ENDELSE
		
		hist  = HISTOGRAM(bin, MIN = 0, MAX = nw-1, BINSIZE = 1)								;Create histogram of binned values
		
		IF KEYWORD_SET (w_trop) THEN BEGIN
			xrange = [5, 13]
		ENDIF ELSE BEGIN
			xrange = [5, 45]
		ENDELSE
												
		IF (a EQ 0) THEN BEGIN																	;Set up to plot first domain
			PLOT, bin, 100.0*FLOAT(hist)/TOTAL(hist), /NODATA, $
				XRANGE = xrange, $
				XTITLE = 'Column-maximum w (m/s)', $
				XSTYLE = 1, $
				YRANGE = [0.01,100.0], $
				YTITLE = 'Occurrence (%)', $
				YLOG   = 1, $
				YSTYLE = 1, $
				TITLE  = 'Profiles with w > 4.5 m/s'	

			OPLOT, xbin, 100.0*FLOAT(hist)/TOTAL(hist), LINES = lines[a], COLOR = color[a], THICK = 3	;Overplot for first domain
		ENDIF ELSE $
			OPLOT, xbin, 100.0*FLOAT(hist)/TOTAL(hist), LINES = lines[a], COLOR = color[a], THICK = 3	;For other two domains, overplot
	ENDFOR


XYOUTS, 0.7, 0.85, 'racm-esrl', COLOR = color[0], /NORMAL
XYOUTS, 0.7, 0.80, 'cbmz' 	  , COLOR = color[1], /NORMAL
XYOUTS, 0.7, 0.75, 'mozcart'  , COLOR = color[2], /NORMAL

IF KEYWORD_SET(eps) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file

END