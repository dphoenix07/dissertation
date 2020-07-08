PRO DC3_PLOT_GV_TIMESERIES, flight_name, $
	DATE_RANGE = date_range, $
	EPS        = eps, $
	PNG        = png

;+
; Name:
;		DC3_PLOT_GV_TIMESERIES
; Purpose:
;		This is a procedure to plot a timeseries of DC3 rf02 measurements. 
; Calling sequence:
;		DC3_PLOT_GV_TIMESERIES
; Input:
;		flight_name : Desired flight name to plot measurments for.
; Output:
;		A timeseries of DC3 GV measurements.
; Keywords:
;		DATE_RANGE : A 2-element {CDATE} array containing timeseries range for plotting.
;		EPS        : If set, output to PostScript.
;		PNG        : If set, write PNG image.
; Author and history:
;		Cameron R. Homeyer  2012-08-29.
;-

COMPILE_OPT IDL2																									;Set compile options

IF (N_ELEMENTS(flight_name) EQ 0) THEN flight_name = '20120519'									;Set default flight name

IF (N_ELEMENTS(date_range ) EQ 0) THEN $
	date_range = [MAKE_DATE(2012, 5, 20), MAKE_DATE(2012, 5, 20, 0, 25)]							;Set default date (time) range

z    = DC3_READ_VAR('GGALT',           flight_name)													;Read aircraft measurements
T    = DC3_READ_VAR('ATX',             flight_name)
o3   = DC3_READ_VAR('FO3_ACD',         flight_name)
co   = DC3_READ_VAR('CO',              flight_name)
hno3 = DC3_READ_VAR('HNO3',            flight_name)
co2  = DC3_READ_VAR('CO2',             flight_name)
nox  = DC3_READ_VAR('NO+NO2',          flight_name)
ch4  = DC3_READ_VAR('Methane',         flight_name)
h2o  = DC3_READ_VAR('X_H2O',           flight_name)
part = DC3_READ_VAR('CONC1DC_LWIO',    flight_name)
hcl  = DC3_READ_VAR('HCl',             flight_name)
form = DC3_READ_VAR('CH2O_CAMS_pptv',  flight_name)
nbut = DC3_READ_VAR('n_Butane',        flight_name)
tol  = DC3_READ_VAR('Toluene',         flight_name)
benz = DC3_READ_VAR('Benzene',         flight_name)
ipen = DC3_READ_VAR('i_Pentane',       flight_name)
date = DC3_READ_VAR('Time',            flight_name)

tdiff0 = TIME_DIFF(date.values, date_range[0])
tdiff1 = TIME_DIFF(date.values, date_range[1])
it     = WHERE((tdiff0 GE 0) AND (tdiff1 LE 0), nt)													;Get indices of timeseries

epsfile = '~/dc3_' + flight_name + '_gv_timeseries.eps'													;EPS filename
pngfile = '~/dc3_' + flight_name + '_gv_timeseries.png'													;PNG filename

IF KEYWORD_SET(eps) THEN BEGIN	
	PS_ON, FILENAME = epsfile, PAGE_SIZE = [10.0, 10.0], MARGIN = 0.0, /INCHES					;Switch to Postscript device
	DEVICE, /ENCAPSULATED
	!P.FONT     = 0																								;Hardware fonts
	!P.CHARSIZE = 1.75	
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS																							;Load basic color definitions
ENDIF ELSE BEGIN
	SET_PLOT, 'X'
	WINDOW, XSIZE = 1000, YSIZE = 1000																			;Open graphics window
	!P.COLOR      = COLOR_24('black')																		;Foreground color
	!P.BACKGROUND = COLOR_24('white')																		;Background color
	!P.CHARSIZE   = 2.5		
	!P.FONT       = -1																							;Use Hershey fonts
ENDELSE

!P.MULTI = [0, 1, 8]

x  = FINDGEN(nt)																									;Create reference array
ix = WHERE(((date.values.second)[it] EQ 0) AND $
			 (((date.values.minute)[it] MOD 5) EQ 0), nticks)

xticks = STRING((date.values.hour  )[it[ix]], FORMAT="(I2.2)") + ':' + $						;Set x-axis tick names
			STRING((date.values.minute)[it[ix]], FORMAT="(I2.2)")

x0 = 0.1
x1 = 0.9

y8 = 0.9500
y7 = 0.8375
y6 = 0.7250
y5 = 0.6125
y4 = 0.5000
y3 = 0.3875
y2 = 0.2750
y1 = 0.1625
y0 = 0.0500

color0   = COLOR_24('black')
color1   = COLOR_24('red'  )

Zrange    = [  10,   15]																						;Set tracer axis ranges
Trange    = [ -60,  -40]
o3range   = [  50,  600]
corange   = [  25,  150]
hno3range = [   0, 4000]
co2range  = [ 390,  400]
h2orange  = [   5,  250]
partrange = [   0.1,  100]
ch4range  = [1650, 1950]
noxrange  = [ 250, 1500]
hclrange  = [   0,  200]
formrange = [   0, 1200]
nbutrange = [   0,  200]
tolrange  = [   0,   25]
benzrange = [   0,   40]
ipenrange = [   0,  200]

Ztitle    = 'Alt (km)'																							;Set tracer axis titles
Ttitle    = 'Temp (C)'
o3title   = 'O3 (ppbv)'
cotitle   = 'CO (ppbv)'
hno3title = 'HNO3 (pptv)'
co2title  = 'CO2 (ppmv)'
h2otitle  = 'H2O (ppmv)'
parttitle = 'Cloud (#/L)'
ch4title  = 'CH4 (ppbv)'
noxtitle  = 'NOx (pptv)'
hcltitle  = 'HCl (pptv)'
formtitle = 'CH2O (pptv)'
nbuttitle = 'n-Butane (pptv)'
toltitle  = 'Toluene (pptv)'
benztitle = 'Benzene (pptv)'
ipentitle = 'i-Pentane (pptv)'

PLOT, x, x, /NODATA, $																							;Set up plot
	THICK    = 2, $
	XRANGE   = [0, nt-1], $
	XTICKS   = nticks -1, $
	XMINOR   = 5, $
	XTICKN   = REPLICATE(' ', nticks), $
	XSTYLE   = 1, $
	YRANGE   = [0, 1], $
	YTICKS   = 1, $
	YTICKN   = [' ', ' '], $
	YSTYLE   = 1, $
	POSITION = [x0, y7, x1, y8], $
	TITLE    = 'DC3 ' + flight_name + ' GV Measurements'

AXIS, YAXIS = 0, $																								;Draw altitude axis
	SAVE   = 1, $
	YRANGE = Zrange, $
	YTITLE = Ztitle, $
	YTICKS = 1, $
	YSTYLE = 1, $
	COLOR  = color0

OPLOT, x, 0.001*(Z.values)[it], COLOR = color0, THICK = 2													;Plot temperature measurments

AXIS, YAXIS = 1, $																								;Draw temperature axis
	SAVE   = 1, $
	YRANGE = Trange, $
	YTITLE = Ttitle, $
	YTICKS = 1, $
	YSTYLE = 1, $
	COLOR  = color1

OPLOT, x, (T.values)[it], COLOR = color1, THICK = 2													;Plot temperature measurments

PLOT, x, x, /NODATA, $																							;Set up plot
	THICK    = 2, $
	XRANGE   = [0, nt-1], $
	XTICKS   = nticks -1, $
	XMINOR   = 5, $
	XTICKN   = REPLICATE(' ', nticks), $
	XSTYLE   = 1, $
	YRANGE   = [0, 1], $
	YTICKS   = 1, $
	YTICKN   = [' ', ' '], $
	YSTYLE   = 1, $
	POSITION = [x0, y6, x1, y7]

AXIS, YAXIS = 0, $																								;Draw altitude axis
	SAVE   = 1, $
	YRANGE = o3range, $
	YTITLE = o3title, $
	YTICKS = 1, $
	YSTYLE = 1, $
	COLOR  = color0

OPLOT, x, (o3.values)[it], COLOR = color0, THICK = 2													;Plot temperature measurments

AXIS, YAXIS = 1, $																								;Draw temperature axis
	SAVE   = 1, $
	YRANGE = corange, $
	YTITLE = cotitle, $
	YTICKS = 1, $
	YSTYLE = 1, $
	COLOR  = color1

OPLOT, x, (co.values)[it], COLOR = color1, THICK = 2													;Plot temperature measurments

PLOT, x, x, /NODATA, $																							;Set up plot
	THICK    = 2, $
	XRANGE   = [0, nt-1], $
	XTICKS   = nticks -1, $
	XMINOR   = 5, $
	XTICKN   = REPLICATE(' ', nticks), $
	XSTYLE   = 1, $
	YRANGE   = [0, 1], $
	YTICKS   = 1, $
	YTICKN   = [' ', ' '], $
	YSTYLE   = 1, $
	POSITION = [x0, y5, x1, y6]

AXIS, YAXIS = 0, $																								;Draw altitude axis
	SAVE   = 1, $
	YRANGE = hno3range, $
	YTITLE = hno3title, $
	YTICKS = 1, $
	YSTYLE = 1, $
	COLOR  = color0

OPLOT, x, (hno3.values)[it], COLOR = color0, THICK = 2												;Plot temperature measurments

AXIS, YAXIS = 1, $																								;Draw temperature axis
	SAVE   = 1, $
	YRANGE = co2range, $
	YTITLE = co2title, $
	YTICKS = 1, $
	YSTYLE = 1, $
	COLOR  = color1

OPLOT, x, (co2.values)[it], COLOR = color1, THICK = 2													;Plot temperature measurments

PLOT, x, x, /NODATA, $																							;Set up plot
	THICK    = 2, $
	XRANGE   = [0, nt-1], $
	XTICKS   = nticks -1, $
	XMINOR   = 5, $
	XTICKN   = REPLICATE(' ', nticks), $
	XSTYLE   = 1, $
	YRANGE   = [0, 1], $
	YTICKS   = 1, $
	YTICKN   = [' ', ' '], $
	YSTYLE   = 1, $
	POSITION = [x0, y4, x1, y5]

AXIS, YAXIS = 0, $																								;Draw altitude axis
	SAVE   = 1, $
	YRANGE = noxrange, $
	YTITLE = noxtitle, $
	YTICKS = 1, $
	YSTYLE = 1, $
	COLOR  = color0

OPLOT, x, (nox.values)[it], COLOR = color0, THICK = 2													;Plot temperature measurments

AXIS, YAXIS = 1, $																								;Draw temperature axis
	SAVE   = 1, $
	YRANGE = ch4range, $
	YTITLE = ch4title, $
	YTICKS = 1, $
	YSTYLE = 1, $
	COLOR  = color1

OPLOT, x, (ch4.values)[it], COLOR = color1, THICK = 2													;Plot temperature measurments

PLOT, x, x, /NODATA, $																							;Set up plot
	THICK    = 2, $
	XRANGE   = [0, nt-1], $
	XTICKS   = nticks -1, $
	XMINOR   = 5, $
	XTICKN   = REPLICATE(' ', nticks), $
	XSTYLE   = 1, $
	YRANGE   = [0, 1], $
	YTICKS   = 1, $
	YTICKN   = [' ', ' '], $
	YSTYLE   = 1, $
	POSITION = [x0, y3, x1, y4]

AXIS, YAXIS = 0, $																								;Draw altitude axis
	SAVE   = 1, $
	YRANGE = h2orange, $
	YTITLE = h2otitle, $
	YTICKS = 1, $
	YSTYLE = 1, $
	COLOR  = color0

OPLOT, x, (h2o.values)[it], COLOR = color0, THICK = 2													;Plot temperature measurments

AXIS, YAXIS = 1, /YLOG, $																								;Draw temperature axis
	SAVE   = 1, $
	YRANGE = partrange, $
	YTITLE = parttitle, $
	YTICKS = 1, $
	YSTYLE = 1, $
	COLOR  = color1

OPLOT, x, (part.values)[it], COLOR = color1, THICK = 2												;Plot temperature measurments

PLOT, x, x, /NODATA, $																							;Set up plot
	THICK    = 2, $
	XRANGE   = [0, nt-1], $
	XTICKS   = nticks -1, $
	XMINOR   = 5, $
	XTICKN   = REPLICATE(' ', nticks), $
	XSTYLE   = 1, $
	YRANGE   = [0, 1], $
	YTICKS   = 1, $
	YTICKN   = [' ', ' '], $
	YSTYLE   = 1, $
	POSITION = [x0, y2, x1, y3]

AXIS, YAXIS = 0, $																								;Draw altitude axis
	SAVE   = 1, $
	YRANGE = hclrange, $
	YTITLE = hcltitle, $
	YTICKS = 1, $
	YSTYLE = 1, $
	COLOR  = color0

OPLOT, x, (hcl.values)[it], COLOR = color0, THICK = 2													;Plot temperature measurments

AXIS, YAXIS = 1, $																								;Draw temperature axis
	SAVE   = 1, $
	YRANGE = formrange, $
	YTITLE = formtitle, $
	YTICKS = 1, $
	YSTYLE = 1, $
	COLOR  = color1

OPLOT, x, (form.values)[it], COLOR = color1, THICK = 2												;Plot temperature measurments

PLOT, x, x, /NODATA, $																							;Set up plot
	THICK    = 2, $
	XRANGE   = [0, nt-1], $
	XTICKS   = nticks -1, $
	XMINOR   = 5, $
	XTICKN   = REPLICATE(' ', nticks), $
	XSTYLE   = 1, $
	YRANGE   = [0, 1], $
	YTICKS   = 1, $
	YTICKN   = [' ', ' '], $
	YSTYLE   = 1, $
	POSITION = [x0, y1, x1, y2]

AXIS, YAXIS = 0, $																								;Draw altitude axis
	SAVE   = 1, $
	YRANGE = nbutrange, $
	YTITLE = nbuttitle, $
	YTICKS = 1, $
	YSTYLE = 1, $
	COLOR  = color0

OPLOT, x, (nbut.values)[it], COLOR = color0, PSYM = 4													;Plot temperature measurments

AXIS, YAXIS = 1, $																								;Draw temperature axis
	SAVE   = 1, $
	YRANGE = tolrange, $
	YTITLE = toltitle, $
	YTICKS = 1, $
	YSTYLE = 1, $
	COLOR  = color1

OPLOT, x, (tol.values)[it], COLOR = color1, PSYM = 4												;Plot temperature measurments

PLOT, x, x, /NODATA, $																							;Set up plot
	THICK    = 2, $
	XRANGE   = [0, nt-1], $
	XTICKS   = nticks -1, $
	XMINOR   = 5, $
	XTICKN   = xticks, $
	XTITLE   = 'Time (UTC)', $
	XSTYLE   = 1, $
	YRANGE   = [0, 1], $
	YTICKS   = 1, $
	YTICKN   = [' ', ' '], $
	YSTYLE   = 1, $
	POSITION = [x0, y0, x1, y1]

AXIS, YAXIS = 0, $																								;Draw altitude axis
	SAVE   = 1, $
	YRANGE = benzrange, $
	YTITLE = benztitle, $
	YTICKS = 1, $
	YSTYLE = 1, $
	COLOR  = color0

OPLOT, x, (benz.values)[it], COLOR = color0, PSYM = 4													;Plot temperature measurments

AXIS, YAXIS = 1, $																								;Draw temperature axis
	SAVE   = 1, $
	YRANGE = ipenrange, $
	YTITLE = ipentitle, $
	YTICKS = 1, $
	YSTYLE = 1, $
	COLOR  = color1

OPLOT, x, (ipen.values)[it], COLOR = color1, PSYM = 4											;Plot temperature measurments

!P.MULTI = 0
IF KEYWORD_SET(eps) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file


END
