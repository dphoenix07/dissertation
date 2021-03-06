PRO SET_TRACER_ICBC, event, scheme, date, $
	DOMAIN   = domain


;+
; Name:
;		SET_TRACER_ICBC
; Purpose:
;		Reads in O3 and CO values from previous WRF-Chem simulation, interpolates them
;		to model domain, and sets them as IC/BC for downscaled simulation.
; Calling sequence:
;		SET_TRACER_ICBC, event, scheme, date
; Input:
;		event      : Model simulation name. (e.g., '20120530')
;		scheme 	   : Model initial state. 
;		date   	   : CDATE
; Output:
;		A wrfinput_d<domain> file with two added tracers with values set by a previous
;		WRF-Chem simulation
; Keywords:
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
; Author and history:
;		Daniel B. Phoenix	    2018-06-25. 
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(date      ) EQ 0) THEN date   = MAKE_DATE(2012,5,30,21)							;Set default start date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain = 1												;Set default domain

outdir  = !WRF_DIRECTORY + event + '/paper/plots/nexrad_timeseries/'
FILE_MKDIR, outdir

;; First read in old simulation data

event_old  = '20120529'
scheme_old = 'nssl'
date_old   = MAKE_DATE(2012,5,29,21)

y_old        = (WRF_READ_VAR('Latitude' , date_old, event_old, scheme_old, DOMAIN=domain)).values
x_old        = (WRF_READ_VAR('Longitude', date_old, event_old, scheme_old, DOMAIN=domain)).values
z_old        = (WRF_READ_VAR('Z'		, date_old, event_old, scheme_old, DOMAIN=domain)).values
co_old		 = (WRF_READ_VAR('CO'       , date_old, event_old, scheme_old, DOMAIN=domain)).values
o3_old		 = (WRF_READ_VAR('O3'		, date_old, event_old, scheme_old, DOMAIN=domain)).values


; Read in wrfinput_d<domain> file

y_new        = (WRF_READ_VAR('Latitude' ,     date,     event,     scheme, DOMAIN=domain)).values
x_new        = (WRF_READ_VAR('Longitude',     date,     event,     scheme, DOMAIN=domain)).values
z_new        = (WRF_READ_VAR('Z'		,     date,     event,     scheme, DOMAIN=domain)).values

;+ Interpolate O3 and CO values from old grid to new grid
ix = INTERPOL(FINDGEN(N_ELEMENTS(x_old)),x_old,x_new)
iy = INTERPOL(FINDGEN(N_ELEMENTS(y_old)),y_old,y_new)
iz = INTERPOL(FINDGEN(N_ELEMENTS(z_old)),z_old,z_new)

o3_new  = INTERPOLATE(o3_old[*,*,0],ix,iy)*1.0E3
co_new  = INTERPOLATE(co_old[*,*,0],ix,iy)*1.0E3

STOP

dim = SIZE(o3_new, /DIMENSIONS)																			;Get dimension sizes
offset=0
y0 = y_new[          offset ,          offset ]														;Set domain boundary points
y1 = y_new[          offset ,dim[1]-(1+offset)]
y2 = y_new[dim[0]-(1+offset),dim[1]-(1+offset)]
y3 = y_new[dim[0]-(1+offset),          offset ]
x0 = x_new[          offset ,          offset ]
x1 = x_new[          offset ,dim[1]-(1+offset)]
x2 = x_new[dim[0]-(1+offset),dim[1]-(1+offset)]
x3 = x_new[dim[0]-(1+offset),          offset ]

xc = INTERPOLATE(x_new, 0.5*(dim[0]-1), 0.5*(dim[1]-1))											;Get central grid point
yc = INTERPOLATE(y_new, 0.5*(dim[0]-1), 0.5*(dim[1]-1))

table   = [COLOR_24(200, 200, 200),VISUALIZE_88D_COLOR()]												;Set reflectivity color table
rlevels = [-100.0, 5.0*FINDGEN(N_ELEMENTS(table))]														;Set reflectivity contour levels
wfactor = 400.0/(dim[0]) + 400.0/(dim[1])																	;Set map factor

dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string
date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Create date string

outdir  = !WRF_DIRECTORY + event + '/' + scheme + '/plots/'
epsfile = outdir + dom_string + '_' + date_string + '.eps'											;EPS filename
pdffile = outdir + dom_string + '_' + date_string + '.pdf'											;PDF filename
pngfile = outdir + dom_string + '_' + date_string + '.png'											;PNG filename

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
		PS_ON, FILENAME = epsfile, PAGE_SIZE = [4.0,3.0], MARGIN = 0.0, /INCHES;PAGE_SIZE = 0.001*dim*wfactor			;Switch to Postscript device
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
	TITLE     = 'WRF-' + event + ' ' + dom_string + ' valid ' + date_string, $
	POSITION  = map_pos

IF KEYWORD_SET(image) THEN BEGIN
	ij0 = CONVERT_COORD([(!X.WINDOW)[0],(!Y.WINDOW)[0]], /NORMAL, /TO_DEVICE)
	ij1 = CONVERT_COORD([(!X.WINDOW)[1],(!Y.WINDOW)[1]], /NORMAL, /TO_DEVICE)

	xsize = LONG(ij1[0] - ij0[0])
	ysize = LONG(ij1[1] - ij0[1])

	image0 = (o3_new)[offset:(dim[0]-(1+offset)), offset:(dim[1]-(1+offset))]
	dim    = dim - (2*offset)
	
	IF KEYWORD_SET(eps) THEN $
		image0 = INTERPOLATE(image0, MAKEN(0, dim[0]-1, xsize/10), MAKEN(0, dim[1]-1, ysize/10), /GRID) ELSE $
		image0 = INTERPOLATE(image0, MAKEN(0, dim[0]-1, xsize   ), MAKEN(0, dim[1]-1, ysize   ), /GRID)

	image0 = IMAGE_24(COLOR_LOOKUP_24((image0 < 75.0), table[1:*], MIN = 0.0, MAX = 75.0, $
				MISSING = COLOR_24(200, 200, 200), /NAN))
	TV, image0, ij0[0], ij0[1], TRUE = 3, XSIZE = xsize, YSIZE = ysize, /DEVICE
ENDIF ELSE $	
	CONTOUR, (o3_new[*,*,0]*1.0E3), x_new, y_new, $												;Contour reflectivity values
		OVERPLOT  = 1, $
		FILL      = 1, $
		LEVELS    = rlevels, $
		C_COLOR   = table

MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines

IF KEYWORD_SET(section) THEN BEGIN
	IF (scheme EQ '20110408') THEN ij = [100, 119, 300, 157]
	IF (scheme EQ '20110521') THEN ij = [150, 116, 350, 130]
	IF (scheme EQ '20110618') THEN ij = [040, 060, 240, 080]

	xysect = MAP_2POINTS((x.values)[ij[0],ij[1]],(y.values)[ij[0],ij[1]],$
								(x.values)[ij[2],ij[3]],(y.values)[ij[2],ij[3]], NPATH = 10)
	
	OPLOT, xysect[0,*], xysect[1,*], THICK = 4
	XYOUTS, xysect[0,0], xysect[1,0], 'A', ALIGN = 1
	XYOUTS, xysect[0,-1], xysect[1,-1], 'B', ALIGN = 0
ENDIF

MAP_SET, yc, xc, 0, CONIC = 1, $																				;Redraw map boundaries
	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
	ISOTROPIC = 1, $
	NOERASE   = 1, $
	POSITION  = map_pos
	
COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
	RANGE = [0, 75], $
	TICKS = 5, $
	TITLE = 'Ozone concentration (ppb)', $
	POSIT = bar_pos

IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
	
	IF KEYWORD_SET(pdf) THEN PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS						;Convert to PDF
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file


END