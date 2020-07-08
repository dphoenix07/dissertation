PRO TMATRIX_REFLECTIVITY, date1, date2, run, experiment, $
	OFFSET  = offset, $
	DOMAIN  = domain, $
	REGION  = region, $
	Z_buff  = z_buff, $
	IMAGE   = image, $
	SECTION = section, $
	EPS     = eps, $
	PDF     = pdf, $
	PNG     = png

;+
; Name:
;		TMATRIX_REFLECTIVITY
; Purpose:
;		This is a template for creating IDL procedure files. 
; Calling sequence:
;		TMATRIX_REFLECTIVITY, experiment, date
; Input:
;		run   	   : String variable of run name. (e.g., '20120519')
;		experiment : String variable of initial state. (e.g., 'morrison')
;		date  	   : Desired date {CDATE}.
; Output:
;		A map of simulated composite reflectivity. 
; Keywords:
;		DOMAIN : Simulation domain number. Default is 1. 
;		EPS    : If set, output to PostScript.
;		PDF    : If set, output to PDF.
;		PNG    : If set, write PNG image.
; Author and history:
;		Cameron R. Homeyer  2012-10-05.
;		Daniel B. Phoenix	2016-06-04. Edited original to read output from PRD.
;										Current version will loop over all times in
;										T-matrix/case directory. Needs to be fixed
;-

COMPILE_OPT IDL2																									;Set compile options

print_bounds = 0

IF (N_ELEMENTS(run       ) EQ 0) THEN run        = '20120519'
IF (N_ELEMENTS(experiment) EQ 0) THEN experiment = 'morrison'
IF (N_ELEMENTS(tracer    ) EQ 0) THEN tracer     = 'BL_tracer'
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 2
IF (N_ELEMENTS(offset    ) EQ 0) THEN offset     = 0 ELSE print_bounds = 1

;IF (run EQ '20120519') THEN region = [50, 50, 250, 190]
;IF (run EQ '20120529') THEN region = [50, 115, 230, 270] 

;; indices for manuscript
IF (run EQ '20120529') THEN region = [70, 80, 220, 200]
	IF (experiment EQ 'milbyau') THEN region = [40, 80, 200, 210]
	IF (experiment EQ 'qnse'   ) THEN region = [50, 80, 200, 200]
	IF (experiment EQ 'acm2'   ) THEN BEGIN													; For blank figure
		region = [70, 80, 220, 200]
		experiment = 'qnse'
	ENDIF
	
;IF (run EQ '20120601') THEN region = [60, 80, 270, 250] 

;IF (run EQ '20120519') THEN region = [95,120,190,215]										; For paper images
;IF (experiment EQ 'nssl_qnse') THEN region = [85,120,180,215]
;; end

time1 = MAKE_ISO_DATE_STRING(date1, PREC='MINUTE', /COMPACT, /UTC)
time2 = MAKE_ISO_DATE_STRING(date2, PREC='MINUTE', /COMPACT, /UTC)

tmatrix_dir = !WRF_DIRECTORY + run + '/tmatrix/' + experiment + '/'
CD, tmatrix_dir
tfile = FILE_SEARCH('wrfout*', COUNT = ntfile)		

file1 = WHERE('wrfout_d02_'+ STRING(time1) EQ tfile)
file2 = WHERE('wrfout_d02_'+ STRING(time2) EQ tfile)
file1 = file1[0] 
file2 = file2[0]

times = [ ]
FOR i = file1, file2 DO BEGIN
	time  = tfile[i]
	times = [times, time]
ENDFOR
PRINT, times

x  = WRF_READ_VAR('Longitude', date1, run, 'nssl', DOMAIN = domain, INDICES = region)		;Read variables
y  = WRF_READ_VAR('Latitude',  date1, run, 'nssl', DOMAIN = domain, INDICES = region)

;tmatrix_dir = !WRF_DIRECTORY + run + '/tmatrix/' + experiment + '/'
;
;CD, tmatrix_dir
;tfile = FILE_SEARCH('wrfout*', COUNT = nfile)														;Get file

FOREACH file, times DO BEGIN 
	CD, tmatrix_dir + '/' + file
	infile = FILE_SEARCH('*.nc', COUNT = nfile)

	R 	 = [ ]
	refl = [ ]
	
	FOR t = 0, nfile - 1 DO BEGIN  
		refl    = (TMATRIX_READ_VAR('data', run, experiment, file, t, $
					DOMAIN = dom, INDICES = region)).values
		R 		= [[[R]],[[refl]]]
	ENDFOR

	bad = WHERE (R EQ 0.0, bad_count, COMPLEMENT = good, NCOMPLEMENT = good_count)
	R [bad ] = -35.0000
	R [good] = R [good]

	dim = SIZE(x.values, /DIMENSIONS)																			;Get dimension sizes

	y0 = y.values[          offset ,          offset ]														;Set domain boundary points
	y1 = y.values[          offset ,dim[1]-(1+offset)]
	y2 = y.values[dim[0]-(1+offset),dim[1]-(1+offset)]
	y3 = y.values[dim[0]-(1+offset),          offset ]
	x0 = x.values[          offset ,          offset ]
	x1 = x.values[          offset ,dim[1]-(1+offset)]
	x2 = x.values[dim[0]-(1+offset),dim[1]-(1+offset)]
	x3 = x.values[dim[0]-(1+offset),          offset ]

	xc = INTERPOLATE(x.values, 0.5*(dim[0]-1), 0.5*(dim[1]-1))											;Get central grid point
	yc = INTERPOLATE(y.values, 0.5*(dim[0]-1), 0.5*(dim[1]-1))

	IF (print_bounds) THEN $
		PRINT, xc, yc, '  [',STRING([y0,x0,y1,x1,y2,x2,y3,x3], FORMAT="(F8.3,',')"), ']'

	table   = [COLOR_24(200, 200, 200),VISUALIZE_88D_COLOR()]												;Set reflectivity color table
	rlevels = [-100.0, 5.0*FINDGEN(N_ELEMENTS(table))]													;Set reflectivity contour levels
	;rlevels = [-100.0, 5.0 + 5.0*FINDGEN(N_ELEMENTS(table))]											;***for comparison with t-matrix
	wfactor = 400.0/(dim[0]) + 400.0/(dim[1])																	;Set map factor

;	dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string
;	date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Create date string

	experiment = 'acm2'
	outdir  = !WRF_DIRECTORY + run + '/paper/plots/'
	epsfile = outdir + experiment + '_' + STRING(file) + '_tmat.eps'												;EPS filename
	pdffile = outdir + experiment + '_' + STRING(file) + '_tmat.pdf'												;PDF filename
	pngfile = outdir + experiment + '_' + STRING(file) + '_tmat.png'												;PNG filename

	FILE_MKDIR, outdir																								;Create output directory, if necessary

;	map_pos = [0.05, 0.15, 0.95, 0.95]																			;Set map position
	map_pos = [0.0, 0.0, 1.0, 1.0]																				;map position for paper
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
;		TITLE     = 'WRF-' + experiment + ' ' + dom_string + ' valid ' + date_string, $
		TITLE 	  = experiment + ' ' + file, $
		POSITION  = map_pos

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

		CONTOUR, MAX(R, DIM = 3), x.values, y.values, $												;Contour reflectivity values
	;	CONTOUR, R.values[*,*,37], x.values, y.values, $													;For T-Matrix comparison
			OVERPLOT  = 1, $
			FILL      = 1, $
			LEVELS    = rlevels, $
			C_COLOR   = table

;	MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines

	IF KEYWORD_SET(section) THEN BEGIN
		IF (run EQ '20110408') THEN ij = [100, 119, 300, 157]
		IF (run EQ '20110521') THEN ij = [150, 116, 350, 130]
		IF (run EQ '20110618') THEN ij = [040, 060, 240, 080]

		xysect = MAP_2POINTS((x.values)[ij[0],ij[1]],(y.values)[ij[0],ij[1]],$
									(x.values)[ij[2],ij[3]],(y.values)[ij[2],ij[3]], NPATH = 10)
	
		OPLOT, xysect[0,*], xysect[1,*], THICK = 4
		XYOUTS, xysect[0,0], xysect[1,0], 'A', ALIGN = 1
		XYOUTS, xysect[0,-1], xysect[1,-1], 'B', ALIGN = 0
	ENDIF

;	MAP_SET, yc, xc, 0, CONIC = 1, $																				;Redraw map boundaries
;		LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
;		ISOTROPIC = 1, $
;			POSITION  = map_pos
;	
;	COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
;		RANGE = [0, 75], $
;		TICKS = 5, $
;		TITLE = 'Reflectivity (dBZ)', $
;	NOERASE   = 1, $
;		POSIT = bar_pos

	IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
			LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
		PS_OFF																											;Turn PS off
	
		IF KEYWORD_SET(pdf) THEN PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS						;Convert to PDF
	ENDIF ELSE IF KEYWORD_SET(png) THEN $
		WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file
ENDFOREACH

END
