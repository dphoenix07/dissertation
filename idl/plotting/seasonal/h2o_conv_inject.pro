PRO H2O_CONV_INJECT, event, scheme, start_date, end_date, $
	DOMAIN   = domain, $
	REGION   = region, $
	PNG	     = png, $
	EPS   	 = eps, $
	ZBUFF    = zbuff


;+
; Name:
;		H2O_CONV_INJECT
; Purpose:
;		Find occurrences of convective injection of h2o and estimate scale  
; Calling sequence:
;		H2O_CONV_INJECT, run, scheme, start_date, end_date
; Input:
;		event      : Model simulation name. (e.g., '20120519')
;		scheme 	   : Model initial state. Typically 'morrison' or 'nssl'.
;		start_date : String variable of the start date requested (e.g., '20120519T2300Z')
;		end_date   : String variable of the end date requested (e.g., '20120520T0300Z')
; Output:
;		Number of grid points where h2o > 25 ppmv is transported to stratosphere from
;		troposphere.
; Keywords:
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		VERBOSE : If set, message writing process.
; Author and history:
;		Daniel B. Phoenix	    2017-07-24. 
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1											;Set default domain


outdir  = !WRF_DIRECTORY + event + '/' + scheme + '/plots/h2o_conv_inject/'

FILE_MKDIR, outdir

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(event, scheme, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(event, scheme, start_date, end_date, /DATE)	


FOREACH date, date_arr DO BEGIN
	conv  = (WRF_READ_VAR('Updraft_tracer' , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    h2o   = (WRF_READ_VAR('H2O'            , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E6				
    co    = (WRF_READ_VAR('CO'             , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E3					
    y     = (WRF_READ_VAR('Latitude'       , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    x     = (WRF_READ_VAR('Longitude'      , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    z     = (WRF_READ_VAR('Z'		       , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    ztrop = (WRF_READ_VAR('Z_trop'	       , date, event, scheme, DOMAIN=domain, INDICES=region)).values	
	R 	  =  WRF_READ_VAR('REFL'           , date, event, scheme, DOMAIN=domain, INDICES=region)
    cloud = (WRF_READ_VAR('CLOUD_MIX_TOTAL', date, event, scheme, DOMAIN=domain, INDICES=region)).values
    
    dim 	 = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
    ztrop    = MEDIAN(ztrop, 100)
    xyz_trop = REBIN(ztrop, dim[0], dim[1], dim[2], /SAMPLE)
    yy       = REBIN(y,     dim[0], dim[1], dim[2], /SAMPLE)
    xx       = REBIN(x,     dim[0], dim[1], dim[2], /SAMPLE)
 
 	trp_conv = WHERE(conv GT 0.5, tp_good, COMPLEMENT=tp_bad)
 	IF (tp_good GT 0) THEN BEGIN
        xyz_trop [trp_conv] = xyz_trop[trp_conv]
        z		 [trp_conv] = z       [trp_conv]
        h2o		 [trp_conv] = h2o	  [trp_conv]
    
        xyz_trop [tp_bad] = !Values.F_NaN
        z		 [tp_bad] = !Values.F_NaN
        h2o		 [tp_bad] = !Values.F_NaN
        
        trop = WHERE((z-xyz_trop) GT 750.0, tr_count, COMPLEMENT = strat)	
        IF (tr_count GT 0) THEN BEGIN
        	h2o[trop ] = h2o[trop]
        	h2o[strat] = !Values.F_NaN
        ENDIF
        
        ncloud = WHERE(cloud LT 1.0E-5, ncloud_count, COMPLEMENT=cloud_h2o)
        IF (ncloud_count GT 0) THEN BEGIN
        	h2o[ncloud   ] = h2o[ncloud]
        	h2o[cloud_h2o] = !Values.F_NaN
        ENDIF


        wrap = WHERE((h2o GT 120.0), wr_count)
        
        PRINT, date
        PRINT, 'qualified counts=', wr_count
	    PRINT, 'maximum h2o concentration (ppmv)=', MAX(h2o,/NAN)
	    
	    max_h2o = MAX(h2o,/NAN)
	    loc     = WHERE(h2o EQ max_h2o)
	    
	    ;Print info to file
	    temp_date = MAKE_ISO_DATE_STRING(date,/COMPACT,/UTC)
	    fname = outdir+temp_date+'.txt'
	    OPENW, lun, fname, /GET_LUN     
	    PRINTF, lun, date
	    PRINTF, lun, 'longitude				           = ' + STRING(xx[loc])
	    PRINTF, lun, 'latitude				           = ' + STRING(yy[loc])
	    PRINTF, lun, 'tropopause height (m)            = ' + STRING(xyz_trop[loc])
	    PRINTF, lun, 'tropopause relative (m)          = ' + STRING(z[loc]-xyz_trop[loc])
	    PRINTF, lun, 'maximum h2o concentration (ppmv) = ' + STRING(MAX(h2o,/NAN))
    
	    FREE_LUN, lun
    
	    PRINT, 'longitude=',xx[loc]
	    PRINT, 'latitude=', yy[loc]
	    PRINT, 'tropopause height (m)=', xyz_trop[loc]
	    PRINT, 'tropopause relative (m)=', z[loc]-xyz_trop[loc]
	ENDIF

temp_date = MAKE_ISO_DATE_STRING(date,/COMPACT,/UTC)

bad = WHERE (R.values EQ 0.0, bad_count, COMPLEMENT = good, NCOMPLEMENT = good_count)
R.values [bad ] = -35.0000
R.values [good] = R.values [good]

dim = SIZE(x, /DIMENSIONS)																			;Get dimension sizes

epsfile = outdir + temp_date  + '.eps'						;EPS filename
pdffile = outdir + temp_date  + '.pdf'						;PDF filename
pngfile = outdir + temp_date  + '.png'						;PNG filename

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

table   = [COLOR_24(200, 200, 200),VISUALIZE_88D_COLOR()]												;Set reflectivity color table
rlevels = [-100.0, 5.0*FINDGEN(N_ELEMENTS(table))]													;Set reflectivity contour levels
;rlevels = [-100.0, 5.0 + 5.0*FINDGEN(N_ELEMENTS(table))]											;***for comparison with t-matrix
wfactor = 400.0/(dim[0]) + 400.0/(dim[1])																	;Set map factor

dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string
date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Create date string

;map_pos = [0.0, 0.0, 1.0, 1.0]																				;map position for paper
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
	TITLE     = 'WRF-' + scheme + ' ' + dom_string + ' valid ' + date_string, $
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
	CONTOUR, MAX(R.values, DIM=3), x, y, $												;Contour reflectivity values
		OVERPLOT  = 1, $
		FILL      = 1, $
		LEVELS    = rlevels, $
		C_COLOR   = table

USERSYM_CIRCLE, /FILL															;Load plane symbol at flight path orientation

IF (wr_count GT 0) THEN BEGIN
	FOR i=0, wr_count-1 DO BEGIN
		PLOTS, (xx)[wrap[i]], (yy)[wrap[i]], $																		;Overplot plane symbol
			PSYM    = 8, $
			SYMSIZE = 4 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
			NOCLIP  = 0, $
			COLOR   = COLOR_24('black')
	ENDFOR

	PLOTS, (xx)[loc], (yy)[loc], $								;Overplot plane symbol
	    PSYM    = 8, $
	    SYMSIZE = 4 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
	    NOCLIP  = 0, $
	    COLOR   = COLOR_24('white')

ENDIF


MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines

IF KEYWORD_SET(section) THEN BEGIN
	IF (run EQ '20110408') THEN ij = [100, 119, 300, 157]
	IF (run EQ '20110521') THEN ij = [150, 116, 350, 130]
	IF (run EQ '20110618') THEN ij = [040, 060, 240, 080]

	xysect = MAP_2POINTS((x)[ij[0],ij[1]],(y)[ij[0],ij[1]],$
								(x)[ij[2],ij[3]],(y)[ij[2],ij[3]], NPATH = 10)
	
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
	TITLE = 'Reflectivity (dBZ)', $
	POSIT = bar_pos

IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
	
	IF KEYWORD_SET(pdf) THEN PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS						;Convert to PDF
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file

ENDFOREACH

END