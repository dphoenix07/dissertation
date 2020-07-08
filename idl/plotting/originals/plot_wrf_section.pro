PRO PLOT_WRF_SECTION, var, date, run, state, $
	TROPW   = tropw, $
	TRACER  = tracer, $
	REGION  = region, $
	INDICES = indices, $
	WIDTH   = width, $
	DOMAIN  = domain, $
	WINDS   = winds, $
	EPS     = eps, $
	PDF     = pdf, $
	PNG     = png


;+
; Name:
;		PLOT_WRF_SECTION
; Purpose:
;		This is a procedure to plot a vertical secion of a given WRF variable
;		. 
; Calling sequence:
;		PLOT_WRF_SECTION, var, date, run, state
; Input:
;		var   : WRF output variable to color-fill
;		date  : Analysis date {CDATE}
;		run   : WRF simulation name. (e.g., 'APR04')
;		state : WRF initial state. (e.g., 'era_mp8')
; Output:
;		A vertical section plot.
; Keywords:
;		TROPW   : If set, map vertical velocity at the tropopause.
;		INDICES : WRF grid indices to compute vertical section for. e.g., [10, 200, 300, 100]
;		DOMAIN  : WRF domain number. Default is 2
;		EPS     : If set, output to PostScript.
;		PDF     : If set, write and convert PostScript to PDF.
;		PNG     : If set, write PNG image.
; Author and history:
;		Cameron R. Homeyer  2012-11-28.
;-

COMPILE_OPT IDL2																									;Set compile options

wstride  = 25
wstride2 = 9

IF (N_ELEMENTS(var  ) EQ 0) THEN var   = 'BL_tracer'													;Set default color-filled variable
IF (N_ELEMENTS(date ) EQ 0) THEN date  = MAKE_DATE(2012, 5, 19, 12)									;Set default analysis date
IF (N_ELEMENTS(run  ) EQ 0) THEN run   = '20120519'													;Set default model run
IF (N_ELEMENTS(state) EQ 0) THEN state = 'morrison'
IF (N_ELEMENTS(domain ) EQ 0) THEN domain  = 2

x      = WRF_READ_VAR('Longitude', date, run, state, DOMAIN = domain)
dims   = SIZE(x.values, /DIMENSIONS)

yin = dims[0] / 2
xin1 = 1
xin2 = dims[1] - 1

IF (N_ELEMENTS(indices) EQ 0) THEN indices = [xin1, yin, xin2, yin]
;IF (N_ELEMENTS(indices) EQ 0) THEN indices = [50, 125, 250, 125]									;Set default section grid indices

date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Set date string

IF KEYWORD_SET(tropw ) THEN map_var = 'w'         ELSE $
IF KEYWORD_SET(tracer) THEN map_var = 'BL_tracer' $
							  ELSE map_var = 'REFL'

IF (N_ELEMENTS(region) GT 0) THEN BEGIN
	x = WRF_READ_VAR('Longitude', date, run, state, DOMAIN = domain)								;Read coordinates
	y = WRF_READ_VAR('Latitude',  date, run, state, DOMAIN = domain)

	x.values = (x.values + 360.0) MOD 360.0
	region[[0,2]] = (region[[0,2]] + 360.0) MOD 360.0

	ij = WHERE(((x.values GE region[0]) AND (y.values GE region[1]) AND $
					(x.values LE region[2]) AND (y.values LE region[3])), nregion)
					
	IF (nregion GT 0) THEN BEGIN
		ij = ARRAY_INDICES(x.values, ij)
	
		indices = [MIN(ij[0,*]), MIN(ij[1,*]), MAX(ij[0,*]), MAX(ij[1,*])]
	ENDIF
ENDIF

IF (N_ELEMENTS(width) GT 0) THEN BEGIN
	dir      = (ATAN((indices[2]-indices[0]), (indices[3]-indices[1]))/!DTOR + 360.0) MOD 360.0
	ddir     = !DTOR*((dir+90) MOD 360.0)
	indices0 = indices - 0.5*width*[SIN(ddir),COS(ddir),SIN(ddir),COS(ddir)]
	indices1 = indices + 0.5*width*[SIN(ddir),COS(ddir),SIN(ddir),COS(ddir)]
	rindices = [FLOOR(indices0[0] < indices0[2] < indices1[0] < indices1[2]), $
					FLOOR(indices0[1] < indices0[3] < indices1[1] < indices1[3]), $
					CEIL(indices0[0] > indices0[2] > indices1[0] > indices1[2]), $
					CEIL(indices0[1] > indices0[3] > indices1[1] > indices1[3])]
ENDIF ELSE $
	rindices = indices

x     = WRF_READ_VAR('Longitude', date, run, state, DOMAIN = domain, INDICES = rindices)	;Read variables
y     = WRF_READ_VAR('Latitude',  date, run, state, DOMAIN = domain, INDICES = rindices)
z     = WRF_READ_VAR('Z',         date, run, state, DOMAIN = domain, INDICES = rindices)
cloud = WRF_READ_VAR('CLOUD_MIX', date, run, state, DOMAIN = domain, INDICES = rindices)
vmap  = WRF_READ_VAR(map_var,     date, run, state, DOMAIN = domain, INDICES = rindices)
trop  = WRF_READ_VAR('Z_trop',    date, run, state, DOMAIN = domain, INDICES = rindices)

IF ((var EQ 'PV') OR (var EQ 'dthetadz')) THEN BEGIN
	th        =           WRF_READ_VAR('T', date, run, state, DOMAIN = domain, INDICES = rindices)
	th.values = ((1000.0/(WRF_READ_VAR('P', date, run, state, DOMAIN = domain, INDICES = rindices)).values)^(!Rair/!Cp))*(th.values)	;Compute potential temperature
ENDIF

dim   = SIZE(Z.values, /DIMENSIONS)
nx    = dim[0]
ny    = dim[1]
nz    = dim[2]

IF (var EQ 'dthetadz') THEN BEGIN
	vwrf        = {values : WRF_STABILITY(th.values, z.values)}
	vwrf.values = 1000.0*vwrf.values
ENDIF ELSE IF (var EQ 'deluv') THEN BEGIN
	u = WRF_READ_VAR('u', date, run, state, DOMAIN = domain, INDICES = rindices)
	v = WRF_READ_VAR('v', date, run, state, DOMAIN = domain, INDICES = rindices)
	vwrf = u;WRF_READ_VAR('w', date, run, state, DOMAIN = domain, INDICES = rindices)
	vwrf.values = SQRT(u.values^2 + v.values^2); + vwrf.values^2)
ENDIF ELSE IF (var EQ 'KEz') THEN BEGIN
	vwrf = WRF_READ_VAR('P', date, run, state, DOMAIN = domain, INDICES = rindices)
	w    = WRF_READ_VAR('w', date, run, state, DOMAIN = domain, INDICES = rindices)
	vwrf = 50.0*(SHIFT(vwrf.values,0,0,1) - SHIFT(vwrf.values,0,0,-1))									;compute grid volume pressure in Pa
	vwrf = {values : 0.5*(vwrf*9000.0/!g)*(w.values^2)}
ENDIF ELSE $
	IF (var NE map_var) THEN $
		vwrf = WRF_READ_VAR(var, date, run, state, DOMAIN = domain, INDICES = rindices)

nsect = 2*LONG(SQRT((indices[2]-indices[0])^2 + (indices[3]-indices[1])^2))
isect = MAKEN(indices[0], indices[2], nsect) - (rindices[0] < rindices[2])
jsect = MAKEN(indices[1], indices[3], nsect) - (rindices[1] < rindices[3])
ksect = FINDGEN(nz)

iisect = REBIN(       isect,         nsect, nz, /SAMPLE)
jjsect = REBIN(       jsect,         nsect, nz, /SAMPLE)
kksect = REBIN(REFORM(ksect, 1, nz), nsect, nz, /SAMPLE)

zsect  = INTERPOLATE(z.values,     iisect, jjsect, kksect, MISSING = !Values.F_NaN)

IF KEYWORD_SET(eps) THEN BEGIN
	dz     = 100.0
	zmin   = 0000.0
	zmax   = 20000.0
	nzsect = LONG((zmax-zmin)/dz) + 1
	iisect = REBIN(                 isect,             nsect, nzsect, /SAMPLE)
	jjsect = REBIN(                 jsect,             nsect, nzsect, /SAMPLE)
	kksect = REBIN(REFORM(FINDGEN(nzsect), 1, nzsect), nsect, nzsect, /SAMPLE)
	FOR i = 0, nsect-1 DO kksect[i,*] = INTERPOL(FINDGEN(nz), zsect[i,*], zmin + dz*FINDGEN(nzsect))
ENDIF ELSE BEGIN
	dz     = 100.0
	zmin   = 00000.0
	zmax   = 20000.0
	nzsect = LONG((zmax-zmin)/dz) + 1
	iisect = REBIN(                 isect,             nsect, nzsect, /SAMPLE)
	jjsect = REBIN(                 jsect,             nsect, nzsect, /SAMPLE)
	kksect = REBIN(REFORM(FINDGEN(nzsect), 1, nzsect), nsect, nzsect, /SAMPLE)
	FOR i = 0, nsect-1 DO kksect[i,*] = INTERPOL(FINDGEN(nz), zsect[i,*], zmin + dz*FINDGEN(nzsect))
;	zmin   = 0.0
;	zmax   = 15000.0
;	nzsect = nz
ENDELSE

IF KEYWORD_SET(winds) THEN BEGIN	
	IF (var EQ 'deluv') THEN BEGIN
		w = WRF_READ_VAR('w', date, run, state, DOMAIN = domain, INDICES = rindices)
	ENDIF ELSE BEGIN
		u = WRF_READ_VAR('u', date, run, state, DOMAIN = domain, INDICES = rindices)
		v = WRF_READ_VAR('v', date, run, state, DOMAIN = domain, INDICES = rindices)
		w = WRF_READ_VAR('w', date, run, state, DOMAIN = domain, INDICES = rindices)
	ENDELSE
	
	iratio   = ABS(indices[3]-indices[1])/ABS(indices[2]-indices[0])
	u.values = SQRT((u.values*SIN(ATAN(iratio)))^2 + $
						 (v.values*COS(ATAN(iratio)))^2)
ENDIF

xsect = INTERPOLATE(x.values, isect, jsect)
ysect = INTERPOLATE(y.values, isect, jsect)

PRINT, xsect[0], xsect[-1], ysect[0], ysect[-1]

IF (N_ELEMENTS(width) GT 0) THEN BEGIN
	isect0 = MAKEN(indices0[0], indices0[2], nsect) - (rindices[0] < rindices[2])
	isect1 = MAKEN(indices1[0], indices1[2], nsect) - (rindices[0] < rindices[2])
	jsect0 = MAKEN(indices0[1], indices0[3], nsect) - (rindices[1] < rindices[3])
	jsect1 = MAKEN(indices1[1], indices1[3], nsect) - (rindices[1] < rindices[3])

	xsect0 = INTERPOLATE(x.values, isect0, jsect0)
	xsect1 = INTERPOLATE(x.values, isect1, jsect1)
	ysect0 = INTERPOLATE(y.values, isect0, jsect0)
	ysect1 = INTERPOLATE(y.values, isect1, jsect1)

	FOR i = 0, width DO BEGIN
		isect  = MAKEN(indices0[0], indices0[2], nsect) - (rindices[0] < rindices[2]) + FLOAT(i*(indices1[0]-indices0[0]))/width
		jsect  = MAKEN(indices0[1], indices0[3], nsect) - (rindices[1] < rindices[3]) + FLOAT(i*(indices1[1]-indices0[1]))/width
		iisect = REBIN(isect, nsect, nzsect, /SAMPLE)
		jjsect = REBIN(jsect, nsect, nzsect, /SAMPLE)
		
		kksect = REBIN(REFORM(ksect, 1, nz), nsect, nz, /SAMPLE)
		zsect0 = INTERPOLATE(z.values, iisect[*,0:nz-1], jjsect[*,0:nz-1], kksect, MISSING = !Values.F_NaN)
	
		kksect = REBIN(REFORM(FINDGEN(nzsect), 1, nzsect), nsect, nzsect, /SAMPLE)
		FOR j = 0, nsect-1 DO kksect[j,*] = INTERPOL(FINDGEN(nz), zsect0[j,*], zmin + dz*FINDGEN(nzsect))		

		IF (i EQ 0) THEN BEGIN
			IF (var NE map_var) THEN $
				vsect = INTERPOLATE(vwrf.values,  iisect, jjsect, kksect, MISSING = !Values.F_NaN) ELSE $		;Interpolate variables to section
				vsect = INTERPOLATE(vmap.values,  iisect, jjsect, kksect, MISSING = !Values.F_NaN)
	
			csect    = INTERPOLATE(cloud.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)
			zsect    = INTERPOLATE(z.values,     iisect, jjsect, kksect, MISSING = !Values.F_NaN)
			trpsect  = INTERPOLATE(trop.values,   isect,  jsect        )

			IF (var EQ 'deluv') THEN vsect = vsect - REBIN(MEAN(vsect,DIM=2,/NAN),nsect,nzsect)

			IF (N_ELEMENTS(th) GT 0) THEN $
				thsect = INTERPOLATE(th.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)
	
			IF KEYWORD_SET(winds) THEN BEGIN
				usect = INTERPOLATE(u.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)
				wsect = INTERPOLATE(w.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)
			ENDIF
		ENDIF ELSE BEGIN
			IF (var NE map_var) THEN BEGIN
				IF (var EQ 'deluv') THEN vsect += (vsect - REBIN(MEAN(vsect,DIM=2,/NAN),nsect,nzsect)) ELSE $			
				vsect += INTERPOLATE(vwrf.values,  iisect, jjsect, kksect, MISSING = !Values.F_NaN)
			ENDIF ELSE $
				vsect += INTERPOLATE(vmap.values,  iisect, jjsect, kksect, MISSING = !Values.F_NaN)
	
			csect    += INTERPOLATE(cloud.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)
			zsect    += INTERPOLATE(z.values,     iisect, jjsect, kksect, MISSING = !Values.F_NaN)
			trpsect  += INTERPOLATE(trop.values,   isect,  jsect                                 )

			IF (N_ELEMENTS(th) GT 0) THEN $
				thsect += INTERPOLATE(th.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)
	
			IF KEYWORD_SET(winds) THEN BEGIN
				usect += INTERPOLATE(u.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)
				wsect += INTERPOLATE(w.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)
			ENDIF
		ENDELSE
	ENDFOR
	
	vsect   = vsect/(width+1)
	csect   = csect/(width+1)
	zsect   = zsect/(width+1)
	trpsect = trpsect/(width+1)
	
	IF (N_ELEMENTS(th) GT 0) THEN thsect = thsect/(width+1)
	
	IF KEYWORD_SET(winds) THEN BEGIN
		usect = usect/(width+1)
		wsect = wsect/(width+1)
	ENDIF
ENDIF ELSE BEGIN
	IF (var NE map_var) THEN $
		vsect = INTERPOLATE(vwrf.values,  iisect, jjsect, kksect, MISSING = !Values.F_NaN) ELSE $		;Interpolate variables to section
		vsect = INTERPOLATE(vmap.values,  iisect, jjsect, kksect, MISSING = !Values.F_NaN)
	
	csect    = INTERPOLATE(cloud.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)
	zsect    = INTERPOLATE(z.values,     iisect, jjsect, kksect, MISSING = !Values.F_NaN)
	trpsect  = INTERPOLATE(trop.values,   isect,  jsect        )

	IF (var EQ 'deluv') THEN vsect = vsect - REBIN(MEAN(vsect,DIM=2,/NAN),nsect,nzsect)

	IF (N_ELEMENTS(th) GT 0) THEN $
		thsect = INTERPOLATE(th.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)
	
	IF KEYWORD_SET(winds) THEN BEGIN
		usect = INTERPOLATE(u.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)
		wsect = INTERPOLATE(w.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)
	ENDIF
ENDELSE
	
CASE var OF
	'REFL'     : BEGIN
					 table  = NEXRAD_DBZ_COLOR_24(6)
					 levels = -30.0 + 2.5*FINDGEN(N_ELEMENTS(table))
					 range  = [-30, 75]
					 ticks  = 7
					 title  = 'Reflectivity (dBZ)'
					 END
	'PV'       : BEGIN
					 table  = BLUE_RED_24(20, 0.0, 0.75)
					 levels = -10.0 + FINDGEN(N_ELEMENTS(table))
					 range  = [-10, 10]
					 ticks  = 4
					 title  = 'Potential Vorticity (pvu)'
					 END
	'dthetadz' : BEGIN
					 table  = BLUE_RED_24(20, 0.0, 0.5)
					 levels = FINDGEN(N_ELEMENTS(table))
					 range  = [0, 20]
					 ticks  = 4
					 title  = 'dTheta/dz (K km^-1)'
					 END
	'w'        : BEGIN
					 table  = BLUE_RED_24(20, 0.0, 1.0)
					 levels = -4.0 + 0.4*FINDGEN(N_ELEMENTS(table))
					 range  = [-4, 4]
					 ticks  = 4
					 title  = 'w (m/s)'
					 END
	'deluv'    : BEGIN
					 table  = BLUE_RED_24(20, 0.0, 1.0)
					 levels = -10.0 + FINDGEN(N_ELEMENTS(table))
					 range  = [-10, 10]
					 ticks  = 4
					 title  = 'dU!DH!N (m s!U-1!N)'
					 END
	'BL_tracer' : BEGIN
					 vsect  = 100.0*vsect
					 table  = [COLOR_24('white'),WHITE_RED_24(99, 0.2, 0.6)];[COLOR_24('white'), CYAN_MAGENTA_24(19)]
					 levels = FINDGEN(100)
					 range  = [0, 100]
					 ticks  = 4
					 title  = var + ' (%)'
					 END
	'KEz'       : BEGIN
					 vsect  = vsect/100000.0
					 table  = BLUE_RED_24(100,0.1)
					 levels = FINDGEN(100)
					 range  = [0,100]
					 ticks  = 4
					 title  = var + ' (x10!U5!N J)'
					 END
	'H2O'       : BEGIN
					 vsect     = (1.0E6)*vsect
					 table     = CYAN_MAGENTA_24(36)
					 levels    = [1 + FINDGEN(9),10.0*(1 + FINDGEN(9)),100.0*(1 + FINDGEN(9)),1000.0*(1 + FINDGEN(9))]
					 range     = [1, 10000]
					 ticks     = 4
					 ctickname = ['1','10','10!U2!N','10!U3!N','10!U4!N']
					 title     = 'H!D2!NO (ppm)'
					 END
	'GRAUPEL'  : BEGIN
					 vsect     = vsect
					 table     = WHITE_BLUE_24(10,0.1)
					 levels    = [0.01,0.1,1.0,10.0,100.0,1000.0,10000.0,100000.0,1000000.0,10000000.0]
					 range     = [0.01, 100000000.0]
					 ticks     = 5
					 ctickname = ['0.01','1.0','10!U2!N','10!U4!N','10!U6!N','10!U8!N']
					 title     = 'Graupel Conc. (# L!U-1!N)'
					 END
	ELSE       : BEGIN
					 vsect  = 100.0*vsect
					 table  = [COLOR_24('white'),WHITE_BLUE_24(99, 0.15, 0.75)];[COLOR_24('white'), CYAN_MAGENTA_24(19)]
					 levels = FINDGEN(100)
					 range  = [0, 100]
					 ticks  = 4
					 title  = var + ' (' + vwrf.units + ')'
					 END
ENDCASE

epsfile = '~/' + var + '_' + date_string + '_section.eps'
pdffile = '~/' + var + '_' + date_string + '_section.pdf'
pngfile = '~/' + var + '_' + date_string + '_section.png'

IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN	
	PS_ON, FILENAME = epsfile, PAGE_SIZE = [10.0, 5.0], MARGIN = 0.0, /INCHES					;Switch to Postscript device
	DEVICE, /ENCAPSULATED
	!P.FONT     = 0																								;Hardware fonts
	!P.CHARSIZE = 1.25
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS																							;Load basic color definitions
	thick_scale = 3
ENDIF ELSE BEGIN
	SET_PLOT, 'X'
	WINDOW, XSIZE = 1200, YSIZE = 600																		;Open graphics window
	!P.COLOR      = COLOR_24('black')																		;Foreground color
	!P.BACKGROUND = COLOR_24('white')																		;Background color
	!P.CHARSIZE   = 2.0		
	!P.FONT       = -1																							;Use Hershey fonts
	thick_scale   = 1
ENDELSE

!P.MULTI = [0, 2, 1]																								;Set multiple plots

map_pos  = [0.025, 0.22, 0.375, 0.92]																		;Set map position
bar_pos  = [0.055, 0.11, 0.345, 0.13]																		;Set color bar position
xsn_pos  = [0.450, 0.22, 0.975, 0.92]																		;Set cross-section position
bar2_pos = [0.500, 0.11, 0.925, 0.13]																		;Set color bar position

y0 = y.values[          0 ,          0 ]																	;Set domain boundary points
y1 = y.values[          0 ,dim[1]-(1+0)]
y2 = y.values[dim[0]-(1+0),dim[1]-(1+0)]
y3 = y.values[dim[0]-(1+0),          0 ]
x0 = x.values[          0 ,          0 ]
x1 = x.values[          0 ,dim[1]-(1+0)]
x2 = x.values[dim[0]-(1+0),dim[1]-(1+0)]
x3 = x.values[dim[0]-(1+0),          0 ]

xc = INTERPOLATE(x.values, 0.5*(dim[0]-1), 0.5*(dim[1]-1))											;Get central grid point
yc = INTERPOLATE(y.values, 0.5*(dim[0]-1), 0.5*(dim[1]-1))

MAP_SET, yc, xc, 0, CONIC = 1, $																				;Draw map
	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
	ISOTROPIC = 1, $
	TITLE     = date_string, $
	POSITION  = map_pos

CASE map_var OF 
	'REFL' : BEGIN
				mtable   = NEXRAD_DBZ_COLOR_24()
				mlevels  = -30.0 + 5.0*FINDGEN(N_ELEMENTS(mtable))
				mrange   = [-30, 75]
				mticks   = 7
				mtitle   = 'Composite Reflectivity (dB)'
				map_plot = MAX(vmap.values, DIM = 3, /NAN)
				END
	'w'    : BEGIN
				mtable  = BLUE_RED_24(40, 0.0)
				mlevels = -5.0 + 0.25*FINDGEN(40)
				mrange  = [-5, 5]
				mticks  = 2
				mtitle  = 'Tropopause Vertical Velocity (m/s)'
	
				i   = REBIN(FINDGEN(nx), nx, ny)
				j   = REBIN(REFORM(FINDGEN(ny), 1, ny), nx, ny)
				k   = REBIN(REFORM(FINDGEN(nz), 1, 1, nz), nx, ny, nz)
				k   = MAX((ABS(Z.values - REBIN(trop.values,nx,ny,nz, /SAMPLE) LE 1.0))*k, DIM = 3)

				map_plot = (INTERPOLATE(vmap.values, i, j, k) > mrange[0])
				END
	'BL_tracer' : BEGIN
						mtable  = [COLOR_24(100,100,100),BLUE_RED_24(20, 0.05)]
						mlevels = [-20.0, -2.5 + 0.25*FINDGEN(20)]
						mrange  = [-2.5, 2.5]
						mticks  = 2
						mtitle  = 'BL Tracer > 0 Relative Altitude (km)'
	
						map_plot = (0.01*ROUND(100.0*vmap.values) GT 0)*(z.values)
						map_plot = 0.001*(MAX(map_plot, DIM=3, /NAN) - trop.values)
					  END
ENDCASE

CONTOUR, map_plot, x.values, y.values, $
	OVERPLOT = 1, $
	FILL     = 1, $
	C_COLOR  = mtable, $
	LEVELS   = mlevels

IF KEYWORD_SET(tracer) THEN mtable = mtable[1:*]

MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines

OPLOT, xsect, ysect, THICK = 2

IF (N_ELEMENTS(width) GT 0) THEN $
	OPLOT, [xsect0,REVERSE(xsect1),xsect0[0]], [ysect0,REVERSE(ysect1),ysect0[0]]

XYOUTS, xsect[0], ysect[0] + 0.025*[ysect[nsect-1] - ysect[0]], 'A', $
	ALIGN = 0.5, CHARSIZE = 1.5*!P.CHARSIZE, CHARTHICK = 2.0

XYOUTS, xsect[nsect-1], ysect[nsect-1] + 0.025*[ysect[nsect-1] - ysect[0]], 'B', $
	ALIGN = 0.5, CHARSIZE = 1.5*!P.CHARSIZE, CHARTHICK = 2.0

COLOR_BAR_24_KPB, mtable, OVER = mtable[-1], UNDER = mtable[0], $									;Draw color bar
	TICKS = mticks, $
	RANGE = mrange, $
	TITLE = mtitle, $
	POSIT = bar_pos

!P.POSITION = 0

ivsect = WHERE((vsect LT range[0]), nnan)
IF (nnan GT 0) THEN vsect[ivsect] = !Values.F_NaN

IF KEYWORD_SET(eps) THEN BEGIN
	ij0 = CONVERT_COORD(xsn_pos[0:1], /NORMAL, /TO_DEVICE)
	ij1 = CONVERT_COORD(xsn_pos[2:*], /NORMAL, /TO_DEVICE)

	xsize = LONG(ij1[0] - ij0[0])
	ysize = LONG(ij1[1] - ij0[1])

	image = IMAGE_24(COLOR_LOOKUP_24(((vsect < range[1])), table, MIN = range[0], $
				MAX = range[1], MISSING = COLOR_24(200, 200, 200), /NAN))
	TV, image, ij0[0], ij0[1], TRUE = 3, XSIZE = xsize, YSIZE = ysize, /DEVICE
ENDIF

CONTOUR, ((vsect < range[1])), FINDGEN(nsect), 0.001*zsect, NODATA = KEYWORD_SET(eps), $
	CELL_FILL= 1, $
	C_COLOR  = table, $
	LEVELS   = levels, $
	XRANGE   = [0, nsect -1], $
	XTICKS   = 1, $
	XTICKN   = ['A', 'B'], $
	XSTYLE   = 1, $
	YRANGE   = 0.001*[zmin, zmax], $
	YSTYLE   = 1, $
	YTITLE   = 'Altitude (km)', $
	TITLE    = 'Vertical Section', $
	POSITION = xsn_pos

CONTOUR, csect, FINDGEN(nsect), 0.001*zsect, $
	OVERPLOT = 1, $
	LEVELS   = 0.1, $
	C_COLOR  = COLOR_24('gray50'), $
	C_THICK  = 3*thick_scale

USERSYM_CIRCLE, /FILL

;IF (var EQ 'PV') THEN $
;	CONTOUR, thsect, FINDGEN(nsect), 0.001*zsect, $
;		OVERPLOT = 1, $
;		LEVELS   = 300 + FINDGEN(200), $;[310.0, 320.0, 330.0 + 10.0*FINDGEN(8), 420.0, 450.0, 500.0], $
;		C_THICK  = thick_scale; $
;ELSE $
;	OPLOT, 10*FINDGEN(nsect/10), 0.001*trpsect[0:*:10], PSYM = 8, SYMSIZE = 2.5 - KEYWORD_SET(eps)
OPLOT, FINDGEN(nsect), 0.001*trpsect, THICK = 2

IF KEYWORD_SET(winds) THEN BEGIN
;	CONTOUR, usect, FINDGEN(nsect), 0.001*zsect, LEVELS = 5*FINDGEN(20), FOLLOW = 1, C_THICK = 2, /OVER
;	VELOVECT, usect[0:*:wstride,2:*:wstride2], 0*((wsect[0:*:wstride,2:*:wstride2]) < MAX(usect, /NAN)), $
;		(FINDGEN(nsect))[0:*:wstride], REFORM(0.001*zsect[nsect-1,2:*:wstride2]), /OVERPLOT, $
;		LENGTH = 1.5, NOCLIP = 0;, COLOR = COLOR_24('red')
	usect = usect - 14.0
	VELOVECT, usect[0:*:wstride,2:*:wstride2], ((wsect[0:*:wstride,2:*:wstride2]) < MAX(ABS(usect), /NAN)), $
		(FINDGEN(nsect))[0:*:wstride], REFORM(0.001*zsect[nsect-1,2:*:wstride2]), /OVERPLOT, $
		LENGTH = 1.5, NOCLIP = 0;, COLOR = COLOR_24('red')
	
;	wlevels = [-1,-0.75,-0.5,-0.25,0.25,0.5,0.75,1.0]
;	CONTOUR, wsect, FINDGEN(nsect), 0.001*zsect, LEVELS = wlevels, C_COLOR = COLOR_24('red'), $
;		C_LINES = 2*(wlevels LT 0), C_THICK = 2*thick_scale, /OVER
ENDIF

COLOR_BAR_24_KPB, table,$;, UNDER = table[0], OVER = table[-1], $																						;Draw color bar
	TICKS = ticks, $
	RANGE = range, $
	TITLE = title, $
	POSIT = bar2_pos, $
	XTICKN = ctickname

!P.MULTI = 0																										;Reset multiple plots
!P.POSITION = 0

IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
	
	IF KEYWORD_SET(pdf) THEN PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS						;Convert PostScript to PDF
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file

END
