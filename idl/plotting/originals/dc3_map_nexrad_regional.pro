PRO DC3_MAP_NEXRAD_REGIONAL, flight_name, date, $
	HIGH_RESOLUTION = high_resolution, $
	ZRELATIVE       = zrelative, $
	DBZ_THRESH      = dbz_thresh, $
	ALTITUDE        = altitude, $
	TRACK           = track, $
	TRAJECTORIES    = trajectories, $
	FILTER          = filter, $
	CLUTTER         = clutter, $
	SMOOTH          = smooth, $
	FALCON          = falcon, $
	CONTRIBUTING    = contributing, $
	EPS             = eps, $
	PDF             = pdf, $
	PNG             = png, $
	GIF             = gif

;+
; Name:
;		DC3_MAP_NEXRAD_REGIONAL
; Purpose:
;		This is a procedure to map regional composites of NEXRAD radar data for
;		select DC3 flights. 
; Calling sequence:
;		DC3_MAP_NEXRAD_REGIONAL, flight_name
; Input:
;		flight_name : DC3 GV flight name. (e.g., 'rf02')
; Output:
;		Map of NEXRAD composite reflectivity.
; Keywords:
;		HIGH_RESOLUTION : If set, interpolate reflectivity image to 4x original grid.
;		ALTITUDE        : Optional keyword to specify a constant altitude for reflectivity map.
;								Default is composite reflectivity (column maximum). 
;		TRACK           : If set, overplot flight track on reflectivity map.
;		EPS             : If set, output to PostScript.
;		PNG             : If set, save PNG image. 
; Author and history:
;		Cameron R. Homeyer  2012-08-22.
;-

COMPILE_OPT IDL2																								;Set compile options

IF (N_ELEMENTS(zrelative ) EQ 0) THEN zrelative  = 0
IF (N_ELEMENTS(dbz_thresh) EQ 0) THEN dbz_thresh = 10.0

xgv = DC3_READ_VAR('GGLON', flight_name)																;Read aircraft coordinates
ygv = DC3_READ_VAR('GGLAT', flight_name)
tgv = DC3_READ_VAR('Time', flight_name)

xdc8 = DC3_READ_VAR('Longitude', flight_name, /DC8)												;Read aircraft coordinates
ydc8 = DC3_READ_VAR('Latitude',  flight_name, /DC8)
tdc8 = DC3_READ_VAR('Time',      flight_name, /DC8)

CASE flight_name OF
	'20120606' : BEGIN
					 xfal = DC3_READ_VAR('GPS_LON_NP', 'rf07', /FALCON)
					 yfal = DC3_READ_VAR('GPS_LAT_NP', 'rf07', /FALCON)
					 tfal = DC3_READ_VAR('Time',       'rf07', /FALCON)
					 END
	'20120611' : BEGIN
					 xfal = DC3_READ_VAR('GPS_LON_NP', ['rf10','rf11'], /FALCON)
					 yfal = DC3_READ_VAR('GPS_LAT_NP', ['rf10','rf11'], /FALCON)
					 tfal = DC3_READ_VAR('Time',       ['rf10','rf11'], /FALCON)
					 END
	ELSE       : BEGIN
					 xfal = -1
					 yfal = -1
					 tfal = -1
					 END
ENDCASE					 

IF KEYWORD_SET(falcon) THEN BEGIN
	CASE flight_name OF
		'20120529' : BEGIN
						 xfal = DC3_READ_VAR('GPS_LON_NP', ['rf01','rf02'], /FALCON)
						 yfal = DC3_READ_VAR('GPS_LAT_NP', ['rf01','rf02'], /FALCON)
						 tfal = DC3_READ_VAR('Time',       ['rf01','rf02'], /FALCON)
						 END
		'20120530' : BEGIN
						 xfal = DC3_READ_VAR('GPS_LON_NP', ['rf03','rf04'], /FALCON)
						 yfal = DC3_READ_VAR('GPS_LAT_NP', ['rf03','rf04'], /FALCON)
						 tfal = DC3_READ_VAR('Time',       ['rf03','rf04'], /FALCON)
						 END
		'20120605' : BEGIN
						 xfal = DC3_READ_VAR('FMC_LON_NP', ['rf05','rf06'], /FALCON)
						 yfal = DC3_READ_VAR('FMC_LAT_NP', ['rf05','rf06'], /FALCON)
						 tfal = DC3_READ_VAR('Time',       ['rf05','rf06'], /FALCON)
						 END
		'20120612' : BEGIN
						 xfal = DC3_READ_VAR('GPS_LON_NP', 'rf12', /FALCON)
						 yfal = DC3_READ_VAR('GPS_LAT_NP', 'rf12', /FALCON)
						 tfal = DC3_READ_VAR('Time',       'rf12', /FALCON)
						 END
		ELSE       : MESSAGE, 'Not a Falcon-only flight!'	
	ENDCASE
	
	flight_name = flight_name + '_falcon'
ENDIF

indir = !DC3_DATA + 'nexrad/' + flight_name + '/'													;Set regional composite input directory
rfile = FILE_SEARCH(indir + '*.nc', COUNT = nfile)

IF (N_ELEMENTS(date) GT 0) THEN BEGIN
	date_string = MAKE_ISO_DATE_STRING(date, PREC = 'MINUTE', /COMPACT, /UTC)
	rfile       = indir + date_string + '.nc'
	nfile       = 1
ENDIF

IF KEYWORD_SET(trajectories) THEN BEGIN
	CASE flight_name OF
		'20120519' : trajs = TRAJ3D_READ_FILE_3('/Users/chomeyer/data/dc3/traj3d/rf02/backward_combo.nc')
		'20120530' : trajs = TRAJ3D_READ_FILE_3('/Users/chomeyer/data/dc3/traj3d/rf07/backward_combo_dc8.nc')
		ELSE       : trajs = -1
	ENDCASE
ENDIF

outdir = !WRF_DIRECTORY + '/test/plots/'																					;Set output directory for images
FILE_MKDIR, outdir																							;Create output directory, if necessary

loop0 = 0
FOR i = loop0, nfile-1 DO BEGIN;loop0 DO BEGIN;
	IF (zrelative GT 0) THEN $
		table = BLUE_GRAY_RED_24(40, 19, 0.28) ELSE $
		table = NEXRAD_DBZ_COLOR_24()																		;Set color table

	tv_table = table
	
	data        = NEXRAD_READ_LEVEL2_2(rfile[i])														;Read data NEXRAD_READ_LEVEL2(INFILE = rfile[i])
	date_string = STRMID(rfile[i], 16, 14, /REVERSE_OFFSET)										;Create date string
	date        = READ_ISO_DATE_STRING(date_string)

	IF KEYWORD_SET(filter) THEN BEGIN
		ifilter = WHERE((data.dbz.wvalues LT 0.33), nfilter)
		IF (nfilter GT 0) THEN data.dbz.values[ifilter] = !Values.F_NaN
	ENDIF

	IF KEYWORD_SET(clutter) THEN data = NEXRAD_REMOVE_CLUTTER(data)							;Remove clutter data

	IF (flight_name EQ '20120530') THEN BEGIN
		i0 = INDEX_OF_NEAREST(data.x.values, 258.25)
		i1 = INDEX_OF_NEAREST(data.x.values, 265.75)
		j0 = INDEX_OF_NEAREST(data.y.values,  35.00)
		j1 = INDEX_OF_NEAREST(data.y.values,  40.14)
		
		x   = {values : (data.x.values)[i0:i1], long_name : data.x.long_name, $
				 units  : data.x.units, n : i1-i0+1}		
		y   = {values : (data.y.values)[j0:j1], long_name : data.y.long_name, $
				 units  : data.y.units, n : j1-j0+1}		
		dbz = {values : (data.dbz.values)[i0:i1,j0:j1,*], long_name : data.dbz.long_name, $
				 units  : data.dbz.units, n : N_ELEMENTS((data.dbz.values)[i0:i1,j0:j1,*])}
		
		data = {dbz : dbz, x : x, y : y, z : data.z, nbeams : data.nbeams}
	ENDIF


	IF KEYWORD_SET(smooth) THEN FOR k = 0, data.z.n -1 DO $
		data.dbz.values[*,*,k] = GAUSS_SMOOTH((data.dbz.values)[*,*,k], [1.5,1.5], /NAN, /EDGE_TRUNCATE)

	epsfile = outdir + STRTRIM(i,2) + '.eps'															;Set image filenames
	pdffile = outdir + STRTRIM(i,2) + '.pdf'
	pngfile = outdir + STRTRIM(i,2) + '.png'
	giffile = outdir + date_string + '.gif'
	
	IF ~KEYWORD_SET(eps) AND ~KEYWORD_SET(pdf) AND (i EQ loop0) THEN BEGIN
		SET_PLOT, 'X'
;		WINDOW, XSIZE = LONG(3.75*data.x.n), YSIZE = LONG(1.2*2.5*data.y.n)							;Open graphics window
		WINDOW, XSIZE = LONG(2*data.x.n), YSIZE = LONG(1.2*2*data.y.n)							;Open graphics window
		!P.COLOR      = COLOR_24('black')																;Foreground color
		!P.BACKGROUND = COLOR_24('white')																;Background color
		!P.CHARSIZE   = 1.5		
		!P.FONT       = -1																					;Use Hershey fonts
		thick_scale   = 1
	ENDIF

	IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
		PS_ON, FILENAME = epsfile, MARGIN = 0.0, $
			PAGE_SIZE = [data.x.n/50.0, data.y.n/50.0 + 0.2*data.y.n/50.0], /INCHES			;Switch to Postscript device
		DEVICE, /ENCAPSULATED
		!P.FONT     = 0																						;Hardware fonts
		!P.CHARSIZE = 1.5
		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
			LOAD_BASIC_COLORS																					;Load basic color definitions
		thick_scale   = 3
	ENDIF
	!P.THICK = thick_scale
	!X.THICK = thick_scale
	!Y.THICK = thick_scale
	
	x0 = MIN(data.x.values)																					;Extract region limits
	x1 = MAX(data.x.values)
	y0 = MIN(data.y.values)
	y1 = MAX(data.y.values)
	
	map_pos = [0.05, 0.15, 0.95, 0.95]																	;Set map position
	bar_pos = [0.25, 0.12, 0.75, 0.14]																	;Set color bar position
	
	IF (zrelative GT 0) THEN BEGIN
		min_value = -4.0
		max_value =  4.0
		
		trop = DC3_READ_GFS_HIRES(flight_name, date)
		
		IF (flight_name EQ '20120530') THEN $
			trop = {z1 : {values : (trop.z1.values)[i0:i1,j0:j1], long_name : trop.z1.long_name, units : trop.z1.units}, $
					  z2 : {values : (trop.z2.values)[i0:i1,j0:j1], long_name : trop.z2.long_name, units : trop.z2.units}, $
					  z3 : {values : (trop.z3.values)[i0:i1,j0:j1], long_name : trop.z3.long_name, units : trop.z3.units}, $
					  x  : {values : (trop.x.values )[i0:i1      ], long_name : trop.x.long_name,  units : trop.x.units }, $
					  y  : {values : (trop.y.values )[j0:j1      ], long_name : trop.y.long_name,  units : trop.y.units }  }

		
		ibad = WHERE(~FINITE(data.dBZ.values), bcount)
		IF (bcount GT 0) THEN data.dBZ.values[ibad] = -33.0
		
		zgtv_alt = MAX((data.dBZ.values GE dbz_thresh)*REBIN(REFORM(data.z.values, 1, 1, data.z.n), $
						 data.x.n, data.y.n, data.z.n, /SAMPLE), DIM = 3)
				
		i0 = WHERE((zgtv_alt EQ 0.0), n0)

		k  = REFORM(INDEX_OF_NEAREST_CRH(data.z.values, zgtv_alt),   data.x.n, data.y.n)
		iz = REBIN(       FINDGEN(data.x.n),               data.x.n, data.y.n, /SAMPLE)
		jz = REBIN(REFORM(FINDGEN(data.y.n), 1, data.y.n), data.x.n, data.y.n, /SAMPLE)
		
		wt       = (dbz_thresh               - data.dbz.values[iz,jz,k+1])/$
					  (data.dbz.values[iz,jz,k] - data.dbz.values[iz,jz,k+1])
							  			  
		dbz_plot = (1 - wt)*data.z.values[k+1] + $
					  (    wt)*data.z.values[k  ]
				
		IF (flight_name EQ '20120530') THEN BEGIN
			ihigh = WHERE((trop.z1.values GT 12.5), nhigh)
			IF (nhigh GT 0) THEN trop.z1.values[ihigh] = 12.0
		ENDIF
		
		IF (SIZE(trop, /TNAME) EQ 'STRUCT') THEN BEGIN
			CASE zrelative OF 
				1 : BEGIN
					 dbz_plot   = (((dbz_plot - trop.z1.values) < max_value) > min_value)			;Compute relative altitude
					 trop_title = '1st'																				;Set tropopause title string
					 END
				2 : BEGIN
					 dbz_plot   = (((dbz_plot - trop.z2.values) < max_value) > min_value)
					 trop_title = '2nd'
					 END
				3 : BEGIN
					 dbz_plot   = (((dbz_plot - trop.z3.values) < max_value) > min_value)
					 trop_title = '3rd'
					 END
				ELSE : BEGIN
					 dbz_plot[*] = !Values.F_NaN
					 trop_title  = 'Invalid'
					 END
			ENDCASE
		ENDIF

		IF (n0 GT 0) THEN dbz_plot[i0] = !Values.F_NaN

		title0   = 'NEXRAD >' + STRTRIM(LONG(dbz_thresh),2) + ' dBZ Reflectivity Relative to GFS ' + $	;Set plot title
					  trop_title + ' Trop'
		
		barticks = 4																							;Set color bar chars
		bartitle = 'Relative Altitude (km)'
	ENDIF ELSE IF (N_ELEMENTS(altitude) GT 0) THEN BEGIN
		tv_table  = table[8:*]
		min_value = 10.0
		max_value = 75.0
		
		k        = INDEX_OF_NEAREST(data.z.values, altitude)
		dbz_plot = ((data.dBZ.values)[*,*,k] < 75.0)
		title0   = 'NEXRAD ' + STRING((data.z.values)[k], FORMAT="(F4.1)") + $
					  ' km Reflectivity'
	
		barticks = 7
		bartitle = 'Reflectivity (dBZ)'
	ENDIF ELSE IF KEYWORD_SET(contributing) THEN BEGIN
		min_value = 0.0
		max_value = 2.0
		table     = BLUE_RED_24(8)
		tv_table  = table
		
		dbz_plot = (((data.z1_cont - data.z0_cont)/data.nbeams) < max_value)
		title0   = 'Contributing NEXRAD Beam Sampling'

		barticks = 4
		bartitle = 'Vertical Sampling (km)'
	ENDIF ELSE BEGIN
		tv_table  = table[8:*]
		min_value = 10.0
		max_value = 75.0
		
		dbz_plot = (MAX(data.dBZ.values, DIMENSION = 3, /NAN) < 75.0)
		title0   = 'NEXRAD Composite Reflectivity'

		barticks = 7
		bartitle = 'Reflectivity (dBZ)'
	ENDELSE

	MAP_SET, LIMIT = [y0,x0,y1,x1], /ISOTROPIC, POSITION = map_pos, TITLE = title0 + ' valid ' + date_string					;Set Map
;	MAP_SET, LIMIT = [y0,FLOOR(x0) -1,y1,CEIL(x1)+1.5], /ISOTROPIC, POSITION = map_pos, $								;Set map
;		TITLE = title0 + ' valid ' + date_string

;print, y0,FLOOR(x0) -1,y1,CEIL(x1)+1.5

;	IF KEYWORD_SET(gif) THEN BEGIN
;		xy0 = CONVERT_COORD(0.0,0.0,/NORMAL,/TO_DATA)
;		xy2 = CONVERT_COORD(1.0,1.0,/NORMAL,/TO_DATA)
;		
;		PRINT, xy0, xy2
;	ENDIF

	IF KEYWORD_SET(high_resolution) THEN BEGIN
		ix       = 0.25*FINDGEN(4*(data.x.n-1) + 1)
		iy       = 0.25*FINDGEN(4*(data.y.n-1) + 1)
		dbz_plot = INTERPOLATE(dbz_plot, ix, iy, /GRID)
	ENDIF
	
	IF KEYWORD_SET(gif) THEN cmiss = COLOR_24('white') $
							  ELSE cmiss = COLOR_24(100,100,100)

	TV_MAP_IMAGE, tv_table, dbz_plot, $
		LATMIN    = y0, $
		LATMAX    = y1, $
		LONMIN    = x0, $
		LONMAX    = x1, $
		MIN_VALUE = min_value, $
		MAX_VALUE = max_value, $
		MISSING   = !Values.F_NaN, $
		CMISSING  = cmiss
	
	IF ~KEYWORD_SET(gif) THEN MAP_CONTINENTS, /USA, /HIRES											;Draw continents
	
	IF KEYWORD_SET(trajectories) THEN BEGIN		
		IF (N_ELEMENTS(trajs) GT 0) THEN BEGIN
			USERSYM_CIRCLE, /FILL
			
			it0 = WHERE((TIME_DIFF(trajs.date, date) EQ 0), n0)
						
			IF (n0) THEN BEGIN
				PLOTS, (trajs.x)[*,it0], (trajs.y)[*,it0], PSYM = 8, SYMSIZE = 3 
				
				FOR ip = 0, trajs.np -1 DO $
						OPLOT, (trajs.x)[ip,0:it0], (trajs.y)[ip,0:it0], COLOR = COLOR_24('yellow'), THICK = 2
			ENDIF ELSE BEGIN
				itn = WHERE((TIME_DIFF(trajs.date, date) LT 0), nn)
				itp = WHERE((TIME_DIFF(trajs.date, date) GT 0), np)
								
				IF (nn GT 0) AND (np GT 0) THEN BEGIN
					xt = 0.5*(trajs.x)[*,itn[0]] + 0.5*(trajs.x)[*,itp[np-1]]
					yt = 0.5*(trajs.y)[*,itn[0]] + 0.5*(trajs.y)[*,itp[np-1]]
					
					PLOTS, xt, yt, PSYM = 8, SYMSIZE = 3
					
					FOR ip = 0, trajs.np -1 DO $
						OPLOT, [xt,REFORM((trajs.x)[ip,itp])], [yt,REFORM((trajs.y)[ip,itp])], COLOR = COLOR_24('yellow'), THICK = 2
				ENDIF
			ENDELSE
		ENDIF
	ENDIF
	
	IF KEYWORD_SET(track) THEN BEGIN
		IF (SIZE(tgv, /TNAME) EQ 'STRUCT') AND ~KEYWORD_SET(falcon) THEN BEGIN
			dtgv = TIME_DIFF(tgv.values, date)
			igv  = WHERE(((dtgv GE -150) AND (dtgv LE 150)), ngv)
			igv0 = WHERE((dtgv EQ 0), npsym)
			
			IF (dtgv[0] LE 0) THEN BEGIN
				IF (ngv GT 0) THEN BEGIN
					OPLOT, (xgv.values)[0:igv[0]], (ygv.values)[0:igv[0]], THICK = 5*thick_scale/3, COLOR = COLOR_24('white')
					OPLOT, (xgv.values)[  igv   ], (ygv.values)[  igv   ], THICK = 5*thick_scale/3
					
					IF (npsym) AND (igv0 GT 0) THEN BEGIN
						dx = (xgv.values)[igv0] - (xgv.values)[(igv0-30) > 0]							;Compute coordinate lengths for past 30 seconds										
						dy = (ygv.values)[igv0] - (ygv.values)[(igv0-30) > 0]
						d  = SQRT(dx^2 + dy^2)
						po = ACOS(dy/d) + 2.0*(!Pi - ACOS(dy/d))*(dx LT 0)
						
						USERSYM_PLANE, /FILL, ORIENTATION = po[0]/!DDTOR									;Load plane symbol at flight path orientation
						
						PLOTS, (xgv.values)[igv0], (ygv.values)[igv0], $								;Overplot plane symbol
							PSYM    = 8, $
							SYMSIZE = 8 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
							NOCLIP  = 0
					ENDIF
				ENDIF ELSE $
					OPLOT, xgv.values, ygv.values, THICK = 5*thick_scale/3, COLOR = COLOR_24('white')
			ENDIF
		ENDIF
		
		IF (SIZE(tdc8, /TNAME) EQ 'STRUCT') AND ~KEYWORD_SET(falcon) THEN BEGIN	
			dtdc8 = TIME_DIFF(tdc8.values, date)
			idc8  = WHERE(((dtdc8 GE -150) AND (dtdc8 LE 150)), ndc8)
			idc80 = WHERE((dtdc8 EQ 0), npsym)
			
			IF (dtdc8[0] LE 0) THEN BEGIN
				IF (ndc8 GT 0) THEN BEGIN
					OPLOT, (xdc8.values)[0:idc8[0]], (ydc8.values)[0:idc8[0]], THICK = 5*thick_scale/3, COLOR = COLOR_24('black')
					OPLOT, (xdc8.values)[  idc8   ], (ydc8.values)[  idc8   ], THICK = 5*thick_scale/3, COLOR = COLOR_24('white')
					
					IF (npsym) AND (idc80 GT 0) THEN BEGIN
						dx = (xdc8.values)[idc80] - (xdc8.values)[(idc80-30) > 0]							;Compute coordinate lengths for past 30 seconds										
						dy = (ydc8.values)[idc80] - (ydc8.values)[(idc80-30) > 0]
						d  = SQRT(dx^2 + dy^2)
						po = ACOS(dy/d) + 2.0*(!Pi - ACOS(dy/d))*(dx LT 0)
						
						USERSYM_PLANE, /FILL, ORIENTATION = po[0]/!DDTOR									;Load plane symbol at flight path orientation
						
						PLOTS, (xdc8.values)[idc80], (ydc8.values)[idc80], $								;Overplot plane symbol
							PSYM    = 8, $
							SYMSIZE = 8 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
							NOCLIP  = 0, $
							COLOR   = COLOR_24('white')
					ENDIF
				ENDIF ELSE $
					OPLOT, xdc8.values, ydc8.values, THICK = 5*thick_scale/3, COLOR = COLOR_24('black')
			ENDIF
		ENDIF

		IF (SIZE(tfal, /TNAME) EQ 'STRUCT') THEN BEGIN	
			dtfal = TIME_DIFF(tfal.values, date)
			ifal  = WHERE(((dtfal GE -150) AND (dtfal LE 150)), nfal)
			ifal0 = WHERE((dtfal EQ 0), npsym)
			ile0  = WHERE((dtfal LE 0), nle0)
			
			IF (dtfal[0] LE 0) THEN BEGIN
				IF (nfal GT 0) THEN BEGIN
					OPLOT, (xfal.values)[0:ifal[0]], (yfal.values)[0:ifal[0]], THICK = 5*thick_scale/3, COLOR = COLOR_24(153, 102,  51)
					OPLOT, (xfal.values)[  ifal   ], (yfal.values)[  ifal   ], THICK = 5*thick_scale/3, COLOR = COLOR_24(255, 204, 153)
					
					IF (npsym) AND (ifal0 GT 0) THEN BEGIN
						dx = (xfal.values)[ifal0] - (xfal.values)[(ifal0-30) > 0]							;Compute coordinate lengths for past 30 seconds										
						dy = (yfal.values)[ifal0] - (yfal.values)[(ifal0-30) > 0]
						d  = SQRT(dx^2 + dy^2)
						po = ACOS(dy/d) + 2.0*(!Pi - ACOS(dy/d))*(dx LT 0)
						
						USERSYM_PLANE, /FILL, ORIENTATION = po[0]/!DDTOR									;Load plane symbol at flight path orientation
						
						PLOTS, (xfal.values)[ifal0], (yfal.values)[ifal0], $								;Overplot plane symbol
							PSYM    = 8, $
							SYMSIZE = 7 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
							NOCLIP  = 0, $
							COLOR   = COLOR_24(255, 204, 153)
					ENDIF
				ENDIF ELSE $
					OPLOT, (xfal.values)[ile0], (yfal.values)[ile0], THICK = 5*thick_scale/3, COLOR = COLOR_24(153, 102,  51)
			ENDIF
		ENDIF

		IF (SIZE(tfal, /TNAME) EQ 'STRUCT') AND ~KEYWORD_SET(falcon) THEN BEGIN
			POLYFILL, [0.825, 0.935, 0.935, 0.825, 0.825], [0.7975, 0.7975, 0.91, 0.91, 0.7975], COLOR = COLOR_24(100,100,100), /NORMAL
			PLOTS,    [0.825, 0.935, 0.935, 0.825, 0.825], [0.7975, 0.7975, 0.91, 0.91, 0.7975], THICK = thick_scale, /NORMAL
			XYOUTS, 0.83, 0.88, 'GV', /NORMAL
			PLOTS, [0.90, 0.93], [0.8875, 0.8875], COLOR = COLOR_24('white'), /NORMAL, THICK = 5*thick_scale/3
			XYOUTS, 0.83, 0.85, 'DC8', COLOR = COLOR_24('white'), /NORMAL
			PLOTS, [0.90, 0.93], [0.8575, 0.8575], /NORMAL, THICK = 5*thick_scale/3
			XYOUTS, 0.83, 0.82, 'Falcon', COLOR = COLOR_24(255, 204, 153), /NORMAL
			PLOTS, [0.90, 0.93], [0.8275, 0.8275], COLOR = COLOR_24(153, 102,  51), /NORMAL, THICK = 5*thick_scale/3
		ENDIF ELSE IF KEYWORD_SET(falcon) THEN BEGIN
			POLYFILL, [0.805, 0.935, 0.935, 0.805, 0.805], [0.8725, 0.8725, 0.91, 0.91, 0.8725], COLOR = COLOR_24(100,100,100), /NORMAL
			PLOTS,    [0.805, 0.935, 0.935, 0.805, 0.805], [0.8725, 0.8725, 0.91, 0.91, 0.8725], THICK = thick_scale, /NORMAL
			XYOUTS, 0.81, 0.88, 'Falcon', COLOR = COLOR_24(255, 204, 153), /NORMAL
			PLOTS, [0.90, 0.93], [0.8875, 0.8875], COLOR = COLOR_24(153, 102,  51), /NORMAL, THICK = 5*thick_scale/3
		ENDIF ELSE BEGIN
			POLYFILL, [0.825, 0.935, 0.935, 0.825, 0.825], [0.835, 0.835, 0.91, 0.91, 0.835], COLOR = COLOR_24(100,100,100), /NORMAL
			PLOTS,    [0.825, 0.935, 0.935, 0.825, 0.825], [0.835, 0.835, 0.91, 0.91, 0.835], THICK = thick_scale, /NORMAL
			XYOUTS, 0.83, 0.88, 'GV', /NORMAL
			PLOTS, [0.90, 0.93], [0.8875, 0.8875], COLOR = COLOR_24('white'), /NORMAL, THICK = 5*thick_scale/3
			XYOUTS, 0.83, 0.85, 'DC8', COLOR = COLOR_24('white'), /NORMAL
			PLOTS, [0.90, 0.93], [0.8575, 0.8575], /NORMAL, THICK = 5*thick_scale/3
		ENDELSE
	ENDIF
	
;	MAP_SET, LIMIT = [y0,x0,y1,x1], /ISOTROPIC, POSITION = map_pos, $							;Set map
;		TITLE = title0 + ' valid ' + date_string, /NOERASE
	
	XYOUTS, 0.95, 0.02, 'Cameron R. Homeyer, University of Oklahoma (chomeyer@ou.edu)', ALIGN = 1.0, /NORMAL, CHARSIZE = 0.75*!P.CHARSIZE
	
	IF KEYWORD_SET(clutter) AND ~KEYWORD_SET(zrelative) THEN BEGIN
		table     = (NEXRAD_DBZ_COLOR_24())[6:*]														;Set color bar table
		min_value = 0																							;Set color bar range
		barticks  = 5																							;Set number of color bar ticks
	ENDIF

	IF KEYWORD_SET(zrelative) THEN $
		COLOR_BAR_24_KPB, table, OVER = table[-1], UNDER = table[0], $							;Draw color bar
			TICKS = barticks, $
			RANGE = [min_value, max_value], $
			TITLE = bartitle, $
			POSIT = bar_pos ELSE $
		COLOR_BAR_24_KPB, table, OVER = table[-1], $													;Draw color bar
			TICKS = barticks, $
			RANGE = [min_value, max_value], $
			TITLE = bartitle, $
			POSIT = bar_pos
	
	IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
			LOAD_BASIC_COLORS, /RESET																		;Reset color table to linear ramp
		PS_OFF																									;Turn PS off
		IF KEYWORD_SET(pdf) THEN $
			PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS
	ENDIF ELSE IF KEYWORD_SET(png) THEN $
		WRITE_PNG, pngfile, TVRD(TRUE = 1) $															;Write PNG
	ELSE IF KEYWORD_SET(gif) THEN BEGIN
		image = TVRD(TRUE=3)
		color = COLOR_24(image[*,*,0], image[*,*,1], image[*,*,2])
		isort = SORT(color)
		iuniq = UNIQ(color[isort])
		nu    = N_ELEMENTS(iuniq)
		
		comp = COMPONENT_24(color[isort[iuniq]])
		irgb = BYTARR(SIZE(color,/DIM))
		FOR iu = 0, nu -1 DO irgb[WHERE(color EQ color[isort[iuniq[iu]]])] = iu
		
		itrans = WHERE((color[isort[iuniq]] EQ COLOR_24('white')))
		
		WRITE_GIF, giffile, irgb, REFORM(comp[*,0]), REFORM(comp[*,1]), REFORM(comp[*,2]), $								;Write PNG
			TRANSPARENT = itrans
	ENDIF

	!P.THICK    = 0
	!X.THICK    = 0
	!Y.THICK    = 0
	!P.POSITION = 0
ENDFOR

END


