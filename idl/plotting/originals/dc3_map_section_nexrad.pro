PRO DC3_MAP_SECTION_NEXRAD_DP, flight_name, $
	DC8             = dc8, $
	FALCON          = falcon, $
	HIGH_RESOLUTION = high_resolution, $
	CLUTTER         = clutter, $
	EPS             = eps, $
	PDF             = pdf, $
	PNG             = png

;+
; Name:
;		DC3_MAP_SECTION_NEXRAD
; Purpose:
;		This is a procedure to map composites of NEXRAD radar data and draw
;		aircraft-relative vertical cross-sections for select DC3 flights. 
; Calling sequence:
;		DC3_MAP_SECTION_NEXRAD, flight_name
; Input:
;		flight_name : DC3 GV flight name. (e.g., 'rf02')
; Output:
;		Map and sections of NEXRAD composite reflectivity.
; Keywords:
;		HIGH_RESOLUTION : If set, interpolate reflectivity image to 4x original grid.
;		EPS             : If set, output to PostScript.
;		PNG             : If set, save PNG image. 
; Author and history:
;		Cameron R. Homeyer  2012-08-22.
;-

COMPILE_OPT IDL2																								;Set compile options

IF (N_ELEMENTS(flight_name) EQ 0) THEN flight_name = '20120519'

IF KEYWORD_SET(dc8) THEN BEGIN
	xplane = DC3_READ_VAR('Longitude',    flight_name, /DC8)										;Read aircraft coordinates
	yplane = DC3_READ_VAR('Latitude',     flight_name, /DC8)
	zplane = DC3_READ_VAR('GPS_Altitude', flight_name, /DC8)
	tplane = DC3_READ_VAR('Time',         flight_name, /DC8)
	
	pstring = '_dc8'
	pcolor1 = COLOR_24('white')
	pcolor2 = COLOR_24('black')
ENDIF ELSE IF KEYWORD_SET(falcon) THEN BEGIN
	CASE flight_name OF
		'20120529' : BEGIN
						 xplane = DC3_READ_VAR('GPS_LON_NP', ['rf01','rf02'], /FALCON)
						 yplane = DC3_READ_VAR('GPS_LAT_NP', ['rf01','rf02'], /FALCON)
						 zplane = DC3_READ_VAR('GPS_ALT_NP', ['rf01','rf02'], /FALCON)
						 tplane = DC3_READ_VAR('Time',       ['rf01','rf02'], /FALCON)
						 END
		'20120530' : BEGIN
						 xplane = DC3_READ_VAR('GPS_LON_NP', ['rf03','rf04'], /FALCON)
						 yplane = DC3_READ_VAR('GPS_LAT_NP', ['rf03','rf04'], /FALCON)
						 zplane = DC3_READ_VAR('GPS_ALT_NP', ['rf03','rf04'], /FALCON)
						 tplane = DC3_READ_VAR('Time',       ['rf03','rf04'], /FALCON)
						 END
		'20120605' : BEGIN
						 xplane = DC3_READ_VAR('FMC_LON_NP', ['rf05','rf06'], /FALCON)
						 yplane = DC3_READ_VAR('FMC_LAT_NP', ['rf05','rf06'], /FALCON)
						 zplane = DC3_READ_VAR('H',          ['rf05','rf06'], /FALCON)
						 tplane = DC3_READ_VAR('Time',       ['rf05','rf06'], /FALCON)
						 END
		'20120606' : BEGIN
						 xplane = DC3_READ_VAR('GPS_LON_NP', 'rf07', /FALCON)
						 yplane = DC3_READ_VAR('GPS_LAT_NP', 'rf07', /FALCON)
						 zplane = DC3_READ_VAR('GPS_ALT_NP', 'rf07', /FALCON)
						 tplane = DC3_READ_VAR('Time',       'rf07', /FALCON)
						 END
		'20120611' : BEGIN
						 xplane = DC3_READ_VAR('GPS_LON_NP', ['rf10','rf11'], /FALCON)
						 yplane = DC3_READ_VAR('GPS_LAT_NP', ['rf10','rf11'], /FALCON)
						 zplane = DC3_READ_VAR('GPS_ALT_NP', ['rf10','rf11'], /FALCON)
						 tplane = DC3_READ_VAR('Time',       ['rf10','rf11'], /FALCON)
						 END
		'20120612' : BEGIN
						 xplane = DC3_READ_VAR('GPS_LON_NP', 'rf12', /FALCON)
						 yplane = DC3_READ_VAR('GPS_LAT_NP', 'rf12', /FALCON)
						 zplane = DC3_READ_VAR('GPS_ALT_NP', 'rf12', /FALCON)
						 tplane = DC3_READ_VAR('Time',       'rf12', /FALCON)
						 END
		ELSE       : MESSAGE, 'Not a Falcon-only flight!'	
	ENDCASE
	
	pstring = '_falcon'
	pcolor1 = COLOR_24(255, 204, 153)
	pcolor2 = COLOR_24(153, 102,  51)

	IF ((flight_name NE '20120606') AND (flight_name NE '20120611')) THEN $
		flight_name = flight_name + '_falcon'
ENDIF ELSE BEGIN
	xplane = DC3_READ_VAR('GGLON', flight_name)														;Read aircraft coordinates
	yplane = DC3_READ_VAR('GGLAT', flight_name)
	zplane = DC3_READ_VAR('GGALT', flight_name)
	tplane = DC3_READ_VAR('Time',  flight_name)

	pstring = '_gv'
	pcolor1 = COLOR_24('black')
	pcolor2 = COLOR_24('white')
ENDELSE

indir = !DC3_DATA + 'nexrad/' + flight_name + '/'													;Set regional composite input directory
rfile = FILE_SEARCH(indir + '*.nc', COUNT = nfile)

date_string = STRMID(rfile, 16, 14, /REVERSE_OFFSET)												;Create date strings
rdate       = READ_ISO_DATE_STRING(date_string)
rdt         = TIME_DIFF(rdate[1], rdate[0])															;Get radar analysis time step

iplt  = WHERE((tplane.values.second EQ 0), nplot)													;Find whole minites in flight path
tplot = (tplane.values)[iplt]																				;Get plot times

it0 = LONARR(nplot)
FOR i = 0, nplot -1 DO $
	it0[i] = (WHERE((TIME_DIFF(tplot[i], rdate) LE 0)))[0] -1									;Get index of initial radar date

dx         = 1.0																								;Set cross-section resolution in km
dxsect     = 200.0																							;Set cross-section width in km
dxsect_arc = dxsect/(0.001*!Earth.aEe)																	;Compute cross-section arc width
nsect      = LONG(dxsect/dx) + 1																			;Compute number of elements in cross-section
xsect      = -0.5*dxsect + dx*FINDGEN(nsect)

z0     =  0.00																									;Set cross-section altitude parameters
z1     = 20.00
dzsect =  0.25
nzsect = LONG((z1-z0)/dzsect) + 1
zsect  = z0 + dzsect*FINDGEN(nzsect)

outdir = !WRF_DIRECTORY + '/test/plots/'															;Set output directory for images
FILE_MKDIR, outdir																							;Create output directory, if necessary

bar_pos  = [0.58, 0.705, 0.90, 0.720]																	;Set plot positions
key_pos  = [0.55, 0.550, 0.95, 0.950]
lrsn_pos = [0.05, 0.075, 0.45, 0.450]
rfsn_pos = [0.55, 0.075, 0.95, 0.450]

FOR i = 0, nplot -1 DO BEGIN
	table  = NEXRAD_DBZ_COLOR_24()																		;Set color table
	
	IF (i EQ 0) THEN BEGIN
		data1 = NEXRAD_READ_LEVEL2(INFILE = rfile[it0[i]])											;Read radar data
		data2 = NEXRAD_READ_LEVEL2(INFILE = rfile[it0[i]+1])
		
		trop1 = DC3_READ_GFS_HIRES(flight_name, rdate[it0[i]  ])									;Read tropopause
		trop2 = DC3_READ_GFS_HIRES(flight_name, rdate[it0[i]+1])
		
		x0 = MIN(data1.x.values)																			;Extract region limits
		x1 = MAX(data1.x.values)
		y0 = MIN(data1.y.values)
		y1 = MAX(data1.y.values)
	ENDIF ELSE IF (it0[i] NE itlast) THEN BEGIN
		data1 = data2																							;Move radar data
		data2 = NEXRAD_READ_LEVEL2(INFILE = rfile[it0[i]+1])										;Read radar data

		trop1 = trop2																							;Move tropopause
		trop2 = DC3_READ_GFS_HIRES(flight_name, rdate[it0[i]+1])									;Read tropopause
	ENDIF
	
	data            = data1																					;Copy data structure
	weight          = TIME_DIFF(tplot[i], rdate[it0[i]])/FLOAT(rdt)							;Compute analysis weight
	data.dbz.values = (1.0 - weight)*data1.dbz.values + weight*data2.dbz.values			;Store weighted mean of neighboring reflectivity analyses
	ptrop           = (1.0 - weight)*trop1.z1.values + weight*trop2.z1.values
	strop           = (1.0 - weight)*trop1.z2.values + weight*trop2.z2.values
	
	IF KEYWORD_SET(clutter) THEN $
		data = NEXRAD_REMOVE_CLUTTER(data)																;Remove clutter

	xsym =       (xplane.values)[iplt[i]]																;Get coordinates of plane
	ysym =       (yplane.values)[iplt[i]]
	zsym = 0.001*(zplane.values)[iplt[i]]
	
	dx = (xplane.values)[iplt[i]] - (xplane.values)[(iplt[i]-30) > 0]							;Compute coordinate lengths for past 30 seconds										
	dy = (yplane.values)[iplt[i]] - (yplane.values)[(iplt[i]-30) > 0]
	d  = SQRT(dx^2 + dy^2)																					;Compute path distance
	po = ACOS(dy/d) + 2.0*(!Pi - ACOS(dy/d))*(dx LT 0)												;Compute angle of orientation

	xy0 = LL_ARC_DISTANCE([xsym,ysym], 0.5*dxsect_arc, po[0]/!DDTOR -  90.0, /DEGREES)	;Compute end points of cross-sections
	xy1 = LL_ARC_DISTANCE([xsym,ysym], 0.5*dxsect_arc, po[0]/!DDTOR +  90.0, /DEGREES)
	xy2 = LL_ARC_DISTANCE([xsym,ysym], 0.5*dxsect_arc, po[0]/!DDTOR - 180.0, /DEGREES)
	xy3 = LL_ARC_DISTANCE([xsym,ysym], 0.5*dxsect_arc, po[0]/!DDTOR,         /DEGREES)
	
	path       = MAP_2POINTS(xy0[0], xy0[1], xy1[0], xy1[1], NPATH = nsect)					;Compute coordinates of cross-sections
	path2      = MAP_2POINTS(xy2[0], xy2[1], xy3[0], xy3[1], NPATH = nsect)
	path[0,*]  = ( path[0,*] + 360.0) MOD 360.0														;Convert longitudes to 0->360
	path2[0,*] = (path2[0,*] + 360.0) MOD 360.0

	ixsect1 = INTERPOL(FINDGEN(data1.x.n), data1.x.values, REFORM( path[0,*], nsect))	;Compute interpolation coordinates
	iysect1 = INTERPOL(FINDGEN(data1.y.n), data1.y.values, REFORM( path[1,*], nsect))
	ixsect2 = INTERPOL(FINDGEN(data1.x.n), data1.x.values, REFORM(path2[0,*], nsect))
	iysect2 = INTERPOL(FINDGEN(data1.y.n), data1.y.values, REFORM(path2[1,*], nsect))
	izsect  = INTERPOL(FINDGEN(data1.z.n), data1.z.values, zsect                    )

	ixsect1 = REBIN(       ixsect1,            nsect, nzsect, /SAMPLE)						;Expand coordinates to two dimensions
	iysect1 = REBIN(       iysect1,            nsect, nzsect, /SAMPLE)
	ixsect2 = REBIN(       ixsect2,            nsect, nzsect, /SAMPLE)
	iysect2 = REBIN(       iysect2,            nsect, nzsect, /SAMPLE)
	izsect  = REBIN(REFORM(izsect, 1, nzsect), nsect, nzsect, /SAMPLE)

	dbz_sect1 = INTERPOLATE(data.dbz.values, ixsect1, iysect1, izsect, MISSING = !Values.F_NaN)
	dbz_sect2 = INTERPOLATE(data.dbz.values, ixsect2, iysect2, izsect, MISSING = !Values.F_NaN)
	
	trop1_sect1 = INTERPOLATE(ptrop, ixsect1, iysect1)
	trop1_sect2 = INTERPOLATE(ptrop, ixsect2, iysect2)
	trop2_sect1 = INTERPOLATE(strop, ixsect1, iysect1)
	trop2_sect2 = INTERPOLATE(strop, ixsect2, iysect2)
	
	epsfile = outdir + STRTRIM(i,2) + '.eps'															;Set image filenames
	pdffile = outdir + STRTRIM(i,2) + '.pdf'
	pngfile = outdir + STRTRIM(i,2) + '.png'
	
	IF ~KEYWORD_SET(eps) AND ~KEYWORD_SET(pdf) AND (i EQ 0) THEN BEGIN
		SET_PLOT, 'X'
		WINDOW, XSIZE = 1100, YSIZE = 850																;Open graphics window
		!P.COLOR      = COLOR_24('black')																;Foreground color
		!P.BACKGROUND = COLOR_24('white')																;Background color
		!P.CHARSIZE   = 1.5		
		!P.FONT       = -1																					;Use Hershey fonts
		thick_scale   = 1
	ENDIF

	IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
		PS_ON, FILENAME = epsfile, MARGIN = 0.0, PAGE_SIZE = [5.5, 4.25], /INCHES			;Switch to Postscript device
		DEVICE, /ENCAPSULATED
		!P.FONT     = 0																						;Hardware fonts
		!P.CHARSIZE = 0.5
		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
			LOAD_BASIC_COLORS																					;Load basic color definitions
		thick_scale   = 2
	ENDIF
	!P.THICK = thick_scale
	!X.THICK = thick_scale
	!Y.THICK = thick_scale
	!P.MULTI = [0, 2, 2]
		
	min_value = -30.0																							;Set minimum reflectivity value
	max_value = 75.0																							;Set maximum reflectivity value
	barticks  = 7																								;Set color bar ticks
	
	dbz_plot = (MAX(data.dBZ.values, DIMENSION = 3, /NAN) < 75.0)

	IF KEYWORD_SET(high_resolution) THEN BEGIN
		ix       = 0.25*FINDGEN(4*(data.x.n-1) + 1)
		iy       = 0.25*FINDGEN(4*(data.y.n-1) + 1)
		dbz_plot = INTERPOLATE(dbz_plot, ix, iy, /GRID)
	ENDIF

	MAP_SET, LIMIT = [y0,x0,y1,x1], /ISOTROPIC, $													;Set map
		TITLE = 'Composite Radar Reflectivity'

	TV_MAP_IMAGE, table, dbz_plot, $																		;Map image
		LATMIN    = y0, $
		LATMAX    = y1, $
		LONMIN    = x0, $
		LONMAX    = x1, $
		MIN_VALUE = min_value, $
		MAX_VALUE = max_value, $
		MISSING   = !Values.F_NaN, $
		CMISSING  = COLOR_24(100,100,100)
	
	MAP_CONTINENTS, /USA, /HIRES																			;Draw continents
					
	USERSYM_PLANE, ORIENTATION = po[0]/!DDTOR, /FILL												;Load plane symbol
						
	PLOTS, xsym, ysym, $																						;Overplot filled plane symbol
		PSYM    = 8, $
		SYMSIZE = 8 - 6*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
		COLOR   = pcolor1, $
		NOCLIP  = 0

	USERSYM_PLANE, ORIENTATION = po[0]/!DDTOR															;Load plane symbol
						
	PLOTS, xsym, ysym, $																						;Overplot plane symbol outline
		PSYM    = 8, $
		SYMSIZE = 8 - 6*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
		THICK   = 1 + (KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
		COLOR   = pcolor2, $
		NOCLIP  = 0

	PLOT, FINDGEN(2), /NODATA, $
		XRANGE = [0, 1], $
		XTICKS = 1, $
		XTICKN = [' ', ' '], $
		XSTYLE = 1, $
		YRANGE = [0, 1], $
		YTICKS = 1, $
		YTICKN = [' ', ' '], $
		YSTYLE = 1, $
		POSIT  = key_pos

	XYOUTS, 0.025, 0.85, 'NEXRAD WSR-88D Composite valid: ' + $									;Write valid time string to window
		MAKE_ISO_DATE_STRING(tplot[i], PREC='MINUTE', /COMPACT, /UTC)

	IF KEYWORD_SET(dc8) THEN $
		XYOUTS, 0.025, 0.70, 'Aircraft: NASA DC-8' ELSE $											;Write aircraft
	IF KEYWORD_SET(falcon) THEN $
		XYOUTS, 0.025, 0.70, 'Aircraft: DLR Falcon' ELSE $											;Write aircraft
		XYOUTS, 0.025, 0.70, 'Aircraft: NSF-NCAR GV'
	
	XYOUTS, 0.025, 0.55, 'Black Lines in Sections: GFS Tropopause(s)'							;Write key information
	XYOUTS, 0.025, 0.10, 'Cameron R. Homeyer, NCAR (chomeyer@ucar.edu)'						;Write contact information
	
	ij0 = CONVERT_COORD(lrsn_pos[0:1], /NORMAL, /TO_DEVICE)
	ij1 = CONVERT_COORD(lrsn_pos[2:*], /NORMAL, /TO_DEVICE)

	xsize = LONG(ij1[0] - ij0[0])
	ysize = LONG(ij1[1] - ij0[1])

	IF (KEYWORD_SET(eps) OR KEYWORD_SET(pdf)) THEN $
		dbz_sect1 = INTERPOLATE(dbz_sect1, MAKEN(0, nsect-1,  1200), MAKEN(0, nzsect-1,   800), /GRID) ELSE $
		dbz_sect1 = INTERPOLATE(dbz_sect1, MAKEN(0, nsect-1, xsize), MAKEN(0, nzsect-1, ysize), /GRID)

	image = IMAGE_24(COLOR_LOOKUP_24(dbz_sect1, table, MIN = -30.0, MAX = 75.0, $
				MISSING = COLOR_24(100, 100, 100), /NAN))
	TV, image, ij0[0], ij0[1], TRUE = 3, XSIZE = xsize, YSIZE = ysize, /DEVICE
			 
	CONTOUR, FLTARR(nsect, nzsect), xsect, zsect, /NODATA, $										;Draw cross-section
		XRANGE    = [-0.5*dxsect, 0.5*dxsect], $
		XTICKS    = 4, $
		XTITLE    = 'Distance (km)', $
		XSTYLE    = 1, $
		YRANGE    = [z0, z1], $
		YTITLE    = 'Altitude (km)', $
		YSTYLE    = 1, $
		POSITION  = lrsn_pos, $
		TITLE     = 'Vertical Section: Left-to-Right of Flight Path'

	USERSYM_PLANE, /FILL																						;Load plane symbol
						
	PLOTS, 0, zsym, $																							;Overplot filled plane symbol
		PSYM    = 8, $
		SYMSIZE = 8 - 6*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
		COLOR   = pcolor1, $
		NOCLIP  = 0

	USERSYM_PLANE																								;Load plane symbol
						
	PLOTS, 0, zsym, $																							;Overplot plane symbol outline
		PSYM    = 8, $
		SYMSIZE = 8 - 6*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
		THICK   = 1 + (KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
		COLOR   = pcolor2, $
		NOCLIP  = 0
	
	OPLOT, xsect, trop1_sect1, THICK = 2*thick_scale												;Overplot GFS tropopause altitudes
	OPLOT, xsect, trop2_sect1, THICK = 2*thick_scale

	ij0 = CONVERT_COORD(rfsn_pos[0:1], /NORMAL, /TO_DEVICE)
	ij1 = CONVERT_COORD(rfsn_pos[2:*], /NORMAL, /TO_DEVICE)

	xsize = LONG(ij1[0] - ij0[0])
	ysize = LONG(ij1[1] - ij0[1])

	IF (KEYWORD_SET(eps) OR KEYWORD_SET(pdf)) THEN $
		dbz_sect2 = INTERPOLATE(dbz_sect2, MAKEN(0, nsect-1,  1200), MAKEN(0, nzsect-1,   800), /GRID) ELSE $
		dbz_sect2 = INTERPOLATE(dbz_sect2, MAKEN(0, nsect-1, xsize), MAKEN(0, nzsect-1, ysize), /GRID)

	image = IMAGE_24(COLOR_LOOKUP_24(dbz_sect2, table, MIN = -30.0, MAX = 75.0, $
				MISSING = COLOR_24(100, 100, 100), /NAN))
	TV, image, ij0[0], ij0[1], TRUE = 3, XSIZE = xsize, YSIZE = ysize, /DEVICE
			 
	CONTOUR, FLTARR(nsect, nzsect), xsect, zsect, /NODATA, $										;Draw cross-section
		XRANGE    = [-0.5*dxsect, 0.5*dxsect], $
		XTICKS    = 4, $
		XTITLE    = 'Distance (km)', $
		XSTYLE    = 1, $
		YRANGE    = [z0, z1], $
		YTITLE    = 'Altitude (km)', $
		YSTYLE    = 1, $
		POSITION  = rfsn_pos, $
		TITLE     = 'Vertical Section: Rear-to-Front of Flight Path'
	
	OPLOT, xsect, trop1_sect2, THICK = 2*thick_scale												;Overplot GFS tropopause altitudes
	OPLOT, xsect, trop2_sect2, THICK = 2*thick_scale

	USERSYM_PLANE, ORIENTATION = 90.0, /FILL															;Load plane symbol
						
	PLOTS, 0, zsym, $																							;Overplot filled plane symbol
		PSYM    = 8, $
		SYMSIZE = 8 - 6*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
		COLOR   = pcolor1, $
		NOCLIP  = 0

	USERSYM_PLANE, ORIENTATION = 90.0																	;Load plane symbol
						
	PLOTS, 0, zsym, $																							;Overplot plane symbol outline
		PSYM    = 8, $
		SYMSIZE = 8 - 6*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
		THICK   = 1 + (KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
		COLOR   = pcolor2, $
		NOCLIP  = 0
	
	IF KEYWORD_SET(clutter) THEN BEGIN
		table     = (NEXRAD_DBZ_COLOR_24())[6:*]														;Set color bar table
		min_value = 0																							;Set color bar range
		barticks  = 5																							;Set number of color bar ticks
	ENDIF

	COLOR_BAR_24_KPB, table, OVER = table[-1], $														;Draw color bar
		TICKS = barticks, $
		RANGE = [min_value, max_value], $
		TITLE = 'Reflectivity (dB)', $
		POSIT = bar_pos
	
	!P.POSITION = 0
	
	IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
			LOAD_BASIC_COLORS, /RESET																		;Reset color table to linear ramp
		PS_OFF																									;Turn PS off
		IF KEYWORD_SET(pdf) THEN $
			PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS
	ENDIF ELSE IF KEYWORD_SET(png) THEN $
		WRITE_PNG, pngfile, TVRD(TRUE = 1)																;Write PNG
	
	!P.MULTI = 0

	itlast = it0[i]																							;Update most recent file index
ENDFOR

END


