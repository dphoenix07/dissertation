PRO VISUALIZE_88D_MAP, data, var, var2, $
	LIMIT           = limit, $
	SMOOTH          = smooth, $
	MAP_TYPE        = map_type, $
	COLOR_INDEX     = color_index, $
	MISSING         = missing, $
	BAR_MIN         = bar_min, $
	BAR_MAX         = bar_max, $
	BAR_TICKS       = bar_ticks, $
	ALTITUDE        = k, $
	DBZ_THRESH      = dbz_thresh, $
	ZRELATIVE       = zrelative, $
	ARC             = arc, $
	TROPOPAUSE      = tropopause, $
	ZMELT           = zmelt, $
	RADARS          = radars, $
	RANGE_RINGS     = range_rings, $
	RANGE_INTERVAL  = range_interval, $
	TRACKS          = tracks, $
	COUNTIES        = counties, $
	COUNTY_FILE     = county_file, $
	REPORTS         = reports, $
	FILENAME        = filename, $
	MAP_DATA        = map_data, $
	IMAGE           = image, $
	EPS             = EPS, $
	PNG             = png, $
	NC              = nc

;+
; Name:
;		VISUALIZE_88D_MAP
; Purpose:
;		This is a procedure to map regional composites of NEXRAD radar data for the VISUALIZE_88D viewer. 
; Calling sequence:
;		VISUALIZE_88D_MAP, data, variable
; Input:
;		data : NEXRAD data structure.
;		var  : Variable to be color-filled. (e.g., 'Z_H', 'Z_DR', 'K_DP')
;		var2 : Variable to be contoured. (e.g., 'Z_H', 'Z_DR', 'K_DP')
; Output:
;		Map of NEXRAD data.
;		IMAGE           : Set to named variable to get image when PNG keyword set.
; Keywords:
;		LIMIT           : Optional keyword to specify map limit
;		SMOOTH          : If set, apply horizontal Gaussian smoothing to the map field.
;		MAP_TYPE        : 0 = column-maximum, 1 = constant altitude, 2 = echo top, 3 = storm labeling algorithm
;		COLOR_INDEX     : Index for color table returned by VISUALIZE_88D_COLOR. Default is 0.
;		MISSING         : 24-bit color for missing data (i.e., background).
;		BAR_MIN         : Array of color bar minimum values for variables.
;		BAR_MAX         : Array of color bar maximum values for variables.
;		BAR_TICKS       : Array of color bar number of ticks for variables.
;		ALTITUDE        : Optional keyword to specify a constant altitude index for map.
;		DBZ_THRESH      : Optional keyword to set reflectivity threshold (in dBZ) to compute
;								altitude for.
;		ZRELATIVE       : If set, plot reflectivity relative altitude to ERA-Interim primary tropopause.
;		ARC				 : Optional keyword to specify cross-section path.
;		TROPOPAUSE      : Optional keyword to specify constant tropopause altitude to be used in 
;								tropopause-relative (ZRELATIVE = 1) echo top maps.
;		ZMELT           : Optional keyword to specify constant melting level altitude to be used
;								in CSA identification map.
;		RADARS          : If set, overplot radar locations
;		COUNTIES        : If set, draw county outlines.
;		COUNTY_FILE     : Path to county shapefile.
;		REPORTS         : Optional SPC reports structure for mapping.
;		FILENAME        : Output file path for image or PostScript file.
;		MAP_DATA        : Optional named variable to return map value structure to. 
;		IMAGE           : Optional to specify variable name to return image when generating PNG images.
;		EPS             : If set, output to encapsulated PostScript.
;		PNG             : If set, write PNG image using Z-buffer.
;		NC              : If set, output map to netCDF.
; Author and history:
;		Cameron R. Homeyer  2015-01-22.
;-

COMPILE_OPT IDL2																									;Set compile options

limit[[1,3]] = (limit[[1,3]] + 360.0) MOD 360.0															;Fix longitude limits

nc_info  = TAG_NAMES(data)																						;Get tag names
var_info = TAG_NAMES(data.dbz)																				;Get tag names
void     = WHERE((var_info EQ 'WVALUES'),v2orv3test)													;Check for v2 or v3 compositing method file
void     = WHERE((nc_info  EQ 'NOBS'),  sparse_test)

IF (N_ELEMENTS(missing  ) EQ 0) THEN missing   = COLOR_24(200,200,200)							;Set default color for missing values
IF (N_ELEMENTS(bar_min  ) EQ 0) THEN bar_min   = [ 0.0,-1.0,-1.0,0.7]							;Set color bar defaults
IF (N_ELEMENTS(bar_max  ) EQ 0) THEN bar_max   = [75.0, 4.0, 4.0,1.0]
IF (N_ELEMENTS(bar_ticks) EQ 0) THEN bar_ticks = [   5,   5,   5,  3]

CASE STRTRIM(var,2) OF
	'Z_H' : BEGIN
				min_value = bar_min[0]																			;Set mapping thresholds based on variable type
				max_value = bar_max[0]
				barticks  = bar_ticks[0]
				units     = 'dBZ'
				bartitle  = 'Z!DH!N (dBZ)'
				values    = data.dbz.values
			  END
	'Z_DR' : BEGIN
				min_value = bar_min[1]
				max_value = bar_max[1]
				barticks  = bar_ticks[1]
				units     = 'dB'
				bartitle  = 'Z!DDR!N (dB)'
				IF ((data.date.year LT 2015) AND (~sparse_test)) THEN $
					values = data.zdr.values + 0.35 ELSE $
					values = data.zdr.values

				ilow      = WHERE(((data.dbz.values LT 2.5) OR (values GE 8.0)),nlow)
				IF (nlow GT 0) THEN values[ilow] = !Values.F_NaN
			  END
	'K_DP' : BEGIN
				min_value = bar_min[2]
				max_value = bar_max[2]
				barticks  = bar_ticks[2]
				units     = 'deg km^-1'
				bartitle  = 'K!DDP!N (deg km!U-1!N)'
				values    = data.kdp.values

				ilow      = WHERE((data.dbz.values LT 2.5),nlow)
				IF (nlow GT 0) THEN values[ilow] = !Values.F_NaN
			  END
	'rho_HV' : BEGIN
				min_value = bar_min[3]
				max_value = bar_max[3]
				barticks  = bar_ticks[3]
				units     = ' '
				IF KEYWORD_SET(eps) THEN bartitle  = '!9' + STRING("162B) + '!X!DHV!N' $
										  ELSE bartitle  = '!4' + STRING( 113B) + '!X!DHV!N'
				values    = data.rHV.values

				ilow      = WHERE((data.dbz.values LT 2.5),nlow)
				IF (nlow GT 0) THEN values[ilow] = !Values.F_NaN
			  END
ENDCASE


CASE map_type OF 
	0 : BEGIN
			map_plot = MAX(values,DIM=3,/NAN)																;Set map plot to column-maximum of variable
			table    = VISUALIZE_88D_COLOR(color_index)													;Set color bar
			title0   = 'Column-Maximum Map'																	;Set title string
		 END
	1 : BEGIN
			map_plot = values[*,*,k]																			;Set map plot to constant altitude of variable
			table    = VISUALIZE_88D_COLOR(color_index)													;Set color bar
			title0   = STRING((data.z.values)[k], FORMAT="(F4.1)") + ' km Altitude Map'		;Set title string
		 END
	2 : BEGIN
			map_plot = MAX((values GE dbz_thresh)*(FINITE(SHIFT(values,0,0,1)))*$				;Compute altitude of reflectivity surface
							(FINITE(SHIFT(values,0,0,2)))*REBIN(REFORM(data.z.values, 1, 1, $	
							data.z.n), data.x.n, data.y.n, data.z.n, /SAMPLE), DIM = 3, /NAN)

			izero = WHERE((map_plot EQ 0.0), n0)															;Find 0s
			IF (n0 GT 0) THEN map_plot[izero] = !Values.F_NaN											;Remove altitudes for areas with no echo

			IF KEYWORD_SET(zrelative) THEN BEGIN
				map_plot  = map_plot - tropopause															;Adjust altitudes
				min_value = -5.0																					;Set color bar chars
				max_value =  5.0
				barticks  = 10
				units     = 'km'
				bartitle  = 'Tropopause-Relative Altitude (km)'
				table     = BLUE_GRAY_RED_24(40, 19, 0.22)
			ENDIF ELSE BEGIN
				min_value =  5.0																					;Set color bar chars
				max_value = 20.0
				barticks  = 3
				units     = 'km'
				bartitle  = 'Altitude (km)'		
				table     = VISUALIZE_88D_COLOR(color_index)
			ENDELSE
			
			title0 = STRING(dbz_thresh, FORMAT="(F4.1)") + ' dBZ Echo Top Map'					;Set title string
		 END
	3 : BEGIN
			map_plot = FLOAT(NEXRAD_SL3D(data, ZMELT = zmelt))											;Set map plot to column-maximum of variable
			inan     = WHERE((map_plot EQ 0.0),nnan)
			IF (nnan GT 0) THEN map_plot[inan] = !Values.F_NaN
			table    = [COLOR_24('gray30'),COLOR_24(255,0,0),COLOR_24(255,222,0),$				;Set color bar
							COLOR_24(78,186,25),COLOR_24(150,25,150),COLOR_24(25,75,255),$
							COLOR_24('cyan')]
			min_value = 1.0
			max_value = 7.0
			barticks  = 7
			units     = ' '
			title0    = 'Storm Classification'																;Set title string
		 END
ENDCASE
	
IF KEYWORD_SET(smooth) THEN BEGIN
	IF (map_type EQ 2) THEN $
		ibad = WHERE((~FINITE(map_plot) OR (map_plot LT min_value)), nbad) ELSE $				;Get indices of missing values
		ibad = WHERE((~FINITE(map_plot)), nbad)

	IF (nbad GT 0) THEN BEGIN
		CASE STRTRIM(var,2) OF
			'Z_H'    : IF (map_type EQ 2) THEN map_plot[ibad] = min_value $						;Set background value to smooth with
													ELSE map_plot[ibad] = 0.0
			'Z_DR'   : map_plot[ibad] = 0.0
			'K_DP'   : map_plot[ibad] = 0.0
			'rho_HV' : map_plot[ibad] = 1.0
		ENDCASE
	ENDIF

	map_plot = GAUSS_SMOOTH(map_plot, [1.5,1.5], /NAN, /EDGE_TRUNCATE)							;If set, smooth map variable
	IF (nbad GT 0) THEN map_plot[ibad] = !Values.F_NaN													;Remove bad areas
ENDIF

IF KEYWORD_SET(nc) THEN BEGIN
	id  = NCDF_CREATE(filename, CLOBBER = 1)																;Open netCDF file for writing
	xid = NCDF_DIMDEF(id, 'Longitude', data.x.n)															;Define dimensions
	yid = NCDF_DIMDEF(id, 'Latitude',  data.y.n)

	vid = NCDF_VARDEF(id, 'Longitude', [xid], /FLOAT)													;Create longitude variable
	NCDF_ATTPUT, id, 'Longitude', 'long_name', 'Longitude'											;Write attributes
	NCDF_ATTPUT, id, 'Longitude', 'units',     'degrees East'

	vid = NCDF_VARDEF(id, 'Latitude', [yid], /FLOAT)													;Create latitude variable
	NCDF_ATTPUT, id, 'Latitude', 'long_name', 'Latitude'												;Write attributes
	NCDF_ATTPUT, id, 'Latitude', 'units',     'degrees North'

	vid = NCDF_VARDEF(id, STRTRIM(var,2), [xid,yid], /FLOAT)											;Create reflectivity variable
	NCDF_ATTPUT, id, STRTRIM(var,2), 'long_name', title0												;Write attributes
	NCDF_ATTPUT, id, STRTRIM(var,2), 'units',     units

	NCDF_CONTROL, id, /ENDEF																					;Exit define mode

	NCDF_VARPUT, id, 'Longitude',    data.x.values														;Write coordinates to file
	NCDF_VARPUT, id, 'Latitude',     data.y.values
	NCDF_VARPUT, id, STRTRIM(var,2), map_plot

	NCDF_ATTPUT, id, 'date', MAKE_ISO_DATE_STRING(data.date), /GLOBAL								;Write global attributes
	NCDF_ATTPUT, id, 'Author', 'Output by VISUALIZE_88D software (chomeyer@ou.edu)', /GLOBAL
	NCDF_CLOSE, id																									;Close netCDF file
ENDIF ELSE BEGIN
	IF KEYWORD_SET(eps) THEN BEGIN
		PS_ON, FILENAME = filename, PAGE_SIZE = [8.5,7.0], MARGIN = 0.0, /INCHES				;Turn PostScript on
		DEVICE, /ENCAPSULATED
		!P.THICK    = 3																							;Increase line thickness
		!X.THICK    = 3
		!Y.THICK    = 3
		!P.FONT     = 0																							;Hardware fonts
		!P.CHARSIZE = 1.25																						;Set character size
	ENDIF ELSE BEGIN
		IF KEYWORD_SET(png) THEN BEGIN
			SET_PLOT, 'Z'																							;Set graphics device to Z-buffer
			DEVICE, SET_PIXEL_DEPTH = 24, SET_RESOLUTION = [850, 700], $							;Set device resolution and bit depth
				SET_CHARACTER_SIZE = [6, 10]
		ENDIF ELSE IF (PATH_SEP() EQ '/') THEN SET_PLOT, 'X' $										;Set device to X if unix
													 ELSE SET_PLOT, 'WIN'

		!P.BACKGROUND = COLOR_24('white')
		!P.COLOR      = COLOR_24('black')
		!P.CHARSIZE   = 2.0 - 0.35*((PATH_SEP() EQ '\') AND (~KEYWORD_SET(png)))				;Scale for Windows
		!P.FONT       = -1																						;Use Hershey fonts
		!P.THICK      = 1																							;Increase line thickness
		!X.THICK      = 1
		!Y.THICK      = 1
	ENDELSE

	levels  = min_value + ((max_value-min_value)/N_ELEMENTS(table))*FINDGEN(N_ELEMENTS(table));Compute contour levels
	map_pos = [0.12, 0.24, 0.95, 0.95]																		;Set map position

	ERASE

	ratio   = (limit[2]-limit[0])/(limit[3]-limit[1])													;Compute dy/dx
	nximage = 1200
	nyimage = LONG(ratio*nximage)																				;Scale number of lat pixels
	ii = INTERPOL(FINDGEN(data.x.n),data.x.values,MAKEN(limit[1],limit[3],nximage))
	ji = INTERPOL(FINDGEN(data.y.n),data.y.values,MAKEN(limit[0],limit[2],nyimage))
	ii2 = REBIN(       ROUND(ii),              nximage, nyimage)
	ji2 = REBIN(REFORM(ROUND(ji), 1, nyimage), nximage, nyimage)
	ibad = WHERE(((ii2 LT 0) OR (ji2 LT 0) OR $
				(ii2 GT data.x.n-1) OR (ji2 GT data.y.n-1)), nbad)

	map_plot2 = map_plot[ROUND(ii2),ROUND(ji2)]															;Extract map values for region
	
	IF (map_type NE 3) THEN BEGIN
		map_plot = INTERPOLATE(map_plot, ii, ji, /GRID, MISSING = !Values.F_NaN)				;Ensure high-resolution image
		inan     = WHERE((FINITE(map_plot2) AND ~FINITE(map_plot)), nnan)							;Find echo lost to interpolation
		IF (nnan GT 0) THEN map_plot[inan] = map_plot2[inan]											;Fix echo
	ENDIF ELSE $
		map_plot = map_plot2
	
	
	IF (map_type EQ 3) THEN IF (nbad GT 0) THEN map_plot[ibad] = !Values.F_NaN

	ilow = WHERE((map_plot LE min_value),nlow)
	IF (nlow GT 0) THEN map_plot[ilow] = min_value														;Fix exceedances 

	ihigh = WHERE((map_plot GE max_value),nhigh)
	IF (nhigh GT 0) THEN map_plot[ihigh] = max_value													;Fix exceedances 

	IF (map_type EQ 3) THEN BEGIN
		nclass = FLTARR(7)																						;Create array to store pixel counts in each SL3D classification
		FOR i = 1, 7 DO BEGIN
			void        = WHERE((map_plot EQ i), ni)														;Search for number of grid points of each SL3D classification
			nclass[i-1] = ni																						;Store count
		ENDFOR

		COLOR_BAR_24_KPB, table, $																				;Draw color bar
			TICKS  = barticks, $
			RANGE  = [min_value, max_value], $
			XTICKN = REPLICATE(' ', 8), $
			POSIT  = [0.1, 0.10, 0.9, 0.12]
		AXIS, XAXIS = 0, XTICKS = 6, XTICKV = (6.0/7.0)*FINDGEN(max_value) + 10.0/7.0, $
			XTICKLEN = 0.0, XTICKN = ['UPDFT','DCu','SCu/DSR','SR','TrAnv','ThickAnv','ThinAnv']		
		FOR i = 0, 6 DO $
			XYOUTS, 0.1 + 0.5*(0.8/7.0) + i*(0.8/7.0), 0.02, ALIGN = 0.5, $
				STRTRIM(STRING(100.0*(nclass[i]/TOTAL(nclass)), FORMAT="(F5.1)"),2) + '%', $
				/NORMAL, CHARSIZE = 0.75*!P.CHARSIZE
	ENDIF ELSE $
		COLOR_BAR_24_KPB, table, OVER = table[-1], UNDER = table[0], $								;Draw color bar
			TICKS = barticks, $
			RANGE = [min_value, max_value], $
			TITLE = bartitle, $
			POSIT = [0.25, 0.10, 0.75, 0.12]

	MAP_SET, 0, 180, 0, LIMIT = limit, /ISOTROPIC, POSITION = map_pos, /NOBORDER, $			;Set map
		TITLE = title0 + ' valid ' + MAKE_ISO_DATE_STRING(data.date), /NOERASE

	TV_MAP_IMAGE, table, map_plot, $																			;Map image
		LATMIN    = limit[0], $
		LATMAX    = limit[2], $
		LONMIN    = limit[1], $
		LONMAX    = limit[3], $
		MIN_VALUE = min_value, $
		MAX_VALUE = max_value, $
		MISSING   = !Values.F_NaN, $
		CMISSING  = missing

	IF ((map_type LT 2) AND (STRTRIM(var2,2) NE 'None')) THEN BEGIN
		CASE STRTRIM(var2,2) OF
			'Z_H' : BEGIN
						levels   = MAKEN(bar_min[0], bar_max[0], bar_ticks[0] + 1)
						contours = data.dbz.values
					  END
			'Z_DR' : BEGIN
						levels = MAKEN(bar_min[1], bar_max[1], bar_ticks[1] + 1)
						IF ((data.date.year LT 2015) AND (~sparse_test)) THEN $
							contours = data.zdr.values + 0.35 ELSE $
							contours = data.zdr.values
					  END
			'K_DP' : BEGIN
						levels   = MAKEN(bar_min[2], bar_max[2], bar_ticks[2] + 1)
						contours = data.kdp.values
					  END
			'rho_HV' : BEGIN
						levels   = MAKEN(bar_min[3], bar_max[3], bar_ticks[3] + 1)
						contours = data.rHV.values
					  END
		ENDCASE

		CASE map_type OF
			0 : contours = MAX(contours,DIM=3,/NAN)
			1 : contours = contours[*,*,k]
		ENDCASE
		
		IF KEYWORD_SET(smooth) THEN BEGIN
			ibad = WHERE((~FINITE(contours)), nbad)
			IF (nbad GT 0) THEN contours[ibad] = 0.0 + (var2 EQ 'rho_HV')
			contours = GAUSS_SMOOTH(contours, [1.0,1.0], /NAN, /EDGE_TRUNCATE)					;If set, smooth map variable
			IF (nbad GT 0) THEN contours[ibad] = !Values.F_NaN											;Remove bad areas
		ENDIF

		FOR i = 0, N_ELEMENTS(levels) -1 DO $		
			CONTOUR, contours, data.x.values, data.y.values, OVERPLOT = 1, $
				LEVELS = levels[i], THICK = (i+1)*!P.THICK, C_LINESTYLE = 2*(levels[i] LT 0)
	ENDIF	

	MAP_CONTINENTS, /USA, /CONTINENTS, /HIRES, COLOR = COLOR_24('black'), THICK = 2*!P.THICK	;Draw continents

	IF (KEYWORD_SET(counties) AND (STRLEN(county_file) GT 0)) THEN MAP_COUNTIES, county_file

	IF KEYWORD_SET(radars) THEN BEGIN
		info = NEXRAD_STATION_INFO()
		USERSYM_CIRCLE, /FILL
		PLOTS, info.lon, info.lat, COLOR = COLOR_24('white'), PSYM = 8, SYMSIZE = 4.0, NOCLIP = 0
		PLOTS, info.lon, info.lat, COLOR = COLOR_24('black'), PSYM = 8, SYMSIZE = 3.0, NOCLIP = 0

		FOR i = 0, N_ELEMENTS(info.statid) -1 DO $
			XYOUTS, info.lon[i], info.lat[i] + 0.15, info.statid[i], COLOR = COLOR_24('white'), $	;Plot station ID
				ALIGN = 0.5, /DATA, CHARTHICK = 2*!P.THICK, NOCLIP = 0
	ENDIF

	IF KEYWORD_SET(range_rings) THEN BEGIN
		xc = limit[1] + 0.5*(limit[3] - limit[1])															;Get center point of domain
		yc = limit[0] + 0.5*(limit[2] - limit[0])
		
		distance = 0.001*MAP_2POINTS(limit[1], limit[0], limit[3], limit[2], /METERS)			;Get distance across domain
		ncircle  = CEIL(distance/range_interval)															;Get maximum number of circles
		
		USERSYM_CIRCLE, /FILL
		PLOTS, xc, yc, PSYM = 8, SYMSIZE = 3.0, NOCLIP = 0
		FOR i = 1, ncircle DO MAP_CIRCLE, xc, yc, i*range_interval, THICK = 3*!P.THICK		;Draw circles
	ENDIF

	IF (TOTAL(arc) NE 0.0) THEN $
		PLOTS, arc[0,*], arc[1,*], THICK = 4*!P.THICK, COLOR = COLOR_24('black')				;Draw arc

	IF (SIZE(reports,/TNAME) EQ 'STRUCT') THEN BEGIN
		IF (SIZE(reports.tornado,/TNAME) EQ 'STRUCT') THEN BEGIN
			tdiff   = TIME_DIFF(data.date,reports.tornado.t)
			irecent = WHERE((ABS(tdiff) LE 300),nrecent)
			IF (nrecent GT 0) THEN BEGIN
				USERSYM_CIRCLE, /FILL
				FOR i = 0, nrecent - 1 DO BEGIN
					PLOTS, (reports.tornado.x)[irecent[i]], (reports.tornado.y)[irecent[i]], $
						COLOR = COLOR_24('white'), PSYM = 8, SYMSIZE = 4.0, NOCLIP = 0
					PLOTS, (reports.tornado.x)[irecent[i]], (reports.tornado.y)[irecent[i]], $
						COLOR = COLOR_24('red'),   PSYM = 8, SYMSIZE = 3.0, NOCLIP = 0
					XYOUTS, (reports.tornado.x)[irecent[i]] + 0.015*(limit[3]-limit[1]), $
						(reports.tornado.y)[irecent[i]] - 0.015*(limit[2]-limit[0]), $
						STRING(((reports.tornado.t)[irecent[i]]).hour,FORMAT="(I2.2)") + ':' + $
						STRING(((reports.tornado.t)[irecent[i]]).minute,FORMAT="(I2.2)") + 'Z', $
						COLOR = COLOR_24('white'), CHARTHICK = 2, NOCLIP = 0
				ENDFOR
			ENDIF
		ENDIF
		IF (SIZE(reports.wind,/TNAME) EQ 'STRUCT') THEN BEGIN
			tdiff   = TIME_DIFF(data.date,reports.wind.t)
			irecent = WHERE((ABS(tdiff) LE 300),nrecent)
			IF (nrecent GT 0) THEN BEGIN
				USERSYM_CIRCLE, /FILL
				FOR i = 0, nrecent - 1 DO BEGIN
					PLOTS, (reports.wind.x)[irecent[i]], (reports.wind.y)[irecent[i]], $
						COLOR = COLOR_24('white'), PSYM = 8, SYMSIZE = 4.0, NOCLIP = 0
					PLOTS, (reports.wind.x)[irecent[i]], (reports.wind.y)[irecent[i]], $
						COLOR = COLOR_24('blue'),   PSYM = 8, SYMSIZE = 3.0, NOCLIP = 0
					XYOUTS, (reports.wind.x)[irecent[i]] + 0.015*(limit[3]-limit[1]), $
						(reports.wind.y)[irecent[i]] - 0.015*(limit[2]-limit[0]), $
						STRING(((reports.wind.t)[irecent[i]]).hour,FORMAT="(I2.2)") + ':' + $
						STRING(((reports.wind.t)[irecent[i]]).minute,FORMAT="(I2.2)") + 'Z', $
						COLOR = COLOR_24('white'), CHARTHICK = 2, NOCLIP = 0
				ENDFOR
			ENDIF
		ENDIF
		IF (SIZE(reports.hail,/TNAME) EQ 'STRUCT') THEN BEGIN
			tdiff   = TIME_DIFF(data.date,reports.hail.t)
			irecent = WHERE((ABS(tdiff) LE 300),nrecent)
			IF (nrecent GT 0) THEN BEGIN
				USERSYM_CIRCLE, /FILL
				FOR i = 0, nrecent - 1 DO BEGIN
					PLOTS, (reports.hail.x)[irecent[i]], (reports.hail.y)[irecent[i]], $
						COLOR = COLOR_24('white'), PSYM = 8, SYMSIZE = 4.0, NOCLIP = 0
					PLOTS, (reports.hail.x)[irecent[i]], (reports.hail.y)[irecent[i]], $
						COLOR = COLOR_24('green'), PSYM = 8, SYMSIZE = 3.0, NOCLIP = 0
					XYOUTS, (reports.hail.x)[irecent[i]] + 0.015*(limit[3]-limit[1]), $
						(reports.hail.y)[irecent[i]] - 0.015*(limit[2]-limit[0]), $
						STRING(((reports.hail.t)[irecent[i]]).hour,FORMAT="(I2.2)") + ':' + $
						STRING(((reports.hail.t)[irecent[i]]).minute,FORMAT="(I2.2)") + 'Z', $
						COLOR = COLOR_24('white'), CHARTHICK = 2, NOCLIP = 0
				ENDFOR
			ENDIF
		ENDIF
	ENDIF

	IF (SIZE(tracks,/TNAME) EQ 'STRUCT') THEN BEGIN
		tdiff = TIME_DIFF(tracks.date, data.date)
		igood = WHERE((tdiff EQ 0), ngood)
	
		FOR i = 0, ngood -1 DO BEGIN
			MAP_CIRCLE, tracks.x[igood[i]], tracks.y[igood[i]], 10.0, $								;Draw a 5 km circle
				COLOR = COLOR_24('white'), THICK = 2*!P.THICK
			XYOUTS, tracks.x[igood[i]] + 0.25, tracks.y[igood[i]] - 0.075, $						;Show storm number
				STRTRIM(tracks.storm[igood[i]],2), COLOR = COLOR_24('white'), $
				CHARTHICK = 2, ALIGN = 0.5
		ENDFOR
	ENDIF

	MAP_GRID_CYLINDRICAL_CRH, limit[1], limit[3], 4, limit[0], limit[2], 4, /LABEL, $		;Draw grid
		GLINESTYLE = -KEYWORD_SET(counties)
					
	IF KEYWORD_SET(eps) THEN BEGIN
		PS_OFF																										;Turn PS off

		IF (PATH_SEP() EQ '/') THEN SET_PLOT, 'X' $														;Set device to X if unix
									  ELSE SET_PLOT, 'WIN'

		!P.COLOR      = COLOR_24('black')																	;Foreground color
		!P.BACKGROUND = COLOR_24('white')																	;Background color
		!P.CHARSIZE   = 2.0
		!P.FONT       = -1																						;Use Hershey fonts
		!P.THICK      = 1																							;Decrease line thickness
		!X.THICK      = 1
		!Y.THICK      = 1
	ENDIF

	IF KEYWORD_SET(png) THEN BEGIN
		image = TVRD(/TRUE)																						;Read image from Z_buffer
		IF (N_ELEMENTS(filename) GT 0) THEN WRITE_PNG, filename, image								;If keyword set and filename given, write PNG image

		IF (PATH_SEP() EQ '/') THEN SET_PLOT, 'X' $														;Set device to X if unix
									  ELSE SET_PLOT, 'WIN'
	ENDIF

	dim      = SIZE(map_plot, /DIMENSIONS)																	;Get dimension of map values
	map_data = {values : map_plot,                        $											;Create map data structure to return to named variable
					x      : MAKEN(limit[1],limit[3],dim[0]), $
					y      : MAKEN(limit[0],limit[2],dim[1]), $
					nx     : dim[0],                          $
					ny     : dim[1]                           }
ENDELSE

END


