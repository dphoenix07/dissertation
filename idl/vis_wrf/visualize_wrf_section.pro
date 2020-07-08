PRO VISUALIZE_88D_SECTION, data, var, var2, x0, y0, x1, y1, $
	COLOR_INDEX      = color_index, $
	MISSING          = missing, $
	BAR_MIN          = bar_min, $
	BAR_MAX          = bar_max, $
	BAR_TICKS        = bar_ticks, $
	SMOOTH           = smooth, $
	ZRANGE           = zrange, $
	MAP_TYPE         = map_type, $
	TROPOPAUSE       = tropopause, $
	ZMELT            = zmelt, $
	NEAREST_NEIGHBOR = nearest_neighbor, $
	FILENAME         = filename, $
	IMAGE            = image, $
	EPS              = eps, $
	PNG              = png, $
	NC               = nc

;+
; Name:
;		VISUALIZE_88D_SECTION
; Purpose:
;		This is a procedure to plot a NEXRAD cross-section for the VISUALIZE_88D widget. 
; Calling sequence:
;		VISUALIZE_88D_SECTION
; Input:
;		data : NEXRAD composite region name.
;		var  : Variable to color-fill. (e.g., 'Z_H', 'Z_DR', 'K_DP') 
;		var2 : Variable to contour. (e.g., 'Z_H', 'Z_DR', 'K_DP') 
;		x0   : Initial longitude point.
;		y0   : Initial latitude point.
;		x1   : Final longitude point.
;		y1   : Final latitude point.
; Output:
;		A map of composite reflectivity with cross-section coordinates and corresponding
;		reflectivity cross-section.
; Keywords:
;		COLOR_INDEX      : Index for color table returned by VISUALIZE_88D_COLOR. Default is 0.
;		MISSING          : 24-bit color for missing data (i.e., background). 
;		BAR_MIN          : Array of color bar minimum values for variables.
;		BAR_MAX          : Array of color bar maximum values for variables.
;		BAR_TICKS        : Array of color bar number of ticks for variables.
;		SMOOTH           : If set, apply Gaussian smoothing to radar data.
;		ZRANGE           : Keyword to specify altitude range.
;		MAP_TYPE         : Keyword to specify current map type (If CSA, plots ID line at bottom of section).
;		TROPOPAUSE       : Optional keyword to specify constant tropopause height to include in plot.
;		ZMELT            : Optional keyword to specify constant melting level for CSA identification.
;		NEAREST_NEIGHBOR : If set, create vertical cross-section using nearest-neighbor approach. Default is linear interpolation.
;		FILENAME         : Output file path for image or PostScript file.
;		IMAGE           : Optional to specify variable name to return image when generating PNG images.
;		EPS              : If set, output to PostScript.
;		PNG              : If set, write PNG image using Z-buffer.
;		NC               : If set, output to netCDF.
; Author and history:
;		Cameron R. Homeyer  2015-01-22.
;								  2015-09-22. Added option to plot nearest-neighbor section.
;-

COMPILE_OPT IDL2																									;Set compile options

table = VISUALIZE_88D_COLOR(color_index)																	;Set color bar

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
				min_value = bar_min[0]																			;Set section thresholds based on variable type
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
				units     = ' '
				barticks  = bar_ticks[3]
				IF KEYWORD_SET(eps) THEN bartitle  = '!9' + STRING("162B) + '!X!DHV!N' $
										  ELSE bartitle  = '!4' + STRING( 113B) + '!X!DHV!N'
				values    = data.rHV.values

				ilow      = WHERE((data.dbz.values LT 2.5),nlow)
				IF (nlow GT 0) THEN values[ilow] = !Values.F_NaN
			  END
ENDCASE

xy    = MAP_2POINTS(x0, y0, x1, y1, DPATH = 0.01, /RHUMB)											;Compute cross-section path
nsect = N_ELEMENTS(xy[0,*])																					;Get number of elements in section
x     = (REFORM(xy[0,*], nsect) + 360.0) MOD 360.0														;Extract coordinates
y     =  REFORM(xy[1,*], nsect)

degree_to_km = 0.002*!DPi*(6.378137D+06)/360.0D0														;Set unit conversion from degrees to km
dx           = ABS(SHIFT(x, -1) - x)*(degree_to_km*COS(y*(!DPI/180.0D0)))						;Compute longitude and latitude lengths
dy           = ABS(SHIFT(y, -1) - y)*(degree_to_km)
distance     = [0.0, SQRT(dx[0:nsect-2]^2 + dy[0:nsect-2]^2)]										;Compute distance array

z0    = zrange[0]																									;Set cross-section altitude parameters
z1    = zrange[1]
nz    = LONG((z1 - z0)/0.25) + 1
z     = MAKEN(z0,z1,nz)

ix = INTERPOL(FINDGEN(data.x.n), data.x.values, x)														;Compute interpolation coordinates
iy = INTERPOL(FINDGEN(data.y.n), data.y.values, y)
iz = INTERPOL(FINDGEN(data.z.n), data.z.values, z)

ix = REBIN(       ix,         nsect, nz, /SAMPLE)														;Expand coordinates to two dimensions
iy = REBIN(       iy,         nsect, nz, /SAMPLE)
iz = REBIN(REFORM(iz, 1, nz), nsect, nz, /SAMPLE)

IF KEYWORD_SET(nearest_neighbor) THEN BEGIN
	isect = INDEX_OF_NEAREST_CRH(data.x.values, x)														;Get grid indices for cross-section path
	jsect = INDEX_OF_NEAREST_CRH(data.y.values, y)
	k0    = INDEX_OF_NEAREST(data.z.values, z0)															;Get grid indices for altitude range
	k1    = INDEX_OF_NEAREST(data.z.values, z1)
	nz    = k1 - k0 + 1																							;Set number of vertical levels
	ksect = k0 + LINDGEN(nz)																					;Set vertical indices for section
	z     = data.z.values[k0:k1]																				;Extract altitude array
	
	isect = REBIN(       isect,       nsect, nz)															;Rebin section indices to two dimensions
	jsect = REBIN(       jsect,       nsect, nz)
	ksect = REBIN(REFORM(ksect,1,nz), nsect, nz)

	section_plot =  values[isect,jsect,ksect]																;Get nearest-neighbor grid columns along cross-section path
ENDIF ELSE $
	section_plot = INTERPOLATE(values,  ix, iy, iz, MISSING = !Values.F_NaN)					;Interpolate variable to cross-section

IF KEYWORD_SET(smooth) THEN BEGIN
	ibad = WHERE((~FINITE(section_plot)), nbad)															;Get indices of marginal/missing values

	CASE STRTRIM(var,2) OF
		'Z_H'    : section_plot[ibad] = 0.0
		'Z_DR'   : section_plot[ibad] = 0.0
		'K_DP'   : section_plot[ibad] = 0.0
		'rho_HV' : section_plot[ibad] = 1.0
	ENDCASE

	section_plot = GAUSS_SMOOTH(section_plot, [2.0,0.0], /NAN, /EDGE_TRUNCATE)					;If set, smooth section variable
	IF (nbad GT 0) THEN section_plot[ibad] = !Values.F_NaN											;Remove bad areas
ENDIF

IF KEYWORD_SET(nc) THEN BEGIN
	id  = NCDF_CREATE(filename, CLOBBER = 1)																;Open netCDF file for writing
	xid = NCDF_DIMDEF(id, 'Path',     nsect)																;Define dimensions
	zid = NCDF_DIMDEF(id, 'Altitude', nz   )

	vid = NCDF_VARDEF(id, 'Distance', [xid], /FLOAT)													;Create longitude variable
	NCDF_ATTPUT, id, 'Distance', 'long_name', 'Distance along cross-section'					;Write attributes
	NCDF_ATTPUT, id, 'Distance', 'units',     'km'

	vid = NCDF_VARDEF(id, 'Longitude', [xid], /FLOAT)													;Create longitude variable
	NCDF_ATTPUT, id, 'Longitude', 'long_name', 'Longitude'											;Write attributes
	NCDF_ATTPUT, id, 'Longitude', 'units',     'degrees East'

	vid = NCDF_VARDEF(id, 'Latitude', [xid], /FLOAT)													;Create latitude variable
	NCDF_ATTPUT, id, 'Latitude', 'long_name', 'Latitude'												;Write attributes
	NCDF_ATTPUT, id, 'Latitude', 'units',     'degrees North'

	vid = NCDF_VARDEF(id, 'Altitude', [zid], /FLOAT)													;Create latitude variable
	NCDF_ATTPUT, id, 'Altitude', 'long_name', 'Altitude'												;Write attributes
	NCDF_ATTPUT, id, 'Altitude', 'units',     'km'

	vid = NCDF_VARDEF(id, STRTRIM(var,2), [xid,zid], /FLOAT)											;Create reflectivity variable
	NCDF_ATTPUT, id, STRTRIM(var,2), 'long_name', 'Vertical Section of ' + STRTRIM(var,2)	;Write attributes
	NCDF_ATTPUT, id, STRTRIM(var,2), 'units',     units

	NCDF_CONTROL, id, /ENDEF																					;Exit define mode

	NCDF_VARPUT, id, 'Distance',     TOTAL(distance, /CUMULATIVE)									;Write coordinates to file
	NCDF_VARPUT, id, 'Longitude',    x
	NCDF_VARPUT, id, 'Latitude',     y
	NCDF_VARPUT, id, 'Altitude',     z
	NCDF_VARPUT, id, STRTRIM(var,2), section_plot

	NCDF_ATTPUT, id, 'date', MAKE_ISO_DATE_STRING(data.date), /GLOBAL								;Write global attributes
	NCDF_ATTPUT, id, 'Author', 'Output by VISUALIZE_88D software (chomeyer@ou.edu)', /GLOBAL
	NCDF_CLOSE, id																									;Close netCDF file
ENDIF ELSE BEGIN
	IF KEYWORD_SET(eps) THEN BEGIN	
		PS_ON, FILENAME = filename, PAGE_SIZE = [10.24, 7.68], MARGIN = 0.0, /INCHES			;Switch to Postscript device
		DEVICE, /ENCAPSULATED
		!P.THICK    = 3																							;Increase line thickness
		!X.THICK    = 3
		!Y.THICK    = 3
		!P.FONT     = 0																							;Hardware fonts
		!P.CHARSIZE = 1.4																							;Set character size
	ENDIF ELSE BEGIN
		IF KEYWORD_SET(png) THEN BEGIN
			SET_PLOT, 'Z'																							;Set graphics device to Z-buffer
			DEVICE, SET_PIXEL_DEPTH = 24, SET_RESOLUTION = [1024, 768], $							;Set device resolution and bit depth
				SET_CHARACTER_SIZE = [6, 10]
		ENDIF ELSE BEGIN
			IF (PATH_SEP() EQ '/') THEN SET_PLOT, 'X' $													;Set device to X if unix
										  ELSE SET_PLOT, 'WIN'

			WINDOW, XSIZE = 1024, YSIZE = 768, XPOS = (GET_SCREEN_SIZE())[0] - 1024 - 100, $	;Create window for vertical section
						TITLE = 'Vertical Section', /FREE
		ENDELSE
	
		!P.COLOR      = COLOR_24('black')																	;Foreground color
		!P.BACKGROUND = COLOR_24('white')																	;Background color
		!P.CHARSIZE   = 2.0																						;Set character size
		!P.FONT       = -1																						;Use Hershey fonts
		ERASE																											;Erase window
	ENDELSE

	xsn_pos = [0.10, 0.28, 0.925, 0.87]																		;Set cross-section position
	bar_pos = [0.25, 0.10, 0.750, 0.12]																		;Set color bar position

	latticks     = REFORM((MAP_2POINTS(x0, y0, x1, y1, NPATH = 6))[1,*], 6)
	nlatticks    = 5
	latticknames = STRING(latticks, FORMAT="(F6.2)")
	nlatminor    = 2

	lonticks     = (REFORM((MAP_2POINTS(x0, y0, x1, y1, NPATH = 6))[0,*], 6) + 360.0) MOD 360.0
	nlonticks    = 5
	lonticknames = STRING(lonticks, FORMAT="(F7.2)")
	nlonminor    = 2

	COLOR_BAR_24_KPB, table, OVER = table[-1], UNDER = table[0], $									;Draw color bar
		TICKS = barticks, $
		RANGE = [min_value, max_value], $
		TITLE = bartitle, $
		POSIT = bar_pos

	ij0 = CONVERT_COORD(xsn_pos[0:1], /NORMAL, /TO_DEVICE)
	ij1 = CONVERT_COORD(xsn_pos[2:*], /NORMAL, /TO_DEVICE)

	xsize = LONG(ij1[0] - ij0[0])
	ysize = LONG(ij1[1] - ij0[1])

	IF KEYWORD_SET(eps) THEN BEGIN
		section_plot2 = CONGRID(section_plot, 1200, 800)												;Resample image
		section_plot  = INTERPOLATE(section_plot, MAKEN(0, nsect-1, 1200), MAKEN(0, nz-1, 800), /GRID) ;Interpolate to high resolution
		inan          = WHERE((FINITE(section_plot2) AND ~FINITE(section_plot)), nnan)		;Find points lost to interpolation
		IF (nnan GT 0) THEN section_plot[inan] = section_plot2[inan]								;Fix points
	ENDIF ELSE BEGIN
		section_plot2 = CONGRID(section_plot, xsize, ysize)											;Resample image
		section_plot  = INTERPOLATE(section_plot, MAKEN(0, nsect-1, xsize), MAKEN(0, nz-1, ysize), /GRID) ;Interpolate to high resolution
		inan          = WHERE((FINITE(section_plot2) AND ~FINITE(section_plot)), nnan)		;Find points lost to interpolation
		IF (nnan GT 0) THEN section_plot[inan] = section_plot2[inan]								;Fix points
	ENDELSE

	ilow = WHERE((section_plot LT min_value),nlow)
	IF (nlow GT 0) THEN section_plot[ilow] = min_value													;Fix exceedances 

	ihigh = WHERE((section_plot GT max_value),nhigh)
	IF (nhigh GT 0) THEN section_plot[ihigh] = max_value												;Fix exceedances 

	image = IMAGE_24(COLOR_LOOKUP_24(section_plot, table, MIN = min_value, MAX = max_value, $
				MISSING = missing, /NAN))
	TV, image, ij0[0], ij0[1], TRUE = 3, XSIZE = xsize, YSIZE = ysize, /DEVICE

	CONTOUR, FLTARR(nsect, nz), FINDGEN(nsect), z, /NODATA, /NOERASE, $							;Draw cross-section
		XRANGE    = [0, nsect -1], $
		XTICKS    = 1, $
		XTICKN    = [' ', ' '], $
		XSTYLE    = 1, $
		YRANGE    = zrange, $
		YTITLE    = 'Altitude (km)', $
		YSTYLE    = 1, $
		POSITION  = xsn_pos

	IF (STRTRIM(var2,2) NE 'None') THEN BEGIN
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
		
		IF KEYWORD_SET(nearest_neighbor) THEN $
			contours = contours[isect,jsect,ksect] $														;Get nearest-neighbor grid columns along cross-section path
		ELSE $
			contours = INTERPOLATE(contours, ix, iy, iz, MISSING = !Values.F_NaN)				;Interpolate variable to cross-section

		IF KEYWORD_SET(smooth) THEN BEGIN
			ibad = WHERE((~FINITE(contours)), nbad)
			IF (nbad GT 0) THEN contours[ibad] = 0.0 + (var2 EQ 'rho_HV')
			contours = GAUSS_SMOOTH(contours, [2.0,0.0], /NAN, /EDGE_TRUNCATE)					;If set, smooth map variable
			IF (nbad GT 0) THEN contours[ibad] = !Values.F_NaN											;Remove bad areas
		ENDIF

		FOR i = 0, N_ELEMENTS(levels) -1 DO $		
			CONTOUR, contours, FINDGEN(nsect), z, OVERPLOT = 1, $
				LEVELS = levels[i], THICK = (i+1)*!P.THICK, C_LINESTYLE = 2*(levels[i] LT 0.0)
	ENDIF	

	AXIS, XAXIS  = 0, $																							;Draw bottom axis
		XTITLE    = 'Latitude (deg N)', $
		XTICKS    = nlatticks, $
		XTICKNAME = STRTRIM(latticknames,2), $	
		XMINOR    = nlatminor

	AXIS, XAXIS  = 1, $																							;Draw top axis
		XTITLE    = 'Longitude (deg E)', $
		XTICKS    = nlonticks, $
		XTICKNAME = STRTRIM(lonticknames,2), $
		XMINOR    = nlonminor

	XYOUTS, 0.5, 0.95, 'Vertical Section valid ' + MAKE_ISO_DATE_STRING(data.date), $		;Add title to plot
		ALIGN = 0.5, /NORMAL, CHARSIZE = 1.25*!P.CHARSIZE

	XYOUTS, xsn_pos[0] + 0.5*(xsn_pos[2]-xsn_pos[0]), 0.15, 'Distance = ' + $					;Write cross-section distance to window
		STRTRIM(STRING(TOTAL(distance), FORMAT="(F8.2)"),2) + ' km', ALIGN = 0.5, /NORMAL

	IF (tropopause GT 0) THEN BEGIN
		OPLOT, !X.CRANGE, [tropopause,tropopause], THICK = 2 + 4*KEYWORD_SET(eps), $			;Draw tropopause
			COLOR = COLOR_24('black')
		XYOUTS, 0.66*nsect, tropopause+0.5, 'Tropopause Level', /DATA, $							;Label tropopause
			COLOR = COLOR_24('black')
	ENDIF

	IF (map_type EQ 3) THEN BEGIN
		map_plot = FLOAT(NEXRAD_SL3D(data, ZMELT = zmelt))												;Set map plot to column-maximum of variable
		i0       = WHERE((map_plot EQ 0.0), n0)
		IF (n0 GT 0) THEN map_plot[i0] = !Values.F_NaN

		IF KEYWORD_SET(smooth) THEN BEGIN
			ibad     = WHERE((~FINITE(map_plot)), nbad)
			map_plot = GAUSS_SMOOTH(map_plot, [1.0,1.0], /NAN, /EDGE_TRUNCATE)					;If set, smooth map variable
			IF (nbad GT 0) THEN map_plot[ibad] = !Values.F_NaN											;Remove bad areas
		ENDIF

		csa_sect = map_plot[((ROUND(ix[*,0]) > 0) < (data.x.n-1)), $
								  ((ROUND(iy[*,0]) > 0) < (data.y.n-1))]									;Extract csa classification to cross-section path
		csa_col  = COLOR_LOOKUP_24(csa_sect, [COLOR_24('black'),COLOR_24(255,0,0),$			;Get colors of classification
							COLOR_24(255,222,0),COLOR_24(78,186,25),COLOR_24(150,25,150),$
							COLOR_24(25,75,255),COLOR_24('cyan')], MIN = 1.0, MAX = 7.0, $
							MISSING = missing, /NAN)
		
		PLOTS, FINDGEN(nsect), REPLICATE(z0-0.15,nsect), THICK = 5 + 10*KEYWORD_SET(eps), $	;Plot CSA classification line
			COLOR = csa_col
	ENDIF

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
ENDELSE

END
