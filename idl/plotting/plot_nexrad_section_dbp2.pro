PRO PLOT_NEXRAD_SECTION_DBP2, experiment, x0, y0, x1, y1, date, $
	DOMAIN           = domain, $
	MAP_TYPE         = map_type, $
	SECTION_TYPE     = section_type, $
	CHEMICAL         = chemical, $
	COORDINATES      = coordinates, $
	AVE_SECTION      = ave_section, $
	NEAREST_NEIGHBOR = nearest_neighbor, $
	REGION		     = region, $
	ZRANGE           = zrange, $
	POTTEMP          = pottemp, $
	KEY			     = key, $
	EPS              = eps, $
	PNG              = png


;+
; Name:
;		PLOT_WRF_SECTION
; Purpose:
;		This is a procedure to plot a vertical cross-section of WRF output.
; Calling sequence:
;		PLOT_WRF_SECTION, date, experiment, state
; Input:
;		date       : Analysis date
;		experiment : Simulation name. (e.g., 'APR04')
;		state      : Simulation state name. (e.g., 'era')
; Output:
;		A vertical section plot.
; Keywords:
;		DOMAIN       : Domain name. Default is 'd02'.
;		MAP_TYPE     : An integer value specifying the map type. e.g., 0
;		SECTION_TYPE : An integer value specifying the cross-section type. e.g., 0
;		CHEMICAL     : String name of chemical variable for cross-section. Default is 'O3'
;		COORDINATES  : If set, cross-section end points are longitude-latitude values.
;							Otherwise, grid indices are expected.
;		ZRANGE       : Optional keyword to specify the altitude range of the cross-section in km.
;							Default is [0,20]. 
;		EPS          : If set, output to PostScript.
;		PNG          : If set, write PNG image.
; Author and history:
;		Cameron R. Homeyer  2012-11-28.
;-

COMPILE_OPT IDL2																									;Set compile options

IF (N_ELEMENTS(date        ) EQ 0) THEN date         = MAKE_DATE(2013, 5, 17, 20, 50)			;Set default input variables
IF (N_ELEMENTS(experiment  ) EQ 0) THEN experiment   = '17MAY2013'
IF (N_ELEMENTS(state       ) EQ 0) THEN state        = 'CHEMISTRY'
IF (N_ELEMENTS(x0          ) EQ 0) THEN x0           = 0
IF (N_ELEMENTS(y0          ) EQ 0) THEN y0           = 0
IF (N_ELEMENTS(x1          ) EQ 0) THEN x1           = 1
IF (N_ELEMENTS(y1          ) EQ 0) THEN y1           = 1

IF (N_ELEMENTS(domain      ) EQ 0) THEN domain       = 1										;Set default keyword variables
IF (N_ELEMENTS(map_type    ) EQ 0) THEN map_type     = 0
IF (N_ELEMENTS(section_type) EQ 0) THEN section_type = 0
IF (N_ELEMENTS(chemical    ) EQ 0) THEN chemical     = 'O3'
IF (N_ELEMENTS(zrange      ) EQ 0) THEN zrange       = [1.0, 20.0]
IF (N_ELEMENTS(key		   ) EQ 0) THEN key 		 = chemical

date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Set date string

CASE experiment OF 
	'20120530' : BEGIN
		limit = [36.3, 256.3, 40.1, 266.6]
		nexrad  = NEXRAD_3_1_LEVEL3_3D_READ_NCDF(date, version = '3_1', product = '3d')
		experiment = '20120530_ncar'
		state 	   = 'd03_30km'
		domain = 1
	END
ENDCASE

data     = NEXRAD_FILTER(nexrad)
data 	 = NEXRAD_REMOVE_CLUTTER(data)

table = GRIDRAD_VIEWER_COLOR(color_index)																	;Set color bar
				
nc_info  = TAG_NAMES(data)																						;Get tag names
var_info = TAG_NAMES(data.dbz)																				;Get tag names
void     = WHERE((var_info EQ 'WVALUES'),v2orv3test)													;Check for v2 or v3 compositing method file
void     = WHERE((nc_info  EQ 'NOBS'),  sparse_test)

IF (N_ELEMENTS(missing   ) EQ 0) THEN missing    = COLOR_24(200,200,200)						;Set default color for missing values
IF (N_ELEMENTS(text_color) EQ 0) THEN text_color = COLOR_24('white')								;Set default color for missing values
IF (N_ELEMENTS(bar_min   ) EQ 0) THEN bar_min    = [ 0.0,-1.0,-1.0,0.7,0.00, 0.0, 0.0,-6.0,-6.0, 5.0,-2.0];Set color bar defaults
IF (N_ELEMENTS(bar_max   ) EQ 0) THEN bar_max    = [75.0, 4.0, 4.0,1.0,0.05,45.0,10.0, 6.0, 6.0,20.0, 2.0]
IF (N_ELEMENTS(bar_ticks ) EQ 0) THEN bar_ticks  = [   5,   5,   5,  3,   5,   5,   5,   6,   6,   3,   4]


wrf_x     = WRF_READ_VAR('Longitude',       date, experiment, state, DOMAIN = domain)			;Read variables
wrf_y     = WRF_READ_VAR('Latitude',        date, experiment, state, DOMAIN = domain)
wrf_z 	  = WRF_READ_VAR('Z', 				date, experiment, state, DOMAIN = domain)
wrf_trop  = WRF_READ_VAR('Z_trop',          date, experiment, state, DOMAIN = domain)

PRINT, 'Section Start (Lon, Lat): '
PRINT, x0, y0
PRINT
PRINT, 'Section End (Lon, Lat): '
PRINT, x1, y1
 
ztrop1 = MEDIAN(wrf_trop.values, 30)
dim 	 = SIZE(wrf_z.values, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
xyz_trop = REBIN(ztrop1, dim[0], dim[1], dim[2], /SAMPLE)

;IF KEYWORD_SET(coordinates) THEN BEGIN
	PRINT, 'coordinates called'
	region = [x0,y0,x1,y1]
	data.x.values = (data.x.values + 360.0) MOD 360.0															;Ensure longitudes on 0-360 grid
	region[[0,2]] = (region[[0,2]] + 360.0) MOD 360.0
		x0=region[0]
		x1=region[2]
		y0=region[1]
		y1=region[3]
	ij = WHERE(((data.x.values GE x0) AND (data.y.values GE y0) AND $											;Find all grid points between desired endpoints of cross-section
					(data.x.values LE x1) AND (data.y.values LE y1)), nxy)
					
	IF (nxy GT 0) THEN BEGIN
		ij = ARRAY_INDICES(data.x.values, ij)																		;Expand indices of search to 2-D indices of grid
		x0 = MIN(ij[0,*])																							;Reset cross-section end points to grid indices
		y0 = MIN(ij[1,*])
		x1 = MAX(ij[0,*])
		y1 = MAX(ij[1,*])
	ENDIF
;ENDIF

nx    = SIZE(data.x.values, /DIMENSIONS)	
ny    = SIZE(data.y.values, /DIMENSIONS)	
nz    = SIZE(data.z.values, /DIMENSIONS)	

min_value = bar_min[0]																			;Set section thresholds based on variable type
max_value = bar_max[0]
barticks  = bar_ticks[0]
units     = 'dBZ'
bartitle  = 'Z!DH!N (dBZ)'
values    = data.dbz.values

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

datadx = data.x.values[1] - data.x.values[0]
datady = data.y.values[1] - data.y.values[0]

IF KEYWORD_SET(nearest_neighbor) THEN BEGIN
	isect0 = VALUE_LOCATE(data.x.values, datadx*ROUND(x/datadx))									;Get grid indices for cross-section path
	jsect0 = VALUE_LOCATE(data.y.values, datady*ROUND(y/datady))
	k0    = VALUE_LOCATE(data.z.values, z0)															;Get grid indices for altitude range
	k1    = VALUE_LOCATE(data.z.values, z1)
	nz0   = k1 - k0 + 1																							;Set number of vertical levels
	ksect0 = k0 + LINDGEN(nz0)																					;Set vertical indices for section
	z0    = data.z.values[k0:k1]																				;Extract altitude array

	
	isect = REBIN(       isect0,        nsect, nz0)														;Rebin section indices to two dimensions
	jsect = REBIN(       jsect0,        nsect, nz0)
	ksect = REBIN(REFORM(ksect0,1,nz0), nsect, nz0)

	section_plot =  values[isect,jsect,ksect]																;Get nearest-neighbor grid columns along cross-section path
	section_plot = INTERPOLATE(section_plot, FINDGEN(nsect), $										;Convert section to regular 0.25-km grid
							INTERPOL(FINDGEN(nz0), z0, z), /GRID)
ENDIF ELSE $
	section_plot = INTERPOLATE(values,  ix, iy, iz, MISSING = !Values.F_NaN)					;Interpolate variable to cross-section

IF KEYWORD_SET(median) THEN BEGIN
	ibad = WHERE((~FINITE(section_plot)), nbad)															;Get indices of marginal/missing values

	section_plot = MEDIAN(section_plot, 5)																	;If set, smooth section variable
	IF (nbad GT 0) THEN section_plot[ibad] = !Values.F_NaN											;Remove bad areas
ENDIF

CASE map_type OF
	0 : BEGIN
		 var           = data.dbz.values	;Read radar reflectivity
		 ;bad = WHERE (var.values EQ 0.0, bad_count, COMPLEMENT = good, NCOMPLEMENT = good_count)
		 ;var.values [bad ] = -35.0000
		 
		 map_plot      = MAX(var, DIM = 3, /NAN)													;Compute column-maximum map
		 map_bar_title = 'Reflectivity (dBZ)'																;Set color bar title
		 map_bar_min   = 0.0																						;Set echo top minimum
		 map_bar_max   = 75.0																					;Set echo top maximum
		 map_bar_ticks = 5																						;Set number of color bar ticks
		 map_table     = [COLOR_24(230, 230, 230),VISUALIZE_88D_COLOR(0)]							;Set color table
		 map_levels    = [-100.0, 5.0*FINDGEN(N_ELEMENTS(map_table))]								;Set contour levels
		 END

	ELSE : MESSAGE, 'Requested map type does not exist!'
ENDCASE

CASE section_type OF
	0 : BEGIN

		 ;fill_sect = section_plot						

		 fill_sect_table  = VISUALIZE_88D_COLOR(0)														;Set color-fill cross-section specs
		 fill_sect_levels = 5.0*FINDGEN(N_ELEMENTS(fill_sect_table))
		 sect_bar_min     = 0.0	
		 sect_bar_max     = 75.0
		 sect_bar_ticks   = 5	
		 sect_bar_title   = 'Reflectivity (dBZ)'
		 		 
		 END
	ELSE : MESSAGE, 'Requested cross-section type does not exist!'									;Message error
ENDCASE

;IF (~KEYWORD_SET(pottemp)) THEN BEGIN
;    var = WRF_READ_VAR('T', date, experiment, state, DOMAIN = domain)						;Read temperature variable from WRF output
;    var.values = ((1000.0/(WRF_READ_VAR('P', date, experiment, state, $						;Compute potential temperature
;    					DOMAIN = domain)).values)^(!Rair/!Cp))*(var.values)
;    cont_sect  = INTERPOLATE(var.values,   iisect, jjsect, kksect, MISSING = !Values.F_NaN)	;Interpolate potential temperature to contour cross-section
;    cont_sect_color  = COLOR_24('black')																;Set contour cross-section specs
;    cont_sect_levels = 200.0 + 4.0*FINDGEN(250)
;    cont_sect_thick  = 1
;ENDIF

;cont_sect1 = INTERPOLATE(cloud.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)	;Interpolate cloud concentration to contour cross-section
;cont_sect1_color  = COLOR_24('gray50')																;Set contour cross-section specs
;cont_sect1_levels = 1.0E-5
;cont_sect1_thick  = 3

;u = (WRF_READ_VAR('u', date, experiment, state, DOMAIN = domain)).values						;Read temperature variable from WRF output
;v = (WRF_READ_VAR('v', date, experiment, state, DOMAIN = domain)).values	
;w = (WRF_READ_VAR('w', date, experiment, state, DOMAIN = domain)).values
;u_sect = INTERPOLATE(u, iisect, jjsect, kksect, MISSING = !Values.F_NaN)	;Interpolate wind speeds to contour cross-section		 
;v_sect = INTERPOLATE(v, iisect, jjsect, kksect, MISSING = !Values.F_NaN)	;Interpolate wind speeds to contour cross-section		 
;w_sect = INTERPOLATE(w, iisect, jjsect, kksect, MISSING = !Values.F_NaN)
;
;IF (~KEYWORD_SET(pottemp)) THEN BEGIN
;	time0 = SYSTIME(/SECONDS)
;    trop.values = MEDIAN(trop.values,30)
;    time_trop = SYSTIME(/SECONDS) - time0
;	trop_sect = INTERPOLATE(trop.values, isect , jsect)
;ENDIF ELSE BEGIN
;    theta_trop = FLTARR(dim[0],dim[1])
;    FOR ii = 0, dim[0]-1 DO BEGIN
;    	FOR jj = 0, dim[1]-1 DO BEGIN
;    		aaa = [INDEX_OF_NEAREST_CRH(z.values[ii,jj,*],xyz_trop[ii,jj,*])]
;    		theta_trop[ii,jj] = theta[ii,jj,aaa]
;    	ENDFOR
;    ENDFOR
;	theta_trop = MEDIAN(theta_trop, 30)
;	theta_sect = INTERPOLATE(theta_trop, isect, jsect)
;ENDELSE

outdir  = !WRF_DIRECTORY + experiment + '/paper/plots/xsect_final/' 
epsfile = outdir + date_string + '.eps'											
pdffile = outdir + date_string + '.pdf'											
pngfile = outdir + date_string + '.png'											

FILE_MKDIR, outdir																								;Create output directory, if necessary

IF KEYWORD_SET(eps) THEN BEGIN	
	PS_ON, FILENAME = epsfile, PAGE_SIZE = [10.0, 5.0], MARGIN = 0.0, /INCHES					;Switch to Postscript device
	DEVICE, /ENCAPSULATED
	!P.FONT     = 0																								;Hardware fonts
	!P.CHARSIZE = 1.25
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS																							;Load basic color definitions
	thick_scale = 3
ENDIF ELSE BEGIN
	SET_PLOT, 'X'
	WINDOW, XSIZE = 1200, YSIZE = 700																		;Open graphics window
	!P.COLOR      = COLOR_24('black')																		;Foreground color
	!P.BACKGROUND = COLOR_24('white')																		;Background color
	!P.CHARSIZE   = 2.5		
	!P.FONT       = -1																						;Use Hershey fonts
	thick_scale   = 1
ENDELSE

!P.MULTI = [0, 2, 1]																								;Set multiple plots

map_pos      = [0.025, 0.22, 0.375, 0.92]																	;Set map position
map_bar_pos  = [0.055, 0.11, 0.345, 0.13]																	;Set map color bar position
xsn_pos      = [0.450, 0.22, 0.975, 0.92]																	;Set cross-section position
xsn_bar_pos  = [0.500, 0.11, 0.925, 0.13]																	;Set cross-section color bar position

yll = wrf_y.values[   0,   0]																						;Set domain boundary points
yul = wrf_y.values[   0,ny-1]
yur = wrf_y.values[nx-1,ny-1]
ylr = wrf_y.values[nx-1,   0]
xll = wrf_x.values[   0,   0]
xul = wrf_x.values[   0,ny-1]
xur = wrf_x.values[nx-1,ny-1]
xlr = wrf_x.values[nx-1,   0]

xc = INTERPOLATE(wrf_x.values, 0.5*(nx-1), 0.5*(ny-1))														;Get central grid point
yc = INTERPOLATE(wrf_y.values, 0.5*(nx-1), 0.5*(ny-1))

IF (xc EQ 0.0) THEN BEGIN
	wrf_x.values   = REBIN(       FINDGEN(nx),         nx, ny)											;Set coordinates to grid indices
	wrf_y.values   = REBIN(REFORM(FINDGEN(ny), 1, ny), nx, ny)
	map_pos[0] = 0.075																							;Adjust map position
	map_pos[1] = 0.300
ENDIF

title0   = 'Column-Maximum Map'

IF (xc NE 0.0) THEN $
	MAP_SET, yc, xc, 0, CONIC = 1, $																			;Draw map
		LIMIT     = [yll,xll,yul,xul,yur,xur,ylr,xlr], $
		ISOTROPIC = 1, $
		TITLE     = title0 + ' valid ' + MAKE_ISO_DATE_STRING(data.date), $
		POSITION  = map_pos

TV_MAP_IMAGE, table, map_plot, $																;Map image
	LATMIN    = limit[0], $
	LATMAX    = limit[2], $
	LONMIN    = limit[1], $
	LONMAX    = limit[3], $
	MIN_VALUE = min_value, $
	MAX_VALUE = max_value, $
	MISSING   = !Values.F_NaN, $
	CMISSING  = missing

levels   = MAKEN(bar_min[0], bar_max[0], bar_ticks[0] + 1)

;; Comment out these two parts for manuscript image 
MAP_CONTINENTS, /USA, /CONTINENTS, /HIRES, COLOR = COLOR_24('black'), THICK = 2*!P.THICK		;Draw continents
;MAP_CONTINENTS, /USA, /CONTINENTS

;MAP_SET, 0, 180, 0, CONIC = 1, STANDARD_PARALLELS=[0,1], $																;Draw map
;MAP_SET, 0, 180, 0, CONIC = 1, /NOBORDER, $																;Draw map
;	LIMIT     = limit, $
MAP_SET, yc, xc, 0, CONIC = 1,  /NOBORDER, $																;Draw map
	LIMIT     = [yll,xll,yul,xul,yur,xur,ylr,xlr], $
	ISOTROPIC = 1, $
	TITLE     = title0 + ' valid ' + MAKE_ISO_DATE_STRING(data.date), $
	POSITION  = map_pos, $
	NOERASE   = 1

COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $;UNDER = table[0], $									;Draw color bar
	TICKS = barticks, $
	RANGE = [min_value, max_value], $
	TITLE = bartitle, $
	POSIT = map_bar_pos

xsect = INTERPOLATE(data.x.values, isect0, jsect0)
ysect = INTERPOLATE(data.y.values, isect0, jsect0)

IF (xc NE 0.0) THEN BEGIN
;	MAP_CONTINENTS, /CONT, /USA, THICK = thick_scale													;Draw continental & state outlines
	OPLOT, xsect, ysect, THICK = 2*thick_scale															;Superimpose cross-section path

	XYOUTS, xsect[0], ysect[0] + 0.025*[ysect[nsect-1] - ysect[0]], 'A', $						;Label endpoints of cross-section on map
		ALIGN = 0.5, CHARSIZE = 1.5*!P.CHARSIZE, CHARTHICK = 2.0*thick_scale

	XYOUTS, xsect[nsect-1], ysect[nsect-1] + 0.025*[ysect[nsect-1] - ysect[0]], 'B', $
		ALIGN = 0.5, CHARSIZE = 1.5*!P.CHARSIZE, CHARTHICK = 2.0*thick_scale
ENDIF ELSE BEGIN
	OPLOT, isect, jsect, THICK = 2*thick_scale
	
	XYOUTS, isect[0], jsect[0] + 0.025*[jsect[nsect-1] - jsect[0]], 'A', $						;Label endpoints of cross-section on map
		ALIGN = 0.5, CHARSIZE = 1.5*!P.CHARSIZE, CHARTHICK = 2.0*thick_scale

	XYOUTS, isect[nsect-1], jsect[nsect-1] + 0.025*[jsect[nsect-1] - jsect[0]], 'B', $
		ALIGN = 0.5, CHARSIZE = 1.5*!P.CHARSIZE, CHARTHICK = 2.0*thick_scale
ENDELSE
;
;COLOR_BAR_24_KPB, map_table[1:*], OVER = map_table[-1], UNDER = map_table[0], $						;Draw map color bar
;	TICKS = map_bar_ticks, $
;	RANGE = [map_bar_min, map_bar_max], $
;	TITLE = map_bar_title, $
;	POSIT = map_bar_pos
;
;!P.POSITION = 0
;
;storm_motion = 21.0
;x_freq = 25
;z_freq = 5
;n_nsect = nsect/x_freq

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin Image Plot 																		 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ij0 = CONVERT_COORD(xsn_pos[0:1], /NORMAL, /TO_DEVICE)
;ij1 = CONVERT_COORD(xsn_pos[2:*], /NORMAL, /TO_DEVICE)
;
;xsize = LONG(ij1[0] - ij0[0])
;ysize = LONG(ij1[1] - ij0[1])

;section_plot = fill_sect

dz     = 100.0																					;Set altitude resolution of cross-section
nzsect = LONG(1000.0*(zrange[1]-zrange[0])/dz) + 1												;Compute number of levels for cross-section based on altitude range
zsect  = 1000.0*zrange[0] + dz*FINDGEN(nzsect)													;Set altitude array for plotting

;table = [HCL_COLOR_TABLE(10, HUE_RANGE = [220.0,220.0], SAT_RANGE = [0.2,1.0], /REV), $
;               HCL_COLOR_TABLE(10, HUE_RANGE = [  0.0,  0.0], SAT_RANGE = [0.2,1.0])]
     
COLOR_BAR_24_KPB, table, OVER = table[-1], UNDER = table[0], $                                                          ;Draw color bar
         TICKS = barticks, $
		 RANGE = [sect_bar_min, sect_bar_max], $
		 TITLE = sect_bar_title, $
		 POSIT = xsn_bar_pos

ij0 = CONVERT_COORD(xsn_pos[0:1], /NORMAL, /TO_DEVICE)
ij1 = CONVERT_COORD(xsn_pos[2:*], /NORMAL, /TO_DEVICE)

xsize = LONG(ij1[0] - ij0[0])
ysize = LONG(ij1[1] - ij0[1])

IF KEYWORD_SET(eps) THEN BEGIN
        section_plot2 = CONGRID(section_plot, 1200, 800)                                                                                                ;Resample image
        section_plot  = INTERPOLATE(section_plot, MAKEN(0, nsect-1, 1200), MAKEN(0, nzsect-1, 800), /GRID) ;Interpolate to high resolution
        inan          = WHERE((FINITE(section_plot2) AND ~FINITE(section_plot)), nnan)          ;Find points lost to interpolation
        IF (nnan GT 0) THEN section_plot[inan] = section_plot2[inan]                                                            ;Fix points
ENDIF ELSE BEGIN
        section_plot2 = CONGRID(section_plot, xsize, ysize)                                                                                     ;Resample image
        section_plot  = INTERPOLATE(section_plot, MAKEN(0, nsect-1, xsize), MAKEN(0, nzsect-1, ysize), /GRID) ;Interpolate to high resolution
        inan          = WHERE((FINITE(section_plot2) AND ~FINITE(section_plot)), nnan)          ;Find points lost to interpolation
        IF (nnan GT 0) THEN section_plot[inan] = section_plot2[inan]                                                            ;Fix points
ENDELSE

ilow = WHERE((section_plot LT sect_bar_min),nlow)
IF (nlow GT 0) THEN section_plot[ilow] = sect_bar_min                                                                                                       ;Fix exceedances 

ihigh = WHERE((section_plot GT sect_bar_max),nhigh)
IF (nhigh GT 0) THEN section_plot[ihigh] = sect_bar_max                                                                                            ;Fix exceedances 

image = IMAGE_24(COLOR_LOOKUP_24(section_plot, table, MIN = sect_bar_min, MAX = sect_bar_max, $
                        MISSING = missing, /NAN))
TV, image, ij0[0], ij0[1], TRUE = 3, XSIZE = xsize, YSIZE = ysize, /DEVICE

STOP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End Image Plot 																		 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



IF (~KEYWORD_SET(pottemp)) THEN BEGIN
    CONTOUR, section_plot, FINDGEN(nsect), 0.001*zsect, $														;Draw cross-section with color-filled variable
    	CELL_FILL = 0, $
    	C_COLOR   = fill_sect_table, $
    	LEVELS    = fill_sect_levels, $
    	XRANGE    = [0, nsect -1], $
    	XTICKS    = 1, $
    	XTICKN    = ['A', 'B'], $
    	XSTYLE    = 1, $
    	YRANGE    = zrange, $
 ;   	YLOG	  = 1, $
    	YSTYLE    = 1, $
    	YTITLE    = 'Altitude (km)', $
    	TITLE     = 'Vertical Section', $
		FOLLOW    = 1, $
    	POSITION  = xsn_pos, $
    	NODATA    = 1

ENDIF ELSE BEGIN
    CONTOUR, section_plot, FINDGEN(nsect), zsect, $														;Draw cross-section with color-filled variable
    	CELL_FILL = 1, $
    	C_COLOR   = fill_sect_table, $
    	LEVELS    = fill_sect_levels, $
    	XRANGE    = [0, nsect -1], $
    	XTICKS    = 1, $
    	XTICKN    = ['A', 'B'], $
    	XSTYLE    = 1, $
    	YRANGE    = zrange, $
    	YSTYLE    = 1, $
    	YTITLE    = 'Potential Temperature (K)', $
    	TITLE     = 'Vertical Section', $
    	FOLLOW    = 1, $
    	POSITION  = xsn_pos, $
    	NODATA    = 1

ENDELSE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Beginning of block of code to plot storm-relative horizontal wind as vectors
;These vectors are plotted over color-filled ozone contours
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;dim = SIZE(u_sect,/DIMENSIONS)
;angle_sect = ATAN((x1-x0)/(y1-y0))*!RADEG + 180.0
;angle_wind = FLTARR(dim[0],dim[1])
;angle_new  = FLTARR(dim[0],dim[1])
;theta      = FLTARR(dim[0],dim[1])
;spd        = FLTARR(dim[0],dim[1])
;dot        = FLTARR(dim[0],dim[1])
;delta_angle = angle_sect - 90.0
;
;FOR i = 0, dim[0]-1 DO BEGIN
;	FOR j = 0, dim[1]-1 DO BEGIN
;    	IF ((u_sect[i,j] GT 0.0) AND (v_sect[i,j] GT 0.0)) THEN angle_wind[i,j] = ATAN(u_sect[i,j]/v_sect[i,j])*!RADEG
;        IF ((u_sect[i,j] LT 0.0) AND (v_sect[i,j] GT 0.0)) THEN angle_wind[i,j] = ATAN(u_sect[i,j]/v_sect[i,j])*!RADEG + 360.0
;        IF ((u_sect[i,j] LT 0.0) AND (v_sect[i,j] LT 0.0)) THEN angle_wind[i,j] = ATAN(u_sect[i,j]/v_sect[i,j])*!RADEG + 180.0
;        IF ((u_sect[i,j] GT 0.0) AND (v_sect[i,j] LT 0.0)) THEN angle_wind[i,j] = ATAN(u_sect[i,j]/v_sect[i,j])*!RADEG + 180.0
;
;		angle_new[i,j] = angle_wind[i,j] - angle_sect
;		IF (angle_new[i,j] LT 0.0) THEN angle_new[i,j] = 360.0 + angle_new[i,j]
;       
;        theta[i,j] = angle_wind[i,j] - (angle_sect) 
;		spd  [i,j] = SQRT(u_sect[i,j]^2 + v_sect[i,j]^2 + w_sect[i,j]^2)
;		dot  [i,j] = spd[i,j] * COS(theta[i,j]*!DDTOR) - storm_motion
;	ENDFOR
;ENDFOR
;
;;spd = SQRT(u_sect^2 + v_sect^2 + w_sect^2)
;;section_wind = spd * SIN(angle_new*!DDTOR) - storm_motion
;section_wind = dot
;
;h_sample = FLTARR(dim[0],dim[1])*!Values.F_NaN
;h_sample[0:dim[0]-1:x_freq, 0:dim[1]-1:z_freq]= section_wind [0:dim[0]-1:x_freq, 0:dim[1]-1:z_freq]
;
;w_sample = FLTARR(dim[0],dim[1])*!Values.F_NaN
;w_sample[0:dim[0]-1:x_freq, 0:dim[1]-1:z_freq]= w_sect[0:dim[0]-1:x_freq, 0:dim[1]-1:z_freq]
;
;;IF (section_type EQ 3) THEN VELOVECT, h_sample, w_sample, FINDGEN(nsect), 0.001*zsect, /OVERPLOT, LENGTH= 50, THICK=3
;
;IF ((section_type EQ 3) OR (section_type EQ 2)) THEN VELOVECT, section_wind[0:*:x_freq,0:*:z_freq], w_sect[0:*:x_freq,0:*:z_freq], $
;					(FINDGEN(nsect))[0:*:x_freq], 0.001*zsect[0:*:z_freq], /OVERPLOT, LENGTH=4, THICK = 1
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End of block of code to plot storm-relative horizontal wind as vectors
;These vectors are plotted over color-filled ozone contours
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Commented out theta lines for manuscript

IF (~KEYWORD_SET(pottemp)) THEN BEGIN
    CONTOUR,  cont_sect, FINDGEN(nsect), 0.001*zsect, $														;Overplot contour cross-section
    	OVERPLOT = 1, $
    	FOLLOW	 = 1, $
    	LEVELS   = cont_sect_levels, $
    	C_COLOR  = cont_sect_color, $
    	C_THICK  = cont_sect_thick*thick_scale

	IF (~(section_type EQ 0) OR (section_type EQ 1)) THEN BEGIN
        CONTOUR, cont_sect1, FINDGEN(nsect), 0.001*zsect, $														;Overplot contour cross-section
        	OVERPLOT = 1, $
        	LEVELS   = cont_sect1_levels, $
        	C_COLOR  = cont_sect1_color, $
        	C_THICK  = cont_sect1_thick*thick_scale

		IF ((state EQ 'd03_30km') OR (state EQ 'd03_30km_icloud')) THEN $
 		    var = (WRF_READ_VAR('O3_tracer', date, experiment, state, DOMAIN = domain)).values*1.0E3			;Read temperature variable from WRF output
		
		IF (state EQ 'd02_30km') OR (experiment NE '20120530_ncar') THEN $
 		    var = (WRF_READ_VAR('O3', date, experiment, state, DOMAIN = domain)).values*1.0E3			;Read temperature variable from WRF output

 		
 		cont_sect  = INTERPOLATE(var,   iisect, jjsect, kksect, MISSING = !Values.F_NaN)					;Interpolate potential temperature to contour cross-section
 		cont_sect_color  = COLOR_24('black')																;Set contour cross-section specs
 		cont_sect_levels = 25.0*FINDGEN(40)
 		cont_sect_thick  = 1

	   	;IF (chemical NE 'O3_tracer') THEN BEGIN
		   CONTOUR, cont_sect, FINDGEN(nsect),  0.001*zsect, $															;Overplot contour cross-section
			OVERPLOT = 1, $
			LEVELS   = [100.0], $;, 125.0, 150.0], $;[100.0, 150.0, 200.0, 250.0], $
			C_COLOR  = cont_sect_color, $
			C_THICK  = cont_sect_thick*thick_scale*3.0
		;ENDIF
	ENDIF
ENDIF ELSE BEGIN
	IF ((section_type EQ 2) OR (section_type EQ 3)) THEN BEGIN
        CONTOUR, cont_sect1, FINDGEN(nsect), zsect, $														;Overplot contour cross-section
        	OVERPLOT = 1, $
        	LEVELS   = cont_sect1_levels, $
        	C_COLOR  = cont_sect1_color, $
        	C_THICK  = cont_sect1_thick*thick_scale
	ENDIF
ENDELSE

USERSYM_CIRCLE, /FILL																						;Load circle plotting symbol

IF (~KEYWORD_SET(pottemp)) THEN BEGIN
    OPLOT, 10*FINDGEN(nsect/10), 0.001*trop_sect[0:*:10], $													;Draw tropopause altitudes as circles
    	PSYM = 8, SYMSIZE = 2.5 - KEYWORD_SET(eps)
ENDIF ELSE BEGIN
    OPLOT, 10*FINDGEN(nsect/10), theta_sect[0:*:10], $														;Draw tropopause altitudes as circles
    	PSYM = 8, SYMSIZE = 2.5 - KEYWORD_SET(eps)
ENDELSE

;OPLOT, 10*FINDGEN(nsect/10), 0.001*zsect0[*,43], $															;Draw tropopause altitudes as circles
;	PSYM = 2, SYMSIZE = 2.5 - KEYWORD_SET(eps)


;COLOR_BAR_24_KPB, fill_sect_table, UNDER = fill_sect_table[0], OVER = fill_sect_table[-1], $				;Draw cross-section color bar
;	TICKS  = sect_bar_ticks, $
;	RANGE  = [sect_bar_min, sect_bar_max], $
;	TITLE  = sect_bar_title, $
;	POSIT  = xsn_bar_pos, $
;	XTICKN = sect_tick_name

!P.MULTI = 0																										;Reset multiple plots
!P.POSITION = 0

IF KEYWORD_SET(eps) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file

END
