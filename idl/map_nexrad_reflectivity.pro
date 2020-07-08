PRO MAP_NEXRAD_REFLECTIVITY, event, date, $
	OFFSET = offset, $
	DOMAIN = domain, $
	REGION = region, $
	Z_buff = z_buff, $
	IMAGE  = image, $
	SECTION = section, $
	EPS = eps, $
	PNG = png

;+
; Name:
;		MAP_NEXRAD_REFLECTIVITY
; Purpose:
;		This is a template for creating IDL procedure files. 
; Calling sequence:
;		MAP_NEXRAD_REFLECTIVITY, event, date, date1
; Input:
;		event  	   : String variable of event name. (e.g., '20120519')
;		date  	   : CDATE
; Output:
;		A map of simulated composite reflectivity. 
; Keywords:
;		DOMAIN : Simulation domain number. Default is 1. 
;		EPS    : If set, output to PostScript.
;		PDF    : If set, output to PDF.
;		PNG    : If set, write PNG image.
; Author and history:
;		Cameron R. Homeyer  2012-10-05.
;-

COMPILE_OPT IDL2																				;Set compile options

print_bounds = 0

IF (N_ELEMENTS(event     ) EQ 0) THEN event      = '20120519'
IF (N_ELEMENTS(date      ) EQ 0) THEN date       = MAKE_DATE(2012,5,19,22,30)
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 2
IF (N_ELEMENTS(offset    ) EQ 0) THEN offset     = 0 ELSE print_bounds = 1

date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT)

nexfile  = !NEXRAD_DIRECTORY + event + '/' + date_string + 'Z.nc'
nexrad   = NEXRAD_READ_LEVEL2_2(nexfile)													;Read NEXRAD composite

CASE event OF
	'20120519' : BEGIN
;		limit = [33.1, 258.3, 39.0, 263.9] 
;		limit = [33.5, 257.6, 38.5, 264.5]													; Matches model domain
;		limit = [33.5, 258.3, 38.5, 264.5]													; Better aspect ratio
		limit = [35.0, 259.6, 37.0, 262.0]													; For paper
	END

	'20120529' : BEGIN
;		limit = [34.8, 259.7, 39.3, 264.6] 
		limit = [35.5, 259.7, 37.8, 263.0]
	END

	'20120530' : BEGIN
		limit = [36.3, 256.3, 40.1, 266.6]
	END

	'20120601' : BEGIN
;		limit = [33.1, 256.3, 38.0, 262.0]
		limit = [34.2, 257.0, 37.1, 260.6]
	END

	'20110518' : BEGIN
		limit = [30.8, 256.9, 44.7, 276.5]
		nexrad  = NEXRAD_3_1_LEVEL3_3D_READ_NCDF(date, version = '3_1', product = '3d')
	END
	
	'20130805' : BEGIN
		limit = [20.8, 242.9, 45.7, 284.5]
		nexrad  = NEXRAD_3_1_LEVEL3_3D_READ_NCDF(date, version = '3_1', product = '3d')
	END
ENDCASE

data     = NEXRAD_FILTER(nexrad)
data 	 = NEXRAD_REMOVE_CLUTTER(data)

IF (N_ELEMENTS(missing  ) EQ 0) THEN missing   = COLOR_24(200,200,200)							;Set default color for missing values
IF (N_ELEMENTS(bar_min  ) EQ 0) THEN bar_min   = [ 0.0,-1.0,-1.0,0.7]							;Set color bar defaults
IF (N_ELEMENTS(bar_max  ) EQ 0) THEN bar_max   = [75.0, 4.0, 4.0,1.0]
IF (N_ELEMENTS(bar_ticks) EQ 0) THEN bar_ticks = [   5,   5,   5,  3]

min_value = bar_min[0]																			;Set mapping thresholds based on variable type
max_value = bar_max[0]
barticks  = bar_ticks[0]
units     = 'dBZ'
bartitle  = 'Z!DH!N (dBZ)'
values    = data.dbz.values

map_plot = MAX(values,DIM=3,/NAN)																;Set map plot to column-maximum of variable
table    = VISUALIZE_88D_COLOR(color_index)														;Set color bar
title0   = 'Column-Maximum Map'

dim = SIZE(data.dbz.values, /DIMENSIONS)
wfactor = 400.0/(dim[0]) + 400.0/(dim[1])

outdir  = !WRF_DIRECTORY + event + '/paper/plots/'
epsfile = outdir + date_string + '.eps'															;EPS filename
pdffile = outdir + date_string + '.pdf'															;PDF filename
pngfile = outdir + date_string + '.png'															;PNG filename

FILE_MKDIR, outdir																								;Create output directory, if necessary

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
;SET_PLOT, 'X'
;WINDOW, XSIZE = wfactor*(dim[0]), YSIZE = wfactor*(dim[1])										;Open graphics window

!P.BACKGROUND = COLOR_24('white')
!P.COLOR      = COLOR_24('black')
!P.CHARSIZE   = 2.0 - 0.35*((PATH_SEP() EQ '\') AND (~KEYWORD_SET(png)))						;Scale for Windows
!P.FONT       = -1																				;Use Hershey fonts
!P.THICK      = 1																				;Increase line thickness
!X.THICK      = 1
!Y.THICK      = 1

levels  = min_value + ((max_value-min_value)/N_ELEMENTS(table))*FINDGEN(N_ELEMENTS(table))		;Compute contour levels
															
map_pos = [0.05, 0.15, 0.95, 0.95]																;Set map position
;map_pos = [0.0, 0.0, 1.0, 1.0]
bar_pos = [0.25, 0.10, 0.75, 0.12]	

ERASE

ratio   = (limit[2]-limit[0])/(limit[3]-limit[1])												;Compute dy/dx
nximage = 1200
nyimage = LONG(ratio*nximage)																	;Scale number of lat pixels
ii = INTERPOL(FINDGEN(data.x.n),data.x.values,MAKEN(limit[1],limit[3],nximage))
ji = INTERPOL(FINDGEN(data.y.n),data.y.values,MAKEN(limit[0],limit[2],nyimage))
ii2 = REBIN(       ROUND(ii),              nximage, nyimage)
ji2 = REBIN(REFORM(ROUND(ji), 1, nyimage), nximage, nyimage)
ibad = WHERE(((ii2 LT 0) OR (ji2 LT 0) OR $
			(ii2 GT data.x.n-1) OR (ji2 GT data.y.n-1)), nbad)

map_plot2 = map_plot[ROUND(ii2),ROUND(ji2)]														;Extract map values for region

map_plot = INTERPOLATE(map_plot, ii, ji, /GRID, MISSING = !Values.F_NaN)						;Ensure high-resolution image
inan     = WHERE((FINITE(map_plot2) AND ~FINITE(map_plot)), nnan)								;Find echo lost to interpolation
IF (nnan GT 0) THEN map_plot[inan] = map_plot2[inan]											;Fix echo

IF (nbad GT 0) THEN map_plot[ibad] = !Values.F_NaN

ilow = WHERE((map_plot LE min_value),nlow)
IF (nlow GT 0) THEN map_plot[ilow] = min_value													;Fix exceedances 

ihigh = WHERE((map_plot GE max_value),nhigh)
IF (nhigh GT 0) THEN map_plot[ihigh] = max_value												;Fix exceedances 

COLOR_BAR_24_KPB, table, OVER = table[-1], UNDER = table[0], $									;Draw color bar
	TICKS = barticks, $
	RANGE = [min_value, max_value], $
	TITLE = bartitle, $
	POSIT = [0.25, 0.10, 0.75, 0.12]

MAP_SET, 0, 180, 0, LIMIT = limit, /ISOTROPIC, POSITION = map_pos, /NOBORDER, $					;Set map
	TITLE = title0 + ' valid ' + MAKE_ISO_DATE_STRING(data.date), $
	/NOERASE


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

MAP_GRID_CYLINDRICAL_CRH, limit[1], limit[3], 4, limit[0], limit[2], 4, /LABEL, $				;Draw grid
	GLINESTYLE = -KEYWORD_SET(counties)
;; End

IF KEYWORD_SET(eps) THEN BEGIN
    IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
	      LOAD_BASIC_COLORS, /RESET                                           												;Reset color table to linear ramp
   	   PS_OFF                                                                											;Turn PS off
ENDIF ELSE IF KEYWORD_SET(png) THEN $
         WRITE_PNG, pngfile, TVRD(TRUE = 1)  

END
