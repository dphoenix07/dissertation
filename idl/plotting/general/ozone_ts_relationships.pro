PRO OZONE_TS_RELATIONSHIPS, hour, $
	LANDFALL  = landfall, $
	EPA		  = epa, $
	PNG		  = png, $
	EPS		  = eps, $
	CLOBBER   = clobber

;+
;NAME:
;     OZONE_TS_RELATIONSHIPS
;PURPOSE:
;     Plots o3 concentration vs pressure and wind speed for different categorizations.
;CATEGORY:
;     Data handling utility.
;CALLING SEQUENCE:
;     OZONE_TS_RELATIONSHIPS, date0, outfile
;INPUT:
;		start_date : user specified start date to plot in yyyymmdd format (e.g., '20170101')
;		end_date   : user specified end date to plot in yyyymmdd format (e.g., '20180101')
;KEYWORDS:
;     PLOT      : If set, plot sample maps.
;     DIRECTORY : Output directory for wind file.
;	  CLOBBER   : If set, overwrite existing file. This is the default.
;OUTPUT:
;     Netcdf file.
;MODIFICATION HISTORY:
;		D. Phoenix:       2019-02-10.
;-

COMPILE_OPT IDL2																									;Set compile options

yr_arr = ['1980','1981','1982','1983','1984','1985','1986','1987','1988','1989','1990', $
			'1991','1992','1993','1994','1995','1996','1997','1998','1999','2000', $
			'2001','2002','2003','2004','2005','2006','2007','2008','2009','2010', $
			'2011','2012','2013','2014','2015','2016','2017']
;yr_arr = ['2011','2012','2013']
mth_arr = ['07','08','09','10','11']

;Get monthly averaged data
infile = !WRF_DIRECTORY + 'general/o3_data/monthly_ave_data/monthly_ave_071980_122017.nc'																;Set input file path
id  = NCDF_OPEN(infile)																						;Open input file for reading	
NCDF_VARGET, id, 'O3'  , o3_dm
NCDF_VARGET, id, 'O3_DM', o3_dmmax
NCDF_CLOSE,  id																								;Close input file

dallas_x = (-96.86)
dallas_y = 32.85
ts_data = read_hurdat2()

inan = WHERE(ts_data.p EQ -999)
ts_data.p[inan] = !Values.F_NaN

nots_arr_tot1 = [ ]
ts_arr_tot1   = [ ]

nots_arr_tot2 = FLTARR(30,(8760/6))
ts_arr_tot2   = FLTARR(30,(8760/6))

ilandfall = 0
month_arr=[]
istorm_arr = []
instorm_arr = []
o3_storms  = [ ]
o3_nstorms = [ ]
pres_storms= [ ]
wind_storms = [ ]
distance = [ ]
o3dm_storms = []

o3dm_storms_yr = []
o3_storms_yr   = []
wind_storms_yr = []
pres_storms_yr = []
distance_yr = []

FOREACH year, yr_arr DO BEGIN
	PRINT, year	
	imonth = WHERE(((ts_data.date.year GE year) AND (ts_data.date.year LT year+1)) AND $
		((ts_data.date.month GE 7) AND (ts_data.date.month LT 12)))

;    extr_storms = WHERE(((ts_data.class[imonth] EQ 'TD') OR (ts_data.class[imonth] EQ 'TS') OR $
;    	(ts_data.class[imonth] EQ 'HU')), istorms)
	 extr_storms = WHERE(((ts_data.class[imonth] EQ 'TD') OR (ts_data.class[imonth] EQ 'TS') OR $
    	(ts_data.class[imonth] EQ 'HU')) AND ((ts_data.x[imonth] LE -75.0) AND $
    	(ts_data.x[imonth] GT -100.0) AND (ts_data.y[imonth] LE 40.0) AND $
    	(ts_data.y[imonth] GE 20.0)), istorms)
   IF (istorms GT 0) THEN iperiod = imonth[extr_storms] ELSE iperiod = 0

   istorm_arr = [istorm_arr, istorms]
   IF KEYWORD_SET(landfall) THEN BEGIN
  	 landfall = WHERE(((ts_data.id[iyear] EQ 'L') OR (ts_data.id[iyear] EQ 'C')) AND $
  	 	((ts_data.class[iyear] EQ 'TD') OR (ts_data.class[iyear] EQ 'TS') OR $
  	  	(ts_data.class[iyear] EQ 'HU')), ilandfall)
  	 IF (ilandfall GT 0) THEN iperiod = iyear[landfall] ELSE iperiod = 0
   ENDIF

   o3dir = !WRF_DIRECTORY + 'general/o3_data/'
   o3file = o3dir + 'dallas_' + year + '.csv'
   pngfile = o3dir + 'o3pdf_extrstorms_8hrmean_v2.png'
   o3_data = READ_CSV(o3file)
   
   ;Compute a running 8-hour mean of O3 concentrations for Dallas (Fort Worth uses same data)
   o3_8hr  = OZONE_DALLAS_NAAQS(year)	
   o3_max  = o3_8hr.ozone_daily_max
   o3dm_datestr = o3_8hr.ozone_datestr
   o3_8hr  = o3_8hr.ozone_8hr_total
   
   IF (KEYWORD_SET(NO_AVE)) THEN BEGIN
   		o3_8hr = o3_data.field14*1.0E3
   		pngfile = o3dir + 'o3pdf_extrstorms_hourly.png'
   ENDIF
   
   IF KEYWORD_SET(landfall) THEN pngfile = o3dir + 'timeseries_landfall_closest_storms.png'
   
   o3_datestr =  STRMID(o3_data.field12,0,4)+STRMID(o3_data.field12,5,2)+STRMID(o3_data.field12,8,2) + $;'T' + $
   				  STRMID(o3_data.field13,0,2)+STRMID(o3_data.field13,3,2) ;+ 'Z'

   ts_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),0,4) +  $
  		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),5,2) + $
  		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),8,2) + $; 'T' + $
  		  STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),11,2) + $
  		   STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),14,2) ;+ 'Z'

   ;Create 1-hour bin with 6 hour interval
   time_arr = [ ]
   date1 = MAKE_DATE(year,07,01,hour,00)
   date2 = MAKE_DATE(year,12,01,hour,00)
   end_datestr = STRMID(MAKE_ISO_DATE_STRING(date2),0,4) + STRMID(MAKE_ISO_DATE_STRING(date2),5,2) + $
   			 	STRMID(MAKE_ISO_DATE_STRING(date2),8,2) + STRMID(MAKE_ISO_DATE_STRING(date2),11,2) + $
   		  	 	STRMID(MAKE_ISO_DATE_STRING(date2),14,2) 
   nt_yr = 8760/24
   FOR i = 0, nt_yr - 1 DO BEGIN
   		datestr = STRMID(MAKE_ISO_DATE_STRING(date1),0,4) + STRMID(MAKE_ISO_DATE_STRING(date1),5,2) + $
   			 STRMID(MAKE_ISO_DATE_STRING(date1),8,2) + STRMID(MAKE_ISO_DATE_STRING(date1),11,2) + $
   		  	 STRMID(MAKE_ISO_DATE_STRING(date1),14,2) 
   		time_arr = [time_arr, datestr]
   		IF (datestr EQ end_datestr) THEN BREAK
   		date1 = TIME_INC(date1, 86400)
   ENDFOR 
   
   ;Create O3 bins
   yrange = [10, 110] ;ozone
   xrange = [10, 110] ;wind
   nxbins = 20
   nybins = 20
   dy   	 = FLOAT(yrange[1] - yrange[0])/nybins														;Compute y bin spacing
   dx 		 = FLOAT(xrange[1] - xrange[0])/nxbins 
   xbin 	 = dx + xrange[0] + dx*FINDGEN(nxbins)
   ybin 	 = dy + yrange[0] + dy*FINDGEN(nybins)
   
   nots_arr   = [ ]
   ts_arr     = [ ]
   FOR i = 0 , N_ELEMENTS(time_arr) -1 DO BEGIN
   	   date1 = time_arr[i]
   	   PRINT, date1
   	   
   	   mth = WHERE(STRMID(time_arr[i],4,2) EQ mth_arr)
	   io3 = WHERE(o3_datestr EQ time_arr[i], o3count)
   	   io3dm = WHERE(o3dm_datestr EQ time_arr[i], o3dmcount)

   	   IF (io3[0] EQ -1) THEN io3 = [0]
   	   its   = WHERE(ts_datestr EQ date1, tscount)
   	   
;   	   IF (tscount EQ 0) THEN BEGIN
;   	   		data_bin = LONG((o3_8hr[io3]-o3_range[0])/do3) 
;    	 	o3_nstorms = [o3_nstorms, o3_8hr[io3]]
;  	   ENDIF ELSE data_bin = [0]
   	   
   	   IF (tscount GT 0) THEN BEGIN
   	      	o3_storms   = [o3_storms, MAX((o3_8hr[io3]-o3_dm[mth]),/NAN)]
   	      	o3dm_storms = [o3dm_storms, MAX((o3_max[io3dm]-o3_dmmax[mth]),/NAN)]
   	      	wind_storms = [wind_storms,MAX(ts_data.wspd[iperiod[its]],/NAN)]   	      	
			pres_storms = [pres_storms, MIN(ts_data.p[iperiod[its]],/NAN)]
			distance    = [distance,MEAN(SQRT((dallas_x - ts_data.x[iperiod[its]])^2 + $
							(dallas_y - ts_data.y[iperiod[its]])^2))	]		
	   ENDIF  	  
   ENDFOR  	
	
	o3dm_storms_yr = [o3dm_storms_yr, o3dm_storms] 
	o3_storms_yr   = [o3_storms_yr,     o3_storms]
	wind_storms_yr = [wind_storms_yr, wind_storms]
	pres_storms_yr = [pres_storms_yr, pres_storms]
	distance_yr    = [distance_yr,    	 distance]
ENDFOREACH

o3_wind_correlate  = CORRELATE(o3dm_storms_yr,wind_storms_yr)
o3_wind_covariance = CORRELATE(o3dm_storms_yr,wind_storms_yr,/COVARIANCE)

PRINT, 'the o3 and wind correlation coefficient is: ' + STRTRIM(o3_wind_correlate,1)
PRINT, 'the o3 and wind correlation covariance is: ' + STRTRIM(o3_wind_covariance,1)
PRINT, ' '

o3_pres_correlate  = CORRELATE(o3dm_storms_yr,pres_storms_yr)
o3_pres_covariance = CORRELATE(o3dm_storms_yr,pres_storms_yr,/COVARIANCE)

PRINT, 'the o3 and pressure correlation coefficient is: ' + STRTRIM(o3_pres_correlate,1)
PRINT, 'the o3 and pressure correlation covariance is: ' + STRTRIM(o3_pres_covariance,1)
PRINT, ' '

o3_dist_correlate  = CORRELATE(o3dm_storms_yr,distance_yr)
o3_dist_covariance = CORRELATE(o3dm_storms_yr,distance_yr,/COVARIANCE)

PRINT, 'the o3 and distance correlation coefficient is: ' + STRTRIM(o3_dist_correlate,1)
PRINT, 'the o3 and distance correlation covariance is: ' + STRTRIM(o3_dist_covariance,1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
IF ~KEYWORD_SET(nowindow) THEN BEGIN
	IF KEYWORD_SET(eps) THEN BEGIN	
		PS_ON, FILENAME = epsfile, PAGE_SIZE = [6.0, 8.0], MARGIN = 0.0, /INCHES				;Switch to Postscript device
		DEVICE, /ENCAPSULATED
		!P.FONT     = 0																			;Hardware fonts
		!P.CHARSIZE = 1.0	
		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
			LOAD_BASIC_COLORS																	;Load basic color definitions
	ENDIF ELSE BEGIN
		SET_PLOT, 'X'
		WINDOW, XSIZE = 900, YSIZE = 600														;Open graphics window
		!P.COLOR      = COLOR_24('black')														;Foreground color
		!P.BACKGROUND = COLOR_24('white')														;Background color
		!P.CHARSIZE   = 2.0		
		!P.FONT       = -1																		;Use Hershey fonts
	ENDELSE
ENDIF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

bar_pos = [0.15, 0.11, 0.85, 0.13]																			;Set color bar position
map_pos = [0.1, 0.25, 0.95, 0.90]

PLOT, wind_storms_yr, distance_yr, /NODATA, $
			ytitle = 'Storm Distance from DFW (km)', xtitle = "Storm Wind Speed (knots)", PSYM = 4, $
			xrange = [0, 170], ylog = 0, yrange = [0,3000], POSITION = map_pos; , title = start_date

USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation    
FOR s = 0, N_ELEMENTS(o3dm_storms_yr)-1 DO BEGIN
    ;table = HCL_COLOR_TABLE(120, HUE_RANGE = [100.0, 300.0])
    table = BLUE_RED_24(120)
    ullevels = FINDGEN(120) - 60.0
    iul = WHERE(ullevels EQ ROUND(o3dm_storms_yr[s]))
    
    PLOTS, wind_storms_yr[s], distance_yr[s]*100,  $
    	    PSYM    = 8, $
    	    SYMSIZE = 4, $
    		NOCLIP  = 0, $
			COLOR   = table[iul]
ENDFOR

COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
	RANGE = [-60, 60], $
	TICKS = 5, $
	TITLE = 'Normalized Ozone Concentration (ppb)', $
	POSIT = bar_pos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
STOP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
bar_pos = [0.15, 0.11, 0.85, 0.13]																			;Set color bar position
map_pos = [0.1, 0.25, 0.95, 0.90]

PLOT, pres_storms_yr, distance_yr, /NODATA, $
			ytitle = 'Storm Distance from DFW (km)', xtitle = "Storm Central Pressure (hPa)", PSYM = 4, $
			xrange = [950, 1020], ylog = 0, yrange = [0,3000], POSITION = map_pos; , title = start_date

USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation    
FOR s = 0, N_ELEMENTS(o3dm_storms_yr)-1 DO BEGIN
    ;table = HCL_COLOR_TABLE(120, HUE_RANGE = [100.0, 300.0])
    table = BLUE_RED_24(120)
    ullevels = FINDGEN(120) - 60.0
    iul = WHERE(ullevels EQ ROUND(o3dm_storms_yr[s]))
    
    PLOTS, pres_storms_yr[s], distance_yr[s]*100,  $
    	    PSYM    = 8, $
    	    SYMSIZE = 4, $
    		NOCLIP  = 0, $
			COLOR   = table[iul]
ENDFOR

COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
	RANGE = [-60, 60], $
	TICKS = 5, $
	TITLE = 'Normalized Ozone Concentration (ppb)', $
	POSIT = bar_pos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

date1   = MAKE_ISO_DATE_STRING(date,/COMPACT,/UTC)

epsfile = outdir +  date1  + '.eps'						;EPS filename
pdffile = outdir +  date1  + '.pdf'						;PDF filename
pngfile = outdir +  date1  + '.png'						;PNG filename


IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
	
	IF KEYWORD_SET(pdf) THEN PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS						;Convert to PDF
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file


STOP


p1 = SCATTERPLOT(o3dm_storms_yr, wind_storms_yr, $
	TITLE  = 'O3 vs Wind Speed: ' + STRTRIM(hour,1) + 'Z', $
	YTITLE = 'Storm Wind Speed (knots)', $ 
	XTITLE = 'DFW O3 Conc. (ppb)', $
	;YRANGE = [0,200], $
	YRANGE = [0, 50], $
	XRANGE = [-100,100])

l1 = LINFIT(o3dm_storms_yr, wind_storms_yr)
xplot1 = FINDGEN(100)-50
yplot1 = l1[0] + (l1[1])*xplot1

trend = PLOT(xplot1, yplot1, $
		/OVERPLOT, $
		COLOR = COLOR_24('red'), $
		THICK=2, $
		LINESTYLE=3, $
		XRANGE = [-50,50])


p3 = SCATTERPLOT( o3dm_storms_yr, distance_yr, $
	TITLE  = 'O3 vs Distance: ' + STRTRIM(hour,1) + 'Z', $
	YTITLE = 'Storm Distance from DFW (km)', $ 
	XTITLE = 'DFW O3 Conc. (ppb)', $
	YRANGE = [0,100], $
	XRANGE = [-100,100])

l3 = LINFIT(o3dm_storms_yr, distance_yr)
xplot3 = FINDGEN(140)-0
yplot3 = l3[0] + (l3[1])*xplot3

trend = PLOT(xplot3, yplot3, $
		/OVERPLOT, $
		COLOR = COLOR_24('red'), $
		THICK=2, $
		LINESTYLE=3, $
		XRANGE = [-50,50])



pres_storms_yr = FLOAT(pres_storms_yr)
inan = WHERE(pres_storms_yr LT 0)
REMOVE, inan, pres_storms_yr
REMOVE, inan, o3dm_storms_yr

p2 = SCATTERPLOT( o3dm_storms_yr, pres_storms_yr, $
	TITLE  = 'O3 vs Pressure: ' + STRTRIM(hour,1) + 'Z', $
	YTITLE = 'Storm Pressure (hPa)', $ 
	XTITLE = 'DFW O3 Conc. (ppb)', $
	;YRANGE = [950,1020], $
	YRANGE  = [990, 1020], $
	XRANGE = [-100,100])

l2 = LINFIT(o3dm_storms_yr, pres_storms_yr)
xplot2 = FINDGEN(60)-30
yplot2 = l2[0] + (l2[1])*xplot2

trend = PLOT(xplot2, yplot2, $
		/OVERPLOT, $
		COLOR = COLOR_24('red'), $
		THICK=2, $
		LINESTYLE=3, $
		XRANGE = [-50,50])


STOP


nstorm_o3 = HISTOGRAM(nots_arr_tot1, BINSIZE = 1, MIN = 0, MAX = (num_bins -1))	
storm_o3  = HISTOGRAM(ts_arr_tot1, BINSIZE = 1, MIN = 0, MAX = (num_bins -1))	

;nstorm_o3 = TOTAL(nots_arr_tot1,2)
;storm_o3  = TOTAL(ts_arr_tot1,2)

IF KEYWORD_SET(eps) THEN BEGIN	
	PS_ON, FILENAME = epsfile, PAGE_SIZE = [6.45, 8.25], MARGIN = 0.0, /INCHES											;Switch to Postscript device
	DEVICE, /ENCAPSULATED
	!P.FONT     = 0																														;Hardware fonts
	!P.CHARSIZE = 1.25	
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS																													;Load basic color definitions
ENDIF ELSE BEGIN
	SET_PLOT, 'X'
	WINDOW, XSIZE = 600, YSIZE = 800																								;Open graphics window
	!P.COLOR      = COLOR_24('black')																								;Foreground color
	!P.BACKGROUND = COLOR_24('white')																								;Background color
	!P.CHARSIZE   = 1.8		
	!P.FONT       = -1																													;Use Hershey fonts
ENDELSE

!P.MULTI = [0,1,2]
title = '8-hr Mean O3 Concentrations: July-Nov' 
IF (KEYWORD_SET(NO_AVE)) THEN title = 'Hourly O3 Concentrations: July-Nov'
PLOT, o3bin, o3bin, /NODATA, $
	XRANGE = [10,100], $
	XSTYLE = 1, $
	XTITLE = 'O3 Concentration (ppb)', $
	YRANGE = [0, 20], $
	YSTYLE = 1, $
	YTITLE = 'Frequency (%)', $
	TITLE  = title

OPLOT, o3bin, 100.0*(FLOAT(nstorm_o3)/TOTAL(nstorm_o3)), THICK = 4, PSYM = 10
OPLOT, o3bin, 100.0*(FLOAT(storm_o3)/TOTAL(storm_o3  )), THICK = 2, PSYM = 10, COLOR = COLOR_24('red')

;For entire o3 range
XYOUTS, 45.0, 17.5, 'No Storms = ' + STRTRIM(LONG(TOTAL(nstorm_o3)),1), /DATA
XYOUTS, 45.0, 16.0, 'Tropical Storms = '+ STRTRIM(LONG(TOTAL(storm_o3)),1) , COLOR = COLOR_24('red'), /DATA


PLOT, o3bin, o3bin, /NODATA, $
	XRANGE = [45,100], $
	XSTYLE = 1, $
	XTITLE = 'O3 Concentration (ppb)', $
	YRANGE = [0, 5], $
	YSTYLE = 1, $
	YTITLE = 'Frequency (%)', $
	TITLE  = title

OPLOT, o3bin, 100.0*(FLOAT(nstorm_o3)/TOTAL(nstorm_o3)), THICK = 4, PSYM = 10
OPLOT, o3bin, 100.0*(FLOAT(storm_o3)/TOTAL(storm_o3  )), THICK = 2, PSYM = 10, COLOR = COLOR_24('red')

;For zooming in on the O3 extremes (45-100 ppb)
XYOUTS, 80.0, 4.5, 'No Storms', /DATA
XYOUTS, 80.0, 4.0, 'Tropical Storms', COLOR = COLOR_24('red'), /DATA

IF ~KEYWORD_SET(nowindow) THEN BEGIN
	IF KEYWORD_SET(eps) THEN BEGIN
		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
			LOAD_BASIC_COLORS, /RESET															;Reset color table to linear ramp
		PS_OFF																					;Turn PS off
	ENDIF ELSE IF KEYWORD_SET(png) THEN $
		WRITE_PNG, pngfile, TVRD(TRUE=1)														;Write PNG image
ENDIF

PRINT, 'tropical storm mean o3 concentration: ' + STRTRIM(MEAN(o3_storms,/NAN),1)
PRINT, 'no storm mean o3 concentration: ' + STRTRIM(MEAN(o3_nstorms,/NAN),1)

PRINT, 'tropical storm max o3 concentration: ' + STRTRIM(MAX(o3_storms,/NAN),1)
PRINT, 'no storm max o3 concentration: ' + STRTRIM(MAX(o3_nstorms,/NAN),1)

isort = SORT(o3_storms)
imid  = N_ELEMENTS(o3_storms)/2
PRINT, 'tropical storm median o3 concentration: ' + STRTRIM(o3_storms[isort[imid]],1)

isort = SORT(o3_nstorms)
imid  = N_ELEMENTS(o3_nstorms)/2
PRINT, 'no storm median o3 concentration: ' + STRTRIM(o3_nstorms[isort[imid]],1)


STOP           
END
