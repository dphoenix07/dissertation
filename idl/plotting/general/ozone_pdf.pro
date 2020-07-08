PRO OZONE_PDF, $
	LANDFALL  = landfall, $
	NO_AVE	  = no_ave, $
	PNG		  = png, $
	EPS		  = eps, $
	CLOBBER   = clobber

;+
;NAME:
;     OZONE_PDF
;PURPOSE:
;     Compare the number of times the daily 8-hr mean o3 concentration exceeds
;		different thresholds (55, 65, 75, 80, 90, 100 ppb) 
;CATEGORY:
;     Data handling utility.
;CALLING SEQUENCE:
;     OZONE_PDF, date0, outfile
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
;		D. Phoenix:       2019-02-10.	PDF of O3 for TS vs Non-TS: July1-Dec1
;						  2019-02-14.	NEED TO UPDATE THIS TO BE CONSISTENT WITH
;										O3_PDF_DAILYMAX BEFORE USING AGAIN
;-

COMPILE_OPT IDL2																									;Set compile options

yr_arr = ['1980','1981','1982','1983','1984','1985','1986','1987','1988','1989','1990', $
			'1991','1992','1993','1994','1995','1996','1997','1998','1999','2000', $
			'2001','2002','2003','2004','2005','2006','2007','2008','2009','2010', $
			'2011','2012','2013','2014','2015','2016','2017']
;yr_arr = ['2011','2012','2013']

ts_data = read_hurdat2()

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

FOREACH year, yr_arr DO BEGIN
	PRINT, year	
	imonth = WHERE(((ts_data.date.year GE year) AND (ts_data.date.year LT year+1)) AND $
		((ts_data.date.month GE 10) AND (ts_data.date.month LT 12)))

    extr_storms = WHERE(((ts_data.class[imonth] EQ 'TD') OR (ts_data.class[imonth] EQ 'TS') OR $
    	(ts_data.class[imonth] EQ 'HU')), istorms)
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
   o3_data  = OZONE_DALLAS_NAAQS(year)	
   o3_8hr  = o3_data.ozone_8hr_total
   
   IF (KEYWORD_SET(NO_AVE)) THEN BEGIN
   		o3_data = OZONE_DALLAS_NAAQS(year)	
   		o3_8hr  = o3_data.ozone_hourly
   		pngfile = o3dir + 'o3pdf_extrstorms_hourly.png'
   ENDIF
   
   IF KEYWORD_SET(landfall) THEN pngfile = o3dir + 'timeseries_landfall_closest_storms.png'
   
   o3_datestr =  STRMID(o3_data.date,0,4)+STRMID(o3_data.date,5,2)+STRMID(o3_data.date,8,2) + $;'T' + $
   				  STRMID(o3_data.time,0,2)+STRMID(o3_data.time,3,2) ;+ 'Z'

   ts_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),0,4) +  $
  		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),5,2) + $
  		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),8,2) + $; 'T' + $
  		  STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),11,2) + $
  		   STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),14,2) ;+ 'Z'

   ;Create 1-hour bin with 6 hour interval
   time_arr = [ ]
   date1 = MAKE_DATE(year,10,01,06,00)
   date2 = MAKE_DATE(year,12,01,06,00)
   end_datestr = STRMID(MAKE_ISO_DATE_STRING(date2),0,4) + STRMID(MAKE_ISO_DATE_STRING(date2),5,2) + $
   			 	STRMID(MAKE_ISO_DATE_STRING(date2),8,2) + STRMID(MAKE_ISO_DATE_STRING(date2),11,2) + $
   		  	 	STRMID(MAKE_ISO_DATE_STRING(date2),14,2) 
   nt_yr = 8760/6
   FOR i = 0, nt_yr - 1 DO BEGIN
   		datestr = STRMID(MAKE_ISO_DATE_STRING(date1),0,4) + STRMID(MAKE_ISO_DATE_STRING(date1),5,2) + $
   			 STRMID(MAKE_ISO_DATE_STRING(date1),8,2) + STRMID(MAKE_ISO_DATE_STRING(date1),11,2) + $
   		  	 STRMID(MAKE_ISO_DATE_STRING(date1),14,2) 
   		time_arr = [time_arr, datestr]
   		IF (datestr EQ end_datestr) THEN BREAK
   		date1 = TIME_INC(date1, 21600)
   ENDFOR 

   ;Create O3 bins
   o3_range = [0, 150]
   xrange   = [0, nt_yr]
   num_bins = 30
   do3   	 = FLOAT(o3_range[1] - o3_range[0])/num_bins														;Compute y bin spacing
   dx 		 = 1 
   o3bin 	 = do3 + o3_range[0] + do3*FINDGEN(num_bins)
   
   nots_arr   = [ ]
   ts_arr     = [ ]
   FOR i = 0 , N_ELEMENTS(time_arr) -1 DO BEGIN
   	   date1 = time_arr[i]
   	   PRINT, date1
   	   
	   io3 = WHERE(o3_datestr EQ time_arr[i], o3count)
   	   IF (io3[0] EQ -1) THEN io3 = [0]
   	   its   = WHERE(ts_datestr EQ date1, tscount)
   	   IF (tscount EQ 0) THEN BEGIN
   	   		data_bin = LONG((o3_8hr[io3]-o3_range[0])/do3) 
    	 	o3_nstorms = [o3_nstorms, o3_8hr[io3]]
  	   ENDIF ELSE data_bin = [0]
   	   IF (tscount GT 0) THEN BEGIN
   	   		ts_bin   = LONG((o3_8hr[io3]-o3_range[0])/do3) 
   	      	o3_storms = [o3_storms, o3_8hr[io3]]
   	   ENDIF ELSE ts_bin = [0]
   	  
   	   ;keep incase things get screwed up
   	   ;IF (tscount EQ 0) THEN data_bin = LONG((o3_8hr[io3]-o3_range[0])/do3) $
   	   ;	ELSE data_bin = [0]
   	   ;IF (tscount GT 0) THEN ts_bin   = LONG((o3_8hr[io3]-o3_range[0])/do3) $
   	   ;	ELSE ts_bin = [0]
   	   
   	   ;hist_all = HISTOGRAM(data_bin, BINSIZE = 1, MIN = 0, MAX = (num_bins -1))								;Calculate density
   	   ;nots_arr = [[nots_arr],[hist_all]] 
       nots_arr = [nots_arr,data_bin]
   	   ;hist_ts = HISTOGRAM(ts_bin, BINSIZE = 1, MIN = 0, MAX = (num_bins -1))								;Calculate density
   	   ;ts_arr = [[ts_arr],[hist_ts]] 
   	   ts_arr = [ts_arr,ts_bin] 
   ENDFOR  	
	;nots_arr_tot1 = [[nots_arr_tot1],[nots_arr]]
	;ts_arr_tot1   = [[ts_arr_tot1  ],[ts_arr  ]]
	nots_arr_tot1 = [nots_arr_tot1,nots_arr]
	ts_arr_tot1   = [ts_arr_tot1  ,ts_arr  ]
ENDFOREACH

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
title = '8-hr Mean O3 Concentrations: Oct-Nov' 
IF (KEYWORD_SET(NO_AVE)) THEN title = 'Hourly O3 Concentrations: July'
PLOT, o3bin, o3bin, /NODATA, $
	XRANGE = [10,100], $
	XSTYLE = 1, $
	XTITLE = 'O3 Concentration (ppb)', $
	YRANGE = [0, 20], $
	YSTYLE = 1, $
	YTITLE = 'Frequency (%)', $
	TITLE  = title

OPLOT, o3bin, 100.0*(FLOAT(nstorm_o3[1:-1])/TOTAL(nstorm_o3[1:-1])), THICK = 4, PSYM = 10
OPLOT, o3bin, 100.0*(FLOAT(storm_o3 [1:-1])/TOTAL(storm_o3 [1:-1])), THICK = 2, PSYM = 10, COLOR = COLOR_24('red')

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

OPLOT, o3bin, 100.0*(FLOAT(nstorm_o3[1:-1])/TOTAL(nstorm_o3[1:-1])), THICK = 4, PSYM = 10
OPLOT, o3bin, 100.0*(FLOAT(storm_o3 [1:-1])/TOTAL(storm_o3 [1:-1])), THICK = 2, PSYM = 10, COLOR = COLOR_24('red')

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
