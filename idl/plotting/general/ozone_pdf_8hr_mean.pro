PRO OZONE_PDF_8HR_MEAN, $
	LANDFALL  = landfall, $
	EPA		  = epa, $
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
;-

COMPILE_OPT IDL2																									;Set compile options

yr_arr = ['1980','1981','1982','1983','1984','1985','1986','1987','1988','1989','1990', $
			'1991','1992','1993','1994','1995','1996','1997','1998','1999','2000', $
			'2001','2002','2003','2004','2005','2006','2007','2008','2009','2010', $
			'2011','2012','2013','2014','2015','2016','2017']
yr_arr = ['2011','2012','2013']

ts_data = read_hurdat2()

nots_arr_tot1 = [ ]
ts_arr_tot1   = [ ]

nots_arr_tot2 = FLTARR(30,(8760/6))
ts_arr_tot2   = FLTARR(30,(8760/6))

ilandfall = 0
month_arr=[]
FOREACH year, yr_arr DO BEGIN
	PRINT, year	
	imonth = WHERE(((ts_data.date.year GE year) AND (ts_data.date.year LT year+1)) AND $
		((ts_data.date.month GE 7) AND (ts_data.date.month LT 12)))

    extr_storms = WHERE(((ts_data.class[imonth] EQ 'TD') OR (ts_data.class[imonth] EQ 'TS') OR $
    	(ts_data.class[imonth] EQ 'HU')), istorms)
    IF (istorms GT 0) THEN iperiod = imonth[extr_storms] ELSE iperiod = 0

   IF KEYWORD_SET(landfall) THEN BEGIN
  	 landfall = WHERE(((ts_data.id[iyear] EQ 'L') OR (ts_data.id[iyear] EQ 'C')) AND $
  	 	((ts_data.class[iyear] EQ 'TD') OR (ts_data.class[iyear] EQ 'TS') OR $
  	  	(ts_data.class[iyear] EQ 'HU')), ilandfall)
  	 IF (ilandfall GT 0) THEN iperiod = iyear[landfall] ELSE iperiod = 0
   ENDIF

   o3dir = !WRF_DIRECTORY + 'general/o3_data/'
   o3file = o3dir + 'dallas_' + year + '.csv'
   o3_data = READ_CSV(o3file)
   
   ;Compute a running 8-hour mean of O3 concentrations for Dallas (Fort Worth uses same data)
   o3_8hr  = OZONE_DALLAS_NAAQS(year)	
   
   pngfile = o3dir + 'o3pdf_extrstorms_dailymax.png'
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
   date1 = MAKE_DATE(year,07,01,06,00)
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
   
   nots_arr = [ ]
   ts_arr   = [ ]
   FOR i = 0 , N_ELEMENTS(time_arr) -1, 4 DO BEGIN
   	   date1 = time_arr[i]
   	   PRINT, date1
   	   
   	   indices = [ ]
   	   PRINT, '4 hours to average'
   	   FOR hh=0,3 DO BEGIN
   	   		PRINT, time_arr[i+hh]
   	   		io3     = WHERE(o3_datestr EQ time_arr[i+hh], o3count)
   	   		indices = [indices, io3]
   	   ENDFOR
   	   IF (io3[0] EQ -1) THEN io3 = [0]
   	   its   = WHERE(ts_datestr EQ date1, tscount)
   	   
   	   daily_max = o3_8hr[indices]
   	   IF (KEYWORD_SET(EPA)) THEN daily_max = MAX(o3_8hr[indices],/NAN)
   	   STOP
   	   IF (tscount EQ 0) THEN data_bin = LONG((daily_max-o3_range[0])/do3) $
   	   	ELSE data_bin = [0]
   	   IF (tscount GT 0) THEN ts_bin   = LONG((daily_max-o3_range[0])/do3) $
   	   	ELSE ts_bin = [0]
   	  
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
title = '8-hr Mean O3 Concentrations: July-Nov' 
IF (KEYWORD_SET(EPA)) THEN title = 'Daily Max 8-hr Mean O3: July-Nov'
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
XYOUTS, 55.0, 17.5, 'Non-Tropical Storms', /DATA
XYOUTS, 55.0, 16.0, 'Tropical Storms', COLOR = COLOR_24('red'), /DATA


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
XYOUTS, 75.0, 4.5, 'Non-Tropical Storms', /DATA
XYOUTS, 75.0, 4.0, 'Tropical Storms', COLOR = COLOR_24('red'), /DATA

IF ~KEYWORD_SET(nowindow) THEN BEGIN
	IF KEYWORD_SET(eps) THEN BEGIN
		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
			LOAD_BASIC_COLORS, /RESET															;Reset color table to linear ramp
		PS_OFF																					;Turn PS off
	ENDIF ELSE IF KEYWORD_SET(png) THEN $
		WRITE_PNG, pngfile, TVRD(TRUE=1)														;Write PNG image
ENDIF
STOP           
END
