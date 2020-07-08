PRO OZONE_PDF_DAILYMAX, storm_type, $
	LANDFALL   = landfall, $
	ATLANTIC   = atlantic, $
	GULF	   = gulf, $
	;STORM_TYPE = storm_type, $ 	
	EPA		   = epa, $
	PNG		   = png, $
	EPS		   = eps, $
	CLOBBER    = clobber

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
;							2019-02-13. ***The PDFs of Gulf/Atl/TD/TS/HU only include
;										the excluded storms in the "No Storms" population.
;										Make changes to do "Storm Type vs No Storms"
;										Also make script (maybe box plot script) to compare
;										different storms and gulf vs atlantic storms.
;-

COMPILE_OPT IDL2																									;Set compile options

IF (N_ELEMENTS(storm_type) EQ 0) THEN storm_type = 0

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
o3_storms  = [ ]
o3_nstorms = [ ]

july_nstorm_count=0
aug_nstorm_count =0
sept_nstorm_count=0
oct_nstorm_count =0
nov_nstorm_count =0

july_storm_count=0
aug_storm_count =0
sept_storm_count=0
oct_storm_count =0
nov_storm_count =0

o3_ns_arr = []
o3_s_arr=[]

FOREACH year, yr_arr DO BEGIN
	PRINT, year	
	imonth = WHERE(((ts_data.date.year GE year) AND (ts_data.date.year LT year+1)) AND $
		((ts_data.date.month GE 11) AND (ts_data.date.month LT 12)))

    ;extr_storms = WHERE(((ts_data.class[imonth] EQ 'TD') OR (ts_data.class[imonth] EQ 'TS') OR $
    ;	(ts_data.class[imonth] EQ 'HU')), istorms, COMPLEMENT = nostorms)
    ;extr_storms = WHERE(((ts_data.class[imonth] EQ 'TS')), istorms)
	extr_storms = WHERE(((ts_data.class[imonth] EQ 'TD') OR (ts_data.class[imonth] EQ 'TS') OR $
  			(ts_data.class[imonth] EQ 'HU')) AND ((ts_data.x[imonth] LE -75.0) AND $
			(ts_data.x[imonth] GT -100.0) AND (ts_data.y[imonth] LE 40.0) AND $
			(ts_data.y[imonth] GE 20.0)), istorms)
    IF (istorms GT 0) THEN iperiod = imonth[extr_storms] ELSE iperiod = 0
	
   ts_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),0,4) +  $
  		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),5,2) + $
  		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),8,2)

	storm_label1 = 'Storms'
	IF (TYPENAME(storm_type) EQ 'STRING') THEN BEGIN
    	extr_storms = WHERE(((ts_data.class[imonth] EQ storm_type)), istorms)
		
		IF (storm_type EQ 'TD') THEN storm_label1 = 'TD Storms'
		IF (storm_type EQ 'TS') THEN storm_label1 = 'TS Storms'
		IF (storm_type EQ 'HU') THEN storm_label1 = 'HU Storms'
		iperiod = imonth[extr_storms]
	ENDIF
	
   IF KEYWORD_SET(landfall) THEN BEGIN
  	 landfall = WHERE(((ts_data.id[imonth] EQ 'L') OR (ts_data.id[imonth] EQ 'C')) AND $
  	 	((ts_data.class[imonth] EQ 'TD') OR (ts_data.class[imonth] EQ 'TS') OR $
  	  	(ts_data.class[imonth] EQ 'HU')), ilandfall)
  	 IF (ilandfall GT 0) THEN iperiod = imonth[landfall] ELSE iperiod = 0
   ENDIF

	igulf = WHERE(ts_data.x[iperiod] LE -80.0, gulfcount, COMPLEMENT = iatlantic)
	IF (KEYWORD_SET(GULF)) 		THEN iperiod = iperiod[igulf]
	IF (KEYWORD_SET(ATLANTIC))	THEN iperiod = iperiod[iatlantic]
	
   ts_datestr1 = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),0,4) +  $
  		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),5,2) + $
  		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),8,2)
			
   o3dir = !WRF_DIRECTORY + 'general/o3_data/'
   o3file = o3dir + 'dallas_' + year + '.csv'
   o3_data = READ_CSV(o3file)
   
   ;Compute a running 8-hour mean of O3 concentrations for Dallas (Fort Worth uses same data)
   o3_8hr  = OZONE_DALLAS_NAAQS(year)	
      
   o3_datestr =  STRMID(o3_data.field12,0,4)+STRMID(o3_data.field12,5,2)+STRMID(o3_data.field12,8,2)

   ;Create 1-hour bin with 6 hour interval
   time_arr = [ ]
   date1 = MAKE_DATE(year,11,01,06,00)
   date2 = MAKE_DATE(year,12,01,06,00)
   end_datestr = STRMID(MAKE_ISO_DATE_STRING(date2),0,4) + STRMID(MAKE_ISO_DATE_STRING(date2),5,2) + $
   			 	STRMID(MAKE_ISO_DATE_STRING(date2),8,2) 
  
   nt_yr = 8760/24
   FOR i = 0, nt_yr - 1 DO BEGIN
   		datestr = STRMID(MAKE_ISO_DATE_STRING(date1),0,4) + STRMID(MAKE_ISO_DATE_STRING(date1),5,2) + $
   			 STRMID(MAKE_ISO_DATE_STRING(date1),8,2) 
   		time_arr = [time_arr, datestr]
   		IF (datestr EQ end_datestr) THEN BREAK
   		date1 = TIME_INC(date1, 86400)
   ENDFOR 

   ;Create O3 bins
   o3_range = [0, 150]
   xrange   = [0, nt_yr]
   num_bins = 30
   do3   	 = FLOAT(o3_range[1] - o3_range[0])/num_bins														;Compute y bin spacing
   dx 		 = 1 
   o3bin 	 = 0.5*do3 + o3_range[0] + do3*FINDGEN(num_bins)

   nots_arr   = [ ]
   ts_arr     = [ ]
   FOR i = 0 , N_ELEMENTS(time_arr) -1 DO BEGIN
   	   io3  = WHERE(STRMID(o3_8hr.ozone_datestr,0,8) EQ time_arr[i], o3count)
   	   IF (io3[0] EQ -1) THEN io3 = [0]
   	   its   = WHERE(ts_datestr  EQ time_arr[i], tscount )
   	   its1  = WHERE(ts_datestr1 EQ time_arr[i], tscount1)

   	   IF (tscount EQ 0) THEN BEGIN
   	   		data_bin = LONG(((o3_8hr.ozone_daily_max[io3])-o3_range[0])/do3) 
   	 	    o3_nstorms = [o3_nstorms, o3_8hr.ozone_daily_max[io3]]
   	  		IF (STRMID(time_arr[i],4,2) EQ '07') THEN july_nstorm_count += 1 
   	  		IF (STRMID(time_arr[i],4,2) EQ '08') THEN aug_nstorm_count += 1 
   	  		IF (STRMID(time_arr[i],4,2) EQ '09') THEN sept_nstorm_count += 1 
   	  		IF (STRMID(time_arr[i],4,2) EQ '10') THEN oct_nstorm_count += 1 
   	  		IF (STRMID(time_arr[i],4,2) EQ '11') THEN nov_nstorm_count += 1 
   	   ENDIF ELSE data_bin = [0]
   	   IF (tscount1 GT 0) THEN BEGIN
   	   		ts_bin   = LONG(((o3_8hr.ozone_daily_max[io3])-o3_range[0])/do3) 
   	  	    o3_storms = [o3_storms, o3_8hr.ozone_daily_max[io3]]
   	  		IF (STRMID(time_arr[i],4,2) EQ '07') THEN july_storm_count += 1 
   	  		IF (STRMID(time_arr[i],4,2) EQ '08') THEN aug_storm_count += 1 
   	  		IF (STRMID(time_arr[i],4,2) EQ '09') THEN sept_storm_count += 1 
   	  		IF (STRMID(time_arr[i],4,2) EQ '10') THEN oct_storm_count += 1 
   	  		IF (STRMID(time_arr[i],4,2) EQ '11') THEN nov_storm_count += 1 
   	   ENDIF ELSE ts_bin = [0]
       nots_arr = [nots_arr,data_bin]
   	   ts_arr = [ts_arr,ts_bin] 
   ENDFOR  	
	nots_arr_tot1 = [nots_arr_tot1,nots_arr]
	ts_arr_tot1   = [ts_arr_tot1  ,ts_arr  ]

ENDFOREACH

nstorm_o3 = HISTOGRAM(nots_arr_tot1, BINSIZE = 1, MIN = 0, MAX = (num_bins -1))	
storm_o3  = HISTOGRAM(ts_arr_tot1, BINSIZE = 1, MIN = 0, MAX = (num_bins -1))	

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
title = 'Daily Max 8-hr Mean O3: NOV'
PLOT, o3bin, o3bin, /NODATA, $
	XRANGE = [5,115], $
	XSTYLE = 1, $
	XTITLE = 'O3 Concentration (ppb)', $
	YRANGE = [0, 25], $
	YSTYLE = 1, $
	YTITLE = 'Frequency (%)', $
	TITLE  = title

OPLOT, o3bin, 100.0*(FLOAT(nstorm_o3[1:-1])/TOTAL(nstorm_o3[1:-1])), THICK = 4, PSYM = 10
OPLOT, o3bin, 100.0*(FLOAT(storm_o3 [1:-1])/TOTAL(storm_o3 [1:-1])), THICK = 2, PSYM = 10, COLOR = COLOR_24('red')

storm_label = 'All Storms = '
IF (TYPENAME(storm_type) EQ 'STRING') THEN storm_label = storm_label1 + ' = '
IF (KEYWORD_SET(GULF)) THEN storm_label = 'Gulf ' + storm_label1 + ' = '
IF (KEYWORD_SET(ATLANTIC)) THEN storm_label = 'Atlantic ' + storm_label1 + ' = '

;For entire o3 range
XYOUTS, 60.0, 22.5, 'No Storms = ' + STRTRIM(LONG(aug_nstorm_count),1), /DATA
XYOUTS, 60.0, 21.0, storm_label + STRTRIM(LONG(aug_storm_count),1), COLOR = COLOR_24('red'), /DATA


;PLOT, o3bin, o3bin, /NODATA, $
;	XRANGE = [55,120], $
;	XSTYLE = 1, $
;	XTITLE = 'O3 Concentration (ppb)', $
;	YRANGE = [0, 10], $
;	YSTYLE = 1, $
;	YTITLE = 'Frequency (%)', $
;	TITLE  = title
;
;OPLOT, o3bin, 100.0*(FLOAT(nstorm_o3[1:-1])/TOTAL(nstorm_o3[1:-1])), THICK = 4, PSYM = 10
;OPLOT, o3bin, 100.0*(FLOAT(storm_o3 [1:-1])/TOTAL(storm_o3 [1:-1] )), THICK = 2, PSYM = 10, COLOR = COLOR_24('red')
;
;storm_label = 'All Storms '
;IF (TYPENAME(storm_type) EQ 'STRING') THEN storm_label = storm_label1 
;IF (KEYWORD_SET(GULF)) THEN storm_label = 'Gulf ' + storm_label1 
;IF (KEYWORD_SET(ATLANTIC)) THEN storm_label = 'Atlantic ' + storm_label1 
;
;;For zooming in on the O3 extremes (45-100 ppb)
;XYOUTS, 90.0, 9.0, 'No Storms', /DATA
;XYOUTS, 90.0, 8.25, storm_label, COLOR = COLOR_24('red'), /DATA

basin = 'all'
IF (KEYWORD_SET(GULF)) THEN basin = 'gulf'
IF (KEYWORD_SET(ATLANTIC)) THEN basin = 'atlantic'
IF (TYPENAME(storm_type) EQ 'LONG') THEN storm_type = 'all'

outdir = o3dir + 'pdf/'
pngfile = outdir + 'O3PDF_' + storm_type + '_' + basin +'_dailymax_excludebin1.png'
pngfile = outdir + 'O3PDF_' + storm_type + '_' + basin +'_dailymax_excludebin1_JULY-NOV.png'

epsfile = outdir + 'O3PDF_' + storm_type + '_' + basin +'_dailymax_excludebin1_NOV_new_area.eps'

IF KEYWORD_SET(landfall) THEN pngfile = o3dir + 'timeseries_landfall_closest_storms.png'
PRINT, pngfile
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
