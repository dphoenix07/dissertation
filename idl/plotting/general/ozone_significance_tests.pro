PRO OZONE_SIGNIFICANCE_TESTS, storm_type, $
	LANDFALL   = landfall, $
	ATLANTIC   = atlantic, $
	GULF	   = gulf, $
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
NCDF_VARGET, id, 'O3_DM', o3_dm
NCDF_CLOSE,  id																								;Close input file

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

FOREACH year, yr_arr DO BEGIN
	PRINT, year	
	imonth = WHERE(((ts_data.date.year GE year) AND (ts_data.date.year LT year+1)) AND $
		((ts_data.date.month GE 07) AND (ts_data.date.month LT 12)))

;    extr_storms = WHERE(((ts_data.class[imonth] EQ 'TD') OR (ts_data.class[imonth] EQ 'TS') OR $
;    	(ts_data.class[imonth] EQ 'HU')), istorms, COMPLEMENT = nostorms)


extr_storms = WHERE(((ts_data.class[imonth] EQ 'TD') OR (ts_data.class[imonth] EQ 'TS') OR $
    (ts_data.class[imonth] EQ 'HU')) AND ((ts_data.x[imonth] LE -75.0) AND $
    	(ts_data.x[imonth] GT -100.0) AND (ts_data.y[imonth] LE 40.0) AND $
    	(ts_data.y[imonth] GE 20.0)), istorms)
    	
    	
;extr_storms = WHERE(((ts_data.class[imonth] EQ 'HU')) AND ((ts_data.x[imonth] LE -5.0) AND $
;    	(ts_data.x[imonth] GT -75.0) AND (ts_data.y[imonth] LE 40.0) AND $
;    	(ts_data.y[imonth] GE 20.0)), istorms)
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
  	  	(ts_data.class[imonth] EQ 'HU')) AND ((ts_data.x[imonth] LE -75.0) AND $
    	(ts_data.x[imonth] GT -100.0) AND (ts_data.y[imonth] LE 40.0) AND $
    	(ts_data.y[imonth] GE 20.0)), ilandfall)
  	 
  	 ;Fill in other times between making landfall and the end of the storm
  	 IF (ilandfall GT 0) THEN BEGIN
  	 	iperiod = imonth[landfall] 
  	 	FOR lf = 0, N_ELEMENTS(iperiod)-1 DO BEGIN
  	 		land_end    = (ts_data.iend - iperiod[lf])
  	 		land_end2   = WHERE(land_end GT 0)
  	 		land_end3   = WHERE(MIN(land_end2[land_end]))
  	 		endpt 	    = land_end[land_end2[land_end3]]
  	 		index 	    = WHERE(land_end EQ endpt[0])
  	 		iperiod_add = [iperiod[lf]-6:ts_data.iend[index]]
  	 		iperiod 	= [iperiod,iperiod_add]
  	 	ENDFOR
  	 	  	 		PRINT, 'new landfall code'
	
  	 ENDIF ELSE iperiod = 0
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
   date1 = MAKE_DATE(year,07,01,06,00)
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
   o3_range = [-40, 60]
   xrange   = [0, nt_yr]
   num_bins = 20
   do3   	 = FLOAT(o3_range[1] - o3_range[0])/num_bins														;Compute y bin spacing
   dx 		 = 1 
   o3bin 	 = 0.5*do3 + o3_range[0] + do3*FINDGEN(num_bins)

   nots_arr   = [ ]
   ts_arr     = [ ]
   FOR i = 0 , N_ELEMENTS(time_arr) -1 DO BEGIN
   	   mth = WHERE(STRMID(time_arr[i],4,2) EQ mth_arr)

   	   io3  = WHERE(STRMID(o3_8hr.ozone_datestr,0,8) EQ time_arr[i], o3count)
   	   IF (io3[0] EQ -1) THEN io3 = [0]
   	   its   = WHERE(ts_datestr  EQ time_arr[i], tscount )
   	   its1  = WHERE(ts_datestr1 EQ time_arr[i], tscount1)

   	   IF (tscount EQ 0) THEN BEGIN
   	   		data_bin = LONG(((o3_8hr.ozone_daily_max[io3]-o3_dm[mth])-o3_range[0])/do3) 
   	 	    o3_nstorms = [o3_nstorms, (o3_8hr.ozone_daily_max[io3])]
;   	 	    o3_nstorms = [o3_nstorms, (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]
   	  		IF (STRMID(time_arr[i],4,2) EQ '07') THEN july_nstorm_count += 1 
   	  		IF (STRMID(time_arr[i],4,2) EQ '08') THEN aug_nstorm_count += 1 
   	  		IF (STRMID(time_arr[i],4,2) EQ '09') THEN sept_nstorm_count += 1 
   	  		IF (STRMID(time_arr[i],4,2) EQ '10') THEN oct_nstorm_count += 1 
   	  		IF (STRMID(time_arr[i],4,2) EQ '11') THEN nov_nstorm_count += 1 
   	   ENDIF ELSE data_bin = [0]
   	   IF (tscount1 GT 0) THEN BEGIN
   	   		ts_bin   = LONG(((o3_8hr.ozone_daily_max[io3]-o3_dm[mth])-o3_range[0])/do3) 
   	  	    o3_storms = [o3_storms, (o3_8hr.ozone_daily_max[io3])]
;   	  	    o3_storms = [o3_storms, (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]
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

nstorm_pdf = (FLOAT(nstorm_o3[1:-1])/TOTAL(nstorm_o3[1:-1]))
storm_pdf = (FLOAT(storm_o3[1:-1])/TOTAL(storm_o3[1:-1]))

PRINT, RS_TEST(o3_storms, o3_nstorms, UX = ux, UY = uy)
PRINT, 'Mann-Whitney Statistics: Ux = ', ux, ', Uy = ', uy
 
PRINT, 'tropical storm std dev o3 concentration: ' + STRTRIM(STDDEV(o3_storms),1)
PRINT, 'no storm std dev o3 concentration: ' + STRTRIM(STDDEV(o3_nstorms),1)

PRINT, 'tropical storm variance o3 concentration: ' + STRTRIM((STDDEV(o3_storms)^2),1)
PRINT, 'no storm variance o3 concentration: ' + STRTRIM((STDDEV(o3_nstorms)^2),1)

PRINT, 'tropical storm mean o3 concentration: ' + STRTRIM(MEAN(o3_storms,/NAN),1)
PRINT, 'no storm mean o3 concentration: ' + STRTRIM(MEAN(o3_nstorms,/NAN),1)

PRINT, ' '
PRINT, 'tropical storm max o3 concentration: ' + STRTRIM(MAX(o3_storms,/NAN),1)
PRINT, 'no storm max o3 concentration: ' + STRTRIM(MAX(o3_nstorms,/NAN),1)

PRINT, ' '
isort = SORT(o3_storms)
imid  = N_ELEMENTS(o3_storms)/2
PRINT, 'tropical storm median o3 concentration: ' + STRTRIM(o3_storms[isort[imid]],1)

isort = SORT(o3_nstorms)
imid  = N_ELEMENTS(o3_nstorms)/2
PRINT, 'no storm median o3 concentration: ' + STRTRIM(o3_nstorms[isort[imid]],1)

PRINT, ' '
;Student T-Test
student_ttest = TM_TEST(o3_storms,o3_nstorms,/UNEQUAL)
PRINT, 'the student t-test test statistic is: ' + STRTRIM(student_ttest[0],1)
PRINT, 'the student t-test significance value is: ' + STRTRIM(student_ttest[1],1)

;Calculate Hellinger Distance
k = N_ELEMENTS(nstorm_o3)
FOR i = 0, k-2 DO BEGIN
	IF (i EQ 0) THEN sum1 = TOTAL((SQRT(nstorm_pdf[i])-SQRT(storm_pdf[i]))^2) $
	ELSE sum1 += TOTAL((SQRT(nstorm_pdf[i])-SQRT(storm_pdf[i]))^2)
ENDFOR
hellinger = (1/SQRT(2))*SQRT(sum1)

PRINT, ' '
PRINT, 'the hellinger distance is: ' + STRTRIM(hellinger,1) 
PRINT, 'but the hellinger distance is not a robust statistic as it depends on the bin size'
PRINT, 'need to bootstrap the hellinger distance 1000 times and calculate the p-value'

;Test of two-proportions
;phat = x/n where x = # of events where daily max 8-hr o3 exceeds 80 ppb and n is total 
;number of points

zstar_arr = []
FOR ii=0,99 DO BEGIN
    s_indices  = ROUND(RANDOMU(seed, 50) * N_ELEMENTS(o3_storms)-1)
    ns_indices = ROUND(RANDOMU(seed, 50) * N_ELEMENTS(o3_nstorms)-1)
    
    sepa = WHERE(o3_storms[s_indices] GE 70.0, i70_storms)
    n_storms = N_ELEMENTS(o3_storms[s_indices])
    
    nsepa = WHERE(o3_nstorms[ns_indices] GE 70.0, i70_nstorms)
    n_nstorms = N_ELEMENTS(o3_nstorms[ns_indices])
    
    phat1 = FLOAT(i70_storms)/FLOAT(n_storms)
    phat2 = FLOAT(i70_nstorms)/FLOAT(n_nstorms)
    
    pstar = (FLOAT(i70_storms+i70_nstorms) / FLOAT(n_storms+n_nstorms))
    
    num = phat1 - phat2
    den = SQRT(pstar*(1-pstar)*((1/FLOAT(n_storms))+(1/FLOAT(n_nstorms))))
    
    zstar = num/den
	zstar_arr = [zstar_arr, zstar]
ENDFOR

PRINT, 'The z-test statistic is: ' + STRTRIM(MEAN(zstar),1) + ' go look up the p-value'
PRINT, 'storm population size = ', N_ELEMENTS(o3_storms)
PRINT, 'no-storm population size = ', N_ELEMENTS(o3_nstorms)

STOP           
END
