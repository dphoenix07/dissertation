PRO PLOT_STORM_TYPE_BOX, $
	NORMALIZE = normalize, $
	EPS 	  = eps, $
	PNG 	  = png

;+
; Name:
;		PLOT_STORM_TYPE_BOX
; Purpose:
;		This is a procedure to plot boxplots of Daily Max O3 8-hr Mean Concentrations
;		for different storm type scenarios (TD, TS, HU, Gulf, Atlantic)
; Calling sequence:
;		PLOT_STORM_TYPE_BOX, date
; Input:
;		date  : Analysis date {CDATE}.
; Output:
;		A boxplot of storm altitude frequencies for NEXRAD observations,
;		WRF reflectivity, and WRF cloud top.
; Keywords:
;		EPS  : If set, output to PostScript.
;		PNG  : If set, write PNG image.
; Author and history:
;		Daniel B. Phoenix	02-14-19.
;						    02-21-19.	If more than one storm type exists at the same
;										time, that time is discarded. This makes it 
;										clearer that box plots for each storm type are
;										not being contaminated by other storm types.

;-

COMPILE_OPT IDL2																															;Set compile options


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

;+create variables for yearly arrays
o3_allstorms_yr =[]
o3_nostorms_yr  =[]
o3_all_td_yr = []
o3_all_ts_yr = []
o3_all_hu_yr = []

;o3_all_gulf_yr = []
;o3_all_atl_yr = []
o3_all_egulf_yr = []
o3_all_wgulf_yr = []
o3_all_ac_yr = []
o3_all_atl_yr = []

;o3_gulf_td_yr = []
;o3_gulf_ts_yr = []
;o3_gulf_hu_yr = []
o3_wgulf_td_yr = []
o3_wgulf_ts_yr = []
o3_wgulf_hu_yr = []
o3_egulf_td_yr = []
o3_egulf_ts_yr = []
o3_egulf_hu_yr = []

;o3_atlantic_td_yr = []
;o3_atlantic_ts_yr = []
;o3_atlantic_hu_yr = []
o3_ac_td_yr = []
o3_ac_ts_yr = []
o3_ac_hu_yr = []
o3_atl_td_yr = []
o3_atl_ts_yr = []
o3_atl_hu_yr = []

o3_landfall_yr = []
;-end

FOREACH year, yr_arr DO BEGIN
	PRINT, year	
	imonth = WHERE(((ts_data.date.year GE year) AND (ts_data.date.year LT year+1)) AND $
		((ts_data.date.month GE 7) AND (ts_data.date.month LT 12)))

    extr_storms = WHERE(((ts_data.class[imonth] EQ 'TD') OR (ts_data.class[imonth] EQ 'TS') OR $
    	(ts_data.class[imonth] EQ 'HU')), istorms, COMPLEMENT = nostorms)
    
     loc = WHERE(((ts_data.x[imonth[extr_storms]] LE -70.0) AND (ts_data.x[imonth[extr_storms]] GT -100.0) AND $
 			(ts_data.y[imonth[extr_storms]] LE 40.0) AND (ts_data.y[imonth[extr_storms]] GE 20.0)), loc_count, COMPLEMENT = iout)
	IF (loc_count GT 0) THEN iperiod = imonth[extr_storms[loc]] ELSE iperiod = 0
	
   ts_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),0,4) +  $
  		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),5,2) + $
  		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),8,2)

;;STARTCASE
    ;+All TD, TS, HU storms
    ;all_td = WHERE(((ts_data.class[imonth] EQ 'TD')), istorms)
    ;all_ts = WHERE(((ts_data.class[imonth] EQ 'TS')), istorms)
    ;all_hu = WHERE(((ts_data.class[imonth] EQ 'HU')), istorms)
	
	;all_td_iperiod = imonth[all_td]
	;all_ts_iperiod = imonth[all_ts]
	;all_hu_iperiod = imonth[all_hu]

    all_td = WHERE(((ts_data.class[imonth] EQ 'TD') AND (ts_data.x[imonth] LE -70.0) AND $
    	 (ts_data.x[imonth] GT -100.0) AND (ts_data.y[imonth] LE 40.0) AND $
    	 (ts_data.y[imonth] GE 20.0)), istorms)
    all_ts = WHERE(((ts_data.class[imonth] EQ 'TS') AND (ts_data.x[imonth] LE -70.0) AND $
    	 (ts_data.x[imonth] GT -100.0) AND (ts_data.y[imonth] LE 40.0) AND $
    	 (ts_data.y[imonth] GE 20.0)), istorms)
    all_hu = WHERE(((ts_data.class[imonth] EQ 'HU') AND (ts_data.x[imonth] LE -70.0) AND $
    	 (ts_data.x[imonth] GT -100.0) AND (ts_data.y[imonth] LE 40.0) AND $
    	 (ts_data.y[imonth] GE 20.0)), istorms)
	
	all_td_iperiod = imonth[all_td]
	all_ts_iperiod = imonth[all_ts]
	all_hu_iperiod = imonth[all_hu]
    
 	;-end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;Create three regions (W Gulf, E Gulf, Atl Coast)
;Restrict total analysis to Gulf and Atlantic coast (exclude tracks a certain distance
;from DFW)
;West Gulf - Lon(100W-90W)
;East Gulf - Lon(90W-80W)
;Atl Coast - Lon(80W-75W), Lat(20-40N)
;Far Atl   - Lon(east of 75W), Lat(20-40N)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

 	;+All gulf and atlantic storms
 	igulf = WHERE(ts_data.x[imonth] LE -80.0, gulfcount, COMPLEMENT = iatlantic)
	all_gulf_iperiod = imonth[igulf]
	all_atlantic_iperiod = imonth[iatlantic]

 	iwgulf = WHERE(((ts_data.x[imonth] LE -90.0) AND (ts_data.x[imonth] GT -100.0) AND $
 			(ts_data.y[imonth] LE 35.0) AND (ts_data.y[imonth] GE 20.0)), wgulfcount, COMPLEMENT = iout)
	all_wgulf_iperiod = imonth[iwgulf]

 	iegulf = WHERE(((ts_data.x[imonth] LE -82.0) AND (ts_data.x[imonth] GT -90.0) AND $
 			(ts_data.y[imonth] LE 35.0) AND (ts_data.y[imonth] GE 20.0)), egulfcount, COMPLEMENT = iout)
	all_egulf_iperiod = imonth[iegulf]

 	iac = WHERE(((ts_data.x[imonth] LE -75.0) AND (ts_data.x[imonth] GT -82.0) AND $
 			(ts_data.y[imonth] LE 40.0) AND (ts_data.y[imonth] GE 20.0)), account, COMPLEMENT = iout)
	all_ac_iperiod = imonth[iac]

 	iatl = WHERE(((ts_data.x[imonth] LE -50.0) AND (ts_data.x[imonth] GT -75.0) AND $
 			(ts_data.y[imonth] LE 40.0) AND (ts_data.y[imonth] GE 15.0)), atlcount, COMPLEMENT = iout)
	all_atl_iperiod = imonth[iatl]
	
	PRINT, egulfcount
	PRINT, wgulfcount
	PRINT, account	
	PRINT, atlcount
	;-end
	
	;+Gulf/Atlantic TD, TS, HU storms
	;igulf = WHERE(ts_data.x[all_td_iperiod] LE -80.0, gulfcount, COMPLEMENT = iatlantic)
	;gulf_td_iperiod = all_td_iperiod[igulf]
	;atlantic_td_iperiod = all_td_iperiod[iatlantic]
	;
	;igulf = WHERE(ts_data.x[all_ts_iperiod] LE -80.0, gulfcount, COMPLEMENT = iatlantic)	
	;gulf_ts_iperiod = all_ts_iperiod[igulf]
	;atlantic_ts_iperiod = all_ts_iperiod[iatlantic]
;
	;igulf = WHERE(ts_data.x[all_hu_iperiod] LE -80.0, gulfcount, COMPLEMENT = iatlantic)	
	;gulf_hu_iperiod = all_hu_iperiod[igulf]
	;atlantic_hu_iperiod = all_hu_iperiod[iatlantic]

    all_td_wgulf = WHERE(((ts_data.class[imonth] EQ 'TD') AND (ts_data.x[imonth] LE -90.0) AND $
    	 (ts_data.x[imonth] GT -100.0) AND (ts_data.y[imonth] LE 35.0) AND $
    	 (ts_data.y[imonth] GE 20.0)), istorms)
    all_td_egulf = WHERE(((ts_data.class[imonth] EQ 'TD') AND (ts_data.x[imonth] LE -82.0) AND $
    	 (ts_data.x[imonth] GT -90.0) AND (ts_data.y[imonth] LE 35.0) AND $
    	 (ts_data.y[imonth] GE 20.0)), istorms)
    all_td_ac = WHERE(((ts_data.class[imonth] EQ 'TD') AND (ts_data.x[imonth] LE -75.0) AND $
    	 (ts_data.x[imonth] GT -82.0) AND (ts_data.y[imonth] LE 40.0) AND $
    	 (ts_data.y[imonth] GE 20.0)), istorms)
    all_td_atl = WHERE(((ts_data.class[imonth] EQ 'TD') AND (ts_data.x[imonth] GE -75.0) AND $
    	 (ts_data.x[imonth] LE -50.0) AND (ts_data.y[imonth] LE 40.0) AND $
    	 (ts_data.y[imonth] GE 15.0)), istorms)
	
	wgulf_td_iperiod = imonth[all_td_wgulf]
	egulf_td_iperiod = imonth[all_td_egulf]
	ac_td_iperiod = imonth[all_td_ac]
	atl_td_iperiod = imonth[all_td_atl]

    all_ts_wgulf = WHERE(((ts_data.class[imonth] EQ 'TS') AND (ts_data.x[imonth] LE -90.0) AND $
    	 (ts_data.x[imonth] GT -100.0) AND (ts_data.y[imonth] LE 35.0) AND $
    	 (ts_data.y[imonth] GE 20.0)), istorms)
    all_ts_egulf = WHERE(((ts_data.class[imonth] EQ 'TS') AND (ts_data.x[imonth] LE -82.0) AND $
    	 (ts_data.x[imonth] GT -90.0) AND (ts_data.y[imonth] LE 35.0) AND $
    	 (ts_data.y[imonth] GE 20.0)), istorms)
    all_ts_ac = WHERE(((ts_data.class[imonth] EQ 'TS') AND (ts_data.x[imonth] LE -75.0) AND $
    	 (ts_data.x[imonth] GT -82.0) AND (ts_data.y[imonth] LE 40.0) AND $
    	 (ts_data.y[imonth] GE 20.0)), istorms)
    all_ts_atl = WHERE(((ts_data.class[imonth] EQ 'TS') AND (ts_data.x[imonth] GE -75.0) AND $
    	 (ts_data.x[imonth] LE -50.0) AND (ts_data.y[imonth] LE 40.0) AND $
    	 (ts_data.y[imonth] GE 15.0)), istorms)
	
	wgulf_ts_iperiod = imonth[all_ts_wgulf]
	egulf_ts_iperiod = imonth[all_ts_egulf]
	ac_ts_iperiod = imonth[all_ts_ac]
	atl_ts_iperiod = imonth[all_ts_atl]

    all_hu_wgulf = WHERE(((ts_data.class[imonth] EQ 'HU') AND (ts_data.x[imonth] LE -90.0) AND $
    	 (ts_data.x[imonth] GT -100.0) AND (ts_data.y[imonth] LE 35.0) AND $
    	 (ts_data.y[imonth] GE 20.0)), istorms)
    all_hu_egulf = WHERE(((ts_data.class[imonth] EQ 'HU') AND (ts_data.x[imonth] LE -82.0) AND $
    	 (ts_data.x[imonth] GT -90.0) AND (ts_data.y[imonth] LE 35.0) AND $
    	 (ts_data.y[imonth] GE 20.0)), istorms)
    all_hu_ac = WHERE(((ts_data.class[imonth] EQ 'HU') AND (ts_data.x[imonth] LE -75.0) AND $
    	 (ts_data.x[imonth] GT -82.0) AND (ts_data.y[imonth] LE 40.0) AND $
    	 (ts_data.y[imonth] GE 20.0)), istorms)
    all_hu_atl = WHERE(((ts_data.class[imonth] EQ 'HU') AND (ts_data.x[imonth] GE -75.0) AND $
    	 (ts_data.x[imonth] LE -50.0) AND (ts_data.y[imonth] LE 40.0) AND $
    	 (ts_data.y[imonth] GE 15.0)), istorms)
	
	wgulf_hu_iperiod = imonth[all_hu_wgulf]
	egulf_hu_iperiod = imonth[all_hu_egulf]
	ac_hu_iperiod = imonth[all_hu_ac]
	atl_hu_iperiod = imonth[all_hu_atl]
	;-end
	
 	;+All landfalling storms
  	landfall = WHERE(((ts_data.id[imonth] EQ 'L') OR (ts_data.id[imonth] EQ 'C')) AND $
  	 	((ts_data.class[imonth] EQ 'TD') OR (ts_data.class[imonth] EQ 'TS') OR $
  	  	(ts_data.class[imonth] EQ 'HU')), ilandfall)
  	IF (ilandfall GT 0) THEN landfall_iperiod = imonth[landfall] ELSE landfall_iperiod = 0
	;-end
;;ENDCASE	

	;+create date strings for each case
   ts_datestr1 = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),0,4) +  $
  		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),5,2) + $
  		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),8,2)

   all_td_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[all_td_iperiod]),0,4) +  $
  		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[all_td_iperiod]),5,2) + $
  		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[all_td_iperiod]),8,2)

   all_ts_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[all_ts_iperiod]),0,4) +  $
  		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[all_ts_iperiod]),5,2) + $
  		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[all_ts_iperiod]),8,2)

   all_hu_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[all_hu_iperiod]),0,4) +  $
  		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[all_hu_iperiod]),5,2) + $
  		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[all_hu_iperiod]),8,2)

  ;all_gulf_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[all_gulf_iperiod]),0,4) +  $
  ;		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[all_gulf_iperiod]),5,2) + $
  ;		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[all_gulf_iperiod]),8,2)
;
  ;all_atl_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[all_atlantic_iperiod]),0,4) +  $
  ;		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[all_atlantic_iperiod]),5,2) + $
  ;		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[all_atlantic_iperiod]),8,2)

  all_ac_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[all_ac_iperiod]),0,4) +  $
  		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[all_ac_iperiod]),5,2) + $
  		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[all_ac_iperiod]),8,2)

  all_atl_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[all_atl_iperiod]),0,4) +  $
  		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[all_atl_iperiod]),5,2) + $
  		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[all_atl_iperiod]),8,2)

	all_egulf_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[all_egulf_iperiod]),0,4) +  $
  		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[all_egulf_iperiod]),5,2) + $
  		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[all_egulf_iperiod]),8,2)

	all_wgulf_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[all_wgulf_iperiod]),0,4) +  $
  		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[all_wgulf_iperiod]),5,2) + $
  		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[all_wgulf_iperiod]),8,2)

   ;gulf_td_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[gulf_td_iperiod]),0,4) +  $
  ;		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[gulf_td_iperiod]),5,2) + $
  ;		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[gulf_td_iperiod]),8,2)
;
   ;gulf_ts_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[gulf_ts_iperiod]),0,4) +  $
  ;		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[gulf_ts_iperiod]),5,2) + $
  ;		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[gulf_ts_iperiod]),8,2)
;
   ;gulf_hu_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[gulf_hu_iperiod]),0,4) +  $
  ;		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[gulf_hu_iperiod]),5,2) + $
  ;		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[gulf_hu_iperiod]),8,2)

   wgulf_td_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[wgulf_td_iperiod]),0,4) +  $
  		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[wgulf_td_iperiod]),5,2) + $
  		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[wgulf_td_iperiod]),8,2)

   wgulf_ts_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[wgulf_ts_iperiod]),0,4) +  $
  		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[wgulf_ts_iperiod]),5,2) + $
  		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[wgulf_ts_iperiod]),8,2)

   wgulf_hu_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[wgulf_hu_iperiod]),0,4) +  $
  		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[wgulf_hu_iperiod]),5,2) + $
  		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[wgulf_hu_iperiod]),8,2)

   egulf_td_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[egulf_td_iperiod]),0,4) +  $
  		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[egulf_td_iperiod]),5,2) + $
  		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[egulf_td_iperiod]),8,2)

   egulf_ts_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[egulf_ts_iperiod]),0,4) +  $
  		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[egulf_ts_iperiod]),5,2) + $
  		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[egulf_ts_iperiod]),8,2)

   egulf_hu_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[egulf_hu_iperiod]),0,4) +  $
  		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[egulf_hu_iperiod]),5,2) + $
  		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[egulf_hu_iperiod]),8,2)

   ;atlantic_td_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[atlantic_td_iperiod]),0,4) +  $
  ;		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[atlantic_td_iperiod]),5,2) + $
  ;		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[atlantic_td_iperiod]),8,2)
;
   ;atlantic_ts_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[atlantic_ts_iperiod]),0,4) +  $
  ;		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[atlantic_ts_iperiod]),5,2) + $
  ;		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[atlantic_ts_iperiod]),8,2)
;
   ;atlantic_hu_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[atlantic_hu_iperiod]),0,4) +  $
  ;		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[atlantic_hu_iperiod]),5,2) + $
  ;		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[atlantic_hu_iperiod]),8,2)

   ac_td_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[ac_td_iperiod]),0,4) +  $
  		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[ac_td_iperiod]),5,2) + $
  		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[ac_td_iperiod]),8,2)

   ac_ts_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[ac_ts_iperiod]),0,4) +  $
  		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[ac_ts_iperiod]),5,2) + $
  		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[ac_ts_iperiod]),8,2)

   ac_hu_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[ac_hu_iperiod]),0,4) +  $
  		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[ac_hu_iperiod]),5,2) + $
  		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[ac_hu_iperiod]),8,2)

   atl_td_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[atl_td_iperiod]),0,4) +  $
  		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[atl_td_iperiod]),5,2) + $
  		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[atl_td_iperiod]),8,2)

   atl_ts_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[atl_ts_iperiod]),0,4) +  $
  		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[atl_ts_iperiod]),5,2) + $
  		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[atl_ts_iperiod]),8,2)

   atl_hu_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[atl_hu_iperiod]),0,4) +  $
  		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[atl_hu_iperiod]),5,2) + $
  		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[atl_hu_iperiod]),8,2)

   landfall_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[landfall_iperiod]),0,4) +  $
  		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[landfall_iperiod]),5,2) + $
  		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[landfall_iperiod]),8,2)
  	;-end
			
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
      
   ;+create variable arrays for hourly save
   o3_allstorms_arr = []
   o3_nostorms_arr = []
   o3_all_td_arr = []
   o3_all_ts_arr = []
   o3_all_hu_arr = []

   ;o3_all_gulf_arr = []
   ;o3_all_atl_arr = []
   o3_all_egulf_arr = []
   o3_all_ac_arr = []
   o3_all_atl_arr = []
   o3_all_wgulf_arr = []

   ;o3_gulf_td_arr = []
   ;o3_gulf_ts_arr = []
   ;o3_gulf_hu_arr = []

   o3_wgulf_td_arr = []
   o3_wgulf_ts_arr = []
   o3_wgulf_hu_arr = []
   o3_egulf_td_arr = []
   o3_egulf_ts_arr = []
   o3_egulf_hu_arr = []

   ;o3_atlantic_td_arr = []
   ;o3_atlantic_ts_arr = []
   ;o3_atlantic_hu_arr = []

   o3_ac_td_arr = []
   o3_ac_ts_arr = []
   o3_ac_hu_arr = []
   o3_atl_td_arr = []
   o3_atl_ts_arr = []
   o3_atl_hu_arr = []

   o3_landfall_arr = []
	
	;+variables for immediate read
	o3_all_td = []
    o3_all_ts = []
    o3_all_hu = []
    
    ;o3_all_gulf = []
    ;o3_all_atl = []
    o3_all_egulf = []
    o3_all_wgulf = []
    o3_all_ac = []
    o3_all_atl = []
    
    ;o3_gulf_td = []
    ;o3_gulf_ts = []
    ;o3_gulf_hu = []
    o3_wgulf_td = []
    o3_wgulf_ts = []
    o3_wgulf_hu = []
    o3_egulf_td = []
    o3_egulf_ts = []
    o3_egulf_hu = []

    ;o3_atlantic_td = []
    ;o3_atlantic_ts = []
    ;o3_atlantic_hu = []
    o3_ac_td = []
    o3_ac_ts = []
    o3_ac_hu = []
    o3_atl_td = []
    o3_atl_ts = []
    o3_atl_hu = []
    o3_landfall = []
    ;-end create

   FOR i = 0 , N_ELEMENTS(time_arr) -1 DO BEGIN
   	   PRINT, 'new time = ', time_arr[i]
   	   io3  = WHERE(STRMID(o3_8hr.ozone_datestr,0,8) EQ time_arr[i], o3count)
   	   mth  = WHERE(STRMID(time_arr[i],4,2) EQ mth_arr)

   	   IF (io3[0] EQ -1) THEN io3 = [0]
   	   
   	   its   = WHERE(ts_datestr  EQ time_arr[i], tscount )
   	   its1  = WHERE(ts_datestr1 EQ time_arr[i], tscount1)
   	   iall_td = WHERE(all_td_datestr EQ time_arr[i], all_td_count)
   	   iall_ts = WHERE(all_ts_datestr EQ time_arr[i], all_ts_count)
   	   iall_hu = WHERE(all_hu_datestr EQ time_arr[i], all_hu_count)

       ;iall_gulf = WHERE(all_gulf_datestr EQ time_arr[i], all_gulf_count)
   	   ;iall_atl = WHERE(all_atl_datestr EQ time_arr[i], all_atl_count)
       iall_egulf = WHERE(all_egulf_datestr EQ time_arr[i], all_egulf_count)
       iall_wgulf = WHERE(all_wgulf_datestr EQ time_arr[i], all_wgulf_count)
   	   iall_ac = WHERE(all_ac_datestr EQ time_arr[i], all_ac_count)
   	   iall_atl = WHERE(all_atl_datestr EQ time_arr[i], all_atl_count)

   	   ;igulf_td = WHERE(gulf_td_datestr EQ time_arr[i], gulf_td_count)
   	   ;igulf_ts = WHERE(gulf_ts_datestr EQ time_arr[i], gulf_ts_count)
   	   ;igulf_hu = WHERE(gulf_hu_datestr EQ time_arr[i], gulf_hu_count)
   	   iwgulf_td = WHERE(wgulf_td_datestr EQ time_arr[i], wgulf_td_count)
   	   iwgulf_ts = WHERE(wgulf_ts_datestr EQ time_arr[i], wgulf_ts_count)
   	   iwgulf_hu = WHERE(wgulf_hu_datestr EQ time_arr[i], wgulf_hu_count)
   	   iegulf_td = WHERE(egulf_td_datestr EQ time_arr[i], egulf_td_count)
   	   iegulf_ts = WHERE(egulf_ts_datestr EQ time_arr[i], egulf_ts_count)
   	   iegulf_hu = WHERE(egulf_hu_datestr EQ time_arr[i], egulf_hu_count)
   	   
   	   ;iatlantic_td = WHERE(atlantic_td_datestr EQ time_arr[i], atlantic_td_count)
   	   ;iatlantic_ts = WHERE(atlantic_ts_datestr EQ time_arr[i], atlantic_ts_count)
   	   ;iatlantic_hu = WHERE(atlantic_hu_datestr EQ time_arr[i], atlantic_hu_count)
   	   iac_td = WHERE(ac_td_datestr EQ time_arr[i], ac_td_count)
   	   iac_ts = WHERE(ac_ts_datestr EQ time_arr[i], ac_ts_count)
   	   iac_hu = WHERE(ac_hu_datestr EQ time_arr[i], ac_hu_count)
   	   iatl_td = WHERE(atl_td_datestr EQ time_arr[i], atl_td_count)
   	   iatl_ts = WHERE(atl_ts_datestr EQ time_arr[i], atl_ts_count)
   	   iatl_hu = WHERE(atl_hu_datestr EQ time_arr[i], atl_hu_count)
   	   ilandfall = WHERE(landfall_datestr EQ time_arr[i], landfall_count)

	   
	   ;IF (tscount GT 0) THEN BEGIN
	       ;PRINT, time_arr[i]
	       ;PRINT, 'its   = ', its   
           ;PRINT, 'its1  = ', its1  
           ;PRINT, 'iall_td = ', iall_td 
           ;PRINT, 'iall_ts = ', iall_ts 
           ;PRINT, 'iall_hu = ', iall_hu 
           ;PRINT, 'iall_gulf= ', iall_gulf
           ;PRINT, 'iall_atl = ', iall_atl 
           ;PRINT, 'igulf_td = ', igulf_td 
           ;PRINT, 'igulf_ts = ', igulf_ts 
           ;PRINT, 'igulf_hu = ', igulf_hu 
           ;PRINT, 'iatlantic_td= ', iatlantic_td
           ;PRINT, 'iatlantic_ts= ', iatlantic_ts
           ;PRINT, 'iatlantic_hu= ', iatlantic_hu
           ;PRINT, 'ilandfall = ', ilandfall 
		   ;
		   ;IF (((all_td_count GT 0) AND (all_ts_count GT 0)) OR $
		   ;		((all_td_count GT 0) AND (all_hu_count GT 0)) OR $
		   ;		((all_ts_count GT 0) AND (all_hu_count GT 0)) OR $
		   ;		((all_gulf_count GT 0) AND (all_hu_count GT 0))) THEN BEGIN
		   ;			PRINT, 'non-exclusive populations, skip time'
		   ;			CONTINUE
		   ;	ENDIF
		;ENDIF
		;PRINT, 'exclusive populations'
		
   	   IF (~KEYWORD_SET(NORMALIZE)) THEN BEGIN
  	       ;+All storms and no storms
   	       IF (tscount EQ 0) THEN $
   	 	        o3_nstorms = [o3_nstorms, o3_8hr.ozone_daily_max[io3]]
   	       IF (tscount1 GT 0) THEN $
   	  	        o3_storms = [o3_storms, o3_8hr.ozone_daily_max[io3]]
	   ENDIF

   	   IF (KEYWORD_SET(NORMALIZE)) THEN BEGIN
  	       ;+All storms and no storms
   	       IF (tscount EQ 0) THEN $
   	 	        o3_nstorms = [o3_nstorms,  (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]
   	       IF (tscount1 GT 0) THEN $
   	  	        o3_storms = [o3_storms,  (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]
	   ENDIF
          	   
   	   o3_allstorms_arr = [o3_allstorms_arr, o3_storms ]
   	   o3_nostorms_arr  = [o3_nostorms_arr , o3_nstorms] 
   	   ;-end
   	   
   	   ;+Subsets
   	   IF (~KEYWORD_SET(NORMALIZE)) THEN BEGIN
   	   		IF (all_td_count GT 0) THEN $
   	 	    	o3_all_td = [o3_all_td, o3_8hr.ozone_daily_max[io3]]
   	 	    IF (all_ts_count GT 0) THEN $
   	 	        o3_all_ts = [o3_all_ts, o3_8hr.ozone_daily_max[io3]]
   	 	    IF (all_hu_count GT 0) THEN $
   	 	        o3_all_hu = [o3_all_hu, o3_8hr.ozone_daily_max[io3]]

    	    ; IF (all_gulf_count GT 0) THEN $
  	 	    ;    o3_all_gulf = [o3_all_gulf, o3_8hr.ozone_daily_max[io3]]
   	 	    ;IF (all_atl_count GT 0) THEN $
   	 	    ;    o3_all_atl = [o3_all_atl, o3_8hr.ozone_daily_max[io3]]
    	     IF (all_egulf_count GT 0) THEN $
  	 	        o3_all_egulf = [o3_all_egulf, o3_8hr.ozone_daily_max[io3]]
    	     IF (all_wgulf_count GT 0) THEN $
  	 	        o3_all_wgulf = [o3_all_wgulf, o3_8hr.ozone_daily_max[io3]]
   	 	    IF (all_ac_count GT 0) THEN $
   	 	        o3_all_ac = [o3_all_ac, o3_8hr.ozone_daily_max[io3]]
   	 	    IF (all_atl_count GT 0) THEN $
   	 	        o3_all_atl = [o3_all_atl, o3_8hr.ozone_daily_max[io3]]

   	 	   ; IF (gulf_td_count GT 0) THEN $
   	 	   ;     o3_gulf_td = [o3_gulf_td, o3_8hr.ozone_daily_max[io3]]
   	 	   ; IF (gulf_ts_count GT 0) THEN $
   	 	   ;     o3_gulf_ts = [o3_gulf_ts, o3_8hr.ozone_daily_max[io3]]
   	 	   ; IF (gulf_hu_count GT 0) THEN $
   	 	   ;     o3_gulf_hu = [o3_gulf_hu, o3_8hr.ozone_daily_max[io3]]
   	 	    IF (wgulf_td_count GT 0) THEN $
   	 	        o3_wgulf_td = [o3_wgulf_td, o3_8hr.ozone_daily_max[io3]]
   	 	    IF (wgulf_ts_count GT 0) THEN $
   	 	        o3_wgulf_ts = [o3_wgulf_ts, o3_8hr.ozone_daily_max[io3]]
   	 	    IF (wgulf_hu_count GT 0) THEN $
   	 	        o3_wgulf_hu = [o3_wgulf_hu, o3_8hr.ozone_daily_max[io3]]
   	 	    IF (egulf_td_count GT 0) THEN $
   	 	        o3_egulf_td = [o3_egulf_td, o3_8hr.ozone_daily_max[io3]]
   	 	    IF (egulf_ts_count GT 0) THEN $
   	 	        o3_egulf_ts = [o3_egulf_ts, o3_8hr.ozone_daily_max[io3]]
   	 	    IF (egulf_hu_count GT 0) THEN $
   	 	        o3_egulf_hu = [o3_egulf_hu, o3_8hr.ozone_daily_max[io3]]
   	 	   ; IF (atlantic_td_count GT 0) THEN $
   	 	   ;     o3_atlantic_td = [o3_atlantic_td, o3_8hr.ozone_daily_max[io3]]
   	 	   ; IF (atlantic_ts_count GT 0) THEN $
   	 	   ;     o3_atlantic_ts = [o3_atlantic_ts, o3_8hr.ozone_daily_max[io3]]
   	 	   ; IF (atlantic_hu_count GT 0) THEN $
   	 	   ;     o3_atlantic_hu = [o3_atlantic_hu, o3_8hr.ozone_daily_max[io3]]
   	 	    IF (ac_td_count GT 0) THEN $
   	 	        o3_ac_td = [o3_ac_td, o3_8hr.ozone_daily_max[io3]]
   	 	    IF (ac_ts_count GT 0) THEN $
   	 	        o3_ac_ts = [o3_ac_ts, o3_8hr.ozone_daily_max[io3]]
   	 	    IF (ac_hu_count GT 0) THEN $
   	 	        o3_ac_hu = [o3_ac_hu, o3_8hr.ozone_daily_max[io3]]
   	 	    IF (atl_td_count GT 0) THEN $
   	 	        o3_atl_td = [o3_atl_td, o3_8hr.ozone_daily_max[io3]]
   	 	    IF (atl_ts_count GT 0) THEN $
   	 	        o3_atl_ts = [o3_atl_ts, o3_8hr.ozone_daily_max[io3]]
   	 	    IF (atl_hu_count GT 0) THEN $
   	 	        o3_atl_hu = [o3_atl_hu, o3_8hr.ozone_daily_max[io3]]
   	 	    IF (landfall_count GT 0) THEN $
   	 	        o3_landfall = [o3_landfall, o3_8hr.ozone_daily_max[io3]]
   	  ENDIF
   	  
   	  IF (KEYWORD_SET(NORMALIZE)) THEN BEGIN
   	   		IF (all_td_count GT 0) THEN $
   	 	    	o3_all_td = [o3_all_td, (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]
   	 	    IF (all_ts_count GT 0) THEN $
   	 	        o3_all_ts = [o3_all_ts, (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]
   	 	    IF (all_hu_count GT 0) THEN $
   	 	        o3_all_hu = [o3_all_hu, (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]

    	    ; IF (all_gulf_count GT 0) THEN $
  	 	    ;    o3_all_gulf = [o3_all_gulf, (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]
   	 	    ;IF (all_atl_count GT 0) THEN $
   	 	    ;    o3_all_atl = [o3_all_atl, (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]
    	     IF (all_egulf_count GT 0) THEN $
  	 	        o3_all_egulf = [o3_all_egulf, (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]
    	     IF (all_wgulf_count GT 0) THEN $
  	 	        o3_all_wgulf = [o3_all_wgulf, (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]
   	 	    IF (all_ac_count GT 0) THEN $
   	 	        o3_all_ac = [o3_all_ac, (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]
   	 	    IF (all_atl_count GT 0) THEN $
   	 	        o3_all_atl = [o3_all_atl, (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]

   	 	    ;IF (gulf_td_count GT 0) THEN $
   	 	    ;    o3_gulf_td = [o3_gulf_td, (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]
   	 	    ;IF (gulf_ts_count GT 0) THEN $
   	 	    ;    o3_gulf_ts = [o3_gulf_ts, (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]
   	 	    ;IF (gulf_hu_count GT 0) THEN $
   	 	    ;    o3_gulf_hu = [o3_gulf_hu, (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]
   	 	    IF (wgulf_td_count GT 0) THEN $
   	 	        o3_wgulf_td = [o3_wgulf_td, (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]
   	 	    IF (wgulf_ts_count GT 0) THEN $
   	 	        o3_wgulf_ts = [o3_wgulf_ts, (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]
   	 	    IF (wgulf_hu_count GT 0) THEN $
   	 	        o3_wgulf_hu = [o3_wgulf_hu, (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]
   	 	    IF (egulf_td_count GT 0) THEN $
   	 	        o3_egulf_td = [o3_egulf_td, (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]
   	 	    IF (egulf_ts_count GT 0) THEN $
   	 	        o3_egulf_ts = [o3_egulf_ts, (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]
   	 	    IF (egulf_hu_count GT 0) THEN $
   	 	        o3_egulf_hu = [o3_egulf_hu, (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]

   	 	    ;IF (atlantic_td_count GT 0) THEN $
   	 	    ;    o3_atlantic_td = [o3_atlantic_td, (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]
   	 	    ;IF (atlantic_ts_count GT 0) THEN $
   	 	    ;    o3_atlantic_ts = [o3_atlantic_ts, (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]
   	 	    ;IF (atlantic_hu_count GT 0) THEN $
   	 	    ;    o3_atlantic_hu = [o3_atlantic_hu, (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]
   	 	    IF (ac_td_count GT 0) THEN $
   	 	        o3_ac_td = [o3_ac_td, (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]
   	 	    IF (ac_ts_count GT 0) THEN $
   	 	        o3_ac_ts = [o3_ac_ts, (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]
   	 	    IF (ac_hu_count GT 0) THEN $
   	 	        o3_ac_hu = [o3_ac_hu, (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]
   	 	    IF (atl_td_count GT 0) THEN $
   	 	        o3_atl_td = [o3_atl_td, (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]
   	 	    IF (atl_ts_count GT 0) THEN $
   	 	        o3_atl_ts = [o3_atl_ts, (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]
   	 	    IF (atl_hu_count GT 0) THEN $
   	 	        o3_atl_hu = [o3_atl_hu, (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]

   	 	    IF (landfall_count GT 0) THEN $
   	 	        o3_landfall = [o3_landfall, (o3_8hr.ozone_daily_max[io3]-o3_dm[mth])]
	   ENDIF   	  

       o3_all_td_arr = [o3_all_td_arr, o3_all_td]
       o3_all_ts_arr = [o3_all_ts_arr, o3_all_ts]
       o3_all_hu_arr = [o3_all_hu_arr, o3_all_hu]
 
       ;o3_all_gulf_arr =[o3_all_gulf_arr,o3_all_gulf]
       ;o3_all_atl_arr = [o3_all_atl_arr ,o3_all_atl]
       o3_all_egulf_arr =[o3_all_egulf_arr,o3_all_egulf]
       o3_all_wgulf_arr =[o3_all_wgulf_arr,o3_all_wgulf]
       o3_all_ac_arr = [o3_all_ac_arr ,o3_all_ac]
       o3_all_atl_arr = [o3_all_atl_arr ,o3_all_atl]
 
       ;o3_gulf_td_arr = [o3_gulf_td_arr ,o3_gulf_td]
       ;o3_gulf_ts_arr = [o3_gulf_ts_arr ,o3_gulf_ts]
       ;o3_gulf_hu_arr = [o3_gulf_hu_arr ,o3_gulf_hu]
       o3_wgulf_td_arr = [o3_wgulf_td_arr ,o3_wgulf_td]
       o3_wgulf_ts_arr = [o3_wgulf_ts_arr ,o3_wgulf_ts]
       o3_wgulf_hu_arr = [o3_wgulf_hu_arr ,o3_wgulf_hu]
       o3_egulf_td_arr = [o3_egulf_td_arr ,o3_egulf_td]
       o3_egulf_ts_arr = [o3_egulf_ts_arr ,o3_egulf_ts]
       o3_egulf_hu_arr = [o3_egulf_hu_arr ,o3_egulf_hu]

       ;o3_atlantic_td_arr = [o3_atlantic_td_arr,o3_atlantic_td]
       ;o3_atlantic_ts_arr = [o3_atlantic_ts_arr,o3_atlantic_ts]
       ;o3_atlantic_hu_arr = [o3_atlantic_hu_arr,o3_atlantic_hu]
       o3_ac_td_arr = [o3_ac_td_arr,o3_ac_td]
       o3_ac_ts_arr = [o3_ac_ts_arr,o3_ac_ts]
       o3_ac_hu_arr = [o3_ac_hu_arr,o3_ac_hu]
       o3_atl_td_arr = [o3_atl_td_arr,o3_atl_td]
       o3_atl_ts_arr = [o3_atl_ts_arr,o3_atl_ts]
       o3_atl_hu_arr = [o3_atl_hu_arr,o3_atl_hu]

       o3_landfall_arr = [o3_landfall_arr,o3_landfall]
	
   ENDFOR  	;loop over hours
	o3_allstorms_yr =[o3_allstorms_yr , o3_allstorms_arr]
	o3_nostorms_yr  =[o3_nostorms_yr  , o3_nostorms_arr ]
    o3_all_td_yr = [ o3_all_td_yr,  o3_all_td_arr]
    o3_all_ts_yr = [ o3_all_ts_yr,  o3_all_ts_arr]
    o3_all_hu_yr = [ o3_all_hu_yr,  o3_all_hu_arr]

    ;o3_all_gulf_yr = [o3_all_gulf_yr, o3_all_gulf_arr]
    ;o3_all_atl_yr = [o3_all_atl_yr, o3_all_atl_arr]
    o3_all_egulf_yr = [o3_all_egulf_yr, o3_all_egulf_arr]
    o3_all_wgulf_yr = [o3_all_wgulf_yr, o3_all_wgulf_arr]
    o3_all_ac_yr = [o3_all_ac_yr, o3_all_ac_arr]
    o3_all_atl_yr = [o3_all_atl_yr, o3_all_atl_arr]

    ;o3_gulf_td_yr = [o3_gulf_td_yr, o3_gulf_td_arr]
    ;o3_gulf_ts_yr = [o3_gulf_ts_yr, o3_gulf_ts_arr]
    ;o3_gulf_hu_yr = [o3_gulf_hu_yr, o3_gulf_hu_arr]
    o3_wgulf_td_yr = [o3_wgulf_td_yr, o3_wgulf_td_arr]
    o3_wgulf_ts_yr = [o3_wgulf_ts_yr, o3_wgulf_ts_arr]
    o3_wgulf_hu_yr = [o3_wgulf_hu_yr, o3_wgulf_hu_arr]
    o3_egulf_td_yr = [o3_egulf_td_yr, o3_egulf_td_arr]
    o3_egulf_ts_yr = [o3_egulf_ts_yr, o3_egulf_ts_arr]
    o3_egulf_hu_yr = [o3_egulf_hu_yr, o3_egulf_hu_arr]

    ;o3_atlantic_td_yr = [o3_atlantic_td_yr, o3_atlantic_td_arr]
    ;o3_atlantic_ts_yr = [o3_atlantic_ts_yr, o3_atlantic_ts_arr]
    ;o3_atlantic_hu_yr = [o3_atlantic_hu_yr, o3_atlantic_hu_arr]
    o3_ac_td_yr = [o3_ac_td_yr, o3_ac_td_arr]
    o3_ac_ts_yr = [o3_ac_ts_yr, o3_ac_ts_arr]
    o3_ac_hu_yr = [o3_ac_hu_yr, o3_ac_hu_arr]
    o3_atl_td_yr = [o3_atl_td_yr, o3_atl_td_arr]
    o3_atl_ts_yr = [o3_atl_ts_yr, o3_atl_ts_arr]
    o3_atl_hu_yr = [o3_atl_hu_yr, o3_atl_hu_arr]
    o3_landfall_yr = [o3_landfall_yr, o3_landfall_arr]
ENDFOREACH

o3_storm_sort  = o3_allstorms_yr[SORT(o3_allstorms_yr) ]
o3_nstorm_sort = o3_nostorms_yr [SORT(o3_nostorms_yr)]
o3_all_td_sort =  o3_all_td_yr[SORT(o3_all_td_yr)]
o3_all_ts_sort =  o3_all_ts_yr[SORT(o3_all_ts_yr)]
o3_all_hu_sort =  o3_all_hu_yr[SORT(o3_all_hu_yr)]

;o3_all_gulf_sort = o3_all_gulf_yr[SORT(o3_all_gulf_yr)]
;o3_all_atl_sort = o3_all_atl_yr[SORT(o3_all_atl_yr)]
o3_all_egulf_sort = o3_all_egulf_yr[SORT(o3_all_egulf_yr)]
o3_all_wgulf_sort = o3_all_wgulf_yr[SORT(o3_all_wgulf_yr)]
o3_all_ac_sort = o3_all_ac_yr[SORT(o3_all_ac_yr)]
o3_all_atl_sort = o3_all_atl_yr[SORT(o3_all_atl_yr)]

;o3_gulf_td_sort = o3_gulf_td_yr[SORT(o3_gulf_td_yr)]
;o3_gulf_ts_sort = o3_gulf_ts_yr[SORT(o3_gulf_ts_yr)]
;o3_gulf_hu_sort = o3_gulf_hu_yr[SORT(o3_gulf_hu_yr)]
o3_wgulf_td_sort = o3_wgulf_td_yr[SORT(o3_wgulf_td_yr)]
o3_wgulf_ts_sort = o3_wgulf_ts_yr[SORT(o3_wgulf_ts_yr)]
o3_wgulf_hu_sort = o3_wgulf_hu_yr[SORT(o3_wgulf_hu_yr)]
o3_egulf_td_sort = o3_egulf_td_yr[SORT(o3_egulf_td_yr)]
o3_egulf_ts_sort = o3_egulf_ts_yr[SORT(o3_egulf_ts_yr)]
o3_egulf_hu_sort = o3_egulf_hu_yr[SORT(o3_egulf_hu_yr)]

;o3_atlantic_td_sort = o3_atlantic_td_yr[SORT(o3_atlantic_td_yr)]
;o3_atlantic_ts_sort = o3_atlantic_ts_yr[SORT(o3_atlantic_ts_yr)]
;o3_atlantic_hu_sort = o3_atlantic_hu_yr[SORT(o3_atlantic_hu_yr)]
o3_ac_td_sort = o3_ac_td_yr[SORT(o3_ac_td_yr)]
o3_ac_ts_sort = o3_ac_ts_yr[SORT(o3_ac_ts_yr)]
o3_ac_hu_sort = o3_ac_hu_yr[SORT(o3_ac_hu_yr)]
o3_atl_td_sort = o3_atl_td_yr[SORT(o3_atl_td_yr)]
o3_atl_ts_sort = o3_atl_ts_yr[SORT(o3_atl_ts_yr)]
o3_atl_hu_sort = o3_atl_hu_yr[SORT(o3_atl_hu_yr)]

o3_landfall_sort = o3_landfall_yr[SORT(o3_landfall_yr)]

o3box=[]
no3box=[]
o3box1 = []
o3box2 = []

;+storms
IF (N_ELEMENTS(o3_storm_sort) MOD 2 EQ 0) THEN BEGIN
	No3_storm_sort 	= N_ELEMENTS(o3_storm_sort)
	o3_med = (o3_storm_sort[(No3_storm_sort/2)-1] + o3_storm_sort[(No3_storm_sort/2)]) / 2.0
	lower_half = o3_storm_sort[0:(No3_storm_sort/2)-1]
	upper_half = o3_storm_sort[(No3_storm_sort/2):(No3_storm_sort-1)]
ENDIF ELSE BEGIN
	No3_storm_sort 	= N_ELEMENTS(o3_storm_sort)
	o3_med = o3_storm_sort[(No3_storm_sort/2)] 
	lower_half = o3_storm_sort[0:(No3_storm_sort/2)-1]
	upper_half = o3_storm_sort[(No3_storm_sort/2):(No3_storm_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_storm_sort[0.05*No3_storm_sort]
quartile_95 = o3_storm_sort[0.95*No3_storm_sort]

o3ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]
PRINT, 'all storms= ' + STRING(o3ptile[0]) + STRING(o3ptile[1]) + STRING(o3ptile[2]) + $
	STRING(o3ptile[3]) + STRING(o3ptile[4])
o3box = [[o3box],[o3ptile]]
o3box1 = [[o3box1],[o3ptile]]
o3box2 = [[o3box2],[o3ptile]]

;all td storms
IF (N_ELEMENTS(o3_all_td_sort) MOD 2 EQ 0) THEN BEGIN
	No3_all_td_sort 	= N_ELEMENTS(o3_all_td_sort)
	o3_med = (o3_all_td_sort[(No3_all_td_sort/2)-1] + o3_all_td_sort[(No3_all_td_sort/2)]) / 2.0
	lower_half = o3_all_td_sort[0:(No3_all_td_sort/2)-1]
	upper_half = o3_all_td_sort[(No3_all_td_sort/2):(No3_all_td_sort-1)]
ENDIF ELSE BEGIN
	No3_all_td_sort 	= N_ELEMENTS(o3_all_td_sort)
	o3_med = o3_all_td_sort[(No3_all_td_sort/2)] 
	lower_half = o3_all_td_sort[0:(No3_all_td_sort/2)-1]
	upper_half = o3_all_td_sort[(No3_all_td_sort/2):(No3_all_td_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_all_td_sort[0.05*No3_all_td_sort]
quartile_95 = o3_all_td_sort[0.95*No3_all_td_sort]

o3ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]
PRINT, 'all td= ' + STRING(o3ptile[0]) + STRING(o3ptile[1]) + STRING(o3ptile[2]) + $
	STRING(o3ptile[3]) + STRING(o3ptile[4])

o3box = [[o3box],[o3ptile]]
o3box1 = [[o3box1],[o3ptile]]

;all ts storms
IF (N_ELEMENTS(o3_all_ts_sort) MOD 2 EQ 0) THEN BEGIN
	No3_all_ts_sort 	= N_ELEMENTS(o3_all_ts_sort)
	o3_med = (o3_all_ts_sort[(No3_all_ts_sort/2)-1] + o3_all_ts_sort[(No3_all_ts_sort/2)]) / 2.0
	lower_half = o3_all_ts_sort[0:(No3_all_ts_sort/2)-1]
	upper_half = o3_all_ts_sort[(No3_all_ts_sort/2):(No3_all_ts_sort-1)]
ENDIF ELSE BEGIN
	No3_all_ts_sort 	= N_ELEMENTS(o3_all_ts_sort)
	o3_med = o3_all_ts_sort[(No3_all_ts_sort/2)] 
	lower_half = o3_all_ts_sort[0:(No3_all_ts_sort/2)-1]
	upper_half = o3_all_ts_sort[(No3_all_ts_sort/2):(No3_all_ts_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_all_ts_sort[0.05*No3_all_ts_sort]
quartile_95 = o3_all_ts_sort[0.95*No3_all_ts_sort]

o3ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]
PRINT, 'all ts= ' + STRING(o3ptile[0]) + STRING(o3ptile[1]) + STRING(o3ptile[2]) + $
	STRING(o3ptile[3]) + STRING(o3ptile[4])

o3box = [[o3box],[o3ptile]]
o3box1 = [[o3box1],[o3ptile]]

;all hu storms
IF (N_ELEMENTS(o3_all_hu_sort) MOD 2 EQ 0) THEN BEGIN
	No3_all_hu_sort 	= N_ELEMENTS(o3_all_hu_sort)
	o3_med = (o3_all_hu_sort[(No3_all_hu_sort/2)-1] + o3_all_hu_sort[(No3_all_hu_sort/2)]) / 2.0
	lower_half = o3_all_hu_sort[0:(No3_all_hu_sort/2)-1]
	upper_half = o3_all_hu_sort[(No3_all_hu_sort/2):(No3_all_hu_sort-1)]
ENDIF ELSE BEGIN
	No3_all_hu_sort 	= N_ELEMENTS(o3_all_hu_sort)
	o3_med = o3_all_hu_sort[(No3_all_hu_sort/2)] 
	lower_half = o3_all_hu_sort[0:(No3_all_hu_sort/2)-1]
	upper_half = o3_all_hu_sort[(No3_all_hu_sort/2):(No3_all_hu_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_all_hu_sort[0.05*No3_all_hu_sort]
quartile_95 = o3_all_hu_sort[0.95*No3_all_hu_sort]

o3ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]
PRINT, 'all hu= ' + STRING(o3ptile[0]) + STRING(o3ptile[1]) + STRING(o3ptile[2]) + $
	STRING(o3ptile[3]) + STRING(o3ptile[4])

o3box = [[o3box],[o3ptile]]
o3box1 = [[o3box1],[o3ptile]]

;;all gulf storms
;IF (N_ELEMENTS(o3_all_gulf_sort) MOD 2 EQ 0) THEN BEGIN
;	No3_all_gulf_sort 	= N_ELEMENTS(o3_all_gulf_sort)
;	o3_med = (o3_all_gulf_sort[(No3_all_gulf_sort/2)-1] + o3_all_gulf_sort[(No3_all_gulf_sort/2)]) / 2.0
;	lower_half = o3_all_gulf_sort[0:(No3_all_gulf_sort/2)-1]
;	upper_half = o3_all_gulf_sort[(No3_all_gulf_sort/2):(No3_all_gulf_sort-1)]
;ENDIF ELSE BEGIN
;	No3_all_gulf_sort 	= N_ELEMENTS(o3_all_gulf_sort)
;	o3_med = o3_all_gulf_sort[(No3_all_gulf_sort/2)] 
;	lower_half = o3_all_gulf_sort[0:(No3_all_gulf_sort/2)-1]
;	upper_half = o3_all_gulf_sort[(No3_all_gulf_sort/2):(No3_all_gulf_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = o3_all_gulf_sort[0.05*No3_all_gulf_sort]
;quartile_95 = o3_all_gulf_sort[0.95*No3_all_gulf_sort]
;
;o3ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]
;PRINT, 'all gulf= ' + STRING(o3ptile[0]) + STRING(o3ptile[1]) + STRING(o3ptile[2]) + $
;	STRING(o3ptile[3]) + STRING(o3ptile[4])
;
;o3box = [[o3box],[o3ptile]]
;
;;all atlantic storms
;IF (N_ELEMENTS(o3_all_atl_sort) MOD 2 EQ 0) THEN BEGIN
;	No3_all_atl_sort 	= N_ELEMENTS(o3_all_atl_sort)
;	o3_med = (o3_all_atl_sort[(No3_all_atl_sort/2)-1] + o3_all_atl_sort[(No3_all_atl_sort/2)]) / 2.0
;	lower_half = o3_all_atl_sort[0:(No3_all_atl_sort/2)-1]
;	upper_half = o3_all_atl_sort[(No3_all_atl_sort/2):(No3_all_atl_sort-1)]
;ENDIF ELSE BEGIN
;	No3_all_atl_sort 	= N_ELEMENTS(o3_all_atl_sort)
;	o3_med = o3_all_atl_sort[(No3_all_atl_sort/2)] 
;	lower_half = o3_all_atl_sort[0:(No3_all_atl_sort/2)-1]
;	upper_half = o3_all_atl_sort[(No3_all_atl_sort/2):(No3_all_atl_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = o3_all_atl_sort[0.05*No3_all_atl_sort]
;quartile_95 = o3_all_atl_sort[0.95*No3_all_atl_sort]
;
;o3ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]
;PRINT, 'all atlantic= ' + STRING(o3ptile[0]) + STRING(o3ptile[1]) + STRING(o3ptile[2]) + $
;	STRING(o3ptile[3]) + STRING(o3ptile[4])
;o3box = [[o3box],[o3ptile]]

;all egulf storms
IF (N_ELEMENTS(o3_all_egulf_sort) MOD 2 EQ 0) THEN BEGIN
	No3_all_egulf_sort 	= N_ELEMENTS(o3_all_egulf_sort)
	o3_med = (o3_all_egulf_sort[(No3_all_egulf_sort/2)-1] + o3_all_egulf_sort[(No3_all_egulf_sort/2)]) / 2.0
	lower_half = o3_all_egulf_sort[0:(No3_all_egulf_sort/2)-1]
	upper_half = o3_all_egulf_sort[(No3_all_egulf_sort/2):(No3_all_egulf_sort-1)]
ENDIF ELSE BEGIN
	No3_all_egulf_sort 	= N_ELEMENTS(o3_all_egulf_sort)
	o3_med = o3_all_egulf_sort[(No3_all_egulf_sort/2)] 
	lower_half = o3_all_egulf_sort[0:(No3_all_egulf_sort/2)-1]
	upper_half = o3_all_egulf_sort[(No3_all_egulf_sort/2):(No3_all_egulf_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_all_egulf_sort[0.05*No3_all_egulf_sort]
quartile_95 = o3_all_egulf_sort[0.95*No3_all_egulf_sort]

o3ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]
PRINT, 'all egulf= ' + STRING(o3ptile[0]) + STRING(o3ptile[1]) + STRING(o3ptile[2]) + $
	STRING(o3ptile[3]) + STRING(o3ptile[4])

o3box = [[o3box],[o3ptile]]
o3box1 = [[o3box1],[o3ptile]]

;all wgulf storms
IF (N_ELEMENTS(o3_all_wgulf_sort) MOD 2 EQ 0) THEN BEGIN
	No3_all_wgulf_sort 	= N_ELEMENTS(o3_all_wgulf_sort)
	o3_med = (o3_all_wgulf_sort[(No3_all_wgulf_sort/2)-1] + o3_all_wgulf_sort[(No3_all_wgulf_sort/2)]) / 2.0
	lower_half = o3_all_wgulf_sort[0:(No3_all_wgulf_sort/2)-1]
	upper_half = o3_all_wgulf_sort[(No3_all_wgulf_sort/2):(No3_all_wgulf_sort-1)]
ENDIF ELSE BEGIN
	No3_all_wgulf_sort 	= N_ELEMENTS(o3_all_wgulf_sort)
	o3_med = o3_all_wgulf_sort[(No3_all_wgulf_sort/2)] 
	lower_half = o3_all_wgulf_sort[0:(No3_all_wgulf_sort/2)-1]
	upper_half = o3_all_wgulf_sort[(No3_all_wgulf_sort/2):(No3_all_wgulf_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_all_wgulf_sort[0.05*No3_all_wgulf_sort]
quartile_95 = o3_all_wgulf_sort[0.95*No3_all_wgulf_sort]

o3ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]
PRINT, 'all wgulf= ' + STRING(o3ptile[0]) + STRING(o3ptile[1]) + STRING(o3ptile[2]) + $
	STRING(o3ptile[3]) + STRING(o3ptile[4])

o3box = [[o3box],[o3ptile]]
o3box1 = [[o3box1],[o3ptile]]

;all atlantic coast storms
IF (N_ELEMENTS(o3_all_ac_sort) MOD 2 EQ 0) THEN BEGIN
	No3_all_ac_sort 	= N_ELEMENTS(o3_all_ac_sort)
	o3_med = (o3_all_ac_sort[(No3_all_ac_sort/2)-1] + o3_all_ac_sort[(No3_all_ac_sort/2)]) / 2.0
	lower_half = o3_all_ac_sort[0:(No3_all_ac_sort/2)-1]
	upper_half = o3_all_ac_sort[(No3_all_ac_sort/2):(No3_all_ac_sort-1)]
ENDIF ELSE BEGIN
	No3_all_ac_sort 	= N_ELEMENTS(o3_all_ac_sort)
	o3_med = o3_all_ac_sort[(No3_all_ac_sort/2)] 
	lower_half = o3_all_ac_sort[0:(No3_all_ac_sort/2)-1]
	upper_half = o3_all_ac_sort[(No3_all_ac_sort/2):(No3_all_ac_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_all_ac_sort[0.05*No3_all_ac_sort]
quartile_95 = o3_all_ac_sort[0.95*No3_all_ac_sort]

o3ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]
PRINT, 'all atlantic coast= ' + STRING(o3ptile[0]) + STRING(o3ptile[1]) + STRING(o3ptile[2]) + $
	STRING(o3ptile[3]) + STRING(o3ptile[4])

o3box = [[o3box],[o3ptile]]
o3box1 = [[o3box1],[o3ptile]]

;all far atlantic storms
IF (N_ELEMENTS(o3_all_atl_sort) MOD 2 EQ 0) THEN BEGIN
	No3_all_atl_sort 	= N_ELEMENTS(o3_all_atl_sort)
	o3_med = (o3_all_atl_sort[(No3_all_atl_sort/2)-1] + o3_all_atl_sort[(No3_all_atl_sort/2)]) / 2.0
	lower_half = o3_all_atl_sort[0:(No3_all_atl_sort/2)-1]
	upper_half = o3_all_atl_sort[(No3_all_atl_sort/2):(No3_all_atl_sort-1)]
ENDIF ELSE BEGIN
	No3_all_atl_sort 	= N_ELEMENTS(o3_all_atl_sort)
	o3_med = o3_all_atl_sort[(No3_all_atl_sort/2)] 
	lower_half = o3_all_atl_sort[0:(No3_all_atl_sort/2)-1]
	upper_half = o3_all_atl_sort[(No3_all_atl_sort/2):(No3_all_atl_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_all_atl_sort[0.05*No3_all_atl_sort]
quartile_95 = o3_all_atl_sort[0.95*No3_all_atl_sort]

o3ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]
PRINT, 'all far atlantic= ' + STRING(o3ptile[0]) + STRING(o3ptile[1]) + STRING(o3ptile[2]) + $
	STRING(o3ptile[3]) + STRING(o3ptile[4])

o3box = [[o3box],[o3ptile]]
o3box1 = [[o3box1],[o3ptile]]

;;gulf td storms
;IF (N_ELEMENTS(o3_gulf_td_sort) MOD 2 EQ 0) THEN BEGIN
;	No3_gulf_td_sort 	= N_ELEMENTS(o3_gulf_td_sort)
;	o3_med = (o3_gulf_td_sort[(No3_gulf_td_sort/2)-1] + o3_gulf_td_sort[(No3_gulf_td_sort/2)]) / 2.0
;	lower_half = o3_gulf_td_sort[0:(No3_gulf_td_sort/2)-1]
;	upper_half = o3_gulf_td_sort[(No3_gulf_td_sort/2):(No3_gulf_td_sort-1)]
;ENDIF ELSE BEGIN
;	No3_gulf_td_sort 	= N_ELEMENTS(o3_gulf_td_sort)
;	o3_med = o3_gulf_td_sort[(No3_gulf_td_sort/2)] 
;	lower_half = o3_gulf_td_sort[0:(No3_gulf_td_sort/2)-1]
;	upper_half = o3_gulf_td_sort[(No3_gulf_td_sort/2):(No3_gulf_td_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = o3_gulf_td_sort[0.05*No3_gulf_td_sort]
;quartile_95 = o3_gulf_td_sort[0.95*No3_gulf_td_sort]
;
;o3ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]
;PRINT, 'gulf td= ' + STRING(o3ptile[0]) + STRING(o3ptile[1]) + STRING(o3ptile[2]) + $
;	STRING(o3ptile[3]) + STRING(o3ptile[4])
;
;o3box = [[o3box],[o3ptile]]
;
;;gulf ts storms
;IF (N_ELEMENTS(o3_gulf_ts_sort) MOD 2 EQ 0) THEN BEGIN
;	No3_gulf_ts_sort 	= N_ELEMENTS(o3_gulf_ts_sort)
;	o3_med = (o3_gulf_ts_sort[(No3_gulf_ts_sort/2)-1] + o3_gulf_ts_sort[(No3_gulf_ts_sort/2)]) / 2.0
;	lower_half = o3_gulf_ts_sort[0:(No3_gulf_ts_sort/2)-1]
;	upper_half = o3_gulf_ts_sort[(No3_gulf_ts_sort/2):(No3_gulf_ts_sort-1)]
;ENDIF ELSE BEGIN
;	No3_gulf_ts_sort 	= N_ELEMENTS(o3_gulf_ts_sort)
;	o3_med = o3_gulf_ts_sort[(No3_gulf_ts_sort/2)] 
;	lower_half = o3_gulf_ts_sort[0:(No3_gulf_ts_sort/2)-1]
;	upper_half = o3_gulf_ts_sort[(No3_gulf_ts_sort/2):(No3_gulf_ts_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = o3_gulf_ts_sort[0.05*No3_gulf_ts_sort]
;quartile_95 = o3_gulf_ts_sort[0.95*No3_gulf_ts_sort]
;
;o3ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]
;PRINT, 'gulf ts= ' + STRING(o3ptile[0]) + STRING(o3ptile[1]) + STRING(o3ptile[2]) + $
;	STRING(o3ptile[3]) + STRING(o3ptile[4])
;
;o3box = [[o3box],[o3ptile]]
;
;;gulf hu storms
;IF (N_ELEMENTS(o3_gulf_hu_sort) MOD 2 EQ 0) THEN BEGIN
;	No3_gulf_hu_sort 	= N_ELEMENTS(o3_gulf_hu_sort)
;	o3_med = (o3_gulf_hu_sort[(No3_gulf_hu_sort/2)-1] + o3_gulf_hu_sort[(No3_gulf_hu_sort/2)]) / 2.0
;	lower_half = o3_gulf_hu_sort[0:(No3_gulf_hu_sort/2)-1]
;	upper_half = o3_gulf_hu_sort[(No3_gulf_hu_sort/2):(No3_gulf_hu_sort-1)]
;ENDIF ELSE BEGIN
;	No3_gulf_hu_sort 	= N_ELEMENTS(o3_gulf_hu_sort)
;	o3_med = o3_gulf_hu_sort[(No3_gulf_hu_sort/2)] 
;	lower_half = o3_gulf_hu_sort[0:(No3_gulf_hu_sort/2)-1]
;	upper_half = o3_gulf_hu_sort[(No3_gulf_hu_sort/2):(No3_gulf_hu_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = o3_gulf_hu_sort[0.05*No3_gulf_hu_sort]
;quartile_95 = o3_gulf_hu_sort[0.95*No3_gulf_hu_sort]
;
;o3ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]
;PRINT, 'gulf hu= ' + STRING(o3ptile[0]) + STRING(o3ptile[1]) + STRING(o3ptile[2]) + $
;	STRING(o3ptile[3]) + STRING(o3ptile[4])
;
;o3box = [[o3box],[o3ptile]]

;wgulf td storms
IF (N_ELEMENTS(o3_wgulf_td_sort) MOD 2 EQ 0) THEN BEGIN
	No3_wgulf_td_sort 	= N_ELEMENTS(o3_wgulf_td_sort)
	o3_med = (o3_wgulf_td_sort[(No3_wgulf_td_sort/2)-1] + o3_wgulf_td_sort[(No3_wgulf_td_sort/2)]) / 2.0
	lower_half = o3_wgulf_td_sort[0:(No3_wgulf_td_sort/2)-1]
	upper_half = o3_wgulf_td_sort[(No3_wgulf_td_sort/2):(No3_wgulf_td_sort-1)]
ENDIF ELSE BEGIN
	No3_wgulf_td_sort 	= N_ELEMENTS(o3_wgulf_td_sort)
	o3_med = o3_wgulf_td_sort[(No3_wgulf_td_sort/2)] 
	lower_half = o3_wgulf_td_sort[0:(No3_wgulf_td_sort/2)-1]
	upper_half = o3_wgulf_td_sort[(No3_wgulf_td_sort/2):(No3_wgulf_td_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_wgulf_td_sort[0.05*No3_wgulf_td_sort]
quartile_95 = o3_wgulf_td_sort[0.95*No3_wgulf_td_sort]

o3ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]
PRINT, 'wgulf td= ' + STRING(o3ptile[0]) + STRING(o3ptile[1]) + STRING(o3ptile[2]) + $
	STRING(o3ptile[3]) + STRING(o3ptile[4])

o3box = [[o3box],[o3ptile]]
o3box2 = [[o3box2],[o3ptile]]

;wgulf ts storms
IF (N_ELEMENTS(o3_wgulf_ts_sort) MOD 2 EQ 0) THEN BEGIN
	No3_wgulf_ts_sort 	= N_ELEMENTS(o3_wgulf_ts_sort)
	o3_med = (o3_wgulf_ts_sort[(No3_wgulf_ts_sort/2)-1] + o3_wgulf_ts_sort[(No3_wgulf_ts_sort/2)]) / 2.0
	lower_half = o3_wgulf_ts_sort[0:(No3_wgulf_ts_sort/2)-1]
	upper_half = o3_wgulf_ts_sort[(No3_wgulf_ts_sort/2):(No3_wgulf_ts_sort-1)]
ENDIF ELSE BEGIN
	No3_wgulf_ts_sort 	= N_ELEMENTS(o3_wgulf_ts_sort)
	o3_med = o3_wgulf_ts_sort[(No3_wgulf_ts_sort/2)] 
	lower_half = o3_wgulf_ts_sort[0:(No3_wgulf_ts_sort/2)-1]
	upper_half = o3_wgulf_ts_sort[(No3_wgulf_ts_sort/2):(No3_wgulf_ts_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_wgulf_ts_sort[0.05*No3_wgulf_ts_sort]
quartile_95 = o3_wgulf_ts_sort[0.95*No3_wgulf_ts_sort]

o3ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]
PRINT, 'wgulf ts= ' + STRING(o3ptile[0]) + STRING(o3ptile[1]) + STRING(o3ptile[2]) + $
	STRING(o3ptile[3]) + STRING(o3ptile[4])

o3box = [[o3box],[o3ptile]]
o3box2 = [[o3box2],[o3ptile]]

;wgulf hu storms
IF (N_ELEMENTS(o3_wgulf_hu_sort) MOD 2 EQ 0) THEN BEGIN
	No3_wgulf_hu_sort 	= N_ELEMENTS(o3_wgulf_hu_sort)
	o3_med = (o3_wgulf_hu_sort[(No3_wgulf_hu_sort/2)-1] + o3_wgulf_hu_sort[(No3_wgulf_hu_sort/2)]) / 2.0
	lower_half = o3_wgulf_hu_sort[0:(No3_wgulf_hu_sort/2)-1]
	upper_half = o3_wgulf_hu_sort[(No3_wgulf_hu_sort/2):(No3_wgulf_hu_sort-1)]
ENDIF ELSE BEGIN
	No3_wgulf_hu_sort 	= N_ELEMENTS(o3_wgulf_hu_sort)
	o3_med = o3_wgulf_hu_sort[(No3_wgulf_hu_sort/2)] 
	lower_half = o3_wgulf_hu_sort[0:(No3_wgulf_hu_sort/2)-1]
	upper_half = o3_wgulf_hu_sort[(No3_wgulf_hu_sort/2):(No3_wgulf_hu_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_wgulf_hu_sort[0.05*No3_wgulf_hu_sort]
quartile_95 = o3_wgulf_hu_sort[0.95*No3_wgulf_hu_sort]

o3ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]
PRINT, 'wgulf hu= ' + STRING(o3ptile[0]) + STRING(o3ptile[1]) + STRING(o3ptile[2]) + $
	STRING(o3ptile[3]) + STRING(o3ptile[4])

o3box = [[o3box],[o3ptile]]
o3box2 = [[o3box2],[o3ptile]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End W-Gulf section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;egulf td storms
IF (N_ELEMENTS(o3_egulf_td_sort) MOD 2 EQ 0) THEN BEGIN
	No3_egulf_td_sort 	= N_ELEMENTS(o3_egulf_td_sort)
	o3_med = (o3_egulf_td_sort[(No3_egulf_td_sort/2)-1] + o3_egulf_td_sort[(No3_egulf_td_sort/2)]) / 2.0
	lower_half = o3_egulf_td_sort[0:(No3_egulf_td_sort/2)-1]
	upper_half = o3_egulf_td_sort[(No3_egulf_td_sort/2):(No3_egulf_td_sort-1)]
ENDIF ELSE BEGIN
	No3_egulf_td_sort 	= N_ELEMENTS(o3_egulf_td_sort)
	o3_med = o3_egulf_td_sort[(No3_egulf_td_sort/2)] 
	lower_half = o3_egulf_td_sort[0:(No3_egulf_td_sort/2)-1]
	upper_half = o3_egulf_td_sort[(No3_egulf_td_sort/2):(No3_egulf_td_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_egulf_td_sort[0.05*No3_egulf_td_sort]
quartile_95 = o3_egulf_td_sort[0.95*No3_egulf_td_sort]

o3ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]
PRINT, 'egulf td= ' + STRING(o3ptile[0]) + STRING(o3ptile[1]) + STRING(o3ptile[2]) + $
	STRING(o3ptile[3]) + STRING(o3ptile[4])

o3box = [[o3box],[o3ptile]]
o3box2 = [[o3box2],[o3ptile]]

;egulf ts storms
IF (N_ELEMENTS(o3_egulf_ts_sort) MOD 2 EQ 0) THEN BEGIN
	No3_egulf_ts_sort 	= N_ELEMENTS(o3_egulf_ts_sort)
	o3_med = (o3_egulf_ts_sort[(No3_egulf_ts_sort/2)-1] + o3_egulf_ts_sort[(No3_egulf_ts_sort/2)]) / 2.0
	lower_half = o3_egulf_ts_sort[0:(No3_egulf_ts_sort/2)-1]
	upper_half = o3_egulf_ts_sort[(No3_egulf_ts_sort/2):(No3_egulf_ts_sort-1)]
ENDIF ELSE BEGIN
	No3_egulf_ts_sort 	= N_ELEMENTS(o3_egulf_ts_sort)
	o3_med = o3_egulf_ts_sort[(No3_egulf_ts_sort/2)] 
	lower_half = o3_egulf_ts_sort[0:(No3_egulf_ts_sort/2)-1]
	upper_half = o3_egulf_ts_sort[(No3_egulf_ts_sort/2):(No3_egulf_ts_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_egulf_ts_sort[0.05*No3_egulf_ts_sort]
quartile_95 = o3_egulf_ts_sort[0.95*No3_egulf_ts_sort]

o3ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]
PRINT, 'egulf ts= ' + STRING(o3ptile[0]) + STRING(o3ptile[1]) + STRING(o3ptile[2]) + $
	STRING(o3ptile[3]) + STRING(o3ptile[4])

o3box = [[o3box],[o3ptile]]
o3box2 = [[o3box2],[o3ptile]]

;egulf hu storms
IF (N_ELEMENTS(o3_egulf_hu_sort) MOD 2 EQ 0) THEN BEGIN
	No3_egulf_hu_sort 	= N_ELEMENTS(o3_egulf_hu_sort)
	o3_med = (o3_egulf_hu_sort[(No3_egulf_hu_sort/2)-1] + o3_egulf_hu_sort[(No3_egulf_hu_sort/2)]) / 2.0
	lower_half = o3_egulf_hu_sort[0:(No3_egulf_hu_sort/2)-1]
	upper_half = o3_egulf_hu_sort[(No3_egulf_hu_sort/2):(No3_egulf_hu_sort-1)]
ENDIF ELSE BEGIN
	No3_egulf_hu_sort 	= N_ELEMENTS(o3_egulf_hu_sort)
	o3_med = o3_egulf_hu_sort[(No3_egulf_hu_sort/2)] 
	lower_half = o3_egulf_hu_sort[0:(No3_egulf_hu_sort/2)-1]
	upper_half = o3_egulf_hu_sort[(No3_egulf_hu_sort/2):(No3_egulf_hu_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_egulf_hu_sort[0.05*No3_egulf_hu_sort]
quartile_95 = o3_egulf_hu_sort[0.95*No3_egulf_hu_sort]

o3ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]
PRINT, 'egulf hu= ' + STRING(o3ptile[0]) + STRING(o3ptile[1]) + STRING(o3ptile[2]) + $
	STRING(o3ptile[3]) + STRING(o3ptile[4])

o3box = [[o3box],[o3ptile]]
o3box2 = [[o3box2],[o3ptile]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End E-Gulf Section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;atlantic td storms
;IF (N_ELEMENTS(o3_atlantic_td_sort) MOD 2 EQ 0) THEN BEGIN
;	No3_atlantic_td_sort 	= N_ELEMENTS(o3_atlantic_td_sort)
;	o3_med = (o3_atlantic_td_sort[(No3_atlantic_td_sort/2)-1] + o3_atlantic_td_sort[(No3_atlantic_td_sort/2)]) / 2.0
;	lower_half = o3_atlantic_td_sort[0:(No3_atlantic_td_sort/2)-1]
;	upper_half = o3_atlantic_td_sort[(No3_atlantic_td_sort/2):(No3_atlantic_td_sort-1)]
;ENDIF ELSE BEGIN
;	No3_atlantic_td_sort 	= N_ELEMENTS(o3_atlantic_td_sort)
;	o3_med = o3_atlantic_td_sort[(No3_atlantic_td_sort/2)] 
;	lower_half = o3_atlantic_td_sort[0:(No3_atlantic_td_sort/2)-1]
;	upper_half = o3_atlantic_td_sort[(No3_atlantic_td_sort/2):(No3_atlantic_td_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = o3_atlantic_td_sort[0.05*No3_atlantic_td_sort]
;quartile_95 = o3_atlantic_td_sort[0.95*No3_atlantic_td_sort]
;
;o3ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]
;PRINT, 'atl td= ' + STRING(o3ptile[0]) + STRING(o3ptile[1]) + STRING(o3ptile[2]) + $
;	STRING(o3ptile[3]) + STRING(o3ptile[4])
;
;o3box = [[o3box],[o3ptile]]
;
;;atlantic ts storms
;IF (N_ELEMENTS(o3_atlantic_ts_sort) MOD 2 EQ 0) THEN BEGIN
;	No3_atlantic_ts_sort 	= N_ELEMENTS(o3_atlantic_ts_sort)
;	o3_med = (o3_atlantic_ts_sort[(No3_atlantic_ts_sort/2)-1] + o3_atlantic_ts_sort[(No3_atlantic_ts_sort/2)]) / 2.0
;	lower_half = o3_atlantic_ts_sort[0:(No3_atlantic_ts_sort/2)-1]
;	upper_half = o3_atlantic_ts_sort[(No3_atlantic_ts_sort/2):(No3_atlantic_ts_sort-1)]
;ENDIF ELSE BEGIN
;	No3_atlantic_ts_sort 	= N_ELEMENTS(o3_atlantic_ts_sort)
;	o3_med = o3_atlantic_ts_sort[(No3_atlantic_ts_sort/2)] 
;	lower_half = o3_atlantic_ts_sort[0:(No3_atlantic_ts_sort/2)-1]
;	upper_half = o3_atlantic_ts_sort[(No3_atlantic_ts_sort/2):(No3_atlantic_ts_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = o3_atlantic_ts_sort[0.05*No3_atlantic_ts_sort]
;quartile_95 = o3_atlantic_ts_sort[0.95*No3_atlantic_ts_sort]
;
;o3ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]
;PRINT, 'atl ts= ' + STRING(o3ptile[0]) + STRING(o3ptile[1]) + STRING(o3ptile[2]) + $
;	STRING(o3ptile[3]) + STRING(o3ptile[4])
;
;o3box = [[o3box],[o3ptile]]
;
;;atlantic hu storms
;IF (N_ELEMENTS(o3_atlantic_hu_sort) MOD 2 EQ 0) THEN BEGIN
;	No3_atlantic_hu_sort 	= N_ELEMENTS(o3_atlantic_hu_sort)
;	o3_med = (o3_atlantic_hu_sort[(No3_atlantic_hu_sort/2)-1] + o3_atlantic_hu_sort[(No3_atlantic_hu_sort/2)]) / 2.0
;	lower_half = o3_atlantic_hu_sort[0:(No3_atlantic_hu_sort/2)-1]
;	upper_half = o3_atlantic_hu_sort[(No3_atlantic_hu_sort/2):(No3_atlantic_hu_sort-1)]
;ENDIF ELSE BEGIN
;	No3_atlantic_hu_sort 	= N_ELEMENTS(o3_atlantic_hu_sort)
;	o3_med = o3_atlantic_hu_sort[(No3_atlantic_hu_sort/2)] 
;	lower_half = o3_atlantic_hu_sort[0:(No3_atlantic_hu_sort/2)-1]
;	upper_half = o3_atlantic_hu_sort[(No3_atlantic_hu_sort/2):(No3_atlantic_hu_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = o3_atlantic_hu_sort[0.05*No3_atlantic_hu_sort]
;quartile_95 = o3_atlantic_hu_sort[0.95*No3_atlantic_hu_sort]
;
;o3ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]
;PRINT, 'atl hu= ' + STRING(o3ptile[0]) + STRING(o3ptile[1]) + STRING(o3ptile[2]) + $
;	STRING(o3ptile[3]) + STRING(o3ptile[4])
;
;o3box = [[o3box],[o3ptile]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Start Atlantic Coast Storms															 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ac td storms
IF (N_ELEMENTS(o3_ac_td_sort) MOD 2 EQ 0) THEN BEGIN
	No3_ac_td_sort 	= N_ELEMENTS(o3_ac_td_sort)
	o3_med = (o3_ac_td_sort[(No3_ac_td_sort/2)-1] + o3_ac_td_sort[(No3_ac_td_sort/2)]) / 2.0
	lower_half = o3_ac_td_sort[0:(No3_ac_td_sort/2)-1]
	upper_half = o3_ac_td_sort[(No3_ac_td_sort/2):(No3_ac_td_sort-1)]
ENDIF ELSE BEGIN
	No3_ac_td_sort 	= N_ELEMENTS(o3_ac_td_sort)
	o3_med = o3_ac_td_sort[(No3_ac_td_sort/2)] 
	lower_half = o3_ac_td_sort[0:(No3_ac_td_sort/2)-1]
	upper_half = o3_ac_td_sort[(No3_ac_td_sort/2):(No3_ac_td_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_ac_td_sort[0.05*No3_ac_td_sort]
quartile_95 = o3_ac_td_sort[0.95*No3_ac_td_sort]

o3ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]
PRINT, 'atl td= ' + STRING(o3ptile[0]) + STRING(o3ptile[1]) + STRING(o3ptile[2]) + $
	STRING(o3ptile[3]) + STRING(o3ptile[4])

o3box = [[o3box],[o3ptile]]
o3box2 = [[o3box2],[o3ptile]]

;ac ts storms
IF (N_ELEMENTS(o3_ac_ts_sort) MOD 2 EQ 0) THEN BEGIN
	No3_ac_ts_sort 	= N_ELEMENTS(o3_ac_ts_sort)
	o3_med = (o3_ac_ts_sort[(No3_ac_ts_sort/2)-1] + o3_ac_ts_sort[(No3_ac_ts_sort/2)]) / 2.0
	lower_half = o3_ac_ts_sort[0:(No3_ac_ts_sort/2)-1]
	upper_half = o3_ac_ts_sort[(No3_ac_ts_sort/2):(No3_ac_ts_sort-1)]
ENDIF ELSE BEGIN
	No3_ac_ts_sort 	= N_ELEMENTS(o3_ac_ts_sort)
	o3_med = o3_ac_ts_sort[(No3_ac_ts_sort/2)] 
	lower_half = o3_ac_ts_sort[0:(No3_ac_ts_sort/2)-1]
	upper_half = o3_ac_ts_sort[(No3_ac_ts_sort/2):(No3_ac_ts_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_ac_ts_sort[0.05*No3_ac_ts_sort]
quartile_95 = o3_ac_ts_sort[0.95*No3_ac_ts_sort]

o3ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]
PRINT, 'atl ts= ' + STRING(o3ptile[0]) + STRING(o3ptile[1]) + STRING(o3ptile[2]) + $
	STRING(o3ptile[3]) + STRING(o3ptile[4])

o3box = [[o3box],[o3ptile]]
o3box2 = [[o3box2],[o3ptile]]

;ac hu storms
IF (N_ELEMENTS(o3_ac_hu_sort) MOD 2 EQ 0) THEN BEGIN
	No3_ac_hu_sort 	= N_ELEMENTS(o3_ac_hu_sort)
	o3_med = (o3_ac_hu_sort[(No3_ac_hu_sort/2)-1] + o3_ac_hu_sort[(No3_ac_hu_sort/2)]) / 2.0
	lower_half = o3_ac_hu_sort[0:(No3_ac_hu_sort/2)-1]
	upper_half = o3_ac_hu_sort[(No3_ac_hu_sort/2):(No3_ac_hu_sort-1)]
ENDIF ELSE BEGIN
	No3_ac_hu_sort 	= N_ELEMENTS(o3_ac_hu_sort)
	o3_med = o3_ac_hu_sort[(No3_ac_hu_sort/2)] 
	lower_half = o3_ac_hu_sort[0:(No3_ac_hu_sort/2)-1]
	upper_half = o3_ac_hu_sort[(No3_ac_hu_sort/2):(No3_ac_hu_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_ac_hu_sort[0.05*No3_ac_hu_sort]
quartile_95 = o3_ac_hu_sort[0.95*No3_ac_hu_sort]

o3ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]
PRINT, 'atl hu= ' + STRING(o3ptile[0]) + STRING(o3ptile[1]) + STRING(o3ptile[2]) + $
	STRING(o3ptile[3]) + STRING(o3ptile[4])

o3box = [[o3box],[o3ptile]]
o3box2 = [[o3box2],[o3ptile]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End Atlantic Coast Storms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;atl td storms
IF (N_ELEMENTS(o3_atl_td_sort) MOD 2 EQ 0) THEN BEGIN
	No3_atl_td_sort 	= N_ELEMENTS(o3_atl_td_sort)
	o3_med = (o3_atl_td_sort[(No3_atl_td_sort/2)-1] + o3_atl_td_sort[(No3_atl_td_sort/2)]) / 2.0
	lower_half = o3_atl_td_sort[0:(No3_atl_td_sort/2)-1]
	upper_half = o3_atl_td_sort[(No3_atl_td_sort/2):(No3_atl_td_sort-1)]
ENDIF ELSE BEGIN
	No3_atl_td_sort 	= N_ELEMENTS(o3_atl_td_sort)
	o3_med = o3_atl_td_sort[(No3_atl_td_sort/2)] 
	lower_half = o3_atl_td_sort[0:(No3_atl_td_sort/2)-1]
	upper_half = o3_atl_td_sort[(No3_atl_td_sort/2):(No3_atl_td_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_atl_td_sort[0.05*No3_atl_td_sort]
quartile_95 = o3_atl_td_sort[0.95*No3_atl_td_sort]

o3ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]
PRINT, 'atl td= ' + STRING(o3ptile[0]) + STRING(o3ptile[1]) + STRING(o3ptile[2]) + $
	STRING(o3ptile[3]) + STRING(o3ptile[4])

o3box = [[o3box],[o3ptile]]
o3box2 = [[o3box2],[o3ptile]]

;atl ts storms
IF (N_ELEMENTS(o3_atl_ts_sort) MOD 2 EQ 0) THEN BEGIN
	No3_atl_ts_sort 	= N_ELEMENTS(o3_atl_ts_sort)
	o3_med = (o3_atl_ts_sort[(No3_atl_ts_sort/2)-1] + o3_atl_ts_sort[(No3_atl_ts_sort/2)]) / 2.0
	lower_half = o3_atl_ts_sort[0:(No3_atl_ts_sort/2)-1]
	upper_half = o3_atl_ts_sort[(No3_atl_ts_sort/2):(No3_atl_ts_sort-1)]
ENDIF ELSE BEGIN
	No3_atl_ts_sort 	= N_ELEMENTS(o3_atl_ts_sort)
	o3_med = o3_atl_ts_sort[(No3_atl_ts_sort/2)] 
	lower_half = o3_atl_ts_sort[0:(No3_atl_ts_sort/2)-1]
	upper_half = o3_atl_ts_sort[(No3_atl_ts_sort/2):(No3_atl_ts_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_atl_ts_sort[0.05*No3_atl_ts_sort]
quartile_95 = o3_atl_ts_sort[0.95*No3_atl_ts_sort]

o3ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]
PRINT, 'atl ts= ' + STRING(o3ptile[0]) + STRING(o3ptile[1]) + STRING(o3ptile[2]) + $
	STRING(o3ptile[3]) + STRING(o3ptile[4])

o3box = [[o3box],[o3ptile]]
o3box2 = [[o3box2],[o3ptile]]

;atl hu storms
IF (N_ELEMENTS(o3_atl_hu_sort) MOD 2 EQ 0) THEN BEGIN
	No3_atl_hu_sort 	= N_ELEMENTS(o3_atl_hu_sort)
	o3_med = (o3_atl_hu_sort[(No3_atl_hu_sort/2)-1] + o3_atl_hu_sort[(No3_atl_hu_sort/2)]) / 2.0
	lower_half = o3_atl_hu_sort[0:(No3_atl_hu_sort/2)-1]
	upper_half = o3_atl_hu_sort[(No3_atl_hu_sort/2):(No3_atl_hu_sort-1)]
ENDIF ELSE BEGIN
	No3_atl_hu_sort 	= N_ELEMENTS(o3_atl_hu_sort)
	o3_med = o3_atl_hu_sort[(No3_atl_hu_sort/2)] 
	lower_half = o3_atl_hu_sort[0:(No3_atl_hu_sort/2)-1]
	upper_half = o3_atl_hu_sort[(No3_atl_hu_sort/2):(No3_atl_hu_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_atl_hu_sort[0.05*No3_atl_hu_sort]
quartile_95 = o3_atl_hu_sort[0.95*No3_atl_hu_sort]

o3ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]
PRINT, 'atl hu= ' + STRING(o3ptile[0]) + STRING(o3ptile[1]) + STRING(o3ptile[2]) + $
	STRING(o3ptile[3]) + STRING(o3ptile[4])

o3box = [[o3box],[o3ptile]]
o3box2 = [[o3box2],[o3ptile]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End Far Atlantic Storms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;landfalling storms
IF (N_ELEMENTS(o3_landfall_sort) MOD 2 EQ 0) THEN BEGIN
	No3_landfall_sort 	= N_ELEMENTS(o3_landfall_sort)
	o3_med = (o3_landfall_sort[(No3_landfall_sort/2)-1] + o3_landfall_sort[(No3_landfall_sort/2)]) / 2.0
	lower_half = o3_landfall_sort[0:(No3_landfall_sort/2)-1]
	upper_half = o3_landfall_sort[(No3_landfall_sort/2):(No3_landfall_sort-1)]
ENDIF ELSE BEGIN
	No3_landfall_sort 	= N_ELEMENTS(o3_landfall_sort)
	o3_med = o3_landfall_sort[(No3_landfall_sort/2)] 
	lower_half = o3_landfall_sort[0:(No3_landfall_sort/2)-1]
	upper_half = o3_landfall_sort[(No3_landfall_sort/2):(No3_landfall_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_landfall_sort[0.05*No3_landfall_sort]
quartile_95 = o3_landfall_sort[0.95*No3_landfall_sort]

o3ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]
PRINT, 'landfalling = ' + STRING(o3ptile[0]) + STRING(o3ptile[1]) + STRING(o3ptile[2]) + $
	STRING(o3ptile[3]) + STRING(o3ptile[4])

o3box = [[o3box],[o3ptile]]
o3box1 = [[o3box1],[o3ptile]]

;+no storms
IF (N_ELEMENTS(no3_storm_sort) MOD 2 EQ 0) THEN BEGIN
	No3_nstorm_sort 	= N_ELEMENTS(o3_nstorm_sort)
	no3_med = (no3_nstorm_sort[(Nno3_nstorm_sort/2)-1] + no3_nstorm_sort[(Nno3_nstorm_sort/2)]) / 2.0
	lower_half = no3_nstorm_sort[0:(Nno3_nstorm_sort/2)-1]
	upper_half = no3_nstorm_sort[(Nno3_nstorm_sort/2):(Nno3_nstorm_sort-1)]
ENDIF ELSE BEGIN
	No3_nstorm_sort 	= N_ELEMENTS(o3_nstorm_sort)
	o3n_med = o3_nstorm_sort[(No3_nstorm_sort/2)] 
	lower_half = o3_nstorm_sort[0:(No3_nstorm_sort/2)-1]
	upper_half = o3_nstorm_sort[(No3_nstorm_sort/2):(No3_nstorm_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_nstorm_sort[0.05*No3_nstorm_sort]
quartile_95 = o3_nstorm_sort[0.95*No3_nstorm_sort]
PRINT, 'no storms= ' + STRING(o3ptile[0]) + STRING(o3ptile[1]) + STRING(o3ptile[2]) + $
	STRING(o3ptile[3]) + STRING(o3ptile[4])


no3box = [quartile_05, quartile_25, o3n_med, quartile_75,quartile_95]

;outdir  = !WRF_DIRECTORY + event + '/boxplot/'
;epsfile = !WRF_DIRECTORY + event + '/boxplot/wrf_storm_box.eps'
;pngfile = !WRF_DIRECTORY + event + '/boxplot/wrf_storm_box.png'
;FILE_MKDIR, outdir

IF KEYWORD_SET(eps) THEN BEGIN	
	PS_ON, FILENAME = epsfile, PAGE_SIZE = [6.45, 8.25], MARGIN = 0.0, /INCHES											;Switch to Postscript device
	DEVICE, /ENCAPSULATED
	!P.FONT     = 0																														;Hardware fonts
	!P.CHARSIZE = 1.25	
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS																													;Load basic color definitions
ENDIF; ELSE BEGIN
;	SET_PLOT, 'X'
;	WINDOW, XSIZE = 645, YSIZE = 825																								;Open graphics window
;	!P.COLOR      = COLOR_24('black')																								;Foreground color
;	!P.BACKGROUND = COLOR_24('white')																								;Background color
;	!P.CHARSIZE   = 1.8		
;	!P.FONT       = -1																													;Use Hershey fonts
;ENDELSE

;!P.MULTI = [0,2,4]


data  = [[o3box],[no3box]]
data1 = [[o3box1],[no3box]]
data2 = [[o3box2],[no3box]]

;title = ['All Storms','All TD','All TS','All HU','All EGulf', 'All WGulf', 'All AtlCoast', 'All Atl', $;'All Gulf', 'All Atl',
;		'Gulf TD','Gulf TS','Gulf HU','Atl TD','Atl TS','Atl HU','Landfalling','No Storms']

title = ['All Storms','All TD','All TS','All HU','All EGulf', 'All WGulf', 'All AtlCoast', 'All FarAtl', $
		'WGulf TD','WGulf TS','WGulf HU','EGulf TD','EGulf TS','EGulf HU','AtlCoast TD','AtlCoast TS','AtlCoast HU', $
		'FarAtl TD','FarAtl TS','FarAtl HU','Landfalling','No Storms']

title1 = ['All Storms','All TD','All TS','All HU','All EGulf', 'All WGulf', 'All AtlCoast', 'All FarAtl', $
		'Landfalling','No Storms']

title2 = ['All Storms','WGulf TD','WGulf TS','WGulf HU','EGulf TD','EGulf TS','EGulf HU',$
		'AtlCoast TD','AtlCoast TS','AtlCoast HU', 'FarAtl TD','FarAtl TS','FarAtl HU','No Storms']

IF (~KEYWORD_SET(NORMALIZE)) THEN BEGIN
	xrange = [20, 105]
	title1  = 'July-Nov Ozone'
ENDIF
IF (KEYWORD_SET(NORMALIZE)) THEN BEGIN
	xrange = [-40, 40]
	title_name  = 'July-Nov Ozone Anomalies'
ENDIF
boxes = BOXPLOT(data1, $
		TITLE		= title_name, $
		XRANGE 		= xrange, $
		YRANGE 		= [-1, 10], $
		XTITLE 		= 'Normalized Ozone Concentration (ppb)', $
		YTICKNAME 	= title1, $
		YTICKVALUES = INDGEN(N_ELEMENTS(title1)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

boxes = BOXPLOT(data2, $
		XRANGE 		= title_name, $
		YRANGE 		= [-1, 14], $
		XTITLE 		= 'Normalized Ozone Concentration (ppb)', $
		YTICKNAME 	= title2, $
		YTICKVALUES = INDGEN(N_ELEMENTS(title2)), $	
		FONT_SIZE	= 20, $	
		HORIZONTAL	= 1)

!P.MULTI = 0
STOP
IF (N_ELEMENTS(pngfile) NE 0) THEN print, pngfile

IF KEYWORD_SET(eps) THEN BEGIN
    IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
	      LOAD_BASIC_COLORS, /RESET                                           												;Reset color table to linear ramp
   	   PS_OFF                                                                											;Turn PS off
ENDIF ELSE IF KEYWORD_SET(png) THEN $
         WRITE_PNG, pngfile, TVRD(TRUE = 1)                                    											;Write PNG file

END
