FUNCTION PM_DALLAS_NAAQS, year, $
	PNG		  = png, $
	EPS		  = eps, $
	CLOBBER   = clobber

;+
;NAME:
;     TRAJ3D_RAP_P
;PURPOSE:
;     This copies variables from ERA-Interim analysis into a single
;     file in pm_dpure coordinates for use in TRAJ3D.
;     W at the surface is set to zero.  One pm_dpure level is added at 
;     the top of the domain (p = 0), where w is also set to zero.
;CATEGORY:
;     Data handling utility.
;CALLING SEQUENCE:
;     TRAJ3D_RAP_P, date0, outfile
;INPUT:
;		flight_name : RAF flight name (e.g., 'rf01')
;		direction   : 'forward' or 'backward'
;		ndays       : Length of trajectory run in days.  Default is 5.
;KEYWORDS:
;     PLOT      : If set, plot sample maps.
;     DIRECTORY : Output directory for pm_dp file.
;	  CLOBBER   : If set, overwrite existing file. This is the default.
;OUTPUT:
;     Netcdf file.
;MODIFICATION HISTORY:
;		C. Homeyer:       2015-06-22.
;-

COMPILE_OPT IDL2																									;Set compile options

indir = !WRF_DIRECTORY + 'general/precursors/pm25/'
infile = indir + 'dallas_pm_' + year + '.csv'

headers= ['State Code','County Code','Site Num','Parameter Code','POC','Latitude','Longitude', $
	'Datum','Parameter Name','Date Local','Time Local','Date GMT','Time GMT','Sample Measurement', $
	'Units of Measurement','MDL','Uncertainty','Qualifier','Method Type','Method Code','Method Name', $
	'State Name','County Name','Date of Last Change']

data = READ_CSV(infile);, HEADER = headers)

CASE year OF 
	'2005' : BEGIN
			site1 = WHERE(data.field03 EQ 1006)
   			site2 = WHERE(data.field03 EQ 3008)
         	site3 = WHERE(data.field03 EQ 3009)
         	site4 = WHERE(data.field03 EQ 3011)
         	
         	pm1 = data.field14[site1]
            pm2 = data.field14[site2]
            pm3 = data.field14[site3]
            pm4 = data.field14[site4]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
			END

	'2006' : BEGIN
			site1 = WHERE(data.field03 EQ 1006)
   			site2 = WHERE(data.field03 EQ 3011)

         	pm1 = data.field14[site1]
            pm2 = data.field14[site2]

            date1 = data.field12[site1]
            date2 = data.field12[site2]

            time1 = data.field13[site1]
            time2 = data.field13[site2]
			END

	'2007' : BEGIN
			site1 = WHERE(data.field03 EQ 1006)
   			site2 = WHERE(data.field03 EQ 3011)

         	pm1 = data.field14[site1]
            pm2 = data.field14[site2]

            date1 = data.field12[site1]
            date2 = data.field12[site2]

            time1 = data.field13[site1]
            time2 = data.field13[site2]
			END

	'2008' : BEGIN
			site1 = WHERE(data.field03 EQ 1006)
   			site2 = WHERE(data.field03 EQ 3011)

         	pm1 = data.field14[site1]
            pm2 = data.field14[site2]

            date1 = data.field12[site1]
            date2 = data.field12[site2]

            time1 = data.field13[site1]
            time2 = data.field13[site2]
			END

	'2009' : BEGIN
			site1 = WHERE(data.field03 EQ 1006)
   			site2 = WHERE(data.field03 EQ 3011)

         	pm1 = data.field14[site1]
            pm2 = data.field14[site2]

            date1 = data.field12[site1]
            date2 = data.field12[site2]

            time1 = data.field13[site1]
            time2 = data.field13[site2]
			END

	'2010' : BEGIN
			site1 = WHERE(data.field03 EQ 1006)
   			site2 = WHERE(data.field03 EQ 3011)

         	pm1 = data.field14[site1]
            pm2 = data.field14[site2]

            date1 = data.field12[site1]
            date2 = data.field12[site2]

            time1 = data.field13[site1]
            time2 = data.field13[site2]
			END
		
	'2011' : BEGIN
			site1 = WHERE(data.field03 EQ 1006)
   			site2 = WHERE(data.field03 EQ 3011)

         	pm1 = data.field14[site1]
            pm2 = data.field14[site2]

            date1 = data.field12[site1]
            date2 = data.field12[site2]

            time1 = data.field13[site1]
            time2 = data.field13[site2]
            END

	'2012' : BEGIN
			site1 = WHERE(data.field03 EQ 1006)
   			site2 = WHERE(data.field03 EQ 3011)

         	pm1 = data.field14[site1]
            pm2 = data.field14[site2]

            date1 = data.field12[site1]
            date2 = data.field12[site2]

            time1 = data.field13[site1]
            time2 = data.field13[site2]
            END

	'2013' : BEGIN
			site1 = WHERE(data.field03 EQ 1006)
   			site2 = WHERE(data.field03 EQ 3011)

         	pm1 = data.field14[site1]
            pm2 = data.field14[site2]

            date1 = data.field12[site1]
            date2 = data.field12[site2]

            time1 = data.field13[site1]
            time2 = data.field13[site2]
            END

	'2014' : BEGIN
			site1 = WHERE(data.field03 EQ 1006)
   			site2 = WHERE(data.field03 EQ 3011)

         	pm1 = data.field14[site1]
            pm2 = data.field14[site2]

            date1 = data.field12[site1]
            date2 = data.field12[site2]

            time1 = data.field13[site1]
            time2 = data.field13[site2]
            END

	'2015' : BEGIN
			site1 = WHERE(data.field03 EQ 1006)
   			site2 = WHERE(data.field03 EQ 3011)

         	pm1 = data.field14[site1]
            pm2 = data.field14[site2]

            date1 = data.field12[site1]
            date2 = data.field12[site2]

            time1 = data.field13[site1]
            time2 = data.field13[site2]
            END

	'2016' : BEGIN
			site1 = WHERE(data.field03 EQ 1006)
   			site2 = WHERE(data.field03 EQ 3011)

         	pm1 = data.field14[site1]
            pm2 = data.field14[site2]

            date1 = data.field12[site1]
            date2 = data.field12[site2]

            time1 = data.field13[site1]
            time2 = data.field13[site2]
            END

	'2017' : BEGIN
			site1 = WHERE(data.field03 EQ 1006)
   			site2 = WHERE(data.field03 EQ 3011)

         	pm1 = data.field14[site1]
            pm2 = data.field14[site2]

            date1 = data.field12[site1]
            date2 = data.field12[site2]

            time1 = data.field13[site1]
            time2 = data.field13[site2]
            END

ENDCASE         

num_hours = [N_ELEMENTS(site1),N_ELEMENTS(site2),N_ELEMENTS(site3)]
pm_hourly = [ ]
pm_8hr_total = [ ]

pm1_8hr = FLTARR(N_ELEMENTS(site1))
FOR tt = 0, N_ELEMENTS(site1)-9 DO BEGIN
	pm1_8hr[tt] = MEAN(pm1[tt:tt+8],/NAN)
ENDFOR
pm_hourly = [pm_hourly, pm1]
PRINT, 'Done calculating site 1'

IF (N_ELEMENTS(site2) GT 0) THEN BEGIN
    pm2_8hr = FLTARR(N_ELEMENTS(site2))
    FOR tt = 0, N_ELEMENTS(site2)-9 DO BEGIN
    	pm2_8hr[tt] = MEAN(pm2[tt:tt+8],/NAN)
    ENDFOR
    pm_hourly = [pm_hourly, pm2]
    pm_8hr_total = [pm1_8hr, pm2_8hr ]

    PRINT, 'Done calculating site 2'
ENDIF

IF (N_ELEMENTS(site3) GT 0) THEN BEGIN
    pm3_8hr = FLTARR(N_ELEMENTS(site3))
    FOR tt = 0, N_ELEMENTS(site3)-9 DO BEGIN
    	pm3_8hr[tt] = MEAN(pm3[tt:tt+8],/NAN)
    ENDFOR
    pm_hourly = [pm_hourly, pm3]
    PRINT, 'Done calculating site 3'
    
    pm_8hr_total = [pm1_8hr, pm2_8hr, pm3_8hr]
ENDIF

IF (N_ELEMENTS(site4) GT 0) THEN BEGIN
    pm4_8hr = FLTARR(N_ELEMENTS(site4))
    FOR tt = 0, N_ELEMENTS(site4)-9 DO BEGIN
    	pm4_8hr[tt] = MEAN(pm4[tt:tt+8],/NAN)
    ENDFOR   
    PRINT, 'Done calculating site 4'
pm_8hr_total = [pm_8hr_total, pm4_8hr]
pm_hourly = [pm_hourly, pm4]
ENDIF

IF (N_ELEMENTS(site5) GT 0) THEN BEGIN
    pm5_8hr = FLTARR(N_ELEMENTS(site5))
    FOR tt = 0, N_ELEMENTS(site5)-9 DO BEGIN
    	pm5_8hr[tt] = MEAN(pm5[tt:tt+8],/NAN)
    ENDFOR   
    PRINT, 'Done calculating site 5'
pm_8hr_total = [pm_8hr_total, pm5_8hr]
pm_hourly = [pm_hourly, pm5]
ENDIF

IF (N_ELEMENTS(site6) GT 0) THEN BEGIN
    pm6_8hr = FLTARR(N_ELEMENTS(site6))
    FOR tt = 0, N_ELEMENTS(site6)-9 DO BEGIN
    	pm6_8hr[tt] = MEAN(pm6[tt:tt+8],/NAN)
    ENDFOR   
    PRINT, 'Done calculating site 6'
pm_8hr_total = [pm_8hr_total, pm6_8hr]
pm_hourly = [pm_hourly, pm6]
ENDIF

;Return, pm_8hr_total

;Calculate the daily max
;Also return binned pm concentrations by hour to compute PDFs of the pm Diurnal Cycle
time_arr = [ ]
date1 = MAKE_DATE(year,07,01,06,00)
date2 = MAKE_DATE(year,12,01,06,00)
end_datestr = STRMID(MAKE_ISO_DATE_STRING(date2),0,4) + STRMID(MAKE_ISO_DATE_STRING(date2),5,2) + $
			 	STRMID(MAKE_ISO_DATE_STRING(date2),8,2) + STRMID(MAKE_ISO_DATE_STRING(date2),11,2) + $
		  	 	STRMID(MAKE_ISO_DATE_STRING(date2),14,2) 

pm_datestr =  STRMID(data.field12,0,4)+STRMID(data.field12,5,2)+STRMID(data.field12,8,2) + $
   				 STRMID(data.field13,0,2)+STRMID(data.field13,3,2) 

dt = 3600
nt = TIME_DIFF(date2,date1)
nt = nt/dt
FOR i = 0, nt - 1 DO BEGIN
	datestr = STRMID(MAKE_ISO_DATE_STRING(date1),0,4) + STRMID(MAKE_ISO_DATE_STRING(date1),5,2) + $
		 STRMID(MAKE_ISO_DATE_STRING(date1),8,2) + STRMID(MAKE_ISO_DATE_STRING(date1),11,2) + $
	  	 STRMID(MAKE_ISO_DATE_STRING(date1),14,2) 
	time_arr = [time_arr, datestr]
	IF (datestr EQ end_datestr) THEN BREAK
	date1 = TIME_INC(date1, dt)
ENDFOR 

pm_daily_max = [] 
pm_datestr = [ ]
FOR i = 0 , nt-1, 24 DO BEGIN
	indices=[]
   	FOR hh=0,23 DO BEGIN
   		ipm     = WHERE(pm_datestr EQ time_arr[i+hh], pmcount)
   		indices = [indices, ipm]
   	ENDFOR
	;Find daily max pm
	pm_daily_max = [pm_daily_max, [MAX(pm_8hr_total[indices],/NAN), $
		MAX(pm_8hr_total[indices],/NAN), MAX(pm_8hr_total[indices],/NAN), $
		MAX(pm_8hr_total[indices],/NAN)]]
	pm_datestr = [pm_datestr, [time_arr[i],time_arr[i+6],time_arr[i+12],time_arr[i+18]]]
ENDFOR


RETURN, {pm_daily_max 		: pm_daily_max, $
				pm_datestr 	: pm_datestr, $
				pm_8hr_total : pm_8hr_total, $
				pm_hourly	: pm_hourly, $
				date            : data.field12, $
				time			: data.field13}
		
END
