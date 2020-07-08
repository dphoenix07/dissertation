FUNCTION RH_DP_DALLAS_NAAQS, year, $
	PNG		  = png, $
	EPS		  = eps, $
	CLOBBER   = clobber

;+
;NAME:
;     TRAJ3D_RAP_P
;PURPOSE:
;     This copies variables from ERA-Interim analysis into a single
;     file in rh_dpure coordinates for use in TRAJ3D.
;     W at the surface is set to zero.  One rh_dpure level is added at 
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
;     DIRECTORY : Output directory for rh_dp file.
;	  CLOBBER   : If set, overwrite existing file. This is the default.
;OUTPUT:
;     Netcdf file.
;MODIFICATION HISTORY:
;		C. Homeyer:       2015-06-22.
;-

COMPILE_OPT IDL2																									;Set compile options

indir = !WRF_DIRECTORY + 'general/met_obs/rh_dp/'
infile = indir + 'dallas_rh_dp_' + year + '.csv'

headers= ['State Code','County Code','Site Num','Parameter Code','POC','Latitude','Longitude', $
	'Datum','Parameter Name','Date Local','Time Local','Date GMT','Time GMT','Sample Measurement', $
	'Units of Measurement','MDL','Uncertainty','Qualifier','Method Type','Method Code','Method Name', $
	'State Name','County Name','Date of Last Change']

data = READ_CSV(infile);, HEADER = headers)

CASE year OF 
	'2004' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
            rh_dp1 = data.field14[site1]
            date1 = data.field12[site1]
            time1 = data.field13[site1]
			END

	'2005' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
            rh_dp1 = data.field14[site1]
            date1 = data.field12[site1]
            time1 = data.field13[site1]
			END

	'2006' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
            rh_dp1 = data.field14[site1]
            date1 = data.field12[site1]
            time1 = data.field13[site1]
			END

	'2007' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
            rh_dp1 = data.field14[site1]
            date1 = data.field12[site1]
            time1 = data.field13[site1]
			END

	'2008' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
            rh_dp1 = data.field14[site1]
            date1 = data.field12[site1]
            time1 = data.field13[site1]
			END

	'2009' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
            rh_dp1 = data.field14[site1]
            date1 = data.field12[site1]
            time1 = data.field13[site1]
			END

	'2010' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
            rh_dp1 = data.field14[site1]
            date1 = data.field12[site1]
            time1 = data.field13[site1]
			END
		
	'2011' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
            rh_dp1 = data.field14[site1]
            date1 = data.field12[site1]
            time1 = data.field13[site1]
            END

	'2012' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
            rh_dp1 = data.field14[site1]
            date1 = data.field12[site1]
            time1 = data.field13[site1]
            END

	'2013' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
            rh_dp1 = data.field14[site1]
            date1 = data.field12[site1]
            time1 = data.field13[site1]
            END

	'2014' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
            rh_dp1 = data.field14[site1]
            date1 = data.field12[site1]
            time1 = data.field13[site1]
            END

	'2015' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
            rh_dp1 = data.field14[site1]
            date1 = data.field12[site1]
            time1 = data.field13[site1]
            END

	'2016' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
            rh_dp1 = data.field14[site1]
            date1 = data.field12[site1]
            time1 = data.field13[site1]
            END

	'2017' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
            rh_dp1 = data.field14[site1]
            date1 = data.field12[site1]
            time1 = data.field13[site1]
            END

ENDCASE         

num_hours = [N_ELEMENTS(site1),N_ELEMENTS(site2),N_ELEMENTS(site3)]
rh_dp_hourly = [ ]

rh_dp1_8hr = FLTARR(N_ELEMENTS(site1))
FOR tt = 0, N_ELEMENTS(site1)-9 DO BEGIN
	rh_dp1_8hr[tt] = MEAN(rh_dp1[tt:tt+8],/NAN)
ENDFOR
rh_dp_8hr_total = rh_dp1_8hr
rh_dp_hourly = [rh_dp_hourly, rh_dp1]
PRINT, 'Done calculating site 1'

;Calculate the daily max
;Also return binned o3 concentrations by hour to compute PDFs of the O3 Diurnal Cycle
time_arr = [ ]
date1 = MAKE_DATE(year,07,01,06,00)
date2 = MAKE_DATE(year,12,01,06,00)
end_datestr = STRMID(MAKE_ISO_DATE_STRING(date2),0,4) + STRMID(MAKE_ISO_DATE_STRING(date2),5,2) + $
			 	STRMID(MAKE_ISO_DATE_STRING(date2),8,2) + STRMID(MAKE_ISO_DATE_STRING(date2),11,2) + $
		  	 	STRMID(MAKE_ISO_DATE_STRING(date2),14,2) 

o3_datestr =  STRMID(data.field12,0,4)+STRMID(data.field12,5,2)+STRMID(data.field12,8,2) + $
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

rh_dp_daily_max = [] 
rh_dp_datestr = [ ]
FOR i = 0 , nt-1, 24 DO BEGIN
	indices=[]
   	FOR hh=0,23 DO BEGIN
   		io3     = WHERE(o3_datestr EQ time_arr[i+hh], o3count)
   		indices = [indices, io3]
   	ENDFOR
	;Find daily max rh_dp
	rh_dp_daily_max = [rh_dp_daily_max, [MAX(rh_dp_8hr_total[indices],/NAN), $
		MAX(rh_dp_8hr_total[indices],/NAN), MAX(rh_dp_8hr_total[indices],/NAN), $
		MAX(rh_dp_8hr_total[indices],/NAN)]]
	rh_dp_datestr = [rh_dp_datestr, [time_arr[i],time_arr[i+6],time_arr[i+12],time_arr[i+18]]]
ENDFOR


RETURN, {rh_dp_daily_max 		: rh_dp_daily_max, $
				rh_dp_datestr 	: rh_dp_datestr, $
				rh_dp_8hr_total : rh_dp_8hr_total, $
				rh_dp_hourly	: rh_dp_hourly, $
				date            : data.field12, $
				time			: data.field13}
		
END
