PRO REANALYSIS_COMPOSITE_FILE, lev, $
	PNG		  = png, $
	EPS		  = eps, $
	CLOBBER   = clobber

;+
;NAME:
;     REANALYSIS_COMPOSITE_FILE
;PURPOSE:
;     This copies variables from ERA-Interim analysis into a single
;     file in pressure coordinates for use in TRAJ3D.
;     W at the surface is set to zero.  One pressure level is added at 
;     the top of the domain (p = 0), where w is also set to zero.
;CATEGORY:
;     Data handling utility.
;CALLING SEQUENCE:
;     REANALYSIS_COMPOSITE_FILE, date0, outfile
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
;		D. Phoenix:       2019-02-15.
;-

COMPILE_OPT IDL2																									;Set compile options

IF (N_ELEMENTS(lev) EQ 0) THEN lev = 250.0

dallas_x = (360.0-96.86)
dallas_y = 32.85

x = FINDGEN(360)
y = FINDGEN(181)-90.0

;x0 = dallas_x - 1.0
;y0 = dallas_y - 1.0
;x1 = dallas_x + 1.0
;y1 = dallas_y + 1.0

yc = 32.85
xc = 360.0-96.86

x0 = 360.0-105.0
y0 = 22.0
x1 = 360.0-62.73
y1 = 52.5

ix0 = WHERE(ROUND(x0) EQ x)
ix1 = WHERE(ROUND(x1) EQ x) 
iy0 = WHERE(ROUND(y0) EQ y)
iy1 = WHERE(ROUND(y1) EQ y) 

ts_data = READ_HURDAT2()

;Truncate data to only look at years with 00,06,12,18Z hours
imonth = WHERE(((ts_data.date.year GE '1980') AND (ts_data.date.year LT '2018')) AND $
	((ts_data.date.month GE 07) AND (ts_data.date.month LT 11)) AND (ts_data.date.hour EQ 00))

extr_storms = WHERE(((ts_data.class[imonth] EQ 'TD') OR (ts_data.class[imonth] EQ 'TS') OR $
    (ts_data.class[imonth] EQ 'HU')), istorms)
;iperiod = imonth[extr_storms]

icount = WHERE(((ts_data.x[imonth[extr_storms]] LE -75.0) AND (ts_data.x[imonth[extr_storms]] GT -82.0) AND $
		(ts_data.y[imonth[extr_storms]] LE 40.0) AND (ts_data.y[imonth[extr_storms]] GE 20.0)), account, COMPLEMENT = iout)
iperiod = imonth[extr_storms[icount]]

nt = N_ELEMENTS(ts_data.date[iperiod])

years  = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),0,4) 
months = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),5,2) 
days   = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),8,2) 
hours  = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),11,2)

ts_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),0,4) +  $
	STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),5,2) + $
	 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),8,2) + $
	 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),11,2) + '00'
		
;Set arrays
;mth_arr=['08','09','10']
;day_arr=[31,30,31]

;mth_arr = ['07','08','09','10','11']
;day_arr = [31  ,  31,  30];,  31,  30]

mth_arr = ['07','08','09','10'];,'11']
day_arr = [31  ,  31,  30,  31];,  30]
;hr_arr = [hour]

yr_arr = ['1980','1981','1982','1983','1984','1985','1986','1987','1988','1989','1990', $
			'1991','1992','1993','1994','1995','1996','1997','1998','1999','2000', $
			'2001','2002','2003','2004','2005','2006','2007','2008','2009','2010', $
			'2011','2012','2013','2014','2015','2016','2017']

;yr_arr = ['1996','1997','1998','1999','2000','2001','2002','2003','2004','2005','2006',$
;		'2007','2008','2009','2010','2011','2012','2013','2014','2015','2016']

;yr_arr=['2000','2001','2002','2003','2004','2005','2006','2007','2008','2009','2010', $
;		'2011','2012','2013','2014','2015']

m=0		
ny = N_ELEMENTS(yr_arr)
mt = N_ELEMENTS(mth_arr)

o3_nstorm = []
o3dm_nstorm =[]

o3_storm = []
o3dm_storm = []
	 	
;define arrays
o3_storm_epa = []
o3dm_storm_epa = []
o3_nstorm_epa = []
o3dm_nstorm_epa = []
o3_storm_nepa = []
o3dm_storm_nepa = []
o3_nstorm_nepa = []
o3dm_nstorm_nepa = []

pressure_nstorm_epa = []
temp_nstorm_epa	  = []
Z_nstorm_epa	 	  = []
u_nstorm_epa	 	  = []
v_nstorm_epa		  = []
sh_nstorm_epa		  = []
wsp_nstorm_epa	  = []

pressure_nstorm_nepa = []
temp_nstorm_nepa	  = []
Z_nstorm_nepa	 	  = []
u_nstorm_nepa	 	  = []
v_nstorm_nepa		  = []
sh_nstorm_nepa		 =[]
wsp_nstorm_nepa	  = []

pressure_storm_epa = []
temp_storm_epa	  = []
Z_storm_epa	 	  = []
u_storm_epa	 	  = []
v_storm_epa		  = []
sh_storm_epa		  = []
wsp_storm_epa	  	  = []

pressure_storm_nepa = []
temp_storm_nepa	  = []
Z_storm_nepa	 	  = []
u_storm_nepa	 	  = []
v_storm_nepa		  = []
sh_storm_nepa		  = []
wsp_storm_nepa	  = []

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  o3_nstorm_epa2 	  = []
  o3dm_nstorm_epa2 	  = []
  pressure_nstorm_epa2 = []
  temp_nstorm_epa2	  = []
  Z_nstorm_epa2	 	  = []
  u_nstorm_epa2	 	  = []
  v_nstorm_epa2		  = []
  sh_nstorm_epa2	  = []
  wsp_nstorm_epa2	  = []

  o3_nstorm_nepa2 	  = []
  o3dm_nstorm_nepa2 	 =[]
  pressure_nstorm_nepa2 =[]
  temp_nstorm_nepa2	  = []
  Z_nstorm_nepa2	 	=[] 
  u_nstorm_nepa2	 	=[] 
  v_nstorm_nepa2		=[] 
  sh_nstorm_nepa2		=[] 
  wsp_nstorm_nepa2	  = []

  o3_storm_epa2 	  =[]	 
  o3dm_storm_epa2 	  = []
  pressure_storm_epa2  = []
  temp_storm_epa2	  = []
  Z_storm_epa2	 	  = []
  u_storm_epa2	 	  = []
  v_storm_epa2		  = []
  sh_storm_epa2		  = []
  wsp_storm_epa2  	  = []

  o3_storm_nepa2 	  = []
  o3dm_storm_nepa2 	  = []
  pressure_storm_nepa2 = []
  temp_storm_nepa2	  = []
  Z_storm_nepa2	 	  = []
  u_storm_nepa2	 	  = []
  v_storm_nepa2		  = []
  sh_storm_nepa2	  = []
  wsp_storm_nepa2	  = []

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


nt=N_ELEMENTS(years)
FOREACH year, yr_arr DO BEGIN
    ;date1 = years[m] + months[m] + days[m] + hour + '00'
    ;PRINT, date1

    ;Create 1-hour bin with 6 hour interval
    time_arr = [ ]
    date1 = MAKE_DATE(year,07,01,00)
    date2 = MAKE_DATE(year,11,01,00)
    end_datestr = STRMID(MAKE_ISO_DATE_STRING(date2),0,4) + STRMID(MAKE_ISO_DATE_STRING(date2),5,2) + $
    			 	STRMID(MAKE_ISO_DATE_STRING(date2),8,2) + STRMID(MAKE_ISO_DATE_STRING(date1),11,2) +'00'
    
    nt_yr = 8760/6
    FOR i = 0, nt_yr - 1 DO BEGIN
    		datestr = STRMID(MAKE_ISO_DATE_STRING(date1),0,4) + STRMID(MAKE_ISO_DATE_STRING(date1),5,2) + $
    			 STRMID(MAKE_ISO_DATE_STRING(date1),8,2) + STRMID(MAKE_ISO_DATE_STRING(date1),11,2) +'00'
    		time_arr = [time_arr, datestr]
    		IF (datestr EQ end_datestr) THEN BREAK
    		;date1 = TIME_INC(date1, 21600)
    		date1 = TIME_INC(date1, 86400)
    ENDFOR 
    
	o3_data      = OZONE_DALLAS_NAAQS(year)

    o3_storm1_epa = []
    o3dm_storm1_epa = []
    o3_nstorm1_epa = []
    o3dm_nstorm1_epa = []
    o3_storm1_nepa = []
    o3dm_storm1_nepa = []
    o3_nstorm1_nepa = []
    o3dm_nstorm1_nepa = []

    pressure_nstorm1_epa = []
    temp_nstorm1_epa	 = []
    Z_nstorm1_epa	 	= []
    u_nstorm1_epa	 	= []
    v_nstorm1_epa		= []
    sh_nstorm1_epa		= []
    wsp_nstorm1_epa	    = []
    
    pressure_nstorm1_nepa = []
    temp_nstorm1_nepa	 = []
    Z_nstorm1_nepa	 	= []
    u_nstorm1_nepa	 	= []
    v_nstorm1_nepa		= []
    sh_nstorm1_nepa		= []
    wsp_nstorm1_nepa	= []
    
    pressure_storm1_epa = []
    temp_storm1_epa	 	= []
    Z_storm1_epa	 	= []
    u_storm1_epa	 	= []
    v_storm1_epa		= []
    sh_storm1_epa		= []
    wsp_storm1_epa	    = []
    
    pressure_storm1_nepa = []
    temp_storm1_nepa	 = []
    Z_storm1_nepa	 	= []
    u_storm1_nepa	 	= []
    v_storm1_nepa		= []
    sh_storm1_nepa		= []
    wsp_storm1_nepa	    = []
	
	nt=N_ELEMENTS(time_arr)
	FOR tt = 0, nt-1 DO BEGIN
	    mth = WHERE(STRMID(time_arr[tt],4,2) EQ mth_arr)
		
	    o3_datestr =  STRMID(o3_data.date,0,4)+STRMID(o3_data.date,5,2)+STRMID(o3_data.date,8,2) + $
	    			  STRMID(o3_data.time,0,2)+STRMID(o3_data.time,3,2) 
        io3   = WHERE(o3_datestr EQ time_arr[tt])
        io3dm = WHERE(o3_data.ozone_datestr EQ time_arr[tt])
        		
 	   its   = WHERE(ts_datestr  EQ time_arr[tt], tscount )
 	   
 	   PRINT, time_arr[tt]
 	   year  = STRMID(time_arr[tt],0,4) 
       month = STRMID(time_arr[tt],4,2) 
       day   = STRMID(time_arr[tt],6,2) 
       hour  = STRMID(time_arr[tt],8,2)

 	   IF (tscount EQ 0) THEN BEGIN
	        o3_nstorm = [o3_nstorm, MAX(o3_data.ozone_hourly[io3],/NAN)]
		    o3dm_nstorm = [o3dm_nstorm, MAX(o3_data.ozone_daily_max[io3dm],/NAN)]
		    
		    date1 = MAKE_DATE(year,month,day,hour)
			PRINT, date1
            pressure  = (ERA_INTERIM_READ_VAR('P' ,date1)).values
            temp	  = (ERA_INTERIM_READ_VAR('T' ,date1)).values
            Z	 	  = (ERA_INTERIM_READ_VAR('Z' ,date1)).values
            u	 	  = (ERA_INTERIM_READ_VAR('u' ,date1)).values
            v		  = (ERA_INTERIM_READ_VAR('v' ,date1)).values
			sh		  = (ERA_INTERIM_READ_VAR('SH',date1)).values

        	;interpolate to 950 hPa pressure level	
            dim = SIZE(Z, /DIMENSIONS)
            nx = dim[0]
            ny = dim[1]
            nz = dim[2]
            ix = REBIN(FINDGEN(nx), nx, ny)
            iy = REBIN(REFORM(FINDGEN(ny), 1, ny), nx, ny)
            
            ;Calculate interpolation index for the 10.5 km level in each model grid column
            iz = FLTARR(nx, ny)
            FOR i = 0, nx-1 DO FOR j = 0, ny-1 DO iz[i,j] = INTERPOL(FINDGEN(nz), REFORM(pressure[i,j,*], nz), lev)
            
            ;Interpolate ozone volume to 10.5 km altitude map
            temp_map  = INTERPOLATE(temp, ix, iy, iz)
            gph_map   = INTERPOLATE(Z,    ix, iy, iz)
            u_map     = INTERPOLATE(u,    ix, iy, iz)
            v_map     = INTERPOLATE(v,    ix, iy, iz)
			sh_map    = INTERPOLATE(sh,   ix, iy, iz)
			
			IF ((MAX(o3_data.ozone_daily_max[io3dm],/NAN) GE 70.0)) THEN BEGIN
				o3_nstorm1_epa		 = [o3_nstorm1_epa, MAX(o3_data.ozone_hourly[io3],/NAN)]
				o3dm_nstorm1_epa	 = [o3dm_nstorm1_epa, MAX(o3_data.ozone_daily_max[io3],/NAN)]
				pressure_nstorm1_epa = [[[pressure_nstorm1_epa]],[[pressure  [ix0:ix1,iy0:iy1,0]]]]
				temp_nstorm1_epa	 = [[[temp_nstorm1_epa	  ]],[[temp_map  [ix0:ix1,iy0:iy1]]]]
				Z_nstorm1_epa	 	 = [[[Z_nstorm1_epa	   	  ]],[[ gph_map  [ix0:ix1,iy0:iy1]]]]
				u_nstorm1_epa	 	 = [[[u_nstorm1_epa	      ]],[[ u_map	  [ix0:ix1,iy0:iy1]]]]
				v_nstorm1_epa		 = [[[v_nstorm1_epa	      ]],[[ v_map	  [ix0:ix1,iy0:iy1]]]]
				sh_nstorm1_epa		 = [[[sh_nstorm1_epa	  ]],[[sh_map	  [ix0:ix1,iy0:iy1]]]]
				wsp_nstorm1_epa	     = [[[wsp_nstorm1_epa	  ]],[[SQRT(u_map^2 + v_map^2)]    ]]
			ENDIF

			IF ((MAX(o3_data.ozone_daily_max[io3dm],/NAN) LT 70.0)) THEN BEGIN
				o3_nstorm1_nepa		 = [o3_nstorm1_nepa, MAX(o3_data.ozone_hourly[io3],/NAN)]
				o3dm_nstorm1_nepa	 = [o3dm_nstorm1_nepa, MAX(o3_data.ozone_daily_max[io3],/NAN)]
				pressure_nstorm1_nepa = [[[pressure_nstorm1_nepa]],[[pressure  [ix0:ix1,iy0:iy1,0]]]]
				temp_nstorm1_nepa	  = [[[temp_nstorm1_nepa	]],[[temp_map  [ix0:ix1,iy0:iy1]]]]
				Z_nstorm1_nepa	 	  = [[[Z_nstorm1_nepa	   	]],[[ gph_map  [ix0:ix1,iy0:iy1]]]]
				u_nstorm1_nepa	 	  = [[[u_nstorm1_nepa	   	]],[[ u_map	[ix0:ix1,iy0:iy1]]]]
				v_nstorm1_nepa		  = [[[v_nstorm1_nepa	   	]],[[ v_map	[ix0:ix1,iy0:iy1]]]]
				sh_nstorm1_nepa		  = [[[sh_nstorm1_nepa	 	]],[[sh_map	[ix0:ix1,iy0:iy1]]]]
				wsp_nstorm1_nepa	  = [[[wsp_nstorm1_nepa	 	]],[[SQRT(u_map^2 + v_map^2)]    ]]
			ENDIF
   	   ENDIF 
   	   
   	   IF (tscount GT 0) THEN BEGIN
	        o3_storm = [o3_storm, MAX(o3_data.ozone_hourly[io3],/NAN)]
		    o3dm_storm = [o3dm_storm, MAX(o3_data.ozone_daily_max[io3dm],/NAN)]

            pressure  = (ERA_INTERIM_READ_VAR('P' ,date1)).values
            temp	  = (ERA_INTERIM_READ_VAR('T' ,date1)).values
            Z	 	  = (ERA_INTERIM_READ_VAR('Z' ,date1)).values
            u	 	  = (ERA_INTERIM_READ_VAR('u' ,date1)).values
            v		  = (ERA_INTERIM_READ_VAR('v' ,date1)).values
			sh		  = (ERA_INTERIM_READ_VAR('SH',date1)).values

        	;interpolate to 950 hPa pressure level	
            dim = SIZE(Z, /DIMENSIONS)
            nx = dim[0]
            ny = dim[1]
            nz = dim[2]
            ix = REBIN(FINDGEN(nx), nx, ny)
            iy = REBIN(REFORM(FINDGEN(ny), 1, ny), nx, ny)
            
            ;Calculate interpolation index for the 10.5 km level in each model grid column
            iz = FLTARR(nx, ny)
            FOR i = 0, nx-1 DO FOR j = 0, ny-1 DO iz[i,j] = INTERPOL(FINDGEN(nz), REFORM(pressure[i,j,*], nz), lev)
            
            ;Interpolate ozone volume to 10.5 km altitude map
            temp_map  = INTERPOLATE(temp, ix, iy, iz)
            gph_map   = INTERPOLATE(Z,    ix, iy, iz)
            u_map     = INTERPOLATE(u,    ix, iy, iz)
            v_map     = INTERPOLATE(v,    ix, iy, iz)
			sh_map    = INTERPOLATE(sh,   ix, iy, iz)
			
			IF ((MAX(o3_data.ozone_daily_max[io3dm],/NAN) GE 70.0)) THEN BEGIN
				o3_storm1_epa		= [o3_storm1_epa, MAX(o3_data.ozone_hourly[io3],/NAN)]
				o3dm_storm1_epa	    = [o3dm_storm1_epa, MAX(o3_data.ozone_daily_max[io3],/NAN)]
				pressure_storm1_epa = [[[pressure_storm1_epa]],[[pressure  [ix0:ix1,iy0:iy1,0]]]]
				temp_storm1_epa	  	= [[[temp_storm1_epa	]],[[temp_map  [ix0:ix1,iy0:iy1]]]]
				Z_storm1_epa	 	= [[[Z_storm1_epa	    ]],[[ gph_map  [ix0:ix1,iy0:iy1]]]]
				u_storm1_epa	 	= [[[u_storm1_epa	    ]],[[ u_map	  [ix0:ix1,iy0:iy1]]]]
				v_storm1_epa		= [[[v_storm1_epa	    ]],[[ v_map	  [ix0:ix1,iy0:iy1]]]]
				sh_storm1_epa		= [[[sh_storm1_epa	    ]],[[sh_map	  [ix0:ix1,iy0:iy1]]]]
				wsp_storm1_epa	    = [[[wsp_storm1_epa	    ]],[[SQRT(u_map^2 + v_map^2)]   ]]
			ENDIF

			IF ((MAX(o3_data.ozone_daily_max[io3dm],/NAN) LT 70.0)) THEN BEGIN
				o3_storm1_nepa		 = [o3_storm1_nepa, MAX(o3_data.ozone_hourly[io3],/NAN)]
				o3dm_storm1_nepa	 = [o3dm_storm1_nepa, MAX(o3_data.ozone_daily_max[io3],/NAN)]
				pressure_storm1_nepa = [[[pressure_storm1_nepa]],[[pressure  [ix0:ix1,iy0:iy1,0]]]]
				temp_storm1_nepa	 = [[[temp_storm1_nepa	  ]],[[temp_map  [ix0:ix1,iy0:iy1]]]]
				Z_storm1_nepa	 	 = [[[Z_storm1_nepa	   	  ]],[[ gph_map  [ix0:ix1,iy0:iy1]]]]
				u_storm1_nepa	 	 = [[[u_storm1_nepa	   	  ]],[[ u_map	[ix0:ix1,iy0:iy1]]]]
				v_storm1_nepa		 = [[[v_storm1_nepa	   	  ]],[[ v_map	[ix0:ix1,iy0:iy1]]]]
				sh_storm1_nepa		 = [[[sh_storm1_nepa	  ]],[[sh_map	[ix0:ix1,iy0:iy1]]]]
				wsp_storm1_nepa	     = [[[wsp_storm1_nepa	  ]],[[SQRT(u_map^2 + v_map^2)]   ]]
			ENDIF
			
			 
   	   ENDIF
   	 ENDFOR
   	 
   	  IF (N_ELEMENTS(SIZE(pressure_nstorm1_epa,/DIMENSIONS)) GT 2) THEN BEGIN
		  o3_nstorm_epa 	  = [o3_nstorm_epa,    o3_nstorm1_epa]
		  o3dm_nstorm_epa 	  = [o3dm_nstorm_epa,o3dm_nstorm1_epa]
		  pressure_nstorm_epa = [[[pressure_nstorm_epa]], [[MEAN(pressure_nstorm1_epa, DIM=3,/NAN)]]]
		  temp_nstorm_epa	  = [[[temp_nstorm_epa]]	 ,[[MEAN(temp_nstorm1_epa	 , DIM=3,/NAN)]]]
		  Z_nstorm_epa	 	  = [[[Z_nstorm_epa]]	 ,	  [[MEAN(Z_nstorm1_epa       , DIM=3,/NAN)]]]
		  u_nstorm_epa	 	  = [[[u_nstorm_epa]]	 ,    [[MEAN(u_nstorm1_epa       , DIM=3,/NAN)]]]
		  v_nstorm_epa		  = [[[v_nstorm_epa]]	 ,    [[MEAN(v_nstorm1_epa       , DIM=3,/NAN)]]]
		  sh_nstorm_epa		  = [[[sh_nstorm_epa]]	 ,    [[MEAN(sh_nstorm1_epa		 , DIM=3,/NAN)]]]
		  wsp_nstorm_epa	  = [[[wsp_nstorm_epa]], 	  [[MEAN(wsp_nstorm1_epa	 , DIM=3,/NAN)]]]     
   	  ENDIF
   	 
   	  IF (N_ELEMENTS(SIZE(pressure_nstorm1_nepa,/DIMENSIONS)) GT 2) THEN BEGIN
		  o3_nstorm_nepa 	  = [o3_nstorm_nepa,o3_nstorm1_nepa]
		  o3dm_nstorm_nepa 	  = [o3dm_nstorm_nepa,o3dm_nstorm1_nepa]
		  pressure_nstorm_nepa = [[[pressure_nstorm_nepa]],[[MEAN(pressure_nstorm1_nepa, DIM=3,/NAN)]]]
		  temp_nstorm_nepa	  = [[[temp_nstorm_nepa]]	 , [[MEAN(temp_nstorm1_nepa	   , DIM=3,/NAN)]]]
		  Z_nstorm_nepa	 	  = [[[Z_nstorm_nepa]]	 ,     [[MEAN(Z_nstorm1_nepa       , DIM=3,/NAN)]]]
		  u_nstorm_nepa	 	  = [[[u_nstorm_nepa]]	 ,     [[MEAN(u_nstorm1_nepa       , DIM=3,/NAN)]]]
		  v_nstorm_nepa		  = [[[v_nstorm_nepa]]	 ,     [[MEAN(v_nstorm1_nepa       , DIM=3,/NAN)]]]
		  sh_nstorm_nepa		  = [[[sh_nstorm_nepa]]	 , [[MEAN(sh_nstorm1_nepa	   , DIM=3,/NAN)]]]
		  wsp_nstorm_nepa	  = [[[wsp_nstorm_nepa]],      [[MEAN(wsp_nstorm1_nepa	   , DIM=3,/NAN)]]]     
   	  ENDIF
   	  
   	  IF (N_ELEMENTS(SIZE(pressure_storm1_epa,/DIMENSIONS)) GT 2) THEN BEGIN
		  o3_storm_epa 	  	  = [o3_storm_epa,o3_storm1_epa]
		  o3dm_storm_epa 	  = [o3dm_storm_epa,o3dm_storm1_epa]
		  pressure_storm_epa  = [[[pressure_storm_epa]],[[MEAN(pressure_storm1_epa, DIM=3,/NAN)]]]
		  temp_storm_epa	  = [[[temp_storm_epa]]	 ,  [[MEAN(temp_storm1_epa	  , DIM=3,/NAN)]]]
		  Z_storm_epa	 	  = [[[Z_storm_epa]]	 ,  [[MEAN(Z_storm1_epa       , DIM=3,/NAN)]]]
		  u_storm_epa	 	  = [[[u_storm_epa]]	 ,  [[MEAN(u_storm1_epa       , DIM=3,/NAN)]]]
		  v_storm_epa		  = [[[v_storm_epa]]	 ,  [[MEAN(v_storm1_epa       , DIM=3,/NAN)]]]
		  sh_storm_epa		  = [[[sh_storm_epa]]	 ,  [[MEAN(sh_storm1_epa	  , DIM=3,/NAN)]]]
		  wsp_storm_epa	  	  = [[[wsp_storm_epa]], 	[[MEAN(wsp_storm1_epa	  , DIM=3,/NAN)]]]     
   	  ENDIF
   	  
   	  IF (N_ELEMENTS(SIZE(pressure_storm1_nepa,/DIMENSIONS)) GT 2) THEN BEGIN
		  o3_storm_nepa 	  = [o3_storm_nepa,o3_storm1_nepa]
		  o3dm_storm_nepa 	  = [o3dm_storm_nepa,o3dm_storm1_nepa]
		  pressure_storm_nepa = [[[pressure_storm_nepa]], [[MEAN(pressure_storm1_nepa, DIM=3,/NAN)]]]
		  temp_storm_nepa	  = [[[temp_storm_nepa]]	 ,[[MEAN(temp_storm1_nepa	 , DIM=3,/NAN)]]]
		  Z_storm_nepa	 	  = [[[Z_storm_nepa]]	 ,    [[MEAN(Z_storm1_nepa       , DIM=3,/NAN)]]]
		  u_storm_nepa	 	  = [[[u_storm_nepa]]	 ,    [[MEAN(u_storm1_nepa       , DIM=3,/NAN)]]]
		  v_storm_nepa		  = [[[v_storm_nepa]]	 ,    [[MEAN(v_storm1_nepa       , DIM=3,/NAN)]]]
		  sh_storm_nepa		  = [[[sh_storm_nepa]]	 ,    [[MEAN(sh_storm1_nepa	 	 , DIM=3,/NAN)]]]
		  wsp_storm_nepa	  = [[[wsp_storm_nepa]], 	  [[MEAN(wsp_storm1_nepa	 , DIM=3,/NAN)]]]     
	  ENDIF

		HELP, Z_storm_epa
ENDFOREACH  


yc = 32.85
xc = 360.0-96.86

x0 = 360.0-105.0
y0 = 22.0
x1 = 360.0-62.73
y1 = 52.5

map_pos1 = [0.05, 0.15, 0.95, 0.95]																			;Set map position
;bar_pos1 = [0.25, 0.10, 0.75, 0.12]																			;Set color bar position

table   = [COLOR_24(200, 200, 200),VISUALIZE_88D_COLOR(0)]												;Set reflectivity color table
levels = FINDGEN(N_ELEMENTS(table))*20 + 500.0

IF KEYWORD_SET(z_buff) THEN BEGIN
	SET_PLOT, 'Z'																									;Output to Z buffer
	DEVICE, SET_PIXEL_DEPTH = 24, SET_RESOLUTION = [wfactor*(dim[0]), wfactor*(dim[1])], $	;Set device resolution and bit depth
		SET_CHARACTER_SIZE = [12, 20]
	!P.COLOR      = COLOR_24('black')																		;Foreground color
	!P.BACKGROUND = COLOR_24('white')																		;Background color
	!P.CHARSIZE   = 1.5																							;Set character size
	!P.FONT       = -1
ENDIF ELSE BEGIN
	IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN	
		PS_ON, FILENAME = epsfile, PAGE_SIZE = [4.0,4.0], MARGIN = 0.0, /INCHES;PAGE_SIZE = 0.001*dim*wfactor			;Switch to Postscript device
		DEVICE, /ENCAPSULATED
		!P.FONT     = 0																								;Hardware fonts
		!P.CHARSIZE = 0.75	
		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
			LOAD_BASIC_COLORS																							;Load basic color definitions
	ENDIF ELSE BEGIN
		SET_PLOT, 'X'
		WINDOW, XSIZE = 1200, YSIZE = 1200										;Open graphics window
		!P.COLOR      = COLOR_24('black')																		;Foreground color
		!P.BACKGROUND = COLOR_24('white')																		;Background color
		!P.CHARSIZE   = 2.0		
		!P.FONT       = -1																							;Use Hershey fonts
	ENDELSE
ENDELSE

!P.MULTI=[0,2,2]

map_pos1 = [0.05, 0.55, 0.45, 0.95]																			;Set map position
map_pos2 = [0.55, 0.55, 0.95, 0.95]																			;Set map position
map_pos3 = [0.05, 0.05, 0.45, 0.45]																			;Set map position
map_pos4 = [0.55, 0.05, 0.95, 0.45]																			;Set map position

map_plot1 = (MEAN(Z_nstorm_nepa,DIM=3,/NAN))
map_plot2 = (MEAN(Z_storm_epa,DIM=3,/NAN))
map_plot3 = (MEAN(Z_nstorm_epa,DIM=3,/NAN))
map_plot4 = (MEAN(Z_storm_nepa,DIM=3,/NAN))

MAP_SET, yc, xc, 0, CONIC = 1, $																;Draw map
	LIMIT     = [y0,x0,y1,x1], $
	ISOTROPIC = 1, $
	TITLE     = 'No Storms/Low O3', $
	ADVANCE	  = 1, $
	POSITION  = map_pos1

;map_plot = MEAN(Z_storm_epa,DIM=3,/NAN) - MEAN(Z_nstorm_nepa,DIM=3,/NAN)
Z_storm =   (MEAN(Z_storm_epa,DIM=3,/NAN)) ;+   MEAN(Z_storm_nepa,DIM=3,/NAN)) / 2
Z_nstorm = (MEAN(Z_nstorm_nepa,DIM=3,/NAN)) ;+  MEAN(Z_nstorm_nepa,DIM=3,/NAN))/2
;Z_epa = (Z_storm_epa + Z_nstorm_epa) / 2
;Z_nepa = (Z_storm_nepa + Z_nstorm_nepa) / 2

;map_plot1 = Z_storm ;- Z_nstorm
;map_plot =  MEAN(Z_storm_epa,DIM=3,/NAN);Z_storm_epa 
;map_plot = wsp_storm_epa[ix0:ix1,iy0:iy1] -wsp_nstorm_nepa[ix0:ix1,iy0:iy1]

;levels = 2.0*FINDGEN(14) -14.0

;levels = 10*FINDGEN(100) + 1450
levels = 40*FINDGEN(25) + 10400
table  = BLUE_RED_24(N_ELEMENTS(levels))

CONTOUR, map_plot1*1.0E-3, x[ix0:ix1], y[iy0:iy1], $												;Contour reflectivity values
	OVERPLOT  = 1, $
	FILL      = 0, $
	LEVELS    = levels*1.0E-3, $
	C_THICK    = 3, $
	C_LABELS   = [1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0, $
					1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0], $ ;,1,0,1,0,1,0,1,0,1,0,1,0,1,0], $
	;				1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0, $
	;				1,0,1,0], $
	C_CHARSIZE = 1.5

MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines

MAP_SET, yc, xc, 0, CONIC = 1, $																;Draw map
	LIMIT     = [y0,x0,y1,x1], $
	ISOTROPIC = 1, $
	TITLE     = 'Storms/High O3', $
	ADVANCE	  = 1, $
	POSITION  = map_pos2

levels = 40*FINDGEN(25) + 10400
table  = BLUE_RED_24(N_ELEMENTS(levels))

CONTOUR, map_plot2*1.0E-3, x[ix0:ix1], y[iy0:iy1], $												;Contour reflectivity values
	OVERPLOT  = 1, $
	FILL      = 0, $
	LEVELS    = levels*1.0E-3, $
	C_THICK    = 3, $
	C_LABELS   = [1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0, $
					1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0], $ ;,1,0,1,0,1,0,1,0,1,0,1,0,1,0], $
	;				1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0, $
	;				1,0,1,0], $
	C_CHARSIZE = 1.5

MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines

MAP_SET, yc, xc, 0, CONIC = 1, $																;Draw map
	LIMIT     = [y0,x0,y1,x1], $
	ISOTROPIC = 1, $
	TITLE     = 'No Storms/High O3', $
	ADVANCE	  = 1, $
	POSITION  = map_pos3

;levels = 10*FINDGEN(100) + 1450
levels = 40*FINDGEN(25) + 10400
table  = BLUE_RED_24(N_ELEMENTS(levels))

CONTOUR, map_plot3*1.0E-3, x[ix0:ix1], y[iy0:iy1], $												;Contour reflectivity values
	OVERPLOT  = 1, $
	FILL      = 0, $
	LEVELS    = levels*1.0E-3, $
	C_THICK    = 3, $
	C_LABELS   = [1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0, $
					1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0], $ ;,1,0,1,0,1,0,1,0,1,0,1,0,1,0], $
	;				1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0, $
	;				1,0,1,0], $
	C_CHARSIZE = 1.5

MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines

MAP_SET, yc, xc, 0, CONIC = 1, $																;Draw map
	LIMIT     = [y0,x0,y1,x1], $
	ISOTROPIC = 1, $
	TITLE     = 'Storms/Low O3', $
	ADVANCE	  = 1, $
	POSITION  = map_pos4

;levels = 10*FINDGEN(100) + 1450
levels = 40*FINDGEN(25) + 10400
table  = BLUE_RED_24(N_ELEMENTS(levels))

CONTOUR, map_plot4*1.0E-3, x[ix0:ix1], y[iy0:iy1], $												;Contour reflectivity values
	OVERPLOT  = 1, $
	FILL      = 0, $
	LEVELS    = levels*1.0E-3, $
	C_THICK    = 3, $
	C_LABELS   = [1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0, $
					1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0], $ ;,1,0,1,0,1,0,1,0,1,0,1,0,1,0], $
	;				1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0, $
	;				1,0,1,0], $
	C_CHARSIZE = 1.5

MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines

STOP

nxy = 60
xreg = MAKEN(MIN(x[ix0:ix1]), MAX(x[ix0:ix1]), nxy)
yreg = MAKEN(MIN(y[iy0:iy1]), MAX(y[iy0:iy1]), nxy)

x1=x[ix0:ix1]
y1=y[iy0:iy1]
;dim = SIZE(MEAN(u_storm_epa,DIM=3,/NAN),/DIMENSIONS)
dim = SIZE(u_storm_epa,/DIMENSIONS)
xgrid = REBIN(x1,dim[0],dim[1],/SAMPLE)
ygrid = REBIN(REFORM(y1,1,dim[1]),dim[0],dim[1],/SAMPLE)

TRIANGULATE, xgrid, ygrid, tri

;u_anom = MEAN(u_storm_epa, DIM=3,/NAN) - MEAN(u_nstorm_nepa, DIM=3,/NAN)
;v_anom = MEAN(v_storm_epa, DIM=3,/NAN) - MEAN(v_nstorm_nepa, DIM=3,/NAN)
u_anom = u_storm_epa; - u_nstorm_nepa
v_anom = v_storm_epa; - v_nstorm_nepa

;ureg = TRIGRID(xgrid, ygrid, u_anom, tri, XOUT = xreg, YOUT = yreg)
;vreg = TRIGRID(xgrid, ygrid, v_anom, tri, XOUT = xreg, YOUT = yreg)
;
;VELOVECT, ureg, vreg, xreg, yreg, OVERPLOT  = 1, LENGTH = 2.0

MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines

STOP
COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
	RANGE = [-28, 28], $
	TICKS = 5, $
	TITLE = 'GPH Anomaly (m)', $
	POSIT = bar_pos



STOP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

IF (N_ELEMENTS(SIZE(pressure_nstorm_epa,/DIMENSIONS)) GT 2) THEN BEGIN
	  o3_nstorm_epa2 	  = [o3_nstorm_epa2,    o3_nstorm_epa]
	  o3dm_nstorm_epa2 	  = [o3dm_nstorm_epa2,o3dm_nstorm_epa]
	  pressure_nstorm_epa2 = [[[pressure_nstorm_epa2]], [[MEAN(pressure_nstorm_epa, DIM=3,/NAN)]]]
	  temp_nstorm_epa2	  = [[[temp_nstorm_epa2]]	 ,[[MEAN(temp_nstorm_epa	 , DIM=3,/NAN)]]]
	  Z_nstorm_epa2	 	  = [[[Z_nstorm_epa2]]	 ,	  [[MEAN(Z_nstorm_epa       , DIM=3,/NAN)]]]
	  u_nstorm_epa2	 	  = [[[u_nstorm_epa2]]	 ,    [[MEAN(u_nstorm_epa       , DIM=3,/NAN)]]]
	  v_nstorm_epa2		  = [[[v_nstorm_epa2]]	 ,    [[MEAN(v_nstorm_epa       , DIM=3,/NAN)]]]
	  sh_nstorm_epa2	  = [[[sh_nstorm_epa2]]	 ,    [[MEAN(sh_nstorm_epa		, DIM=3,/NAN)]]]
	  wsp_nstorm_epa2	  = [[[wsp_nstorm_epa2]], 	  [[MEAN(wsp_nstorm_epa	 	, DIM=3,/NAN)]]]     
 ENDIF

IF (N_ELEMENTS(SIZE(pressure_nstorm_nepa,/DIMENSIONS)) GT 2) THEN BEGIN
	  o3_nstorm_nepa2 	  = [o3_nstorm_nepa2,o3_nstorm_nepa]
	  o3dm_nstorm_nepa2 	  = [o3dm_nstorm_nepa2,o3dm_nstorm_nepa]
	  pressure_nstorm_nepa2 = [[[pressure_nstorm_nepa2]],[[MEAN(pressure_nstorm_nepa, DIM=3,/NAN)]]]
	  temp_nstorm_nepa2	  = [[[temp_nstorm_nepa2]]	 , [[MEAN(temp_nstor1_nepa	   , DIM=3,/NAN)]]]
	  Z_nstorm_nepa2	 	  = [[[Z_nstorm_nepa2]]	 , [[MEAN(Z_nstorm_nepa       , DIM=3,/NAN)]]]
	  u_nstorm_nepa2	 	  = [[[u_nstorm_nepa2]]	 ,     [[MEAN(u_nstorm_nepa       , DIM=3,/NAN)]]]
	  v_nstorm_nepa2		  = [[[v_nstorm_nepa2]]	 ,     [[MEAN(v_nstorm_nepa       , DIM=3,/NAN)]]]
	  sh_nstorm_nepa2		  = [[[sh_nstorm_nepa2]]	 , [[MEAN(sh_nstorm_nepa	   , DIM=3,/NAN)]]]
	  wsp_nstorm_nepa2	  = [[[wsp_nstorm_nepa2]],      [[MEAN(wsp_nstorm_nepa	   , DIM=3,/NAN)]]]     
 ENDIF
 
IF (N_ELEMENTS(SIZE(pressure_storm_epa,/DIMENSIONS)) GT 2) THEN BEGIN
	  o3_storm_epa2 	  	  = [o3_storm_epa2,o3_storm_epa]
	  o3dm_storm_epa2 	  = [o3dm_storm_epa2,o3dm_storm_epa]
	  pressure_storm_epa2  = [[[pressure_storm_epa2]],[[MEAN(pressure_storm_epa, DIM=3,/NAN)]]]
	  temp_storm_epa2	  = [[[temp_storm_epa2]]	 ,  [[MEAN(temp_storm_epa	  , DIM=3,/NAN)]]]
	  Z_storm_epa2	 	  = [[[Z_storm_epa2]]	 ,  [[MEAN(Z_storm_epa       , DIM=3,/NAN)]]]
	  u_storm_epa2	 	  = [[[u_storm_epa2]]	 ,  [[MEAN(u_storm_epa       , DIM=3,/NAN)]]]
	  v_storm_epa2		  = [[[v_storm_epa2]]	 ,  [[MEAN(v_storm_epa       , DIM=3,/NAN)]]]
	  sh_storm_epa2		  = [[[sh_storm_epa2]]	 ,  [[MEAN(sh_storm_epa	  , DIM=3,/NAN)]]]
	  wsp_storm_epa2  	  = [[[wsp_storm_epa2]], 	[[MEAN(wsp_storm_epa	  , DIM=3,/NAN)]]]     
 ENDIF
 
IF (N_ELEMENTS(SIZE(pressure_storm_nepa,/DIMENSIONS)) GT 2) THEN BEGIN
	  o3_storm_nepa2 	  = [o3_storm_nepa2,o3_storm_nepa]
	  o3dm_storm_nepa2 	  = [o3dm_storm_nepa2,o3dm_storm_nepa]
	  pressure_storm_nepa2 = [[[pressure_storm_nepa2]], [[MEAN(pressure_storm_nepa, DIM=3,/NAN)]]]
	  temp_storm_nepa2	  = [[[temp_storm_nepa2]]	 ,[[MEAN(temp_storm_nepa	 , DIM=3,/NAN)]]]
	  Z_storm_nepa2	 	  = [[[Z_storm_nepa2]]	 ,    [[MEAN(Z_storm_nepa       , DIM=3,/NAN)]]]
	  u_storm_nepa2	 	  = [[[u_storm_nepa2]]	 ,    [[MEAN(u_storm_nepa       , DIM=3,/NAN)]]]
	  v_storm_nepa2		  = [[[v_storm_nepa2]]	 ,    [[MEAN(v_storm_nepa       , DIM=3,/NAN)]]]
	  sh_storm_nepa2	  = [[[sh_storm_nepa2]]	 ,    [[MEAN(sh_storm_nepa	 	 , DIM=3,/NAN)]]]
	  wsp_storm_nepa2	  = [[[wsp_storm_nepa2]], 	  [[MEAN(wsp_storm_nepa	 , DIM=3,/NAN)]]]     
 ENDIF


;;Write anomalies to file in case you want to look at different relationships
outfile = 'hourly_reanalysis_091996_102017_SEPT_18Z.nc'
outdir  = !WRF_DIRECTORY + 'general/o3_data/hourly_data/'
FILE_MKDIR, outdir

CATCH, error_status																								;Catch any errors with netcdf control or file creation

IF (error_status NE 0) THEN BEGIN
	NCDF_CLOSE, oid																								;Close previous failed file
	oid = NCDF_CREATE(outdir + outfile, CLOBBER = 1)								;Create output file for writing
ENDIF ELSE $
	oid = NCDF_CREATE(outdir + outfile, CLOBBER = 1)								;Create output file for writing

PRINT, 'start creating variable names'

dim = SIZE(o3_storm_epa, /DIMENSIONS)																			;Get grid dimension sizes
tid = NCDF_DIMDEF(oid, 'storm_epa_Hours', dim[0])																		;Define output dimensions in netCDF file

vid = NCDF_VARDEF(oid, 'O3_storm_epa', [tid], /FLOAT)												;Define the longitude variable
NCDF_ATTPUT, oid, 'O3_storm_epa', 'long_name', 'O3 storm concentration 1996-2017'								;Name attribute
NCDF_ATTPUT, oid, 'O3_storm_epa', 'units',     'ppb'												;Units attribute

vid = NCDF_VARDEF(oid, 'O3DM_storm_epa', [tid], /FLOAT)												;Define the longitude variable
NCDF_ATTPUT, oid, 'O3DM_storm_epa', 'long_name', 'O3 DM storm concentration 1996-2017'								;Name attribute
NCDF_ATTPUT, oid, 'O3DM_storm_epa', 'units',     'ppb'												;Units attribute

PRINT, 'done o3'
dim = SIZE(Z_nstorm_nepa2, /DIMENSIONS)
xid = NCDF_DIMDEF(oid, 'storm_epa_lon', dim[0])																		;Define output dimensions in netCDF file
yid = NCDF_DIMDEF(oid, 'storm_epa_lat' , dim[1])																		;Define output dimensions in netCDF file

PRINT, 'start met'
vid = NCDF_VARDEF(oid, 'Pressure_storm_epa', [xid, yid], /FLOAT)												;Define the longitude variable
NCDF_ATTPUT, oid, 'Pressure_storm_epa', 'long_name', 'Pressure in lowest layer storm 1996-2017'								;Name attribute
NCDF_ATTPUT, oid, 'Pressure_storm_epa', 'units',     'hPa'												;Units attribute

vid = NCDF_VARDEF(oid, 'Temp_storm_epa', [xid, yid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'Temp_storm_epa', 'long_name', 'Temperature at 950 hPa storm 1996-2017'									;Name attribute
NCDF_ATTPUT, oid, 'Temp_storm_epa', 'units',     'deg F'												;Units attribute

vid = NCDF_VARDEF(oid, 'Z_storm_epa', [xid, yid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'Z_storm_epa', 'long_name', 'Geopotential Hgt at 950 hPa storm 1996-2017'									;Name attribute
NCDF_ATTPUT, oid, 'Z_storm_epa', 'units',     'm'												;Units attribute

vid = NCDF_VARDEF(oid, 'U_storm_epa', [xid, yid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'U_storm_epa', 'long_name', 'U wind at 950 hPa storm 1996-2017'									;Name attribute
NCDF_ATTPUT, oid, 'U_storm_epa', 'units',     'm/s'												;Units attribute

vid = NCDF_VARDEF(oid, 'V_storm_epa', [xid, yid], /FLOAT)												;Define the latitVde variable
NCDF_ATTPUT, oid, 'V_storm_epa', 'long_name', 'V wind at 950 hPa storm 1996-2017'									;Name attribVte
NCDF_ATTPUT, oid, 'V_storm_epa', 'Vnits',     'm/s'												;Vnits attribVte

vid = NCDF_VARDEF(oid, 'SH_storm_epa', [xid, yid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'SH_storm_epa', 'long_name', 'SH at 950 hPa storm 1996-2017'									;Name attribute
NCDF_ATTPUT, oid, 'SH_storm_epa', 'units',     '%'												;Units attribute

vid = NCDF_VARDEF(oid, 'WSP_storm_epa', [xid, yid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'WSP_storm_epa', 'long_name', 'WSP at 950 hPa storm 1996-2017'									;Name attribute
NCDF_ATTPUT, oid, 'WSP_storm_epa', 'units',     'm/s'												;Units attribute

PRINT, 'done met epa'
dim = SIZE(o3_nstorm_nepa, /DIMENSIONS)																			;Get grid dimension sizes
tid = NCDF_DIMDEF(oid, 'storm_nepa_Hours', dim[0])																		;Define output dimensions in netCDF file

vid = NCDF_VARDEF(oid, 'O3_storm_nepa', [tid], /FLOAT)												;Define the longitude variable
NCDF_ATTPUT, oid, 'O3_storm_nepa', 'long_name', 'O3 storm concentration 1996-2017'								;Name attribute
NCDF_ATTPUT, oid, 'O3_storm_nepa', 'units',     'ppb'												;Units attribute

vid = NCDF_VARDEF(oid, 'O3DM_storm_nepa', [tid], /FLOAT)												;Define the longitude variable
NCDF_ATTPUT, oid, 'O3DM_storm_nepa', 'long_name', 'O3 DM storm concentration 1996-2017'								;Name attribute
NCDF_ATTPUT, oid, 'O3DM_storm_nepa', 'units',     'ppb'												;Units attribute

PRINT, 'done o3 nepa'
dim = SIZE(Z_nstorm_nepa2, /DIMENSIONS)
xid = NCDF_DIMDEF(oid, 'storm_nepa_lon', dim[0])																		;Define output dimensions in netCDF file
yid = NCDF_DIMDEF(oid, 'storm_nepa_lat' , dim[1])																		;Define output dimensions in netCDF file

vid = NCDF_VARDEF(oid, 'Pressure_storm_nepa', [xid, yid], /FLOAT)												;Define the longitude variable
NCDF_ATTPUT, oid, 'Pressure_storm_nepa', 'long_name', 'Pressure in lowest layer storm 1996-2017'								;Name attribute
NCDF_ATTPUT, oid, 'Pressure_storm_nepa', 'units',     'hPa'												;Units attribute

vid = NCDF_VARDEF(oid, 'Temp_storm_nepa', [xid, yid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'Temp_storm_nepa', 'long_name', 'Temperature at 950 hPa storm 1996-2017'									;Name attribute
NCDF_ATTPUT, oid, 'Temp_storm_nepa', 'units',     'deg F'												;Units attribute

vid = NCDF_VARDEF(oid, 'Z_storm_nepa', [xid, yid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'Z_storm_nepa', 'long_name', 'Geopotential Hgt at 950 hPa storm 1996-2017'									;Name attribute
NCDF_ATTPUT, oid, 'Z_storm_nepa', 'units',     'm'												;Units attribute

vid = NCDF_VARDEF(oid, 'U_storm_nepa', [xid, yid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'U_storm_nepa', 'long_name', 'U wind at 950 hPa storm 1996-2017'									;Name attribute
NCDF_ATTPUT, oid, 'U_storm_nepa', 'units',     'm/s'												;Units attribute

vid = NCDF_VARDEF(oid, 'V_storm_nepa', [xid, yid], /FLOAT)												;Define the latitVde variable
NCDF_ATTPUT, oid, 'V_storm_nepa', 'long_name', 'V wind at 950 hPa storm 1996-2017'									;Name attribVte
NCDF_ATTPUT, oid, 'V_storm_nepa', 'Vnits',     'm/s'												;Vnits attribVte

vid = NCDF_VARDEF(oid, 'SH_storm_nepa', [xid, yid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'SH_storm_nepa', 'long_name', 'SH at 950 hPa storm 1996-2017'									;Name attribute
NCDF_ATTPUT, oid, 'SH_storm_nepa', 'units',     '%'												;Units attribute

vid = NCDF_VARDEF(oid, 'WSP_storm_nepa', [xid, yid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'WSP_storm_nepa', 'long_name', 'WSP at 950 hPa storm 1996-2017'									;Name attribute
NCDF_ATTPUT, oid, 'WSP_storm_nepa', 'units',     'm/s'												;Units attribute

PRINT, 'done met nepa'
dim = SIZE(o3_nstorm_epa, /DIMENSIONS)																			;Get grid dimension sizes
tid = NCDF_DIMDEF(oid, 'nstorm_epa_Hours', dim[0])																		;Define output dimensions in netCDF file

vid = NCDF_VARDEF(oid, 'O3_nstorm_epa', [tid], /FLOAT)												;Define the longitude variable
NCDF_ATTPUT, oid, 'O3_nstorm_epa', 'long_name', 'O3 nstorm concentration 1996-2017'								;Name attribute
NCDF_ATTPUT, oid, 'O3_nstorm_epa', 'units',     'ppb'												;Units attribute

vid = NCDF_VARDEF(oid, 'O3DM_nstorm_epa', [tid], /FLOAT)												;Define the longitude variable
NCDF_ATTPUT, oid, 'O3DM_nstorm_epa', 'long_name', 'O3 DM nstorm concentration 1996-2017'								;Name attribute
NCDF_ATTPUT, oid, 'O3DM_nstorm_epa', 'units',     'ppb'												;Units attribute

dim = SIZE(Z_nstorm_nepa2, /DIMENSIONS)
xid = NCDF_DIMDEF(oid, 'nstorm_epa_lon', dim[0])																		;Define output dimensions in netCDF file
yid = NCDF_DIMDEF(oid, 'nstorm_epa_lat' , dim[1])																		;Define output dimensions in netCDF file

vid = NCDF_VARDEF(oid, 'Pressure_nstorm_epa', [xid, yid], /FLOAT)												;Define the longitude variable
NCDF_ATTPUT, oid, 'Pressure_nstorm_epa', 'long_name', 'Pressure in lowest layer nstorm 1996-2017'								;Name attribute
NCDF_ATTPUT, oid, 'Pressure_nstorm_epa', 'units',     'hPa'												;Units attribute

vid = NCDF_VARDEF(oid, 'Temp_nstorm_epa', [xid, yid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'Temp_nstorm_epa', 'long_name', 'Temperature at 950 hPa nstorm 1996-2017'									;Name attribute
NCDF_ATTPUT, oid, 'Temp_nstorm_epa', 'units',     'deg F'												;Units attribute

vid = NCDF_VARDEF(oid, 'Z_nstorm_epa', [xid, yid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'Z_nstorm_epa', 'long_name', 'Geopotential Hgt at 950 hPa nstorm 1996-2017'									;Name attribute
NCDF_ATTPUT, oid, 'Z_nstorm_epa', 'units',     'm'												;Units attribute

vid = NCDF_VARDEF(oid, 'U_nstorm_epa', [xid, yid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'U_nstorm_epa', 'long_name', 'U wind at 950 hPa nstorm 1996-2017'									;Name attribute
NCDF_ATTPUT, oid, 'U_nstorm_epa', 'units',     'm/s'												;Units attribute

vid = NCDF_VARDEF(oid, 'V_nstorm_epa', [xid, yid], /FLOAT)												;Define the latitVde variable
NCDF_ATTPUT, oid, 'V_nstorm_epa', 'long_name', 'V wind at 950 hPa nstorm 1996-2017'									;Name attribVte
NCDF_ATTPUT, oid, 'V_nstorm_epa', 'Vnits',     'm/s'												;Vnits attribVte

vid = NCDF_VARDEF(oid, 'SH_nstorm_epa', [xid, yid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'SH_nstorm_epa', 'long_name', 'SH at 950 hPa nstorm 1996-2017'									;Name attribute
NCDF_ATTPUT, oid, 'SH_nstorm_epa', 'units',     '%'												;Units attribute

vid = NCDF_VARDEF(oid, 'WSP_nstorm_epa', [xid, yid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'WSP_nstorm_epa', 'long_name', 'WSP at 950 hPa nstorm 1996-2017'									;Name attribute
NCDF_ATTPUT, oid, 'WSP_nstorm_epa', 'units',     'm/s'												;Units attribute


PRINT, 'done nstorm epa'
dim = SIZE(o3_nstorm_nepa, /DIMENSIONS)																			;Get grid dimension sizes
tid = NCDF_DIMDEF(oid, 'nstorm_nepa_Hours', dim[0])																		;Define output dimensions in netCDF file

vid = NCDF_VARDEF(oid, 'O3_nstorm_nepa', [tid], /FLOAT)												;Define the longitude variable
NCDF_ATTPUT, oid, 'O3_nstorm_nepa', 'long_name', 'O3 nstorm concentration 1996-2017'								;Name attribute
NCDF_ATTPUT, oid, 'O3_nstorm_nepa', 'units',     'ppb'												;Units attribute

vid = NCDF_VARDEF(oid, 'O3DM_nstorm_nepa', [tid], /FLOAT)												;Define the longitude variable
NCDF_ATTPUT, oid, 'O3DM_nstorm_nepa', 'long_name', 'O3 DM nstorm concentration 1996-2017'								;Name attribute
NCDF_ATTPUT, oid, 'O3DM_nstorm_nepa', 'units',     'ppb'												;Units attribute

dim = SIZE(Z_nstorm_nepa2, /DIMENSIONS)
xid = NCDF_DIMDEF(oid, 'nstorm_nepa_lon', dim[0])																		;Define output dimensions in netCDF file
yid = NCDF_DIMDEF(oid, 'nstorm_nepa_lat' , dim[1])																		;Define output dimensions in netCDF file

PRINT, 'done o3 nepa'
vid = NCDF_VARDEF(oid, 'Pressure_nstorm_nepa', [xid, yid], /FLOAT)												;Define the longitude variable
NCDF_ATTPUT, oid, 'Pressure_nstorm_nepa', 'long_name', 'Pressure in lowest layer nstorm 1996-2017'								;Name attribute
NCDF_ATTPUT, oid, 'Pressure_nstorm_nepa', 'units',     'hPa'												;Units attribute

vid = NCDF_VARDEF(oid, 'Temp_nstorm_nepa', [xid, yid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'Temp_nstorm_nepa', 'long_name', 'Temperature at 950 hPa nstorm 1996-2017'									;Name attribute
NCDF_ATTPUT, oid, 'Temp_nstorm_nepa', 'units',     'deg F'												;Units attribute

vid = NCDF_VARDEF(oid, 'Z_nstorm_nepa', [xid, yid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'Z_nstorm_nepa', 'long_name', 'Geopotential Hgt at 950 hPa nstorm 1996-2017'									;Name attribute
NCDF_ATTPUT, oid, 'Z_nstorm_nepa', 'units',     'm'												;Units attribute

vid = NCDF_VARDEF(oid, 'U_nstorm_nepa', [xid, yid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'U_nstorm_nepa', 'long_name', 'U wind at 950 hPa nstorm 1996-2017'									;Name attribute
NCDF_ATTPUT, oid, 'U_nstorm_nepa', 'units',     'm/s'												;Units attribute

vid = NCDF_VARDEF(oid, 'V_nstorm_nepa', [xid, yid], /FLOAT)												;Define the latitVde variable
NCDF_ATTPUT, oid, 'V_nstorm_nepa', 'long_name', 'V wind at 950 hPa nstorm 1996-2017'									;Name attribVte
NCDF_ATTPUT, oid, 'V_nstorm_nepa', 'Vnits',     'm/s'												;Vnits attribVte

vid = NCDF_VARDEF(oid, 'SH_nstorm_nepa', [xid, yid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'SH_nstorm_nepa', 'long_name', 'SH at 950 hPa nstorm 1996-2017'									;Name attribute
NCDF_ATTPUT, oid, 'SH_nstorm_nepa', 'units',     '%'												;Units attribute

vid = NCDF_VARDEF(oid, 'WSP_nstorm_nepa', [xid, yid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'WSP_nstorm_nepa', 'long_name', 'WSP at 950 hPa nstorm 1996-2017'									;Name attribute
NCDF_ATTPUT, oid, 'WSP_nstorm_nepa', 'units',     'm/s'												;Units attribute


PRINT, 'done creating variables'
NCDF_CONTROL, oid, /ENDEF

PRINT, 'start assigning variables'

NCDF_VARPUT, oid, 'O3_storm_epa'		, o3_storm_epa2
NCDF_VARPUT, oid, 'O3DM_storm_epa'		, o3dm_storm_epa2
NCDF_VARPUT, oid, 'Pressure_storm_epa'  , pressure_storm_epa2
NCDF_VARPUT, oid, 'Temp_storm_epa'		, temp_storm_epa2
NCDF_VARPUT, oid, 'Z_storm_epa' 		, z_storm_epa2
NCDF_VARPUT, oid, 'U_storm_epa' 		, u_storm_epa2
NCDF_VARPUT, oid, 'V_storm_epa' 		, v_storm_epa2
NCDF_VARPUT, oid, 'SH_storm_epa' 		, sh_storm_epa2
NCDF_VARPUT, oid, 'WSP_storm_epa'  		, wsp_storm_epa2

NCDF_VARPUT, oid, 'O3_storm_nepa'		, o3_storm_nepa2
NCDF_VARPUT, oid, 'O3DM_storm_nepa'		, o3dm_storm_nepa2
NCDF_VARPUT, oid, 'Pressure_storm_nepa' , pressure_storm_nepa2
NCDF_VARPUT, oid, 'Temp_storm_nepa'		, temp_storm_nepa2
NCDF_VARPUT, oid, 'Z_storm_nepa' 		, z_storm_nepa2
NCDF_VARPUT, oid, 'U_storm_nepa' 		, u_storm_nepa2
NCDF_VARPUT, oid, 'V_storm_nepa' 		, v_storm_nepa2
NCDF_VARPUT, oid, 'SH_storm_nepa' 		, sh_storm_nepa2
NCDF_VARPUT, oid, 'WSP_storm_nepa'  	, wsp_storm_nepa2

NCDF_VARPUT, oid, 'O3_nstorm_epa'		, o3_nstorm_epa2
NCDF_VARPUT, oid, 'O3DM_nstorm_epa'		, o3dm_nstorm_epa2
NCDF_VARPUT, oid, 'Pressure_nstorm_epa' , pressure_nstorm_epa2
NCDF_VARPUT, oid, 'Temp_nstorm_epa'		, temp_nstorm_epa2
NCDF_VARPUT, oid, 'Z_nstorm_epa' 		, z_nstorm_epa2
NCDF_VARPUT, oid, 'U_nstorm_epa' 		, u_nstorm_epa2
NCDF_VARPUT, oid, 'V_nstorm_epa' 		, v_nstorm_epa2
NCDF_VARPUT, oid, 'SH_nstorm_epa' 		, sh_nstorm_epa2
NCDF_VARPUT, oid, 'WSP_nstorm_epa'  	, wsp_nstorm_epa2

NCDF_VARPUT, oid, 'O3_nstorm_nepa'		, o3_nstorm_nepa2
NCDF_VARPUT, oid, 'O3DM_nstorm_nepa'	, o3dm_nstorm_nepa2
NCDF_VARPUT, oid, 'Pressure_nstorm_nepa' ,pressure_nstorm_nepa2
NCDF_VARPUT, oid, 'Temp_nstorm_nepa'	, temp_nstorm_nepa2
NCDF_VARPUT, oid, 'Z_nstorm_nepa' 		, z_nstorm_nepa2
NCDF_VARPUT, oid, 'U_nstorm_nepa' 		, u_nstorm_nepa2
NCDF_VARPUT, oid, 'V_nstorm_nepa' 		, v_nstorm_nepa2
NCDF_VARPUT, oid, 'SH_nstorm_nepa' 		, sh_nstorm_nepa2
NCDF_VARPUT, oid, 'WSP_nstorm_nepa'  	, wsp_nstorm_nepa2

PRINT, 'done assigning variables'

NCDF_CLOSE, oid																									;Close output file

PRINT, 'File ' + outfile + ' processed.' 

STOP


p1 = SCATTERPLOT(o3_array, wsp_array, $
	TITLE  = 'O3 vs Wind Speed Anomalies: ' + STRTRIM(hour,1) + 'Z', $
	YTITLE = 'Wind Speed Anomalies (m/s)', $ 
	XTITLE = 'O3 Anomalies (ppb)', $
	YRANGE = [-10,10], $
	XRANGE = [-40,20])
;	XRANGE = [-40,80])
	;XRANGE = [-60,60])

l1 = LINFIT(o3_array, wsp_array)
;xplot = FINDGEN(120)-40
xplot = FINDGEN(80)-40
yplot = l1[0] + (l1[1])*xplot

trend = PLOT(xplot, yplot, $
		/OVERPLOT, $
		COLOR = COLOR_24('red'), $
		THICK=2, $
		LINESTYLE=3, $
;		xrange = [-35,80])
		XRANGE = [-40,20])
;zero = PLOT([-40,80],[0,0], /OVERPLOT, THICK=2)
zero = PLOT([-40,20],[0,0], /OVERPLOT, THICK=2)

PRINT, 'O3-Wind Correlation: ' + STRING(CORRELATE(o3_array, wsp_array))
PRINT, 'O3-Wind Covariance: ' + STRING(CORRELATE(o3_array, wsp_array,/COVARIANCE))
 
p2 = SCATTERPLOT( o3_array, temp_array, $
	TITLE  = 'O3 vs Temperature Anomalies: ' + STRTRIM(hour,1) + 'Z', $
	YTITLE = 'Temperature Anomalies (K)', $ 
	XTITLE = 'O3 Anomalies (ppb)', $
	YRANGE = [-25,25], $
	XRANGE = [-40,20])

l2 = LINFIT(o3_array, temp_array)
xplot2 = FINDGEN(80)-40
yplot2 = l2[0] + (l2[1])*xplot2

trend = PLOT(xplot2, yplot2, $
		/OVERPLOT, $
		COLOR = COLOR_24('red'), $
		THICK=2, $
		LINESTYLE=3, $
		xrange = [-40,20])

zero = PLOT([-40,20],[0,0], /OVERPLOT, THICK=2)

PRINT, 'O3-Temp Correlation: ' + STRING(CORRELATE(o3_array, temp_array))
PRINT, 'O3-Temp Covariance: ' + STRING(CORRELATE(o3_array, temp_array,/COVARIANCE))

p3= SCATTERPLOT(o3_array, gph_array, $
	TITLE  = 'O3 vs GPH Anomalies: ' + STRTRIM(hour,1) + 'Z', $
	YTITLE = 'Geopotential Height Anomalies (m)', $ 
	XTITLE = 'O3 Anomalies (ppb)', $
	YRANGE = [-100,100], $
	XRANGE = [-40,20])

l3 = LINFIT(o3_array, gph_array)
xplot3 = FINDGEN(80)-40
yplot3 = l3[0] + (l3[1])*xplot3

trend3 = PLOT(xplot3, yplot3, $
		/OVERPLOT, $
		COLOR = COLOR_24('red'), $
		THICK=2, $
		LINESTYLE=3, $
		xrange = [-40,20])

zero = PLOT([-40,20],[0,0], /OVERPLOT, THICK=2)

PRINT, 'O3-GPH Correlation: ' + STRING(CORRELATE(o3_array, gph_array))
PRINT, 'O3-GPH Covariance: ' + STRING(CORRELATE(o3_array,  gph_array,/COVARIANCE))



END
