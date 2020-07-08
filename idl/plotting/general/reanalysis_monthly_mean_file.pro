PRO REANALYSIS_MONTHLY_MEAN_FILE, hour, reanalysis, lev, $
	PNG		  = png, $
	EPS		  = eps, $
	CLOBBER   = clobber

;+
;NAME:
;     PLOT_DFW_ANOMALIES
;PURPOSE:
;     This copies variables from ERA-Interim analysis into a single
;     file in pressure coordinates for use in TRAJ3D.
;     W at the surface is set to zero.  One pressure level is added at 
;     the top of the domain (p = 0), where w is also set to zero.
;CATEGORY:
;     Data handling utility.
;CALLING SEQUENCE:
;     PLOT_DFW_ANOMALIES, date0, outfile
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
;		D. Phoenix:       2019-02-13.
;-

COMPILE_OPT IDL2																									;Set compile options

IF (N_ELEMENTS(lev) EQ 0) THEN lev = 950.0
reanalysis = 'ERA_Interim'

dallas_x = (360.0-96.86)
dallas_y = 32.85

x = FINDGEN(360)
y = FINDGEN(181)-90.0

x0 = dallas_x - 1.0
y0 = dallas_y - 1.0
x1 = dallas_x + 1.0
y1 = dallas_y + 1.0

ix0 = WHERE(ROUND(x0) EQ x)
ix1 = WHERE(ROUND(x1) EQ x) 
iy0 = WHERE(ROUND(y0) EQ y)
iy1 = WHERE(ROUND(y1) EQ y) 

ts_data = READ_HURDAT2()

;Truncate data to only look at years with 00,06,12,18Z hours
imonth = WHERE(((ts_data.date.year GE '1996') AND (ts_data.date.year LT '2017')) AND $
	((ts_data.date.month GE 7) AND (ts_data.date.month LT 12)) AND (ts_data.date.hour EQ hour))

extr_storms = WHERE(((ts_data.class[imonth] EQ 'TD') OR (ts_data.class[imonth] EQ 'TS') OR $
    (ts_data.class[imonth] EQ 'HU')), istorms)
iperiod = imonth[extr_storms]

nt = N_ELEMENTS(ts_data.date[iperiod])

years  = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),0,4) 
months = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),5,2) 
days   = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),8,2) 
hours  = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),11,2)

ibad = WHERE((ts_data.date[iperiod] NE 00) OR (ts_data.date[iperiod] NE 06) OR $
		(ts_data.date[iperiod] NE 12) OR (ts_data.date[iperiod] NE 18))
		
;TS data has occasional non-00,06,12,18 hours. Remove those to be compatible with
;reanalysis data
REMOVE, ibad,  years
REMOVE, ibad, months
REMOVE, ibad,   days
REMOVE, ibad,  hours

;From Reanalysis monthly mean files, create month mean for entire period
mth_arr = ['07','08','09','10','11']
;mth_arr=['08','09','10']
;day_arr=[31,30,31]
day_arr = [31,  31,  30,  31,  30]
;hr_arr  = ['00'];,'06','12','18'	   ]
hr_arr = [hour]

;yr_arr = ['1980','1981','1982','1983','1984','1985','1986','1987','1988','1989','1990', $
;			'1991','1992','1993','1994','1995','1996','1997','1998','1999','2000', $
;			'2001','2002','2003','2004','2005','2006','2007','2008','2009','2010', $
;			'2011','2012','2013','2014','2015','2016','2017']

yr_arr = ['1996','1997','1998','1999','2000','2001','2002','2003','2004','2005','2006',$
		'2007','2008','2009','2010','2011','2012','2013','2014','2015','2016']
;yr_arr=['2011','2012','2013']

m=0		
ny = N_ELEMENTS(yr_arr)
mt = N_ELEMENTS(mth_arr)

month_p    =FLTARR(mt)
month_gph  =FLTARR(mt)
month_u    =FLTARR(mt)
month_v    =FLTARR(mt)
month_temp =FLTARR(mt)
month_o3   =FLTARR(mt)
month_wsp  =FLTARR(mt)
month_sh   =FLTARR(mt)
month_o3max=FLTARR(mt)

pressure1 = []
temp1     = []
z1        = []
u1        = []
v1        = []
o31       = []
o3max	  = []
wsp       = []
sh1		  = []

FOREACH month, mth_arr DO BEGIN
    ;re-zero arrays for each month
    ym_press = []
    ym_temp  = []
    ym_z     = []
    ym_u     = []
    ym_v     = []
    ym_o3    = []
    ym_wsp	 = []
	ym_sh	 = []
	ym_o3max = []
			
	FOREACH year, yr_arr DO BEGIN
        ;re-zero arrays for each year
        dm_press = []
        dm_temp  = []
        dm_z     = []
        dm_u     = []
        dm_v     = []
        dm_o3    = []
        dm_wsp	 = []
		dm_sh	 = []
		dm_o3max = []
		
		o3_data = OZONE_DALLAS_NAAQS(year)
    	o3_datestr = STRMID(o3_data.date,0,4)+STRMID(o3_data.date,5,2)+STRMID(o3_data.date,8,2) + $
    				  STRMID(o3_data.time,0,2)+STRMID(o3_data.time,3,2) 

		FOR d = 0, day_arr[m]-1 DO BEGIN
   			    date1 = MAKE_DATE(year,month,STRING(d+1),hour);hours[i])
		   		io3 = WHERE((STRMID(MAKE_ISO_DATE_STRING(date1,/COMPACT),0,8) + $
    				STRMID(MAKE_ISO_DATE_STRING(date1,/COMPACT),9,4)) EQ o3_datestr)
    	        
    	        io3dm = WHERE((STRMID(MAKE_ISO_DATE_STRING(date1,/COMPACT),0,8) + $
    				STRMID(MAKE_ISO_DATE_STRING(date1,/COMPACT),9,4)) EQ o3_data.ozone_datestr)
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
				
                ;no difference between these two since not looping over hour
                pressure1 = [ pressure1,MEAN(pressure  [ix0:ix1,iy0:iy1,0],  /NAN)]
                temp1	  = [ temp1	   ,MEAN(temp_map	 [ix0:ix1,iy0:iy1],  /NAN)]
                Z1	 	  = [ Z1	   ,MEAN( gph_map	 [ix0:ix1,iy0:iy1],  /NAN)]
                u1	 	  = [ u1	   ,MEAN( u_map	 	 [ix0:ix1,iy0:iy1],  /NAN)]
                v1		  = [ v1	   ,MEAN( v_map		 [ix0:ix1,iy0:iy1],  /NAN)]
        		sh1		  = [ sh1	   ,MEAN(sh_map		 [ix0:ix1,iy0:iy1],  /NAN)]
        	    o31 	  = [ o31	   ,MEAN(o3_data.ozone_8hr_total[io3], /NAN)] 
        	    o3max	  = [o3max 	   ,MAX(o3_data.ozone_daily_max[io3dm],/NAN)]	 
        	    wsp	      = [ wsp	   ,SQRT(u1^2 + v1^2)					    ]     
        	       	
        	    ;create daily mean
        	    dm_press = [dm_press, MEAN(pressure1,/NAN)]
        	    dm_temp  = [dm_temp,  MEAN(temp1,/NAN)    ]
        	    dm_z     = [dm_z, 	  MEAN(Z1,/NAN)       ]
        	    dm_u     = [dm_u, 	  MEAN(u1,/NAN)       ]
        	    dm_v     = [dm_u,	  MEAN(v1,/NAN)       ]
        	    dm_o3    = [dm_o3,    MEAN(o31,/NAN)	  ]
        	    dm_wsp   = [dm_wsp,   MEAN(wsp,/NAN)	  ]
        	    dm_sh	 = [dm_sh,    MEAN(sh1,/NAN)	  ]
        	    dm_o3max = [dm_o3max, MAX(o3max,/NAN)	  ]
		ENDFOR
        ;create yearly mean
        ym_press = [ym_press, MEAN(dm_press,/NAN)]
        ym_temp  = [ym_temp,  MEAN(dm_temp,/NAN )]
        ym_z     = [ym_z, 	  MEAN(dm_z,/NAN)    ]
        ym_u     = [ym_u, 	  MEAN(dm_u,/NAN)    ]
        ym_v     = [ym_u,	  MEAN(dm_v,/NAN)    ]
        ym_o3    = [ym_o3,    MEAN(dm_o3,/NAN)   ]
		ym_wsp   = [ym_wsp,   MEAN(dm_wsp,/NAN)  ]
		ym_sh	 = [ym_sh,	  MEAN(dm_sh,/NAN)   ]
		ym_o3max = [ym_o3max, MEAN(dm_o3max,/NAN)]
	ENDFOREACH
	;create monthly mean
	month_p    [m]= MEAN(ym_press,/NAN)
    month_temp [m]= MEAN(ym_temp ,/NAN)
    month_u    [m]= MEAN(ym_u    ,/NAN)
    month_v    [m]= MEAN(ym_v    ,/NAN)
    month_gph  [m]= MEAN(ym_z    ,/NAN)
    month_o3   [m]= MEAN(ym_o3   ,/NAN)
    month_wsp  [m]= MEAN(ym_wsp  ,/NAN)
    month_sh   [m]= MEAN(ym_sh   ,/NAN)
    month_o3max[m]= MEAN(ym_o3max,/NAN)
    m+=1
ENDFOREACH
;;; Normally should come back here and write an output file

;time    = MAKE_ISO_DATE_STRING(date, PRECISION='minute', /COMPACT, /UTC)
outfile = 'monthly_mean_' + hour + 'Z_' + reanalysis + '1.nc'
outdir  = !WRF_DIRECTORY + 'general/monthly_reanalysis/'
FILE_MKDIR, outdir

;iid = NCDF_OPEN(infile)																						;Open input file for reading
;NCDF_VARGET, iid, 'T', values																				;Read single variable for output file definition
;NCDF_ATTGET, iid, 'DX', dx, /GLOBAL																		;Read grid resolution
;NCDF_ATTGET, iid, 'DT', dt, /GLOBAL																		;Read grid resolution

dim = SIZE(month_gph, /DIMENSIONS)																			;Get grid dimension sizes
CATCH, error_status																								;Catch any errors with netcdf control or file creation

IF (error_status NE 0) THEN BEGIN
	NCDF_CLOSE, oid																								;Close previous failed file
	oid = NCDF_CREATE(outdir + outfile, CLOBBER = 1)								;Create output file for writing
ENDIF ELSE $
	oid = NCDF_CREATE(outdir + outfile, CLOBBER = 1)								;Create output file for writing

tid = NCDF_DIMDEF(oid, 'Month', dim[0])																		;Define output dimensions in netCDF file

PRINT, 'start creating variable names'
vid = NCDF_VARDEF(oid, 'PSFC', [tid], /FLOAT)												;Define the longitude variable
NCDF_ATTPUT, oid, 'PSFC', 'long_name', 'Pressure in the lowest model level'								;Name attribute
NCDF_ATTPUT, oid, 'PSFC', 'units',     'hPa'												;Units attribute

vid = NCDF_VARDEF(oid, 'GPH', [tid], /FLOAT)												;Define the longitude variable
NCDF_ATTPUT, oid, 'GPH', 'long_name', 'Geopotential Height at 950 hPa'								;Name attribute
NCDF_ATTPUT, oid, 'GPH', 'units',     'm'												;Units attribute

vid = NCDF_VARDEF(oid, 'Temperature', [tid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'Temperature', 'long_name', 'Temperature at 950 hPa'									;Name attribute
NCDF_ATTPUT, oid, 'Temperature', 'units',     'K'												;Units attribute

vid = NCDF_VARDEF(oid, 'U', [tid], /FLOAT)													;Define the pressure variable
NCDF_ATTPUT, oid, 'U', 'long_name', 'U-component of wind at 950 hPa'														;Name attribute
NCDF_ATTPUT, oid, 'U', 'units',     'm/s'																	;Units attribute

vid = NCDF_VARDEF(oid, 'V', [tid], /FLOAT)													;Define the pressure variable
NCDF_ATTPUT, oid, 'V', 'long_name', 'V-component of wind at 950 hPa'														;Name attribute
NCDF_ATTPUT, oid, 'V', 'units',     'm/s'																	;Units attribute

vid = NCDF_VARDEF(oid, 'O3', [tid], /FLOAT)													;Define the geopotential height variable
NCDF_ATTPUT, oid, 'O3', 'long_name', 'Surface Ozone'											;Name attribute
NCDF_ATTPUT, oid, 'O3', 'units',     'ppb'																	;Units attribute

vid = NCDF_VARDEF(oid, 'O3_Daily_Max', [tid], /FLOAT)													;Define the geopotential height variable
NCDF_ATTPUT, oid, 'O3_Daily_Max', 'long_name', 'Daily Max Surface O3'											;Name attribute
NCDF_ATTPUT, oid, 'O3_Daily_Max', 'units',     'ppb'																	;Units attribute

vid = NCDF_VARDEF(oid, 'WSPD', [tid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'WSPD', 'long_name', 'Wind Speed at 950 hPa'									;Name attribute
NCDF_ATTPUT, oid, 'WSPD', 'units',     'm/s'												;Units attribute

vid = NCDF_VARDEF(oid, 'SH', [tid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'SH', 'long_name', 'Specific Humidity at 950 hPa'									;Name attribute
NCDF_ATTPUT, oid, 'SH', 'units',     '%'												;Units attribute

PRINT, 'done creating variables'
NCDF_CONTROL, oid, /ENDEF

PRINT, 'start assigning variables'
NCDF_VARPUT, oid, 'PSFC'		, month_p
NCDF_VARPUT, oid, 'GPH'   		, month_gph
NCDF_VARPUT, oid, 'Temperature' , month_temp
NCDF_VARPUT, oid, 'U'  			, month_u
NCDF_VARPUT, oid, 'V'   	    , month_v
NCDF_VARPUT, oid, 'O3'			, month_o3
NCDF_VARPUT, oid, 'O3_Daily_Max', month_o3max
NCDF_VARPUT, oid, 'WSPD'        , month_wsp
NCDF_VARPUT, oid, 'SH'   		, month_sh
PRINT, 'done assigning variables'

NCDF_CLOSE, oid																									;Close output file
;NCDF_CLOSE, iid																									;Close input file

PRINT, 'File ' + outfile + ' processed.' 

STOP
;Now for each year,month,day,hour compute the anomaly during storm track periods

v_array  = [ ]
o3_array = [ ]
sh_array = [ ]
u_array = []
gph_array=[]
temp_array=[]
;pres_array=[]
wsp_array =[]

;re-zero these arrays
;pressure1 = []
temp1     = []
z1        = []
u1        = []
v1        = []
sh1		  = []
o31       = []
wsp       = []

nt=N_ELEMENTS(years)
FOR m=0, nt-1 DO BEGIN
    date1 = MAKE_DATE(years[m],months[m],days[m],hour);hours[i])
    PRINT, date1
    ;IF (MAKE_ISO_DATE_STRING(date1) EQ (MAKE_ISO_DATE_STRING(date2))) THEN BREAK
    pressure  = (ERA_INTERIM_READ_VAR('P' ,date1)).values
    temp	  = (ERA_INTERIM_READ_VAR('T' ,date1)).values
    Z	 	  = (ERA_INTERIM_READ_VAR('Z' ,date1)).values
    u	 	  = (ERA_INTERIM_READ_VAR('u' ,date1)).values
    v		  = (ERA_INTERIM_READ_VAR('v' ,date1)).values
   
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
	
	;pressure1 = MEAN(pressure[ix0:ix1,iy0:iy1,4],/NAN)
    temp1     = MEAN(temp_map    [ix0:ix1,iy0:iy1],/NAN)
    z1        = MEAN( gph_map    [ix0:ix1,iy0:iy1],/NAN)
    u1        = MEAN(   u_map    [ix0:ix1,iy0:iy1],/NAN)
    v1        = MEAN(   v_map    [ix0:ix1,iy0:iy1],/NAN)
    sh1       = MEAN(  sh_map    [ix0:ix1,iy0:iy1],/NAN)
    wsp		  = SQRT(u1^2 + v1^2)
    
    ;Find anomaly for each hour
    mth = WHERE(months[m] EQ mth_arr)
	v_anom  = (v1 - month_v[mth])
	v_array = [v_array, v_anom]     

	u_anom  = (u1 - month_u[mth])
	u_array = [u_array, u_anom]     

	sh_anom  = (sh1 - month_sh[mth])
	sh_array = [sh_array, sh_anom]     

	gph_anom  = (z1 - month_gph[mth])
	gph_array = [gph_array, gph_anom]     

	temp_anom  = (temp1 - month_temp[mth])
	temp_array = [temp_array, temp_anom]     
 
 	;pres_anom  = (pressure1 - month_p[mth])
	;pres_array = [pres_array, pres_anom]   
	
	wsp_anom  = (wsp - month_wsp[mth])  
    wsp_array = [wsp_array, wsp_anom]
   
    ;Save o3 concentration for each hour
    IF (m EQ 0) THEN BEGIN
    	year 	 = date1.year
		o3_data  = OZONE_DALLAS_NAAQS(STRTRIM(year,1))	
	ENDIF
	IF (m GT 0) AND (years[m] NE years[m-1]) THEN BEGIN
		year = years[m]
		o3_data  = OZONE_DALLAS_NAAQS(STRTRIM(year,1))
	ENDIF

	o3_8hr   = o3_data.ozone_8hr_total
    o3_datestr = STRMID(o3_data.date,0,4)+STRMID(o3_data.date,5,2)+STRMID(o3_data.date,8,2) + $
    				  STRMID(o3_data.time,0,2)+STRMID(o3_data.time,3,2) 

    io3 = WHERE((STRMID(MAKE_ISO_DATE_STRING(date1,/COMPACT),0,8) + $
    	STRMID(MAKE_ISO_DATE_STRING(date1,/COMPACT),9,4)) EQ o3_datestr)
    
    PRINT, MAX(o3_8hr[io3],/NAN)
    o3_anom  = MAX(o3_8hr[io3],/NAN) - month_o3[mth]
    o3_array = [o3_array, o3_anom]
ENDFOR   

STOP
	filename = MAKE_ISO_DATE_STRING(date1,/COMPACT,/UTC)
	outdir = '/data3/dphoenix/wrf/general/reanalysis_plots/o3_wind/'
	FILE_MKDIR, outdir
	pngfile = outdir + 'o3_wind_' + filename + '.png' 

    IF ~KEYWORD_SET(nowindow) THEN BEGIN
    	IF KEYWORD_SET(eps) THEN BEGIN
    		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
    			LOAD_BASIC_COLORS, /RESET															;Reset color table to linear ramp
    		PS_OFF																					;Turn PS off
    	ENDIF ELSE IF KEYWORD_SET(png) THEN $
    		WRITE_PNG, pngfile, TVRD(TRUE=1)														;Write PNG image
    ENDIF

	PRINT, date1
	date1 = TIME_INC(date1, 21600)

END
