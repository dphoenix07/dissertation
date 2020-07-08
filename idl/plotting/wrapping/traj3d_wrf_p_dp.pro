PRO TRAJ3D_WRF_P_DP, $
        date1,date2, experiment, state, $
        DOMAIN  = domain, $
	CLOBBER   = clobber

;+
;NAME:
;     TRAJ3D_RAP_P
;PURPOSE:
;     This copies variables from ERA-Interim analysis into a single
;     file in pressure coordinates for use in TRAJ3D.
;     W at the surface is set to zero.  One pressure level is added at 
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
;     DIRECTORY : Output directory for wind file.
;		CLOBBER   : If set, overwrite existing file. This is the default.
;OUTPUT:
;     Netcdf file.
;MODIFICATION HISTORY:
;		C. Homeyer:       2015-06-22.
;-

COMPILE_OPT IDL2																									;Set compile options


IF (N_ELEMENTS(clobber  ) EQ 0) THEN clobber   = 1								;Default is clobber old file

dt              = 300 
date_string1 = MAKE_ISO_DATE_STRING(date1, PREC='MINUTE', /COMPACT, /UTC)  
date_string2 = MAKE_ISO_DATE_STRING(date2, PREC='MINUTE', /COMPACT, /UTC)

IF (N_ELEMENTS(directory) EQ 0) THEN outdir    = '/data3/dphoenix/wrf/20120530_ncar/d03_30km_icloud/winds/'			;Default output directory for wind file
IF (N_ELEMENTS(outfile) EQ 0) THEN outfile = outdir + 'wrf_winds.nc'

PRINT, outfile

data_domain = 'REGIONAL'						;Data domain (Regional or Global)
data_source = 'WRF FORECAST'						;Data source

x_name      = 'Longitude'						;Name of x-coordinate
y_name      = 'Latitude'						;Name of y-coordinate
z_name      = 'Altitude'						;Name of z-coordinate

;dx          = 0.025                                                      ;Lon delta matches your input resolution: 0.03 degrees for a 3-km domain
;x_regular   =     1											;Set x-coordinate spacing flag
;x_periodic  =     0												;Set x-coordinate periodicity flag
;;x_min       = 264.
;;x_max       = 291.
;x_min 		=  252.1
;x_max		=  274.1
;nx          = LONG((x_max-x_min)/dx) + 1
;xout        = x_min + dx*FINDGEN(nx)
;
;dy          =   0.025                                                    ;Lat delta matches your input resolution: 0.03 degrees for a 3-km domain
;y_regular   =     1												;Set y-coordinate spacing flag
;y_periodic  =     0												;Set y-coordinate periodicity flag
;;y_min       = 29.0
;y_min       =  32.9																							;Set minimum of y-coordinate domain
;y_max       =  42.9																							;Set maximum of y-coordinate domain
;ny          = LONG((y_max-y_min)/dy) + 1
;yout        = y_min + dy*FINDGEN(ny)

IF (state EQ 'd02_30km') THEN BEGIN
	date1       =  MAKE_DATE(2012, 5, 30, 20, 00)
	date2       =  MAKE_DATE(2012, 5, 31, 06, 00)
	dx          =  0.025
	dy          =  0.025
	x_min 		=  252.1
	x_max		=  274.1
	y_min       =  32.9																							;Set minimum of y-coordinate domain
	y_max       =  42.9																							;Set maximum of y-coordinate domain
ENDIF

IF (state EQ 'd03_30km_icloud') THEN BEGIN
	date1   	=  MAKE_DATE(2012, 5, 30, 20, 30)
	date2   	=  MAKE_DATE(2012, 5, 31, 03, 00)
	dx 			=  0.005
	dy 			=  0.005
	x_min       =  256.3																						;Set minimum of x-coordinate domain
	x_max       =  269.4																						;Set maximum of x-coordinate domain
	y_min		=   36.1
	y_max		=   40.2
ENDIF

x_regular   =     1																								;Set x-coordinate spacing flag
x_periodic  =     0																								;Set x-coordinate periodicity flag
nx	        = LONG((x_max-x_min)/dx) + 1
xout        = x_min + dx*FINDGEN(nx)

y_regular   =     1																								;Set y-coordinate spacing flag
y_periodic  =     0																								;Set y-coordinate periodicity flag
ny          = LONG((y_max-y_min)/dy) + 1
yout        = y_min + dy*FINDGEN(ny)

dz          =   200.0                                                   ; A regular altitude grid of 200 m resolution
z_regular   =     1							  				;Set z-coordinate spacing flag
z_periodic  =     0											;Set z-coordinate periodicity flag
z_min       =     0.0
z_max       = 31900.0
nz_out	    = LONG((z_max-z_min)/dz) + 1								;Number of output altitude levels
z_out	    = z_min + dz*FINDGEN(nz_out)							;Create altitude array

PRINT, 'From           : ', MAKE_ISO_DATE_STRING(date1)
PRINT, 'To             : ', MAKE_ISO_DATE_STRING(date2)
PRINT, 'Output file    : ', outfile
PRINT, 'nx             : ', nx, ' points'
PRINT, 'ny             : ', ny, ' points'
PRINT, 'nz             : ', nz_out, ' points'

PRINT, '[x_min, x_max] : [', x_min, ', ', x_max, ']'
PRINT, '[y_min, y_max] : [', y_min, ', ', y_max, ']'

oid = NCDF_CREATE(outfile, CLOBBER = clobber, /NETCDF3_64BIT)								;Create the netCDF file
NCDF_ATTPUT, oid, 'data_domain', data_domain, /GLOBAL							;Create global attribute for use by TRAJ3D
NCDF_ATTPUT, oid, 'data_source', data_source, /GLOBAL							;Create global attribute for use by TRAJ3D

xid = NCDF_DIMDEF(oid, x_name,     nx       )								;Define the longitude dimension
yid = NCDF_DIMDEF(oid, y_name,     ny       )								;Define the latitude dimension
zid = NCDF_DIMDEF(oid, z_name,     nz_out    )								;Define the altitude dimension
tid = NCDF_DIMDEF(oid, 'Time',     /UNLIMITED)								;Define the time dimension

vid = NCDF_VARDEF(oid, x_name, xid, /FLOAT)								;Define the longitude variable
NCDF_ATTPUT, oid, x_name, 'units',     'degrees'							;Write units
NCDF_ATTPUT, oid, x_name, 'long_name', 'Longitude'							;Write long name
NCDF_ATTPUT, oid, x_name, 'regular',	x_regular							;Write coordinate spacing flag
NCDF_ATTPUT, oid, x_name, 'periodic',	x_periodic							;Write coordinate periodicity flag
NCDF_ATTPUT, oid, x_name, 'min',			x_min						;Write minimum of coordinate domain
NCDF_ATTPUT, oid, x_name, 'max',			x_max						;Write maximum of coordinate domain

vid = NCDF_VARDEF(oid, y_name, yid, /FLOAT)								;Define the latitude variable
NCDF_ATTPUT, oid, y_name, 'units',     'degrees'							;Write units
NCDF_ATTPUT, oid, y_name, 'long_name', 'Latitude'							;Write long name
NCDF_ATTPUT, oid, y_name, 'regular',	y_regular							;Write coordinate spacing flag
NCDF_ATTPUT, oid, y_name, 'periodic',	y_periodic							;Write coordinate periodicity flag
NCDF_ATTPUT, oid, y_name, 'min',			y_min						;Write minimum of coordinate domain
NCDF_ATTPUT, oid, y_name, 'max',			y_max						;Write maximum of coordinate domain

vid = NCDF_VARDEF(oid, z_name, zid, /FLOAT)								;Define the altitude variable
NCDF_ATTPUT, oid, z_name, 'units',			'm'						;Write units
NCDF_ATTPUT, oid, z_name, 'long_name',	'Altitude'							;Write long name
NCDF_ATTPUT, oid, z_name, 'regular',		z_regular						;Write coordinate spacing flag
NCDF_ATTPUT, oid, z_name, 'periodic',		z_periodic						;Write coordinate periodicity flag
NCDF_ATTPUT, oid, z_name, 'min',			z_min						;Write minimum of coordinate domain
NCDF_ATTPUT, oid, z_name, 'max',			z_max						;Write maximum of coordinate domain

vid = NCDF_VARDEF(oid, 'Time_interval',      /LONG)							;Define the time interval variable
NCDF_ATTPUT, oid, 'Time_interval', 'units',     's'
NCDF_ATTPUT, oid, 'Time_interval', 'long_name', 'Time interval between data samples'

vid = NCDF_VARDEF(oid, 'Julian_day'   , tid, /LONG)							;Define the Julian day variable
NCDF_ATTPUT, oid, 'Julian_day'   , 'units',     'day'
NCDF_ATTPUT, oid, 'Julian_day'   , 'long_name', 'Julian day'

vid = NCDF_VARDEF(oid, 'Seconds'      , tid, /LONG)							;Define the seconds variable
NCDF_ATTPUT, oid, 'Seconds'      , 'units',     's'
NCDF_ATTPUT, oid, 'Seconds'      , 'long_name', 'Time of day in seconds'

vid = NCDF_VARDEF(oid, 'u', [xid, yid, zid, tid], /FLOAT)						;Define variable
NCDF_ATTPUT, oid, 'u', 'units',		'm/s'																	;Write units
NCDF_ATTPUT, oid, 'u', 'long_name',	'Zonal wind speed'						;Write long name

vid = NCDF_VARDEF(oid, 'v', [xid, yid, zid, tid], /FLOAT)						;Define variable
NCDF_ATTPUT, oid, 'v', 'units',		'm/s'																	;Write units
NCDF_ATTPUT, oid, 'v', 'long_name',	'Meridional wind speed'						;Write long name

vid = NCDF_VARDEF(oid, 'w', [xid, yid, zid, tid], /FLOAT)						;Define variable
NCDF_ATTPUT, oid, 'w', 'units',		'm/s'																	;Write units
NCDF_ATTPUT, oid, 'w', 'long_name',	'Vertical wind speed'						;Write long name

NCDF_CONTROL, oid, /ENDEF										;End define mode

NCDF_VARPUT, oid, x_name,				xout						;Write the longitudes
NCDF_VARPUT, oid, y_name,				yout						;Write the latitudes
NCDF_VARPUT, oid, 'Altitude',			z_out							;Write the altitudes
NCDF_VARPUT, oid, 'Time_interval',	dt								;Write time interval between data slices

s = 0
date = date1
WHILE (TIME_DIFF(date, date2) LE 0) DO BEGIN								;Repeat read/write process below for t1 to t2
	t0   = DATE_TO_TIME(date)
	
	NCDF_VARPUT, oid, 'Julian_day',		t0.jday,	OFFSET = s				;Julian day of current data slice
	NCDF_VARPUT, oid, 'Seconds',		t0.seconds,	OFFSET = s				;Time in seconds of current data slice

	PRINT, s, ' ', MAKE_ISO_DATE_STRING(date)
    
    date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)  
	indir  = !WRF_DIRECTORY + '20120530_ncar/' + state + '/reduced/'														;Set input directory
	infile = FILE_SEARCH(indir + 'd01' + '*.nc', COUNT = nfile)													;Set input filepath

	outdim = [nx, ny, nz_out]
 	id = NCDF_OPEN(infile[s])
	IF (s EQ 0) THEN BEGIN
        NCDF_VARGET, id, 'Longitude', xwrf
        NCDF_VARGET, id, 'Latitude', ywrf
  	ENDIF
    
    NCDF_VARGET, id, 'Z',        zwrf
    NCDF_VARGET, id, 'u',        uwrf0
    NCDF_VARGET, id, 'v',        vwrf0
    NCDF_VARGET, id, 'w',        wwrf0
	TRIANGULATE, xwrf, ywrf, tri								;Compute Delaunay triangulation
	NCDF_CLOSE, id
	
      xwrf  = (xwrf + 360.0) MOD 360.0
	PRINT,'xwrf range:', MIN(xwrf), MAX(xwrf)
	PRINT,'xout range:',xout[0], xout[nx-1]
	PRINT,'ywrf range:', MIN(ywrf), MAX(ywrf)
	PRINT,'yout range:',yout[0], yout[ny-1]
     
      dim   = SIZE(zwrf, /DIMENSIONS)
      nxwrf    = dim[0]
      nywrf    = dim[1]
      nzwrf    = dim[2]

      uwrf = FLTARR(nxwrf, nywrf, nz_out)
      vwrf = FLTARR(nxwrf, nywrf, nz_out)
      wwrf = FLTARR(nxwrf, nywrf, nz_out)
      FOR j = 0, nywrf -1 DO BEGIN
       	FOR i = 0, nxwrf -1 DO BEGIN
          uwrf[i,j,*] = INTERPOL(uwrf0[i,j,*], zwrf[i,j,*], z_out)                  ; Interpolate wind fields to regular altitude grid
          vwrf[i,j,*] = INTERPOL(vwrf0[i,j,*], zwrf[i,j,*], z_out)
          wwrf[i,j,*] = INTERPOL(wwrf0[i,j,*], zwrf[i,j,*], z_out)
       	ENDFOR
      ENDFOR
	
	k250 = INDEX_OF_NEAREST(z_out, 11000.0)
	print, 'After vertical interpolation u_max,v_max @11km:',MAX(uwrf[*,*,k250],/NAN), MAX(vwrf[*,*,k250],/NAN);,z_out[k250]
	
	uinter = FLTARR(nx, ny, nz_out)
	vinter = FLTARR(nx, ny, nz_out)
	winter = FLTARR(nx, ny, nz_out)
	
	FOR k = 1, nz_out-3 DO BEGIN
		uinter[*,*,k] = TRIGRID(xwrf, ywrf, uwrf[*,*,k-1], tri, XOUT = xout, YOUT = yout)
		vinter[*,*,k] = TRIGRID(xwrf, ywrf, vwrf[*,*,k-1], tri, XOUT = xout, YOUT = yout)
		winter[*,*,k] = TRIGRID(xwrf, ywrf, wwrf[*,*,k-1], tri, XOUT = xout, YOUT = yout)
	ENDFOR

	print,'After Triangulation:', MAX(uinter[*,*,k250],/NAN), MAX(vinter[*,*,k250],/NAN),MAX(winter[*,*,k250],/NAN)       
	
	NCDF_VARPUT, oid, 'u', uinter, OFFSET = [0,0,0,s], COUNT = [nx,ny,nz_out,1]			;Write to outfile
	NCDF_VARPUT, oid, 'v', vinter, OFFSET = [0,0,0,s], COUNT = [nx,ny,nz_out,1]	        	;Write to outfile
	NCDF_VARPUT, oid, 'w', winter, OFFSET = [0,0,0,s], COUNT = [nx,ny,nz_out,1]			;Write to outfile

	s += 1												;Increment time index
	date = TIME_INC(date, dt)

ENDWHILE

NCDF_CLOSE, oid																									;Close output file

END
