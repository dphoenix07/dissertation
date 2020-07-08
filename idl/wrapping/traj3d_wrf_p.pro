PRO TRAJ3D_WRF_P, run, state, outfile, $
	PLOT      = plot, $
	DIRECTORY = directory, $
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

COMPILE_OPT IDL2																								;Set compile options

IF (N_ELEMENTS(directory) EQ 0) THEN outdir    = '/data3/dphoenix/wrf/20120530_ncar/d02_30km/winds/'			;Default output directory for wind file
IF (N_ELEMENTS(clobber  ) EQ 0) THEN clobber   = 1																;Default is clobber old file
IF (N_ELEMENTS(domain   ) EQ 0) THEN domain    = 'd01'															;Default is clobber old file

indir  = !WRF_DIRECTORY + run + '/' + state + '/reduced/'														;Set input directory
infile = FILE_SEARCH(indir + domain + '*.nc', COUNT = nfile)													;Set input filepath

dt		  = 3600																								;Time interval in s (1 hour)

IF (N_ELEMENTS(outfile) EQ 0) THEN outfile = outdir + 'wrf_winds.nc'

data_domain = 'REGIONAL'																						;Data domain (Regional or Global)
data_source = 'WRF FORECAST'																					;Data source

x_name      = 'Longitude'																						;Name of x-coordinate
y_name      = 'Latitude'																						;Name of y-coordinate
z_name      = 'Altitude'																						;Name of z-coordinate

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

IF (state EQ 'd03_30km') THEN BEGIN
	date1   	=  MAKE_DATE(2012, 5, 30, 20, 30)
	date2   	=  MAKE_DATE(2012, 5, 31, 05, 45)
	dx 			=  0.005
	dy 			=  0.005
	x_min       =  256.3																						;Set minimum of x-coordinate domain
	x_max       =  269.4																						;Set maximum of x-coordinate domain
	y_min		=   36.1
	y_max		=   40.2
ENDIF

x_regular   =     1																								;Set x-coordinate spacing flag
x_periodic  =     0																								;Set x-coordinate periodicity flag
nx_out      = LONG((x_max-x_min)/dx) + 1
xout        = x_min + dx*FINDGEN(nx_out)

y_regular   =     1																								;Set y-coordinate spacing flag
y_periodic  =     0																								;Set y-coordinate periodicity flag
ny_out      = LONG((y_max-y_min)/dy) + 1
yout        = y_min + dy*FINDGEN(ny_out)

dz          =   100.0
z_regular   =     1																								;Set z-coordinate spacing flag
z_periodic  =     0																								;Set z-coordinate periodicity flag
z_min       =     0.0
z_max		= 31900.0
nz_out		= LONG((z_max-z_min)/dz) + 1																		;Number of output altitude levels
z_out			= z_min + dz*FINDGEN(nz_out)																	;Create altitude array

PRINT, 'From           : ', MAKE_ISO_DATE_STRING(date1)
PRINT, 'To             : ', MAKE_ISO_DATE_STRING(date2)
PRINT, 'Output file    : ', outfile
PRINT, 'nx             : ', nx_out, ' points'
PRINT, 'ny             : ', ny_out, ' points'
PRINT, 'nz             : ', nz_out, ' points'

PRINT, '[x_min, x_max] : [', x_min, ', ', x_max, ']'
PRINT, '[y_min, y_max] : [', y_min, ', ', y_max, ']'

FILE_MKDIR, outdir																									;Create output directory, if neccessary

oid = NCDF_CREATE(outfile, CLOBBER = clobber, /NETCDF3_64BIT)															;Create the netCDF file
NCDF_ATTPUT, oid, 'data_domain', data_domain, /GLOBAL													;Create global attribute for use by TRAJ3D
NCDF_ATTPUT, oid, 'data_source', data_source, /GLOBAL													;Create global attribute for use by TRAJ3D

xid = NCDF_DIMDEF(oid, x_name,     nx_out    )															;Define the longitude dimension
yid = NCDF_DIMDEF(oid, y_name,     ny_out    )															;Define the latitude dimension
zid = NCDF_DIMDEF(oid, z_name,     nz_out    )															;Define the altitude dimension
tid = NCDF_DIMDEF(oid, 'Time',     /UNLIMITED)															;Define the time dimension

vid = NCDF_VARDEF(oid, x_name, xid, /FLOAT)																;Define the longitude variable
NCDF_ATTPUT, oid, x_name, 'units',     'degrees'														;Write units
NCDF_ATTPUT, oid, x_name, 'long_name', 'Longitude'														;Write long name
NCDF_ATTPUT, oid, x_name, 'regular',	x_regular														;Write coordinate spacing flag
NCDF_ATTPUT, oid, x_name, 'periodic',	x_periodic														;Write coordinate periodicity flag
NCDF_ATTPUT, oid, x_name, 'min',			x_min																;Write minimum of coordinate domain
NCDF_ATTPUT, oid, x_name, 'max',			x_max																;Write maximum of coordinate domain

vid = NCDF_VARDEF(oid, y_name, yid, /FLOAT)																;Define the latitude variable
NCDF_ATTPUT, oid, y_name, 'units',     'degrees'														;Write units
NCDF_ATTPUT, oid, y_name, 'long_name', 'Latitude'														;Write long name
NCDF_ATTPUT, oid, y_name, 'regular',	y_regular														;Write coordinate spacing flag
NCDF_ATTPUT, oid, y_name, 'periodic',	y_periodic														;Write coordinate periodicity flag
NCDF_ATTPUT, oid, y_name, 'min',			y_min																;Write minimum of coordinate domain
NCDF_ATTPUT, oid, y_name, 'max',			y_max																;Write maximum of coordinate domain

vid = NCDF_VARDEF(oid, z_name, zid, /FLOAT)																;Define the altitude variable
NCDF_ATTPUT, oid, z_name, 'units',			'm'															;Write units
NCDF_ATTPUT, oid, z_name, 'long_name',	'Altitude'														;Write long name
NCDF_ATTPUT, oid, z_name, 'regular',		z_regular													;Write coordinate spacing flag
NCDF_ATTPUT, oid, z_name, 'periodic',		z_periodic													;Write coordinate periodicity flag
NCDF_ATTPUT, oid, z_name, 'min',			z_min																;Write minimum of coordinate domain
NCDF_ATTPUT, oid, z_name, 'max',			z_max																;Write maximum of coordinate domain

vid = NCDF_VARDEF(oid, 'Time_interval',      /LONG)													;Define the time interval variable
NCDF_ATTPUT, oid, 'Time_interval', 'units',     's'
NCDF_ATTPUT, oid, 'Time_interval', 'long_name', 'Time interval between data samples'

vid = NCDF_VARDEF(oid, 'Julian_day'   , tid, /LONG)													;Define the Julian day variable
NCDF_ATTPUT, oid, 'Julian_day'   , 'units',     'day'
NCDF_ATTPUT, oid, 'Julian_day'   , 'long_name', 'Julian day'

vid = NCDF_VARDEF(oid, 'Seconds'      , tid, /LONG)													;Define the seconds variable
NCDF_ATTPUT, oid, 'Seconds'      , 'units',     's'
NCDF_ATTPUT, oid, 'Seconds'      , 'long_name', 'Time of day in seconds'

vid = NCDF_VARDEF(oid, 'u', [xid, yid, zid, tid], /FLOAT)											;Define variable
NCDF_ATTPUT, oid, 'u', 'units',		'm/s'																	;Write units
NCDF_ATTPUT, oid, 'u', 'long_name',	'Zonal wind speed'												;Write long name

vid = NCDF_VARDEF(oid, 'v', [xid, yid, zid, tid], /FLOAT)											;Define variable
NCDF_ATTPUT, oid, 'v', 'units',		'm/s'																	;Write units
NCDF_ATTPUT, oid, 'v', 'long_name',	'Meridional wind speed'											;Write long name

vid = NCDF_VARDEF(oid, 'w', [xid, yid, zid, tid], /FLOAT)											;Define variable
NCDF_ATTPUT, oid, 'w', 'units',		'm/s'																	;Write units
NCDF_ATTPUT, oid, 'w', 'long_name',	'Vertical wind speed'											;Write long name

vid = NCDF_VARDEF(oid, 'u_shift', [xid, yid, zid, tid], /FLOAT)											;Define variable
NCDF_ATTPUT, oid, 'u_shift', 'units',		'm/s'																	;Write units
NCDF_ATTPUT, oid, 'u_shift', 'long_name',	'Zonal wind speed'												;Write long name

vid = NCDF_VARDEF(oid, 'v_shift', [xid, yid, zid, tid], /FLOAT)											;Define variable
NCDF_ATTPUT, oid, 'v_shift', 'units',		'm/s'																	;Write units
NCDF_ATTPUT, oid, 'v_shift', 'long_name',	'Meridional wind speed'											;Write long name

vid = NCDF_VARDEF(oid, 'w_shift', [xid, yid, zid, tid], /FLOAT)											;Define variable
NCDF_ATTPUT, oid, 'w_shift', 'units',		'm/s'																	;Write units
NCDF_ATTPUT, oid, 'w_shift', 'long_name',	'Vertical wind speed'											;Write long name

NCDF_CONTROL, oid, /ENDEF																						;End define mode

NCDF_VARPUT, oid, x_name,				xout																	;Write the longitudes
NCDF_VARPUT, oid, y_name,				yout																	;Write the latitudes
NCDF_VARPUT, oid, 'Altitude',			z_out																	;Write the altitudes
NCDF_VARPUT, oid, 'Time_interval',	dt																		;Write time interval between data slices

s = 0
date = date1
WHILE (TIME_DIFF(date, date2) LE 0) DO BEGIN																;Repeat read/write process below for t1 to t2
	t0   = DATE_TO_TIME(date)
	PRINT, t0
	
	NCDF_VARPUT, oid, 'Julian_day',	t0.jday,		OFFSET = s											;Julian day of current data slice
	NCDF_VARPUT, oid, 'Seconds',		t0.seconds,	OFFSET = s											;Time in seconds of current data slice

	PRINT, s, ' ', MAKE_ISO_DATE_STRING(date)

	outdim = [nx_out, ny_out, nz_out]

	id = NCDF_OPEN(infile[s])
	IF (s EQ 0) THEN BEGIN
		NCDF_VARGET, id, 'Longitude', xwrf
		NCDF_VARGET, id, 'Latitude',  ywrf
		TRIANGULATE, xwrf, ywrf, tri																			;Compute Delaunay triangulation
	ENDIF
	
	NCDF_VARGET, id, 'u',         uwrf
	NCDF_VARGET, id, 'v',         vwrf
	NCDF_VARGET, id, 'w',         wwrf
	NCDF_VARGET, id, 'Z',         z
	NCDF_CLOSE , id

	;Make dummy x & y interpolation indices
	dim = SIZE(wwrf, /DIMENSIONS)
    nx  = dim[0]
    ny  = dim[1]
    nz  = dim[2]
	nz1 = 320
	ix  = REBIN(FINDGEN(nx), nx, ny, nz1)
	iy  = REBIN(REFORM(FINDGEN(ny), 1, ny), nx, ny, nz1)
    
    iz = FLTARR(nx, ny, nz1)
    FOR i = 0, nx-1 DO FOR j = 0, ny-1 DO iz[i,j,*] = INTERPOL(FINDGEN(nz), REFORM(z[i,j,*], nz), FINDGEN(nz1)*100.0)

    u_wrf     = INTERPOLATE(uwrf, ix, iy, iz)
    v_wrf     = INTERPOLATE(vwrf, ix, iy, iz)
    w_wrf     = INTERPOLATE(wwrf, ix, iy, iz)

;	ixx = REBIN(FINDGEN(nx), nx, ny)
;	iyy = REBIN(REFORM(FINDGEN(ny), 1, ny), nx, ny)
;	izz = FLTARR(nx, ny)
;    FOR i = 0, nx-1 DO FOR j = 0, ny-1 DO izz[i,j] = INTERPOL(FINDGEN(nz), REFORM(z[i,j,*], nz), 10000.0)

;    uu_wrf     = INTERPOLATE(uwrf, ixx, iyy, izz)
;    vv_wrf     = INTERPOLATE(vwrf, ixx, iyy, izz)
;    ww_wrf     = INTERPOLATE(wwrf, ixx, iyy, izz)

	uinter = FLTARR(nx_out, ny_out, nz_out)*!Values.F_NaN
	vinter = FLTARR(nx_out, ny_out, nz_out)*!Values.F_NaN
	winter = FLTARR(nx_out, ny_out, nz_out)*!Values.F_NaN

	uinter_shift = FLTARR(nx_out, ny_out, nz_out)
	vinter_shift = FLTARR(nx_out, ny_out, nz_out)
	winter_shift = FLTARR(nx_out, ny_out, nz_out)
	
	FOR k = 1, 319 DO BEGIN
		uinter_shift[*,*,k] = SHIFT(TRIGRID(xwrf, ywrf, u_wrf[*,*,k-1], tri, XOUT = SHIFT(CONVERT_LONGITUDE(xout), nx/2), YOUT = yout), -nx/2, 0)
		vinter_shift[*,*,k] = SHIFT(TRIGRID(xwrf, ywrf, v_wrf[*,*,k-1], tri, XOUT = SHIFT(CONVERT_LONGITUDE(xout), nx/2), YOUT = yout), -nx/2, 0)
		winter_shift[*,*,k] = SHIFT(TRIGRID(xwrf, ywrf, w_wrf[*,*,k-1], tri, XOUT = SHIFT(CONVERT_LONGITUDE(xout), nx/2), YOUT = yout), -nx/2, 0)
	ENDFOR

	xwrf = xwrf + 360.0
	
	FOR k = 1, 319 DO BEGIN
		uinter[*,*,k] = TRIGRID(xwrf, ywrf, u_wrf[*,*,k-1], tri, XOUT = xout, YOUT = yout)
		vinter[*,*,k] = TRIGRID(xwrf, ywrf, v_wrf[*,*,k-1], tri, XOUT = xout, YOUT = yout)
		winter[*,*,k] = TRIGRID(xwrf, ywrf, w_wrf[*,*,k-1], tri, XOUT = xout, YOUT = yout)
	ENDFOR

	NCDF_VARPUT, oid, 'u', uinter, OFFSET = [0,0,0,s], COUNT = [nx_out,ny_out,nz_out,1]				;Write to outfile
	NCDF_VARPUT, oid, 'v', vinter, OFFSET = [0,0,0,s], COUNT = [nx_out,ny_out,nz_out,1]				;Write to outfile
	NCDF_VARPUT, oid, 'w', winter, OFFSET = [0,0,0,s], COUNT = [nx_out,ny_out,nz_out,1]				;Write to outfile
	
	NCDF_VARPUT, oid, 'u_shift', uinter_shift, OFFSET = [0,0,0,s], COUNT = [nx_out,ny_out,nz_out,1]				;Write to outfile
	NCDF_VARPUT, oid, 'v_shift', vinter_shift, OFFSET = [0,0,0,s], COUNT = [nx_out,ny_out,nz_out,1]				;Write to outfile
	NCDF_VARPUT, oid, 'w_shift', winter_shift, OFFSET = [0,0,0,s], COUNT = [nx_out,ny_out,nz_out,1]				;Write to outfile

	s += 1																											;Increment time index
	date = TIME_INC(date, dt)
	PRINT, date, dt
ENDWHILE

NCDF_CLOSE, oid																									;Close output file

END
