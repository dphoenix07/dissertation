FUNCTION TRAJ3D_WRF_TEST_PARAMS, run, experiment

;+
; Name:
;		TRAJ3D_WRF_TEST_PARAMS
; Purpose:
;		This is a function to return parameters for reverse domain filling
;		trajectory points for GFS volumes. 
; Calling sequence:
;		value = TRAJ3D_WRF_TEST_PARAMS(date)
; Inputs:
;		date  : Date of analysis time for trajectories
; Output:
;		value : Parameters for the trajectory code.
; Keywords:
;		None.
; Author and history:
;		Cameron R. Homeyer  2010-07-01.
;-

COMPILE_OPT IDL2																										;Set Compile Options

CASE experiment OF
	'd02_30km' : BEGIN
				 date1  = MAKE_DATE(2012, 5, 30, 20)
				 date2  = MAKE_DATE(2012, 5, 31, 06)
				 domain = [252.1, 32.9, 274.1, 42.9]
				 END

	'd03_30km' : BEGIN
				 date1  = MAKE_DATE(2012, 5, 30, 20,30)
				 date2  = MAKE_DATE(2012, 5, 31, 05,45)
				 ;domain = [252.1, 32.9, 274.1, 42.9]										;Full domain
				 ;domain = [(-92.7334+360.0), 37.0776, (-91.441+360.0), 37.6449]			;Clear air
				 domain = [(-97.7542+360.0), 37.2695, (-96.4236+360.0), 37.7827]			;Cloud air
				 END
	
	'd03_30km_icloud' : BEGIN
				 ;date1  = MAKE_DATE(2012, 5, 30, 20,30)
				 date2  = MAKE_DATE(2012, 5, 30, 21,20)
				 date1  = MAKE_DATE(2012, 5, 31, 03,00)
				 ;domain = [252.1, 32.9, 274.1, 42.9]										;Full domain
				 ;domain = [(-92.7334+360.0), 37.0776, (-91.441+360.0), 37.6449]			;Clear air
				 ;domain = [(-97.7542+360.0), 37.2695, (-96.4236+360.0), 37.7827]			;Cloud air
				 ;domain = [(-94.614+360.0), 37.1736, (-93.3067+360.0), 37.7208]			;Leading wrapping
				 ;domain = [(-95.2632+360.0), 36.7029, (-93.9591+360.0), 37.2432]			;Leading wrapping south
				 ;domain = [(-94.6524+360.0), 36.7029, (-93.9591+360.0), 37.2432]			;Leading wrapping south 2
				 ;domain = [(-101.732+360.0), 38.5166, (-100.443+360.0), 38.9871]			;Trailing wrapping
				 ;domain = [(-101.101+360.0), 39.0077, (-100.775+360.0), 39.2001]			;Updraft
				 ;domain = [(-100.842+360.0), 39.5526, (-100.836+360.0), 39.5574]			;small updraft location
				 domain = [(-101.149+360.0), 39.4595, (-100.467+360.0), 39.6706]			;overshoot (forward traj start 2120 UTC)
				 END
	ELSE    : MESSAGE, 'No initial conditions available!'
ENDCASE

dx     = 0.005
dy     = 0.005
outdir = '/data3/dphoenix/wrf/20120530_ncar/' + experiment + '/traj3d/'													;Set output directory

nx = (domain[2] - domain[0])/dx + 1																				;Calculate number of points for x
x  = domain[0] + dx*FINDGEN(nx)																					;Create longitude array

ny = (domain[3] - domain[1])/dy + 1																				;Calculate number of points for y
y  = domain[1] + dy*FINDGEN(ny)																					;Create latitude array

infile = '/data3/dphoenix/wrf/20120530_ncar/' + experiment + '/winds/wrf_winds.nc'
id = NCDF_OPEN(infile)
NCDF_VARGET, id, 'Altitude', z

;z  = (var.values)
nz = N_ELEMENTS(z)

x = REBIN(x,                   nx, ny, nz, /SAMPLE)														;Expand coordinates to 3 dimensions
y = REBIN(REFORM(y, 1, ny   ), nx, ny, nz, /SAMPLE)
z = REBIN(REFORM(z, 1, 1, nz), nx, ny, nz, /SAMPLE)


np = N_ELEMENTS(x)																									;Get number of elements

RETURN,  {x      : {name : 'Longitude', values : REFORM(x, np), units : 'degrees'}, $			;Output trajectory parameters
			 y      : {name : 'Latitude',  values : REFORM(y, np), units : 'degrees'}, $
			 z      : {name : 'Altitude',  values : REFORM(z, np), units : 'hPa'    }, $
			 seed   : 0,                                                               $
			 outdir : outdir,                                                          $
			 date1  : date1,                                                           $
			 date2  : date2,                                                           $
			 domain : domain,                                                          $
			 np     : np,                                                              $
			 nx     : LONG(nx),                                                        $
			 ny     : LONG(ny),                                                        $
			 nz     : LONG(nz)                                                         }

END
