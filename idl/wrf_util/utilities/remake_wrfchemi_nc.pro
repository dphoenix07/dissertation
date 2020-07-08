PRO REMAKE_WRFCHEMI_NC, run, scheme, $
	TRACER     = tracer, $
	DBP_TR     = dbp_tr, $
	DEF_TR	   = def_tr, $
	SOA		   = soa, $
	TIME	   = time, $
	DOMAIN     = domain, $
	VERBOSE    = verbose

;+
; Name:
;		REMAKE_WRFCHEMI_NC
; Purpose:
;		This is a procedure for remaking WRFchemi input files by transferring
;		staggered variables to the regular common grid.
;		regular common output grid. 
; Calling sequence:
;		REMAKE_WRFCHEMI_NC, run, scheme
; Input:
;		run   : Model simulation name. (e.g., '20120519')
;		scheme : Model initial state. Typically 'morrison' or 'nssl'.
; Output:
;		A restructured netCDF file.
; Keywords:
;		TRACER  : If set, read/write passive tracers.
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		VERBOSE : If set, message writing process.
; Author and history:
;		Daniel Phoenix	    2018-02-12. 
;-

COMPILE_OPT IDL2																										;Set compile options
	

IF (N_ELEMENTS(time   )  EQ 0) THEN time    = '20110808T1200Z' 
IF (N_ELEMENTS(domain )  EQ 0) THEN domain  = 'd02'															;Set default domain
IF (N_ELEMENTS(verbose)  EQ 0) THEN verbose = 1
	
indir  = !WRF_DIRECTORY + run + '/' + scheme + '/'															;Set input directory
infile = FILE_SEARCH(indir + 'wrfchemi_' + domain + '_' + time + '*.nc', COUNT = nfile)							;Set input filepath
outdir = indir + 'reduced_wrfchemi/'																						;Set output filepath

FILE_MKDIR, outdir																									;Create output directory, if neccessary
	
FOR i = 0, nfile -1 DO BEGIN	
	outfile = STRMID(infile[i], 16, /REVERSE_OFFSET)														;Set output file name
PRINT, outfile		
	iid = NCDF_OPEN(infile[i])																						;Open input file for reading
	IF (i EQ 0) THEN BEGIN
		NCDF_VARGET, iid, 'E_ISO', values																				;Read single variable for output file definition
		NCDF_ATTGET, iid, 'DX', dx, /GLOBAL																		;Read grid resolution
		NCDF_ATTGET, iid, 'DT', dt, /GLOBAL																		;Read grid resolution

		dim = SIZE(values, /DIMENSIONS)																			;Get grid dimension sizes
	ENDIF

	CATCH, error_status																								;Catch any errors with netcdf control or file creation

	IF (error_status NE 0) THEN BEGIN
		NCDF_CLOSE, oid																								;Close previous failed file
		oid = NCDF_CREATE(outdir + domain + '_' + outfile, CLOBBER = 1, /NETCDF3_64BIT)                         ;Create output file for writing							;Create output file for writing
	ENDIF ELSE $
		oid = NCDF_CREATE(outdir + domain + '_' + outfile, CLOBBER = 1, /NETCDF3_64BIT)                         ;Create output file for writing								;Create output file for writing

	xid = NCDF_DIMDEF(oid, 'x', dim[0])																			;Define output file dimensions
	yid = NCDF_DIMDEF(oid, 'y', dim[1])	
	zid = NCDF_DIMDEF(oid, 'z', dim[2])
	tid = NCDF_DIMDEF(oid, 't', 14    )	


	vid = NCDF_VARDEF(oid, 'Time', [tid], /CHAR)																;Define the time variable
	NCDF_ATTPUT, oid, 'Time', 'long_name', 'ISO Date String'												;Name attribute
	NCDF_ATTPUT, oid, 'Time', 'units',     'YYYYMMDD_HHMM_'												;Units attribute
	
;	vid = NCDF_VARDEF(oid, 'Longitude', [xid, yid], /FLOAT)												;Define the longitude variable
;	NCDF_ATTPUT, oid, 'Longitude', 'long_name', 'Grid Point Longitude'								;Name attribute
;	NCDF_ATTPUT, oid, 'Longitude', 'units',     'degrees E'												;Units attribute
;	
;	vid = NCDF_VARDEF(oid, 'Latitude', [xid, yid], /FLOAT)												;Define the latitude variable
;	NCDF_ATTPUT, oid, 'Latitude', 'long_name', 'Grid Point Latitude'									;Name attribute
;	NCDF_ATTPUT, oid, 'Latitude', 'units',     'degrees N'												;Units attribute
		
	vid = NCDF_VARDEF(oid, 'E_ISO', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_ISO', 'long_name', 'Isoprene Emissions'
	NCDF_ATTPUT, oid, 'E_ISO', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_SO2', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_SO2', 'long_name', 'SO2 Emissions'
	NCDF_ATTPUT, oid, 'E_SO2', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_NO', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_NO', 'long_name', 'NO Emissions'
	NCDF_ATTPUT, oid, 'E_NO', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_NO2', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_NO2', 'long_name', 'NO2 Emissions'
	NCDF_ATTPUT, oid, 'E_NO2', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_CO', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_CO', 'long_name', 'CO Emissions'
	NCDF_ATTPUT, oid, 'E_CO', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_ETH', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_ETH', 'long_name', 'ETH Emissions'
	NCDF_ATTPUT, oid, 'E_ETH', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_HC3', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_HC3', 'long_name', 'HC3 Emissions'
	NCDF_ATTPUT, oid, 'E_HC3', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_HC5', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_HC5', 'long_name', 'HC5 Emissions'
	NCDF_ATTPUT, oid, 'E_HC5', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_HC8', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_HC8', 'long_name', 'HC8 Emissions'
	NCDF_ATTPUT, oid, 'E_HC8', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_XYL', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_XYL', 'long_name', 'XYL Emissions'
	NCDF_ATTPUT, oid, 'E_XYL', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_OL2', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_OL2', 'long_name', 'OL2 Emissions'
	NCDF_ATTPUT, oid, 'E_OL2', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_OLT', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_OLT', 'long_name', 'OLT Emissions'
	NCDF_ATTPUT, oid, 'E_OLT', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_OLI', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_OLI', 'long_name', 'OLI Emissions'
	NCDF_ATTPUT, oid, 'E_OLI', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_TOL', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_TOL', 'long_name', 'TOL Emissions'
	NCDF_ATTPUT, oid, 'E_TOL', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_CSL', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_CSL', 'long_name', 'CSL Emissions'
	NCDF_ATTPUT, oid, 'E_CSL', 'units',     'mol km^-2 hr^-1'
		
	vid = NCDF_VARDEF(oid, 'E_HCHO', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_HCHO', 'long_name', 'HCHO Emissions'
	NCDF_ATTPUT, oid, 'E_HCHO', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_ALD', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_ALD', 'long_name', 'ALD Emissions'
	NCDF_ATTPUT, oid, 'E_ALD', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_KET', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_KET', 'long_name', 'KET Emissions'
	NCDF_ATTPUT, oid, 'E_KET', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_ORA2', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_ORA2', 'long_name', 'ORA2 Emissions'
	NCDF_ATTPUT, oid, 'E_ORA2', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_NH3', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_NH3', 'long_name', 'NH3 Emissions'
	NCDF_ATTPUT, oid, 'E_NH3', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_PM25I', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_PM25I', 'long_name', 'PM25I Emissions'
	NCDF_ATTPUT, oid, 'E_PM25I', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_PM25J', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_PM25J', 'long_name', 'PM25J Emissions'
	NCDF_ATTPUT, oid, 'E_PM25J', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_PM_10', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_PM_10', 'long_name', 'PM_10 Emissions'
	NCDF_ATTPUT, oid, 'E_PM_10', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_ECI', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_ECI', 'long_name', 'ECI Emissions'
	NCDF_ATTPUT, oid, 'E_ECI', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_ECJ', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_ECJ', 'long_name', 'ECJ Emissions'
	NCDF_ATTPUT, oid, 'E_ECJ', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_ORGI', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_ORGI', 'long_name', 'ORGI Emissions'
	NCDF_ATTPUT, oid, 'E_ORGI', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_ORGJ', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_ORGJ', 'long_name', 'ORGJ Emissions'
	NCDF_ATTPUT, oid, 'E_ORGJ', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_SO4I', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_SO4I', 'long_name', 'SO4I Emissions'
	NCDF_ATTPUT, oid, 'E_SO4I', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_SO4J', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_SO4J', 'long_name', 'SO4J Emissions'
	NCDF_ATTPUT, oid, 'E_SO4J', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_NO3I', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_NO3I', 'long_name', 'NO3I Emissions'
	NCDF_ATTPUT, oid, 'E_NO3I', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_NO3J', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_NO3J', 'long_name', 'NO3J Emissions'
	NCDF_ATTPUT, oid, 'E_NO3J', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_NAAJ', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_NAAJ', 'long_name', 'NAAJ Emissions'
	NCDF_ATTPUT, oid, 'E_NAAJ', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_NAAI', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_NAAI', 'long_name', 'NAAI Emissions'
	NCDF_ATTPUT, oid, 'E_NAAI', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_ORGI_A', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_ORGI_A', 'long_name', 'ORGI_A Emissions'
	NCDF_ATTPUT, oid, 'E_ORGI_A', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_ORGJ_A', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_ORGJ_A', 'long_name', 'ORGJ_A Emissions'
	NCDF_ATTPUT, oid, 'E_ORGJ_A', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_ORGI_BB', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_ORGI_BB', 'long_name', 'ORGI_BB Emissions'
	NCDF_ATTPUT, oid, 'E_ORGI_BB', 'units',     'mol km^-2 hr^-1'

	vid = NCDF_VARDEF(oid, 'E_ORGJ_BB', [xid, yid, zid], /FLOAT)
	NCDF_ATTPUT, oid, 'E_ORGJ_BB', 'long_name', 'ORGJ_BB Emissions'
	NCDF_ATTPUT, oid, 'E_ORGJ_BB', 'units',     'mol km^-2 hr^-1'

	NCDF_ATTPUT, oid, 'DX', dx, /GLOBAL
	NCDF_ATTPUT, oid, 'DT', dt, /GLOBAL

	NCDF_CONTROL, oid, /ENDEF

	NCDF_VARPUT, oid, 'Time', STRMID(outfile, 0, 14)														;Write date string to file
	
;	NCDF_VARGET, iid, 'XLONG',     values																		;Read longitude values
;	NCDF_VARPUT, oid, 'Longitude', values																		;Write longitude
;	NCDF_VARGET, iid, 'XLAT',     y																				;Read latitude values
;	NCDF_VARPUT, oid, 'Latitude', y																				;Write latitude
		
	NCDF_VARGET, iid, 'E_ISO', values
	NCDF_VARPUT, oid, 'E_ISO', values																				;Write SO2

	NCDF_VARGET, iid, 'E_SO2', values																				;Read NO2
	NCDF_VARPUT, oid, 'E_SO2', values																				;Write NO2

	NCDF_VARGET, iid, 'E_NO', values																				;Read NO
	NCDF_VARPUT, oid, 'E_NO', values																				;Write NO

	NCDF_VARGET, iid, 'E_NO2', values																				;Read O3
	NCDF_VARPUT, oid, 'E_NO2', values																				;Write O3

	NCDF_VARGET, iid, 'E_CO', values																			;Read HNO3
	NCDF_VARPUT, oid, 'E_CO', values																			;Write HNO3

	NCDF_VARGET, iid, 'E_ETH', values																			;Read H2O2
	NCDF_VARPUT, oid, 'E_ETH', values																			;Write H2O2

	NCDF_VARGET, iid, 'E_HC3', values																			;Read HCHO
	NCDF_VARPUT, oid, 'E_HC3', values																			;Write HCHO

	NCDF_VARGET, iid, 'E_HC5', values																				;Read OP1
	NCDF_VARPUT, oid, 'E_HC5', values																				;Write OP1

	NCDF_VARGET, iid, 'E_HC8', values																				;Read OP2
	NCDF_VARPUT, oid, 'E_HC8', values																				;Write OP2

	NCDF_VARGET, iid, 'E_XYL', values																				;Read NH3
	NCDF_VARPUT, oid, 'E_XYL', values																				;Write NH3

	NCDF_VARGET, iid, 'E_OL2', values																			;Read N2O5
	NCDF_VARPUT, oid, 'E_OL2', values																			;Write N2O5

	NCDF_VARGET, iid, 'E_OLT', values																				;Read NO3
	NCDF_VARPUT, oid, 'E_OLT', values																				;Write NO3

	NCDF_VARGET, iid, 'E_OLI', values																				;Read PAN
	NCDF_VARPUT, oid, 'E_OLI', values																				;Write PAN

	NCDF_VARGET, iid, 'E_TOL', values																				;Read HC3
	NCDF_VARPUT, oid, 'E_TOL', values																				;Write HC3

	NCDF_VARGET, iid, 'E_CSL', values																				;Read HC5
	NCDF_VARPUT, oid, 'E_CSL', values																				;Write HC5
	
	NCDF_VARGET, iid, 'E_HCHO', values																				;Read HC8
	NCDF_VARPUT, oid, 'E_HCHO', values																				;Write HC8

	NCDF_VARGET, iid, 'E_ALD', values																				;Read ETH
	NCDF_VARPUT, oid, 'E_ALD', values																				;Write ETH

	NCDF_VARGET, iid, 'E_KET', values																				;Read CO
	NCDF_VARPUT, oid, 'E_KET', values																				;Write CO

	NCDF_VARGET, iid, 'E_ORA2', values																				;Read OLT
	NCDF_VARPUT, oid, 'E_ORA2', values																				;Write OLT

	NCDF_VARGET, iid, 'E_NH3', values																				;Read OLI
	NCDF_VARPUT, oid, 'E_NH3', values																				;Write OLI

	NCDF_VARGET, iid, 'E_PM25I', values																				;Read TOL
	NCDF_VARPUT, oid, 'E_PM25I', values																				;Write TOL

	NCDF_VARGET, iid, 'E_PM25J', values																			;Read ACO3
	NCDF_VARPUT, oid, 'E_PM25J', values																			;Write ACO3

	NCDF_VARGET, iid, 'E_PM_10', values																			;Read TPAN
	NCDF_VARPUT, oid, 'E_PM_10', values																			;Write TPAN
	
	NCDF_VARGET, iid, 'E_ECI', values																			;Read HONO
	NCDF_VARPUT, oid, 'E_ECI', values																			;Write HONO

	NCDF_VARGET, iid, 'E_ECJ', values																			;Read HNO4
	NCDF_VARPUT, oid, 'E_ECJ', values																			;Write HNO4
	
	NCDF_VARGET, iid, 'E_ORGI', values																				;Read KET
	NCDF_VARPUT, oid, 'E_ORGI', values																				;Write KET

	NCDF_VARGET, iid, 'E_ORGJ', values																			;Read ONIT
	NCDF_VARPUT, oid, 'E_ORGJ', values																			;Write ONIT

	NCDF_VARGET, iid, 'E_SO4I', values																				;Read ISO
	NCDF_VARPUT, oid, 'E_SO4I', values																				;Write ISO

	NCDF_VARGET, iid, 'E_SO4J', values																				;Read HCL
	NCDF_VARPUT, oid, 'E_SO4J', values																				;Write HCL

	NCDF_VARGET, iid, 'E_NO3I', values																				;Read HO
	NCDF_VARPUT, oid, 'E_NO3I', values																				;Write HO

	NCDF_VARGET, iid, 'E_NO3J', values																				;Read HO2
	NCDF_VARPUT, oid, 'E_NO3J', values																				;Write HO2

	NCDF_VARGET, iid, 'E_NAAJ', values																			;Read PBL height
	NCDF_VARPUT, oid, 'E_NAAJ',  values																			;Write PBL height to output file
	
	NCDF_VARGET, iid, 'E_NAAI', values																				;Read terrain height
	NCDF_VARPUT, oid, 'E_NAAI',  values																				;Write terrain height to output file

	NCDF_VARGET, iid, 'E_ORGI_A', values																				;Read surface heat flux 
	NCDF_VARPUT, oid, 'E_ORGI_A', values																				;Write surface heat flux to output file

	NCDF_VARGET, iid, 'E_ORGJ_A', values																				;Read surface heat flux 
	NCDF_VARPUT, oid, 'E_ORGJ_A', values																				;Write surface heat flux to output file

	NCDF_VARGET, iid, 'E_ORGI_BB', values																				;Read surface heat flux 
	NCDF_VARPUT, oid, 'E_ORGI_BB', values																				;Write surface heat flux to output file

	NCDF_VARGET, iid, 'E_ORGJ_BB', values																				;Read surface heat flux 
	NCDF_VARPUT, oid, 'E_ORGJ_BB', values																				;Write surface heat flux to output file

	NCDF_CLOSE, oid																									;Close output file
	NCDF_CLOSE, iid																									;Close input file

	
	IF KEYWORD_SET(verbose) THEN PRINT, 'File ' + infile[i] + ' processed.'							;Print verbose message
ENDFOR

END
