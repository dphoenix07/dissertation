PRO REMAKE_WRF_NC_CHEM, run, state, $
	TRACER  = tracer, $
	DOMAIN  = domain, $
	VERBOSE = verbose

;+
; Name:
;		REMAKE_WRF_NC
; Purpose:
;		This is a procedure for remaking WRF output files by eliminating unused
;		variables and converting perturbation and base state variables into
;		full output variables. Staggered variables are also transferred to the
;		regular common output grid. 
; Calling sequence:
;		REMAKE_WRF_NC, run, state
; Input:
;		run   : Model simulation name. (e.g., 'APR04')
;		state : Model initial state. Typically 'era' or 'gfs'.
; Output:
;		A restructured netCDF file.
; Keywords:
;		TRACER  : If set, read/write passive tracers.
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		VERBOSE : If set, message writing process.
; Author and history:
;		Cameron R. Homeyer  2012-11-23.
;				    2015-02-15. Updated to make tracers optional
;		Daniel Phoenix	    2015-12-23. Added chemistry and cloud mixing ratio
;-

COMPILE_OPT IDL2																										;Set compile options
	
IF (N_ELEMENTS(domain) EQ 0) THEN domain = 'd02'															;Set default domain
	
indir  = !WRF_DIRECTORY + run + '/' + state + '/'															;Set input directory
infile = FILE_SEARCH(indir + 'wrfout_' + domain + '*.nc', COUNT = nfile)							;Set input filepath
outdir = indir + 'reduced/'																						;Set output filepath

FILE_MKDIR, outdir																									;Create output directory, if neccessary
	
FOR i = 0, nfile -1 DO BEGIN	
	outfile = STRMID(infile[i], 16, /REVERSE_OFFSET)														;Set output file name
		
	iid = NCDF_OPEN(infile[i])																						;Open input file for reading
	IF (i EQ 0) THEN BEGIN
		NCDF_VARGET, iid, 'T', values																				;Read single variable for output file definition
		NCDF_ATTGET, iid, 'DX', dx, /GLOBAL																		;Read grid resolution
		NCDF_ATTGET, iid, 'DT', dt, /GLOBAL																		;Read grid resolution

		dim = SIZE(values, /DIMENSIONS)																			;Get grid dimension sizes
	ENDIF
	
	CATCH, error_status																								;Catch any errors with netcdf control or file creation

	IF (error_status NE 0) THEN BEGIN
		NCDF_CLOSE, oid																								;Close previous failed file
		oid = NCDF_CREATE(outdir + domain + '_' + outfile, CLOBBER = 1)								;Create output file for writing
	ENDIF ELSE $
		oid = NCDF_CREATE(outdir + domain + '_' + outfile, CLOBBER = 1)								;Create output file for writing
	
	xid = NCDF_DIMDEF(oid, 'x', dim[0])																			;Define output file dimensions
	yid = NCDF_DIMDEF(oid, 'y', dim[1])	
	zid = NCDF_DIMDEF(oid, 'z', dim[2])	
	tid = NCDF_DIMDEF(oid, 't', 14    )	

	vid = NCDF_VARDEF(oid, 'Time', [tid], /CHAR)																;Define the time variable
	NCDF_ATTPUT, oid, 'Time', 'long_name', 'ISO Date String'												;Name attribute
	NCDF_ATTPUT, oid, 'Time', 'units',     'YYYYMMDD_HHMM_'												;Units attribute
	
	vid = NCDF_VARDEF(oid, 'Longitude', [xid, yid], /FLOAT)												;Define the longitude variable
	NCDF_ATTPUT, oid, 'Longitude', 'long_name', 'Grid Point Longitude'								;Name attribute
	NCDF_ATTPUT, oid, 'Longitude', 'units',     'degrees E'												;Units attribute
	
	vid = NCDF_VARDEF(oid, 'Latitude', [xid, yid], /FLOAT)												;Define the latitude variable
	NCDF_ATTPUT, oid, 'Latitude', 'long_name', 'Grid Point Latitude'									;Name attribute
	NCDF_ATTPUT, oid, 'Latitude', 'units',     'degrees N'												;Units attribute
		
	vid = NCDF_VARDEF(oid, 'P', [xid, yid, zid], /FLOAT)													;Define the pressure variable
	NCDF_ATTPUT, oid, 'P', 'long_name', 'Air Pressure'														;Name attribute
	NCDF_ATTPUT, oid, 'P', 'units',     'hPa'																	;Units attribute
	
	vid = NCDF_VARDEF(oid, 'T', [xid, yid, zid], /FLOAT)													;Define the temperature variable
	NCDF_ATTPUT, oid, 'T', 'long_name', 'Air Temperature'													;Name attribute
	NCDF_ATTPUT, oid, 'T', 'units',     'K'																	;Units attribute

	vid = NCDF_VARDEF(oid, 'Td', [xid, yid, zid], /FLOAT)													;Define the temperature variable
	NCDF_ATTPUT, oid, 'Td', 'long_name', 'Dew Point Temperature'										;Name attribute
	NCDF_ATTPUT, oid, 'Td', 'units',     'C'																	;Units attribute

	vid = NCDF_VARDEF(oid, 'Z', [xid, yid, zid], /FLOAT)													;Define the geopotential height variable
	NCDF_ATTPUT, oid, 'Z', 'long_name', 'Geopotential Height'											;Name attribute
	NCDF_ATTPUT, oid, 'Z', 'units',     'm'																	;Units attribute
	
	vid = NCDF_VARDEF(oid, 'u', [xid, yid, zid], /FLOAT)													;Define the zonal wind variable
	NCDF_ATTPUT, oid, 'u', 'long_name', 'Zonal Wind'														;Name attribute
	NCDF_ATTPUT, oid, 'u', 'units',     'm s^-1'																;Units attribute
	
	vid = NCDF_VARDEF(oid, 'v', [xid, yid, zid], /FLOAT)													;Define the meridional wind variable
	NCDF_ATTPUT, oid, 'v', 'long_name', 'Meridional Wind'													;Name attribute
	NCDF_ATTPUT, oid, 'v', 'units',     'm s^-1'																;Units attribute
	
	vid = NCDF_VARDEF(oid, 'w', [xid, yid, zid], /FLOAT)													;Define the vertical wind variable
	NCDF_ATTPUT, oid, 'w', 'long_name', 'Vertical Wind'													;Name attribute
	NCDF_ATTPUT, oid, 'w', 'units',     'm s^-1'																;Units attribute
	
	vid = NCDF_VARDEF(oid, 'PV', [xid, yid, zid], /FLOAT)													;Define the radar reflectivity variable
	NCDF_ATTPUT, oid, 'PV', 'long_name', 'Potential Vorticity'											;Name attribute
	NCDF_ATTPUT, oid, 'PV', 'units',     'pvu'																;Units attribute

	vid = NCDF_VARDEF(oid, 'absvor', [xid, yid, zid], /FLOAT)											;Define the radar reflectivity variable
	NCDF_ATTPUT, oid, 'absvor', 'long_name', 'Absolute Vorticity'										;Name attribute
	NCDF_ATTPUT, oid, 'absvor', 'units',     's^-1'															;Units attribute

	vid = NCDF_VARDEF(oid, 'REFL', [xid, yid, zid], /FLOAT)												;Define the radar reflectivity variable
	NCDF_ATTPUT, oid, 'REFL', 'long_name', '10 cm Radar Reflectivity'									;Name attribute
	NCDF_ATTPUT, oid, 'REFL', 'units',     'dBZ'																;Units attribute

	vid = NCDF_VARDEF(oid, 'CLOUD_NUM', [xid,yid,zid], /FLOAT)												;Define the cloud fraction variable
	NCDF_ATTPUT, oid, 'CLOUD_NUM', 'long_name', 'Cloud Particle # Concentration'						;Name attribute
	NCDF_ATTPUT, oid, 'CLOUD_NUM', 'units',     'L^-1'															;Units attribute

        vid = NCDF_VARDEF(oid, 'CLOUD_MIX', [xid,yid,zid], /FLOAT)                                                                                              ;Define the cloud mixing ratio variable
        NCDF_ATTPUT, oid, 'CLOUD_MIX', 'long_name', 'Cloud Particle Mixing Ratio'                                            ;Name attribute
        NCDF_ATTPUT, oid, 'CLOUD_MIX', 'units',     'kg kg-1'                                                                                                                      ;Units attribute

	vid = NCDF_VARDEF(oid, 'H2O', [xid,yid,zid], /FLOAT)													;Define the H2O variable
	NCDF_ATTPUT, oid, 'H2O', 'long_name', 'Water Vapor Mixing Ratio'									;Name attribute
	NCDF_ATTPUT, oid, 'H2O', 'units',     'kg kg^-1'														;Units attribute

        vid = NCDF_VARDEF(oid, 'SO2', [xid, yid, zid], /FLOAT)                                                                                                       ;Define the SO2 variable
        NCDF_ATTPUT, oid, 'SO2', 'long_name', 'SO2 mixing ratio'                                        ;Name attribute
        NCDF_ATTPUT, oid, 'SO2', 'units', 'ppmv'                                                                                                                        ;Units attribute

        vid = NCDF_VARDEF(oid, 'SULF', [xid, yid, zid], /FLOAT)                                                                                                      ;Define the sulfur variable
        NCDF_ATTPUT, oid, 'SULF', 'long_name', 'SULF mixing ratio'                                      ;Name attribute
        NCDF_ATTPUT, oid, 'SULF', 'units', 'ppmv'                                                                                                                       ;Units attribute

        vid = NCDF_VARDEF(oid, 'NO2', [xid, yid, zid], /FLOAT)                                                                                                       ;Define the NO2 attribute
        NCDF_ATTPUT, oid, 'NO2', 'long_name', 'NO2 mixing ratio'                                        ;Name attribute
        NCDF_ATTPUT, oid, 'NO2', 'units', 'ppmv'                                                                                                                        ;Units attribute

        vid = NCDF_VARDEF(oid, 'NO', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the NO attribute
        NCDF_ATTPUT, oid, 'NO', 'long_name', 'NO mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'NO', 'units', 'ppmv'                                                                                                                         ;Units attribute

        vid = NCDF_VARDEF(oid, 'O3', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the O3 attribute
        NCDF_ATTPUT, oid, 'O3', 'long_name', 'O3 mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'O3', 'units', 'ppmv'                                                                                                                         ;Units attribute

        vid = NCDF_VARDEF(oid, 'HNO3', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the HNO3 attribute
        NCDF_ATTPUT, oid, 'HNO3', 'long_name', 'HNO3 mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'HNO3', 'units', 'ppmv'                                                                                                                         ;Units attribute

        vid = NCDF_VARDEF(oid, 'H2O2', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the H2O2 attribute
        NCDF_ATTPUT, oid, 'H2O2', 'long_name', 'H2O2 mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'H2O2', 'units', 'ppmv'                                                                                                                         ;Units attribute

        vid = NCDF_VARDEF(oid, 'ALD', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the ALD attribute
        NCDF_ATTPUT, oid, 'ALD', 'long_name', 'ALD mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'ALD', 'units', 'ppmv'                                                                                                                         ;Units attribute

        vid = NCDF_VARDEF(oid, 'HCHO', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the HCHO attribute
        NCDF_ATTPUT, oid, 'HCHO', 'long_name', 'HCHO mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'HCHO', 'units', 'ppmv' 															 ;Units attribute

        vid = NCDF_VARDEF(oid, 'OP1', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the OP1 attribute
        NCDF_ATTPUT, oid, 'OP1', 'long_name', 'OP1 mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'OP1', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'OP2', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the OP2 attribute
        NCDF_ATTPUT, oid, 'OP2', 'long_name', 'OP2 mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'OP2', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'PAA', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the PAA attribute
        NCDF_ATTPUT, oid, 'PAA', 'long_name', 'PAA mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'PAA', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'ORA1', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the ORA1 attribute
        NCDF_ATTPUT, oid, 'ORA1', 'long_name', 'ORA1 mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'ORA1', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'ORA2', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the ORA2 attribute
        NCDF_ATTPUT, oid, 'ORA2', 'long_name', 'ORA2 mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'ORA2', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'NH3', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the NH3 attribute
        NCDF_ATTPUT, oid, 'NH3', 'long_name', 'NH3 mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'NH3', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'N2O5', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the N2O5 attribute
        NCDF_ATTPUT, oid, 'N2O5', 'long_name', 'N2O5 mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'N2O5', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'NO3', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the NO3 attribute
        NCDF_ATTPUT, oid, 'NO3', 'long_name', 'NO3 mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'NO3', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'PAN', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the PAN attribute
        NCDF_ATTPUT, oid, 'PAN', 'long_name', 'PAN mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'PAN', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'HC3', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the HC3 attribute
        NCDF_ATTPUT, oid, 'HC3', 'long_name', 'HC3 mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'HC3', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'HC5', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the HC5 attribute
        NCDF_ATTPUT, oid, 'HC5', 'long_name', 'HC5 mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'HC5', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'HC8', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the HC8 attribute
        NCDF_ATTPUT, oid, 'HC8', 'long_name', 'HC8 mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'HC8', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'ETH', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the ETH attribute
        NCDF_ATTPUT, oid, 'ETH', 'long_name', 'ETH mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'ETH', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'CO', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the CO attribute
        NCDF_ATTPUT, oid, 'CO', 'long_name', 'CO mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'CO', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'OL2', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the OL2 attribute
        NCDF_ATTPUT, oid, 'OL2', 'long_name', 'OL2 mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'OL2', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'OLT', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the OLT attribute
        NCDF_ATTPUT, oid, 'OLT', 'long_name', 'OLT mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'OLT', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'OLI', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the OLI attribute
        NCDF_ATTPUT, oid, 'OLI', 'long_name', 'OLI mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'OLI', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'TOL', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the TOL attribute
        NCDF_ATTPUT, oid, 'TOL', 'long_name', 'TOL mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'TOL', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'XYL', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the XYL attribute
        NCDF_ATTPUT, oid, 'XYL', 'long_name', 'XYL mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'XYL', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'ACO3', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the ACO3 attribute
        NCDF_ATTPUT, oid, 'ACO3', 'long_name', 'ACO3 mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'ACO3', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'TPAN', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the TPAN attribute
        NCDF_ATTPUT, oid, 'TPAN', 'long_name', 'TPAN mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'TPAN', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'HONO', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the HONO attribute
        NCDF_ATTPUT, oid, 'HONO', 'long_name', 'HONO mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'HONO', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'HNO4', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the HNO4 attribute
        NCDF_ATTPUT, oid, 'HNO4', 'long_name', 'HNO4 mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'HNO4', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'KET', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the KET attribute
        NCDF_ATTPUT, oid, 'KET', 'long_name', 'KET mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'KET', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'GLY', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the GLY attribute
        NCDF_ATTPUT, oid, 'GLY', 'long_name', 'GLY mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'GLY', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'MGLY', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the MGLY attribute
        NCDF_ATTPUT, oid, 'MGLY', 'long_name', 'MGLY mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'MGLY', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'DCB', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the DCB attribute
        NCDF_ATTPUT, oid, 'DCB', 'long_name', 'DCB mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'DCB', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'ONIT', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the ONIT attribute
        NCDF_ATTPUT, oid, 'ONIT', 'long_name', 'ONIT mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'ONIT', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'CSL', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the CSL attribute
        NCDF_ATTPUT, oid, 'CSL', 'long_name', 'CSL mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'CSL', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'ISO', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the ISO attribute
        NCDF_ATTPUT, oid, 'ISO', 'long_name', 'ISO mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'ISO', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'HCL', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the HCL attribute
        NCDF_ATTPUT, oid, 'HCL', 'long_name', 'HCL mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'HCL', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'HO', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the HO attribute
        NCDF_ATTPUT, oid, 'HO', 'long_name', 'HO mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'HO', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'HO2', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the HO2 attribute
        NCDF_ATTPUT, oid, 'HO2', 'long_name', 'HO2 mixing ratio'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'HO2', 'units', 'ppmv'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'SO4aj', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the SO4aj attribute
        NCDF_ATTPUT, oid, 'SO4aj', 'long_name', 'Sulfate conc. Acc. mode'                                   ;Name attribute
        NCDF_ATTPUT, oid, 'SO4aj', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'SO4ai', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the SO4ai attribute
        NCDF_ATTPUT, oid, 'SO4ai', 'long_name', 'Sulfate conc. Aitken mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'SO4ai', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'NH4aj', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the NH4aj attribute
        NCDF_ATTPUT, oid, 'NH4aj', 'long_name', 'Ammonium conc. Acc. mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'NH4aj', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'NH4ai', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the NH4ai attribute
        NCDF_ATTPUT, oid, 'NH4ai', 'long_name', 'Ammonium conc. Aitken mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'NH4ai', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'NO3aj', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the NO3aj attribute
        NCDF_ATTPUT, oid, 'NO3aj', 'long_name', 'Nitrate conc. Acc. mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'NO3aj', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute  

        vid = NCDF_VARDEF(oid, 'NO3ai', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the NO3ai attribute
        NCDF_ATTPUT, oid, 'NO3ai', 'long_name', 'Nitrate conc. Aitken mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'NO3ai', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute 

        vid = NCDF_VARDEF(oid, 'NAaj', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the NAaj attribute
        NCDF_ATTPUT, oid, 'NAaj', 'long_name', 'Sodium conc. Acc. mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'NAaj', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute 

        vid = NCDF_VARDEF(oid, 'NAai', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the NAai attribute
        NCDF_ATTPUT, oid, 'NAai', 'long_name', 'Sodium conc. Aitken mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'NAai', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute 

        vid = NCDF_VARDEF(oid, 'CLaj', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the CLaj attribute
        NCDF_ATTPUT, oid, 'CLaj', 'long_name', 'Chloride conc. Acc. mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'CLaj', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute 

        vid = NCDF_VARDEF(oid, 'CLai', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the CLai attribute
        NCDF_ATTPUT, oid, 'CLai', 'long_name', 'Chloride conc. Aitken mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'CLai', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute 

        vid = NCDF_VARDEF(oid, 'ORGARO1j', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the ORGARO1j attribute
        NCDF_ATTPUT, oid, 'ORGARO1j', 'long_name', 'SOA Anth. org. conc. from aromatics Acc. mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'ORGARO1j', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute 

        vid = NCDF_VARDEF(oid, 'ORGARO1i', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the ORGARO1i attribute
        NCDF_ATTPUT, oid, 'ORGARO1i', 'long_name', 'SOA Anth. org. conc. from aromatics Aitken mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'ORGARO1i', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute 

        vid = NCDF_VARDEF(oid, 'ORGARO2j', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the ORGARO2j attribute
        NCDF_ATTPUT, oid, 'ORGARO2j', 'long_name', 'SOA Anth. org. conc. from aromatics Acc. mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'ORGARO2j', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute 

        vid = NCDF_VARDEF(oid, 'ORGARO2i', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the ORGARO2i attribute
        NCDF_ATTPUT, oid, 'ORGARO2i', 'long_name', 'SOA Anth. org. conc. from aromatics Aitken mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'ORGARO2i', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute 

        vid = NCDF_VARDEF(oid, 'ORGALK1j', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the ORGALK1j attribute
        NCDF_ATTPUT, oid, 'ORGALK1j', 'long_name', 'SOA Anth. org. conc. from alkanes and others except aromatics Acc. mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'ORGALK1j', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute 

        vid = NCDF_VARDEF(oid, 'ORGALK1i', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the ORGALK1i attribute
        NCDF_ATTPUT, oid, 'ORGALK1i', 'long_name', 'SOA Anth. org. conc. from alkanes and others except aromatics Aitken mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'ORGALK1i', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute

        vid = NCDF_VARDEF(oid, 'ORGOLE1j', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the ORGOLE1j attribute
        NCDF_ATTPUT, oid, 'ORGOLE1j', 'long_name', 'SOA Anth. org. conc. from alkenes and others except aromatics Acc. mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'ORGOLE1j', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute

        vid = NCDF_VARDEF(oid, 'ORGOLE1i', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the ORGOLE1i attribute
        NCDF_ATTPUT, oid, 'ORGOLE1i', 'long_name', 'SOA Anth. org. conc. from alkenes and others except aromatics Aitken mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'ORGOLE1i', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute

        vid = NCDF_VARDEF(oid, 'ORGBA1j', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the ORGBA1j attribute
        NCDF_ATTPUT, oid, 'ORGBA1j', 'long_name', 'SOA Biog. org. conc. from aromatics Acc. mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'ORGBA1j', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute

        vid = NCDF_VARDEF(oid, 'ORGBA1i', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the ORGBA1i attribute
        NCDF_ATTPUT, oid, 'ORGBA1i', 'long_name', 'SOA Biog. org. conc. from aromatics Aitken mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'ORGBA1i', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute

        vid = NCDF_VARDEF(oid, 'ORGBA2j', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the ORGBA2j attribute
        NCDF_ATTPUT, oid, 'ORGBA2j', 'long_name', 'SOA Biog. org. conc. from aromatics Acc. mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'ORGBA2j', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute

        vid = NCDF_VARDEF(oid, 'ORGBA2i', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the ORGBA2i attribute
        NCDF_ATTPUT, oid, 'ORGBA2i', 'long_name', 'SOA Biog. org. conc. from aromatics Aitken mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'ORGBA2i', 'units', 'ug/kg-dryair' 															    ;Units attribute	 

        vid = NCDF_VARDEF(oid, 'ORGBA3j', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the ORGBA3j attribute
        NCDF_ATTPUT, oid, 'ORGBA3j', 'long_name', 'SOA Biog. org. conc. from aromatics Acc. mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'ORGBA3j', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute
                
        vid = NCDF_VARDEF(oid, 'ORGBA3i', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the ORGBA3i attribute
        NCDF_ATTPUT, oid, 'ORGBA3i', 'long_name', 'SOA Biog. org. conc. from aromatics Aitken mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'ORGBA3i', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute    

        vid = NCDF_VARDEF(oid, 'ORGBA4j', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the ORGBA4j attribute
        NCDF_ATTPUT, oid, 'ORGBA4j', 'long_name', 'SOA Biog. org. conc. from aromatics Acc. mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'ORGBA4j', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute
                
        vid = NCDF_VARDEF(oid, 'ORGBA4i', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the ORGBA4i attribute
        NCDF_ATTPUT, oid, 'ORGBA4i', 'long_name', 'SOA Biog. org. conc. from aromatics Aitken mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'ORGBA4i', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute    

        vid = NCDF_VARDEF(oid, 'ORGPAj', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the ORGPAj attribute
        NCDF_ATTPUT, oid, 'ORGPAj', 'long_name', 'Prim. anth. org. conc. from aromatics Acc. mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'ORGPAj', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute
                
        vid = NCDF_VARDEF(oid, 'ORGPAi', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the ORGPAi attribute
        NCDF_ATTPUT, oid, 'ORGPAi', 'long_name', 'Prim. anth. org. conc. from aromatics Aitken mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'ORGPAi', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute    

        vid = NCDF_VARDEF(oid, 'ECj', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the ECj attribute
        NCDF_ATTPUT, oid, 'ECj', 'long_name', 'Elemental carbon Acc. mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'ECj', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute   

        vid = NCDF_VARDEF(oid, 'ECi', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the ECi attribute
        NCDF_ATTPUT, oid, 'ECi', 'long_name', 'Elemental carbon Aitken mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'ECi', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute   

        vid = NCDF_VARDEF(oid, 'P25j', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the P25j attribute
        NCDF_ATTPUT, oid, 'P25j', 'long_name', 'Primary PM2.5 Acc. mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'P25j', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute   

        vid = NCDF_VARDEF(oid, 'P25i', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the P25i attribute
        NCDF_ATTPUT, oid, 'P25i', 'long_name', 'Primary PM2.5 Aitken mode'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'P25i', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute   

        vid = NCDF_VARDEF(oid, 'ANTHA', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the ANTHA attribute
        NCDF_ATTPUT, oid, 'ANTHA', 'long_name', 'Coarse anthropogenic aerosols'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'ANTHA', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute   

        vid = NCDF_VARDEF(oid, 'SEAS', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the SEAS attribute
        NCDF_ATTPUT, oid, 'SEAS', 'long_name', 'Coarse marine aerosols'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'SEAS', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute   

        vid = NCDF_VARDEF(oid, 'SOILa', [xid, yid, zid], /FLOAT)                                                                                                        ;Define the SOILa attribute
        NCDF_ATTPUT, oid, 'SOILa', 'long_name', 'Coarse soil-derived aerosols'                                          ;Name attribute
        NCDF_ATTPUT, oid, 'SOILa', 'units', 'ug/kg-dryair'                                                                                                                        ;Units attribute   

	IF KEYWORD_SET(tracer) THEN BEGIN
		vid = NCDF_VARDEF(oid, 'TR_tracer', [xid, yid, zid], /FLOAT)									;Define the tropospheric tracer variable
		NCDF_ATTPUT, oid, 'TR_tracer', 'long_name', 'Tropospheric Tracer'								;Name attribute
		NCDF_ATTPUT, oid, 'TR_tracer', 'units',     'Troposphere = 1, 0 otherwise'					;Units attribute

		vid = NCDF_VARDEF(oid, 'BL_tracer', [xid, yid, zid], /FLOAT)									;Define the tropospheric boundary layer tracer variable
		NCDF_ATTPUT, oid, 'BL_tracer', 'long_name', 'Tropospheric Boundary Layer Tracer'			;Name attribute
		NCDF_ATTPUT, oid, 'BL_tracer', 'units',     'Boundary Layer = 1, 0 otherwise'				;Units attribute

		vid = NCDF_VARDEF(oid, 'MT_tracer', [xid, yid, zid], /FLOAT)									;Define the middle troposphere tracer variable
		NCDF_ATTPUT, oid, 'MT_tracer', 'long_name', 'Middle Troposphere Tracer'						;Name attribute
		NCDF_ATTPUT, oid, 'MT_tracer', 'units',     'Troposphere = 1, 0 otherwise'					;Units attribute

		vid = NCDF_VARDEF(oid, 'UT_tracer', [xid, yid, zid], /FLOAT)									;Define the upper troposphere tracer variable
		NCDF_ATTPUT, oid, 'UT_tracer', 'long_name', 'Upper Troposphere Tracer'						;Name attribute
		NCDF_ATTPUT, oid, 'UT_tracer', 'units',     'Troposphere = 1, 0 otherwise'					;Units attribute

		vid = NCDF_VARDEF(oid, 'ST_tracer', [xid, yid, zid], /FLOAT)									;Define the Stratospheric tracer variable
		NCDF_ATTPUT, oid, 'ST_tracer', 'long_name', 'Stratospheric Tracer'							;Name attribute
		NCDF_ATTPUT, oid, 'ST_tracer', 'units',     'Stratosphere = 1, 0 otherwise'				;Units attribute

		vid = NCDF_VARDEF(oid, 'LS_tracer', [xid, yid, zid], /FLOAT)									;Define the lower stratosphere tracer variable
		NCDF_ATTPUT, oid, 'LS_tracer', 'long_name', 'Lower Stratosphere Tracer'						;Name attribute
		NCDF_ATTPUT, oid, 'LS_tracer', 'units',     'Stratosphere = 1, 0 otherwise'				;Units attribute

		vid = NCDF_VARDEF(oid, 'US_tracer', [xid, yid, zid], /FLOAT)									;Define the overworld stratosphere tracer variable
		NCDF_ATTPUT, oid, 'US_tracer', 'long_name', 'Upper Stratosphere (Overworld) Tracer'		;Name attribute
		NCDF_ATTPUT, oid, 'US_tracer', 'units',     'Stratosphere = 1, 0 otherwise'				;Units attribute
	ENDIF
		
	vid = NCDF_VARDEF(oid, 'PBL', [xid, yid], /FLOAT)														;Define the PBL height variable
	NCDF_ATTPUT, oid, 'PBL', 'long_name', 'PBL Height'														;Name attribute
	NCDF_ATTPUT, oid, 'PBL', 'units',     'm'																	;Units attribute
	
	vid = NCDF_VARDEF(oid, 'Z0', [xid, yid], /FLOAT)														;Define the terrain height variable
	NCDF_ATTPUT, oid, 'Z0', 'long_name', 'Terrain Height'													;Name attribute
	NCDF_ATTPUT, oid, 'Z0', 'units',     'm'																	;Units attribute
	
	vid = NCDF_VARDEF(oid, 'T_sfc', [xid, yid], /FLOAT)													;Define the surface temperature variable
	NCDF_ATTPUT, oid, 'T_sfc', 'long_name', 'Air Temperature at 2 M'									;Name attribute
	NCDF_ATTPUT, oid, 'T_sfc', 'units',     'K'																;Units attribute
	
	vid = NCDF_VARDEF(oid, 'p_sfc', [xid, yid], /FLOAT)													;Define the surface pressure variable
	NCDF_ATTPUT, oid, 'p_sfc', 'long_name', 'Surface Pressure'											;Name attribute
	NCDF_ATTPUT, oid, 'p_sfc', 'units',     'hPa'															;Units attribute
	
	vid = NCDF_VARDEF(oid, 'u_sfc', [xid, yid], /FLOAT)													;Define the surface zonal wind variable
	NCDF_ATTPUT, oid, 'u_sfc', 'long_name', 'Zonal Wind at 10 M'										;Name attribute
	NCDF_ATTPUT, oid, 'u_sfc', 'units',     'm s^-1'														;Units attribute
	
	vid = NCDF_VARDEF(oid, 'v_sfc', [xid, yid], /FLOAT)													;Define the surface meridional wind variable
	NCDF_ATTPUT, oid, 'v_sfc', 'long_name', 'Meridional Wind at 10 M'									;Name attribute
	NCDF_ATTPUT, oid, 'v_sfc', 'units',     'm s^-1'														;Units attribute

	vid = NCDF_VARDEF(oid, 'Z_trop', [xid, yid], /FLOAT)													;Define the tropopause height variable
	NCDF_ATTPUT, oid, 'Z_trop', 'long_name', 'WMO Primary Tropopause Geopotential Height'		;Name attribute
	NCDF_ATTPUT, oid, 'Z_trop', 'units',     'm'																;Units attribute

	vid = NCDF_VARDEF(oid, 'Z_trop2', [xid, yid], /FLOAT)													;Define the tropopause height variable
	NCDF_ATTPUT, oid, 'Z_trop2', 'long_name', 'WMO Secondary Tropopause Geopotential Height'	;Name attribute
	NCDF_ATTPUT, oid, 'Z_trop2', 'units',     'm'															;Units attribute

	vid = NCDF_VARDEF(oid, 'PRECIP', [xid, yid], /FLOAT)													;Define the precip variable
	NCDF_ATTPUT, oid, 'PRECIP', 'long_name', 'Accumulated Total Grid-scale Precipitation'		;Name attribute
	NCDF_ATTPUT, oid, 'PRECIP', 'units',     'mm'															;Units attribute

	NCDF_ATTPUT, oid, 'DX', dx, /GLOBAL
	NCDF_ATTPUT, oid, 'DT', dt, /GLOBAL

	PRINT, 'DONE CREATING VARIABLES'

	NCDF_CONTROL, oid, /ENDEF

	PRINT, 'BEGIN WRITING VARIABLES'

	NCDF_VARPUT, oid, 'Time', STRMID(outfile, 0, 14)														;Write date string to file
	
	NCDF_VARGET, iid, 'XLONG',     values																		;Read longitude values
	NCDF_VARPUT, oid, 'Longitude', values																		;Write longitude

	NCDF_VARGET, iid, 'XLAT',     y																				;Read latitude values
	NCDF_VARPUT, oid, 'Latitude', y																				;Write latitude
	
	NCDF_VARGET, iid, 'PB', p																						;Read pressure variables
	NCDF_VARGET, iid, 'P',  values	
	p = 0.01*(p + values)																							;Convert pressure to hPa
	NCDF_VARPUT, oid, 'P', p	
	
	NCDF_VARGET, iid, 'QVAPOR', values																			;Read the water vapor mixing ratio
	NCDF_VARPUT, oid, 'H2O', values																				;Write the water vapor mixing ratio
	
	evap = 1000.0*values*P/(1000.0*values+0.622)																;Compute the vapor pressure (in hPa)
	NCDF_VARPUT, oid, 'Td',	5417.0/(19.83 - ALOG(evap/6.11)) + !KtoC									;Compute dew point temperature via C-C equation and write to file

	NCDF_VARGET, iid, 'PHB', z																						;Read geopotential variables
	NCDF_VARGET, iid, 'PH',  values	
	z = (z + values)/!g																								;Convert geopotential to height (km)
	z = (0.5*(SHIFT(z, 0, 0, -1) + z))[*,*,0:(dim[2]-1)]													;Keep geopotential height at half-mass levels
	NCDF_VARPUT, oid, 'Z', z	
	
	NCDF_VARGET, iid, 'U', values																					;Read zonal wind
	u = (0.5*(SHIFT(values, -1, 0, 0) + values))[0:(dim[0]-1),*,*]										;Keep zonal wind at half-x levels
	NCDF_VARPUT, oid, 'u', u																						;Write zonal wind
		
	NCDF_VARGET, iid, 'V', values																					;Read meridional wind
	v = (0.5*(SHIFT(values, 0, -1, 0) + values))[*,0:(dim[1]-1),*]										;Keep meridional wind at half-y levels
	NCDF_VARPUT, oid, 'v', v																						;Write meridional wind
		
	NCDF_VARGET, iid, 'T', T																						;Read temperature array

	absvor = WRF_VORTICITY(u, v, y, dx, /ABSOLUTE)															;Compute absolute vorticity
	values = -!g*(absvor*WRF_STABILITY(T, p) - (SHIFT(v,  0,  0, -1) - SHIFT(v, 0, 0, 1))/$	;Compute full potential vorticity (PV)
				(SHIFT(p,  0,  0, -1) - SHIFT(p, 0, 0, 1))*(SHIFT(T, -1,  0,  0) - $
				SHIFT(T, 1, 0, 0))/(2.0*dx) + (SHIFT(u,  0,  0, -1) - SHIFT(u, 0, 0, 1))/$
				(SHIFT(p,  0,  0, -1) - SHIFT(p, 0, 0, 1))*(SHIFT(T,  0, -1,  0) - $
				SHIFT(T, 0, 1, 0))/(2.0*dx))

	NCDF_VARPUT, oid, 'PV', 1.0E4*values																		;Write PV to file
	NCDF_VARPUT, oid, 'absvor', absvor																			;Write absolute vorticity to file

	T = (T + 300.0)*((p/1000.0)^(!Rair/!Cp))																	;Convert potential temp to T (K)
	NCDF_VARPUT, oid, 'T', T																						;Write temperature array

	NCDF_VARGET, iid, 'W', values																					;Read vertical wind
	values = (0.5*(SHIFT(values, 0, 0, -1) + values))[*,*,0:(dim[2]-1)]								;Keep vertical wind at half-mass levels
	NCDF_VARPUT, oid, 'w', values																					;Write vertical wind
	
	NCDF_VARGET, iid, 'REFL_10CM', values																		;Read reflectivity
	NCDF_VARPUT, oid, 'REFL',      values																		;Write reflectivity

	; ** no cloud water # conc. in morrison scheme **
	NCDF_VARGET, iid, 'QNCLOUD',   cloud																		;Read cloud concentration
	NCDF_VARGET, iid, 'QNRAIN',    values																		;Read rain conc.
	cloud += values																									;Add rain conc. to cloud variable
	NCDF_VARGET, iid, 'QNICE',     values																		;Read ice conc.
	cloud += values																									;Add ice conc. to cloud variable
	NCDF_VARGET, iid, 'QNSNOW',    values																		;Read snow conc.
	cloud += values																									;Add snow conc. to cloud variable
	NCDF_VARGET, iid, 'QNGRAUPEL', values																		;Read graupel conc.
	cloud += values	 		
	; ** no hail in morrison scheme **																						;Add graupel conc. to cloud variable
	NCDF_VARGET, iid, 'QNHAIL',    values																		;Read hail conc.
	cloud += values																									;Add hail conc. to cloud variable

	NCDF_VARPUT, oid, 'CLOUD_NUM',     cloud																		;Write cloud conc.


        NCDF_VARGET, iid, 'QCLOUD',   cloud_mix                                                                                                                                            ;Read cloud mixing ratio
        NCDF_VARGET, iid, 'QRAIN',    values                                                                                                                                           ;Read rain mixing ratio
        cloud_mix += values                                                                                                                                                                                                 ;Add rain mixing ratio to cloud variable
        NCDF_VARGET, iid, 'QICE',     values                                                                                                                                           ;Read ice mixing ratio
        cloud_mix += values                                                                                                                                                                                                 ;Add ice mixing ratio to cloud variable
        NCDF_VARGET, iid, 'QSNOW',    values                                                                                                                                           ;Read snow mixing ratio
        cloud_mix += values                                                                                                                                                                                                 ;Add snow mixing ratio to cloud variable
        NCDF_VARGET, iid, 'QGRAUP', values                                                                                                                                           ;Read graupel mixing ratio
        cloud_mix += values                                                                                                                                                                                                 ;Add graupel mixing ratio to cloud variable
	;** no hail in morrison scheme **
        NCDF_VARGET, iid, 'QHAIL',    values                                                                                                                                           ;Read hail mixing ratio
        cloud_mix += values                                                                                                                                                                                               ;Add hail mixing ratio to cloud variable
 
       NCDF_VARPUT, oid, 'CLOUD_MIX',     cloud_mix  


	IF KEYWORD_SET(tracer) THEN BEGIN
		NCDF_VARGET, iid, 'tr17_1',    values																	;Read tracer
		NCDF_VARPUT, oid, 'TR_tracer', values																	;Write tracer
	
		NCDF_VARGET, iid, 'tr17_2',    values																	;Read tracer
		NCDF_VARPUT, oid, 'BL_tracer', values																	;Write tracer
	
		NCDF_VARGET, iid, 'tr17_3',    values																	;Read tracer
		NCDF_VARPUT, oid, 'MT_tracer', values																	;Write tracer
	
		NCDF_VARGET, iid, 'tr17_4',    values																	;Read tracer
		NCDF_VARPUT, oid, 'UT_tracer', values																	;Write tracer
	
		NCDF_VARGET, iid, 'tr17_5',    values																	;Read tracer
		NCDF_VARPUT, oid, 'ST_tracer', values																	;Write tracer
	
		NCDF_VARGET, iid, 'tr17_6',    values																	;Read tracer
		NCDF_VARPUT, oid, 'LS_tracer', values																	;Write tracer
	
		NCDF_VARGET, iid, 'tr17_7',    values																	;Read tracer
		NCDF_VARPUT, oid, 'US_tracer', values																	;Write tracer
	ENDIF
	
	NCDF_VARGET, iid, 'PBLH', values																				;Read PBL height
	NCDF_VARPUT, oid, 'PBL',  values																				;Write PBL height to output file
	
	NCDF_VARGET, iid, 'HGT', values																				;Read terrain height
	NCDF_VARPUT, oid, 'Z0',  values																				;Write terrain height to output file
	
	NCDF_VARGET, iid, 'T2',    values																			;Read surface temperature
	NCDF_VARPUT, oid, 'T_sfc', values																			;Write surface temperature
	
	NCDF_VARGET, iid, 'PSFC',  values																			;Read surface pressure
	NCDF_VARPUT, oid, 'p_sfc', 0.01*values																		;Write surface pressure (in hPa)
		
	NCDF_VARGET, iid, 'U10',   values																			;Read surface zonal wind
	NCDF_VARPUT, oid, 'u_sfc', values																			;Write surface zonal wind
		
	NCDF_VARGET, iid, 'V10',   values																			;Read surface meridional wind
	NCDF_VARPUT, oid, 'v_sfc', values																			;Write surface meridional wind

	trop = MAKE_ARRAY([dim[0],dim[1],2], VALUE = !Values.F_NaN)											;Create array to store tropopause altitudes
	FOR ii = 0, dim[0] -1 DO $
		FOR jj = 0, dim[1] -1 DO $
			trop[ii,jj,*] = TROPOPAUSE(T[ii,jj,*], 0.001*z[ii,jj,*], p[ii,jj,*], NTROPS = 2)		;Compute tropopause altitudes

	NCDF_VARPUT, oid, 'Z_trop',  REFORM(1000.0*trop[*,*,0], dim[0], dim[1])							;Output tropopause altitudes
	NCDF_VARPUT, oid, 'Z_trop2', REFORM(1000.0*trop[*,*,1], dim[0], dim[1])
		

	NCDF_VARGET, iid, 'RAINNC', values																			;Read total accumulated precip
	NCDF_VARPUT, oid, 'PRECIP', values																			;Write precip

	;assign chemistry
	NCDF_VARGET, iid, 'so2', values																				;Read SO2 
	NCDF_VARPUT, oid, 'SO2', values																				;Write SO2

	NCDF_VARGET, iid, 'sulf', values																			;Read sulfur mixing ratio
	NCDF_VARPUT, oid, 'SULF', values																			;Write sulf

	NCDF_VARGET, iid, 'no2', values																				;Read NO2
	NCDF_VARPUT, oid, 'NO2', values																				;Write NO2

	NCDF_VARGET, iid, 'no', values																				;Read NO
	NCDF_VARPUT, oid, 'NO', values																				;Write NO

	NCDF_VARGET, iid, 'o3', values																				;Read O3
	NCDF_VARPUT, oid, 'O3', values																				;Write O3

	NCDF_VARGET, iid, 'hno3', values																			;Read HNO3
	NCDF_VARPUT, oid, 'HNO3', values																			;Write HNO3

	NCDF_VARGET, iid, 'h2o2', values																			;Read H2O2
	NCDF_VARPUT, oid, 'H2O2', values																			;Write H2O2

	NCDF_VARGET, iid, 'ald', values																				;Read ALD
	NCDF_VARPUT, oid, 'ALD', values																				;Write ALD

	NCDF_VARGET, iid, 'hcho', values																			;Read HCHO
	NCDF_VARPUT, oid, 'HCHO', values																			;Write HCHO

	NCDF_VARGET, iid, 'op1', values																				;Read OP1
	NCDF_VARPUT, oid, 'OP1', values																				;Write OP1

	NCDF_VARGET, iid, 'op2', values																				;Read OP2
	NCDF_VARPUT, oid, 'OP2', values																				;Write OP2

	NCDF_VARGET, iid, 'paa', values																				;Read PAA
	NCDF_VARPUT, oid, 'PAA', values																				;Write PAA

	NCDF_VARGET, iid, 'ora1', values																			;Read ORA1
	NCDF_VARPUT, oid, 'ORA1', values																			;Write ORA1

	NCDF_VARGET, iid, 'ora2', values																			;Read ORA2
	NCDF_VARPUT, oid, 'ORA2', values																			;Write ORA2

	NCDF_VARGET, iid, 'nh3', values																				;Read NH3
	NCDF_VARPUT, oid, 'NH3', values																				;Write NH3

	NCDF_VARGET, iid, 'n2o5', values																			;Read N2O5
	NCDF_VARPUT, oid, 'N2O5', values																			;Write N2O5

	NCDF_VARGET, iid, 'no3', values																				;Read NO3
	NCDF_VARPUT, oid, 'NO3', values																				;Write NO3

	NCDF_VARGET, iid, 'pan', values																				;Read PAN
	NCDF_VARPUT, oid, 'PAN', values																				;Write PAN

	NCDF_VARGET, iid, 'hc3', values																				;Read HC3
	NCDF_VARPUT, oid, 'HC3', values																				;Write HC3

	NCDF_VARGET, iid, 'hc5', values																				;Read HC5
	NCDF_VARPUT, oid, 'HC5', values																				;Write HC5
	
	NCDF_VARGET, iid, 'hc8', values																				;Read HC8
	NCDF_VARPUT, oid, 'HC8', values																				;Write HC8

	NCDF_VARGET, iid, 'eth', values																				;Read ETH
	NCDF_VARPUT, oid, 'ETH', values																				;Write ETH

	NCDF_VARGET, iid, 'co', values																				;Read CO
	NCDF_VARPUT, oid, 'CO', values																				;Write CO

	NCDF_VARGET, iid, 'ol2', values																				;Read OL2
	NCDF_VARPUT, oid, 'OL2', values																				;Write OL2

	NCDF_VARGET, iid, 'olt', values																				;Read OLT
	NCDF_VARPUT, oid, 'OLT', values																				;Write OLT

	NCDF_VARGET, iid, 'oli', values																				;Read OLI
	NCDF_VARPUT, oid, 'OLI', values																				;Write OLI

	NCDF_VARGET, iid, 'tol', values																				;Read TOL
	NCDF_VARPUT, oid, 'TOL', values																				;Write TOL

	NCDF_VARGET, iid, 'xyl', values																				;Read XYL
	NCDF_VARPUT, oid, 'XYL', values																				;Write XYL

	NCDF_VARGET, iid, 'aco3', values																			;Read ACO3
	NCDF_VARPUT, oid, 'ACO3', values																			;Write ACO3

	NCDF_VARGET, iid, 'tpan', values																			;Read TPAN
	NCDF_VARPUT, oid, 'TPAN', values																			;Write TPAN
	
	NCDF_VARGET, iid, 'hono', values																			;Read HONO
	NCDF_VARPUT, oid, 'HONO', values																			;Write HONO

	NCDF_VARGET, iid, 'hno4', values																			;Read HNO4
	NCDF_VARPUT, oid, 'HNO4', values																			;Write HNO4
	
	NCDF_VARGET, iid, 'ket', values																				;Read KET
	NCDF_VARPUT, oid, 'KET', values																				;Write KET

	NCDF_VARGET, iid, 'gly', values																				;Read GLY
	NCDF_VARPUT, oid, 'GLY', values																				;Write GLY

	NCDF_VARGET, iid, 'mgly', values																			;Read MGLY
	NCDF_VARPUT, oid, 'MGLY', values																			;Write MGLY

	NCDF_VARGET, iid, 'dcb', values																				;Read DCB
	NCDF_VARPUT, oid, 'DCB', values																				;Write DCB

	NCDF_VARGET, iid, 'onit', values																			;Read ONIT
	NCDF_VARPUT, oid, 'ONIT', values																			;Write ONIT

	NCDF_VARGET, iid, 'csl', values																				;Read CSL
	NCDF_VARPUT, oid, 'CSL', values																				;Write CSL

	NCDF_VARGET, iid, 'iso', values																				;Read ISO
	NCDF_VARPUT, oid, 'ISO', values																				;Write ISO

	NCDF_VARGET, iid, 'hcl', values																				;Read HCL
	NCDF_VARPUT, oid, 'HCL', values																				;Write HCL

	NCDF_VARGET, iid, 'ho', values																				;Read HO
	NCDF_VARPUT, oid, 'HO', values																				;Write HO

	NCDF_VARGET, iid, 'ho2', values																				;Read HO2
	NCDF_VARPUT, oid, 'HO2', values																				;Write HO2

	NCDF_VARGET, iid, 'so4aj', values																			;Read SO4aj
	NCDF_VARPUT, oid, 'SO4aj', values																			;Write SO4aj

	NCDF_VARGET, iid, 'so4ai', values																			;Read SO4ai
	NCDF_VARPUT, oid, 'SO4ai', values																			;Write SO4ai

	NCDF_VARGET, iid, 'nh4aj', values																			;Read NH4aj
	NCDF_VARPUT, oid, 'NH4aj', values																			;Write NH4aj

	NCDF_VARGET, iid, 'nh4ai', values																			;Read NH4ai
	NCDF_VARPUT, oid, 'NH4ai', values																			;Write NH4ai

	NCDF_VARGET, iid, 'no3aj', values																			;Read NO3aj
	NCDF_VARPUT, oid, 'NO3aj', values																			;Write NO3aj

	NCDF_VARGET, iid, 'no3ai', values																			;Read NO3ai
	NCDF_VARPUT, oid, 'NO3ai', values																			;Write NO3ai

	NCDF_VARGET, iid, 'naaj', values																			;Read NAaj
	NCDF_VARPUT, oid, 'NAaj', values																			;Write NAaj
	
	NCDF_VARGET, iid, 'naai', values																			;Read NAai
	NCDF_VARPUT, oid, 'NAai', values																			;Write NAai

	NCDF_VARGET, iid, 'claj', values																			;Read CLaj
	NCDF_VARPUT, oid, 'CLaj', values																			;Write CLaj

	NCDF_VARGET, iid, 'clai', values																			;Read CLai
	NCDF_VARPUT, oid, 'CLai', values																			;Write CLai

	NCDF_VARGET, iid, 'orgaro1j', values																			;Read ORGARO1j
	NCDF_VARPUT, oid, 'ORGARO1j', values																			;Write ORGARO1j

	NCDF_VARGET, iid, 'orgaro1i', values																			;Read ORGARO1i
	NCDF_VARPUT, oid, 'ORGARO1i', values																			;Write ORGARO1i

	NCDF_VARGET, iid, 'orgaro2j', values																			;Read ORGARO2j
	NCDF_VARPUT, oid, 'ORGARO2j', values																			;Write ORGARO2j

	NCDF_VARGET, iid, 'orgaro2i', values																			;Read ORGARO2i
	NCDF_VARPUT, oid, 'ORGARO2i', values																			;Write ORGARO2i

	NCDF_VARGET, iid, 'orgalk1j', values																			;Read ORGALK1j
	NCDF_VARPUT, oid, 'ORGALK1j', values																			;Write ORGALK1j
	
	NCDF_VARGET, iid, 'orgalk1i', values																			;Read ORGALK1i
	NCDF_VARPUT, oid, 'ORGALK1i', values																			;Write ORGALK1i

	NCDF_VARGET, iid, 'orgole1j', values																			;Read ORGOLE1j
	NCDF_VARPUT, oid, 'ORGOLE1j', values																			;Write ORGOLE1j

	NCDF_VARGET, iid, 'orgole1i', values																			;Read ORGOLE1i
	NCDF_VARPUT, oid, 'ORGOLE1i', values																			;Write ORGOLE1i

	NCDF_VARGET, iid, 'orgba1j', values																			;Read ORGBA1j
	NCDF_VARPUT, oid, 'ORGBA1j', values																			;Write ORGBA1j

	NCDF_VARGET, iid, 'orgba1i', values																			;Read ORGBA1i
	NCDF_VARPUT, oid, 'ORGBA1i', values																			;Write ORGBA1i	
		
        NCDF_VARGET, iid, 'orgba2j', values                                                                                                                                                     ;Read ORGBA2j
        NCDF_VARPUT, oid, 'ORGBA2j', values                                                                                                                                                     ;Write ORGBA2j
  
        NCDF_VARGET, iid, 'orgba2i', values                                                                                                                                                     ;Read ORGBA2i
        NCDF_VARPUT, oid, 'ORGBA2i', values                                                                                                                                                     ;Write ORGBA2i 
 
        NCDF_VARGET, iid, 'orgba3j', values                                                                                                                                                     ;Read ORGBA3j
       NCDF_VARPUT, oid, 'ORGBA3j', values                                                                                                                                                     ;Write ORGBA3j

        NCDF_VARGET, iid, 'orgba3i', values                                                                                                                                                     ;Read ORGBA3i
        NCDF_VARPUT, oid, 'ORGBA3i', values                                                                                                                                                     ;Write ORGBA3i
 
        NCDF_VARGET, iid, 'orgba4j', values                                                                                                                                                     ;Read ORGBA4j
        NCDF_VARPUT, oid, 'ORGBA4j', values                                                                                                                                                     ;Write ORGBA4j

        NCDF_VARGET, iid, 'orgba4i', values                                                                                                                                                     ;Read ORGBA4i
        NCDF_VARPUT, oid, 'ORGBA4i', values                                                                                                                                                     ;Write ORGBA4i
 
	NCDF_VARGET, iid, 'orgpaj', values																			;Read ORGPAj
	NCDF_VARPUT, oid, 'ORGPAj', values																			;Write ORGPAj

	NCDF_VARGET, iid, 'ecj', values																				;Read ECj
	NCDF_VARPUT, oid, 'ECj', values																				;Write ECj

	NCDF_VARGET, iid, 'eci', values																				;Read ECi
	NCDF_VARPUT, oid, 'ECi', values																				;Write ECi

	NCDF_VARGET, iid, 'p25j', values																			;Read P25j
	NCDF_VARPUT, oid, 'P25j', values																			;Write P25j

	NCDF_VARGET, iid, 'p25i', values																			;Read P25i
	NCDF_VARPUT, oid, 'P25i', values																			;Write P25i

	NCDF_VARGET, iid, 'antha', values																			;Read ANTHA
	NCDF_VARPUT, oid, 'ANTHA', values																			;Write ANTHA

	NCDF_VARGET, iid, 'seas', values																			;Read SEAS
	NCDF_VARPUT, oid, 'SEAS', values																			;Write SEAS

	NCDF_VARGET, iid, 'soila', values																			;Read SOILa
	NCDF_VARPUT, oid, 'SOILa', values																			;Write SOILa


	NCDF_CLOSE, oid																									;Close output file
	NCDF_CLOSE, iid																									;Close input file
	
	
	IF KEYWORD_SET(verbose) THEN PRINT, 'File ' + infile[i] + ' processed.'							;Print verbose message
ENDFOR

END
