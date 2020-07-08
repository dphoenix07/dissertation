PRO RENAME_CHEM_FILES

;+
; Name:
;		RENAME_CHEM_FILES
; Purpose:
;		This is a procedure for remaking WRF output files by eliminating unused
;		variables and converting perturbation and base state variables into
;		full output variables. Staggered variables are also transferred to the
;		regular common output grid. 
; Calling sequence:
;		RENAME_CHEM_FILES, run, scheme
; Input:
;		None.
; Output:
;		A restructured netCDF file.
; Keywords:
;		TRACER  : If set, read/write passive tracers.
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		VERBOSE : If set, message writing process.
; Author and history:
;		Daniel Phoenix	    2016-04-10.
;-

COMPILE_OPT IDL2																										;Set compile options
	
IF (N_ELEMENTS(domain )  EQ 0) THEN domain = 'd02'															;Set default domain
IF (N_ELEMENTS(verbose)  EQ 0) THEN verbose = 1
	
indir  = !WRF_DIRECTORY + '/wrfchemi/'																	;Set input directory
infile = FILE_SEARCH(indir + '*.nc', COUNT = nfile)							;Set input filepath
outdir = indir + 'renamed/'																			;Set output filepath

FILE_MKDIR, outdir																									;Create output directory, if neccessary

t12 = [50, 48, 49, 50, 45, 48, 54, 45, 48, 49, 95, 49, 50, 58, 48, 48, 58, 48, 48]
t12 = BYTE(t12)
t_new = REBIN(t12,19,12,/SAMPLE) 
	
FOR i = 0, nfile -1 DO BEGIN	
	outfile = STRMID(infile[i], 16, /REVERSE_OFFSET)														;Set output file name
		
	iid = NCDF_OPEN(infile[i])																						;Open input file for reading
	NCDF_VARGET, iid, 'Times', times
	PRINT,STRING(times)

	IF (i EQ 0) THEN BEGIN
		NCDF_VARGET, iid, 'E_ISO', values																				;Read single variable for output file definition
		
		dim = SIZE(values, /DIMENSIONS)																			;Get grid dimension sizes
	ENDIF
	
	CATCH, error_status																								;Catch any errors with netcdf control or file creation

	IF (error_status NE 0) THEN BEGIN
		NCDF_CLOSE, oid																								;Close previous failed file
		oid = NCDF_CREATE(outdir + outfile, CLOBBER = 1)								;Create output file for writing
	ENDIF ELSE $
		oid = NCDF_CREATE(outdir + outfile, CLOBBER = 1)								;Create output file for writing
	
	xid  = NCDF_DIMDEF(oid, 'x'         , dim[0])																			;Define output file dimensions
	yid  = NCDF_DIMDEF(oid, 'y'         , dim[1])	
	zid  = NCDF_DIMDEF(oid, 'z'         , dim[2])	
	tid  = NCDF_DIMDEF(oid, 'Time'      , dim[3])	
	date = NCDF_DIMDEF(oid, 'DateStrLen', 14    )	

;	vid = NCDF_VARDEF(oid, 'Time', [tid], /CHAR)																;Define the time variable
;	NCDF_ATTPUT, oid, 'Time', 'long_name', 'ISO Date String'												;Name attribute
;	NCDF_ATTPUT, oid, 'Time', 'units',     'YYYYMMDD_HHMM_'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Times', [date, tid], /CHAR)																;Define the time variable
	
	NCDF_CONTROL, oid, /ENDEF


	NCDF_VARGET, iid, 'Times', values
	values = t_new
	NCDF_VARPUT, oid, 'Times', values
		

	NCDF_CLOSE, oid																									;Close output file
	NCDF_CLOSE, iid																									;Close input file
	
	IF KEYWORD_SET(verbose) THEN PRINT, 'File ' + infile[i] + ' processed.'							;Print verbose message
ENDFOR

END