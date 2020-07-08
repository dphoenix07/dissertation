FUNCTION TMATRIX_READ_VAR, var, run, state, file, t, $
	DOMAIN  = domain, $
	INDICES = indices

;+
; Name:
;		TMATRIX_READ_VAR
; Purpose:
;		This is a function to read regridded WRF files. 
; Calling sequence:
;		R = TMATRIX_READ_VAR('data', MAKE_DATE(2012, 4, 5), '20120519', 'morrison')
; Inputs:
;		var   : WRF netCDF variable name. (e.g., 'REFL')
;		date  : Analysis date
;		run   : Simulation name. (e.g., '20120519')
;		state : Source for initial model state. (e.g., 'morrison')
; Output:
;		Structure of WRF variable.
; Keywords:
;		DOMAIN  : Domain number. (e.g., 1)
;		INDICES : x-y indices to read. 
; Author and history:
;		Cameron R. Homeyer  2012-11-26.
;		Daniel B. Phoenix	2016-06-04. Edited original to read output from PRS.
;-

COMPILE_OPT IDL2																				;Set Compile Options


infile  = !WRF_DIRECTORY + run + '/tmatrix/' + state + '/' + file + '/' + $
			file + '.ncm0k' + STRTRIM(t,1) + '_MF1t000000_dBZ.nc'   														 ;Set input file path


IF FILE_TEST(infile) THEN BEGIN
	id  = NCDF_OPEN(infile)																						;Open input file for reading	
	inq = NCDF_INQUIRE(id)

;	IF (inq.ngatts GT 0) THEN BEGIN
;		NCDF_ATTGET, id, 'DX', dx, /GLOBAL
;	ENDIF ELSE BEGIN
;		CASE domain OF
;			1 : dx = 10000.0
;			2 : dx = 2000.0
;		ENDCASE
;	ENDELSE

	IF (N_ELEMENTS(indices) GT 0) THEN BEGIN
		ioff = (indices[0] < indices[2])
		nx   = (indices[0] > indices[2]) - ioff + 1
		joff = (indices[1] < indices[3])
		ny   = (indices[1] > indices[3]) - joff + 1

		vid = NCDF_VARID( id, var)
		inq = NCDF_VARINQ(id, vid)
		IF (inq.ndims EQ 3) THEN BEGIN
			NCDF_DIMINQ, id, 2, name, nz
			NCDF_VARGET, id, var, values, OFFSET = [ioff,joff,0], COUNT = [nx,ny,nz]			;Read variable
		ENDIF ELSE $
			NCDF_VARGET, id, var, values, OFFSET = [ioff,joff  ], COUNT = [nx,ny   ]			;Read variable
	ENDIF ELSE $
		NCDF_VARGET, id, var, values																			;Read variable
	
	NCDF_CLOSE,  id																								;Close input file
	
	RETURN, {values    : values}     
ENDIF ELSE $
	RETURN, -1																										;If file doesn't exist, return missing flag

END