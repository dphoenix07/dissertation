FUNCTION WRF_READ_VAR, var, date, run, state, $
	DOMAIN   = domain, $
	WRAPPING = wrapping, $
	H2O		 = h2o, $
	BIN_PRO  = bin_pro, $
	TR_TR	 = tr_tr, $
	OVER	 = over, $
	UT		 = ut, $
	INDICES  = indices

;+
; Name:
;		WRF_READ_VAR
; Purpose:
;		This is a function to read regridded WRF files. 
; Calling sequence:
;		T = WRF_READ_VAR('T', MAKE_DATE(2012, 4, 5), '20120519', 'morrison')
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
;-

COMPILE_OPT IDL2																				;Set Compile Options

IF (N_ELEMENTS(state   ) EQ 0) THEN state    = 'morrison'											;Set default initial model state
IF (N_ELEMENTS(domain  ) EQ 0) THEN domain   = 2
IF (N_ELEMENTS(wrapping) EQ 0) THEN wrapping = 0
IF (N_ELEMENTS(h2o	   ) EQ 0) THEN h2o 	 = 0

dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)") + '_'										;Set domain string

reduced = '/reduced/'

IF (KEYWORD_SET(wrapping)) THEN BEGIN
	reduced = '/reduced/o3_wrapping/'
	;dom_string = 'anvil_flag_'
	;dom_string = 'o3_anom_'
	dom_string = 'stt_flag_'
ENDIF

IF (KEYWORD_SET(h2o)) THEN BEGIN
	reduced = '/reduced/h2o_timeseries_bugfix/'
	dom_string = 'h2o_350k_'
ENDIF

IF (KEYWORD_SET(over)) THEN BEGIN
	;reduced = '/plots/overshoot_tracking/'
	;dom_string = 'overshoot_statistics_'
	reduced = '/reduced/overshoot_statistics_v2/'
	dom_string = ''
ENDIF

IF (KEYWORD_SET(ut)) THEN BEGIN
	reduced = '/reduced/overshoot_statistics_v2_ut/'
	dom_string = ''
ENDIF

IF (KEYWORD_SET(bin_pro)) THEN BEGIN
	reduced = '/reduced/binned_profiles/'
	dom_string = ''
ENDIF

IF (KEYWORD_SET(tr_tr)) THEN BEGIN
	reduced = '/reduced/tracer_tracer_files_h2o_o3/'
	dom_string = ''
ENDIF

;IF (KEYWORD_SET(tr_tr)) THEN BEGIN
;	reduced = '/reduced/tracer_tracer_files_co_o3/'
;	dom_string = ''
;ENDIF


date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Create date string
infile      = !WRF_DIRECTORY + run + '/' + state + reduced + dom_string $
			+ date_string + '.nc'																;Set input file path

IF FILE_TEST(infile) THEN BEGIN
	id  = NCDF_OPEN(infile)																						;Open input file for reading	
	inq = NCDF_INQUIRE(id)

	NCDF_ATTGET, id, var, 'long_name', name
	NCDF_ATTGET, id, var, 'units',     units

	IF (inq.ngatts GT 0) THEN BEGIN
		NCDF_ATTGET, id, 'DX', dx, /GLOBAL
	ENDIF ELSE BEGIN
		CASE domain OF
			1 : dx = 10000.0
			2 : dx = 2000.0
		ENDCASE
	ENDELSE

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
	
	RETURN, {values    : values,       $																	;Return variable structure
				long_name : STRING(name), $
				units     : STRING(units), $
				dx        : dx }
ENDIF ELSE $
	RETURN, -1																										;If file doesn't exist, return missing flag

END
