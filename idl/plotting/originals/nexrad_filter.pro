FUNCTION NEXRAD_FILTER, data

;+
; Name:
;		NEXRAD_FILTER
; Purpose:
;		This is a function to filter NEXRAD composites by bin weights. 
; Calling sequence:
;		data = NEXRAD_FILTER(data)
; Inputs:
;		data : NEXRAD composite data structure.
; Output:
;		Filtered NEXRAD composite data structure.
; Keywords:
;		None.
; Author and history:
;		Cameron R. Homeyer  2015-10-02.
;							2015-01-05. Slight modification to sparse format filtering.
;-

COMPILE_OPT IDL2																									;Set Compile Options

data2 = data																										;Copy data structure for filtering

nc_info  = TAG_NAMES(data)																						;Get tag names
var_info = TAG_NAMES(data.dbz)																				;Get tag names

void = WHERE((var_info EQ 'WVALUES'     ), v2orv3test)												;Check for v2 or v3 compositing method file
void = WHERE((nc_info  EQ 'FILES_MERGED'), v3test    )												;Test if v3 composite
void = WHERE((nc_info  EQ 'NOBS'        ), v3sparse  )												;Test if v3 sparse format

IF (v3test) THEN BEGIN
	IF (v3sparse) THEN BEGIN
		wthresh  = 1.33 - 1.0*(data.date.year LT 2009)													;Set default bin weight threshold for filtering

		echo_frequency = FLOAT(data.nobs)																	;Initialize array to compute frequency of radar obs with echo
		ipos           = WHERE((data.nobs GT 0),npos)													;Find bins with obs
		IF (npos GT 0) THEN $
			echo_frequency[ipos] = FLOAT(data.necho[ipos])/FLOAT(data.nobs[ipos])				;Compute echo frequency (number of scans with echo out of total number of scans)
		echo_thresh    = 0.5
	ENDIF ELSE BEGIN
		wthresh  = 3.0 - 2.34*(data.date.year LT 2009)													;Set default bin weight threshold for filtering
		wthresh2 = 0.33																							;Set default bin weight threshold for filtering
	ENDELSE
ENDIF ELSE wthresh = 0.33

IF (v2orv3test) THEN BEGIN
	IF (SIZE(data.zdr, /TNAME) EQ 'STRUCT') THEN BEGIN
		IF (v3test) THEN BEGIN
			IF (v3sparse) THEN BEGIN
				wvalues = data2.zdr.wvalues
				ifilter = WHERE((((wvalues LT wthresh ) AND (data2.dbz.values LE 18.5)) OR $	;Find observations with low weight
									  ((echo_frequency LT echo_thresh) AND (data.nobs GE 3))), nfilter)
				IF (nfilter GT 0) THEN data2.zdr.values[ifilter] = !Values.F_NaN					;Remove low weight obs			

				wvalues = data2.kdp.wvalues
				ifilter = WHERE((((wvalues LT wthresh ) AND (data2.dbz.values LE 18.5)) OR $	;Find observations with low weight
									  ((echo_frequency LT echo_thresh) AND (data.nobs GE 3))), nfilter)
				IF (nfilter GT 0) THEN data2.kdp.values[ifilter] = !Values.F_NaN					;Remove low weight obs			

				wvalues = data2.rhv.wvalues
				ifilter = WHERE((((wvalues LT wthresh ) AND (data2.dbz.values LE 18.5)) OR $	;Find observations with low weight
									  ((echo_frequency LT echo_thresh) AND (data.nobs GE 3))), nfilter)
				IF (nfilter GT 0) THEN data2.rhv.values[ifilter] = !Values.F_NaN					;Remove low weight obs
			ENDIF ELSE BEGIN
				wvalues = data2.zdr.wvalues
				ifilter = WHERE((((wvalues LT wthresh ) AND (data2.dbz.values LE 18.5)) OR $	;Find observations with low weight
										(wvalues LT wthresh2) OR  (data2.dbz.values LT  2.5)), nfilter)
				IF (nfilter GT 0) THEN data2.zdr.values[ifilter] = !Values.F_NaN					;Remove low weight obs			

				wvalues = data2.kdp.wvalues
				ifilter = WHERE((((wvalues LT wthresh ) AND (data2.dbz.values LE 18.5)) OR $	;Find observations with low weight
										(wvalues LT wthresh2) OR  (data2.dbz.values LT  2.5)), nfilter)
				IF (nfilter GT 0) THEN data2.kdp.values[ifilter] = !Values.F_NaN					;Remove low weight obs			

				wvalues = data2.rhv.wvalues
				ifilter = WHERE((((wvalues LT wthresh ) AND (data2.dbz.values LE 18.5)) OR $	;Find observations with low weight
										(wvalues LT wthresh2) OR  (data2.dbz.values LT  2.5)), nfilter)
				IF (nfilter GT 0) THEN data2.rhv.values[ifilter] = !Values.F_NaN					;Remove low weight obs
			ENDELSE
		ENDIF ELSE BEGIN
			wvalues = data2.zdr.wvalues
			ifilter = WHERE((wvalues LT wthresh), nfilter)												;Find observations with low weight
			IF (nfilter GT 0) THEN data2.zdr.values[ifilter] = !Values.F_NaN						;Remove low weight obs

			wvalues = data2.kdp.wvalues
			ifilter = WHERE((wvalues LT wthresh), nfilter)												;Find observations with low weight
			IF (nfilter GT 0) THEN data2.kdp.values[ifilter] = !Values.F_NaN						;Remove low weight obs		
		ENDELSE
	ENDIF
ENDIF

IF (v2orv3test) THEN wvalues = data2.dbz.wvalues $														;Extract weights
					 ELSE wvalues = 0.0*data2.dbz.values + 1.0											;Otherwise, create dummy weights		 

IF (v3test) THEN BEGIN
	IF (v3sparse) THEN $
		ifilter = WHERE((((wvalues LT wthresh ) AND (data2.dbz.values LE 18.5)) OR $			;Find observations with low weight
							  ((echo_frequency LT echo_thresh) AND (data.nobs GE 3))), nfilter) ELSE $
		ifilter = WHERE((((wvalues LT wthresh ) AND (data2.dbz.values LE 18.5)) OR $			;Find observations with low weight
								(wvalues LT wthresh2) OR  (data2.dbz.values LT  2.5)), nfilter)
ENDIF ELSE $
	ifilter = WHERE((wvalues LT wthresh), nfilter)														;Find observations with low weight

IF (nfilter GT 0) THEN data2.dbz.values[ifilter] = !Values.F_NaN									;Remove low weight obs

RETURN, data2																										;Return filtered data structure

END
