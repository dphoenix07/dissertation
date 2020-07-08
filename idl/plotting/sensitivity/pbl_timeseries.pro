PRO PBL_TIMESERIES, event, start_date, end_date, $
	PBL_HGT  = pbl_hgt, $
	SFC_TEMP = sfc_temp, $
	SFC_PRES = sfc_pres, $
	SFC_HFX  = sfc_hfx, $
	SFC_LH	 = sfc_lh, $
	BOWEN    = bowen, $
	TD		 = Td, $
	ABSVOR   = absvor, $
	PRECIP   = precip, $
	GAS	   	 = gas, $
	CHEM_VAR = chem_var 

;+
; Name:
;		PBL_TIMESERIES
; Purpose:
;		Compares the PBL height for given range of times among different PBL schemes
; Calling sequence:
;		PBL_TIMESERIES, event, start_date, end_date
; Input:
;		event      : Model simulation name. (e.g., '20120519')
;		start_date : String variable of the start date requested (e.g., '20120519T2300Z')
;		end_date   : String variable of the end date requested (e.g., '20120520T0300Z')
; Output:
;		Table of PBL schemes and respective mean pbl height for time range
; Keywords:
;		PBL_HGT    : If set, prints table of mean pbl heights
;		SFC_TEMP   : If set, prints table of mean surface temperatures
;		SFC_PRES   : If set, prints table of mean surface pressures
; Author and history:
;		Daniel B. Phoenix	    2016-07-27. 
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (KEYWORD_SET(event) EQ 0) THEN event = '20120519'
domain = 2

CASE event OF
	'20120519' : BEGIN
  		schemes = ['nssl_ysu','nssl_qnse','nssl_acm2']
;		region  = [50, 50, 250, 190]															;Subset region 
		region  = [98,147,103,152]																;Small box near initiation
  		END

  	'20120529' : BEGIN
  		schemes = ['nssl','qnse','acm2']	
  		region  = [50, 115, 230, 270] 
  		END
  	
  	'20120530' : BEGIN
  		schemes = ['tracer']
  		END
  		
  	'20120601' : BEGIN
  		schemes = ['nssl','qnse','acm2']	
  		END
ENDCASE


date = MK_DATE_ARR(event, schemes[0], start_date, end_date, /DATE)								;Create array of CDATEs 
time = MK_DATE_ARR(event, schemes[0], start_date, end_date, /TIME)
num_date = N_ELEMENTS(date)
var_mean = FLTARR(num_date,3)
var_arr = [ ] 

PRINT, FORMAT = '("Date", 10X, "YSU", 10X, "QNSE", 10X, "ACM2")'								;Print table header information
PRINT, '====================================================================='

IF KEYWORD_SET(PBL_HGT) THEN BEGIN																;Print table for mean pbl height
	title  = 'PBL Height (m)'
	FOR i =0, num_date-1 DO BEGIN
		FOR s = 0,2 DO BEGIN
			var  = (WRF_READ_VAR('PBL', date[i], event, schemes[s], DOMAIN = domain, INDICES = region)).values
			var_mean [*,s] = MEAN(var)
		ENDFOR
		PRINT, FORMAT = '(I2, 7X, F10.4, 3X, F10.4, 4X, F10.4)', i, $
				var_mean[i,0], var_mean[i,1], var_mean[i,2]	

	var_arr = [[[var_arr]],[[var_mean]]]
	ENDFOR
ENDIF

IF KEYWORD_SET(SFC_TEMP) THEN BEGIN																;Print table for mean surface temperature
	title = 'Surface Temperature (K)'
	FOR i =0, num_date-1 DO BEGIN
		FOR s = 0, 2 DO BEGIN
			var  = (WRF_READ_VAR('T_sfc', date[i], event, schemes[s], DOMAIN = domain, INDICES = region)).values
			var_mean [*,s] = MEAN(var)
		ENDFOR
		PRINT, FORMAT = '(I2, 7X, F10.4, 3X, F10.4, 4X, F10.4)', i, $
				var_mean[i,0], var_mean[i,1], var_mean[i,2]	

	var_arr = [[[var_arr]],[[var_mean]]]
	ENDFOR
ENDIF

IF KEYWORD_SET(SFC_PRES) THEN BEGIN																;Print table for mean surface pressure
	title = 'Surface Pressure (hPa)'
	FOR i =0, num_date-1 DO BEGIN
		FOR s = 0, 2 DO BEGIN
			var  = (WRF_READ_VAR('p_sfc', date[i], event, schemes[s], DOMAIN = domain, INDICES = region)).values
			var_mean [*,s] = MEAN(var)
		ENDFOR
		PRINT, FORMAT = '(I2, 7X, F10.4, 3X, F10.4, 4X, F10.4)', i, $
				var_mean[i,0], var_mean[i,1], var_mean[i,2]	

	var_arr = [[[var_arr]],[[var_mean]]]
	ENDFOR
ENDIF

IF KEYWORD_SET(SFC_HFX) THEN BEGIN																;Print table for mean surface sensible heat flux
	title = 'Surface Heat Flux (W m-2)'
	FOR i =0, num_date-1 DO BEGIN
		FOR s = 0, 2 DO BEGIN
			var  = (WRF_READ_VAR('HFX', date[i], event, schemes[s], DOMAIN = domain, INDICES = region)).values
			var_mean [*,s] = MEAN(var)
		ENDFOR
		PRINT, FORMAT = '(I2, 7X, F10.4, 3X, F10.4, 4X, F10.4)', i, $
				var_mean[i,0], var_mean[i,1], var_mean[i,2]	

	var_arr = [[[var_arr]],[[var_mean]]]
	ENDFOR
ENDIF

IF KEYWORD_SET(SFC_LH) THEN BEGIN																;Print table for mean surface latent heat flux
	title = 'Surface Latent Heat Flux (W m-2)'
	FOR i =0, num_date-1 DO BEGIN
		FOR s = 0, 2 DO BEGIN
			var  = (WRF_READ_VAR('LH', date[i], event, schemes[s], DOMAIN = domain, INDICES = region)).values
			var_mean [*,s] = MEAN(var)		
		ENDFOR
		PRINT, FORMAT = '(I2, 7X, F10.4, 3X, F10.4, 4X, F10.4)', i, $
				var_mean[i,0], var_mean[i,1], var_mean[i,2]	

	var_arr = [[[var_arr]],[[var_mean]]]
	ENDFOR
ENDIF

IF KEYWORD_SET(BOWEN) THEN BEGIN																;Print table for mean surface bowen ratio
	title = 'Bowen Ratio at the Surface'
	FOR i =0, num_date-1 DO BEGIN		
		FOR s = 0, 2 DO BEGIN
			sfc_hfx  = (WRF_READ_VAR('HFX', date[i], event, schemes[s], DOMAIN = domain, INDICES = region)).values
			sfc_lh  = (WRF_READ_VAR('LH', date[i], event, schemes[s], DOMAIN = domain, INDICES = region)).values
			var   = (sfc_hfx / sfc_lh) ^ (-1)
			var_mean [*,s] = MEAN(var)
		ENDFOR
	PRINT, FORMAT = '(I2, 7X, F10.4, 3X, F10.4, 4X, F10.4)', i, $
			var_mean[i,0], var_mean[i,1], var_mean[i,2]	

	var_arr = [[[var_arr]],[[var_mean]]]
	ENDFOR
ENDIF

IF KEYWORD_SET(Td) THEN BEGIN																;Print table for mean surface bowen ratio
	title = 'Dew Point Temperature at the Surface'
	FOR i =0, num_date-1 DO BEGIN		
		FOR s = 0, 2 DO BEGIN
			var  = (WRF_READ_VAR('Td', date[i], event, schemes[s], DOMAIN = domain, INDICES = region)).values
			var   = var[*,*,0]
			var_mean [*,s] = MEAN(var)
		ENDFOR
	PRINT, FORMAT = '(I2, 7X, F10.4, 3X, F10.4, 4X, F10.4)', i, $
			var_mean[i,0], var_mean[i,1], var_mean[i,2]	

	var_arr = [[[var_arr]],[[var_mean]]]
	ENDFOR
ENDIF

IF KEYWORD_SET(absvor) THEN BEGIN																;Print table for mean surface bowen ratio
	title = 'Absolute Vorticity at the Surface'
	FOR i =0, num_date-1 DO BEGIN		
		FOR s = 0, 2 DO BEGIN
			var  = (WRF_READ_VAR('absvor', date[i], event, schemes[s], DOMAIN = domain, INDICES = region)).values
			var   = var[*,*,0]
			var_mean [*,s] = MEAN(var)
		ENDFOR
	PRINT, FORMAT = '(I2, 7X, F10.4, 3X, F10.4, 4X, F10.4)', i, $
			var_mean[i,0], var_mean[i,1], var_mean[i,2]	

	var_arr = [[[var_arr]],[[var_mean]]]
	ENDFOR
ENDIF

IF KEYWORD_SET(precip) THEN BEGIN																;Print table for mean surface bowen ratio
	title = 'Accumulated Grid-scale Precipitation at the Surface'
	FOR i =0, num_date-1 DO BEGIN		
		FOR s = 0, 2 DO BEGIN
			var  = (WRF_READ_VAR('PRECIP', date[i], event, schemes[s], DOMAIN = domain, INDICES = region)).values
			var_mean [*,s] = MEAN(var)
		ENDFOR
	PRINT, FORMAT = '(I2, 7X, F10.4, 3X, F10.4, 4X, F10.4)', i, $
			var_mean[i,0], var_mean[i,1], var_mean[i,2]	

	var_arr = [[[var_arr]],[[var_mean]]]
	ENDFOR
ENDIF


IF KEYWORD_SET(gas) THEN BEGIN																;Print table for mean surface bowen ratio
	title = chem_var
	FOR i =0, num_date-1 DO BEGIN		
		FOR s = 0, 2 DO BEGIN
			var  = (WRF_READ_VAR(chem_var, date[i], event, schemes[s], DOMAIN = domain, INDICES = region)).values * 1.0E3 
			var_mean [*,s] = MEAN(var[*,*,0])
		ENDFOR
	PRINT, FORMAT = '(I2, 7X, F10.4, 3X, F10.4, 4X, F10.4)', i, $
			var_mean[i,0], var_mean[i,1], var_mean[i,2]	

	var_arr = [[[var_arr]],[[var_mean]]]
	ENDFOR
ENDIF

yrange = [MIN(var_arr[0,*,*]), MAX(var_arr[0,*,*])]
PLOT, var_arr, /NODATA, $
	TITLE  	  = title, $
	XRANGE 	  = [0, num_date-1], $	
	XTICKS 	  = (num_date-1), $
	XMINOR 	  = 1, $
	XTICKNAME = time, $
	YRANGE 	  = yrange, $
	XTITLE 	  = 'Time (UTC)', $
	YTITLE 	  = title, $
	CHARSIZE  = 1.5
	
OPLOT, var_arr[0,0,*], PSYM=0
OPLOT, var_arr[0,1,*], PSYM=1
OPLOT, var_arr[0,2,*], PSYM=2

END 