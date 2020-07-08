PRO W_TIMESERIES, event, start_date, end_date, $
	W_TROP = w_trop, $
	DOMAIN = domain, $
	PNG    = png


;+
; Name:
;               W_TIMESERIES
; Purpose:
;               This is a procedure to plot the domain-maximum vertical velocity
;				for a series of timesteps. Multiple model simulations (e.g., can 
;				use different MP schemes) are plotted.
; Calling sequence:
;               W_TIMESERIES, event, scheme, variable, date, end_hour
; Inputs:
;               event      : String variable of run name. (e.g., '20120519')
;				scheme     : String variable of microphysics scheme (e.g., 'morrison')
;				start_date : String variable of the start date (e.g., '20120519T2300Z')
;				end_date   : String variable of the end date (e.g., '20120520T0300Z')
; Output:
;               A timeseries plot of vertical velocity for different model simulations.
; Keywords:
;				PNG		   : Set if output to PNG desired.
; Author and history:
;               Daniel B. Phoenix	2016-01-21.
;-


COMPILE_OPT IDL2																				;Set compile options

IF (N_ELEMENTS(event	 ) EQ 0) THEN event 	 = '20120519'									;Set defaults
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'
IF (N_ELEMENTS(domain	 ) EQ 0) THEN domain	 = 2
IF (N_ELEMENTS(png		 ) EQ 0) THEN png		 = 0

scheme = 'morrison'

date_arr = MK_DATE_ARR(event, scheme, start_date, end_date, /DATE)								
ndates 	 = N_ELEMENTS(date_arr) 																;Allocate array

;times = MK_DATE_ARR(event, scheme, start_date, end_date, /TIME)

mo_w_arr   = [ ]
my_w_arr   = [ ]
nssl_w_arr = [ ]

FOREACH date, date_arr DO BEGIN
		w    = (WRF_READ_VAR('w'     , date, event, 'morrison', DOMAIN = domain)).values
		z    = (WRF_READ_VAR('Z'     , date, event, 'morrison', DOMAIN = domain)).values
		trop = (WRF_READ_VAR('Z_trop', date, event, 'morrison', DOMAIN = domain)).values
		IF KEYWORD_SET(w_trop) THEN BEGIN
			dim = SIZE(w, /DIMENSIONS)
			nx  = dim[0]
			ny  = dim[1]
			nz  = dim[2]
			i   = REBIN(FINDGEN(nx), nx, ny)
			j   = REBIN(REFORM(FINDGEN(ny), 1, ny), nx, ny)
			k   = REBIN(REFORM(FINDGEN(nz), 1, 1, nz), nx, ny, nz)
			k   = MAX((ABS(z - REBIN(trop,nx,ny,nz, /SAMPLE) LE 1.0))*k, DIM = 3)
			w   = INTERPOLATE(w, i, j, k)
			mo_w_max = MAX(w)
			mo_w_arr = [mo_w_arr, mo_w_max]
		ENDIF ELSE BEGIN
		mo_w_max = MAX(w)
		mo_w_arr = [mo_w_arr, mo_w_max]
		ENDELSE
ENDFOREACH

FOREACH date, date_arr DO BEGIN
		w    = (WRF_READ_VAR('w'     , date, event, 'milbyau', DOMAIN = domain)).values
		z    = (WRF_READ_VAR('Z'     , date, event, 'milbyau', DOMAIN = domain)).values
		trop = (WRF_READ_VAR('Z_trop', date, event, 'milbyau', DOMAIN = domain)).values
		IF KEYWORD_SET(w_trop) THEN BEGIN
			dim = SIZE(w, /DIMENSIONS)
			nx  = dim[0]
			ny  = dim[1]
			nz  = dim[2]
			i   = REBIN(FINDGEN(nx), nx, ny)
			j   = REBIN(REFORM(FINDGEN(ny), 1, ny), nx, ny)
			k   = REBIN(REFORM(FINDGEN(nz), 1, 1, nz), nx, ny, nz)
			k   = MAX((ABS(z - REBIN(trop,nx,ny,nz, /SAMPLE) LE 1.0))*k, DIM = 3)
			w   = INTERPOLATE(w, i, j, k)
			my_w_max = MAX(w)
			my_w_arr = [my_w_arr, my_w_max]
		ENDIF ELSE BEGIN
		my_w_max = MAX(w)
		my_w_arr = [my_w_arr, my_w_max]
		ENDELSE
ENDFOREACH

FOREACH date, date_arr DO BEGIN
		w    = (WRF_READ_VAR('w'     , date, event, 'nssl', DOMAIN = domain)).values
		z    = (WRF_READ_VAR('Z'     , date, event, 'nssl', DOMAIN = domain)).values
		trop = (WRF_READ_VAR('Z_trop', date, event, 'nssl', DOMAIN = domain)).values
		IF KEYWORD_SET(w_trop) THEN BEGIN
			dim = SIZE(w, /DIMENSIONS)
			nx  = dim[0]
			ny  = dim[1]
			nz  = dim[2]
			i   = REBIN(FINDGEN(nx), nx, ny)
			j   = REBIN(REFORM(FINDGEN(ny), 1, ny), nx, ny)
			k   = REBIN(REFORM(FINDGEN(nz), 1, 1, nz), nx, ny, nz)
			k   = MAX((ABS(z - REBIN(trop,nx,ny,nz, /SAMPLE) LE 1.0))*k, DIM = 3)
			w   = INTERPOLATE(w, i, j, k)
			nssl_w_max = MAX(w)
			nssl_w_arr = [nssl_w_arr, nssl_w_max]
		ENDIF ELSE BEGIN
		nssl_w_max = MAX(w)
		nssl_w_arr = [nssl_w_arr, nssl_w_max]
		ENDELSE
ENDFOREACH



;PLOT, mo_w_arr, XTITLE =  'Timestep', YTITLE = 'Vertical Velocity (m/s)'
;OPLOT, my_w_arr, LINESTYLE = 1
;OPLOT, nssl_w_arr, LINESTYLE = 2

title = 'Maximum W (m/s) '
region = 'in Domain'
IF KEYWORD_SET(w_trop) THEN region = 'at Tropopause'

MO    = PLOT(mo_w_arr, TITLE = title + region , XTITLE =  'Timestep', $
			 YTITLE = 'Maximum W (m/s)')
MY    = PLOT(my_w_arr, LINESTYLE = 1, /OVERPLOT)
NSSL  = PLOT(nssl_w_arr, LINESTYLE = 2, /OVERPLOT)
leg   = LEGEND(TARGET = [MO, MY, NSSL], /LINESTYLE)

leg[0].label = 'MO'
leg[1].label = 'MY'
leg[2].label = 'NSSL'



END