PRO PLOT_OVERSHOOT_RELATIONSHIPS, event, scheme, $
	DOMAIN   = domain, $
	PNG	     = png, $
	EPS   	 = eps


;+
; Name:
;		PLOT_OVERSHOOT_RELATIONSHIPS
; Purpose:
;		Plots relationships between tropopause temperature, overshoot depth, h2o, o3, co 
;		concentration, vertical velocity, and RHi
; Calling sequence:
;		PLOT_OVERSHOOT_RELATIONSHIPS, run, scheme, start_date, end_date
; Input:
;		event      : Model simulation name. (e.g., '20120519')
;		scheme 	   : Model initial state. Typically 'morrison' or 'nssl'.
; Output:
;		Timeseries of fraction of convection in domain and chemical signature 
; Keywords:
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		VERBOSE : If set, message writing process.
; Author and history:
;		Daniel B. Phoenix	    2018-07-17. 
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1											;Set default domain

outdir  = !WRF_DIRECTORY + event + '/' + scheme + '/plots/overshoot_tracking/'
FILE_MKDIR, outdir

dim = SIZE(x, /DIMENSIONS)

dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string
date = MAKE_DATE(2011,5,23,23)

overshoot_total  = (WRF_READ_VAR('Max_Overshoot'     , date, event, scheme, DOMAIN=domain, INDICES=region, OVER=1)).values
max_h2o_total    = (WRF_READ_VAR('Max_H2O'           , date, event, scheme, DOMAIN=domain, INDICES=region, OVER=1)).values							;Read temperature variable from WRF output
ave_h2o_total    = (WRF_READ_VAR('Mean_H2O'          , date, event, scheme, DOMAIN=domain, INDICES=region, OVER=1)).values					
max_h2o_nc_total = (WRF_READ_VAR('Max_H2O_NC'        , date, event, scheme, DOMAIN=domain, INDICES=region, OVER=1)).values							;Read temperature variable from WRF output
ave_h2o_nc_total = (WRF_READ_VAR('Mean_H2O_NC'       , date, event, scheme, DOMAIN=domain, INDICES=region, OVER=1)).values					
max_cloud_total  = (WRF_READ_VAR('Max_Cloud'         , date, event, scheme, DOMAIN=domain, INDICES=region, OVER=1)).values							;Read temperature variable from WRF output
ave_cloud_total  = (WRF_READ_VAR('Mean_Cloud'        , date, event, scheme, DOMAIN=domain, INDICES=region, OVER=1)).values					
max_ice_total    = (WRF_READ_VAR('Max_Ice'           , date, event, scheme, DOMAIN=domain, INDICES=region, OVER=1)).values							;Read temperature variable from WRF output
ave_ice_total    = (WRF_READ_VAR('Mean_Ice'          , date, event, scheme, DOMAIN=domain, INDICES=region, OVER=1)).values					
max_rhi_total    = (WRF_READ_VAR('Max_RHI'           , date, event, scheme, DOMAIN=domain, INDICES=region, OVER=1)).values					
ave_rhi_total    = (WRF_READ_VAR('Mean_RHI'          , date, event, scheme, DOMAIN=domain, INDICES=region, OVER=1)).values
max_co_total     = (WRF_READ_VAR('Max_CO'            , date, event, scheme, DOMAIN=domain, INDICES=region, OVER=1)).values
ave_co_total     = (WRF_READ_VAR('Mean_CO'           , date, event, scheme, DOMAIN=domain, INDICES=region, OVER=1)).values
min_o3_total     = (WRF_READ_VAR('Min_O3'	         , date, event, scheme, DOMAIN=domain, INDICES=region, OVER=1)).values
ave_o3_total     = (WRF_READ_VAR('Mean_O3'	         , date, event, scheme, DOMAIN=domain, INDICES=region, OVER=1)).values	
ave_temp_total   = (WRF_READ_VAR('Mean_T'            , date, event, scheme, DOMAIN=domain, INDICES=region, OVER=1)).values
max_w_total      = (WRF_READ_VAR('Max_W'             , date, event, scheme, DOMAIN=domain, INDICES=region, OVER=1)).values
unstable_total   = (WRF_READ_VAR('Unstable_Layers'   , date, event, scheme, DOMAIN=domain, INDICES=region, OVER=1)).values 
unstable_ic      = (WRF_READ_VAR('Unstable_Layers_IC', date, event, scheme, DOMAIN=domain, INDICES=region, OVER=1)).values 

STOP

PLOT, overshoot_total[sort(overshoot_total)], max_h2o_total[sort(overshoot_total)], $
			xtitle = 'Overshoot Depth (km)', ytitle = "Max H2O concentration (ppmv)", PSYM = 4, $
			yrange = [1, 300], ylog = 1, xrange = [0,5]

PLOT, overshoot_total[sort(overshoot_total)], max_h2o_nc_total[sort(overshoot_total)], $
			xtitle = 'Overshoot Depth (km)', ytitle = "Max H2O concentration outside cloud (ppmv)", PSYM = 4, $
			yrange = [1, 300], ylog = 1, xrange = [0,5]

PLOT, overshoot_total, ave_h2o_total, $
			xtitle = 'Overshoot Depth (km)', ytitle = "Mean H2O concentration (ppmv)", PSYM = 4, $
			yrange = [1, 300], ylog = 1, xrange = [0,5]

PLOT, overshoot_total, ave_h2o_nc_total, $
			xtitle = 'Overshoot Depth (km)', ytitle = "Mean H2O concentration outside cloud (ppmv)", PSYM = 4, $
			yrange = [1, 300], ylog = 1, xrange = [0,5]

PLOT, ave_temp_total, max_h2o_total, $
			xtitle = 'Mean Trop. Temp (K)', ytitle = "Max H2O concentration (ppmv)", PSYM = 4, $
			yrange = [1, 300], ylog = 1, xrange = [200,250]

PLOT, max_rhi_total, max_h2o_total, $
			xtitle = 'Max RHi (%)', ytitle = "Max H2O concentration (ppmv)", PSYM = 4, $
			yrange = [1, 300], ylog = 1, xrange = [1,1000]

PLOT, max_rhi_total, max_h2o_nc_total, $
			xtitle = 'Max RHi (%)', ytitle = "Max H2O concentration outside cloud (ppmv)", PSYM = 4, $
			yrange = [1, 300], ylog = 1, xrange = [1,1000]

PLOT, ave_rhi_total, max_h2o_total, $
			xtitle = 'Mean RHi (%)', ytitle = " Mean H2O concentration (ppmv)", PSYM = 4, $
			yrange = [1, 300], ylog = 1, xrange = [0,8]

PLOT, ave_rhi_total, max_h2o_nc_total, $
			xtitle = 'Mean RHi (%)', ytitle = " Mean H2O concentration outside cloud (ppmv)", PSYM = 4, $
			yrange = [1, 300], ylog = 1, xrange = [0,8]

PLOT, ave_temp_total, max_h2o_total, $
			xtitle = 'Mean Trop. Temp (K)', ytitle = "Mean H2O concentration (ppmv)", PSYM = 4, $
			yrange = [1, 300], ylog = 1, xrange = [200,250]

PLOT, max_w_total, ave_h2o_total, $
			xtitle = 'Max W (m/s)', ytitle = "Mean H2O concentration (ppmv)", PSYM = 4, $
			yrange = [0, 500], xrange = [0,40]

PLOT, ave_temp_total, max_w_total,  $
			xtitle = 'Mean Trop. Temp (K)', ytitle = 'Max W (m/s)', PSYM = 4, $
			yrange = [0, 40], xrange = [200,250]

PLOT, overshoot_total, ave_temp_total, $
			xtitle = 'Overshoot Depth (km)', ytitle = 'Mean Trop. Temp (K)', PSYM = 4, $
			yrange = [200,250], xrange = [0,5]

PLOT, overshoot_total,max_w_total, $
			xtitle = 'Overshoot Depth (km)', ytitle = 'Max W (m/s)', PSYM = 4, $
			yrange = [0, 40], xrange = [0,5]

PLOT, overshoot_total, ave_o3_total, $
			xtitle = 'Overshoot Depth (km)', ytitle = 'Mean O3 concentration (ppbv)', PSYM = 4, $
			yrange = [190,950], xrange = [0,5]

PLOT, overshoot_total, min_o3_total, $
			xtitle = 'Overshoot Depth (km)', ytitle = 'Min O3 concentration (ppbv)', PSYM = 4, $
			yrange = [0,100], xrange = [0,5]

PLOT, overshoot_total, ave_co_total, $
			xtitle = 'Overshoot Depth (km)', ytitle = 'Mean CO concentration (ppbv)', PSYM = 4, $
			yrange = [30,80], xrange = [0,5]

PLOT, overshoot_total, max_co_total, $
			xtitle = 'Overshoot Depth (km)', ytitle = 'Max CO concentration (ppbv)', PSYM = 4, $
			yrange = [60,250], xrange = [0,5]

PLOT, max_h2o_total, max_co_total, $
			xtitle = 'Max H2O concentration (ppmv)', ytitle = 'Max CO concentration (ppbv)', PSYM = 4, $
			yrange = [50,250], xrange = [1,300], XLOG = 1 

PLOT, max_h2o_nc_total, max_co_total, $
			xtitle = 'Max H2O concentration outside cloud (ppmv)', ytitle = 'Max CO concentration (ppbv)', PSYM = 4, $
			yrange = [50,250], xrange = [1,300], XLOG = 1 

PLOT, overshoot_total, (max_h2o_total/max_co_total), $
			xtitle = 'Overshoot Depth (km)', ytitle = 'Max H2O:CO ratio', PSYM = 4, $
			yrange = [0,2.5], xrange = [0,5]

PLOT, overshoot_total, (max_h2o_nc_total/max_co_total), $
			xtitle = 'Overshoot Depth (km)', ytitle = 'Max H2O (nc):CO ratio', PSYM = 4, $
			yrange = [0,2.5], xrange = [0,5]

PLOT, max_h2o_total, min_o3_total, $
			xtitle = 'Max H2O concentration (ppmv)', ytitle = 'Min O3 concentration (ppbv)', PSYM = 4, $
			yrange = [0,100], xrange = [1,300], XLOG = 1 

PLOT, max_h2o_nc_total, min_o3_total, $
			xtitle = 'Max H2O concentration outside cloud (ppmv)', ytitle = 'Min O3 concentration (ppbv)', PSYM = 4, $
			yrange = [0,100], xrange = [1,300], XLOG = 1 

PLOT, ave_h2o_total, ave_o3_total, $
			xtitle = 'Mean H2O concentration (ppmv)', ytitle = 'Mean O3 concentration (ppbv)', PSYM = 4, $
			yrange = [190,950], xrange = [1,300], XLOG = 1 

PLOT, ave_h2o_nc_total, ave_o3_total, $
			xtitle = 'Mean H2O concentration outside cloud (ppmv)', ytitle = 'Mean O3 concentration (ppbv)', PSYM = 4, $
			yrange = [190,950], xrange = [1,300], XLOG = 1 

PLOT, ave_h2o_total, ave_co_total, $
			xtitle = 'Mean H2O concentration (ppmv)', ytitle = 'Mean CO concentration (ppbv)', PSYM = 4, $
			yrange = [30,80], xrange = [1,300], XLOG = 1 

PLOT, ave_h2o_nc_total, ave_co_total, $
			xtitle = 'Mean H2O concentration outside cloud (ppmv)', ytitle = 'Mean CO concentration (ppbv)', PSYM = 4, $
			yrange = [30,80], xrange = [1,300], XLOG = 1 

PLOT, max_h2o_total, max_ice_total, $
			xtitle = 'Max H2O concentration (ppmv)', ytitle = 'Max Ice concentration (kg/kg)', PSYM = 4, $
			yrange = [0.0005,0.0025], xrange = [1,300], XLOG = 1

PLOT, max_h2o_nc_total, max_ice_total, $
			xtitle = 'Max H2O concentration outside cloud (ppmv)', ytitle = 'Max Ice concentration (kg/kg)', PSYM = 4, $
			yrange = [0.0005,0.0025], xrange = [1,300], XLOG = 1

PLOT, ave_h2o_total, ave_ice_total, $
			xtitle = 'Mean H2O concentration (ppmv)', ytitle = 'Mean Ice concentration (kg/kg)', PSYM = 4, $
			yrange = [0.000001,0.00025], xrange = [1,300], XLOG = 1

PLOT, ave_h2o_nc_total, ave_ice_total, $
			xtitle = 'Mean H2O concentration outside cloud (ppmv)', ytitle = 'Mean Ice concentration (kg/kg)', PSYM = 4, $
			yrange = [0.000001,0.00025], xrange = [1,300], XLOG = 1

PLOT, max_h2o_total, max_cloud_total, $
			xtitle = 'Max H2O concentration (ppmv)', ytitle = 'Max Cloud concentration (kg/kg)', PSYM = 4, $
			yrange = [0.0002,0.015], xrange = [1,300], XLOG = 1

PLOT, max_h2o_nc_total, max_cloud_total, $
			xtitle = 'Max H2O concentration outside cloud (ppmv)', ytitle = 'Max Cloud concentration (kg/kg)', PSYM = 4, $
			yrange = [0.0002,0.015], xrange = [1,300], XLOG = 1

PLOT, ave_h2o_total, ave_cloud_total, $
			xtitle = 'Mean H2O concentration (ppmv)', ytitle = 'Mean Cloud concentration (kg/kg)', PSYM = 4, $
			yrange = [0.00001,0.0004], xrange = [1,300], XLOG = 1

PLOT, ave_h2o_nc_total, ave_cloud_total, $
			xtitle = 'Mean H2O concentration outside cloud (ppmv)', ytitle = 'Mean Cloud concentration (kg/kg)', PSYM = 4, $
			yrange = [0.00001,0.0004], xrange = [1,300], XLOG = 1

;			
;OPLOT, overshoot_total[sort(overshoot_total)], ave_h2o_total[sort(overshoot_total)], $
;			COLOR = COLOR_24('red'), PSYM = 4


date1   = MAKE_ISO_DATE_STRING(date,/COMPACT,/UTC)

epsfile = outdir +  date1  + '.eps'						;EPS filename
pdffile = outdir +  date1  + '.pdf'						;PDF filename
pngfile = outdir +  date1  + '.png'						;PNG filename


IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
	
	IF KEYWORD_SET(pdf) THEN PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS						;Convert to PDF
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file


END