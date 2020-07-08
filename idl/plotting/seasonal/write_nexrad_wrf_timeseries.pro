PRO WRITE_NEXRAD_WRF_TIMESERIES, run, experiment, start_date, end_date, $
	DOMAIN        = domain, $
	REGION        = region, $
	CONVECTIVE 	  = convective, $
	NONCONVECTIVE = nonconvective, $
	TROPICS		  = tropics, $
	MIDLATS   	  = midlats, $
	PNG	     	  = png, $
	EPS   	 	  = eps


;+
; Name:
;		WRITE_H2O_TIMESERIES_FILE
; Purpose:
;		Calculates mass (kg) of h2o in some layer
; Calling sequence:
;		WRITE_H2O_TIMESERIES_FILE, run, scheme, start_date, end_date
;		WRITE_H2O_TIMESERIES_FILE, '20120530_ncar','d03_30km','20120530T2200Z','20120531T0500Z'
; Input:
;		event      : Model simulation name. (e.g., '20120519')
;		scheme 	   : Model initial state. Typically 'morrison' or 'nssl'.
;		start_date : String variable of the start date requested (e.g., '20120519T2300Z')
;		end_date   : String variable of the end date requested (e.g., '20120520T0300Z')
; Output:
;		Map of tropopause relative cloud tops with markers where cloud top > 1km above 
;		tropopause
; Keywords:
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		VERBOSE : If set, message writing process.
; Author and history:
;		Daniel B. Phoenix	    2018-10-22. 
;-

COMPILE_OPT IDL2																				;Set compile options

start_time = SYSTIME(/SECONDS)	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1											;Set default domain

dbz_thres = 5.0

outdir  = !WRF_DIRECTORY + run + '/' + experiment + '/reduced/nexrad_wrf_timeseries/'
FILE_MKDIR, outdir

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(run, experiment, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(run, experiment, start_date, end_date, /DATE)	

z = (WRF_READ_VAR('Z', date_arr[0], run, experiment, DOMAIN = domain, INDICES = region)).values
dim = SIZE(z,/DIMENSIONS)
    
cloud_conv_fraction    = [ ]
wrf_etop_conv_fraction = [ ]
gr_conv_fraction	   = [ ] 

date_index = 0
index0  = FLTARR(dim[0],dim[1])*!Values.F_NaN

FOREACH date, date_arr DO BEGIN
	PRINT, date

	nexrad  = NEXRAD_3_1_LEVEL3_3D_READ_NCDF(date, version = '3_1', product = '3d')
	nexrad = NEXRAD_FILTER(nexrad)
	
	values   = nexrad.dbz.values
	map_plot = MAX((values GE dbz_thres)*(FINITE(SHIFT(values,0,0,1)))*$				;Compute altitude of reflectivity surface
					(FINITE(SHIFT(values,0,0,2)))*REBIN(REFORM(nexrad.z.values, 1, 1, $	
					nexrad.z.n), nexrad.x.n, nexrad.y.n, nexrad.z.n, /SAMPLE), DIM = 3, /NAN)
	
	izero = WHERE((map_plot EQ 0.0), n0)															;Find 0s
	IF (n0 GT 0) THEN map_plot[izero] = !Values.F_NaN											;Remove altitudes for areas with no echo

    y     = (WRF_READ_VAR('Latitude'       , date, run, experiment, DOMAIN=domain, INDICES=region)).values
    x     = (WRF_READ_VAR('Longitude'      , date, run, experiment, DOMAIN=domain, INDICES=region)).values
    z     = (WRF_READ_VAR('Z'		       , date, run, experiment, DOMAIN=domain, INDICES=region)).values
    ztrop = (WRF_READ_VAR('Z_trop'	       , date, run, experiment, DOMAIN=domain, INDICES=region)).values	
	cloud = (WRF_READ_VAR('CLOUD_MIX_TOTAL', date, run, experiment, DOMAIN=domain, INDICES=region)).values
	refl  = (WRF_READ_VAR('REFL'		   , date, run, experiment, DOMAIN=domain, INDICES=region)).values	

	dim   = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
    
 	IF (experiment EQ 'd02_30km'	   ) THEN dx = 2500.0
	IF (experiment EQ 'd03_30km'	   ) THEN dx = 500.0
	IF (experiment EQ 'd03_30km_icloud') THEN dx = 500.0
	IF ((run EQ '20110518') OR (run EQ '20130805')) THEN dx = 3000.0

	ztrop    = MEDIAN(ztrop, 30)    
	trop_3d  = REBIN (ztrop, dim[0], dim[1], dim[2], /SAMPLE)
	
    overshoot    = 0.001*((MAX((cloud    GE 1.0E-5   )*z, DIM = 3, /NAN)) - trop_3d[*,*,0])						;Set map variable to cloud top altitude
    wrf_etop     = 0.001*((MAX((refl     GE dbz_thres)*z, DIM = 3, /NAN)) - trop_3d[*,*,0])						;Set map variable to cloud top altitude

	;+ start nexrad convective fraction calculation
	ix = INTERPOL(FINDGEN(nexrad.x.n),nexrad.x.values,x+360.0)
	iy = INTERPOL(FINDGEN(nexrad.y.n),nexrad.y.values,y	     )

	gr_etop      = INTERPOLATE(map_plot,ix,iy)
	gr_overshoot = (gr_etop - (trop_3d[*,*,0]*0.001))	

	gr_over  = WHERE(gr_overshoot GT 0.5, gr_count, COMPLEMENT = gr_nover)	
	gr_conv_fraction = [TEMPORARY(gr_conv_fraction), FLOAT(gr_count)/FLOAT(N_ELEMENTS(gr_overshoot))]
	;- end nexrad convective fraction calculation
 
    overshoot1 = WHERE(overshoot GT 0.5, over_count, COMPLEMENT = nover     )	
    wrf_etop1  = WHERE(wrf_etop GT 0.0,  etop_count, COMPLEMENT = etop_nover) 					

	cloud_conv_fraction    = [TEMPORARY(cloud_conv_fraction)   , FLOAT(over_count)/FLOAT(N_ELEMENTS(overshoot))]
	wrf_etop_conv_fraction = [TEMPORARY(wrf_etop_conv_fraction), FLOAT(etop_count)/FLOAT(N_ELEMENTS(wrf_etop))]
 
	date_index = date_index + 1

ENDFOREACH

dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string
date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Create date string

outdir  = !WRF_DIRECTORY + run + '/' + experiment + '/plots/h2o_timeseries/'

epsfile = outdir + dom_string + '_' + date_string + '_final2.eps'											;EPS filename
pdffile = outdir + dom_string + '_' + date_string + '.pdf'											;PDF filename
pngfile = outdir + dom_string + '_' + date_string + '.png'											;PNG filename

FILE_MKDIR, outdir																								;Create output directory, if necessary
wfactor = 400.0/(dim[0]) + 400.0/(dim[1])																	;Set map factor

IF KEYWORD_SET(z_buff) THEN BEGIN
	SET_PLOT, 'Z'																									;Output to Z buffer
	DEVICE, SET_PIXEL_DEPTH = 24, SET_RESOLUTION = [wfactor*(dim[0]), wfactor*(dim[1])], $	;Set device resolution and bit depth
		SET_CHARACTER_SIZE = [12, 20]
	!P.COLOR      = COLOR_24('black')																		;Foreground color
	!P.BACKGROUND = COLOR_24('white')																		;Background color
	!P.CHARSIZE   = 1.5																							;Set character size
	!P.FONT       = -1
ENDIF ELSE BEGIN
	IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN	
		PS_ON, FILENAME = epsfile, PAGE_SIZE = [16.0,8.0], MARGIN = 0.0, /INCHES;PAGE_SIZE = 0.001*dim*wfactor			;Switch to Postscript device
		DEVICE, /ENCAPSULATED
		!P.FONT     = 0																								;Hardware fonts
		!P.CHARSIZE = 0.75	
		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
			LOAD_BASIC_COLORS																							;Load basic color definitions
	ENDIF ELSE BEGIN
		SET_PLOT, 'X'
		WINDOW, XSIZE = wfactor*(dim[0]), YSIZE = wfactor*(dim[1])										;Open graphics window
		!P.COLOR      = COLOR_24('black')																		;Foreground color
		!P.BACKGROUND = COLOR_24('white')																		;Background color
		!P.CHARSIZE   = 2.0		
		!P.FONT       = -1																							;Use Hershey fonts
	ENDELSE
ENDELSE

IF (run EQ '20110518') THEN BEGIN 
    PLOT, wrf_etop_conv_fraction, $
    	YRANGE 	 = [0.0,5.0], $
    	YTITLE   = 'WRF Echo Top Fraction (%)', $
    	COLOR    = COLOR_24('black'), $
    	TITLE    = 'Convective Fraction', $
 		XTICKINTERVAL = 12, $
    	CHARSIZE = 2, $
    	/NODATA
    
    AXIS, YAXIS = 0, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE 	 = [0.0,5.0], $
    	YTITLE   = 'WRF Echo Top Fraction (%)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
  		XTICKINTERVAL = 12, $
	    CHARSIZE = 2
   	
   OPLOT, wrf_etop_conv_fraction*100.0, COLOR = COLOR_24('red'), THICK=2
   OPLOT, [0,250], [0,0], COLOR = COLOR_24('black'), THICK=2
        
     AXIS, YAXIS = 0, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE 	 = [0.0, 5.0], $
    	YTITLE   = 'GridRad Echo Top Fraction (%)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
  		XTICKINTERVAL = 12, $
	    CHARSIZE = 2
    	
    OPLOT, gr_conv_fraction*100.0, COLOR = COLOR_24('black'), THICK=4

    AXIS, YAXIS = 1, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE 	 = [0.0, 5.0], $
    	YTITLE   = 'WRF Cloud Top Fraction (%)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 6, $
    	YSTYLE   = 1, $
 		XTICKINTERVAL = 12, $
    	CHARSIZE = 2
    	
	OPLOT, cloud_conv_fraction*100.0,  COLOR = COLOR_24('gray30'), THICK=3, LINESTYLE=2
    
  XYOUTS, 12, 6.8E8, '- dh2o/dt' , COLOR = COLOR_24('red'	  ), CHARSIZE=2, /DATA
  XYOUTS, 12, 6.7E8, '- net' 	, COLOR = COLOR_24('darkgreen'), CHARSIZE=2, /DATA
  XYOUTS, 12, 6.6E8, '- h2o flux', COLOR = COLOR_24('blue'     ), CHARSIZE=2, /DATA
  XYOUTS, 12, 6.5E8, '- Tot h2o' , COLOR = COLOR_24('gray60'   ), CHARSIZE=2, /DATA
  XYOUTS, 12, 6.4E8, 'h2o-conv'  , COLOR = COLOR_24('gray30'   ), CHARSIZE=2, /DATA
  XYOUTS, 12, 6.3E8, 'h2o-nconv' , COLOR = COLOR_24('gray70'   ), CHARSIZE=2, /DATA
ENDIF

IF (run EQ '20130805') THEN BEGIN 
    PLOT, wrf_etop_conv_fraction, $
    	YRANGE 	 = [0.0,0.5], $
    	YTITLE   = 'WRF Echo Top Fraction (%)', $
    	COLOR    = COLOR_24('black'), $
    	TITLE    = 'Convective Fraction', $
 		XTICKINTERVAL = 12, $
    	CHARSIZE = 2, $
    	/NODATA
    
    AXIS, YAXIS = 0, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE 	 = [0.0,0.5], $
    	YTITLE   = 'WRF Echo Top Fraction (%)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
  		XTICKINTERVAL = 12, $
	    CHARSIZE = 2
   	
   OPLOT, wrf_etop_conv_fraction*100.0, COLOR = COLOR_24('red'), THICK=2
   OPLOT, [0,250], [0,0], COLOR = COLOR_24('black'), THICK=2
        
     AXIS, YAXIS = 0, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE 	 = [0.0, 0.5], $
    	YTITLE   = 'GridRad Echo Top Fraction (%)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
  		XTICKINTERVAL = 12, $
	    CHARSIZE = 2
    	
    OPLOT, gr_conv_fraction*100.0, COLOR = COLOR_24('black'), THICK=4

    AXIS, YAXIS = 1, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE 	 = [0.0, 2.0], $
    	YTITLE   = 'WRF Cloud Top Fraction (%)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 6, $
    	YSTYLE   = 1, $
 		XTICKINTERVAL = 12, $
    	CHARSIZE = 2
    	
	OPLOT, cloud_conv_fraction*100.0,  COLOR = COLOR_24('gray30'), THICK=3, LINESTYLE=2
    
  XYOUTS, 12, 6.8E8, '- dh2o/dt' , COLOR = COLOR_24('red'	  ), CHARSIZE=2, /DATA
  XYOUTS, 12, 6.7E8, '- net' 	, COLOR = COLOR_24('darkgreen'), CHARSIZE=2, /DATA
  XYOUTS, 12, 6.6E8, '- h2o flux', COLOR = COLOR_24('blue'     ), CHARSIZE=2, /DATA
  XYOUTS, 12, 6.5E8, '- Tot h2o' , COLOR = COLOR_24('gray60'   ), CHARSIZE=2, /DATA
  XYOUTS, 12, 6.4E8, 'h2o-conv'  , COLOR = COLOR_24('gray30'   ), CHARSIZE=2, /DATA
  XYOUTS, 12, 6.3E8, 'h2o-nconv' , COLOR = COLOR_24('gray70'   ), CHARSIZE=2, /DATA
ENDIF

IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
	
	IF KEYWORD_SET(pdf) THEN PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS						;Convert to PDF
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file

STOP

PRINT, 'time to run code: ' , SYSTIME(/SECONDS)-start_time

fname = outdir+run+'_etop.txt'
OPENW, lun, fname, /GET_LUN     

PRINTF, lun, FORMAT = '("  Date", 6X, "d%h2o/dt", 6X, "Horiz. Flux", 6X, "Vert. Flux", 6X, "%h2o-Conv.", 6X, "%h2o-NConv.", 6X, "%h2o-Total", 6X, "layer size")'								
PRINTF, lun, '============================================================================================================================'

FOR i = 0, N_ELEMENTS(date_arr)-1 DO BEGIN
	PRINTF, lun, FORMAT = '(I3, 4X, E12.4, 4X, E12.4, 4X, E12.4, 4X, E12.4, 4X, E12.4, 4X, E12.4, 4X, F12.2)', i,(dh2o_dt[i]/dair_dt[i]),(time_flux[i]/time_air_flux[i]), net[i],h2o_vertflux_conv[i], h2o_vertflux_nconv[i],h2o_vertflux[i], value_count_arr[i]		
ENDFOR
FREE_LUN, lun


STOP
END

