PRO OVERSHOOT_ZTROP_BIN, event, start_date, end_date, $
	DOMAIN       = domain, $
	PERCENTILE   = percentile, $
	PERCENT_DIFF = percent_diff, $
	VAR2         = var2, $
	ALT		     = alt, $
	REGION       = region, $
	NONCONV      = nonconv, $
	PNG	         = png, $
	EPS   	     = eps


;+
; Name:
;		OVERSHOOT_ZTROP_BIN
; Purpose:
;		Maps O3, CO, and H2O concentrations to
; Calling sequence:
;		OVERSHOOT_ZTROP_BIN, run, start_date, end_date
; Example: OVERSHOOT_ZTROP_BIN,'20110518','20110520T0000Z','20110520T0300Z'
; Input:
;		event      : Model simulation name. (e.g., '20120519')
;		start_date : String variable of the start date requested (e.g., '20120519T2300Z')
;		end_date   : String variable of the end date requested (e.g., '20120520T0300Z')
; Output:
;		Mean O3, CO, and H2O concentrations binned in tropopause height vs tropopause
;		relative height altitude for convective and non-convective air
; Keywords:
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
; Author and history:
;		Daniel B. Phoenix	    2019-04-01. 
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1											;Set default domain
IF (N_ELEMENTS(in_cloud  ) EQ 0) THEN in_cloud   = 1
IF (N_ELEMENTS(threshold ) EQ 0) THEN threshold  = 1000.0
IF (N_ELEMENTS(xrange	 ) EQ 0) THEN xrange     = [  0, 800]
IF (N_ELEMENTS(xtitle	 ) EQ 0) THEN xtitle     = 'Ozone (ppbv)'
IF (N_ELEMENTS(yrange	 ) EQ 0) THEN yrange     = [-5,   5]
IF (N_ELEMENTS(ytitle	 ) EQ 0) THEN ytitle     = 'Relative Altitude'
IF (N_ELEMENTS(nxbin 	 ) EQ 0) THEN nxbin      = 50
IF (N_ELEMENTS(nybin 	 ) EQ 0) THEN nybin      = 50
IF (N_ELEMENTS(var2		 ) EQ 0) THEN var2		 = 0

CASE event OF
	'20110518' : BEGIN
		scheme = ['seasonal_final/bigger_domain']
		label  = ['racm-yellowstone']
		key    = 'test'
		END
	'20130805' : BEGIN
		scheme = ['nest_final']
		label  = ['racm-yellowstone']
		key    = 'test_aug'
		END
ENDCASE

IF (KEYWORD_SET(ALT)) THEN BEGIN
	yrange = [0, 25]
	ytitle = 'Altitude'
ENDIF

IF (N_ELEMENTS(region) EQ 0) THEN BEGIN
	IF (event EQ '20120519') THEN region = [50, 50, 250, 190]
ENDIF

outdir  = !WRF_DIRECTORY + event + '/paper/plots/chemistry_profiles/'
epsfile = outdir + key + '_' + end_date + '.eps'						;EPS filename
pdffile = outdir + key + '_' + end_date + '.pdf'						;PDF filename
pngfile = outdir + key + '_' + end_date + '.png'						;PNG filename

FILE_MKDIR, outdir

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(event, scheme, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(event, scheme, start_date, end_date, /DATE)	

IF ~KEYWORD_SET(nowindow) THEN BEGIN
	IF KEYWORD_SET(eps) THEN BEGIN	
		PS_ON, FILENAME = epsfile, PAGE_SIZE = [6.0, 8.0], MARGIN = 0.0, /INCHES				;Switch to Postscript device
		DEVICE, /ENCAPSULATED
		!P.FONT     = 0																			;Hardware fonts
		!P.CHARSIZE = 1.0	
		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
			LOAD_BASIC_COLORS																	;Load basic color definitions
	ENDIF ELSE BEGIN
		SET_PLOT, 'X'
		WINDOW, XSIZE = 1200, YSIZE = 900														;Open graphics window
		!P.COLOR      = COLOR_24('black')														;Foreground color
		!P.BACKGROUND = COLOR_24('white')														;Background color
		!P.CHARSIZE   = 2.0		
		!P.FONT       = -1																		;Use Hershey fonts
	ENDELSE
ENDIF

!P.MULTI=[0,3,3]

in_cloud = 0
c = 0

var_arr = ['H2O', 'O3', 'CO']
tracer1_c   = 0
ralt1_c     = 0
xyz_trop1_c = 0
trop_3d_nc  = 0
ralt_arr_nc = 0
tracer2_nc  = 0

FOREACH var, var_arr DO BEGIN 
	in_cloud = c
	
	;FOR c = 0, 1 DO BEGIN  
        ;IF c EQ 0 THEN BEGIN
        ;	IF var EQ 'H2O'  THEN BEGIN
        ;		position = [3,2,1]
        ;		bar_title  = 'H2O (ppmv)'
        ;		IF KEYWORD_SET(percent_diff) THEN xtitle   = 'H2O (%)'
        ;		tr_min = 1.0
        ;		tr_max = 600.0
        ;		position = [0.07,0.52,0.28,0.54]
        ;	ENDIF
        ;	IF var EQ 'O3'   THEN BEGIN
        ;		position = [3,2,2]
        ;		bar_title   = 'O3 (ppbv)'
        ;		IF KEYWORD_SET(percent_diff) THEN xtitle   = 'O3 (%)'
        ;		tr_min = 0.0
        ;		tr_max = 1000.0
        ;		position = [0.42,0.52,0.63,0.54]
		;	ENDIF
        ;	IF var EQ 'CO'   THEN BEGIN
        ;		position = [3,2,3]
        ;		bar_title   = 'CO (ppbv)'
        ;		IF KEYWORD_SET(percent_diff) THEN xtitle   = 'CO (%)'
        ;		tr_min = 0.0
        ;		tr_max = 100.0
        ;		position = [0.75,0.52,0.97,0.54]
        ;	ENDIF
        ;ENDIF 
        ;IF c EQ 1 THEN BEGIN
        ;	IF var EQ 'H2O'  THEN BEGIN
        ;		position = [3,2,4]
        ;		bar_title   = 'H2O (ppmv)'
        ;		IF KEYWORD_SET(percent_diff) THEN xtitle   = 'H2O (%)'
        ;		tr_min = 1.0
        ;		tr_max = 600.0
        ;	ENDIF
        ;	IF var EQ 'O3'   THEN BEGIN
        ;		position = [3,2,5]
        ;		bar_title   = 'O3 (ppbv)'
        ;		IF KEYWORD_SET(percent_diff) THEN xtitle   = 'O3 (%)'
        ;		tr_min = 0.0
        ;		tr_max = 1000.0
		;	ENDIF
        ;	IF var EQ 'CO'   THEN BEGIN
        ;		position = [3,2,6]
        ;		bar_title   = 'CO (ppbv)'
        ;		IF KEYWORD_SET(percent_diff) THEN xtitle   = 'CO (%)'
        ;		tr_min = 0.0
        ;		tr_max = 100.0
        ;	ENDIF
        ;ENDIF
                
 	  	IF KEYWORD_SET(percent_diff) THEN xrange1 = [-200,200]        
        yrange = [-5, 5]
        IF (KEYWORD_SET(ALT)) THEN yrange = [0, 25] 

        plt_var1 = [ ] 
        ralt_arr_c = [ ] 
		trop_3d_c = [ ]
		tracer2_c = [ ]
		ralt_arr_nc = [ ] 
		trop_3d_nc = [ ]
		tracer2_nc = [ ]

        	FOREACH date, date_arr DO BEGIN
				PRINT, "Processing date: ", date
        		z		 = (WRF_READ_VAR('Z'     , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read height values
        		dim 	 = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
        		z_trop   = (WRF_READ_VAR('Z_trop', date, event, scheme, DOMAIN = domain, INDICES = region)).values
        		z_trop   = MEDIAN(z_trop, 30) 												
        
        		precip   = (WRF_READ_VAR('PRECIP'	     , date, event, scheme, DOMAIN = domain, INDICES = region)).values
        		w	     = (WRF_READ_VAR('w' 	 	     , date, event, scheme, DOMAIN = domain, INDICES = region)).values
        		cld_tr   = (WRF_READ_VAR('Cloud_tracer'  , date, event, scheme, DOMAIN = domain, INDICES = region)).values
        		updraft  = (WRF_READ_VAR('Updraft_tracer', date, event, scheme, DOMAIN = domain, INDICES = region)).values
        
        		xyz_trop = REBIN(z_trop, dim[0], dim[1], dim[2], /SAMPLE)
        
        		cloud    = (WRF_READ_VAR('CLOUD_MIX_TOTAL' , date, event, scheme, DOMAIN = domain, INDICES = region)).values * 1.0E3			;Read in cloud mixing ratio values   
        		plt_var1  = (WRF_READ_VAR(var      		  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ozone values
        		
        		IF var EQ 'O3'   THEN factor = 1.0E3
        		IF var EQ 'H2O'  THEN factor = 1.0E6
        		IF var EQ 'CO'   THEN factor = 1.0E3
        		
        		tracer = plt_var1 * factor
        		ralt   = (z - xyz_trop) * 1.0E-3
                		
        		IF (KEYWORD_SET(ALT)) THEN ralt = z * 1.0E-3
        
				;IF (c EQ 0) THEN BEGIN
				   conv_values = WHERE(updraft GT 0.1, conv_count, COMPLEMENT = non_conv, $							;Find values in cloud 
									NCOMPLEMENT = nconv_count)
			  
					IF (conv_count GT 0) THEN BEGIN
						tracer1_c   = tracer  [conv_values]
						ralt1_c     = ralt    [conv_values]
						xyz_trop1_c = xyz_trop[conv_values]
					ENDIF
	  
				;ENDIF ELSE BEGIN
				   nconv_values = WHERE(updraft LT 0.1, nconv_count, COMPLEMENT = conv, $							;Find values in cloud 
									NCOMPLEMENT = conv_count)
			  
					IF (nconv_count GT 0) THEN BEGIN
						tracer1_nc   = tracer  [nconv_values]
						ralt1_nc     = ralt    [nconv_values]
						xyz_trop1_nc = xyz_trop[nconv_values]
					ENDIF
				;ENDELSE       
    	    					
    	    	trop_3d_c  = [trop_3d_c, xyz_trop1_c*1.0E-3]
	        	ralt_arr_c = [ralt_arr_c, ralt1_c ]
	        	tracer2_c  = [tracer2_c, tracer1_c]

    	    	trop_3d_nc  = [trop_3d_nc, xyz_trop1_nc*1.0E-3]
	        	ralt_arr_nc = [ralt_arr_nc, ralt1_nc ]
	        	tracer2_nc  = [tracer2_nc, tracer1_nc]
        	ENDFOREACH	;date

		yrange = [-4,4]
		nybin  = 16
		dy     = FLOAT(yrange[1] - yrange[0])/nybin															;Compute y bin spacing
		ybin   = 0.5*dy + yrange[0] + dy*FINDGEN(nybin)

		xrange = [10,18]
		nxbin  = 8
		dx     = FLOAT(xrange[1] - xrange[0])/nxbin														;Set bin parameters for regular scale
		xbin   = 0.5*dx + dx*FINDGEN(nxbin) + xrange[0]

		FOR p = 0, 2 DO BEGIN
			IF p EQ 0 THEN BEGIN
				trop_3d = trop_3d_c
				ralt_arr = ralt_arr_c
				tracer2 = tracer2_c

				IF var EQ 'H2O'  THEN BEGIN
					position1 = [3,2,1]
					bar_title  = 'H2O (ppmv)'
					IF KEYWORD_SET(percent_diff) THEN xtitle   = 'H2O (%)'
					tr_min = 1.0
					tr_max = 200.0
					tr_min_diff = -300.0
					tr_max_diff =  300.0
					position = [0.25,0.69,0.47,0.71]
				ENDIF
				IF var EQ 'O3'   THEN BEGIN
					position1 = [3,2,2]
					bar_title   = 'O3 (ppbv)'
					IF KEYWORD_SET(percent_diff) THEN xtitle   = 'O3 (%)'
					tr_min = 0.0
					tr_max = 600.0
					tr_min_diff = -2300.0
					tr_max_diff =  2300.0
					position = [0.25,0.36,0.47,0.38]
				ENDIF
				IF var EQ 'CO'   THEN BEGIN
					position1 = [3,2,3]
					bar_title   = 'CO (ppbv)'
					IF KEYWORD_SET(percent_diff) THEN xtitle   = 'CO (%)'
					tr_min = 0.0
					tr_max = 120.0
					tr_min_diff = -40.0
					tr_max_diff =  40.0
					position = [0.25,0.04,0.47,0.06]
				ENDIF
			ENDIF 
			IF p EQ 1 THEN BEGIN
				trop_3d = trop_3d_nc
				ralt_arr = ralt_arr_nc
				tracer2 = tracer2_nc
				
				IF var EQ 'H2O'  THEN BEGIN
					position1 = [3,2,4]
					bar_title   = 'H2O (ppmv)'
					IF KEYWORD_SET(percent_diff) THEN xtitle   = 'H2O (%)'
					tr_min = 1.0
					tr_max = 200.0
				ENDIF
				IF var EQ 'O3'   THEN BEGIN
					position1 = [3,2,5]
					bar_title   = 'O3 (ppbv)'
					IF KEYWORD_SET(percent_diff) THEN xtitle   = 'O3 (%)'
					tr_min = 0.0
					tr_max = 600.0
				ENDIF
				IF var EQ 'CO'   THEN BEGIN
					position1 = [3,2,6]
					bar_title   = 'CO (ppbv)'
					IF KEYWORD_SET(percent_diff) THEN xtitle   = 'CO (%)'
					tr_min = 0.0
					tr_max = 120.0
				ENDIF
			ENDIF
			IF p EQ 2 THEN BEGIN
				IF var EQ 'H2O'  THEN BEGIN
					bar_title  = 'H2O (ppmv)'
					IF KEYWORD_SET(percent_diff) THEN xtitle   = 'H2O (%)'
					tr_min_diff = -300.0
					tr_max_diff =  300.0
					position_diff = [0.75,0.69,0.95,0.71]
				ENDIF
				IF var EQ 'O3'   THEN BEGIN
					bar_title   = 'O3 (ppbv)'
					IF KEYWORD_SET(percent_diff) THEN xtitle   = 'O3 (%)'
					tr_min_diff = -200.0
					tr_max_diff =  200.0
					position_diff = [0.75,0.36,0.95,0.38]
				ENDIF
				IF var EQ 'CO'   THEN BEGIN
					bar_title   = 'CO (ppbv)'
					IF KEYWORD_SET(percent_diff) THEN xtitle   = 'CO (%)'
					tr_min_diff = -40.0
					tr_max_diff =  40.0
					position_diff = [0.75,0.04,0.95,0.06]
				ENDIF
			ENDIF 

			tracer_mean = FLTARR(nxbin, nybin)
			ibin = 0
			FOR xx = 0, nxbin-1 DO BEGIN
				FOR yy = 0, nybin-1 DO BEGIN
					ibin = WHERE((trop_3d GE xbin[xx]-0.5*dx) AND (trop_3d LT xbin[xx]+0.5*dx) AND $
							(ralt_arr GE ybin[yy]-0.5*dy) AND (ralt_arr LT ybin[yy]+0.5*dy), nbin)

					IF (nbin GT 0) THEN $
						tracer_mean [xx,yy] = MEAN(tracer2[ibin],/NAN)
				ENDFOR
			ENDFOR

			IF (p EQ 0) THEN tracer_mean_c = tracer_mean
			IF (p EQ 1) THEN tracer_mean_nc = tracer_mean

			IF (p NE 2) THEN BEGIN
				pmax   = 25.0*(LONG(MEAN(tracer_mean) + 2*STDDEV(tracer_mean))/25 + 1)											;Calculate maximum count for each gas concentration w/in 2 STD DEV												
				table = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])
				col    = COLOR_LOOKUP_24(tracer_mean, table, MIN_VALUE = tr_min, MAX_VALUE = tr_max, MISSING = table[-1])
		 
				none   = WHERE((tracer_mean EQ 0), none_count)
				IF (none_count GT 0) THEN col[none] = COLOR_24('white')												;Set counts of zero to white
			ENDIF ELSE BEGIN
				tracer_mean_diff = tracer_mean_c - tracer_mean_nc
				
				PRINT, tracer_mean_diff
				pmax   = 25.0*(LONG(MEAN(tracer_mean_diff) + 2*STDDEV(tracer_mean_diff))/25 + 1)											;Calculate maximum count for each gas concentration w/in 2 STD DEV												
 				table  = BLUE_RED_24(1000, 0.1, 1.0, PS = ps)
				col    = COLOR_LOOKUP_24(tracer_mean_diff, table, MIN_VALUE = tr_min_diff, MAX_VALUE = tr_max_diff, MISSING = COLOR_24('white')	)
				PRINT, tr_min_diff
				PRINT, tr_max_diff
				none   = WHERE(((tracer_mean_c EQ 0) OR (tracer_mean_nc EQ 0)), none_count)
				IF (none_count GT 0) THEN col[none] = COLOR_24('white')												;Set counts of zero to white
				
			ENDELSE
		
			PLOT, xbin, ybin, /NODATA, $														;Set up plot window for normal scale
				TITLE    = title, $
				XRANGE   = xrange, $
				XSTYLE   = 1, $
				XTICKNAM = REPLICATE(' ', 20), $
				XTICKLEN = 0.0001, $
				YRANGE   = yrange, $
				YSTYLE   = 1, $
				YMARGIN  = [8,2], $
				YTICKNAM = REPLICATE(' ', 20)
		
			FOR j = 0, nybin -1 DO BEGIN
				FOR i = 0, nxbin -1 DO BEGIN

					POLYFILL, [xrange[0] + i*dx,     xrange[0] + (i+1)*dx, $					;Draw polygons (for normal scale)
								  xrange[0] + (i+1)*dx, xrange[0] + i*dx, $
								  xrange[0] + i*dx], $
								 [yrange[0] + j*dy,     yrange[0] + j*dy, $
								  yrange[0] + (j+1)*dy, yrange[0] + (j+1)*dy, $
								  yrange[0] + j*dy], $
								 COLOR = col[i,j], /DATA
				ENDFOR
			ENDFOR

			AXIS, YAXIS = 0, $																		;Redraw axes that are covered by hist
				YRANGE = yrange, $
				YSTYLE = 1, $
				YTITLE = ytitle, $
				_EXTRA = _extra

			AXIS, XAXIS = 0, /SAVE, $
				XRANGE = xrange, $
				XLOG   = xlog, $
				XSTYLE = 1, $
				XTITLE = 'Tropopause Height (km)', $
				_EXTRA = _extra

			AXIS, YAXIS = 1, $																		;Redraw axes that are covered by hist
				YRANGE = yrange, $
				YTICKN = REPLICATE(' ', 20), $
				YSTYLE = 1, $
				_EXTRA = _extra

			AXIS, XAXIS = 1, $
				XRANGE = xrange, $
				XLOG   = xlog, $
				XTICKN = REPLICATE(' ', 20), $
				XSTYLE = 1, $
				_EXTRA = _extra

			xy   = CONVERT_COORD(xrange, yrange, /DATA, /TO_NORMAL)									;Normalize x and y coordinates to [0,1]
			dxax = xy[0,1] - xy[0,0]																;Compute difference between xy
			dyax = xy[1,1] - xy[1,0]
			
			x1   = xy[0,0] + 0.1*dxax																;Compute coordinates to center color bar
			x2   = xy[0,1] - 0.1*dxax
			y1   = xy[1,0] - 0.26*dyax
			y2   = xy[1,0] - 0.24*dyax

			OPLOT, xrange, [0.0, 0.0]																;Plot RALT 0 reference line	

			IF (p NE 2) THEN BEGIN
				COLOR_BAR_24, table, $
					RANGE = [tr_min, tr_max], $
					TITLE = bar_title, $
					TICKS = 1, $
					POSIT = position
			ENDIF

			IF (p EQ 2) THEN BEGIN
				COLOR_BAR_24, table, $
					RANGE = [tr_min_diff, tr_max_diff], $
					TITLE = bar_title, $
					TICKS = 1, $
					POSIT = position_diff
			ENDIF
		ENDFOR
	;ENDFOR
ENDFOREACH ;var



!P.MULTI = 0
IF KEYWORD_SET(eps) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																;Reset color table to linear ramp
	PS_OFF																						;Turn PS off
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)															;Write PNG file

STOP
END