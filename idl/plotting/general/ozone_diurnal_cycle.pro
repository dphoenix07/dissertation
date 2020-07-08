PRO OZONE_DIURNAL_CYCLE, $
	LANDFALL  = landfall, $
	PNG		  = png, $
	EPS		  = eps, $
	CLOBBER   = clobber

;+
;NAME:
;     OZONE_DIURNAL_CYCLE
;PURPOSE:
;     This copies variables from ERA-Interim analysis into a single
;     file in pressure coordinates for use in TRAJ3D.
;     W at the surface is set to zero.  One pressure level is added at 
;     the top of the domain (p = 0), where w is also set to zero.
;CATEGORY:
;     Data handling utility.
;CALLING SEQUENCE:
;     OZONE_DIURNAL_CYCLE, date0, outfile
;INPUT:
;		start_date : user specified start date to plot in yyyymmdd format (e.g., '20170101')
;		end_date   : user specified end date to plot in yyyymmdd format (e.g., '20180101')
;KEYWORDS:
;     PLOT      : If set, plot sample maps.
;     DIRECTORY : Output directory for wind file.
;	  CLOBBER   : If set, overwrite existing file. This is the default.
;OUTPUT:
;     Netcdf file.
;MODIFICATION HISTORY:
;		D. Phoenix:       2019-02-09.
;-

COMPILE_OPT IDL2																									;Set compile options

yr_arr = ['1980','1981','1982','1983','1984','1985','1986','1987','1988','1989','1990', $
			'1991','1992','1993','1994','1995','1996','1997','1998','1999','2000', $
			'2001','2002','2003','2004','2005','2006','2007','2008','2009','2010', $
			'2011','2012','2013','2014','2015','2016','2017']
;yr_arr = ['2011','2012','2013']

ts_data = read_hurdat2()

nots_arr_tot1 = [ ]
ts_arr_tot1   = [ ]

nots_arr_tot2 = FLTARR(30,(8760/6))
ts_arr_tot2   = FLTARR(30,(8760/6))

nots_yr_tot = [ ]
ts_yr_tot   = [ ]

ilandfall = 0
FOREACH year, yr_arr DO BEGIN
	PRINT, year
    iyear = WHERE((ts_data.date.year GE year) AND (ts_data.date.year LT year+1))
    
    extr_storms = WHERE(((ts_data.class[iyear] EQ 'TD') OR (ts_data.class[iyear] EQ 'TS') OR $
    	(ts_data.class[iyear] EQ 'HU')), istorms)
    IF (istorms GT 0) THEN iperiod = iyear[extr_storms] ELSE iperiod = 0

    IF KEYWORD_SET(landfall) THEN BEGIN
    	landfall = WHERE(((ts_data.id[iyear] EQ 'L') OR (ts_data.id[iyear] EQ 'C')) AND $
    		((ts_data.class[iyear] EQ 'TD') OR (ts_data.class[iyear] EQ 'TS') OR $
    	  	(ts_data.class[iyear] EQ 'HU')), ilandfall)
    	IF (ilandfall GT 0) THEN iperiod = iyear[landfall] ELSE iperiod = 0
    ENDIF

	PRINT, 'ilandfall= ', ilandfall
	PRINT, 'istorms= ', istorms
	PRINT, 'iperiod= ', N_ELEMENTS(iperiod)
    PRINT, 'done reading ts file'
    
    o3dir = !WRF_DIRECTORY + 'general/o3_data/'
    o3file = o3dir + 'dallas_' + year + '.csv'
    o3_data = READ_CSV(o3file)
    
    o3_8hr  = OZONE_DALLAS_NAAQS(year)
    
    pngfile = o3dir + 'timeseries_extrstorms.png'
    IF KEYWORD_SET(landfall) THEN pngfile = o3dir + 'timeseries_landfall_closest_storms.png'
    
    o3_datestr =  STRMID(o3_data.field12,0,4)+STRMID(o3_data.field12,5,2)+STRMID(o3_data.field12,8,2) + $;'T' + $
    				  STRMID(o3_data.field13,0,2)+STRMID(o3_data.field13,3,2) ;+ 'Z'
    
    ts_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),0,4) +  $
    		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),5,2) + $
    		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),8,2) + $; 'T' + $
    		  STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),11,2) + $
    		   STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),14,2) ;+ 'Z'
    
    
    ;Create 1-hour bin with 6 hour interval
    time_arr = [ ]
    date1 = MAKE_DATE(year,01,01,00,00)
    nt_yr = 8760
    FOR i = 0, nt_yr - 1 DO BEGIN
    	datestr = STRMID(MAKE_ISO_DATE_STRING(date1),0,4) + STRMID(MAKE_ISO_DATE_STRING(date1),5,2) + $
    			 STRMID(MAKE_ISO_DATE_STRING(date1),8,2) + STRMID(MAKE_ISO_DATE_STRING(date1),11,2) + $
    		  	 STRMID(MAKE_ISO_DATE_STRING(date1),14,2) 
    	time_arr = [time_arr, datestr]
    	date1 = TIME_INC(date1, 3600)
    ENDFOR 
    
    ;Create O3 bins
    o3_range = [0, 120]
    xrange   = [0, 23]
    num_bins = 24
    do3   	 = FLOAT(o3_range[1] - o3_range[0])/num_bins														;Compute y bin spacing
    dx 		 = 1 
    o3bin 	 = do3 + o3_range[0] + do3*FINDGEN(num_bins)
    
    xtitle = 'Hour (UTC)'
    ytitle = 'O3 (ppb)'
    
    ;loop over hours in day
	nots_arr_tot1 = [ ]
	ts_arr_tot1   = [ ]
	y=0
	FOR i = 0 , nt_yr-1, 24 DO BEGIN
    	nots_arr = [ ]
    	ts_arr   = [ ]
    	FOR hh = 0 , 23 -1 DO BEGIN
    	    date1 = time_arr[i+hh]
    	    PRINT, date1
    	    
    	    io3   = WHERE(o3_datestr EQ date1, o3count)
    	    IF (io3[0] EQ -1) THEN io3 = [0]
    	    its   = WHERE(ts_datestr EQ date1, tscount)
    	    
    	    IF (tscount EQ 0) THEN data_bin = LONG((o3_8hr.ozone_8hr_total[io3]-o3_range[0])/do3) $
    	    	ELSE data_bin = [0]
    	    IF (tscount GT 0) THEN ts_bin   = LONG((o3_8hr.ozone_8hr_total[io3]-o3_range[0])/do3) $
    	    	ELSE ts_bin = [0]
    	    
    	    hist_all = HISTOGRAM(data_bin, BINSIZE = 1, MIN = 0, MAX = (num_bins -1))								;Calculate density
    	    nots_arr = [[nots_arr], [hist_all]]
        
    	    hist_ts = HISTOGRAM(ts_bin, BINSIZE = 1, MIN = 0, MAX = (num_bins -1))								;Calculate density
    	    ts_arr = [[ts_arr],[hist_ts]] 
    	ENDFOR;hour  	
		nots_arr_tot1 = [[[nots_arr_tot1]],[[nots_arr]]]
		ts_arr_tot1   = [[[ts_arr_tot1  ]],[[ts_arr  ]]]
		y+=1
	ENDFOR;day	
	nots_yr_tot = [[[nots_yr_tot]],[[TOTAL(nots_arr_tot1,3)]]]
	ts_yr_tot   = [[[ts_yr_tot]], [[TOTAL(ts_arr_tot1,3)]]]
ENDFOREACH;year

nots_arr_tot = TOTAL(nots_arr_tot1,3)
ts_arr_tot   = TOTAL(ts_arr_tot1,3)

table_nots = GRAY_LOGSCALE_24(20, 0.50, 0.0, PS = ps)	
col_nots   = COLOR_LOOKUP_24(nots_arr_tot, table_nots, MIN_VALUE = 0.0, MAX_VALUE = 1000.0, MISSING = table_nots[-1])
none      = WHERE((nots_arr_tot EQ 0), none_count)
IF (none_count GT 0) THEN col_nots[none] = COLOR_24('white')											;Set counts of zero to white

table_ts = GRAY_LOGSCALE_24(20, 0.50, 0.0, PS = ps)
col_ts   = COLOR_LOOKUP_24(ts_arr_tot, table_ts, MIN_VALUE = 0.0, MAX_VALUE = 1000.0, MISSING = table_ts[-1])
none      = WHERE((ts_arr_tot EQ 0), none_count)
IF (none_count GT 0) THEN col_ts[none] = COLOR_24('white')											;Set counts of zero to white

!P.MULTI = [0, 2, 1]

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
		WINDOW, XSIZE = 1200, YSIZE = 800														;Open graphics window
		!P.COLOR      = COLOR_24('black')														;Foreground color
		!P.BACKGROUND = COLOR_24('white')														;Background color
		!P.CHARSIZE   = 2.0		
		!P.FONT       = -1																		;Use Hershey fonts
	ENDELSE
ENDIF

USERSYM_CIRCLE, /FILL																		;Load circle user plot symbol

PLOT, o3bin, FINDGEN(nt_yr), /NODATA, $														;Set up plot window for normal scale
	TITLE    = 'O3 Non-Storm Periods, Years: ' + STRING(yr_arr[0]) + $
					' - ' + STRING(yr_arr[-1]), $
	XRANGE   = xrange, $
	XSTYLE   = 1, $
	XTICKNAM = REPLICATE(' ', 20), $
	XTICKLEN = 0.0001, $
	YRANGE   = o3_range, $
	YSTYLE   = 1, $
	YMARGIN  = [8,2], $
	YTICKNAM = REPLICATE(' ', 20)

FOR j = 0, num_bins -1 DO BEGIN
	FOR i = 0, 23 -1 DO BEGIN
		POLYFILL, [xrange[0] + i*dx,     xrange[0] + (i+1)*dx, $					;Draw polygons (for normal scale)
					  xrange[0] + (i+1)*dx, xrange[0] + i*dx, $
					  xrange[0] + i*dx], $
					 [o3_range[0] + j*do3,     o3_range[0] + j*do3, $
					  o3_range[0] + (j+1)*do3, o3_range[0] + (j+1)*do3, $
					  o3_range[0] + j*do3], $
					 COLOR = col_nots[j,i], /DATA
	ENDFOR
ENDFOR
		
AXIS, YAXIS = 0, $																		;Redraw axes that are covered by hist
	YRANGE = o3_range, $
	YSTYLE = 1, $
	YTITLE = ytitle, $
	_EXTRA = _extra

AXIS, XAXIS = 0, /SAVE, $
	XRANGE = xrange, $
	XSTYLE = 1, $
	XTITLE = xtitle, $
	_EXTRA = _extra

AXIS, YAXIS = 1, $																		;Redraw axes that are covered by hist
	YRANGE = o3_range, $
	YTICKN = REPLICATE(' ', 20), $
	YSTYLE = 1, $
	_EXTRA = _extra

AXIS, XAXIS = 1, $
	XRANGE = xrange, $
	XTICKN = REPLICATE(' ', 20), $
	XSTYLE = 1, $
	_EXTRA = _extra

OPLOT, xrange, [80.0, 80.0], THICK = 2																			;Plot RALT 0 reference line	

;;;;;;;;;;;;;;;
;;Now plot TS - O3 data

title = 'O3 Storm Periods, Years: ' + STRING(yr_arr[0]) + $
			' - ' + STRING(yr_arr[-1])
IF KEYWORD_SET(landfall) THEN $
	title = 'O3  Close and Landfalling TD Storm Periods, Years: ' + STRING(yr_arr[0]) + $
				' - ' + STRING(yr_arr[-1])

PLOT, o3bin, FINDGEN(nt_yr), /NODATA, $														;Set up plot window for normal scale
	TITLE    = title, $
	XRANGE   = xrange, $
	XSTYLE   = 1, $
	XTICKNAM = REPLICATE(' ', 20), $
	XTICKLEN = 0.0001, $
	YRANGE   = o3_range, $
	YSTYLE   = 1, $
	YMARGIN  = [8,2], $
	YTICKNAM = REPLICATE(' ', 20)

FOR j = 0, num_bins -1 DO BEGIN
	FOR i = 0, 23 -1 DO BEGIN
		POLYFILL, [xrange[0] + i*dx,     xrange[0] + (i+1)*dx, $					;Draw polygons (for normal scale)
					  xrange[0] + (i+1)*dx, xrange[0] + i*dx, $
					  xrange[0] + i*dx], $
					 [o3_range[0] + j*do3,     o3_range[0] + j*do3, $
					  o3_range[0] + (j+1)*do3, o3_range[0] + (j+1)*do3, $
					  o3_range[0] + j*do3], $
					 COLOR = col_ts[j,i], /DATA
	ENDFOR
ENDFOR
		
AXIS, YAXIS = 0, $																		;Redraw axes that are covered by hist
	YRANGE = o3_range, $
	YSTYLE = 1, $
	YTITLE = ytitle, $
	_EXTRA = _extra

AXIS, XAXIS = 0, /SAVE, $
	XRANGE = xrange, $
	XSTYLE = 1, $
	XTITLE = xtitle, $
	_EXTRA = _extra

AXIS, YAXIS = 1, $																		;Redraw axes that are covered by hist
	YRANGE = o3_range, $
	YTICKN = REPLICATE(' ', 20), $
	YSTYLE = 1, $
	_EXTRA = _extra

AXIS, XAXIS = 1, $
	XRANGE = xrange, $
	XTICKN = REPLICATE(' ', 20), $
	XSTYLE = 1, $
	_EXTRA = _extra

OPLOT, xrange, [80.0, 80.0], THICK = 2																			;Plot RALT 0 reference line	
			
COLOR_BAR_24, table_nots, $
	RANGE = [0.0, 1000.0], $
	TITLE = 'Count (#)', $
	TICKS = 1, $
	POSIT = [0.25,0.09,0.75,0.1]

IF ~KEYWORD_SET(nowindow) THEN BEGIN
	IF KEYWORD_SET(eps) THEN BEGIN
		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
			LOAD_BASIC_COLORS, /RESET															;Reset color table to linear ramp
		PS_OFF																					;Turn PS off
	ENDIF ELSE IF KEYWORD_SET(png) THEN $
		WRITE_PNG, pngfile, TVRD(TRUE=1)														;Write PNG image
ENDIF           

STOP
END
