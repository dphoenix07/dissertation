PRO PLOT_OZONE_TS_TIMESERIES, $
	LANDFALL  = landfall, $
	PNG		  = png, $
	EPS		  = eps, $
	CLOBBER   = clobber

;+
;NAME:
;     PLOT_OZONE_TS_RELATIONSHIPS
;PURPOSE:
;     This copies variables from ERA-Interim analysis into a single
;     file in pressure coordinates for use in TRAJ3D.
;     W at the surface is set to zero.  One pressure level is added at 
;     the top of the domain (p = 0), where w is also set to zero.
;CATEGORY:
;     Data handling utility.
;CALLING SEQUENCE:
;     PLOT_OZONE_TS_RELATIONSHIPS, date0, outfile
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

day1 =0
day2 =0
day3 =0
day4 =0
day5 =0
day6 =0
day7 =0
day8 =0
day9 =0
day10=0
day11=0
day12=0
day13=0
day14=0
day15=0
day16=0
day17=0
day18=0
day19=0
day20=0
day21=0
day22=0
day23=0
day24=0
day25=0
day26=0
day27=0
day28=0
day29=0
day30=0
day31=0
storm_counts =0

day1_o3 = []
day2_o3 = []
day3_o3 = []
day4_o3 = []
day5_o3 = []
day6_o3 = []
day7_o3 = []
day8_o3 = []
day9_o3 = []
day10_o3 =[]
day11_o3 =[]
day12_o3 =[]
day13_o3 =[]
day14_o3 =[]
day15_o3 =[]
day16_o3 =[]
day17_o3 =[]
day18_o3 =[]
day19_o3 =[]
day20_o3 =[]
day21_o3 =[]
day22_o3 =[]
day23_o3 =[]
day24_o3 =[]
day25_o3 =[]
day26_o3 =[]
day27_o3 =[]
day28_o3 =[]
day29_o3 =[]
day30_o3 =[]
day31_o3 =[]


ilandfall = 0
FOREACH year, yr_arr DO BEGIN
	PRINT, year
    ;iyear = WHERE((ts_data.date.year GE year) AND (ts_data.date.year LT year+1))
	iyear = WHERE(((ts_data.date.year GE year) AND (ts_data.date.year LT year+1)) AND $
		((ts_data.date.month GE 11) AND (ts_data.date.month LT 12)))
    
    extr_storms = WHERE(((ts_data.class[iyear] EQ 'TD') OR (ts_data.class[iyear] EQ 'TS') OR $
    	(ts_data.class[iyear] EQ 'HU')), istorms)
    IF (istorms GT 0) THEN iperiod = iyear[extr_storms] ELSE iperiod = 0

   IF KEYWORD_SET(landfall) THEN BEGIN
  	 landfall = WHERE(((ts_data.id[iyear] EQ 'L') OR (ts_data.id[iyear] EQ 'C')) AND $
  	 	((ts_data.class[iyear] EQ 'TD') OR (ts_data.class[iyear] EQ 'TS') OR $
  	  	(ts_data.class[iyear] EQ 'HU')), ilandfall)
  	 
  	 ;Fill in other times between making landfall and the end of the storm
  	 IF (ilandfall GT 0) THEN BEGIN
  	 	iperiod = iyear[landfall] 
  	 	FOR lf = 0, N_ELEMENTS(iperiod)-1 DO BEGIN
  	 		land_end    = (ts_data.iend - iperiod[lf])
  	 		land_end2   = WHERE(land_end GT 0)
  	 		land_end3   = WHERE(MIN(land_end2[land_end]))
  	 		endpt 	    = land_end[land_end2[land_end3]]
  	 		index 	    = WHERE(land_end EQ endpt[0])
  	 		iperiod_add = [iperiod[lf]-6:ts_data.iend[index]]
  	 		iperiod 	= [iperiod,iperiod_add]
  	 	ENDFOR
  	 		
  	 ENDIF ELSE iperiod = 0
   ENDIF
	PRINT, 'ilandfall= ', ilandfall
	PRINT, 'istorms= ', istorms
	PRINT, 'iperiod= ', N_ELEMENTS(iperiod)
    PRINT, 'done reading ts file'
    
    o3dir = !WRF_DIRECTORY + 'general/o3_data/'
    o3file = o3dir + 'dallas_' + year + '.csv'    
    o3_data  = OZONE_DALLAS_NAAQS(year)
    
    pngfile = o3dir + 'timeseries_extrstorms.png'
    epsfile = o3dir + 'timeseries_extrstorms_HU.eps'
    IF KEYWORD_SET(landfall) THEN epsfile = o3dir + 'timeseries_landfall_closest_storms.eps'
    
    o3_datestr =  STRMID(o3_data.date,0,4)+STRMID(o3_data.date,5,2)+STRMID(o3_data.date,8,2) + $;'T' + $
    				  STRMID(o3_data.time,0,2)+STRMID(o3_data.time,3,2) ;+ 'Z'
    
    ts_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),0,4) +  $
    		STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),5,2) + $
    		 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),8,2) + $; 'T' + $
    		  STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),11,2) + $
    		   STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),14,2) ;+ 'Z'
    
 	storm_counts += N_ELEMENTS(ts_datestr)

 	FOR tt = 0, N_ELEMENTS(ts_datestr)-1 DO BEGIN
    	IF (STRMID(ts_datestr[tt],6,2) EQ '01') THEN day1 += 1 
    	IF (STRMID(ts_datestr[tt],6,2) EQ '02') THEN day2 += 1 
    	IF (STRMID(ts_datestr[tt],6,2) EQ '03') THEN day3 += 1 
    	IF (STRMID(ts_datestr[tt],6,2) EQ '04') THEN day4 += 1 
    	IF (STRMID(ts_datestr[tt],6,2) EQ '05') THEN day5 += 1 
    	IF (STRMID(ts_datestr[tt],6,2) EQ '06') THEN day6 += 1 
    	IF (STRMID(ts_datestr[tt],6,2) EQ '07') THEN day7 += 1 
    	IF (STRMID(ts_datestr[tt],6,2) EQ '08') THEN day8 += 1 
    	IF (STRMID(ts_datestr[tt],6,2) EQ '09') THEN day9 += 1 
    	IF (STRMID(ts_datestr[tt],6,2) EQ '10') THEN day10 += 1 
    	IF (STRMID(ts_datestr[tt],6,2) EQ '11') THEN day11 += 1 
    	IF (STRMID(ts_datestr[tt],6,2) EQ '12') THEN day12 += 1 
    	IF (STRMID(ts_datestr[tt],6,2) EQ '13') THEN day13 += 1 
    	IF (STRMID(ts_datestr[tt],6,2) EQ '14') THEN day14 += 1 
    	IF (STRMID(ts_datestr[tt],6,2) EQ '15') THEN day15 += 1 
    	IF (STRMID(ts_datestr[tt],6,2) EQ '16') THEN day16 += 1 
    	IF (STRMID(ts_datestr[tt],6,2) EQ '17') THEN day17 += 1 
    	IF (STRMID(ts_datestr[tt],6,2) EQ '18') THEN day18 += 1 
    	IF (STRMID(ts_datestr[tt],6,2) EQ '19') THEN day19 += 1 
    	IF (STRMID(ts_datestr[tt],6,2) EQ '20') THEN day20 += 1 
    	IF (STRMID(ts_datestr[tt],6,2) EQ '21') THEN day21 += 1 
    	IF (STRMID(ts_datestr[tt],6,2) EQ '22') THEN day22 += 1 
    	IF (STRMID(ts_datestr[tt],6,2) EQ '23') THEN day23 += 1 
    	IF (STRMID(ts_datestr[tt],6,2) EQ '24') THEN day24 += 1 
    	IF (STRMID(ts_datestr[tt],6,2) EQ '25') THEN day25 += 1 
    	IF (STRMID(ts_datestr[tt],6,2) EQ '26') THEN day26 += 1 
    	IF (STRMID(ts_datestr[tt],6,2) EQ '27') THEN day27 += 1 
    	IF (STRMID(ts_datestr[tt],6,2) EQ '28') THEN day28 += 1 
    	IF (STRMID(ts_datestr[tt],6,2) EQ '29') THEN day29 += 1 
    	IF (STRMID(ts_datestr[tt],6,2) EQ '30') THEN day30 += 1 
    	IF (STRMID(ts_datestr[tt],6,2) EQ '31') THEN day31 += 1 
	ENDFOR

 
    
    ;Create 1-hour bin with 6 hour interval
    time_arr = [ ]
    date1 = MAKE_DATE(year,11,01,06,00)
    nt_yr = 8760/6
    nt_yr = 124
    FOR i = 0, nt_yr - 1 DO BEGIN
    	datestr = STRMID(MAKE_ISO_DATE_STRING(date1),0,4) + STRMID(MAKE_ISO_DATE_STRING(date1),5,2) + $
    			 STRMID(MAKE_ISO_DATE_STRING(date1),8,2) + STRMID(MAKE_ISO_DATE_STRING(date1),11,2) + $
    		  	 STRMID(MAKE_ISO_DATE_STRING(date1),14,2) 
    	time_arr = [time_arr, datestr]
    	date1 = TIME_INC(date1, 21600)
    ENDFOR 
    
    ;Create O3 bins
    o3_range = [0, 150]
    xrange   = [0, nt_yr]
    num_bins = 30
    do3   	 = FLOAT(o3_range[1] - o3_range[0])/num_bins														;Compute y bin spacing
    dx 		 = 1 
    o3bin 	 = do3 + o3_range[0] + do3*FINDGEN(num_bins)
    
    xtitle = 'Time since Sept 1 (6-hr interval)'
    ytitle = 'O3 Concentration (ppb)'
    
    nots_arr = [ ]
    ts_arr   = [ ]
    FOR i = 0 , nt_yr -1 DO BEGIN
    	date1 = time_arr[i]
		
    	io3   = WHERE(o3_datestr EQ date1, o3count)
    	IF (io3[0] EQ -1) THEN io3 = [0]
    	its   = WHERE(ts_datestr EQ date1, tscount)
    	
    	IF (tscount EQ 0) THEN data_bin = LONG((o3_data.ozone_8hr_total[io3]-o3_range[0])/do3) $
    		ELSE data_bin = [0]
    	IF (tscount GT 0) THEN ts_bin   = LONG((o3_data.ozone_8hr_total[io3]-o3_range[0])/do3) $
    		ELSE ts_bin = [0]
    	
    	hist_all = HISTOGRAM(data_bin, BINSIZE = 1, MIN = 0, MAX = (num_bins -1))								;Calculate density
    	nots_arr = [[nots_arr],[hist_all]] 
    
    	hist_ts = HISTOGRAM(ts_bin, BINSIZE = 1, MIN = 0, MAX = (num_bins -1))								;Calculate density
    	ts_arr = [[ts_arr],[hist_ts]] 

	    IF (STRMID(time_arr[i],6,2) EQ '01') THEN day1_o3 = [day1_o3 ,o3_data.ozone_8hr_total[io3]]
        IF (STRMID(time_arr[i],6,2) EQ '02') THEN day2_o3 = [day2_o3 ,o3_data.ozone_8hr_total[io3]]
        IF (STRMID(time_arr[i],6,2) EQ '03') THEN day3_o3 = [day3_o3 ,o3_data.ozone_8hr_total[io3]]
        IF (STRMID(time_arr[i],6,2) EQ '04') THEN day4_o3 = [day4_o3 ,o3_data.ozone_8hr_total[io3]]
        IF (STRMID(time_arr[i],6,2) EQ '05') THEN day5_o3 = [day5_o3 ,o3_data.ozone_8hr_total[io3]]
        IF (STRMID(time_arr[i],6,2) EQ '06') THEN day6_o3 = [day6_o3 ,o3_data.ozone_8hr_total[io3]]
        IF (STRMID(time_arr[i],6,2) EQ '07') THEN day7_o3 = [day7_o3 ,o3_data.ozone_8hr_total[io3]]
        IF (STRMID(time_arr[i],6,2) EQ '08') THEN day8_o3 = [day8_o3 ,o3_data.ozone_8hr_total[io3]]
        IF (STRMID(time_arr[i],6,2) EQ '09') THEN day9_o3 = [day9_o3 ,o3_data.ozone_8hr_total[io3]]
        IF (STRMID(time_arr[i],6,2) EQ '10') THEN day10_o3 = [ day10_o3,o3_data.ozone_8hr_total[io3]]
        IF (STRMID(time_arr[i],6,2) EQ '11') THEN day11_o3 = [ day11_o3,o3_data.ozone_8hr_total[io3]]
        IF (STRMID(time_arr[i],6,2) EQ '12') THEN day12_o3 = [ day12_o3,o3_data.ozone_8hr_total[io3]]
        IF (STRMID(time_arr[i],6,2) EQ '13') THEN day13_o3 = [ day13_o3,o3_data.ozone_8hr_total[io3]]
        IF (STRMID(time_arr[i],6,2) EQ '14') THEN day14_o3 = [ day14_o3,o3_data.ozone_8hr_total[io3]]
        IF (STRMID(time_arr[i],6,2) EQ '15') THEN day15_o3 = [ day15_o3,o3_data.ozone_8hr_total[io3]]
        IF (STRMID(time_arr[i],6,2) EQ '16') THEN day16_o3 = [ day16_o3,o3_data.ozone_8hr_total[io3]]
        IF (STRMID(time_arr[i],6,2) EQ '17') THEN day17_o3 = [ day17_o3,o3_data.ozone_8hr_total[io3]]
        IF (STRMID(time_arr[i],6,2) EQ '18') THEN day18_o3 = [ day18_o3,o3_data.ozone_8hr_total[io3]]
        IF (STRMID(time_arr[i],6,2) EQ '19') THEN day19_o3 = [ day19_o3,o3_data.ozone_8hr_total[io3]]
        IF (STRMID(time_arr[i],6,2) EQ '20') THEN day20_o3 = [ day20_o3,o3_data.ozone_8hr_total[io3]]
        IF (STRMID(time_arr[i],6,2) EQ '21') THEN day21_o3 = [ day21_o3,o3_data.ozone_8hr_total[io3]]
        IF (STRMID(time_arr[i],6,2) EQ '22') THEN day22_o3 = [ day22_o3,o3_data.ozone_8hr_total[io3]]
        IF (STRMID(time_arr[i],6,2) EQ '23') THEN day23_o3 = [ day23_o3,o3_data.ozone_8hr_total[io3]]
        IF (STRMID(time_arr[i],6,2) EQ '24') THEN day24_o3 = [ day24_o3,o3_data.ozone_8hr_total[io3]]
        IF (STRMID(time_arr[i],6,2) EQ '25') THEN day25_o3 = [ day25_o3,o3_data.ozone_8hr_total[io3]]
        IF (STRMID(time_arr[i],6,2) EQ '26') THEN day26_o3 = [ day26_o3,o3_data.ozone_8hr_total[io3]]
        IF (STRMID(time_arr[i],6,2) EQ '27') THEN day27_o3 = [ day27_o3,o3_data.ozone_8hr_total[io3]]
        IF (STRMID(time_arr[i],6,2) EQ '28') THEN day28_o3 = [ day28_o3,o3_data.ozone_8hr_total[io3]]
        IF (STRMID(time_arr[i],6,2) EQ '29') THEN day29_o3 = [ day29_o3,o3_data.ozone_8hr_total[io3]]
        IF (STRMID(time_arr[i],6,2) EQ '30') THEN day30_o3 = [ day30_o3,o3_data.ozone_8hr_total[io3]]
        IF (STRMID(time_arr[i],6,2) EQ '31') THEN day31_o3 = [ day31_o3,o3_data.ozone_8hr_total[io3]]

    ENDFOR  	
	nots_arr_tot1 = [[[nots_arr_tot1]],[[nots_arr]]]
	ts_arr_tot1   = [[[ts_arr_tot1  ]],[[ts_arr  ]]]
	

ENDFOREACH

nots_arr_tot = TOTAL(nots_arr_tot1,3)
ts_arr_tot   = TOTAL(ts_arr_tot1,3)

table_nots = GRAY_LOGSCALE_24(20, 0.50, 0.0, PS = ps)	
col_nots   = COLOR_LOOKUP_24(nots_arr_tot, table_nots, MIN_VALUE = 0.0, MAX_VALUE = 50.0, MISSING = table_nots[-1])
none      = WHERE((nots_arr_tot EQ 0), none_count)
IF (none_count GT 0) THEN col_nots[none] = COLOR_24('white')											;Set counts of zero to white

table_ts = GRAY_LOGSCALE_24(20, 0.50, 0.0, PS = ps)
col_ts   = COLOR_LOOKUP_24(ts_arr_tot, table_ts, MIN_VALUE = 0.0, MAX_VALUE = 50.0, MISSING = table_ts[-1])
none      = WHERE((ts_arr_tot EQ 0), none_count)
IF (none_count GT 0) THEN col_ts[none] = COLOR_24('white')											;Set counts of zero to white

!P.MULTI = [0, 1, 2]

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
	TITLE    = 'O3 Concentrations for Non-Storm Periods, Years: ' + STRING(yr_arr[0]) + $
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
	FOR i = 0, nt_yr -1 DO BEGIN
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

title = 'O3 Concentrations for Storm Periods, Years: ' + STRING(yr_arr[0]) + $
			' - ' + STRING(yr_arr[-1])
IF KEYWORD_SET(landfall) THEN $
	title = 'O3 Concentrations for Close and Landfalling TD Storm Periods, Years: ' + STRING(yr_arr[0]) + $
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
	FOR i = 0, nt_yr -1 DO BEGIN
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
	RANGE = [0.0, 50.0], $
	TITLE = 'Frequency (%)', $
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

days = [day1,day2,day3,day4,day5,day6,day7,day8,day9,day10,day11,day12,day13,day14,day15,$
	day16,day17,day18,day19,day20,day21,day22,day23,day24,day25,day26,day27,day28,day29,day30,day31]

days_o3= [MEAN(day1_o3),MEAN(day2_o3),MEAN(day3_o3),MEAN(day4_o3),MEAN(day5_o3),MEAN(day6_o3),MEAN(day7_o3),MEAN(day8_o3),MEAN(day9_o3),MEAN(day10_o3),MEAN(day11_o3),MEAN(day12_o3),MEAN(day13_o3),MEAN(day14_o3),MEAN(day15_o3),$
	MEAN(day16_o3),MEAN(day17_o3),MEAN(day18_o3),MEAN(day19_o3),MEAN(day20_o3),MEAN(day21_o3),MEAN(day22_o3),MEAN(day23_o3),MEAN(day24_o3),MEAN(day25_o3),MEAN(day26_o3),MEAN(day27_o3),MEAN(day28_o3),MEAN(day29_o3),MEAN(day30_o3)]


STOP
END
