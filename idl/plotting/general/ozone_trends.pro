PRO OZONE_TRENDS, $
	PNG		  = png, $
	EPS		  = eps, $
	CLOBBER   = clobber

;+
;NAME:
;     OZONE_TRENDS
;PURPOSE:
;     Plots monthly ozone trends (October 1980-2017)
;CATEGORY:
;     Data handling utility.
;CALLING SEQUENCE:
;     OZONE_TRENDS, date0, outfile
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
;		D. Phoenix:       2019-02-12.
;-

COMPILE_OPT IDL2																									;Set compile options

IF (N_ELEMENTS(date1) EQ 0) THEN date1 = MAKE_DATE(1980,01,01,00)
IF (N_ELEMENTS(date2) EQ 0) THEN date2 = MAKE_DATE(2018,01,01,00)
nt = DATE_DIFF(date2,date1) / 21600.

mth_arr = ['07','08','09','10','11']
day_arr = [31  ,  31,  30,  31,  30]
hr_arr  = ['00'];,'06','12','18'	   ]
yr_arr = ['1996','1997','1998','1999','2000','2001','2002','2003','2004','2005','2006',$
		'2007','2008','2009','2010','2011','2012','2013','2014','2015','2016']
;yr_arr=['2011','2012','2013']

m=0		
ny = N_ELEMENTS(yr_arr)
mt = N_ELEMENTS(mth_arr)

month_p   =FLTARR(mt)
month_gph =FLTARR(mt)
month_u   =FLTARR(mt)
month_v   =FLTARR(mt)
month_temp=FLTARR(mt)

ozone_month = []
FOREACH month, mth_arr DO BEGIN
	PRINT, month
	ozone_year = [ ]
	FOREACH year, yr_arr DO BEGIN
		data = OZONE_DALLAS_NAAQS(year)
		date1 = STRING(year) + STRING(month)
		o3_datestr =  STRMID(data.date,0,4)+STRMID(data.date,5,2)+STRMID(data.date,8,2) + $
				  STRMID(data.time,0,2)+STRMID(data.time,3,2) 
		io3 = WHERE(STRMID(o3_datestr,0,6) EQ (year + month))

        ozone_year = [ozone_year,data.ozone_8hr_total[io3]]     
	ENDFOREACH

	;create monthly mean
	IF (month EQ '07') THEN month1 = ozone_year
	IF (month EQ '08') THEN month2 = ozone_year
	IF (month EQ '09') THEN month3 = ozone_year
	IF (month EQ '10') THEN month4 = ozone_year
	IF (month EQ '11') THEN month5 = ozone_year
ENDFOREACH

STOP
	filename = MAKE_ISO_DATE_STRING(date1,/COMPACT,/UTC)
	outdir = '/data3/dphoenix/wrf/general/reanalysis_plots/o3_wind/'
	FILE_MKDIR, outdir
	pngfile = outdir + 'o3_wind_' + filename + '.png' 

    IF ~KEYWORD_SET(nowindow) THEN BEGIN
    	IF KEYWORD_SET(eps) THEN BEGIN
    		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
    			LOAD_BASIC_COLORS, /RESET															;Reset color table to linear ramp
    		PS_OFF																					;Turn PS off
    	ENDIF ELSE IF KEYWORD_SET(png) THEN $
    		WRITE_PNG, pngfile, TVRD(TRUE=1)														;Write PNG image
    ENDIF

	PRINT, date1
	date1 = TIME_INC(date1, 21600)

END
