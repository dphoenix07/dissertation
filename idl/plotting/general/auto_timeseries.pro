PRO AUTO_TIMESERIES, $
	PNG           = png, $
	ANIMATE       = animate


;+
; Name:
;               AUTO_WRF_PLOT
; Purpose:
;               This is a procedure to produce a desired number of WRF plots. Best
;				used for creating plots to combined in a GIF loop.
; Calling sequence:
;               AUTO_WRF_PLOT, event, scheme, variable, date, end_hour
; Inputs:
;               event      : String variable of run name. (e.g., '20120519')
;				scheme     : String variable of microphysics scheme (e.g., 'morrison')
;				start_date : String variable of the start date requested (e.g., '20120519T2300Z')
;				end_date   : String variable of the end date requested (e.g., '20120520T0300Z')
; Output:
;               A set of plots and an animated GIF of those plots.
; Keywords:
;				REFL		: If set, plots reflectivity.
;				TROPO3		: If set, plots ozone concentrations at tropopause.
;				ANIMATE	    : If set, calls WRITE_ANIMATED_GIF to create animation.
;				PNG			: Set if output to PNG desired.
; Author and history:
;               Daniel B. Phoenix	2016-01-21.
;-

COMPILE_OPT IDL2																				;Set compile options


plot_ozone_ts_timeseries,'1980',/png
plot_ozone_ts_timeseries,'1981',/png
plot_ozone_ts_timeseries,'1982',/png
plot_ozone_ts_timeseries,'1983',/png
plot_ozone_ts_timeseries,'1984',/png
plot_ozone_ts_timeseries,'1985',/png
plot_ozone_ts_timeseries,'1986',/png
plot_ozone_ts_timeseries,'1987',/png
plot_ozone_ts_timeseries,'1988',/png
plot_ozone_ts_timeseries,'1989',/png
plot_ozone_ts_timeseries,'1990',/png
plot_ozone_ts_timeseries,'1991',/png
plot_ozone_ts_timeseries,'1992',/png
plot_ozone_ts_timeseries,'1993',/png
plot_ozone_ts_timeseries,'1994',/png
plot_ozone_ts_timeseries,'1995',/png
plot_ozone_ts_timeseries,'1996',/png
plot_ozone_ts_timeseries,'1997',/png
plot_ozone_ts_timeseries,'1998',/png
plot_ozone_ts_timeseries,'1999',/png
plot_ozone_ts_timeseries,'2000',/png
plot_ozone_ts_timeseries,'2001',/png
plot_ozone_ts_timeseries,'2002',/png
plot_ozone_ts_timeseries,'2003',/png
plot_ozone_ts_timeseries,'2004',/png
plot_ozone_ts_timeseries,'2005',/png
plot_ozone_ts_timeseries,'2006',/png
plot_ozone_ts_timeseries,'2007',/png
plot_ozone_ts_timeseries,'2008',/png
plot_ozone_ts_timeseries,'2009',/png
plot_ozone_ts_timeseries,'2010',/png
plot_ozone_ts_timeseries,'2011',/png
plot_ozone_ts_timeseries,'2012',/png
plot_ozone_ts_timeseries,'2013',/png
plot_ozone_ts_timeseries,'2014',/png
plot_ozone_ts_timeseries,'2015',/png
plot_ozone_ts_timeseries,'2016',/png
plot_ozone_ts_timeseries,'2017',/png

END