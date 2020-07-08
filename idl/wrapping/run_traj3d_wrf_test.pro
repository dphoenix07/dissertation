;*******************************************************************************
PRO RUN_TRAJ3D_WRF_TEST, run, experiment, $
		ISENTROPIC = isentropic, $
		CLOBBER    = clobber, $
		PLOT       = plot,    $
		VERBOSE    = verbose

;+
;NAME:
;     RUN_TRAJ3D_WRF_TEST
;PURPOSE:
;		This program computes back trajectories for GFS analysis domains
;		to look at reverse domain filling.
;CATEGORY:
;     Trajectory model.
;CALLING SEQUENCE:
;     RUN_TRAJ3D_WRF_TEST, run, date1, date2
;INPUT:
;		date      : GFS Analysis date (CDATE structure).
;		direction : 'forward' or 'backward'
;		ndays     : Length of trajectory run in days.  Default is 10.
;KEYWORDS:
;     None.
;OUTPUT:
;     File containing particle trajectories.
;MODIFICATION HISTORY:
;		Kenneth P. Bowman.  2005-12-09
;		Cameron R. Homeyer. 2010-04-26 Converted from START08 file
;-

COMPILE_OPT IDL2																											;Set compile options

SWITCH N_PARAMS() OF
	0 : date      = MAKE_DATE(2012, 5, 30, 20)																			;Default flight name
	1 : direction = 'backward'																							;Default direction
	2 : ndays     = 1																									;Default run length (days)
ENDSWITCH

PRINT, 'Getting run information and initial conditions'

params       = TRAJ3D_WRF_TEST_PARAMS(run,experiment)																		;Get input parameters
date_string1 = MAKE_ISO_DATE_STRING(params.date1, PREC='HOUR', /COMPACT, /UTC)
date_string2 = MAKE_ISO_DATE_STRING(params.date2, PREC='HOUR', /COMPACT, /UTC)

;infile is the file created by traj3d_wrf_p.pro	
infile = '/data3/dphoenix/wrf/20120530_ncar/' + experiment + '/winds/wrf_winds.nc'										;Wind file
outdir = params.outdir																									;Trajectory output directory
ni     = params.np																										;Number of initial conditions

ntdays = 1440																												;Number of timesteps per day
ntsave = 288																												;Number of saves per day
ntplot = 4																													;Number of plots per day

FILE_MKDIR, outdir																										;Make output directory, if needed

PRINT
PRINT, 'Executing RUN_TRAJ3D_WRF_TEST for multiple initial conditions.'
PRINT, '   Input file        : ', infile
PRINT, '   Output directory  : ', outdir
PRINT, '   Number of runs    : ', ni
PRINT, '   ndays             : ', ndays
PRINT, '   ntdays            : ', ntdays
PRINT, '   ntsave            : ', ntsave
PRINT, '   ntplot            : ', ntplot
PRINT

PRINT, ni, ' initial conditions.'

outfile = outdir + run + '.ncd'																						;Output file

TRAJ3D_2_3, infile, params.date2, params.date1, ntdays, ntsave, ntplot, params, /VERBOSE, $		;Run trajectories
	TRELATIVE = 1, $
	CLOBBER   = clobber, $
	OUTFILE   = outfile, $
	PLOT      = plot

PRINT
PRINT, 'RUN_TRAJ3D_WRF_TEST complete.'

END
