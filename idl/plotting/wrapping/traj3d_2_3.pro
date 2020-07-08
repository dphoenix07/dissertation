PRO TRAJ3D_2_3, infile, datei, datef, ntdays, ntsave, ntplot, r0, $
	PLOT          = plot, $
   RESTART       = restart, $
   OUTFILE       = outfile, $
   MULTIFILE     = multifile, $
   CLOBBER       = clobber, $
   Z_UNSTICK     = z_unstick, $
   LAST_OUTPUT   = last_output, $
   LAST_POSITION = last_position, $
   TRELATIVE     = trelative, $
   VERBOSE       = verbose, $
   QUIET         = quiet, $
   DEBUG         = debug

;+
;NAME:
;     TRAJ3D_2_3
;VERSION NUMBER:
;     2.3.  This version is identical to version 2.3, except that all procedure and function names have
;				been changed to TRAJ3D_2_3_...
;PURPOSE:
;     This computes Lagrangian 3-dimensional trajectories using global gridded
;     winds.
;CATEGORY:
;     Trajectory model.
;CALLING SEQUENCE:
;     TRAJ3D_2_3, infile, datei, datef, ntdays, ntsave, ntplot, r0
;INPUT:
;     infile      : name of the netCDF input file containing wind data.
;     datei       : Initial date and time
;     datef       : Final date and time
;     ntdays      : Number of timesteps per day
;     ntsave      : Number of saves per day
;     ntplot      : Number of plots per day
;     r0          : Initial particle positions
;KEYWORDS:
;     PLOT          : Set to 'sh', 'nh', 'global', or '3d' for different views
;     RESTART       : String variable.  If present, the initial particle positions are read from the file restart.
;     OUTFILE       : String variable.  If present, write the results to file specified by outfile.
;     MULTIFILE     : String variable.  If present, write the results to multiple output files using 
;                     multifile as the base for constructing the filename.  If OUTFILE is set, this 
;                     keyword is ignored.
;     CLOBBER       : if set, overwrite existing output files.
;     LAST_OUTPUT   : Returns the name of the last output file to the calling program
;     LAST_POSITION : Returns the positions of the particles at the end of the run.
;     VERBOSE       : if set, send verbose informational output to the terminal.
;     QUIET         : if set, do not send default output to the terminal.  This will turn
;							 off all output from the program unless VERBOSE is set.
;     DEBUG         : if set, execute debugging sections.
;OUTPUT:
;     If OUTFILE or MULTIFILE are set, particle positions are written to the
;     specified files.
;SYSTEM VARIABLES USED:
;     !a0     : radius of the Earth (m)
;     !xymax  : size of polar Cartesian grids (degrees latitude)
;     !nlimit : region to use north polar Cartesian grid (degrees latitude)
;     !slimit : region to use south polar Cartesian grid (degrees latitude)
;MODIFICATION HISTORY:
;     KPB, August, 1993.
;     KPB, June, 1999.  Updated to use structures.
;     KPB&TLE, November, 2001. Updated to write results to different outfiles.
;     
;     Copyright 2001.  Kenneth P. Bowman.  Please do not distribute without permission.
;     Prof. Kenneth P. Bowman
;     Department of Atmospheric Sciences
;     Texas A&M University
;     College Station, TX   77843-3150
;     409-862-4060, k-bowman@tamu.edu
;-

COMPILE_OPT IDL2

traj3d_version = '2.3'																										;Version information (string)
systime0       = SYSTIME(/SECONDS)																						;Get current time

IF (N_ELEMENTS(plot) GT 0) THEN BEGIN
	SET_PLOT, 'X'
	WINDOW, 0, XSIZE= 400, YSIZE = 400
	!P.MULTI = 0
ENDIF

IF (N_ELEMENTS(restart) GT 0) THEN BEGIN
	TRAJ3D_2_3_INITIAL_RESTART, restart, ntdays, ntsave, ntplot, np, dateir, r0, DEBUG = debug		;Read initial particle positions
	IF ((TIME_DIFF(dateir, datei) NE 0) AND ~KEYWORD_SET(quiet)) THEN BEGIN									;Check start time
		PRINT, 'dateir : ', MAKE_ISO_DATE_STRING(dateir)
		PRINT, 'datei  : ', MAKE_ISO_DATE_STRING(datei)
		MESSAGE, 'dateir is not equal to datei.'
	ENDIF
ENDIF

iid = NCDF_OPEN(infile, /NOWRITE)																						;Open the netCDF input file
NCDF_CONTROL, iid, /NOVERBOSE																								;Temporarily turn error messages off
w_exists = NCDF_VARID(iid, 'w')																							;Check for existence of w-field
NCDF_CONTROL, iid, /VERBOSE																								;Turn error messages back on
IF(w_exists EQ -1) THEN no_w = 1 ELSE no_w = 0																		;Set number of velocity components

ti      = DATE_TO_TIME(datei)																								;Initial time
tf      = DATE_TO_TIME(datef)																								;Final time
ntdays  = LONG(ntdays)																										;Number of timesteps per day
ntsave  = LONG(ntsave)																										;Number of saves per day
ntplot  = LONG(ntplot)																										;Number of plots per day
dt0     = LONG((86400/ntdays)*(TIME_DIFF(tf, ti)/ABS(TIME_DIFF(tf, ti))))									;Nominal timestep size
IF((86400 MOD dt0) NE 0) THEN MESSAGE, 'Timestep must be an integral divisor of 86,400.'				;Timestep not integral divisor of one day
IF((ntsave LT 1) OR (ntsave GT ntdays)) THEN MESSAGE, 'Incorrect number of saves per day'				;Incorrect number of saves per day
IF((ntplot LT 1) OR (ntplot GT ntdays)) THEN MESSAGE, 'Incorrect number of plots per day'				;Incorrect number of plots per day

t0      = ti																													;Time at beginning of current timestep
t1      = TIME_INC(t0, dt0, /CHECK)																						;Increment time by dt0
IF(dt0 GT 0) THEN BEGIN
	t1.seconds = (t1.seconds/dt0)*dt0																					;Round down to integral timestep
ENDIF ELSE BEGIN
	t1.seconds = ((t1.seconds-dt0-1)/dt0)*dt0																			;Round up to integral timestep
	t1 = TIME_INC(t1, 0)
ENDELSE
dt      = TIME_DIFF(t1, t0)																								;Actual next timestep size
s       = 0																														;Advection time-step counter
ss      = 0																														;Save-step counter
tt0     = t0																													;Time of previous data slice
tt1     = t0																													;Time of subsequent data slice

np = N_ELEMENTS(r0.x.values)																								;Number of parcels
r  = r0																															;Set initial particle locations

date0 = TIME_TO_DATE(t0)
date1 = TIME_TO_DATE(t1)

IF ~KEYWORD_SET(quiet) THEN BEGIN
	PRINT
	PRINT, '********************************************************************************'
	PRINT, 'Executing TRAJ3D Version ', traj3d_version
	PRINT, 'Initial date     : ', MAKE_ISO_DATE_STRING(datei)
	PRINT, 'Final date       : ', MAKE_ISO_DATE_STRING(datef)
	PRINT, 'Input file       : ', infile
	IF (N_ELEMENTS(outfile)   GT 0) THEN PRINT, 'Output file      : ', outfile
	IF (N_ELEMENTS(multifile) GT 0) THEN PRINT, 'Output files     : ', multifile
	PRINT, 'ntdays           : ', ntdays, ' timesteps per day'
	PRINT, 'ntsave           : ', ntsave, ' saves per day'
	PRINT, 'ntplot           : ', ntplot, ' plots per day'
	PRINT, 'dt0              : ', dt0,    ' seconds'
	PRINT, 'np at start      : ', np,     ' particles'
ENDIF
IF KEYWORD_SET(debug) THEN PRINT, s, r.x.values[0], r.y.values[0], r.z.values[0]
IF KEYWORD_SET(verbose) THEN BEGIN
	PRINT
	PRINT, '               INDEX                         TIME       '
	PRINT, '     Time       Save     Point                          '
	PRINT, '         s        ss        np           Date     Time  '
	PRINT, s, ss, np, MAKE_ISO_DATE_STRING(date0), FORMAT = "(3I10,'     ',A)"
ENDIF

IF (N_ELEMENTS(outfile) GT 0) THEN BEGIN																				;Create the output file
	IF KEYWORD_SET(debug) THEN PRINT, 'Creating single output file : ', outfile
	last_output = outfile
	TRAJ3D_2_3_WRITE_XYZ, oid, ss, t0, r, ntdays, ntsave, ntplot, $											;Write to the output file
		VERSION = traj3d_version, $
		CREATE  = outfile, $
		NO_W    = no_w, $
		CLOBBER = clobber, $
		DEBUG   = debug
ENDIF ELSE IF (N_ELEMENTS(multifile) GT 0) THEN BEGIN																;Create first of multiple files
	IF (N_ELEMENTS(restart) EQ 0) THEN BEGIN																			;Don't overwrite restart file
		last_output = multifile + MAKE_ISO_DATE_STRING(datei, /COMPACT, PRECISION = 'hour') + '.ncd'	;Output file name
		IF KEYWORD_SET(debug) THEN PRINT, 'Creating first output file  : ', last_output
		TRAJ3D_2_3_WRITE_XYZ, oid, ss, t0, r, ntdays, ntsave, ntplot, $										;Write output file
			VERSION = traj3d_version, $
			CREATE  = last_output, $
			NO_W    = no_w, $
			CLOBBER = clobber, $
			DEBUG   = debug
		NCDF_CLOSE, oid																										;Close output file
	ENDIF
ENDIF

IF (N_ELEMENTS(plot) GT 0) THEN $
	TRAJ3D_PLOT_MAPS_2, r.x.values, r.y.values, r.z.values, REGION = plot									;Plot initial parcel locations

WHILE(dt NE 0) DO BEGIN																										;Compute new parcel locations

	TRAJ3D_2_3_UVW, iid, dt0, t0, vg0, vg1, vs0, vs1, vn0, vn1, DEBUG = debug								;Get winds if necessary
	TRAJ3D_2_3_ADVECT, dt, r, t0, vg0, vg1, vs0, vs1, vn0, vn1, Z_UNSTICK = z_unstick, DEBUG = debug;Advect the particles
	s = s + 1																													;Increment time step counter
   
	IF KEYWORD_SET(trelative) THEN BEGIN
;		PRINT, TIME_DIFF(t1, ti, /CHECK), (!tauDSolar/ntsave), TIME_DIFF(t1, tf, /CHECK)
		IF (((TIME_DIFF(t1, ti, /CHECK) MOD (!tauDSolar/ntsave)) EQ 0) OR $									;Is this a save step?
			(TIME_DIFF(t1, tf, /CHECK) EQ 0)) THEN save_step = 1 $												;Or the last time step?
		ELSE save_step = 0
	ENDIF ELSE BEGIN
		IF (((t1.seconds MOD (!tauDSolar/ntsave))  EQ 0) OR $														;Is this a save step?
			(TIME_DIFF(t1, tf, /CHECK) EQ 0)) THEN save_step = 1 $												;Or the last time step?
		ELSE save_step = 0
	ENDELSE

	IF save_step THEN BEGIN
		IF KEYWORD_SET(verbose) THEN PRINT, s, ss, np, MAKE_ISO_DATE_STRING(date1), FORMAT = "(3I10,'     ',A)"

		IF (N_ELEMENTS(outfile) GT 0) THEN BEGIN																		;Use existing output file
			TRAJ3D_2_3_WRITE_XYZ, oid, ss, t1, r, ntdays, ntsave, ntplot, DEBUG = debug					;Write to output file
		ENDIF ELSE IF (N_ELEMENTS(multifile) GT 0) THEN BEGIN														;Create next output file
			last_output = multifile + MAKE_ISO_DATE_STRING(date1, /COMPACT, PRECISION = 'hour') + '.ncd'
			TRAJ3D_2_3_WRITE_XYZ, oid, ss, t1, r, ntdays, ntsave, ntplot, $									;Write to output file
				VERSION = traj3d_version, $
				CREATE  = last_output, $
				NO_W    = no_w, $
				CLOBBER = clobber, $
				DEBUG   = debug
			NCDF_CLOSE, oid																									;Close output file
		ENDIF

      IF (N_ELEMENTS(plot) GT 0) THEN BEGIN
         ERASE
         TRAJ3D_PLOT_MAPS_2, r.x.values, r.y.values, r.z.values, REGION = plot
		ENDIF
   ENDIF
 
	t0    = t1																													;Increment timestep start time
	t1    = TIME_INC(t0, dt0, /CHECK)																					;Increment timestep end time
	IF (TIME_DIFF(t1, tf, /CHECK)*(dt0/ABS(dt0)) GT 0) THEN t1 = tf											;Stepped past tf
	dt    = TIME_DIFF(t1, t0, /CHECK)																					;Actual next time step size
	date0 = TIME_TO_DATE(t0)
	date1 = TIME_TO_DATE(t1)

ENDWHILE

IF (vg0.data_domain EQ 'GLOBAL') THEN BEGIN
	TRAJ3D_2_3_UVW_FREE, vs0
	TRAJ3D_2_3_UVW_FREE, vs1
	TRAJ3D_2_3_UVW_FREE, vn0
	TRAJ3D_2_3_UVW_FREE, vn1
ENDIF
TRAJ3D_2_3_UVW_FREE, vg0
TRAJ3D_2_3_UVW_FREE, vg1

NCDF_CLOSE, iid																												;Close input file
IF KEYWORD_SET(outfile) THEN NCDF_CLOSE, oid																			;Close output file

last_position = r
IF ~KEYWORD_SET(quiet) THEN BEGIN
	PRINT, 'np at end        : ', N_ELEMENTS(r.x.values),     ' particles'
	PRINT, 'Last output file : ', last_output                        
	PRINT, 'Elapsed time     : ', SYSTIME(/SECONDS) - systime0, ' seconds.'
	PRINT, 'TRAJ3D complete.'
	PRINT
ENDIF

END
