PRO TRAJ3D_2_3_TINDEX, iid, dt0, t, s0, s1, tt0, tt1, DEBUG = debug

;+
;NAME:
;     TRAJ3D_2_3_TINDEX
;PURPOSE:
;     This computes the index of the previous and subsequent input data slices given the 
;     current time.
;CATEGORY:
;     3-D trajectory model.
;CALLING SEQUENCE:
;     TRAJ3D_2_3_TINDEX, iid, dt0, t, s0, s1, tt0, tt1
;INPUT:
;     iid : netCDF file id
;     dt0 : time step size
;     t   : time at which velocities are needed
;OUTPUT:
;     s0  : data index for preceding slice
;     s1  : data index for subsequent slice
;     tt0 : time of preceding slice
;     tt1 : time of subsequent slice
;KEYWORDS:
;     DEBUG   : if set, execute debugging sections.
;MODIFICATION HISTORY:
;     KPB, June, 1995.
;
;     Copyright 2001.  Kenneth P. Bowman.  Please do not distribute without permission.
;     Prof. Kenneth P. Bowman
;     Department of Atmospheric Sciences
;     Texas A&M University
;     College Station, TX   77843-3150
;     409-862-4060, k-bowman@tamu.edu
;-

COMPILE_OPT IDL2

NCDF_VARGET, iid, 'Time_interval',   dtt														;Read interval between data slices
IF ((dtt MOD dt0) NE 0) THEN MESSAGE, 'dt must be an integral divisor of dtt.'

NCDF_VARGET1, iid, 'Julian_day', jday,    OFFSET = 0										;Julian day of initial time slice
NCDF_VARGET1, iid, 'Seconds',    seconds, OFFSET = 0										;Time in seconds of initial time slice
ti = MAKE_TIME(JULDAY = jday, SECONDS = seconds)											;Create time structure

s0 = TIME_DIFF(t, ti)/dtt																			;Index of preceding time slice
s1 = s0 + 1																								;Index of subsequent time slice

IF((dt0 LT 0L) AND ((TIME_DIFF(t, ti) MOD dtt) EQ 0)) THEN BEGIN						;Adjust for backward time steps
	s0 = s0 - 1
	s1 = s1 - 1
ENDIF

NCDF_DIMINQ, iid, 'Time', name, ntt
IF((s0 LT 0) OR (s1 GT ntt-1)) THEN MESSAGE, 'Requested time out of range in TRAJ3D_2_3_TINDEX.'

NCDF_VARGET1, iid, 'Julian_day', jday,    OFFSET = s0										;Julian day for time slice s0
NCDF_VARGET1, iid, 'Seconds',    seconds, OFFSET = s0										;Time in seconds for time slice s0
tt0 = MAKE_TIME(JULDAY = jday, SECONDS = seconds)											;Create time structure

NCDF_VARGET1, iid, 'Julian_day', jday,    OFFSET = s1										;Julian day for time slice s1
NCDF_VARGET1, iid, 'Seconds',    seconds, OFFSET = s1										;Time in seconds for time slice s1
tt1 = MAKE_TIME(JULDAY = jday, SECONDS = seconds)											;Create time structure

IF KEYWORD_SET(debug) THEN BEGIN
   PRINT, 'TRAJ3D_2_3_TINDEX :      dt0 = ', dt0
   PRINT, 'TRAJ3D_2_3_TINDEX :      tt0 = ', tt0, '     s0 = ', s0
   PRINT, 'TRAJ3D_2_3_TINDEX :      t   = ', t
   PRINT, 'TRAJ3D_2_3_TINDEX :      tt1 = ', tt1, '     s1 = ', s1
ENDIF

END
