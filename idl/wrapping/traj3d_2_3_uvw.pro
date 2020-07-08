PRO TRAJ3D_2_3_UVW, iid, dt0, t, vg0, vg1, vs0, vs1, vn0, vn1, DEBUG = debug

;+
;NAME:
;     TRAJ3D_2_3_UVW
;PURPOSE:
;     This reads winds from the input data file.
;CATEGORY:
;     3-D trajectory model.
;CALLING SEQUENCE:
;     TRAJ3D_2_3_UVW, iid, dt0, t, tt0, tt1, vg, vs, vn
;CALLED BY:
;     TRAJ3D_2_3
;INPUT:
;     iid : id of the NetCDF input file.
;     dt0 : time step size (positive or negative)
;     t   : time at which velocities are needed
;     tt0 : time at which velocities are currently known
;     tt1 : time at which velocities are currently known
;OUTPUT:
;     vg  : u-, v-, and optionally w-velocities at two time levels on global or regional grid.
;     vs  : u-, v-, and optionally w-velocities at two time levels on SH Cartesian grid.
;     vn  : u-, v-, and optionally w-velocities at two time levels on NH Cartesian grid.
;KEYWORDS:
;     DEBUG : If set, execute debugging code.
;PROCEDURE:
;     This following golbal attributes must be set in the input file :
;        data_domain      : GLOBAL or REGIONAL
;        data_source      : UKMO, NCEP, CCM3, or null
;     The procedure requires the following attributes be set in the input file
;     for each coordinate variable (x, y, z):
;        regular  : 0 if coordinate grid is irregularly spaced, 1 if regularly spaced
;        periodic : 0 if coordinate grid is not periodic, 1 if periodic
;        min      : minimum value of coordinate domain
;        max      : maximum value of coordinate domain
;     It also checks to see if the vertical velocity (w) is included in the file.
;     Data are read for the necessary timesteps and stored in the velocity arrays.
;MODIFICATION HISTORY:
;     KPB, 1995-06.
;     KPB, 1995-09.  Minor modifications for consistency with other procedures.
;     KPB, 1999-06.  Updated to use structures.
;     KPB, 2002-01.  Updated to optionally use regional grids of data.
;		KPB, 2005-12.  Modified to use pointers to velocity arrays for efficiency.
;
;     Copyright 2002.  Kenneth P. Bowman.  Please do not distribute without permission.
;     Prof. Kenneth P. Bowman
;     Department of Atmospheric Sciences
;     Texas A&M University
;     College Station, TX   77843-3150
;     979-862-4060, k-bowman@tamu.edu
;-

COMPILE_OPT IDL2																						;Set compile options.

TRAJ3D_2_3_TINDEX, iid, dt0, t, s0, s1, tt0, tt1, DEBUG = debug						;Indices of data timesteps

IF (N_ELEMENTS(vg0) EQ 0) THEN BEGIN															;Create velocity data structure

	IF KEYWORD_SET(debug) THEN PRINT, 'TRAJ3D_2_3_UVW :      Initial time step'

	NCDF_CONTROL, iid, /NOVERBOSE                                         			;Temporarily turn error messages off
	wid = NCDF_VARID(iid, 'w')																		;Check for existence of w-field
	NCDF_CONTROL, iid, /VERBOSE                                           			;Turn error messages back on
	IF(wid EQ -1) THEN nv = 2 ELSE nv = 3														;Find number of velocity components

	NCDF_ATTGET, iid, 'data_domain', data_domain, /GLOBAL									;Read the data domain attribute
	data_domain = STRUPCASE(STRING(data_domain))												;Convert to string

	NCDF_ATTGET, iid, 'data_source', data_source, /GLOBAL									;Read the data source attribute
	data_source = STRUPCASE(STRING(data_source))												;Convert to string
	
	u   = TRAJ3D_2_3_READ_VELOCITY(iid, 'u', s0, DEBUG = debug)							;Read u-component at s0
	v   = TRAJ3D_2_3_READ_VELOCITY(iid, 'v', s0, DEBUG = debug)							;Read v-component at s0
	w   = TRAJ3D_2_3_READ_VELOCITY(iid, 'w', s0, DEBUG = debug)							;Read w-component at s0
	vg0 = {s           : s0,          $															;Timestep index
			 time        : tt0,         $															;Time
			 nv          : nv,          $															;Number of velocity components
			 u           : u,           $															;u-velocity at s0
			 v           : v,           $															;v-velocity at s0
			 w           : w,           $															;w-velocity at s0
	       data_domain : data_domain, $															;Domain descriptor
	       data_source : data_source  }															;Source descriptor

	u   = TRAJ3D_2_3_READ_VELOCITY(iid, 'u', s1, DEBUG = debug)							;Read u-component at s1
	v   = TRAJ3D_2_3_READ_VELOCITY(iid, 'v', s1, DEBUG = debug)							;Read v-component at s1
	w   = TRAJ3D_2_3_READ_VELOCITY(iid, 'w', s1, DEBUG = debug)							;Read w-component at s1
	vg1 = {s           : s1,          $															;Timestep index
			 time        : tt1,         $															;Time
			 nv          : nv,          $															;Number of velocity components
			 u           : u,           $															;u-velocity at s1
			 v           : v,           $															;v-velocity at s1
			 w           : w,           $															;w-velocity at s1
	       data_domain : data_domain, $															;Domain descriptor
	       data_source : data_source  }															;Source descriptor

	IF (STRUPCASE(vg0.data_domain) EQ 'GLOBAL') THEN $
		TRAJ3D_2_3_UVW_TO_CART, vg0, vs0, vn0, DEBUG = debug								;Transform to Cartesian coordinates for polar regions
	IF (STRUPCASE(vg1.data_domain) EQ 'GLOBAL') THEN $
		TRAJ3D_2_3_UVW_TO_CART, vg1, vs1, vn1, DEBUG = debug								;Transform to Cartesian coordinates for polar regions

ENDIF ELSE IF(TIME_DIFF(t,   tt0) EQ 0) THEN BEGIN											;Update velocities for forward timestep

	IF KEYWORD_SET(debug) THEN PRINT, 'TRAJ3D_2_3_UVW :      Forward time step'

	TRAJ3D_2_3_UVW_FREE, vg0																		;Free timestep 0 velocity arrays
	vg0 = vg1																							;Move velocities from timestep 1 to 0

	IF (STRUPCASE(vg0.data_domain) EQ 'GLOBAL') THEN BEGIN
		us_tmp = vs0.u.values																		;Save u0 pointer
		vs_tmp = vs0.v.values																		;Save v0 pointer
		un_tmp = vn0.u.values																		;Save u0 pointer
		vn_tmp = vn0.v.values																		;Save v0 pointer
		IF (vg0.nv EQ 3) THEN BEGIN
			ws_tmp = vs0.w.values																	;Save w0 pointer
			wn_tmp = vn0.w.values																	;Save w0 pointer
		ENDIF
		
		vs0 = vs1																						;Move velocities from 1 to 0
		vn0 = vn1																						;Move velocities from 1 to 0
		
		vs1.u.values = us_tmp																		;Swap u pointer
		vs1.v.values = vs_tmp																		;Swap v pointer
		vn1.u.values = un_tmp																		;Swap u pointer
		vn1.v.values = vn_tmp																		;Swap v pointer
		IF (vg0.nv EQ 3) THEN BEGIN
			vs1.w.values = ws_tmp																	;Swap w pointer
			vn1.w.values = wn_tmp																	;Swap w pointer
		ENDIF
	ENDIF
	
	vg1.s    = s1																						;Set timestep index
	vg1.time = tt1																						;Set time

	vg1.u = TRAJ3D_2_3_READ_VELOCITY(iid, 'u', s1, DEBUG = debug)						;Read u-component at s1
	vg1.v = TRAJ3D_2_3_READ_VELOCITY(iid, 'v', s1, DEBUG = debug)						;Read v-component at s1
	vg1.w = TRAJ3D_2_3_READ_VELOCITY(iid, 'w', s1, DEBUG = debug)						;Read w-component at s1

	IF (STRUPCASE(vg1.data_domain) EQ 'GLOBAL') THEN $
		TRAJ3D_2_3_UVW_TO_CART, vg1, vs1, vn1, DEBUG = debug								;Transform to Cartesian coordinates for polar regio

	IF KEYWORD_SET(debug) THEN BEGIN
  		PRINT, 'TRAJ3D_2_3_UVW :     Data source     : ', vg1.data_source
  		PRINT, 'TRAJ3D_2_3_UVW :     Data domain     : ', vg1.data_domain
	END
	
ENDIF ELSE IF(TIME_DIFF(t,   tt1) EQ 0) THEN BEGIN											;Update velocities for backward timestep

	IF KEYWORD_SET(debug) THEN PRINT, 'TRAJ3D_2_3_UVW :      Backward time step'

	TRAJ3D_2_3_UVW_FREE, vg1																		;Free timestep 1 velocity arrays
	vg1 = vg0																							;Move velocities from timestep 0 to 1

	IF (STRUPCASE(vg1.data_domain) EQ 'GLOBAL') THEN BEGIN
		us_tmp = vs1.u.values																		;Save u1 pointer
		vs_tmp = vs1.v.values																		;Save v1 pointer
		un_tmp = vn1.u.values																		;Save u1 pointer
		vn_tmp = vn1.v.values																		;Save v1 pointer
		IF (vg0.nv EQ 3) THEN BEGIN
			ws_tmp = vs1.w.values																	;Save w1 pointer
			wn_tmp = vn1.w.values																	;Save w1 pointer
		ENDIF
		
		vs1 = vs0																						;Move velocities from 0 to 1
		vn1 = vn0																						;Move velocities from 0 to 1

		vs0.u.values = us_tmp																		;Swap u pointer
		vs0.v.values = vs_tmp																		;Swap v pointer
		vn0.u.values = un_tmp																		;Swap u pointer
		vn0.v.values = vn_tmp																		;Swap v pointer
		IF (vg1.nv EQ 3) THEN BEGIN
			vs0.w.values = ws_tmp																	;Swap w pointer
			vn0.w.values = wn_tmp																	;Swap w pointer
		ENDIF
	ENDIF
	
	vg0.s    = s0																						;Set timestep index
	vg0.time = tt0																						;Set time

	vg0.u = TRAJ3D_2_3_READ_VELOCITY(iid, 'u', s0, DEBUG = debug)						;Read u-component at s0
	vg0.v = TRAJ3D_2_3_READ_VELOCITY(iid, 'v', s0, DEBUG = debug)						;Read v-component at s0
	vg0.w = TRAJ3D_2_3_READ_VELOCITY(iid, 'w', s0, DEBUG = debug)						;Read w-component at s0

	IF (STRUPCASE(vg0.data_domain) EQ 'GLOBAL') THEN $
		TRAJ3D_2_3_UVW_TO_CART, vg0, vs0, vn0, DEBUG = debug								;Transform to Cartesian coordinates for polar regions

	IF KEYWORD_SET(debug) THEN BEGIN
  		PRINT, 'TRAJ3D_2_3_UVW :     Data source     : ', vg1.data_source
  		PRINT, 'TRAJ3D_2_3_UVW :     Data domain     : ', vg1.data_domain
	END

ENDIF

IF KEYWORD_SET(debug) THEN BEGIN
  	PRINT, 'TRAJ3D_2_3_UVW :      tt0 = ', tt0, '     s0 = ', s0
  	PRINT, 'TRAJ3D_2_3_UVW :      t   = ', t
  	PRINT, 'TRAJ3D_2_3_UVW :      tt1 = ', tt1, '     s1 = ', s1
END

END
