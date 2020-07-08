FUNCTION ERA_INTERIM_READ_COORD, name, infile

;+
; Name:
;       ERA_INTERIM_READ_COORD
; Purpose:
;       This program reads one spatial coordinate variable from a ERA-Interim netCDF file.
; Category:
;       ERA-Interim data utility.
; Calling sequence:
;       coord = ERA_INTERIM_READ_COORD(name, infile)
; Inputs:
;       name   : Name of coordinate to read.  This must be 'Longitude', 'Latitude', 
;					  or 'Altitude'.
;       infile : Input file name.
; Keywords:
;	     None.
; Output:
;       Data structure containing the requested coordinate and auxiliary information.
; Procedure:
;       This programs opens one ERA-Interim data file and returns a structure containing the values 
;       of the requested coordinate.  The file is closed before exiting.
; Author:
;       Cameron R. Homeyer 2012-03-13.
;-

COMPILE_OPT IDL2																;Set compile options

IF (N_PARAMS() LT 2) THEN MESSAGE, $
	'You must specify a coordinate name and an input file name.'

IF (name EQ 'Longitude') THEN periodic = 1 ELSE periodic = 0	;Set periodic flag

id = NCDF_OPEN(infile)														;Open input file
NCDF_VARGET, id, name, values												;Get coordinate values
NCDF_ATTGET, id, name, 'long_name', long_name						;Get long name attribute
NCDF_ATTGET, id, name, 'units',     units								;Get units attribute
IF (name NE 'Altitude') THEN $
	NCDF_ATTGET, id, name, 'delta',  delta $							;Get units attribute
	ELSE delta = ''
NCDF_CLOSE,  id																;Close input file

RETURN, {name      : name,               $							;Return data structure
			values    : values,             $
			n         : N_ELEMENTS(values), $
			delta     : delta,              $
			long_name : STRING(long_name),  $
			units     : STRING(units),      $
			periodic  : periodic            }

END
