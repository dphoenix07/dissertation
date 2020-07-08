FUNCTION DATE_INC, date0, dt, CHECK = check

;+
;NAME:
;		DATE_INC
;PURPOSE:
;		This computes a new date given a start date and a time increment
;     in seconds.
;CATEGORY:
;		Date and time calculations.
;CALLING SEQUENCE:
;		date = DATE_INC(date0, dt)
;INPUT:
;		date0  : structure containing the start date
;		dt     : time increment in seconds (positive or negative)
;OUTPUT:
;		structure containing the new date
;KEYWORDS:
;		check  : If set, input parameter range checking is turned on.
;					Check can also be set to a two-element array containing
;					the lower and upper limits on the Julian day.
;PROCEDURE:
;		This function calls the TIME_TO_DATE and TIME_INC functions.
;MODIFICATION HISTORY:
;     Kenneth Bowman, 1999-10.
;-

COMPILE_OPT IDL2

RETURN, TIME_INC(date0, dt, CHECK = check)

END
