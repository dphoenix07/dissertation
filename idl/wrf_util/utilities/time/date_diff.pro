FUNCTION DATE_DIFF, date1, date0

;+
;NAME:
;		DATE_DIFF
;PURPOSE:
;		This computes the difference in seconds between date0 and date1.
;CATEGORY:
;		Date and time calculations.
;CALLING SEQUENCE:
;		date = DATE_DIFF(date0, date1)
;INPUT:
;		date0 : structure containing date0
;		date1 : structure containing date1
;OUTPUT:
;		long integer
;KEYWORDS:
;		None.
;PROCEDURE:
;		This function calls the TIME_DIFF function.
;MODIFICATION HISTORY:
;     Kenneth Bowman, 2001-07.
;-

RETURN, TIME_DIFF(date1, date0)

END
