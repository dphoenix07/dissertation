PRO JTIME__DEFINE

;+
;NAME:
;		JTIME__DEFINE
;PURPOSE:
;		This procedure defines the JTIME date structure.
;CATEGORY:
;		Date and time calculations.
;CALLING SEQUENCE:
;		Called automatically as time = {JTIME}
;INPUT:
;		None.
;OUTPUT:
;		None
;KEYWORDS:
;		None.
;MODIFICATION HISTORY:
;		Kenneth Bowman, 2001-07.
;-

COMPILE_OPT IDL2

date = {JTIME,	jday		: 0,	$
					seconds	: 0}

END
