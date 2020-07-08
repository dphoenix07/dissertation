PRO JTIME_NO_LEAP__DEFINE

;+
;NAME:
;		JTIME_NO_LEAP__DEFINE
;PURPOSE:
;		This procedure defines the JTIME_NO_LEAP date structure.
;CATEGORY:
;		Date and time calculations.
;CALLING SEQUENCE:
;		Called automatically as time = {JTIME_NO_LEAP}
;INPUT:
;		None.
;OUTPUT:
;		None
;KEYWORDS:
;		None.
;MODIFICATION HISTORY:
;     Kenneth Bowman, 2001-07.
;-

COMPILE_OPT IDL2

date = {JTIME_NO_LEAP,	jday		: 0,	$
								seconds	: 0}

END
