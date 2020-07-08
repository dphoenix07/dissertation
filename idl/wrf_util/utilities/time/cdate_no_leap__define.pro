PRO CDATE_NO_LEAP__DEFINE

;+
;NAME:
;		CDATE_NO_LEAP__DEFINE
;PURPOSE:
;		This procedure defines the CDATE_NO_LEAP date structure.
;CATEGORY:
;		Date and time calculations.
;CALLING SEQUENCE:
;		Called automatically as date = {CDATE_NO_LEAP}
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

date = {CDATE_NO_LEAP,	year		: 0,	$
								month		: 0,	$
								day		: 0, $
								hour		: 0, $
								minute	: 0,	$
								second	: 0}

END
