PRO CDATE__DEFINE

;+
;NAME:
;		CDATE__DEFINE
;PURPOSE:
;		This procedure defines the CDATE date structure.
;CATEGORY:
;		Date and time calculations.
;CALLING SEQUENCE:
;		Called automatically as date = {CDATE}
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

date = {CDATE,	year		: 0, $
					month		: 0, $
					day		: 0, $
					hour		: 0, $
					minute	: 0, $
					second	: 0}

END
