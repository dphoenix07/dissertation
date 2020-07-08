FUNCTION COLOR_24_DP, r, g, b

; NAME:
;		COLOR_24
; PURPOSE:
;		Convert r, g, and b color value(s) to 24-bit color values.  R, g, and b
;		should be integers in the range [0, 255].  If not, they are converted to LONGs 
;		and then truncated to that range. R, g, and b can be three scalars or three 
;     arrays of equal dimension.
;
;		If g and b are omitted and r is a numerical expression, then r is assumed to
;		represent a grayscale value.  That is, the g and b values are set equal to r.
;
;		This function also includes a set of predefined colors that can be selected
;		by name.  If g and b are omitted and r is STRING expression, COLOR_24 
;		attempts to find a color with that name in the predefined table of colors.  
;		If the current device is not a 24-bit device (such as the PS device), COLOR_24 
;     assumes that LOAD_BASIC_COLORS has been called to load the predefined colors
;     into the color table.
; CATEGORY:
;		Color calculations.
; CALLING SEQUENCE:
;		color = COLOR_24(r, g, b) for 24-bit color
;		color = COLOR_24(r)       for 24-bit grayscale
;		color = COLOR_24('name')  for pre-defined color
; INPUT:
;		r   : red value(s).    r is converted to LONG and truncated to the range [0, 255]
;		g   : green value(s).  g is converted to LONG and truncated to the range [0, 255]
;		b   : blue value(s).   b is converted to LONG and truncated to the range [0, 255]
;
;		r   : string containing the name of a predefined color
; OUTPUT:
;	Scalar or array of 24-bit color value(s) of type LONG.
; MODIFICATION HISTORY:
;   K. Bowman, 1999-02-11.
;   K. Bowman, 2001-02-14.  Modified to include predefined colors by name.
;   K. Bowman, 2003-10-09.  Modified to allow single numerical value for r to indicate grayscale.
;   K. Bowman, 2005-11-09.  Added more colors.

COMPILE_OPT IDL2																					;Set compile options

IF (N_PARAMS() EQ 1L) THEN BEGIN																;Find a predefined color
	IF (SIZE(r, /TNAME) EQ 'STRING') THEN BEGIN											;Look for name in table
	   IF (!D.N_COLORS NE 256^3) THEN BEGIN
			CASE STRUPCASE(r) OF
				'BLACK'        : RETURN,  0
				'WHITE'        : RETURN,  1
				'GRAY10'       : RETURN,  2
				'GRAY20'       : RETURN,  3
				'GRAY30'       : RETURN,  4
				'GRAY40'       : RETURN,  5
				'GRAY50'       : RETURN,  6
				'GRAY60'       : RETURN,  7
				'GRAY70'       : RETURN,  8
				'GRAY80'       : RETURN,  9
				'GRAY90'       : RETURN, 10
				'RED'          : RETURN, 11
				'GREEN'        : RETURN, 12
				'BLUE'         : RETURN, 13
				'YELLOW'       : RETURN, 14
				'MAGENTA'      : RETURN, 15
				'CYAN'         : RETURN, 16
				'LIGHTRED'     : RETURN, 17
				'LIGHTGREEN'   : RETURN, 18
				'LIGHTBLUE'    : RETURN, 19
				'LIGHTYELLOW'  : RETURN, 20
				'LIGHTMAGENTA' : RETURN, 21
				'LIGHTCYAN'    : RETURN, 22
				'DARKRED'      : RETURN, 23
				'ORANGE'       : RETURN, 24
				'DARKGREEN'    : RETURN, 25
				'DARKBLUE'     : RETURN, 26
				'OLIVE'        : RETURN, 27
				'PURPLE'       : RETURN, 28
				'BLUEGREEN'    : RETURN, 29
				'BROWN'        : RETURN, 30
				ELSE           : BEGIN
										 MESSAGE, 'Color ' + r + ' is not defined.', /CONTINUE
										 RETURN, 0
									  END
			ENDCASE
	   ENDIF ELSE BEGIN
			CASE STRUPCASE(r) OF
				'BLACK'        : RETURN, COLOR_24(  0,   0,   0)
				'WHITE'        : RETURN, COLOR_24(255, 255, 255)
				'GRAY10'       : RETURN, COLOR_24( 25,  25,  25)
				'GRAY20'       : RETURN, COLOR_24( 51,  51,  51)
				'GRAY30'       : RETURN, COLOR_24( 76,  76,  76)
				'GRAY40'       : RETURN, COLOR_24(102, 102, 102)
				'GRAY50'       : RETURN, COLOR_24(127, 127, 127)
				'GRAY60'       : RETURN, COLOR_24(153, 153, 153)
				'GRAY70'       : RETURN, COLOR_24(178, 178, 178)
				'GRAY80'       : RETURN, COLOR_24(204, 204, 204)
				'GRAY90'       : RETURN, COLOR_24(229, 229, 229)
				'RED'          : RETURN, COLOR_24(255,   0,   0)
				'GREEN'        : RETURN, COLOR_24(  0, 255,   0)
				'BLUE'         : RETURN, COLOR_24(  0,   0, 255)
				'YELLOW'       : RETURN, COLOR_24(255, 255,   0)
				'MAGENTA'      : RETURN, COLOR_24(255,   0, 255)
				'CYAN'         : RETURN, COLOR_24(  0, 255, 255)
				'LIGHTRED'     : RETURN, COLOR_24(255, 127, 127)
				'LIGHTGREEN'   : RETURN, COLOR_24(127, 255, 127)
				'LIGHTBLUE'    : RETURN, COLOR_24(127, 127, 255)
				'LIGHTYELLOW'  : RETURN, COLOR_24(255, 255, 127)
				'LIGHTMAGENTA' : RETURN, COLOR_24(255, 127, 255)
				'LIGHTCYAN'    : RETURN, COLOR_24(127, 255, 255)
				'DARKRED'      : RETURN, COLOR_24(127,   0,   0)
				'DARKGREEN'    : RETURN, COLOR_24(  0, 127,   0)
				'DARKBLUE'     : RETURN, COLOR_24(  0,   0, 127)
				'OLIVE'        : RETURN, COLOR_24(127, 127,   0)
				'PURPLE'       : RETURN, COLOR_24(127,   0, 127)
				'BLUEGREEN'    : RETURN, COLOR_24(  0, 127, 127)
				'ORANGE'       : RETURN, COLOR_24(255, 127,   0)
				'BROWN'        : RETURN, COLOR_24(153, 102,  51)
				ELSE           : BEGIN
										  MESSAGE, 'Color ' + r + ' is not defined.', /CONTINUE
										  RETURN, 0
									  END
			ENDCASE
		ENDELSE
   ENDIF ELSE BEGIN																				;Assume gray
   	g = r
     	b = r
   ENDELSE
ENDIF

RETURN,      ((0 > LONG(r)) < 255) + $														;Convert RGB to 24-bit color
        256*(((0 > LONG(g)) < 255) + $
        256* ((0 > LONG(b)) < 255))
END
