PRO ANIMATE

;+
; Name:
;	ANIMATE
;
; Purpose:
;	Create a sample animation sequence
; 
; Calling sequence: 
;	ANIMATE
;
; Inputs:
;	None
;
; Output:
;	Interactive animation sequence
;
; Keywords:
;	None
;
; Author and history:
;
;-

COMPILE_OPT IDL2				;Set compile options

xsize 	= 300					;Width of window
ysize 	= 300					;Height of window
nframes = 20					;Number of frames
speed 	= 6					;Animation speed

np 	= 1000					;Number of points
x 	= FINDGEN(np)/np			;Create x-coordinate
y 	= SIN(2.0 * !PI * x)			;Function to plot

WINDOW, XSIZE = xsize, YSIZE = ysize, $
   /PIXMAP					;Create pixmap

!P.BACKGROUND = COLOR_24('white')		;Set background to white
IF (!D.N_COLORS EQ 256^3) THEN bpp = 3 $
   			  ELSE bpp = 1		;Bytes per pixel

PRINT, 'Memory required : ', $			;Memory requirements
   (xsize*ysize*nframes*bpp)/(2.0^20), ' MB'

XINTERANIMATE, /SHOWLOAD, $			;1) Initialize animator
   SET = [xsize, ysize, nframes], $
   TITLE = 'Animation Demo 1'

FOR n = 0, nframes-1 DO BEGIN			;Create each frame
   PLOT, x, SHIFT(y, n*(np/nframes)), $		;Plot graph
      COLOR = 0, PSYM = 3
   image = TVRD(TRUE = 3)			;Read image from pixmap
   XINTERANIMATE, IMAGE = image, FRAME = n	;2) Copy image to animator
ENDFOR

XINTERANIMATE, speed				;3) Run animation

!P.BACKGROUND = 0 				;Set background to black

END	

