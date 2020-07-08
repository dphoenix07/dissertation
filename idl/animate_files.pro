PRO ANIMATE_FILES, date, scheme, var

;+
; Name:
;	ANIMATE_FILES
;
; Purpose:
;	Animate existing created PNG files
;
; Calling sequence:
;	ANIMATE_FILES, date, scheme
;
; Inputs:
;	indir : path to input directory
;
; Outputs:
;	Interactive animation sequence
;
; Keywords:
;	None
;
; Author and history:
;
;-

COMPILE_OPT IDL2																			;Set compile options

IF (N_ELEMENTS(date) EQ 0) THEN  	date 	= '20120519'
IF (N_ELEMENTS(scheme) EQ 0) THEN	scheme 	= 'morrison'

indir = !WRF_DIRECTORY + '/' + date + '/' + scheme + '/plots/' + var + '/'							;Input directory

file = FILE_SEARCH(indir + '*', $															;Find all files in indir
   COUNT = nframes)

IF (nframes EQ 0) THEN $
   MESSAGE, 'No files found in ' + indir

status = QUERY_PNG(file[0], info)															;Get frame size
xsize = info.dimensions[0]																	;Width of graphic window
ysize = info.dimensions[1]																	;Height of graphic window
speed = 5																					;Initial animation speed

IF (!D.N_COLORS EQ 256^3) THEN bpp = 3 $													;Bytes per pixel
			  ELSE bpp = 1

PRINT, 'Memory required : ', $																;Memory requirement
   (xsize*ysize*nframes*bpp)/(2.0^20), ' MB'

XINTERANIMATE, /SHOWLOAD, $																	;1) Initialize animator
   SET = [xsize, ysize, nframes];, $			
 ;  TITLE = 'Simulated Radar Reflectivity'

FOR n = 0, nframes - 1 DO BEGIN																;Create each frame
   image = READ_PNG(file[n])																;Read image from file
   XINTERANIMATE, IMAGE = image, FRAME = n													;2) Copy image to animator
ENDFOR

XINTERANIMATE, speed																		;3) Run animator

END


