PRO WRITE_ANIMATED_GIF, date, scheme, variable, SPEED = speed

;+
; Name:
;		WRITE_ANIMATED_GIF
; Purpose:
;		This is a procedure to write an animated gif. 
; Calling sequence:
;		WRITE_ANIMATED_GIF, date, scheme, variable
; Input:
;		date : 		Case date (e.g., '20120519')
;		scheme: 	MP scheme (e.g., 'morrison')
;		variable: 	variable plotted (e.g., 'tropo3' or 'refl') 
;		
; Output:
;		None.
; Keywords:
;		SPEED : Animation speed. Higher the number, slower the animation
; Author and history:
;		Cameron R. Homeyer  2013-08-10.
;-

COMPILE_OPT IDL2																									;Set compile options

;directory = !WRF_DIRECTORY + date + '/' + scheme + '/plots/' + variable + '/'
directory = '/home/dphoenix/info_tech/'

IF (N_ELEMENTS(speed    ) EQ 0) THEN speed     = 5														;Set default image animation speed

;gif_name = date + '_' + scheme + '_' + variable + '.gif'
gif_name = 'TwoWaysToScaleWindVectors.gif'
									
FILE_DELETE, directory + gif_name, /ALLOW_NONEXISTENT													;Delete existing animation

images = FILE_SEARCH(directory + '*.gif', COUNT = nimage)											;Find GIF images

IF (nimage GT 0) THEN BEGIN
	FOR i = 0, nimage - 1 DO BEGIN
		READ_GIF, images[i], image, r, g, b																	;Read GIF image
	
		FOR j = 1, speed DO $					
			WRITE_GIF, directory + gif_name, image, r, g, b, /MULTIPLE							;Write images to file
	ENDFOR
	WRITE_GIF, directory + gif_name, /CLOSE														;Close GIF file so future animations do not append
ENDIF ELSE BEGIN
	images = FILE_SEARCH(directory + '*.png', COUNT = nimage)										;Find GIF images

	FOR i = 0, nimage - 1 DO BEGIN
		READ_PNG, images[i], image0																			;Read PNG image
		
		color = COLOR_24(image0[0,*,*],image0[1,*,*],image0[2,*,*])									;Compute 24-bit colors of image
		dim   = SIZE(color, /DIMENSIONS)

		isort  = SORT(color)																						;Sort color values
		iuniq  = UNIQ(color[isort])																			;Find unique colors
	
		table  = (color[isort])[iuniq]																		;Extract unique values (color palette)
		rgb    = COMPONENT_24(table)																			;Get red green blue components
		
		image  = INDEX_OF_NEAREST_CRH(table, color)														;Get color indices of image
		image  = REFORM(BYTE(image), dim[1], dim[2])														;Reform image and convert to BYTE index
		
		FOR j = 1, speed DO $
			WRITE_GIF, directory + gif_name, image, $											;Write images to file
				rgb[*,0], rgb[*,1], rgb[*,2], /MULTIPLE
	ENDFOR
	WRITE_GIF, directory + gif_name, /CLOSE														;Close GIF file so future animations do not append
ENDELSE

END
