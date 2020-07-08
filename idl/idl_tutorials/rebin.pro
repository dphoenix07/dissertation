; IDL Version 8.4.1 (linux x86_64 m64)
; Journal File for dphoenix@aardvark.som.nor.ou.edu
; Working directory: /home/dphoenix
; Date: Mon Dec 28 20:54:09 2015
 

; Generate 2-D coordinates and plot a sample function

; Number of x and y grid points
nx = 21
ny = 26

; x and y grid point spacing
dx = 1.0/(nx-1)
dy = 1.0/(ny-1)

; compute 1-D x and y coordinates
x = dx*FINDGEN(nx)
y = dy*FINDGEN(ny)

; Expand x and y coordinates to 2-D
xx = REBIN(x, nx, ny, /SAMPLE)
yy = REBIN(REFORM(y, 1, ny), nx, ny, /SAMPLE)

; Compute z
z = SIN(2.0 * !PI * xx) * SIN(2.0 * !PI * yy)
HELP, xx, yy, z

; Plot contour graph
CONTOUR, z, x, y, /FOLLOW, $
	LEVELS = -1.0+0.2*FINDGEN(11), $
	TITLE = 'Plot of sin(2 pi x) * sin(2 pi y)', $
	XTITLE = 'x', $
	YTITLE = 'y'
