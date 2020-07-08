PRO WRFRT_GRID_MAP

COMPILE_OPT IDL2

PS_ON, FILENAME = '~/wrfgrid.eps', PAGE_SIZE = [6.0,5.0], MARGIN = 0.0, /INCHES			;Switch to Postscript device
DEVICE, /ENCAPSULATED
!P.FONT     = 0																								;Hardware fonts
!P.CHARSIZE = 1.0	
MAP_WRF_GRID, -95.28, 39.97, 211, 166, 10.0, TRUE_LATS = [10.0, 60.0]
MAP_WRF_GRID, -95.28, 39.97, 301, 241, 2.0, OVER = [80*10,55*10], TRUE_LATS = [10.0, 60.0]
PS_OFF

END