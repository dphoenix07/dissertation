; IDL Version 8.4.1 (linux x86_64 m64)
; Journal File for dphoenix@aardvark.som.nor.ou.edu
; Working directory: /home/dphoenix/idl/idl_tutorials
; Date: Wed Dec 30 15:38:40 2015
 
PRO PLOT_POWER

COMPILE_OPT IDL2

n = 0

READ, n, PROMPT = 'Enter exponent and <cr>: '
 
x = FINDGEN (10)
y = x^n

PLOT, x, y, $
	TITLE = 'Plot of x^' + STRTRIM(STRING(n), 2), $
	XTITLE = 'x', $
	YTITLE = 'y'

END
