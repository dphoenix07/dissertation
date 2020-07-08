; Script to plot log10(x)

n = 10				; Number of points to plot
x = 1.0 + FINDGEN(n)		; Compute abcissa
y = ALOG(x)			; Compute ordinate
PLOT, x,y,$			; Plot the graph
	TITLE = 'Plot of base-10 logarithm of x', $
	XTITLE = 'x', $
	YTITLE = 'log10(x)'
