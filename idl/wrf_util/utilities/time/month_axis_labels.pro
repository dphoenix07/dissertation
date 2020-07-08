PRO MONTH_AXIS_LABELS

;	This program demonstrates how to add centered month labels
;	to the time axis of a graph.


nmonths = 12																	;Number of months
t       = (0.5 + FINDGEN(nmonths))/nmonths							;Mid-month times

temp    = 10.0 + 10.0*RANDOMN(seed, nmonths)							;Fake data

PLOT, t, temp, $																;Draw graph
	XTITLE = 'Month', $
	XTICKS = nmonths, $
	XRANGE = [0.0, 1.0], $
	XSTYLE = 1, $
	XTICKNAME = REPLICATE(' ', nmonths + 1), $						;nmonths+1 blank strings
	YTITLE = 'Temperature'
AXIS, XAXIS = 0, $															;Add month labels to axis
	XTICKS = nmonths - 1, $													;nmonths-1 tick intervals
	XTICKV = t, $
	XTICKLEN = 0.0001, $
	XTICKNAME = ['JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', $
					 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC']
					 
END
