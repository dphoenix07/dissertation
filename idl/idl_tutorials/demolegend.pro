pro demolegend

; demolegend.pro
;
; this file demonstrates the use of the Legend routine in multiplots

!p.multi=[0,1,2]

meanings = ['good stuff', 'better stuff', 'best stuff']
psyms = [-1,-2,-4]
lines = [1,2,4]

x=FINDGEN(11)

FOR nplots=0,1 DO BEGIN

        PLOT,x,x^1.3,/nodata
        for i=0,2 do OPLOT, x, x^(1.1+(i/10.)), psym=psyms[i],   $
                linestyle=lines[i]
        LEGEND, meanings, /right, /top, psym=psyms,  $
                linestyle=lines
ENDFOR
!p.multi=[0,1,1]

return
end