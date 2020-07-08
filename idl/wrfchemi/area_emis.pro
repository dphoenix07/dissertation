PRO AREA_EMIS, primpath, primnames, primdata, ix, jx, lat, lon

ix = 1332     ; x and y dimensions of emission files
jx = 1008

; ---------------------
; get lat and lon of 
; center of grid point
; --------------------

lat = FLTARR(ix, jx)
lon = FLTARR(ix, jx)

FOR i=0,ix-1 DO BEGIN
    FOR j=0,jx-1 DO BEGIN
        mapcf, i+1.5, j+1.5, xlat, xlon
        lat(i,j) = xlat
        lon(i,j) = xlon
    ENDFOR
ENDFOR

; ---------------------
; read emission files
; --------------------

primdata = FLTARR(ix,jx)
data = FLTARR(ix, jx)

    name = primnames
    PRINT, '  Reading area emissions for ' + name
    primfile = primpath + name
;    SPAWN,'gunzip '+primfile+'.gz'
    OPENR,1, primfile
    READF,1, data
    CLOSE,1
    primdata(*,*) = data
;    SPAWN,'gzip ' + primfile

END
