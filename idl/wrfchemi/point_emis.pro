PRO point_emis, primpath, primnames, primdata, ipoint

 nprim = n_elements(primnames)
;
; ipoint = 103249L  ; for EPA99
; ipoint = 168516L  ; for EPA05
;--------------------
; read point source 
; emissions
;--------------------

 primdata = FLTARR(nprim, ipoint)

 FOR iprim = 0, nprim -1 DO BEGIN
    tmp = fltarr(12)
    filename = primpath + primnames(iprim)
;    PRINT,iprim, ' Reading point emissions for ', primnames(iprim)
;    SPAWN, 'gunzip ' + filename + '.gz'
    OPENR, 1, filename
    count = 0L
    FOR  i = 0, ipoint/12 - 1 DO BEGIN
        READF, 1, tmp
        primdata(iprim,count:count+11) = tmp
        count = count + 12
    ENDFOR
    if ipoint-count gt 0 then begin
        tmp = FLTARR(ipoint-count)
        READF, 1, tmp
        primdata(iprim, count:count+ipoint-count-1) = tmp
    endif
    CLOSE, 1
;    SPAWN,'gzip '+filename
ENDFOR


END


PRO point_emis_info, primpath, infofile, lat, lon, stackD, stackH, ipoint, reltype

;--------------------
; Read release point
; info file
;--------------------
; ipoint = 103249L  ; for EPA99
; ipoint = 168516L  ; for EPA05
print,'ipoint: ',ipoint

 state = STRARR(ipoint)
 county = STRARR(ipoint)
 siteID      = STRARR(ipoint)
 reportID    = STRARR(ipoint)
 unitID      = STRARR(ipoint)
 processID   = STRARR(ipoint)
 reltype     = FLTARR(ipoint)
 stackH      = FLTARR(ipoint)
 stackD      = FLTARR(ipoint)
 exitT       = FLTARR(ipoint)
 flowV       = FLTARR(ipoint)
 flowR       = FLTARR(ipoint)
 lon         = FLTARR(ipoint)
 lat         = FLTARR(ipoint)

 OPENR, 1, infofile
 strdum = ' '
 FOR i = 0L, ipoint-1  DO BEGIN
    READF, 1, strdum
    state(i)    = STRCOMPRESS(STRMID(strdum,0,2),/REMOVE_ALL)
    county(i)   = STRCOMPRESS(STRMID(strdum,2,3),/REMOVE_ALL)
    siteID(i)   = STRCOMPRESS(STRMID(strdum,5,15),/REMOVE_ALL)
    reportID(i) = STRCOMPRESS(STRMID(strdum,20,8),/REMOVE_ALL)     ; mcb changed from 20,6 to 20,8
    unitID(i)   = STRCOMPRESS(STRMID(strdum,28,8),/REMOVE_ALL)     ; mcb changed from 26,6 to 28,8
    processID(i)= STRCOMPRESS(STRMID(strdum,36,8),/REMOVE_ALL)     ; mcb changed from 32,6 to 36,8
    reltype(i)  = STRCOMPRESS(STRMID(strdum,45,2),/REMOVE_ALL)     ; mcb changed from 39,2 to 45,2
    stackH(i)   = FLOAT(STRMID(strdum,48,10))                      ; mcb changed from 42,10 to 48,10
    stackD(i)   = FLOAT(STRMID(strdum,58,10))
    exitT(i)    = FLOAT(STRMID(strdum,68,10))
    flowV(i)    = FLOAT(STRMID(strdum,78,10))
    flowR(i)    = FLOAT(STRMID(strdum,88,10))
    lon(i)      = FLOAT(STRMID(strdum,98,11))
    lat(i)      = FLOAT(STRMID(strdum,109,10))
;         10        20        30        40        50        60        70        80        90       100       110       120       130
;01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
;01012012345678901234012345670123456701234567 01 01234567890123456789012345678901234567890123456789012345678900123456789
;0100110583111       509106125226371371808614  2     48.768     5.791    348.15    15.606  410.7302  -86.57390  32.38379     0.000   7897 1A
;0100110708711       543270125864961376997514  2     53.340     5.486    355.37    20.604  486.8600  -86.73880  32.64893     0.000   7897

    if i le 3 then $
     print, state(i), ' ', county(i), ' ', siteID(i), ' ', reportID(i), ' ', unitID(i), ' ', processID(i), ' ', reltype(i), stackH(i), stackD(i), exitT(i), flowV(i), flowR(i), lon(i), lat(i)

 ENDFOR
 CLOSE, 1

END
