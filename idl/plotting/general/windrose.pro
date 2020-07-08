;----------------------------------------------------------------------------
; :File:        windrose.pro
; :Author:      Brian Vanderwende
;
; :Description: Generates a windrose from data selected
; 
; :Input params:
;     speed - the speed input in the form of an array
;     dir - the direction input in the form of an array
;     bWidth - the bin width
;     sBins - the speed categories [m/s], ascending array of four elements
;     pTitle - title of the graph to be produced
;     fPath - the postscript file path
;     flag - value for flagged data
;     
; :Example:
;	  sArray = [5,10,15,20]
;     windrose, dArray, sArray, 10, "Test Windrose", "/output/test.ps", -99.0
;----------------------------------------------------------------------------

pro windrose

bWidth = 5.0
sBins = [5,10,15,20]
pTitle = 'Wind Rose'
fPath = '/data3/dphoenix/wrf/general/windrose.ps'
flag = -99.0

o3_storm_sept = []
wind_spd_storm_sept = []
wind_dir_storm_sept = []

o3_nstorm_sept = []
wind_spd_nstorm_sept = []
wind_dir_nstorm_sept = []

infile      = !WRF_DIRECTORY + 'general/o3_data/hourly_data/hourly_anomalies_091980_101995_SEPT_18Z.nc'																;Set input file path

;Get monthly averaged data
id  = NCDF_OPEN(infile)																						;Open input file for reading	

NCDF_VARGET, id, 'O3_storm'					, o3_storm
NCDF_VARGET, id, 'Wind_Speed_storm'   		, wind_spd_storm
NCDF_VARGET, id, 'Wind_Direction_storm'		, wind_dir_storm

NCDF_VARGET, id, 'O3_nstorm'				, o3_nstorm
NCDF_VARGET, id, 'Wind_Speed_nstorm'   		, wind_spd_nstorm
NCDF_VARGET, id, 'Wind_Direction_nstorm'	, wind_dir_nstorm
NCDF_CLOSE, id

o3_storm_sept = [o3_storm_sept, o3_storm]
wind_spd_storm_sept = [wind_spd_storm_sept, wind_spd_storm]
wind_dir_storm_sept = [wind_dir_storm_sept, wind_dir_storm]

o3_nstorm_sept = [o3_nstorm_sept, o3_nstorm]
wind_spd_nstorm_sept = [wind_spd_nstorm_sept, wind_spd_nstorm]
wind_dir_nstorm_sept = [wind_dir_nstorm_sept, wind_dir_nstorm]

infile      = !WRF_DIRECTORY + 'general/o3_data/hourly_data/hourly_anomalies_091996_102000_SEPT_18Z.nc'																;Set input file path

;Get monthly averaged data
id  = NCDF_OPEN(infile)																						;Open input file for reading	

NCDF_VARGET, id, 'O3_storm'					, o3_storm
NCDF_VARGET, id, 'Wind_Speed_storm'   		, wind_spd_storm
NCDF_VARGET, id, 'Wind_Direction_storm'		, wind_dir_storm

NCDF_VARGET, id, 'O3_nstorm'				, o3_nstorm
NCDF_VARGET, id, 'Wind_Speed_nstorm'   		, wind_spd_nstorm
NCDF_VARGET, id, 'Wind_Direction_nstorm'	, wind_dir_nstorm
NCDF_CLOSE, id

o3_storm_sept = [o3_storm_sept, o3_storm]
wind_spd_storm_sept = [wind_spd_storm_sept, wind_spd_storm]
wind_dir_storm_sept = [wind_dir_storm_sept, wind_dir_storm]

o3_nstorm_sept = [o3_nstorm_sept, o3_nstorm]
wind_spd_nstorm_sept = [wind_spd_nstorm_sept, wind_spd_nstorm]
wind_dir_nstorm_sept = [wind_dir_nstorm_sept, wind_dir_nstorm]


infile      = !WRF_DIRECTORY + 'general/o3_data/hourly_data/hourly_anomalies_092001_102004_SEPT_18Z.nc'																;Set input file path

;Get monthly averaged data
id  = NCDF_OPEN(infile)																						;Open input file for reading	

NCDF_VARGET, id, 'O3_storm'					, o3_storm
NCDF_VARGET, id, 'Wind_Speed_storm'   		, wind_spd_storm
NCDF_VARGET, id, 'Wind_Direction_storm'		, wind_dir_storm

NCDF_VARGET, id, 'O3_nstorm'				, o3_nstorm
NCDF_VARGET, id, 'Wind_Speed_nstorm'   		, wind_spd_nstorm
NCDF_VARGET, id, 'Wind_Direction_nstorm'	, wind_dir_nstorm
NCDF_CLOSE, id

o3_storm_sept = [o3_storm_sept, o3_storm]
wind_spd_storm_sept = [wind_spd_storm_sept, wind_spd_storm]
wind_dir_storm_sept = [wind_dir_storm_sept, wind_dir_storm]

o3_nstorm_sept = [o3_nstorm_sept, o3_nstorm]
wind_spd_nstorm_sept = [wind_spd_nstorm_sept, wind_spd_nstorm]
wind_dir_nstorm_sept = [wind_dir_nstorm_sept, wind_dir_nstorm]

infile      = !WRF_DIRECTORY + 'general/o3_data/hourly_data/hourly_anomalies_092005_102017_SEPT_18Z.nc'																;Set input file path

;Get monthly averaged data
id  = NCDF_OPEN(infile)																						;Open input file for reading	

NCDF_VARGET, id, 'O3_storm'					, o3_storm
NCDF_VARGET, id, 'Wind_Speed_storm'   		, wind_spd_storm
NCDF_VARGET, id, 'Wind_Direction_storm'		, wind_dir_storm

NCDF_VARGET, id, 'O3_nstorm'				, o3_nstorm
NCDF_VARGET, id, 'Wind_Speed_nstorm'   		, wind_spd_nstorm
NCDF_VARGET, id, 'Wind_Direction_nstorm'	, wind_dir_nstorm
NCDF_CLOSE, id

o3_storm_sept = [o3_storm_sept, o3_storm]
wind_spd_storm_sept = [wind_spd_storm_sept, wind_spd_storm]
wind_dir_storm_sept = [wind_dir_storm_sept, wind_dir_storm]

o3_nstorm_sept = [o3_nstorm_sept, o3_nstorm]
wind_spd_nstorm_sept = [wind_spd_nstorm_sept, wind_spd_nstorm]
wind_dir_nstorm_sept = [wind_dir_nstorm_sept, wind_dir_nstorm]


speed = wind_spd_nstorm_sept
dir = wind_dir_nstorm_sept
; Constants

  cols = [220,200,180,110,80]

; Set up plot area

  loadct, 39
  
  ; Specify plot dimensions/formatting
  !x.style = 1
  !y.style = 1
  !p.thick = 1.5
  !x.thick = 4
  !y.thick = 4
  !p.charsize = 1.4
  !p.font = 0
  
  ; Set output device to postscript
  set_plot, 'ps'
  
  ; Set multiplotting to off
  !p.multi = 0
  
  ; Set device parameters
  device, /portrait, ysize = 10, yoffset = 16, xsize = 13, $
          /HELVETICA, /ISOLATIN1

; Begin wind rose generation

  nslots = 360.0 / bWidth + 1.0

  wdbin = fltarr(nslots,5)
  wdegrees = findgen(nslots) * bWidth

  for n = 0L, n_elements(speed) - 1 do begin
    if speed(n) NE flag AND dir(n) NE flag then begin
      bin = (dir(n) - (dir(n) MOD bWidth)) / bWidth
      if dir(n) MOD bWidth GE (bWidth / 2.0) then begin
        bin = bin + 1
      endif
      wdbin(bin,0) = wdbin(bin,0) + 1
      if speed(n) LT sBins(3) then wdbin(bin,1) = wdbin(bin,1) + 1
      if speed(n) LT sBins(2) then wdbin(bin,2) = wdbin(bin,2) + 1
      if speed(n) LT sBins(1) then wdbin(bin,3) = wdbin(bin,3) + 1
      if speed(n) LT sBins(0) then wdbin(bin,4) = wdbin(bin,4) + 1
    endif
  endfor
    
  ; Normalize by largest so that each direction is a percentage of largest
  wdbin = wdbin / total(wdbin(*,0)) * 100
  
  ; Determine bounds of graph based on bin width
  outer = max(ceil(wdbin / 10.0)) * 10.0
  inner = outer / 2.0
    
  device, filename = fPath, /color 
    plot, [0,0], [0,0], xstyle = 5, ystyle = 5, $
      xrange = [-1 * outer,outer], yrange = [-1 * outer,outer], position = aspect(1.0), title = pTitle
    axis, 0, 0, xaxis = 0, xtickinterval = 100.0, xminor = 1, xtickname = [' ',' ',' '], xstyle = 1
    axis, 0, 0, yaxis = 0, ytickinterval = 100.0, yminor = 1, ytickname = [' ',' ',' '], ystyle = 1
      
    for n = 0, nslots - 1 do begin
      for s = 0, 4 do begin
        i = bWidth / 2.0
        polyfill, [0, wdbin(n,s) * sin((wdegrees(n) - i) * !pi / 180), wdbin(n,s) * sin((wdegrees(n) + i) * !pi / 180)], $
                  [0, wdbin(n,s) * cos((wdegrees(n) - i) * !pi / 180), wdbin(n,s) * cos((wdegrees(n) + i) * !pi / 180)], $
                  color = 0
        
        i = bWidth / 2.0 - 0.5
        lenm = wdbin(n,s) - 0.05 * (outer / 10.0)
        polyfill, [0, lenm * sin((wdegrees(n) - i) * !pi / 180), lenm * sin((wdegrees(n) + i) * !pi / 180)], $
                  [0, lenm * cos((wdegrees(n) - i) * !pi / 180), lenm * cos((wdegrees(n) + i) * !pi / 180)], $
                  color = cols[s]
      endfor
    endfor
      
    ; plot concentric circles
      
    oc = circle(0, 0, outer)
    plots, oc, linestyle = 2
    xyouts, outer + 0.3, 0.2, string(outer, format = '(I2)') + '%', charsize = 1
      
    ic = circle(0, 0, inner)
    plots, ic, linestyle = 2
    if inner LT 10.0 then fStr = '(I1)' else fStr = '(I2)'
    xyouts, inner + 0.3, 0.2, string(inner, format = fStr), charsize = 1
      
    ; generate legend
    ;   sFct is a scaling factor to make legend same regardless of plot size... not elegant!!
    sFct = outer / 10.0
    
    polyfill, [4.96*sFct, 11.24*sFct, 11.24*sFct, 4.96*sFct], [-4.96*sFct, -4.96*sFct, -10.02*sFct, -10.02*sFct], color = 0
    polyfill, [5*sFct, 11.2*sFct, 11.2*sFct, 5*sFct], [-5*sFct, -5*sFct, -9.99*sFct, -9.99*sFct], color = 299
    polyfill, [5.2*sFct, 5.7*sFct, 5.7*sFct, 5.2*sFct], [-5.9*sFct, -5.9*sFct, -6.5*sFct, -6.5*sFct], color = cols[4]
    polyfill, [5.2*sFct, 5.7*sFct, 5.7*sFct, 5.2*sFct], [-6.7*sFct, -6.7*sFct, -7.3*sFct, -7.3*sFct], color = cols[3]
    polyfill, [5.2*sFct, 5.7*sFct, 5.7*sFct, 5.2*sFct], [-7.5*sFct, -7.5*sFct, -8.1*sFct, -8.1*sFct], color = cols[2]
    polyfill, [5.2*sFct, 5.7*sFct, 5.7*sFct, 5.2*sFct], [-8.3*sFct, -8.3*sFct, -8.9*sFct, -8.9*sFct], color = cols[1]
    polyfill, [5.2*sFct, 5.7*sFct, 5.7*sFct, 5.2*sFct], [-9.1*sFct, -9.1*sFct, -9.7*sFct, -9.7*sFct], color = cols[0]
    xyouts, 5.1*sFct, -5.6*sFct, 'Bin width - ' + string(bWidth, format = '(I2.2)') + '!9%!3', $
            charsize = 0.7, charthick = 1.0
    xyouts, 5.8*sFct, -6.4*sFct, '00 - ' + string(sBins(0), format = '(I2.2)') + ' m/s', charsize = 0.7, charthick = 1.0
    xyouts, 5.8*sFct, -7.2*sFct, string(sBins(0), format = '(I2.2)') + ' - ' + $
            string(sBins(1), format = '(I2.2)') + ' m/s', charsize = 0.7, charthick = 1.0
    xyouts, 5.8*sFct, -8.0*sFct, string(sBins(1), format = '(I2.2)') + ' - ' + $
            string(sBins(2), format = '(I2.2)') + ' m/s', charsize = 0.7, charthick = 1.0
    xyouts, 5.8*sFct, -8.8*sFct, string(sBins(2), format = '(I2.2)') + ' - ' + $
            string(sBins(3), format = '(I2.2)') + ' m/s', charsize = 0.7, charthick = 1.0
    xyouts, 5.8*sFct, -9.6*sFct, string(sBins(3), format = '(I2.2)') + '+ m/s', charsize = 0.7, charthick = 1.0
  device, /close
  
  print, 'All Done!'
end