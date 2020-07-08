; Subroutine for the transformation of geographical latitude and 
; longitude to cartesian (i,j) coordinates in mercator projection
; taken from WPS/geogrid/src/module_map_utils.F

PRO llij_merc, lat, lon, i, j, truelat1, lat1, lon1, knowni, knownj, dx

 rad_per_deg = !pi/180     
 deg_per_rad = 180/!pi 
 re_m = 6370.

; Preliminary variables
 deltalon = lon - lon1
 If (deltalon lt -180.) then deltalon = deltalon + 360
 If (deltalon gt +180.) then deltalon = deltalon - 360

 clain = COS(rad_per_deg*truelat1)
  dlon = dx/(re_m*clain)  

; Compute distance from equator to origin, and store in rsw
 rsw = 0
 If (lat1 ne 0) then $
  rsw = (ALOG(TAN(0.5*((lat1+90.)*rad_per_deg))))/dlon

; Compute i/j coordinate from lat lon

 i = knowni + (deltalon/(dlon*deg_per_rad))
 j = knownj + (ALOG(TAN(0.5*((lat + 90.) * rad_per_deg)))) / $
             dlon - rsw
End
