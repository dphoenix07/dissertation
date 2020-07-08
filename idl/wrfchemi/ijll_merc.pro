; Subroutine for the transformation of cartesian (i,j) coordinates to the 
; geographical latitude and longitudes in mercator projection
; taken from WPS/geogrid/src/module_map_utils.F

PRO ijll_merc, i, j, lat, lon, truelat1, lat1, lon1, knowni, knownj, dx

 rad_per_deg = !pi/180     
 deg_per_rad = 180/!pi 
 re_m = 6370.

; Preliminary variables
 clain = COS(rad_per_deg*truelat1)
  dlon = dx/(re_m*clain)  

; Compute distance from equator to origin, and store in rsw
 rsw = 0
 If (lat1 ne 0) then $
  rsw = (ALOG(TAN(0.5*((lat1+90.)*rad_per_deg))))/dlon

; Compute the lat/lon from i/j to mercator projection

 lat = 2.0*ATAN(EXP(dlon*(rsw + j-knownj)))*deg_per_rad - 90.
 lon = (i-knowni)*dlon*deg_per_rad + lon1
 If (lon gt 180.) then lon = lon - 360.
 If (lon lt -180.) then lon = lon + 360.

End
