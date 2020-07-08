@ijll_lc.pro
@llij.pro

  dummy='  '
   nx = 173
   ny = 128
   smoke_area = 4.*4.

   lon_ref = fltarr(nx, ny)
   lat_ref = fltarr(nx,ny)
   openr,1,'CAMx4km_points2_geo_clip.csv'
   readf,1,dummy
   for i=0,nx-1 do begin
      for j=0,ny-1 do begin
         readf,1,dummy
         parts = str_sep(dummy,',')
         lat_ref[i,j] = parts[3]
         lon_ref[i,j] = parts[2]
      endfor
   endfor
   close,1
   
   lon_data = fltarr(nx, ny)    ;with this I differ up to 2 km from what Christine has 
   lat_data = fltarr(nx,ny)
   clon =  lon_ref[0]
   clat = lat_ref[0]
   truelat1 = 33.
   truelat2 = 45.
   std_lon = -97.
   dx = 4.0
   knowni = 0.5
   knownj = 0.5
   for i=0,nx-1 do begin
      for j=0,ny-1 do begin
         ijll_lc, i+.5,j+.5, a1, a2,  truelat1, truelat2, 1, std_lon,clat, clon, knowni, knownj, dx
         lon_data[i,j]  = a2
         lat_data[i, j] = a1
      endfor
   endfor


;with this I differ up to 2 km from what Christine has 
      
lonnei = fltarr(40000000)
latnei = fltarr(40000000)
count = 0L 
nbins = 16   ;4
nbrow = 4    ;2

for ix = 0, nx - 1 do begin
   for jx = 0, ny - 1 do begin
      ict = 0L
      for ic = 0, nbrow-1 do begin
         for jc = 0, nbrow-1 do begin
            x1 = float(ic)/float(nbrow) + float(1./(2.*nbrow))
            y1 = float(jc)/float(nbrow) + float(1./(2.*nbrow))
            ijll_lc, ix+1.+x1, jx+1.+y1, a1, a2,  truelat1, truelat2, 1, std_lon,clat, clon, knowni, knownj, dx
            
            lonnei[count+ict] = a2
            latnei[count+ict] = a1 
            ict = ict + 1L

         endfor
      endfor
             
      count = count + nbins
   endfor
endfor

ncount = count

end
