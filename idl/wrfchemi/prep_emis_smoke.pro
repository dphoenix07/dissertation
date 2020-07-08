PRO ijll_lc_camx, xloc, yloc, ylat, xlon, truelat1, truelat2, hemi, stdlon,lat1, lon1, knowni, knownj, dx

;     Input arguments:
;c        iway                Conversion type
;c                            0 = geodetic to Lambert Conformal
;c                            1 = Lambert Conformal to geodetic
;c        phic                Central latitude (deg, neg for southern hem)
;c;        xlonc               Central longitude (deg, neg for western hem)
;c        truelat1            First true latitute (deg, neg for southern hem)
;c        truelat2            Second true latitute (deg, neg for southern hem)
;c        xloc/yloc           Projection coordinates (km)
;c        xlon/ylat           Longitude/Latitude (deg)
;c

iway = 0
phic = 40.
xlonc=-97.

conv=57.29578
a=6370.

if (phic lt 0) then sign = -1. else sign = 1.

pole = 90.
if (abs(truelat1) gt 90.) then begin
   truelat1 = 60.
   truelat2 = 30.
   truelat1 = sign*truelat1
   truelat2 = sign*truelat2
endif
if (truelat1 eq truelat2) then begin
   xn = sin(abs(truelat2)/conv)
endif else begin
   xn = alog10(cos(truelat1/conv)) - alog10(cos(truelat2/conv))
   xn = xn/(alog10(tan((45. - abs(truelat1)/2.)/conv)) - alog10(tan((45. - abs(truelat2)/2.)/conv)))
endelse

psi1 = 90. - abs(truelat1)
psi1 = psi1/conv
if (phic lt 0.) then begin
   psi1 = -psi1
   pole = -pole
endif
psi0 = (pole - phic)/conv
xc = 0.
yc = -a/xn*sin(psi1)*(tan(psi0/2.)/tan(psi1/2.))^xn
;c
;c-----Calculate lat/lon of the point (xloc,yloc)
;c
xloc = xloc + xc
yloc = yloc + yc
if (yloc eq 0.) then begin
   if (xloc ge 0.) then flp = 90./conv
   if (xloc lt 0.) then flp = -90./conv
endif else begin
   if (phic lt 0.) then flp = atan(xloc,yloc)  else flp = atan(xloc,-yloc)
endelse


flpp = (flp/xn)*conv + xlonc
if (flpp lt -180.) then flpp = flpp + 360.
if (flpp gt  180.) then flpp = flpp - 360.
xlon = flpp

r = sqrt(xloc*xloc + yloc*yloc)
if (phic lt 0.) then r = -r
if (truelat1 eq truelat2) then cell = r/(a*tan(psi1)) else cell = (r*xn)/(a*sin(psi1))

rxn  = 1.0/xn
cel1 = tan(psi1/2.)*cell^rxn
cel2 = atan(cel1)
psx  = 2.*cel2*conv
ylat = pole - psx
end


PRO distance, lat1,lon1,lat2,lon2, d

  r = 6.370e6
  cutpoint = 1.e-9
  degToRad = 0.017453293

  rlat1 = lat1 * degToRad
  rlon1 = -1*lon1 * degToRAD
  rlat2 = lat2 * degToRad
  rlon2 =-1*lon2 * degToRad

  dellat = (rlat2 - rlat1)
  dellon = (rlon2 - rlon1)
  if ( abs(dellat) lE cutpoint ) then d=0

  if ( abs(dellat) gt cutpoint ) then begin
      a = sin (dellat/2.) * sin(dellat/2.) + cos(rlat1)*cos(rlat2) $
        * sin(dellon/2.) * sin(dellon/2.)
      if (a lt 1.00) then begin
          c = 2.*asin(sqrt(a) )
      endif else if a ge 1.0 then begin
          c = 2. * asin(1.00)
      endif
  d = r * c   ; in meters
  endif

end

@ijll_lc.pro
@llij.pro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Smoke units: moles/hr/grid for gas and grams/hr/grid for particles
;based on Dennis, but I am pretty sure they are per km2
; wrf units: moles/hr/km2 for gas and ug/m2/sec for particles

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


input='4km'
wrfdomain='d02'

print,'WRF domain '+wrfdomain+' SMOKE:'+input


set_plot,'ps'
psfile='prep_emis_smoke.eps'
print, psfile
device,xs=19,ys=20,xoff=1,yoff=1,filename=psfile,/color
loadct,26
!p.multi=[0,2,3]
!p.charsize=1.7

answer=''
read, answer,prompt='Read SMOKE Emission data? '
if answer eq 'y' then begin

  dummy='  '
  if input eq '4km' then begin
     nx = 173
     ny = 128
     openr,1,'CAMx4km_points2_geo_clip.csv'
     lon_ref = fltarr(nx, ny)
     lat_ref = fltarr(nx,ny)
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
  endif
  if input eq '12km' then begin
     openr,1,'./Code_old/CAMxDomain_latlong.csv'
     nx = 239
     ny = 206
     lon_ref = fltarr(nx, ny)
     lat_ref = fltarr(nx,ny)
     readf,1,dummy
     for i=0,nx-1 do begin
        for j=0,ny-1 do begin
           readf,1,dummy
           parts = str_sep(dummy,',')
           lat_ref[i,j] = parts[2]
           lon_ref[i,j] = parts[3]
        endfor
     endfor
     close,1  
  endif
 
   
  lon_data = fltarr(nx, ny)     ;with this I differ up to 2 km from what Christine has 
  lat_data = fltarr(nx,ny)
  if input eq '4km' then begin
;     clon =  lon_ref[0]
;     clat = lat_ref[0]
     truelat1 = 33.
     truelat2 = 45.
     std_lon = -97.
     dx = 4.0
     knowni = 0.
     knownj = 0.
     xloc = -1084.    ;km
     yloc = -328.    ;km
  endif
  if input eq '12km' then begin
 ;    clon =  -111.279           ;-121.391L
 ;    clat = 41.4928              ;28.6031L
     truelat1 = 33.
     truelat2 = 45.
     std_lon = -97.
     dx = 12.
     knowni= 100.5              ;nx/2. 
     knownj =100.5              ;ny/2.
     xloc = -2388.              ;km
     yloc = -948.               ;km
  endif

  for i=0,nx-1 do begin
     for j=0,ny-1 do begin
        ijll_lc_camx, xloc+i*dx+0.5*dx, yloc+j*dx+0.5*dx, a1, a2,  truelat1, truelat2, 1, std_lon,clat, clon, knowni, knownj, dx
        lon_data[i,j]  = a2
        lat_data[i, j] = a1
     endfor
  endfor

stop
;split cells into subcells:
       
lonnei = fltarr(40000000)
latnei = fltarr(40000000)
count = 0L 
nbins = 16   ;4   ; = nbrow*nbrow
nbrow = 4    ;2

;print,'TEST!!!!!!!!!!'
;nbins=1
;nbrow=1

for ix = 0, nx - 1 do begin
   for jx = 0, ny - 1 do begin
      ict = 0L
      for ic = 0, nbrow-1 do begin
         for jc = 0, nbrow-1 do begin
            x1 = float(ic)/float(nbrow) + float(1./(2.*nbrow))
            y1 = float(jc)/float(nbrow) + float(1./(2.*nbrow))
            ijll_lc_camx, xloc+1*dx+ix*dx+x1*dx, yloc-0.*dx+jx*dx+y1*dx, a1, a2,  truelat1, truelat2, 1, std_lon,clat, clon, knowni, knownj, dx
            lonnei[count+ict] = a2
            latnei[count+ict] = a1 
            ict = ict + 1L
         endfor
      endfor
             
      count = count + nbins
   endfor
endfor


ncount = count
latnei = extrac(latnei,0,ncount)
lonnei = extrac(lonnei,0,ncount)
wrfi = intarr(ncount)
wrfj = intarr(ncount)

if input eq '4km' then nnames=33
if input eq '4km' then openr,1,'./Code_Dennis/agts_l.ar.20080612.1.color04.2018_CO.uam.asc'
if input eq '12km' then nnames=36
if input eq '12km' then openr,1,'./emis_2008/agts_l.20080820.1.westus12.2008_CO.ascii'


  for i=0,3 do readf,1,dummy
   names  = strarr(nnames)
   for i=0,nnames-1 do begin
      readf,1,dummy
      names[i] = strcompress(dummy,/remove_all)
   endfor
   emis = fltarr(nnames,24, nx,ny)
   field = fltarr(nx,ny)
   
   for ihr=0,23 do begin
      readf,1,dummy
      for i=0,nnames-1 do begin 
         readf,1, dummy
         readf,1, field
         emis[i,ihr,*,*] = (field)
      endfor
   endfor
   
   
   close,1
endif

if wrfdomain eq 'd01' then id=ncdf_open('wrfinput_d01')
if wrfdomain eq 'd02' then id=ncdf_open('wrfinput_d02')
ncdf_attget,id,'MOAD_CEN_LAT',moad_cen_lat,/global
ncdf_attget,id,'TRUELAT1',wrftruelat1,/global
ncdf_attget,id,'TRUELAT2',wrftruelat2,/global
ncdf_attget,id,'STAND_LON',wrfstdlon,/global
ncdf_attget,id,'WEST-EAST_GRID_DIMENSION',wrf_ix,/global    & wrf_ix=wrf_ix-1
ncdf_attget,id,'SOUTH-NORTH_GRID_DIMENSION',wrf_jx,/global  & wrf_jx=wrf_jx-1
ncdf_attget,id,'DX',wrfdx,/global & wrfdx=wrfdx/1000.
ncdf_attget,id,'CEN_LAT',wrflat1,/global
ncdf_attget,id,'CEN_LON',wrflon1,/global
ncdf_varget,id,'XLONG',wrflon
ncdf_varget,id,'XLAT',wrflat
ncdf_close,id

knowni = (wrf_ix+1)/2.
knownj = (wrf_jx+1)/2.   


; do area emission assignment to WRF grid here

dist = fltarr(ncount)-999.  ; distance in meters between NEI cell center and WRF cell center
openw,1,'NEIarea_WRFassign.dat'
printf,1,ncount
d=-99
for i = 0L, ncount-1 do begin
  ;  note that for d02 I cannot use the center coordinates. 
     llij_lc, latnei[i],lonnei[i],xi, yj, wrftruelat1, wrftruelat2, 1, wrfstdlon,wrflat[0,0], wrflon[0,0], 0.,0., wrfdx
     printf,1, latnei[i], lonnei[i], xi, yj
     wrfi[i] = round(xi)
     wrfj[i] = round(yj)
     if wrfi[i] lt wrf_ix and wrfj[i] lt wrf_jx  and wrfi[i] gt 0 and wrfj[i] gt 0 then $
        distance,latnei[i],lonnei[i],wrflat[wrfi[i],wrfj[i]], wrflon[wrfi[i],wrfj[i]],  d
     dist[i] = d
  endfor
close,1
print, 'Number of grid cells used from NEI area: ', ncount


; assign emissions and create output files

for hrloop = 0,0 do begin   

   hr = strcompress(string(hrloop),/remove_all)
   if hrloop le 9 then hr='0'+hr
   
   file_emi_in = './wrf_emissions_NEI2005/wrfchemi_'+wrfdomain+'_2011-07-21_'+hr+':00:00'
   file_emi_out = './wrf_emissions_out/'+ file_basename(file_emi_in)

   spawn,'cp '+file_emi_in+'   '+file_emi_out
   print, 'cp '+file_emi_in+'   '+file_emi_out
   
   
    for ispec = 0,0  do begin

       scale=1
       ismoke_add = -1
       
; scalefactors are taken from 2008 emissions
; !!!!!!!!!! cvheck!!!!!!!!!!!!!!!!!!
       case ispec of
          0: begin
             wrfspec='CO'
             ismoke = where(names eq 'CO')
             smokespec  = names[ismoke[0]]
          end
          1: begin
             wrfspec='NO'
             ismoke = where(names eq 'NO')
             smokespec  = names[ismoke[0]]
             scale  = 46./30    ; adjust for mw
          end
          2: begin
             wrfspec='NO2'
             ismoke = where(names eq 'NO2')
             smokespec  = names[ismoke[0]]
          end
          3: begin
             wrfspec='BIGALK'
             ismoke = where(names eq 'PAR')
             smokespec  = names[ismoke[0]]
             scale = 14./72/5.
          end
          4: begin
             wrfspec='C10H16'
             ismoke = where(names eq 'TERP')
             smokespec  = names[ismoke[0]]
          end
          5:begin
             wrfspec='C2H4'
             ismoke = where(names eq 'ETH')
             smokespec  = names[ismoke[0]]
          end   
          6:begin
             wrfspec='C2H5OH'
             ismoke = where(names eq 'ETOH')
             smokespec  = names[ismoke[0]]
             scale = 32./46
          end   
          7:begin
             wrfspec='C2H6'
             ismoke = where(names eq 'ETHA')
             smokespec  = names[ismoke[0]]
             scale = 30./34
          end   
          8:begin
             wrfspec='C3H6'
             ismoke = where(names eq 'OLE')
             smokespec  = names[ismoke[0]]
             scale = 27./42
          end         
          9:begin
             wrfspec='C3H8'
             ismoke = where(names eq 'PRPA')
             smokespec  = names[ismoke[0]]
          end   
          10:begin
             wrfspec='CH2O'
             ismoke = where(names eq 'FORM')
             smokespec  = names[ismoke[0]]
          end   
          11:begin
             wrfspec='CH3CHO'
             ismoke = where(names eq 'ALD2')
             smokespec  = names[ismoke[0]]
          end   
          12:begin
             wrfspec='CH3COCH3'
             ismoke = where(names eq 'ACET')
             smokespec  = names[ismoke[0]]
          end   
          13:begin
             wrfspec='CH3OH'
             ismoke = where(names eq 'MEOH')
             smokespec  = names[ismoke[0]]
          end   
          14:begin
             wrfspec='MEK'
             ismoke = where(names eq 'KET')
             smokespec  = names[ismoke[0]]
          end   
          15:begin
             wrfspec='SO2'
             ismoke = where(names eq 'SO2')
             smokespec  = names[ismoke[0]]
          end   
          16:begin
             wrfspec='TOLUENE'
             ismoke = where(names eq 'TOL') ; also add xyl and benz
             smokespec  = names[ismoke[0]]
             ismoke_add = [where(names eq 'XYL'), where(names eq 'BENZ')]
          end   
          17:begin
             wrfspec='OC'
             ismoke = where(names eq 'POC') 
             smokespec  = names[ismoke[0]]
             scale= 1./3600.
          end   
          18:begin
             wrfspec='BC'
             ismoke = where(names eq 'PEC') 
             smokespec  = names[ismoke[0]]
             scale= 1./3600.
          end   
          19:begin
             wrfspec='sulf'
             ismoke = where(names eq 'PSO4') 
             smokespec  = names[ismoke[0]]
             scale= 1./3600.
          end   
          20:begin
             wrfspec='PM_10'
             ismoke = where(names eq 'PMC') 
             smokespec  = names[ismoke[0]]
             scale= 0.0099/3600.
          end   
          21:begin
             wrfspec='PM_25'
             ismoke = where(names eq 'PMFINE') 
             smokespec  = names[ismoke[0]]
             scale= 0.02/3600.
          end   
       endcase
       
       ;fac = 1./(dx*dx) *( nbins*nbins) * scale ;  need to divide by grid area ???
fac = 1.*scale/(dx*dx)   ; this should be it, gives same results no matter how many subcells
fac = 1.*scale   ; this should be it, gives same results no matter how many subcells


       if ismoke lt 0 then print, 'No SMOKE input for '+wrfspec
       
       if ismoke[0] ge 0 then begin

          wrfemis = fltarr(wrf_ix, wrf_jx)
          wrfemis_n=fltarr(wrf_ix, wrf_jx)
          
          count=0L
          
          emisdata  = reform( emis[ismoke,hrloop,*,*]*fac)
          if ismoke_add[0] ge 0 then begin
             for iadd=0,n_elements(ismoke_add)-1 do $
                emisdata = emisdata + reform( emis[ismoke_add[iadd],hrloop,*,*]*fac)
             for iadd=0,n_elements(ismoke_add)-1 do print, names[ismoke_add[iadd]]
          endif

          for ix = 0, nx - 1 do begin
             for jx = 0, ny - 1 do begin
                
; idl index starts at 0, grid index at 1!!!        
                
                for innerloop = 0, nbins-1 do begin
                   ii = wrfi[count]-1
                   jj = wrfj[count]-1
                   if ii ge 0 and jj ge 0 and ii lt wrf_ix and jj lt wrf_jx then begin
                      wrfemis[ii,jj] =  wrfemis[ii,jj] + emisdata[ix,jx]
                      wrfemis_n[ii,jj] =  wrfemis_n[ii,jj] +1L
                   endif 
                   count = count+1L
                endfor
             endfor
          endfor
; average:
          for ix=0,wrf_ix-1 do begin
             for jx=0,wrf_jx-1 do begin
                if wrfemis_n[ix,jx] gt 0 then $
      ;             wrfemis[ix,jx] = wrfemis[ix,jx] / (wrfdx*wrfdx)  ;/wrfemis_n[ix,jx]
                   wrfemis[ix,jx] = wrfemis[ix,jx] /wrfemis_n[ix,jx]
             endfor
          endfor
          
          id2=ncdf_open(file_emi_in)
          ncdf_varget,id2,'E_'+wrfspec,wrfemis_old
          wrfemis_new = wrfemis_old & wrfemis_new[*] = 0.
          wrfemis_old = total(wrfemis_old,3)
          ncdf_close,id2

          wrfemis_new[*,*,0] = wrfemis

          id=ncdf_open(file_emi_out,/write)
          ncdf_varput,id,'E_'+wrfspec,wrfemis_new
          ncdf_close,id
          kpos=where(wrfemis gt 0)

          smoketotal= total( emis[ismoke,hrloop,*,*])*scale
          print,wrfspec,' ',hrloop, 1./scale, total( emis[ismoke,hrloop,*,*]), total(wrfemis)*wrfdx*wrfdx,  $
                    smoketotal/(total(wrfemis)*wrfdx*wrfdx), total(wrfemis_old[kpos])/total(wrfemis[kpos])
          
; plot emission maps for hour XX
          limit=[36.5,-109.3,41.5, -101.7]
          m1=min(wrfemis(where(wrfemis gt 0)))
          m2=max(wrfemis)
          nlevs=30
          levs=findgen(nlevs)*(m2-m1)/nlevs+m1 
          levs = reverse(m2-alog10(findgen(nlevs)+1)/alog10(nlevs)*(m2-m1))
          colors=findgen(nlevs)/(nlevs-1)*233+20
          
          map_set,limit=limit, tit='WRF-Chem '+wrfspec,/advance,/noerase
          for ix=0,wrf_ix-2 do begin
             for jx=0,wrf_jx-2 do begin
                if wrfemis[ix,jx] gt 0 then begin
                   icol = where(abs(wrfemis[ix,jx]-levs) eq min(abs(wrfemis[ix,jx]-levs)))
                   x1=[wrflon[ix, jx],wrflon[ix+1,jx], wrflon[ix+1,jx+1], wrflon[ix,jx+1]]
                   x2=[wrflat[ix, jx],wrflat[ix+1,jx], wrflat[ix+1,jx+1], wrflat[ix,jx+1]]
                   polyfill, x1, x2, color=colors[icol[0]],/fill
                endif
             endfor
          endfor
          map_continents,/us,/coast,/hires
          
          map_set,limit=limit, tit='SMOKE '+smokespec,/advance,/noerase
          for ix=0,nx-2 do begin
             for jx=0,ny-2 do begin
                if emisdata[ix,jx] gt 0 then begin
                   icol = where(abs(emisdata[ix,jx]-levs) eq min(abs(emisdata[ix,jx]-levs)))
                   x1=[lon_data[ix, jx],lon_data[ix+1,jx], lon_data[ix+1,jx+1], lon_data[ix,jx+1]]
                   x2=[lat_data[ix, jx],lat_data[ix+1,jx], lat_data[ix+1,jx+1], lat_data[ix,jx+1]]
                   polyfill, x1, x2, color=colors[icol[0]],/fill
                endif
             endfor
          endfor
          map_continents,/us,/coast,/hires
       endif

    endfor
 endfor
device,/close
 set_plot,'x' & !p.multi=0

   
end
