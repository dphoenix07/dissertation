@ijll_lc.pro
@llij.pro

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

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;   MAIN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

answer=''
read, answer,prompt='Read SMOKE Emission data? '
if answer eq 'y' then begin
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
         lon_data[ i,j] = a2
         lat_data[i, j] = a1
      endfor
   endfor
   
; old code and dennis' code give same results

   openr,1,'./Code_Dennis/agts_l.ar.20080612.1.color04.2018_CO.uam.asc'
   for i=0,3 do readf,1,dummy
   names  = strarr(33)
   for i=0,33-1 do begin
      readf,1,dummy
      names[i] = dummy
   endfor
   emis = fltarr(33,24, nx,ny)
   field = fltarr(nx,ny)
   
   for ihr=0,23 do begin
      readf,1,dummy
      for i=0,33-1 do begin 
         readf,1, dummy
         readf,1, field
         emis[i,ihr,*,*] = (field)
      endfor
   endfor
   
   
   close,1
endif



; map on to WRF domain and add to wrfchemi already created with prepemis.pro
device,retain=2
map_set,/continents, limit=[38,-110,43,-95],/noerase,/us

read, answer, prompt='create wrfchemi? '
if answer eq 'y' then begin
   for itype=0,0 do begin
      for ihr =  13,13 do begin
         hr = strcompress(string(ihr),/remove_all)
         if ihr le 9 then hr='0'+hr
         
         case itype of        
            0: begin
               file_emi_in = 'wrfchemi_d02_2012-06-30_'+hr+':00:00'
               file_emi_out = 'wrfchemi_d02_2012-06-30_'+hr+':00:00_tracer'
            end
         endcase
         
         spawn,'cp '+file_emi_in+'   '+file_emi_out
         print, 'cp '+file_emi_in+'   '+file_emi_out
         
         
         id=ncdf_open('wrfinput_d01')
         ncdf_attget,id,'MOAD_CEN_LAT',moad_cen_lat,/global
         ncdf_attget,id,'TRUELAT1',truelat1,/global
         ncdf_attget,id,'TRUELAT2',truelat2,/global
         ncdf_attget,id,'STAND_LON',stdlon,/global
         ncdf_attget,id,'WEST-EAST_GRID_DIMENSION',wrf_ix,/global    & wrf_ix=wrf_ix-1
         ncdf_attget,id,'SOUTH-NORTH_GRID_DIMENSION',wrf_jx,/global  & wrf_jx=wrf_jx-1
         ncdf_attget,id,'DX',dx,/global & dx=dx/1000.
         ncdf_attget,id,'CEN_LAT',lat1,/global
         ncdf_attget,id,'CEN_LON',lon1,/global
         ncdf_varget,id,'XLONG',xlong
         ncdf_varget,id,'XLAT',xlat
         ncdf_close,id
         hemi=1
         
         for i=0,248*194.-1 do plots,xlong[i], xlat[i], psym=3
         for i=0,1.*nx*ny-1 do plots,lon_data[i], lat_data[i], psym=4, color=220
         
         
         specs = 'E_'+['CO','NO','BIGALK','C10H16','ISOP','C2H4','C2H5OH','C2H6','C3H6','C3H8','CH2O','CH3CHO','CH3COCH3','CH3OH','MEK','NO2','SO2','TOLUENE','OC','BC','sulf','PM_10','PM_25']
         index =       [0,    2,    20,     22,     15,      9,    12,       10,   19,    21,     13,    5,       4,        17,    16,   3,     -1,    -1,     30,  27,   31,     32,      28]
         scale =      [1., 46./30, 14./72/5., 1.,  1.,    28./28 , 32./46, 30./34, 27./42, 1, 30./30,   1 ,     1.,        1 ,   1,   1,      1,    92./92.,1./3600., 1./3600., 1./3600., 0.0099/3600., 0.02/3600.]
         
         conv = 1./smoke_area    ; for gas from mole/hr to mole/hr/km2

; in scale also correct for the mw difference (cdphe/mozart mw)
         nspecs = n_elements(specs)
         
         
         for ispec=0,nspecs-1 do begin
            spec=specs[ispec]
            print, spec, '  ',hr
            
            id=ncdf_open(file_emi_in)
            ncdf_varget,id,spec,emi_in
            ncdf_close,id
            if index[ispec] lt 0 then emi_data[*] = -999.
           if index[ispec] ge 0 then begin
               emi_data = (reform(emis[index[ispec],ihr,*,*]))
               print, spec, names[index[ispec]], scale[ispec], format='(2A12, F8.2)'
            endif
            if index[ispec] lt 0 and spec eq 'E_TOLUENE' then begin
               emi_data = (reform(emis[26,ihr,*,*] + emis[23,ihr,*,*] +emis[7,ihr,*,*]))
               print, spec, names[26], names[23], names[7], scale[ispec], format='(4A12, F8.2)'
            endif 
               
            ni = n_elements(lon_ref[*,0])
            nj = n_elements(lat_ref[0,*])
            ni_data = n_elements(xlong[*,0])
            nj_data = n_elements(xlong[0,*])
            nz = n_elements(emi_in[0,0,*])
            emi_out = emi_in    ;fltarr(ni, nj, nz)
            n_emi_out =  fltarr(ni, nj, nz)
            
            if ispec eq 0 then begin
               ; find mapping indices for domain
               read, answer, prompt='Calculate mapping? if n then read from file '
               if answer eq 'y' then begin
                  iref_index = fltarr(ni, nj)  ; for each SMOKE grid get index for WRF grid
                  jref_index = fltarr(ni, nj)
                  d_index = fltarr(ni, nj)-999.
                  for i=0,ni-1 do begin
                     for j=0,nj-1 do begin
                        llij_lc, lat_ref[i,j], lon_ref[i,j],xi,yj, truelat1, truelat2,1, stdlon,xlat[0,0], xlong[0,0],0.,0., dx     
                        i2 = round(xi)
                        j2 = round(yj)
                        distance,lat_ref[i,j], lon_ref[i,j], xlat[i2,j2], xlong[i2, j2], d
                        
                        iref_index[i,j] = i2
                        jref_index[i,j] = j2
                        d_index[i,j] = d
                     endfor
                  endfor
                  openw,1,'Smoke4km_WRF_mapping.dat'
                  printf,1,iref_index
                  printf,1,jref_index
                  printf,1,d_index
                  close,1
               endif
               
               if answer ne 'y' then begin
                  iref_index = fltarr(ni, nj)
                  jref_index = fltarr(ni, nj)
                  d_index = fltarr(ni, nj)-999.
                  openr,1,'Smoke4km_WRF_mapping.dat'
                  readf,1,iref_index
                  readf,1,jref_index
                  readf,1,d_index
                  close,1
               endif
            endif

            emi_out = emi_in & emi_out[*] =0
            emi_cover = emi_in[*,*,0] & emi_cover[*] = 0
            
            for  i=0,ni-1 do begin
               for j=0,nj-1 do begin
                  emi_out[iref_index[i,j], jref_index[i,j],0] = emi_data[i,j]
                  emi_cover[iref_index[i,j], jref_index[i,j]] =  emi_cover[iref_index[i,j], jref_index[i,j]]+1
                  plots,xlong[iref_index[i,j], jref_index[i,j]], xlat[iref_index[i,j], jref_index[i,j]], psym=1,color=220
               endfor
            endfor
            k=where(emi_cover gt 0)
            emi_out[k]=emi_out[k]/emi_cover[k]
                
               id=ncdf_open(file_emi_out,/write)
               ncdf_varput,id,spec,emi_out
               ncdf_close,id
            


         endfor
         
      endfor
   endfor
endif












end
