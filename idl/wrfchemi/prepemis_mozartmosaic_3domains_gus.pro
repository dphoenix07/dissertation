; this program splits the NEI 4km emissions into 1 km bins
; write the results to files <species>.subset (24 hours of data)
; lat and lon values are written to separate files
; units are converted to WRF units
; creates emissions files for one day, 24 hrs
; Jan 2009: adapt to MOZART chemical mechanism
; Aerosols:
; Feb 2009: changed the way the pixels are summed/averaged. Now get
; first units for EPA per unit area, then sum up and divide by actual
; number of pixels that went into the sum. Results
; are similar to the former way just slight change in spatial
; distribution. 
; Feb 2009: adjusted to assignment John did
; Feb 2009: downloaded NEI05_v3
; March 2009: added aerosol speciation for MADE-Sorgam/GOCART
; Feb 2010: aerosol speciationfor GOCART
; Jun 2010: added emissions for CO Tracers
; Dec 2010: add emissions XNO2 and XNO
; March 2011: corrected for center coordinate issue
; Sep 2013: add MOZAIC aerosol species
; Apr 2013: adapt for NEI 2011 emissions


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
  d = r * c
  endif

end

@area_emis.pro   ; read in EPA area emissions
@mapcf.pro       ; calculate lat/lon from EPA grid
@ijll_lc.pro     ; WRF lambert projection; calculate lat/lon from grid index
@llij_lc.pro     ; WRF lambert projection; calculate i/j from lat/lon
@point_emis.pro  ; read in EPA point emissions
@create_netcdffile_mozmam.pro   ; create empty wrfchemi files

;==============================
domain = 'd01'
wrflevs = 11

wrfheight=[0.0,    54.1,   118.9,   194.6,   281.0,   378.3,   486.3,   616.0,   767.4,   940.4,  1135.0]   ;WRFV3.3_Jul2011

; get domain information
;id=ncdf_open('/glade/p/work/barthm/0622-1km.inputfiles/wrfinput_'+domain)
id=ncdf_open('/glade/u/home/cuchiara/simulations/SEAC4RS_Sep2_wks02/wrfinput_'+domain)

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

knowni = (wrf_ix+1)/2.
knownj = (wrf_jx+1)/2.   
;
;nei_path = '/ur/pfister/emis_NEI05_v4/Sat/'
;output_path = '/data/pfister/FRAPPE/wrf_emissions_NEI2005/Sat_NEI2005/'

;nei_path = '/ur/pfister/emis_NEI05_v4/Sun/';
;output_path = '/data/pfister/FRAPPE/wrf_emissions_NEI2005/Sun_NEI2005/'

;nei_path = '/ur/pfister/emis_NEI05_v4/Weekday/'
;output_path = '/data/pfister/FRAPPE/wrf_emissions_NEI2005/Weekday_NEI2005/'

nei_path = '/glade/p/work/barthm/WRFemis_NEI2011/'
;output_path = '/glade/p/work/barthm/WRFemis_NEI2011/DC3/'
output_path = '/glade/scratch/cuchiara/'

print,'should sulf be in aerosol units????'
;stop

;ipoint=168516L        ; NEI2005
ipoint=224097L 


;==============================
reflat = fltarr(wrf_ix, wrf_jx)
reflon = fltarr(wrf_ix, wrf_jx)
ll_lon = fltarr(wrf_ix, wrf_jx)
ll_lat = fltarr(wrf_ix, wrf_jx)
for i=0,wrf_ix-1 do begin
    for j=0,wrf_jx-1 do begin
        ijll_lc, i+1,j+1, a, b, truelat1, truelat2, $
          hemi, stdlon,lat1, lon1, knowni, knownj, dx
        reflat[i,j] = a
        reflon[i,j] = b
         ijll_lc, i+0.5,j+0.5, a, b, truelat1, truelat2, $
          hemi, stdlon,lat1, lon1, knowni, knownj, dx
        ll_lat[i,j] = a
        ll_lon[i,j] = b
    endfor
endfor



;create empty netcdf files
yyyy = '2013'
mm = '09'
dd = '01'
hrs = strcompress(string(indgen(24)),/remove_all)
hrs[0:9] = '0'+hrs[0:9]
timestring = yyyy+'-'+mm+'-'+dd+'_'+hrs+':00:00'
ncfiles = output_path+'wrfchemi_'+domain+'_'+timestring

for i=12,23 do $
;for i=0,11 do $
;for i=0,0 do $
create_netcdffile_gocart, ncfiles[i], timestring[i], wrflevs, wrf_ix, wrf_jx, dx*1e3, dx*1e3, $
  lat1, lon1, truelat1, truelat2, moad_cen_lat, 1, nei_path


; NEI species:
specnames = ['CO','NOX','SO2','NH3', $
        'HC01', 'HC02', 'HC03', 'HC04', 'HC05', $       ; see readme text for mapping of VOCs
      	'HC06', 'HC07', 'HC08', 'HC09', 'HC10', $
      	'HC11', 'HC12', 'HC13', 'HC14', 'HC15', $
      	'HC16', 'HC17', 'HC18', 'HC19', 'HC20', $
      	'HC21', 'HC22', 'HC23', 'HC24', 'HC25', $
      	'HC26', 'HC27', 'HC28', 'HC29', 'HC30', $
      	'HC31', 'HC32', 'HC33', 'HC34', 'HC35', $
      	'HC36', 'HC37', 'HC38', 'HC39', 'HC40', $
      	'HC41', 'HC42', 'HC43', 'HC44', 'HC45', $
      	'HC46', 'HC47', 'HC48', 'HC49', 'HC50', $
        'PM02', 'PM03', 'PM01', $
      	'PM04', 'PM05', 'PM10-PRI']
; HC50 = specnames(53)
;1) PMFINE - unspeciated primary PM2.5       specnames(56)
;2) PSO4   - PM2.5 sulfate                   specnames(54)
;3) PNO3   - PM2.5 nitrate                   specnames(55)
;4) POC    - PM2.5 organic carbon            specnames(57)
;5) PEC    - PM2.5 elemental carbon          specnames(58)
;6) PNCOM  - PM2.5 Non-Carbon organic mass
;7) PNH4   - PM2.5 ammonium                  

nspec = n_elements(specnames)
molweight=fltarr(nspec)
molweight[0:3] = [28.0103989,46.0055389, 64.065, 17.031 ]
molweight[54] = 96. ; so4
; rough domain borders (to limit the amount of NEI data to deal with)
latmin = 0
latmax = 75
lonmin = -140
lonmax = -30

; dimensions of NEI grid
nei_ix = 1332
nei_jx = 1008


ihr = ['HR01','HR02','HR03','HR04','HR05','HR06','HR07','HR08','HR09','HR10','HR11','HR12', $
       'HR13','HR14','HR15','HR16','HR17','HR18','HR19','HR20','HR21','HR22','HR23','HR24']


AREA_EMIS, nei_path+'/area4k/HR01/', 'CO', areadata, ix, jx, arealat, arealon
; do that to get arealat and arealon - centers of the 4 x 4 km NEI grid


lonnei = fltarr(40000000)
latnei = fltarr(40000000)

count = 0L 
nbins = 16   ;4
nbrow = 4    ;2
 
for ix = 0, nei_ix - 1 do begin
    for jx = 0, nei_jx - 1 do begin

        if arealat[ix,jx] gt latmin and arealat[ix,jx] le latmax and $
           arealon[ix,jx] gt lonmin and arealon[ix,jx] le lonmax then begin

 ;mcb
            ict = 0L
            for ic = 0, nbrow-1 do begin
             for jc = 0, nbrow-1 do begin
               ict = ict + 1L
               x1 = float(ic)/float(nbrow) + float(1./(2.*nbrow))
               y1 = float(jc)/float(nbrow) + float(1./(2.*nbrow))
               mapcf, ix+1.+x1, jx+1.+y1, latsub, lonsub
               lonnei[count+ict] = lonsub
               latnei[count+ict] = latsub
             endfor
            endfor
             
            count = count + nbins
;mcb
     

        endif
    endfor
endfor

ncount = count

latnei = extrac(latnei,0,ncount)
lonnei = extrac(lonnei,0,ncount)
wrfi = intarr(ncount)
wrfj = intarr(ncount)


; do area emission assignment to WRF grid here
dist = fltarr(ncount)-999.  ; distance in meters between NEI cell center and WRF cell center
openw,1,'NEIarea_WRFassign_'+domain+'.dat'
printf,1,ncount
d=-99
for i = 0L, ncount-1 do begin
     llij_lc, latnei[i],lonnei[i],xi, yj, truelat1, truelat2, hemi, stdlon,lat1, lon1, knowni, knownj, dx
     printf,1, latnei[i], lonnei[i], xi, yj
     wrfi[i] = fix(xi)
     wrfj[i] = fix(yj)
     if fix(xi) le wrf_ix and fix(yj) le wrf_jx and fix(xi) gt 0 and fix(yj) gt 0 then $
        distance,latnei[i],lonnei[i],reflat[fix(xi)-1, fix(yj)-1], reflon[fix(xi)-1, fix(yj)-1],  d
     dist[i] = d
endfor
close,1
print, 'Number of grid cells used from NEI area: ', ncount

; do point emission assignment to WRF grid here

infofile  = nei_path+'/point/Relpnt_info.txt' ; point release info file
point_emis_info, primpath, infofile, pointlat, pointlon, stackD, stackH, ipoint, reltype

wrfipoint = fltarr(ipoint)
wrfjpoint = fltarr(ipoint)
kpoint = fltarr(ipoint)
dist_point = fltarr(ipoint)
openw,1,'NEIpoint_WRFassign_'+domain+'.dat'

for i = 0L, ipoint-1L do begin
    llij_lc, pointlat[i],pointlon[i],xi, yj, truelat1, truelat2, hemi, stdlon,lat1, lon1, knowni, knownj, dx
    wrfipoint[i] = fix(xi)
    wrfjpoint[i] = fix(yj)
    k = where(wrfheight le stackH[i], nk)
    kpoint[i] = k[nk-1]
    printf,1, pointlat[i],pointlon[i], stackh[i], wrfipoint[i], wrfjpoint[i], kpoint[i]
    if fix(xi) le wrf_ix and fix(yj) le wrf_jx  and fix(xi) gt 0 and fix(yj) gt 0 then $
        distance,pointlat[i],pointlon[i],reflat[fix(xi)-1, fix(yj)-1], reflon[fix(xi)-1, fix(yj)-1],  d
     dist_point[i] = d
endfor
close,1


;for hrloop = 12,23 do begin
;for hrloop = 0,0 do begin
for hrloop = 0,11 do begin

    print, 'Hour: ', ihr(hrloop)

    wrfemis = fltarr(nspec, wrf_ix, wrf_jx, wrflevs)
    wrfemis_n = intarr(nspec, wrf_ix, wrf_jx, wrflevs)


    for ispec = 0, nspec-1 do begin

        areapath = nei_path+'/area4k/'+ihr[hrloop]+'/'
        areadata = fltarr(nei_ix, nei_jx)
        openr,1,areapath + specnames[ispec]
        readf,1,areadata
        close,1

; convert primary emissions from short tons/hr/gridcell  to
; moles/hr/km2 for primary, from moles/hr/gridcell to moles/hr/km2 for
; VOC and from short tons/hr/gridcell  to micrograms/m2/sec for
; particulates
; 1 short ton = 907.185 kg = 907185 g = 9.07185 ug

        if ispec le 3 or ispec eq 54 then begin
            print, 'area  Primary: ', specnames[ispec]
            fac = 9.07184e5/molweight(ispec) ;/(dx*dx)
        endif else if ispec gt 3 and ispec le 53 then begin
            print, 'area  VOC: ', specnames[ispec]
            fac = 1.;/(dx*dx)
        endif else if ispec gt 54 then begin
            print, 'area  Particulate: ', specnames[ispec]
            fac = 9.07184e11/3600./ (1e3*1e3) ;(dx*dx*1e3*1e3)
        endif

        areadata = areadata*fac/nbins  ; normalize to km-2
; split into nbins subcells and assign to WRF grid

        count=0L
        for ix = 0, nei_ix - 1 do begin
            for jx = 0, nei_jx - 1 do begin
                if arealat[ix,jx] gt latmin and arealat[ix,jx] le latmax and $
                   arealon[ix,jx] gt lonmin and arealon[ix,jx] le lonmax then begin

; idl index starts at 0, grid index at 1!!!
                    
                    emis  = areadata[ix,jx]
                    
                    for innerloop = 0, nbins-1 do begin
                        ii = wrfi[count]-1
                        jj = wrfj[count]-1
                        if ii ge 0 and jj ge 0 and ii lt wrf_ix and jj lt wrf_jx then begin
                            wrfemis[ispec,ii,jj,0] =  wrfemis[ispec,ii,jj,0] + emis
                            wrfemis_n[ispec,ii,jj,0] =  wrfemis_n[ispec,ii,jj,0] +1L
                        endif 
                        count = count+1L
                    endfor
                endif
            endfor
        endfor


; average:
        for ix=0,wrf_ix-1 do begin
            for jx=0,wrf_jx-1 do begin
                if wrfemis_n[ispec,ix,jx,0] gt 0 then $
                  wrfemis[ispec,ix,jx,0]=wrfemis[ispec,ix,jx,0]/wrfemis_n[ispec,ix,jx,0]
            endfor
        endfor

; get point data, units like area emissions but in rates per point source
; should include plume rise calculation!

        pointpath = nei_path+'/point/'+ihr[hrloop]+'/'
        POINT_EMIS, pointpath, specnames[ispec], pointdata, ipoint

        if ispec le 3 or ispec eq 54 then begin
            print, 'point Primary: ', specnames[ispec]
            fac = 9.07184e5/molweight(ispec)/(dx*dx)
        endif else if ispec gt 3 and ispec le 53 then begin
            print, 'point VOC: ', specnames[ispec]
            fac = 1./(dx*dx)
        endif else if ispec ge 55 then begin
            print, 'point Particulate: ', specnames[ispec]
            fac = 9.07184e11/3600./(dx*dx*1e3*1e3)
        endif
        pointdata = pointdata*fac

        for i=0L,ipoint-1L do begin
            ii = wrfipoint[i]-1
            jj = wrfjpoint[i]-1
            if ii ge 0 and jj ge 0 and ii lt wrf_ix and jj lt wrf_jx then begin
                wrfemis[ispec,ii,jj,kpoint[i]] = wrfemis[ispec,ii,jj,kpoint[i]] + pointdata[i]
            endif
        endfor

    endfor                      ; end of species loop



; do the speciation and write to netcdf  files

    ncid = ncdf_open(ncfiles[hrloop],/write)
    print,'Writing to ',ncfiles[hrloop]

; primary species
    zerofield=reform(wrfemis[0,*,*,*]) & zerofield[*] = 0.

    ncdf_varput,ncid,'E_CO',reform(wrfemis[0,*,*,*])
    ncdf_varput,ncid,'E_CO02',reform(wrfemis[0,*,*,*])
    ncdf_varput,ncid,'E_CO03',zerofield
    ncdf_varput,ncid,'E_CO_A',reform(wrfemis[0,*,*,*])
    ncdf_varput,ncid,'E_CO03',zerofield
    ncdf_varput,ncid,'E_CO_BB',zerofield

    ncdf_varput,ncid,'E_NO',reform(wrfemis[1,*,*,*])*0.9
    ncdf_varput,ncid,'E_NO2',reform(wrfemis[1,*,*,*])*0.1
    ncdf_varput,ncid,'E_XNO',reform(wrfemis[1,*,*,*])*0.9
    ncdf_varput,ncid,'E_XNO2',reform(wrfemis[1,*,*,*])*0.1
    ncdf_varput,ncid,'E_SO2',reform(wrfemis[2,*,*,*])
    ncdf_varput,ncid,'E_NH3',reform(wrfemis[3,*,*,*])

; VOCs   

; BIGALK =
;;;   tmp = wrfemis[7,*,*,*]+wrfemis[8,*,*,*]+wrfemis[9,*,*,*]+wrfemis[23,*,*,*]+wrfemis[42,*,*,*]+wrfemis[43,*,*,*]
   tmp = wrfemis[7,*,*,*]+wrfemis[8,*,*,*]+wrfemis[9,*,*,*] +wrfemis[42,*,*,*] +wrfemis[43,*,*,*] + wrfemis[52,*,*,*]
   ncdf_varput,ncid,'E_BIGALK',reform(tmp)

; BIGENE = 
;;;   ncdf_varput,ncid,'E_BIGENE', reform(wrfemis[12,*,*,*]+wrfemis[19,*,*,*]+wrfemis[49,*,*,*]+wrfemis[50,*,*,*])
   ncdf_varput,ncid,'E_BIGENE', reform(wrfemis[12,*,*,*] +wrfemis[49,*,*,*]+wrfemis[50,*,*,*])

; C2H4 = ethylene
   ncdf_varput,ncid,'E_C2H4', reform(wrfemis[10,*,*,*]) 

; C2H5OH = ethanol
   ncdf_varput,ncid,'E_C2H5OH',reform(wrfemis[51,*,*,*])           ; Ethanol is now in NEI 2011

; C2H6 = Alk1
   ncdf_varput,ncid,'E_C2H6',reform(wrfemis[5,*,*,*]) 

; C3H6 = propylene
    ncdf_varput,ncid,'E_C3H6',reform(wrfemis[11,*,*,*]+wrfemis[39,*,*,*]) 

; C3H8 = propane
     ncdf_varput,ncid,'E_C3H8',reform(wrfemis[6,*,*,*] + wrfemis[48,*,*,*] ) 

; CH2O = formaldehyde
     ncdf_varput,ncid,'E_CH2O',reform(wrfemis[17,*,*,*]) 

; CH3CHO = acetaldehyde
     ncdf_varput,ncid,'E_CH3CHO',reform(wrfemis[18,*,*,*]+wrfemis[19,*,*,*])

;CH3COCH3 = acetone
     ncdf_varput,ncid,'E_CH3COCH3',reform(wrfemis[21,*,*,*])

;CH3OH = methanol
     ncdf_varput,ncid,'E_CH3OH',reform(wrfemis[24,*,*,*])

; MEK=  methyl-ethyle-ketone
     ncdf_varput,ncid,'E_MEK',reform(wrfemis[22,*,*,*] + wrfemis[23,*,*,*] )

; TOLUENE
;;;     ncdf_varput,ncid,'E_BENZENE',reform(wrfemis[15,*,*,*]+wrfemis[16,*,*,*]+wrfemis[20,*,*,*]+wrfemis[41,*,*,*] +$
;;;                                         wrfemis[44,*,*,*]+wrfemis[45,*,*,*]+wrfemis[46,*,*,*]+wrfemis[47,*,*,*] +$
;;;                                         wrfemis[25,*,*,*]+wrfemis[26,*,*,*]+wrfemis[28,*,*,*]+wrfemis[29,*,*,*])
     ncdf_varput,ncid,'E_TOLUENE',reform(wrfemis[15,*,*,*] + wrfemis[44,*,*,*] )

; BENZENE
     ncdf_varput,ncid,'E_BENZENE',reform(wrfemis[41,*,*,*] )

; XYLENE
     ncdf_varput,ncid,'E_XYLENE',reform(wrfemis[16,*,*,*]+ wrfemis[45,*,*,*]+wrfemis[46,*,*,*]+wrfemis[47,*,*,*] )

; ISOP
     ncdf_varput,ncid,'E_ISOP',reform(wrfemis[13,*,*,*] )

; C10H16
     ncdf_varput,ncid,'E_C10H16',reform(wrfemis[14,*,*,*])

; C2H2
     ncdf_varput,ncid,'E_C2H2',reform(wrfemis[40,*,*,*])

; GLYOXAL
     ncdf_varput,ncid,'E_GLY',reform(wrfemis[25,*,*,*])

; MACR
     ncdf_varput,ncid,'E_MACR',reform(wrfemis[30,*,*,*])

; MGLY
     ncdf_varput,ncid,'E_MGLY',reform(wrfemis[26,*,*,*] + wrfemis[27,*,*,*]  )

; MVK
     ncdf_varput,ncid,'E_MVK',reform(wrfemis[31,*,*,*])

; HCOOH
     ncdf_varput,ncid,'E_HCOOH',reform(wrfemis[33,*,*,*])

; BZALD
     ncdf_varput,ncid,'E_BZALD',reform(wrfemis[20,*,*,*])

; PHENOL
     ncdf_varput,ncid,'E_PHENOL',reform(wrfemis[28,*,*,*] + wrfemis[37,*,*,*]  )

; CRESOL
     ncdf_varput,ncid,'E_CRESOL',reform(wrfemis[29,*,*,*])

; HYDRALD
     ncdf_varput,ncid,'E_HYDRALD',reform(wrfemis[32,*,*,*])

; CH3COOH
     ncdf_varput,ncid,'E_CH3COOH',reform(wrfemis[34,*,*,*] + wrfemis[35,*,*,*]  )

; XYLOL
     ncdf_varput,ncid,'E_XYLOL',reform(wrfemis[36,*,*,*])



; Particulates

     ncdf_varput,ncid,'E_sulf',reform(wrfemis[54,*,*,*])
print, min(reform(wrfemis[54,*,*,*])), max(reform(wrfemis[54,*,*,*]))
     ncdf_varput,ncid,'E_PM_25',reform(wrfemis[56,*,*,*])
     ncdf_varput,ncid,'E_BC',reform(wrfemis[58,*,*,*])
     ncdf_varput,ncid,'E_OC',reform(wrfemis[57,*,*,*])    
     ncdf_varput,ncid,'E_PM_10',reform(wrfemis[59,*,*,*])

; MOZAIC aerosols:
;;;        'PM02', 'PM03', 'PM01', $
;;;      	'PM04', 'PM05', 'PM10-PRI']
; HC50 = specnames(53)
;1) PMFINE - unspeciated primary PM2.5       specnames(56)
;2) PSO4   - PM2.5 sulfate                   specnames(54)
;3) PNO3   - PM2.5 nitrate                   specnames(55)
;4) POC    - PM2.5 organic carbon            specnames(57)
;5) PEC    - PM2.5 elemental carbon          specnames(58)
;6) PNCOM  - PM2.5 Non-Carbon organic mass
;7) PNH4   - PM2.5 ammonium                  

     ncdf_varput,ncid,'E_SO4I',0.15*reform(wrfemis[54,*,*,*])
     ncdf_varput,ncid,'E_SO4J',0.85*reform(wrfemis[54,*,*,*])

     ncdf_varput,ncid,'E_PM25I',0.15*reform(wrfemis[56,*,*,*])
     ncdf_varput,ncid,'E_PM25J',0.85*reform(wrfemis[56,*,*,*])

     ncdf_varput,ncid,'E_ECI',0.15*reform(wrfemis[58,*,*,*])
     ncdf_varput,ncid,'E_ECJ',0.85*reform(wrfemis[58,*,*,*])

     ncdf_varput,ncid,'E_ORGI',0.15*reform(wrfemis[57,*,*,*])    
      ncdf_varput,ncid,'E_ORGJ',0.85*reform(wrfemis[57,*,*,*])    

      ncdf_varput,ncid,'E_NO3I',0.15*reform(wrfemis[55,*,*,*])    
      ncdf_varput,ncid,'E_NO3J',0.85*reform(wrfemis[55,*,*,*])    

;      ncdf_varput,ncid,'E_NH4I',0.15*reform(wrfemis[59,*,*,*])    
;      ncdf_varput,ncid,'E_NH4J',0.85*reform(wrfemis[59,*,*,*])    ; not in specnames


     ncdf_close,ncid
    
endfor               ; end of hour loop


;spawn, 'ncrcat wrfchemi_hour_0* wrfchemi_hour_10 wrfchemi_hour_11  wrfchemi_00z_d02'
;spawn, 'ncrcat wrfchemi_hour_1[23456789] wrfchemi_hour_2* wrfchemi_12z_d02'



end
