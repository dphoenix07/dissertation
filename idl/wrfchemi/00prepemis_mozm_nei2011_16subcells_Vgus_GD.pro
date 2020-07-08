; NEI-2011 emissions - April 2015
; speciated for extended MOZART with MOSAIC aerosols
; 
; add ipoint to arguments of POINT_EMIS
;
; change only in the section of "user modified"
;
; Specification is for WRFchem_3.2 MOZCART
;
; this program splits the NEI 4km emissions into 1 km bins
; write the results to files <species>.subset (24 hours of data)
; lat and lon values are written to separate files
; units are converted to WRF units
; creates emissions files for one day, 24 hrs

;------------------------------------------------------------------------
PRO mean_height, emis_height_max,  ph, phb, hgt, eta_dim, wrfheight, wrflevs

alt = (ph+phb)/9.81

for i=0,eta_dim-1 do alt[*,*,i]=alt[*,*,i]-hgt

mean_alt = fltarr(eta_dim)

for i=0,eta_dim-1 do begin
 mean_alt[i] = mean(alt[*,*,i]-alt[*,*,0])
;print,i, mean_alt[i], stddev(alt[*,*,i]-alt[*,*,0])
endfor

index = where(mean_alt lt emis_height_max)
;print,index
wrflevs= n_elements(index)

wrfheight =fltarr(wrflevs)
wrfheight = mean_alt[index]

print,''
print,'emis_level  model mean_height' 
for i=0,wrflevs-1 do print,i,wrfheight[i]

end 
;-------------------------------------------------------------------------

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
;-------------------------------------------------------------------------

@area_emis.pro
@mapcf.pro
@ijll_lc.pro
@llij_lc.pro
@point_emis.pro
@create_nc_mozm.pro ; create empty wrfchemi files

;############################################################################
;user modified:

;input wrfinput file 
domain = 'd02'
;wrfinput_file  = '/glade/p/work/barthm/run.0529.v36.15km/wrfinput_d01'
;wrfinput_file  = '/glade/p/work/barthm/0622-1km.inputfiles/Domain2/wrfinput_'+domain 
;wrfinput_file  = '/glade/scratch/barthm/0622.run.1km/chemistry_3domains/D02/wrfinput_'+domain 
wrfinput_file  = '/glade/scratch/cuchiara/Vsimulation/wrfinput_'+domain 
;wrfinput_file  = '/glade/p/work/cuchiara/WRFV3_tracer/SEAC4RS_Sep2_wks_chem/wrfinput_'+domain
;input emission data directory

;area_emis_dir  = '/data2/jeff/data_emis_NEI-99/'
;point_emis_dir = '/data2/jeff/data_emis_NEI-99/'
;ipoint = 103249L

;area_emis_dir  = '/data1/alma/DATA_EMIS/NEI05_week/'
;point_emis_dir = '/data1/alma/DATA_EMIS/NEI05_week/'
;ipoint = 168516L
;area_emis_dir  = '/glade/p/acd/alma/WRF_emisrgd/WRFemis_NEI2011/'
;point_emis_dir  = '/glade/p/acd/alma/WRF_emisrgd/WRFemis_NEI2011/'

area_emis_dir  = '/glade/scratch/cuchiara/WRFemiss_NEII2011/'
point_emis_dir  = '/glade/scratch/cuchiara/WRFemiss_NEII2011/'
ipoint = 224097L 


;pick maximum height for emission: 1200 meter
emis_height_max = 1100.

;output_dir = '/glade/p/work/barthm/WRFemis_NEI2011/PREPEMIS/'
;output_dir = '/glade/p/work/barthm/WRFemis_NEI2011/DC3/0622_smallerdomains/'
output_dir = '/glade/scratch/cuchiara/'

;end of user modified
;##############################################################################

; get infomation from wrfinput file

id = ncdf_open(wrfinput_file)

ncdf_attget,id,'DX',dx_temp,/GLOBAL
ncdf_attget,id,'WEST-EAST_GRID_DIMENSION',wrf_ii,/GLOBAL
ncdf_attget,id,'SOUTH-NORTH_GRID_DIMENSION',wrf_jj,/GLOBAL
ncdf_attget,id,'BOTTOM-TOP_GRID_DIMENSION',eta_dim,/GLOBAL
ncdf_attget,id,'CEN_LAT',moad_cen_lat,/GLOBAL
ncdf_attget,id,'CEN_LON',stand_lon,/GLOBAL
ncdf_attget,id,'TRUELAT1',truelat1,/GLOBAL
ncdf_attget,id,'TRUELAT2',truelat2,/GLOBAL
ncdf_attget,id,'MAP_PROJ',map_proj,/GLOBAL

ncdf_varget,id,'PH',ph
ncdf_varget,id,'PHB',phb
ncdf_varget,id,'HGT',hgt

ncdf_close,id

; set local values

wrf_ix = wrf_ii - 1
wrf_jx = wrf_jj - 1

dx     = dx_temp/1000.
hemi   =  1
lat1   =  moad_cen_lat
lon1   =  stand_lon
knowni = (wrf_ix+1)/2.
knownj = (wrf_jx+1)/2.
    
; get wrfheight and wrflevs

mean_height, emis_height_max, ph, phb, hgt, eta_dim, wrfheight, wrflevs  

; get latmin,latmax,lonmin and lonmax
; rough domain borders (to limit the amount of NEI data to deal with)
; (pick 5 degree extension on all 4 sides)

degree_extention = 5.0

i=0
j=0

ijll_lc, i,j, a, b, truelat1, truelat2, $
          hemi, stand_lon,lat1, lon1, knowni, knownj, dx

latmin = round(a - degree_extention)
lonmin = round(b - degree_extention)
;print, latmin, lonmin

i=wrf_ix - 1
j=wrf_jx - 1

ijll_lc, i,j, a, b, truelat1, truelat2, $
          hemi, stand_lon,lat1, lon1, knowni, knownj, dx

latmax = round(a + degree_extention)
lonmax = round(b + degree_extention)
;print, latmax, lonmax

; get reflat and reflon

reflat = fltarr(wrf_ix, wrf_jx)
reflon = fltarr(wrf_ix, wrf_jx)

for i=0,wrf_ix-1 do begin
    for j=0,wrf_jx-1 do begin
        ijll_lc, i+1.5,j+1.5, a, b, truelat1, truelat2, $
          hemi, stand_lon,lat1, lon1, knowni, knownj, dx
        reflat[i,j] = a
        reflon[i,j] = b
    endfor
endfor

;create empty netcdf files

;yyyy = '2003'
;mm   = '03'
;dd   = '05'

hrs = strcompress(string(indgen(24)),/remove_all)
hrs[0:9] = '0'+hrs[0:9]

; hourly outputs
;timestring = yyyy+'-'+mm+'-'+dd+'_'+hrs+':00:00'
;ncfiles = output_dir +'wrfchemi_'+domain+'_'+timestring

timestring = '_hr'+hrs
file_out = 'wrfchemi_'+domain+timestring
ncfiles = output_dir + file_out

print, ''
print, 'Creating the following files in ', output_dir

for i=0,23 do begin 
;for i=0,0  do begin 

create_nc_mozm, ncfiles[i], timestring[i], wrflevs, wrf_ix, wrf_jx, dx*1e3, dx*1e3, $
  lat1, lon1, truelat1, truelat2, moad_cen_lat,  eta_dim, wrfheight, map_proj  

;GUS
; create_nc_mozm, ncfiles[i],  wrflevs, wrf_ix, wrf_jx, dx*1e3, dx*1e3, $
;  lat1, lon1, truelat1, truelat2, moad_cen_lat,  eta_dim, wrfheight, map_proj

print,i+1,'   ', file_out[i]

endfor

print, ''

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
        'PM01', 'PM02', 'PM03', $
        'PM04', 'PM05', 'PM06', 'PM07', 'PM10-PRI']
; HC50 = specnames(53)
; PM01 : PMFINE  - unspeciated primary PM2.5       specnames(54) ;(56)
; PM02 : PSO4    - PM2.5 sulfate                   specnames(55) ;(54)
; PM03 : PNO3    - PM2.5 nitrate                   specnames(56) ;(55)
; PM04 : POC     - PM2.5 organic carbon            specnames(57)
; PM05 : PEC     - PM2.5 elemental carbon          specnames(58)
; Units PM species are in ton/hr or ton/day
; Units POC and PEC tons of Carbon per hour or per day

;1) PM01 (54): PMFINE - unspeciated primary PM2.5
;2) PM02: PSO4   - PM2.5 sulfate
;3) PM03: PNO3   - PM2.5 nitrate
;4) PM04 (57): POC    - PM2.5 organic carbon
;5) PM05 (58): PEC    - PM2.5 elemental carbon
;6) PM06 (59): PNCOM  - PM2.5 Non-Carbon organic mass (associated with POC)
;7) PM07 (60): PNH4   - PM2.5 ammonium
;8) PM08: PAL    - PM2.5 aluminum
;9) PM09: PCA    - PM2.5 calcium
;16) PM16: PCL    - PM2.5 chloride
;17) PM17: PNA    - PM2.5 sodium

molweight = [28.0103989,46.0055389, 64.065, 17.031 ]
nspec = n_elements(specnames)

; dimensions of NEI grid
nei_ix = 1332
nei_jx = 1008


ihr = ['HR01','HR02','HR03','HR04','HR05','HR06','HR07','HR08','HR09','HR10','HR11','HR12', $
       'HR13','HR14','HR15','HR16','HR17','HR18','HR19','HR20','HR21','HR22','HR23','HR24']


AREA_EMIS, area_emis_dir +'area4k/HR01/', 'CO', areadata, ix, jx, arealat, arealon
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

;            mapcf, ix+1.25, jx+1.25, latsub, lonsub
;            lonnei[count] = lonsub
;            latnei[count] = latsub
;
;;            mapcf, ix+1.75, jx+1.25, latsub, lonsub
;            lonnei[count+1L] = lonsub
;            latnei[count+1L] = latsub
;            
;            mapcf, ix+1.75, jx+1.75, latsub, lonsub
;            lonnei[count+2L] = lonsub
;            latnei[count+2L] = latsub
;
;            mapcf, ix+1.25, jx+1.75, latsub, lonsub
;            lonnei[count+3L] = lonsub
;            latnei[count+3L] = latsub
;
;            count = count + 4L

        endif
    endfor
endfor

ncount = count

latnei = extrac(latnei,0,ncount)
lonnei = extrac(lonnei,0,ncount)
wrfi = intarr(ncount)
wrfj = intarr(ncount)


; do area emission assignment to WRF grid here
dist = fltarr(ncount)-999.  ; diatance in meters between NEI cell center and WRF cell center

openw,1,'NEIarea_WRFassign.dat'

printf,1,ncount
d=-99

for i = 0L, ncount-1 do begin
     llij_lc, latnei[i],lonnei[i],xi, yj, truelat1, truelat2, hemi, stand_lon,lat1, lon1, knowni, knownj, dx
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

infofile  = point_emis_dir+ 'point/Relpnt_info.txt' ; point release info file

point_emis_info, primpath, infofile, pointlat, pointlon, stackD, stackH, ipoint, reltype

wrfipoint = fltarr(ipoint)
wrfjpoint = fltarr(ipoint)
kpoint = fltarr(ipoint)
dist_point = fltarr(ipoint)

openw,1,'NEIpoint_WRFassign.dat'

for i = 0L, ipoint-1L do begin
    llij_lc, pointlat[i],pointlon[i],xi, yj, truelat1, truelat2, hemi, stand_lon,lat1, lon1, knowni, knownj, dx
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

for hrloop = 0,23 do begin
;;for hrloop = 1,23 do begin
;;;;;;for hrloop = 0,0  do begin

    print, 'Hour: ', ihr(hrloop)

    wrfemis = fltarr(nspec, wrf_ix, wrf_jx, wrflevs)
    wrfemis_n = intarr(nspec, wrf_ix, wrf_jx, wrflevs)


    for ispec = 0, nspec-1 do begin

        areapath = area_emis_dir+ 'area4k/'+ihr[hrloop]+'/'

        areadata = fltarr(nei_ix, nei_jx)
        openr,1,areapath + specnames[ispec]
        readf,1,areadata

        close,1

; convert primary emissions from 
; primary: short tons/hr/gridcell to moles/hr/km2 
; VOC:  from moles/hr/gridcell to moles/hr/km2 for
; particulates: from short tons/hr/gridcell  to micrograms/m2/sec for
; 1 short ton = 907.185 kg 
; species 3->53 are VOCs
;    HC50 = specnames(53)
;    PMFINE (54)


        if ispec le 3  then begin
            print, 'area  Primary: ', specnames[ispec]
            fac = 9.07184e5/molweight(ispec) ;/(dx*dx)
        endif else if ispec gt 3 and ispec le 53 then begin
            print, 'area  VOC: ', specnames[ispec]
            fac = 1.;/(dx*dx)
        endif else if ispec ge 54 then begin
            print, 'area  Particulate: ', specnames[ispec]
            fac = 9.07184e11/3600./(1e3*1e3) ;(dx*dx*1e3*1e3)
        endif

        areadata = areadata*fac/nbins  ; normalize to km-2
; split into nbins subcells and assign to WRF grid
        ;;areadata = areadata*fac/4.  ; factor of 4 needed because I am splitting into 4 subcells

; split into 4 subcells and assign to WRF grid

        count=0L
        for ix = 0, nei_ix - 1 do begin
            for jx = 0, nei_jx - 1 do begin
                if arealat[ix,jx] gt latmin and arealat[ix,jx] le latmax and $
                   arealon[ix,jx] gt lonmin and arealon[ix,jx] le lonmax then begin

; idl index starts at 0, grid index at 1!!!
                    
                    emis  = areadata[ix,jx]
                    for innerloop = 0, nbins-1 do begin
                   ;;; for innerloop = 0, 3 do begin
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
;alma
; average:
        for ix=0,wrf_ix-1 do begin
            for jx=0,wrf_jx-1 do begin
                if wrfemis_n[ispec,ix,jx,0] gt 0 then $
                  wrfemis[ispec,ix,jx,0]=wrfemis[ispec,ix,jx,0]/wrfemis_n[ispec,ix,jx,0]
            endfor
        endfor

; get point data, units like area emissions but in rates per point source
; should include plume rise calculation!

        pointpath = point_emis_dir+'point/'+ihr[hrloop]+'/'

        POINT_EMIS, pointpath, specnames[ispec], pointdata, ipoint


        if ispec le 3 then begin
;            print, 'Primary: ', specnames[ispec]
            fac = 9.07184e5/molweight(ispec)/(dx*dx)
        endif else if ispec gt 3 and ispec le 53 then begin
;            print, 'VOC: ', specnames[ispec]
            fac = 1./(dx*dx)
        endif else if ispec ge 54 then begin
;            print, 'Particulate: ', specnames[ispec]
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
    ncdf_varput,ncid,'E_CO_A',reform(wrfemis[0,*,*,*])
    ncdf_varput,ncid,'E_CO_BB',zerofield

    ncdf_varput,ncid,'E_NO',reform(wrfemis[1,*,*,*])*0.9
    ncdf_varput,ncid,'E_NO2',reform(wrfemis[1,*,*,*])*0.08
    ncdf_varput,ncid,'E_HONO',reform(wrfemis[1,*,*,*])*0.02
    ncdf_varput,ncid,'E_SO2',reform(wrfemis[2,*,*,*])
    ncdf_varput,ncid,'E_NH3',reform(wrfemis[3,*,*,*])

; VOC Speciation   

    ncdf_varput,ncid,'E_C2H5OH',reform(wrfemis[51,*,*,*])  ;ethanol

; BIGALK = 
   tmp = wrfemis[7,*,*,*]+wrfemis[8,*,*,*]+wrfemis[9,*,*,*]+wrfemis[42,*,*,*]+wrfemis[43,*,*,*]+wrfemis[52,*,*,*]
   ncdf_varput,ncid,'E_BIGALK',reform(tmp)
; BIGENE = 
   ncdf_varput,ncid,'E_BIGENE', reform(wrfemis[12,*,*,*]+wrfemis[49,*,*,*]+wrfemis[50,*,*,*])

; C2H4 = ethylene
   ncdf_varput,ncid,'E_C2H4', reform(wrfemis[10,*,*,*])

; C2H6 = Alk1
   ncdf_varput,ncid,'E_C2H6',reform(wrfemis[5,*,*,*])

; C3H6 = propylene
    ncdf_varput,ncid,'E_C3H6',reform(wrfemis[11,*,*,*]+wrfemis[39,*,*,*])

; C3H8 = propane
     ncdf_varput,ncid,'E_C3H8',reform(wrfemis[48,*,*,*]+wrfemis[6,*,*,*])

; CH2O = formaldehyde
     ncdf_varput,ncid,'E_CH2O',reform(wrfemis[17,*,*,*])

; CH3CHO = acetaldehyde
     ncdf_varput,ncid,'E_CH3CHO',reform(wrfemis[18,*,*,*]+wrfemis[19,*,*,*])

;CH3COCH3 = acetone
     ncdf_varput,ncid,'E_CH3COCH3',reform(wrfemis[21,*,*,*])

;CH3OH = methanol
     ncdf_varput,ncid,'E_CH3OH',reform(wrfemis[24,*,*,*])

; MEK=  methyl-ethyle-ketone
     ncdf_varput,ncid,'E_MEK',reform(wrfemis[22,*,*,*]+wrfemis[23,*,*,*])

; TOLUENE
     ncdf_varput,ncid,'E_TOLUENE',reform(wrfemis[44,*,*,*]+wrfemis[15,*,*,*])

; ISOP
     ncdf_varput,ncid,'E_ISOP',reform(wrfemis[13,*,*,*])

; C10H16
     ncdf_varput,ncid,'E_C10H16',reform(wrfemis[14,*,*,*])

; ++alma: Alma added extra species for expanded MOZART
; BENZENE 
     ncdf_varput,ncid,'E_BENZENE',reform(wrfemis[41,*,*,*])
; XYLENE 
     ncdf_varput,ncid,'E_XYLENE',reform(wrfemis[16,*,*,*]+wrfemis[45,*,*,*]+wrfemis[46,*,*,*]+wrfemis[47,*,*,*])
; C2H2
     ncdf_varput,ncid,'E_C2H2',reform(wrfemis[40,*,*,*])
; GLYOXAL
     ncdf_varput,ncid,'E_GLY',reform(wrfemis[25,*,*,*])
; MACR
     ncdf_varput,ncid,'E_MACR',reform(wrfemis[30,*,*,*])
; MGLY
     ncdf_varput,ncid,'E_MGLY',reform(wrfemis[26,*,*,*]+wrfemis[27,*,*,*])
; MVK
     ncdf_varput,ncid,'E_MVK',reform(wrfemis[31,*,*,*])
; HCOOH formic acid
     ncdf_varput,ncid,'E_HCOOH',reform(wrfemis[33,*,*,*])
; +BZALD benzald
     ncdf_varput,ncid,'E_BZALD',reform(wrfemis[20,*,*,*])
; +PHENOL
     ncdf_varput,ncid,'E_PHENOL',reform(wrfemis[28,*,*,*]+wrfemis[37,*,*,*])
; +CRESOL
     ncdf_varput,ncid,'E_CRESOL',reform(wrfemis[29,*,*,*])
; +HYDRALD
     ncdf_varput,ncid,'E_HYDRALD',reform(wrfemis[32,*,*,*])
; +CH3COOH
     ncdf_varput,ncid,'E_CH3COOH',reform(wrfemis[34,*,*,*]+wrfemis[35,*,*,*])
; +XYLOL
     ncdf_varput,ncid,'E_XYLOL',reform(wrfemis[36,*,*,*])
; +IVOC = 0.2*NMVOC
     ncdf_varput,ncid,'E_IVOC',reform(0.2*(wrfemis[38,*,*,*]))
;-wrfemis[4,*,*,*]))
;---alma

; Particulates

; for gocart aerosols
     ncdf_varput,ncid,'E_sulf',reform(wrfemis[55,*,*,*])
print, min(reform(wrfemis[55,*,*,*])), max(reform(wrfemis[55,*,*,*]))


; for MOSAIC aerosols

     ncdf_varput,ncid,'E_SO4I',0.15*reform(wrfemis[55,*,*,*])
     ncdf_varput,ncid,'E_SO4J',0.85*reform(wrfemis[55,*,*,*])

     ncdf_varput,ncid,'E_PM25I',0.15*reform(wrfemis[54,*,*,*])
     ncdf_varput,ncid,'E_PM25J',0.85*reform(wrfemis[54,*,*,*])

     ncdf_varput,ncid,'E_ECI',0.15*reform(wrfemis[58,*,*,*])
     ncdf_varput,ncid,'E_ECJ',0.85*reform(wrfemis[58,*,*,*])

; organic carbon is Tons of C; use 1.4 to convert to POA
     ;ncdf_varput,ncid,'E_ORGI',0.15*reform(wrfemis[57,*,*,*] * 1.4)    
     ; ncdf_varput,ncid,'E_ORGJ',0.85*reform(wrfemis[57,*,*,*] * 1.4)    
     ncdf_varput,ncid,'E_ORGI',0.15*reform(wrfemis[57,*,*,*]+wrfemis[59,*,*,*])
      ncdf_varput,ncid,'E_ORGJ',0.85*reform(wrfemis[57,*,*,*]+wrfemis[59,*,*,*])

      ncdf_varput,ncid,'E_NO3I',0.15*reform(wrfemis[56,*,*,*])
      ncdf_varput,ncid,'E_NO3J',0.85*reform(wrfemis[56,*,*,*])

      ncdf_varput,ncid,'E_NH4I',0.15*reform(wrfemis[60,*,*,*])
      ncdf_varput,ncid,'E_NH4J',0.85*reform(wrfemis[60,*,*,*])

     ncdf_varput,ncid,'E_PM_10',reform(wrfemis[61,*,*,*])


    ncdf_close,ncid
    
endfor               ; end of hour loop

;spawn, '/usr/bin/ncrcat wrfchemi_hour_0* wrfchemi_hour_10 wrfchemi_hour_11  wrfchemi_00z_d02'
;spawn, '/usr/bin/ncrcat wrfchemi_hour_1[23456789] wrfchemi_hour_2* wrfchemi_12z_d02'
;spawn, 'rm wrfchemi_hour*'
;spawn, 'rm *.dat'

end
