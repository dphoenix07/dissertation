;+
;NAME: PLOT_VERTICAL_TRANSPORT.PRO
;
;PROPOSE:
;
;CALCULATE ALL CHEMICAL VERTICAL TRANSPORT
;
;UPDATE:
;
;          LYY 07/14/2015

;====cal date====
sdate = 'june11';'june11' ;'may21'
sexp = 'cuexp01'
;========dir-wrf
dir_wrf = '/glade/scratch/liyunyao/run-cu-wrfchem-45-1/'
;dir_wrf = '/glade/p/acd/megan/output/chem/exp317/'
;dir_wrf = '/glade/scratch/liyunyao/WRF-OUTPUT/may29/'
;filename_wrf1 = 'wrfout1_10min_d01_2012-05-29_18:00:00'
;filename_wrf2 = 'wrfout1_10min_d01_2012-05-29_18:00:00'
;filename_wrf3 = 'wrfout1_10min_d01_2012-05-29_18:00:00'
;===================
outdir = '/glade/p/work/liyunyao/plots/mullendore/'+sdate+'/'+sexp+'/'
upmin = 2
upmax = 8
scal = ['vfd','pvfd','nvfd','npvfd','tvfd','tpvfd']
std = ['upw','00','20','30','40','50']
refltd = [00,00,20,30,40,50]
WRFspec = ['mass','co','o3']
molarweight = [28.8,28,48]
ispec = (size(WRFspec))(1)
itd = (size(refltd))(1)
ical = (size(scal))(1)

readflag = 1
ilmm = 420
ijmm = 480
ikmm = 40
;2.start and end time
ts_wrf = 18;16             ;wrf output start hour
ts_wrf_min = 00
tinv_wrf = 10           ;wrf output interval
ts_plot_hour =21; 17;19       ;plot start hour
ts_plot_min = 00        ;plot start min
te_plot_hour = 24;21;23       ;plot end hour
te_plot_min = 00        ;plot end min
dx = 1000;3000;600
dy = 1000;3000;600
;map region
latmin = 35.5;34;34.5
latmax = 37.5;40;35.75
lonmin = -99.75;-95;-87
lonmax = -96.25;-88;-85.7

;==========READ WRF DATA===========
;cdfid = ncdf_open(dir_wrf+filename_wrf)
if readflag eq 1 then begin
  cdfid = ncdf_open(dir_wrf+filename_wrf1)
  var = 'XLONG' & speciesid = ncdf_varid(cdfid,var) & ncdf_varget,cdfid,speciesid,dloncen_wrf
  var = 'XLAT' & speciesid = ncdf_varid(cdfid,var) & ncdf_varget,cdfid,speciesid,dlatcen_wrf
  dloncen = dloncen_wrf(*,*,0) & dlatcen = dlatcen_wrf(*,*,0)
  cdfid = ncdf_open(dir_wrf+filename_wrf2)
  var = 'PH' & speciesid = ncdf_varid(cdfid,var) & ncdf_varget,cdfid,speciesid,pressh
  ilmm = (size(pressh))(1)
  ijmm = (size(pressh))(2)
  ikmm = (size(pressh))(3)-1
  ntime = (size(pressh))(4)
  var = 'PHB' & speciesid = ncdf_varid(cdfid,var) & ncdf_varget,cdfid,speciesid,presshb
  height3d = (pressh + presshb)/9.8*0.001
  pressh = 0
  presshb = 0

;press
  var = 'P' & speciesid = ncdf_varid(cdfid,var) & ncdf_varget,cdfid,speciesid,pressp
  var = 'PB' & speciesid = ncdf_varid(cdfid,var) & ncdf_varget,cdfid,speciesid,pressb
  press3d = (pressp + pressb)*0.01
  pressp = 0
  pressb = 0
  cdfid = ncdf_open(dir_wrf+filename_wrf2)
  var = 'REFL_10CM' & speciesid = ncdf_varid(cdfid,var) & ncdf_varget,cdfid,speciesid,refl
  maxrefl = max(refl,dimension=3)
  cdfid = ncdf_open(dir_wrf+filename_wrf3)
  var = 'W' & speciesid = ncdf_varid(cdfid,var) & ncdf_varget,cdfid,speciesid,wwind
  var = 'T' & speciesid = ncdf_varid(cdfid,var) & ncdf_varget,cdfid,speciesid,kel
  kel = (kel+300)*(press3d/1000)^0.286
  var = 'QVAPOR' & speciesid = ncdf_varid(cdfid,var) & ncdf_varget,cdfid,speciesid,qvapor
;CAL RHO 
  rd=287.058
  rho = fltarr(ilmm,ijmm,ikmm,ntime)
  rho = press3d*100/(rd*kel*((1+qvapor/0.622)/(1+qvapor)))
  print,rho(55,55,0,16)
  press3d = 0
  kel = 0
  qvapor = 0
;========= mean height======(find mean height profile in center of domain)
  aa = where((dloncen gt lonmin) and (dloncen lt lonmax) and (dlatcen gt latmin) and (dlatcen lt latmax),count1)
  meanheight = fltarr(ikmm)
  heightmid = fltarr(ilmm,ijmm,ikmm,ntime)
  heighttemp = fltarr(ilmm,ijmm,ikmm)
  heighttemp2 = fltarr(ilmm,ijmm)
  for ik = 0,ikmm-1 do begin
    heightmid(*,*,ik,*)=0.5*(height3d(*,*,ik,*)+height3d(*,*,ik+1,*))
  endfor
  heighttemp = mean(heightmid,DIMENSION=4)
  for ik = 0,ikmm-1 do begin
    heighttemp2 = heighttemp(*,*,ik)
    meanheight(ik) = mean(heighttemp2(aa))
  endfor
  heightmid = 0
  heighttemp = 0
  heighttemp2 = 0
endif
print,'read ok'
;=================cal time==============(Find altitude and time where column max reflectivity exceeds 20 dbz)
itstart = ((ts_plot_hour-ts_wrf)*60+ts_plot_min-ts_wrf_min)/tinv_wrf
itend = ((te_plot_hour-ts_wrf)*60+te_plot_min-ts_wrf_min)/tinv_wrf
print,itstart,itend,itend-itstart+1
;vfd = fltarr(ispec,ical,itd,itend-itstart+1,ikmm-1)
for it = itstart,itend do begin
  ikmax=0
  for ik = 0,ikmm-1 do begin
    zz=where((dloncen gt lonmin) and (dloncen lt lonmax) and (dlatcen gt latmin) and (dlatcen lt latmax) and (max(refl(*,*,ik,it)) gt 20),count111)
    if (count111 gt 0) then ikmax=ik
  endfor
  print,it,ikmax
endfor

;==================cal VT===========
;1.tivfd (1h 2h 3h time-integrated flux divergence Skamarock fig 12)
for isp = 0,ispec-1 do begin
  print,WRFspec(isp)
  chemvar=0
  if isp gt 0 then begin
  fileID = ncdf_open(dir_wrf+filename_wrf2)
  varID = ncdf_varid(fileID,WRFspec(isp))
          ncdf_varget,fileID,varID,chemvar
  chemvar = chemvar/1e3
  endif else begin
    chemvar=fltarr(ilmm,ijmm,ikmm,ntime)+1.0
  endelse
;  for icl = 0,ical-1 do begin
  for id = 0,itd-1 do begin
    pvfd = fltarr(ikmm-1,itend-itstart+1)
    vfd = fltarr(ikmm-1,itend-itstart+1)
    nvfd = vfd
    npvfd = vfd
    tvfd = vfd
    tpvfd = vfd
    for it = itstart,itend do begin
      trefl = maxrefl(*,*,it)
      if id eq 0 then begin
        ll1 = where(meanheight gt upmin,countll1)	;level where mean ?? profile greater than 2
        ll2 = where(meanheight gt upmax,countll2)	;level where mean ?? profile greater than 8
;        print,ll1(00),ll2(00)
        wtemp1 = (wwind(*,*,ll1(0),it)+wwind(*,*,ll1(0)-1,it))/2.0
        wtemp2 = (wwind(*,*,ll2(0),it)+wwind(*,*,ll2(0)-1,it))/2.0
;        wtemp2 = max(wwind(*,*,*,it),dimension=3)
        if (countll1 gt 0) and (countll2 gt 0 ) then begin
          ss = where((dloncen gt lonmin) and (dloncen lt lonmax) and (dlatcen gt latmin) and (dlatcen lt latmax) and (wtemp1 gt 2) and (wtemp2 gt 5) and (trefl gt refltd(id)),countss)
        endif
      endif else begin
        ss = where((dloncen gt lonmin) and (dloncen lt lonmax) and (dlatcen gt latmin) and (dlatcen lt latmax) and (trefl gt refltd(id)),countss)
;      print,countss
      endelse
      if countss gt 0 then begin
        for ik = 0,ikmm-2 do begin
          dz = meanheight(ik+1)-meanheight(ik)
          wwind1temp = (wwind(*,*,ik,it)+wwind(*,*,ik+1,it))/2.0
          wwind2temp = (wwind(*,*,ik+1,it)+wwind(*,*,ik+2,it))/2.0
          rho1temp = rho(*,*,ik,it)
          rho2temp = rho(*,*,ik+1,it)
          chemvar1temp = chemvar(*,*,ik,it)
          chemvar2temp = chemvar(*,*,ik+1,it)
          w1 = wwind1temp(ss)
          w2 = wwind2temp(ss)
          if ((isp eq 1) and (id eq 5) and (it eq 9)) then begin
            print,ik,mean(w1)
          endif
          rho1 = rho1temp(ss)
          rho2 = rho2temp(ss)
          var1 = chemvar1temp(ss)
          var2 = chemvar2temp(ss)
          wwind1temp = 0
          wwind2temp = 0
          chemvar1temp = 0
          chemvar2temp = 0
          rho1temp = 0
          rho2temp = 0
          div1 = rho1*w1*var1*molarweight(isp)/28.8*dx*dy
          div2 = rho2*w2*var2*molarweight(isp)/28.8*dx*dy
          div = (div2-div1)/dz/1000
          countp1=0
          countn1=0
          for ic = 0,countss-1 do begin 
            tvfd(ik,it-itstart)=tvfd(ik,it-itstart)+div(ic)
            if ((w1(ic) ge 0) and (w2(ic) ge 0)) then begin
              vfd(ik,it-itstart)=vfd(ik,it-itstart)+div(ic)
              countp1=countp1+1
            endif else begin
              if ((w1(ic) le 0) and (w2(ic) le 0)) then begin
                nvfd(ik,it-itstart)=nvfd(ik,it-itstart)+div(ic)
                countn1=countn1+1
              endif
            endelse
          endfor
          tpvfd(ik,it-itstart)=tvfd(ik,it-itstart)*1e9/countss/dx/dy
          if countp1 gt 0 then begin
            pvfd(ik,it-itstart)=vfd(ik,it-itstart)*1e9/countp1/dx/dy
          endif else begin
            pvfd(ik,it-itstart)=0
          endelse
          if countn1 gt 0 then begin
            npvfd(ik,it-itstart)=nvfd(ik,it-itstart)*1e9/countn1/dx/dy
          endif else begin
            npvfd(ik,it-itstart)=0
          endelse
        endfor
      endif
    endfor
    openw,2,outdir+WRFspec(isp)+'-'+scal(0)+'-'+std(id)+'.txt'
    for ik=0,ikmm-2 do begin
      printf,2,vfd(ik,*),format='(21F20.8)'
    endfor
    close,2
    openw,3,outdir+WRFspec(isp)+'-'+scal(1)+'-'+std(id)+'.txt'
    for ik=0,ikmm-2 do begin
      printf,3,pvfd(ik,*),format='(21F20.8)'
    endfor
    close,3
    openw,4,outdir+WRFspec(isp)+'-'+scal(2)+'-'+std(id)+'.txt'
    for ik=0,ikmm-2 do begin
      printf,4,nvfd(ik,*),format='(21F20.8)'
    endfor
    close,4
    openw,5,outdir+WRFspec(isp)+'-'+scal(3)+'-'+std(id)+'.txt'
    for ik=0,ikmm-2 do begin
      printf,5,npvfd(ik,*),format='(21F20.8)'
    endfor
    close,5
    openw,6,outdir+WRFspec(isp)+'-'+scal(4)+'-'+std(id)+'.txt'
    for ik=0,ikmm-2 do begin
      printf,6,tvfd(ik,*),format='(21F20.8)'
    endfor
    close,6
    openw,7,outdir+WRFspec(isp)+'-'+scal(5)+'-'+std(id)+'.txt'
    for ik=0,ikmm-2 do begin
      printf,7,tpvfd(ik,*),format='(21F20.8)'
    endfor
    close,7
  endfor
endfor
print,meanheight
end
