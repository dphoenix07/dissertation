MEtFILE = '/data/pfister/FRAPPE/wrfinput_d02'

id = ncdf_open(metfile)
ncdf_varget,id,'PH',ph
ncdf_varget,id,'PHB',phb
ncdf_varget,id,'HGT',hgt

nlev=n_elements(ph[0,0,*])
alt = (ph+phb)/9.81
for i=0,nlev-1 do alt[*,*,i]=alt[*,*,i]-hgt

ncdf_close,id

meanalt=fltarr(nlev)
for i=0,nlev-1 do begin
    print,i, mean(alt[*,*,i]-alt[*,*,0]), stddev(alt[*,*,i]-alt[*,*,0]) ;, mean(alt27[*,*,i]-alt27[*,*,0])
    meanalt[i]= mean(alt[*,*,i]-alt[*,*,0])
endfor
print, meanalt,format='(40(F8.1,","))'

end 
