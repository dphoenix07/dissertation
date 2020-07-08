
PRO create_nc_mozm, emisfile, timestring, zdim, WE, SN, dx, dy, cenlat, cenlon, truelat1, truelat2, moad_cen_lat, eta_dim, z, mapproj 
;PRO create_nc_mozm, emisfile, timestring, zdim, WE, SN, dx, dy, cenlat, cenlon, truelat1, truelat2, moad_cen_lat, mapproj, neiinfo

ncid = ncdf_create(emisfile, /clobber)
; create dimensions
;timeid = ncdf_dimdef(ncid, 'Time',1) 
timeid = ncdf_dimdef(ncid, 'Time',/UNLIMITED) 
datestrlenid = ncdf_dimdef(ncid, 'DateStrLen',19) 
weid = ncdf_dimdef(ncid, 'west_east',WE) 
snid = ncdf_dimdef(ncid, 'south_north',SN) 
btid = ncdf_dimdef(ncid, 'bottom_top',eta_dim)
stagid = ncdf_dimdef(ncid,'emissions_zdim_stag',zdim)


; write global attributes


ncdf_attput,ncid,/GLOBAL,/long,'WEST-EAST_GRID_DIMENSION',WE+1
ncdf_attput,ncid,/GLOBAL,/long,'SOUTH-NORTH_GRID_DIMENSION',SN+1
ncdf_attput,ncid,/GLOBAL,/long,'BOTTOM-TOP_GRID_DIMENSION',eta_dim
ncdf_attput,ncid,/GLOBAL,/char, 'TITLE','Created by Alma Hodzic '
ncdf_attput,ncid,/GLOBAL,/float,'DX',dx
ncdf_attput,ncid,/GLOBAL,/float,'DY',dy
ncdf_attput,ncid,/GLOBAL,/float,'HEIGHT',z
ncdf_attput,ncid,/GLOBAL,/float,'CEN_LAT',cenlat
ncdf_attput,ncid,/GLOBAL,/float,'CEN_LON',cenlon
ncdf_attput,ncid,/GLOBAL,/float,'TRUELAT1',truelat1
ncdf_attput,ncid,/GLOBAL,/float,'TRUELAT2',truelat2
ncdf_attput,ncid,/GLOBAL,/float,'MOAD_CEN_LAT',moad_cen_lat
ncdf_attput,ncid,/GLOBAL,/long,'MAP_PROJ',mapproj
;ncdf_attput,ncid,/GLOBAL,/char,'MMINLU','USGS'
;ncdf_attput,ncid,/GLOBAL,/char,'MMINLU','MODIFIED_IGBP_MODIS_NOAH'



;emis_ant:e_co,e_no,e_no2,e_bigalk,e_bigene,e_c2h4,e_c2h5oh,e_c2h6,e_c3h6,e_c3h8,e_ch2o,e_ch3cho,e_ch3coch3,e_ch3oh,e_mek,e_so2,e_toluene,e_benzene,e_xylene,e_nh3,e_isop,e_c10h16,e_pm25i,e_pm25j,e_eci,e_ecj,e_orgi,e_orgj,e_so4i,e_so4j,e_no3i,e_no3j,e_nh4i,e_nh4j,e_nai,e_naj,e_cli,e_clj,e_co_a,e_orgi_a,e_orgj_a,e_co_bb,e_orgi_bb,e_orgj_bb,e_pm_10,e_c2h2,e_gly,e_sulf,e_macr,e_mgly,e_mvk,e_hcooh,e_hono

names='E_'+ ['CO','NO','NO2','SO2','NH3','ETH','HC3','HC5','HC8', $
			'OL2','OLT','OLI','ISO','TOL','XYL','HCHO','ALD','KET', $
			'CSL','ORA2']

nnames=n_elements(names)


; define varaiable fields
tvarid = ncdf_vardef(ncid,'Times',[datestrlenid, timeid ],/char)

for iname=0,nnames-1 do begin
    varid = ncdf_vardef(ncid, names[iname], [weid,snid,stagid, timeid], /FLOAT)
    ncdf_attput,ncid, names[iname], 'FieldType',  /long, 104
    ncdf_attput,ncid, names[iname], 'MemoryOrder',/char,'XYZ'
    ncdf_attput,ncid, names[iname], 'description',/char,'EMISSIONS'
    ncdf_attput,ncid, names[iname], 'units',      /char,'mole km-2 hr-1'
    ncdf_attput,ncid, names[iname], 'stagger',    /char,'Z'
endfor

varid = ncdf_vardef(ncid, 'E_PM25I', [weid,snid,stagid, timeid], /FLOAT)
ncdf_attput,ncid,'E_PM25I', 'FieldType',  /long, 104
ncdf_attput,ncid,'E_PM25I', 'MemoryOrder',/char,'XYZ'
ncdf_attput,ncid,'E_PM25I', 'description',/char,'EMISSIONS'
ncdf_attput,ncid,'E_PM25I', 'units',      /char,'ug m-2 s-1'
ncdf_attput,ncid,'E_PM25I', 'stagger',    /char,'Z'

varid = ncdf_vardef(ncid, 'E_PM25J', [weid,snid,stagid, timeid], /FLOAT)
ncdf_attput,ncid,'E_PM25J', 'FieldType',  /long, 104
ncdf_attput,ncid,'E_PM25J', 'MemoryOrder',/char,'XYZ'
ncdf_attput,ncid,'E_PM25J', 'description',/char,'EMISSIONS'
ncdf_attput,ncid,'E_PM25J', 'units',      /char,'ug m-2 s-1'
ncdf_attput,ncid,'E_PM25J', 'stagger',    /char,'Z'

varid = ncdf_vardef(ncid, 'E_SO4I', [weid,snid,stagid, timeid], /FLOAT)
ncdf_attput,ncid,'E_SO4I', 'FieldType',  /long, 104
ncdf_attput,ncid,'E_SO4I', 'MemoryOrder',/char,'XYZ'
ncdf_attput,ncid,'E_SO4I', 'description',/char,'EMISSIONS'
ncdf_attput,ncid,'E_SO4I', 'units',      /char,'ug m-2 s-1'
ncdf_attput,ncid,'E_SO4I', 'stagger',    /char,'Z'

varid = ncdf_vardef(ncid, 'E_SO4J', [weid,snid,stagid, timeid], /FLOAT)
ncdf_attput,ncid,'E_SO4J', 'FieldType',  /long, 104
ncdf_attput,ncid,'E_SO4J', 'MemoryOrder',/char,'XYZ'
ncdf_attput,ncid,'E_SO4J', 'description',/char,'EMISSIONS'
ncdf_attput,ncid,'E_SO4J', 'units',      /char,'ug m-2 s-1'
ncdf_attput,ncid,'E_SO4J', 'stagger',    /char,'Z'

varid = ncdf_vardef(ncid, 'E_NO3I', [weid,snid,stagid, timeid], /FLOAT)
ncdf_attput,ncid,'E_NO3I', 'FieldType',  /long, 104
ncdf_attput,ncid,'E_NO3I', 'MemoryOrder',/char,'XYZ'
ncdf_attput,ncid,'E_NO3I', 'description',/char,'EMISSIONS'
ncdf_attput,ncid,'E_NO3I', 'units',      /char,'ug m-2 s-1'
ncdf_attput,ncid,'E_NO3I', 'stagger',    /char,'Z'

varid = ncdf_vardef(ncid, 'E_NO3J', [weid,snid,stagid, timeid], /FLOAT)
ncdf_attput,ncid,'E_NO3J', 'FieldType',  /long, 104
ncdf_attput,ncid,'E_NO3J', 'MemoryOrder',/char,'XYZ'
ncdf_attput,ncid,'E_NO3J', 'description',/char,'EMISSIONS'
ncdf_attput,ncid,'E_NO3J', 'units',      /char,'ug m-2 s-1'
ncdf_attput,ncid,'E_NO3J', 'stagger',    /char,'Z'

varid = ncdf_vardef(ncid, 'E_ORGI', [weid,snid,stagid, timeid], /FLOAT)
ncdf_attput,ncid,'E_ORGI', 'FieldType',  /long, 104
ncdf_attput,ncid,'E_ORGI', 'MemoryOrder',/char,'XYZ'
ncdf_attput,ncid,'E_ORGI', 'description',/char,'EMISSIONS'
ncdf_attput,ncid,'E_ORGI', 'units',      /char,'ug m-2 s-1'
ncdf_attput,ncid,'E_ORGI', 'stagger',    /char,'Z'

varid = ncdf_vardef(ncid, 'E_ORGJ', [weid,snid,stagid, timeid], /FLOAT)
ncdf_attput,ncid,'E_ORGJ', 'FieldType',  /long, 104
ncdf_attput,ncid,'E_ORGJ', 'MemoryOrder',/char,'XYZ'
ncdf_attput,ncid,'E_ORGJ', 'description',/char,'EMISSIONS'
ncdf_attput,ncid,'E_ORGJ', 'units',      /char,'ug m-2 s-1'
ncdf_attput,ncid,'E_ORGJ', 'stagger',    /char,'Z'

varid = ncdf_vardef(ncid, 'E_ECI', [weid,snid,stagid, timeid], /FLOAT)
ncdf_attput,ncid,'E_ECI', 'FieldType',  /long, 104
ncdf_attput,ncid,'E_ECI', 'MemoryOrder',/char,'XYZ'
ncdf_attput,ncid,'E_ECI', 'description',/char,'EMISSIONS'

varid = ncdf_vardef(ncid, 'E_ECJ', [weid,snid,stagid, timeid], /FLOAT)
ncdf_attput,ncid,'E_ECJ', 'FieldType',  /long, 104
ncdf_attput,ncid,'E_ECJ', 'MemoryOrder',/char,'XYZ'
ncdf_attput,ncid,'E_ECJ', 'description',/char,'EMISSIONS'
ncdf_attput,ncid,'E_ECJ', 'units',      /char,'ug m-2 s-1'
ncdf_attput,ncid,'E_ECJ', 'stagger',    /char,'Z'

varid = ncdf_vardef(ncid, 'E_PM_10', [weid,snid,stagid, timeid], /FLOAT)
ncdf_attput,ncid,'E_PM_10', 'FieldType',  /long, 104
ncdf_attput,ncid,'E_PM_10', 'MemoryOrder',/char,'XYZ'
ncdf_attput,ncid,'E_PM_10', 'description',/char,'EMISSIONS'
ncdf_attput,ncid,'E_PM_10', 'units',      /char,'ug m-2 s-1'
ncdf_attput,ncid,'E_PM_10', 'stagger',    /char,'Z'


ncdf_control,ncid, /endef

; write fields
ncdf_varput,ncid,'Times',timestring

ncdf_close,ncid

end



