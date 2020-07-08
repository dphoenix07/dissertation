;Dec 2010: add xnox

PRO create_netcdffile_gocart, emisfile, timestring, zdim, WE, SN, dx, dy, cenlat, cenlon, truelat1, truelat2, moad_cen_lat, mapproj, neiinfo

ncid = ncdf_create(emisfile, /clobber)
; create dimensions
;timeid = ncdf_dimdef(ncid, 'Time',1) 
timeid = ncdf_dimdef(ncid, 'Time',/UNLIMITED) 
datestrlenid = ncdf_dimdef(ncid, 'DateStrLen',19) 
weid = ncdf_dimdef(ncid, 'west_east',WE) 
snid = ncdf_dimdef(ncid, 'south_north',SN) 
btid = ncdf_dimdef(ncid, 'bottom_top',28)
stagid = ncdf_dimdef(ncid,'emissions_zdim_stag',zdim)


; write global attributes

ncdf_attput,ncid,/GLOBAL,/long,'WEST-EAST_GRID_DIMENSION',WE+1
ncdf_attput,ncid,/GLOBAL,/long,'SOUTH-NORTH_GRID_DIMENSION',SN+1
ncdf_attput,ncid,/GLOBAL,/long,'BOTTOM-TOP_GRID_DIMENSION',28
ncdf_attput,ncid,/GLOBAL,/char, 'TITLE','Created by G. Pfister '+neiinfo
ncdf_attput,ncid,/GLOBAL,/float,'DX',dx
ncdf_attput,ncid,/GLOBAL,/float,'DY',dy
ncdf_attput,ncid,/GLOBAL,/float,'CEN_LAT',cenlat
ncdf_attput,ncid,/GLOBAL,/float,'CEN_LON',cenlon
ncdf_attput,ncid,/GLOBAL,/float,'TRUELAT1',truelat1
ncdf_attput,ncid,/GLOBAL,/float,'TRUELAT2',truelat2
ncdf_attput,ncid,/GLOBAL,/float,'MOAD_CEN_LAT',moad_cen_lat
ncdf_attput,ncid,/GLOBAL,/long,'MAP_PROJ',mapproj
;ncdf_attput,ncid,/GLOBAL,/char,'MMINLU','USGS'
ncdf_attput,ncid,/GLOBAL,/char,'MMINLU','MODIFIED_IGBP_MODIS_NOAH'


names='E_'+ ['CO','NO','NO2','BIGALK','BIGENE','C2H4','C2H5OH','C2H6','C3H6','C3H8', $
             'CH2O','CH3CHO','CH3COCH3','CH3OH','MEK','SO2','TOLUENE','BENZENE','XYLENE', $
             'NH3','ISOP','C10H16','C2H2','GLY','MACR','MGLY','MVK','HCOOH','BZALD',  $
             'PHENOL','CRESOL','HYDRALD','CH3COOH','XYLOL','sulf','CO_A', 'CO_BB','CO02','CO03','XNO','XNO2']

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

varid = ncdf_vardef(ncid, 'E_PM_10', [weid,snid,stagid, timeid], /FLOAT)
ncdf_attput,ncid,'E_PM_10', 'FieldType',  /long, 104
ncdf_attput,ncid,'E_PM_10', 'MemoryOrder',/char,'XYZ'
ncdf_attput,ncid,'E_PM_10', 'description',/char,'EMISSIONS'
ncdf_attput,ncid,'E_PM_10', 'units',      /char,'ug m-2 s-1'
ncdf_attput,ncid,'E_PM_10', 'stagger',    /char,'Z'

varid = ncdf_vardef(ncid, 'E_ECI', [weid,snid,stagid, timeid], /FLOAT)
ncdf_attput,ncid,'E_ECI', 'FieldType',  /long, 104
ncdf_attput,ncid,'E_ECI', 'MemoryOrder',/char,'XYZ'
ncdf_attput,ncid,'E_ECI', 'description',/char,'EMISSIONS'
ncdf_attput,ncid,'E_ECI', 'units',      /char,'ug m-2 s-1'
ncdf_attput,ncid,'E_ECI', 'stagger',    /char,'Z'

varid = ncdf_vardef(ncid, 'E_ECJ', [weid,snid,stagid, timeid], /FLOAT)
ncdf_attput,ncid,'E_ECJ', 'FieldType',  /long, 104
ncdf_attput,ncid,'E_ECJ', 'MemoryOrder',/char,'XYZ'
ncdf_attput,ncid,'E_ECJ', 'description',/char,'EMISSIONS'
ncdf_attput,ncid,'E_ECJ', 'units',      /char,'ug m-2 s-1'
ncdf_attput,ncid,'E_ECJ', 'stagger',    /char,'Z'


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

varid = ncdf_vardef(ncid, 'E_NH4I', [weid,snid,stagid, timeid], /FLOAT)
ncdf_attput,ncid,'E_NH4I', 'FieldType',  /long, 104
ncdf_attput,ncid,'E_NH4I', 'MemoryOrder',/char,'XYZ'
ncdf_attput,ncid,'E_NH4I', 'description',/char,'EMISSIONS'
ncdf_attput,ncid,'E_NH4I', 'units',      /char,'ug m-2 s-1'
ncdf_attput,ncid,'E_NH4I', 'stagger',    /char,'Z'

varid = ncdf_vardef(ncid, 'E_NH4J', [weid,snid,stagid, timeid], /FLOAT)
ncdf_attput,ncid,'E_NH4J', 'FieldType',  /long, 104
ncdf_attput,ncid,'E_NH4J', 'MemoryOrder',/char,'XYZ'
ncdf_attput,ncid,'E_NH4J', 'description',/char,'EMISSIONS'
ncdf_attput,ncid,'E_NH4J', 'units',      /char,'ug m-2 s-1'
ncdf_attput,ncid,'E_NH4J', 'stagger',    /char,'Z'


; also add fields for GOCART

; sulf based on registry should be in moles

varid = ncdf_vardef(ncid, 'E_PM_25', [weid,snid,stagid, timeid], /FLOAT)
ncdf_attput,ncid,'E_PM_25', 'FieldType',  /long, 104
ncdf_attput,ncid,'E_PM_25', 'MemoryOrder',/char,'XYZ'
ncdf_attput,ncid,'E_PM_25', 'description',/char,'EMISSIONS'
ncdf_attput,ncid,'E_PM_25', 'units',      /char,'ug m-2 s-1'
ncdf_attput,ncid,'E_PM_25', 'stagger',    /char,'Z'


varid = ncdf_vardef(ncid, 'E_OC', [weid,snid,stagid, timeid], /FLOAT)
ncdf_attput,ncid,'E_OC', 'FieldType',  /long, 104
ncdf_attput,ncid,'E_OC', 'MemoryOrder',/char,'XYZ'
ncdf_attput,ncid,'E_OC', 'description',/char,'EMISSIONS'
ncdf_attput,ncid,'E_OC', 'units',      /char,'ug m-2 s-1'
ncdf_attput,ncid,'E_OC', 'stagger',    /char,'Z'

varid = ncdf_vardef(ncid, 'E_BC', [weid,snid,stagid, timeid], /FLOAT)
ncdf_attput,ncid,'E_BC', 'FieldType',  /long, 104
ncdf_attput,ncid,'E_BC', 'MemoryOrder',/char,'XYZ'
ncdf_attput,ncid,'E_BC', 'description',/char,'EMISSIONS'
ncdf_attput,ncid,'E_BC', 'units',      /char,'ug m-2 s-1'
ncdf_attput,ncid,'E_BC', 'stagger',    /char,'Z'



ncdf_control,ncid, /endef

; write fields
ncdf_varput,ncid,'Times',timestring

ncdf_close,ncid

end



