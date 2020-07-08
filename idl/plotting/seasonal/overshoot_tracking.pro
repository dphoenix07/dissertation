PRO OVERSHOOT_TRACKING, event, scheme, start_date, end_date, $
	DOMAIN   = domain, $
	REGION   = region, $
	RAD	     = rad, $
	TROP_REL = trop_rel, $
	ECHO_TOP = echo_top, $
	MAP_PLOT = map_plot, $
	PNG	     = png, $
	EPS   	 = eps


;+
; Name:
;		OVERSHOOT_TRACKING
; Purpose:
;		Identifies overshoots and calculates quantities related to h2o transport within
;		radius of overshoots.
; Calling sequence:
;		OVERSHOOT_TRACKING, run, scheme, start_date, end_date
; Input:
;		event      : Model simulation name. (e.g., '20120519')
;		scheme 	   : Model initial state. Typically 'morrison' or 'nssl'.
;		start_date : String variable of the start date requested (e.g., '20120519T2300Z')
;		end_date   : String variable of the end date requested (e.g., '20120520T0300Z')
; Output:
;		Map of tropopause relative cloud tops with markers where cloud top > 1km above 
;		tropopause
; Keywords:
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		VERBOSE : If set, message writing process.
; Author and history:
;		Daniel B. Phoenix	    2018-06-28. 
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1											;Set default domain
IF (N_ELEMENTS(rad		 ) EQ 0) THEN rad		 = 0.65

outdir  = !WRF_DIRECTORY + event + '/' + scheme + '/reduced/overshoot_statistics/'
FILE_MKDIR, outdir

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(event, scheme, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(event, scheme, start_date, end_date, /DATE)	

unstable_layers_tot    = []
unstable_layers_ic     = []
overshoot_total        = []
max_cloud_total        = []
ave_cloud_total 	   = []
total_cloud_mass	   = []
max_ice_total     	   = []
ave_ice_total     	   = []
max_h2o_total          = []
ave_h2o_total          = []
max_h2o_nc_total       = []
ave_h2o_nc_total       = []
max_perc_h2o_total     = []
ave_perc_h2o_total     = []
max_perc_h2o_nc_total  = []
ave_perc_h2o_nc_total  = []
h2o_kg_strat		   = []
min_o3_total          = []
ave_o3_total  	      = []
max_co_total          = []
ave_co_total  	      = []
max_w_total 		  = []
ave_temp_total		  = []
mean_trop_cloud_total = []
trop_cloud_kg_total	  = []
mean_trop_refl_total  = []
updraft_size_total    = []
updraft_prev_total    = []
cyl_temp_tot		  = []
cyl_u_tot			  = []
cyl_v_tot			  = []
cyl_z_tot			  = []
cyl_dh2o_tot		  = []
mean_flow_u_5km	  = []
mean_flow_v_5km	  = []
zone1 			  = []
; Define radius (in degrees)
zr = rad
FOREACH date, date_arr DO BEGIN
	map_plot = 0

    y     = (WRF_READ_VAR('Latitude'       , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    x     = (WRF_READ_VAR('Longitude'      , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    z     = (WRF_READ_VAR('Z'		       , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    ztrop = (WRF_READ_VAR('Z_trop'	       , date, event, scheme, DOMAIN=domain, INDICES=region)).values	
	h2o   = (WRF_READ_VAR('H2O'			   , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E6	
	o3    = (WRF_READ_VAR('O3'			   , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E3	
	co    = (WRF_READ_VAR('CO'			   , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E3	
	temp  = (WRF_READ_VAR('T'  			   , date, event, scheme, DOMAIN=domain, INDICES=region)).values
	pres  = (WRF_READ_VAR('P'  			   , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E2
	u     = (WRF_READ_VAR('u'  			   , date, event, scheme, DOMAIN=domain, INDICES=region)).values
	v     = (WRF_READ_VAR('v'  			   , date, event, scheme, DOMAIN=domain, INDICES=region)).values
	w     = (WRF_READ_VAR('w'  			   , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    press = (WRF_READ_VAR('P'	           , date, event, scheme, DOMAIN=domain, INDICES=region)).values							
    temp  = (WRF_READ_VAR('T'	           , date, event, scheme, DOMAIN=domain, INDICES=region)).values						
 	R 	  =  WRF_READ_VAR('REFL'           , date, event, scheme, DOMAIN=domain, INDICES=region)
	cloud =  WRF_READ_VAR('CLOUD_MIX_TOTAL', date, event, scheme, DOMAIN=domain, INDICES=region)
  	qice  = (WRF_READ_VAR('ICE_MIX'        , date, event, scheme, DOMAIN=domain, INDICES=region)).values
	theta = WRF_READ_VAR('T', date, event, scheme, DOMAIN = domain)												;Read temperature variable from WRF output
	theta.values = ((1000.0/(WRF_READ_VAR('P', date, event, scheme, $											;Compute potential temperature
	 						DOMAIN = domain)).values)^(!Rair/!Cp))*(theta.values)
  	
  	horiz_wind  = SQRT(u^2 + v^2)
  	horiz_wind1 = SQRT(u^2 + v^2)
  	z_cyl    = z
  	ztrop1   = MEDIAN(ztrop, 100)
    dim 	 = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
    xyz_trop = REBIN(ztrop1, dim[0], dim[1], dim[2], /SAMPLE)
    yy       = REBIN(y,      dim[0], dim[1], dim[2], /SAMPLE)
    xx       = REBIN(x,      dim[0], dim[1], dim[2], /SAMPLE)

	molec_weight = 18.0
    r_star = 8.314
	dx = 3000.0
    
    air_kgm3   = ((press*1.0E2 * 28.97) / (r_star * temp))*1.0E-3*dx*dx
    h2o_ugm3   = ((h2o * press*1.0E2 * molec_weight) / (r_star * temp))*dx*dx*1.0E-9 
 	
 	;;!!!! I think I did something like this before, look in PBL -related code? !!!!
 	;;!!!! Add in calculations for vertical profiles here too!!!!!
 	perc_h2o = FLTARR(dim[0],dim[1],dim[2])*!Values.F_NaN
 	dh2o_dz  = FLTARR(dim[0],dim[1],dim[2])*!Values.F_NaN
 	cloud_kg = FLTARR(dim[0],dim[1],dim[2])*!Values.F_NaN
 	h2o_kg   = FLTARR(dim[0],dim[1],dim[2])*!Values.F_NaN
 	FOR ii = 0, dim[0]-1 DO BEGIN
 		FOR jj = 0, dim[1]-1 DO BEGIN
 			FOR kk = 0, dim[2]-3 DO BEGIN
 				h2o_kg[ii,jj,kk] = h2o_ugm3[ii,jj,kk]*(z[ii,jj,kk+1]-z[ii,jj,kk])	
 				air_kg = air_kgm3[ii,jj,kk]*(z[ii,jj,kk+1]-z[ii,jj,kk])	
 				perc_h2o [ii,jj,kk] = (h2o_kg[ii,jj,kk]/air_kg)*100.0 
 				h2o_kg_2 = h2o_ugm3[ii,jj,kk+1]*(z[ii,jj,kk+2]-z[ii,jj,kk+1])
 				dh2o_dz [ii,jj,kk] = (h2o_kg_2-h2o_kg[ii,jj,kk])/(z[ii,jj,kk+1]-z[ii,jj,kk])
 				cloud_kg[ii,jj,kk] = cloud.values[ii,jj,kk]*air_kg
 			ENDFOR
 		ENDFOR
 	ENDFOR
 	
  	z_cyl  = z
   	z_plot = z 
  	xyz_trop_plot = xyz_trop

 	;Find where cloud overshoots the tropopause by 1 km or greater
 	overshoot = 0.001*((MAX((cloud.values GE 1.0E-5)*z, DIM = 3, /NAN)) - xyz_trop)						;Set map variable to cloud top altitude
    over = WHERE((overshoot) GT 1.0, ov_count, COMPLEMENT = dud)	

 	;Find where echo tops overshoot the tropopause by 10 m or greater	
	IF KEYWORD_SET(echo_top) THEN BEGIN
		overshoot = 0.001*((MAX((R.values GE 5.0)*z, DIM = 3, /NAN)) - xyz_trop)
    	over = WHERE((overshoot) GE 0.01, ov_count, COMPLEMENT = dud)	
	ENDIF
     
    ;; Rebin 2D overshoot array to 3D space
    overshoot_3d = REBIN(overshoot, dim[0], dim[1], dim[2], /SAMPLE)
    over_3d = WHERE((overshoot_3d) GE 0.0, ov_count_3d, COMPLEMENT = dud_3d)	

	km5_array = FLTARR(dim[0],dim[1],dim[2])+5000.0
	;Find index of tropopause --> find temperature at the tropopause
	trop_temp 	  = FLTARR(dim[0],dim[1])*!Values.F_NaN
	trop_cloud 	  = FLTARR(dim[0],dim[1])*!Values.F_NaN
	trop_cloud_kg = FLTARR(dim[0],dim[1])*!Values.F_NaN
	trop_refl 	  = FLTARR(dim[0],dim[1])*!Values.F_NaN
	u_5km 		  = FLTARR(dim[0],dim[1])*!Values.F_NaN
	v_5km 		  = FLTARR(dim[0],dim[1])*!Values.F_NaN
	FOR ii = 0, dim[0]-1 DO BEGIN
		FOR jj = 0, dim[1]-1 DO BEGIN
			index5km = VALUE_LOCATE(REFORM(z[ii,jj,*],dim[2],1,1),REFORM(km5_array[ii,jj,*],dim[2],1,1))
			index 	 = VALUE_LOCATE(REFORM(z[ii,jj,*],dim[2],1,1),REFORM(xyz_trop[ii,jj,*],dim[2],1,1))
			trop_temp    [ii,jj] = temp[ii,jj,index[0]]
			trop_cloud   [ii,jj] = cloud.values[ii,jj,index[0]]
			trop_cloud_kg[ii,jj] = cloud_kg[ii,jj,index[0]]
			trop_refl    [ii,jj] = R.values[ii,jj,index[0]]
			u_5km		 [ii,jj] = u[ii,jj,index5km[0]]
			v_5km		 [ii,jj] = v[ii,jj,index5km[0]]
		ENDFOR
	ENDFOR
    
    ;; Create h2o, co, o3 arrays outside of cloud (want to compare cloud/non-cloud environments)
    cld = WHERE(cloud.values GE 1.0E-5, COMPLEMENT = out_cloud)
   
    h2o_nc  	        = h2o
    h2o_kg_nc		    = h2o_kg
    perc_h2o_nc         = perc_h2o
    o3_nc               = o3
    co_nc               = co
    theta_cd            = theta.values
  	h2o_kg_nc	 [cld]  = !Values.F_NaN
  	h2o_nc     	 [cld]  = !Values.F_NaN 
    perc_h2o_nc  [cld]  = !Values.F_NaN 
    o3_nc  		 [cld]  = !Values.F_NaN 
    co_nc   	 [cld]  = !Values.F_NaN 
    theta_cd[out_cloud] = !Values.F_NaN
     
    PRINT, date
    PRINT, 'qualified counts=', ov_count

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;For each overshoot identified, group neighboring overshoots within radius as a storm
	radius_list = [ ]
	FOR i = 0, ov_count-1 DO BEGIN
		IF (i GE ov_count) THEN BREAK
		PRINT, 'Working on overshoot #: ', i, ' of total: ', ov_count
	    
	    ;Calculate distance in between all overshoots, if distance is less than specified
	    ;radius, overshoots are grouped as a single storm
	    radius = SQRT((ABS(x[over[i]] - x[over]))^2 + (ABS(y[over[i]] - y[over]))^2) 	
	    storm  = WHERE(radius LT zr, storm_count)

		;Of overshoots grouped as a storm, find the center of the group
		IF (storm_count GT 0) THEN BEGIN
		     PRINT, 'max overshoot depth: ', MAX(overshoot[over[storm]])		     
		     center = WHERE(overshoot[over[storm]] EQ MAX(overshoot[over[storm]]))		     
		     IF (N_ELEMENTS(center)) GT 1 THEN list_add = over[storm[N_ELEMENTS(center)/2]]
		     IF (N_ELEMENTS(center)) EQ 1 THEN list_add = [over[storm[center]]]
		ENDIF
		
		PRINT, 'removing ', N_ELEMENTS(storm), ' points'	
		;Remove excess overshoots (only really care about the storm center)
		IF  (N_ELEMENTS(storm) EQ ov_count) THEN over = list_add
		IF ~(N_ELEMENTS(storm) EQ ov_count) THEN BEGIN
			REMOVE, storm, over
			over = [list_add, over]
		ENDIF

		;;Add in other points that aren't a part of a storm		
		ov_count = N_ELEMENTS(over)
		PRINT, 'new count = ', ov_count
	
	ENDFOR

	;Second sweep through to make sure all excess overshoots were removed
	;;;++++!!!!!DBP comment out temporarily so dead times could get produced
	PRINT, 'entering double check'
	FOR i = 0, ov_count-1 DO BEGIN
		IF (i GE ov_count) THEN BREAK
       radius = SQRT((ABS(x[over[i]] - x[over]))^2 + (ABS(y[over[i]] - y[over]))^2) 	
		storm  = WHERE(radius LT zr, storm_count)
		IF (storm_count GT 1) THEN BEGIN 
			center = WHERE(overshoot[over[storm]] EQ MAX(overshoot[over[storm]]))
			REMOVE, storm, over
			over = [[over[storm[center]]], over]
		ENDIF
		ov_count = N_ELEMENTS(over)
	ENDFOR
	;;;-end
	over1 = over
	over = [ ]
	over = over1[UNIQ(over1, SORT(over1))]
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;Set up cylinder around storm centers and calculate quantities 
	ov_count = N_ELEMENTS(over)
	FOR i = 0, ov_count-1 DO BEGIN
		;Create 3d radius for cylinder
	    radius = SQRT((ABS(x - x[over[i]]))^2 + (ABS(y - y[over[i]]))^2)	            
		radius_list = [[[radius_list]],[[radius]]]		
	
		;Zone is the cylinder around storms (for all altitudes)
		radius_3d = REBIN(radius,dim[0],dim[1],dim[2],/SAMPLE)
		zone   = WHERE(radius_3d LT zr, zone_count, COMPLEMENT = non_zone)	
		zone2d = WHERE(radius    LT zr, zone2d_count, COMPLEMENT = non_zone2d)
		
        ;; Top of volume is 1 km above overshoot
		z_top = 1000.0*(overshoot[over[i]] + 1.0) + ztrop1[over[i]]
        lms = WHERE((z - xyz_trop) GE 0.0 AND (z LE z_top), COMPLEMENT = outside) 

		;Added 01/08/2019
		cyl_bottom = ztrop1[over[i]]    
		cyl_top =  cyl_bottom + overshoot[over[i]]*1000.0 + 1000.0
		new_zone = WHERE((z[zone] GT cyl_bottom) AND (z[zone] LT cyl_top), COMPLEMENT=out)   
				
		iupdraft = WHERE(trop_refl[zone2d] GT 5.0, updraft_count)
		up_prev  = WHERE((trop_refl[zone2d] GT 5.0) AND (w[zone2d] GT 3.0), prev_count)
		;-end add

		;Altitudes corresponding to the cylinder
  		zstorms = FLTARR(dim[0],dim[1],dim[2])      
        zstorms [zone]= z[zone]
        zstorms [non_zone] = !Values.F_NaN
        
        ;zcylinder is altitudes in cylinders around storms corresponding to the lms layer 
        ;lms layer is from top of overshoot to 1km above overshoot (for all x,y)
        zcylinder = zstorms[lms]
        zcylinder = zcylinder[WHERE(FINITE(zcylinder))]			;altitudes in the cylinder
        tempcylinder = temp[WHERE(FINITE(zcylinder))]			;temperatures in the cylinder 
        
		;Find dtemp/dz in cylinder where dz = 1m
        ;dtdz = FLTARR(N_ELEMENTS(zcylinder))
        ;dz = 1.0														;dz = 1m
        ;temp_sort = tempcylinder[sort(zcylinder)]						;temperatures in cylinder sorted by altitude (lowest z - highest z)
		;FOR zz = 0, N_ELEMENTS(zcylinder)-2 DO BEGIN
		;	dtdz[zz] = (temp_sort[zz+1] - temp_sort[zz]) / dz     
		;ENDFOR
        ;End dt/dz
        
        ;Set points outside cylinder to NaN (this is not necessary when using the [zone[new_zone]]
        ;method, might have been set up to preserve indexing for calculating profiles   
        h2o         [outside] = !Values.F_NaN
        perc_h2o    [outside] = !Values.F_NaN
        dh2o_dz		[outside] = !Values.F_NaN
        co          [outside] = !Values.F_NaN
        o3          [outside] = !Values.F_NaN
        qice        [outside] = !Values.F_NaN 
        cloud.values[outside] = !Values.F_NaN  
		cloud_kg	[outside] = !Values.F_NaN
		theta.values[outside] = !Values.F_NaN
		theta_cd    [outside] = !Values.F_NaN
		temp  	    [outside] = !Values.F_NaN
		horiz_wind  [outside] = !Values.F_NaN
		
		;Attempt to calculate RHi (this needs to be looked at-->it is not correct)
		;es  = 611.20 * EXP((2.834E6/461.5)*((1/273.15) - (1/temp[zone])))
		;ws  = 0.622  * (es / (pres[zone] - es))
		;rhi = (qice[zone] / ws) * 100.0	
	
		;;01/08/2019 I think we want the zone (which is the cylinder around storms) and 
		;;the LMS indices combined from here on
		;;If high H2O concentration around storm, print these quantities to txt file
		IF (MAX (h2o[zone],/NAN) GT 120.0) THEN BEGIN
			temp_date = MAKE_ISO_DATE_STRING(date,/COMPACT,/UTC)
		    fname = outdir+temp_date+'_storm_id_' + STRMID(STRING(i),11) + '.txt'
		    OPENW, lun, fname, /GET_LUN     
		    PRINTF, lun, date
		    PRINTF, lun, 'storm number = ', STRING(i)
		    PRINTF, lun, 'Max Overshooting Depth         : ' + STRING(MAX(overshoot[over [i]],/NAN))
			PRINTF, lun, 'Max h2o concentration          : ' + STRING(MAX  (h2o      [zone[new_zone]  ],/NAN))
		    PRINTF, lun, 'Mean h2o concentration         : ' + STRING(MEAN (h2o      [zone[new_zone]  ],/NAN))
			PRINTF, lun, 'Max percent h2o concentration  : ' + STRING(MAX  (perc_h2o [zone[new_zone]  ],/NAN))
		    PRINTF, lun, 'Mean percent h2o concentration : ' + STRING(MEAN (perc_h2o [zone[new_zone]  ],/NAN))
			PRINTF, lun, 'Total h2o mass (kg) 			 : ' + STRING(TOTAL(h2o_kg_nc[zone[new_zone]  ],/NAN))
			;PRINTF, lun, 'Max rhi concentration         : ' + STRING(MAX (rhi               ,/NAN))
		    ;PRINTF, lun, 'Mean rhi concentration        : ' + STRING(MEAN(rhi               ,/NAN))
			PRINTF, lun, 'Max co concentration           : ' + STRING(MAX  (co       [zone[new_zone]  ],/NAN))
		    PRINTF, lun, 'Mean co concentration          : ' + STRING(MEAN (co       [zone[new_zone]  ],/NAN))
			PRINTF, lun, 'Min o3 concentration           : ' + STRING(MIN  (o3       [zone[new_zone]  ],/NAN))
		    PRINTF, lun, 'Mean o3 concentration          : ' + STRING(MEAN (o3       [zone[new_zone]  ],/NAN))
		    PRINTF, lun, 'Mean temperature (K)           : ' + STRING(MEAN (trop_temp[over[i	   ]  ],/NAN)) 
		    PRINTF, lun, 'Max updraft speed (m/s)        : ' + STRING(MAX  (w        [zone[new_zone]  ],/NAN))
			FREE_LUN, lun

		    PRINT, 'storm number: ', i
		    PRINT, 'Max Overshooting Depth         : ', MAX (overshoot[over[i]],/NAN)
		    PRINT, 'Max  h2o concentration         : ', MAX (h2o      [zone [new_zone]  ],/NAN)
		    PRINT, 'Mean h2o concentration         : ', MEAN(h2o      [zone [new_zone]  ],/NAN)
			PRINT, 'Max percent h2o concentration  : ', MAX (perc_h2o [zone [new_zone]  ],/NAN)
		    PRINT, 'Mean percent h2o concentration : ', MEAN(perc_h2o [zone [new_zone]  ],/NAN)
		    PRINT, 'Total h2o mass (kg) 		   : ', TOTAL(h2o_kg_nc[zone[new_zone]  ],/NAN)
			;PRINT, 'Max  rhi concentration        : ', MAX (rhi               ,/NAN)
		    ;PRINT, 'Mean rhi concentration        : ', MEAN(rhi               ,/NAN)
		    PRINT, 'Mean temperature (K)           : ', MEAN(trop_temp[over[i]],/NAN) 
		    PRINT, 'Max updraft speed (m/s)        : ', MAX (w		[zone [new_zone]  ],/NAN)
		ENDIF
	
		theta.values[non_zone] = !Values.F_NaN
		theta_cd    [non_zone] = !Values.F_NaN
		temp   		[non_zone] = !Values.F_NaN
		horiz_wind  [non_zone] = !Values.F_NaN
		
		unstable_ic  = FLTARR(dim[0],dim[1],dim[2])
		unstable_tot = FLTARR(dim[0],dim[1],dim[2])

		;checks = FLTARR(dim[0],dim[1])
		;FOR xx = 0, dim[0]-1 DO BEGIN
		;	FOR yy = 0, dim[1]-1 DO BEGIN
		;		checks[xx,yy] = WHERE(FINITE(theta.values[xx,yy,*]))
		;	ENDFOR
		;ENDFOR
		;
		;STOP
		
		mean_t    = FLTARR(dim[2])
		mean_wind = FLTARR(dim[2])
		mean_flow = FLTARR(dim[2])
		
		;;Find profiles of temp, wind, num of unstable layers in-cloud and total
		;;01/08/19 this is be a time-consuming part of the code
		;;01/11/19 consider doing this at the beginning of the code where dh2o/dz is calculated
		FOR ii=0, dim[0]-1 DO BEGIN
			FOR jj=0,dim[1]-1 DO BEGIN
;				check = WHERE(FINITE(theta.values[ii,jj,*]))
;				IF (check EQ -1) THEN jj = jj+1
				FOR kk = 0, dim[2]-2 DO BEGIN
					dth_ic  = (theta_cd[ii,jj,kk+1] - theta_cd[ii,jj,kk])
					dth_tot = (theta.values[ii,jj,kk+1] - theta.values[ii,jj,kk])
						IF (dth_ic  LT 0.0) THEN unstable_ic [ii,jj,kk] = 1
						IF (dth_tot LT 0.0) THEN unstable_tot[ii,jj,kk] = 1
;					mean_t   [kk] = MEAN(temp      [*,*,kk],/NAN)
;					mean_wind[kk] = MEAN(horiz_wind[*,*,kk],/NAN)						
;					mean_flow_k   = INDEX_OF_NEAREST(z[ii,jj,*], 5000.0) 
;					mean_flow[kk] = MEAN(horiz_wind1[ii,jj,mean_flow_k-4:mean_flow_k+4])			
				ENDFOR
			ENDFOR
			PRINT, 'ov_count= ', i
			PRINT, ii
		ENDFOR
		;;-end profile calculation
		
		PRINT, 'Number of unstable layers: ', TOTAL(unstable_tot)
		PRINT, 'storm number: ', i
						
		;;Create storm arrays if doing multiple storms
		;;01/08/19 using the zone index, I think we want zone + lms
;		mean_temp 		    = [mean_temp, MEAN(mean_t,/NAN)]
;		mean_utls_wind      = [mean_utls_wind, MEAN(mean_wind,/NAN)]
;		mean_storm_motion   = [mean_storm_motion, MEAN(mean_flow,/NAN)]
		
		unstable_layers_tot    = [unstable_layers_tot	  , TOTAL(unstable_tot)]
		unstable_layers_ic     = [unstable_layers_ic 	  , TOTAL(unstable_ic )]
		overshoot_total        = [overshoot_total   	  , MAX(overshoot[over[i]])]
		max_cloud_total        = [max_cloud_total         , MAX (cloud.values[zone[new_zone]],/NAN)]
		ave_cloud_total 	   = [ave_cloud_total         , MEAN(cloud.values[zone[new_zone]],/NAN)]
		total_cloud_mass	   = [total_cloud_mass		  , TOTAL(cloud_kg[zone[new_zone]],/NAN)]
		max_ice_total     	   = [max_ice_total           , MAX (qice[zone[new_zone]]  ,/NAN)]
		ave_ice_total     	   = [ave_ice_total           , MEAN(qice[zone[new_zone]]  ,/NAN)]
		max_h2o_total          = [max_h2o_total           , MAX (h2o[zone[new_zone]]  ,/NAN)]
		ave_h2o_total          = [ave_h2o_total           , MEAN(h2o[zone[new_zone]]  ,/NAN)]
		max_h2o_nc_total       = [max_h2o_nc_total        , MAX (h2o_nc[zone[new_zone]],/NAN)]
		ave_h2o_nc_total       = [ave_h2o_nc_total        , MEAN(h2o_nc[zone[new_zone]],/NAN)]
		max_perc_h2o_total     = [max_perc_h2o_total      , MAX (perc_h2o[zone[new_zone]]  ,/NAN)]
		ave_perc_h2o_total     = [ave_perc_h2o_total      , MEAN(perc_h2o[zone[new_zone]]  ,/NAN)]
		max_perc_h2o_nc_total  = [max_perc_h2o_nc_total   , MAX (perc_h2o_nc[zone[new_zone]],/NAN)]
		ave_perc_h2o_nc_total  = [ave_perc_h2o_nc_total   , MEAN(perc_h2o_nc[zone[new_zone]],/NAN)]
		h2o_kg_strat		   = [h2o_kg_strat		  	  ,TOTAL(h2o_kg_nc[zone[new_zone]  ],/NAN)]
		
		;max_rhi             = [max_rhi            , MAX (rhi        ,/NAN)]
		;mean_rhi  	        = [mean_rhi           , MEAN(rhi        ,/NAN)]
		
		min_o3_total          = [min_o3_total             , MIN (o3[zone[new_zone]]   ,/NAN)]
		ave_o3_total  	      = [ave_o3_total             , MEAN(o3[zone[new_zone]]   ,/NAN)]
		max_co_total          = [max_co_total             , MAX (co[zone[new_zone]]   ,/NAN)]
		ave_co_total  	      = [ave_co_total             , MEAN(co[zone[new_zone]]   ,/NAN)]
		max_w_total 		  = [max_w_total              , MAX (w		 [zone[new_zone]	],/NAN)]
		ave_temp_total		  = [ave_temp_total           , MEAN(trop_temp[zone2d],/NAN)] 
		mean_trop_cloud_total = [mean_trop_cloud_total	  , MEAN(trop_cloud[zone2d],/NAN)]
		trop_cloud_kg_total	  = [trop_cloud_kg_total  	  , TOTAL(trop_cloud_kg[zone2d],/NAN)]
		mean_trop_refl_total  = [mean_trop_refl_total	  , MEAN(trop_refl[zone2d],/NAN)]
		updraft_size_total    = [updraft_size_total  	  , updraft_count]
		updraft_prev_total    = [updraft_prev_total 	  , prev_count]
		cyl_temp_tot		  = [cyl_temp_tot			  , temp[zone[new_zone]]]
		cyl_u_tot			  = [cyl_u_tot				  , u[zone[new_zone]]]
		cyl_v_tot			  = [cyl_v_tot				  , v[zone[new_zone]]]
		cyl_z_tot			  = [cyl_z_tot				  , z_cyl[zone[new_zone]]]
		cyl_dh2o_tot		  = [cyl_dh2o_tot			  , dh2o_dz[zone[new_zone]]]
         mean_flow_u_5km	  = [ mean_flow_u_5km         , MEAN(u_5km,/NAN)]
         mean_flow_v_5km	  = [ mean_flow_v_5km         , MEAN(v_5km,/NAN)]
		
		PRINT,'done creating storm arrays'
		
		IF ((MAX (h2o[zone],/NAN)) GT 120.0) THEN zone1 = [zone1, over[i]]						;For plotting star
	ENDFOR
	
	PRINT, 'moving on'
	
;;+start
	;Don't want to create time dimension in arrays, want to write each time to file
	;unstable_total_tot    = unstable_layers_tot
	;unstable_total_ic     = unstable_layers_ic 
	;overshoot_total       = overshoot_depth    
	;max_h2o_total         = max_h2o 	       
	;ave_h2o_total         = mean_h2o 	       
	;max_h2o_nc_total      = max_h2o_nc 	   
	;ave_h2o_nc_total      = mean_h2o_nc 	   
	;max_perc_h2o_total    = max_perc_h2o 	       
	;ave_perc_h2o_total    = mean_perc_h2o 	       
	;max_perc_h2o_nc_total = max_perc_h2o_nc 	   
	;ave_perc_h2o_nc_total = mean_perc_h2o_nc 	
	;max_cloud_total       = max_cloud 	       
	;ave_cloud_total       = mean_cloud 	   
	;total_cloud_mass 	  = total_cloud
	;max_ice_total         = max_ice 	       
	;ave_ice_total         = mean_ice 	       
	;;max_rhi_total         = max_rhi 	       
	;;ave_rhi_total         = mean_rhi 	       
	;max_co_total          = max_co 	       
	;ave_co_total          = mean_co 	       
	;min_o3_total          = min_o3 	       
	;ave_o3_total          = mean_o3 	       
	;ave_temp_total        = mean_temp         
	;max_w_total	          = max_w		       
	;mean_trop_cloud_total = mean_trop_cloud	
	;trop_cloud_kg_total	  = trop_cloud_kg_tot
	;mean_trop_refl_total  = mean_trop_refl	
	;cyl_temp_tot		  = cyl_temp
    ;cyl_u_tot			  = cyl_u	
    ;cyl_v_tot			  = cyl_v	
    ;cyl_z_tot			  = cyl_z	
    ;cyl_dh2o_tot		  = cyl_dh2o
    ;mean_flow_u_5km		  = flow_u_5km
    ;mean_flow_v_5km		  = flow_v_5km
    ;updraft_size_total    = updraft_size
    ;updraft_prev_total    = updraft_prevalence
;;-end	

;	h2o_zone        = [h2o_zone		  , zone1		   ]


;	PLOT, overshoot_depth[sort(overshoot_depth)], max_h2o[sort(overshoot_depth)], $
;				xtitle = 'Overshoot Depth (km)', ytitle = "H2O concentration (ppmv)", PSYM = 4
;
;	OPLOT, overshoot_depth[sort(overshoot_depth)], mean_h2o[sort(overshoot_depth)], COLOR = COLOR_24('red'), PSYM = 4
				
	;IF (MAX(max_h2o, /NAN) GT 120.0) THEN map_plot = 1 

;;Do map plot if keyword set, this is a good check to make sure targeting correct areas	
IF (KEYWORD_SET(map_plot)) THEN BEGIN
	PRINT, 'map plt selected'
	;png = 1 
	rad_dim = SIZE(radius_list,/DIMENSIONS)
	IF (N_ELEMENTS(rad_dim) EQ 2) THEN radius_final = radius_list
	IF (N_ELEMENTS(rad_dim) EQ 3) THEN radius_final = MIN(radius_list,DIM=3)
	
	zone = WHERE(radius_final LT 0.5, zone_count)

    bad = WHERE (R.values EQ 0.0, bad_count, COMPLEMENT = good, NCOMPLEMENT = good_count)
    R.values [bad ] = -35.0000
    R.values [good] = R.values [good]
    
    dim = SIZE(x, /DIMENSIONS)																		;Get dimension sizes
    
    offset = 0
    y0 = y[          offset ,          offset ]														;Set domain boundary points
    y1 = y[          offset ,dim[1]-(1+offset)]
    y2 = y[dim[0]-(1+offset),dim[1]-(1+offset)]
    y3 = y[dim[0]-(1+offset),          offset ]
    x0 = x[          offset ,          offset ]
    x1 = x[          offset ,dim[1]-(1+offset)]
    x2 = x[dim[0]-(1+offset),dim[1]-(1+offset)]
    x3 = x[dim[0]-(1+offset),          offset ]
    
    xc = INTERPOLATE(x, 0.5*(dim[0]-1), 0.5*(dim[1]-1))												;Get central grid point
    yc = INTERPOLATE(y, 0.5*(dim[0]-1), 0.5*(dim[1]-1))
    
    table   = [COLOR_24(200, 200, 200),VISUALIZE_88D_COLOR()]										;Set reflectivity color table
    rlevels = [-100.0, 5.0*FINDGEN(N_ELEMENTS(table))]												;Set reflectivity contour levels
    ;rlevels = [-100.0, 5.0 + 5.0*FINDGEN(N_ELEMENTS(table))]										;***for comparison with t-matrix
    wfactor = 400.0/(dim[0]) + 400.0/(dim[1])														;Set map factor
    
    dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")												;Set domain string
    date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Create date string
    
    ;map_pos = [0.0, 0.0, 1.0, 1.0]																	;map position for paper
    map_pos = [0.05, 0.15, 0.95, 0.95]																;Set map position
    bar_pos = [0.25, 0.10, 0.75, 0.12]																;Set color bar position
    
    IF KEYWORD_SET(z_buff) THEN BEGIN
    	SET_PLOT, 'Z'																									;Output to Z buffer
    	DEVICE, SET_PIXEL_DEPTH = 24, SET_RESOLUTION = [wfactor*(dim[0]), wfactor*(dim[1])], $	;Set device resolution and bit depth
    		SET_CHARACTER_SIZE = [12, 20]
    	!P.COLOR      = COLOR_24('black')																		;Foreground color
    	!P.BACKGROUND = COLOR_24('white')																		;Background color
    	!P.CHARSIZE   = 1.5																							;Set character size
    	!P.FONT       = -1
    ENDIF ELSE BEGIN
    	IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN	
    		PS_ON, FILENAME = epsfile, PAGE_SIZE = [4.0,4.0], MARGIN = 0.0, /INCHES;PAGE_SIZE = 0.001*dim*wfactor			;Switch to Postscript device
    		DEVICE, /ENCAPSULATED
    		!P.FONT     = 0																								;Hardware fonts
    		!P.CHARSIZE = 0.75	
    		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
    			LOAD_BASIC_COLORS																							;Load basic color definitions
    	ENDIF ELSE BEGIN
    		SET_PLOT, 'X'
    		WINDOW, XSIZE = wfactor*(dim[0]), YSIZE = wfactor*(dim[1])										;Open graphics window
    		!P.COLOR      = COLOR_24('black')																		;Foreground color
    		!P.BACKGROUND = COLOR_24('white')																		;Background color
    		!P.CHARSIZE   = 2.0		
    		!P.FONT       = -1		
    		thick_scale   = 1
    	ENDELSE
    ENDELSE
    
    MAP_SET, yc, xc, 0, CONIC = 1, /NOBORDER, $																;Draw map
    	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
    	ISOTROPIC = 1, $
    	TITLE     = 'WRF-' + scheme + ' ' + dom_string + ' valid ' + date_string, $
    	POSITION  = map_pos
    
    IF KEYWORD_SET(image) THEN BEGIN
    	ij0 = CONVERT_COORD([(!X.WINDOW)[0],(!Y.WINDOW)[0]], /NORMAL, /TO_DEVICE)
    	ij1 = CONVERT_COORD([(!X.WINDOW)[1],(!Y.WINDOW)[1]], /NORMAL, /TO_DEVICE)
    
    	xsize = LONG(ij1[0] - ij0[0])
    	ysize = LONG(ij1[1] - ij0[1])
    
    	image0 = (MAX(R.values, DIM=3))[offset:(dim[0]-(1+offset)), offset:(dim[1]-(1+offset))]
    	dim    = dim - (2*offset)
    	
    	IF KEYWORD_SET(eps) THEN $
    		image0 = INTERPOLATE(image0, MAKEN(0, dim[0]-1, xsize/10), MAKEN(0, dim[1]-1, ysize/10), /GRID) ELSE $
    		image0 = INTERPOLATE(image0, MAKEN(0, dim[0]-1, xsize   ), MAKEN(0, dim[1]-1, ysize   ), /GRID)
    
    	image0 = IMAGE_24(COLOR_LOOKUP_24((image0 < 75.0), table[1:*], MIN = 0.0, MAX = 75.0, $
    				MISSING = COLOR_24(200, 200, 200), /NAN))
    	TV, image0, ij0[0], ij0[1], TRUE = 3, XSIZE = xsize, YSIZE = ysize, /DEVICE
    ENDIF ELSE $	
    
    	 map_plot      = 0.001*(MAX((cloud.values GE 1.0E-5)*z, DIM = 3, /NAN))						;Set map variable to cloud top altitude
    	 map_bar_title = 'Cloud Top Altitude (km)'														;Set color bar title
    	 map_bar_min   = 5.0																						;Set echo top minimum
    	 map_bar_max   = 20.0																					;Set echo top maximum
    	 map_bar_ticks = 3																						;Set number of color bar ticks
    	 map_table     = [COLOR_24(230, 230, 230),VISUALIZE_88D_COLOR(0)]							;Set color table
    	 map_levels    = [0.0, 5.0 + FINDGEN(N_ELEMENTS(map_table))]								;Set contour levels
    
    	IF (KEYWORD_SET(TROP_REL)) THEN BEGIN
    		map_plot      = 0.001*((MAX((cloud.values GE 1.0E-5)*z, DIM = 3, /NAN)) - xyz_trop)						;Set map variable to cloud top altitude
    		
    		bad = WHERE(FINITE(map_plot,/NAN), bad_count, COMPLEMENT=good, NCOMPLEMENT=good_count)
    		IF (bad_count GT 0) THEN BEGIN
    			map_plot[bad ] = -100.0
    			map_plot[good] = map_plot[good]
    		ENDIF
    		
    	 	map_bar_title = 'Cloud Top Relative-Altitude (km)'														;Set color bar title
    	 	map_bar_min   = -2.5																						;Set echo top minimum
    	 	map_bar_max   = 2.5																				;Set echo top maximum
    	 	map_bar_ticks = 2																						;Set number of color bar ticks
    	 	map_table     = [COLOR_24('gray90'), BLUE_RED_24(30,0.0)]							          					    ;Set color table
    	 	map_levels    = [-100.0, MAKEN(-2.5,2.5,30)]  									              ;Set contour levels																					;Set number of color bar ticks
    ;		map_table 	  = BLUE_RED_24(30,0.0)
    ;	 	map_levels    = [MAKEN(-2.5,2.5,30)]  									              ;Set contour levels
    	ENDIF
    
    	IF (KEYWORD_SET(ECHO_TOP)) THEN BEGIN
    		echo = 5.0
    		map_plot = ((MAX((R.values GE echo)*z_plot, DIM = 3)) - xyz_trop_plot) * 0.001
    		
    		bad = WHERE(FINITE(map_plot,/NAN), bad_count, COMPLEMENT=good, NCOMPLEMENT=good_count)
    		IF (bad_count GT 0) THEN BEGIN
    			map_plot[bad ] = -100.0
    			map_plot[good] = map_plot[good]
    		ENDIF
    		
    	 	map_bar_title = 'Echo Top Relative Altitude (km)'														;Set color bar title
    	 	map_bar_min   = -2.5																						;Set echo top minimum
    	 	map_bar_max   = 2.5																				;Set echo top maximum
    	 	map_bar_ticks = 2																						;Set number of color bar ticks
    	 	map_table     = [COLOR_24('gray90'), BLUE_RED_24(30,0.0)]							          					    ;Set color table
    	 	map_levels    = [-100.0, MAKEN(-2.5,2.5,30)]  									              ;Set contour levels																					;Set number of color bar ticks
    	 	;map_bar_min   = 5.0																						;Set echo top minimum
    	 	;map_bar_max   = 20.0																					;Set echo top maximum
    	 	;map_bar_ticks = 3																						;Set number of color bar ticks
    		;map_table     = [COLOR_24(230, 230, 230),VISUALIZE_88D_COLOR(0)]							;Set color table
    	 	;map_levels    = [0.0, 5.0 + FINDGEN(N_ELEMENTS(map_table))]								;Set contour levels
    	ENDIF
    
    	CONTOUR, map_plot, x, y, $																	;Contour values
            OVERPLOT  = 1, $
            FILL      = 1, $
            LEVELS    = map_levels, $
            C_COLOR   = map_table, $
            TITLE     = date_string, $
            POSITION  = map_pos
    
    ;;; to plot reflectivity
    ;	CONTOUR, MAX(R.values, DIM=3), x, y, $															;Contour reflectivity values
    ;		OVERPLOT  = 1, $
    ;		FILL      = 1, $
    ;		LEVELS    = rlevels, $
    ;		C_COLOR   = table
    ;;; end
    
    ;Circles show overshoots
    USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation
    
    labels = INDGEN(ov_count)

    IF (ov_count GT 0) THEN BEGIN
    	FOR i=0, ov_count-1 DO BEGIN
    		PLOTS, (x)[over[i]], (y)[over[i]], $																		;Overplot plane symbol
    			PSYM    = 8, $
    			SYMSIZE = 4 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
    			NOCLIP  = 0, $
    			COLOR   = COLOR_24('black')
    		XYOUTS, (x)[over[i]], (y)[over[i]], labels[i], /DATA
    	ENDFOR
    ENDIF

	;Stars show storm centers
   ;USERSYM_STAR, /FILL																					;Load plane symbol at flight path orientation
;
   ;IF (N_ELEMENTS(zone1) GT 0) THEN BEGIN
   ;	FOR i=0, N_ELEMENTS(zone1)-1 DO BEGIN
   ;		PLOTS, (x)[zone1[i]], (y)[zone1[i]], $																		;Overplot plane symbol
   ;			PSYM    = 8, $
   ;			SYMSIZE = 4 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
   ;			NOCLIP  = 0, $
   ;			COLOR   = COLOR_24('red')
   ;		;XYOUTS, (xx)[over[i]], (y)[over[i]], labels[i], /DATA
   ;	ENDFOR
   ;ENDIF

 	;comment out
   ;IF (zone_count GT 0) THEN BEGIN
   ;	FOR i=0, zone_count-1 DO BEGIN
   ;		PLOTS, (x)[zone[i]], (y)[zone[i]], $																		;Overplot plane symbol
   ;			PSYM    = 8, $
   ;			SYMSIZE = 4 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
   ;			NOCLIP  = 0, $
   ;			COLOR   = COLOR_24('black')
   ;		XYOUTS, (x)[zone[i]], (y)[zone[i]], labels[i], /DATA
   ;	ENDFOR
   ;ENDIF
    
    MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines
    
    IF KEYWORD_SET(section) THEN BEGIN
    	IF (run EQ '20110408') THEN ij = [100, 119, 300, 157]
    	IF (run EQ '20110521') THEN ij = [150, 116, 350, 130]
    	IF (run EQ '20110618') THEN ij = [040, 060, 240, 080]
    
    	xysect = MAP_2POINTS((x)[ij[0],ij[1]],(y)[ij[0],ij[1]],$
    								(x)[ij[2],ij[3]],(y)[ij[2],ij[3]], NPATH = 10)
    	
    	OPLOT, xysect[0,*], xysect[1,*], THICK = 4
    	XYOUTS, xysect[0,0], xysect[1,0], 'A', ALIGN = 1
    	XYOUTS, xysect[0,-1], xysect[1,-1], 'B', ALIGN = 0
    ENDIF
    
    MAP_SET, yc, xc, 0, CONIC = 1, $																				;Redraw map boundaries
    	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
    	ISOTROPIC = 1, $
    	NOERASE   = 1, $
    	POSITION  = map_pos
    	
    COLOR_BAR_24_KPB, map_table[1:*], OVER = map_table[-1], UNDER = map_table[0], $						;Draw map color bar
    	TICKS = map_bar_ticks, $
    	RANGE = [map_bar_min, map_bar_max], $
    	TITLE = map_bar_title, $
    	POSIT = bar_pos
    
    ;;; to plot reflectivity (comment out)
    ;COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
    ;	RANGE = [0, 75], $
    ;	TICKS = 5, $
    ;	TITLE = 'Reflectivity (dBZ)', $
    ;	POSIT = bar_pos
    ;;; end
    
    date1   = MAKE_ISO_DATE_STRING(date,/COMPACT,/UTC)
    
    epsfile = outdir +  date1  + '.eps'						;EPS filename
    pdffile = outdir +  date1  + '.pdf'						;PDF filename
    pngfile = outdir +  date1  + '.png'						;PNG filename
    
    IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
    	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
    		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
    	PS_OFF																											;Turn PS off
    	
    	IF KEYWORD_SET(pdf) THEN PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS						;Convert to PDF
    ENDIF ELSE IF KEYWORD_SET(png) THEN $
    	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file
ENDIF  ;; map plot

	PRINT, 'start writing file'
	;;Write values to file for later plotting (using plot_overshoot_relationships.pro)
	IF (domain EQ 1) THEN domain1 = 'd01'
	time    = MAKE_ISO_DATE_STRING(date, PRECISION='minute', /COMPACT, /UTC)
	infile  = !WRF_DIRECTORY + event + '/' + scheme + '/reduced/' + domain1 + '_' + time + '.nc'						;Set input filepath
	outdir  = !WRF_DIRECTORY + event + '/' + scheme + '/reduced/overshoot_statistics/'
	outfile = STRMID(infile, 16, /REVERSE_OFFSET)														;Set output file name
	FILE_MKDIR, outdir
	
	iid = NCDF_OPEN(infile)																						;Open input file for reading
	NCDF_VARGET, iid, 'T', values																				;Read single variable for output file definition
	NCDF_ATTGET, iid, 'DX', dx, /GLOBAL																		;Read grid resolution
	NCDF_ATTGET, iid, 'DT', dt, /GLOBAL																		;Read grid resolution
    
	dim = SIZE(values, /DIMENSIONS)																			;Get grid dimension sizes
	CATCH, error_status																								;Catch any errors with netcdf control or file creation
    
	IF (error_status NE 0) THEN BEGIN
		NCDF_CLOSE, oid																								;Close previous failed file
		oid = NCDF_CREATE(outdir + outfile, CLOBBER = 1, /NETCDF3_64BIT)								;Create output file for writing
	ENDIF ELSE $
		oid = NCDF_CREATE(outdir + outfile, CLOBBER = 1, /NETCDF3_64BIT)								;Create output file for writing

	xid  = NCDF_DIMDEF(oid, 'x', dim[0])																			;Define output file dimensions
	yid  = NCDF_DIMDEF(oid, 'y', dim[1])	
	zid  = NCDF_DIMDEF(oid, 'z', dim[2])	
	tid  = NCDF_DIMDEF(oid, 't', 14    )	
	nid  = NCDF_DIMDEF(oid, 'n', N_ELEMENTS(max_h2o_total))
	n2id = NCDF_DIMDEF(oid,'n2', N_ELEMENTS(cyl_u_tot))
	
	vid = NCDF_VARDEF(oid, 'Time', [tid], /CHAR)																;Define the time variable
	NCDF_ATTPUT, oid, 'Time', 'long_name', 'ISO Date String'												;Name attribute
	NCDF_ATTPUT, oid, 'Time', 'units',     'YYYYMMDD_HHMM_'												;Units attribute
	
	vid = NCDF_VARDEF(oid, 'Max_Overshoot', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Max_Overshoot', 'long_name', 'Maximum Overshooting Depth'						;Name attribute
	NCDF_ATTPUT, oid, 'Max_Overshoot', 'units',     'km'												;Units attribute

;	vid = NCDF_VARDEF(oid, 'Mean_Temp', [nid,zid], /FLOAT)													;Define the latitude variable
;	NCDF_ATTPUT, oid, 'Mean_Temp', 'long_name', 'Mean Temp Profile'						;Name attribute
;	NCDF_ATTPUT, oid, 'Mean_Temp', 'units',     'K'												;Units attribute
;
;	vid = NCDF_VARDEF(oid, 'Mean_UTLS_Wind', [nid,zid], /FLOAT)													;Define the latitude variable
;	NCDF_ATTPUT, oid, 'Mean_UTLS_Wind', 'long_name', 'Mean UTLS Wind Profile'						;Name attribute
;	NCDF_ATTPUT, oid, 'Mean_UTLS_Wind', 'units',     'm/s'												;Units attribute
;
;	vid = NCDF_VARDEF(oid, 'Mean_Storm_Motion', [nid], /FLOAT)													;Define the latitude variable
;	NCDF_ATTPUT, oid, 'Mean_Storm_Motion', 'long_name', 'Mean Wind at 5 km'						;Name attribute
;	NCDF_ATTPUT, oid, 'Mean_Storm_Motion', 'units',     'm/s'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Unstable_Layers', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Unstable_Layers', 'long_name', 'Number of Unstable Layers'						;Name attribute
	NCDF_ATTPUT, oid, 'Unstable_Layers', 'units',     '#'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Unstable_Layers_IC', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Unstable_Layers_IC', 'long_name', 'Number of Unstable Layers In Cloud'						;Name attribute
	NCDF_ATTPUT, oid, 'Unstable_Layers_IC', 'units',     '#'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Max_H2O', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Max_H2O', 'long_name', 'Max Water Vapor Concentration'						;Name attribute
	NCDF_ATTPUT, oid, 'Max_H2O', 'units',     'ppmv'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Mean_H2O', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Mean_H2O', 'long_name', 'Mean Water Vapor Concentration'						;Name attribute
	NCDF_ATTPUT, oid, 'Mean_H2O', 'units',     'ppmv'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Max_H2O_NC', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Max_H2O_NC', 'long_name', 'Max Water Vapor Concentration Outside Cloud'						;Name attribute
	NCDF_ATTPUT, oid, 'Max_H2O_NC', 'units',     'ppmv'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Mean_H2O_NC', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Mean_H2O_NC', 'long_name', 'Mean Water Vapor Concentration Outside Cloud'						;Name attribute
	NCDF_ATTPUT, oid, 'Mean_H2O_NC', 'units',     'ppmv'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Max_Perc_H2O', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Max_Perc_H2O', 'long_name', 'Max Percent Water Vapor'						;Name attribute
	NCDF_ATTPUT, oid, 'Max_Perc_H2O', 'units',     '%'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Mean_Perc_H2O', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Mean_Perc_H2O', 'long_name', 'Mean Percent Water Vapor'						;Name attribute
	NCDF_ATTPUT, oid, 'Mean_Perc_H2O', 'units',     '%'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Max_Perc_H2O_NC', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Max_Perc_H2O_NC', 'long_name', 'Max Percent Water Vapor Outside Cloud'						;Name attribute
	NCDF_ATTPUT, oid, 'Max_Perc_H2O_NC', 'units',     '%'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Mean_Perc_H2O_NC', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Mean_Perc_H2O_NC', 'long_name', 'Mean Percent Water Vapor Outside Cloud'						;Name attribute
	NCDF_ATTPUT, oid, 'Mean_Perc_H2O_NC', 'units',     '%'												;Units attribute

	vid = NCDF_VARDEF(oid, 'H2O_NC_Strat_Mass', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'H2O_NC_Strat_Mass', 'long_name', 'Total Mass of H2O Outside Cloud'						;Name attribute
	NCDF_ATTPUT, oid, 'H2O_NC_Strat_Mass', 'units',     'kg'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Max_Cloud', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Max_Cloud', 'long_name', 'Max Cloud Concentration'						;Name attribute
	NCDF_ATTPUT, oid, 'Max_Cloud', 'units',     'kg/kg'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Mean_Cloud', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Mean_Cloud', 'long_name', 'Mean Cloud Concentration'						;Name attribute
	NCDF_ATTPUT, oid, 'Mean_Cloud', 'units',     'kg/kg'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Total_Cloud_Mass', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Total_Cloud_Mass', 'long_name', 'Total Cloud Mass'						;Name attribute
	NCDF_ATTPUT, oid, 'Total_Cloud_Mass', 'units',     'kg'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Max_Ice', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Max_Ice', 'long_name', 'Max Ice Concentration'						;Name attribute
	NCDF_ATTPUT, oid, 'Max_Ice', 'units',     'kg/kg'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Mean_Ice', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Mean_Ice', 'long_name', 'Mean Ice Concentration'						;Name attribute
	NCDF_ATTPUT, oid, 'Mean_Ice', 'units',     'kg/kg'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Max_RHI', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Max_RHI', 'long_name', 'Max Relative Humidity w.r.t. Ice'						;Name attribute
	NCDF_ATTPUT, oid, 'Max_RHI', 'units',     '%'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Mean_RHI', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Mean_RHI', 'long_name', 'Mean Relative Humidity w.r.t. Ice'						;Name attribute
	NCDF_ATTPUT, oid, 'Mean_RHI', 'units',     '%'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Max_CO', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Max_CO', 'long_name', 'Max Carbon Monoxide Concentration'						;Name attribute
	NCDF_ATTPUT, oid, 'Max_CO', 'units',     'ppbv'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Mean_CO', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Mean_CO', 'long_name', 'Mean Carbon Monoxide Concentration'						;Name attribute
	NCDF_ATTPUT, oid, 'Mean_CO', 'units',     'ppbv'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Min_O3', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Min_O3', 'long_name', 'Min Ozone Concentration'						;Name attribute
	NCDF_ATTPUT, oid, 'Min_O3', 'units',     'ppbv'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Mean_O3', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Mean_O3', 'long_name', 'Mean Ozone Concentration'						;Name attribute
	NCDF_ATTPUT, oid, 'Mean_O3', 'units',     'ppbv'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Mean_Refl', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Mean_Refl', 'long_name', 'Mean Reflectivity at Tropopause'						;Name attribute
	NCDF_ATTPUT, oid, 'Mean_Refl', 'units',     'dBZ'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Mean_Trop_Cloud', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Mean_Trop_Cloud', 'long_name', 'Mean Cloud Concentration at Tropopause'						;Name attribute
	NCDF_ATTPUT, oid, 'Mean_Trop_Cloud', 'units',     'kg/kg'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Total_Trop_Cloud_Mass', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Total_Trop_Cloud_Mass', 'long_name', 'Total Cloud Mass at Tropopause'						;Name attribute
	NCDF_ATTPUT, oid, 'Total_Trop_Cloud_Mass', 'units',     'kg'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Mean_T', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Mean_T', 'long_name', 'Mean Temperature at the Tropopause'						;Name attribute
	NCDF_ATTPUT, oid, 'Mean_T', 'units',     'K'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Max_W', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Max_W', 'long_name', 'Max Updraft in Overshoot'						;Name attribute
	NCDF_ATTPUT, oid, 'Max_W', 'units',     'm/s'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Cylinder_Temps', [n2id], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Cylinder_Temps', 'long_name', 'Temperatures in Cylinder'						;Name attribute
	NCDF_ATTPUT, oid, 'Cylinder_Temps', 'units',     'K'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Cylinder_U', [n2id], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Cylinder_U', 'long_name', 'U wind in Cylinder'						;Name attribute
	NCDF_ATTPUT, oid, 'Cylinder_U', 'units',     'm/s'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Cylinder_V', [n2id], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Cylinder_V', 'long_name', 'V wind in Cylinder'						;Name attribute
	NCDF_ATTPUT, oid, 'Cylinder_V', 'units',     'm/s'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Cylinder_Z', [n2id], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Cylinder_Z', 'long_name', 'Z in Cylinder'						;Name attribute
	NCDF_ATTPUT, oid, 'Cylinder_Z', 'units',     'm'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Cylinder_dh2o_dz', [n2id], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Cylinder_dh2o_dz', 'long_name', 'dh2o/dz  in Cylinder'						;Name attribute
	NCDF_ATTPUT, oid, 'Cylinder_dh2o_dz', 'units',     '%kg/km'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Mean_U_5km', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Mean_U_5km', 'long_name', 'Mean U at 5 km'						;Name attribute
	NCDF_ATTPUT, oid, 'Mean_U_5km', 'units',     'm/s'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Mean_V_5km', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Mean_V_5km', 'long_name', 'Mean V at 5 km'						;Name attribute
	NCDF_ATTPUT, oid, 'Mean_V_5km', 'units',     'm/s'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Updraft_Size', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Updraft_Size', 'long_name', 'Updraft Size'						;Name attribute
	NCDF_ATTPUT, oid, 'Updraft_Size', 'units',     '# of gridpoints'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Updraft_Prevalence', [nid], /FLOAT)													;Define the latitude variable
	NCDF_ATTPUT, oid, 'Updraft_Prevalence', 'long_name', 'Updraft Prevalence'						;Name attribute
	NCDF_ATTPUT, oid, 'Updraft_Prevalence', 'units',     '# of gridpoints'												;Units attribute

	NCDF_ATTPUT, oid, 'DX', dx, /GLOBAL
	NCDF_ATTPUT, oid, 'DT', dt, /GLOBAL

	PRINT, 'Done creating variables'
	NCDF_CONTROL, oid, /ENDEF

	PRINT, 'Output file closed successfully' 

	NCDF_VARPUT, oid, 'Time', STRMID(outfile, 0, 14)														;Write date string to file
	NCDF_VARPUT, oid, 'Max_Overshoot'     	 , overshoot_total
;	NCDF_VARPUT, oid, 'Mean_Temp'            , mean_temp_total 
;	NCDF_VARPUT, oid, 'Mean_UTLS_Wind'       , mean_utls_wind_total
;	NCDF_VARPUT, oid, 'Mean_Storm_Motion'    , mean_storm_motion_total
	NCDF_VARPUT, oid, 'Unstable_Layers'      , unstable_layers_tot
	NCDF_VARPUT, oid, 'Unstable_Layers_IC'   , unstable_layers_ic
	NCDF_VARPUT, oid, 'Max_H2O'              , max_h2o_total
	NCDF_VARPUT, oid, 'Mean_H2O'             , ave_h2o_total
	NCDF_VARPUT, oid, 'Max_H2O_NC'           , max_h2o_nc_total
	NCDF_VARPUT, oid, 'Mean_H2O_NC'          , ave_h2o_nc_total
	NCDF_VARPUT, oid, 'Max_Perc_H2O'         , max_perc_h2o_total
	NCDF_VARPUT, oid, 'Mean_Perc_H2O'        , ave_perc_h2o_total
	NCDF_VARPUT, oid, 'Max_Perc_H2O_NC'      , max_perc_h2o_nc_total
	NCDF_VARPUT, oid, 'Mean_Perc_H2O_NC'     , ave_perc_h2o_nc_total
	NCDF_VARPUT, oid, 'H2O_NC_Strat_Mass'    , h2o_kg_strat
	NCDF_VARPUT, oid, 'Max_Cloud'            , max_cloud_total
	NCDF_VARPUT, oid, 'Mean_Cloud'           , ave_cloud_total
	NCDF_VARPUT, oid, 'Total_Cloud_Mass'     , total_cloud_mass
	NCDF_VARPUT, oid, 'Max_Ice'              , max_ice_total
	NCDF_VARPUT, oid, 'Mean_Ice'             , ave_ice_total
;	NCDF_VARPUT, oid, 'Max_RHI'              , max_rhi_total
;	NCDF_VARPUT, oid, 'Mean_RHI'             , ave_rhi_total
	NCDF_VARPUT, oid, 'Max_CO'               , max_co_total
	NCDF_VARPUT, oid, 'Mean_CO'		         , ave_co_total
	NCDF_VARPUT, oid, 'Min_O3'               , min_o3_total
	NCDF_VARPUT, oid, 'Mean_O3'              , ave_o3_total
	NCDF_VARPUT, oid, 'Mean_Refl'            , mean_trop_refl_total
	NCDF_VARPUT, oid, 'Mean_Trop_Cloud'      , mean_trop_cloud_total
	NCDF_VARPUT, oid, 'Total_Trop_Cloud_Mass', trop_cloud_kg_total	 
	NCDF_VARPUT, oid, 'Mean_T'               , ave_temp_total
	NCDF_VARPUT, oid, 'Max_W'                , max_w_total
	NCDF_VARPUT, oid, 'Cylinder_Temps'		 , cyl_temp_tot	
	NCDF_VARPUT, oid, 'Cylinder_U'			 , cyl_u_tot		
	NCDF_VARPUT, oid, 'Cylinder_V'			 , cyl_v_tot		
	NCDF_VARPUT, oid, 'Cylinder_Z'			 , cyl_z_tot		
	NCDF_VARPUT, oid, 'Cylinder_dh2o_dz'	 , cyl_dh2o_tot	
	NCDF_VARPUT, oid, 'Mean_U_5km'			 , mean_flow_u_5km	
	NCDF_VARPUT, oid, 'Mean_V_5km'			 , mean_flow_V_5km	
	NCDF_VARPUT, oid, 'Updraft_Size'		 , updraft_size_total
	NCDF_VARPUT, oid, 'Updraft_Prevalence'	 , updraft_prev_total
	
	PRINT, 'done writing variables'
	
	NCDF_CLOSE, oid																									;Close output file
	NCDF_CLOSE, iid																									;Close input file
	
	PRINT, 'File ' + infile + ' processed.' 

ENDFOREACH	;date	
END