PRO WRITE_H2O_TIMESERIES_350K_FILE, event, scheme, start_date, end_date, $
	DOMAIN   = domain, $
	REGION   = region, $
	MID_LAT  = mid_lat, $
	REFL	 = refl, $
	PNG	     = png, $
	Z_BUFF   = z_buff, $
	EPS   	 = eps


;+
; Name:
;		WRITE_H2O_TIMESERIES_350K_FILE
; Purpose:
;		Computes fraction of domain where updraft_tracer = 1
; Calling sequence:
;		WRITE_H2O_TIMESERIES_350K_FILE, run, scheme, start_date, end_date
; Input:
;		event      : Model simulation name. (e.g., '20120519')
;		scheme 	   : Model initial state. Typically 'morrison' or 'nssl'.
;		start_date : String variable of the start date requested (e.g., '20120519T2300Z')
;		end_date   : String variable of the end date requested (e.g., '20120520T0300Z')
; Output:
;		Timeseries of fraction of convection in domain and chemical signature 
; Keywords:
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		VERBOSE : If set, message writing process.
; Author and history:
;		Daniel B. Phoenix	    2017-07-18. 
;								2017-09-09. Same as CONVECTIVE_FRACTION but with map plots
;											and track the location on the timeseries
;								2018-03-15. Filtered locations where tropopause exceeded
;											15 km. 
;								2018-06-11. Filtered locations where 350-380K qualified
;											layer were below the lapse rate tropopause.
;											Added in convective intensity (depth of 
;											overshoot), mean tropopause temperature, and
;											fractional amount of stratospheric air.
;								2018-06-14. Re-tooled H2O_TIMESERIES_350K to write a file
;											with necessary information for plotting.
;											PLOT_H2O_TIMESERIES_350K will read this file
;											and create the plots.
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1											;Set default domain


outdir  = !WRF_DIRECTORY + event + '/' + scheme + '/reduced/h2o_timeseries_bugfix/'
FILE_MKDIR, outdir

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(event, scheme, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(event, scheme, start_date, end_date, /DATE)	

intensity1     = [ ]
conv_fraction  = [ ]
strat_fraction = [ ]
max_h2o		   = [ ]
ave_h2o_nc	   = [ ]
ave_h2o_c	   = [ ]
ave_o3_nc	   = [ ]
ave_o3_c	   = [ ]
trop_temp1     = [ ]

;; calculate first date 
	updraft  = (WRF_READ_VAR('Updraft_tracer' , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region)).values
	h2o		 = (WRF_READ_VAR('H2O' 			  , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E6
	cld_conv = (WRF_READ_VAR('Cloud_tracer'   , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region)).values
    o3       = (WRF_READ_VAR('O3'             , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E3					
    co       = (WRF_READ_VAR('CO'             , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E3					
    utls_o3 = (WRF_READ_VAR('UTLS_tracer'     , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E3
    y       = (WRF_READ_VAR('Latitude'        , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region)).values
    x       = (WRF_READ_VAR('Longitude'       , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region)).values
    z       = (WRF_READ_VAR('Z'		          , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region)).values
    ztrop   = (WRF_READ_VAR('Z_trop'	      , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region)).values	
	cloud   =  WRF_READ_VAR('CLOUD_MIX_TOTAL' , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region)
 	temp	   	 = (WRF_READ_VAR('T'          , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region)).values							;Read temperature variable from WRF output
 	theta	   	 =  WRF_READ_VAR('T'          , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region)							;Read temperature variable from WRF output
	theta.values = ((1000.0/(WRF_READ_VAR('P' , date_arr[0], event, scheme, DOMAIN = domain)).values)^(!Rair/!Cp))*(theta.values)

	dim 	 = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
 	ztrop    = MEDIAN(ztrop, 100)
    xyz_trop = REBIN(ztrop, dim[0], dim[1], dim[2], /SAMPLE)

;;+ To filter out areas where the 350K surface is below the lapse-rate tropopause
	count = 0
 	IF (KEYWORD_SET(tropics)) THEN igood = WHERE(xyz_trop GT 15000.0, count, COMPLEMENT = ibad)
 	IF (KEYWORD_SET(mid_lat)) THEN igood = WHERE(xyz_trop LT 15000.0, count, COMPLEMENT = ibad)

	IF (KEYWORD_SET(mid_lat)) THEN PRINT, 'Only calculating mid-latitude points'
	
	IF (count GT 0) THEN BEGIN
        z           [ibad] = !Values.F_NaN
        xyz_trop    [ibad] = !Values.F_NaN
        cloud.values[ibad] = !Values.F_NaN
        h2o         [ibad] = !Values.F_NaN
        theta.values[ibad] = !Values.F_NaN
        updraft     [ibad] = !Values.F_NaN
        temp        [ibad] = !Values.F_NaN
    ENDIF

;+ slow method (see better approach below)
;	FOR i = 0, dim[0]-1 DO BEGIN
;		FOR j = 0, dim[1]-1 DO BEGIN
;			FOR k = dim[2]-1, 0, -1 DO IF (theta.values[i,j,k] GT 350.0) THEN BEGIN
;				ke = k
;				ENDIF 
;			IF (z[i,j,ke] LT xyz_trop[i,j,ke]) THEN BEGIN
;               z           [i,j,*] = !Values.F_NaN
;               xyz_trop    [i,j,*] = !Values.F_NaN
;               cloud.values[i,j,*] = !Values.F_NaN
;               h2o         [i,j,*] = !Values.F_NaN
;               theta.values[i,j,*] = !Values.F_NaN
;               updraft     [i,j,*] = !Values.F_NaN
;               temp	    [i,j,*] = !Values.F_NaN
;			ENDIF
;		ENDFOR
;	ENDFOR
;;- end filtering

    dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string
    date_string = MAKE_ISO_DATE_STRING(date_arr[0], PREC='MINUTE', /COMPACT, /UTC)							;Create date string
    
	convection = FLOAT(WHERE(updraft GT 0.1, count, COMPLEMENT=nconv))

	h2o_strat = WHERE(((theta.values GT 350.0) AND (theta.values LT 380.0)), sh2o_count, COMPLEMENT=h2o_trop)
	overshoot = WHERE(cloud.values[h2o_strat] GT 1.0E-5, count, COMPLEMENT = nover)
		
	conv_vol  = FINDGEN(dim[0],dim[1],dim[2])*0.0
	strat_vol = FINDGEN(dim[0],dim[1],dim[2])*0.0
	
	conv_vol[h2o_strat[overshoot]] = 1.0
	conv_vol[nover	             ] = 0.0
	
	strat_vol[h2o_strat] = 1.0
	
	;;+ find points in LMS that are actually below the tropopause and filter out
	false_strat = WHERE(z[h2o_strat] LT xyz_trop[h2o_strat],false_count)
	
	IF (false_count) GT 0 THEN BEGIN
        z           [h2o_strat[false_strat]] = !Values.F_NaN
        xyz_trop    [h2o_strat[false_strat]] = !Values.F_NaN
        cloud.values[h2o_strat[false_strat]] = !Values.F_NaN
        h2o         [h2o_strat[false_strat]] = !Values.F_NaN
        theta.values[h2o_strat[false_strat]] = !Values.F_NaN
        updraft     [h2o_strat[false_strat]] = !Values.F_NaN
        temp	    [h2o_strat[false_strat]] = !Values.F_NaN
		strat_vol   [h2o_strat[false_strat]] = !Values.F_NaN
		conv_vol    [h2o_strat[false_strat]] = !Values.F_NaN
	ENDIF
	;;- end filter
	
	volume1 	   = TOTAL(conv_vol * 9 * z, /NAN)	
	vol_size       = SIZE(conv_vol,/DIMENSIONS)		
	strat1         = TOTAL(strat_vol * 9 * z, /NAN)
	total_size     = TOTAL(z * 9, /NAN)	
	conv_fraction  = FLOAT((volume1/total_size)) 
	strat_fraction = FLOAT((strat1 /total_size)) 

	nan = WHERE(conv_vol EQ 0.0, count_nan)
	conv_vol [nan] = !Values.F_NaN
	
	over_depth = conv_vol * z
	
	IF (conv_fraction GT 0.00001) THEN BEGIN
		intensity  = MEAN((MAX(over_depth, DIM=3, /NAN) - ztrop), /NAN)
	ENDIF ELSE BEGIN
		intensity = 0.0
	ENDELSE
	
 	h2o_c  = FINDGEN(dim[0],dim[1],dim[2])*0.0
 	h2o_nc = FINDGEN(dim[0],dim[1],dim[2])*0.0
 	 	
 	h2o_c  [convection] = h2o[convection]
 	h2o_c  [nconv     ] = !Values.F_NaN

 	h2o_nc [nconv     ] = h2o[nconv     ]
 	h2o_nc [convection] = !Values.F_NaN
 		
 	;h2o_strat = WHERE(((theta.values GT 350.0) AND (theta.values LT 380.0)), sh2o_count, COMPLEMENT=h2o_trop)
	h2o_lms = h2o[h2o_strat]
	h2o_lms[h2o_strat[false_strat]] = !Values.F_NaN
	
	ave_h2o_bg = MEAN(h2o_lms,/NAN) 

	trop_temp = FLTARR(dim[0],dim[1])*!Values.F_NaN
	FOR ii = 0, dim[0]-1 DO BEGIN
		FOR jj = 0, dim[1]-1 DO BEGIN
			index = VALUE_LOCATE(REFORM(z[ii,jj,*],dim[2],1,1),REFORM(xyz_trop[ii,jj,*],dim[2],1,1))
			trop_temp [ii,jj] = temp[ii,jj,index[0]]
		ENDFOR
	ENDFOR

	trop_temp1 = MEAN(trop_temp,/NAN)

	PRINT, 'intensity (m) = ', intensity
	PRINT, 'conv_fraction (%) = ', FLOAT((volume1/total_size))*100.0
	PRINT, 'strat_fraction (%) = ', FLOAT((strat1/total_size))*100.0
	PRINT, 'trop_temp (K) = ', trop_temp1  
	PRINT, 'updated code'

;; end

;; now calculate remaining dates
date_arr = date_arr[1:*]
i = 0
FOREACH date, date_arr DO BEGIN
	PRINT, 'Processing: ', date
	updraft  = (WRF_READ_VAR('Updraft_tracer' , date, event, scheme, DOMAIN=domain, INDICES=region)).values
	h2o		 = (WRF_READ_VAR('H2O' 			  , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E6
	cld_conv = (WRF_READ_VAR('Cloud_tracer'   , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    o3       = (WRF_READ_VAR('O3'             , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E3					
    co       = (WRF_READ_VAR('CO'             , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E3					
    utls_o3  = (WRF_READ_VAR('UTLS_tracer'    , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E3
    y        = (WRF_READ_VAR('Latitude'       , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    x        = (WRF_READ_VAR('Longitude'      , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    z        = (WRF_READ_VAR('Z'		         , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    ztrop    = (WRF_READ_VAR('Z_trop'	     , date, event, scheme, DOMAIN=domain, INDICES=region)).values	
	cloud    =  WRF_READ_VAR('CLOUD_MIX_TOTAL', date, event, scheme, DOMAIN=domain, INDICES=region)
 	temp	   	 = (WRF_READ_VAR('T', date_arr[0], event, scheme, DOMAIN = domain)).values							;Read temperature variable from WRF output
 	theta	   	 = WRF_READ_VAR('T', date_arr[0], event, scheme, DOMAIN = domain)							;Read temperature variable from WRF output
	theta.values = ((1000.0/(WRF_READ_VAR('P', date_arr[0], event, scheme, DOMAIN = domain)).values)^(!Rair/!Cp))*(theta.values)

	dim 	 = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
  	ztrop    = MEDIAN(ztrop, 100)
    xyz_trop = REBIN(ztrop, dim[0], dim[1], dim[2], /SAMPLE)
	
;;+ To filter out areas where the 350K surface is below the lapse-rate tropopause
	count = 0
 	IF (KEYWORD_SET(tropics)) THEN igood = WHERE(xyz_trop GT 15000.0, count, COMPLEMENT = ibad)
 	IF (KEYWORD_SET(mid_lat)) THEN igood = WHERE(xyz_trop LT 15000.0, count, COMPLEMENT = ibad)

	IF (count GT 0) THEN BEGIN
        z           [ibad] = !Values.F_NaN
        xyz_trop    [ibad] = !Values.F_NaN
        cloud.values[ibad] = !Values.F_NaN
        h2o         [ibad] = !Values.F_NaN
        theta.values[ibad] = !Values.F_NaN
        updraft     [ibad] = !Values.F_NaN
        temp		[ibad] = !Values.F_NaN
    ENDIF

;+ slow method (see better approach below)
;	FOR i = 0, dim[0]-1 DO BEGIN
;		FOR j = 0, dim[1]-1 DO BEGIN
;			FOR k = dim[2]-1, 0, -1 DO IF (theta.values[i,j,k] GT 350.0) THEN BEGIN
;				ke = k
;				ENDIF 
;			IF (z[i,j,ke] LT xyz_trop[i,j,ke]) THEN BEGIN
;               z           [i,j,*] = !Values.F_NaN
;               xyz_trop    [i,j,*] = !Values.F_NaN
;               cloud.values[i,j,*] = !Values.F_NaN
;               h2o         [i,j,*] = !Values.F_NaN
;               theta.values[i,j,*] = !Values.F_NaN
;               updraft     [i,j,*] = !Values.F_NaN
;               temp	    [i,j,*] = !Values.F_NaN
;			ENDIF
;		ENDFOR
;	ENDFOR
;;- end filtering

    dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string
    date_string = MAKE_ISO_DATE_STRING(date_arr[0], PREC='MINUTE', /COMPACT, /UTC)							;Create date string
    
	convection = FLOAT(WHERE(updraft GT 0.1, count, COMPLEMENT=nconv))

	h2o_strat = WHERE(((theta.values GT 350.0) AND (theta.values LT 380.0)), sh2o_count, COMPLEMENT=h2o_trop)
	overshoot = WHERE(cloud.values[h2o_strat] GT 1.0E-5, count, COMPLEMENT = nover)
		
	conv_vol  = FINDGEN(dim[0],dim[1],dim[2])*0.0
	strat_vol = FINDGEN(dim[0],dim[1],dim[2])*0.0
	
	conv_vol[h2o_strat[overshoot]] = 1.0
	conv_vol[nover	             ] = 0.0
	
	strat_vol[h2o_strat] = 1.0
	
	;;+ find points in LMS that are actually below the tropopause and filter out
	false_strat = WHERE(z[h2o_strat] LT xyz_trop[h2o_strat],false_count)
	
	IF (false_count) GT 0 THEN BEGIN
        z           [h2o_strat[false_strat]] = !Values.F_NaN
        xyz_trop    [h2o_strat[false_strat]] = !Values.F_NaN
        cloud.values[h2o_strat[false_strat]] = !Values.F_NaN
        h2o         [h2o_strat[false_strat]] = !Values.F_NaN
        theta.values[h2o_strat[false_strat]] = !Values.F_NaN
        updraft     [h2o_strat[false_strat]] = !Values.F_NaN
        temp	    [h2o_strat[false_strat]] = !Values.F_NaN
		strat_vol   [h2o_strat[false_strat]] = !Values.F_NaN
		conv_vol    [h2o_strat[false_strat]] = !Values.F_NaN
	ENDIF
	;;- end filter
		
	volume1 	   = TOTAL(conv_vol * 9 * z, /NAN)	
	vol_size       = SIZE(conv_vol,/DIMENSIONS)		
	strat1         = TOTAL(strat_vol * 9 * z, /NAN)
	total_size     = TOTAL(z * 9, /NAN)	
	conv_fraction  = FLOAT((volume1/total_size)) 
	strat_fraction = FLOAT((strat1 /total_size)) 

	nan = WHERE(conv_vol EQ 0.0, count_nan)
	conv_vol [nan] = !Values.F_NaN
	
	over_depth = conv_vol * z
	
	IF (conv_fraction GT 0.00001) THEN BEGIN
		intensity  = MEAN((MAX(over_depth, DIM=3, /NAN) - ztrop), /NAN)
	ENDIF ELSE BEGIN
		intensity = 0.0
	ENDELSE
	
 	h2o_c  = FINDGEN(dim[0],dim[1],dim[2])*0.0
 	h2o_nc = FINDGEN(dim[0],dim[1],dim[2])*0.0
 	 	
 	h2o_c  [convection] = h2o[convection]
 	h2o_c  [nconv     ] = !Values.F_NaN

 	h2o_nc [nconv     ] = h2o[nconv     ]
 	h2o_nc [convection] = !Values.F_NaN
 		
 	;h2o_strat = WHERE(((theta.values GT 350.0) AND (theta.values LT 380.0)), sh2o_count, COMPLEMENT=h2o_trop)
	h2o_lms = h2o[h2o_strat]
	h2o_lms[h2o_strat[false_strat]] = !Values.F_NaN
	
	ave_h2o_bg = MEAN(h2o_lms,/NAN) 

	trop_temp = FLTARR(dim[0],dim[1])*!Values.F_NaN
	FOR ii = 0, dim[0]-1 DO BEGIN
		FOR jj = 0, dim[1]-1 DO BEGIN
			index = VALUE_LOCATE(REFORM(z[ii,jj,*],dim[2],1,1),REFORM(xyz_trop[ii,jj,*],dim[2],1,1))
			trop_temp [ii,jj] = temp[ii,jj,index[0]]
		ENDFOR
	ENDFOR

	trop_temp1 = MEAN(trop_temp,/NAN)

	PRINT, 'intensity (m) = ', intensity
	PRINT, 'conv_fraction (%) = ', conv_fraction*100.0
	PRINT, 'strat_fraction (%) = ', FLOAT((strat1/total_size))*100.0
	PRINT, 'trop_temp (K) = ', trop_temp1	
	PRINT, 'updated code'

;	h2o		     = (WRF_READ_VAR('H2O' 		      , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E6
;    o3           = (WRF_READ_VAR('O3'             , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E3					
;    y            = (WRF_READ_VAR('Latitude'       , date, event, scheme, DOMAIN=domain, INDICES=region)).values
;    x            = (WRF_READ_VAR('Longitude'      , date, event, scheme, DOMAIN=domain, INDICES=region)).values
;    z            = (WRF_READ_VAR('Z'		      , date, event, scheme, DOMAIN=domain, INDICES=region)).values
;  	theta	   	 =  WRF_READ_VAR('T'	          , date, event, scheme, DOMAIN=domain)							;Read temperature variable from WRF output
;    ztrop   	 = (WRF_READ_VAR('Z_trop'	      , date, event, scheme, DOMAIN=domain, INDICES=region)).values	
;	cloud   	 =  WRF_READ_VAR('CLOUD_MIX_TOTAL', date, event, scheme, DOMAIN=domain, INDICES=region)
;
;	theta.values = ((1000.0/(WRF_READ_VAR('P', date, event, scheme, DOMAIN = domain)).values)^(!Rair/!Cp))*(theta.values)
;
;	dim 	 = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
;  	ztrop    = MEDIAN(ztrop, 100)
;    xyz_trop = REBIN(ztrop, dim[0], dim[1], dim[2], /SAMPLE)

	h2o_slice   = FLTARR(dim[0],dim[1])
	cloud_slice = FLTARR(dim[0],dim[1])
	k = 0

	h2o_bottom = FLTARR(dim[0],dim[1],dim[2])*!Values.F_NaN
	h2o_bottom [h2o_strat] = h2o[h2o_strat]
	h2o_bottom[h2o_strat[false_strat]] = !Values.F_NaN
	
	FOR i = 0, dim[0]-1 DO BEGIN
		FOR j = 0, dim[1]-1 DO BEGIN
			FOR k = 0, dim[2]-1 DO BEGIN
				ke = k
				IF (FINITE(h2o_bottom[i,j,k])) THEN BREAK
			ENDFOR
			h2o_slice[i,j]   = h2o[i,j,ke]
			cloud_slice[i,j] = cloud.values[i,j,ke]
		ENDFOR
	ENDFOR

	IF (domain EQ 1) THEN domain1 = 'd01'
	time    = MAKE_ISO_DATE_STRING(date, PRECISION='minute', /COMPACT, /UTC)
	infile  = !WRF_DIRECTORY + event + '/' + scheme + '/reduced/' + domain1 + '_' + time + '.nc'						;Set input filepath
	outfile = STRMID(infile, 16, /REVERSE_OFFSET)														;Set output file name
	outfile = 'h2o_350k_' + outfile
	outdir  = !WRF_DIRECTORY + event + '/' + scheme + '/reduced/h2o_timeseries_bugfix/'
		
	iid = NCDF_OPEN(infile)																						;Open input file for reading
	NCDF_VARGET, iid, 'T', values																				;Read single variable for output file definition
	NCDF_ATTGET, iid, 'DX', dx, /GLOBAL																		;Read grid resolution
	NCDF_ATTGET, iid, 'DT', dt, /GLOBAL																		;Read grid resolution
    
	dim = SIZE(values, /DIMENSIONS)																			;Get grid dimension sizes
	CATCH, error_status																								;Catch any errors with netcdf control or file creation
    
	IF (error_status NE 0) THEN BEGIN
		NCDF_CLOSE, oid																								;Close previous failed file
		oid = NCDF_CREATE(outdir + outfile, CLOBBER = 1)								;Create output file for writing
	ENDIF ELSE $
		oid = NCDF_CREATE(outdir + outfile, CLOBBER = 1)								;Create output file for writing
	
	xid = NCDF_DIMDEF(oid, 'x', dim[0])																			;Define output file dimensions
	yid = NCDF_DIMDEF(oid, 'y', dim[1])	
	zid = NCDF_DIMDEF(oid, 'z', dim[2])	
	tid = NCDF_DIMDEF(oid, 't', 14    )	
	
	vid = NCDF_VARDEF(oid, 'Time', [tid], /CHAR)																;Define the time variable
	NCDF_ATTPUT, oid, 'Time', 'long_name', 'ISO Date String'												;Name attribute
	NCDF_ATTPUT, oid, 'Time', 'units',     'YYYYMMDD_HHMM_'												;Units attribute
	
	vid = NCDF_VARDEF(oid, 'Longitude', [xid, yid], /FLOAT)												;Define the longitude variable
	NCDF_ATTPUT, oid, 'Longitude', 'long_name', 'Grid Point Longitude'								;Name attribute
	NCDF_ATTPUT, oid, 'Longitude', 'units',     'degrees E'												;Units attribute
	
	vid = NCDF_VARDEF(oid, 'Latitude', [xid, yid], /FLOAT)												;Define the latitude variable
	NCDF_ATTPUT, oid, 'Latitude', 'long_name', 'Grid Point Latitude'									;Name attribute
	NCDF_ATTPUT, oid, 'Latitude', 'units',     'degrees N'												;Units attribute
	
	vid = NCDF_VARDEF(oid, 'P', [xid, yid, zid], /FLOAT)													;Define the pressure variable
	NCDF_ATTPUT, oid, 'P', 'long_name', 'Air Pressure'														;Name attribute
	NCDF_ATTPUT, oid, 'P', 'units',     'hPa'																	;Units attribute

    vid = NCDF_VARDEF(oid, 'Z', [xid, yid, zid], /FLOAT)													;Define the geopotential height variable
	NCDF_ATTPUT, oid, 'Z', 'long_name', 'Geopotential Height'											;Name attribute
	NCDF_ATTPUT, oid, 'Z', 'units',     'm'																	;Units attribute

	vid = NCDF_VARDEF(oid, 'Convective_Fraction', /FLOAT)												;Define the latitude variable
	NCDF_ATTPUT, oid, 'Convective_Fraction', 'long_name', 'Convective Fraction'									;Name attribute
	NCDF_ATTPUT, oid, 'Convective_Fraction', 'units',     '%'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Stratospheric_Fraction', /FLOAT)												;Define the latitude variable
	NCDF_ATTPUT, oid, 'Stratospheric_Fraction', 'long_name', 'Stratospheric Fraction'									;Name attribute
	NCDF_ATTPUT, oid, 'Stratospheric_Fraction', 'units',     '%'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Convective_Intensity', /FLOAT)												;Define the latitude variable
	NCDF_ATTPUT, oid, 'Convective_Intensity', 'long_name', 'Mean Depth of Overshoots'									;Name attribute
	NCDF_ATTPUT, oid, 'Convective_Intensity', 'units',     'km'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Stratospheric_H2O', /FLOAT)												;Define the latitude variable
	NCDF_ATTPUT, oid, 'Stratospheric_H2O', 'long_name', 'Mean Conc. of H2O in Strat.'									;Name attribute
	NCDF_ATTPUT, oid, 'Stratospheric_H2O', 'units',     'ppmv'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Tropopause_Temperature', /FLOAT)												;Define the latitude variable
	NCDF_ATTPUT, oid, 'Tropopause_Temperature', 'long_name', 'Mean Temp. at Tropopause'									;Name attribute
	NCDF_ATTPUT, oid, 'Tropopause_Temperature', 'units',     'K'												;Units attribute

	vid = NCDF_VARDEF(oid, 'H2O_slice', [xid, yid], /FLOAT)												;Define the latitude variable
	NCDF_ATTPUT, oid, 'H2O_slice', 'long_name', 'H2O Conc. at 350 K surface'									;Name attribute
	NCDF_ATTPUT, oid, 'H2O_slice', 'units',     'ppmv'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Cloud_slice', [xid, yid], /FLOAT)												;Define the latitude variable
	NCDF_ATTPUT, oid, 'Cloud_slice', 'long_name', 'Cloud Conc. at 350 K surface'									;Name attribute
	NCDF_ATTPUT, oid, 'Cloud_slice', 'units',     'kg/kg'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Cloud_Mixing_Ratio', [xid, yid, zid], /FLOAT)												;Define the latitude variable
	NCDF_ATTPUT, oid, 'Cloud_Mixing_Ratio', 'long_name', 'Cloud Mixing Ratio'									;Name attribute
	NCDF_ATTPUT, oid, 'Cloud_Mixing_Ratio', 'units',     'kg/kg'												;Units attribute

	vid = NCDF_VARDEF(oid, 'Tropopause_Height', [xid, yid, zid], /FLOAT)												;Define the latitude variable
	NCDF_ATTPUT, oid, 'Tropopause_Height', 'long_name', '3D Tropopause Height'									;Name attribute
	NCDF_ATTPUT, oid, 'Tropopause_Height', 'units',     'm'												;Units attribute

	NCDF_ATTPUT, oid, 'DX', dx, /GLOBAL
	NCDF_ATTPUT, oid, 'DT', dt, /GLOBAL
    
	NCDF_CONTROL, oid, /ENDEF
    
	NCDF_VARPUT, oid, 'Time', STRMID(outfile, 0, 14)														;Write date string to file
	
	NCDF_VARGET, iid, 'Longitude', values																		;Read longitude values
	NCDF_VARPUT, oid, 'Longitude', values																		;Write longitude
    
	NCDF_VARGET, iid, 'Latitude', y																				;Read latitude values
	NCDF_VARPUT, oid, 'Latitude', y																				;Write latitude
	
	NCDF_VARGET, iid, 'P', p																						;Read pressure variables
	NCDF_VARPUT, oid, 'P', p	

	NCDF_VARGET, iid, 'Z', z																						;Read geopotential variables
	NCDF_VARPUT, oid, 'Z', z	
    
	NCDF_VARPUT, oid, 'Convective_Fraction'   , conv_fraction*100.0
	NCDF_VARPUT, oid, 'Stratospheric_Fraction', strat_fraction*100.0
	NCDF_VARPUT, oid, 'Convective_Intensity'  , intensity*0.001
	NCDF_VARPUT, oid, 'Stratospheric_H2O'     , ave_h2o_bg
	NCDF_VARPUT, oid, 'Tropopause_Temperature', trop_temp1
	NCDF_VARPUT, oid, 'Cloud_slice'           , cloud_slice
	NCDF_VARPUT, oid, 'H2O_slice'             , h2o_slice
	NCDF_VARPUT, oid, 'Cloud_Mixing_Ratio'    , cloud.values
	NCDF_VARPUT, oid, 'Tropopause_Height'     , xyz_trop

	NCDF_CLOSE, oid																									;Close output file
	NCDF_CLOSE, iid																									;Close input file
	
	PRINT, 'File ' + infile + ' processed.' 
            
ENDFOREACH

END