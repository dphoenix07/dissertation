PRO PLOT_UT_RELATIONSHIPS, event, scheme, start_date, end_date, $
	DOMAIN   = domain, $
	H2O_AVE	 = h2o_ave, $
	H2O_KG   = h2o_kg, $
	H2O_CO   = h2o_co, $
	CO		 = co, $
	TEMP     = temp, $
	ICE		 = ice, $
	UPDRAFT  = updraft, $
	UNSTABLE = unstable, $
	RHI	     = rhi, $
	PNG	     = png, $
	EPS   	 = eps


;+
; Name:
;		PLOT_UT_RELATIONSHIPS
; Purpose:
;		Plots relationships between tropopause temperature, overshoot depth, h2o, o3, co 
;		concentration, vertical velocity, and RHi
; Calling sequence:
;		PLOT_UT_RELATIONSHIPS, run, scheme, start_date, end_date
; Input:
;		event      : Model simulation name. (e.g., '20120519')
;		scheme 	   : Model initial state. Typically 'morrison' or 'nssl'.
; Output:
;		Timeseries of fraction of convection in domain and chemical signature 
; Keywords:
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		VERBOSE : If set, message writing process.
; Author and history:
;		Daniel B. Phoenix	    2018-07-17. 
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1											;Set default domain

outdir  = !WRF_DIRECTORY + event + '/' + scheme + '/plots/overshoot_tracking/'
FILE_MKDIR, outdir

indir  = !WRF_DIRECTORY + event + '/' + scheme + '/reduced/overshoot_statistics_v2_ut/'															;Set input directory
infile = FILE_SEARCH(indir + '*.nc', COUNT = nfile)							;Set input filepath

dim = SIZE(x, /DIMENSIONS)

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(event, scheme, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(event, scheme, start_date, end_date, /DATE)	

dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string

IF ~KEYWORD_SET(nowindow) THEN BEGIN
	IF KEYWORD_SET(eps) THEN BEGIN	
		PS_ON, FILENAME = epsfile, PAGE_SIZE = [6.0, 8.0], MARGIN = 0.0, /INCHES				;Switch to Postscript device
		DEVICE, /ENCAPSULATED
		!P.FONT     = 0																			;Hardware fonts
		!P.CHARSIZE = 1.0	
		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
			LOAD_BASIC_COLORS																	;Load basic color definitions
	ENDIF ELSE BEGIN
		SET_PLOT, 'X'
		WINDOW, XSIZE = 900, YSIZE = 600														;Open graphics window
		!P.COLOR      = COLOR_24('black')														;Foreground color
		!P.BACKGROUND = COLOR_24('white')														;Background color
		!P.CHARSIZE   = 2.0		
		!P.FONT       = -1																		;Use Hershey fonts
	ENDELSE
ENDIF

unstable_total_tot    = [ ]
unstable_total_ic     = [ ]
overshoot_total       = [ ]
max_h2o_total         = [ ]
ave_h2o_total         = [ ]
max_h2o_nc_total      = [ ]
ave_h2o_nc_total      = [ ]
max_perc_h2o_total    = [ ]
ave_perc_h2o_total    = [ ]
max_perc_h2o_nc_total = [ ]
ave_perc_h2o_nc_total = [ ]
h2o_kg_strat_total	  = [ ]
max_cloud_total       = [ ]
ave_cloud_total       = [ ]
total_cloud_mass	  = [ ]
max_ice_total         = [ ]
ave_ice_total         = [ ]
max_rhi_total         = [ ]
ave_rhi_total         = [ ]
max_co_total          = [ ]
ave_co_total          = [ ]
min_o3_total          = [ ]
ave_o3_total          = [ ]
ave_cyl_temp		  = [ ]
ave_temp_total        = [ ]
max_w_total	          = [ ]
mean_ztrop_total	  = [ ]
mean_trop_refl_total  = [ ]
mean_trop_cloud_total = [ ]
trop_cloud_kg_total	  = [ ]
cyl_temp_tot	      = [ ]
cyl_u_tot		      = [ ]
cyl_v_tot		      = [ ]
cyl_z_tot		      = [ ]
cyl_dh2o_tot	      = [ ]
mean_flow_u_5km	      = [ ]
mean_flow_V_5km	      = [ ]
updraft_size_total	  = [ ]


FOREACH date, date_arr DO BEGIN
	PRINT, date

	IF (KEYWORD_SET(H2O_AVE)) THEN BEGIN
		overshoot_depth     = (WRF_READ_VAR('Max_Overshoot'     , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		max_h2o 	        = (WRF_READ_VAR('Max_H2O'           , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values							;Read temperature variable from WRF output
		mean_h2o 	        = (WRF_READ_VAR('Mean_H2O'          , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values					
		mean_h2o_nc 	    = (WRF_READ_VAR('Mean_H2O_NC'       , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values					
		mean_ztrop		 	= (WRF_READ_VAR('Mean_Ztrop'		, date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		mean_temp           = (WRF_READ_VAR('Mean_T'            , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		cyl_temp            = (WRF_READ_VAR('Mean_Cyl_Temp'	    , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		mean_o3             = (WRF_READ_VAR('Mean_O3'	        , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values	
		mean_co             = (WRF_READ_VAR('Mean_CO'	        , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values	
		
		overshoot_total       = [overshoot_total       , overshoot_depth    ]
		max_h2o_total         = [max_h2o_total         , max_h2o 	        ]
		ave_temp_total        = [ave_temp_total        , mean_temp          ]
		ave_cyl_temp		  = [ave_cyl_temp		   , cyl_temp			]
		ave_h2o_total         = [ave_h2o_total         , mean_h2o 	        ]
		ave_h2o_nc_total      = [ave_h2o_nc_total      , mean_h2o_nc 	    ]
		mean_ztrop_total	  = [mean_ztrop_total	   , mean_ztrop			]
		ave_co_total          = [ave_co_total          , mean_co 		    ]
		ave_o3_total          = [ave_o3_total          , mean_o3			]
	ENDIF ELSE IF (KEYWORD_SET(H2O_KG)) THEN BEGIN
		overshoot_depth     = (WRF_READ_VAR('Max_Overshoot'     , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		mean_ztrop		 	= (WRF_READ_VAR('Mean_Ztrop'		, date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		h2o_kg_strat		= (WRF_READ_VAR('H2O_NC_Strat_Mass' , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values	 
		mean_temp           = (WRF_READ_VAR('Mean_T'            , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		cyl_temp            = (WRF_READ_VAR('Mean_Cyl_Temp'	    , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		
		h2o_kg_strat_total    = [h2o_kg_strat_total    , h2o_kg_strat		]
		ave_temp_total        = [ave_temp_total        , mean_temp          ]
		ave_cyl_temp		  = [ave_cyl_temp		   , cyl_temp			]
		overshoot_total       = [overshoot_total       , overshoot_depth    ]
		mean_ztrop_total	  = [mean_ztrop_total	   , mean_ztrop			]
	ENDIF ELSE IF (KEYWORD_SET(H2O_CO)) THEN BEGIN
		overshoot_depth     = (WRF_READ_VAR('Max_Overshoot'     , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		mean_ztrop		 	= (WRF_READ_VAR('Mean_Ztrop'		, date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		mean_h2o 	        = (WRF_READ_VAR('Mean_H2O'          , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values					
		mean_h2o_nc 	    = (WRF_READ_VAR('Mean_H2O_NC'       , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values					
		mean_co             = (WRF_READ_VAR('Mean_CO'           , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		mean_temp           = (WRF_READ_VAR('Mean_T'            , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
;		cyl_temp            = (WRF_READ_VAR('Mean_Cyl_Temp'	    , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		
		ave_co_total          = [ave_co_total          , mean_co 	        ]
		ave_h2o_total         = [ave_h2o_total         , mean_h2o 	        ]
		ave_h2o_nc_total      = [ave_h2o_nc_total      , mean_h2o_nc 	    ]
		overshoot_total       = [overshoot_total       , overshoot_depth    ]
		ave_temp_total        = [ave_temp_total        , mean_temp          ]
;		ave_cyl_temp		  = [ave_cyl_temp		   , cyl_temp			]
		mean_ztrop_total	  = [mean_ztrop_total	   , mean_ztrop			]
	ENDIF ELSE IF (KEYWORD_SET(CO)) THEN BEGIN
		max_co 	            = (WRF_READ_VAR('Max_CO'            , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		mean_co             = (WRF_READ_VAR('Mean_CO'           , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		mean_ztrop		 	= (WRF_READ_VAR('Mean_Ztrop'		, date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		overshoot_depth     = (WRF_READ_VAR('Max_Overshoot'     , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		mean_temp           = (WRF_READ_VAR('Mean_T'            , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		cyl_temp            = (WRF_READ_VAR('Mean_Cyl_Temp'	    , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values

		overshoot_total       = [overshoot_total       , overshoot_depth    ]
		max_co_total          = [max_co_total          , max_co 	        ]
		ave_temp_total        = [ave_temp_total        , mean_temp          ]
		ave_cyl_temp		  = [ave_cyl_temp		   , cyl_temp			]
		ave_co_total          = [ave_co_total          , mean_co 	        ]
		mean_ztrop_total	  = [mean_ztrop_total	   , mean_ztrop			]
	ENDIF ELSE IF (KEYWORD_SET(TEMP)) THEN BEGIN
		mean_temp           = (WRF_READ_VAR('Mean_T'            , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		overshoot_depth     = (WRF_READ_VAR('Max_Overshoot'     , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		mean_ztrop		 	= (WRF_READ_VAR('Mean_Ztrop'		, date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		cyl_temp            = (WRF_READ_VAR('Mean_Cyl_Temp'	    , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values

		overshoot_total       = [overshoot_total       , overshoot_depth    ]
		mean_ztrop_total	  = [mean_ztrop_total	   , mean_ztrop			]
		ave_cyl_temp		  = [ave_cyl_temp		   , cyl_temp			]
		ave_temp_total        = [ave_temp_total        , mean_temp          ]
	ENDIF ELSE IF (KEYWORD_SET(ICE)) THEN BEGIN
		mean_ice 	        = (WRF_READ_VAR('Mean_Ice'          , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values					
		overshoot_depth     = (WRF_READ_VAR('Max_Overshoot'     , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		mean_ztrop		 	= (WRF_READ_VAR('Mean_Ztrop'		, date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		mean_h2o 	        = (WRF_READ_VAR('Mean_H2O'          , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values					
		mean_h2o_nc 	    = (WRF_READ_VAR('Mean_H2O_NC'       , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values					
		mean_temp           = (WRF_READ_VAR('Mean_T'            , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		mean_co             = (WRF_READ_VAR('Mean_CO'           , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		unstable_layers_tot = (WRF_READ_VAR('Unstable_Layers'   , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values 
		cyl_temp            = (WRF_READ_VAR('Mean_Cyl_Temp'	    , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values

		unstable_total_tot    = [unstable_total_tot    , unstable_layers_tot]
		ave_temp_total        = [ave_temp_total        , mean_temp          ]
		ave_cyl_temp		  = [ave_cyl_temp		   , cyl_temp			]
		ave_co_total          = [ave_co_total          , mean_co 	        ]
		ave_h2o_total         = [ave_h2o_total         , mean_h2o 	        ]
		ave_h2o_nc_total      = [ave_h2o_nc_total      , mean_h2o_nc 	    ]
		overshoot_total       = [overshoot_total       , overshoot_depth    ]
		mean_ztrop_total	  = [mean_ztrop_total	   , mean_ztrop			]
		ave_ice_total         = [ave_ice_total         , mean_ice 	        ]
	ENDIF ELSE IF (KEYWORD_SET(UPDRAFT)) THEN BEGIN
		mean_ice 	        = (WRF_READ_VAR('Mean_Ice'          , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values					
		overshoot_depth     = (WRF_READ_VAR('Max_Overshoot'     , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		mean_ztrop		 	= (WRF_READ_VAR('Mean_Ztrop'		, date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		mean_h2o 	        = (WRF_READ_VAR('Mean_H2O'          , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values					
		mean_h2o_nc 	    = (WRF_READ_VAR('Mean_H2O_NC'       , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values					
		mean_temp           = (WRF_READ_VAR('Mean_T'            , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		mean_co             = (WRF_READ_VAR('Mean_CO'           , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		updraft_size		= (WRF_READ_VAR('Updraft_Size'		, date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		cyl_temp            = (WRF_READ_VAR('Mean_Cyl_Temp'	    , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		mean_o3             = (WRF_READ_VAR('Mean_O3'	        , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values	
		max_w	            = (WRF_READ_VAR('Max_W'             , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values

		ave_temp_total        = [ave_temp_total        , mean_temp          ]
		ave_cyl_temp		  = [ave_cyl_temp		   , cyl_temp			]
		ave_co_total          = [ave_co_total          , mean_co 	        ]
		ave_o3_total          = [ave_o3_total          , mean_o3			]
		ave_h2o_total         = [ave_h2o_total         , mean_h2o 	        ]
		ave_h2o_nc_total      = [ave_h2o_nc_total      , mean_h2o_nc 	    ]
		overshoot_total       = [overshoot_total       , overshoot_depth    ]
		mean_ztrop_total	  = [mean_ztrop_total	   , mean_ztrop			]
		ave_ice_total         = [ave_ice_total         , mean_ice 	        ]
		updraft_size_total	  = [updraft_size_total	   , updraft_size   	]
		max_w_total	          = [max_w_total           , max_w		        ]
	ENDIF ELSE IF (KEYWORD_SET(UNSTABLE)) THEN BEGIN
		mean_ztrop		 	= (WRF_READ_VAR('Mean_Ztrop'		, date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		overshoot_depth     = (WRF_READ_VAR('Max_Overshoot'     , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		unstable_layers_tot = (WRF_READ_VAR('Unstable_Layers'   , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values 
		unstable_layers_ic  = (WRF_READ_VAR('Unstable_Layers_IC', date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values 
		mean_temp           = (WRF_READ_VAR('Mean_T'            , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		mean_h2o 	        = (WRF_READ_VAR('Mean_H2O'          , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values					
		mean_h2o_nc 	    = (WRF_READ_VAR('Mean_H2O_NC'       , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values					
		cyl_temp            = (WRF_READ_VAR('Mean_Cyl_Temp'	    , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values

		ave_h2o_total         = [ave_h2o_total         , mean_h2o 	        ]
		ave_h2o_nc_total      = [ave_h2o_nc_total      , mean_h2o_nc 	    ]
		ave_temp_total        = [ave_temp_total        , mean_temp          ]
		ave_cyl_temp		  = [ave_cyl_temp		   , cyl_temp			]
		unstable_total_tot    = [unstable_total_tot    , unstable_layers_tot]
		unstable_total_ic     = [unstable_total_ic     , unstable_layers_ic ]
		overshoot_total       = [overshoot_total       , overshoot_depth    ]
		mean_ztrop_total	  = [mean_ztrop_total	   , mean_ztrop			]
	ENDIF ELSE IF (KEYWORD_SET(RHI)) THEN BEGIN
		mean_ztrop		 	= (WRF_READ_VAR('Mean_Ztrop'		, date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		overshoot_depth     = (WRF_READ_VAR('Max_Overshoot'     , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		mean_temp           = (WRF_READ_VAR('Mean_T'            , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		mean_h2o 	        = (WRF_READ_VAR('Mean_H2O'          , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values					
		mean_h2o_nc 	    = (WRF_READ_VAR('Mean_H2O_NC'       , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values					
		max_rhi		        = (WRF_READ_VAR('Max_RHI'           , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values					
		ave_rhi   		    = (WRF_READ_VAR('Mean_RHI'          , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		cyl_temp            = (WRF_READ_VAR('Mean_Cyl_Temp'	    , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values

		ave_h2o_total         = [ave_h2o_total         , mean_h2o 	        ]
		ave_h2o_nc_total      = [ave_h2o_nc_total      , mean_h2o_nc 	    ]
		ave_temp_total        = [ave_temp_total        , mean_temp          ]
		ave_cyl_temp		  = [ave_cyl_temp		   , cyl_temp			]
		max_rhi_total         = [max_rhi_total		   , max_rhi			]	
		ave_rhi_total         = [ave_rhi_total		   , ave_rhi			] 
		overshoot_total       = [overshoot_total       , overshoot_depth    ]
		mean_ztrop_total	  = [mean_ztrop_total	   , mean_ztrop			]
	ENDIF ELSE BEGIN	
		max_h2o_nc 	        = (WRF_READ_VAR('Max_H2O_NC'        , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values							;Read temperature variable from WRF output
		mean_h2o_nc 	    = (WRF_READ_VAR('Mean_H2O_NC'       , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values					
		max_perc_h2o 	    = (WRF_READ_VAR('Max_Perc_H2O'      , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values							;Read temperature variable from WRF output
		mean_perc_h2o 	    = (WRF_READ_VAR('Mean_Perc_H2O'     , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values					
		max_perc_h2o_nc 	= (WRF_READ_VAR('Max_Perc_H2O_NC'   , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values							;Read temperature variable from WRF output
		mean_perc_h2o_nc 	= (WRF_READ_VAR('Mean_Perc_H2O_NC'  , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values					
		max_cloud 	        = (WRF_READ_VAR('Max_Cloud'         , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values							;Read temperature variable from WRF output
		mean_cloud 	        = (WRF_READ_VAR('Mean_Cloud'        , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values					
		total_cloud 	    = (WRF_READ_VAR('Total_Cloud_Mass'  , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values					
		max_ice 	        = (WRF_READ_VAR('Max_Ice'           , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values							;Read temperature variable from WRF output
	;	max_rhi		        = (WRF_READ_VAR('Max_RHI'           , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values					
	;	ave_rhi   		    = (WRF_READ_VAR('Mean_RHI'          , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		min_o3 	            = (WRF_READ_VAR('Min_O3'	        , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		mean_o3             = (WRF_READ_VAR('Mean_O3'	        , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values	
		max_w	            = (WRF_READ_VAR('Max_W'             , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values

		mean_trop_refl  = (WRF_READ_VAR('Mean_Refl'            , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		mean_trop_cloud = (WRF_READ_VAR('Mean_Trop_Cloud'      , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		trop_cloud_kg   = (WRF_READ_VAR('Total_Trop_Cloud_Mass', date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		cyl_temp	    = (WRF_READ_VAR('Cylinder_Temps'	   , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		cyl_u		    = (WRF_READ_VAR('Cylinder_U'		   , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values	
		cyl_v		    = (WRF_READ_VAR('Cylinder_V'		   , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		cyl_z		    = (WRF_READ_VAR('Cylinder_Z'		   , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values
		cyl_dh2o	    = (WRF_READ_VAR('Cylinder_dh2o_dz'	   , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values 
		flow_u_5km	    = (WRF_READ_VAR('Mean_U_5km'		   , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values 
		flow_V_5km	    = (WRF_READ_VAR('Mean_V_5km'		   , date, event, scheme, DOMAIN=domain, INDICES=region, UT=1)).values

		max_h2o_nc_total      = [max_h2o_nc_total      , max_h2o_nc 	    ]
		max_perc_h2o_total    = [max_perc_h2o_total    , max_perc_h2o 	    ]
		ave_perc_h2o_total    = [ave_perc_h2o_total    , mean_perc_h2o 	    ]
		max_perc_h2o_nc_total = [max_perc_h2o_nc_total , max_perc_h2o_nc 	]
		ave_perc_h2o_nc_total = [ave_perc_h2o_nc_total , mean_perc_h2o_nc   ]
		max_cloud_total       = [max_cloud_total       , max_cloud 	        ]
		ave_cloud_total       = [ave_cloud_total       , mean_cloud 	    ]
		total_cloud_mass      = [total_cloud_mass      , total_cloud 	    ]
		max_ice_total         = [max_ice_total         , max_ice 	        ]
		;max_rhi_total         = [max_rhi_total         , max_rhi 	        ]
		;ave_rhi_total         = [ave_rhi_total         , mean_rhi 	        ]
		max_co_total          = [max_co_total          , max_co 	        ]
		ave_co_total          = [ave_co_total          , mean_co 	        ]
		min_o3_total          = [min_o3_total          , min_o3 	        ]
		ave_o3_total          = [ave_o3_total          , mean_o3 	        ]
		max_w_total	          = [max_w_total           , max_w		        ]
		mean_ztrop_total	  = [mean_ztrop_total	   , mean_ztrop			]
		mean_trop_refl_total  = [mean_trop_refl_total  , mean_trop_refl     ]
		mean_trop_cloud_total = [mean_trop_cloud_total , mean_trop_cloud    ]
		trop_cloud_kg_total	  = [trop_cloud_kg_total   , trop_cloud_kg      ]
		cyl_temp_tot	      = [cyl_temp_tot	       , cyl_temp	        ]
		cyl_u_tot		      = [cyl_u_tot		       , cyl_u		        ]
		cyl_v_tot		      = [cyl_v_tot		       , cyl_v		        ]
		cyl_z_tot		      = [cyl_z_tot		       , cyl_z		        ]
		cyl_dh2o_tot	      = [cyl_dh2o_tot	       , cyl_dh2o	        ]
		mean_flow_u_5km	      = [mean_flow_u_5km	   , flow_u_5km	        ]
		mean_flow_V_5km	      = [mean_flow_V_5km	   , flow_V_5km	        ]
		unstable_total_tot    = [unstable_total_tot    , unstable_layers_tot]
		unstable_total_ic     = [unstable_total_ic     , unstable_layers_ic ]
	ENDELSE
ENDFOREACH

bar_pos = [0.15, 0.11, 0.85, 0.13]																			;Set color bar position
map_pos = [0.1, 0.25, 0.95, 0.90]

IF (KEYWORD_SET(H2O_KG)) THEN BEGIN

	PLOT, overshoot_total, ave_temp_total, /NODATA, $
				xtitle = 'Overshoot Depth (km)', ytitle = "Mean Tropopause Temp (K)", PSYM = 4, $
				yrange = [190, 240], ylog = 0, xrange = [0,5], title = start_date, POSITION = map_pos

	h2o_kg_strat_total = h2o_kg_strat_total/1.0E6
	;;;Color by average h2o concentration
	USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation    
	FOR s = 0, N_ELEMENTS(h2o_kg_strat_total)-1 DO BEGIN
	    table = HCL_COLOR_TABLE(245, HUE_RANGE = [100.0, 300.0])
	    h2olevels = FINDGEN(245)
	    ih2o = WHERE(h2olevels EQ ROUND(h2o_kg_strat_total[s]))
	    
	    PLOTS, overshoot_total[s], ave_temp_total[s], $
	    	    PSYM    = 8, $
	    	    SYMSIZE = 4, $
	    		NOCLIP  = 0, $
				COLOR = table[ih2o]
	ENDFOR
	
	COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
		RANGE = [0, 245], $
		TICKS = 5, $
		TITLE = 'H2O Mass (kg/10^6)', $
		POSIT = bar_pos

ENDIF

IF (KEYWORD_SET(H2O_AVE)) THEN BEGIN
	
	;ibad = WHERE(mean_ztrop_total*1.0E-3 LT 9.5) 
	IF (N_ELEMENTS(ibad) GT 1) THEN BEGIN
		ave_h2o_nc_total[ibad] = !Values.F_NaN
		overshoot_total[ibad] = !Values.F_NaN
		mean_ztrop_total[ibad] = !Values.F_NaN
		ave_co_total[ibad] = !Values.F_NaN
		ave_o3_total[ibad] = !Values.F_NaN
	ENDIF
	
	;PLOT, overshoot_total, mean_ztrop_total*1.0E-3, /NODATA, $
	;			xtitle = 'Overshoot Depth (km)', ytitle = "Mean Tropopause Height (km)", PSYM = 4, $
	;			yrange = [8, 18], xlog = 0, xrange = [0,5], title = start_date, POSITION = map_pos
;
	;USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation    
	;FOR s = 0, N_ELEMENTS(ave_h2o_total)-1 DO BEGIN
	;    table = HCL_COLOR_TABLE(50, HUE_RANGE = [100.0, 300.0])
	;    h2olevels = FINDGEN(50)
	;    ih2o = WHERE(h2olevels EQ ROUND(ave_h2o_total[s]))
	;    
	;    PLOTS, overshoot_total[s], mean_ztrop_total[s]*1.0E-3, $
	;    	    PSYM    = 8, $
	;    	    SYMSIZE = 4, $
	;    		NOCLIP  = 0, $
	;			COLOR = table[ih2o]
	;ENDFOR
	;
	;COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
	;	RANGE = [0, 50], $
	;	TICKS = 5, $
	;	TITLE = 'Average H2O Concentration (ppm)', $
	;	POSIT = bar_pos


	;PLOT, overshoot_total, ave_cyl_temp, /NODATA, $
	;			xtitle = 'Overshoot Depth (km)', ytitle = "Mean Cylinder Temp (K)", PSYM = 4, $
	;			yrange = [240, 190], ylog = 0, xrange = [0,5], title = start_date, POSITION = map_pos

	;PLOT, overshoot_total, ave_temp_total, /NODATA, $
	;			xtitle = 'Overshoot Depth (km)', ytitle = "Mean Tropopause Temp (K)", PSYM = 4, $
	;			yrange = [240, 190], ylog = 0, xrange = [0,5], title = start_date, POSITION = map_pos

	;;Color by average h2o concentration
	;USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation    
	;FOR s = 0, N_ELEMENTS(ave_h2o_nc_total)-1 DO BEGIN
	;    table = HCL_COLOR_TABLE(30, HUE_RANGE = [100.0, 300.0])
	;    h2olevels = FINDGEN(30)
	;    ih2o = WHERE(h2olevels EQ ROUND(ave_h2o_nc_total[s]))
	;    
	;    PLOTS, overshoot_total[s], ave_cyl_temp[s], $
	;    	    PSYM    = 8, $
	;    	    SYMSIZE = 4, $
	;    		NOCLIP  = 0, $
	;			COLOR = table[ih2o]
	;ENDFOR
	;
	;COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
	;	RANGE = [0, 30], $
	;	TICKS = 5, $
	;	TITLE = 'Average H2O (NC) Concentration (ppm)', $
	;	POSIT = bar_pos
	
	PLOT, overshoot_total, ave_cyl_temp, /NODATA, $
				xtitle = 'Overshoot Depth (km)', ytitle = "Mean Cylinder Temp (K)", PSYM = 4, $
				yrange = [250, 210], ylog = 0, xrange = [0,5], title = start_date, POSITION = map_pos
				;xtitle = 'Overshoot Depth (normalized)', ytitle = "Mean Cylinder Temp (K)", PSYM = 4, $
				;yrange = [240, 190], ylog = 0, xrange = [0,1], title = start_date, POSITION = map_pos

	h2o_norm  = FLTARR(N_ELEMENTS(ave_h2o_nc_total))
	over_norm = FLTARR(N_ELEMENTS(ave_h2o_nc_total))
	temp_norm = FLTARR(N_ELEMENTS(ave_h2o_nc_total))
	
	ih2o    = SORT(ave_h2o_nc_total)  
	h2osort = ave_h2o_nc_total[ih2o]  
	h2o5    = h2osort[PERCENTILE(h2osort,5, /NOSORT,/NAN)]
	h2o95   = h2osort[PERCENTILE(h2osort,95,/NOSORT,/NAN)]
	
	iover    = SORT(overshoot_total)  
	oversort = overshoot_total[iover]  
	over5    = oversort[PERCENTILE(oversort,5, /NOSORT,/NAN)]
	over95   = oversort[PERCENTILE(oversort,95,/NOSORT,/NAN)]
	
	itemp    = SORT(ave_cyl_temp)  
	tempsort = ave_cyl_temp[itemp]  
	temp5    = tempsort[PERCENTILE(tempsort,5, /NOSORT,/NAN)]
	temp95   = tempsort[PERCENTILE(tempsort,95,/NOSORT,/NAN)]
	
	;USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation    
	;FOR s = 0, N_ELEMENTS(ave_h2o_nc_total)-1 DO BEGIN
	;	
	;	h2o_norm[s]  = ((ave_h2o_nc_total[s] - h2o5 ) / (h2o95  - h2o5 )) * 100.0
	;	over_norm[s] = ((overshoot_total[s]  - over5) / (over95 - over5))
	;	temp_norm[s] = ((ave_cyl_temp[s]     - temp5) / (temp95 - temp5))
;
	;	IF (h2o_norm[s] LT 0) THEN h2o_norm[s] = 0.0
	;	
	;    table = HCL_COLOR_TABLE(10, HUE_RANGE = [100.0, 300.0])
	;    h2olevels = FINDGEN(10)
	;    ih2o = WHERE(h2olevels EQ ROUND(ave_h2o_nc_total[s]))
;
	;    ;table = HCL_COLOR_TABLE(100, HUE_RANGE = [100.0, 300.0])
	;    ;h2olevels = FINDGEN(100)
	;    ;ih2o = WHERE(h2olevels EQ ROUND(h2o_norm[s]))
	;    
	;    PLOTS, overshoot_total[s], ave_cyl_temp[s], $
	;    	    PSYM    = 8, $
	;    	    SYMSIZE = 4, $
	;    		NOCLIP  = 0, $
	;			COLOR = table[ih2o]
	;ENDFOR
	;
	;COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
	;	RANGE = [0, 10], $
	;	TICKS = 5, $
	;	TITLE = 'Average H2O (NC) Concentration (ppmv)', $
;;		TITLE = 'Normalized H2O (NC) Concentration (x100)', $
	;	POSIT = bar_pos

	;Color by CO concentration
	USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation    
	FOR s = 0, N_ELEMENTS(ave_co_total)-1 DO BEGIN
		
	    table = HCL_COLOR_TABLE(100, HUE_RANGE = [100.0, 300.0])
	    colevels = FINDGEN(100)+65.0
	    ico = WHERE(colevels EQ ROUND(ave_co_total[s]))
	    
	    PLOTS, overshoot_total[s], ave_cyl_temp[s], $
	    	    PSYM    = 8, $
	    	    SYMSIZE = 4, $
	    		NOCLIP  = 0, $
				COLOR = table[ico]
	ENDFOR
	
	COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
		RANGE = [65, 165], $
		TICKS = 5, $
		TITLE = 'Average CO Concentration (ppbv)', $
		POSIT = bar_pos



	;;Color by max h2o concentration
	;USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation    
	;FOR s = 0, N_ELEMENTS(max_h2o_total)-1 DO BEGIN
	;	table = HCL_COLOR_TABLE(250, HUE_RANGE = [100.0, 300.0])
	;	h2olevels = FINDGEN(250)
	;	ih2o = WHERE(h2olevels EQ ROUND(max_h2o_total[s]))
	;
	;	PLOTS, overshoot_total[s], mean_ztrop_total[s]*1.0E-3, $
	;			PSYM    = 8, $
	;			SYMSIZE = 4, $
	;			NOCLIP  = 0, $
	;			COLOR = table[ih2o]
	;ENDFOR
;
	;COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
	;	RANGE = [0, 250], $
	;	TICKS = 5, $
	;	TITLE = 'Max H2O Concentration (ppm)', $
	;	POSIT = bar_pos
ENDIF

IF (KEYWORD_SET(H2O_CO)) THEN BEGIN
	ibad = WHERE(mean_ztrop_total*1.0E-3 LT 9.5) 
	ave_h2o_total[ibad]    = !Values.F_NaN
	overshoot_total[ibad]  = !Values.F_NaN
	mean_ztrop_total[ibad] = !Values.F_NaN
	ave_co_total[ibad]	   = !Values.F_NaN
	
	ave_ratio= (ave_h2o_total/ave_co_total)*100.0
	PLOT, ave_ratio, ave_temp_total, /NODATA, $
				xtitle = 'Ave H2O:CO ratio (%)', ytitle = "Mean Tropopause Temp (K)", PSYM = 4, $
				CHARSIZE = 2, yrange = [240, 190], ylog = 0, xrange = [0,60], title = start_date, $
				POSITION = map_pos
					
	USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation    
	FOR s = 0, N_ELEMENTS(ave_h2o_total)-1 DO BEGIN
	    table = HCL_COLOR_TABLE(30, HUE_RANGE = [100.0, 300.0])
	    h2olevels = FINDGEN(30)
	    ih2o = WHERE(h2olevels EQ ROUND(ave_h2o_nc_total[s]))
	    
	    PLOTS, ave_ratio[s], ave_temp_total[s], $
	    	    PSYM    = 8, $
	    	    SYMSIZE = 4, $
	    		NOCLIP  = 0, $
				COLOR = table[ih2o]
	ENDFOR
	
	COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
		RANGE = [0, 30], $
		TICKS = 5, $
		TITLE = 'Average H2O (NC) Concentration (ppm)', $
		POSIT = bar_pos
	
	;USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation    
	;FOR s = 0, N_ELEMENTS(mass_factor)-1 DO BEGIN
	;    table = HCL_COLOR_TABLE(100, HUE_RANGE = [100.0, 300.0])
	;    h2olevels = FINDGEN(100)
	;    ih2o = WHERE(h2olevels EQ ROUND(mass_factor[s]))
	;    
	;    PLOTS, overshoot_total[s], ave_temp_total[s], $
	;    	    PSYM    = 8, $
	;    	    SYMSIZE = 4, $
	;    		NOCLIP  = 0, $
	;			COLOR = table[ih2o]
	;ENDFOR
	;
	;COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
	;	RANGE = [20, 120], $
	;	TICKS = 5, $
	;	TITLE = 'Average H2O:(H2O:CO) Ratio (x100)', $
	;	POSIT = bar_pos
ENDIF

IF (KEYWORD_SET(CO)) THEN BEGIN
	ibad = WHERE(mean_ztrop_total*1.0E-3 LT 9.5) 
	overshoot_total[ibad]  = !Values.F_NaN
	mean_ztrop_total[ibad] = !Values.F_NaN
	ave_co_total[ibad]	   = !Values.F_NaN


	;;PLOT, mean_ztrop_total*1.0E-3, overshoot_total, /NODATA, $
	;;			ytitle = 'Overshoot Depth (km)', xtitle = "Mean Tropopause Height (km)", PSYM = 4, $
	;;			xrange = [8, 18], xlog = 0, yrange = [0,5], title = start_date

	;PLOT, overshoot_total, ave_temp_total, /NODATA, $
	;			xtitle = 'Overshoot Depth (km)', ytitle = "Mean Tropopause Temp (K)", PSYM = 4, $
	;			yrange = [240, 190], ylog = 0, xrange = [0,5], title = start_date, POSITION = map_pos

	PLOT, overshoot_total, ave_cyl_temp, /NODATA, $
				xtitle = 'Overshoot Depth (km)', ytitle = "Mean Cylinder Temp (K)", PSYM = 4, $
				yrange = [240, 190], ylog = 0, xrange = [0,5], title = start_date, POSITION = map_pos

	;;;Color by average co concentration
	USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation    
	FOR s = 0, N_ELEMENTS(ave_co_total)-1 DO BEGIN
	    table = HCL_COLOR_TABLE(50, HUE_RANGE = [100.0, 300.0])
	    colevels = 30.0+FINDGEN(50)
	    ico = WHERE(colevels EQ ROUND(ave_co_total[s]))
	    
	    PLOTS, overshoot_total[s], ave_cyl_temp[s], $
	    	    PSYM    = 8, $
	    	    SYMSIZE = 4, $
	    		NOCLIP  = 0, $
				COLOR = table[ico]
	ENDFOR
	
	COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
		RANGE = [30, 80], $
		TICKS = 5, $
		TITLE = 'Average CO Concentration (ppm)', $
		POSIT = bar_pos

	;;Color by max h2o concentration
	;USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation    
	;FOR s = 0, N_ELEMENTS(max_co_total)-1 DO BEGIN
	;	table = HCL_COLOR_TABLE(200, HUE_RANGE = [100.0, 300.0])
	;	colevels = 50.0+FINDGEN(200)
	;	ico = WHERE(colevels EQ ROUND(max_co_total[s]))
	;
	;	PLOTS, overshoot_total[s], mean_ztrop_total[s]*1.0E-3, $
	;			PSYM    = 8, $
	;			SYMSIZE = 4, $
	;			NOCLIP  = 0, $
	;			COLOR = table[ico]
	;ENDFOR
;
	;COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
	;	RANGE = [50, 250], $
	;	TICKS = 5, $
	;	TITLE = 'Max CO Concentration (ppm)', $
	;	POSIT = bar_pos

ENDIF

IF (KEYWORD_SET(RHI)) THEN BEGIN
	inan = WHERE(ave_rhi_total EQ 9.9692100e+36, count)
	IF (count GT 0) THEN ave_rhi_total[inan] = !Values.F_NaN
	
	ave_rhi_total = ave_rhi_total*100.0
	
	;PLOT, ave_rhi_total, ave_temp_total, /NODATA, $
	;			xtitle = 'Ave RHi (%)', ytitle = "Mean Tropopause Temp (K)", PSYM = 4, $
	;			yrange = [240, 190], ylog = 0, xrange = [0,10], title = start_date, POSITION = map_pos

	PLOT, ave_rhi_total, ave_cyl_temp, /NODATA, $
				xtitle = 'Ave RHi (%)', ytitle = "Mean Cylinder Temp (K)", PSYM = 4, $
				yrange = [240, 190], ylog = 0, xrange = [0,10], title = start_date, POSITION = map_pos
;				xtitle = 'Normalized RHi', ytitle = "Mean Cylinder Temp (K)", PSYM = 4, $
;				yrange = [240, 190], ylog = 0, xrange = [0,1], title = start_date, POSITION = map_pos


	h2o_norm  = FLTARR(N_ELEMENTS(ave_h2o_nc_total))
	rhi_norm  = FLTARR(N_ELEMENTS(ave_h2o_nc_total))
	temp_norm = FLTARR(N_ELEMENTS(ave_h2o_nc_total))
	
	ih2o    = SORT(ave_h2o_nc_total)  
	h2osort = ave_h2o_nc_total[ih2o]  
	h2o5    = h2osort[PERCENTILE(h2osort,5, /NOSORT,/NAN)]
	h2o95   = h2osort[PERCENTILE(h2osort,95,/NOSORT,/NAN)]
	
	irhi    = SORT(ave_rhi_total)  
	rhisort = ave_rhi_total[irhi]  
	rhi5    = rhisort[PERCENTILE(rhisort,5, /NOSORT,/NAN)]
	rhi95   = rhisort[PERCENTILE(rhisort,95,/NOSORT,/NAN)]
	
	itemp    = SORT(ave_cyl_temp)  
	tempsort = ave_cyl_temp[itemp]  
	temp5    = tempsort[PERCENTILE(tempsort,5, /NOSORT,/NAN)]
	temp95   = tempsort[PERCENTILE(tempsort,95,/NOSORT,/NAN)]
	

	;;;Color by average co concentration
	USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation    
	FOR s = 0, N_ELEMENTS(ave_h2o_nc_total)-1 DO BEGIN

		h2o_norm[s]  = ((ave_h2o_nc_total[s] - h2o5 ) / (h2o95  - h2o5 )) * 100.0
		rhi_norm[s]  = ((ave_rhi_total[s]    - rhi5)  / (rhi95  - rhi5))
		temp_norm[s] = ((ave_cyl_temp[s]     - temp5) / (temp95 - temp5))

		IF (h2o_norm[s] LT 0) THEN h2o_norm[s] = 0.0
		
	   ;table = HCL_COLOR_TABLE(100, HUE_RANGE = [100.0, 300.0])
	   ;h2olevels = FINDGEN(100)
	   ;ih2o = WHERE(h2olevels EQ ROUND(h2o_norm[s]))

	    table = HCL_COLOR_TABLE(10, HUE_RANGE = [100.0, 300.0])
	    h2olevels = FINDGEN(10)
	    ih2o = WHERE(h2olevels EQ ROUND(ave_h2o_nc_total[s]))
	    
	    PLOTS, ave_rhi_total[s], ave_cyl_temp[s], $
	    	    PSYM    = 8, $
	    	    SYMSIZE = 4, $
	    		NOCLIP  = 0, $
				COLOR = table[ih2o]
	ENDFOR
	
	COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
		RANGE = [0, 10], $
		TICKS = 5, $
		TITLE = 'Average H2O (NC) Concentration (ppm)', $
;		TITLE = 'Normalized H2O (NC) Concentration (x100)', $
		POSIT = bar_pos
ENDIF


IF (KEYWORD_SET(TEMP)) THEN BEGIN
	ibad = WHERE(mean_ztrop_total*1.0E-3 LT 9.5) 
	overshoot_total[ibad] = !Values.F_NaN
	mean_ztrop_total[ibad]= !Values.F_NaN
	ave_temp_total[ibad]  = !Values.F_NaN
	ave_cyl_temp[ibad]	  = !Values.F_NaN
	
	;;PLOT, mean_ztrop_total*1.0E-3, overshoot_total, /NODATA, $
	;;			ytitle = 'Overshoot Depth (km)', xtitle = "Mean Tropopause Height (km)", PSYM = 4, $
	;;			xrange = [8, 18], xlog = 0, yrange = [0,5], title = start_date

	PLOT, overshoot_total, mean_ztrop_total*1.0E-3, /NODATA, $
				xtitle = 'Overshoot Depth (km)', ytitle = "Mean Tropopause Height (km)", PSYM = 4, $
				yrange = [9, 16], ylog = 0, xrange = [0,5], title = start_date, POSITION = map_pos

	
	;;;Color by average temp concentration
	USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation    
	FOR s = 0, N_ELEMENTS(ave_temp_total)-1 DO BEGIN
	    table = HCL_COLOR_TABLE(25, HUE_RANGE = [100.0, 300.0])
	    templevels = 200.0+FINDGEN(25)
	    itemp = WHERE(templevels EQ ROUND(ave_cyl_temp[s]))
	    
	    PLOTS, overshoot_total[s], mean_ztrop_total[s]*1.0E-3, $
	    	    PSYM    = 8, $
	    	    SYMSIZE = 4, $
	    		NOCLIP  = 0, $
				COLOR   = table[itemp]
	ENDFOR
	
	COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
		RANGE = [200, 225], $
		TICKS = 5, $
		TITLE = 'Average Cylinder Temperature (K)', $
;		TITLE = 'Average Tropopause Temperature (K)', $
		POSIT = bar_pos
ENDIF

IF (KEYWORD_SET(UPDRAFT)) THEN BEGIN
	
	;ibad = WHERE(mean_ztrop_total*1.0E-3 LT 9.5) 
	IF (N_ELEMENTS(ibad) GT 1) THEN BEGIN
		ave_h2o_total[ibad] = !Values.F_NaN
		overshoot_total[ibad] = !Values.F_NaN
		mean_ztrop_total[ibad] = !Values.F_NaN
		ave_temp_total[ibad] = !Values.F_NaN
		updraft_size_total[ibad] = !Values.F_NaN
		ave_co_total [ibad] = !Values.F_NaN
	ENDIF
		
	;PLOT, overshoot_total, ave_temp_total, /NODATA, $
	;			xtitle = 'Overshoot Depth (km)', ytitle = "Mean Tropopause Temp (K)", PSYM = 4, $
	;			yrange = [190, 240], ylog = 0, xrange = [0,5], title = start_date, POSITION = map_pos
	;
	;;;;Color by average temp concentration
	;USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation    
	;FOR s = 0, N_ELEMENTS(updraft_size_total)-1 DO BEGIN
	;    table = HCL_COLOR_TABLE(100, HUE_RANGE = [100.0, 300.0])
	;    updraftlevels = FINDGEN(100)
	;    iup = WHERE(updraftlevels EQ ROUND(updraft_size_total[s]))
	;    
	;    PLOTS, overshoot_total[s], ave_temp_total[s], $
	;    	    PSYM    = 8, $
	;    	    SYMSIZE = 4, $
	;    		NOCLIP  = 0, $
	;			COLOR   = table[iup]
	;ENDFOR
	;
	;COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
	;	RANGE = [0, 100], $
	;	TICKS = 5, $
	;	TITLE = 'Updraft Strength (area*updraft)', $
	;	POSIT = bar_pos

	;;Plot, updraft vs temp and color by h2o
	;PLOT, updraft_size_total, ave_temp_total, /NODATA, $
	;			xtitle = 'Updraft Strength (area*speed)', ytitle = "Mean Tropopause Temp (K)", PSYM = 4, $
	;			charsize = 2, yrange = [240, 190], ylog = 0, xrange = [1,600], title = start_date, $
	;			POSITION = map_pos, _EXTRA = _extra

	;PLOT, updraft_size_total, ave_cyl_temp, /NODATA, $
	;			xtitle = 'Updraft Strength (area*speed)', ytitle = "Mean Cylinder Temp (K)", PSYM = 4, $
	;			yrange = [250, 210], ylog = 0, xrange = [1,600], title = start_date, POSITION = map_pos
;	;			xtitle = 'Normalized Updraft Strength (area*speed)', ytitle = "Mean Cylinder Temp (K)", PSYM = 4, $
;	;			yrange = [240, 190], ylog = 0, xrange = [0,1], title = start_date, POSITION = map_pos

	PLOT, max_w_total, ave_cyl_temp, /NODATA, $
				xtitle = 'Max W (m/s)', ytitle = "Mean Cylinder Temp (K)", PSYM = 4, $
				yrange = [250, 210], ylog = 0, xrange = [0,50], title = start_date, POSITION = map_pos
	

	;h2o_norm 	  = FLTARR(N_ELEMENTS(ave_h2o_nc_total))
	;updraft_norm  = FLTARR(N_ELEMENTS(ave_h2o_nc_total))
	;temp_norm 	  = FLTARR(N_ELEMENTS(ave_h2o_nc_total))
	;
	;ih2o    = SORT(ave_h2o_nc_total)  
	;h2osort = ave_h2o_nc_total[ih2o]  
	;h2o5    = h2osort[PERCENTILE(h2osort,5, /NOSORT,/NAN)]
	;h2o95   = h2osort[PERCENTILE(h2osort,95,/NOSORT,/NAN)]
	;
	;iupdraft    = SORT(updraft_size_total)  
	;updraftsort = updraft_size_total[iupdraft]  
	;updraft5    = updraftsort[PERCENTILE(updraftsort,5, /NOSORT,/NAN)]
	;updraft95   = updraftsort[PERCENTILE(updraftsort,95,/NOSORT,/NAN)]
	;
	;itemp    = SORT(ave_cyl_temp)  
	;tempsort = ave_cyl_temp[itemp]  
	;temp5    = tempsort[PERCENTILE(tempsort,5, /NOSORT,/NAN)]
	;temp95   = tempsort[PERCENTILE(tempsort,95,/NOSORT,/NAN)]
;
	;;;;Color by average h2o concentration
	;USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation    
	;FOR s = 0, N_ELEMENTS(ave_h2o_nc_total)-1 DO BEGIN
	;   
	;	h2o_norm[s]     = ((ave_h2o_nc_total[s]   - h2o5 )    / (h2o95     - h2o5 )) * 100.0
	;	updraft_norm[s] = ((updraft_size_total[s] - updraft5) / (updraft95 - updraft5))
	;	temp_norm[s]    = ((ave_cyl_temp[s]       - temp5)    / (temp95    - temp5))
;
	;	IF (h2o_norm[s] LT 0) THEN h2o_norm[s] = 0.0
	;	
	;    ;table = HCL_COLOR_TABLE(100, HUE_RANGE = [100.0, 300.0])
	;    ;h2olevels = FINDGEN(100)
	;    ;ih2o = WHERE(h2olevels EQ ROUND(h2o_norm[s]))
	;   
	;    table = HCL_COLOR_TABLE(30, HUE_RANGE = [100.0, 300.0])
	;    h2olevels = FINDGEN(30)
	;    ih2o = WHERE(h2olevels EQ ROUND(ave_h2o_nc_total[s]))
	;    
	;    PLOTS, updraft_size_total[s], ave_cyl_temp[s], $
	;    	    PSYM    = 8, $
	;    	    SYMSIZE = 4, $
	;    		NOCLIP  = 0, $
	;			COLOR = table[ih2o]
	;ENDFOR
	;	
	;COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
	;	RANGE  = [0, 30], $
	;	TICKS  = 5, $
	;	TITLE  = 'Average H2O (NC) Concentration (ppm)', $
;;	TITLE = 'Normalized H2O (NC) Concentration (x100)', $
	;	POSIT  = bar_pos, $
	;	_EXTRA = charsize

	;Color by CO concentration
	USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation    
	FOR s = 0, N_ELEMENTS(ave_co_total)-1 DO BEGIN
		
	    table = HCL_COLOR_TABLE(100, HUE_RANGE = [100.0, 300.0])
	    colevels = FINDGEN(100)+65.0
	    ico = WHERE(colevels EQ ROUND(ave_co_total[s]))
	    
	    PLOTS, max_w_total[s], ave_cyl_temp[s], $
	    	    PSYM    = 8, $
	    	    SYMSIZE = 4, $
	    		NOCLIP  = 0, $
				COLOR = table[ico]
	ENDFOR
	
	COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
		RANGE = [65, 165], $
		TICKS = 5, $
		TITLE = 'Average CO Concentration (ppbv)', $
		POSIT = bar_pos

ENDIF

IF (KEYWORD_SET(ICE)) THEN BEGIN
	ibad = WHERE(mean_ztrop_total*1.0E-3 LT 9.5) 
	ave_h2o_total[ibad] = !Values.F_NaN
	overshoot_total[ibad] = !Values.F_NaN
	mean_ztrop_total[ibad] = !Values.F_NaN
	ave_temp_total[ibad] = !Values.F_NaN
	ave_ice_total[ibad] = !Values.F_NaN
	
	;PLOT, overshoot_total, ave_temp_total, /NODATA, $
	;			xtitle = 'Overshoot Depth (km)', ytitle = "Mean Tropopause Temp (K)", PSYM = 4, $
	;			yrange = [190, 240], ylog = 0, xrange = [0,5], title = start_date, POSITION = map_pos
;;
	;ave_ice_total = ave_ice_total*1.0E5
	;;;;Color by average temp concentration
	;USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation    
	;FOR s = 0, N_ELEMENTS(ave_ice_total)-1 DO BEGIN
	;    table = HCL_COLOR_TABLE(50, HUE_RANGE = [100.0, 300.0])
	;    icelevels = FINDGEN(50)
	;    iice = WHERE(icelevels EQ ROUND(ave_ice_total[s]))
	;    
	;    PLOTS, overshoot_total[s], ave_temp_total[s], $
	;    	    PSYM    = 8, $
	;    	    SYMSIZE = 4, $
	;    		NOCLIP  = 0, $
	;			COLOR   = table[iice]
	;ENDFOR
	;
	;COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
	;	RANGE = [0, 50], $
	;	TICKS = 5, $
	;	TITLE = 'Average Ice (10^5 kg/kg)', $
	;	POSIT = bar_pos
	

	;;Plot h2o in ice-temperature space
	;PLOT, ave_ice_total*1.0E5, ave_temp_total, /NODATA, $
	;			xtitle = 'Average Ice Mixing Ratio (10^5 kg/kg)', ytitle = "Mean Tropopause Temp (K)", PSYM = 4, $
	;			yrange = [240, 190], ylog = 0, xrange = [0,50], title = start_date, POSITION = map_pos

	PLOT, ave_ice_total*1.0E5, ave_cyl_temp, /NODATA, $
				xtitle = 'Average Ice Mixing Ratio (10^5 kg/kg)', ytitle = "Mean Cylinder Temp (K)", PSYM = 4, $
				yrange = [240, 190], ylog = 0, xrange = [0,50], title = start_date, POSITION = map_pos
;				xtitle = 'Normalized Ice Mixing Ratio', ytitle = "Mean Cylinder Temp (K)", PSYM = 4, $
;				yrange = [240, 190], ylog = 0, xrange = [0,1], title = start_date, POSITION = map_pos

	h2o_norm  = FLTARR(N_ELEMENTS(ave_h2o_nc_total))
	ice_norm  = FLTARR(N_ELEMENTS(ave_h2o_nc_total))
	temp_norm = FLTARR(N_ELEMENTS(ave_h2o_nc_total))
	
	ih2o    = SORT(ave_h2o_nc_total)  
	h2osort = ave_h2o_nc_total[ih2o]  
	h2o5    = h2osort[PERCENTILE(h2osort,5, /NOSORT,/NAN)]
	h2o95   = h2osort[PERCENTILE(h2osort,95,/NOSORT,/NAN)]
	
	ave_ice_total = ave_ice_total*1.0E5
	iice          = SORT(ave_ice_total)  
	icesort       = ave_ice_total[iice]  
	ice5          = icesort[PERCENTILE(icesort,5, /NOSORT,/NAN)]
	ice95         = icesort[PERCENTILE(icesort,95,/NOSORT,/NAN)]
	
	itemp    = SORT(ave_cyl_temp)  
	tempsort = ave_cyl_temp[itemp]  
	temp5    = tempsort[PERCENTILE(tempsort,5, /NOSORT,/NAN)]
	temp95   = tempsort[PERCENTILE(tempsort,95,/NOSORT,/NAN)]

	;;;;;Color by average temp concentration
	USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation    
	FOR s = 0, N_ELEMENTS(ave_h2o_nc_total)-1 DO BEGIN
		h2o_norm[s]  = ((ave_h2o_nc_total[s] - h2o5 ) / (h2o95  - h2o5 )) * 100.0
		ice_norm[s]  = ((ave_ice_total[s]    - ice5 ) / (ice95  - ice5 ))
		temp_norm[s] = ((ave_cyl_temp[s]     - temp5) / (temp95 - temp5))

		IF (h2o_norm[s] LT 0) THEN h2o_norm[s] = 0.0
		
	    ;table = HCL_COLOR_TABLE(100, HUE_RANGE = [100.0, 300.0])
	    ;h2olevels = FINDGEN(100)
	    ;ih2o = WHERE(h2olevels EQ ROUND(h2o_norm[s]))
	   
	    table = HCL_COLOR_TABLE(10, HUE_RANGE = [100.0, 300.0])
	    h2olevels = FINDGEN(10)
	    ih2o = WHERE(h2olevels EQ ROUND(ave_h2o_nc_total[s]))
	    
	    PLOTS, ave_ice_total[s], ave_cyl_temp[s], $
	    	    PSYM    = 8, $
	    	    SYMSIZE = 4, $
	    		NOCLIP  = 0, $
				COLOR   = table[ih2o]
	ENDFOR
	
	COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
		RANGE = [0, 10], $
		TICKS = 5, $
		TITLE = 'Average H2O (NC) Concentration (ppm)', $
;		TITLE = 'Normalized H2O (NC) Concentration (x100)', $
		POSIT = bar_pos
	
	;Color by CO concentration
	;	USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation    
	;FOR s = 0, N_ELEMENTS(ave_co_total)-1 DO BEGIN
	;    table = HCL_COLOR_TABLE(80, HUE_RANGE = [100.0, 300.0])
	;    colevels = 30.0+FINDGEN(80)
	;    ico = WHERE(colevels EQ ROUND(ave_co_total[s]))
	;    
	;    PLOTS, ave_ice_total[s]*1.0E5, ave_temp_total[s], $
	;    	    PSYM    = 8, $
	;    	    SYMSIZE = 4, $
	;    		NOCLIP  = 0, $
	;			COLOR = table[ico]
	;ENDFOR
	;
	;COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
	;	RANGE = [30, 110], $
	;	TICKS = 5, $
	;	TITLE = 'Average CO Concentration (ppm)', $
	;	POSIT = bar_pos

	
	;;Plot unstable layers in ice-temperature space
	;PLOT, ave_ice_total*1.0E5, ave_temp_total, /NODATA, $
	;			xtitle = 'Ave Ice Concentration (10^5 kg/kg)', ytitle = "Mean Tropopause Temp (K)", PSYM = 4, $
	;			yrange = [190, 240], ylog = 0, xrange = [0,50], title = start_date, POSITION = map_pos
;;;
	;;;;;Color by num unstable layers
	;USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation    
	;FOR s = 0, N_ELEMENTS(unstable_total_tot)-1 DO BEGIN
	;    table = HCL_COLOR_TABLE(2300, HUE_RANGE = [100.0, 300.0])
	;    ullevels = FINDGEN(2300)
	;    iul = WHERE(ullevels EQ ROUND(unstable_total_tot[s]))
	;    
	;    PLOTS, ave_ice_total[s]*1.0E5, ave_temp_total[s], $
	;    	    PSYM    = 8, $
	;    	    SYMSIZE = 4, $
	;    		NOCLIP  = 0, $
	;			COLOR   = table[iul]
	;ENDFOR
	;
	;COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
	;	RANGE = [0, 2300], $
	;	TICKS = 5, $
	;	TITLE = 'Num. Unstable Layers Total', $
	;	POSIT = bar_pos

ENDIF


IF (KEYWORD_SET(UNSTABLE)) THEN BEGIN
	
	iunstable = WHERE(unstable_total_tot GT 1, icount, COMPLEMENT = stable)
	IF (icount GT 0) THEN BEGIN
		h2o_unstable   = ave_h2o_total[iunstable]
		over_unstable  = overshoot_total[iunstable]
		ztrop_unstable = mean_ztrop_total[iunstable]
		temp_unstable  = ave_temp_total[iunstable]

		h2o_stable 	   = ave_h2o_total[stable]
		over_stable    = overshoot_total[stable]
		ztrop_stable   = mean_ztrop_total[stable]
		temp_stable    = ave_temp_total[stable]
	ENDIF
	
	PLOT, unstable_total_tot, ave_temp_total, /NODATA, $
				xtitle = 'Unstable Layers (#)', ytitle = "Mean Tropopause Temp (K)", PSYM = 4, $
				yrange = [230, 180], ylog = 0, xrange = [0,10000], title = start_date, POSITION = map_pos

	USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation    
	FOR s = 0, N_ELEMENTS(ave_h2o_total)-1 DO BEGIN
	    table = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])
	    ullevels = FINDGEN(20)
	    iul = WHERE(ullevels EQ ROUND(ave_h2o_nc_total[s]))
	    
	    PLOTS, unstable_total_tot[s], ave_temp_total[s], $
	    	    PSYM    = 8, $
	    	    SYMSIZE = 4, $
	    		NOCLIP  = 0, $
				COLOR   = table[iul]
	ENDFOR
	
	COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
		RANGE = [0, 20], $
		TICKS = 5, $
		TITLE = 'Average H2O (NC) Concentration (ppmv)', $
		POSIT = bar_pos

;
	;;;;Color by unstable layers 
	;;USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation    
	;;FOR s = 0, N_ELEMENTS(unstable_total_ic)-1 DO BEGIN
	;;    table = HCL_COLOR_TABLE(2000, HUE_RANGE = [100.0, 300.0])
	;;    ullevels = FINDGEN(2000)
	;;    iul = WHERE(ullevels EQ ROUND(unstable_total_ic[s]))
	;;    
	;;    PLOTS, overshoot_total[s], mean_ztrop_total[s]*1.0E-3, $
	;;    	    PSYM    = 8, $
	;;    	    SYMSIZE = 4, $
	;;    		NOCLIP  = 0, $
	;;			COLOR   = table[iul]
	;;ENDFOR
	;;
	;;COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
	;;	RANGE = [0, 2000], $
	;;	TICKS = 5, $
	;;	TITLE = 'Num. Unstable Layers In-Cloud', $
	;;	POSIT = bar_pos
;
	;PRINT, MAX(unstable_total_tot,/NAN)
	;USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation    
	;FOR s = 0, N_ELEMENTS(unstable_total_tot)-1 DO BEGIN
	;    table = HCL_COLOR_TABLE(10000, HUE_RANGE = [100.0, 300.0])
	;    ullevels = FINDGEN(10000)
	;    iul = WHERE(ullevels EQ ROUND(unstable_total_tot[s]))
	;    
	;    PLOTS, overshoot_total[s], ave_temp_total[s], $
	;    	    PSYM    = 8, $
	;    	    SYMSIZE = 4, $
	;    		NOCLIP  = 0, $
	;			COLOR   = table[iul]
	;ENDFOR
	;
	;COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
	;	RANGE = [0, 10000], $
	;	TICKS = 5, $
	;	TITLE = 'Num. Unstable Layers Total', $
	;	POSIT = bar_pos


	;For stable areas
	;PLOT, over_stable, temp_stable, /NODATA, $
	;			xtitle = 'Overshoot Depth (km)', ytitle = "Mean Tropopause Temp (K)", PSYM = 4, $
	;			yrange = [200, 230], ylog = 0, xrange = [0,5], title = start_date, POSITION = map_pos
;;;;
	;USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation    
	;FOR s = 0, N_ELEMENTS(h2o_stable)-1 DO BEGIN
	;    table = HCL_COLOR_TABLE(50, HUE_RANGE = [100.0, 300.0])
	;    ullevels = FINDGEN(50)
	;    iul = WHERE(ullevels EQ ROUND(h2o_stable[s]))
	;    
	;    PLOTS, over_stable[s], temp_stable[s], $
	;    	    PSYM    = 8, $
	;    	    SYMSIZE = 4, $
	;    		NOCLIP  = 0, $
	;			COLOR   = table[iul]
	;ENDFOR
	;
	;COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
	;	RANGE = [0, 50], $
	;	TICKS = 5, $
	;	TITLE = 'Average H2O NC Concentration-Stable (ppmv)', $
	;	POSIT = bar_pos

	;For unstable areas
	;PLOT, over_unstable, temp_unstable, /NODATA, $
	;			xtitle = 'Overshoot Depth (km)', ytitle = "Mean Tropopause Temp (K)", PSYM = 4, $
	;			yrange = [230, 200], ylog = 0, xrange = [0,5], title = start_date, POSITION = map_pos
;;;
	;USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation    
	;FOR s = 0, N_ELEMENTS(h2o_unstable)-1 DO BEGIN
	;    table = HCL_COLOR_TABLE(50, HUE_RANGE = [100.0, 300.0])
	;    ullevels = FINDGEN(50)
	;    iul = WHERE(ullevels EQ ROUND(h2o_unstable[s]))
	;    
	;    PLOTS, over_unstable[s], temp_unstable[s], $
	;    	    PSYM    = 8, $
	;    	    SYMSIZE = 4, $
	;    		NOCLIP  = 0, $
	;			COLOR   = table[iul]
	;ENDFOR
	;
	;COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
	;	RANGE = [0, 50], $
	;	TICKS = 5, $
	;	TITLE = 'Average H2O Concentration-Unstable(ppmv)', $
	;	POSIT = bar_pos

ENDIF



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

STOP

END