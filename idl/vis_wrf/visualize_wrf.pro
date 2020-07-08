;***********************************************************************
PRO VISUALIZE_WRF_EVENT, ev

COMPILE_OPT IDL2																														;Set compile options

WIDGET_CONTROL, ev.top, GET_UVALUE = state																					;Get state structure

;HELP, ev, /STR																														;For debugging

valid_coord               = 0																										;Flag for coord validity
refresh_graphics_now      = 0																										;Graphics refresh flag
save_graphics_now         = 0																										;Save graphics flag
refresh_alts              = 0																										;Refresh altitudes flag
change_file               = 0																										;Change file flag

CASE ev.id OF																															;Handle widget events	
	state.main_id : BEGIN
		IF (ev.tab EQ 2) THEN BEGIN
			WIDGET_CONTROL, state.bgnd_gray_draw_id,  GET_VALUE = gray_window											;Get IDs of background color draw windows
			WIDGET_CONTROL, state.bgnd_white_draw_id, GET_VALUE = white_window
			WIDGET_CONTROL, state.bgnd_tan_draw_id,   GET_VALUE = tan_window
			WIDGET_CONTROL, state.bgnd_blue_draw_id,  GET_VALUE = blue_window
			
			WSET, gray_window
			image  = IMAGE_24(REBIN([state.missing_color[0],state.missing_color[0]],2,2,/SAMPLE), TRUE = 3)					;Convert data to image
			x0     = CONVERT_COORD(0.0, 0.0, /NORMAL, /TO_DEVICE)															;Device coords of lower left corner
			x1     = CONVERT_COORD(1.0, 1.0, /NORMAL, /TO_DEVICE)															;Device coords of upper right corner
			txsize = LONG(x1[0] - x0[0] + 1) > 2																				;Width (device coords)
			tysize = LONG(x1[1] - x0[1] + 1) > 2																				;Height (device coords)
			TV, [[[CONGRID(image[*,*,0], txsize, tysize)]], $																;Display image
				  [[CONGRID(image[*,*,1], txsize, tysize)]], $		
				  [[CONGRID(image[*,*,2], txsize, tysize)]]], $		
					x0[0], x0[1]+1, TRUE = 3, XSIZE = xsize, YSIZE = ysize, /DEVICE		
			PLOTS, [0.0,1.0,1.0,0.0,0.0], [0.0,0.0,0.95,0.95,0.0], THICK = 2, /NORMAL

			WSET, white_window
			image  = IMAGE_24(REBIN([state.missing_color[1],state.missing_color[1]],2,2,/SAMPLE), TRUE = 3)					;Convert data to image
			x0     = CONVERT_COORD(0.0, 0.0, /NORMAL, /TO_DEVICE)															;Device coords of lower left corner
			x1     = CONVERT_COORD(1.0, 1.0, /NORMAL, /TO_DEVICE)															;Device coords of upper right corner
			txsize = LONG(x1[0] - x0[0] + 1) > 2																				;Width (device coords)
			tysize = LONG(x1[1] - x0[1] + 1) > 2																				;Height (device coords)
			TV, [[[CONGRID(image[*,*,0], txsize, tysize)]], $																;Display image
				  [[CONGRID(image[*,*,1], txsize, tysize)]], $		
				  [[CONGRID(image[*,*,2], txsize, tysize)]]], $		
					x0[0], x0[1]+1, TRUE = 3, XSIZE = xsize, YSIZE = ysize, /DEVICE		
			PLOTS, [0.0,1.0,1.0,0.0,0.0], [0.0,0.0,0.95,0.95,0.0], THICK = 2, /NORMAL

			WSET, tan_window
			image  = IMAGE_24(REBIN([state.missing_color[2],state.missing_color[2]],2,2,/SAMPLE), TRUE = 3)					;Convert data to image
			x0     = CONVERT_COORD(0.0, 0.0, /NORMAL, /TO_DEVICE)															;Device coords of lower left corner
			x1     = CONVERT_COORD(1.0, 1.0, /NORMAL, /TO_DEVICE)															;Device coords of upper right corner
			txsize = LONG(x1[0] - x0[0] + 1) > 2																				;Width (device coords)
			tysize = LONG(x1[1] - x0[1] + 1) > 2																				;Height (device coords)
			TV, [[[CONGRID(image[*,*,0], txsize, tysize)]], $																;Display image
				  [[CONGRID(image[*,*,1], txsize, tysize)]], $		
				  [[CONGRID(image[*,*,2], txsize, tysize)]]], $		
					x0[0], x0[1]+1, TRUE = 3, XSIZE = xsize, YSIZE = ysize, /DEVICE		
			PLOTS, [0.0,1.0,1.0,0.0,0.0], [0.0,0.0,0.95,0.95,0.0], THICK = 2, /NORMAL

			WSET, blue_window
			image  = IMAGE_24(REBIN([state.missing_color[3],state.missing_color[3]],2,2,/SAMPLE), TRUE = 3)					;Convert data to image
			x0     = CONVERT_COORD(0.0, 0.0, /NORMAL, /TO_DEVICE)															;Device coords of lower left corner
			x1     = CONVERT_COORD(1.0, 1.0, /NORMAL, /TO_DEVICE)															;Device coords of upper right corner
			txsize = LONG(x1[0] - x0[0] + 1) > 2																				;Width (device coords)
			tysize = LONG(x1[1] - x0[1] + 1) > 2																				;Height (device coords)
			TV, [[[CONGRID(image[*,*,0], txsize, tysize)]], $																;Display image
				  [[CONGRID(image[*,*,1], txsize, tysize)]], $		
				  [[CONGRID(image[*,*,2], txsize, tysize)]]], $		
					x0[0], x0[1]+1, TRUE = 3, XSIZE = xsize, YSIZE = ysize, /DEVICE		
			PLOTS, [0.0,1.0,1.0,0.0,0.0], [0.0,0.0,0.95,0.95,0.0], THICK = 2, /NORMAL

			WIDGET_CONTROL, state.color_table1_draw_id, GET_VALUE = table1_window									;Get IDs of color table draw windows
			WIDGET_CONTROL, state.color_table2_draw_id, GET_VALUE = table2_window
			WIDGET_CONTROL, state.color_table3_draw_id, GET_VALUE = table3_window
			WIDGET_CONTROL, state.color_table4_draw_id, GET_VALUE = table4_window
			WIDGET_CONTROL, state.color_table5_draw_id, GET_VALUE = table5_window

			WSET, table1_window
			table  = VISUALIZE_WRF_COLOR(0)
			image  = IMAGE_24(REBIN(table, N_ELEMENTS(table), 2, /SAMPLE), TRUE = 3)								;Convert data to image
			x0     = CONVERT_COORD(0.0, 0.0, /NORMAL, /TO_DEVICE)															;Device coords of lower left corner
			x1     = CONVERT_COORD(1.0, 1.0, /NORMAL, /TO_DEVICE)															;Device coords of upper right corner
			txsize = LONG(x1[0] - x0[0] + 1) > 2																				;Width (device coords)
			tysize = LONG(x1[1] - x0[1] + 1) > 2																				;Height (device coords)
			TV, [[[CONGRID(image[*,*,0], txsize, tysize)]], $																;Display image
				  [[CONGRID(image[*,*,1], txsize, tysize)]], $		
				  [[CONGRID(image[*,*,2], txsize, tysize)]]], $		
					x0[0], x0[1]+1, TRUE = 3, XSIZE = xsize, YSIZE = ysize, /DEVICE		
			PLOTS, [0.0,1.0,1.0,0.0,0.0], [0.0,0.0,0.95,0.95,0.0], THICK = 2, /NORMAL

			WSET, table2_window		
			table  = VISUALIZE_WRF_COLOR(1)		
			image  = IMAGE_24(REBIN(table, N_ELEMENTS(table), 2, /SAMPLE), TRUE = 3)								;Convert data to image
			x0     = CONVERT_COORD(0.0, 0.0, /NORMAL, /TO_DEVICE)															;Device coords of lower left corner
			x1     = CONVERT_COORD(1.0, 1.0, /NORMAL, /TO_DEVICE)															;Device coords of upper right corner
			txsize = LONG(x1[0] - x0[0] + 1) > 2																				;Width (device coords)
			tysize = LONG(x1[1] - x0[1] + 1) > 2																				;Height (device coords)
			TV, [[[CONGRID(image[*,*,0], txsize, tysize)]], $																;Display image
				  [[CONGRID(image[*,*,1], txsize, tysize)]], $		
				  [[CONGRID(image[*,*,2], txsize, tysize)]]], $		
					x0[0], x0[1]+1, TRUE = 3		
			PLOTS, [0.0,1.0,1.0,0.0,0.0], [0.0,0.0,0.95,0.95,0.0], THICK = 2, /NORMAL

			WSET, table3_window		
			table  = VISUALIZE_WRF_COLOR(2)		
			image  = IMAGE_24(REBIN(table, N_ELEMENTS(table), 2, /SAMPLE), TRUE = 3)								;Convert data to image
			x0     = CONVERT_COORD(0.0, 0.0, /NORMAL, /TO_DEVICE)															;Device coords of lower left corner
			x1     = CONVERT_COORD(1.0, 1.0, /NORMAL, /TO_DEVICE)															;Device coords of upper right corner
			txsize = LONG(x1[0] - x0[0] + 1) > 2																				;Width (device coords)
			tysize = LONG(x1[1] - x0[1] + 1) > 2																				;Height (device coords)
			TV, [[[CONGRID(image[*,*,0], txsize, tysize)]], $																;Display image
				  [[CONGRID(image[*,*,1], txsize, tysize)]], $		
				  [[CONGRID(image[*,*,2], txsize, tysize)]]], $		
					x0[0], x0[1]+1, TRUE = 3		
			PLOTS, [0.0,1.0,1.0,0.0,0.0], [0.0,0.0,0.95,0.95,0.0], THICK = 2, /NORMAL

			WSET, table4_window		
			table  = VISUALIZE_WRF_COLOR(3)		
			image  = IMAGE_24(REBIN(table, N_ELEMENTS(table), 2, /SAMPLE), TRUE = 3)								;Convert data to image
			x0     = CONVERT_COORD(0.0, 0.0, /NORMAL, /TO_DEVICE)															;Device coords of lower left corner
			x1     = CONVERT_COORD(1.0, 1.0, /NORMAL, /TO_DEVICE)															;Device coords of upper right corner
			txsize = LONG(x1[0] - x0[0] + 1) > 2																				;Width (device coords)
			tysize = LONG(x1[1] - x0[1] + 1) > 2																				;Height (device coords)
			TV, [[[CONGRID(image[*,*,0], txsize, tysize)]], $																;Display image
				  [[CONGRID(image[*,*,1], txsize, tysize)]], $		
				  [[CONGRID(image[*,*,2], txsize, tysize)]]], $		
					x0[0], x0[1]+1, TRUE = 3		
			PLOTS, [0.0,1.0,1.0,0.0,0.0], [0.0,0.0,0.95,0.95,0.0], THICK = 2, /NORMAL

			WSET, table5_window		
			table  = VISUALIZE_WRF_COLOR(4)		
			image  = IMAGE_24(REBIN(table, N_ELEMENTS(table), 2, /SAMPLE), TRUE = 3)								;Convert data to image
			x0     = CONVERT_COORD(0.0, 0.0, /NORMAL, /TO_DEVICE)															;Device coords of lower left corner
			x1     = CONVERT_COORD(1.0, 1.0, /NORMAL, /TO_DEVICE)															;Device coords of upper right corner
			txsize = LONG(x1[0] - x0[0] + 1) > 2																				;Width (device coords)
			tysize = LONG(x1[1] - x0[1] + 1) > 2																				;Height (device coords)
			TV, [[[CONGRID(image[*,*,0], txsize, tysize)]], $																;Display image
				  [[CONGRID(image[*,*,1], txsize, tysize)]], $		
				  [[CONGRID(image[*,*,2], txsize, tysize)]]], $		
					x0[0], x0[1]+1, TRUE = 3		
			PLOTS, [0.0,1.0,1.0,0.0,0.0], [0.0,0.0,0.95,0.95,0.0], THICK = 2, /NORMAL

			WSET, state.draw_window																									;Set window to plot window
		ENDIF
	END

	;TEXT WIDGET EVENTS        ***************************************************************************

	state.indir_text_id : BEGIN
		WIDGET_CONTROL, state.indir_text_id, GET_VALUE = root																;Get the text
		state.indir = STRING(root)																									;Save value
		state.indir_altered  = 1																									;Set directory altered flag
		WIDGET_CONTROL, state.indir_text_id, SET_VALUE = STRING(root)													;Set the text
	END

	state.x1_text_id : BEGIN
		ON_IOERROR, revert
		WIDGET_CONTROL, state.x1_text_id, GET_VALUE = root																	;Get the text
		state.x1 = FLOAT(root)																										;Save value
		WIDGET_CONTROL, state.x1_text_id, SET_VALUE = STRING(state.x1, FORMAT="(F10.2)")							;Set the text
		refresh_graphics_now = 1																									;Set flag to refresh graphics
		valid_coord          = 1																									;Set flag that it is a valid coordinate
		state.redraw_map     = 1																									;Set flag to redraw map
		
		IF (TOTAL(FINITE([state.x1, state.x2, state.y1, state.y2])) EQ 4.0) THEN BEGIN
			IF (state.zoom) THEN $
				state.map_limit = [(state.y1 < state.y2),(state.x1 < state.x2),$										;Store longitude-latitude limit for map
										 (state.y1 > state.y2),(state.x1 > state.x2)] $
			ELSE $
				state.arc = MAP_2POINTS(state.x1, state.y1, state.x2, state.y2, $										;Compute longitudes and latitudes on arc
												NPATH = state.arc_elements, /RHUMB)
		ENDIF ELSE BEGIN
			IF (state.zoom) THEN $
				state.map_limit = state.file_domain $																			;Reset map limit to original domain
			ELSE $
				state.arc = MAKE_ARRAY([2, state.arc_elements], VALUE = !VALUES.D_NAN)								;Fill arc with NaNs
		ENDELSE
	END
		
	state.x2_text_id : BEGIN
		ON_IOERROR, revert
		WIDGET_CONTROL, state.x2_text_id, GET_VALUE = root																	;Get the text
		state.x2 = FLOAT(root)																										;Save value
		WIDGET_CONTROL, state.x2_text_id, SET_VALUE = STRING(state.x2, FORMAT="(F10.2)")							;Set the text
		refresh_graphics_now = 1																									;Set flag to refresh graphics
		valid_coord          = 1																									;Set flag that it is a valid coordinate
		state.redraw_map     = 1																									;Set flag to redraw map
		
		IF (TOTAL(FINITE([state.x1, state.x2, state.y1, state.y2])) EQ 4.0) THEN BEGIN
			IF (state.zoom) THEN $
				state.map_limit = [(state.y1 < state.y2),(state.x1 < state.x2),$										;Store longitude-latitude limit for map
										 (state.y1 > state.y2),(state.x1 > state.x2)] $
			ELSE $
				state.arc = MAP_2POINTS(state.x1, state.y1, state.x2, state.y2, $										;Compute longitudes and latitudes on arc
												NPATH = state.arc_elements, /RHUMB)
		ENDIF ELSE BEGIN
			IF (state.zoom) THEN $
				state.map_limit = state.file_domain $																			;Reset map limit to original domain
			ELSE $
				state.arc = MAKE_ARRAY([2, state.arc_elements], VALUE = !VALUES.D_NAN)								;Fill arc with NaNs
		ENDELSE
	END

	state.y1_text_id : BEGIN
		ON_IOERROR, revert
		WIDGET_CONTROL, state.y1_text_id, GET_VALUE = root																	;Get the text
		state.y1 = FLOAT(root)																										;Save value
		WIDGET_CONTROL, state.y1_text_id, SET_VALUE = STRING(state.y1, FORMAT="(F10.2)")							;Set the text
		refresh_graphics_now = 1																									;Set flag to refresh graphics
		valid_coord          = 1																									;Set flag that it is a valid coordinate
		state.redraw_map     = 1																									;Set flag to redraw map
		
		IF (TOTAL(FINITE([state.x1, state.x2, state.y1, state.y2])) EQ 4.0) THEN BEGIN
			IF (state.zoom) THEN $
				state.map_limit = [(state.y1 < state.y2),(state.x1 < state.x2),$										;Store longitude-latitude limit for map
										 (state.y1 > state.y2),(state.x1 > state.x2)] $
			ELSE $
				state.arc = MAP_2POINTS(state.x1, state.y1, state.x2, state.y2, $										;Compute longitudes and latitudes on arc
												NPATH = state.arc_elements, /RHUMB)
		ENDIF ELSE BEGIN
			IF (state.zoom) THEN $
				state.map_limit = state.file_domain $																			;Reset map limit to original domain
			ELSE $
				state.arc = MAKE_ARRAY([2, state.arc_elements], VALUE = !VALUES.D_NAN)								;Fill arc with NaNs
		ENDELSE
	END

	state.y2_text_id : BEGIN
		ON_IOERROR, revert
		WIDGET_CONTROL, state.y2_text_id, GET_VALUE = root																	;Get the text
		state.y2 = FLOAT(root)																										;Save value
		WIDGET_CONTROL, state.y2_text_id, SET_VALUE = STRING(state.y2, FORMAT="(F10.2)")							;Set the text
		refresh_graphics_now = 1																									;Set flag to refresh graphics
		valid_coord          = 1																									;Set flag that it is a valid coordinate
		state.redraw_map     = 1																									;Set flag to redraw map
		
		IF (TOTAL(FINITE([state.x1, state.x2, state.y1, state.y2])) EQ 4.0) THEN BEGIN
			IF (state.zoom) THEN $
				state.map_limit = [(state.y1 < state.y2),(state.x1 < state.x2),$										;Store longitude-latitude limit for map
										 (state.y1 > state.y2),(state.x1 > state.x2)] $
			ELSE $
				state.arc = MAP_2POINTS(state.x1, state.y1, state.x2, state.y2, $										;Compute longitudes and latitudes on arc
												NPATH = state.arc_elements, /RHUMB)
		ENDIF ELSE BEGIN
			IF (state.zoom) THEN $
				state.map_limit = state.file_domain $																			;Reset map limit to original domain
			ELSE $
				state.arc = MAKE_ARRAY([2, state.arc_elements], VALUE = !VALUES.D_NAN)								;Fill arc with NaNs
		ENDELSE
	END

	state.map_echotop_trop_alt_id : BEGIN
		WIDGET_CONTROL, state.map_echotop_trop_alt_id, GET_VALUE = root												;Get the text
		state.tropopause = FLOAT(root)																				;Save value
		WIDGET_CONTROL, state.map_echotop_trop_alt_id, SET_VALUE = STRING(root)										;Set the text	
		IF (state.map_type EQ 2) THEN BEGIN
			refresh_graphics_now = 1
			state.redraw_map     = 1
		ENDIF
	END

	state.melting_level_alt_id : BEGIN
		WIDGET_CONTROL, state.melting_level_alt_id, GET_VALUE = root													;Get the text
		state.zmelt = FLOAT(root)																									;Save value
		WIDGET_CONTROL, state.melting_level_alt_id, SET_VALUE = STRING(root)											;Set the text	
		IF (state.map_type EQ 3) THEN BEGIN
			refresh_graphics_now = 1
			state.redraw_map     = 1
		ENDIF
	END
	
	state.z1_text_id : BEGIN
		ON_IOERROR, revert
		WIDGET_CONTROL, state.z1_text_id, GET_VALUE = root																	;Get the text
		state.z1 = FLOAT(root)																										;Save value
		WIDGET_CONTROL, state.z1_text_id, SET_VALUE = STRING(state.z1, FORMAT="(F5.2)")							;Set the text
	END
		
	state.z2_text_id : BEGIN
		ON_IOERROR, revert
		WIDGET_CONTROL, state.z2_text_id, GET_VALUE = root																	;Get the text
		state.z2 = FLOAT(root)																										;Save value
		WIDGET_CONTROL, state.z2_text_id, SET_VALUE = STRING(state.z2, FORMAT="(F5.2)")							;Set the text
	END

	state.range_ring_text_id : BEGIN
		WIDGET_CONTROL, state.range_ring_text_id, GET_VALUE = root														;Get the text
		state.range_interval = FLOAT(root)																						;Save value
		WIDGET_CONTROL, state.range_ring_text_id, SET_VALUE = STRING(root)											;Set the text	
		IF (state.range_rings) THEN BEGIN
			refresh_graphics_now = 1
			state.redraw_map     = 1
		ENDIF
	END

	state.county_path_text_id : BEGIN
		WIDGET_CONTROL, state.county_path_text_id, GET_VALUE = root														;Get the text

		county_path = STRING(root)
		IF (STRLEN(county_path) GT 3) THEN BEGIN
			IF (STRMID(county_path,2,/REVERSE) EQ 'shp') THEN BEGIN
				state.county_path = county_path																					;Save value
				WIDGET_CONTROL, state.county_path_text_id, SET_VALUE = county_path									;Set the text
				IF (state.draw_counties) THEN refresh_graphics_now = 1													;Refresh graphics
			ENDIF
		ENDIF
	END


;; WON'T NEED STORM REPORTS
	state.reports_path_text_id : BEGIN
		WIDGET_CONTROL, state.reports_path_text_id, GET_VALUE = root													;Get the text

		reports_path = STRING(root)
		IF (STRLEN(reports_path) GT 3) THEN BEGIN
			IF (STRMID(reports_path,2,/REVERSE) EQ 'csv') THEN BEGIN
				state.reports_path = reports_path																				;Save value
				WIDGET_CONTROL, state.reports_path_text_id, SET_VALUE = reports_path									;Set the text
				IF (state.draw_reports) THEN BEGIN
					*state.reports       = READ_SPC_REPORTS(reports_path)													;Read reports
					refresh_graphics_now = 1																						;Refresh graphics
				ENDIF
			ENDIF
		ENDIF
	END
					
	state.zh_min_text_id : BEGIN																		
		ON_IOERROR, revert
		WIDGET_CONTROL, state.zh_min_text_id, GET_VALUE = root															;Get the text
		state.bar_min[0] = FLOAT(root)																							;Save value
		WIDGET_CONTROL, state.zh_min_text_id, SET_VALUE = STRING(state.bar_min[0], FORMAT="(F8.2)")			;Set the text
	END

	state.zh_max_text_id : BEGIN
		ON_IOERROR, revert
		WIDGET_CONTROL, state.zh_max_text_id, GET_VALUE = root															;Get the text
		state.bar_max[0] = FLOAT(root)																							;Save value
		WIDGET_CONTROL, state.zh_max_text_id, SET_VALUE = STRING(state.bar_max[0], FORMAT="(F8.2)")			;Set the text
	END

	state.zh_ticks_text_id : BEGIN
		ON_IOERROR, revert
		WIDGET_CONTROL, state.zh_ticks_text_id, GET_VALUE = root															;Get the text
		state.bar_ticks[0] = LONG(root)																							;Save value
		WIDGET_CONTROL, state.zh_ticks_text_id, SET_VALUE = STRING(state.bar_ticks[0], FORMAT="(I8)")		;Set the text
	END

	state.zdr_min_text_id : BEGIN
		ON_IOERROR, revert
		WIDGET_CONTROL, state.zdr_min_text_id, GET_VALUE = root															;Get the text
		state.bar_min[1] = FLOAT(root)																							;Save value
		WIDGET_CONTROL, state.zdr_min_text_id, SET_VALUE = STRING(state.bar_min[1], FORMAT="(F8.2)")			;Set the text
	END

	state.zdr_max_text_id : BEGIN
		ON_IOERROR, revert
		WIDGET_CONTROL, state.zdr_max_text_id, GET_VALUE = root															;Get the text
		state.bar_max[1] = FLOAT(root)																							;Save value
		WIDGET_CONTROL, state.zdr_max_text_id, SET_VALUE = STRING(state.bar_max[1], FORMAT="(F8.2)")			;Set the text
	END

	state.zdr_ticks_text_id : BEGIN
		ON_IOERROR, revert
		WIDGET_CONTROL, state.zdr_ticks_text_id, GET_VALUE = root														;Get the text
		state.bar_ticks[1] = LONG(root)																							;Save value
		WIDGET_CONTROL, state.zdr_ticks_text_id, SET_VALUE = STRING(state.bar_ticks[1], FORMAT="(I8)")		;Set the text
	END
	
	state.kdp_min_text_id : BEGIN
		ON_IOERROR, revert
		WIDGET_CONTROL, state.kdp_min_text_id, GET_VALUE = root															;Get the text
		state.bar_min[2] = FLOAT(root)																							;Save value
		WIDGET_CONTROL, state.kdp_min_text_id, SET_VALUE = STRING(state.bar_min[2], FORMAT="(F8.2)")			;Set the text
	END

	state.kdp_max_text_id : BEGIN
		ON_IOERROR, revert
		WIDGET_CONTROL, state.kdp_max_text_id, GET_VALUE = root															;Get the text
		state.bar_max[2] = FLOAT(root)																							;Save value
		WIDGET_CONTROL, state.kdp_max_text_id, SET_VALUE = STRING(state.bar_max[2], FORMAT="(F8.2)")			;Set the text
	END

	state.kdp_ticks_text_id : BEGIN
		ON_IOERROR, revert
		WIDGET_CONTROL, state.kdp_ticks_text_id, GET_VALUE = root														;Get the text
		state.bar_ticks[2] = LONG(root)																							;Save value
		WIDGET_CONTROL, state.kdp_ticks_text_id, SET_VALUE = STRING(state.bar_ticks[2], FORMAT="(I8)")		;Set the text
	END

	state.rhv_min_text_id : BEGIN
		ON_IOERROR, revert
		WIDGET_CONTROL, state.rhv_min_text_id, GET_VALUE = root															;Get the text
		state.bar_min[3] = FLOAT(root)																							;Save value
		WIDGET_CONTROL, state.rhv_min_text_id, SET_VALUE = STRING(state.bar_min[3], FORMAT="(F8.2)")			;Set the text
	END

	state.rhv_max_text_id : BEGIN
		ON_IOERROR, revert
		WIDGET_CONTROL, state.rhv_max_text_id, GET_VALUE = root															;Get the text
		state.bar_max[3] = FLOAT(root)																							;Save value
		WIDGET_CONTROL, state.rhv_max_text_id, SET_VALUE = STRING(state.bar_max[3], FORMAT="(F8.2)")			;Set the text
	END

	state.rhv_ticks_text_id : BEGIN
		ON_IOERROR, revert
		WIDGET_CONTROL, state.rhv_ticks_text_id, GET_VALUE = root														;Get the text
		state.bar_ticks[3] = LONG(root)																							;Save value
		WIDGET_CONTROL, state.rhv_ticks_text_id, SET_VALUE = STRING(state.bar_ticks[3], FORMAT="(I8)")		;Set the text
	END

	state.frame_rate_text_id : BEGIN
		WIDGET_CONTROL, state.frame_rate_text_id, GET_VALUE = root														;Get the text
		state.frame_rate = FLOAT(root)																							;Save value
		WIDGET_CONTROL, state.frame_rate_text_id, SET_VALUE = STRING(root)											;Set the text	
	END

	;DROPLIST EVENTS *************************************************************************************
	
	state.file_select_id : BEGIN
		IF (ev.index NE state.file_index) THEN BEGIN
			state.file_index = ev.index																							;Store file index
			change_file      = 1
		ENDIF
	END
	
	state.map_variable_id : BEGIN																					;Map variables (REFL, PRECIP)
		IF (ev.index NE state.map_variable_index) THEN BEGIN
			state.map_variable_index = ev.index																					;Store map index
			refresh_graphics_now = 1
			state.redraw_map     = 1
		ENDIF
	END

	state.section_variable_id : state.sect_variable_index = ev.index													;Store section index

	state.map_variable2_id : BEGIN																					;For cross section (REFL, CLOUD, H2O, W, PV)
		IF (ev.index NE state.map_variable_index2) THEN BEGIN
			state.map_variable_index2 = ev.index																				;Store map index
			refresh_graphics_now = 1
			state.redraw_map     = 1
		ENDIF
	END

	state.section_variable2_id : state.sect_variable_index2 = ev.index												;Store section index

;; ALSO HAVE SECOND DROP DOWN FOR CROSS SECTIONS OF TRACE GASES

	state.map_constalt_select_id : BEGIN																			;Set to tropopause (Trace gases at trop)
		state.alt_index = ev.index																									;Altitude index for constant altitude map
		IF (state.map_type EQ 1) THEN BEGIN
			refresh_graphics_now = 1																								;Set draw flags
			state.redraw_map     = 1
		ENDIF
	END

	state.map_echotop_select_id  : BEGIN
		state.echotop_index = ev.index																							;Echo top index for map
		IF (state.map_type EQ 2) THEN BEGIN
			refresh_graphics_now = 1																								;Set draw flags
			state.redraw_map     = 1
		ENDIF
	END
	
	state.first_file_select_id : IF (ev.index NE state.ianimate0) THEN state.ianimate0 = ev.index			;Store file index
	state.last_file_select_id  : IF (ev.index NE state.ianimate1) THEN state.ianimate1 = ev.index			;Store file index


	;RADIO BUTTON EVENTS *********************************************************************************
	
	state.mouse_zoom_id		: IF (ev.select) THEN state.zoom = 1														;Set mouse mode
	state.mouse_section_id	: IF (ev.select) THEN state.zoom = 0

	state.map_columnmax_id  : BEGIN		
		IF (ev.select) THEN BEGIN
			state.map_type       = 0																								;Set map type
			refresh_graphics_now = 1
			state.redraw_map     = 1
		ENDIF
	END
	
	state.map_constalt_id	: BEGIN																				;Make const. alt one level (at tropopause)
		IF (ev.select) THEN BEGIN
			state.map_type       = 1																								;Set map type
			refresh_graphics_now = 1
			state.redraw_map     = 1
		ENDIF
	END
		
	state.map_echotop_id		: BEGIN
		IF (ev.select) THEN BEGIN
			IF (state.map_variable_index NE 0) THEN $
				WIDGET_CONTROL, state.map_variable_id, SET_DROPLIST_SELECT = 0											;Reset map variable droplist

			state.map_variable_index = 0																							;Set map variable to Z_H
			state.map_type           = 2																							;Set map type
			refresh_graphics_now     = 1
			state.redraw_map         = 1
		ENDIF
	END

	state.map_csa_id			: BEGIN
		IF (ev.select) THEN BEGIN
			state.map_type       = 3																								;Set map type
			refresh_graphics_now = 1
			state.redraw_map     = 1
		ENDIF
	END
	
	state.map_echotop_yes_id : BEGIN
		IF (ev.select) THEN BEGIN
			state.zrelative = 1																										;Set keyword for tropopause-relative maps
			IF (state.map_type EQ 2) THEN BEGIN
				refresh_graphics_now = 1
				state.redraw_map     = 1
			ENDIF
		ENDIF
	END

	state.map_echotop_no_id : BEGIN
		IF (ev.select) THEN BEGIN
			state.zrelative = 0
			IF (state.map_type EQ 2) THEN BEGIN
				refresh_graphics_now = 1
				state.redraw_map     = 1
			ENDIF
		ENDIF
	END

;; PROBABLY WON'T NEED ANY DECLUTTERING/FILTERING OPTIONS
	state.clutter_yes_id : BEGIN
		IF (ev.select) THEN BEGIN
			state.clutter = 1																											;Set flag to remove clutter when reading composite
			change_file   = 1																											;Set flag to read in file
		ENDIF
	END

	state.clutter_no_id : BEGIN
		IF (ev.select) THEN BEGIN
			state.clutter = 0
			change_file   = 1
		ENDIF
	END

	state.filter_yes_id : BEGIN
		IF (ev.select) THEN BEGIN
			state.filter  = 1																											;Set keyword for bin-weight filtering
			change_file   = 1																											;Set flag to read in file
		ENDIF
	END

	state.filter_no_id : BEGIN
		IF (ev.select) THEN BEGIN
			state.filter  = 0																											;Set keyword for bin-weight filtering
			change_file   = 1																											;Set flag to read in file
		ENDIF
	END

	state.smooth_yes_id : BEGIN
		IF (ev.select) THEN BEGIN
			state.smooth         = 1																								;Set keyword for Gaussian smooting
			refresh_graphics_now = 1
			state.redraw_map     = 1
		ENDIF
	END

	state.smooth_no_id : BEGIN
		IF (ev.select) THEN BEGIN
			state.smooth         = 0
			refresh_graphics_now = 1
			state.redraw_map     = 1
		ENDIF
	END

	state.neighbor_yes_id : IF (ev.select) THEN state.nearest_neighbor = 1						;Turn on nearest-neighbor cross-section option
	state.neighbor_no_id  : IF (ev.select) THEN state.nearest_neighbor = 0						;Turn off nearest-neighbor cross-section option

	state.radar_loc_yes_id : BEGIN
		IF (ev.select) THEN BEGIN
			state.radars         = 1																			;Set keyword for radar locations
			refresh_graphics_now = 1
			state.redraw_map     = 1
		ENDIF
	END

	state.radar_loc_no_id : BEGIN
		IF (ev.select) THEN BEGIN
			state.radars         = 0																			;Set keyword for radar locations
			refresh_graphics_now = 1
			state.redraw_map     = 1
		ENDIF
	END

	state.range_ring_yes_id : BEGIN
		IF (ev.select) THEN BEGIN
			state.range_rings    = 1																			;Set keyword for range rings
			refresh_graphics_now = 1
			state.redraw_map     = 1
		ENDIF
	END

	state.range_ring_no_id : BEGIN
		IF (ev.select) THEN BEGIN
			state.range_rings    = 0																			;Set keyword for range rings
			refresh_graphics_now = 1
			state.redraw_map     = 1
		ENDIF
	END

	state.county_yes_id : BEGIN
		IF (ev.select) THEN BEGIN
			state.draw_counties = 1																				;Turn on county lines
			IF (STRLEN(state.county_path) GT 0) THEN BEGIN
				refresh_graphics_now = 1																		;Refresh graphics
				state.redraw_map     = 1																		;Redraw map specifically
			ENDIF
		ENDIF
	END

	state.county_no_id  : BEGIN
		IF (ev.select) THEN BEGIN
			state.draw_counties = 0																				;Turn off county lines
			IF (STRLEN(state.county_path) GT 0) THEN BEGIN
				refresh_graphics_now = 1																		;Refresh graphics
				state.redraw_map     = 1																		;Redraw map specifically
			ENDIF
		ENDIF
	END

;; WON'T NEED STORM REPORTS
	state.reports_yes_id : BEGIN
		IF (ev.select) THEN BEGIN
			state.draw_reports = 1																				;Turn on SPC reports
			IF (STRLEN(state.reports_path) GT 0) THEN BEGIN
				*state.reports       = READ_SPC_REPORTS(state.reports_path)							;Read reports file
				refresh_graphics_now = 1																		;Refresh graphics
				state.redraw_map     = 1																		;Redraw map specifically
			ENDIF
		ENDIF
	END

	state.reports_no_id : BEGIN
		IF (ev.select) THEN BEGIN
			state.draw_reports = 0																				;Turn off SPC reports
			*state.reports     = -1																				;Read reports file
			IF (STRLEN(state.reports_path) GT 0) THEN BEGIN
				refresh_graphics_now = 1																		;Refresh graphics
				state.redraw_map     = 1																		;Redraw map specifically
			ENDIF
		ENDIF
	END
	
	state.bgnd_gray_select_id : BEGIN
		IF (ev.select) THEN BEGIN
			state.missing        = state.missing_color[0]
			refresh_graphics_now = 1
			state.redraw_map     = 1
			
			WIDGET_CONTROL, state.bgnd_white_select_id, SET_BUTTON = 0
			WIDGET_CONTROL, state.bgnd_tan_select_id,   SET_BUTTON = 0
			WIDGET_CONTROL, state.bgnd_blue_select_id,  SET_BUTTON = 0
		ENDIF
	END

	state.bgnd_white_select_id : BEGIN
		IF (ev.select) THEN BEGIN
			state.missing        = state.missing_color[1]
			refresh_graphics_now = 1
			state.redraw_map     = 1
			
			WIDGET_CONTROL, state.bgnd_gray_select_id, SET_BUTTON = 0
			WIDGET_CONTROL, state.bgnd_tan_select_id,  SET_BUTTON = 0
			WIDGET_CONTROL, state.bgnd_blue_select_id, SET_BUTTON = 0
		ENDIF
	END

	state.bgnd_tan_select_id : BEGIN
		IF (ev.select) THEN BEGIN
			state.missing        = state.missing_color[2]
			refresh_graphics_now = 1
			state.redraw_map     = 1
			
			WIDGET_CONTROL, state.bgnd_gray_select_id,  SET_BUTTON = 0
			WIDGET_CONTROL, state.bgnd_white_select_id, SET_BUTTON = 0
			WIDGET_CONTROL, state.bgnd_blue_select_id,  SET_BUTTON = 0
		ENDIF
	END

	state.bgnd_blue_select_id : BEGIN
		IF (ev.select) THEN BEGIN
			state.missing        = state.missing_color[3]
			refresh_graphics_now = 1
			state.redraw_map     = 1
			
			WIDGET_CONTROL, state.bgnd_gray_select_id,  SET_BUTTON = 0
			WIDGET_CONTROL, state.bgnd_white_select_id, SET_BUTTON = 0
			WIDGET_CONTROL, state.bgnd_tan_select_id,   SET_BUTTON = 0
		ENDIF
	END

	state.color_table1_id : BEGIN
		IF (ev.select) THEN BEGIN
			state.icolor         = 0
			refresh_graphics_now = 1
			state.redraw_map     = 1
		ENDIF
	END
	
	state.color_table2_id : BEGIN
		IF (ev.select) THEN BEGIN
			state.icolor         = 1
			refresh_graphics_now = 1
			state.redraw_map     = 1
		ENDIF
	END
	
	state.color_table3_id : BEGIN
		IF (ev.select) THEN BEGIN
			state.icolor         = 2
			refresh_graphics_now = 1
			state.redraw_map     = 1
		ENDIF
	END
	
	state.color_table4_id : BEGIN
		IF (ev.select) THEN BEGIN
			state.icolor         = 3
			refresh_graphics_now = 1
			state.redraw_map     = 1
		ENDIF
	END

	state.color_table5_id : BEGIN
		IF (ev.select) THEN BEGIN
			state.icolor         = 4
			refresh_graphics_now = 1
			state.redraw_map     = 1
		ENDIF
	END	
			
	;MAIN ACTION BUTTON EVENTS ***************************************************************************

	state.indir_browse_id : BEGIN
		indir = DIALOG_PICKFILE(TITLE = 'Choose WRF Directory', PATH = state.indir, $				;Request input file path
								 DIALOG_PARENT = state.base_id, /DIRECTORY)
		IF (STRLEN(indir) GT 0) THEN BEGIN
			state.indir = indir
			state.indir_altered  = 1																								;Set directory altered flag
			WIDGET_CONTROL, state.indir_text_id, SET_VALUE = indir														;Set the text
		ENDIF
	END
		
	state.draw_id : BEGIN
		IF (ev.type EQ 2) THEN BEGIN
			xymotion      = CONVERT_COORD(ev.x, ev.y, /DEVICE, /TO_DATA)												;Calculate mouse position
			state.xmotion = xymotion[0]																							;Store xmotion coord
			state.ymotion = xymotion[1]																							;Store ymotion coord
			WIDGET_CONTROL, state.x_motion_text_id, SET_VALUE = STRING(state.xmotion, FORMAT="(F10.2)")		;Set xmotion textbox value
			WIDGET_CONTROL, state.y_motion_text_id, SET_VALUE = STRING(state.ymotion, FORMAT="(F10.2)")		;Set ymotion textbox value
			IF (SIZE((*state.map), /TNAME) EQ 'STRUCT') THEN $
				WIDGET_CONTROL, state.map_motion_text_id, SET_VALUE = $													;Set map value textbox value
					STRING(INTERPOLATE((*state.map).values,INTERPOL(FINDGEN((*state.map).nx),$
					(*state.map).x,(state.xmotion+360.0) MOD 360.0),INTERPOL(FINDGEN((*state.map).ny),$
					(*state.map).y,state.ymotion)), FORMAT="(F10.2)")
			IF ((state.first_point_chosen) AND (~state.second_point_chosen)) THEN BEGIN
				refresh_graphics_now = 1																							;Refresh graphics
			ENDIF
		ENDIF

		IF ((ev.type EQ 1) AND (state.first_point_chosen)) THEN BEGIN													;Check for first point
			IF (ev.release) THEN BEGIN																								;Check for button release
				x2y2click = CONVERT_COORD(ev.x, ev.y, /DEVICE, /TO_DATA)													;Calculate second point
				state.x2  = x2y2click[0]																							;Store x2
				state.y2  = x2y2click[1]																							;Store y2
				WIDGET_CONTROL, state.x2_text_id, SET_VALUE = STRING(state.x2, $										;Set x2 textbox value
					FORMAT="(F10.2)")	
				WIDGET_CONTROL, state.y2_text_id, SET_VALUE = STRING(state.y2, $										;Set y2 textbox value
					FORMAT="(F10.2)")
				state.second_point_chosen  = 1																					;Second point chosen
				refresh_graphics_now       = 1																					;Refresh graphics
			ENDIF
		ENDIF

		IF ((ev.type EQ 0) AND (~state.first_point_chosen)) THEN BEGIN													;Check for first point
			IF (ev.press) THEN BEGIN																								;Check for button press
				x1y1click = CONVERT_COORD(ev.x, ev.y, /DEVICE, /TO_DATA)													;Calculate first point
				state.x1  = x1y1click[0]																							;Store x1
				state.y1  = x1y1click[1]																							;Store y1
				WIDGET_CONTROL, state.x1_text_id, SET_VALUE = STRING(state.x1, $										;Set x1 textbox value
					FORMAT="(F10.2)")
				WIDGET_CONTROL, state.y1_text_id, SET_VALUE = STRING(state.y1, $										;Set y1 textbox value
					FORMAT="(F10.2)")
	
				state.x2 = !VALUES.F_NAN																							;Clear x2 value
				state.y2 = !VALUES.F_NAN																							;Clear y2 value
				WIDGET_CONTROL, state.x2_text_id, SET_VALUE = '	'															;Clear x2 textbox
				WIDGET_CONTROL, state.y2_text_id, SET_VALUE = '	'															;Clear y2 textbox
				state.first_point_chosen  = 1																						;First point chosen
				state.second_point_chosen = 0																						;Second point chosen
				refresh_graphics_now      = 1																						;Refresh graphics
			ENDIF
		ENDIF
	END

	state.reset_zoom_id : BEGIN
		state.map_limit      = state.file_domain																				;Reset map limit to original domain
		refresh_graphics_now = 1																									;Refresh graphics
		state.redraw_map     = 1																									;Redraw map specifically
	END
	
	state.refresh_map_id : BEGIN
		state.x1 	  = !VALUES.F_NAN																								;Rid x1 location
		state.x2 	  = !VALUES.F_NAN																								;Rid x2 location
		state.y1 	  = !VALUES.F_NAN																								;Rid y1 location
		state.y2 	  = !VALUES.F_NAN																								;Rid y2 location
		state.xbox[*] = !VALUES.F_NAN																								;Rid xybox coordinates
		state.ybox[*] = !VALUES.F_NAN
		state.arc[*]  = !VALUES.F_NAN																								;Rid old arc
		WIDGET_CONTROL, state.x1_text_id, SET_VALUE = '	'																	;Clear x1 text window
		WIDGET_CONTROL, state.y1_text_id, SET_VALUE = '	'																	;Clear y1 text window
		WIDGET_CONTROL, state.x2_text_id, SET_VALUE = '	'																	;Clear x2 text window
		WIDGET_CONTROL, state.y2_text_id, SET_VALUE = '	'																	;Clear y2 text window
		state.first_point_chosen  = 0																								;First point not chosen
		state.second_point_chosen = 0																								;Second point not chosen
		refresh_graphics_now      = 1																								;Refresh graphics
		state.redraw_map          = 1																								;Redraw map specifically
	END
		
	state.plot_section_id : BEGIN		
		IF ((~FINITE(state.x1) AND ~FINITE(state.y1)) AND $																;Check for both points not chosen
			(~FINITE(state.x2) AND ~FINITE(state.y2))) THEN BEGIN
			
			rc = DIALOG_MESSAGE(['     Section Not Chosen!     '], $
										TITLE = 'Mapping Error', $
										DIALOG_PARENT = state.draw_id)
		ENDIF
		
		IF ((FINITE(state.x1) AND FINITE(state.y1)) AND $																	;Check for both points chosen
			 (FINITE(state.x2) AND FINITE(state.y2))) THEN BEGIN

			IF ((state.x1 EQ state.x2) AND (state.y1 EQ state.y2)) THEN $
				rc = DIALOG_MESSAGE(['Start and end points of section must be different.'], $
											TITLE = 'Mapping Error', $
											DIALOG_PARENT = state.draw_id) $
			ELSE BEGIN		
				IF (state.section_window NE 0) THEN BEGIN
					DEVICE, WINDOW_STATE = window_state																			;Get state of window indices
					IF (N_ELEMENTS(window_state) GT state.section_window) THEN $
						IF (window_state[state.section_window]) THEN WDELETE, state.section_window					;Delete previous section window
				ENDIF
		
				VISUALIZE_WRF_SECTION, *state.data, (state.variable_list[state.var_list_index])$					;Draw section
					[state.sect_variable_index], (state.variable_list2[state.var_list_index])$
					[state.sect_variable_index2], state.x1, state.y1, state.x2, state.y2, $
					MAP_TYPE = state.map_type, TROPOPAUSE = state.tropopause, $
					ZMELT = state.zmelt, SMOOTH = state.smooth, ZRANGE  = [state.z1,state.z2], $
					COLOR_INDEX = state.icolor, MISSING = state.missing, BAR_MIN = state.bar_min, $
					BAR_MAX = state.bar_max, BAR_TICKS = state.bar_ticks, $
					NEAREST_NEIGHBOR = state.nearest_neighbor

				state.section_window = !D.WINDOW																					;Store window id of section plot
				WSET, state.draw_window																								;Reset window to the draw widget

				refresh_graphics_now = 1																							;Set draw flags
				state.redraw_map     = 1
			ENDELSE
		ENDIF
	END

	state.map_png_button_id     : save_graphics_now = 1																	;Set save graphics flag
	state.map_eps_button_id     : save_graphics_now = 1																	;Set save graphics flag
	state.map_nc_button_id      : save_graphics_now = 1																	;Set save graphics flag
	state.section_png_button_id : save_graphics_now = 1																	;Set save graphics flag
	state.section_eps_button_id : save_graphics_now = 1																	;Set save graphics flag
	state.section_nc_button_id  : save_graphics_now = 1																	;Set save graphics flag

	state.help_id : BEGIN																											;Help text
		help = ['The input directory for NEXRAD composite files can be changed via typing', $
				  'or browsing. If typing, you must hit <cr> to set changes. Upon update,', $
				  'the file list should populate in the scroll window. Click on a single', $
				  'file name to load that data. The up and down arrows can be used to step', $
				  'through frames. Movies can be generated for chosen map or section settings', $
				  'via the ANIMATION tab on the left hand side of the window. Note: .MOV output', $
				  'only possible with IDL license, .GIF output is slow.', $
				  ' ', $
				  'The mouse can be used to perform two important operations: 1. zooming of', $
				  'the map domain and 2. selection of cross-section paths. To set the mouse', $
				  'drawing mode, choose the appropriate radio button under the MAIN tab.', $
				  ' ', $
				  'To change the end points of a vertical section, press the mouse button to', $
				  'select the inital point, then drag the mouse to the desired end point ', $
				  'and release. The values in the "Point 1" and "Point 2" text boxes on the', $
				  'MAIN tab indicate the values most recently chosen for the desired section,', $
				  'which can also be manually updated. When finished choosing endpoints, click', $
				  'the "Plot Section" button under the MAIN tab to generate the vertical ', $
				  'section.', $
				  ' ', $
				  'The default variable for visualization is the radar reflectivity factor at', $
				  'horizontal polarization (Z_H). Alternatives (if available) can be selected', $
				  'for maps and cross-sections via the drop-down lists under the MAIN tab. A ', $
				  'second radar variable can be superimposed as contours (with levels defined', $
				  'using the color table settings (i.e., ranges and tick marks) for each.', $
				  'Additional plotting settings such as decluttering, filtering by composite', $
				  'bin weights, or Gaussian smoothing are available.', $
				  ' ', $
				  'In addition to variable type, there is functionality for choosing different', $
				  'map types: column-maximum, constant altitude (CAPPI), echo top (Z_H only), ', $
				  'and classifications from the Storm Labeling in 3-D (SL3D) algorithm. To ', $
				  'change the map type, select the desired radio button under the MAIN tab.', $
				  ' ', $
				  'Plotting properties including the altitude axis range for vertical sections,', $
				  'tropopause altitude, melting level altitude (used for SL3D), and multiple', $
				  'plotting add-on options can be set via the CUSTOMIZATION tab.', $
				  ' ', $
				  'Background color and radar variable color tables and value ranges can be ', $
				  'modified via the COLOR tab.', $
				  ' ', $
				  'To save either the reference map or the vertical section, click the button', $
				  'of the desired output format on the MAIN tab: PNG image, EPS (Encapsulated', $
				  'PostScript), or NC (gridded netCDF).']
		rc = DIALOG_MESSAGE(help, /INFORMATION, TITLE = 'Using the NEXRAD WSR-88D 3-D Composite Viewer', $
								  DIALOG_PARENT = state.action_id)
	END

	state.quit_id : BEGIN
		quit = DIALOG_MESSAGE(['Are you sure you want to quit?'], /DEFAULT_NO, /QUESTION, $
					DIALOG_PARENT = state.action_id, TITLE='Quitting')
		IF (quit EQ 'Yes') THEN BEGIN
			PTR_FREE, state.data, state.map, state.tracks, state.reports													;Free pointer variable data from memory
			IF (state.section_window NE 0) THEN BEGIN
				DEVICE, WINDOW_STATE = window_state
				IF (window_state[state.section_window]) THEN WDELETE, state.section_window								;Delete previous section window
			ENDIF
			WIDGET_CONTROL, /DESTROY, ev.top																							;Destroy widgets
			PRINT, "NEXRAD WSR-88D 3-D Composite Viewer Exited."
			RETURN
		ENDIF
	END

	state.county_path_browse_id : BEGIN
		county_path = DIALOG_PICKFILE(TITLE = 'Choose US County Shapefile', $											;Request input file path
								 DIALOG_PARENT = state.base_id)
		
		IF (STRLEN(county_path) GT 3) THEN BEGIN
			IF (STRLOWCASE(STRMID(county_path,2,/REVERSE)) EQ 'shp') THEN BEGIN
				state.county_path = county_path
				WIDGET_CONTROL, state.county_path_text_id, SET_VALUE = county_path											;Set the text
				IF (state.draw_counties) THEN BEGIN
					refresh_graphics_now = 1																									;Refresh graphics
					state.redraw_map     = 1																									;Redraw map specifically
				ENDIF
			ENDIF
		ENDIF
	END

;; WON'T NEED STORM REPORTS
	state.reports_path_browse_id : BEGIN
		reports_path = DIALOG_PICKFILE(TITLE = 'Choose SPC Storm Reports CSV File', $									;Request input file path
								 DIALOG_PARENT = state.base_id)
		
		IF (STRLEN(reports_path) GT 3) THEN BEGIN
			IF (STRLOWCASE(STRMID(reports_path,2,/REVERSE)) EQ 'csv') THEN BEGIN
				state.reports_path = reports_path
				WIDGET_CONTROL, state.reports_path_text_id, SET_VALUE = reports_path										;Set the text
				IF (state.draw_reports) THEN BEGIN
					*state.reports     = READ_SPC_REPORTS(reports_path)														;Read reports file
					refresh_graphics_now = 1																							;Refresh graphics
					state.redraw_map     = 1																							;Redraw map specifically
				ENDIF
			ENDIF
		ENDIF
	END

	state.cw_animate_id : BEGIN
		nimage = state.ianimate1 - state.ianimate0 + 1																			;Calculate number of images for animation
		
		IF (nimage GT 1) THEN BEGIN
			IF (N_ELEMENTS(state.animation_images) GT 0) THEN $
				state.animation_images.Remove, INDGEN(N_ELEMENTS(state.animation_images))								;Remove any existing images
		
			state.cw_animate_widget_id = WIDGET_BASE(TITLE = 'VISUALIZE_WRF Animation')								;Create base widget
			
			IF (TOTAL(FINITE(state.arc)) GT 0) THEN $
				animate = CW_ANIMATE(state.cw_animate_widget_id, state.sect_xsize, state.sect_ysize, nimage) ELSE $	;Turn widget into animator
				animate = CW_ANIMATE(state.cw_animate_widget_id, state.xsize, state.ysize, nimage)					;Turn widget into animator
			WIDGET_CONTROL, /REALIZE, state.cw_animate_widget_id
			FOR ifile = state.ianimate0, state.ianimate1 DO BEGIN
				id   = NCDF_OPEN(state.indir + ((state.file_list)[0])[ifile])												;Open file for reading
				inq  = NCDF_INQUIRE(id)																									;Inquire about netCDF file
				FOR i = 0, inq.nvars -1 DO $
					IF (i EQ 0) THEN vnames = (NCDF_VARINQ(id, i)).name $														;Poplulate netcdf variable names
									ELSE vnames = [vnames, (NCDF_VARINQ(id, i)).name]
				NCDF_CLOSE, id
				iv2 = WHERE((vnames EQ 'nbeams_contributing'),v2test)															;Search for variables unique to compositing method
				iv3 = WHERE((vnames EQ 'files_merged'       ),v3test)

				IF (v3test) THEN $
					data = NEXRAD_READ_LEVEL2_COMPOSITE(state.indir + ((state.file_list)[0])[ifile]) $				;Load NEXRAD data
				ELSE IF (v2test) THEN $
					data = NEXRAD_READ_LEVEL2_2(state.indir + ((state.file_list)[0])[ifile]) $							;Load NEXRAD data
				ELSE $
					data = NEXRAD_READ_LEVEL2(INFILE = state.indir + ((state.file_list)[0])[ifile])					;Load NEXRAD data

				data = NEXRAD_FILL_HOLES(data)																						;Fill echo holes
				IF (state.clutter) THEN data = NEXRAD_REMOVE_CLUTTER(data)													;Remove clutter
				IF (state.filter ) THEN data = NEXRAD_FILTER(data)																;Apply bin-weight filtering

				IF (TOTAL(FINITE(state.arc)) GT 0) THEN $
					VISUALIZE_WRF_SECTION, data, (state.variable_list[state.var_list_index])$							;Draw section
						[state.sect_variable_index], (state.variable_list2[state.var_list_index])$
						[state.sect_variable_index2], state.x1, state.y1, state.x2, state.y2, $
						MAP_TYPE = state.map_type, TROPOPAUSE = state.tropopause, $
						ZMELT = state.zmelt, SMOOTH = state.smooth, ZRANGE = [state.z1, state.z2], $
						IMAGE = image, COLOR_INDEX = state.icolor, MISSING = state.missing, $
						BAR_MIN = state.bar_min, BAR_MAX = state.bar_max, BAR_TICKS = state.bar_ticks, $
						NEAREST_NEIGHBOR = state.nearest_neighbor, /PNG $
				ELSE $
				VISUALIZE_WRF_MAP, data, (state.variable_list[state.var_list_index])[state.map_variable_index], $	;Draw map
						(state.variable_list2[state.var_list_index])[state.map_variable_index2], LIMIT = state.map_limit, $
						ARC = state.arc, SMOOTH = state.smooth, MAP_TYPE = state.map_type, ALTITUDE = state.alt_index, $
						DBZ_THRESH = FLOAT((state.echotop_list)[state.echotop_index]), TRACKS = *state.tracks, $
						ZRELATIVE = state.zrelative, TROPOPAUSE = state.tropopause, $
						ZMELT = state.zmelt, IMAGE = image, COLOR_INDEX = state.icolor, MISSING = state.missing, $
						BAR_MIN = state.bar_min, BAR_MAX = state.bar_max, BAR_TICKS = state.bar_ticks, $
						COUNTIES = state.draw_counties, COUNTY_FILE = state.county_path, $
						RANGE_RINGS = state.range_rings, RANGE_INTERVAL = state.range_interval, $
						RADARS = state.radars, REPORTS = *state.reports, /PNG

				CW_ANIMATE_LOAD, animate, FRAME= ifile - state.ianimate0, IMAGE=image									;Load image into CW_ANIMATE
				state.animation_images.Add, image																					;Add current image to list
			ENDFOR

			WIDGET_CONTROL, ev.top, SET_UVALUE = state																			;Save state structure

			WSET, state.draw_window																										;Reset draw window

			CW_ANIMATE_RUN, animate, LONG(2*state.frame_rate)																	;Start animation
			WIDGET_CONTROL, state.cw_animate_widget_id, SET_UVALUE = state													;Store state structure in base widget uvalue
			XMANAGER, 'VISUALIZE_WRF Animation', state.cw_animate_widget_id, $
				EVENT_HANDLER = 'VISUALIZE_WRF_EVENT'	
		ENDIF
	END

	state.save_animation_mov_id : BEGIN
		nimages = N_ELEMENTS(state.animation_images)																				;Count number of animation images available

		IF (nimages GT 0) THEN BEGIN
			movie_file = $																													;Request output file name
				DIALOG_PICKFILE(TITLE = 'Enter output file name', PATH = state.outdir, $
									 GET_PATH = outdir, DIALOG_PARENT = state.base_id)
			IF (STRLEN(movie_file) GT 0) THEN BEGIN
				state.outdir = outdir																									;Store path for next save
				WIDGET_CONTROL, ev.top, SET_UVALUE = state																		;Save state structure

				video  = IDLffVideoWrite(movie_file)																				;Create object for writing movie
				IF (TOTAL(FINITE(state.arc)) GT 0) THEN $
					stream = video.AddVideoStream(state.sect_xsize, state.sect_ysize,$									;Initialize video stream
									state.frame_rate,bit_rate=10000*8192,Codec='mpeg4') ELSE $
					stream = video.AddVideoStream(state.xsize,state.ysize,$													;Initialize video stream
									state.frame_rate,bit_rate=10000*8192,Codec='mpeg4')
				
				FOR i = 0, nimages -1 DO time = video.Put(stream, (state.animation_images)[i])						;Add each image to video stream
				
				video.Cleanup																												;Close movie and destroy object

				rc = DIALOG_MESSAGE(['QuickTime movie saved!'], $																;If file name empty, message error
								TITLE = 'Save Success', DIALOG_PARENT = state.draw_id)
			ENDIF ELSE $
				rc = DIALOG_MESSAGE(['Cannot save movie: no filename given!'], $											;If file name empty, message error
								TITLE = 'Save Error', DIALOG_PARENT = state.draw_id)
		ENDIF ELSE $
			rc = DIALOG_MESSAGE(['Animation must be created in order to save.', $										;If no images loaded, message error
							'Press ANIMATE button first.'], $
							TITLE = 'Save Error', DIALOG_PARENT = state.draw_id)
	END

	state.save_animation_gif_id : BEGIN
		nimages = N_ELEMENTS(state.animation_images)																				;Count number of animation images available

		IF (nimages GT 0) THEN BEGIN
			movie_file = $																													;Request output file name
				DIALOG_PICKFILE(TITLE = 'Enter output file name', PATH = state.outdir, $
									 GET_PATH = outdir, DIALOG_PARENT = state.base_id)
			IF (STRLEN(movie_file) GT 0) THEN BEGIN
				state.outdir = outdir																									;Store path for next save
				WIDGET_CONTROL, ev.top, SET_UVALUE = state																		;Save state structure

				FOR i = 0, nimages - 1 DO BEGIN
					image0 = state.animation_images[i]																				;Extract image
					color  = COLOR_24(image0[0,*,*],image0[1,*,*],image0[2,*,*])											;Compute 24-bit colors of image
					IF (i EQ 0) THEN BEGIN
						isort  = SORT(color)																								;Sort color values
						iuniq  = UNIQ(color[isort])																					;Find unique colors
					
						table  = (color[isort])[iuniq]																				;Extract unique values (color palette)
						rgb    = COMPONENT_24(table)																					;Get red green blue components
					ENDIF
					
					image  = INDEX_OF_NEAREST_CRH(table, color)																	;Get color indices of image
					
					IF (TOTAL(FINITE(state.arc)) GT 0) THEN $
						image  = REFORM(BYTE(image),state.sect_xsize,state.sect_ysize) ELSE $							;Reform image and convert to BYTE index
						image  = REFORM(BYTE(image),state.xsize,state.ysize)													;Reform image and convert to BYTE index
					
					FOR j = 0, LONG(6/state.frame_rate) DO $
						WRITE_GIF, movie_file, image, rgb[*,0], rgb[*,1], rgb[*,2], /MULTIPLE							;Write image to animate GIF
				ENDFOR
				WRITE_GIF, movie_file, /CLOSE
				
				rc = DIALOG_MESSAGE(['Animated GIF saved!'], $																	;If file name empty, message error
								TITLE = 'Save Success', DIALOG_PARENT = state.draw_id)
			ENDIF ELSE $
				rc = DIALOG_MESSAGE(['Cannot save movie: no filename given!'], $											;If file name empty, message error
								TITLE = 'Save Error', DIALOG_PARENT = state.draw_id)
		ENDIF ELSE $
			rc = DIALOG_MESSAGE(['Animation must be created in order to save.', $										;If no images loaded, message error
							'Press ANIMATE button first.'], $
							TITLE = 'Save Error', DIALOG_PARENT = state.draw_id)
	END

	;UNKNOWN EVENTS **************************************************************************************

	ELSE: PRINT, 'Unknown event : ', ev

ENDCASE

;; HEAVY EDITING HERE

WIDGET_CONTROL, ev.top, SET_UVALUE = state																						;Save state structure

IF (state.indir_altered) THEN BEGIN
	file_list = FILE_SEARCH(state.indir + '*.nc')																				;Search for netCDF files
	IF (N_ELEMENTS(state.file_list) GT 0) THEN BEGIN
		state.file_list.Remove, 0																										;If previous file list, remove
		IF (N_ELEMENTS(state.animation_images) GT 0) THEN $
				state.animation_images.Remove, INDGEN(N_ELEMENTS(state.animation_images))								;Remove any existing images from animation
	ENDIF

	IF (STRLEN(file_list[0]) GT 0) THEN BEGIN
		file_list = STRMID(file_list, STRLEN(state.indir))																		;Remove input directory from file list
		state.file_list.Add, file_list																								;Add current file list
		WIDGET_CONTROL, state.file_select_id,       SET_VALUE = file_list
		WIDGET_CONTROL, state.first_file_select_id, SET_VALUE = file_list
		WIDGET_CONTROL, state.last_file_select_id,  SET_VALUE = file_list
		state.ianimate0 = 0
		state.ianimate1 = 0

		id   = NCDF_OPEN(state.indir + file_list[0])																				;Open file for reading
		inq  = NCDF_INQUIRE(id)																											;Inquire about netCDF file
		FOR i = 0, inq.nvars -1 DO $
			IF (i EQ 0) THEN vnames = (NCDF_VARINQ(id, i)).name $																;Poplulate netcdf variable names
							ELSE vnames = [vnames, (NCDF_VARINQ(id, i)).name]
		NCDF_CLOSE, id
		iv2 = WHERE((vnames EQ 'nbeams_contributing'),v2test)																	;Search for variables unique to compositing method
		iv3 = WHERE((vnames EQ 'files_merged'       ),v3test)

		IF (v3test) THEN BEGIN
			*state.data = NEXRAD_READ_LEVEL2_COMPOSITE(state.indir + file_list[0])										;Load NEXRAD data
			IF (SIZE((*state.data).zdr, /TNAME) EQ 'STRUCT') THEN state.var_list_index = 2 $							;Check for polarimetric variables
																			 ELSE state.var_list_index = 0
		ENDIF ELSE IF (v2test) THEN BEGIN
			*state.data = NEXRAD_READ_LEVEL2_2(state.indir + file_list[0])													;Load NEXRAD data
			IF (SIZE((*state.data).zdr, /TNAME) EQ 'STRUCT') THEN state.var_list_index = 1 $							;Check for polarimetric variables
																			 ELSE state.var_list_index = 0
		ENDIF ELSE BEGIN
			*state.data = NEXRAD_READ_LEVEL2(INFILE = state.indir + file_list[0])										;Load NEXRAD data
			IF (SIZE((*state.data).zdr, /TNAME) EQ 'STRUCT') THEN state.var_list_index = 2 $							;Check for polarimetric variables
																			 ELSE state.var_list_index = 0
		ENDELSE

		*state.data = NEXRAD_FILL_HOLES(*state.data)																				;Fill echo holes
		IF (state.clutter) THEN *state.data = NEXRAD_REMOVE_CLUTTER(*state.data)										;Remove clutter
		IF (state.filter ) THEN *state.data = NEXRAD_FILTER(*state.data)													;Apply bin-weight filtering

		IF FILE_TEST(state.indir + 'tracks.txt') THEN $
			*state.tracks = NEXRAD_READ_TRACKS(state.indir + 'tracks.txt') ELSE $
			*state.tracks = -1

		WIDGET_CONTROL, state.map_variable_id,      SET_VALUE = (state.variable_list )[state.var_list_index]		;Set variable lists
		WIDGET_CONTROL, state.section_variable_id,  SET_VALUE = (state.variable_list )[state.var_list_index]
		WIDGET_CONTROL, state.map_variable2_id,     SET_VALUE = (state.variable_list2)[state.var_list_index]		;Set variable lists
		WIDGET_CONTROL, state.section_variable2_id, SET_VALUE = (state.variable_list2)[state.var_list_index]

		WIDGET_CONTROL, state.map_constalt_select_id, SET_VALUE = $															;Set altitude array for constant altitude maps
			STRING((*state.data).z.values,FORMAT="(F4.1)")
		state.alt_index = 0																												;Reset constant altitude index
		
		limit             = [(*state.data).y.values[ 0],(*state.data).x.values[ 0], $									;Set domain & map limit
									(*state.data).y.values[-1],(*state.data).x.values[-1]]
		state.map_limit   = limit
		state.file_domain = limit
		
	ENDIF ELSE BEGIN
		state.file_list.Add, '                        '																			;Add blank list
		WIDGET_CONTROL, state.file_select_id,       SET_VALUE = (state.file_list)[0]									;Erase file list
		WIDGET_CONTROL, state.first_file_select_id, SET_VALUE = (state.file_list)[0]
		WIDGET_CONTROL, state.last_file_select_id,  SET_VALUE = (state.file_list)[0]
		state.ianimate0 = 0
		state.ianimate1 = 0
		*state.data = -1																													;Remove data
	ENDELSE

	state.map_variable_index   = 0																									;Reset plotting specs
	state.sect_variable_index  = 0
	state.map_variable_index2  = 0
	state.sect_variable_index2 = 0
	state.map_type             = 0
	WIDGET_CONTROL, state.map_columnmax_id,  /SET_BUTTON																		;Set the default map type button for display
	
	state.x1 	  = !VALUES.F_NAN																										;Rid x1 location
	state.x2 	  = !VALUES.F_NAN																										;Rid x2 location
	state.y1 	  = !VALUES.F_NAN																										;Rid y1 location
	state.y2 	  = !VALUES.F_NAN																										;Rid y2 location
	state.xbox[*] = !VALUES.F_NAN																										;Rid xybox coordinates
	state.ybox[*] = !VALUES.F_NAN
	state.arc[*]  = !VALUES.F_NAN																										;Rid old arc
	WIDGET_CONTROL, state.x1_text_id, SET_VALUE = '	'																			;Clear x1 text window
	WIDGET_CONTROL, state.y1_text_id, SET_VALUE = '	'																			;Clear y1 text window
	WIDGET_CONTROL, state.x2_text_id, SET_VALUE = '	'																			;Clear x2 text window
	WIDGET_CONTROL, state.y2_text_id, SET_VALUE = '	'																			;Clear y2 text window
	state.first_point_chosen  = 0																										;First point not chosen
	state.second_point_chosen = 0																										;Second point not chosen

	state.file_index     = 0																											;Reset file index
	state.indir_altered  = 0																											;Reset flag
	refresh_graphics_now = 1																											;Refresh graphics
	state.redraw_map     = 1																											;Redraw map specifically
ENDIF

IF (change_file) THEN BEGIN
	IF (N_ELEMENTS((state.file_list)[0]) GE 1) THEN BEGIN
		id   = NCDF_OPEN(state.indir + ((state.file_list)[0])[state.file_index])										;Open file for reading
		inq  = NCDF_INQUIRE(id)																											;Inquire about netCDF file
		FOR i = 0, inq.nvars -1 DO $
			IF (i EQ 0) THEN vnames = (NCDF_VARINQ(id, i)).name $																;Poplulate netcdf variable names
							ELSE vnames = [vnames, (NCDF_VARINQ(id, i)).name]
		NCDF_CLOSE, id
		iv2 = WHERE((vnames EQ 'nbeams_contributing'),v2test)																	;Search for variables unique to compositing method
		iv3 = WHERE((vnames EQ 'files_merged'       ),v3test)

		IF (v3test) THEN BEGIN
			*state.data = NEXRAD_READ_LEVEL2_COMPOSITE(state.indir + ((state.file_list)[0])[state.file_index])	;Load NEXRAD data
			IF (SIZE((*state.data).zdr, /TNAME) EQ 'STRUCT') THEN var_list_index = 2 $									;Check for polarimetric variables
																			 ELSE var_list_index = 0
		ENDIF ELSE IF (v2test) THEN BEGIN
			*state.data = NEXRAD_READ_LEVEL2_2(state.indir + ((state.file_list)[0])[state.file_index])			;Load NEXRAD data
			IF (SIZE((*state.data).zdr, /TNAME) EQ 'STRUCT') THEN var_list_index = 1 $									;Check for polarimetric variables
																			 ELSE var_list_index = 0
		ENDIF ELSE BEGIN
			*state.data = NEXRAD_READ_LEVEL2(INFILE = state.indir + ((state.file_list)[0])[state.file_index])	;Load NEXRAD data
			IF (SIZE((*state.data).zdr, /TNAME) EQ 'STRUCT') THEN var_list_index = 2 $									;Check for polarimetric variables
																			 ELSE var_list_index = 0
		ENDELSE

		*state.data = NEXRAD_FILL_HOLES(*state.data)																				;Fill echo holes
		IF (state.clutter) THEN *state.data = NEXRAD_REMOVE_CLUTTER(*state.data)										;Remove clutter
		IF (state.filter ) THEN *state.data = NEXRAD_FILTER(*state.data)													;Apply bin-weight filtering

		IF (var_list_index NE state.var_list_index) THEN BEGIN
			state.var_list_index = var_list_index																					;Update variable list
			WIDGET_CONTROL, state.map_variable_id,     SET_VALUE = (state.variable_list)[state.var_list_index]	;Set variable lists
			WIDGET_CONTROL, state.section_variable_id, SET_VALUE = (state.variable_list)[state.var_list_index]
		ENDIF

		WIDGET_CONTROL, state.map_constalt_select_id, GET_VALUE = zold
		IF (N_ELEMENTS(FLOAT(zold)) NE (*state.data).z.n) THEN refresh_alts = 1 $										;Check conditions for updating altitude list
		ELSE IF (TOTAL(FLOAT(zold) - (*state.data).z.values) NE 0.0) THEN refresh_alts = 1
						
		IF (refresh_alts) THEN BEGIN
			WIDGET_CONTROL, state.map_constalt_select_id, SET_VALUE = $														;Set altitude array for constant altitude maps
				STRING((*state.data).z.values,FORMAT="(F4.1)")
			state.alt_index = 0																											;Reset constant altitude index
		ENDIF

		limit = [(*state.data).y.values[ 0],(*state.data).x.values[ 0], $													;Set domain & map limit
					(*state.data).y.values[-1],(*state.data).x.values[-1]]

		IF (TOTAL(state.file_domain - limit) NE 0) THEN BEGIN
			state.map_limit   = limit																									;If different from last, then update
			state.file_domain = limit
		ENDIF

		refresh_graphics_now = 1																										;Set draw flags
		state.redraw_map     = 1
	ENDIF
ENDIF

;; END EDITS

IF (refresh_graphics_now) THEN BEGIN		
	IF (state.cw_animate_widget_id GE 0) THEN BEGIN
		IF WIDGET_INFO(state.cw_animate_widget_id,/VALID) THEN $
			WIDGET_CONTROL, state.cw_animate_widget_id, /DESTROY $															;Destroy animation widget
		ELSE $
			state.cw_animate_widget_id = -1
	ENDIF

	IF (state.zoom) THEN BEGIN
		IF ((state.first_point_chosen) AND (~state.second_point_chosen)) THEN BEGIN
			IF (state.overwrite_set EQ 0) THEN BEGIN
				DEVICE, GET_GRAPHICS_FUNCTION = gfunct, SET_GRAPHICS_FUNCTION = 6											;Allow "rubber banding"
				state.graphics_funct = gfunct																							;Save old graphics function for conversion
				state.overwrite_set  = 1																								;Allow overwrite for motion case
				PLOTS, [state.x1, state.x1,      state.xmotion, state.xmotion, state.x1, state.x1     ], $		;Plot initial box
						 [state.y1, state.ymotion, state.ymotion, state.y1,      state.y1, state.ymotion], $
						 THICK = 2, COLOR = COLOR_24('white')
				state.xbox = [state.x1, state.x1,      state.xmotion, state.xmotion, state.x1, state.x1     ]	;Store box arrays
				state.ybox = [state.y1, state.ymotion, state.ymotion, state.y1,      state.y1, state.ymotion]
			ENDIF	
			PLOTS, state.xbox, state.ybox, THICK=2, COLOR = COLOR_24('white')												;Erase previous box
	
			PLOTS, [state.x1, state.x1,      state.xmotion, state.xmotion, state.x1, state.x1     ], $			;Plot current box
					 [state.y1, state.ymotion, state.ymotion, state.y1,      state.y1, state.ymotion], $
					 THICK = 2, COLOR = COLOR_24('white')
			state.xbox = [state.x1, state.x1,      state.xmotion, state.xmotion, state.x1, state.x1     ]		;Store box arrays
			state.ybox = [state.y1, state.ymotion, state.ymotion, state.y1,      state.y1, state.ymotion]
		ENDIF
	
		IF ((state.first_point_chosen) AND (state.second_point_chosen)) THEN BEGIN										;Plot section endpoint
			PLOTS, state.xbox, state.ybox, THICK=2, COLOR = COLOR_24('white') 											;Erase previous box
	
			DEVICE, SET_GRAPHICS_FUNCTION = state.graphics_funct																;Revert to old graphics function
			
			IF ((state.x1 NE state.x2) AND (state.y1 NE state.y2)) THEN BEGIN
				state.map_limit = [(state.y1 < state.y2),(state.x1 < state.x2),$											;Store longitude-latitude limit for map
										 (state.y1 > state.y2),(state.x1 > state.x2)]

				state.x1 	  = !VALUES.F_NAN																							;Rid x1 location
				state.x2 	  = !VALUES.F_NAN																							;Rid x2 location
				state.y1 	  = !VALUES.F_NAN																							;Rid y1 location
				state.y2 	  = !VALUES.F_NAN																							;Rid y2 location
				state.xbox[*] = !VALUES.F_NAN																							;Rid xybox coordinates
				state.ybox[*] = !VALUES.F_NAN
				state.arc[*]  = !VALUES.F_NAN																							;Rid old arc
				WIDGET_CONTROL, state.x1_text_id, SET_VALUE = '	'																;Clear x1 text window
				WIDGET_CONTROL, state.y1_text_id, SET_VALUE = '	'																;Clear y1 text window
				WIDGET_CONTROL, state.x2_text_id, SET_VALUE = '	'																;Clear x2 text window
				WIDGET_CONTROL, state.y2_text_id, SET_VALUE = '	'																;Clear y2 text window
				state.overwrite_set      = 0																							;Reset overwrite instruction for future sections
				state.first_point_chosen = 0																							;Reset first point chosen
				state.redraw_map         = 1																							;Redraw map after zoom
			ENDIF
		ENDIF
	ENDIF ELSE BEGIN
		USERSYM_CIRCLE, /FILL																											;Load circle usersym

		IF ((state.first_point_chosen) AND (~state.second_point_chosen)) THEN BEGIN
			IF (state.overwrite_set EQ 0) THEN BEGIN
				DEVICE, GET_GRAPHICS_FUNCTION = gfunct, SET_GRAPHICS_FUNCTION = 6											;Allow "rubber banding"
				state.graphics_funct = gfunct																							;Save old graphics function for conversion
				state.overwrite_set  = 1																								;Allow overwrite for motion case
				state.arc[*] = !VALUES.D_NAN																							;Initialize arc for redraw
				PLOTS, state.arc[0,*], state.arc[1,*], THICK = 2, COLOR = COLOR_24('white')							;Plot initial arc
			ENDIF	
			PLOTS, state.arc[0,*], state.arc[1,*], THICK=2, COLOR = COLOR_24('white') 									;Erase previous arc

			arc = MAP_2POINTS(state.x1, state.y1, state.xmotion, state.ymotion, NPATH = state.arc_elements, /RHUMB)		;Compute longitudes and latitudes of current arc
			PLOTS, arc[0,*], arc[1,*], THICK = 2, COLOR = COLOR_24('white')												;Plot current great-circle arc
			state.arc = arc																												;Store current arc
		ENDIF

		IF ((state.first_point_chosen) AND (state.second_point_chosen)) THEN BEGIN										;Plot section endpoint
			PLOTS, state.arc[0,*], state.arc[1,*], THICK = 2, COLOR = COLOR_24('white')								;Erase previous arc

			arc = MAP_2POINTS(state.x1, state.y1, state.x2, state.y2, NPATH = state.arc_elements, /RHUMB)		;Compute longitudes and latitudes on arc

			DEVICE, SET_GRAPHICS_FUNCTION = state.graphics_funct																;Revert to old graphics function
			PLOTS, arc[0,*], arc[1,*], THICK = 4, COLOR = COLOR_24('black')												;Plot great-circle arc

			update_flight_time       = 1																								;Set flag to update flight time
			state.arc					 = arc																							;Store final arc	
			state.overwrite_set      = 0																								;Reset overwrite instruction for future sections
			state.first_point_chosen = 0																								;Reset first point chosen
			state.redraw_map         = 0
		ENDIF
	ENDELSE

	IF (state.redraw_map) THEN BEGIN
		WSET, state.draw_window																											;Set window to plot window

		IF (SIZE(*state.data, /TNAME) EQ 'STRUCT') THEN $
			VISUALIZE_WRF_MAP, *state.data, (state.variable_list[state.var_list_index])[state.map_variable_index], $	;Draw map
				(state.variable_list2[state.var_list_index])[state.map_variable_index2], LIMIT = state.map_limit, $
				SMOOTH = state.smooth, MAP_TYPE = state.map_type, ALTITUDE = state.alt_index, $
				DBZ_THRESH = FLOAT((state.echotop_list)[state.echotop_index]), TRACKS = *state.tracks, $
				ARC = state.arc, ZRELATIVE = state.zrelative, TROPOPAUSE = state.tropopause, $
				ZMELT = state.zmelt, COLOR_INDEX = state.icolor, MISSING = state.missing, $
				BAR_MIN = state.bar_min, BAR_MAX = state.bar_max, BAR_TICKS = state.bar_ticks, $
				COUNTIES = state.draw_counties, COUNTY_FILE = state.county_path, RADARS = state.radars, $
				RANGE_RINGS = state.range_rings, RANGE_INTERVAL = state.range_interval, $
				REPORTS = *state.reports, MAP_DATA = *state.map $
		ELSE BEGIN
			MAP_SET, 0, 180, 0, LIMIT = state.map_limit, /ISO, /USA, /CONTINENTS, $
				/HIRES, POSITION = [0.1,0.2,0.9,0.9]
			*state.map = -1																											;Clear prior map data
		ENDELSE
					
		state.redraw_map = 0
	ENDIF
	
	WIDGET_CONTROL, ev.top, SET_UVALUE = state																				;Save state structure
ENDIF

IF (save_graphics_now) THEN BEGIN
	IF (ev.id EQ state.map_png_button_id) THEN BEGIN
		image_file = $																													;Request output file name
			DIALOG_PICKFILE(TITLE = 'Enter output file name', PATH = state.outdir, $
								 GET_PATH = outdir, DIALOG_PARENT = state.base_id)
		IF (STRLEN(image_file) GT STRLEN(outdir)) THEN BEGIN
			state.outdir = outdir																									;Store path for next save
			WIDGET_CONTROL, ev.top, SET_UVALUE = state																		;Save state structure
			VISUALIZE_WRF_MAP, *state.data, (state.variable_list[state.var_list_index])[state.map_variable_index], $	;Draw map
				(state.variable_list2[state.var_list_index])[state.map_variable_index2], LIMIT = state.map_limit, $
				SMOOTH = state.smooth, MAP_TYPE = state.map_type, ALTITUDE = state.alt_index, $
				DBZ_THRESH = FLOAT((state.echotop_list)[state.echotop_index]), TRACKS = *state.tracks, $
				ZRELATIVE = state.zrelative, ARC = state.arc, TROPOPAUSE = state.tropopause, $
				ZMELT = state.zmelt, FILENAME = image_file, COLOR_INDEX = state.icolor, MISSING = state.missing, $
				BAR_MIN = state.bar_min, BAR_MAX = state.bar_max, BAR_TICKS = state.bar_ticks, $
				COUNTIES = state.draw_counties, COUNTY_FILE = state.county_path, $
				RANGE_RINGS = state.range_rings, RANGE_INTERVAL = state.range_interval, $
				RADARS = state.radars, REPORTS = *state.reports, /PNG

			rc = DIALOG_MESSAGE(['Map image saved!'], $										;If file name empty, message error
							TITLE = 'Save Success', DIALOG_PARENT = state.draw_id)
		ENDIF ELSE IF (STRLEN(image_file) GT 0) THEN $
			rc = DIALOG_MESSAGE(['Cannot save image: no filename given!'], $											;If file name empty, message error
							TITLE = 'Save Error', DIALOG_PARENT = state.draw_id)
	ENDIF
	IF (ev.id EQ state.map_eps_button_id) THEN BEGIN
		image_file = $																													;Request output file name
			DIALOG_PICKFILE(TITLE = 'Enter output file name', PATH = state.outdir, $
								 GET_PATH = outdir, DIALOG_PARENT = state.base_id)
		IF (STRLEN(image_file) GT STRLEN(outdir)) THEN BEGIN
			state.outdir = outdir																									;Store path for next save
			WIDGET_CONTROL, ev.top, SET_UVALUE = state																		;Save state structure
			VISUALIZE_WRF_MAP, *state.data, (state.variable_list[state.var_list_index])[state.map_variable_index], $	;Draw map
				(state.variable_list2[state.var_list_index])[state.map_variable_index2], LIMIT = state.map_limit, $
				SMOOTH = state.smooth, MAP_TYPE = state.map_type, ALTITUDE = state.alt_index, $
				DBZ_THRESH = FLOAT((state.echotop_list)[state.echotop_index]), TRACKS = *state.tracks, $
				ZRELATIVE = state.zrelative, ARC = state.arc, TROPOPAUSE = state.tropopause, $
				ZMELT = state.zmelt, FILENAME = image_file, COLOR_INDEX = state.icolor, MISSING = state.missing, $
				BAR_MIN = state.bar_min, BAR_MAX = state.bar_max, BAR_TICKS = state.bar_ticks, $
				COUNTIES = state.draw_counties, COUNTY_FILE = state.county_path, $
				RANGE_RINGS = state.range_rings, RANGE_INTERVAL = state.range_interval, $
				RADARS = state.radars, REPORTS = *state.reports, /EPS
			WSET, state.draw_window

			rc = DIALOG_MESSAGE(['Map PostScript saved!'], $										;If file name empty, message error
							TITLE = 'Save Success', DIALOG_PARENT = state.draw_id)
		ENDIF ELSE IF (STRLEN(image_file) GT 0) THEN $
			rc = DIALOG_MESSAGE(['Cannot save EPS: no filename given!'], $												;If file name empty, message error
							TITLE = 'Save Error', DIALOG_PARENT = state.draw_id)
	ENDIF
	IF (ev.id EQ state.map_nc_button_id) THEN BEGIN
		image_file = $																													;Request output file name
			DIALOG_PICKFILE(TITLE = 'Enter output file name', PATH = state.outdir, $
								 GET_PATH = outdir, DIALOG_PARENT = state.base_id)
		IF (STRLEN(image_file) GT STRLEN(outdir)) THEN BEGIN
			state.outdir = outdir																									;Store path for next save
			WIDGET_CONTROL, ev.top, SET_UVALUE = state																		;Save state structure
			VISUALIZE_WRF_MAP, *state.data, (state.variable_list[state.var_list_index])[state.map_variable_index], $	;Draw map
				(state.variable_list2[state.var_list_index])[state.map_variable_index2], LIMIT = state.map_limit, $
				SMOOTH = state.smooth, MAP_TYPE = state.map_type, ALTITUDE = state.alt_index, $
				DBZ_THRESH = FLOAT((state.echotop_list)[state.echotop_index]), TRACKS = *state.tracks, $
				ZRELATIVE = state.zrelative, ARC = state.arc, TROPOPAUSE = state.tropopause, $
				ZMELT = state.zmelt, FILENAME = image_file, COLOR_INDEX = state.icolor, MISSING = state.missing, $
				BAR_MIN = state.bar_min, BAR_MAX = state.bar_max, BAR_TICKS = state.bar_ticks, $
				COUNTIES = state.draw_counties, COUNTY_FILE = state.county_path, $
				RANGE_RINGS = state.range_rings, RANGE_INTERVAL = state.range_interval, $
				RADARS = state.radars, REPORTS = *state.reports, /NC

			rc = DIALOG_MESSAGE(['Map netCDF saved!'], $										;If file name empty, message error
							TITLE = 'Save Success', DIALOG_PARENT = state.draw_id)
		ENDIF ELSE IF (STRLEN(image_file) GT 0) THEN $
			rc = DIALOG_MESSAGE(['Cannot save netCDF: no filename given!'], $											;If file name empty, message error
							TITLE = 'Save Error', DIALOG_PARENT = state.draw_id)
	ENDIF
	IF (ev.id EQ state.section_png_button_id) THEN BEGIN
		image_file = $																												;Request output file name
			DIALOG_PICKFILE(TITLE = 'Enter output file name', PATH = state.outdir, $
								 GET_PATH = outdir, DIALOG_PARENT = state.base_id)
		IF (STRLEN(image_file) GT STRLEN(outdir)) THEN BEGIN
			state.outdir = outdir																								;Store path for next save
			VISUALIZE_WRF_SECTION, *state.data, (state.variable_list[state.var_list_index])$					;Draw section
				[state.sect_variable_index], (state.variable_list2[state.var_list_index])$
				[state.sect_variable_index2], state.x1, state.y1, state.x2, state.y2, $
				MAP_TYPE = state.map_type, TROPOPAUSE = state.tropopause, $
				ZMELT = state.zmelt, SMOOTH = state.smooth, ZRANGE = [state.z1, state.z2], $
				FILENAME = image_file, COLOR_INDEX = state.icolor, MISSING = state.missing, $
				BAR_MIN = state.bar_min, BAR_MAX = state.bar_max, BAR_TICKS = state.bar_ticks, $
				NEAREST_NEIGHBOR = state.nearest_neighbor, /PNG

			WSET, state.draw_window																								;Set window to plot window
			IF (SIZE(*state.data, /TNAME) EQ 'STRUCT') THEN $
			VISUALIZE_WRF_MAP, *state.data, (state.variable_list[state.var_list_index])[state.map_variable_index], $	;Draw map
				(state.variable_list2[state.var_list_index])[state.map_variable_index2], LIMIT = state.map_limit, $
					SMOOTH = state.smooth, MAP_TYPE = state.map_type, ALTITUDE = state.alt_index, $
					DBZ_THRESH = FLOAT((state.echotop_list)[state.echotop_index]), TRACKS = *state.tracks, $
					ARC = state.arc, ZRELATIVE = state.zrelative, TROPOPAUSE = state.tropopause, $
					ZMELT = state.zmelt, COLOR_INDEX = state.icolor, MISSING = state.missing, $
					BAR_MIN = state.bar_min, BAR_MAX = state.bar_max, BAR_TICKS = state.bar_ticks, $
					COUNTIES = state.draw_counties, COUNTY_FILE = state.county_path, $
					RANGE_RINGS = state.range_rings, RANGE_INTERVAL = state.range_interval, $
					RADARS = state.radars, REPORTS = *state.reports $
			ELSE $
				MAP_SET, 0, 180, 0, LIMIT = state.map_limit, /ISO, /USA, /CONTINENTS, $
					/HIRES, POSITION = [0.1,0.2,0.9,0.9]

			WIDGET_CONTROL, ev.top, SET_UVALUE = state																	;Save state structure

			rc = DIALOG_MESSAGE(['Cross-section image saved!'], $										;If file name empty, message error
							TITLE = 'Save Success', DIALOG_PARENT = state.draw_id)
		ENDIF ELSE IF (STRLEN(image_file) GT 0) THEN $
			rc = DIALOG_MESSAGE(['Cannot save image: no filename given!'], $										;If file name empty, message error
							TITLE = 'Save Error', DIALOG_PARENT = state.draw_id)
	ENDIF
	IF (ev.id EQ state.section_eps_button_id) THEN BEGIN
		image_file = $																													;Request output file name
			DIALOG_PICKFILE(TITLE = 'Enter output file name', PATH = state.outdir, $
								 GET_PATH = outdir, DIALOG_PARENT = state.base_id)
		IF (STRLEN(image_file) GT STRLEN(outdir)) THEN BEGIN
			state.outdir = outdir																									;Store path for next save
			VISUALIZE_WRF_SECTION, *state.data, (state.variable_list[state.var_list_index])$					;Draw section
				[state.sect_variable_index], (state.variable_list2[state.var_list_index])$
				[state.sect_variable_index2], state.x1, state.y1, state.x2, state.y2, $
				MAP_TYPE = state.map_type, TROPOPAUSE = state.tropopause, $
				ZMELT = state.zmelt, SMOOTH = state.smooth, ZRANGE = [state.z1, state.z2], $
				FILENAME = image_file, COLOR_INDEX = state.icolor, MISSING = state.missing, $
				BAR_MIN = state.bar_min, BAR_MAX = state.bar_max, BAR_TICKS = state.bar_ticks, $
				NEAREST_NEIGHBOR = state.nearest_neighbor, /EPS
			WSET,  state.draw_window
			IF (SIZE(*state.data, /TNAME) EQ 'STRUCT') THEN $
			VISUALIZE_WRF_MAP, *state.data, (state.variable_list[state.var_list_index])[state.map_variable_index], $	;Draw map
				(state.variable_list2[state.var_list_index])[state.map_variable_index2], LIMIT = state.map_limit, $
					SMOOTH = state.smooth, MAP_TYPE = state.map_type, ALTITUDE = state.alt_index, $
					DBZ_THRESH = FLOAT((state.echotop_list)[state.echotop_index]), TRACKS = *state.tracks, $
					ARC = state.arc, ZRELATIVE = state.zrelative, TROPOPAUSE = state.tropopause, $
					ZMELT = state.zmelt, COLOR_INDEX = state.icolor, MISSING = state.missing, $
					BAR_MIN = state.bar_min, BAR_MAX = state.bar_max, BAR_TICKS = state.bar_ticks, $
					COUNTIES = state.draw_counties, COUNTY_FILE = state.county_path, $
					RANGE_RINGS = state.range_rings, RANGE_INTERVAL = state.range_interval, $
					RADARS = state.radars, REPORTS = *state.reports $
			ELSE $
				MAP_SET, 0, 180, 0, LIMIT = state.map_limit, /ISO, /USA, /CONTINENTS, $
					/HIRES, POSITION = [0.1,0.2,0.9,0.9]

			WIDGET_CONTROL, ev.top, SET_UVALUE = state																		;Save state structure

			rc = DIALOG_MESSAGE(['Cross-section PostScript saved!'], $										;If file name empty, message error
							TITLE = 'Save Success', DIALOG_PARENT = state.draw_id)
		ENDIF ELSE IF (STRLEN(image_file) GT 0) THEN $
			rc = DIALOG_MESSAGE(['Cannot save EPS: no filename given!'], $												;If file name empty, message error
							TITLE = 'Save Error', DIALOG_PARENT = state.draw_id)
	ENDIF
	IF (ev.id EQ state.section_nc_button_id) THEN BEGIN
		image_file = $																													;Request output file name
			DIALOG_PICKFILE(TITLE = 'Enter output file name', PATH = state.outdir, $
								 GET_PATH = outdir, DIALOG_PARENT = state.base_id)
		IF (STRLEN(image_file) GT STRLEN(outdir)) THEN BEGIN
			state.outdir = outdir																									;Store path for next save
			VISUALIZE_WRF_SECTION, *state.data, (state.variable_list[state.var_list_index])$					;Draw section
				[state.sect_variable_index], (state.variable_list2[state.var_list_index])$
				[state.sect_variable_index2], state.x1, state.y1, state.x2, state.y2, $
				MAP_TYPE = state.map_type, TROPOPAUSE = state.tropopause, $
				ZMELT = state.zmelt, SMOOTH = state.smooth, ZRANGE = [state.z1, state.z2], $
				FILENAME = image_file, COLOR_INDEX = state.icolor, MISSING = state.missing, $
				BAR_MIN = state.bar_min, BAR_MAX = state.bar_max, BAR_TICKS = state.bar_ticks, $
				NEAREST_NEIGHBOR = state.nearest_neighbor, /NC
			WIDGET_CONTROL, ev.top, SET_UVALUE = state																		;Save state structure

			rc = DIALOG_MESSAGE(['Cross-section netCDF saved!'], $										;If file name empty, message error
							TITLE = 'Save Success', DIALOG_PARENT = state.draw_id)
		ENDIF ELSE IF (STRLEN(image_file) GT 0) THEN $
			rc = DIALOG_MESSAGE(['Cannot save netCDF: no filename given!'], $											;If file name empty, message error
							TITLE = 'Save Error', DIALOG_PARENT = state.draw_id)
	ENDIF
ENDIF

!P.POSITION = 0																														;Reset position

RETURN

revert: 																																	;Revert to old coordinates
	bad_coord = DIALOG_MESSAGE(['Coordinates must be provided using numbers only!'], $							;Message Error
				DIALOG_PARENT = state.action_id, TITLE='Input Error')
	WIDGET_CONTROL, state.x1_text_id, SET_VALUE = STRING(state.x1, FORMAT="(F10.2)")								;Set the text
	WIDGET_CONTROL, state.x2_text_id, SET_VALUE = STRING(state.x2, FORMAT="(F10.2)")								;Set the text
	WIDGET_CONTROL, state.y1_text_id, SET_VALUE = STRING(state.y1, FORMAT="(F10.2)")								;Set the text
	WIDGET_CONTROL, state.y2_text_id, SET_VALUE = STRING(state.y2, FORMAT="(F10.2)")								;Set the text

END
;********************************************************************************************************
PRO VISUALIZE_WRF, indir

; NAME:
;		VISUALIZE_WRF
; PURPOSE:
;		Interactive NEXRAD WSR-88D radar composite viewer.
; CATEGORY:
;		X browser.
; CALLING SEQUENCE:
;		VISUALIZE_WRF
; INPUTS:
;		indir : Root input directory of radar data.
; OUTPUTS:
;		None.
; KEYWORDS:
;		None.
; COMMON BLOCKS:
;		None.
; Author and history:
;		Cameron R. Homeyer  2015-01-21.
;-

COMPILE_OPT IDL2																									;Set compile options

load_data = 0																										;Set load data flag

CD, CURRENT = path																								;Get path of User home directory

IF (N_ELEMENTS(indir) EQ 0) THEN indir     = path + PATH_SEP() $									;Set default input directory
									 ELSE load_data = 1

IF (PATH_SEP() EQ '/') THEN SET_PLOT, 'X' $																;Set device to X if unix
							  ELSE SET_PLOT, 'WIN'

!P.BACKGROUND = COLOR_24('gray70')																			;Set background color
!P.COLOR      = COLOR_24('black')																			;Set foreground color

missing_color = [COLOR_24(200,200,200), COLOR_24(255,255,255), $									;Set background color options
					  COLOR_24(255,240,215), COLOR_24(202,225,255)]

xsize = 850																											;Default draw window width
ysize = 700																											;Default draw window height
;xsize = 1200																											;Default draw window width
;ysize =  900																											;Default draw window height

;; WRF SPECIFIC VARIABLES 

variable_list = LIST()
variable_list.Add, ['Z_H   ']
variable_list.Add, ['Z_H   ', 'Z_DR  ', 'K_DP  ']
variable_list.Add, ['Z_H   ', 'Z_DR  ', 'K_DP  ', 'rho_HV']

variable_list2 = LIST()
variable_list2.Add, ['None  ', 'Z_H   ']
variable_list2.Add, ['None  ', 'Z_H   ', 'Z_DR  ', 'K_DP  ']
variable_list2.Add, ['None  ', 'Z_H   ', 'Z_DR  ', 'K_DP  ', 'rho_HV']

file_list = LIST()
file_list.Add, '                        '

IF FILE_TEST(indir + 'cb_2014_us_county_5m.shp') THEN $
	county_path = indir + 'cb_2014_us_county_5m.shp' $
ELSE BEGIN
	IF (PATH_SEP() EQ '/') THEN BEGIN
		IF FILE_TEST('/Users/chomeyer/idl_Shared/lib/plotting/cb_2014_us_county_5m.shp') THEN $
				county_path = '/Users/chomeyer/idl_Shared/lib/plotting/cb_2014_us_county_5m.shp' ELSE $
		IF FILE_TEST('/data1/idl_Shared/lib/plotting/cb_2014_us_county_5m.shp') THEN $
				county_path = '/data1/idl_Shared/lib/plotting/cb_2014_us_county_5m.shp' $
		ELSE  county_path = ''
	ENDIF ELSE  county_path = ''
ENDELSE

state = {indir                : indir, $
			indir_altered        : 0, $
			outdir               : indir, $
			xsize                : xsize, $
			ysize                : ysize, $
			draw_window          : 0, $
			sect_xsize           : 1024, $
			sect_ysize           : 768, $
			section_window       : 0, $
			icolor               : 0, $
			missing              : missing_color[0], $
			missing_color        : missing_color, $
			zoom                 : 0, $
			data                 : PTR_NEW(/ALLOCATE_HEAP), $
			map                  : PTR_NEW(/ALLOCATE_HEAP), $
			tracks               : PTR_NEW(/ALLOCATE_HEAP), $
			reports              : PTR_NEW(/ALLOCATE_HEAP), $
			filter               : 0, $
			clutter              : 0, $
			smooth               : 0, $
			file_list            : file_list, $
			file_index           : 0, $
			file_domain          : [25.0,257.0,48.0,285.0], $
			ianimate0            : 0, $
			ianimate1            : 0, $
			animation_images     : LIST(), $
			first_point_chosen   : 0, $
			second_point_chosen  : 0, $
			overwrite_set        : 0, $
			graphics_funct       : 0, $
			x                    : 0.0, $
			y                    : 0.0, $
			xmotion              : 0.0, $
			ymotion              : 0.0, $
			x1                   : !VALUES.F_NAN, $															;point coordinates
			y1                   : !VALUES.F_NAN, $
			x2                   : !VALUES.F_NAN, $
			y2                   : !VALUES.F_NAN, $
			z1                   : 1.0, $
			z2                   : 20.0, $
			nearest_neighbor     : 1, $
			radars               : 0, $
			range_rings          : 0, $
			county_path          : county_path, $
			draw_counties        : 0, $
			reports_path         : '', $
			draw_reports         : 0, $
			xbox                 : MAKE_ARRAY(6, VALUE = !VALUES.F_NAN), $							;xy-box coordinates
			ybox                 : MAKE_ARRAY(6, VALUE = !VALUES.F_NAN), $
			arc                  : MAKE_ARRAY([2,101], VALUE = !VALUES.D_NAN), $					;Initialize arc
			arc_elements         : 101, $
			redraw_arc           : 0, $
			variable_list        : variable_list, $
			variable_list2       : variable_list2, $
			bar_min              : [ 0.0,-1.0,-1.0,0.7], $
			bar_max              : [75.0, 4.0, 4.0,1.0], $
			bar_ticks            : [   5,   5,   5,  3], $
			map_limit            : [25.0,257.0,48.0,285.0], $
			map_type             : 0, $
			redraw_map           : 0, $
			var_list_index  	   : 0, $
			map_variable_index   : 0, $
			sect_variable_index  : 0, $
			map_variable_index2  : 0, $
			sect_variable_index2 : 0, $
			alt_index            : 0, $
			echotop_list         : ['5','10','15','18.5','25','35','45'],$
			echotop_index        : 0, $
			zrelative            : 0, $
			tropopause           : 0.0, $
			zmelt                : 0.0, $
			range_interval       : 100.0, $
			frame_rate           : 6.0, $
			cw_animate_widget_id : -1, $
	base_id											: 0, $														;Widget ids
	main_id                                : 0, $
		controls_id									: 0, $
			directory_id							: 0, $
				indir_id                      : 0, $
					indir_label_id					: 0, $
					indir_text_id              : 0, $
					indir_browse_id            : 0, $
				file_id                      	: 0, $
					file_label_id					: 0, $
					file_select_id					: 0, $
			mouse_mode_id                    : 0, $
				mouse_label_id                : 0, $
				mouse_button_id               : 0, $
					mouse_zoom_id              : 0, $
					mouse_section_id           : 0, $
			action_id								: 0, $
				reset_zoom_id                 : 0, $
				refresh_map_id                : 0, $
				plot_section_id					: 0, $
				help_id								: 0, $
				quit_id								: 0, $
			save_graphics_id						: 0, $
				save_map_label_id					: 0, $
				map_png_button_id					: 0, $
				map_eps_button_id					: 0, $
				map_nc_button_id              : 0, $
				save_section_label_id			: 0, $
				section_png_button_id			: 0, $
				section_eps_button_id			: 0, $
				section_nc_button_id          : 0, $
			plotting_specs_id                : 0, $
				plot_variables_id             : 0, $
					map_variable_id            : 0, $
					section_variable_id        : 0, $
				plot_variables2_id            : 0, $
					map_variable2_id           : 0, $
					section_variable2_id       : 0, $
				map_type_id                   : 0, $
					map_label_id               : 0, $
					map_button_id              : 0, $
						map_columnmax_id        : 0, $
						map_constalt_id         : 0, $
						map_echotop_id          : 0, $
						map_csa_id              : 0, $
				cappi_echotop_specs_id        : 0, $
					map_constalt_select_id     : 0, $
					map_echotop_select_id      : 0, $
				filter_id                     : 0, $
					filter_query_id            : 0, $
					filter_button_id           : 0, $
						filter_yes_id           : 0, $
						filter_no_id            : 0, $
				clutter_id                    : 0, $
					clutter_query_id           : 0, $
					clutter_button_id          : 0, $
						clutter_yes_id          : 0, $
						clutter_no_id           : 0, $
				smooth_id                     : 0, $
					smooth_query_id            : 0, $
					smooth_button_id           : 0, $
						smooth_yes_id           : 0, $
						smooth_no_id            : 0, $
				map_tropopause_rel_id         : 0, $
					map_echotop_query_id       : 0, $
					map_echotop_button_id      : 0, $
						map_echotop_yes_id      : 0, $
						map_echotop_no_id       : 0, $
			points_id								: 0, $
				points_label_id               : 0, $
				pmotion_id							: 0, $
					motion_label_id				: 0, $
					x_motion_text_id				: 0, $
					y_motion_text_id				: 0, $
					map_motion_text_id         : 0, $
				p1_id									: 0, $
					p1_label_id						: 0, $
					x1_text_id						: 0, $
					y1_text_id						: 0, $
				p2_id									: 0, $
					p2_label_id						: 0, $
					x2_text_id						: 0, $
					y2_text_id						: 0, $
			version_id								: 0, $
		properties_id                       : 0, $
				tropopause_alt_id             : 0, $
					map_echotop_trop_label_id  : 0, $
					map_echotop_trop_alt_id    : 0, $
					trop_alt_units_id          : 0, $
				melting_level_id              : 0, $
					melting_level_label_id     : 0, $
					melting_level_alt_id       : 0, $
					melting_level_units_id     : 0, $
				altitude_axis_id              : 0, $
					zaxis_label_id             : 0, $
					z1_text_id                 : 0, $
					z2_text_id                 : 0, $
					zaxis_units_id             : 0, $
			   nearest_neighbor_id           : 0, $
				   neighbor_query_id          : 0, $
				   neighbor_button_id         : 0, $
					   neighbor_yes_id	      : 0, $
					   neighbor_no_id		      : 0, $
			   radar_loc_id                  : 0, $
				   radar_loc_query_id         : 0, $
				   radar_loc_button_id        : 0, $
					   radar_loc_yes_id	      : 0, $
					   radar_loc_no_id         : 0, $
			   range_ring_id                 : 0, $
				   range_ring_query_id        : 0, $
				   range_ring_button_id       : 0, $
					   range_ring_yes_id	      : 0, $
					   range_ring_no_id        : 0, $
				range_ring_interval_id        : 0, $
					range_ring_label_id        : 0, $
					range_ring_text_id         : 0, $
					range_ring_units_id        : 0, $
				draw_counties_id              : 0, $
					county_query_id            : 0, $
					county_button_id           : 0, $
						county_yes_id	         : 0, $
						county_no_id	         : 0, $
				county_path_id                : 0, $
					county_path_label_id	      : 0, $
					county_path_text_id	      : 0, $
					county_path_browse_id      : 0, $
				draw_reports_id               : 0, $
					reports_query_id           : 0, $
					reports_button_id          : 0, $
						reports_yes_id	         : 0, $
						reports_no_id	         : 0, $
				reports_path_id               : 0, $
					reports_path_label_id      : 0, $
					reports_path_text_id	      : 0, $
					reports_path_browse_id     : 0, $
			color_bar_id    	               : 0, $
				color_bgnd_label_id           : 0, $
				color_bgnd_select_id          : 0, $
					bgnd_gray_button_id        : 0, $
						bgnd_gray_select_id     : 0, $
					bgnd_gray_id               : 0, $
						bgnd_gray_draw_id       : 0, $
					bgnd_white_button_id       : 0, $
						bgnd_white_select_id    : 0, $
					bgnd_white_id              : 0, $
						bgnd_white_draw_id      : 0, $
					bgnd_tan_button_id         : 0, $
						bgnd_tan_select_id      : 0, $
					bgnd_tan_id                : 0, $
						bgnd_tan_draw_id        : 0, $
					bgnd_blue_button_id        : 0, $
						bgnd_blue_select_id     : 0, $
					bgnd_blue_id               : 0, $
						bgnd_blue_draw_id       : 0, $
				color_bar_label_id            : 0, $
				color_table_select_id         : 0, $
					color_table_buttons_id     : 0, $
						color_table1_id         : 0, $
						color_table2_id         : 0, $
						color_table3_id         : 0, $
						color_table4_id         : 0, $
						color_table5_id         : 0, $
					color_table_images_id      : 0, $
						color_table1_draw_id    : 0, $
						color_table2_draw_id    : 0, $
						color_table3_draw_id    : 0, $
						color_table4_draw_id    : 0, $
						color_table5_draw_id    : 0, $
				color_bar_label2_id           : 0, $
				color_bar_labels_id           : 0, $
					color_bar_label_text_id    : 0, $
				zh_color_bar_id               : 0, $
					zh_color_bar_label_id      : 0, $
					zh_min_text_id             : 0, $
					zh_max_text_id             : 0, $
					zh_ticks_text_id           : 0, $
				zdr_color_bar_id              : 0, $
					zdr_color_bar_label_id     : 0, $
					zdr_min_text_id            : 0, $
					zdr_max_text_id            : 0, $
					zdr_ticks_text_id          : 0, $
				kdp_color_bar_id              : 0, $
					kdp_color_bar_label_id     : 0, $
					kdp_min_text_id            : 0, $
					kdp_max_text_id            : 0, $
					kdp_ticks_text_id          : 0, $
				rhv_color_bar_id              : 0, $
					rhv_color_bar_label_id     : 0, $
					rhv_min_text_id            : 0, $
					rhv_max_text_id            : 0, $
					rhv_ticks_text_id          : 0, $
		animation_id                        : 0, $
			first_file_id                    : 0, $
				first_file_label_id				: 0, $
				first_file_select_id				: 0, $
			last_file_id                     : 0, $
				last_file_label_id				: 0, $
				last_file_select_id				: 0, $
			animate_id                       : 0, $
				cw_animate_id                 : 0, $
				save_animation_mov_id         : 0, $
				save_animation_gif_id         : 0, $
			frame_rate_id                    : 0, $
				frame_rate_label_id           : 0, $
				frame_rate_text_id            : 0, $
				frame_rate_units_id           : 0, $
		draw_id										: 0}


state.base_id = WIDGET_BASE(title="WRF Viewer", /ROW, UVALUE = 0)							;Create the widget hierarchy
	state.main_id = WIDGET_TAB(state.base_id, LOCATION = 2)
	state.controls_id = WIDGET_BASE(state.main_id, YSIZE = ysize, /COLUMN, /FRAME, TITLE = ' MAIN ')
		state.directory_id			= WIDGET_BASE(state.controls_id, /COLUMN, /FRAME)
			state.indir_id					= WIDGET_BASE(state.directory_id, /ROW)
				state.indir_label_id				= WIDGET_LABEL(state.indir_id, VALUE = $
																		 'Root Input Directory :')
				state.indir_text_id				= WIDGET_TEXT(state.indir_id, XSIZE = 25, YSIZE = 1, $
																		VALUE = state.indir, /EDITABLE)
				state.indir_browse_id			= WIDGET_BUTTON(state.indir_id, VALUE = ' Browse ')
			state.file_id					= WIDGET_BASE(state.directory_id, /ROW)
				state.file_label_id           = WIDGET_LABEL(state.file_id, VALUE = 'Select File : ')
				state.file_select_id				= WIDGET_LIST(state.file_id, VALUE = state.file_list[0],$
															SCR_YSIZE = 72, SCR_XSIZE = 270)
		state.mouse_mode_id			= WIDGET_BASE(state.controls_id, /ROW, /FRAME, YSIZE = 32)
				state.mouse_label_id	= WIDGET_LABEL(state.mouse_mode_id, VALUE = $
																		'Mouse Drawing Mode : ')
				state.mouse_button_id	= WIDGET_BASE(state.mouse_mode_id, /ROW, /EXCLUSIVE)														
					state.mouse_zoom_id			= WIDGET_BUTTON(state.mouse_button_id, VALUE = 'zoom')
					state.mouse_section_id		= WIDGET_BUTTON(state.mouse_button_id, VALUE = 'section')
		
		state.action_id				= WIDGET_BASE(state.controls_id, /ROW, YSIZE = 30)
			state.reset_zoom_id			= WIDGET_BUTTON(state.action_id, VALUE = ' Reset Zoom ')
			state.refresh_map_id			= WIDGET_BUTTON(state.action_id, VALUE = ' Refresh Map ')
			state.plot_section_id		= WIDGET_BUTTON(state.action_id, VALUE = ' Plot Section ')
			state.help_id              = WIDGET_BUTTON(state.action_id, VALUE = ' Help ')
			state.quit_id              = WIDGET_BUTTON(state.action_id, VALUE = ' Quit ')
		state.save_graphics_id		= WIDGET_BASE(state.controls_id, /ROW, YSIZE = 30)
			state.save_map_label_id		= WIDGET_LABEL(state.save_graphics_id, VALUE = 'Save Map : ')
			state.map_png_button_id    = WIDGET_BUTTON(state.save_graphics_id, VALUE = 'PNG')
			state.map_eps_button_id    = WIDGET_BUTTON(state.save_graphics_id, VALUE = 'EPS')
			state.map_nc_button_id     = WIDGET_BUTTON(state.save_graphics_id, VALUE = 'NC')
			state.save_section_label_id= WIDGET_LABEL(state.save_graphics_id, VALUE = '  Save Section : ')
			state.section_png_button_id= WIDGET_BUTTON(state.save_graphics_id, VALUE = 'PNG')
			state.section_eps_button_id= WIDGET_BUTTON(state.save_graphics_id, VALUE = 'EPS')
			state.section_nc_button_id = WIDGET_BUTTON(state.save_graphics_id, VALUE = 'NC' )
		state.plotting_specs_id       = WIDGET_BASE(state.controls_id, /COLUMN, /FRAME)
			state.plot_variables_id		= WIDGET_BASE(state.plotting_specs_id, /ROW, YSIZE = 32)
				state.map_variable_id		= WIDGET_DROPLIST(state.plot_variables_id, VALUE = $
														state.variable_list[state.var_list_index], $
														TITLE = 'Map Var 1  :')
				state.section_variable_id	= WIDGET_DROPLIST(state.plot_variables_id, VALUE = $
														state.variable_list[state.var_list_index], $
														TITLE = 'Section Var 1  :')
			state.plot_variables2_id		= WIDGET_BASE(state.plotting_specs_id, /ROW, YSIZE = 32)
				state.map_variable2_id		= WIDGET_DROPLIST(state.plot_variables2_id, VALUE = $
														state.variable_list2[state.var_list_index], $
														TITLE = 'Map Var 2  :')
				state.section_variable2_id	= WIDGET_DROPLIST(state.plot_variables2_id, VALUE = $
														state.variable_list2[state.var_list_index], $
														TITLE = 'Section Var 2  :')
			state.map_type_id       	= WIDGET_BASE(state.plotting_specs_id, /ROW, YSIZE = 32)
				state.map_label_id			= WIDGET_LABEL(state.map_type_id, VALUE = 'Map Type : ')
				state.map_button_id			= WIDGET_BASE(state.map_type_id, /ROW, /EXCLUSIVE)														
					state.map_columnmax_id		= WIDGET_BUTTON(state.map_button_id, VALUE = 'Column-Max')
					state.map_constalt_id		= WIDGET_BUTTON(state.map_button_id, VALUE = 'CAPPI')
					state.map_echotop_id			= WIDGET_BUTTON(state.map_button_id, VALUE = 'Echo Top')
					state.map_csa_id				= WIDGET_BUTTON(state.map_button_id, VALUE = 'SL3D')
			state.cappi_echotop_specs_id	= WIDGET_BASE(state.plotting_specs_id, /ROW, YSIZE = 32)
				state.map_constalt_select_id	= WIDGET_DROPLIST(state.cappi_echotop_specs_id, VALUE = '    ', $
															TITLE = 'Const. Alt. (km) = ')
				state.map_echotop_select_id 	= WIDGET_DROPLIST(state.cappi_echotop_specs_id, VALUE = state.echotop_list, $
															TITLE = 'Echo Top (dBZ) = ')
			state.filter_id			 		= WIDGET_BASE(state.plotting_specs_id, /ROW, YSIZE = 32)
				state.filter_query_id	   = WIDGET_LABEL(state.filter_id,            VALUE = 'Bin-weight Filtering Option : ')
				state.filter_button_id     = WIDGET_BASE(state.filter_id, /ROW, /EXCLUSIVE)
					state.filter_yes_id			= WIDGET_BUTTON(state.filter_button_id, VALUE = 'On')
					state.filter_no_id			= WIDGET_BUTTON(state.filter_button_id, VALUE = 'Off')
			state.clutter_id			      = WIDGET_BASE(state.plotting_specs_id, /ROW, YSIZE = 32)
				state.clutter_query_id		= WIDGET_LABEL(state.clutter_id,           VALUE = 'Decluttering Option         : ')
				state.clutter_button_id    = WIDGET_BASE(state.clutter_id, /ROW, /EXCLUSIVE)
					state.clutter_yes_id			= WIDGET_BUTTON(state.clutter_button_id, VALUE = 'On')
					state.clutter_no_id			= WIDGET_BUTTON(state.clutter_button_id, VALUE = 'Off')
			state.smooth_id			      = WIDGET_BASE(state.plotting_specs_id, /ROW, YSIZE = 32)
				state.smooth_query_id	  = WIDGET_LABEL(state.smooth_id,              VALUE = 'Gaussian Smoothing Option   : ')
				state.smooth_button_id    = WIDGET_BASE(state.smooth_id, /ROW, /EXCLUSIVE)
					state.smooth_yes_id			= WIDGET_BUTTON(state.smooth_button_id, VALUE = 'On')
					state.smooth_no_id			= WIDGET_BUTTON(state.smooth_button_id, VALUE = 'Off')
			state.map_tropopause_rel_id   = WIDGET_BASE(state.plotting_specs_id, /ROW, YSIZE = 32)
				state.map_echotop_query_id	= WIDGET_LABEL(state.map_tropopause_rel_id, VALUE = 'Tropopause Relative Option  : ')
				state.map_echotop_button_id= WIDGET_BASE(state.map_tropopause_rel_id, /ROW, /EXCLUSIVE)
					state.map_echotop_yes_id	= WIDGET_BUTTON(state.map_echotop_button_id, VALUE = 'On')
					state.map_echotop_no_id		= WIDGET_BUTTON(state.map_echotop_button_id, VALUE = 'Off')
	state.points_id             = WIDGET_BASE(state.controls_id, /COLUMN, /FRAME)
			state.points_label_id       = WIDGET_LABEL(state.points_id, VALUE = '                   Longitude     Latitude     Map Value')
			state.pmotion_id            = WIDGET_BASE(state.points_id, /ROW)
				state.motion_label_id       = WIDGET_LABEL(state.pmotion_id, VALUE = $
																		 'Mouse Position : ') 
				state.x_motion_text_id      = WIDGET_TEXT(state.pmotion_id, XSIZE = 10, YSIZE = 1, $
																		VALUE = '')
				state.y_motion_text_id      = WIDGET_TEXT(state.pmotion_id, XSIZE = 10, YSIZE = 1, $
																		VALUE = '')
				state.map_motion_text_id    = WIDGET_TEXT(state.pmotion_id, XSIZE = 10, YSIZE = 1, $
																		VALUE = '')
			state.p1_id                 = WIDGET_BASE(state.points_id, /ROW)
				state.p1_label_id           = WIDGET_LABEL(state.p1_id, VALUE = 'Point 1        : ')
				state.x1_text_id            = WIDGET_TEXT(state.p1_id, XSIZE = 10, YSIZE = 1, VALUE = '', /EDITABLE)
				state.y1_text_id            = WIDGET_TEXT(state.p1_id, XSIZE = 10, YSIZE = 1, VALUE = '', /EDITABLE)
			state.p2_id                 = WIDGET_BASE(state.points_id, /ROW)
				state.p1_label_id           = WIDGET_LABEL(state.p2_id, VALUE = 'Point 2        : ')
				state.x2_text_id            = WIDGET_TEXT(state.p2_id, XSIZE = 10, YSIZE = 1, VALUE = '', /EDITABLE)
				state.y2_text_id            = WIDGET_TEXT(state.p2_id, XSIZE = 10, YSIZE = 1, VALUE = '', /EDITABLE)
	state.version_id	= WIDGET_LABEL(state.controls_id, /ALIGN_LEFT, VALUE = $
												'Version 3.1 - U. Oklahoma: 2016 (chomeyer@ou.edu)')

	state.properties_id = WIDGET_BASE(state.main_id, YSIZE = ysize, /COLUMN, TITLE = ' CUSTOMIZATION ')
			state.tropopause_alt_id = WIDGET_BASE(state.properties_id, /ROW, YSIZE = 32)
				state.map_echotop_trop_label_id = WIDGET_LABEL(state.tropopause_alt_id, VALUE = 'Tropopause Altitude          : ')
				state.map_echotop_trop_alt_id   = WIDGET_TEXT(state.tropopause_alt_id, XSIZE = 8, YSIZE = 1, $
																VALUE = STRING(state.tropopause,FORMAT="(F5.2)"), /EDITABLE)
				state.trop_alt_units_id         = WIDGET_LABEL(state.tropopause_alt_id, VALUE = ' km')
			state.melting_level_id = WIDGET_BASE(state.properties_id, /ROW, YSIZE = 32)
				state.melting_level_label_id = WIDGET_LABEL(state.melting_level_id, VALUE =  'Melting Level                : ')
				state.melting_level_alt_id   = WIDGET_TEXT(state.melting_level_id, XSIZE = 8, YSIZE = 1, $
																VALUE = STRING(state.zmelt,FORMAT="(F5.2)"), /EDITABLE)
				state.melting_level_units_id = WIDGET_LABEL(state.melting_level_id, VALUE = ' km')
			state.altitude_axis_id       = WIDGET_BASE(state.properties_id, /ROW, YSIZE = 32)
				state.zaxis_label_id        = WIDGET_LABEL(state.altitude_axis_id, VALUE = 'Section Alt. Range (min,max) : ')
				state.z1_text_id            = WIDGET_TEXT(state.altitude_axis_id, XSIZE = 8, YSIZE = 1, $
															VALUE = STRING(state.z1,FORMAT="(F5.2)"), /EDITABLE)
				state.z2_text_id            = WIDGET_TEXT(state.altitude_axis_id, XSIZE = 8, YSIZE = 1, $
															VALUE = STRING(state.z2,FORMAT="(F5.2)"), /EDITABLE)
				state.zaxis_units_id        = WIDGET_LABEL(state.altitude_axis_id, VALUE = ' km')
			state.nearest_neighbor_id    = WIDGET_BASE(state.properties_id, /ROW, YSIZE = 32)
				state.neighbor_query_id     = WIDGET_LABEL(state.nearest_neighbor_id, VALUE = 'Nearest-Neighbor Section?    : ')
				state.neighbor_button_id    = WIDGET_BASE(state.nearest_neighbor_id, /ROW, /EXCLUSIVE)
					state.neighbor_yes_id	   = WIDGET_BUTTON(state.neighbor_button_id, VALUE = 'Yes')
					state.neighbor_no_id			= WIDGET_BUTTON(state.neighbor_button_id, VALUE = 'No')
			state.radar_loc_id    = WIDGET_BASE(state.properties_id, /ROW, YSIZE = 32)
				state.radar_loc_query_id     = WIDGET_LABEL(state.radar_loc_id, VALUE = 'Show radar locations?        : ')
				state.radar_loc_button_id    = WIDGET_BASE(state.radar_loc_id, /ROW, /EXCLUSIVE)
					state.radar_loc_yes_id	   = WIDGET_BUTTON(state.radar_loc_button_id, VALUE = 'Yes')
					state.radar_loc_no_id		= WIDGET_BUTTON(state.radar_loc_button_id, VALUE = 'No')
			state.range_ring_id    = WIDGET_BASE(state.properties_id, /ROW, YSIZE = 32)
				state.range_ring_query_id     = WIDGET_LABEL(state.range_ring_id, VALUE = 'Draw range rings?            : ')
				state.range_ring_button_id    = WIDGET_BASE(state.range_ring_id, /ROW, /EXCLUSIVE)
					state.range_ring_yes_id	   = WIDGET_BUTTON(state.range_ring_button_id, VALUE = 'Yes')
					state.range_ring_no_id		= WIDGET_BUTTON(state.range_ring_button_id, VALUE = 'No')
			state.range_ring_interval_id = WIDGET_BASE(state.properties_id, /ROW, YSIZE = 32)
				state.range_ring_label_id     = WIDGET_LABEL(state.range_ring_interval_id, VALUE = '      Interval    : ')
				state.range_ring_text_id      = WIDGET_TEXT(state.range_ring_interval_id, XSIZE = 8, YSIZE = 1, $
																VALUE = STRING(state.range_interval,FORMAT="(F6.1)"), /EDITABLE)
				state.range_ring_units_id     = WIDGET_LABEL(state.range_ring_interval_id, VALUE = ' km')
			state.draw_counties_id       = WIDGET_BASE(state.properties_id, /ROW, YSIZE = 32)
				state.county_query_id       = WIDGET_LABEL(state.draw_counties_id, VALUE = 'Draw Counties on Map?        : ')
				state.county_button_id      = WIDGET_BASE(state.draw_counties_id, /ROW, /EXCLUSIVE)
					state.county_yes_id	      = WIDGET_BUTTON(state.county_button_id, VALUE = 'Yes')
					state.county_no_id			= WIDGET_BUTTON(state.county_button_id, VALUE = 'No')
			state.county_path_id			  = WIDGET_BASE(state.properties_id, /ROW)
				state.county_path_label_id		= WIDGET_LABEL(state.county_path_id, VALUE = $
																		 '      County File :')
				state.county_path_text_id		= WIDGET_TEXT(state.county_path_id, XSIZE = 25, YSIZE = 1, $
																		VALUE = state.county_path, /EDITABLE)
				state.county_path_browse_id	= WIDGET_BUTTON(state.county_path_id, VALUE = ' Browse ')
			state.draw_reports_id       = WIDGET_BASE(state.properties_id, /ROW, YSIZE = 32)
				state.reports_query_id       = WIDGET_LABEL(state.draw_reports_id, VALUE = 'Draw SPC Storm Reports on Map? : ')
				state.reports_button_id      = WIDGET_BASE(state.draw_reports_id, /ROW, /EXCLUSIVE)
					state.reports_yes_id	      = WIDGET_BUTTON(state.reports_button_id, VALUE = 'Yes')
					state.reports_no_id			= WIDGET_BUTTON(state.reports_button_id, VALUE = 'No')
			state.reports_path_id			  = WIDGET_BASE(state.properties_id, /ROW)
				state.reports_path_label_id		= WIDGET_LABEL(state.reports_path_id, VALUE = $
																		 '      Report File :')
				state.reports_path_text_id		= WIDGET_TEXT(state.reports_path_id, XSIZE = 25, YSIZE = 1, $
																		VALUE = state.reports_path, /EDITABLE)
				state.reports_path_browse_id	= WIDGET_BUTTON(state.reports_path_id, VALUE = ' Browse ')
		state.color_bar_id       = WIDGET_BASE(state.main_id, YSIZE = ysize, /COLUMN, TITLE = ' COLOR ')
			state.color_bgnd_label_id  = WIDGET_LABEL(state.color_bar_id, VALUE = 'Select Background Color:', /ALIGN_LEFT)
			state.color_bgnd_select_id = WIDGET_BASE(state.color_bar_id, /ROW)
				state.bgnd_gray_button_id    = WIDGET_BASE(state.color_bgnd_select_id, /ROW, /EXCLUSIVE)
					state.bgnd_gray_select_id    = WIDGET_BUTTON(state.bgnd_gray_button_id, VALUE = ' ')
				state.bgnd_gray_id           = WIDGET_BASE(state.color_bgnd_select_id, /ROW)
					state.bgnd_gray_draw_id      = WIDGET_DRAW(state.bgnd_gray_id, XSIZE = 40, YSIZE = 25)
				state.bgnd_white_button_id   = WIDGET_BASE(state.color_bgnd_select_id, /ROW, /EXCLUSIVE)
					state.bgnd_white_select_id   = WIDGET_BUTTON(state.bgnd_white_button_id, VALUE = ' ')
				state.bgnd_white_id          = WIDGET_BASE(state.color_bgnd_select_id, /ROW)
					state.bgnd_white_draw_id     = WIDGET_DRAW(state.bgnd_white_id, XSIZE = 40, YSIZE = 25)
				state.bgnd_tan_button_id     = WIDGET_BASE(state.color_bgnd_select_id, /ROW, /EXCLUSIVE)
					state.bgnd_tan_select_id     = WIDGET_BUTTON(state.bgnd_tan_button_id, VALUE = ' ')
				state.bgnd_tan_id            = WIDGET_BASE(state.color_bgnd_select_id, /ROW)
					state.bgnd_tan_draw_id       = WIDGET_DRAW(state.bgnd_tan_id, XSIZE = 40, YSIZE = 25)
				state.bgnd_blue_button_id    = WIDGET_BASE(state.color_bgnd_select_id, /ROW, /EXCLUSIVE)
					state.bgnd_blue_select_id    = WIDGET_BUTTON(state.bgnd_blue_button_id, VALUE = ' ')
				state.bgnd_blue_id           = WIDGET_BASE(state.color_bgnd_select_id, /ROW)
					state.bgnd_blue_draw_id      = WIDGET_DRAW(state.bgnd_blue_id, XSIZE = 40, YSIZE = 25)
			state.color_bar_label_id 	 = WIDGET_LABEL(state.color_bar_id, VALUE = 'Select Color Table (radar variables only):', /ALIGN_LEFT)
			state.color_table_select_id = WIDGET_BASE(state.color_bar_id, /ROW)
				state.color_table_buttons_id = WIDGET_BASE(state.color_table_select_id, /COLUMN, /EXCLUSIVE)
					state.color_table1_id         = WIDGET_BUTTON(state.color_table_buttons_id, VALUE = ' ')
					state.color_table2_id         = WIDGET_BUTTON(state.color_table_buttons_id, VALUE = ' ')
					state.color_table3_id         = WIDGET_BUTTON(state.color_table_buttons_id, VALUE = ' ')
					state.color_table4_id         = WIDGET_BUTTON(state.color_table_buttons_id, VALUE = ' ')
					state.color_table5_id         = WIDGET_BUTTON(state.color_table_buttons_id, VALUE = ' ')
				state.color_table_images_id  = WIDGET_BASE(state.color_table_select_id, /COLUMN)
					state.color_table1_draw_id    = WIDGET_DRAW(state.color_table_images_id, XSIZE = 310, YSIZE = 25)
					state.color_table2_draw_id    = WIDGET_DRAW(state.color_table_images_id, XSIZE = 310, YSIZE = 25)
					state.color_table3_draw_id    = WIDGET_DRAW(state.color_table_images_id, XSIZE = 310, YSIZE = 25)
					state.color_table4_draw_id    = WIDGET_DRAW(state.color_table_images_id, XSIZE = 310, YSIZE = 25)
					state.color_table5_draw_id    = WIDGET_DRAW(state.color_table_images_id, XSIZE = 310, YSIZE = 25)
			state.color_bar_label2_id 	 = WIDGET_LABEL(state.color_bar_id, VALUE = 'Set Color Bar Properties:', /ALIGN_LEFT)
			state.color_bar_labels_id   = WIDGET_BASE(state.color_bar_id, /ROW)
				state.color_bar_label_text_id = WIDGET_LABEL(state.color_bar_labels_id, VALUE = 'Var.          Min.       Max.       Ticks')
			state.zh_color_bar_id       = WIDGET_BASE(state.color_bar_id, /ROW)
				state.zh_color_bar_label_id = WIDGET_LABEL(state.zh_color_bar_id, VALUE = 'Z_H    : ')
				state.zh_min_text_id        = WIDGET_TEXT(state.zh_color_bar_id, XSIZE = 8, YSIZE = 1, $
														 VALUE = STRING(state.bar_min[0],FORMAT="(F8.2)"), /EDITABLE)
				state.zh_max_text_id        = WIDGET_TEXT(state.zh_color_bar_id, XSIZE = 8, YSIZE = 1, $
														 VALUE = STRING(state.bar_max[0],FORMAT="(F8.2)"), /EDITABLE)
				state.zh_ticks_text_id      = WIDGET_TEXT(state.zh_color_bar_id, XSIZE = 8, YSIZE = 1, $
														 VALUE = STRING(state.bar_ticks[0],FORMAT="(I8)"), /EDITABLE)
			state.zdr_color_bar_id       = WIDGET_BASE(state.color_bar_id, /ROW)
				state.zdr_color_bar_label_id = WIDGET_LABEL(state.zdr_color_bar_id, VALUE = 'Z_DR   : ')
				state.zdr_min_text_id        = WIDGET_TEXT(state.zdr_color_bar_id, XSIZE = 8, YSIZE = 1, $
														 VALUE = STRING(state.bar_min[1],FORMAT="(F8.2)"), /EDITABLE)
				state.zdr_max_text_id        = WIDGET_TEXT(state.zdr_color_bar_id, XSIZE = 8, YSIZE = 1, $
														 VALUE = STRING(state.bar_max[1],FORMAT="(F8.2)"), /EDITABLE)
				state.zdr_ticks_text_id      = WIDGET_TEXT(state.zdr_color_bar_id, XSIZE = 8, YSIZE = 1, $
														 VALUE = STRING(state.bar_ticks[1],FORMAT="(I8)"), /EDITABLE)
			state.kdp_color_bar_id       = WIDGET_BASE(state.color_bar_id, /ROW)
				state.kdp_color_bar_label_id = WIDGET_LABEL(state.kdp_color_bar_id, VALUE = 'K_DP   : ')
				state.kdp_min_text_id        = WIDGET_TEXT(state.kdp_color_bar_id, XSIZE = 8, YSIZE = 1, $
														 VALUE = STRING(state.bar_min[2],FORMAT="(F8.2)"), /EDITABLE)
				state.kdp_max_text_id        = WIDGET_TEXT(state.kdp_color_bar_id, XSIZE = 8, YSIZE = 1, $
														 VALUE = STRING(state.bar_max[2],FORMAT="(F8.2)"), /EDITABLE)
				state.kdp_ticks_text_id      = WIDGET_TEXT(state.kdp_color_bar_id, XSIZE = 8, YSIZE = 1, $
														 VALUE = STRING(state.bar_ticks[2],FORMAT="(I8)"), /EDITABLE)
			state.rhv_color_bar_id       = WIDGET_BASE(state.color_bar_id, /ROW)
				state.rhv_color_bar_label_id = WIDGET_LABEL(state.rhv_color_bar_id, VALUE = 'rho_HV : ')
				state.rhv_min_text_id        = WIDGET_TEXT(state.rhv_color_bar_id, XSIZE = 8, YSIZE = 1, $
														 VALUE = STRING(state.bar_min[3],FORMAT="(F8.2)"), /EDITABLE)
				state.rhv_max_text_id        = WIDGET_TEXT(state.rhv_color_bar_id, XSIZE = 8, YSIZE = 1, $
														 VALUE = STRING(state.bar_max[3],FORMAT="(F8.2)"), /EDITABLE)
				state.rhv_ticks_text_id      = WIDGET_TEXT(state.rhv_color_bar_id, XSIZE = 8, YSIZE = 1, $
														 VALUE = STRING(state.bar_ticks[3],FORMAT="(I8)"), /EDITABLE)
	state.animation_id = WIDGET_BASE(state.main_id, YSIZE = ysize, /COLUMN, TITLE = ' ANIMATION ')
		state.first_file_id			   = WIDGET_BASE(state.animation_id, /ROW)
			state.first_file_label_id        = WIDGET_LABEL(state.first_file_id, VALUE = 'Start File: ')
			state.first_file_select_id			= WIDGET_LIST(state.first_file_id, VALUE = state.file_list[0],$
															SCR_YSIZE = 72, SCR_XSIZE = 270)
		state.last_file_id				= WIDGET_BASE(state.animation_id, /ROW)
			state.last_file_label_id        = WIDGET_LABEL(state.last_file_id, VALUE = 'End File  : ')
			state.last_file_select_id		  = WIDGET_LIST(state.last_file_id, VALUE = state.file_list[0],$
															SCR_YSIZE = 72, SCR_XSIZE = 270)
		state.animate_id				   = WIDGET_BASE(state.animation_id, /ROW)
			state.cw_animate_id             = WIDGET_BUTTON(state.animate_id, VALUE = ' GENERATE ANIMATION ')
			state.save_animation_mov_id     = WIDGET_BUTTON(state.animate_id, VALUE = '  SAVE AS .MOV  ')
			state.save_animation_gif_id     = WIDGET_BUTTON(state.animate_id, VALUE = '  SAVE AS .GIF  ')
		state.frame_rate_id           = WIDGET_BASE(state.animation_id, /ROW, YSIZE = 32)
			state.frame_rate_label_id       = WIDGET_LABEL(state.frame_rate_id, VALUE =  'Frame rate : ')
			state.frame_rate_text_id        = WIDGET_TEXT(state.frame_rate_id, XSIZE = 8, YSIZE = 1, $
															VALUE = STRING(state.frame_rate,FORMAT="(F5.2)"), /EDITABLE)
			state.frame_rate_units_id       = WIDGET_LABEL(state.frame_rate_id, VALUE = ' fps')
	state.draw_id     = WIDGET_DRAW(state.base_id, XSIZE = xsize, YSIZE = ysize, /BUTTON_EVENTS, $
											  /MOTION_EVENTS)

WIDGET_CONTROL, state.base_id, /REALIZE																	;Realize widgets
WIDGET_CONTROL, state.draw_id, GET_VALUE = draw_window

!P.BACKGROUND = COLOR_24('white')
!P.COLOR      = COLOR_24('black')
!P.CHARSIZE   = 2.0
!P.FONT       = -1																								;Use Hershey fonts
	
IF (load_data) THEN BEGIN
	file_list = FILE_SEARCH(state.indir + '*.nc')														;Search for netCDF files
	IF (N_ELEMENTS(state.file_list) GT 0) THEN state.file_list.Remove, 0							;If previous file list, remove
	IF (STRLEN(file_list[0]) GT 0) THEN BEGIN
		file_list = STRMID(file_list, STRLEN(state.indir))												;Remove input directory from file list
		state.file_list.Add, file_list																		;Add current file list
		WIDGET_CONTROL, state.file_select_id,       SET_VALUE = file_list
		WIDGET_CONTROL, state.first_file_select_id, SET_VALUE = file_list
		WIDGET_CONTROL, state.last_file_select_id,  SET_VALUE = file_list
		state.ianimate0 = 0
		state.ianimate1 = 0

		id   = NCDF_OPEN(state.indir + file_list[0])														;Open file for reading
		inq  = NCDF_INQUIRE(id)																					;Inquire about netCDF file
		FOR i = 0, inq.nvars -1 DO $
			IF (i EQ 0) THEN vnames = (NCDF_VARINQ(id, i)).name $										;Poplulate netcdf variable names
							ELSE vnames = [vnames, (NCDF_VARINQ(id, i)).name]
		NCDF_CLOSE, id
		iv2 = WHERE((vnames EQ 'nbeams_contributing'),v2test)											;Search for variables unique to compositing method
		iv3 = WHERE((vnames EQ 'files_merged'       ),v3test)

		IF (v3test) THEN BEGIN
			*state.data = NEXRAD_READ_LEVEL2_COMPOSITE(state.indir + file_list[0])				;Load NEXRAD data
			IF (SIZE((*state.data).zdr, /TNAME) EQ 'STRUCT') THEN state.var_list_index = 2 $	;Check for polarimetric variables
																			 ELSE state.var_list_index = 0
		ENDIF ELSE IF (v2test) THEN BEGIN
			*state.data = NEXRAD_READ_LEVEL2_2(state.indir + file_list[0])							;Load NEXRAD data
			IF (SIZE((*state.data).zdr, /TNAME) EQ 'STRUCT') THEN state.var_list_index = 1 $	;Check for polarimetric variables
																			 ELSE state.var_list_index = 0
		ENDIF ELSE BEGIN
			*state.data = NEXRAD_READ_LEVEL2(INFILE = state.indir + file_list[0])				;Load NEXRAD data
			IF (SIZE((*state.data).zdr, /TNAME) EQ 'STRUCT') THEN state.var_list_index = 2 $	;Check for polarimetric variables
																			 ELSE state.var_list_index = 0
		ENDELSE
		
		*state.data = NEXRAD_FILL_HOLES(*state.data)														;Fill echo holes
		IF FILE_TEST(state.indir + 'tracks.txt') THEN $
			*state.tracks = NEXRAD_READ_TRACKS(state.indir + 'tracks.txt') ELSE $
			*state.tracks = -1

		WIDGET_CONTROL, state.map_variable_id,      SET_VALUE = (state.variable_list )[state.var_list_index]		;Set variable lists
		WIDGET_CONTROL, state.section_variable_id,  SET_VALUE = (state.variable_list )[state.var_list_index]
		WIDGET_CONTROL, state.map_variable2_id,     SET_VALUE = (state.variable_list2)[state.var_list_index]		;Set variable lists
		WIDGET_CONTROL, state.section_variable2_id, SET_VALUE = (state.variable_list2)[state.var_list_index]

		WIDGET_CONTROL, state.map_constalt_select_id, SET_VALUE = $									;Set altitude array for constant altitude maps
			STRING((*state.data).z.values,FORMAT="(F4.1)")
		
		limit             = [(*state.data).y.values[ 0],(*state.data).x.values[ 0], $			;Set domain & map limit
									(*state.data).y.values[-1],(*state.data).x.values[-1]]
		state.map_limit   = limit
		state.file_domain = limit

		VISUALIZE_WRF_MAP, *state.data, (state.variable_list[state.var_list_index])[state.map_variable_index], $	;Draw map
			(state.variable_list2[state.var_list_index])[state.map_variable_index2], LIMIT = state.map_limit, $
			SMOOTH = state.smooth, MAP_TYPE = state.map_type, ALTITUDE = state.alt_index, $
			DBZ_THRESH = FLOAT((state.echotop_list)[state.echotop_index]), $
			ARC = state.arc, ZRELATIVE = state.zrelative, TROPOPAUSE = state.tropopause, $
			ZMELT = state.zmelt, COLOR_INDEX = state.icolor, MISSING = state.missing, $
			BAR_MIN = state.bar_min, BAR_MAX = state.bar_max, BAR_TICKS = state.bar_ticks, $
			COUNTIES = state.draw_counties, COUNTY_FILE = state.county_path, $
			RANGE_RINGS = state.range_rings, RANGE_INTERVAL = state.range_interval, $
			RADARS = state.radars, REPORTS = *state.reports, MAP_DATA = *state.map
	ENDIF ELSE BEGIN
		MAP_SET, 0, 180, 0, LIMIT = state.map_limit, /ISO, /USA, /CONTINENTS, $					;Default map if no data
			/HIRES, POSITION = [0.1,0.2,0.9,0.9] 
		*state.map = -1																							;Set map data to missing
	ENDELSE
ENDIF ELSE BEGIN
	MAP_SET, 0, 180, 0, LIMIT = state.map_limit, /ISO, /USA, /CONTINENTS, $						;Default map if no data
		/HIRES, POSITION = [0.1,0.2,0.9,0.9]
	*state.map = -1																								;Set map data to missing
ENDELSE

state.draw_window = draw_window
*state.reports    = -1

WIDGET_CONTROL, state.mouse_section_id,    /SET_BUTTON												;Set the default mouse mode button for display
WIDGET_CONTROL, state.map_columnmax_id,    /SET_BUTTON												;Set the default map type button for display
WIDGET_CONTROL, state.map_echotop_no_id,   /SET_BUTTON												;Set the default tropopause-relative response for display
WIDGET_CONTROL, state.clutter_no_id,       /SET_BUTTON												;Set the default clutter removal response for display
WIDGET_CONTROL, state.filter_no_id,        /SET_BUTTON												;Set the default bin filtering response for display
WIDGET_CONTROL, state.smooth_no_id,        /SET_BUTTON												;Set the default Gaussian smoothing response for display
WIDGET_CONTROL, state.neighbor_yes_id,     /SET_BUTTON												;Set the default section method
WIDGET_CONTROL, state.radar_loc_no_id,     /SET_BUTTON												;Set the default radar location method
WIDGET_CONTROL, state.range_ring_no_id,    /SET_BUTTON												;Set the default range ring method
WIDGET_CONTROL, state.county_no_id,        /SET_BUTTON												;Set the default county mapping method
WIDGET_CONTROL, state.reports_no_id,       /SET_BUTTON												;Set the default severe reports method
WIDGET_CONTROL, state.bgnd_gray_select_id, /SET_BUTTON												;Set the default background color selection
WIDGET_CONTROL, state.color_table1_id,     /SET_BUTTON												;Set the default color table selection

WIDGET_CONTROL, state.base_id, SET_UVALUE = state														;Store state structure in base widget uvalue
XMANAGER, 'VISUALIZE_WRF', state.base_id, /NO_BLOCK													;Start event loop handler

END
