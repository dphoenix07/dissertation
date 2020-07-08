FUNCTION TEMP_DALLAS_NAAQS, year, $
	PNG		  = png, $
	EPS		  = eps, $
	CLOBBER   = clobber

;+
;NAME:
;     TRAJ3D_RAP_P
;PURPOSE:
;     This copies variables from ERA-Interim analysis into a single
;     file in pressure coordinates for use in TRAJ3D.
;     W at the surface is set to zero.  One pressure level is added at 
;     the top of the domain (p = 0), where w is also set to zero.
;CATEGORY:
;     Data handling utility.
;CALLING SEQUENCE:
;     TRAJ3D_RAP_P, date0, outfile
;INPUT:
;		flight_name : RAF flight name (e.g., 'rf01')
;		direction   : 'forward' or 'backward'
;		ndays       : Length of trajectory run in days.  Default is 5.
;KEYWORDS:
;     PLOT      : If set, plot sample maps.
;     DIRECTORY : Output directory for wind file.
;	  CLOBBER   : If set, overwrite existing file. This is the default.
;OUTPUT:
;     Netcdf file.
;MODIFICATION HISTORY:
;		C. Homeyer:       2015-06-22.
;-

COMPILE_OPT IDL2																									;Set compile options

indir = !WRF_DIRECTORY + 'general/met_obs/temp/'
infile = indir + 'dallas_temp_' + year + '.csv'

headers= ['State Code','County Code','Site Num','Parameter Code','POC','Latitude','Longitude', $
	'Datum','Parameter Name','Date Local','Time Local','Date GMT','Time GMT','Sample Measurement', $
	'Units of Measurement','MDL','Uncertainty','Qualifier','Method Type','Method Code','Method Name', $
	'State Name','County Name','Date of Last Change']

data = READ_CSV(infile);, HEADER = headers)

CASE year OF 
	'1980' : BEGIN
			site1 = WHERE(data.field03 EQ 44)
			site2 = WHERE(data.field03 EQ 45)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
			END

	'1981' : BEGIN
			site1 = WHERE(data.field03 EQ 44)
			site2 = WHERE(data.field03 EQ 45)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
			END
		
	'1982' : BEGIN
			site1 = WHERE(data.field03 EQ 44)
			site2 = WHERE(data.field03 EQ 45)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
			END

	'1983' : BEGIN
			site1 = WHERE(data.field03 EQ 44)
			site2 = WHERE(data.field03 EQ 45)
			site3 = WHERE(data.field03 EQ 55)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
			END

	'1984' : BEGIN
			site1 = WHERE(data.field03 EQ 44)
			site2 = WHERE(data.field03 EQ 45)
			site3 = WHERE(data.field03 EQ 55)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
			END

	'1985' : BEGIN
			site1 = WHERE(data.field03 EQ 44)
			site2 = WHERE(data.field03 EQ 45)
			site3 = WHERE(data.field03 EQ 55)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
			END
		
	'1986' : BEGIN
			site1 = WHERE(data.field03 EQ 44)
			site2 = WHERE(data.field03 EQ 45)
			site3 = WHERE(data.field03 EQ 55)
			site4 = WHERE(data.field03 EQ 69)
			site5 = WHERE(data.field03 EQ 86)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
            temp4 = data.field14[site4]
            temp5 = data.field14[site5]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            date5 = data.field12[site5]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
            time5 = data.field13[site5]
			END

	'1987' : BEGIN
			site1 = WHERE(data.field03 EQ 45)
			site2 = WHERE(data.field03 EQ 55)
			site3 = WHERE(data.field03 EQ 69)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
			END

	'1988' : BEGIN
			site1 = WHERE(data.field03 EQ 45)
			site2 = WHERE(data.field03 EQ 55)
			site3 = WHERE(data.field03 EQ 69)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
			END

	'1989' : BEGIN
			site1 = WHERE(data.field03 EQ 45)
			site2 = WHERE(data.field03 EQ 55)
			site3 = WHERE(data.field03 EQ 69)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
			END

	'1990' : BEGIN
			site1 = WHERE(data.field03 EQ 45)
			site2 = WHERE(data.field03 EQ 55)
			site3 = WHERE(data.field03 EQ 69)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
			END

	'1991' : BEGIN
			site1 = WHERE(data.field03 EQ 45)
			site2 = WHERE(data.field03 EQ 55)
			site3 = WHERE(data.field03 EQ 69)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
			END

	'1992' : BEGIN
			site1 = WHERE(data.field03 EQ 45)
			site2 = WHERE(data.field03 EQ 55)
			site3 = WHERE(data.field03 EQ 69)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
			END

	'1993' : BEGIN
			site1 = WHERE(data.field03 EQ 45)
			site2 = WHERE(data.field03 EQ 55)
			site3 = WHERE(data.field03 EQ 69)
			site4 = WHERE(data.field03 EQ 70)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
            temp4 = data.field14[site4]
        
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]

            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
			END

	'1994' : BEGIN
			site1 = WHERE(data.field03 EQ 45)
			site2 = WHERE(data.field03 EQ 55)
			site3 = WHERE(data.field03 EQ 69)
			site4 = WHERE(data.field03 EQ 70)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
            temp4 = data.field14[site4]
        
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]

            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
			END

	'1995' : BEGIN
			site1 = WHERE(data.field03 EQ 45)
			site2 = WHERE(data.field03 EQ 69)
			site3 = WHERE(data.field03 EQ 70)
			site4 = WHERE(data.field03 EQ 87)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
            temp4 = data.field14[site4]
        
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]

            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
			END

	'1996' : BEGIN
			site1 = WHERE(data.field03 EQ 45)
			site2 = WHERE(data.field03 EQ 69)
			site3 = WHERE(data.field03 EQ 70)
			site4 = WHERE(data.field03 EQ 87)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
            temp4 = data.field14[site4]
        
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]

            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
			END

	'1997' : BEGIN
			site1 = WHERE(data.field03 EQ 45)
			site2 = WHERE(data.field03 EQ 69)
			site3 = WHERE(data.field03 EQ 87)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
			END

	'1998' : BEGIN
			site1 = WHERE(data.field03 EQ 45)
			site2 = WHERE(data.field03 EQ 57)
			site3 = WHERE(data.field03 EQ 69)
			site4 = WHERE(data.field03 EQ 75)
			site5 = WHERE(data.field03 EQ 87)
			site6 = WHERE(data.field03 EQ 1006)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
            temp4 = data.field14[site4]
            temp5 = data.field14[site5]
            temp6 = data.field14[site6]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            date5 = data.field12[site5]
            date6 = data.field12[site6]

            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
            time5 = data.field13[site5]
            time6 = data.field13[site6]
			END

	'1999' : BEGIN
			site1 = WHERE(data.field03 EQ 57)
			site2 = WHERE(data.field03 EQ 69)
			site3 = WHERE(data.field03 EQ 75)
			site4 = WHERE(data.field03 EQ 87)
			site5 = WHERE(data.field03 EQ 1006)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
            temp4 = data.field14[site4]
            temp5 = data.field14[site5]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            date5 = data.field12[site5]

            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
            time5 = data.field13[site5]
			END

	'2000' : BEGIN
			site1 = WHERE(data.field03 EQ 57)
			site2 = WHERE(data.field03 EQ 69)
			site3 = WHERE(data.field03 EQ 75)
			site4 = WHERE(data.field03 EQ 87)
			site5 = WHERE(data.field03 EQ 1006)
			site6 = WHERE(data.field03 EQ 3003)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
            temp4 = data.field14[site4]
            temp5 = data.field14[site5]
            temp6 = data.field14[site6]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            date5 = data.field12[site5]
            date6 = data.field12[site6]

            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
            time5 = data.field13[site5]
            time6 = data.field13[site6]
			END

	'2001' : BEGIN
			site1 = WHERE(data.field03 EQ 57)
			site2 = WHERE(data.field03 EQ 69)
			site3 = WHERE(data.field03 EQ 75)
			site4 = WHERE(data.field03 EQ 87)
			site5 = WHERE(data.field03 EQ 1006)
			site6 = WHERE(data.field03 EQ 3003)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
            temp4 = data.field14[site4]
            temp5 = data.field14[site5]
            temp6 = data.field14[site6]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            date5 = data.field12[site5]
            date6 = data.field12[site6]

            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
            time5 = data.field13[site5]
            time6 = data.field13[site6]
			END

	'2002' : BEGIN
			site1 = WHERE(data.field03 EQ 50)
			site2 = WHERE(data.field03 EQ 57)
			site3 = WHERE(data.field03 EQ 69)
			site4 = WHERE(data.field03 EQ 75)
			site5 = WHERE(data.field03 EQ 87)
			site6 = WHERE(data.field03 EQ 1006)
			site7 = WHERE(data.field03 EQ 3003)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
            temp4 = data.field14[site4]
            temp5 = data.field14[site5]
            temp6 = data.field14[site6]
            temp7 = data.field14[site7]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            date5 = data.field12[site5]
            date6 = data.field12[site6]
            date7 = data.field12[site7]

            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
            time5 = data.field13[site5]
            time6 = data.field13[site6]
            time7 = data.field13[site7]
			END

	'2003' : BEGIN
			site1 = WHERE(data.field03 EQ 50)
			site2 = WHERE(data.field03 EQ 57)
			site3 = WHERE(data.field03 EQ 69)
			site4 = WHERE(data.field03 EQ 75)
			site5 = WHERE(data.field03 EQ 87)
			site6 = WHERE(data.field03 EQ 1006)
			site7 = WHERE(data.field03 EQ 3003)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
            temp4 = data.field14[site4]
            temp5 = data.field14[site5]
            temp6 = data.field14[site6]
            temp7 = data.field14[site7]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            date5 = data.field12[site5]
            date6 = data.field12[site6]
            date7 = data.field12[site7]

            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
            time5 = data.field13[site5]
            time6 = data.field13[site6]
            time7 = data.field13[site7]
			END

	'2004' : BEGIN
			site1 = WHERE(data.field03 EQ 50)
			site2 = WHERE(data.field03 EQ 57)
			site3 = WHERE(data.field03 EQ 69)
			site4 = WHERE(data.field03 EQ 75)
			site5 = WHERE(data.field03 EQ 87)
			site6 = WHERE(data.field03 EQ 3003)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
            temp4 = data.field14[site4]
            temp5 = data.field14[site5]
            temp6 = data.field14[site6]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            date5 = data.field12[site5]
            date6 = data.field12[site6]

            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
            time5 = data.field13[site5]
            time6 = data.field13[site6]
			END

	'2005' : BEGIN
			site1 = WHERE(data.field03 EQ 50)
			site2 = WHERE(data.field03 EQ 57)
			site3 = WHERE(data.field03 EQ 69)
			site4 = WHERE(data.field03 EQ 75)
			site5 = WHERE(data.field03 EQ 87)
			site6 = WHERE(data.field03 EQ 3003)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
            temp4 = data.field14[site4]
            temp5 = data.field14[site5]
            temp6 = data.field14[site6]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            date5 = data.field12[site5]
            date6 = data.field12[site6]

            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
            time5 = data.field13[site5]
            time6 = data.field13[site6]
			END

	'2006' : BEGIN
			site1 = WHERE(data.field03 EQ 50)
			site2 = WHERE(data.field03 EQ 57)
			site3 = WHERE(data.field03 EQ 69)
			site4 = WHERE(data.field03 EQ 75)
			site5 = WHERE(data.field03 EQ 87)
			site6 = WHERE(data.field03 EQ 3003)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
            temp4 = data.field14[site4]
            temp5 = data.field14[site5]
            temp6 = data.field14[site6]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            date5 = data.field12[site5]
            date6 = data.field12[site6]

            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
            time5 = data.field13[site5]
            time6 = data.field13[site6]
			END

	'2007' : BEGIN
			site1 = WHERE(data.field03 EQ 50)
			site2 = WHERE(data.field03 EQ 57)
			site3 = WHERE(data.field03 EQ 69)
			site4 = WHERE(data.field03 EQ 75)
			site5 = WHERE(data.field03 EQ 87)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
            temp4 = data.field14[site4]
            temp5 = data.field14[site5]
         
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            date5 = data.field12[site5]
         
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
            time5 = data.field13[site5]
			END

	'2008' : BEGIN
			site1 = WHERE(data.field03 EQ 50)
			site2 = WHERE(data.field03 EQ 57)
			site3 = WHERE(data.field03 EQ 69)
			site4 = WHERE(data.field03 EQ 75)
			site5 = WHERE(data.field03 EQ 87)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
            temp4 = data.field14[site4]
            temp5 = data.field14[site5]
         
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            date5 = data.field12[site5]
         
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
            time5 = data.field13[site5]
			END

	'2009' : BEGIN
			site1 = WHERE(data.field03 EQ 50)
			site2 = WHERE(data.field03 EQ 69)
			site3 = WHERE(data.field03 EQ 75)
			site4 = WHERE(data.field03 EQ 87)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
            temp4 = data.field14[site4]
         
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
         
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
			END

	'2010' : BEGIN
			site1 = WHERE(data.field03 EQ 50)
			site2 = WHERE(data.field03 EQ 69)
			site3 = WHERE(data.field03 EQ 75)
			site4 = WHERE(data.field03 EQ 87)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
            temp4 = data.field14[site4]
         
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
         
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
			END
		
	'2011' : BEGIN
			site1 = WHERE(data.field03 EQ 50)
			site2 = WHERE(data.field03 EQ 69)
			site3 = WHERE(data.field03 EQ 75)
			site4 = WHERE(data.field03 EQ 87)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
            temp4 = data.field14[site4]
         
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
         
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
            END

	'2012' : BEGIN
			site1 = WHERE(data.field03 EQ 50)
			site2 = WHERE(data.field03 EQ 69)
			site3 = WHERE(data.field03 EQ 75)
			site4 = WHERE(data.field03 EQ 87)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
            temp4 = data.field14[site4]
         
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
         
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
            END

	'2013' : BEGIN
			site1 = WHERE(data.field03 EQ 50)
			site2 = WHERE(data.field03 EQ 69)
			site3 = WHERE(data.field03 EQ 75)
			site4 = WHERE(data.field03 EQ 87)

            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
            temp4 = data.field14[site4]
         
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
         
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
            END

	'2014' : BEGIN
			site1 = WHERE(data.field03 EQ 50)
			site2 = WHERE(data.field03 EQ 69)
			site3 = WHERE(data.field03 EQ 75)
			site4 = WHERE(data.field03 EQ 87)
			site5 = WHERE(data.field03 EQ 1067)
			
            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
            temp4 = data.field14[site4]
            temp5 = data.field14[site5]
         
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            date5 = data.field12[site5]

            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
            time5 = data.field12[site5]
            END

	'2015' : BEGIN
			site1 = WHERE(data.field03 EQ 50)
			site2 = WHERE(data.field03 EQ 69)
			site3 = WHERE(data.field03 EQ 75)
			site4 = WHERE(data.field03 EQ 87)
			site5 = WHERE(data.field03 EQ 1067)
			
            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
            temp4 = data.field14[site4]
            temp5 = data.field14[site5]
         
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            date5 = data.field12[site5]

            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
            time5 = data.field12[site5]
            END

	'2016' : BEGIN
			site1 = WHERE(data.field03 EQ 50)
			site2 = WHERE(data.field03 EQ 69)
			site3 = WHERE(data.field03 EQ 75)
			site4 = WHERE(data.field03 EQ 87)
			site5 = WHERE(data.field03 EQ 1067)
			
            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
            temp4 = data.field14[site4]
            temp5 = data.field14[site5]
         
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            date5 = data.field12[site5]

            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
            time5 = data.field12[site5]
            END

	'2017' : BEGIN
			site1 = WHERE(data.field03 EQ 50)
			site2 = WHERE(data.field03 EQ 69)
			site3 = WHERE(data.field03 EQ 75)
			site4 = WHERE(data.field03 EQ 87)
			site5 = WHERE(data.field03 EQ 1067)
			
            temp1 = data.field14[site1]
            temp2 = data.field14[site2]
            temp3 = data.field14[site3]
            temp4 = data.field14[site4]
            temp5 = data.field14[site5]
         
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            date5 = data.field12[site5]

            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
            time5 = data.field12[site5]
            END

ENDCASE         

num_hours = [N_ELEMENTS(site1),N_ELEMENTS(site2),N_ELEMENTS(site3)]
temp_hourly = [ ]

temp1_8hr = FLTARR(N_ELEMENTS(site1))
FOR tt = 0, N_ELEMENTS(site1)-9 DO BEGIN
	temp1_8hr[tt] = MEAN(temp1[tt:tt+8],/NAN)
ENDFOR
temp_hourly = [temp_hourly, temp1]
temp_8hr_total = [temp1_8hr]
PRINT, 'Done calculating site 1'

temp2_8hr = FLTARR(N_ELEMENTS(site2))
FOR tt = 0, N_ELEMENTS(site2)-9 DO BEGIN
	temp2_8hr[tt] = MEAN(temp2[tt:tt+8],/NAN)
ENDFOR
temp_hourly = [temp_hourly, temp2]
temp_8hr_total = [temp1_8hr, temp2_8hr]

PRINT, 'Done calculating site 2'

IF (N_ELEMENTS(site3) GT 0) THEN BEGIN
    temp3_8hr = FLTARR(N_ELEMENTS(site3))
    FOR tt = 0, N_ELEMENTS(site3)-9 DO BEGIN
    	temp3_8hr[tt] = MEAN(temp3[tt:tt+8],/NAN)
    ENDFOR
    temp_hourly = [temp_hourly, temp3]
    PRINT, 'Done calculating site 3'
    
    temp_8hr_total = [temp1_8hr, temp2_8hr, temp3_8hr]
ENDIF

IF (N_ELEMENTS(site4) GT 0) THEN BEGIN
    temp4_8hr = FLTARR(N_ELEMENTS(site4))
    FOR tt = 0, N_ELEMENTS(site4)-9 DO BEGIN
    	temp4_8hr[tt] = MEAN(temp4[tt:tt+8],/NAN)
    ENDFOR   
    PRINT, 'Done calculating site 4'
temp_8hr_total = [temp_8hr_total, temp4_8hr]
temp_hourly = [temp_hourly, temp4]
ENDIF

IF (N_ELEMENTS(site5) GT 0) THEN BEGIN
    temp5_8hr = FLTARR(N_ELEMENTS(site5))
    FOR tt = 0, N_ELEMENTS(site5)-9 DO BEGIN
    	temp5_8hr[tt] = MEAN(temp5[tt:tt+8],/NAN)
    ENDFOR   
    PRINT, 'Done calculating site 5'
temp_8hr_total = [temp_8hr_total, temp5_8hr]
temp_hourly = [temp_hourly, temp5]
ENDIF

IF (N_ELEMENTS(site6) GT 0) THEN BEGIN
    temp6_8hr = FLTARR(N_ELEMENTS(site6))
    FOR tt = 0, N_ELEMENTS(site6)-9 DO BEGIN
    	temp6_8hr[tt] = MEAN(temp6[tt:tt+8],/NAN)
    ENDFOR   
    PRINT, 'Done calculating site 6'
temp_8hr_total = [temp_8hr_total, temp6_8hr]
temp_hourly = [temp_hourly, temp6]
ENDIF

IF (N_ELEMENTS(site7) GT 0) THEN BEGIN
    temp7_8hr = FLTARR(N_ELEMENTS(site7))
    FOR tt = 0, N_ELEMENTS(site7)-9 DO BEGIN
    	temp7_8hr[tt] = MEAN(temp7[tt:tt+8],/NAN)
    ENDFOR   
    PRINT, 'Done calculating site 7'
temp_8hr_total = [temp_8hr_total, temp7_8hr]
temp_hourly = [temp_hourly, temp7]
ENDIF

;Return, temp_8hr_total

;Calculate the daily max
;Also return binned o3 concentrations by hour to compute PDFs of the O3 Diurnal Cycle
time_arr = [ ]
date1 = MAKE_DATE(year,07,01,06,00)
date2 = MAKE_DATE(year,12,01,06,00)
end_datestr = STRMID(MAKE_ISO_DATE_STRING(date2),0,4) + STRMID(MAKE_ISO_DATE_STRING(date2),5,2) + $
			 	STRMID(MAKE_ISO_DATE_STRING(date2),8,2) + STRMID(MAKE_ISO_DATE_STRING(date2),11,2) + $
		  	 	STRMID(MAKE_ISO_DATE_STRING(date2),14,2) 

o3_datestr =  STRMID(data.field12,0,4)+STRMID(data.field12,5,2)+STRMID(data.field12,8,2) + $
   				 STRMID(data.field13,0,2)+STRMID(data.field13,3,2) 

dt = 3600
nt = TIME_DIFF(date2,date1)
nt = nt/dt
FOR i = 0, nt - 1 DO BEGIN
	datestr = STRMID(MAKE_ISO_DATE_STRING(date1),0,4) + STRMID(MAKE_ISO_DATE_STRING(date1),5,2) + $
		 STRMID(MAKE_ISO_DATE_STRING(date1),8,2) + STRMID(MAKE_ISO_DATE_STRING(date1),11,2) + $
	  	 STRMID(MAKE_ISO_DATE_STRING(date1),14,2) 
	time_arr = [time_arr, datestr]
	IF (datestr EQ end_datestr) THEN BREAK
	date1 = TIME_INC(date1, dt)
ENDFOR 

temp_daily_max = [] 
temp_datestr = [ ]
FOR i = 0 , nt-1, 24 DO BEGIN
	indices=[]
   	FOR hh=0,23 DO BEGIN
   		io3     = WHERE(o3_datestr EQ time_arr[i+hh], o3count)
   		indices = [indices, io3]
   	ENDFOR
	;Find daily max temp
	temp_daily_max = [temp_daily_max, [MAX(temp_8hr_total[indices],/NAN), $
		MAX(temp_8hr_total[indices],/NAN), MAX(temp_8hr_total[indices],/NAN), $
		MAX(temp_8hr_total[indices],/NAN)]]
	temp_datestr = [temp_datestr, [time_arr[i],time_arr[i+6],time_arr[i+12],time_arr[i+18]]]
ENDFOR


RETURN, {temp_daily_max 		: temp_daily_max, $
				temp_datestr 	: temp_datestr, $
				temp_8hr_total : temp_8hr_total, $
				temp_hourly	: temp_hourly, $
				date            : data.field12, $
				time			: data.field13}
		
END
