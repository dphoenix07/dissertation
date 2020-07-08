FUNCTION OZONE_NAAQS, year, $
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

indir = !WRF_DIRECTORY + 'general/o3_data/'
infile = indir + 'dallas_' + year + '.csv'

headers= ['State Code','County Code','Site Num','Parameter Code','POC','Latitude','Longitude', $
	'Datum','Parameter Name','Date Local','Time Local','Date GMT','Time GMT','Sample Measurement', $
	'Units of Measurement','MDL','Uncertainty','Qualifier','Method Type','Method Code','Method Name', $
	'State Name','County Name','Date of Last Change']

data = READ_CSV(infile);, HEADER = headers)

CASE year OF 
	'1980' : BEGIN
			site1 = WHERE(data.field03 EQ 44)
			site2 = WHERE(data.field03 EQ 45)
			site3 = WHERE(data.field03 EQ 52)
			site4 = WHERE(data.field03 EQ 1047)

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
            ozone4 = data.field14[site4]*1.0E3
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
			END

	'1981' : BEGIN
			site1 = WHERE(data.field03 EQ 44)
			site2 = WHERE(data.field03 EQ 45)
			site3 = WHERE(data.field03 EQ 52)
			site4 = WHERE(data.field03 EQ 1047)

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
            ozone4 = data.field14[site4]*1.0E3
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
			END
		
	'1982' : BEGIN
			site1 = WHERE(data.field03 EQ 44)
			site2 = WHERE(data.field03 EQ 45)
			site3 = WHERE(data.field03 EQ 52)
			site4 = WHERE(data.field03 EQ 55)

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
            ozone4 = data.field14[site4]*1.0E3
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
			END

	'1983' : BEGIN
			site1 = WHERE(data.field03 EQ 44)
			site2 = WHERE(data.field03 EQ 45)
			site3 = WHERE(data.field03 EQ 52)
			site4 = WHERE(data.field03 EQ 55)

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
            ozone4 = data.field14[site4]*1.0E3
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
			END

	'1984' : BEGIN
			site1 = WHERE(data.field03 EQ 44)
			site2 = WHERE(data.field03 EQ 45)
			site3 = WHERE(data.field03 EQ 52)
			site4 = WHERE(data.field03 EQ 55)

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
            ozone4 = data.field14[site4]*1.0E3
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
			END

	'1985' : BEGIN
			site1 = WHERE(data.field03 EQ 44)
			site2 = WHERE(data.field03 EQ 45)
			site3 = WHERE(data.field03 EQ 52)
			site4 = WHERE(data.field03 EQ 55)

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
            ozone4 = data.field14[site4]*1.0E3
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
			END
		
	'1986' : BEGIN
			site1 = WHERE(data.field03 EQ 44)
			site2 = WHERE(data.field03 EQ 45)
			site3 = WHERE(data.field03 EQ 52)
			site4 = WHERE(data.field03 EQ 55)
			site5 = WHERE(data.field03 EQ 69)
			site6 = WHERE(data.field03 EQ 86)

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
            ozone4 = data.field14[site4]*1.0E3
            ozone5 = data.field14[site5]*1.0E3
            ozone6 = data.field14[site6]*1.0E3
          
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

	'1987' : BEGIN
			site1 = WHERE(data.field03 EQ 45)
			site2 = WHERE(data.field03 EQ 52)
			site3 = WHERE(data.field03 EQ 55)
			site4 = WHERE(data.field03 EQ 69)

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
            ozone4 = data.field14[site4]*1.0E3
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
			END

	'1988' : BEGIN
			site1 = WHERE(data.field03 EQ 45)
			site2 = WHERE(data.field03 EQ 55)
			site3 = WHERE(data.field03 EQ 69)

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
          
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

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
          
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

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
          
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

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
          
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

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
          
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

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
			END

	'1994' : BEGIN
			site1 = WHERE(data.field03 EQ 45)
			site2 = WHERE(data.field03 EQ 55)
			site3 = WHERE(data.field03 EQ 69)

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
			END

	'1995' : BEGIN
			site1 = WHERE(data.field03 EQ 45)
			site2 = WHERE(data.field03 EQ 69)
			site3 = WHERE(data.field03 EQ 87)

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
			END

	'1996' : BEGIN
			site1 = WHERE(data.field03 EQ 45)
			site2 = WHERE(data.field03 EQ 69)
			site3 = WHERE(data.field03 EQ 87)

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
			END

	'1997' : BEGIN
			site1 = WHERE(data.field03 EQ 45)
			site2 = WHERE(data.field03 EQ 69)
			site3 = WHERE(data.field03 EQ 87)

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
			END

	'1998' : BEGIN
			site1 = WHERE(data.field03 EQ 45)
			site2 = WHERE(data.field03 EQ 69)
			site3 = WHERE(data.field03 EQ 75)
			site4 = WHERE(data.field03 EQ 87)

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
            ozone4 = data.field14[site4]*1.0E3
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
			END

	'1999' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
			site2 = WHERE(data.field03 EQ 75)
			site3 = WHERE(data.field03 EQ 87)

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
			END

	'2000' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
			site2 = WHERE(data.field03 EQ 75)
			site3 = WHERE(data.field03 EQ 87)
			site4 = WHERE(data.field03 EQ 3003)

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
            ozone4 = data.field14[site4]*1.0E3
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
			END

	'2001' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
			site2 = WHERE(data.field03 EQ 75)
			site3 = WHERE(data.field03 EQ 87)
			site4 = WHERE(data.field03 EQ 3003)

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
            ozone4 = data.field14[site4]*1.0E3
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
			END

	'2002' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
			site2 = WHERE(data.field03 EQ 75)
			site3 = WHERE(data.field03 EQ 87)
			site4 = WHERE(data.field03 EQ 3003)

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
            ozone4 = data.field14[site4]*1.0E3
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
			END

	'2003' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
			site2 = WHERE(data.field03 EQ 75)
			site3 = WHERE(data.field03 EQ 87)
			site4 = WHERE(data.field03 EQ 3003)

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
            ozone4 = data.field14[site4]*1.0E3
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
			END

	'2004' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
			site2 = WHERE(data.field03 EQ 75)
			site3 = WHERE(data.field03 EQ 87)
			site4 = WHERE(data.field03 EQ 3003)

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
            ozone4 = data.field14[site4]*1.0E3
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
			END

	'2005' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
			site2 = WHERE(data.field03 EQ 75)
			site3 = WHERE(data.field03 EQ 87)
			site4 = WHERE(data.field03 EQ 3003)

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
            ozone4 = data.field14[site4]*1.0E3
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
			END

	'2006' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
			site2 = WHERE(data.field03 EQ 75)
			site3 = WHERE(data.field03 EQ 87)
			site4 = WHERE(data.field03 EQ 3003)

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
            ozone4 = data.field14[site4]*1.0E3
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
			END

	'2007' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
			site2 = WHERE(data.field03 EQ 75)
			site3 = WHERE(data.field03 EQ 87)

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
			END

	'2008' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
			site2 = WHERE(data.field03 EQ 75)
			site3 = WHERE(data.field03 EQ 87)

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
			END

	'2009' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
			site2 = WHERE(data.field03 EQ 75)
			site3 = WHERE(data.field03 EQ 87)

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
			END

	'2010' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
			site2 = WHERE(data.field03 EQ 75)
			site3 = WHERE(data.field03 EQ 87)

            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
			END
		
	'2011' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
            site2 = WHERE(data.field03 EQ 75)
            site3 = WHERE(data.field03 EQ 87)
            
            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
            
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            END

	'2012' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
            site2 = WHERE(data.field03 EQ 75)
            site3 = WHERE(data.field03 EQ 87)
            
            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
            
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            END

	'2013' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
            site2 = WHERE(data.field03 EQ 75)
            site3 = WHERE(data.field03 EQ 87)
            
            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
            
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            END

	'2014' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
            site2 = WHERE(data.field03 EQ 75)
            site3 = WHERE(data.field03 EQ 87)
            
            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
            
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            END

	'2015' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
            site2 = WHERE(data.field03 EQ 75)
            site3 = WHERE(data.field03 EQ 87)
            
            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
            
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            END

	'2016' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
            site2 = WHERE(data.field03 EQ 75)
            site3 = WHERE(data.field03 EQ 87)
            
            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
            
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            END

	'2017' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
            site2 = WHERE(data.field03 EQ 75)
            site3 = WHERE(data.field03 EQ 87)
            
            ozone1 = data.field14[site1]*1.0E3
            ozone2 = data.field14[site2]*1.0E3
            ozone3 = data.field14[site3]*1.0E3
            
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            END

ENDCASE         

num_hours = [N_ELEMENTS(site1),N_ELEMENTS(site2),N_ELEMENTS(site3)]

ozone1_8hr = FLTARR(N_ELEMENTS(site1))
FOR tt = 0, N_ELEMENTS(site1)-9 DO BEGIN
	ozone1_8hr[tt] = MEAN(ozone1[tt:tt+8],/NAN)
ENDFOR

PRINT, 'Done calculating site 1'

ozone2_8hr = FLTARR(N_ELEMENTS(site2))
FOR tt = 0, N_ELEMENTS(site2)-9 DO BEGIN
	ozone2_8hr[tt] = MEAN(ozone2[tt:tt+8],/NAN)
ENDFOR

PRINT, 'Done calculating site 2'

ozone3_8hr = FLTARR(N_ELEMENTS(site3))
FOR tt = 0, N_ELEMENTS(site3)-9 DO BEGIN
	ozone3_8hr[tt] = MEAN(ozone3[tt:tt+8],/NAN)
ENDFOR
PRINT, 'Done calculating site 3'

ozone_8hr_total = [ozone1_8hr, ozone2_8hr, ozone3_8hr]

IF (N_ELEMENTS(site4) GT 0) THEN BEGIN
    ozone4_8hr = FLTARR(N_ELEMENTS(site4))
    FOR tt = 0, N_ELEMENTS(site4)-9 DO BEGIN
    	ozone4_8hr[tt] = MEAN(ozone4[tt:tt+8],/NAN)
    ENDFOR   
    PRINT, 'Done calculating site 4'
ozone_8hr_total = [ozone_8hr_total, ozone4_8hr]
ENDIF

IF (N_ELEMENTS(site5) GT 0) THEN BEGIN
    ozone5_8hr = FLTARR(N_ELEMENTS(site5))
    FOR tt = 0, N_ELEMENTS(site5)-9 DO BEGIN
    	ozone5_8hr[tt] = MEAN(ozone5[tt:tt+8],/NAN)
    ENDFOR   
    PRINT, 'Done calculating site 5'
ozone_8hr_total = [ozone_8hr_total, ozone5_8hr]
ENDIF

IF (N_ELEMENTS(site6) GT 0) THEN BEGIN
    ozone6_8hr = FLTARR(N_ELEMENTS(site6))
    FOR tt = 0, N_ELEMENTS(site6)-9 DO BEGIN
    	ozone6_8hr[tt] = MEAN(ozone6[tt:tt+8],/NAN)
    ENDFOR   
    PRINT, 'Done calculating site 6'
ozone_8hr_total = [ozone_8hr_total, ozone6_8hr]
ENDIF

RETURN, ozone_8hr_total 
		
END
