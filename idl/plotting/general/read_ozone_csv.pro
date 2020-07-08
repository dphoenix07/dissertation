PRO READ_OZONE_CSV, year, $
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

STOP
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

IF (N_ELEMENTS(site4) GT 0) THEN BEGIN
    ozone4_8hr = FLTARR(N_ELEMENTS(site4))
    FOR tt = 0, N_ELEMENTS(site4)-9 DO BEGIN
    	ozone4_8hr[tt] = MEAN(ozone4[tt:tt+8],/NAN)
    ENDFOR   
    PRINT, 'Done calculating site 4'
ENDIF

IF (N_ELEMENTS(site5) GT 0) THEN BEGIN
    ozone5_8hr = FLTARR(N_ELEMENTS(site5))
    FOR tt = 0, N_ELEMENTS(site5)-9 DO BEGIN
    	ozone5_8hr[tt] = MEAN(ozone5[tt:tt+8],/NAN)
    ENDFOR   
    PRINT, 'Done calculating site 5'
ENDIF

IF (N_ELEMENTS(site6) GT 0) THEN BEGIN
    ozone6_8hr = FLTARR(N_ELEMENTS(site6))
    FOR tt = 0, N_ELEMENTS(site6)-9 DO BEGIN
    	ozone6_8hr[tt] = MEAN(ozone6[tt:tt+8],/NAN)
    ENDFOR   
    PRINT, 'Done calculating site 6'
ENDIF

PLOT,  ozone1_8hr, PSYM = 4, TITLE = year, XRANGE = [0,8600], YRANGE = [0,150]
OPLOT, ozone2_8hr, PSYM = 4, COLOR = COLOR_24('red')
OPLOT, ozone3_8hr, PSYM = 4, COLOR = COLOR_24('darkgreen')
IF (N_ELEMENTS(site4) GT 0) THEN OPLOT, ozone4_8hr, PSYM = 4, COLOR = COLOR_24('blue')
IF (N_ELEMENTS(site5) GT 0) THEN OPLOT, ozone5_8hr, PSYM = 1, COLOR = COLOR_24('darkgreen')
IF (N_ELEMENTS(site6) GT 0) THEN OPLOT, ozone6_8hr, PSYM = 1, COLOR = COLOR_24('blue')
OPLOT, [0,10000], [80,80], COLOR = COLOR_24('black'),THICK=3

imax1 = WHERE(ozone1_8hr GT 80.0)
imax2 = WHERE(ozone2_8hr GT 80.0) 
imax3 = WHERE(ozone3_8hr GT 80.0) 
IF (N_ELEMENTS(site4) GT 0) THEN BEGINimax4 = WHERE(ozone4_8hr GT 80.0) 
IF (N_ELEMENTS(site5) GT 0) THEN BEGINimax5 = WHERE(ozone5_8hr GT 80.0) 
IF (N_ELEMENTS(site6) GT 0) THEN BEGINimax6 = WHERE(ozone6_8hr GT 80.0) 

fname = indir + 'epa_exceeds_' + year + '.txt'
OPENW, lun, fname, /GET_LUN     
FOR i = 0, N_ELEMENTS(imax1)-1 DO BEGIN
	PRINTF, lun, 'At Site 1, EPA exceedances of: ' + STRING(ozone1_8hr[imax1[i]]) + ' at date: ' + $
			STRING(date1[imax1[i]]) + ' and time: ' + STRING(time1[imax1[i]])
ENDFOR

FOR i = 0, N_ELEMENTS(imax2)-1 DO BEGIN
	PRINTF, lun, 'At Site 2, EPA exceedances of: ' + STRING(ozone2_8hr[imax2[i]]) + ' at date: ' + $
			STRING(date2[imax2[i]]) + ' and time: ' + STRING(time2[imax2[i]])
ENDFOR

FOR i = 0, N_ELEMENTS(imax3)-1 DO BEGIN
	PRINTF, lun, 'At Site 3, EPA exceedances of: ' + STRING(ozone3_8hr[imax3[i]]) + ' at date: ' + $
			STRING(date3[imax3[i]]) + ' and time: ' + STRING(time3[imax3[i]])
ENDFOR

IF (N_ELEMENTS(site4) GT 0) THEN BEGIN
    FOR i = 0, N_ELEMENTS(imax4)-1 DO BEGIN
    	PRINTF, lun, 'At Site 4, EPA exceedances of: ' + STRING(ozone4_8hr[imax4[i]]) + ' at date: ' + $
    			STRING(date4[imax4[i]]) + ' and time: ' + STRING(time4[imax4[i]])
    ENDFOR
ENDIF

IF (N_ELEMENTS(site5) GT 0) THEN BEGIN
    FOR i = 0, N_ELEMENTS(imax5)-1 DO BEGIN
    	PRINTF, lun, 'At Site 5, EPA exceedances of: ' + STRING(ozone5_8hr[imax5[i]]) + ' at date: ' + $
    			STRING(date5[imax5[i]]) + ' and time: ' + STRING(time5[imax5[i]])
    ENDFOR
ENDIF

IF (N_ELEMENTS(site6) GT 0) THEN BEGIN
    FOR i = 0, N_ELEMENTS(imax6)-1 DO BEGIN
    	PRINTF, lun, 'At Site 6, EPA exceedances of: ' + STRING(ozone6_8hr[imax6[i]]) + ' at date: ' + $
    			STRING(date6[imax6[i]]) + ' and time: ' + STRING(time6[imax6[i]])
    ENDFOR
ENDIF
FREE_LUN, lun

STOP

END
