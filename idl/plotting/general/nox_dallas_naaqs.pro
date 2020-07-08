FUNCTION NOX_DALLAS_NAAQS, year, $
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

indir = !WRF_DIRECTORY + 'general/precursors/noy_data/'
infile = indir + 'dallas_nox_' + year + '.csv'

headers= ['State Code','County Code','Site Num','Parameter Code','POC','Latitude','Longitude', $
	'Datum','Parameter Name','Date Local','Time Local','Date GMT','Time GMT','Sample Measurement', $
	'Units of Measurement','MDL','Uncertainty','Qualifier','Method Type','Method Code','Method Name', $
	'State Name','County Name','Date of Last Change']

data = READ_CSV(infile);, HEADER = headers)

CASE year OF 
	'1980' : BEGIN
			site1 = WHERE(data.field03 EQ 44)
            nox1 = data.field14[site1]
            date1 = data.field12[site1]
            time1 = data.field13[site1]
			END

	'1981' : BEGIN
			site1 = WHERE(data.field03 EQ 44)
			site2 = WHERE(data.field03 EQ 45)

            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
			END
		
	'1982' : BEGIN
			site1 = WHERE(data.field03 EQ 44)
			site2 = WHERE(data.field03 EQ 45)
			site3 = WHERE(data.field03 EQ 55)

            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
			END

	'1983' : BEGIN
			site1 = WHERE(data.field03 EQ 44)
			site2 = WHERE(data.field03 EQ 45)
			site3 = WHERE(data.field03 EQ 55)

            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
          
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

            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
          
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

            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
          
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

            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
            nox4 = data.field14[site4]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
			END

	'1987' : BEGIN
			site1 = WHERE(data.field03 EQ 45)
			site2 = WHERE(data.field03 EQ 55)
			site3 = WHERE(data.field03 EQ 69)

            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
          
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

            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
          
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

            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
          
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

            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
          
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

            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
          
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

            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
          
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

            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
          
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

            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
          
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

            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
          
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

            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
          
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
			site3 = WHERE(data.field03 EQ 75)
			site4 = WHERE(data.field03 EQ 87)

            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
            nox4 = data.field14[site4]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
			END

	'1998' : BEGIN
			site1 = WHERE(data.field03 EQ 45)
			site2 = WHERE(data.field03 EQ 69)
			site3 = WHERE(data.field03 EQ 75)
			site4 = WHERE(data.field03 EQ 87)

            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
            nox4 = data.field14[site4]
          
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

            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
          
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

            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
			END

	'2001' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
			site2 = WHERE(data.field03 EQ 75)
			site3 = WHERE(data.field03 EQ 87)

            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
			END

	'2002' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
			site2 = WHERE(data.field03 EQ 75)
			site3 = WHERE(data.field03 EQ 87)

            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
			END

	'2003' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
			site2 = WHERE(data.field03 EQ 75)
			site3 = WHERE(data.field03 EQ 87)

            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
			END

	'2004' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
			site2 = WHERE(data.field03 EQ 75)
			site3 = WHERE(data.field03 EQ 87)

            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
			END

	'2005' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
			site2 = WHERE(data.field03 EQ 75)
			site3 = WHERE(data.field03 EQ 87)

            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
			END

	'2006' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
			site2 = WHERE(data.field03 EQ 75)
			site3 = WHERE(data.field03 EQ 87)

            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
          
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
			END

	'2007' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
			site2 = WHERE(data.field03 EQ 75)
			site3 = WHERE(data.field03 EQ 87)

            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
          
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

            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
          
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

            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
          
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

            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
          
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
            
            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
            
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
            
            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
            
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
            
            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
            
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
            site4 = WHERE(data.field03 EQ 1067)
            
            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
            nox4 = data.field14[site4]
            
            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
            END

	'2015' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
            site2 = WHERE(data.field03 EQ 75)
            site3 = WHERE(data.field03 EQ 87)
            site4 = WHERE(data.field03 EQ 1067)
            
            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
            nox4 = data.field14[site4]

            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
			END

	'2016' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
            site2 = WHERE(data.field03 EQ 75)
            site3 = WHERE(data.field03 EQ 87)
            site4 = WHERE(data.field03 EQ 1067)
            
            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
            nox4 = data.field14[site4]

            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
            END

	'2017' : BEGIN
			site1 = WHERE(data.field03 EQ 69)
            site2 = WHERE(data.field03 EQ 75)
            site3 = WHERE(data.field03 EQ 87)
            site4 = WHERE(data.field03 EQ 1067)
            
            nox1 = data.field14[site1]
            nox2 = data.field14[site2]
            nox3 = data.field14[site3]
            nox4 = data.field14[site4]

            date1 = data.field12[site1]
            date2 = data.field12[site2]
            date3 = data.field12[site3]
            date4 = data.field12[site4]
            
            time1 = data.field13[site1]
            time2 = data.field13[site2]
            time3 = data.field13[site3]
            time4 = data.field13[site4]
            END
ENDCASE         

num_hours = [N_ELEMENTS(site1),N_ELEMENTS(site2),N_ELEMENTS(site3)]
nox_hourly = [ ]

nox1_8hr = FLTARR(N_ELEMENTS(site1))
FOR tt = 0, N_ELEMENTS(site1)-9 DO BEGIN
	nox1_8hr[tt] = MEAN(nox1[tt:tt+8],/NAN)
ENDFOR
nox_8hr_total = [nox1_8hr]
nox_hourly = [nox_hourly, nox1]
PRINT, 'Done calculating site 1'

IF (N_ELEMENTS(site2) GT 0) THEN BEGIN
    nox2_8hr = FLTARR(N_ELEMENTS(site2))
    FOR tt = 0, N_ELEMENTS(site2)-9 DO BEGIN
    	nox2_8hr[tt] = MEAN(nox2[tt:tt+8],/NAN)
    ENDFOR
    nox_hourly = [nox_hourly, nox2]
    nox_8hr_total = [nox1_8hr, nox2_8hr]
   
    PRINT, 'Done calculating site 2'
ENDIF

IF (N_ELEMENTS(site3) GT 0) THEN BEGIN
    nox3_8hr = FLTARR(N_ELEMENTS(site3))
    FOR tt = 0, N_ELEMENTS(site3)-9 DO BEGIN
    	nox3_8hr[tt] = MEAN(nox3[tt:tt+8],/NAN)
    ENDFOR
    nox_hourly = [nox_hourly, nox3]
    PRINT, 'Done calculating site 3'
    
    nox_8hr_total = [nox1_8hr, nox2_8hr, nox3_8hr]
ENDIF

IF (N_ELEMENTS(site4) GT 0) THEN BEGIN
    nox4_8hr = FLTARR(N_ELEMENTS(site4))
    FOR tt = 0, N_ELEMENTS(site4)-9 DO BEGIN
    	nox4_8hr[tt] = MEAN(nox4[tt:tt+8],/NAN)
    ENDFOR   
    PRINT, 'Done calculating site 4'
nox_8hr_total = [nox_8hr_total, nox4_8hr]
nox_hourly = [nox_hourly, nox4]
ENDIF

IF (N_ELEMENTS(site5) GT 0) THEN BEGIN
    nox5_8hr = FLTARR(N_ELEMENTS(site5))
    FOR tt = 0, N_ELEMENTS(site5)-9 DO BEGIN
    	nox5_8hr[tt] = MEAN(nox5[tt:tt+8],/NAN)
    ENDFOR   
    PRINT, 'Done calculating site 5'
nox_8hr_total = [nox_8hr_total, nox5_8hr]
nox_hourly = [nox_hourly, nox5]
ENDIF

IF (N_ELEMENTS(site6) GT 0) THEN BEGIN
    nox6_8hr = FLTARR(N_ELEMENTS(site6))
    FOR tt = 0, N_ELEMENTS(site6)-9 DO BEGIN
    	nox6_8hr[tt] = MEAN(nox6[tt:tt+8],/NAN)
    ENDFOR   
    PRINT, 'Done calculating site 6'
nox_8hr_total = [nox_8hr_total, nox6_8hr]
nox_hourly = [nox_hourly, nox6]
ENDIF

;Return, nox_8hr_total

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

nox_daily_max = [] 
nox_datestr = [ ]
FOR i = 0 , nt-1, 24 DO BEGIN
	indices=[]
   	FOR hh=0,23 DO BEGIN
   		io3     = WHERE(o3_datestr EQ time_arr[i+hh], o3count)
   		indices = [indices, io3]
   	ENDFOR
	;Find daily max nox
	nox_daily_max = [nox_daily_max, [MAX(nox_8hr_total[indices],/NAN), $
		MAX(nox_8hr_total[indices],/NAN), MAX(nox_8hr_total[indices],/NAN), $
		MAX(nox_8hr_total[indices],/NAN)]]
	nox_datestr = [nox_datestr, [time_arr[i],time_arr[i+6],time_arr[i+12],time_arr[i+18]]]
ENDFOR


RETURN, {nox_daily_max 		  : nox_daily_max, $
				nox_datestr   : nox_datestr, $
				nox_8hr_total : nox_8hr_total, $
				nox_hourly	  : nox_hourly, $
				date          : data.field12, $
				time		  : data.field13}
		
END
