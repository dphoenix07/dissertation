PRO READ_LOG_TABLE, x, logx

COMPILE_OPT IDL2

infile = '~/idl/idl_tutorials/table.txt'	;Input file name

n = FILE_LINES(infile)				;Get number of lines in file

x = FLTARR(n)					;Create array for x values
logx = FLTARR(n)				;Create array for log(x) values

x0 = 0.0					;FLOAT input variable
logx0 = 0.0					;FLOAT input variable

OPENR, iunit, infile, /GET_LUN			;Open input file

FOR i = 0, n-1 DO BEGIN
    READF, iunit, x0, logx0			;Read one line from file
    x[i] = x0					;Store x value
    logx[i] = logx0				;Store log(x) value
ENDFOR

FREE_LUN, iunit					;Close input file

FOR i = 0, n-1 DO PRINT, $
  x[i], logx[i], FORMAT="(2F12.5)"
END

END
