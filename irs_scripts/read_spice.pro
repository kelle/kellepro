FUNCTION read_spice, file, header

;READS in spect.tbl output from spice

;restore, '/data/hillary/1/kelle/idl/kellepro/irs_scripts/read_spice.dat' ;has spice_template

;READ IN WHOLE THING AS STRING ARRAY
nlines=FILE_LINES(file)
filearr=STRARR(nlines)
OPENR, lun_file, file, /GET_LUN
READF, lun_file, filearr
CLOSE, lun_file
FREE_LUN, lun_file

start=where(strmid(filearr,0,1) eq '|')
data=filearr[start[2]+1:nlines-1]

;FORMAT HEADER
IF ARG_PRESENT(header) THEN BEGIN

   header=filearr[0:start[0]-1]

   ;get rid of leading '\char'
   char=where(strpos(header,'\char ') eq 0) 
   header[char]=STRMID(header[char], 6)

   ;get rid of leading '\int'
   int=where(strpos(header,'\int ') eq 0) 
   header[int]=STRMID(header[int], 5)

   ;get rid of leading '\float'
   flt=where(strpos(header,'\float ') eq 0) 
   header[flt]=STRMID(header[flt], 7)

  ;get rid of leading '\' in \HISTORY
   hist=where(strpos(header,'\HISTORY ') eq 0) 
   header[hist]=STRMID(header[hist], 1)

  ;NEED to add END if write out here.

ENDIF


;FORMAT DATA
npts=n_elements(data)
order=intarr(npts)
w=FLTARR(npts)
flux=w
error=w
bit_flag=order

FOR i = 0, npts-1 DO BEGIN
 tmp = STRSPLIT(data[i],/extract)
 order[i] = FIX(tmp[0])
 w[i] = FLOAT(tmp[1])
 flux[i] = FLOAT(tmp[2])
 error[i] = FLOAT(tmp[3])
 bit_flag[i] = FIX(tmp[4])
ENDFOR

;get rid of bonus
trash=where(order EQ 3)
IF trash[0] NE -1 THEN BEGIN 
 order=order[0:trash[0]-1]
 w=w[0:trash[0]-1]
 flux=flux[0:trash[0]-1]
 error=error[0:trash[0]-1]
 bit_flag=bit_flag[0:trash[0]-1]
ENDIF

size=n_elements(flux)
spec=dblarr(2,size)

spec[0,*]=w
spec[1,*]=flux

plot, w, flux

RETURN, spec

END
