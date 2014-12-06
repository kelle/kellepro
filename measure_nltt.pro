FUNCTION avgflux, startw, endw, pix_scale,wavelength, flux
;+
; NAME:
;	AVGFLUX
;
; PURPOSE:
;	This function returns the average flux in a region of a spectra. 
;
; CALLING SEQUENCE:
;
;	Result = AVGFLUX(Startw, Endw, Wavelength, Flux)
;
; INPUTS:
;	Startw:	The wavelength corresponding to the beginning of the region 
;		over which the flux is to be measured.
;
;	Endw:	The wavelength corresponding to the end of the region 
;		over which the flux is to be measured.
;
;	Wavelength:	An array containing the wavelength values
;			 corresponding to the fluxes given in Flux.
;
;	Flux:	An array containing the flux values corresponding to the 
;		wavelengths given in Wavelength.
;
; OUTPUTS:
;	This function returns the average flux in the region as a 
;	double-precison real number.
;
; PROCEDURE:
;	The fractional weight is found for the end pixels.  This fraction 
;	is used for determining the flux to use from that pixel and to 
;	determine the total number of pixels.  The average flux is found 
;	by summing over the fluxes and dividing by the fractional number 
;	of pixels.
;
; MODIFICATION HISTORY:
; 	Written by:	Kelle Cruz, March 2001
;-

num_pixels = n_elements(wavelength)
first = 0
last = num_pixels - 1

;determine the fractional pixel value for the pixels on the edge of the region

frac1 = (wavelength(first) + pix_scale/2 - startw) / pix_scale
frac2 = (endw - (wavelength(last) - pix_scale/2))  / pix_scale

;sum flux

sumflux = 0

FOR i = first, last DO BEGIN
	IF i eq first THEN pixflux = frac1*flux(i) $
	ELSE IF i eq last THEN pixflux = frac2*flux(i) $
	ELSE pixflux = flux(i)
	sumflux = sumflux + pixflux
ENDFOR

realpix = num_pixels - 2 + frac1 + frac2   ;fracional pixel value
avgflux = sumflux/realpix

return, avgflux

END

pro measure_nltt, input_file, output_file,file=file
;VERSION 2

;+
; NAME:
;	MEASURE
;
; PURPOSE:
;	This procedure measures spectral indices of far-red spectra.  
;	Indices measured are:  TiO 5, Ti0 4, Ti0 3, TiO 2, H-alpha, 
;	CaOH, CaH 1, CaH 2, Cah 3, VO-a, TiO-a, and PC3
;
; CALLING SEQUENCE:
;
;	MEASURE, Input_file, Output_file
;
; INPUTS:
;	Input_file:	File that contains list of files containing spectra 
;			in ASCII format.  The files are expected to be FITS 
;			file of either 1 or 4 dimensions
;	Output_file:	Name of comma-delimted file as described below.  
;
; KEYWORD PARAMETERS:
;	FILE:	Set this keyword to use the file name listed in the Input_file
;		as the object name in the Output_file rather than the OBJECT
;		name found in the FITS header.
;
; OUTPUTS:
;	This procedure returns the spectral indices to a comma-delimted file 
;	with a header row.  Following the header, each row has the object 
;	name (from FITS header or file name) followed by the spectral indices.
;
; PROCEDURE:
;
;
;
; ROUTINES CALLED:
;	avgflux
;
; MODIFICATION HISTORY:
; 	Written by:	Kelle Cruz, March 2001.
;       March, 2004     Added FILE keyword
;       May, 2005	v2: Adapted to read fits files instead of ascii
;-

if N_params() lt 2 then begin
     print,"Syntax -  MEASURE_NLTT, 'Input_file, 'Output_file' (, /FILE)"
     print,"Input_file should be ascii file with list of filenames of FITS spectra."
     print, "Output_file is the name of the file to be created."
     print, "Use /FILE keyword to use the filename instead of OBJECT header keyword."
     return
endif

num_files=file_lines(input_file)

sobject = strarr(num_files)
oneline = strarr(num_files)
stio5 = strarr(num_files)
stio4 = strarr(num_files)
stio3 = strarr(num_files)      
stio2 = strarr(num_files)
sha = strarr(num_files)
scaoh = strarr(num_files)
scah1 = strarr(num_files)
scah2 = strarr(num_files)
scah3 = strarr(num_files)
srba = strarr(num_files)
srbb = strarr(num_files)
snaa = strarr(num_files)
snab = strarr(num_files)
scsa= strarr(num_files)
scsb= strarr(num_files)
stioa = strarr(num_files)
stiob = strarr(num_files)
svoa = strarr(num_files)
svob = strarr(num_files)
sva2 = strarr(num_files)
svb2 = strarr(num_files)
svoa_2 = strarr(num_files)
scrha = strarr(num_files)
scrhb = strarr(num_files)
sfeha = strarr(num_files)
sfehb = strarr(num_files)
sca = strarr(num_files)
scb = strarr(num_files)
scc = strarr(num_files)
scd = strarr(num_files)
spc3 = strarr(num_files)

tio5 = dblarr(num_files)
tio4 = dblarr(num_files)
tio3 = dblarr(num_files)   
tio2 = dblarr(num_files)
ha = dblarr(num_files)
caoh = dblarr(num_files)
cah1 = dblarr(num_files)
cah2 = dblarr(num_files)
cah3 = dblarr(num_files)
rba = dblarr(num_files)
rbb = dblarr(num_files)
naa = dblarr(num_files)
nab = dblarr(num_files)
csa = dblarr(num_files)
csb = dblarr(num_files)
tioa = dblarr(num_files)
tiob = dblarr(num_files)
voa = dblarr(num_files)
vob = dblarr(num_files)
va2 = dblarr(num_files)
vb2= dblarr(num_files)
voa_2= dblarr(num_files)
crha = dblarr(num_files)
crhb = dblarr(num_files)
feha = dblarr(num_files)
fehb = dblarr(num_files)
ca = dblarr(num_files)
cb = dblarr(num_files)
cc = dblarr(num_files)
cod = dblarr(num_files)
pc3 = dblarr(num_files)

y=0d
one_file = ' '
openr, input_lun, input_file, /GET_LUN

WHILE NOT EOF(input_lun) DO BEGIN
 READF, input_lun, one_file
 data_file=one_file

 extn=(strsplit(data_file,'.',/extract))[1]
 file_name=(strsplit(data_file,'.',/extract))[0]

 IF extn EQ 'txt' THEN BEGIN
   object=file_name
   data=READSPEC_TXT(data_file,/silent)
 ENDIF                           ;if file type .txt
 IF extn EQ 'fits' THEN BEGIN

   data=KREADSPEC(data_file,h,/silent)
   pix_scale=SXPAR(h,'CD1_1')

   IF keyword_set(file) then object = file_name else $
     object = strtrim(sxpar(h,'OBJECT'),2)
   
 ENDIF ;if file type .fits

IF keyword_set(file) THEN sobject(y) = file_name ELSE sobject(y) = object
print, sobject(y)

flux=data[1,*]
wavelength=data[0,*]

IF extn EQ 'txt' THEN pix_scale=wavelength[1]-wavelength[0]

;file_name=strsplit(data_file,'.fits',/EXTRACT,/REGEX)
;object_data=readfits(data_file,h,/SILENT)
;if size(object_data,/N_dimensions) eq 3 then flux = object_data[*,0,0] else flux = object_data
;wavelength=findgen(n_elements(flux))*SXPAR(h,'CD1_1')+SXPAR(h,'CRVAL1')
;pix_scale= SXPAR(h,'CD1_1')
;if keyword_set(file_set) then sobject(y)=file_name(0)

;Measure indices

;TiO 5

startw = 7126. & endw = 7135.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
num = avgflux(startw,endw,pix_scale,wavelength(a),flux(a))

startw = 7042. & endw = 7046.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
den = avgflux(startw,endw,pix_scale,wavelength(a),flux(a))

tio5(y) = num/den
stio5(y) = string(tio5(y)) + ','
;print, 'tio5', tio5(y)

;TiO 4

startw = 7130. & endw = 7135.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
num = avgflux(startw,endw,pix_scale,wavelength(a),flux(a))

startw = 7115. & endw = 7120.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
den = avgflux(startw,endw,pix_scale,wavelength(a),flux(a))

tio4(y) = num/den
stio4(y) = string(tio4(y)) + ','
;print, 'tio4', tio4(y)
;TiO 3

startw = 7092. & endw = 7097.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
num = avgflux(startw,endw,pix_scale,wavelength(a),flux(a))

startw = 7079. & endw = 7084.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
den = avgflux(startw,endw,pix_scale,wavelength(a),flux(a))

tio3(y) = num/den
stio3(y) = string(tio3(y)) + ','

;TiO 2

startw = 7058. & endw = 7061.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
num = avgflux(startw,endw,pix_scale,wavelength(a),flux(a))

startw = 7043. & endw = 7046.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
den = avgflux(startw,endw,pix_scale,wavelength(a),flux(a))

tio2(y) = num/den
stio2(y) = string(tio2(y)) + ','


;H alpha

startw = 6560. & endw = 6566.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
num = avgflux(startw,endw,pix_scale,wavelength(a),flux(a))

startw = 6545. & endw = 6555.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
den = avgflux(startw,endw,pix_scale,wavelength(a),flux(a))

ha(y) = num/den
sha(y) = string(ha(y)) + ','

;CaOH

startw = 6230. & endw = 6240.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN num = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE num=0

startw = 6345. & endw = 6354.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN den = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE den=-1

;caoh(y) = num/den
IF den eq -1 THEN caoh(y)=0 ELSE caoh(y) = num/den
scaoh(y) = string(caoh(y)) + ','

;CaH 1

startw = 6380. & endw = 6390.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
num = avgflux(startw,endw,pix_scale,wavelength(a),flux(a))

startw = 6345. & endw = 6355.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN den1 = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE den1=-1

startw = 6410. & endw = 6420.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
den2 = avgflux(startw,endw,pix_scale,wavelength(a),flux(a))

;cah1(y) = num/((den1+den2)/2)
IF den1 eq -1 THEN cah1(y) = 0 ELSE cah1(y) = num/((den1+den2)/2)
scah1(y) = string(cah1(y)) + ','

;CaH 2

startw = 6814. & endw = 6846.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
num = avgflux(startw,endw,pix_scale,wavelength(a),flux(a))

startw = 7042. & endw = 7046.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
den = avgflux(startw,endw,pix_scale,wavelength(a),flux(a))

cah2(y) = num/den
scah2(y) = string(cah2(y)) + ','

;CaH 3

startw = 6960. & endw = 6990.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
num = avgflux(startw,endw,pix_scale,wavelength(a),flux(a))

startw = 7042. & endw = 7046.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
den = avgflux(startw,endw,pix_scale,wavelength(a),flux(a))

cah3(y) = num/den
scah3(y) = string(cah3(y)) + ','
;print, 'cah3', cah3(y)

;--------------------
;K99 Spectral Ratios

;TiO-a

startw = 7033. & endw = 7048.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
num = avgflux(startw,endw,pix_scale,wavelength(a),flux(a))

startw = 7058. & endw = 7073.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
den = avgflux(startw,endw,pix_scale,wavelength(a),flux(a))

tioa(y) = num/den
stioa(y) = string(tioa(y)) + ','
;print, 'tioa', tioa(y)

;VO-a

startw = 7350. & endw = 7370.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
num1 = avgflux(startw,endw,pix_scale,wavelength(a),flux(a))

startw = 7550. & endw = 7570.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
num2 = avgflux(startw,endw,pix_scale,wavelength(a),flux(a))

startw = 7430. & endw = 7470.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
den = avgflux(startw,endw,pix_scale,wavelength(a),flux(a))

voa(y) = (num1+num2)/den/2
svoa(y) = string(voa(y)) + ','

;PC3 from Martin et al.

startw = 8230 & endw = 8270
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN num = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE num=0

startw = 7540 & endw = 7580
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN den = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE den=0

IF den EQ 0 THEN pc3(y) = 0 ELSE pc3(y) = num/den
spc3(y) = string(pc3(y)) 
;print, 'pc3', pc3(y)

;check to make sense.

;print, sobject(y), tio5(y), voa(y), crha(y)
;crha(y), rbb(y)/tiob(y), csa(y)/vob(y), cod(y)
;oneline(y)=sobject(y) +',' + svoa(y) + sva2(y) + svob(y) + svb2(y)

;Format output for comma-delimited file

oneline(y) = sobject(y) +',' + stio5(y) + stio4(y) + stio3(y) + $
	     stio2(y)   + sha(y)   + scaoh(y) + scah1(y) + $
	     scah2(y)   + scah3(y) + $
	     stioa(y)   + svoa(y) + spc3(y)
y=y+1
ENDWHILE ;stop looping through files
close, input_lun
FREE_LUN,input_lun

;create header row for output file

header = 'Object,TiO 5,TiO 4,TiO 3,TiO 2, H-alpha, CaOH, CaH 1, CaH 2, Cah 3, TiO-a, VO-a, PC3'

;Write comma-delimited file

openw, lun_output, output_file,/GET_LUN
PRINTF, lun_output, header
FOR i=0,num_files-1 DO BEGIN
	dataline = oneline(i)
	PRINTF, lun_output, dataline
ENDFOR
close, lun_output
FREE_LUN, lun_output


END
