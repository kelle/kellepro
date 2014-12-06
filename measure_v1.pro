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
;       Corrected defn of spectral indices misprinted in K99, April, 2005 
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


pro measure, input_file, output_file

;+
; NAME:
;	MEASURE
;
; 
PURPOSE:
;	This procedure measures spectral indices of IR spectra.  
;	Indices measured are:  TiO 5, Ti0 4, Ti0 3, TiO 2, H-alpha, 
;	CaOH, CaH 1, CaH 2, Cah 3,and  all indices from Table 7 of
;          Kirkpatrick et al. 1999.
;
; CALLING SEQUENCE:
;
;	MEASURE, Input_file, Output_file
;
; INPUTS:
;	Input_file:	File that contains list of files containing spectra 
;			in ASCII format.  The files are assumed to have a 
;			FITS header and then two columns of data as outputted 
;			from IRAF's wspectext routine.
;
;	Output_file:	Name of comma-delimted file as described below.  
;
; OUTPUTS:
;	This procedure returns the spectral indices to a comma-delimted file 
;	with a header row.  Following the header, each row has the object 
;	name (from FITS header) followed by the spectral indices.  
;
; PROCEDURE:
;
;
;
; ROUTINES CALLED:
;	avgflux
;
; EXAMPLE:
;	Please provide a simple example here. An example from the PICKFILE
;	documentation is shown below. Please try to include examples that
;       do not rely on variables or data files that are not defined in
;       the example code. Your example should execute properly if typed
;       in at the IDL command line with no other preparation.
;
;	Create a PICKFILE widget that lets users select only files with 
;	the extensions 'pro' and 'dat'.  Use the 'Select File to Read' title 
;	and store the name of the selected file in the variable F.  Enter:
;
;		F = PICKFILE(/READ, FILTER = ['pro', 'dat'])
;
; MODIFICATION HISTORY:
; 	Written by:	Kelle Cruz, March 2001.
;-

if N_params() lt 2 then begin
     print,"Syntax -  MEASURE_KPNO, 'Input_file, 'Output_file'"
     print,"Input_file should have list of spectra in ASCII format with FITS header."
     print, "Output_file is the name of the file to be created."
     return
endif

w=0D 

files=strarr(200)

unit = 1
openr, unit, input_file
one_file = ' '
WHILE NOT EOF(1) DO BEGIN
	READF, 1, one_file
	files(w) = one_file
	w = w + 1
ENDWHILE
close, unit

files=files(0:w-1)
num_files = n_elements(files)

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

one_line = dblarr(2)
wavelength = dblarr(2500)
flux = dblarr(2500)

FOR y = 0, num_files-1 DO BEGIN

data_file = files(y)
openr, unit, data_file

;Read in FITS header

comment = ' '
blank = ' '
file = ' '

READF, unit, comment
header_check = strmid(comment,0,4)
if header_check eq 'BITP' THEN header=1 else header=0

IF header eq 1 THEN BEGIN

endy = 0
WHILE endy ne 1 DO BEGIN
   READF, unit, comment
   check = strmid(comment,0,4)
   IF check eq 'FILE' THEN file = strmid(comment,11,16)
   IF check eq 'CDEL' THEN pix_scale = float(strmid(comment,10,30))
   IF check eq 'END ' THEN endy = 1
ENDWHILE

IF strmid(file,3,5) eq '.FITS' THEN sobject(y) = strmid(file,0,3)
IF strmid(file,4,5) eq '.FITS' THEN sobject(y) = strmid(file,0,4)
IF strmid(file,5,5) eq '.FITS' THEN sobject(y) = strmid(file,0,5)
IF strmid(file,6,5) eq '.FITS' THEN sobject(y) = strmid(file,0,6)
IF strmid(file,7,5) eq '.FITS' THEN sobject(y) = strmid(file,0,7)
IF strmid(file,8,5) eq '.FITS' THEN sobject(y) = strmid(file,0,8)
IF strmid(file,9,5) eq '.FITS' THEN sobject(y) = strmid(file,0,9)
IF strmid(file,10,5) eq '.FITS' THEN sobject(y) = strmid(file,0,10)

;print, sobject(y)

;Read in blanks + first line of data

WHILE blank eq ' ' DO BEGIN
	READF, unit, comment
	blank = strmid(comment,0,1)
ENDWHILE

ENDIF

IF header eq 0 then sobject(y) = data_file
print, sobject(y)

;Read in data

x=0

WHILE NOT EOF(1) DO BEGIN
	READF, 1, one_line
	wavelength(x) = one_line(0)
	flux(x) = one_line(1)
	x = x + 1
ENDWHILE
close, unit

IF header eq 0 then pix_scale = wavelength(1) - wavelength(0)

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

;Rb-a
startw = 7775.2 & endw = 7785.2
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
num1 = avgflux(startw,endw,pix_scale,wavelength(a),flux(a))

startw = 7815.2 & endw = 7825.2
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
num2 = avgflux(startw,endw,pix_scale,wavelength(a),flux(a))

startw = 7795.2 & endw = 7805.2
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
den = avgflux(startw,endw,pix_scale,wavelength(a),flux(a))

rba(y) = (num1+num2)/2/den
srba(y) = string(rba(y)) + ','
;print, 'rba', rba(y)

;Rb-b
startw = 7922.6 & endw = 7932.6
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
num1 = avgflux(startw,endw,pix_scale,wavelength(a),flux(a))

startw = 7962.6 & endw = 7972.6
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
num2 = avgflux(startw,endw,pix_scale,wavelength(a),flux(a))

startw = 7942.6 & endw = 7952.6
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
den = avgflux(startw,endw,pix_scale,wavelength(a),flux(a))

rbb(y) = (num1+num2)/2/den
srbb(y) = string(rbb(y)) + ','
;print, 'rbb', rbb(y)

;Na-a
startw = 8153.3 & endw = 8163.3
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN num = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE num=0

startw = 8178.3 & endw = 8188.3
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN den = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE den=0

naa(y) = num/den
snaa(y) = string(naa(y)) + ','
;print,'naa', naa(y)

;Na-b
;INCORRECT IN K99 PAPER
startw = 8153.5 & endw = 8163.3
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN num = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE num=0

startw = 8189.8 & endw = 8199.8
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN den = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE den=0

nab(y) = num/den
snab(y) = string(nab(y)) + ','
;print,'nab', nab(y)

;Cs-a
startw = 8496.1 & endw = 8506.1
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN num1 = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE num1=0

startw = 8536.1 & endw = 8546.1
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN num2 = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE num2=0

startw = 8516.1 & endw = 8526.1
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN den = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE den=0

csa(y) = (num1+num2)/2/den
scsa(y) = string(csa(y)) + ','
;print, 'csa', csa(y)

;Cs-b
startw = 8918.5 & endw = 8928.5
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN num1 = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE num1=0

startw = 8958.5 & endw = 8968.5
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN num2 = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE num2=0

startw = 8938.5 & endw = 8948.5
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN den = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE den=0

csb(y) = (num1+num2)/2/den
scsb(y) = string(csb(y)) + ','

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

;TiO-b

startw = 8400. & endw = 8415.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN num = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE num=0

startw = 8455. & endw = 8470.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN den = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE den=0

tiob(y) = num/den
stiob(y) = string(tiob(y)) + ','

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
;sva2(y)=string(voa(y)/2) + ','

;startw = 7350. & endw = 7370. & startw2 = 7550. & endw2 = 7570.
;a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
;b = where(wavelength ge startw2-pix_scale/2 AND wavelength le endw2+pix_scale/2)
;num = avgflux2(startw,endw,startw2, endw2, pix_scale,wavelength(a),flux(a),wavelength(b),flux(b))

;voa_2(y)=num/den
;svoa_2(y)=string(voa_2(y)) + ','

;print, voa(y), voa(y)/2, voa_2(y)

;VO-b

startw = 7860. & endw = 7880.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
num1 = avgflux(startw,endw,pix_scale,wavelength(a),flux(a))

startw = 8080. & endw = 8100.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN num2 = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) else num2=0

startw = 7960. & endw = 8000.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
den = avgflux(startw,endw,pix_scale,wavelength(a),flux(a))

vob(y) = (num1+num2)/den/2
svob(y) = string(vob(y)) + ','

;svb2(y)=string(vob(y)/2) + ','

;CrH-a

startw = 8580. & endw = 8600.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN num = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE num=0

startw = 8621. & endw = 8641.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN den = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE den=0

crha(y) = num/den
scrha(y) = string(crha(y)) + ','

;CrH-b

startw = 9940. & endw = 9960.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN num = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE num =0

startw = 9970. & endw = 9990.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN den = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE den=0

if den eq 0 THEN crhb(y) = 0 else crhb(y) = num/den
scrhb(y) = string(crhb(y)) + ','

;FeH-a

startw = 8660. & endw = 8680.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN num = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE num=0

startw = 8700. & endw = 8720.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN den = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE den=0

feha(y) = num/den
sfeha(y) = string(feha(y)) + ','

;FeH-b

startw = 9863. & endw = 9883.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN num = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE num=0

startw = 9908.0 & endw = 9928.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN den = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE den=0

fehb(y) = num/den
sfehb(y) = string(fehb(y)) + ','

;Color-a

startw = 9800. & endw = 9850.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN num = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE num=0

startw = 7300. & endw = 7350.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN den = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE den=0

ca(y) = num/den
sca(y) = string(ca(y)) + ','

;Color-b

startw = 9800. & endw = 9850.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN num = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE num=0

startw = 7000. & endw = 7050.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN den = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE num=0

cb(y) = num/den
scb(y) = string(cb(y)) + ','

;Color-c

startw = 9800. & endw = 9850.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN num = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE num=0

startw = 8100. & endw = 8150.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN den = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE den=0

cc(y) = num/den
scc(y) = string(cc(y)) + ','

;Color-d

startw = 9675. & endw = 9875.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN num = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE num=0

startw = 7350. & endw = 7550.
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN den = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE den=0

cod(y) = num/den
scd(y) = string(cod(y)) + ','

;PC3 from Martin et al.

startw = 8230 & endw = 8270
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN num = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE num=0

startw = 7540 & endw = 7580
a = where(wavelength ge startw-pix_scale/2 AND wavelength le endw+pix_scale/2)
IF a(0) ne -1 THEN den = avgflux(startw,endw,pix_scale,wavelength(a),flux(a)) ELSE den=0

pc3(y) = num/den
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
	     srba(y)    + srbb(y)  + snaa(y)  +  snab(y) + scsa(y) + $
	     scsb(y)    + stioa(y) + stiob(y) + svoa(y)   + svob(y) +$ 
	     scrha(y)   + scrhb(y) + sfeha(y) + sfehb(y) + sca(y) + $
	     scb(y)     + scc(y)   + scd(y)   + spc3(y)

ENDFOR ;stop looping through files

;create header row for output file

header = 'Object,TiO 5,TiO 4,TiO 3,TiO 2, H-alpha, CaOH, CaH 1, CaH 2, Cah 3, Rb-a, Rb-b, Na-a, Na-b, Cs-a, Cs-b, TiO-a, TiO-b, VO-a, VO-b, CrH-a, CrH-b, FeH-a, FeH-b, Color-a, Color-b,Color-c,Color-d,PC3'
;header = 'Object, VO-a, VO-a2, VO-b, VO-b2'

;Write comma-delimited file

;IF write eq 1 then BEGIN
openw, unit, output_file
PRINTF, 1, header
FOR i=0,num_files-1 DO BEGIN
	dataline = oneline(i)
	PRINTF, 1, dataline
ENDFOR
close, unit
;ENDIF

END
