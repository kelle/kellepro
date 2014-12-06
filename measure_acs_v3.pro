pro measure, input_file, file_key=file_key,verbose=verbose

;+
; NAME:
;	MEASURE
;
; PURPOSE:
;	This procedure measures spectral indices of IR spectra.  
;	Indices measured are:  TiO 5, Ti0 4, Ti0 3, TiO 2, H-alpha, 
;	CaOH, CaH 1, CaH 2, Cah 3,and  all indices from Table 7 of
;          Kirkpatrick et al. 1999.
;
; CALLING SEQUENCE:
;
;	MEASURE, Input_file
;
; INPUTS:
;	Input_file:	File that contains list of FITS files.
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
;	kellepro: avgflux
;       kellepro: kreadspec
;       astropro: readcol
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
; 	Written by:	Kelle Cruz, March 2001. (v1)
;       Major overhaul, Feb 2006
;-

if N_params() lt 1 then begin
     print,"Syntax -  MEASURE, 'Input_file.lis', ( /file,/verbose)"
     print,"Input_file should have .lis extension and contain list of fits files"
     print, "Output_file will be created with '_indices' appended to the root file name."
     return
endif

root='/scr/kelle/optical_spectra/2M_all/fits/'
root='/scr/kelle/optical_spectra/lris/'
;root= '/Users/kelle/Documents/spectra/lris/'


READCOL, input_file,ref,desig,sp_type,spectra_files,$ 
	format='A,A,A,A',/PRESERVE_NULL, comment=';'

;READCOL, input_file,spectra_files, format='A',/PRESERVE_NULL, comment=';'

num_files = n_elements(spectra_files)
output_file=FILE_BASENAME(input_file,'.lis')+'_indices'

OPENW, unit, output_file+'.txt',/get_lun ; open output file for writing

FOR y = 0, num_files-1 DO BEGIN

    data_file = spectra_files[y]
    spec=KREADSPEC(root+data_file,h,/silent)
    wavelength = REFORM(spec[0,*])
    flux = REFORM(spec[1,*])

    object=strtrim(sxpar(h,'OBJECT'),2)
    file_name=(strsplit(data_file,'.',/extract))[0]
    IF keyword_set(file_key) then sobject = file_name else $
              sobject = object

print, ' '
print, sobject

;Measure indices
    i=0
    index=fltarr(39)
    name=strarr(39)

readcol,'indices_optical.lis', ref,index,num1,num2,den1,den2,format='A,A,F,F,F,F',comment='#'
n_indices=n_elements(ref)


FOR i=0,n_indices-1 DO BEGIN

    num = avgflux(num1[i], num2[i], wavelength, flux)
    den = avgflux(den1[i], den2[i], wavelength, flux)

    index[i] = strtrim(string(num/den),1)
    IF KEYWORD_SET(verbose) THEN print, name[i],index[i]

ENDFOR

;--------------------
;PMSU Spectral Ratios
;--------------------

;name[i]='TiO5'
;num = avgflux(7126., 7135, wavelength, flux)
;den = avgflux(7042., 7046, wavelength, flux)
i=++i

name[i]='TiO4'
num = avgflux(7130., 7135., wavelength, flux)
den = avgflux(7115., 7120., wavelength, flux)
index[i] = strtrim(string(num/den),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='TiO3'
num = avgflux(7092., 7097., wavelength, flux)
den = avgflux(7079., 7084., wavelength, flux)
index[i] = strtrim(string(num/den),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

;name[i]='TiO2'
;num = avgflux(7058., 7061., wavelength, flux)
;den = avgflux(7043., 7046., wavelength, flux)
;index[i] = strtrim(string(num/den),1)
;IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
;i=++i

name[i]='Halpha'
num = avgflux(6560., 6566., wavelength, flux)
den = avgflux(6545., 6555., wavelength, flux)
index[i] = strtrim(string(num/den),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i



name[i]='CaH1'
num  = avgflux(6380., 6390., wavelength, flux)
den1 = avgflux(6345., 6355., wavelength, flux)
den2 = avgflux(6410., 6420., wavelength, flux)
index[i] = strtrim(string(num/((den1+den2)/2)),1)
;IF den1 eq -1 THEN cah1 = 0 ELSE cah1 = num/((den1+den2)/2)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='CaH2'
num = avgflux(6814., 6846., wavelength, flux)
den = avgflux(7042., 7046., wavelength, flux)
index[i] = strtrim(string(num/den),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='CaH3'
num = avgflux(6960., 6990., wavelength, flux)
den = avgflux(7042., 7046., wavelength, flux)
index[i]= strtrim(string(num/den),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

;--------------------
;K99 Spectral Ratios
;--------------------

name[i]='Rb-a'
num1 = avgflux(7775.2, 7785.2, wavelength, flux)
num2 = avgflux(7815.2, 7825.2, wavelength, flux)
den  = avgflux(7795.2, 7805.2, wavelength, flux)
index[i]=  strtrim(string((num1+num2)/2/den),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='Rb-b'
num1 = avgflux(7922.6, 7932.6, wavelength, flux)
num2 = avgflux(7962.6, 7972.6, wavelength, flux)
den  = avgflux(7942.6, 7952.6, wavelength, flux)
index[i]=  strtrim(string((num1+num2)/2/den),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='Na-a'
num = avgflux(8153.3, 8163.3, wavelength, flux) 
den = avgflux(8178.3, 8188.3, wavelength, flux)
index[i] = strtrim(string(num/den),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='Na-b'
;INCORRECT IN K99 PAPER
num = avgflux(8153.5, 8163.3, wavelength, flux)
den = avgflux(8189.8, 8199.8, wavelength, flux)
index[i] = strtrim(string(num/den),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='Cs-a'
num1 = avgflux(8496.1, 8506.1, wavelength, flux)
num2 = avgflux(8536.1, 8546.1, wavelength, flux)
den  = avgflux(8516.1, 8526.1, wavelength, flux)
index[i]=  strtrim(string((num1+num2)/2/den),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='Cs-b'
num1 = avgflux(8918.5, 8928.5, wavelength, flux)
num2 = avgflux(8958.5, 8968.5, wavelength, flux)
den  = avgflux(8938.5, 8948.5, wavelength, flux)
index[i]=  strtrim(string((num1+num2)/2/den),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='TiO-a'
num = avgflux(7033., 7048., wavelength, flux)
den = avgflux(7058., 7073., wavelength, flux)
index[i] = strtrim(string(num/den),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='TiO-b'
num = avgflux(8400., 8415., wavelength, flux)
den = avgflux(8455., 8470., wavelength, flux) 
index[i] = strtrim(string(num/den),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='VO-a'
num1 = avgflux(7350., 7370., wavelength, flux)
num2 = avgflux(7550., 7570., wavelength, flux)
den  = avgflux(7430., 7470., wavelength, flux)
index[i] = strtrim(string((num1+num2)/den/2),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='VO-b'
num1 = avgflux(7860., 7880. ,wavelength, flux)
num2 = avgflux(8080., 8100., wavelength, flux) 
den  = avgflux(7960., 8000., wavelength, flux)
index[i] = strtrim(string((num1+num2)/den/2),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='CrH-a'
num = avgflux(8580., 8600., wavelength, flux) 
den = avgflux(8621., 8641., wavelength, flux) 
index[i] = strtrim(string(num/den),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i 

;name[i]='CrH-b'
;num = avgflux(9940., 9960., wavelength, flux)
;den = avgflux(9970., 9990., wavelength, flux)
;index[i] = strtrim(string(num/den),1)
;IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
;i=++i

name[i]='FeH-a'
num = avgflux(8660., 8680., wavelength, flux)
den = avgflux(8700., 8720., wavelength, flux)
index[i] = strtrim(string(num/den),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='FeH-b'
num = avgflux(9863., 9883., wavelength, flux)
den = avgflux(9908.0, 9928., wavelength, flux)
index[i] = strtrim(string(num/den),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='Color-a'
num = avgflux(9800., 9850., wavelength, flux)
den = avgflux(7300., 7350., wavelength, flux)
index[i] = strtrim(string(num/den),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='Color-b'
num = avgflux(9800., 9850., wavelength, flux)
den = avgflux(7000., 7050., wavelength, flux)
index[i] = strtrim(string(num/den),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='Color-c'
num = avgflux(9800., 9850., wavelength, flux)
den = avgflux(8100., 8150., wavelength, flux)
index[i] = strtrim(string(num/den),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='Color-d'
num = avgflux(9675., 9875., wavelength, flux) 
den = avgflux(7350., 7550., wavelength, flux)
index[i] = strtrim(string(num/den),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

;--------------------
;Martin et al 1999 Spectral Ratios
;--------------------

name[i]='VO1'
num = avgflux(7540, 7580, wavelength, flux)
den = avgflux(7420, 7460, wavelength, flux)
index[i] = strtrim(string(num/den),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='VO2'
num = avgflux(7990, 8030, wavelength, flux)
den = avgflux(7900, 7940, wavelength, flux)
index[i] = strtrim(string(num/den),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='PC3' 
num = avgflux(8230, 8270, wavelength, flux)
den = avgflux(7540, 7580, wavelength, flux)
index[i] = strtrim(string(num/den),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

index_name  = name[0:i-1]
index = index[0:i-1]

;Make header and print if it's the first object 
if y eq 0 then begin
    header = 'OBJECT'+ ','+ STRJOIN(name,', ')
    ;header = STRJOIN(name,', ')
    printf, unit, header
    indices=fltarr(num_files,i)
ENDIF

;Format output for comma-delimited file
dataline = sobject +',' + STRJOIN(index,', ')
;dataline = STRJOIN(index,', ')
printf, unit, dataline

indices[y,*]=index


ENDFOR ;stop looping through spectra

save, file=output_file+'.sav',ref,desig,sp_type,spectra_files,indices,index_name

close, unit ;close output file
FREE_LUN,unit

END
