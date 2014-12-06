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
;	Input_file:	File that contains list of FITS files. One per line.
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
;       indices_optical.lis
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
;       Loop over simple indices, Anne Sweet, Dec 2007
;-

if N_params() lt 1 then begin
     print,"Syntax -  MEASURE, 'Input_file.lis', ( /file,/verbose)"
     print,"Input_file should have .lis extension and contain list of fits files"
     print, "Output_file will be created with '_indices' appended to the root file name."
     return
endif

;root='/scr7/asweet/reducedata/alldata/'
root='/scr/kelle/optical_spectra/2M_all/fits/'
;root='/scr/kelle/optical_spectra/lris/'
;root= '/Users/kelle/Documents/spectra/lris/'


READCOL, input_file,ref,desig,sp_type,spectra_files,key,$ 
	format='F,A,F,A,I',/preserve_null, comment='#'

;READCOL, input_file,spectra_files, format='A',/PRESERVE_NULL, comment=';'

num_files = n_elements(spectra_files)
output_file=FILE_BASENAME(input_file,'.lis')+'_indices'

OPENW, unit, output_file+'.txt',/get_lun ; open output file for writing

readcol,'/scr2/kelle/idl/kellepro/indices_optical.lis', index_ref,index_name,numer1,numer2,denom1,denom2,format='A,A,F,F,F,F',comment='#'
n_indices=n_elements(index_ref)

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
    index=fltarr(n_indices+7)

readcol,'/scr2/kelle/idl/kellepro/indices_optical.lis', index_ref,index_name,numer1,numer2,denom1,denom2,format='A,A,F,F,F,F',comment='#'
n_indices=n_elements(index_ref)

FOR i=0,n_indices-1 DO BEGIN

    num = avgflux(numer1[i], numer2[i], wavelength, flux)
    den = avgflux(denom1[i], denom2[i], wavelength, flux)

    index[i] = strtrim(string(num/den),1)
    IF KEYWORD_SET(verbose) THEN print,index_name[i],index[i]

ENDFOR

new_names=strarr(7)
index_name=[index_name,new_names]

;--------------------
;PMSU Spectral Ratios
;--------------------


index_name[i]='CaH1'
num  = avgflux(6380., 6390., wavelength, flux)
den1 = avgflux(6345., 6355., wavelength, flux)
den2 = avgflux(6410., 6420., wavelength, flux)
index[i] = strtrim(string(num/((den1+den2)/2)),1)
;IF den1 eq -1 THEN cah1 = 0 ELSE cah1 = num/((den1+den2)/2)
IF KEYWORD_SET(verbose) THEN print, index_name[i],index[i]
i=++i

;--------------------
;K99 Spectral Ratios
;--------------------

index_name[i]='Rb-a'
num1 = avgflux(7775.2, 7785.2, wavelength, flux)
num2 = avgflux(7815.2, 7825.2, wavelength, flux)
den  = avgflux(7795.2, 7805.2, wavelength, flux)
index[i]=  strtrim(string((num1+num2)/2/den),1)
IF KEYWORD_SET(verbose) THEN print, index_name[i],index[i]
i=++i

index_name[i]='Rb-b'
num1 = avgflux(7922.6, 7932.6, wavelength, flux)
num2 = avgflux(7962.6, 7972.6, wavelength, flux)
den  = avgflux(7942.6, 7952.6, wavelength, flux)
index[i]=  strtrim(string((num1+num2)/2/den),1)
IF KEYWORD_SET(verbose) THEN print, index_name[i],index[i]
i=++i

index_name[i]='Cs-a'
num1 = avgflux(8496.1, 8506.1, wavelength, flux)
num2 = avgflux(8536.1, 8546.1, wavelength, flux)
den  = avgflux(8516.1, 8526.1, wavelength, flux)
index[i]=  strtrim(string((num1+num2)/2/den),1)
IF KEYWORD_SET(verbose) THEN print, index_name[i],index[i]
i=++i

index_name[i]='Cs-b'
num1 = avgflux(8918.5, 8928.5, wavelength, flux)
num2 = avgflux(8958.5, 8968.5, wavelength, flux)
den  = avgflux(8938.5, 8948.5, wavelength, flux)
index[i]=  strtrim(string((num1+num2)/2/den),1)
IF KEYWORD_SET(verbose) THEN print, index_name[i],index[i]
i=++i

index_name[i]='VO-a'
num1 = avgflux(7350., 7370., wavelength, flux)
num2 = avgflux(7550., 7570., wavelength, flux)
den  = avgflux(7430., 7470., wavelength, flux)
index[i] = strtrim(string((num1+num2)/den/2),1)
IF KEYWORD_SET(verbose) THEN print, index_name[i],index[i]
i=++i

index_name[i]='VO-b'
num1 = avgflux(7860., 7880. ,wavelength, flux)
num2 = avgflux(8080., 8100., wavelength, flux) 
den  = avgflux(7960., 8000., wavelength, flux)
index[i] = strtrim(string((num1+num2)/den/2),1)
IF KEYWORD_SET(verbose) THEN print, index_name[i],index[i]
i=++i


;Make header and print if it's the first object 
if y eq 0 then begin
    header = 'OBJECT'+ ','+ STRJOIN(index_name,', ')
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

save, file=output_file+'.sav',ref,desig,sp_type,spectra_files,indices,index_name,key

close, unit ;close output file
FREE_LUN,unit

END
