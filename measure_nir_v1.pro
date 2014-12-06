pro measure_nir, file_key=file_key,verbose=verbose

;+
; NAME:
;	MEASURE
;
; PURPOSE:
;	This procedure measures spectral indices of NIR spectra.  
;	Indices measured are: blah and are from PAPERS
;
;
; CALLING SEQUENCE:
;
;	MEASURE, [,/file, /verbose]
;
; INPUTS:
;	
;
; OUTPUTS:
;	This procedure returns the spectral indices to a comma-delimted file 
;	with a header row.  Following the header, each row has the object 
;	name (from FITS header) followed by the spectral indices.  
;
;       Also outputs SAVE file
;
; PROCEDURE:
;
;
;
; ROUTINES CALLED:
;	avgflux, ratio
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
;       Modified measure.pro to measure NIR data.
;       Kelle Cruz, March 2007
;-

;if N_params() lt 1 then begin
;     print,"Syntax -  MEASURE_NIR, 'Input_file, 'Output_file'"
;     print,"Input_file should have list of fits files"
;     print, "Output_file is the name of the file to be created."
;     return
;endif

FOR data_set = 0,4 do begin

CASE data_set of 
    0: begin
        data_root='/data/hillary/3/kelle/nir_spectra/2M_all/fits/'
        root='/data/hillary/1/kelle/young/indices/'
        input_file='young_nir_spectra.lis'
    end
    1: begin
        data_root='/data/hillary/3/kelle/nir_spectra/standards/McLean/'
        root='/data/hillary/1/kelle/young/indices/standards/'
        input_file='mclean_stds.lis'
    end
    2:begin 
        data_root='/data/hillary/3/kelle/nir_spectra/standards/Leggett/'
        root='/data/hillary/1/kelle/young/indices/standards/'
        input_file='leggett_stds.lis'
    end
    3:begin 
        data_root='/data/hillary/3/kelle/nir_spectra/standards/giants/'
        root='/data/hillary/1/kelle/young/indices/standards/'
        input_file='giants_nir.lis'
    end
    4:begin 
        data_root='/data/hillary/3/kelle/nir_spectra/2M_all/fits/'
        root='/data/hillary/1/kelle/young/indices/standards/'
        input_file='calibrators_nir.lis'
    end
ENDCASE

print, ''
print, input_file
READCOL, root+input_file,ref,desig,sp_type,spectra_files,$ 
	format='A,A,A,A',/PRESERVE_NULL, comment=';'

num_files = n_elements(spectra_files)
output_file=root+FILE_BASENAME(input_file,'.lis')+'_indices'

OPENW, unit, output_file+'.txt',/get_lun ; open output file for writing

sobject=strarr(num_files)

FOR y = 0, num_files-1 DO BEGIN

    data_file = spectra_files[y]

    ext=(strsplit(spectra_files[y],'.',/extract))[1]
    IF ext eq 'fits' then begin
        spec=READFITS(data_root+data_file,h,/silent)
        object=strtrim(sxpar(h,'OBJECT'),2)        
        wavelength = REFORM(spec[*,0])
        flux = REFORM(spec[*,1])
    ENDIF

    IF ext eq 'dat' or ext eq 'txt' THEN BEGIN
        file_key =1             ;use filename as object name
        RDFLOAT,data_root+data_file,wavelength,flux,skipline=6,silent=~keyword_set(verbose)
    ENDIF


    file_name=(strsplit(data_file,'.',/extract))[0]
    IF keyword_set(file_key) then sobject[y] = file_name else $
              sobject[y] = object


IF KEYWORD_SET(verbose) THEN begin
    print, ' '
    print, sobject[y]
endif

;Measure indices
    i=0
    index=fltarr(39)
    name=strarr(39)

;--------------------
;GEBALLE 2002
;--------------------

name[i]='Continuum'
index[i] = strtrim(string(ratio(1.04, 1.05,0.875, 0.885, wavelength, flux, verbose=verbose)),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='H2O_1.2'
index[i] = strtrim(string(ratio(1.26, 1.29,1.13, 1.16, wavelength, flux, verbose=verbose)),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='H2O_1.5G'
index[i] = strtrim(string(ratio(1.57, 1.59,1.46, 1.48, wavelength, flux, verbose=verbose)),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='CH_1.6'
;num = avgflux(1.56, 1.60, wavelength, flux, verbose=verbose)
;den = avgflux(1.635, 1.675, wavelength, flux, verbose=verbose)
;index[i] = strtrim(string(num/den),1)
index[i] = strtrim(string(ratio(1.56, 1.60,1.635, 1.675, wavelength, flux, verbose=verbose)),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='H2O_2.0'
;num = avgflux(2.09, 2.11, wavelength, flux, verbose=verbose)
;den = avgflux(1.975, 1.995, wavelength, flux, verbose=verbose)
;index[i] = strtrim(string(num/den),1)
index[i] = strtrim(string(ratio(2.09, 2.11,1.975, 1.995,wavelength, flux, verbose=verbose)),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='CH_2.2'
;num = avgflux(2.08, 2.12, wavelength, flux, verbose=verbose)
;den = avgflux(2.215, 2.255, wavelength, flux, verbose=verbose)
;index[i]= strtrim(string(num/den),1)
index[i] = strtrim(string(ratio(2.08, 2.12,2.215, 2.255,wavelength, flux, verbose=verbose)),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

;--------------------
;ALLERS 2007
;--------------------

name[i]='H2O_1.5A'
;num = avgflux(1.55, 1.56, wavelength, flux, verbose=verbose)
;den = avgflux(1.492, 1.502, wavelength, flux, verbose=verbose)
;index[i] = strtrim(string(num/den),1)
index[i] = strtrim(string(ratio(1.55, 1.56,1.492, 1.502, wavelength, flux, verbose=verbose)),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='Na'
;num = avgflux(1.15, 1.16, wavelength, flux, verbose=verbose) 
;den = avgflux(1.134, 1.144, wavelength, flux, verbose=verbose)
;index[i] = strtrim(string(num/den),1)
index[i] = strtrim(string(ratio(1.15, 1.16, 1.134, 1.144, wavelength, flux, verbose=verbose)),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i


;------------------
; McLEAN 2003
;------------------

name[i]='H2OA'
;num = avgflux(1.341, 1.345, wavelength, flux, verbose=verbose)
;den = avgflux(1.311, 1.315, wavelength, flux, verbose=verbose)
;index[i] = strtrim(string(num/den),1)
index[i] = strtrim(string(ratio(1.341, 1.345,1.311, 1.315, wavelength, flux,/median,verbose=verbose)),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='H2OB'
index[i] = strtrim(string(ratio(1.454, 1.458,1.568, 1.572, wavelength, flux,/median,verbose=verbose)),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='H2OC'
;num = avgflux(1.786, 1.790, wavelength, flux, verbose=verbose)
;den = avgflux(1.720, 1.724, wavelength, flux, verbose=verbose) 
;index[i] = strtrim(string(num/den),1)
index[i] = strtrim(string(ratio(1.786, 1.790,1.720, 1.724, wavelength, flux,/median,verbose=verbose)),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='H2OD'
;;num = avgflux(1.962, 1.966, wavelength, flux, verbose=verbose)
;den = avgflux(2.073, 2.077, wavelength, flux, verbose=verbose)
;index[i] = strtrim(string(num/den),1)
index[i] = strtrim(string(ratio(1.962, 1.966,2.073, 2.077, wavelength, flux,/median,verbose=verbose)),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='CH4A'
;num = avgflux(1.728, 1.732, wavelength, flux, verbose=verbose) 
;den = avgflux(1.588, 1.592, wavelength, flux, verbose=verbose) 
;index[i] = strtrim(string(num/den),1)
index[i] = strtrim(string(ratio(1.728, 1.732,1.588, 1.592, wavelength, flux,/median,verbose=verbose)),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i 

name[i]='CH4B'
;num = avgflux(2.198, 2.202, wavelength, flux, verbose=verbose) 
;den = avgflux(2.098, 2.102, wavelength, flux, verbose=verbose) 
;index[i] = strtrim(string(num/den),1)
index[i] = strtrim(string(ratio(2.198, 2.202,2.098, 2.102, wavelength, flux,/median,verbose=verbose)),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i 

name[i]='CO'
;num = avgflux(2.298, 2.302, wavelength, flux, verbose=verbose) 
;den = avgflux(2.283, 2.287, wavelength, flux, verbose=verbose) 
;index[i] = strtrim(string(num/den),1)
index[i] = strtrim(string(ratio(2.298, 2.302,2.283, 2.287, wavelength, flux,/median,verbose=verbose)),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i 

name[i]='FeH-J'
;num = avgflux(1.198, 1.202, wavelength, flux, verbose=verbose)
;d;en = avgflux(1.183, 1.187, wavelength, flux, verbose=verbose)
;index[i] = strtrim(string(num/den),1)
index[i] = strtrim(string(ratio(1.198, 1.202,1.183, 1.187, wavelength, flux,/median,verbose=verbose)),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='FeH-z'
;num and den switched from McLean definition
;if y eq 9 then stop
index[i] = strtrim(string(ratio(0.984, 0.988, 0.990, 0.994, wavelength, flux,/median,verbose=verbose)),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
;print, name[i] +' ' + strn(index[i]) +' '+  strn(y)
i=++i


;-----------
; My VO
;-----------

name[i]='VOA'
index[i] = strtrim(string(ratio(1.088, 1.094, 1.055, 1.061, wavelength, flux,/median,verbose=verbose)),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i

name[i]='VOB'
num1  = avgflux(1.15, 1.16, wavelength, flux)
num2 = avgflux(1.22, 1.23, wavelength, flux)
den = avgflux(1.18, 1.20, wavelength, flux)
index[i] = strtrim(string((num1+num2)/2/den),1)
IF KEYWORD_SET(verbose) THEN print, name[i],index[i]
i=++i


;-----------


index_name  = name[0:i-1]
index = index[0:i-1]

;Make header and print if it's the first object 
if y eq 0 then begin
    header = 'OBJECT'+ ','+ STRJOIN(index_name,', ')
    ;header = STRJOIN(name,', ')
    printf, unit, header
    indices=fltarr(num_files,i)
ENDIF

;Format output for comma-delimited file
dataline = sobject[y] +',' + STRJOIN(index,', ')
printf, unit, dataline

indices[y,*]=index

;plot, wavelength, flux
;print, ''
; Print,'Press any key to continue'
; print, ' '
; anykey=get_kbrd(1)

ENDFOR ;stop looping through spectra

save, file=output_file+'.sav',indices,index_name,sobject

close, unit ;close output file
FREE_LUN,unit

ENDFOR  ;stop looping through datasets                         
END
