pro measure, input_file, ps=ps,file_key=file_key,verbose=verbose

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
;       Uncertainties: Since K99 indices use the pseudo continuum as
;       the Numerator and the feature as the denominator, the std dev
;       of the num is used (as a proxy for noise) for the unc in the
;       den when propogathing the unc in the index.
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
;       Loop over indices with 2 num, KC Oct 2008
;       Accept .fits input, KC Oct 2008
;       Propogate Uncertainties, KC Oct 2008
;-

if N_params() lt 1 then begin
     print,"Syntax -  MEASURE, 'Input_file.lis', ( /file,/verbose)"
     print,"Input_file should have .lis extension and contain list of fits files"
     print, "Output_file will be created with '_indices' appended to the root file name."
     return
endif

;root='/scr7/asweet/reducedata/alldata/'
root='/scr/kelle/optical_spectra/2M_all/fits/'
;root='/scr/kelle/optical_spectra/lris/davy/'
;root= '/Users/kelle/Documents/spectra/lris/'

outroot='/scr2/kelle/young/indices/'
@colors_kc

input_file_root=(strsplit(input_file,'.',/extract))[0]
ext=(strsplit(input_file,'.',/extract))[1]

IF KEYWORD_SET(ps) THEN BEGIN
    !p.font=0
    set_plot, 'ps'
    device, encapsulated=0, /helvetica, /isolatin1, $
        landscape=1, /color,xoffset=0,yoffset=11,/inches
    device, file=outroot+input_file_root+'_spec.ps'
    @symbols_ps_kc
ENDIF ELSE BEGIN
    set_plot,'x'
    device, Decomposed=0        ;make colors work for 24-bit display
    black=white ;exchange colors to work with default black backround
    @symbols_kc
;    window, 0, xsize=xsize_window, ysize=xsize_window*aspect_ratio,xpos=0,ypos=0
ENDELSE



If ext eq 'fits' then begin
    spectra_files=input_file
    ref=999.
    desig='99-99'
    sp_type=99
    key=1
endif else begin
    READCOL, input_file,ref,desig,sp_type,spectra_files,key,$ 
             format='F,A,F,A,I',/preserve_null, comment='#'
endelse

num_files = n_elements(spectra_files)
output_file=FILE_BASENAME(input_file,'.lis')+'_indices'
OPENW, unit, output_file+'.txt',/get_lun ; open output file for writing

readcol,'/scr2/kelle/idl/kellepro/indices_optical.lis', index_ref,index_name,numer1,numer2,denom1,denom2,format='A,A,F,F,F,F',comment='#'
n_indices=n_elements(index_ref)

FOR y = 0, num_files-1 DO BEGIN

    data_file = spectra_files[y]
    spec=KREADSPEC(root+data_file,h,silent=~keyword_set(verbose))
    wavelength = REFORM(spec[0,*])
    flux = REFORM(spec[1,*])
    ;if (size(spec))[1] eq 3 then sigma=REFORM(spec[2,*]) else sigma = 0
    sigma=0 ;always use std dev of data to estimate unc

    object=strtrim(sxpar(h,'OBJECT'),2)
    file_name=(strsplit(data_file,'.',/extract))[0]
    IF keyword_set(file_key) then sobject = file_name else $
              sobject = object

 

print, ' '
print, sobject

;Measure indices
    i=0
    index=fltarr(n_indices+7)
    index_sigma=fltarr(n_indices+7)

;--------------------
; Ratios with 1 num and den
;--------------------

plot, wavelength, flux, xr=[7250,9000],xstyle=1,/nodata, ystyle=1
oplot, wavelength, flux

   xyouts, 0.1,0.9, sobject+'!C'+file_name,/normal

readcol,'/scr2/kelle/idl/kellepro/indices_optical.lis', index_ref,index_name,numer1,numer2,denom1,denom2,format='A,A,F,F,F,F',comment='#'
n_indices=n_elements(index_ref)

FOR i=0,n_indices-1 DO BEGIN

    num = avgflux(numer1[i], numer2[i], wavelength, flux, sigma, verbose=verbose)
    den = avgflux(denom1[i], denom2[i], wavelength, flux, sigma, verbose=verbose)
    unc =  avgflux(3./2.*numer1[i] - 1./2.*numer2[i],3./2.*numer2[i] - 1./2.*numer1[i], wavelength, flux, sigma, verbose=verbose)

    if num[0] eq -1 or den[0] eq -1 then index[i] = 0 else begin
        index[i] = num[0]/den[0]
        num[1]=num[0]*unc[1]/unc[0]
        ;num[1] = unc[1]*sqrt(unc[2])/sqrt(num[2])

        ;den[1]= num[1] ; adopt unc in num for denominator since num measures the pseudo-continuum.
        den[1] = den[0]*unc[1]/unc[0]
        ;print, den[1]/den[0]
;        print, num[1],den[1] 
        index_sigma[i] = index[i] * sqrt( (num[1]^2)/(num[0])^2 + (den[1]/den[0])^2 )
    endelse

    IF KEYWORD_SET(verbose) THEN BEGIN
        if  index[i] eq 0 then  print,index_name[i] + ': undefined' else $
          print,index_name[i] + ': ' + strn(index[i],format='(f0.2)') + ' +/- '  + strn(index_sigma[i],format='(f0.2)') + $
          ' (' + strn(index_sigma[i]/index[i]*100, format='(f0.1)') + '%)'
        print, ' '
    ENDIF
 
    if  index[i] ne 0 and numer1[i] gt 7200. then begin 
        polyfill, [numer1[i],numer2[i],numer2[i],numer1[i]],[num[0]-num[1],num[0]-num[1],num[0]+num[1],num[0]+num[1]],color=151+(5*i mod 45)
        polyfill, [denom1[i],denom2[i],denom2[i],denom1[i]],[den[0]-den[1],den[0]-den[1],den[0]+den[1],den[0]+den[1]],color=151+(5*i mod 45)
        xyouts, denom1[i],1.25*num[0],  index_name[i], size=1.0,align=0.5, color=151+(5*i mod 45)
        xyouts, denom1[i] ,den[0]/1.25,  strn(index[i],format='(f0.2)'), size=1.0,align=0.5,color=151+(5*i mod 45)
    endif

ENDFOR

;--------------------
; Ratios with Average of 2 numerators and 1 den
;--------------------

readcol,'/scr2/kelle/idl/kellepro/indices_optical2.lis', index2_ref,index2_name,numer1,numer2,numer3,numer4,denom1,denom2,format='A,A,F,F,F,F,F,F',comment='#'
n_indices2=n_elements(index2_ref)

index_name=[index_name,index2_name]

FOR j=0,n_indices2-1 DO BEGIN

    num1 = avgflux(numer1[j], numer2[j], wavelength, flux, sigma, verbose=verbose)
    num2 = avgflux(numer3[j], numer4[j], wavelength, flux, sigma, verbose=verbose)
    den = avgflux(denom1[j], denom2[j], wavelength, flux, sigma, verbose=verbose)
    unc1 =  avgflux(3./2.*numer1[j] - 1./2.*numer2[j],3./2.*numer2[j] - 1./2.*numer1[j], wavelength, flux, sigma, verbose=verbose)
   unc2 =  avgflux(3./2.*numer3[j] - 1./2.*numer4[j],3./2.*numer4[j] - 1./2.*numer3[j], wavelength, flux, sigma, verbose=verbose)
    if num1[0] eq -1 or num2[0] eq -1 or den[0] eq -1 then index[i] = 0 else begin
        index[i]= (num1[0]+num2[0])/2./den[0]   
        num1[1] =  unc1[1]*sqrt(unc1[2])/sqrt(num1[2])
        num2[1] =  unc2[1]*sqrt(unc2[2])/sqrt(num2[2])
        den[1]=(num1[1] + num2[1])/2  *SQRT(num1[2])/SQRT(den[2]) ; adopt unc in num for denominator since num measures the pseudo-continuum.
        index_sigma[i] = index[i] * sqrt( (num1[1]^2 + num2[1]^2)/(num1[0]+num2[0])^2 + (den[1]/den[0])^2 )
    endelse

    IF KEYWORD_SET(verbose) THEN BEGIN
        if  index[i] eq 0 then  print,index_name[i] + ': undefined' else $
          print,index_name[i] + ': ' + strn(index[i],format='(f0.2)') + ' +/- '  + strn(index_sigma[i],format='(f0.2)') + $
          ' (' + strn(index_sigma[i]/index[i]*100, format='(f0.1)') + '%)'
        print, ' '
    ENDIF
    if  index[i] ne 0 then begin 
        polyfill, [numer1[j],numer2[j],numer2[j],numer1[j]],[num1[0]-num1[1],num1[0]-num1[1],num1[0]+num1[1],num1[0]+num1[1]],color=151+(5*i mod 45)
        polyfill, [numer3[j],numer4[j],numer4[j],numer3[j]],[num2[0]-num2[1],num2[0]-num2[1],num2[0]+num2[1],num2[0]+num2[1]],color=151+(5*i mod 45)
        polyfill, [denom1[j],denom2[j],denom2[j],denom1[j]],[den[0]-den[1],den[0]-den[1],den[0]+den[1],den[0]+den[1]],color=151+(5*i mod 45)
        xyouts,denom1[j],1.25*num1[0],  index_name[i], size=1.0,align=0.5, color=151+(5*i mod 45)
        xyouts, denom1[j] ,den[0]/1.25,  strn(index[i],format='(f0.2)'), size=1.0,align=0.5,color=151+(5*i mod 45)
    endif

    i=++i
ENDFOR

;--------------------
;    VO-a and VO-b
;--------------------
index_name= [index_name,'VO-a']
numer1=7350.
numer2=7370.
numer3=7550.
numer4=7570.
denom1=7430
denom2=7470.

num1 = avgflux(numer1, numer2, wavelength, flux, /sum, verbose=verbose)
num2 = avgflux(numer3, numer4, wavelength, flux, /sum, verbose=verbose)
den = avgflux(denom1, denom2, wavelength, flux, verbose=verbose)

num=(num1[0] + num2[0])/(num1[2] + num2[2])
num_unc = SQRT(num1[1]^2 + num2[1]^2); unc already divided by N in avgflux
den[1]=num_unc; adopt unc in num for denominator since num measures the pseudo-continuum.

index[i] = num/den[0]
index_sigma[i] = index[i] * sqrt( (num_unc/num)^2 + (den[1]/den[0])^2 )

IF KEYWORD_SET(verbose) THEN BEGIN
     print,index_name[i] + ': ' + strn(index[i],format='(f0.2)') + ' +/- '  + strn(index_sigma[i],format='(f0.2)') + $
          ' (' + strn(index_sigma[i]/index[i]*100, format='(f0.1)') + '%)'
        print, ' '  
ENDIF

polyfill, [numer1,numer2,numer2,numer1],[num-num_unc,num-num_unc,num+num_unc,num+num_unc],color=151+(5*i mod 45)
polyfill, [numer3,numer4,numer4,numer3],[num-num_unc,num-num_unc,num+num_unc,num+num_unc],color=151+(5*i mod 45)
polyfill, [denom1,denom2,denom2,denom1],[den[0]-den[1],den[0]-den[1],den[0]+den[1],den[0]+den[1]],color=151+(5*i mod 45)
xyouts, denom1,1.25*num,  index_name[i], size=1.0,align=0.5,color=151+(5*i mod 45)
xyouts, denom1 ,den[0]/1.25,  strn(index[i],format='(f0.2)'), size=1.0,align=0.5,color=151+(5*i mod 45)
i=++i

index_name= [index_name,'VO-b']
numer1=7860.
numer2=7880.
numer3=8080.
numer4=8100.
denom1=7960.
denom2=8000.

num1 = avgflux(numer1, numer2, wavelength, flux, /sum, verbose=verbose)
num2 = avgflux(numer3, numer4, wavelength, flux, /sum, verbose=verbose)
den = avgflux(denom1, denom2, wavelength, flux, verbose=verbose)

num=(num1[0] + num2[0])/(num1[2] + num2[2])
num_unc = SQRT(num1[1]^2 + num2[1]^2); unc already divided by N in avgflux
den[1]=num_unc; adopt unc in num for denominator since num measures the pseudo-continuum.

index[i] = num/den[0]
index_sigma[i] = index[i] * sqrt( (num_unc/num)^2 + (den[1]/den[0])^2 )

IF KEYWORD_SET(verbose) THEN BEGIN
     print,index_name[i] + ': ' + strn(index[i],format='(f0.2)') + ' +/- '  + strn(index_sigma[i],format='(f0.2)') + $
          ' (' + strn(index_sigma[i]/index[i]*100, format='(f0.1)') + '%)'
        print, ' '  
ENDIF

polyfill, [numer1,numer2,numer2,numer1],[num-num_unc,num-num_unc,num+num_unc,num+num_unc],color=151+(5*i mod 45)
polyfill, [numer3,numer4,numer4,numer3],[num-num_unc,num-num_unc,num+num_unc,num+num_unc],color=151+(5*i mod 45)
polyfill, [denom1,denom2,denom2,denom1],[den[0]-den[1],den[0]-den[1],den[0]+den[1],den[0]+den[1]],color=151+(5*i mod 45)
xyouts, denom1, 1.25*num,  index_name[i], size=1.0,align=0.5,color=151+(5*i mod 45)
xyouts, denom1, den[0]/1.25,  strn(index[i],format='(f0.2)'), size=1.0,align=0.5,color=151+(5*i mod 45)
i=++i

;--------------------
;PMSU Spectral Ratios
; two denominators
;--------------------

index_name= [index_name,'CaH1']
num  = avgflux(6380., 6390., wavelength, flux, verbose=verbose)
den1 = avgflux(6345., 6355., wavelength, flux, verbose=verbose)
den2 = avgflux(6410., 6420., wavelength, flux, verbose=verbose)
index[i] = num[0]/((den1[0]+den2[0])/2)
IF KEYWORD_SET(verbose) THEN print, index_name[i],index[i]

;Make header and print if it's the first object 
if y eq 0 then begin
    header = 'OBJECT'+ ','+ STRJOIN(index_name,', ')
    printf, unit, header
    indices=fltarr(num_files,i+1)
    indices_sigma=fltarr(num_files,i+1)
ENDIF

data=strarr(2,i+1)
data[0,*]=index
data[1,*]=index_sigma

;Format output for comma-delimited file
dataline = sobject +',' + STRJOIN(STRJOIN(data,', '),', ')
;dataline = STRJOIN(index,', ')
printf, unit, dataline

indices[y,*]=index
indices_sigma[y,*]=index_sigma

if keyword_set(verbose) and num_files-1 ne y then begin
    print, 'Press any key to continue'
    tmp=get_kbrd()
endif


ENDFOR ;stop looping through spectra

IF KEYWORD_SET(ps) THEN begin
    device,/close
    set_plot,'x'
    MESSAGE, 'Wrote: '+ outroot+input_file_root+'_spec.ps',/info
endif


save, file=output_file+'.sav',ref,desig,sp_type,spectra_files,indices,indices_sigma,index_name,key

close, unit ;close output file
FREE_LUN,unit

END
