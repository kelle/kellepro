pro measure, input_file, ps=ps,file_key=file_key,verbose=verbose, $
	nir=nir,onecolumn=onecolumn,ref=ref, desig=desig, spt=sp_type

;+
; NAME:
;	MEASURE
;
; PURPOSE:
;	This procedure measures spectral indices of IR spectra.  
;	Indices measured are listed in indices_optical.lis and
;	indices_nir.lis
;       TiO 5, Ti0 4, Ti0 3, TiO 2, H-alpha, CaOH, CaH1, CaH2, Cah3,
;       and all indices from Table 7 of Kirkpatrick et al. 1999.
;
; CALLING SEQUENCE:
;
;	MEASURE, Input_file
;
; INPUTS:
;	Input_file: File that contains list of FITS files. 
;			One per line. 
;			tab-delimited
;       examples:
;       /scr2/kelle/young/indices/young.lis
;       /scr2/kelle/nir_indices/nir.lis
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
; ROUTINES CALLED:
;	kellepro: avgflux
;       kellepro: kreadspec
;       astropro: readcol
;       indices_optical.lis
;       indices_nir.lis
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
;       NIR, KC Jan 2009
;-

if N_params() lt 1 then begin
     print,"Syntax -  MEASURE, 'Input_file.lis', ( /file,/verbose,/nir,/onecolummn)"
     print,'Input_file should be tab-delimeted and have .lis extension '
     print,'Columns: ref,desig,sp_type,spectra_files'
     print,"Output_file will be created with '_indices' appended to the root file name."     
     print,'use /one is file just contains list of fits files'
     print,'/ps to create one ps file with all spectra and indices plotted'
	print,"for one file, ref=12345, desig='002345.3+1223345',spt=16"
     return
endif

if keyword_set(nir) then begin
	root1='/scr1/nir_spectra/2M_all/'
	root2='~/Dropbox/Data/nir_spectra/'
	if File_test(root1) then begin
		root=root1
		indices_file='/scr1/idl/kellepro/indices_nir.lis'
    	outroot='/scr1/Analysis/nir_indices/'
	endif
	if file_test(root2) then begin
		root=root2
		indices_file='~/Code/IDL/kellepro/indices_nir.lis'
    	outroot='~/Analysis/nir_indices/'
	endif
endif else begin
    MESSAGE, 'ASSUMING OPTICAL. Use /NIR for NIR data.',/info
    ;root1='/scr/kelle/optical_spectra/2M_all/fits/'
    ;root2='~/Data/optical_spectra/'
	;if file_test(root1) then begin
;		root=root1;
		indices_file='/scr2/kelle/idl/kellepro/indices_optical.lis'
;    	indices_file2='/scr2/kelle/idl/kellepro/indices_optical2.lis'
;	endif else begin
;		root=root2
;		indices_file='~/Library/IDL/kellepro/indices_optical.lis'
;	   	indices_file2='~/Library/IDL/kellepro/indices_optical2.lis'
ENDelse

if ~keyword_set(nir) then begin
	root='./'
	outroot='./'
	indices_file='~/Code/IDL/kellepro/indices_optical.lis'
	indices_file2='~/Code/IDL/kellepro/indices_optical2.lis'
endif

@colors_kc

input_file_root=(strsplit(input_file,'.',/extract))[0]
ext=(strsplit(input_file,'.',/extract))[1]

xsize=18.6267/1.45 ; 2 column wide 44 pica
ysize=xsize*1.45
;scale=1.4
aspect_ratio=ysize/xsize

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
	if file_test(spectra_files[0]) then begin
		 spec=KREADSPEC(spectra_files[0],h,silent=~keyword_set(verbose),/norm)
	endif else begin 
	spec=KREADSPEC(root+spectra_files[0],h,silent=~keyword_set(verbose),/norm)
	endelse
    if keyword_set(ref) then ref=ref else ref=99
	if keyword_set(desig) then desig=desig else desig=99
    if keyword_set(sp_type) then sp_type=sp_type else sp_type=99
    key=1
endif else begin

    if keyword_set(onecolumn) then $
      READCOL, input_file,spectra_files,format='A' $
    ELSE $
      READCOL, input_file,ref,desig,sp_type,spectra_files, format='F,A,F,A',/preserve_null, comment='#'

endelse

num_files = n_elements(spectra_files)
; output_file=outroot+FILE_BASENAME(input_file,'.lis')+'_indices'
output_file=outroot+input_file_root+'_indices'
OPENW, unit, output_file+'.txt',/get_lun ; open output file for writing

FOR y = 0, num_files-1 DO BEGIN

    data_file = spectra_files[y]
	if file_test(data_file) then begin
		 spec=KREADSPEC(data_file,h,silent=~keyword_set(verbose),norm=0)
	endif else begin 
	spec=KREADSPEC(root+data_file,h,silent=~keyword_set(verbose),norm=0)
	endelse
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
	readcol,indices_file, index_ref,index_name,numer1,numer2,denom1,denom2,format='A,A,F,F,F,F',comment='#'
	n_indices=n_elements(index_ref)

    i=0
    if keyword_set(nir) then index=fltarr(n_indices+1) else index=fltarr(n_indices+7)
    if keyword_set(nir) then index_sigma=fltarr(n_indices+1) else index_sigma=fltarr(n_indices+7)

;--------------------
; Ratios with 1 num and den
;--------------------

if keyword_set(nir) then xr=[0.8,2.5] else  xr=[7250,9000]

plot, wavelength, flux, xr=xr,xstyle=1,/nodata, ystyle=1,yr=[0,1.15*max(flux)] 
oplot, wavelength, flux

   xyouts, 0.12,0.9, sobject+ '!C'+file_name,/normal
   if ~keyword_set(onecolumn) then xyouts, 0.12,0.85,strn(ref[y]) + ' ' + strn(sp_type[y], format='(f0.2)'),/normal

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
;stop
    CASE index_name[i] OF
    'H2O_A07': begin & H2O_A07=index[i] & spt=(H2O_A07-0.77)/0.04 & end
    'H2OJ': begin & H2OJ=index[i] & spt=10+(1.949e1 -3.919e1*H2OJ + 1.312e2*H2OJ^2 -2.156e2*H2OJ^3 + 1.038e2*H2OJ^4) & end
    'H2OH': begin & H2OH=index[i] & spt=10+(2.708e1 - 8.45e1 * H2OH + 2.424e2 * H2OH^2 - 3.381e2 * H2OH^3 + 1.491e2 * H2OH^4) & end
        'CH4K': begin & CH4K=index[i] & spt=10+(1.885e1 - 2.246e1* CH4K + 2.534e1 * CH4K^2 - 4.734 * CH4K^3 - 1.259e1 * CH4K^4) & end 
        else: spt=0
    ENDCASE
    if spt eq 0 then str_spt=' ' else str_spt=strn(spt,format='(f0.2)')

    if  index[i] ne 0 and (numer1[i] gt 7200. OR keyword_set(NIR)) then begin 
        polyfill, [numer1[i],numer2[i],numer2[i],numer1[i]],[num[0]-num[1],num[0]-num[1],num[0]+num[1],num[0]+num[1]],color=151+(5*i mod 45)
        polyfill, [denom1[i],denom2[i],denom2[i],denom1[i]],[den[0]-den[1],den[0]-den[1],den[0]+den[1],den[0]+den[1]],color=151+(5*i mod 45)
        ;xyouts, denom1[i],1.15*num[0],  index_name[i], size=1.0,align=0.5, color=151+(5*i mod 45)
        xyouts, denom1[i] ,den[0]/1.5,   index_name[i]+'!C'+strn(index[i],format='(f0.2)')+'!C'+str_spt, size=1.0,align=0.5,color=151+(5*i mod 45)
    endif

ENDFOR

;--------------------
; Ratios with Average of 2 numerators and 1 den
;--------------------

if n_elements(indices_file2) ne 0 THEN BEGIN

    readcol,indices_file2, index2_ref,index2_name,numer1,numer2,numer3,numer4,denom1,denom2,format='A,A,F,F,F,F,F,F',comment='#'
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
    ENDFOR ;loop through weird indices
ENDIF

IF ~Keyword_set(nir) then begin
;--------------------
;    VO-a and VO-b
;--------------------
index_name= [index_name,'VO-a']
numer1v=7350.
numer2v=7370.
numer3v=7550.
numer4v=7570.
denom1v=7430
denom2v=7470.

num1 = avgflux(numer1v, numer2v, wavelength, flux, /sum, verbose=verbose)
num2 = avgflux(numer3v, numer4v, wavelength, flux, /sum, verbose=verbose)
den = avgflux(denom1v, denom2v, wavelength, flux, verbose=verbose)

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

polyfill, [numer1v,numer2v,numer2v,numer1v],[num-num_unc,num-num_unc,num+num_unc,num+num_unc],color=151+(5*i mod 45)
polyfill, [numer3v,numer4v,numer4v,numer3v],[num-num_unc,num-num_unc,num+num_unc,num+num_unc],color=151+(5*i mod 45)
polyfill, [denom1v,denom2v,denom2v,denom1v],[den[0]-den[1],den[0]-den[1],den[0]+den[1],den[0]+den[1]],color=151+(5*i mod 45)
xyouts, denom1v,1.25*num,  index_name[i], size=1.0,align=0.5,color=151+(5*i mod 45)
xyouts, denom1v ,den[0]/1.25,  strn(index[i],format='(f0.2)'), size=1.0,align=0.5,color=151+(5*i mod 45)
i=++i

index_name= [index_name,'VO-b']
numer1v=7860.
numer2v=7880.
numer3v=8080.
numer4v=8100.
denom1v=7960.
denom2v=8000.

num1 = avgflux(numer1v, numer2v, wavelength, flux, /sum, verbose=verbose)
num2 = avgflux(numer3v, numer4v, wavelength, flux, /sum, verbose=verbose)
den = avgflux(denom1v, denom2v, wavelength, flux, verbose=verbose)

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

polyfill, [numer1v,numer2v,numer2v,numer1v],[num-num_unc,num-num_unc,num+num_unc,num+num_unc],color=151+(5*i mod 45)
polyfill, [numer3v,numer4v,numer4v,numer3v],[num-num_unc,num-num_unc,num+num_unc,num+num_unc],color=151+(5*i mod 45)
polyfill, [denom1v,denom2v,denom2v,denom1v],[den[0]-den[1],den[0]-den[1],den[0]+den[1],den[0]+den[1]],color=151+(5*i mod 45)
xyouts, denom1v, 1.25*num,  index_name[i], size=1.0,align=0.5,color=151+(5*i mod 45)
xyouts, denom1v, den[0]/1.25,  strn(index[i],format='(f0.2)'), size=1.0,align=0.5,color=151+(5*i mod 45)
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
i=++i

ENDIF ELSE BEGIN ;not NIR

;NIR Testi index 
if y eq 0 then index_name= [index_name,'sH2OJ']
numer1t=1.265
numer2t=1.305
numer3t=1.09
numer4t=1.13

	if min(wavelength) le 1.09 then begin

		num1 = avgflux(numer1t, numer2t, wavelength, flux, verbose=verbose)
		num2 = avgflux(numer3t, numer4t, wavelength, flux, verbose=verbose)

		;num=(num1[0] - num2[0]);/(num1[2] + num2[2])
		;num_unc = SQRT(num1[1]^2 +  num2[1]^2); unc already divided by N in avgflux
		
		;den=(den1[0] + den2[0]);/(den1[2] + den2[2])
		;den[1]=num_unc; adopt unc in num for denominator since num measures the pseudo-continuum.
		;den_unc = SQRT(num1[1]^2 +  num2[1]^2); unc already divided by N in avgflux
		;den_unc=num_unc

		index[i] = (num1[0]-num2[0])/(0.5*(num1[0]+num2[0]))
		;index_sigma[i] = index[i] * sqrt( (num_unc/num)^2 + (den_unc/den)^2 )
		index_sigma[i] = 0

		

		polyfill, [numer1t,numer2t,numer2t,numer1t],[num1[0]-num1[1],num1[0]-num1[1],num1[0]+num1[1],num1[0]+num1[1]],color=151+(5*i mod 45)
		polyfill, [numer3t,numer4t,numer4t,numer3t],[num2[0]-num2[1],num2[0]-num2[1],num2[0]+num2[1],num2[0]+num2[1]],color=151+(5*i mod 45)
    	
		spt=10*((1.54 * index[i]) + 0.98)
    	
		;xyouts, numer3t, 1.05*num1[0],  index_name[i], size=1.0,align=0.5,color=151+(5*i mod 45)
		xyouts, numer3t, num1[0]/2.5,   index_name[i]+'!C'+strn(index[i],format='(f0.2)')+'!C'+strn(spt,format='(f0.2)'), size=1.0,align=0.5,color=151+(5*i mod 45)

	endif else begin
		index[i]=0
		index_sigma[i] = 0
	endelse
	
	IF KEYWORD_SET(verbose) THEN print, index_name[i],index[i]
	i=++i

ENDELSE
nindex=n_elements(index)
;Make header and print if it's the first object 
if y eq 0 then begin
    header = 'OBJECT'+ ','+ STRJOIN(index_name,', ')
    printf, unit, header
    indices=fltarr(num_files,nindex)
    indices_sigma=fltarr(num_files,nindex)
ENDIF


data=strarr(2,nindex)
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

close, unit ;close output file
FREE_LUN,unit
MESSAGE, 'Wrote: '+ output_file+'.txt', /info

IF KEYWORD_SET(ps) THEN begin
    device,/close
    set_plot,'x'
    MESSAGE, 'Wrote: '+ outroot+input_file_root+'_spec.ps',/info
endif

if keyword_set(onecolumn) then $
  SAVE, file=output_file+'.sav',spectra_files,$
  indices,indices_sigma,index_name $
else $
  SAVE, file=output_file+'.sav',ref,desig,sp_type,spectra_files,$
  indices,indices_sigma,index_name

MESSAGE, 'Wrote: '+ output_file+'.sav', /info


END
