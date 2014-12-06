FUNCTION avgflux, startw, endw, wavelength_big, flux_big,sigma_big,sum=sum, median=median,verbose=verbose

;+
; NAME:
;	AVGFLUX
;
; PURPOSE:
;	This function returns the average (or sum) flux in a region of a spectra. 
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
;	A 3-element vector containing 1) the average (or summed) flux in the region as a 
;	double-precison real number 2) the std deviation on that
;	mean, and 3) the number of pixels in the region.
;
; PROCEDURE:
;	The fractional weight is found for the end pixels.  This fraction 
;	is used for determining the flux to use from that pixel and to 
;	determine the total number of pixels.  The average (mean) flux is found 
;	by summing over the fluxes and dividing by the fractional number 
;	of pixels.
;       The values measured by this function agree with those found by
;       the 'sbands' IRAF procedure (default: norm=yes, /Sum: norm=no).
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
; 	Written by:	Kelle Cruz, March 2001
;       Modified June 2006 to be more versatile, KC
;       Added uncertainty propagation. Oct 2008, KC
;       Added SUM keyword. Oct 2008, KC
;-


if N_params() lt 1 then begin
    print,'Syntax -  a=AVGFLUX(start_wavelength, end wavelength, wavelength_array, flux_array)'
    print,''
    return, -1
endif

IF MIN(wavelength_big) gt startw OR MAX(wavelength_big) lt endw THEN BEGIN
    IF KEYWORD_SET(verbose) THEN print, 'AVGFLUX: Wavelength out of range'
    RETURN, -1
ENDIF

IF (where(wavelength_big ge startw AND wavelength_big-0.0022 le endw))[0] eq -1  THEN BEGIN
    IF KEYWORD_SET(verbose) THEN print, 'AVGFLUX: No data in range'
    RETURN, -1
ENDIF

;just need pixel scale
b = where(wavelength_big ge startw)
pix_scale=wavelength_big[b[0]+1]-wavelength_big[b[0]]

a = where(wavelength_big+pix_scale/2 ge startw AND wavelength_big-pix_scale/2 le endw)

wavelength=wavelength_big[a]
flux=flux_big[a]
IF N_ELEMENTS(sigma_big) gt 1 THEN sigma=sigma_big[a]

num_pixels = n_elements(wavelength)
first = 0
last = num_pixels - 1

IF num_pixels gt 1 THEN BEGIN
;determine the fractional pixel value for the pixels on the edge of the region
    frac1 = (wavelength(first) + pix_scale/2 - startw) / pix_scale
    frac2 = (endw - (wavelength(last) - pix_scale/2))  / pix_scale

;sum flux

    sumflux = 0
    sumsigma2=0
    FOR i = first, last DO BEGIN
        IF i eq first THEN pixflux = frac1*flux[i] $
	ELSE IF i eq last THEN pixflux = frac2*flux[i] $
	ELSE pixflux = flux[i]
	sumflux = sumflux + pixflux
        ;print,sumflux

        IF n_elements(sigma) gt 1 then begin
            IF i eq first THEN sigflux2 = frac1^2*sigma[i]^2 $
            ELSE IF i eq last THEN sigflux2 = frac2^2*sigma[i]^2 $
            ELSE sigflux2 = sigma[i]^2
            sumsigma2 = sumsigma2 + sigflux2
            ;print, sumsigma2
        ENDIF
        ;print, sumflux, pixflux
    ENDFOR

    realpix = num_pixels - 2 + frac1 + frac2 ;fracional pixel value
    avgflux = sumflux/realpix
                                ;use the sample variance if the sigma
                                ;spectrum is not present to estimate uncertainty
    IF n_elements(sigma) gt 1 then sigflux=sqrt(sumsigma2)/realpix else $
        sigflux= SQRT(TOTAL((flux[first:last] -  mean(flux[first:last]))^2)/(num_pixels-1))/sqrt(num_pixels)

ENDIF ;end npixels > 1

IF num_pixels eq 1 THEN BEGIN
    frac = (endw-startw)/pix_scale
    avgflux=frac*flux[0]
ENDIF

;MEDIAN DOENST WORK WELL for LOW RES
; see p.33 of Ahra's notebook
IF keyword_set(median) and num_pixels gt 5 then begin
    if KEYWORD_SET(verbose) THEN print, 'median worked'
    old=avgflux
    avgflux = MEDIAN(flux)
    IF 100*abs(avgflux-old)/old gt 3 then begin
        print, 'AVGFLUX: WARNING: Difference between average and median greater than 3%'
        print,'AVGFLUX: median='+strn(avgflux) + ' avg=' +strn(old) + ' diff%= ' + strn(100*abs(avgflux-old)/old)
    ENDIF ELSE BEGIN
        ;if KEYWORD_SET(verbose) THEN print, 'median worked'
    ENDELSE    
ENDIF

if keyword_set(verbose) then begin
    if  keyword_set(sum) then begin
        scale=floor(alog10(sumflux))
        print, 'sumflux ' + strn(sumflux/10.^(scale),format='(f0.3)') + 'E' + strn(scale) + $
                 ' Num Pixels= '+ strn(num_pixels)+ ' ' + strn(realpix)
    endif else begin
        scale=floor(alog10(avgflux))
        print, 'avgflux ' + strn(avgflux/10.^(scale),format='(f0.3)') +' +/- ' + $
               strn(sigflux/10.^(scale),format='(f0.3)') + 'E' + strn(scale) + $
               '  Num Pixels= '+ strn(num_pixels) +  '  % unc= '+ strn(sigflux/avgflux*100., format='(f0.1)') 
    endelse
ENDIF


if keyword_set(sum) then $
  return, [sumflux,sigflux,realpix] $ 
else $
  return, [avgflux,sigflux,num_pixels] 

END
