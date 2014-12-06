FUNCTION avgflux, startw, endw, wavelength_big, flux_big

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
;       Modified June 2006 to be more versatile, KC
; 	Written by:	Kelle Cruz, March 2001
;-


if N_params() lt 1 then begin
    print,'Syntax -  a=AVGFLUX(start_wavelength, end wavelength, wavelength_array, flux_array)'
    print,''
    return,0
endif

IF MIN(wavelength_big) gt startw OR MAX(wavelength_big) lt endw THEN BEGIN
    print, 'AVGFLUX: Wavelength out of range
    RETURN, -1
ENDIF

b = where(wavelength_big ge startw AND wavelength_big le endw)

IF n_elements(b) eq 1 THEN BEGIN
    print, 'AVGFLUX: BIN TOO SMALL
    RETURN, -1
ENDIF

pix_scale=wavelength_big[b[1]]-wavelength_big[b[0]]
;print, b
;print, ''

a = where(wavelength_big ge startw-pix_scale/2 AND wavelength_big le endw+pix_scale/2)
;print, a
;print, ''

wavelength=wavelength_big[a]
flux=flux_big[a]

;print, flux
;print, ''

num_pixels = n_elements(wavelength)
first = 0
last = num_pixels - 1

;determine the fractional pixel value for the pixels on the edge of the region

frac1 = (wavelength(first) + pix_scale/2 - startw) / pix_scale
frac2 = (endw - (wavelength(last) - pix_scale/2))  / pix_scale

;sum flux

sumflux = 0

;stop

FOR i = first, last DO BEGIN
	IF i eq first THEN pixflux = frac1*flux[i] $
	ELSE IF i eq last THEN pixflux = frac2*flux[i] $
	ELSE pixflux = flux[i]
	sumflux = sumflux + pixflux
        ;print, sumflux, pixflux
ENDFOR

realpix = num_pixels - 2 + frac1 + frac2   ;fracional pixel value
;print, realpix, num_pixels
avgflux = sumflux/realpix

return, avgflux

END
