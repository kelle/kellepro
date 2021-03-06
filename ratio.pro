FUNCTION ratio, startw_num, endw_num, startw_den, endw_den, wavelength, flux,median=median,verbose=verbose

;+
; NAME:
;       RATIO
;
; PURPOSE:
;	This procedure calculates the ratio of the flux in two regions.
;
;
; CALLING SEQUENCE:
;
;
; INPUTS:
;
;
; OUTPUTS:
;       float ratio of num and den
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
;       Modified measure.pro to measure NIR data.
;       Kelle Cruz, March 2007
;-

num = avgflux(startw_num, endw_num, wavelength, flux,median=median,verbose=verbose)
den = avgflux(startw_den, endw_den, wavelength, flux,median=median,verbose=verbose)
  

if num eq -1 or den eq -1 then ratio = 0 else ratio=num/den

;if keyword_set(verbose) then begin
if ratio eq 1 then begin
    print,'num= ' + strn(num)
    print,'den= ' + strn(den)
    print, ratio
    print, ''
ENDIF

RETURN, ratio

END
