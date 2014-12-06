function get_nsfcam_band, header
;+
; NAME:
;	GET_NSFCAM_BAND
; PURPOSE:
;	Get wavelength and bandwidth, based on the filter code in the
;	FITS header.
;
; CALLING SEQUENCE:
;	result = getband(header)
;
; INPUT:
;		header    = string containing FITS header
;
; OUTPUT:
;		result    = 2-element floating-point vector array containing 
;			    wavelength and bandwidth [microns].
;
; PROCEDURES CALLED:
;	sxpar
;
; MODIFICATION HISTORY:
;	Written by K. A. Marsh,  1996 Jan 25. (orginally getband.pro)
;	Modified for NSFCAM by K. Cruz, 2000, Jan 22
;-

; Get the filter code.
    filter = sxpar(header,'FIL_WHEL')

; Convert the filter code into a wavelength and bandwidth.
    result = fltarr(2)

    filterlist = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, $
		   16, 17, 18, 19, 20, 21]
    rlambda    = [0, 1.26, 1.62, 2.12, 3.5, 3.78, 4.85, 1.73, 2.28, 2.21, 0, 1.08, 1.09, 1.28, 1.64, 2.12, $
		  2.16, 2.25, 2.30, 2.26, 0, 4.77]
    dlambda    = [0, 0.31, 0.28, 0.34, 0.61, 0.59, 0.62, 0.10, 0.17, 0.39, 0, 0.01, 0.01, 0.01, 0.02, 0.02, $
		   0.02, 0.02, 0.03, 0.05, 0, 0.23]

;    nfilters = 6
;    ifilter = 0
;    k = -1

   ; while (ifilter eq 0 and k lt nfilters-1) do begin
;	k = k + 1

;	if ((i = strpos(filterlist(k),filter))) ne -1 then begin
;	    ifilter = k
 
            result(0) = rlambda(filter)
            result(1) = dlambda(filter)
;	endif
;    endwhile

    return, result

    end
