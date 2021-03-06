PRO MERGE_SPICE, object, spec, interact=interact,nocalib=nocalib
version='v3'

;COADDS and MERGEs spectra extraced with SPICE and put into
;/data/hillary/3/kelle/ir_spectra/irs/S12/spice_output/

;calls coadd_spice, writefits

if N_params() lt 1 then begin
     print,'   Syntax -  MERGE_SPICE, object [, spec, /INTERACT, /nocalib]'
     print, ''
     print,'   object is number'
     print,'   output to /data/hillary/3/kelle/ir_spectra/irs/S12/idl_output/'
     return
endif



work_dir2 = '/data/hillary/3/kelle/ir_spectra/irs/S12/idl_output/'
work_dir='/data/hillary/3/kelle/ir_spectra/irs/S12/test'

object=strn(object)

IF keyword_set(interact) THEN BEGIN
sl2=COADD_SPICE(object, 2, hdr2, /interact)
sl1=COADD_SPICE(object, 1, hdr1, /interact)
ENDIF ELSE BEGIN
sl2=COADD_SPICE(object, 2, hdr2)
sl1=COADD_SPICE(object, 1, hdr1)
ENDELSE

;TAPE TOGETHER AT 7.6 microns
a=where(sl2[0,*] LT 7.6 AND sl2[0,*] GT 5.2)
b=where(sl1[0,*] GE 7.6 AND sl1[0,*] LE 14.5)
;Scintifically valid portion does not extend beyond 14.5 microns.
;sec 2.3.5 of IRS data handbook and 7.1.5.1 of SOM

size=n_elements(a)+ n_elements(b)
w_merge=FLTARR(size)
flux_merge=w_merge

w_merge[0:n_elements(a)-1]   = sl2[0,a]
w_merge[n_elements(a):size-1]= sl1[0,b]

flux_merge[0:n_elements(a)-1]   = sl2[1,a]
flux_merge[n_elements(a):size-1]= sl1[1,b]

plot, w_merge,flux_merge, xr=[5,16]
oplot, sl2[0,*], sl2[1,*], linestyle=1
oplot, sl1[0,*], sl1[1,*], linestyle=2

spec=FLTARR(2,size)
spec[0,*]=w_merge

IF ~KEYWORD_SET(nocalib) THEN BEGIN
   restore, '/data/hillary/3/kelle/ir_spectra/irs/S12/flux_calibrators/etaDor.dat'
   ;gives merged and scaled Decin model of etaDor: model_w, model_f

   ;read in reduced observation of etaDor
   std_file = GET_FILE('eta', work_dir2)
   obs_std = READFITS(std_file)

   flux_calib=flux_merge * model_f / obs_std[1,*]
   oplot, w_merge, flux_calib, thick =2
   spec[1,*]=flux_calib

ENDIF ELSE BEGIN

   spec[1,*]=flux_merge

ENDELSE

;TACK ON HEADER FROM SL1
header=['SIMPLE  =                    T / Fits standard',$
        'BITPIX  =                  -32 / FOUR-BYTE SINGLE PRECISION FLOATING POINT', $
        hdr1,'END']
hist1='MERGE_SPICE '+ version + ', ran ' + systime(0)
hist2='MERGE SPICE tacked on header from SL1'
SXADDHIST, [hist1,hist2], header

WRITEFITS, work_dir+object+'_merge.fits', spec, header
 
END
