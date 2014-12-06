Function KREADSPEC, FitsPath, h, num, norm=norm, M=plotm,silent=silent

;This function accepts a string filename of a 1-d or 3-d FITS spectra
;Returns a 2-d array with wavelength and normalized flux.

;use readspec_txt to read in ASCII files

if N_params() lt 1 then begin
     print,'Syntax -  KREADSPEC, path [, header, /norm, /M, /silent]'
     print,'Default is to normalize for Ls'
     print, 'Set /M keyword to properly normalize Ms.'
     return,0
ENDIF

;if you want to normalize,then use /norm keyword.
;Default is to normalize for L's in the optical
;Set /M keyword to properly normalize M's.
;written 25 aug 2005 by Sarah and Kelle

;calls AVGFLUX

IF keyword_set(silent) THEN spec=READFITS(FitsPath,h,/silent) ELSE spec=READFITS(FitsPath,h)

info=size(spec)

;dim=SXPAR(h,'NAXIS')
dim=info[0]
d1_size=info[1]
d2_size=info[2]



if dim eq 1 then flux=spec
if dim eq 3 then flux=spec[*,*,0]

if dim eq 1 or dim eq 3 then BEGIN
;    size=n_elements(spec)
    w=findgen(d1_size)*SXPAR(h,'CD1_1')+SXPAR(h,'CRVAL1')-SXPAR(h,'LTV1')*SXPAR(h,'CD1_1')
ENDIF

if dim eq 2 then begin
    if d2_size lt d1_size then begin 
        w=reform(spec[*,0])
        flux=reform(spec[*,1])
    endif else begin
        w=reform(spec[0,*])
        flux=reform(spec[1,*])
    endelse
endif

size=n_elements(flux)


IF KEYWORD_SET(norm) THEN BEGIN

    IF min(w) gt 4000 AND  keyword_set(plotm) then begin ;optical M dwarf
        startw = 8080. & endw = 8155. 
    endif

    IF min(w) gt 4000 THEN begin ; optical L dwarf
        startw = 8240. & endw = 8260.
    Endif

    IF min(w) lt 3 then begin ; NIR
        startw = 1.27 & endw = 1.32
    ENDIF

    num = AVGFLUX(startw,endw,w,flux)
    flux=flux/num
ENDIF

data=dblarr(2,size)
data[0,*]=w
data[1,*]=flux

return,data

END
