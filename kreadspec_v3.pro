Function KREADSPEC, FitsPath, h, num, norm=norm, M=plotm,silent=silent, noisy=noisy,nir=nir,plotyes=plotyes,nregion=nregion
;+
; NAME:
;	KREADSPEC
;
; PURPOSE:
;       Reads in spectra in a variety of formats and returns a 2d array
;       with wavelenght and flux. 
;       Options to normalize and to plot.
;
; CALLING SEQUENCE:
;	Result = KREADSPEC(Fitspath, [H, Num, /NORM, /M,/SILENT,
;	                                      /NOISY, /NIR, /PLOT])
;
; INPUTS:
;	Fitspath: String containing path of spectrum to be read. 
;                 Expects extensions fits, dat, txt, or htm.
;                 BEWARE of multiple periods (.) in filenames.	
;
;
; OPTIONAL INPUTS:
;	Parm2:	Describe optional inputs here. If you don't have any, just
;		delete this section.
;	
; KEYWORD PARAMETERS:
;	KEY1:	Document keyword parameters like this. Note that the keyword
;		is shown in ALL CAPS!
;
;	KEY2:	Yet another keyword. 
;		is just a set or unset flag, say something like:
;		"If Set, foobar subfloatation is used. 
;
; OUTPUTS:
;	Returns the wavelength and flux in a 2 x npixels array
;       Converts NIR spectra to micron if in angstroms.
;
; OPTIONAL OUTPUTS:
;
;       If keyword /PLOT set then the spectrum will be plotted to the
;       screen (or current device)
;
; RESTRICTIONS:
;       only works with filenames with periods used only before file
;       extension.
;       BAD: spectrum.M9.fits
;       GOOD: spectrum_M9.fits
;
;	Expects extensions fits, dat, txt, htm.
;
; PROCEDURE:
;	You can describe the foobar superfloatation method being used here.
;	You might not need this section for your routine.
;
; ROUTINES CALLED:
;	astropro:FITS I/O routines
;       kellepro:avgflux
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
; FUTURE MODIFICATIONS:
;	List here anything that you want to add in the future.
;
; MODIFICATION HISTORY:
;       Written by Kelle Cruz and Sarah Schmidt, Aug 2005
;       Reads FITS or ASCII. KC, March 28, 2007
;       Get rid of NANs. KC, Dec 2007
;       Use .fits extension by default. KC, Oct 2008
;       Perserve sigma spectrum for optical data in addition to NIR. KC, Oct 2008
;-

if N_params() lt 1 then begin
     print,'Syntax -  KREADSPEC, path [, header, /plot, /norm, /M, /silent, /noisy, /nir]'
     print, '   /PLOT to plot spectrum to screen after reading it in'
     print,'    IF /NORM is set, the default is to normalize for Ls'
     print,'         Set /M keyword to properly normalize Ms.'
     print,'    /NIR handles three dimensions (wavelength, flux, and sigma)'
     print,'         code will read it NIR data fine without /NIR set'

     return,0
ENDIF

IF file_test(fitspath) eq 0 then begin
    IF file_test(fitspath+'.fits') eq 0 THEN BEGIN
        message,/con,'KREADSPEC: ERROR - Unable to locate file ' + fitspath
        return,-1
    ENDIF ELSE BEGIN 
        fitspath=fitspath+'.fits'
    ENDELSE
ENDIF

IF ~keyword_set(silent) then MESSAGE,'READING: ' + fitspath,/info

ext=(strsplit(FitsPath,'.',/extract))[1]


IF ext eq 'fits' then begin
    IF keyword_set(silent) THEN spec=READFITS(FitsPath,h,/silent) ELSE spec=READFITS(FitsPath,h)

    info=size(spec)

    if keyword_set(nir) and info[2] eq 3 then sigma_flux=spec[*,2]

;dim=SXPAR(h,'NAXIS')
    dim=info[0]
    d1_size=info[1]
    d2_size=info[2]

    if dim eq 1 then flux=spec
    
    ;Read in flux and sigma spectra from 3D spectra
    ;ignore raw and background, if present
    if dim eq 3 then begin
        bandid=SXPAR(h,'BANDID*')
        flux = spec[*,*,where(strmid(bandid,0,3) eq 'spe')] ; BANDID# = 'spectrum'
        if where(strmid(bandid,0,3) eq 'sig') ne -1 then sigma_flux=spec[*,*,where(strmid(bandid,0,3) eq 'sig')] ; BANDID# = 'sigma'
    ENDIF
    
    ;Make wavelength array
    if dim eq 1 or dim eq 3 then BEGIN
        if sxpar(h,'CD1_1') ne 0 then pixel_size=sxpar(h,'CD1_1')
        if sxpar(h,'CD1_1') eq 0 then pixel_size=sxpar(h,'CDELT1')
        if pixel_size eq 0 then print, 'KREADSPEC: ERROR: Pixel Size = 0.'

        w=findgen(d1_size)*pixel_size+SXPAR(h,'CRVAL1')-SXPAR(h,'LTV1')*pixel_size
    ENDIF

    ;accomodate fits files with wavelengeth and flux arrays 
    ;(probably made with IDL)
    if dim eq 2 then begin
        if d2_size lt d1_size then begin 
            w=reform(spec[*,0])
            flux=reform(spec[*,1])
        endif else begin
            w=reform(spec[0,*])
            flux=reform(spec[1,*])
        endelse
    endif

ENDIF                       ; end FITS

;SDSS Spectrum
IF ext eq 'fit' then begin
    IF keyword_set(silent) THEN spec=READFITS(FitsPath,h,/silent) ELSE spec=READFITS(FitsPath,h)

    flux=reform(spec[*,0])

    info=size(spec)

    dim=info[0]
    d1_size=info[1]
    d2_size=info[2]

    if sxpar(h,'CD1_1') ne 0 then pixel_size=sxpar(h,'CD1_1')
    if sxpar(h,'CD1_1') eq 0 then pixel_size=sxpar(h,'CDELT1')
    if pixel_size eq 0 then print, 'KREADSPEC: ERROR: Pixel Size = 0.'

    w=findgen(d1_size)*pixel_size+SXPAR(h,'CRVAL1')-SXPAR(h,'LTV1')*pixel_size
  

ENDIF ;end fit


IF ext eq 'dat' or ext eq 'txt' THEN BEGIN
    RDFLOAT,FitsPath,w,flux,skipline=6,silent=silent
    MKHDR, h, flux
    print, 'KREADSPEC: Convert '+fitspath+' to fits with TEXT2FITS'
ENDIF

IF ext eq 'htm' THEN BEGIN ;.htm from davy, ascii, no header
    RDFLOAT,FitsPath,w,flux,silent=silent
    print, 'KREADSPEC: Convert '+fitspath+' to fits with TEXT2FITS'
ENDIF

;GET RID of NANs
nans= WHERE(FINITE(flux, /nan) eq 1)
IF  nans[0] ne -1 THEN BEGIN
    notnans= where(finite(flux,/nan) eq 0)
    w=w[notnans]
    flux=flux[notnans]
    if keyword_set(nir) then sigma_flux=sigma_flux[notnans]
    n_nans=n_elements(nans)
    if ~KEYWORD_SET(silent) then begin
        if n_nans lt 10 THEN $
          message,  strn(n_nans) + ' NANs located at ' + strjoin(nans),/continue ELSE $
          message,  strn(n_nans) + ' NANs in spectrum',/continue
    endif
ENDIF


size=n_elements(flux)

;convert to micron if necessary
if max(w) gt 15000 then w = w / 10000 
    

IF KEYWORD_SET(norm) THEN BEGIN

    IF min(w) gt 3000 and max(w) lt 12000 AND keyword_set(plotm) then begin ;optical M dwarf
        startw = 8080. & endw = 8155. 
    endif

    IF min(w) gt 3000 and max(w) lt 12000 AND ~keyword_set(plotm) THEN begin ; optical L dwarf
        IF KEYWORD_SET(noisy) then begin
            startw = 8200. & endw = 8500.
        ENDIF ElSE BEGIN
            startw=8240. & endw = 8260.
        ENDELSE	
    Endif

    IF min(w) lt 3 then begin ; NIR
        startw = 1.275 & endw = 1.325
    ENDIF

    num = AVGFLUX(startw,endw,w,flux)
    flux=flux/num[0]
    if n_elements(sigma_flux) ne 0 then sigma_flux=sigma_flux/num[0]
ENDIF

IF n_elements(nregion) ne 0 then begin
    startw=nregion[0] 
    endw = nregion[1]
    num = AVGFLUX(startw,endw,w,flux)
    flux=flux/num[0]
    if n_elements(sigma_flux) ne 0 then sigma_flux=sigma_flux/num[0]
endif

if n_elements(sigma_flux) ne 0 then data=dblarr(3,size) else data=dblarr(2,size)

data[0,*]=w
data[1,*]=flux

if n_elements(sigma_flux) ne 0 then data[2,*]=sigma_flux

if  keyword_set(plotyes) then plot, data[0,*],data[1,*]

return,data

END
