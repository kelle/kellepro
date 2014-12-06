pro overplot, spectrum,xr_key=xrnge, ps_key=ps, gemini_key=gem, lris_key=lris, file_key=file_key,ms=ms,sec=sec,small=small

;+
; NAME:
;	OVERPLOT
;
; PURPOSE:
;       Creates plots of observed spectra with spectral standards overplotted
;
; CALLING SEQUENCE:
;
;	OVERPLOT, Filename, [ xr= , /ps, /Gemini,/Ms, /mine ]
;
; INPUTS:
;	Filename: String containing name of FITS file to be compared
;	          to spectral standards.
;
; OPTIONAL INPUTS:
;	xr: Two-element array with desired xrange of plot.
;           Default is 6000-11000 Angstroms.
;
; KEYWORD PARAMETERS:
;	PS:     Set this keyword to get postscript output
;
;	GEMINI:	Set this keyword to compare to Gemini standards
;	instead of LRIS
;
; OUTPUTS:
;       Displays to the screen the data (black) and spectral standard
;       in red.
;
; OPTIONAL OUTPUTS:
;	If the PS keyword is set, a postscript file will be generated.
;
; RESTRICTIONS:
;	Describe any "restrictions" here.  Delete this section if there are
;	no important restrictions.
;
; ROUTINES CALLED:
;       READFITS
;       SXPAR
;       AVGFLUX
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
;
; MODIFICATION HISTORY:
; 	Written by:	Kelle Cruz, July 2005
;-


IF N_params() lt 1 then begin
     print,"Syntax - overplot, 'file.fits' (, xr=, /ps,/gem,/file, /M, /mine)"
     print,'xr=[x1,x2] to set xrange'
     print,'/ps to produce postscript output'
     print,'/gem to compare to gemini stds'     
     print,'/lris to compare to LRIS data'     
     print,'/sec to use secondary stds'     
;     print,'/mine to use my stds instead of LRIS'
     print, '/file to use filename instead of object name in header'
     print, '/Ms to overplot M dwarfs instead of L dwarfs'
     print, '/small for small postscript output for lab notebook'
     print, ''
     return
ENDIF

if ~ Keyword_set(xrnge) then xrnge=[6000,10100]

;Make MINE default
IF  keyword_set(gem) OR  keyword_set(lris) OR  keyword_set(sec) THEN BEGIN
    IF keyword_set(gem) THEN root='/data/hillary/2/kelle/optical_spectra/standards/gemini/'
    IF keyword_set(lris) THEN root='/data/hillary/2/kelle/optical_spectra/standards/lris/'
    IF keyword_set(sec) THEN     root='/data/hillary/2/kelle/optical_spectra/standards/mine/dwarfs/secondary_stds/
ENDIF ELSE BEGIN
    root='/data/hillary/2/kelle/optical_spectra/standards/mine/dwarfs/primary_stds/'
ENDELSE

;print, keyword_set(gem),  keyword_set(mine) 
;print, root

IF keyword_set(M) THEN begin
    data=KREADSPEC(spectrum,h,/norm,/M)
ENDIF ELSE begin
    data=KREADSPEC(spectrum,h,/norm)
ENDELSE
spec=data[1,*]
w=data[0,*]

telescope = sxpar(h,'TELESCOP')
date_obs = sxpar(h,'DATE-OBS',count=datecount,/silent)

object=strtrim(sxpar(h,'OBJECT'),2)
file_name=(strsplit(spectrum,'.',/extract))[0]
IF keyword_set(file_key) then object_file = file_name else $
              object_file = object

;IF keyword_set(file_key) THEN BEGIN
;   object=(strsplit(spectrum,'.',/extract))[0]
;ENDIF ELSE BEGIN   
;   object=strtrim(sxpar(h,'OBJECT'),2)
;ENDELSE

;dim=SXPAR(h,'NAXIS3')
;if dim ne 0 then spec=spec[*,*,0]
;size=n_elements(spec)
;w=findgen(size)*SXPAR(h,'CD1_1')+SXPAR(h,'CRVAL1')-SXPAR(h,'LTV1')*SXPAR(h,'CD1_1')
;w=findgen(n_elements(spec))*SXPAR(h,'CD1_1')+SXPAR(h,'CRVAL1')
;pix_scale=SXPAR(h,'CD1_1')*1.
;IF keyword_set(M) THEN begin
;   startw = 8080. & endw = 8155. ; for Ms
;ENDIF ELSE begin
;   startw = 8240. & endw = 8260. ; for Ls
;ENDELSE
;a = where(w ge startw-pix_scale/2 AND w le endw+pix_scale/2)
;num = avgflux(startw,endw,w(a),spec(a))
;specn=spec/num

IF keyword_set(Ms) THEN begin
   stds=file_search(root+'*_M*.fits', count=nfiles)
   pos=transpose(strpos(stds,'_M'))
ENDIF ELSE begin
   stds=file_search(root+'*_[L,T]?*.fits', count=nfiles)
   pos=transpose(strpos(stds,'_L'))
ENDELSe

s=sort(strmid(stds,pos+2,1))
stds=stds[s]

PRINT, 'Which do you want to overplot?: '
FOR m=0, nfiles-1 do begin
	print, strn(m)+') '+stds[m]
ENDFOR
READ, use_file, PROMPT='Enter number of file to use: '

;std=readfits(stds[use_file],h2)
IF keyword_set(Ms) THEN begin
    data2=KREADSPEC(stds[use_file],h2,/M,/norm)
ENDIF ELSE begin
    data2=KREADSPEC(stds[use_file],h2,/norm)
ENDELSE
std=data2[1,*]
w2=data2[0,*]


std_name=file_basename(stds[use_file])
telescope_std = sxpar(h2,'TELESCOP')
date_obs_std = sxpar(h2,'DATE-OBS',count=datecount,/silent)

;if (size(std))[0] ne 1 then std = std(*,0,0)
;w2=findgen(n_elements(std))*SXPAR(h2,'CD1_1')+SXPAR(h2,'CRVAL1')
;pix_scale2=SXPAR(h2,'CD1_1')*1.
;b = where(w2 ge startw-pix_scale2/2 AND w2 le endw+pix_scale2/2)
;num2 = avgflux(startw,endw,w2(b),std(b))
;stdn=std/num2

loadct, 39

plot, w, spec, xr=xrnge,xstyle=1, thick=2.0
oplot, w2, std, color=254 ;red
oplot, [6708,6708],[0,5], linestyle=1

xyouts, 0.12, 0.55 ,object, /normal, charsize=2
xyouts, 0.12, 0.5, telescope+'!C'+date_obs, /normal, align=0
if SIZE(telescope_std,/TNAME) eq 'INT' then telescope_std=''
xyouts, 0.12, 0.9, 'STD: '+std_name +'!C' + telescope_std, /normal, align=0

if Keyword_set(ps) then begin
  set_plot, 'ps'
  device, encapsulated=0, /helvetica, /color

  if keyword_set(small) then begin
      device, filename=object_file+'_oversm.ps', xsize=10.5/2, ysize=8.0/2,/inches,landscape=0, xoffset=0, yoffset=0.5
      xt=0.2
  ENDIF else begin
      device, filename=object_file+'_over.ps', landscape=1, xsize=10.5, ysize=8.0,/inches, xoffset=0.2, yoffset=11
      xt=0.12
  endelse

  !p.font=0
  angstrom=STRING(197B)

  plot, w, spec, xr=xrnge,xstyle=1, thick=2.0, xtitle='Wavelength'
  oplot, w2, std, color=254    ;red

  xyouts, xt, 0.55 ,object, /normal,charsize=2
  xyouts, xt, 0.5, telescope+'!C'+date_obs, /normal, align=0
 
  xyouts, xt, 0.85, 'STD: '+std_name +'!C' + telescope_std , /normal, align=0
 
  device, /close
  set_plot,'x'
endif

END
