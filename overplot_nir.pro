pro overplot_nir, spectrum,xr_key=xrnge, ps_key=ps, small=small, file_key=file_key
;giants=giants,young=young,ms=ms
;gemini_key=gem, lris_key=lris, sec=sec,


;+
; NAME:
;	OVERPLOT_NIR
;
; PURPOSE:
;       Creates plots of observed spectra with spectral standards overplotted
;
; CALLING SEQUENCE:
;
;	OVERPLOT, Filename, [ xr= , /ps, /Gemini,/Ms, /mine,/giants ]
;
; INPUTS:
;	Filename: String containing name of FITS file to be compared
;	          to spectral standards.
;
; OPTIONAL INPUTS:
;	xr: Two-element array with desired xrange of plot.
;           Default is 0.8-2.5 microns.
;
; KEYWORD PARAMETERS:
;	PS:     Set this keyword to get postscript output
;
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
;       kellepro:kreadspec
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
;       Modified for NIR based on type_spex.pro, Mar 2008.
; 	Written by:	Kelle Cruz, July 2005
;-


IF N_params() lt 1 then begin
     print,"Syntax - overplot, 'file.fits' (, xr=, /ps,/gem,,/lris,/M, /sec,/giants,/young,/small,/file)"
     print,'xr=[x1,x2] to set xrange'
     print,'/ps to produce postscript output'
;     print,'/gem to compare to gemini stds'     
;     print,'/lris to compare to LRIS data'     
;     print,'/giants to compare to giants'     
;     print,'/young to compare to young standards'    
;     print,'/sec to use secondary stds'     
     print, '/file to use filename instead of object name in header'
;     print, '/Ms to overplot M dwarfs instead of L dwarfs'
     print, '/small for small postscript output for lab notebook'
     print, ''
     return
ENDIF

IF FILE_TEST('/scr3/kelle/nir_spectra/2M_all/fits/')  then nir_path='/scr3/kelle/nir_spectra/2M_all/fits/'
IF FILE_TEST('/Users/kelle/Dropbox/Data/nir_spectra_low/')  then nir_path='/Users/kelle/Dropbox/Data/nir_spectra_low/'

;IF ~file_test(spectrum) then spectrum=spectrum+'.fits'
IF ~file_test(spectrum) then spectrum=nir_path+spectrum
IF ~file_test(spectrum) then begin
    message, 'File does not exist:' + spectrum
    ;return
endif else begin
    Message, 'Using File: '+spectrum, /info
endelse

if ~ Keyword_set(xrnge) then xrnge=[0.8,2.5]

;Make MINE default
;IF  keyword_set(gem) OR  keyword_set(lris) OR  keyword_set(sec)  OR keyword_set(young) THEN BEGIN
;    IF keyword_set(gem) THEN root='/scr/kelle/optical_spectra/standards/gemini/'
;    IF keyword_set(lris) THEN root='/scr/kelle/optical_spectra/standards/lris/'
;    IF keyword_set(sec) THEN     root='/scr/kelle/optical_spectra/standards/mine/dwarfs/secondary_stds/'
;    IF keyword_set(young) THEN root='/scr/kelle/optical_spectra/standards/young/'
;ENDIF ELSE BEGIN
;    root='/scr/kelle/optical_spectra/standards/mine/dwarfs/primary_stds/'
;ENDELSE
IF FILE_TEST('/scr1/nir_spectra/standards/') then sroot='/scr1/nir_spectra/standards/'
IF FILE_TEST('/Users/kelle/Data/nir_spectra_low/standards/') then sroot='/Users/kelle/Data/nir_spectra_low/standards/'

;IF keyword_set(M) or keyword_set(giants) THEN begin
;    data=KREADSPEC(spectrum,h,num,/norm,/M)
;ENDIF ELSE begin
    data=KREADSPEC(spectrum,h,num,/norm,/nir)
;ENDELSE
spec=data[1,*]
w=data[0,*]
print,max(w)


telescope = sxpar(h,'TELESCOP')
date_obs = sxpar(h,'DATE_OBS',/silent)
if SIZE(date_obs,/type) ne 7 then date_obs='' ; use empty string if no obs date in header
if SIZE(telescope,/type) ne 7 then telescope='' ; use empty string if no obs date in header

object=strtrim(sxpar(h,'OBJECT'),2)
if object eq '0' then file_key = 1 ;use filename if no object name in header
;file_name=(strsplit(spectrum,'.',/extract))[0]
;IF keyword_set(file_key) then object= file_name
file_name=strsplit(spectrum,'.fits',/extract,/regex)
if keyword_set(file_key) then obj_name = file_name[0] else $
  obj_name=object
if strmatch(obj_name,'spex_sxd_*') eq 1 then obj_name = strmid(obj_name,9,strlen(obj_name)-9)
if strmatch(obj_name,'spex_prism_*') eq 1 then obj_name = strmid(obj_name,11,strlen(obj_name)-11)
print,telescope
print,date_obs
print, object
print, obj_name

stds=file_search(sroot+'*_[M,L,T]?*.fits', count=nfiles)
pos=transpose(strpos(stds,'_L'))  

;IF keyword_set(Ms) THEN begin
;   stds=file_search(root+'*_[F,G,K,M]*.fits', count=nfiles)
   ;pos=transpose(strpos(stds,'_M'))
;   pos=transpose(strpos(stds,'.fits'))
;ENDIF 

;IF ~keyword_set(Ms) and  ~keyword_set(giants) then begin
;   stds=file_search(root+'*_[L,T]?*.fits', count=nfiles)
;   pos=transpose(strpos(stds,'_L'))
;ENDif

;if keyword_set(young) then begin
;    stds=file_search(root+'*_[K,M,L]?*.fits', count=nfiles)
;    pos=transpose(strpos(stds,'_',/REVERSE_SEARCH))
;endif

;if keyword_set(giants) then begin
;    root='/scr/kelle/optical_spectra/standards/mine/giants/'
;    stds=file_search(root+'*_[K,M,L]?*.fits', count=nfiles)
;    pos=transpose(strpos(stds,'_',/REVERSE_SEARCH))
;endif


;IF keyword_set(Ms) THEN begin
;    s=sort(strmid(stds,pos-2,2)) 
;ENDIF ELSE BEGIN
;    IF keyword_set(giants) OR  keyword_set(young) THEN  s=sort(strmid(stds,pos+1,3)) ELSE s=sort(strmid(stds,pos+2,2))
;ENDELSE
s=sort(strmid(stds,pos+2,2))
stds=stds[s]

PRINT, 'Which do you want to overplot?: '
FOR m=0, nfiles-1 do begin
	print, strn(m)+') '+stds[m]
ENDFOR
READ, use_file, PROMPT='Enter number of spectrum to overplot: '
IF Keyword_set(ps) THEN READ, use_file2, PROMPT='Enter number of second spectrum to compare: ' else use_file2=0

;IF keyword_set(Ms) THEN begin
;    data2=KREADSPEC(stds[use_file],h2,/M,/norm)
;    data3=KREADSPEC(stds[use_file2],h3,/M,/norm)
;ENDIF ELSE begin
    data2=KREADSPEC(stds[use_file],h2,/norm,/silent,/nir)
    data3=KREADSPEC(stds[use_file2],h3,/norm,/silent,/nir)
;ENDELSE

std=data2[1,*]
w2=data2[0,*]

std2=data3[1,*]
w3=data3[0,*]

std_name=file_basename(stds[use_file])
telescope_std = sxpar(h2,'TELESCOP')
date_obs_std = sxpar(h2,'DATE-OBS',count=datecount,/silent)

std_name2=file_basename(stds[use_file2])
telescope_std2 = sxpar(h3,'TELESCOP')
date_obs_std2 = sxpar(h3,'DATE-OBS',count=datecount,/silent)

flux_unit=SXPAR(h,'YUNITS', count=fluxcount)
;print, flux_unit

loadct, 39

plot, w, spec, xr=xrnge,xstyle=1, thick=2.0
oplot, w2, std, color=254 ;red
;oplot, [6708,6708],[0,5], linestyle=1

xyouts, 0.12, 0.55 ,obj_name, /normal, charsize=2
xyouts, 0.12, 0.5, telescope+'!C'+date_obs, /normal, align=0
if SIZE(telescope_std,/TNAME) eq 'INT' then telescope_std=''
xyouts, 0.12, 0.9, 'STD: '+std_name +'!C' + telescope_std, /normal, align=0

stop

IF Keyword_set(ps) THEN BEGIN
  set_plot, 'ps'
  device, encapsulated=0, /helvetica, /color

  if keyword_set(small) then begin
      device, filename=obj_name+'_oversm.ps', xsize=10.5/2, ysize=8.0/2,/inches,landscape=0, xoffset=0, yoffset=0.5
      xt=0.2
  ENDIF else begin
      device, filename=obj_name+'_over.ps', landscape=1, xsize=10.5, ysize=8.0,/inches, xoffset=0.2, yoffset=11
      xt=0.12
  endelse

  !p.font=0
  angstrom=STRING(197B)

  o1=0.0
  o2=o1+0.8
  o3=o2+0.8
  o4=o3+0.8

  w_plot=where(w2 ge xrnge[0] and w2 le xrnge[1])

  ym=max(std[w_plot]+o4+0.7)
;ym=o4+1

  xts=min(xrnge)+0.05

;TOP
  plot, w2,std+o4, xr=xrnge, yr=[0,ym],xstyle=1, ystyle=1,$
        xtitle='Wavelength (!4l!3m)',ytitle='Flux ('+flux_unit+')'

  xyouts, xts, o4+0.3, 'STD: '+std_name +'!C' + telescope_std , align=0

;MIDDLE OVERPLOT
  oplot, w, spec+o3, thick=3.0
  oplot, w2, std+o3, color=254    ;red

;MIDDLE DATA
  oplot, w, spec+o2, thick=3 ;middle

;BOTTOM STD2
  oplot, w3,std2+o1 ;bottom
  xyouts, xts, o1+0.3, 'STD: '+std_name2 +'!C' + telescope_std2 , align=0

;LABELS
  xyouts, xts, o2+0.7 ,obj_name,charsize=2
  xyouts, xt, 0.92, telescope+'!C'+date_obs, /normal, align=0
 
;  xyouts, xt, 0.85, 'STD: '+std_name +'!C' + telescope_std , /normal, align=0
 
;  if keyword_set(giants) then begin
;      feat, 1.2, /lowg
;  endif else begin
      feat, o3,o4
;  endelse

  device, /close
  set_plot,'x'

ENDIF

END

PRO feat, o3,o4
;,lowg=lowg

; FEAT, [shift , /lowg]
;IF n_elements(shift) EQ 0 THEN shift=0   

tk1 = 0.10 +o3
tk2 = 0.0+o3



;K
oplot, [0.7665,0.7665],[1.05,1.15]+tk1, linestyle=1
oplot, [0.7699,0.7699],[1.05,1.15]+tk1, linestyle=1
xyouts, 0.76,1.17+tk1,'K'

;TiO
oplot, [0.8432,0.86],[1.15,1.15]+tk1;, linestyle=1
oplot, [0.8432,0.8432],[1.10,1.15]+tk1
xyouts, 0.82,1.18+tk1,'TiO'

;CrH
oplot, [0.8611,0.88],[1.25,1.25]+tk1;, linestyle=1
oplot, [0.861,0.861],[1.20,1.25]+tk1
;oplot, [0.99,1.085],[2.0,2.0]+tk1, linestyle=1
xyouts, 0.855,1.28+tk1,'CrH'

;FeH
oplot, [0.8692,0.89],[1.4,1.4]+tk1;, linestyle=1
oplot, [0.8692,0.8692],[1.35,1.4]+tk1
;oplot, [0.99,1.085],[2.0,2.0]+tk1, linestyle=1
xyouts, 0.865,1.43+tk1,'FeH'


;H20
oplot, [0.91,0.95],[1.6,1.6]+tk1;, linestyle=1
xyouts, 0.915, 1.65+tk1,'H2O'

;FeH
oplot, [0.986,0.99],[2.0,2.0]+tk1;, linestyle=1
oplot, [0.986,0.986],[1.95,2.0]+tk1
oplot, [0.99,1.085],[2.0,2.0]+tk1, linestyle=1
xyouts, 0.985,2.05+tk1,'FeH'

;VO
oplot, [1.05,1.08],[1.75,1.75]+tk1;, linestyle=1
xyouts, 1.05,1.80+tk1,'VO'

;Na
oplot, [1.138,1.138],[0,1.85]+tk1, linestyle=1
oplot, [1.141,1.141],[0,1.85]+tk1, linestyle=1
xyouts, 1.13,1.9+tk1,'Na'

;K
oplot, [1.169,1.169],[0,1.85]+tk1, linestyle=1
oplot, [1.178,1.178],[0,1.85]+tk1, linestyle=1
xyouts, 1.17,1.87+tk1,'K'

;FeH
oplot, [1.19,1.205],[2.0,2.0]+tk1;, linestyle=1
oplot, [1.19,1.19],[1.95,2.0]+tk1
xyouts, 1.185,2.05+tk1,'FeH'

;FeH
oplot, [1.24,1.27],[2.15,2.15]+tk1;, linestyle=1
oplot, [1.24,1.24],[2.10,2.15]+tk1
oplot, [1.27,1.33],[2.15,2.15]+tk1, linestyle=1
xyouts, 1.23,2.20+tk1,'FeH'

;K
oplot, [1.243,1.243],[0,2.0]+tk1, linestyle=1
oplot, [1.252,1.252],[0,2.0]+tk1, linestyle=1
xyouts, 1.243,2.03+tk1,'K'

;PaB


;H2O
oplot, [1.3,1.51],[2.1,2.1]+tk1;, linestyle=1
xyouts, 1.38, 2.15+tk1,'H2O'

;K
oplot, [1.517,1.517],[1.75,1.9]+tk1, linestyle=1
xyouts, 1.51,1.95+tk1,'K'

;FeH
oplot, [1.59,1.75],[2.05,2.05]+tk2, linestyle=1
xyouts, 1.65,2.1+tk2,'FeH'

;H2O
oplot, [1.75,2.05],[1.85,1.85]+tk2;, linestyle=1
xyouts, 1.85, 1.9+tk2,'H2O'

;Ca
;oplot, [1.93,1.99],[0,1.7]+tk2, linestyle=1
;xyouts, 1.94, 1.75+tk2,'Ca'

;Na
oplot, [2.206,2.206],[0,1.8]+tk2, linestyle=1
oplot, [2.2089,2.2089],[0,1.8]+tk2, linestyle=1
xyouts, 2.19, 1.83+tk2,'Na'

;CO
oplot, [2.29,2.35],[1.65,1.65]+tk2;, linestyle=1
xyouts, 2.3, 1.70+tk2,'CO'

;H2O
oplot, [2.3,3.2],[1.8,1.8]+tk2;, linestyle=1
xyouts, 2.3, 1.85+tk2,'H2O'


 ;Ch4
;oplot, [1.1,1.24],[1.3,1.3]+tk1+0.5     ;, linestyle=1
;xyouts, 1.14,1.35+tk1 ,'CH4'

 ;Ch4
;oplot, [1.6,1.8],[1.3,1.3]  +tk1+0.5    ;, linestyle=1
;xyouts, 1.67,1.35+tk1 ,'CH4'

 ;Ch4
;oplot, [2.15,2.5],[1.2,1.2] +tk1+0.5    ;, linestyle=1
;xyouts, 2.3, 1.25+tk1 ,'CH4'
 

END

