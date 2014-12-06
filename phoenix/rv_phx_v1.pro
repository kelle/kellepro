PRO rv_phx,file,srad1,srad2,comment, ps=ps,m=m

;written Dec 2009, K. Cruz

if N_params() lt 1 then begin
     print,'Syntax -  rv_phx,file,srad1,srad2,comment,ps=ps,m=m'
     print,'useful information'
     return
endif

root1='/scr1/nir_highres/standards/'
root2='~/Analysis/nir_highres/standards/'
if File_test(root1) then root=root1
if file_test(root2) then root=root2
if ~file_test(file) then file='combined.fits'

if n_elements(srad1) eq 0 then srad1 = 2
if n_elements(srad2) eq 0 then srad2 = 2
if n_elements(comment) eq 0 then comment = ''


@colors_kc
device,decomposed=0
IF keyword_set(m) then begin
	a1=kreadspec(root+'lhs292_49_M65_phx',ha1,/silent)
	rv_a1=1.8
	rv_a1_unc = 0.5
	std1_name='LHS292'
endif else begin
	a1=kreadspec(root+'2M0523-14_L25_phx',ha1,/silent)
	rv_a1=11.82
	rv_a1_unc = 0.16
	std1_name='2M0523'
ENDELSE
IF keyword_set(m) then begin
	a2=kreadspec(root+'bri0021_M95_phx',ha2,/silent)
	rv_a2=20.
	rv_a2_unc = 5.
	std2_name='BRI 0021'
endif else begin
	a2=kreadspec(root+'2M1155-37_L20_phx',ha2,/silent)
	rv_a2=45.63
	rv_a2_unc=0.13
	std2_name='2M1155'
endelse

b=kreadspec(file,hb,/silent)
obj_name=SXPAR(hb,'OBJECT')
obj_date=strtrim(SXPAR(hb,'UTDATE'))

;p 44 lab notebook 3
;v=Delta lambda/avg lambda * c
a_end=(size(a1))[2]-1
pixkm=(a1[0,1]-a1[0,0])/((a1[0,0]+a1[0,a_end])/2)*2.9979e5

;helio correct
;p 31, notebook 3
vha1=sxpar(ha1,'vhelio')
vha2=sxpar(ha2,'vhelio')
vhb=sxpar(hb,'vhelio')

h_offseta1=(vha1/2.99792458e5 +1.0D)
h_offseta2=(vha2/2.99792458e5 +1.0D)
h_offsetb=(vhb/2.99792458e5 +1.0D)
awh1=a1[0,*]*h_offseta1
awh2=a2[0,*]*h_offseta2
bwh=b[0,*]*h_offsetb

af1=a1[1,*]
af2=a2[1,*]
bf=b[1,*]

;print,strn(h_offseta) + ' ' + strn(h_offsetb)
;print, 'vhelio (km/s): '+ strn(vha)+ ' ' + strn(vhb)

;put b on same wavelength as standards but doesnt shift
bfi_a1=interpol(bf,bwh,awh1)
bfi_a2=interpol(bf,bwh,awh2)

;find where a and b overlap
good1=where(finite(bfi_a1) eq 1 and bfi_a1 gt 0)
good2=where(finite(bfi_a2) eq 1 and bfi_a2 gt 0)

;trim arrays
wg1=awh1[good1]
afg1=af1[good1]
bfg1=bfi_a1[good1]

wg2=awh2[good2]
afg2=af2[good2]
bfg2=bfi_a2[good2]

!p.multi=[0,2,2,0,1]
xsize=21
ysize=xsize/2/1.5 * 2 ;2 for two plots
aspect_ratio=ysize/xsize
scale=1.0
xsize_window=600*2

if keyword_set(ps) then begin
    !p.font=0
    set_plot,'ps'
    device, file=strtrim(obj_name,2)+'_'+obj_date+'_rvplots.ps',/helvetica,/isolatin1,color=1
    device,xsize=xsize,ysize=ysize,scale=scale,xoffset=0.25,yoffset=1
	@symbols_ps_kc
	message,'Wrote: '+strtrim(obj_name,2)+'_'+obj_date+'_rvplots.ps',/info
endif else begin
    set_plot,'x'
    device,decomposed=0
    window,2, xsize=xsize_window,ysize=xsize_window*aspect_ratio
	@symbols_kc
endelse

;x-correlate b with a1
;also makes x-correl top panel
ba_shift1=ccpeak_kc(bfg1,afg1,25,srad1)
ba_shift1_avg = (ba_shift1[0] + ba_shift1[1])/2

;velocity of b wrt a1
vshift_b1 = ba_shift1_avg * pixkm
vshift_b1_unc =  ABS(ba_shift1[0] - ba_shift1[1])* pixkm
;print, vshift_b1_unc

vrad_b1 = rv_a1 - vshift_b1
vrad_b1_unc = rv_a1_unc + vshift_b1_unc
;print, rv_a1_unc

;plot spectra (bottom panel)
plot,wg1,afg1,/nodata,/ynozero,xr=[1.551,1.558],xstyle=1,xmargin=[4,2],ymargin=[2,2]
oplot,wg1,afg1,color=red,linestyle=1
oplot, wg1,shift(bfg1,ba_shift1_avg)

comment2= comment + '   srad1=' + strn(srad1); + 'srad2='+ strn(srad2) 
xyouts, 0.06,0.90, 'Comment: ' + comment2 +'!C'+ obj_name + '!C'+ 'obs date: ' + obj_date +'!C'+$
		'std: ' + std1_name +'!C'+$
		'vrad_b= '+strn(vrad_b1,format='(f6.2)')  + pm + strn(vrad_b1_unc,format='(f6.2)') +' km/s',$
		/normal,charsize=0.9

;print, 'pixel scale= ' + strn(pixkm) + ' km/pixel '
;print, 'vshift_b= '+strn(vshift_b)  + ' km/s'
print, 'vrad_b1= '+ strn(vrad_b1)  + ' km/s'
;- - - - - - -
;x b with a2

ba_shift2=ccpeak_kc(bfg2,afg2,40,srad2)
ba_shift2_avg = (ba_shift2[0] + ba_shift2[1])/2

;velocity of b wrt a2
vshift_b2 = ba_shift2_avg * pixkm
vshift_b2_unc = ABS(ba_shift2[0] - ba_shift2[1]) *pixkm

vrad_b2 = rv_a2 - vshift_b2
vrad_b2_unc = rv_a2_unc + vshift_b2_unc

plot,wg2,afg2,/nodata,/ynozero,xr=[1.551,1.558],xstyle=1,xmargin=[4,2],ymargin=[2,2]
oplot,wg2,afg2,color=red,linestyle=1
oplot,wg2,shift(bfg2,ba_shift2_avg)

comment3=comment + '  srad2='+ strn(srad2) 
xyouts, 0.56,0.90, 'Comment: ' + comment3 +'!C'+ obj_name + '!C'+ 'obs date: ' + obj_date +'!C' +$
	'std: ' + std2_name +'!C'+$
	'vrad_b= '+strn(vrad_b2,format='(f6.2)')  + pm + strn(vrad_b2_unc,format='(f6.2)') +' km/s',$
	/normal,charsize=0.9

print, 'vrad_b2= '+ strn(vrad_b2) + ' km/s'


;---
vrad_unc = 1/SQRT(1/vrad_b1_unc^2 + 1/vrad_b2_unc^2)
vrad = (vrad_b1/vrad_b1_unc^2  + vrad_b2/vrad_b2_unc^2) / (1/vrad_b1_unc^2 + 1/vrad_b2_unc^2)
print, strn(vrad) + ' ' + strn(vrad_unc)
xyouts,0.5,0.95, 'vrad= '+ strn(vrad,format='(f6.2)') + pm + strn(vrad_unc,format='(f6.2)')+' km/s',/normal,align=0.5

IF KEYWORD_SET(ps) THEN BEGIN
 device,/close
 set_plot,'x'
 !p.font=-1 ;go back to default (Vector Hershey fonts)
ENDIF

!p.multi=0

END
