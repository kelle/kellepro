PRO rv_phx_v2,file,srad1,srad2,comment, ps=ps,m=m

; 2010 July 19:  added delta log lambda correction
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

;srad is how many pix to use to fit polynomial
if n_elements(srad1) eq 0 then srad1 = 1
if n_elements(srad2) eq 0 then srad2 = 1
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
vhb=sxpar(hb,'vhelio',count=nvhb)

;print, 'vhelio (km/s): '+ strn(vha1)+ ' ' + strn(vha2)+' '+ strn(vhb)
cspeed   = 2.99792458E5       ; km/s
h_offseta1=(vha1/2.99792458e5 +1.0D)
h_offseta2=(vha2/2.99792458e5 +1.0D)
h_offsetb=(vhb/2.99792458e5 +1.0D)
awh1=a1[0,*]*h_offseta1
awh2=a2[0,*]*h_offseta2
bwh=b[0,*]*h_offsetb

af1=a1[1,*]
af2=a2[1,*]
bf=b[1,*]

;find where a and b overlap

wmin1 = max([min(awh1),min(bwh)])
wmax1 = min([max(awh1),max(bwh)])
good_a1=where(awh1 ge wmin1 and awh1 le wmax1,na1)
good_b1=where(bwh ge wmin1 and bwh le wmax1,nb1,comp=bad_b1)

wmin2 = max([min(awh2),min(bwh)])
wmax2 = min([max(awh2),max(bwh)])
good_a2=where(awh2 ge wmin2 and awh2 le wmax2,na2)
good_b2=where(bwh ge wmin2 and bwh le wmax2,nb2,comp=bad_b2)

num1=max([na1,nb1])
num2=max([na2,nb2])

wg1=awh1[good_a1]
afg1=af1[good_a1]
bwg1=bwh[good_b1]
bfg1=bf[good_b1]

wg2=awh2[good_a2]
afg2=af2[good_a2]
bwg2=bwh[good_b2]
bfg2=bf[good_b2]

; --- Resample to constant spacing in log lambda space: v/c = d(ln lambda)
  
  acoef1  = float(num1 - 1)/(alog(wmax1) - alog(wmin1))
  bcoef1  = float(num1) - (acoef1 * (alog(wmax1)))

  acoef2  = float(num2 - 1)/(alog(wmax2) - alog(wmin2))
  bcoef2  = float(num2) - (acoef2 * (alog(wmax2)))

  xpon1   = findgen(num1)+1.0
  wx1    = exp((xpon1-bcoef1)/acoef1)

  xpon2   = findgen(num2)+1.0
  wx2    = exp((xpon2-bcoef2)/acoef2)

;put b on same d lon lambda wavelength scale as standards but doesnt shift
  fxa1 = interpol(afg1,wg1,wx1) 
  fxb1 = interpol(bf,bwh,wx1)

  fxa2 = interpol(afg2,wg2,wx2) 
  fxb2 = interpol(bf,bwh,wx2)

;bfi_a1=interpol(bfg1,bwg1,wg1)
;bfi_a2=interpol(bfg2,bwg2,wg2)
;linterp,reform(bwg1),reform(bfg1),reform(wg1),bfi_a1
;linterp,reform(bwg2),reform(bfg2),reform(wg2),bfi_a2

!p.multi=[0,2,2,0,1]
xsize=21
ysize=xsize/2/1.5 * 2 ;2 for two plots
aspect_ratio=ysize/xsize
scale=1.0
xsize_window=600*1.8

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

ba_shift1=ccpeak_kc(fxb1,fxa1,25,srad1)
;ba_shift1=ccpeak_kc(bfi_a1,afg1,25,srad1)
ba_shift1_avg = (ba_shift1[0] + ba_shift1[1])/2

;velocity of b wrt a1
vshift_b1 = (cspeed * ba_shift1_avg)/acoef1
;vshift_b1 = ba_shift1_avg * pixkm
vshift_b1_unc =  ABS(ba_shift1[0] - ba_shift1[1])* pixkm

vrad_b1 = rv_a1 - vshift_b1
vrad_b1_unc = rv_a1_unc + vshift_b1_unc

rv_offsetb1=(vshift_b1/cspeed +1.0D)
rv_wg1 = wg1 * rv_offsetb1
rv_bwh1= bwh * rv_offsetb1

;plot spectra (bottom panel)
plot, wg1, afg1, /nodata,/ynozero,xr=[1.551,1.5585],xstyle=1,xmargin=[4,2],ymargin=[2,2],yrange=[0.4,1.6],ystyle=1
if bad_b1[0] ne -1 then oplot, rv_bwh1[bad_b1], bf[bad_b1],color=green,linestyle=1
oplot, wg1, afg1, color=red, linestyle=1
oplot, rv_wg1, bfg1

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

;radius needs to be larger because rv of std is 45km/s
;ba_shift2=ccpeak_kc(fxb2,fxa2,25,srad2)
ba_shift2=ccpeak_kc(fxb2,fxa2,30,srad2)
;ba_shift2=ccpeak_kc(bfi_a2,afg2,30,srad2)
ba_shift2_avg = (ba_shift2[0] + ba_shift2[1])/2

;velocity of b wrt a2
vshift_b2 = (cspeed * ba_shift2_avg)/acoef2
;vshift_b2 = ba_shift2_avg * pixkm
vshift_b2_unc = ABS(ba_shift2[0] - ba_shift2[1]) *pixkm

vrad_b2 = rv_a2 - vshift_b2
vrad_b2_unc = rv_a2_unc + vshift_b2_unc

rv_offsetb2=(vshift_b2/2.99792458e5 +1.0D)
rv_wg2=wg2*rv_offsetb2
rv_bwh2=bwh*rv_offsetb2

plot,wg2,afg2,/nodata,/ynozero,xr=[1.551,1.5585],xstyle=1,xmargin=[4,2],ymargin=[2,2],yrange=[0.4,1.6],ystyle=1
if bad_b2[0] ne -1 then oplot,rv_bwh2[bad_b2],bf[bad_b2],color=green,linestyle=1
oplot,wg2,afg2,color=red,linestyle=1
oplot,rv_wg2,bfg2

comment3=comment + '  srad2='+ strn(srad2) 
xyouts, 0.56,0.90, 'Comment: ' + comment3 +'!C'+ obj_name + '!C'+ 'obs date: ' + obj_date +'!C' +$
	'std: ' + std2_name +'!C'+$
	'vrad_b= '+strn(vrad_b2,format='(f6.2)')  + pm + strn(vrad_b2_unc,format='(f6.2)') +' km/s',$
	/normal,charsize=0.9

print, 'vrad_b2= '+ strn(vrad_b2) + ' km/s'

;---
;average of 2
vrad= (vrad_b1 + vrad_b2)/2
;vrad = (vrad_b1/vrad_b1_unc^2  + vrad_b2/vrad_b2_unc^2) / (1/vrad_b1_unc^2 + 1/vrad_b2_unc^2)
;variance of sample of two
vrad_unc= SQRT( ((vrad_b1-vrad)^2 + (vrad_b2 - vrad)^2)/2)
print, 'vrad_avg:'+ strn(vrad) + ' '+pm+' ' + strn(vrad_unc)
xyouts,0.01,0.97, obj_name,/normal,charsize=1.5
xyouts,0.5,0.97, 'vrad= '+ strn(vrad,format='(f6.2)') + pm + $
	strn(vrad_unc,format='(f6.2)')+' km/s',/normal,align=0.5,charsize=1.5

if nvhb eq 0 then begin
	 message,'no vhelio correction',/info
	xyouts,0.5,0.93,'no vhelio correction',/normal,align=0.5,charsize=1.5,color=red
endif
if nvhb ge 1 then message,'two many vhelio corrections',/info

IF KEYWORD_SET(ps) THEN BEGIN
 device,/close
 set_plot,'x'
 !p.font=-1 ;go back to default (Vector Hershey fonts)
ENDIF

!p.multi=0

END
