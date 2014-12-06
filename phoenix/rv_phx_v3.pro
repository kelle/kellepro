PRO rv_phx,file,srad1,srad2,comment, ps=ps,m=m

;TODO 
; figure out histogram
; gaussfit distribution?
; loop progress from Cushing
; make plots

; 2010 July 20: monte carlo the RV measurement
; 2010 July 19:  added delta log lambda correction
; written Dec 2009, K. Cruz

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

;b=kreadspec(file,hb,/silent)
b=readfits(file,hb,/silent)

obj_name=SXPAR(hb,'OBJECT')
obj_date=strtrim(SXPAR(hb,'UTDATE'))

;p 44 lab notebook 3
;v=Delta lambda/avg lambda * c
;a_end=(size(a1))[2]-1
;pixkm=(a1[0,1]-a1[0,0])/((a1[0,0]+a1[0,a_end])/2)*2.9979e5

;helio correct
;p 31, notebook 3
vha1=sxpar(ha1,'vhelio')
vha2=sxpar(ha2,'vhelio')
vhb=sxpar(hb,'vhelio',count=nvhb)
if vhb eq 0 then vhb=17.1

;print, 'vhelio (km/s): '+ strn(vha1)+ ' ' + strn(vha2)+' '+ strn(vhb)
cspeed   = 2.99792458E5       ; km/s
h_offseta1=(vha1/cspeed +1.0D)
h_offseta2=(vha2/cspeed +1.0D)
h_offsetb=(vhb/cspeed +1.0D)

aw1=reform(a1[0,*])*h_offseta1
aw2=reform(a2[0,*])*h_offseta2
bw=reform(b[0,*])*h_offsetb

af1=reform(a1[1,*])
asig1=af1/100
af2=reform(a2[1,*])
asig2=af2/100
bf=reform(b[1,*])
bsig=reform(b[2,*])

;find where a and b overlap
wmin1 = max([min(aw1),min(bw)])
wmax1 = min([max(aw1),max(bw)])
good_a1=where(aw1 ge wmin1 and aw1 le wmax1,na1)
good_b1=where(bw ge wmin1 and bw le wmax1,nb1,comp=bad_b1)

wmin2 = max([min(aw2),min(bw)])
wmax2 = min([max(aw2),max(bw)])
good_a2=where(aw2 ge wmin2 and aw2 le wmax2,na2)
good_b2=where(bw ge wmin2 and bw le wmax2,nb2,comp=bad_b2)

num1=max([na1,nb1])
num2=max([na2,nb2])

bwtmp1 = bw[good_b1]
bwtmp2 = bw[good_b2]
awtmp1 = aw1[good_a1]
awtmp2 = aw2[good_a2]

;-------------------------
 !p.multi=[0,2,2,0,1]
;!p.multi=[0,2,2]
 xsize=21
 ysize=xsize/2/1.5 * 2 ;2 for two plots
 aspect_ratio=ysize/xsize
 scale=1.0
 xsize_window=600*1.7

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

;-------------------------
nloops=300
vrad_arr1=fltarr(nloops)
vrad_arr2=fltarr(nloops)

for i=0,nloops-1 do begin
	mc_loopprogress,i,0,nloops-1,modval=nloops/10.
	
	bftmp1 = bf[good_b1] + (bsig[good_b1] * randomn(seed,nb1,/normal))
	bftmp2 = bf[good_b2] + (bsig[good_b2] * randomn(seed,nb2,/normal))
	aftmp1 = af1[good_a1] + (asig1[good_a1] * randomn(seed,na1,/normal))
	aftmp2 = af2[good_a2] + (asig2[good_a2] * randomn(seed,na2,/normal))
        
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
  fxa1 = interpol(aftmp1,awtmp1,wx1) 
  fxb1 = interpol(bftmp1,bwtmp1,wx1)

  fxa2 = interpol(aftmp2,awtmp2,wx2) 
  fxb2 = interpol(bftmp2,bwtmp2,wx2)

;x-correlate b with a1
;also makes x-correl top panel
	if i mod 100 eq 0 then plot=1 else plot=0
    ba_shift1=ccpeak_kc(fxb1,fxa1,40,srad1,plot=plot)

   ba_shift1_avg = (ba_shift1[0] + ba_shift1[1])/2
   
   ;velocity of b wrt a1
   vshift_b1 = (cspeed * ba_shift1_avg)/acoef1
   ;vshift_b1 = (cspeed * ba_shift1[0])/acoef1
   vshift_b1_unc =  cspeed*(ABS(ba_shift1[0] - ba_shift1[1]))/acoef1
   
   vrad_b1 = rv_a1 - vshift_b1
   ;vrad_b1_unc = rv_a1_unc + vshift_b1_unc
	vrad_b1_unc = 0
   
   vrad_arr1[i]=vrad_b1

	if i mod 100. eq 0 then begin
		rv_offsetb1=(vshift_b1/cspeed +1.0D)
		;rv_w1 = aw1 * rv_offsetb1
		rv_bw1= bw * rv_offsetb1
		
		;plot spectra (bottom panel)
		plot, aw1, af1, /nodata,/ynozero,xr=[1.551,1.5585],xstyle=1,xmargin=[4,2],ymargin=[2,2],yrange=[0.4,1.6],ystyle=1
		if bad_b1[0] ne -1 then oplot, rv_bw1[bad_b1], bf[bad_b1],color=green,linestyle=1
		oplot, aw1, af1, color=red, linestyle=1
		oplot, rv_bw1, bftmp1

		 comment2= comment + '   srad1=' + strn(srad1); + 'srad2='+ strn(srad2) 
		 xyouts, 0.06,0.90, 'Comment: ' + comment2 +'!C'+ obj_name + '!C'+ 'obs date: ' + obj_date +'!C'+$
		 		'std: ' + std1_name +'!C'+$
		 		'vrad_b= '+strn(vrad_b1,format='(f6.2)')  + pm + strn(vrad_b1_unc,format='(f6.2)') +' km/s',$
		 		/normal,charsize=0.9
			;print, 'vrad_b1= '+ strn(vrad_b1)  + ' km/s'
			xyouts,0.5,0.5,'i='+strn(i),/normal
	endif

	


;- - - - - - -
;x b with a2

	if i mod 100 eq 0 then plot =1 else plot=0
   ;radius needs to be larger because rv of std is 45km/s
   ba_shift2=ccpeak_kc(fxb2,fxa2,35,srad2,plot=plot)
   ;ba_shift2=ccpeak_kc(fxb2,fxa2,30,srad2)
   ba_shift2_avg = (ba_shift2[0] + ba_shift2[1])/2
   
   ;velocity of b wrt a2
   vshift_b2 = (cspeed * ba_shift2_avg)/acoef2
   ;vshift_b2 = (cspeed * ba_shift2[0])/acoef2
;   vshift_b2_unc = ABS(ba_shift2[0] - ba_shift2[1]) *pixkm
   
   vrad_b2 = rv_a2 - vshift_b2
;   vrad_b2_unc = rv_a2_unc + vshift_b2_unc
 vrad_b2_unc = 0
   
   vrad_arr2[i]=vrad_b2

	if i mod 100 eq 0 then begin
		rv_offsetb2=(vshift_b2/cspeed +1.0D)
		;rv_wg2=wg2*rv_offsetb2
		rv_bw2=bw*rv_offsetb2
	 
		plot,aw2,af2,/nodata,/ynozero,xr=[1.551,1.5585],xstyle=1,xmargin=[4,2],ymargin=[2,2],yrange=[0.4,1.6],ystyle=1
		if bad_b2[0] ne -1 then oplot,rv_bw2[bad_b2],bf[bad_b2],color=green,linestyle=1
		oplot,aw2,af2,color=red,linestyle=1
		oplot,rv_bw2,bftmp2

		comment3=comment + '  srad2='+ strn(srad2) 
		xyouts, 0.56,0.90, 'Comment: ' + comment3 +'!C'+ obj_name + '!C'+ 'obs date: ' + obj_date +'!C' +$
			'std: ' + std2_name +'!C'+$
			'vrad_b= '+strn(vrad_b2,format='(f6.2)')  + pm + strn(vrad_b2_unc,format='(f6.2)') +' km/s',$
			/normal,charsize=0.9
		
		;print, 'vrad_b2= '+ strn(vrad_b2) + ' km/s'
	endif
	
ENDFOR

;resistant_mean,vrad_arr1,2,vrad1,vrad_unc1
;resistant_mean,vrad_arr2,2,vrad2,vrad_unc2
;vrad1=(resistant_mean(vrad_arr1))[0]
;vrad_unc1=sqrt((moment(vrad_arr1))[1])
;vrad2=(median(vrad_arr2))[0]
;vrad_unc2=sqrt((moment(vrad_arr2))[1])

!p.multi=[0,1,2]
window,3
mc_histogram,vrad_arr1,rvhist_x1,rvhist_y1,binsize=0.5
fit1 = gaussfit(rvhist_x1,rvhist_y1,fitparms1,sigma=sigma1)
vrad1=fitparms1[1]
fwhm1 = 2*SQRT(2*ALOG(2))*fitparms1[2]
height1=fitparms1[0]/2

plot,rvhist_x1,rvhist_y1,psym=10
oplot,[vrad1,vrad1],[0,100]
oplot,rvhist_x1,fit1
oplot,[vrad1-fwhm1/2,vrad1+fwhm1/2],[fitparms1[0]/2,fitparms1[0]/2]
xyouts,vrad1,10,strn(vrad1)
xyouts,vrad1+fwmh1,height1,strn(vrad)+ '!C' + strn(fwhm1/2)

mc_histogram,vrad_arr2,rvhist_x2,rvhist_y2,binsize=0.5
fit2=gaussfit(rvhist_x2,rvhist_y2,fitparms2,sigma=sigma2)
vrad2=fitparms2[1]
fwhm2 = 2*SQRT(2*ALOG(2))*fitparms2[2]
height2=fitparms2[0]/2

plot,rvhist_x2,rvhist_y2,psym=10
oplot,[vrad2,vrad2],[0,100]
oplot,rvhist_x2,fit2
oplot,[vrad2-fwhm2/2,vrad2+fwhm2/2],[fitparms2[0]/2,fitparms2[0]/2]
xyouts,vrad2+fwmh2,height2,strn(vrad2)+ '!C' + strn(fwhm2/2)

;stop
; ;---
; ;average of 2
; vrad= (vrad_b1 + vrad_b2)/2
; vrad= (vrad_b1 + vrad_b2)/2
; ;variance of sample of two
; vrad_unc= SQRT( ((vrad_b1-vrad)^2 + (vrad_b2 - vrad)^2)/2)
;print, 'vrad_avg:'+ strn(vrad) + ' '+pm+' ' + strn(vrad_unc)
xyouts,0.01,0.97, obj_name,/normal,charsize=1.5
;xyouts,0.5,0.97, 'vrad= '+ strn(vrad,format='(f6.2)') + pm + $
;	strn(vrad_unc,format='(f6.2)')+' km/s',/normal,align=0.5,charsize=1.5

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

stop

!p.multi=0

END
