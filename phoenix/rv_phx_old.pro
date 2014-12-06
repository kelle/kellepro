PRO rv_phx,file,nloops,srad,comment, ps=ps,m=m

;TODO 
; make choosing std interactive
; make plot output. add notations to histogram (std, nloops, srad)
; be sure to understand shift wrt std translates into true RV

;MODIFICATION HISTORY
; 2011 Feb: cleaned it up. changed variable names
; 2010 July 20: monte carlo the RV measurement
; 2010 July 19:  added delta log lambda correction
; written Dec 2009, K. Cruz

if N_params() lt 1 then begin
     print,"Syntax -  rv_phx,'combined.fits',nloops,srad2,comment,ps=ps,m=m"
     print,'useful information'
     return
endif

if n_elements(nloops) eq 0 then nloops=500

;if n_elements(file) eq 0 then file='combined.fits' 

; for Hillary
root1='/scr1/nir_highres/standards/'
; for Gloria
root2='/Users/kelle/Analysis/nir_highres/standards/'
; for John's machine
root3='~/Projects/NIR_Radial_Velocities/standards/'
if File_test(root1) then root=root1
if file_test(root2) then root=root2
if file_test(root3) then root=root3
;if ~file_test(file) then file='combined.fits'
;message,'Using combined.fits', /info

;srad is how many pix to use to fit polynomial
if n_elements(srad1) eq 0 then srad1 = 1
if n_elements(comment) eq 0 then comment = ''

@colors_kc
device,decomposed=0

std1=readfits(root+'1155_new.fits',h_std1,/silent)
rv_std1=45.63
rv_unc_std1=0.13
name_std1='2M1155'


; 	std1=kreadspec(root+'lhs292_49_M65_phx',h_std1,/silent)
; 	rv_std1=1.8
; 	rv_unc_std1 = 0.5
; 	name_std1='LHS292'
;	a1=kreadspec(root+'2M0523-14_L25_phx',ha1,/silent)
; 	std2=kreadspec(root+'bri0021_M95_phx',h_std2,/silent)
; 	rv_std2=20.
; 	rv_unc_std2= 5.
; 	name_std2='BRI 0021'
; ;	std2=kreadspec(root+'2M1155-37_L20_phx',h_std2,/silent)

obj=readfits(file,h_obj,/silent)
;obj[0,*] = wavelength
;obj[1,*] = flux
;obj[2,*] = uncertainty on flux

name_obj=SXPAR(h_obj,'OBJECT')
date_obj=strtrim(SXPAR(h_obj,'UTDATE'))

;p 44 lab notebook 3
;v=Delta lambda/avg lambda * c
;a_end=(size(a1))[2]-1
;pixkm=(a1[0,1]-a1[0,0])/((a1[0,0]+a1[0,a_end])/2)*2.9979e5

;helio correct
;p 31, notebook 3
;read vhelio from headers
vh_std1=sxpar(h_std1,'vhelio')
; vh_std2=sxpar(h_std2,'vhelio')
vh_obj=sxpar(h_obj,'vhelio',count=n_vh_obj)
;print, 'vhelio (km/s): '+ strn(vha1)+ ' ' + strn(vha2)+' '+ strn(vhb)
;convert vhelio from km/s to scale factor
;apply heliocentric correction to wavelengths
cspeed   = 2.99792458E5       ; km/s
w_std1=reform(std1[0,*])*(vh_std1/cspeed +1.0D)
w_obj =reform(obj[0,*]) *(vh_obj /cspeed +1.0D)

;assign flux and flux uncertainty to their own variables
f_std1=reform(std1[1,*])
f_sig_std1=reform(std1[2,*])
f_obj=reform(obj[1,*])
f_sig_obj=reform(obj[2,*])

;find where std1 and obj overlap
w_min_std1 = max([min(w_std1),min(w_obj)])
w_max_std1 = min([max(w_std1),max(w_obj)])
;good_a1=where(aw1 ge wmin1 and aw1 le wmax1,na1)
;good_b1=where(bw ge wmin1 and bw le wmax1,nb1,comp=bad_b1)

;figure out number of pixels in each spectra
n_pix_std1=n_elements(w_std1)
;n_pix_std2=n_elements(w_std2)
n_pix_obj=n_elements(w_obj)
;n_pix=max[n_pix_std1,n_pix_std2,n_pix_obj] 
;num1=max([na1,nb1])
;num2=max([na2,nb2])
;num1=max([n])

;bwtmp1 = bw[good_b1]
;bwtmp2 = bw[good_b2]
;awtmp1 = aw1[good_a1]
;awtmp2 = aw2[good_a2]

;bwtmp1 = w_obj
;bwtmp2 = w_obj
;awtmp1 = w_std1
;awtmp2 = w_std2

;-------------------------
 !p.multi=[0,1,2,0,1]
;!p.multi=[0,2,2]
 xsize=21
 ysize=xsize/2/1.5 * 2 ;2 for two plots
 aspect_ratio=ysize/xsize
 scale=1.0
 xsize_window=600*1.7

if keyword_set(ps) then begin
    !p.font=0
    set_plot,'ps'
    device, file=strtrim(name_obj,2)+'_'+date_obj+'_rvplots.ps',/helvetica,/isolatin1,color=1
    device,xsize=xsize,ysize=ysize,scale=scale,xoffset=0.25,yoffset=1
	@symbols_ps_kc
	message,'Wrote: '+strtrim(name_obj,2)+'_'+date_obj+'_rvplots.ps',/info
endif else begin
    set_plot,'x'
    device,decomposed=0
    window,2, xsize=xsize_window,ysize=xsize_window*aspect_ratio
	@symbols_kc
endelse

;-------------------------
rv_arr=fltarr(nloops)

for i=0,nloops-1 do begin
	mc_loopprogress,i,0,nloops-1,modval=nloops/10.
	
;	bftmp1 = bf[good_b1] + (bsig[good_b1] * randomn(seed,nb1,/normal))
	f_tmp_obj = f_obj + (f_sig_obj * randomn(seed,n_pix_obj,/normal))
	;bftmp2 = bf[good_b2] + (bsig[good_b2] * randomn(seed,nb2,/normal))
	;aftmp1 = af1[good_a1] + (asig1[good_a1] * randomn(seed,na1,/normal))
	;aftmp2 = af2[good_a2] + (asig2[good_a2] * randomn(seed,na2,/normal))
	f_tmp_std1 = f_std1 + (f_sig_std1 * randomn(seed,n_pix_std1,/normal))
;	f_tmp_std2 = f_std2 + (f_sig_std2 * randomn(seed,n_pix_std2,/normal))

; --- Resample standards to constant spacing in log lambda space: v/c = d(ln lambda)
  
acoef_std1=float(n_pix_std1 - 1)/(alog(max(w_std1)) - alog(min(w_std1)))
bcoef_std1= float(n_pix_std1) - (acoef_std1 * (alog(max(w_std1)))) 
; acoef1  = float(num1 - 1)/(alog(wmax1) - alog(wmin1))
;  bcoef1  = float(num1) - (acoef1 * (alog(wmax1)))

   xpon1   = findgen(n_pix_std1)+1.0
   w_log_std1    = exp((xpon1-bcoef_std1)/acoef_std1)

;put stds and obj on same d lon lambda wavelength scale as standards but doesnt shift
 ;std1 and obj interpolated to std1 wavelengeth scale
	f_i_std1= interpol(f_tmp_std1,w_std1,w_log_std1)
	f_i_obj1 = interpol(f_tmp_obj,w_obj,w_log_std1)
	 ;fxa1 = interpol(aftmp1,awtmp1,wx1) 

;x-correlate object with std
;also makes x-correl top panel

	mod_num=50
	if i mod mod_num eq 0 then plot=1 else plot=0
    
		shift_objstd1=ccpeak_kc(f_i_obj1,f_i_std1,40,srad1,plot=plot)
		;ba_shift1=ccpeak_kc(fxb1,fxa1,40,srad1,plot=plot)
		;use average of max and fit peak
		shift_objstd1_avg = (shift_objstd1[0] + shift_objstd1[1])/2
  
		;velocity of obj wrt std1
		;   vshift_b1 = (cspeed * ba_shift1_avg)/acoef1
		;  vshift_b1_unc =  cspeed*(ABS(ba_shift1[0] - ba_shift1[1]))/acoef1
		vshift_objstd1 = (cspeed * shift_objstd1_avg)/acoef_std1
		;vshift_objstd1_unc =  cspeed*(ABS(shift_objstd1[0] - shift_objstd1[1]))/acoef_std1

		;velocity of obj
		rv_obj=rv_std1 - vshift_objstd1  
		;   vrad_b1 = rv_a1 - vshift_b1
		;vrad_b1_unc = rv_a1_unc + vshift_b1_unc
		rv_obj_unc = 0
  
		;store calculated RV
		rv_arr[i]=rv_obj

		;every once in a while, show the results of the x-correlation
		if i mod mod_num eq 0 then begin
			rv_offset_objstd1=(vshift_objstd1/cspeed +1.0D)
			;rv_offsetb1=(vshift_b1/cspeed +1.0D)
			;rv_w1 = aw1 * rv_offsetb1
			w_objstd1= w_obj * rv_offset_objstd1
		
			;plot spectra (bottom panel)
			plot, w_objstd1, f_tmp_obj, /nodata,/ynozero,xr=[1.551,1.5585],xstyle=1,xmargin=[4,2],ymargin=[2,2],yrange=[0.4,1.6],ystyle=1
			;if bad_b1[0] ne -1 then oplot, rv_bw1[bad_b1], bf[bad_b1],color=green,linestyle=1
			oplot, w_std1, f_std1, color=red, linestyle=1 
			oplot, w_objstd1, f_tmp_obj ;rv corrected w_obj and random noise f_obj
			; plot, aw1, af1, /nodata,/ynozero,xr=[1.551,1.5585],xstyle=1,xmargin=[4,2],ymargin=[2,2],yrange=[0.4,1.6],ystyle=1
			; 			if bad_b1[0] ne -1 then oplot, rv_bw1[bad_b1], bf[bad_b1],color=green,linestyle=1
			; 			oplot, aw1, af1, color=red, linestyle=1
			; 			oplot, rv_bw1, bftmp1
						
			 comment2= comment + '   srad1=' + strn(srad1); + 'srad2='+ strn(srad2) 
			 xyouts, 0.06,0.90, 'Comment: ' + comment2 +'!C'+ name_obj + '!C'+ 'obs date: ' + date_obj +'!C'+$
			 		'std: ' + name_std1 +'!C'+$
			 		'rv= '+strn(rv_obj,format='(f6.2)')  + pm + strn(rv_obj_unc,format='(f6.2)') +' km/s',$
			 		/normal,charsize=0.9
				;print, 'vrad_b1= '+ strn(vrad_b1)  + ' km/s'
				xyouts,0.5,0.5,'i='+strn(i),/normal
		endif

ENDFOR ;end monte carlo loop

;resistant_mean,vrad_arr1,2,vrad1,vrad_unc1
;resistant_mean,vrad_arr2,2,vrad2,vrad_unc2
;vrad1=(resistant_mean(vrad_arr1))[0]
;vrad_unc1=sqrt((moment(vrad_arr1))[1])
;vrad2=(median(vrad_arr2))[0]
;vrad_unc2=sqrt((moment(vrad_arr2))[1])

!p.multi=[0,1,2]
if ~keyword_set(ps) then window,3

mc_histogram,rv_arr,rvhist_x,rvhist_y,binsize=0.5 ;binsize is in km/s

;added if/then/else to make code work for when xcorrelating an object with itself
if n_elements(rvhist_y) gt 3 then begin
	fit = gaussfit(rvhist_x,rvhist_y,fitparms,sigma=sigma) 
	vrad=fitparms[1]
	fwhm = 2*SQRT(2*ALOG(2))*fitparms[2]
	height=fitparms[0]/2	
endif else begin 
	vrad=rvhist_x[1]
	fwhm=0.5
	height=100
	fit=rvhist_x
endelse

plot,rvhist_x,rvhist_y,psym=10
oplot,[vrad,vrad],[0,max(fit)]
oplot,rvhist_x,fit
oplot,[vrad-fwhm/2,vrad+fwhm/2],[height,height]
xyouts,vrad,10,strn(vrad)
xyouts,vrad+fwhm,height,strn(vrad)+ '!C' + strn(fwhm/2)

plot, w_objstd1, f_tmp_obj, /nodata,/ynozero,xr=[1.551,1.5585],xstyle=1,xmargin=[4,2],ymargin=[2,2],yrange=[0.4,1.6],ystyle=1
;if bad_b1[0] ne -1 then oplot, rv_bw1[bad_b1], bf[bad_b1],color=green,linestyle=1
oplot, w_std1, f_std1, color=red, linestyle=1 
oplot, w_obj*((rv_std1-vrad)/cspeed +1.0D), f_obj ;rv corrected w_obj and random noise f_obj

xyouts,0.01,0.97, name_obj,/normal,charsize=1.5
;xyouts,0.5,0.97, 'vrad= '+ strn(vrad,format='(f6.2)') + pm + $
;	strn(vrad_unc,format='(f6.2)')+' km/s',/normal,align=0.5,charsize=1.5

if n_vh_obj eq 0 then begin
	message,'no vhelio correction',/info
	xyouts,0.5,0.93,'no vhelio correction',/normal,align=0.5,charsize=1.5,color=red
endif
if n_vh_obj gt 1 then message,'too many vhelio corrections',/info

IF KEYWORD_SET(ps) THEN BEGIN
 device,/close
 set_plot,'x'
 !p.font=-1 ;go back to default (Vector Hershey fonts)
ENDIF

!p.multi=0

END
