PRO plot_wise_0355, obj, spt_std, fnu=fnu, ps=ps, flambda=flambda
	
	;NIR spectra overplotting commented out

case spt_std of
	7: begin
		std=11264 & std_name='M7 LHS 3003 U11264'
	end
	8: Begin
		std=50084 & std_name='M8 DENIS-P 1232-6856 U50084'
	END
	9: begin
		std=20596 
		std_name='M9 TVLM 513-46546 U20596'
	end
	10: begin
		std=20927
	std_name='L0  U20927'
end
	11: begin
		std=20701
	std_name='L1 U20701 (binary)'
end
	12: begin
	std=20701
	std_name='L1 U20701 (binary)'
	end	
	
	13: begin
		std=20990 
	std_name='L4 U20990'
	end	
	14: begin
		std=20990 
	std_name='L4 U20990'
	end
	15: begin
		std_ir=99999
		std_opt=11296
		std=10601
	std_name='L5 1507 and L5 avg'
end
16: begin
	std=10601
std_name='L5 U10601'
end
endcase

	; Ref	WISE Designation	SpType text	SpType Comment
	;11264	 J145637.90-280957.0	7	M7		LHS 3003	Yes	8.07	0.258
	;50084	  J123217.36-685601.6	8	M8		DENIS-P 1232-6856	
;	20596	  J150108.17+225001.4	9	M9		TVLM 513-46546	Yes
	; 20744	  J173129.68+272120.7	L0	PX Standard. AB Dor?
	; 20165	  J034543.07+254022.9	L0	K99 L0 Standard. NIR Standard.
	; 10668	 J074642.23+200031.5	L0.5	SpeX Standard
	; 20581	 J143927.27+192919.8	L1	K99 L1 Standard. NIR Standard.
	; 11122	 J130539.94-254106.1	L2:	K99 L2 Standard. NIR Standard.
	; 13076	 J020503.70+125142.2	L5	NIR standard?
	; 11296	 J150747.58-162749.3	L5	K00 PX L5 Standard
	; 10770	  J085035.81+105715.2	L6	K99 L6 Standard
	; 10802	 J090837.56+503203.6	L7	Candidate Standard
	; 50006	 J163229.39+190439.9	L8	K99 L8 Standard. NIR Standard.
	; 10276	 J042348.32-041402.5	T0:	T0 Standard

;;;updated for WISE photometry, ELR 4/25/2011

;default is to plot lambda*f_lambda
; \fnu gives F_nu (mJy)

;CALLS
; kellepro: mag2fluxjy
; kellepro: bb_fnu
; kellepro : colors_kc
; astropro:oploterror

!p.font=0
loadct,39

root='/Users/kelle/Analysis/WISE/'
opt_spec_path='/Users/kelle/Data/optical_spectra/'
nir_spec_path='/Users/kelle/Data/nir_spectra/'
irs_spec_path='/Users/kelle/Data/IRS/'

;READ in all WISE Photometry 
readcol, root+'wise_phot.txt', ref,desig,sptype,grav,sp,j,jerr,h,herr,k,kerr,w1,w1err,w2,w2err,w3,w3err,w4,w4err,irac1,irac1_unc,irac2,irac2_unc,irac3,irac3_unc,irac4,irac4_unc, format=('f,a, a, a, f, f,f, f,f, f,f, f,f, f,f, f,f, f,f, f,f, f,f, f,f, f,f'), comment="#",/preserve_null,delim=string(9b)

o=where(ref eq obj)
s=where(ref eq std_ir)

;print,'w3 err obj', w3err[o], w4err[o]
;print,'w3 err std',w3err[s], w4err[s]

;READ in spectra files for objects with WISE PHOTOMETRY
readcol, root+'wise_spectra.txt',ref_spec,spec_opt,spec_nir, format=('a, a,a'),/silent,/preserve_null,delim=string(9b)

o_spec=where(ref_spec eq strn(obj))
s_spec_ir=where(ref_spec eq strn(std_ir))
s_spec_opt=where(ref_spec eq strn(std_opt))

if o_spec eq -1 then begin 
	opt_spec=''
	nir_spec=''
endif

opt_spec_file_std=(spec_opt(s_spec_opt))[0]
nir_spec_file_std=(spec_nir(s_spec_ir))[0]
print, 'optical spectrum std: '+ opt_spec_file_std
print, 'nir specrum std: ' + nir_spec_file_std

opt_spec_file_obj=(spec_opt(o_spec))[0]
nir_spec_file_obj=(spec_nir(o_spec))[0]
print, 'optical spectrum obj: '+ opt_spec_file_obj
print, 'nir specrum obj: ' + nir_spec_file_obj

xsize=18.6267 ; 2 column wide 44 pica
aspect_ratio=1.0/1.8
ysize=xsize*aspect_ratio
;ysize=xsize/1.8 * 2
if ~keyword_set(ps) then scale_plot=1.3 else scale_plot=1.0

xsize_window=500*scale_plot ;in pixels

@colors_kc ;load lots of colors
;xpalette

If keyword_set(ps) then begin
 @symbols_ps_kc
endif else begin
	@symbols_kc
endelse

if keyword_set(fnu) then begin
    yunits =  'F!D'+nu+'!N(mJy)'
    units='Fnu'
endif

if keyword_set(flambda) then begin
    yunits = 'F!D'+lambda+'!N (ergs/s/cm!U2!N/'+micron+')'
    units='Flam'
endif

if ~keyword_set(fnu) and ~keyword_set(flambda) then begin
    yunits = lambda+' F!D'+lambda+'!N (erg cm!U-2!N s!U-1!N)'
    units='lamFlam'
endif

print, yunits

;-----------
; read in spectra
;----------

if opt_spec_file_std eq '' then begin
	opt_spec_std=[0,0] 
endif else begin
	opt_spec_std=KREADSPEC(opt_spec_path+opt_spec_file_std,hdr_opt_std);,/silent
	yunits = strcompress(fxpar(hdr_opt_std,'BUNIT'),/RE)
	print, 'optical units std: ',yunits
endelse
w_opt_std = opt_spec_std[0,*]/10000. ; convert to microns
f_opt_std = opt_spec_std[1,*]

opt_spec_obj=KREADSPEC(opt_spec_path+opt_spec_file_obj,hdr_opt_obj);,/silent
	yunits = strcompress(fxpar(hdr_opt_obj,'BUNIT'),/RE)
	print, 'optical units obj: ',yunits
	w_opt_obj = opt_spec_obj[0,*]/10000. ; convert to microns
	f_opt_obj = opt_spec_obj[1,*]

;READ NIR SPECTRUM and scale to 2MASS J
;ergs/s/cm^2/A
;readspec is Spex tool:

if nir_spec_file_std eq '' then begin
	w_nir=[0,0]
	f_nir=[0 ,0]
endif else begin
	MC_READSPEC, nir_spec_path+nir_spec_file_std, spec_nir_std,hdr_nir_std;,/silent
	yunits = strcompress(fxpar(hdr_nir_std,'YUNITS'),/RE)
	print, 'nir units standard: ', yunits
	IF yunits NE 'ergss-1cm-2A-1' THEN print, 'UNITS WRONG!!!'
	w_nir_std = spec_nir_std[*,0]
	;scale nir std spectrum to 2MASS J band
	j_mag_std=j(s)
	nir_scale_std=mc_2mphotscale(w_nir_std,spec_nir_std[*,1],0,1,j_mag_std,'J')
	f_nir_std= spec_nir_std[*,1]*nir_scale_std
endelse

;READ OBJECT NIR spectrum and scale to 2MASS J
	MC_READSPEC, nir_spec_path+nir_spec_file_obj, spec_nir_obj,hdr_nir_obj;,/silent
	yunits = strcompress(fxpar(hdr_nir_obj,'YUNITS'),/RE)
	print, 'nir units object: ', yunits
	IF yunits NE 'ergss-1cm-2A-1' THEN print, 'UNITS WRONG!!!'
	w_nir_obj = spec_nir_obj[*,0]
	;scale to 2MASS J band
	j_mag_obj=j(o)
	nir_scale_obj=mc_2mphotscale(w_nir_obj,spec_nir_obj[*,1],0,1,j_mag_obj,'J')
	f_nir_obj= spec_nir_obj[*,1]*nir_scale_obj

;plot lambda * f_lambda
;ergs/s/cm^2/ang -> ergs/s/cm^2/micron -> ergs/s/cm^2
IF ~keyword_set(fnu) and ~keyword_set(flambda) then begin
    f_opt_std=w_opt_std * f_opt_std * 1e4
	f_opt_obj=w_opt_obj * f_opt_obj * 1e4
   
    f_nir_std=w_nir_std * f_nir_std * 1e4
    f_nir_obj=w_nir_obj * f_nir_obj * 1e4		
		MESSAGE, 'Lambda * F_lamba', /info
ENDIF

;CONVERT TO mJY
IF keyword_set(fnu) THEN begin
 f_opt = (f_opt * w_opt^2 / 3e-13)*1e3
 f_nir = (f_nir * w_nir^2 / 3e-13)*1e3
 yunits='mJy'
ENDIF

;scale optical to match nir at 0.8 microns
;startw_opt = 0.81 & endw_opt = 0.83
startw_opt = 0.75 & endw_opt = 1.0

nir_norm_std=NORM_SPEC(w_nir_std, f_nir_std, startw_opt, endw_opt, /num)
opt_norm_std=NORM_SPEC(w_opt_std, f_opt_std, startw_opt, endw_opt, /num)
f_opt_std = f_opt_std * nir_norm_std[0]/opt_norm_std[0]

nir_norm_obj=NORM_SPEC(w_nir_obj, f_nir_obj, startw_opt, endw_opt, /num)
opt_norm_obj=NORM_SPEC(w_opt_obj, f_opt_obj, startw_opt, endw_opt, /num)
f_opt_obj = f_opt_obj * nir_norm_obj[0]/opt_norm_obj[0]

;-----------
;;IRS
;-----------

check_irs=FILE_TEST(irs_spec_path+strn(obj)+'_etador21_merge.fits')
if check_irs eq 1 then begin
irs_spec=READFITS(irs_spec_path+strn(obj)+'_etador21_merge.fits',hdr_irs,/silent)
w_irs = irs_spec[0,*]
f_irs = irs_spec[1,*]*3e4
endif else begin
	w_irs=[0,0]
	f_irs=[0,0]
endelse

;convert IR spectrum to ergs/s/cm^2/A
;from eqn. A.2. on p. 234 of some Spitzer handbook
IF ~KEYWORD_SET(fnu) THEN f_irs=f_irs * 3e-13 / w_irs^2
;convert to mJy
IF KEYWORD_SET(fnu) THEN f_irs=f_irs * 1e3

;-----------
; 2MASS JHK
;----------

w_2M=[1.235,1.662,2.159] ;micron
dw_2M=[0.162,0.251,0.262] ;micron

bands=[1,2,3] ;JHK

mags_2M_obj=[j(o),h(o),k(o)]
mags_2M_obj_unc=[jerr(o),herr(o),kerr(o)]
mags_2M_std=[j(s),h(s),k(s)]
mags_2M_std_unc=[jerr(s),herr(s),kerr(s)]

;convert 2MASS mags to mJy
;mag -> mJy
if keyword_set(fnu) then begin
    f_2M_std=mag2fluxjy(bands, mags_2M_obj) ; mJy
    f_2M_obj=mag2fluxjy(bands, mags_2M_std) ; mJy
endif 

;convert 2MASS mags to lambda * flambda
;mag -> ergs/s/cm^2/micron -> ergs/s/cm^2
If ~keyword_set(fnu)  and ~keyword_set(flambda) then begin
    zero_pt=[3.129e-13, 1.133e-13, 4.283e-14] * 1e7;ergs/s/cm^2/micron
    f_2M_obj=w_2M*zero_pt*10^(-mags_2M_obj/2.5);ergs/s/cm^2
    f_2M_std=w_2M*zero_pt*10^(-mags_2M_std/2.5);ergs/s/cm^2
endif

;convert 2MASS mags to flambda
;mag -> ergs/s/cm^2/micron
if keyword_set(flambda) THEN BEGIN
    zero_pt=[3.129e-13, 1.133e-13, 4.283e-14] * 1e7 ;ergs/s/cm^2/micron
    f_2M_obj=zero_pt*10^(-mags_2M_obj/2.5);ergs/s/cm^2/micron
    f_2M_std=zero_pt*10^(-mags_2M_std/2.5);ergs/s/cm^2/micron
ENDIF

;-----------
; WISE
;----------
w_WISE=[3.3526,4.6028,11.5608,22.0883] ;micron
dw_WISE=[3.80-2.80,5.30-4.00,16.75-7.55,27.00-19.80]
zp=[306.68,170.66,29.045,8.2839]
sigma_wise=[4.60,2.56,0.436,0.436]
;wise_std=[]
wise_obj=[w1(o),w2(o),w3(o),w4(o)]
wise_std=[w1(s),w2(s),w3(s),w4(s)]
sigma_obj=[w1err(o),w2err(o),w3err(o),w4err(o)]
sigma_std=[w1err(s),w2err(s),w3err(s),w4err(s)]
sigzp=[4.60,2.56,0.436,0.2899]

;;;from Marshall's python script
;;;    bands = [{'mag': 'w1mpro', 'sigmag': 'w1sigmpro', 'zp': 306.68, 'sigzp': 4.60,   'wave': 3.3526},
;;;             {'mag': 'w2mpro', 'sigmag': 'w2sigmpro', 'zp': 170.66, 'sigzp': 2.56,   'wave': 4.6028},
;;;             {'mag': 'w3mpro', 'sigmag': 'w3sigmpro', 'zp': 29.045, 'sigzp': 0.436,  'wave': 11.5608},
;;;             {'mag': 'w4mpro', 'sigmag': 'w4sigmpro', 'zp': 8.2839, 'sigzp': 0.2899, 'wave': 22.0883}]


            f_wise_obj = zp * 10.^(-1.*wise_obj/2.5)

            f_wise_std = zp * 10.^(-1.*wise_std/2.5)

            flux_uncert_systematic = sigzp * 10^(-1.*wise_obj/2.5)
            flux_uncert_statistical =  zp * 10^(-1.*wise_obj/2.5) - (zp * 10^(-1.*(wise_obj+sigma_obj)/2.5) )
			flux_uncert = sqrt((flux_uncert_systematic^2.)+(flux_uncert_statistical^2.))
  ;;;Janskys is an F_nu value

  flux_uncert_systematic_std = sigzp * 10^(-1.*wise_std/2.5)
  flux_uncert_statistical_std =  zp * 10^(-1.*wise_std/2.5) - (zp * 10^(-1.*(wise_std+sigma_std)/2.5) )
	flux_uncert_std = sqrt((flux_uncert_systematic_std^2.)+(flux_uncert_statistical_std^2.))


;convert WISE F_nu (Jy) to lambda * flambda
;mJy -> ergs/s/cm^2/micron  -> ergs/s/cm^2
If ~keyword_set(fnu)  and ~keyword_set(flambda) then begin
    a = 3e-9 ;from spitzer handbook appendix A, 1 Jy * c
    f_WISE_std = w_WISE*f_WISE_std * a / (w_WISE)^2. ;ergs/s/cm^2
    f_WISE_obj = w_WISE*f_WISE_obj * a / (w_WISE)^2. ;ergs/s/cm^2
	f_WISE_obj_unc= w_WISE*flux_uncert * a / (w_WISE)^2. ;ergs/s/cm^2
	f_WISE_std_unc= w_WISE*flux_uncert_std * a / (w_WISE)^2. ;ergs/s/cm^2
ENDIF

;convert WISE F_nu to flambda
;mJy -> ergs/s/cm^2/micron  
if keyword_set(flambda) THEN BEGIN
  a = 3e-9 ;from spitzer handbook appendix A, 1 Jy * c
    f_WISE_std = f_WISE_std * a / (w_WISE)^2. ;ergs/s/cm^2/micron
    f_WISE_obj = f_WISE_obj * a / (w_WISE)^2 ;ergs/s/cm^2/micron
END

;-----------
; IRAC
;----------

w_IRAC=[3.561, 4.509, 5.693, 7.982] ; micron
dw_IRAC=[0.750, 1.015, 1.425, 2.905] ; micron

mags_IRAC_obj=[irac1[o],irac2[o],irac3[o],irac4[o]];mags
mags_IRAC_std=[irac1[s],irac2[s],irac3[s],irac4[s]];mags

;convert IRAC mags to lambda * flambda
;mag -> ergs/s/cm^2/micron -> ergs/s/cm^2
If ~keyword_set(fnu)  and ~keyword_set(flambda) then begin
    zero_pt=[6.42,2.66,1.04,0.296] * 1e-8;ergs/s/cm^2/micron
    f_IRAC_obj=w_IRAC*zero_pt*10^(-mags_IRAC_obj/2.5);ergs/s/cm^2
    f_IRAC_std=w_IRAC*zero_pt*10^(-mags_IRAC_std/2.5);ergs/s/cm^2
endif

;convert IRAC F_nu to lambda * flambda
;mJy -> ergs/s/cm^2/micron  -> ergs/s/cm^2
;If ~keyword_set(fnu)  and ~keyword_set(flambda) then begin
;    a = 3e-12 ;spitzer handbook appendix
;    f_IRAC_std = w_IRAC*f_IRAC_std * a / (w_IRAC)^2 ;ergs/s/cm^2
;    f_IRAC_obj = w_IRAC*f_IRAC_obj * a / (w_IRAC)^2 ;ergs/s/cm^2
;ENDIF

;convert IRAC F_nu to flambda
;mJy -> ergs/s/cm^2/micron  
;if keyword_set(flambda) THEN BEGIN
;  a = 3e-12 ;spitzer handbook appendix
;    f_IRAC_std = f_IRAC_std * a / (w_IRAC)^2 ;ergs/s/cm^2/micron
;    f_IRAC_obj = f_IRAC_obj * a / (w_IRAC)^2 ;ergs/s/cm^2/micron
;END

;----------
; SCALE OBJECT TO STANDARD in the optical
;----------

;scale obj fluxes to std_flux at J
;scale = f_2M_std[0]/f_2M_obj[0]

;scale obj fluxes to Standard optical spectrum

opt_norm_std=NORM_SPEC(w_opt_std, f_opt_std, startw_opt, endw_opt, /num)
opt_norm_obj=NORM_SPEC(w_opt_obj, f_opt_obj, startw_opt, endw_opt, /num)

;scale = 1.0/(opt_norm_std[0]/opt_norm_obj[0])
scale = 1.0
print, scale

f_2M_obj = f_2M_obj / scale 
f_WISE_obj = f_WISE_obj / scale 
f_WISE_obj_unc = f_WISE_obj_unc / scale
f_IRAC_obj= f_IRAC_obj / scale
f_nir_obj = f_nir_obj / scale
f_opt_obj = f_opt_obj / scale
;f_irs = f_irs * scale

;-----------
; BLACKBODY
;-----------

w_bb =  findgen(300)/10.+0.1
f_bb=bb_fnu(w_bb,3600.,13.) ; Jy, distance to standard K5, radius is for a brown dwarf!!!

;-----------
; PLOT
;-----------


;ymax=max(f_2M_std,/nan)

;if keyword_set(fnu) then  yr=[ymax/1000,ymax]
if ~keyword_set(fnu) and ~keyword_set(flambda) then yr  =[1e-14,10e-11]
;if keyword_set(flambda) then yr  =[ymax/100000,f_2M_std[0]*10]

IF KEYWORD_SET(ps) THEN BEGIN
    !p.font=0 
 !p.thick=2
 !x.thick=3
 !y.thick=3
	ext='.ps'
	outfile_root='plots/'+strn(obj)	
    set_plot, 'ps'
    device, filename=root+outfile_root+ext, encapsulated=1, $
            /helvetica,/isolatin1,$
            landscape=0, color=1, xsize=xsize, ysize=ysize, scale=scale_plot,$
            xoffset=1, yoffset=1
    @symbols_ps_kc
ENDIF ELSE BEGIN
    set_plot,'x'
    device, Decomposed=0        ;make colors work for 24-bit display
    black=white ;exchange colors to work with default black backround
    @symbols_kc
    window, 1, xsize=xsize_window, ysize=xsize_window*aspect_ratio,xpos=2000,ypos=0
ENDELSE

;SETUP PLOT AXES
plot, w_bb, f_bb, yr=yr,xr=[0.6,30.],/nodata, xstyle=1,ystyle=1,$
      /xlog,/ylog,ytitle=lambda +'F!D'+lambda +'!N (ergs/s/cm!U2!N)', $
	  xtitle='Wavelength ('+micron+')'

	  nir_min=where(w_nir_std ge 0.8)
	  nir_min2=where(w_nir_obj ge 0.9)
	  nir_max=where(w_nir_std ge 2.5)

color_spec_std=158 
;color_spec_std=red
color_spec_obj=grey	 
	
oplot,w_nir_std[nir_min[0]:nir_max[0]],f_nir_std[nir_min[0]:nir_max[0]],color=color_spec_std
oplot, w_nir_obj[nir_min2[0]:*],f_nir_obj[nir_min2[0]:*],color=color_spec_obj

oplot,w_opt_std,f_opt_std,color=color_spec_std
oplot,w_opt_obj,f_opt_obj,color=color_spec_obj

;oplot,w_irs,f_irs,color=blue
;_[0]*nir[1]*1.d3  ;;;1d3 is conversion factor for W m-2 um-1 to erg cm-2 s-1, lam F lam

;plot photometry

;OBJ: 2MASS
plotsym,0,1.7,/fill, color=color_obj
oploterror, w_2M,f_2M_obj,dw_2M/2, [0,0,0],psym=8,errcolor=color_obj

;plotsym,0,1.7,/fill, color=color_obj
;oploterror, w_2M,f_2M_obj,dw_2M/2, [0,0,0],psym=8,errcolor=color_obj

;Std
color_std=blue

plotsym,0,1.0, fill=1, color=color_std
oploterror,w_2m, f_2M_std, dw_2M/2,[0,0,0], psym=8,errcolor=color_std

plotsym,0,1.0, fill=1, color=color_std
plots,w_2m[0], f_2M_std[0], psym=8




;OBJ
color_obj=black

plotsym,4,1.5, color=color_obj, thick=3
oploterror, w_IRAC,f_IRAC_obj, dw_IRAC/2, [0,0,0,0], psym=8, errcolor=color_obj

if w3err[o] ne 0 and w4err[o] eq 0 then begin
	plotsym,1,2,thick=4,color=color_obj
	oploterror, w_WISE[3],f_WISE_obj[3], dw_WISE[3]/2,[0],$
	 psym=8, errcolor=color_obj, symsize=2
	plotsym,0,1.0,/fill, color=color_obj
	oploterror, w_WISE[0:2],f_WISE_obj[0:2], dw_WISE[0:2]/2, $
	f_WISE_obj_unc[0:2], psym=8, errcolor=color_obj, symsize=2
endif else if w3err[o] eq 0 and w4err[o] eq 0 then begin
		plotsym,1,2,color=color_obj
		oploterror, w_WISE[2:3],f_WISE_obj[2:3],$
		 dw_WISE[2:3]/2,[0,0], psym=8, errcolor=color_obj, symsize=2
		plotsym,0,1.0,/fill, color=color_obj
		oploterror, w_WISE[0:1],f_WISE_obj[0:1], dw_WISE[0:1]/2,$
		 f_WISE_obj_unc[0:1], psym=8, errcolor=color_obj, symsize=2
endif else begin
	plotsym,0,2.0, /fill,color=color_obj
	oploterror, w_WISE,f_WISE_obj, dw_WISE/2, f_WISE_obj_unc, psym=8,$
	 errcolor=color_obj
endelse

if w3err[s] ne 0 and w4err[s] eq 0 then begin
	plotsym,1,1,color=color_std
	oploterror, w_WISE[3],f_WISE_std[3], dw_WISE[3]/2,0,$
	 psym=8, errcolor=color_std
	plotsym,0,1.0, /fill,color=color_std
	oploterror, w_WISE[0:2],f_WISE_std[0:2], dw_WISE[0:2]/2, $
		f_WISE_std_unc[0:2], psym=8, errcolor=color_std
endif else if w3err[s] eq 0 and w4err[s] eq 0 then begin
		plotsym,1,1,color=color_std
		oploterror, w_WISE[2:3],f_WISE_std[2:3],$
		 dw_WISE[2:3]/2,[0,0], psym=8, errcolor=color_std
		plotsym,0,1.5,/fill, color=color_std
		oploterror, w_WISE[0:1],f_WISE_std[0:1], dw_WISE[0:1]/2,$
		 f_WISE_std_unc[0:1], psym=8, errcolor=color_std
endif else begin
	plotsym,0,1,/fill, color=color_std
	oploterror, w_WISE,f_WISE_std, dw_WISE/2, f_WISE_std_unc, psym=8,$
	 errcolor=color_std
endelse


plotsym,0,1.0, fill=1, color=color_std
;plotsym,0,1.0, color=color_std, thick=4
oploterror, w_IRAC,f_IRAC_std, dw_IRAC/2, [0,0,0,0], psym=8, errcolor=color_std



;LEGEND
xll=0.3
yofl=0.25
xyouts, xll+0.04, yofl,'L5 Field Standard',/normal
;xyouts, xll+0.08, yofl-0.05,'opt=1507-16 ir=0652+47 ',/normal, size=0.8
plotsym,0, 1.0, /fill, color=color_std
plots,[xll-0.03,xll+0.03],[yofl,yofl]+0.01,/normal,color=color_spec_std
plots, [xll],[yofl+0.01], psym=8,/normal,color=color_std

;plots, [xll-0.01],[yofl+0.01], psym=8,/normal,color=color_std
;plotsym,0, 1.0, fill=0, color=color_std, thick=4
;plots, [xll+0.01],[yofl+0.01], psym=8,/normal,color=color_std

yofl=yofl+0.08
xyouts, xll+0.04, yofl,'2M 0355+11, L5' +gamma,/normal
plotsym,0, 2, /fill, color=black
plots,[xll-0.03,xll+0.03],[yofl,yofl]+0.01,/normal,color=color_spec_obj
plots, [xll],[yofl+0.01], psym=8,/normal,color=color_obj

;xyouts,xll, 0.8,'2M 0355+11, L5'+ gamma,/normal, size=1.5, align=0.5

;yofl=0.50
; xyouts, xll+0.03, 0.13+yofl ,'2MASS' ,/normal
; plotsym, 4, 1.0+yofl, /fill, color=black
; plots, [xll],[0.14]+yofl, psym=8,/normal

; yofl=yofl+0.06
; xyouts, xll+0.03, 0.13+yofl ,'IRAC' ,/normal
; plotsym, 0, 1.0+yofl, fill=0, color=color_std
; plots, [xll],[0.14]+yofl, psym=8,/normal
; 
; yofl=yofl+0.06
; xyouts, xll+0.03, 0.13+yofl ,'2M 0355+11, L5'+gamma ,/normal
; plotsym, 0, 1.0+yofl, /fill, color=color_obj
; plots, [xll],[0.14]+yofl, psym=8,/normal



IF KEYWORD_SET(ps) THEN BEGIN
 device,/close
 set_plot,'x'
 PRINT, 'wrote spectrum to file '+ root+outfile_root+ext
 loadct,0
 !p.font=-1
ENDIF

END
