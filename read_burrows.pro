FUNCTION read_burrows, file,fnu=fnu

path='/scr2/kelle/Burrows_models/2006_models/'
restore, path+'burrows_template.sav'

data=READ_ASCII(path+file, TEMPLATE=template)


w=data.field03 ; in microns
f_lam=data.field05
f_Jy=data.field06

;convert to ers/s/cm^2/A
;from eqn. A.2. on p. 234 of some Spitzer handbook
IF ~KEYWORD_SET(fnu) THEN BEGIN
 flux=f_Jy * 3e-13 / w^2
ENDIF ELSE BEGIN
 flux=f_Jy
ENDELSE

size=n_elements(flux)
spec=dblarr(2,size)

spec[0,*]=w
spec[1,*]=flux

return, spec
;plot, w,flux,/xlog, xr=[0.5,16],xstyle=1

END
