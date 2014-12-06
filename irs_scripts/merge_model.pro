
Procedure used to make etaDor model.

a=where(sl2_model_w LT 7.6 AND sl2_model_w GT 5.2)
b=where(sl1_model_w GE 7.6 AND sl1_model_w LE 14.5)
size=n_elements(a)+ n_elements(b)
w_merge=FLTARR(size)
flux_merge=w_merge
w_merge[0:n_elements(a)-1]   = sl2_model_w[a]
w_merge[n_elements(a):size-1]= sl1_model_w[b]
flux_merge[0:n_elements(a)-1]   = sl2_model_f[a]
flux_merge[n_elements(a):size-1]= sl1_model_f[b]
oplot, w_merge,flux_merge
oplot, w_merge,flux_merge*0.05
model_w=w_merge
model_f=flux_merge*0.05
save,file='/data/hillary/3/kelle/ir_spectra/irs/S12/flux_calibrators/etaDor.dat',model_w,model_f
