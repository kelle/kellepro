PRO predict_mags

j_mag=14.
k_mag=13.

k_flux_mjy=mag2fluxjy(k_mag,3)
j_flux_mjy=mag2fluxjy(j_mag,1)

;print, j_flux_mjy
;print, k_flux_mjy

;wavelength in angs 6000-25000
wave=DINDGEN(3000)*0.01+0.6 ;wavelength from 0.6-30 microns
temp=2200
dist=20 ;star is 20 pc away.

bbflux = BB_FNU(wave,temp,dist)

plot, wave, bbflux,xr=[0.6,3]
oplot, [1],[j_flux_mjy],psym=4
oplot, [2.5],[k_flux_mjy],psym=2

;read in model of same teff
model_root='/data/hillary/1/kelle/Saumon_Models/'
model=READFITS(model_root+'sp_t'+strn(temp)+'g1000f3.fits')
model_wavelength=model[*,0]
r=0.1d ; in solar radii. assumes radius is 1 Jupiter radii
d=dist*3.016e13/6.96e5 ; convert distance in pc to solar radii
model_flux=model[*,1]*1e26*r^2/d^2 ;in mJy
oplot, model_wavelength,model_flux

END
