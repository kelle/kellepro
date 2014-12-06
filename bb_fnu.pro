function bb_fnu, lambda_mic, temp,dist

;returns expected blackbody flux in units of mJy

;lambda_ang: wavlength range in angstroms
;temp in Kelvin
;d in pc

;calls planck

R=0.1d ; in solar radii. assumes radius is 1 Jupiter radii
d=dist*3.016e13/6.96e5 ; convert distance in pc to solar radii

lambda_ang=lambda_mic*1e4; convert microns to angstroms

H=PLANCK(lambda_ang,temp) ; in units of erg/cm^2/s/A

bbflux_lambda=H*r^2/d^2 ; flux at earth in units of erg/cm^2/s/A

bbflux_nu = bbflux_lambda * lambda_mic^2 / 3e-13 *1e3 ; units of mJy

return,bbflux_nu

END
