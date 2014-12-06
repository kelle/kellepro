FUNCTION norm_spec, wavelength, flux, startw, endw, numkey=numkey

;pix_scale = w[1]-w[0]
;a = where(w ge startw-pix_scale/2 AND w le endw+pix_scale/2)
num = avgflux(startw, endw, wavelength, flux)
norm_flux=flux/num

IF keyword_set(numkey) THEN RETURN, num ELSE RETURN, norm_flux

END
