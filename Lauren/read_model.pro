FUNCTION read_model, model_file_path, data_wavelength, data_flux, sigma_flux, weight

spectrum=READFITS(model_file_path,/silent)
        
 model_wavelength=spectrum[*,0]
 model_flux_jy=spectrum[*,1]
 model_flux=model_flux_jy/model_wavelength^2 ; convert to F-lambda   
        
 model_flux_rebin=INTERPOL(model_flux, model_wavelength, data_wavelength)        
        
;--------------------
;NORMALIZING CONSTANT
;--------------------        
 model_flux=model_flux_rebin         
 norm_const=TOTAL((weight*data_flux*model_flux)/sigma_flux^2,/NaN)/TOTAL((weight*model_flux^2)/sigma_flux^2,/Nan)

;redefine model flux with norm constant        
        norm_model_flux=model_flux*norm_const

Return, norm_model_flux

END

