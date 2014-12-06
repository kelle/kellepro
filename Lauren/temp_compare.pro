PRO temp_compare


version='v1'
;root= '/scr3/ljm/'
root='/scr2/kelle/idl/kellepro/Lauren/'
out_dir=root+'idl/plots/'
data_dir = '/scr3/kelle/nir_spectra/2M_all/fits/'
model_dir = '/scr2/kelle/Saumon_Models/fits/'

;-----------------
;Read in output of GoodFits.
; outdata.txt
;-----------------

; change goodfits so that filenames are output to outdata.txt like this
READCOL, root+'pro/outdata_test.txt', ref_num_model, data_file, model_file, teff_model, fsed, grav, sp_type, comment='#', $
         format='f, a, a, f,f, f,f'

;-----------------
;Read in observed temps from Golimowski, etc
; obs_temp.txt
;-----------------

READCOL, root+'obs_temp.txt', ref_num_obs, teff_obs, paper, comment='#', format='f,f,a'
n_obs=n_elements(ref_num_obs)

;Loop over Observed files

for obs_index = 0,n_obs-1 do begin

    model_index=where(ref_num_obs[obs_index] eq ref_num_model)
    ;print, teff_model[model_index], teff_obs[obs_index]
    ;print,  abs(teff_model[model_index] -  teff_obs[obs_index])
    ;print, model_data[model_index]


;read in datafile and normalize to jband

    spectrum=KREADSPEC(data_dir+data_file[model_index], h,/silent,/norm,/nir)
    data_wavelength=spectrum[0,*]
    data_flux=spectrum[1,*]
    sigma_flux=spectrum[2,*]

;read in bestfit modelfile and normalize to datafile

    model_flux_bestfit=read_model(model_dir+model_file[obs_index], data_wavelength, data_flux, sigma_flux)

;read in  modelfile with observed temp, g1000, and fsed=2. and normalize to datafile

    if teff_obs[obs_index] mod 100 ne 0 then strn_teff_obs=strn(fix(teff_obs[obs_index]-50)) else $
      strn_teff_obs = strn(fix(teff_obs[obs_index]))

    model_file_obs=FILE_SEARCH(Model_dir+'sp_t'+strn_teff_obs +'g1000f2.fits', count=count)
    if count ne 1 then print, 'PROBLEM, more than one model file!)'
stop
    model_flux_obsteff=read_model(model_file_obs[0], data_wavelength, data_flux, sigma_flux)

;make plot

;;;;add color defintions

    !P.font=0
    set_plot, 'ps'
    device, file=out_dir+'test_'+strn(fix(ref_num_obs[obs_index]))+'.ps', encapsulated=0,/helvetica,/COLOR,ysize=9.5,yoffset=0.75,/inches
    !p.multi=[0,1,2]

micron=string(181B)+'m'

    plot, [0], [0], xr=[0.6,2.5], yr=[0,1.5],TITLE='Spectra Comparison',XTITLE='Waveglength (um)',YTITLE='Flux (erg cm!E-2!N s!E-1!N '+micron+'!E-1!N)', /nodata 
    oplot, data_wavelength, model_flux_bestfit, COLOR=red
    oplot, data_wavelength, data_flux, COLOR=blue       

    plot, [0], [0], xr=[0.6,2.5], yr=[0,1.5],TITLE='Spectra Comparison',XTITLE='Waveglength (um)',YTITLE='Flux (erg cm!E-2!N s!E-1!N '+micron+'!E-1!N)', /nodata 
    oplot, data_wavelength, model_flux_obsteff, COLOR=red
    oplot, data_wavelength, data_flux, COLOR=blue    
    
    device,/close

    print, 'one done'

endfor

set_plot,'x'

END
