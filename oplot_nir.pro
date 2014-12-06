PRO oplot_nir, fits_file,t=t,g=g,f=f,outfile=outfile

IF N_params() lt 1 then begin
     print,"Syntax - overplot, 'file.fits' (, t=, g=, f=)"
     print, 'file.fits is expected to be in /data/hillary/3/lauren/'
     print, "example: oplot_nir, '2M0036.fits', t=1500, g=300, f=1"
     return
 ENDIF

IF N_elements(t) eq 0 then t='*' else t=strn(t)
IF N_elements(g) eq 0 then g='*' else g=strn(g)
IF N_elements(f) eq 0 then f='*' else f=strn(f)

if g eq '100' then g2 = '4'
if g eq '300' then g2 ='4.5'
if g eq '1000' then g2 = '5'
if g eq '3000' then g2 = '5.5'

root= '/data/hillary/3/lauren/'
data_dir='/data/hillary/3/kelle/nir_spectra/2M_all/fits/'

model_dir='/data/hillary/1/kelle/Saumon_Models/'
model_file=FILE_SEARCH(model_dir,'sp_t'+t+'g'+g+'f'+f+'.fits')
num_model_files=n_elements(model_file)

;------------------
;read in data file
;------------------

    spectrum=READFITS(data_dir+fits_file, h,/silent)
  ;  good_data=where(spectrum[*,0] lt 2.5)
    data_wavelength=spectrum[*,0]
    data_flux=spectrum[*,1]

    good_data= where(data_wavelength lt 2.5)
    data_wavelength=data_wavelength[good_data]
    data_flux=data_flux[good_data]*data_wavelength^2 ; converting from W/m^2/micron to Jy

;eliminate nan
    nan_loc=WHERE(FINITE(data_flux,/NAN))
    IF nan_loc[0] NE -1 THEN data_flux[nan_loc]=0
 
For j=0, num_model_files-1 DO BEGIN
;-------------------
;read in model file
;-------------------

        spectrum=READFITS(model_file[j],/silent)
        model_wavelength=spectrum[*,0]
        model_flux=spectrum[*,1]

;NORMALIZE to J-band peak
        nw1=1.2
        nw2=1.3
    
        data_norm=AVGFLUX(nw1, nw2, data_wavelength, data_flux)
        data_flux=data_flux/data_norm   
;interpol
        model_flux_rebin=INTERPOL(model_flux, model_wavelength, data_wavelength)
;normalization of model
        model_norm=AVGFLUX(nw1, nw2, data_wavelength, model_flux_rebin)
;Scale model to data

        model_flux_rebin=model_flux_rebin/model_norm

ymax = max(data_flux)

if keyword_set(outfile) then begin
set_plot,'ps'
device, file='/data/hillary/3/lauren/idl/plots/new3/'+outfile,encapsulate=1,/helvetica
!p.font=0
endif

loadct, 39
red=240
green=140
blue=48

 plot, data_wavelength, model_flux_rebin,XTITLE='Waveglength (um)',YTITLE='Flux Density (Jy)', /nodata, yr=[0,3.0],xr=[0.8,2.5],xstyle=1
        TITLE='Spectra Comparison'
        oplot, data_wavelength, model_flux_rebin, linestyle=0, color=red,thick=2
        oplot, data_wavelength, data_flux,color=blue,thick=2

       ;xyouts,0.7,0.15, 'Data:' + FILE_BASENAME(fits_file),/normal
       ;xyouts,0.7,0.1,'Model:'+FILE_BASENAME(model_file[j]),/normal
        xyouts,2,0.5, 'Best Fit Model:'
        xyouts,2.0,0.4,'Temp = '+t
        xyouts,2.0,0.3,'log(g) = '+ g2
        xyouts,2.0,0.2,'Fsed = ' +f
;      xyouts,1.5,0.3, 'Chisqu:' +strn(a_chisqu[goodfits[k]])
;       xyouts, 1.5, 0.2, 'Normalization J-band Peak:1.2-1.3'
   
       xyouts, 0.85,1.8, 'Model',color=red
       oplot, [1.0,1.15],[1.83,1.83],linestyle=0, color=red,thick=2

       xyouts, 0.85,1.7, 'Data',color=blue
       oplot,  [1.0,1.15],[1.73,1.73],color=blue,thick=2


if keyword_set(outfile) then begin
device,/close
set_plot,'x'
endif

IF  num_model_files gt 1 AND j ne num_model_files -1 then begin
  print, ' '
  print, 'Press any key to continue.'
  print, ''
  anykey=GET_KBRD(1)
ENDIF

ENDFOR

END
