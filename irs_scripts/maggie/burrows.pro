pro burrows, ref, wave1, fluxa, wave2, fluxb, max_all, $
             unbin_burrows=unbin_burrows, graph_dir=graph_dir

;********************* Create Burrows Graphs *********************

; Read in the data for the Burrows models
; Only read wavelength once---it is the same for all of the models.

readcol, $
  '/data/hillary/1/kelle/Burrows_models/c86.21_T1300_g4.5_3cld30', $
  format='X, X, D, X, X, D', lambda, flux1300
readcol, $
  '/data/hillary/1/kelle/Burrows_models/c86.21_T1400_g4.5_3cld30', $
  format='X, X, X, X, X, D', flux1400
readcol, $
  '/data/hillary/1/kelle/Burrows_models/c86.21_T1500_g4.5_3cld30', $
  format='X, X, X, X, X, D', flux1500
readcol, $ 
  '/data/hillary/1/kelle/Burrows_models/c86.21_T1600_g4.5_3cld30', $
  format='X, X, X, X, X, D', flux1600
readcol, $
  '/data/hillary/1/kelle/Burrows_models/c86.21_T1700_g4.5_3cld30', $
  format='X, X, X, X, X, D', flux1700
readcol, $
  '/data/hillary/1/kelle/Burrows_models/c86.21_T1800_g4.5_3cld30', $
  format='X, X, X, X, X, D', flux1800

  
; Create one array containing all of the wavelengths of our data so
; that the Burrows models can be rebinned to that wavelength range.
totalsize=size(wave1, /n_elements)+size(wave2, /n_elements)
totalwave=fltarr(totalsize)

for i=0, size(wave1, /n_elements)-1 do begin
    totalwave(i)=wave1(i)
endfor
for i=size(wave1, /n_elements), totalsize-1 do begin
    totalwave(i)=wave2(i-size(wave1, /n_elements))
endfor

; Find the average flux of the wavelengths between 7.7 and 8.1 microns
; so that the models and the data can be normalized.
av_data=mean(fluxb(where(wave2 gt 9 and wave2 lt 9.75)))
av_1800=mean(flux1800(where(lambda gt 9 and lambda lt 9.75)))
av_1700=mean(flux1700(where(lambda gt 9 and lambda lt 9.75)))
av_1600=mean(flux1600(where(lambda gt 9 and lambda lt 9.75)))
av_1500=mean(flux1500(where(lambda gt 9 and lambda lt 9.75)))
av_1400=mean(flux1400(where(lambda gt 9 and lambda lt 9.75)))
av_1300=mean(flux1300(where(lambda gt 9 and lambda lt 9.75)))


; Rebin the data from the Burrows models
rebin1800=interpol(flux1800/av_1800, lambda, totalwave)
rebin1700=interpol(flux1700/av_1700, lambda, totalwave)
rebin1600=interpol(flux1600/av_1600, lambda, totalwave)
rebin1500=interpol(flux1500/av_1500, lambda, totalwave)
rebin1400=interpol(flux1400/av_1400, lambda, totalwave)
rebin1300=interpol(flux1300/av_1300, lambda, totalwave)

; Resize the data flux
flux1=fluxa/av_data
flux2=fluxb/av_data


;Graph the output to a postscript file
set_plot, 'ps'
device, $
  filename=graph_dir+ref+'_burrows.ps',$
  /inches, xsize=7, ysize=10.5, xoffset=0.75,$
      yoffset=0.2, encapsulated=0, /portrait

!P.MULTI=[0, 2, 3, 1, 0]

plot, totalwave, rebin1800, $
  xtitle='Wavelength (microns)', $
  ytitle='Flux (relative to flux at 9-9.75 microns)', thick=1, $
  xrange=[4.5, 16], xstyle=1, yrange=[0, max_all], title=$
  'Burrows T=1800 Spectrum and '+ref+' Averaged Spectrum'
oplot, wave1, flux1, thick=2
oplot, wave2, flux2, thick=2

plot, totalwave, rebin1700, $
  xtitle='Wavelength (microns)', $
  ytitle='Flux (relative to flux at 9-9.75 microns)', thick=1, $
  xrange=[4.5, 16], yrange=[0, max_all], title=$
  'Burrows T=1700 Spectrum and '+ref+' Averaged Spectrum'
oplot, wave1, flux1, thick=2
oplot, wave2, flux2, thick=2

plot, totalwave, rebin1600, $
  xtitle='Wavelength (microns)', $
  ytitle='Flux (relative to flux at 9-9.75 microns)', thick=1, $ 
  xrange=[4.5, 16], yrange=[0, max_all], title=$
  'Burrows T=1600 Spectrum and '+ref+' Averaged Spectrum'
oplot, wave1, flux1, thick=2
oplot, wave2, flux2, thick=2

plot, totalwave, rebin1500, $
  xtitle='Wavelength (microns)', $
  ytitle='Flux (relative to flux at 9-9.75 microns)', thick=1, $
  xrange=[4.5, 16], yrange=[0, max_all], title=$
  'Burrows T=1500 Spectrum and '+ref+' Averaged Spectrum'
oplot, wave1, flux1, thick=2
oplot, wave2, flux2, thick=2

plot, totalwave, rebin1400, $
  xtitle='Wavelength (microns)', $
  ytitle='Flux (relative to flux at 9-9.75 microns)', thick=1, $
  xrange=[4.5, 16], yrange=[0, max_all], title=$
  'Burrows T=1400 Spectrum and '+ref+' Averaged Spectrum'
oplot, wave1, flux1, thick=2
oplot, wave2, flux2, thick=2

plot, totalwave, rebin1300, $
  xtitle='Wavelength (microns)', $
  ytitle='Flux (relative to flux at 9-9.75 microns)', thick=1, $
  xrange=[4.5, 16], yrange=[0, max_all], title=$
  'Burrows T=1300 Spectrum and '+ref+' Averaged Spectrum'
oplot, wave1, flux1, thick=2
oplot, wave2, flux2, thick=2

device, /close                    

;********************* Create Unbinned Burrows Graphs *********************

; If you want to plot the unbinned burrows data as well:
if keyword_set(unbin_burrows) eq 1 then begin

; Find maximum flux value in averaged array for normalization
    maxflux=(max(flux1)>max(flux2))

    device, $
      filename=graph_dir+ref+'_burrows_unbinned.ps',$
      /inches, xsize=7, ysize=10.5, xoffset=0.75,$
      yoffset=0.2, encapsulated=0, /portrait

    !P.MULTI=[0, 2, 3, 1, 0]


    ; Create plots
    plot, lambda, flux1800/av_1800, $
      xtitle='Wavelength (microns)', $
      ytitle='Flux (relative to flux at 9-9.75 microns)', thick=1, $
      xrange=[4.5, 16], yrange=[0, max_all], title=$
      'Burrows Unbinned T=1800 Spectrum and '+ref+' Averaged Spectrum'
    oplot, wave1, flux1, thick=2
    oplot, wave2, flux2, thick=2

    plot, lambda, flux1700/av_1700, $
      xtitle='Wavelength (microns)', $
      ytitle='Flux (relative to flux at 9-9.75 microns)', thick=1, $
      xrange=[4.5, 16], yrange=[0, max_all], title=$
      'Burrows Unbinned T=1700 Spectrum and '+ref+' Averaged Spectrum'
    oplot, wave1, flux1, thick=2
    oplot, wave2, flux2, thick=2

    plot, lambda, flux1600/av_1600, $
      xtitle='Wavelength (microns)', $
      ytitle='Flux (relative to flux at 9-9.75 microns)', thick=1, $ 
      xrange=[4.5, 16], yrange=[0, max_all], title=$
      'Burrows Unbinned T=1600 Spectrum and '+ref+' Averaged Spectrum'
    oplot, wave1, flux1, thick=2
    oplot, wave2, flux2, thick=2

    plot, lambda, flux1500/av_1500, $
      xtitle='Wavelength (microns)', $
      ytitle='Flux (relative to flux at 9-9.75 microns)', thick=1, $
      xrange=[4.5, 16], yrange=[0, max_all], title=$
      'Burrows Unbinned T=1500 Spectrum and '+ref+' Averaged Spectrum'
    oplot, wave1, flux1, thick=2
    oplot, wave2, flux2, thick=2

    plot, lambda, flux1400/av_1400, $
      xtitle='Wavelength (microns)', $
      ytitle='Flux (relative to flux at 9-9.75 microns)', thick=1, $
      xrange=[4.5, 16], yrange=[0, max_all], title=$
      'Burrows Unbinned T=1400 Spectrum and '+ref+' Averaged Spectrum'
    oplot, wave1, flux1, thick=2
    oplot, wave2, flux2, thick=2

    plot, lambda, flux1300/av_1300, $
      xtitle='Wavelength (microns)', $
      ytitle='Flux (relative to flux at 9-9.75 microns)', thick=1, $
      xrange=[4.5, 16], yrange=[0, max_all], title=$
      'Burrows Unbinned T=1300 Spectrum and '+ref+' Averaged Spectrum'
    oplot, wave1, flux1, thick=2
    oplot, wave2, flux2, thick=2

    device, /close

endif

set_plot, 'x'
end
