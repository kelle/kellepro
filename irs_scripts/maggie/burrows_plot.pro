; Creates plots of all of the 2nd generation burrows models
; Some EXAMPLES on Pgs 69-70 of LAB NOTEBOOK 

pro burrows_plot


data='/data/hillary/1/mkirklan/home/burrows_models/'

;------------------------------------
; Read in different temperature data - all at 4.5 g (900-2000K)
readcol, $
  data+'c91.21_T900_g4.5_f100_6new', $
  format='X, X, X, X, X, D', flux900_4_5
readcol, $
  data+'c91.21_T1000_g4.5_f100_6new', $
  format='X, X, X, X, X, D', flux1000_4_5
readcol, $
  data+'c91.21_T1100_g4.5_f100_6new', $
  format='X, X, X, X, X, D', flux1100_4_5
readcol, $
  data+'c91.21_T1200_g4.5_f100_6new', $
  format='X, X, X, X, X, D', flux1200_4_5
readcol, $
  data+'c91.21_T1300_g4.5_f100_6new', $
  format='X, X, X, X, X, D', flux1300_4_5
readcol, $
  data+'c91.21_T1400_g4.5_f100_6new', $
  format='X, X, X, X, X, D', flux1400_4_5
readcol, $
  data+'c91.21_T1500_g4.5_f100_6new', $
  format='X, X, X, X, X, D', flux1500_4_5
readcol, $
  data+'c91.21_T1600_g4.5_f100_6new', $
  format='X, X, X, X, X, D', flux1600_4_5
readcol, $
  data+'c91.21_T1700_g4.5_f100_6new', $
  format='X, X, X, X, X, D', flux1700_4_5
readcol, $
  data+'c91.21_T1800_g4.5_f100_6new', $
  format='X, X, X, X, X, D', flux1800_4_5
readcol, $
  data+'c91.21_T1900_g4.5_f100_6new', $
  format='X, X, X, X, X, D', flux1900_4_5
readcol, $
  data+'c91.21_T2000_g4.5_f100_6new', $
  format='X, X, X, X, X, D', flux2000_4_5

;-------------------------------------
; Read in different temperature data - all at 5.0 g (700-2200K)
readcol, $
  data+'c91.21_T700_g5.0_f100_6new', $
  format='X, X, D, X, X, D', lambda, flux700_5_0
readcol, $
  data+'c91.21_T800_g5.0_f100_6new', $
  format='X, X, X, X, X, D', flux800_5_0
readcol, $
  data+'c91.21_T900_g5.0_f100_6new', $
  format='X, X, X, X, X, D', flux900_5_0
readcol, $
  data+'c91.21_T1000_g5.0_f100_6new', $
  format='X, X, X, X, X, D', flux1000_5_0
readcol, $
  data+'c91.21_T1100_g5.0_f100_6new', $
  format='X, X, X, X, X, D', flux1100_5_0
readcol, $
  data+'c91.21_T1200_g5.0_f100_6new', $
  format='X, X, X, X, X, D', flux1200_5_0
readcol, $
  data+'c91.21_T1300_g5.0_f100_6new', $
  format='X, X, X, X, X, D', flux1300_5_0
readcol, $
  data+'c91.21_T1400_g5.0_f100_6new', $
  format='X, X, X, X, X, D', flux1400_5_0
readcol, $
  data+'c91.21_T1500_g5.0_f100_6new', $
  format='X, X, X, X, X, D', flux1500_5_0
readcol, $
  data+'c91.21_T1600_g5.0_f100_6new', $
  format='X, X, X, X, X, D', flux1600_5_0
readcol, $
  data+'c91.21_T1700_g5.0_f100_6new', $
  format='X, X, X, X, X, D', flux1700_5_0
readcol, $
  data+'c91.21_T1800_g5.0_f100_6new', $
  format='X, X, X, X, X, D', flux1800_5_0
readcol, $
  data+'c91.21_T1900_g5.0_f100_6new', $
  format='X, X, X, X, X, D', flux1900_5_0
readcol, $
  data+'c91.21_T2000_g5.0_f100_6new', $
  format='X, X, X, X, X, D', flux2000_5_0
readcol, $
  data+'c91.21_T2100_g5.0_f100_6new', $
  format='X, X, X, X, X, D', flux2100_5_0
readcol, $
  data+'c91.21_T2200_g5.0_f100_6new', $
  format='X, X, X, X, X, D', flux2200_5_0

;-----------------------------------------
; Read in different temperature data - all at 5.5 g (800-2000K)
readcol, $
  data+'c91.21_T800_g5.5_f100_6new', $
  format='X, X, X, X, X, D', flux800_5_5
readcol, $
  data+'c91.21_T900_g5.5_f100_6new', $
  format='X, X, X, X, X, D', flux900_5_5
readcol, $
  data+'c91.21_T1000_g5.5_f100_6new', $
  format='X, X, X, X, X, D', flux1000_5_5
readcol, $
  data+'c91.21_T1100_g5.5_f100_6new', $
  format='X, X, X, X, X, D', flux1100_5_5
readcol, $
  data+'c91.21_T1200_g5.5_f100_6new', $
  format='X, X, X, X, X, D', flux1200_5_5
readcol, $
  data+'c91.21_T1300_g5.5_f100_6new', $
  format='X, X, X, X, X, D', flux1300_5_5
readcol, $
  data+'c91.21_T1400_g5.5_f100_6new', $
  format='X, X, X, X, X, D', flux1400_5_5
readcol, $
  data+'c91.21_T1500_g5.5_f100_6new', $
  format='X, X, X, X, X, D', flux1500_5_5
readcol, $
  data+'c91.21_T1600_g5.5_f100_6new', $
  format='X, X, X, X, X, D', flux1600_5_5
readcol, $
  data+'c91.21_T1700_g5.5_f100_6new', $
  format='X, X, X, X, X, D', flux1700_5_5
readcol, $
  data+'c91.21_T1800_g5.5_f100_6new', $
  format='X, X, X, X, X, D', flux1800_5_5
readcol, $
  data+'c91.21_T1900_g5.5_f100_6new', $
  format='X, X, X, X, X, D', flux1900_5_5
readcol, $
  data+'c91.21_T2000_g5.5_f100_6new', $
  format='X, X, X, X, X, D', flux2000_5_5


;------------------------------------------------------
; Create postscript output

; Temperature Composite Graph 
set_plot, 'ps'

device, filename='/data/hillary/1/mkirklan/home/graphs/burrows_plots/'+$
        'temperature_composite_5_0.ps', /landscape, encapsulated=0, $
        font_size=12, /inches, xsize=10.5, ysize=7.1, $
        xoffset=0, yoffset=11.0

plot, lambda, flux2200_5_0+5, $
  xrange=[4.5, 16], xstyle=1, yrange=[-0.5, 18], ystyle=1, title= $
  'Burrows Model Spectra - 5.0 g - Temperatures 1000K to 2200K',$
  xtitle='Wavelenth (microns)', ytitle='Flux (mJy) + constant', $
  thick=1

oplot, lambda, flux1900_5_0+3.5
oplot, lambda, flux1600_5_0+2
oplot, lambda, flux1300_5_0+1
oplot, lambda, flux1000_5_0

oplot, [4.5, 5.5], [1, 1], linestyle=2
oplot, [4.5, 5.5], [2, 2], linestyle=2
oplot, [4.5, 5.5], [3.5, 3.5], linestyle=2
oplot, [4.5, 5.5], [5, 5], linestyle=2

xyouts, 14.5, 1, '1000 K'
xyouts, 14.5, 2.25, '1300 K'
xyouts, 14.5, 3.75, '1600 K'
xyouts, 14.5, 5.75, '1900 K'
xyouts, 14.5, 7.75, '2200 K'

device, /close
set_plot, 'x'


; Gravity Composite Graphs 
set_plot, 'ps'
device, filename='/data/hillary/1/mkirklan/home/graphs/burrows_plots/'+$
        'gravity_composite_1800.ps', /landscape, encapsulated=0

plot, lambda, flux1800_4_5+5, $
  xrange=[4.5, 16], xstyle=1, yrange=[0.5, 19], ystyle=1, title= $
  'Burrows Model Spectra - 1800K - Gravities from 4.5 g to 5.5 g', $
  xtitle='Wavelength (microns)', ytitle='Flux (mJy) + constant',$
  thick=1

oplot, lambda, flux1800_5_0+2
oplot, lambda, flux1800_5_5

oplot, [4.5, 5.5], [2, 2], linestyle=2
oplot, [4.5, 5.5], [5, 5], linestyle=2

xyouts, 14.5, 1.5, 'g=5.5'
xyouts, 14.5, 4.0, 'g=5.0'
xyouts, 14.5, 8.0, 'g=4.5'
device, /close
set_plot, 'x'


; Temperature Individual Graphs - 4.5 g
set_plot, 'ps'
device, filename='/data/hillary/1/mkirklan/home/graphs/burrows_plots/'+$
        'individual_4_5.ps', /landscape
plot, lambda, flux900_4_5, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 900K - 4.5 g'
plot, lambda, flux1000_4_5, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 1000K - 4.5 g'
plot, lambda, flux1100_4_5, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 1100K - 4.5 g'
plot, lambda, flux1200_4_5, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 1200K - 4.5 g'
plot, lambda, flux1300_4_5, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 1300K - 4.5 g'
plot, lambda, flux1400_4_5, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 1400K - 4.5 g'
plot, lambda, flux1500_4_5, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 1500K - 4.5 g'
plot, lambda, flux1600_4_5, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 1600K - 4.5 g'
plot, lambda, flux1700_4_5, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 1700K - 4.5 g'
plot, lambda, flux1800_4_5, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 1800K - 4.5 g'
plot, lambda, flux1900_4_5, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 1900K - 4.5 g'
plot, lambda, flux2000_4_5, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 2000K - 4.5 g'
device, /close
set_plot, 'x'


; Individual Graphs - 5.0 g
set_plot, 'ps'
device, filename='/data/hillary/1/mkirklan/home/graphs/burrows_plots/'+$
        'individual_5_0.ps', /landscape
plot, lambda, flux700_5_0, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 700K - 5.0 g'
plot, lambda, flux800_5_0, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 800K - 5.0 g'
plot, lambda, flux900_5_0, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 900K - 5.0 g'
plot, lambda, flux1000_5_0, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 1000K - 5.0 g'
plot, lambda, flux1100_5_0, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 1100K - 5.0 g'
plot, lambda, flux1200_5_0, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 1200K - 5.0 g'
plot, lambda, flux1300_5_0, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 1300K - 5.0 g'
plot, lambda, flux1400_5_0, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 1400K - 5.0 g'
plot, lambda, flux1500_5_0, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 1500K - 5.0 g'
plot, lambda, flux1600_5_0, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 1600K - 5.0 g'
plot, lambda, flux1700_5_0, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 1700K - 5.0 g'
plot, lambda, flux1800_5_0, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 1800K - 5.0 g'
plot, lambda, flux1900_5_0, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 1900K - 5.0 g'
plot, lambda, flux2000_5_0, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 2000K - 5.0 g'
plot, lambda, flux2100_5_0, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 2100K - 5.0 g'
plot, lambda, flux2200_5_0, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 2200K - 5.0 g'
device, /close
set_plot, 'x'


; Temperature Individual Graphs - 5.5 g
set_plot, 'ps'
device, filename='/data/hillary/1/mkirklan/home/graphs/burrows_plots/'+$
        'individual_5_5.ps', /landscape
plot, lambda, flux900_5_5, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 900K - 5.5 g'
plot, lambda, flux1000_5_5, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 1000K - 5.5 g'
plot, lambda, flux1100_5_5, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 1100K - 5.5 g'
plot, lambda, flux1200_5_5, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 1200K - 5.5 g'
plot, lambda, flux1300_5_5, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 1300K - 5.5 g'
plot, lambda, flux1400_5_5, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 1400K - 5.5 g'
plot, lambda, flux1500_5_5, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 1500K - 5.5 g'
plot, lambda, flux1600_5_5, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 1600K - 5.5 g'
plot, lambda, flux1700_5_5, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 1700K - 5.5 g'
plot, lambda, flux1800_5_5, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 1800K - 5.5 g'
plot, lambda, flux1900_5_5, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 1900K - 5.5 g'
plot, lambda, flux2000_5_5, $
  xtitle=wavelenth, ytitle=flux, thick=1, $
  xrange=[4.5, 16], xstyle=1, title= $
  'Burrows Model Spectrum  - 2000K - 5.5 g'
device, /close
set_plot, 'x'



end
