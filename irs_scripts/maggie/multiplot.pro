; Creates a !P.Multi plot of the good spectra

pro multiplot

restore, '/data/hillary/1/mkirklan/home/saved/U10287.sav'
flux1_1=flux1
flux1_2=flux2

restore, '/data/hillary/1/mkirklan/home/saved/U20701.sav'
flux2_1=flux1
flux2_2=flux2

restore, '/data/hillary/1/mkirklan/home/saved/U12054.sav'
flux3_1=flux1
flux3_2=flux2

restore, '/data/hillary/1/mkirklan/home/saved/U20336.sav'
flux4_1=flux1
flux4_2=flux2

restore, '/data/hillary/1/mkirklan/home/saved/U10390.sav'
flux5_1=flux1
flux5_2=flux2

restore, '/data/hillary/1/mkirklan/home/saved/U10151.sav'
flux6_1=flux1
flux6_2=flux2

restore, '/data/hillary/1/mkirklan/home/saved/U20568.sav'
flux7_1=flux1
flux7_2=flux2

restore, '/data/hillary/1/mkirklan/home/saved/U10617.sav'
flux8_1=flux1
flux8_2=flux2

restore, '/data/hillary/1/mkirklan/home/saved/U10088.sav'
flux9_1=flux1
flux9_2=flux2

restore, '/data/hillary/1/mkirklan/home/saved/U20587.sav'
flux10_1=flux1
flux10_2=flux2


;------------------------------------------------------------
set_plot, 'ps'
device, $
  filename='/data/hillary/1/mkirklan/home/graphs/good_spectra.ps',$
  /inches, xsize=7, ysize=10.5, xoffset=0.75,$
      yoffset=0.2, encapsulated=0, /portrait

!P.Multi = [0, 2, 5, 1, 0]

plot, wave1, flux1_1, xrange=[4.5, 16], xstyle=1, $
  title='Spectrum of U10287 - M7.5', xtitle='Wavelenth (microns)', $
  ytitle='Flux (relative to average flux from 9-9.75 microns)', thick=1
oplot, wave2, flux1_2

plot, wave1, flux2_1, xrange=[4.5, 16],  xstyle=1, $
  title='Spectrum of U20701 - M9', xtitle='Wavelenth (microns)', $
  ytitle='Flux (relative to average flux from 9-9.75 microns)', thick=1
oplot, wave2, flux2_2

plot, wave1, flux3_1, xrange=[4.5, 16],  xstyle=1, $
  title='Spectrum of U10254 - L1.5', xtitle='Wavelenth (microns)', $
  ytitle='Flux (relative to average flux from 9-9.75 microns)', thick=1
oplot, wave2, flux3_2

plot, wave1, flux4_1, xrange=[4.5, 16],  xstyle=1, $
  title='Spectrum of U20336 - L2', xtitle='Wavelenth (microns)', $
  ytitle='Flux (relative to average flux from 9-9.75 microns)', thick=1
oplot, wave2, flux4_2

plot, wave1, flux5_1, xrange=[4.5, 16],  xstyle=1, $
  title='Spectrum of U10390 - L2.5', xtitle='Wavelenth (microns)', $
  ytitle='Flux (relative to average flux from 9-9.75 microns)', thick=1
oplot, wave2, flux5_2

plot, wave1, flux6_1, xrange=[4.5, 16],  xstyle=1, $
  title='Spectrum of U10151 - L3', xtitle='Wavelenth (microns)', $
  ytitle='Flux (relative to average flux from 9-9.75 microns)', thick=1
oplot, wave2, flux6_2

plot, wave1, flux7_1, xrange=[4.5, 16],  xstyle=1, $
  title='Spectrum of U20568 - L3', xtitle='Wavelenth (microns)', $
  ytitle='Flux (relative to average flux from 9-9.75 microns)', thick=1
oplot, wave2, flux7_2

plot, wave1, flux8_1, xrange=[4.5, 16],  xstyle=1, $
  title='Spectrum of U10167 - L3.5', xtitle='Wavelenth (microns)', $
  ytitle='Flux (relative to average flux from 9-9.75 microns)', thick=1
oplot, wave2, flux8_2

plot, wave1, flux9_1, xrange=[4.5, 16],  xstyle=1, $
  title='Spectrum of U10088 - L5', xtitle='Wavelenth (microns)', $
  ytitle='Flux (relative to average flux from 9-9.75 microns)', thick=1
oplot, wave2, flux9_2

plot, wave1, flux10_1, xrange=[4.5, 16],  xstyle=1, $
  title='Spectrum of U20587 - L5', xtitle='Wavelenth (microns)', $
  ytitle='Flux (relative to average flux from 9-9.75 microns)', thick=1
oplot, wave2, flux10_2

device, /close
set_plot, 'x'

!P.Multi=0
;--------------------------------------------------------------------

end
