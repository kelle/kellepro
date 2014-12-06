pro spexplot

U11982=readfits('/astro/h_data/kelle/data/irtf/aug03/n1/done/U11982.fits')
s2M1726=readfits('/astro/h_data/kelle/data/irtf/aug03/n1/done/2M1726.fits')

;set_plot, 'ps'
;device, filename='/astro/h_data/kelle/data/irtf/aug03/n1/done/plot.ps', encapsulated=0, /helvetica,/landscape, $
;xsize=10.5, ysize=8.6,/inches, xoffset=-0.1, yoffset=11

;plot, U11982[*,0], U11982[*,1];, psym=1
;oplot, s2M1726[*,0], U11982[*,1];, psym=1

;device, /close
;set_plot, 'x'

plotname='/astro/h_data/kelle/data/irtf/aug03/n1/plot.ps'

;---

!P.Multi=[0,1,2,0,1]
set_plot, 'ps'
device, filename=plotname, encapsulated=0, /helvetica, $
landscape=0, ysize=10.0, xsize=8, /inches, xoffset=0, yoffset=0

!Y.Margin=[0,2]

plot, s2M1726[*,0], s2M1726[*,1],xr=[0.8,2.5], xstyle=1, xtickname=[' ',' ',' ',' ',' '],ytitle='Flux'
;, yr=[0,3.0], ystyle=1,yminor=5,$
;charsize=1.1, xtickname=[' ',' ',' ',' ',' '], xstyle=1,$
;ytitle='Normalized Flux+Constant'

!Y.Margin=[4,0]
plot,U11982[*,0], U11982[*,1],xr=[0.8,2.5], xstyle=1,xtitle='Wavelength (!Z(00C5))',ytitle='Flux'

;, yr=[0,2.5], ystyle=1,yminor=5,$
;ytitle='Normalized Flux+Constant', charsize=1.1, xtitle='Wavelength (!Z(00C5
;))',$
;xstyle=1

device, /close
!P.Multi=0
set_plot,'x'

END
