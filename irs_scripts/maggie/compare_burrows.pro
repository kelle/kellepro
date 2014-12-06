; Creates plots in which appropriate Burrows models are plotted against each
; of the good spectra

pro compare_burrows

;--------------------------------------------------------------------
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

;--------------------------------------------------------------------
data='/data/hillary/1/mkirklan/home/burrows_models/'

; Read in different temperature data - all at 4.5 g (1700-2000K)
;readcol, $
;  data+'c91.21_T1700_g4.5_f100_6new', $
;  format='X, X, D, X, X, D', lambda, flux1700_4_5
;readcol, $
;  data+'c91.21_T1800_g4.5_f100_6new', $
;  format='X, X, X, X, X, D', flux1800_4_5
;readcol, $
;  data+'c91.21_T1900_g4.5_f100_6new', $
;  format='X, X, X, X, X, D', flux1900_4_5
;readcol, $
;  data+'c91.21_T2000_g4.5_f100_6new', $
;  format='X, X, X, X, X, D', flux2000_4_5

; Read in different temperature data - all at 5.0 g (1700-2200K)
;readcol, $
;  data+'c91.21_T1700_g5.0_f100_6new', $
;  format='X, X, X, X, X, D', flux1700_5_0
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
  format='X, X, D, X, X, D', lambda, flux2200_5_0

; Read in different temperature data - all at 5.5 g (1700-2000K)
;readcol, $
;  data+'c91.21_T1700_g5.5_f100_6new', $
;  format='X, X, X, X, X, D', flux1700_5_5
;readcol, $
;  data+'c91.21_T1800_g5.5_f100_6new', $
;  format='X, X, X, X, X, D', flux1800_5_5
;readcol, $
;  data+'c91.21_T1900_g5.5_f100_6new', $
;  format='X, X, X, X, X, D', flux1900_5_5
;readcol, $
;  data+'c91.21_T2000_g5.5_f100_6new', $
;  format='X, X, X, X, X, D', flux2000_5_5

;---------------------------------------------------------------------
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

; Find average values for data and models
av_data1=mean(flux1_2(where(wave2 gt 9 and wave2 lt 9.75)))
av_data2=mean(flux2_2(where(wave2 gt 9 and wave2 lt 9.75)))
av_data5=mean(flux5_2(where(wave2 gt 9 and wave2 lt 9.75)))
av_data7=mean(flux7_2(where(wave2 gt 9 and wave2 lt 9.75)))

av2200_5_0=mean(flux2200_5_0(where(lambda gt 9 and lambda lt 9.75)))
av2100_5_0=mean(flux2100_5_0(where(lambda gt 9 and lambda lt 9.75)))
av2000_5_0=mean(flux2000_5_0(where(lambda gt 9 and lambda lt 9.75)))
av1900_5_0=mean(flux1900_5_0(where(lambda gt 9 and lambda lt 9.75)))
av1800_5_0=mean(flux1800_5_0(where(lambda gt 9 and lambda lt 9.75)))

; Rebin the data from the Burrows models
rebin2200_5_0=interpol(flux2200_5_0/av2200_5_0, lambda, totalwave)
rebin2100_5_0=interpol(flux2100_5_0/av2100_5_0, lambda, totalwave)
rebin2000_5_0=interpol(flux2000_5_0/av2000_5_0, lambda, totalwave)
rebin1900_5_0=interpol(flux1900_5_0/av1900_5_0, lambda, totalwave)
rebin1800_5_0=interpol(flux1800_5_0/av1800_5_0, lambda, totalwave)

;----------------------------------------------------------------------
; U10287 Plot
set_plot, 'ps'
device, $
  filename='/data/hillary/1/mkirklan/home/graphs/U10287/U10287_comparison.eps',$
  /landscape, encapsulated=1, font_size=12
plot, wave1-.062, flux1_1/av_data1+0.5, xtitle='Wavelength (microns)', $
  ytitle='Flux (relative to flux at 9-9.75 microns) + constant', thick=2, $
  xrange=[4.5, 16], xstyle=1, yrange=[0, max(flux1_1/av_data1+0.5)+0.35], $
  ystyle=1, title='U10287 and Burrows Model Spectrum'
oplot, wave2-.062, flux1_2/av_data1+0.5, thick=2
oplot, totalwave, rebin2200_5_0, thick=1
xyouts, 13.25, 0.55, 'Data - U10287 - M7.5'
xyouts, 13.25, 0.23, 'Model - 2200K - 5.0 g'

; Create dashed line to show the zero point of the data
oplot, [4.5, 5.5], [0.5, 0.5], linestyle=2

; Denote water feature: 5.25-6.25
oplot, [5.25, 5.25], $
  [max(flux1_1/av_data1+0.5)+0.1, max(flux1_1/av_data1+0.5)+0.2], $
  linestyle=0, thick=1.5
oplot, [6.25, 6.25], $
  [max(flux1_1/av_data1+0.5)+0.1, max(flux1_1/av_data1+0.5)+0.2], $
  thick=1.5
oplot, [5.25, 6.25], $
  [max(flux1_1/av_data1+0.5)+0.2, max(flux1_1/av_data1+0.5)+0.2], $
  thick=1.5
xyouts, 5.6, max(flux1_1/av_data1+0.5)+0.25, 'H!D2!NO', charthick=1.5

; Denote water feature: 6.5-7.0
oplot, [6.5, 6.5], $
  [max(flux1_1/av_data1+0.5)+0.1, max(flux1_1/av_data1+0.5)+0.2], $
  linestyle=0, thick=1.5
oplot, [7.0, 7.0], $
  [max(flux1_1/av_data1+0.5)+0.1, max(flux1_1/av_data1+0.5)+0.2], $
  thick=1.5
oplot, [6.5, 7.0], $
  [max(flux1_1/av_data1+0.5)+0.2, max(flux1_1/av_data1+0.5)+0.2], $
  thick=1.5
xyouts, 6.63, max(flux1_1/av_data1+0.5)+0.25, 'H!D2!NO', charthick=1.5

; Denote methane feature: 7.0-8.7
oplot, [7.0, 7.0], $
  [max(flux1_1/av_data1+0.5)-0.05, max(flux1_1/av_data1+0.5)+0.05], $
  linestyle=0, thick=1.5
oplot, [8.7, 8.7], $
  [max(flux1_1/av_data1+0.5)-0.05, max(flux1_1/av_data1+0.5)+0.05], $
  thick=1.5
oplot, [7.0, 8.7], $
  [max(flux1_1/av_data1+0.5)+0.05, max(flux1_1/av_data1+0.5)+0.05], $
  thick=1.5
xyouts, 7.75, max(flux1_1/av_data1+0.5)+0.1, 'CH!D4!N', charthick=1.5

device, /close

;---------------------------------------------------------------------
; U20701 Plot
set_plot, 'ps'

; eps graph - make sure xyouts is set at 13.25
;device, $
;  filename='/data/hillary/1/mkirklan/home/graphs/U20701/U20701_comparison.eps',$
;  /landscape, encapsulated=1, font_size=12, /inches, xsize=11, ysize=7.1, $
;  xoffset=0.0, yoffset=11.0

;ordinary ps graph - make sure xyouts is set at 14.25
device, $
  filename='/data/hillary/1/mkirklan/home/graphs/U20701/U20701_comparison.eps',$
  /landscape, encapsulated=1, font_size=12, /inches, xsize=11, ysize=7.1, $
  xoffset=0.0, yoffset=11.0

plot, wave1-.062, flux2_1/av_data2+0.5, xtitle='Wavelength (microns)', $
  ytitle='Flux (relative to flux at 9-9.75 microns) + constant', thick=2, $
  xrange=[4.5, 16], xstyle=1, yrange=[0, max(flux2_1/av_data2+0.5)+0.35], $
  ystyle=1, title= 'U20701 and Burrows Model Spectrum'
oplot, wave2-.062, flux2_2/av_data2+0.5, thick=2
oplot, totalwave, rebin2200_5_0, thick=1
xyouts, 13.25, .64, 'Data - U20701 - M9'
xyouts, 13.25, 0.25, 'Model - 2200K - 5.0 g'

; Create dashed line to show the zero point of data
oplot, [4.5, 5.5], [0.5, 0.5], linestyle=2

; Denote water feature: 5.25-6.25
oplot, [5.25, 5.25], $
  [max(flux2_1/av_data2+0.5)+0.1, max(flux2_1/av_data2+0.5)+0.2], $
  linestyle=0, thick=1.5
oplot, [6.25, 6.25], $
  [max(flux2_1/av_data2+0.5)+0.1, max(flux2_1/av_data2+0.5)+0.2], $
  thick=1.5
oplot, [5.25, 6.25], $
  [max(flux2_1/av_data2+0.5)+0.2, max(flux2_1/av_data2+0.5)+0.2], $
  thick=1.5
xyouts, 5.6, max(flux2_1/av_data2+0.5)+0.25, 'H!D2!NO', charthick=1.5

; Denote water feature: 6.5-7.0
oplot, [6.5, 6.5], $
  [max(flux2_1/av_data2+0.5)+0.1, max(flux2_1/av_data2+0.5)+0.2], $
  linestyle=0, thick=1.5
oplot, [7.0, 7.0], $
  [max(flux2_1/av_data2+0.5)+0.1, max(flux2_1/av_data2+0.5)+0.2], $
  thick=1.5
oplot, [6.5, 7.0], $
  [max(flux2_1/av_data2+0.5)+0.2, max(flux2_1/av_data2+0.5)+0.2], $
  thick=1.5
xyouts, 6.63, max(flux2_1/av_data2+0.5)+0.25, 'H!D2!NO', charthick=1.5

; Denote methane feature: 7.0-8.7
oplot, [7.0, 7.0], $
  [max(flux2_1/av_data2+0.5)-0.05, max(flux2_1/av_data2+0.5)+0.05], $
  linestyle=0, thick=1.5
oplot, [8.7, 8.7], $
  [max(flux2_1/av_data2+0.5)-0.05, max(flux2_1/av_data2+0.5)+0.05], $
  thick=1.5
oplot, [7.0, 8.7], $
  [max(flux2_1/av_data2+0.5)+0.05, max(flux2_1/av_data2+0.5)+0.05], $
  thick=1.5
xyouts, 7.75, max(flux2_1/av_data2+0.5)+0.1, 'CH!D4!N', charthick=1.5

device, /close

;---------------------------------------------------------------------
; U10390 Plot
set_plot, 'ps'

device, $
  filename='/data/hillary/1/mkirklan/home/graphs/U10390/U10390_comparison.eps',$
  /landscape, encapsulated=1, font_size=12, /inches, xsize=11, ysize=7.1, $
  xoffset=0.0, yoffset=11.0

plot, wave1-.062, flux5_1/av_data5+2, xtitle='Wavelength (microns)', $
  ytitle='Flux (relative to flux at 9-9.75 microns) + constant', thick=2, $
  xrange=[4.5, 16], xstyle=1, yrange=[0, max(flux5_1/av_data5+2)+0.4], $
  ystyle=1, title= 'U10390 and Burrows Model Spectra' 
oplot, wave2-.062, flux5_2/av_data5+2, thick=2

oplot, totalwave, rebin1800_5_0, thick=1
oplot, totalwave, rebin2000_5_0+0.75, thick=1
oplot, totalwave, rebin2200_5_0+1.25, thick=1

; Create dashed line to show the zero point of data
oplot, [4.5, 5.5], [2, 2], linestyle=2
oplot, [4.5, 5.5], [1.25, 1.25], linestyle=2
oplot, [4.5, 5.5], [0.75, 0.75], linestyle=2

xyouts, 13.25, 1.95, 'Data - U10390 - L2.5'
xyouts, 13.25, 0.18, 'Model - 1800K - g=5.0'
xyouts, 13.25, 0.95, 'Model - 2000K - g=5.0'
xyouts, 13.25, 1.46, 'Model - 2200K - g=5.0'

; Create vertical line to denote aligned spectral feature
oplot, [9.915, 9.915], [0, max(flux5_1/av_data5+2)+0.4], linestyle=1

; Denote water feature: 5.25-6.25
oplot, [5.25, 5.25], $
  [max(flux5_1/av_data5+2)+0.1, max(flux5_1/av_data5+2)+0.2], $
  linestyle=0, thick=1.5
oplot, [6.25, 6.25], $
  [max(flux5_1/av_data5+2)+0.1, max(flux5_1/av_data5+2)+0.2], $
  thick=1.5
oplot, [5.25, 6.25], $
  [max(flux5_1/av_data5+2)+0.2, max(flux5_1/av_data5+2)+0.2], $
  thick=1.5
xyouts, 5.6, max(flux5_1/av_data5+2)+0.25, 'H!D2!NO', charthick=1.5

; Denote water feature: 6.5-7.0
oplot, [6.5, 6.5], $
  [max(flux5_1/av_data5+2)+0.1, max(flux5_1/av_data5+2)+0.2], $
  linestyle=0, thick=1.5
oplot, [7.0, 7.0], $
  [max(flux5_1/av_data5+2)+0.1, max(flux5_1/av_data5+2)+0.2], $
  thick=1.5
oplot, [6.5, 7.0], $
  [max(flux5_1/av_data5+2)+0.2, max(flux5_1/av_data5+2)+0.2], $
  thick=1.5
xyouts, 6.63, max(flux5_1/av_data5+2)+0.25, 'H!D2!NO', charthick=1.5

; Denote methane feature: 7.0-8.7
oplot, [7.0, 7.0], $
  [max(flux5_1/av_data5+2)-0.05, max(flux5_1/av_data5+2)+0.05], $
  linestyle=0, thick=1.5
oplot, [8.7, 8.7], $
  [max(flux5_1/av_data5+2)-0.05, max(flux5_1/av_data5+2)+0.05], $
  thick=1.5
oplot, [7.0, 8.7], $
  [max(flux5_1/av_data5+2)+0.05, max(flux5_1/av_data5+2)+0.05], $
  thick=1.5
xyouts, 7.75, max(flux5_1/av_data5+2)+0.1, 'CH!D4!N', charthick=1.5

device, /close

;---------------------------------------------------------------------
; U20568 Plot
set_plot, 'ps'

device, $
  filename='/data/hillary/1/mkirklan/home/graphs/U20568/U20568_comparison.eps',$
  /landscape, encapsulated=1, font_size=12, /inches, xsize=11, ysize=7.1, $
  xoffset=0.0, yoffset=11.0

plot, wave1-.062, flux7_1/av_data7+1.7, xtitle='Wavelength (microns)', $
  ytitle='Flux (relative to flux at 9-9.75 microns) + constant', thick=2, $
  xrange=[4.5, 16], xstyle=1, yrange=[0, max(flux7_1/av_data7+1.7)+0.4], $
  ystyle=1, title= 'U20568 and Burrows Model Spectra'
oplot, wave2-.062, flux7_2/av_data7+1.7, thick=2

oplot, totalwave, rebin1800_5_0, thick=1
oplot, totalwave, rebin2000_5_0+0.75, thick=1
oplot, totalwave, rebin2200_5_0+1.25, thick=1

; Create dashed line to show the zero point of data
oplot, [4.5, 5.5], [1.7, 1.7], linestyle=2
oplot, [4.5, 5.5], [1.25, 1.25], linestyle=2
oplot, [4.5, 5.5], [0.75, 0.75], linestyle=2

xyouts, 13.25, 1.83, 'Data - U20568 - L3'
xyouts, 13.25, 0.19, 'Model - 1800K - g=5.0'
xyouts, 13.25, 0.94, 'Model - 2000K - g=5.0'
xyouts, 13.25, 1.45, 'Model - 2200K - g=5.0'

; Create vertical line to denote aligned spectral feature
oplot, [9.915, 9.915], [0, max(flux7_1/av_data7+1.7)+0.4], linestyle=1

; Denote water feature: 5.25-6.25
oplot, [5.25, 5.25], $
  [max(flux7_1/av_data7+1.7)+0.1, max(flux7_1/av_data7+1.7)+0.2], $
  linestyle=0, thick=1.5
oplot, [6.25, 6.25], $
  [max(flux7_1/av_data7+1.7)+0.1, max(flux7_1/av_data7+1.7)+0.2], $
  thick=1.5
oplot, [5.25, 6.25], $
  [max(flux7_1/av_data7+1.7)+0.2, max(flux7_1/av_data7+1.7)+0.2], $
  thick=1.5
xyouts, 5.6, max(flux7_1/av_data7+1.7)+0.25, 'H!D2!NO', charthick=1.5

; Denote water feature: 6.5-7.0
oplot, [6.5, 6.5], $
  [max(flux7_1/av_data7+1.7)+0.1, max(flux7_1/av_data7+1.7)+0.2], $
  linestyle=0, thick=1.5
oplot, [7.0, 7.0], $
  [max(flux7_1/av_data7+1.7)+0.1, max(flux7_1/av_data7+1.7)+0.2], $
  thick=1.5
oplot, [6.5, 7.0], $
  [max(flux7_1/av_data7+1.7)+0.2, max(flux7_1/av_data7+1.7)+0.2], $
  thick=1.5
xyouts, 6.63, max(flux7_1/av_data7+1.7)+0.25, 'H!D2!NO', charthick=1.5

; Denote methane feature: 7.0-8.7
oplot, [7.0, 7.0], $
  [max(flux7_1/av_data7+1.7)-0.05, max(flux7_1/av_data7+1.7)+0.05], $
  linestyle=0, thick=1.5
oplot, [8.7, 8.7], $
  [max(flux7_1/av_data7+1.7)-0.05, max(flux7_1/av_data7+1.7)+0.05], $
  thick=1.5
oplot, [7.0, 8.7], $
  [max(flux7_1/av_data7+1.7)+0.05, max(flux7_1/av_data7+1.7)+0.05], $
  thick=1.5
xyouts, 7.75, max(flux7_1/av_data7+1.7)+0.1, 'CH!D4!N', charthick=1.5

device, /close


end
