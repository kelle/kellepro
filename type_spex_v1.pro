; NAME:
;       TYPE_SPEX
;
; PURPOSE:
;       This procedure uses flags set in the input file to create postscript
;       figures of the spectra plotted between spectral standards.
;
; CALLING SEQUENCE:
;
;       TYPE_SPEX, Input_file [, Type, /FILE]
;
; INPUTS:
;       Input_file:  Two column ascii file that contains the file names of
;                    FITS spectra and a type flag.
;                               OR
;                    One FITS file. Type flat must also be given.

; OPTIONAL INPUT:
;       Type: Integer type flag if one object given as input instead
;             of file with list
;
; KEYWORD PARAMETERS:
;       FILE:   Set this keyword to use the file name listed in the Input_file
;               as the object name rather than the OBJECT found in the FITS header.
;
; OUTPUTS:
;       Creates 2-page postscript figures for each object listed in the Input_file. 
;
; PROCEDURE:
;       Normalizes all spectra to peak of J band, 1.27-1.32 microns.
;
; ROUTINES CALLED:
;       kellepro: avg_flux.pro
;       Requires library of standard spectra for comparison
;
; EXAMPLE:
;	Create postscript comparison plots for a list of spectra
;	listed in type.lis. Use the file basename for the labels
;	instead of the OBJECT keyword in header.
;
;               TYPE_SPEX, 'type.lis', /FILE
;
;       Create one postscript comparison plot for one late-L dwarf
;       spectrum called nir_spec.fits. Use the OBJECT name in the
;       header to label spectrum.
;
;                TYPE_SPEX, 'nir_spec.fits', 4
;      
; MODIFICATION HISTORY:
;       Written by:                      Kelle Cruz, February 2004.
;       Added telescope and date headings      K. Cruz, April 2005
;       Added spectral feature labels          K. Cruz, April 2005
;       Changed comparison standards             K. Cruz, Feb 2008
;       Modified for single spectrum input       K. Cruz, Feb 2008
;-

pro type_spex, input_file, type_one, file=file_set

if N_params() lt 1 then begin
     print,'Syntax - type, input_file ,[type,/file]'
     print, ''
     print,'Input file can be list or single fits file'
     print,'If single fits file given, type flag must also be given.'
     print,'The input file consists of two columns, the FITS file name and the'
     print,'number giving the rough initial spectral type according to the'
     print,'following key:'
     print, ''
     print,'-1 not sure'
     print,'0 M7-L1 very early L'
     print,'1 L1-L4 early L'
     print,'2 L2-L5 mid L'
     print,'3 L3-L6'
     print,'4 L5-L8 late L'
     print,'5 L7-T0 very late L'
     print,'6 L8-T4 T'
     return
endif

;------------
; READ IN COMPARISON SPECTRA
;------------

nir_path='/scr3/kelle/nir_spectra/2M_all/fits/'
std_path='/scr3/kelle/nir_spectra/standards/'

M7=readfits(std_path+'VB8_M7.fits',/silent)
M8=readfits(std_path+'VB10_M8.fits',/silent)
L0=readfits(std_path+'2M0345_L0.fits',/silent)

L1=readfits(std_path+'2M1439_L1.fits',/silent)

;L2=readfits(std_path+'2M1726.fits')
;L2=readfits(std_path+'U10764_L2.fits',/silent)
L2=readfits(nir_path+'spex_prism_kelu-1_060411.fits',/silent)
l2_label='Kelu-1 (L2)'

;L3=readfits(std_path+'2M0036_L3.fits',/silent)
L3=readfits(nir_path+'u11291_050323.fits',/silent)
l3_label='2M 1506+13 (L3)'

L4=readfits(nir_path+'U12101_2158-1550_davy.fits',/silent)
l4_label='2M 2158-1500 (L4)'

;L5=readfits(std_path+'2M1507.fits')
;L5=readfits(std_path+'U10742_L5.fits',/silent)
L5=readfits(nir_path+'spex_prism_sdss0835+19_chiu06.fits',/silent)
l5_label='SDSS 0835+19 (L5)'

;L6=readfits(std_path+'2M0103.fits')
L6=readfits(nir_path+'U10880.fits',/silent)
l6_label='2M 1010-04 (L6)'

;L7=readfits(std_path+'D0205_L7.fits',/silent)
L7=readfits(nir_path+'2M0103.fits',/silent)
l7_label='2M 0103+19 (L7)'

L8=readfits(std_path+'2M1632_L8.fits',/silent)
l8_label='2M 1632+19 (L8)'

L9=readfits(nir_path+'spex_prism_0255-4700_adam.fits',/silent)
l9_label='D 0255-4700 (L9)'

T0=readfits(std_path+'2M0423_T0.fits',/silent)
T4=readfits(std_path+'2M2254_T4.fits',/silent)
C1=readfits(std_path+'U11273.fits',/silent)
C2=readfits(std_path+'CGCS322.fits',/silent)
Cd=readfits(std_path+'LP225-12.fits',/silent)

;------------
; READ INPUT FILE or SPECTRUM
;------------
IF file_test(input_file) eq 0 then begin
    message,/con,'KREADSPEC: ERROR - Unable to locate file ' + input_file
    return
ENDIF

ext=(strsplit(input_file,'.',/extract))[1]

if ext eq 'fits' then begin
    file=input_file
    types=type_one
endif else begin 
    READCOL, input_file, file, types, FORMAT='A,I'
endelse

n_files=n_elements(file)

;------------
; LOOP Over INPUT SPECTRA
;------------

FOR i=0,n_files-1 DO BEGIN

    type=types[i]
    file_name=strsplit(file[i],'.fits',/extract,/regex)

;-1 is no idea what it is
;0 M7-L1 very early-L
;1 L1-L4 early-L
;2 L2-L5 mid-L
;3 L3-L6
;4 L5-L8 late-L
;5 L7-T0 T
;6 L8-T4
;-2 carbon

    object_data=readfits(file[i], h,/silent)

    flux=object_data[*,1]
    w=object_data[*,0]

    ;extract object name, telescope, and obs date from header
    object_name=strtrim(sxpar(h,'OBJECT'),2)
    telescope=sxpar(h,'TELESCOP')
    date_obs=sxpar(h,'DATE_OBS')

    if keyword_set(file_set) then obj_name = file_name[0] else $
      obj_name=object_name
    if strmatch(obj_name,'spex_sxd_*') eq 1 then obj_name = strmid(obj_name,9,strlen(obj_name)-9)
    if strmatch(obj_name,'spex_prism_*') eq 1 then obj_name = strmid(obj_name,11,strlen(obj_name)-11)
    print, obj_name

;------------
; 
;------------

    IF type eq -2 then begin
        w1=C1(*,0)
        f1=C1(*,1)
        label1='U11273 (C*)'
        
        w2=Cd(*,0)
        f2=Cd(*,1)
        label2='LP 225-12 (C dwarf)'
        
        w3=C2(*,0)
        f3=C2(*,1)
        label3='CGCS 322 (C*)'
    ENDIF

    IF type eq -1 then begin
        w1=M7(*,0)
        f1=M7(*,1)
        label1='VB 8 (M7)'
        
        w2=L3(*,0)
        f2=L3(*,1)
        label2='2M 0036 (L3)'
        
        w3=T4(*,0)
        f3=T4(*,1)
        label3='2M 2254 (T4)'
    ENDIF

    IF type eq 0 then begin
        w1=M7(*,0)
        f1=M7(*,1)
        label1='VB 8 (M7)'
        
        w2=M8(*,0)
        f2=M8(*,1)
        label2='VB 10 (M8)'
        
        w3=L0(*,0)
        f3=L0(*,1)
        label3='2M 0345 (L0)'

        w4=L1(*,0)
        f4=L1(*,1)
        label4='2M 1439 (L1)'
    ENDIF

    IF type eq 1 then begin
        w1=L1(*,0)
        f1=L1(*,1)
        label1='2M 1439 (L1)'

        w2=L2(*,0)
        f2=L2(*,1)
        label2=l2_label

        w3=L3(*,0)
        f3=L3(*,1)
        label3=l3_label

        w4=L4(*,0)
        f4=L4(*,1)
        label4=l4_label
    ENDIF

    IF type eq 2 then begin
        w1=L2(*,0)
        f1=L2(*,1)
        label1=l2_label

        w2=L3(*,0)
        f2=L3(*,1)
        label2=L3_label

        w3=L4(*,0)
        f3=L4(*,1)
        label3=l4_label

        w4=L5(*,0)
        f4=L5(*,1)
        label4=l5_label
    ENDIF
    IF type eq 3 then begin       
        w1=L3(*,0)
        f1=L3(*,1)
        label1=l3_label
        
        w2=L4(*,0)
        f2=L4(*,1)
        label2=l4_label
        
        w3=L5(*,0)
        f3=L5(*,1)
        label3=l5_label

        w4=L6(*,0)
        f4=L6(*,1)
        label4=l6_label
    ENDIF

    IF type eq 4 then begin
        w1=L5(*,0)
        f1=L5(*,1)
        label1=l5_label
        
        w2=L6(*,0)
        f2=L6(*,1)
        label2=l6_label
        
        w3=L7(*,0)
        f3=L7(*,1)
        label3=l7_label

        w4=L8(*,0)
        f4=L8(*,1)
        label4=l8_label
    ENDIF

    IF type eq 5 then begin
        w1=L7(*,0)
        f1=L7(*,1)
        label1=l7_label
        
        w2=L8(*,0)
        f2=L8(*,1)
        label2=l8_label

        w3=L9(*,0)
        f3=L9(*,1)
        label3=l9_label

        w4=T0(*,0)
        f4=T0(*,1)
        label4='2M 0423 (T0)'
    ENDIF

    IF type eq 6 then begin
        w1=L8(*,0)
        f1=L8(*,1)
        label1=l8_label
        
        w2=L9(*,0)
        f2=L9(*,1)
        label2=l9_label

        w3=T0(*,0)
        f3=T0(*,1)
        label3='2M 0423 (T0)'

        w4=T4(*,0)
        f4=T4(*,1)
        label4='2M 2254 (T4)'
    ENDIF

;---------
; NORMALIZE
;---------
;normalize at top of J band for dwarfs and at H band for carbons.
IF type eq -2 THEN BEGIN
    startw = 1.70 & endw = 1.75
ENDIF ELSE BEGIN
    startw = 1.27 & endw = 1.32
ENDELSE

num = avgflux(startw,endw,w,flux)
nf=flux/num

num = avgflux(startw,endw,w1,f1)
nf1=f1/num

num = avgflux(startw,endw,w2,f2)
nf2=f2/num

num = avgflux(startw,endw,w3,f3)
nf3=f3/num

num = avgflux(startw,endw,w4,f4)
nf4=f4/num
;-----------

 tk1=0 & tk2=0
 if type eq -1 then begin
   tk1 = 0.5
   tk2 = 0.9
 endif
 if type eq 0 then begin
   tk1 = 0.25
   tk2 = -0.25
 endif
 if type eq 1 then begin
   tk1 = 0.15 
   tk2 = -0.20
 endif 
 if type eq 3 then begin
   tk1 = 0.10 
   tk2 = 0.0
 endif
 if type eq 4 then begin
   tk1 = 0.1
   tk2 = 0.15
 endif

set_plot, 'ps'
!p.font=0

device, filename=file_name[0]+'.ps', encapsulated=0, /helvetica,/landscape, $
        xsize=10.5, ysize=8.0, /inches, xoffset=0.15, yoffset=10.7

plot, w2,nf2, xr=[0.7,2.5],yr=[0,3.5],xstyle=1, ystyle=1,$
      xtitle='Wavelength (!4l!3m)', ytitle='Normalized Flux + Constant'
xyouts, 0.75,0.8, label2

oplot, w,nf+1
xyouts, 1.1,1.2, obj_name,charsize=1.5

oplot, w1, nf1+2
xyouts, 0.75,2.8, label1

XYOUTS, 0.85,0.92, telescope,/NORMAL
XYOUTS, 0.85,0.89, date_obs, /NORMAL

;K
oplot, [0.7665,0.7665],[1.05,1.15]+tk1, linestyle=1
oplot, [0.7699,0.7699],[1.05,1.15]+tk1, linestyle=1
xyouts, 0.76,1.17+tk1,'K'

;TiO
oplot, [0.8432,0.86],[1.15,1.15]+tk1;, linestyle=1
oplot, [0.8432,0.8432],[1.10,1.15]+tk1
xyouts, 0.82,1.18+tk1,'TiO'

;CrH
oplot, [0.8611,0.88],[1.25,1.25]+tk1;, linestyle=1
oplot, [0.861,0.861],[1.20,1.25]+tk1
;oplot, [0.99,1.085],[2.0,2.0]+tk1, linestyle=1
xyouts, 0.855,1.28+tk1,'CrH'

;FeH
oplot, [0.8692,0.89],[1.4,1.4]+tk1;, linestyle=1
oplot, [0.8692,0.8692],[1.35,1.4]+tk1
;oplot, [0.99,1.085],[2.0,2.0]+tk1, linestyle=1
xyouts, 0.865,1.43+tk1,'FeH'


;H20
oplot, [0.91,0.95],[1.6,1.6]+tk1;, linestyle=1
xyouts, 0.915, 1.65+tk1,'H2O'

;FeH
oplot, [0.986,0.99],[2.0,2.0]+tk1;, linestyle=1
oplot, [0.986,0.986],[1.95,2.0]+tk1
oplot, [0.99,1.085],[2.0,2.0]+tk1, linestyle=1
xyouts, 0.985,2.05+tk1,'FeH'

;VO
oplot, [1.05,1.08],[1.75,1.75]+tk1;, linestyle=1
xyouts, 1.05,1.80+tk1,'VO'

;Na
oplot, [1.138,1.138],[1.7,1.85]+tk1, linestyle=1
oplot, [1.141,1.141],[1.7,1.85]+tk1, linestyle=1
xyouts, 1.13,1.9+tk1,'Na'

;K
oplot, [1.169,1.169],[1.7,1.85]+tk1, linestyle=1
oplot, [1.178,1.178],[1.7,1.85]+tk1, linestyle=1
xyouts, 1.17,1.87+tk1,'K'

;FeH
oplot, [1.19,1.205],[2.0,2.0]+tk1;, linestyle=1
oplot, [1.19,1.19],[1.95,2.0]+tk1
xyouts, 1.185,2.05+tk1,'FeH'

;FeH
oplot, [1.24,1.27],[2.15,2.15]+tk1;, linestyle=1
oplot, [1.24,1.24],[2.10,2.15]+tk1
oplot, [1.27,1.33],[2.15,2.15]+tk1, linestyle=1
xyouts, 1.23,2.20+tk1,'FeH'

;K
oplot, [1.243,1.243],[1.85,2.0]+tk1, linestyle=1
oplot, [1.252,1.252],[1.85,2.0]+tk1, linestyle=1
xyouts, 1.243,2.03+tk1,'K'

;PaB


;H2O
oplot, [1.3,1.51],[2.1,2.1]+tk1;, linestyle=1
xyouts, 1.38, 2.15+tk1,'H2O'

;K
oplot, [1.517,1.517],[1.75,1.9]+tk1, linestyle=1
xyouts, 1.51,1.95+tk1,'K'

;FeH
oplot, [1.59,1.75],[2.05,2.05]+tk2, linestyle=1
xyouts, 1.65,2.1+tk2,'FeH'

;H2O
oplot, [1.75,2.05],[1.85,1.85]+tk2;, linestyle=1
xyouts, 1.85, 1.9+tk2,'H2O'

;Ca
oplot, [1.93,1.99],[1.7,1.7]+tk2, linestyle=1
xyouts, 1.94, 1.75+tk2,'Ca'

;Na
oplot, [2.206,2.206],[1.7,1.8]+tk2, linestyle=1
oplot, [2.2089,2.2089],[1.7,1.8]+tk2, linestyle=1
xyouts, 2.19, 1.83+tk2,'Na'

;CO
oplot, [2.29,2.35],[1.65,1.65]+tk2;, linestyle=1
xyouts, 2.3, 1.70+tk2,'CO'

;H2O
oplot, [2.3,3.2],[1.8,1.8]+tk2;, linestyle=1
xyouts, 2.3, 1.85+tk2,'H2O'


if type ge 5 then begin

 ;Ch4
 oplot, [1.1,1.24],[1.3,1.3];, linestyle=1
 xyouts, 1.14,1.35,'CH4'

 ;Ch4
 oplot, [1.6,1.8],[1.3,1.3];, linestyle=1
 xyouts, 1.67,1.35,'CH4'

 ;Ch4
 oplot, [2.15,2.5],[1.2,1.2];, linestyle=1
 xyouts, 2.3, 1.25,'CH4'

ENDIF

plot, w4,nf4, xr=[0.7,2.5],yr=[0,3.5], xstyle=1, ystyle=1,$
      xtitle='Wavelength (!4l!3m)', ytitle='Normalized Flux + Constant'
xyouts, 0.75,0.8, label4

oplot, w,nf+1
xyouts, 1.1,1.2, obj_name, charsize=1.5

oplot, w3, nf3+2
xyouts, 0.75,2.8, label3

XYOUTS, 0.85,0.92, telescope,/NORMAL
XYOUTS, 0.85,0.89, date_obs, /NORMAL

;K
oplot, [0.7665,0.7665],[1.05,1.15]+tk1, linestyle=1
oplot, [0.7699,0.7699],[1.05,1.15]+tk1, linestyle=1
xyouts, 0.76,1.17+tk1,'K'

;TiO
oplot, [0.8432,0.86],[1.15,1.15]+tk1;, linestyle=1
oplot, [0.8432,0.8432],[1.10,1.15]+tk1
xyouts, 0.82,1.18+tk1,'TiO'

;CrH
oplot, [0.8611,0.88],[1.25,1.25]+tk1;, linestyle=1
oplot, [0.861,0.861],[1.20,1.25]+tk1
;oplot, [0.99,1.085],[2.0,2.0]+tk1, linestyle=1
xyouts, 0.855,1.28+tk1,'CrH'

;FeH
oplot, [0.8692,0.89],[1.4,1.4]+tk1;, linestyle=1
oplot, [0.8692,0.8692],[1.35,1.4]+tk1
;oplot, [0.99,1.085],[2.0,2.0]+tk1, linestyle=1
xyouts, 0.865,1.43+tk1,'FeH'

;H20
oplot, [0.91,0.95],[1.6,1.6]+tk1;, linestyle=1
xyouts, 0.915, 1.65+tk1,'H2O'

;FeH
oplot, [0.986,0.99],[2.0,2.0]+tk1;, linestyle=1
oplot, [0.986,0.986],[1.95,2.0]+tk1
oplot, [0.99,1.085],[2.0,2.0]+tk1, linestyle=1
xyouts, 0.985,2.05+tk1,'FeH'

;VO
oplot, [1.05,1.08],[1.75,1.75]+tk1;, linestyle=1
xyouts, 1.05,1.80+tk1,'VO'

;Na
oplot, [1.138,1.138],[1.7,1.85]+tk1, linestyle=1
oplot, [1.141,1.141],[1.7,1.85]+tk1, linestyle=1
xyouts, 1.13,1.9+tk1,'Na'

;K
oplot, [1.169,1.169],[1.7,1.85]+tk1, linestyle=1
oplot, [1.178,1.178],[1.7,1.85]+tk1, linestyle=1
xyouts, 1.17,1.87+tk1,'K'

;FeH
oplot, [1.19,1.205],[2.0,2.0]+tk1;, linestyle=1
oplot, [1.19,1.19],[1.95,2.0]+tk1
xyouts, 1.185,2.05+tk1,'FeH'

;FeH
oplot, [1.24,1.27],[2.15,2.15]+tk1;, linestyle=1
oplot, [1.24,1.24],[2.10,2.15]+tk1
oplot, [1.27,1.33],[2.15,2.15]+tk1, linestyle=1
xyouts, 1.23,2.20+tk1,'FeH'

;K
oplot, [1.243,1.243],[1.85,2.0]+tk1, linestyle=1
oplot, [1.252,1.252],[1.85,2.0]+tk1, linestyle=1
xyouts, 1.243,2.03+tk1,'K'

;PaB


;H2O
oplot, [1.3,1.51],[2.1,2.1]+tk1;, linestyle=1
xyouts, 1.38, 2.15+tk1,'H2O'

;K
oplot, [1.517,1.517],[1.75,1.9]+tk1, linestyle=1
xyouts, 1.51,1.95+tk1,'K'

;FeH
oplot, [1.59,1.75],[2.05,2.05]+tk2, linestyle=1
xyouts, 1.65,2.1+tk2,'FeH'

;H2O
oplot, [1.75,2.05],[1.85,1.85]+tk2;, linestyle=1
xyouts, 1.85, 1.9+tk2,'H2O'

;Ca
oplot, [1.93,1.99],[1.7,1.7]+tk2, linestyle=1
xyouts, 1.94, 1.75+tk2,'Ca'

;Na
oplot, [2.206,2.206],[1.7,1.8]+tk2, linestyle=1
oplot, [2.2089,2.2089],[1.7,1.8]+tk2, linestyle=1
xyouts, 2.19, 1.83+tk2,'Na'

;CO
oplot, [2.29,2.35],[1.65,1.65]+tk2;, linestyle=1
xyouts, 2.3, 1.70+tk2,'CO'

;H2O
oplot, [2.3,3.2],[1.8,1.8]+tk2;, linestyle=1
xyouts, 2.3, 1.85+tk2,'H2O'

if type ge 4 then begin

 ;Ch4
 oplot, [1.1,1.24],[1.3,1.3];, linestyle=1
 xyouts, 1.14,1.35,'CH4'

 ;Ch4
 oplot, [1.6,1.8],[1.3,1.3];, linestyle=1
 xyouts, 1.67,1.35,'CH4'

 ;Ch4
 oplot, [2.15,2.5],[1.2,1.2];, linestyle=1
 xyouts, 2.3, 1.25,'CH4'

ENDIF

device, /close

ENDFOR

set_plot, 'x'


END
