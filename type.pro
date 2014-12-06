pro type, Input_file,file=file

; NAME:
;       TYPE
;
; PURPOSE:
;       This procedure uses flags set in the input file to create postscript
;       figures of the spectra plotted between spectral standards.
;
; CALLING SEQUENCE:
;
;       TYPE, Input_file
;
; INPUTS:
;       Input_file:  Two column ascii file that contains the file names of
;                    ascii spectra and a type flag.
;
; OUTPUTS:
;       Creates 2-page postscript figures for each object listed in the Input_file. 
;
; PROCEDURE:
;       Normalizes L dwarfs at 8250 A, Ms at 8100 A, and Carbon stars at 7500 A.
;       The file name listed in the Input_file is used as the object name.
;
; ROUTINES CALLED:
;       avg_flux.pro
;       Also uses standards.dat and stan_giants.dat
;
; TO DO:
;       Add young comparison (0141, giants?)
;
;
; MODIFICATION HISTORY:
;       Written by:     Kelle Cruz, July 2002
;-

if N_params() lt 1 then begin
     print,"Syntax - type, 'file'"
     print, ''
     print,'The input file consists of two columns, the file name and the'
     print,'number giving the rough initial spectral type according to the'
     print,'following key:'
     print, ''
;     print,' young'
     print,'-4 WD'
     print,'-3 not typed'
     print,'-2 carbon'
     print,'-10 early giant'
     print,'-11 mid giant'
     print,'-12 late giant'
     print,'0 M1.5 - M5.5'
     print,'1 M4.5 - M7'
     print,'2 M6 - M8'
     print,'3 M7 - M9'
     print,'4 M8 - L1'
     print,'5 L0 - L3'
     print,'6 L2 - L5'
     print,'7 L4 - L7'
     print,'8 L5 - L8'
     return
endif

;root='/data/hillary/1/kelle/idl/kellepro/'
root='/Users/kelle/Library/IDL/kellepro/'
EG_f=root + 'EG274.dat'

restore, EG_f
restore, root + 'standards.dat'
restore, root + 'stan_giants.dat'

one_line = dblarr(2)
file_line = ''

openr, unit, Input_file,/get_lun

WHILE NOT EOF(unit) DO BEGIN
    ;print, ''
    READF, unit, file_line

    info=strsplit(file_line,/extract)

    data_file=info(0)
    early=fix(info(1))
    ;print, data_file,early
;print, file, early

    IF early ne -3 THEN BEGIN
;-4 plotted next to WD
;-3 is not typed
;-2 is carbon
;-10 is early giant
;-11 is mid giant
;-12 is late giant
;0 M1.5 - M5.5
;1 M4.5 - M7
;2 M6 - M8
;3 M7 - M9
;4 M8 - L1
;5 L0 - L3
;6 L2 - L5
;7 L4 - L7
;8 L5 - L8


;obj_pos=strpos(data_file,'.txt')
;object=strmid(data_file,0,obj_pos))[0]

        IF STRMATCH(data_file, '*.fit*', /FOLD_CASE) eq 1 then extn='fits' ELSE ext = 'txt'
                                ;extn=(strsplit(data_file,'.',/extract))[1]

        file_name=file_basename(data_file,'.'+extn)

        ;file_name=(strsplit(data_file,'.',/extract))[0]


        IF extn EQ 'txt' THEN BEGIN

            ;object=(strsplit(data_file,'.txt',/extract,/regex))[0]
            object=file_name

            data=READSPEC_TXT(data_file)
            telescope = ''
            date_obs= ''
            flux_unit='?'

        ENDIF                   ;if file type .txt

        IF extn EQ 'fits' THEN BEGIN
           
            data=KREADSPEC(data_file,h,/silent)
  
                                ;file=SXPAR(h,'FILE')
            pix_scale=SXPAR(h,'CD1_1',scalecount)

            object = STRCOMPRESS(strtrim(sxpar(h,'OBJECT'),2),/remove_all)

            IF keyword_set(file) then object_file = file_name else $
              object_file = object

            IF object eq '0' then object = file_name
   
            telescope = sxpar(h,'TELESCOP')
            date_obs = sxpar(h,'DATE-OBS',count=datecount,/silent)
            
            ;print, date_obs, datecount
                                ;IF datecount EQ 0 THEN date_obs = sxpar(h,'DATE',count=datecount)
                                ;IF datecount EQ 0 THEN date_obs = sxpar(h,'DATE-OBS',count=datecount)
                                ;print, date_obs
            flux_unit=SXPAR(h,'BUNIT', count=fluxcount)
            if fluxcount eq 0 then flux_unit='?'
            ;print, flux_unit

        ENDIF                   ;if file type .fits

        print, object

        ;stop
        flux=data[1,*]
        w=data[0,*]
        
        ;IF n_elements(pix_scale) EQ 0 THEN pix_scale=w[1]-w[0]
        
        IF max(w) le 9000 then xtop=max(w) ELSE xtop=10000.

        xr=[6000,xtop]
        if xtop gt 9000 THEN es=1 ELSE es=0

;NORMALIZE

        IF early eq 4 THEN BEGIN
            startw = 8080. & endw = 8155.
            ;a = where(w ge startw-pix_scale/2 AND w le endw+pix_scale/2)
            ;num = avgflux(startw,endw, w, flux)
            ;nf1=flux/num
            nf1=NORM_SPEC(w, flux, startw, endw)
            
            startw = 8240. & endw = 8260.
            ;a = where(w ge startw-pix_scale/2 AND w le endw+pix_scale/2)
            ;num = avgflux(startw,endw, w ,flux)
            ;nf2=flux/num
            nf2=NORM_SPEC(w, flux, startw, endw)
        ENDIF
        
;carbon
;change to 7500

        IF early eq -2 THEN BEGIN & startw = 7490. & endw = 7510. 
        ENDIF

;Ms (dwarfs and giants)
        IF (early lt 4 AND early ge 0) OR  (early le -10) THEN BEGIN & startw = 8080. & endw = 8155.  
        ENDIF

;Ls
        IF early gt 4 THEN BEGIN & startw = 8240. & endw = 8260. 
        ENDIF

;wd
        IF early eq -4 THEN BEGIN & startw = 7000. & endw = 7200. 
        ENDIF

        ;a = where(w ge startw-pix_scale/2 AND w le endw+pix_scale/2)
        ;num = avgflux(startw, endw, w, flux)
        ;nf=flux/num
        nf=NORM_SPEC(w, flux, startw, endw)

        !p.font=0
        set_plot, 'ps'
        device, filename=object_file+'.ps', encapsulated=0, /helvetica,/isolatin1,/landscape, $
          xsize=10.5, ysize=8.0,/inches, xoffset=0.3, yoffset=10.7
    
        angstrom=STRING(197B)

        !x.title='Wavelength '+'('+angstrom+')'
        !y.title='Flux ('+flux_unit+')'

;CARBON
        IF early eq -2 then begin

            plot, w, nf
            feat,/lowg

            xyouts, 6100, 1.0, object, charsize=2

;plot,c6_w,c6_fn,xr=xr, yr=[0,4]
;oplot, w, nf+1.5

;xyouts,6100, 1.5, 'C6 HD 92055', charsize=1.5
;xyouts, 6100, 3.5, object, charsize=2

        ENDIF

;WHITE DWAARF
        IF early eq -4 THEN BEGIN

            plot, w, nf,yr=[0,4],xr=xr,xstyle=1

            oplot, eg274_w,eg274_fn+2
   
;Halpha
            oplot, [6563,6563],[1.0,0.8], linestyle=1 

            xyouts, 6100, 3.5, 'EG 274', charsize=1.5
            xyouts, 6100, 2, object, charsize=2

        ENDIF

;---------------
; GIANTS
;---------------
;K5 III HD 133774
;M2 HD 120052 
;M4 HD 80431
;M0 III HD 95578


        IF early eq -10 OR early eq -1 THEN BEGIN
            w1=m0_w & f1=m0_fn
            l1='M0 III HD 95578'
            w2=k5_w & f2=k5_fn
            l2='K5 III HD 133774'
            w3=m2_w & f3=m2_fn
            l3= 'M2 III HD 120052'
            w4=m0_w & f4=m0_fn
            l4= 'M0 III HD 95578' 
        ENDIF
  
    IF early eq -11 THEN BEGIN
        w1=m4_w & f1=m4_fn
        l1='M4 III HD 80431'
        w2=m2_w & f2=m2_fn
        l2='M2 III HD 120052'
        w3=m7_w & f3=m7_fn
        l3='M7 III VY Peg'
        w4=m4_w & f4=m4_fn
        l4='M4 III HD 80431'
    ENDIF

    IF early eq -12 THEN BEGIN
        w1=m8_w & f1=m8_fn
        l1='M8 III BR 0954-0947'
        w2=m7_w & f2=m7_fn
        l2='M7 III VY Peg'
        w3=m9_w & f3=m9_fn
        l3= 'M9 III BR 1219-1336'
        w4= m8_w & f4=m8_fn
        l4='M8 III BR 0954-0947'
    ENDIF

    IF early LE -10 THEN BEGIN

        plot, w1,f1,xr=xr, yr=[0,3], xstyle=1
        oplot, w, nf+0.85
        oplot, w2,f2+1.75
    
        feat,/lowg

        xyouts,6100, 0.8, l1, charsize=1.5
        xyouts, 6100, 1.9, object, charsize=2
        xyouts, 6100, 2.7, l2, charsize=1.5

        plot, w3,f3,xr=xr, yr=[0,3], xstyle=1
        oplot, w, nf+0.85
        oplot, w4,f4+1.75

        feat,/lowg

        xyouts,6100, 0.5, l3, charsize=1.5
        xyouts, 6100, 1.8, object, charsize=2
        xyouts, 6100, 2.6, l4, charsize=1.5

    ENDIF

;----------
;EARLY Ms
;---------

    IF early eq 0 then begin

        plot, m25_w,m25_fn, xr=xr, yr=[0,3], xstyle=1
        oplot, w, nf+0.85
        oplot, m15_w,m15_fn+1.75
    
        feat,/lowg

        xyouts,6100, 0.6, 'M2.5 Gl 250 B', charsize=1.5
        xyouts, 6100, 1.6, object, charsize=2
        xyouts, 6100, 2.6, 'M1.5 Gl 205', charsize=1.5

        XYOUTS, 0.75,0.92, telescope,/NORMAL
        XYOUTS, 0.75,0.89, date_obs, /NORMAL

    ENDIF

    IF early eq 1 OR early eq 0 then begin

        plot, m55_w,m55_flux_n, xr=xr, yr=[0,3], xstyle=1
        oplot, w, nf+0.85
        oplot, m45_w,m45_flux_n+1.75

        feat, /lowg

        xyouts,6100, 0.6, 'M5.5 Gl 65', charsize=1.5
        xyouts, 6100, 1.6, object, charsize=2
        xyouts, 6100, 2.6, 'M4.5 Gl 83.1', charsize=1.5

        XYOUTS, 0.75,0.92, telescope,/NORMAL
        XYOUTS, 0.75,0.89, date_obs, /NORMAL

    ENDIF

    IF early eq 1 OR early eq 2 THEN BEGIN

        plot, m7_lambda,m7_flux_n, xr=xr, yr=[0,3], xstyle=1
        oplot, w, nf+0.75
        oplot, m6_lambda,m6_flux_n+1.75
        feat
        xyouts,6100, 0.3, 'M7 VB 8', charsize=1.5
        xyouts, 6100, 1.3, object, charsize=2
        xyouts, 6100, 2.2, 'M6 LHS 1326', charsize=1.5

        XYOUTS, 0.75,0.92, telescope,/NORMAL
        XYOUTS, 0.75,0.89, date_obs, /NORMAL

    ENDIF

    IF early eq 2 OR early eq 3 then begin

        if es eq 0 then ym=3 else ym=4

        plot, m8_lambda,m8_flux_n, xr=xr, yr=[0,ym], xstyle=1
        oplot, w, nf+0.75+0.3*es
        oplot, m7_lambda,m7_flux_n+1.75+0.5*es
        feat
        xyouts,6100, 0.3, 'M8 VB 10', charsize=1.5
        xyouts, 6100, 1.3+0.3*es, object, charsize=2
        xyouts, 6100, 2.2+0.5*es, 'M7 VB8', charsize=1.5

        XYOUTS, 0.75,0.92, telescope,/NORMAL
        XYOUTS, 0.75,0.89, date_obs, /NORMAL

    ENDIF

    IF early eq 3 OR early eq 4 THEN BEGIN
    
        if es eq 0 then ym=3 else ym=5
    
        plot, m9_lambda,m9_flux_n, xr=xr, yr=[0,ym], xstyle=1

        IF early eq 4 then  oplot, w, nf1+0.85+1.1*es else oplot, w, nf+0.85+1.1*es

        oplot, m8_lambda,m8_flux_n+1.75+1.5*es

        feat,1.

        xyouts,6100, 0.4, 'M9 LHS 2065', charsize=1.5
        xyouts, 6100, 1.3+1.1*es, object, charsize=2
        xyouts, 6100, 2.2+1.5*es, 'M8 VB 10', charsize=1.5

        XYOUTS, 0.75,0.92, telescope,/NORMAL
        XYOUTS, 0.75,0.89, date_obs, /NORMAL
    
    ENDIF

    IF early eq 4 OR early eq 5 THEN BEGIN

        if es eq 0 then ym=2.5 else ym=4

        plot, L1_w,L1_fn, xr=xr, yr=[0,ym], xstyle=1
        IF early eq 4 then oplot, w, nf2+0.5+0.5*es else oplot, w, nf+0.5+0.5*es
        oplot, L0_w,L0_fn+1.2+0.5*es

        feat

        xyouts,6100, 0.2, 'L1 2M1439', charsize=1.5
        xyouts, 6100, 0.9+0.5*es, object, charsize=2
        xyouts, 6100, 1.5+0.5*es, 'L0 2M0345', charsize=1.5

        XYOUTS, 0.75,0.92, telescope,/NORMAL
        XYOUTS, 0.75,0.89, date_obs, /NORMAL

    ENDIF

    IF early eq 5 OR early eq 6 THEN BEGIN

        if es eq 0 then ym=2.5 else ym=4.5

        plot, L3_w,L3_fn, xr=xr, yr=[0,ym], xstyle=1
        oplot, w, nf+0.6+0.4*es
        oplot, L2_w,L2_fn+1.3+0.8*es

        feat

        xyouts,6100, 0.2, 'L3 DENIS 1058', charsize=1.5
        xyouts, 6100, 0.9+0.6*es, object, charsize=2
        xyouts, 6100, 1.6+0.8*es, 'L2 Kelu 1', charsize=1.5

        XYOUTS, 0.75,0.92, telescope,/NORMAL
        XYOUTS, 0.75,0.89, date_obs, /NORMAL

    ENDIF

    IF early eq 6 OR early eq 7 THEN BEGIN

        if es eq 0 then ym=4 else ym=6

        plot, L5_D1228_w,L5_D1228_fn+0.1, xr=xr, yr=[0,ym], xstyle=1
        oplot, w, nf+1.1+0.3*es
        oplot, L4_w,L4_fn+2.4+0.3*es
    
        feat

        xyouts,6100, 0.4, 'L5 DENIS 1228', charsize=1.5
        xyouts, 6100, 1.5+0.3*es, object, charsize=2
        xyouts, 6100, 2.8+0.3*es, 'L4 2M1155', charsize=1.5

        XYOUTS, 0.75,0.92, telescope,/NORMAL
        XYOUTS, 0.75,0.89, date_obs, /NORMAL

    ENDIF

    IF early eq 7 THEN BEGIN

        if es eq 0 then ym=5 else ym=7

        plot, L7_D0205_w,L7_D0205_fn+0.25, xr=xr, yr=[0,ym], xstyle=1
        oplot, w, nf+1.7+0.7*es
        oplot, L6_w,L6_fn+3.0+1.3*es
    
        feat

        xyouts,6100, 0.7, 'L7 DENIS 0205', charsize=1.5
        xyouts, 6100, 2.4+0.7*es, object, charsize=2
        xyouts, 6100, 3.4+1.3*es, 'L6 2M0850', charsize=1.5

    ENDIF

    IF early eq 8 THEN BEGIN

        if es eq 0 then ym=5 else ym=8

        plot, L6_w,L6_fn+0.10, xr=xr, yr=[0,ym], xstyle=1
        oplot, w, nf+1.4+0.7*es
        oplot, L5_D1228_w,L5_D1228_fn+3.2+0.7*es

        feat

        xyouts,6100, 0.4, 'L6 2M0850', charsize=1.5
        xyouts, 6100, 2.2+0.7*es, object, charsize=2
        xyouts, 6100, 3.5+0.7*es, 'L5 DENIS 1228', charsize=1.5

        XYOUTS, 0.75,0.92, telescope,/NORMAL
        XYOUTS, 0.75,0.89, date_obs, /NORMAL

        plot, L8_w,L8_fn+0.1, xr=xr, yr=[0,ym], xstyle=1
        oplot, w, nf+1.8+0.3*es
        oplot, L7_D0205_w,L7_D0205_fn+3.6+0.1*es

        feat

        xyouts,6100, 0.75, 'L8 2M1632', charsize=1.5
        xyouts, 6100, 2.75+0.3*es, object, charsize=2
        xyouts, 6100, 4.5+0.1*es, 'L7 DENIS 0205', charsize=1.5
    
    ENDIF

;On second page
    XYOUTS, 0.80,0.92, telescope,/NORMAL
    XYOUTS, 0.80,0.89, date_obs, /NORMAL
    
    device, /close

ENDIF ;early ne -3

ENDWHILE

close, unit
free_lun,unit

set_plot, 'x'

!x.title=0
!y.title=0

;stop
END
