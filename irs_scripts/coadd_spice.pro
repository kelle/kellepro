FUNCTION coadd_spice, object, order, header, interact=interact

;work_dir = '/data/hillary/3/kelle/ir_spectra/irs/S12/spice_output'
work_dir='/data/hillary/3/kelle/ir_spectra/irs/S12/test'

object=strn(object)

CASE order OF
1: BEGIN
   o='sl1'
   xr=[7,16]
   END
2: BEGIN
   o='sl2'
   xr=[5,8]
   END
ENDCASE

spect_files = FILE_SEARCH(work_dir, '*'+object+'*'+o+'*spect.tbl', COUNT=num_spect)

FOR i = 0, num_spect-1 DO BEGIN

    IF ARG_PRESENT(header) AND i EQ 0 THEN $
      a=READ_SPICE(spect_files[i],header) $
    ELSE $
      a=READ_SPICE(spect_files[i])

    print, spect_files[i]

    w=a[0,*]
    flux=a[1,*]

    IF KEYWORD_SET(interact) THEN BEGIN
        plot, a[0,*], a[1,*], xr=xr, xstyle=1
        xyouts, 0.8,0.8, o+','+strn(i),/normal 
        print, ''
        Print,'Press any key to continue'
        print, ' '
        anykey=get_kbrd(1)
    ENDIF
    
    IF i EQ 0 THEN flux_tot = flux ELSE flux_tot = flux_tot + flux 

ENDFOR 

flux_avg = flux_tot / num_spect

spec=a
spec[1,*]=flux_avg

plot, spec[0,*],spec[1,*]

RETURN, spec

END
