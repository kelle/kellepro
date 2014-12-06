FUNCTION mag2fluxjy, bands, mags

;band 1 =J
;band2 = H
;band3 = K_S

;RETURNS FLUX IN mJy
; All based on Appendix of Spitzer Observer's Manual
; Checked with online calculator:
; http://ssc.spitzer.caltech.edu/tools/magtojy/

;written April, 2007

IF N_params() lt 1 then begin
    print,'Syntax -  J_jy=MAG2FLUXJY([bands], [magnitudes])'
    print,'    Returns flux in mJy'
    print,'    bands:'
    print,'    J=1'
    print,'    H=2'
    print,'    K_S=3'
     return,0
ENDIF

;zero points from Appendix A of Spitzer Cookbook and
;http://www.ipac.caltech.edu/2mass/releases/allsky/doc/sec6_4a.html
;CASE band OF
;    1: zpt_jy=1594
;    2: zpt_jy=1024
;    3: zpt_jy=666.7
;ENDCASE

zpt_jy=[1594,1024,666.7]

zpts=zpt_jy[bands-1]

flux=zpts * 10^(-mags/2.5) ; in Jy
;see Appendix of Spitzer Observer's Manual

;print, flux*1e3

return,flux*1e3 ; return flux in mJy

END
