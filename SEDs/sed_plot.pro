PRO SED_PLOT, obj, nir_scale, norm=norm, fnu=fnu, ps = ps, greykey=greykey,str1=str1,str2=str2,offset=offset,noopt=noopt

;if N_params() lt 1 then begin
;     print,'Syntax -  MAKE_SED, object [,nir_scale, /FNU, /PS, /NORM, OFFSET=offset, /NOOPT]'
;     print,'object is number, /FNU for Jy, /PS to make PS output'
;     return
;endif

;CALLS
; kellepro:sed_make


obj=strn(obj)

sed_make, obj, nir_scale, norm=norm, fnu=fnu, w_opt,f_opt, w_nir, f_nir, w_ir, f_ir, yunits

;plot lambda * f_lambda
;ergs/s/cm^2
IF ~keyword_set(fnu) then begin
    f_opt=w_opt*1e3*f_opt
    f_nir=w_nir*1e3*f_nir
    f_ir=w_ir*1e3*f_ir
ENDIF

;IF KEYWORD_SET(ps) THEN BEGIN
    loadct, 0, ncolors=192, bottom=0
    loadct, 12, ncolors=16, bottom=192 ;16level
    loadct, 6, ncolors = 16, bottom=208 ;prism
    loadct, 38, ncolors=32, bottom=224 ;rainbow18
;ENDIF

    black = 0
    dkgrey= 49                  ;0.25
    grey = 96                   ;0.5
    ltgrey = 144                ;0.75
    white=191

;16level
    green=194                   ; bright green
    cyan= 197
    blue= 198
    purple = 199
    magenta = 200
    pink = 201
    red = 204

;prism
    dkred=211
    dkblue= 221

;rainbow18
    dkpurple = 225
    ltblue=232
    dkgreen= 234
    ltgreen=242
    yellow = 244
    dkyellow=246
    ltorange=248
    orange = 250
    dpurple2=252
    dkred2=254

;IF ~KEYWORD_SET(ps) THEN greykey=1

IF ~KEYWORD_SET(ps) THEN begin
    device, Decomposed=0
    black=white
ENDIF

IF keyword_set(greykey) THEN BEGIN
    lcolor1=grey
    lcolor2=grey
    tk=1
ENDIF ELSE BEGIN
    lcolor1=black
    lcolor2=red
    tk=2
ENDELSE

print, lcolor1, lcolor2

IF N_elements(offset) EQ 0 THEN BEGIN
   offset= 0
   offset2 = -1 
ENDIF ELSE BEGIN
   offset2 = 1
ENDELSE

a=where(w_opt LE 1.0)
b=where(w_nir GE 0.7 AND w_nir LE 2.5)


;stop
ymax=max(f_nir[b],/nan)
;ymax=max(alog10(f_nir[b]))

;ymin=min(f_opt,/nan)


IF KEYWORD_SET(ps) THEN BEGIN
 !p.font=0
 set_plot, 'ps'
 device, filename=obj+'.ps', encapsulated=0, /helvetica,/landscape, $
         xsize=10.5, ysize=8.0, /inches, xoffset=0.15, yoffset=10.7,/color
ENDIF

IF offset2 eq -1 THEN BEGIN
   plot, w_nir[b], f_nir[b], yr=[0,ymax],xr=[0.5,16],/nodata, xstyle=1,/xlog,$
      ytitle=yunits
    ;plot, w_nir[b], f_nir[b], yr=[ymax/100,ymax],xr=[0.5,16],/nodata, xstyle=1,/xlog,/ylog,$
    ;  ytitle=yunits
   XYOUTS,0.6 ,ymax, obj
   IF KEYWORD_SET(str) THEN XYOUTS, 8,ymax, str
ENDIF ELSE BEGIN
   cs=0.75
   IF KEYWORD_SET(str1) THEN XYOUTS, 0.55,offset+0.62, str1, charsize=cs,color=blue
   IF KEYWORD_SET(str2) THEN XYOUTS, 0.55,offset+0.75, str2,charsize=cs,color=blue
   ;XYOUTS, 0.6, offset+0.75, obj
ENDELSE


IF ~KEYWORD_SET(noopt) THEN oplot, w_opt[a], f_opt[a]+offset, color=dkgreen
oplot, w_nir[b], f_nir[b]+offset, color=lcolor1,thick=tk;,linestyle=lstyle
oplot, w_ir,f_ir+offset, color=lcolor2 ;thick=1,linestyle=lstyle

IF KEYWORD_SET(ps) THEN BEGIN
 device,/close
 set_plot,'x'
 PRINT, 'wrote spectrum to file '+ obj+'.ps'
 loadct,0
ENDIF

END
