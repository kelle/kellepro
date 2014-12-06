PRO BURROWS_COMPARE, Temp=temp, grav=grav, metal=metal, ps=ps,eps=eps,over=over,offset=offset,color=color,xr=xr,yr=yr

obj='test'

IF N_params() LT 0 THEN BEGIN
   print, 'SYNTAX - MODEL_SED, obj, T, g [metal, nir_scale, /ps, /eps, /over]'
   print, 'T AND g can be values or arrays'
   RETURN
ENDIF

;calls
;READ_BURROWS, NORM_SPEC, sed_make
; colors_kc


IF N_elements(offset) EQ 0 THEN BEGIN
   offset= 0
   offset2 = -1 
ENDIF ELSE BEGIN
   offset2 = 1
ENDELSE

@colors_kc

IF n_elements(color) EQ 0 THEN color=[red,blue,dkgreen, black,purple]

obj=strn(obj)

IF keyword_set(ps) OR keyword_set(eps) THEN BEGIN
  !p.font=0
  set_plot, 'ps'

  IF keyword_set(ps) THEN BEGIN
     extn='.ps'
     device, filename=obj+'_model'+extn, encapsulated=0, /portrait, yoffset=0.5, xoffset=0
  ENDIF ELSE BEGIN
     extn='.eps'
     device, filename=obj+'_model'+extn, encapsulated=1
  ENDELSE

  device, /helvetica,/color,/inches, xsize=8.0,ysize=10.5

ENDIF ELSE BEGIN
    set_plot,'x'
    device, Decomposed=0        ;make colors work for 24-bit display
    black=white ;exchange colors to work with default black backround
    @symbols_kc
ENDELSE

IF ~keyword_set(temp) then temp=findgen(16)*100+700 else t=temp
IF ~keyword_set(grav) then g=[4.5,5,5.5] else g=grav
IF ~keyword_set(metal) then  metal=[0.3,1,3]
;IF n_g EQ 0 THEN g=[4.5,5,5.5]
;IF n_m EQ 0 THEN metal=[0.3,1,3]

n_T=n_elements(T)
n_g=n_elements(g)
n_m = n_elements(metal)
;stop

IF ~KEYWORD_SET(over) AND (n_T GE 2 OR n_g GE 2) THEN !p.multi=[0,1,4]



p=0
FOR i= 0, n_T-1 DO BEGIN

;   n_g=n_elements(g)
;   n_m = n_elements(metal)

   IF T[i] EQ 2100 OR T[i] EQ 2200 THEN BEGIN
      g_str='5.0'
      n_g=1
      metal_s='solar'
      n_m=1
   ENDIF

   FOR j= 0, n_g-1 DO BEGIN
   
      T_str=strn(T[i])

      IF T[i] NE 2100 AND T[i] NE 2200 THEN BEGIN
         CASE g[j] OF
            4.5: g_str='4.5'
            5.0: g_str='5.0'
            5.5: g_str='5.5'
         ENDCASE
      ENDIF

      n_m = n_elements(metal)

      IF T[i] EQ 2000 AND g[j] EQ 5.0 THEN BEGIN
         z=where(metal NE 3)
         metal2 = metal[z]
         n_m = n_elements(metal2)
      ENDIF ELSE IF T[i] EQ 2100 OR T[i] EQ 2200 THEN BEGIN
         metal2 = 1
         n_m=1
      ENDIF ELSE BEGIN
         metal2=metal
      ENDELSE

      n_m = n_elements(metal2)

      FOR k = 0, n_m-1 DO BEGIN
         IF T[i] NE 2100 AND T[i] NE 2200 THEN BEGIN
            CASE metal2[k] OF
               0.3: metal_s='0.3X'
               3: metal_s='3X'
               1: metal_s='solar'
            ENDCASE
         ENDIF

;READ IN MODEL and normalize

model=READ_BURROWS('T'+T_str+'_g'+g_str+'_f100_'+metal_s,/fnu)
model_w=model[0,*]
;Normalize model to K band peak (F_nu)
startw= 0.824 & endw= 0.826
;startw= 2.15 & endw= 2.25
model_f=NORM_SPEC(model_w,model[1,*],startw, endw)

w=where(model_w GT 0.5)

print,  'T'+T_str+'_g'+g_str+'_f100_'+metal_s

ymax=max(model_f)
if ~keyword_set(yr) then yr=[0,ymax]
if p eq 0 then plot,[0],[0],xr=xr,yr=yr,/nodata,/ylog,xstyle=1

IF KEYWORD_SET(over) THEN BEGIN
   ;if no offset, draw axes on first plot
   ;IF offset2 EQ -1 AND p EQ 0 THEN make_sed, obj, /fnu,/norm,/noopt
;stop
   oplot, model_w[w], model_f[w]+offset, color=color[p],thick=2
   XYOUTS, 0.1, 0.75*(p+2)/10, 'T= '+T_str+', g= '+ g_str +', metal= '+ metal_s,color=color[p],/normal,charsize=2
ENDIF ELSE BEGIN
   ;make_sed, obj, /fnu,/norm,/noopt
   oplot, model_w, model_f, color=red,thick=1
   XYOUTS, 6, 0.85+offset, 'T= '+T_str+', g= '+ g_str +', metal= '+ metal_s
ENDELSE

p=p+1
IF p MOD 4 EQ 0 AND n_g*n_t*n_m GT 4 AND ~KEYWORD_SET(ps) AND ~KEYWORD_SET(eps) THEN BEGIN
   print, ''
   Print,'Press any key to continue'
   print, ' '
   anykey=get_kbrd(1)
ENDIF   

   ENDFOR ;end loop over metal
  ENDFOR ;end loop over g
ENDFOR ;end loop over T

;put data on top of models
;IF keyword_set(over) THEN make_sed, obj, /fnu,/norm, offset=offset,/noopt,/grey

IF keyword_set(ps) OR keyword_set(eps) THEN BEGIN
 device,/close
 set_plot,'x'
 PRINT, 'wrote spectrum to file '+ obj+'_model'+extn
ENDIF

!p.multi=0

END
