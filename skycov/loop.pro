FUNCTION loop, glon, glat,min1,max1,min2,max2,name


OPENW, lun2, name+'.txt',/GET_LUN

   i=0
   x=dblarr(151)
   y=x
   FOR bin100= 50,200,1 DO begin
      bin=0.001234+bin100/1000.D
      area_reg=area(glon, glat,min1,max1,min2,max2,bin,name,lun2)
      x[i]=bin
      y[i]=area_reg
      i=i+1
      IF bin100 EQ 100 THEN value=area_reg
   ENDFOR

   !p.multi=0
   set_plot,'ps'
   device, file=name+'.ps', /portrait
   plot, x,y,/ynozero,psym=1
   IF name EQ 'm31' THEN BEGIN
      area=2.57
      oplot, [0,0.6],[area,area]
   ENDIF
   IF name EQ 'm33' THEN BEGIN
      area=9.05
      oplot, [0,0.6],[area,area]
   ENDIF
   device,/close
   set_plot,'x'

CLOSE, lun2
FREE_LUn, lun2

RETURN, value

END
