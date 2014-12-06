PRO feat, shift,lowg=lowg

; FEAT, [shift , /lowg]

IF n_elements(shift) EQ 0 THEN shift=0

;Halpha
xyouts, 6530, 0.8+shift, 'H!9a!X';, charsize=1.2
oplot, [6563,6563],[0.9,1.7]+shift, linestyle=1 
oplot, [6563,6563],[0.0,0.7]+shift, linestyle=1 

;Li
xyouts, 6680, 1.3+shift, 'Li !7I!X';, charsize=1.2
oplot, [6708,6708],[0.0,1.1]+shift, linestyle=1

;K I
xyouts, 7650, 0.8+shift, 'K !7I!X';, charsize=1.2

oplot, [7665,7665],[1.0,1.7]+shift, linestyle =1
oplot, [7699,7699],[1.0,1.7]+shift, linestyle =1

;Rb I
xyouts, 7790, 0.8+shift, 'Rb !7I!X';, charsize=1.2
xyouts, 7930, 0.8+shift, 'Rb !7I!X';, charsize=1.2

oplot, [7800,7800],[1.0,1.7]+shift, linestyle =1
oplot, [7948,7948],[1.0,1.7]+shift, linestyle =1


;Na I
oplot, [8183,8183],[1.4,2.3]+shift, linestyle =1
oplot, [8195,8195],[1.4,2.3]+shift, linestyle =1

oplot, [8183,8183],[0.4,1.2]+shift, linestyle =1
oplot, [8195,8195],[0.4,1.2]+shift, linestyle =1

xyouts, 8145, 1.3+shift, 'Na !7I!X';, charsize=1.2

;Cs I
xyouts, 8520, 0.8+shift, 'Cs !7I!X', align=0.5;, charsize=1.2
xyouts, 8940, 0.8+shift, 'Cs !7I!X', align=0.5;, charsize=1.2

oplot, [8521,8521],[1.0,1.7]+shift, linestyle =1
oplot, [8943,8943],[1.0,1.7]+shift, linestyle =1


;GIANTS and YOUNG

IF keyword_set(lowg) THEN begin

;print, 'lowg'

;VO
oplot, [7334, 7334, 7534,7534],[1.25,1.3,1.3,1.25]-shift
oplot, [7851,7851,7973,7973],[1.25,1.3,1.3,1.25]-shift
xyouts, 7334, 1.35-shift, 'VO'
xyouts, 7851, 1.35-shift, 'VO'


;K I doublet
;oplot, [7665,7665],[1.2,1.4], linestyle=1
;oplot, [7699,7699],[1.25,1.45], linestyle=1

;Ca II triplet
oplot, [8498,8498],[1.0,1.95]+shift, linestyle=1
oplot, [8542,8542],[1.0,1.95]+shift, linestyle=1
oplot, [8662,8662],[1.0,1.95]+shift, linestyle=1
xyouts, 8490, 2.0+shift, 'Ca !7II!X'



ENDIF

END
