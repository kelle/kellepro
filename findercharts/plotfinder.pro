PRO plotfinder,a,b,c,d,obj,c1a=c1,c2a=c2,c3a=c3,c4a=c4,c5a=c5,c6a=c6,c7a=c7,c8a=c8

;x0=0.0
x0=0.09
x1=0.41
x1mid=(x0+x1)/2
x2=0.59
x3=0.91
x2mid=(x2+x3)/2

y0=0.02
y1=0.45
y1mid=(y0+y1)/2
y2=0.52
y3=0.95
y2mid=(y2+y3)/2

;SET UP CIRCLES

crad=0.015

IF n_elements(c1) EQ 0 THEN c1 = x1mid
IF n_elements(c2) EQ 0 THEN c2 = y2mid

IF n_elements(c3) EQ 0 THEN c3 = x2mid
IF n_elements(c4) EQ 0 THEN c4 = y2mid

IF n_elements(c5) EQ 0 THEN c5 = x1mid
IF n_elements(c6) EQ 0 THEN c6 = y1mid

IF n_elements(c7) EQ 0 THEN c7 = x2mid
IF n_elements(c8) EQ 0 THEN c8 = y1mid


tvimage,bytscl(max(a)-reverse(a)),position=[x0,y2,x1,y3],/keep_aspect
xyouts, x1mid, y3+0.01, 'XDSS R', /normal,align=0.5

plots, circle(c1,c2,crad),/normal

IF n_elements(c1) NE 0 THEN plots,circle(c1,c2,0.015),/normal

tvimage,bytscl(max(b)-reverse(b)),position=[x2,y2,x3,y3],/keep_aspect
xyouts, x2mid,y3+0.01, 'XDSS I', /normal,align=0.5
plots, circle(c3,c4,crad),/normal


tvimage,bytscl(max(c)-reverse(c)),position=[x0,y0,x1,y1],/keep_aspect
xyouts, x1mid, y1+0.01, '2MASS J',/normal,align=0.5
plots, circle(c5,c6,crad),/normal

tvimage,bytscl(max(d)-reverse(d)),position=[x2,y0,x3,y1],/keep_aspect
xyouts, x2mid,y1+0.01, '2MASS K', /normal,align=0.5
plots, circle(c7,c8,crad),/normal

;PRINT OBJECT NAME
xyouts, 0.5,0.98,obj,/normal,align=0.5,charsize=2

;DRAW SCALE BAR
plots, [0.5,0.5],[y2,y3],/normal
xyouts,0.5+0.01,y2mid,'120"',/normal

;DRAW COMPASS
plots,[0.03,0.03],[y2mid,y2mid+0.05],/normal
plots,[0.03,0.07],[y2mid,y2mid],/normal
xyouts, 0.03,y2mid+0.06,'N',charsize=1.5,align=0.5
xyouts, 0.08,y2mid,'E',charsize=1.5,align=0.5

END
