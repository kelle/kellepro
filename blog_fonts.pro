;+
; NAME:
;	TEMPLATE_FIG
;
; PURPOSE:
;	Illustrate good figure making techniques
;
; CALLING SEQUENCE:
;	
;	TEMPLATE, [/PS, /EPS]
;
;	
; KEYWORD PARAMETERS:
;	PS: Create postscript figure. 
;		Output file called 'template.ps'
;
;   EPS: Create encapsulated postscript figure. 
;		Output file called 'template.eps'
;
; OUTPUTS:
;	Plots to screen if no keywords are given.
;	Creates template.ps or template.eps depending on keyword  used.
;
; ROUTINES CALLED:
;	kellepro:colors_kc.pro
;	kellepro:symbols_kc.pro
;	kellepro:symbols_ps_kc.pro
;
; EXAMPLE:
;
;	Plot to screen
;
;		TEMPLATE_FIG
;
;	Create postscript output
;
;		TEMPLATE_FIG, /ps
;
;
; MODIFICATION HISTORY:
; 	Written by:	Kelle Cruz, November 2007
;
;-

PRO blog_fonts
;, ps=ps,eps=eps,scale=scale

;---------------
;  MODIFY THESE
;---------------

outname='blog_fonts.eps'
root='/scr2/kelle/idl/kellepro/'
;---------------

;---------------
;  SET UP PLOT
;---------------

xsize=9 ; 1 column wide
;xsize=18.6267 ; 2 column wide 44 pica
aspect_ratio=1.5
ysize=xsize/aspect_ratio ;rectangle

;@colors_kc

    !p.font=0                   ;use postscript fonts
    set_plot, 'ps'

;   @symbols_ps_kc ;load string symbols and greek letters for Postscript
    device, filename=root+outname, encapsulated=1;,/helvetica,/isolatin1
    device, xsize=xsize, ysize=ysize;, scale=scale

!x.thick=1
!y.thick=1

a=findgen(100)/10.
b=sin(a)

;---------------
;  PLOT DATA
;---------------

plot, a,b,xtitle='Wavelength', ytitle='Flux', /nodata, $
      charsize=cs,xr=[-0.2,10.2],yrange=[-1.1,1.1],xstyle=1,ystyle=1
oplot, a,b, thick=3  ;colors defined in color_kc.pro

;plot, a,b,xtitle='Wavelength' + ' ('+micron +')',ytitle=Psi+'(x)',/nodata, $
;      charsize=cs,xr=[-0.2,10.2],yrange=[-1.1,1.1],xstyle=1,ystyle=1
;oplot, a,b, color=red,thick=3  ;colors defined in color_kc.pro


;symbol names defined in symbols_kc.pro and symbols_ps_kc.pro
;test_string=Phi+Psi+Omega+'!C'+alpha+beta+delta+lambda+nu+pi+'!C'+$
;            leq+geq+times+propto+approx+degr+pm+oplus

;xyouts, 0.5,-0.5, test_string, color=blue,charsize=cs

 device,/close
 set_plot,'x'
 !p.font=-1 ;go back to default (Vector Hershey fonts)

END

