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

PRO TEMPLATE_FIG, ps=ps,eps=eps,scale=scale

;---------------
;  MODIFY THESE
;---------------

outname='template_fig'
root='/scr2/kelle/idl/kellepro/'

;---------------

if ~keyword_set(scale) then scale=1

if keyword_set(ps) then begin
    ext='.ps'
endif else begin
    if keyword_set(eps) then begin
        ps = 1
        ext='.eps'
    endif
endelse

;---------------
;  SET UP PLOT
;---------------

xsize=8.89 ; 1 column wide
;xsize=18.6267 ; 2 column wide 44 pica
ysize=xsize/1.5 ;rectangle
aspect_ratio=ysize/xsize

xsize_window=500 *scale;in pixels

@colors_kc


IF KEYWORD_SET(ps) THEN BEGIN
	;setup for postscript or eps output
    !p.font=0                   ;use postscript fonts
    set_plot, 'ps'
    @symbols_ps_kc ;load string symbols and greek letters for Postscript
    device, filename=root+outname+ext, encapsulated=keyword_set(eps), /helvetica,/isolatin1, landscape=0, color=1
    device, xsize=xsize, ysize=ysize, scale=scale,$
            xoffset=1, yoffset=1
    cs=1                        ;charcter size
ENDIF ELSE BEGIN
	;set up for display to screen
    set_plot,'x'
    device, Decomposed=0 ;make colors work for 24-bit display
    black=white ;exchange colors to work with default black backround
    @symbols_kc ;load string symbols and greek letters for Hershey
    cs=2 ;charcter size
    window, 2, xsize=xsize_window, ysize=xsize_window*aspect_ratio ;,xpos=0,ypos=0
ENDELSE

!x.thick=3
!y.thick=3

a=findgen(100)/10.
b=sin(a)

;---------------
;  PLOT DATA
;---------------

plot, a,b,xtitle='Wavelength' + ' ('+micron +')',ytitle=Psi+'(x)',/nodata, $
      charsize=cs,xr=[-0.2,10.2],yrange=[-1.1,1.1],xstyle=1,ystyle=1
oplot, a,b, color=red,thick=3  ;colors defined in color_kc.pro

;make a triangle with 3 sharp corners
usersym,$
[0.866025, -4.37114e-08,    -0.866025,     0.866026,-4.37114e-08 ],$
[-0.500000,      1.00000,    -0.500000,    -0.500000,1]
oplot,[5,7],[0.1,0.5],psym=8,thick=5

;symbol names defined in symbols_kc.pro and symbols_ps_kc.pro
test_string=Phi+Psi+Omega+'!C'+alpha+beta+delta+lambda+nu+pi+'!C'+$
            leq+geq+times+propto+approx+degr+pm+oplus

xyouts, 0.5,-0.5, test_string, color=blue,charsize=cs
;colors defined in color_kc.pro

IF KEYWORD_SET(ps) THEN BEGIN
 device,/close
 set_plot,'x'
 !p.font=-1 ;go back to default (Vector Hershey fonts)
 Message,'WROTE: ' + root+outname+ext,/info

ENDIF

loadct,0 ;go back to default greyscale color table

END

