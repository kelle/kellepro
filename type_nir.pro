;+
; NAME:
;	TYPE_NIR
;
; PURPOSE:
;	Derive Spectral types based on NIR data
;
; CALLING SEQUENCE:
;	
;	TYPE_NIR, input,file, [/PS, /EPS]
;
;	
; KEYWORD PARAMETERS:
;	PS: Create postscript figures. One per object. 
;		Output file called '[object_name].ps'
;
;   EPS: Create encapsulated postscript figure. 
;		Output file called '[object_name].eps'
;
; OUTPUTS:
;	Plots to screen if no keywords are given.
;	Creates template.ps or template.eps depending on keyword  used.
;
; ROUTINES CALLED:
;	kellepro:colors_kc.pro
;	kellepro:symbols_kc.pro
;	kellepro:symbols_ps_kc.pro
;	pfeatures
;	pbandhead
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
;
;		 ; Written by: Kelle Cruz, Jan 2009
;;
; TO DO
; - add SpType Labels
; - make standards more prominent. 
;-

PRO jlabels, tk1, cs2
	
;TiO
    ; oplot, [0.8432,0.86],[1.05,1.05]+tk1 ;, linestyle=1
    ;     oplot, [0.8432,0.8432],[1.00,1.05]+tk1
    ;     xyouts, 0.84,1.07+tk1,'TiO', charsize=cs2,align=0
    
	pbandhead, 0.8432,2.33,'TiO',charsize=cs2
	pbandhead, 0.8432,0.15,'TiO',/bot,charsize=cs2

;CrH
	pbandhead,0.8611,2.42,'CrH',charsize=cs2
;    oplot, [0.8611,0.88],[1.25,1.25]+tk1 ;, linestyle=1
;    oplot, [0.861,0.861],[1.20,1.25]+tk1
;    xyouts, 0.855,1.27+tk1,'CrH', charsize=cs2
	
;FeH
    ; oplot, [0.8692,0.89],[1.4,1.4]+tk1 ;, linestyle=1
    ;   oplot, [0.8692,0.8692],[1.35,1.4]+tk1
    ;   xyouts, 0.865,1.42+tk1,'FeH',align=0.5, charsize=cs2
  pbandhead,0.8692,2.51,'FeH',charsize=cs2

;H2O + TiO
	pfeatures,[0.92,0.93],2.67,'TiO',/left,align=0.5,charsize=0.5,/noarm
	pfeatures,[0.93,0.95],2.6,'H!D2!NO?',align=0.5,charsize=0.5,/noarm

;Ti
	pfeatures,[0.955,0.98],2.75,'Ti',charsize=cs2,/noarm,align=0.5
;	pfeatures,[0.955,0.98],0.4,'Ti',/bot,charsize=cs2,/noarm,align=0.5

;FeH
    ; oplot, [0.986,0.99],[1.9,1.9]+tk1-0.08 ;, linestyle=1
    ;     oplot, [0.986,0.986],[1.85,1.9]+tk1-0.08
    ;     oplot, [0.99,1.085],[1.9,1.9]+tk1-0.08, linestyle=1
    ;     xyouts, 0.985,1.92+tk1-0.08,'FeH',align=0.5, charsize=cs2
    pbandhead,0.986,2.7,'FeH',charsize=cs2
	pbandhead,0.986,0.33,'FeH',/bot,charsize=cs2
	
;VO
     oplot, [1.04,1.08],[1.6,1.6]+tk1-0.1 ;, linestyle=1
     xyouts, 1.06,1.62+tk1-0.1,'VO',align=0.5, charsize=cs2

;?
	oplot, [1.10,1.12],[1.57,1.57]+tk1 ;, linestyle=1
	xyouts, 1.11,1.59+tk1,'?',align=0.5, charsize=cs2

;Na
    oplot, [1.138,1.138],[1.7,1.8]+tk1-0.15, linestyle=1
    oplot, [1.141,1.141],[1.7,1.8]+tk1-0.15, linestyle=1
    xyouts, 1.14,1.82+tk1-0.15,'Na !7I!X',align=0.8, charsize=cs2

	pfeatures,[1.138,1.141],0.6,'Na !7I!X',/bot,charsize=cs2, align=0.5,linestyle=1,length=4
;K
    oplot, [1.169,1.169],[1.75,1.85]+tk1-0.15, linestyle=1
    oplot, [1.178,1.178],[1.75,1.85]+tk1-0.15, linestyle=1
    ;oplot, [1.169,1.178],[ltop,ltop], linestyle=0
    xyouts, 1.17,1.87+tk1-0.15,'K !7I!X',align=0.5, charsize=cs2
;K
	    oplot, [1.243,1.243],[1.9,2.0]+tk1-0.3, linestyle=1
	    oplot, [1.252,1.252],[1.9,2.0]+tk1-0.3, linestyle=1
	    ;oplot, [1.244,1.253],[ltop,ltop], linestyle=0 ;K I     
	    xyouts, 1.243,2.02+tk1-0.3,'K !7I!X',align=0.5, charsize=cs2

	pfeatures,[1.169,1.178],0.66,'K !7I!X',/bot,charsize=cs2, align=0.5,linestyle=1,length=4
	pfeatures,[1.243,1.252],0.66,'K !7I!X',/bot,charsize=cs2, align=0.5,linestyle=1,length=4

;FeH
    oplot, [1.19,1.205],[2.0,2.0]+tk1-0.18 ;, linestyle=1
    oplot, [1.19,1.19],[1.95,2.0]+tk1-0.18
    xyouts, 1.185,2.02+tk1-0.18,'FeH', charsize=cs2

;FeH
    oplot, [1.24,1.27],[2.15,2.15]+tk1-0.33 ;, linestyle=1
    oplot, [1.24,1.24],[2.10,2.15]+tk1-0.33
    oplot, [1.27,1.33],[2.15,2.15]+tk1-0.33, linestyle=1
    xyouts, 1.33,2.17+tk1-0.33,'FeH', charsize=cs2, align=1
END

PRO TYPE_NIR, input_file, ps=ps,eps=eps, late=late, young=young

if N_params() lt 1 then begin
     print,"Syntax -  TYPE_NIR, 'Input_file_indices.sav', [/ps,/eps]"
     print,"expects output from measure.pro, /nir"
     return
endif


root1='/scr1/Analysis/nir_indices/'
root2='~/Analysis/nir_indices/'
if File_test(root1) then root=root1
if file_test(root2) then root=root2

nir_path1='/scr1/nir_spectra/2M_all/'
nir_path2='~/Data/nir_spectra/'
if File_test(nir_path1) then nir_path=nir_path1
if file_test(nir_path2) then nir_path=nir_path2
;----------
; NIR SPEX PRISM Spectral Standards
;-----------
nir_path_stds = nir_path + 'standards/'

L0 =KREADSPEC(nir_path_stds+'2M0345_L0.fits',hl0,/norm)
L0g=kreadspec(nir_path_stds+'2M0141-46_L0g.fits',hl0g,/norm)
L1 =KREADSPEC(nir_path_stds+'2M2130_L1_kc.fits',hl1,/norm)
L2 =KREADSPEC(nir_path_stds+'Kelu-1_L2.fits',hl2,/norm)
L3 =KREADSPEC(nir_path_stds+'2M1506+13_L3.fits',hl3,/norm) ;1506+13
L4 =KREADSPEC(nir_path_stds+'2M2158-15_L4.fits',hl4,/norm) ;2158-15
L5 =KREADSPEC(nir_path_stds+'SDSS0835+19_L5.fits',hl5,/norm);0835
L5g=KREADSPEC(nir_path_stds+'2M0501-00_L5g.fits',hl1g,/norm)
L6 =KREADSPEC(nir_path_stds+'2M1010-04_L6.fits',hl6,/norm) ;1010-04
L7 =KREADSPEC(nir_path_stds+'2M0103+19_L7_davy.fits',hl7,/norm) ;0103
L8 =KREADSPEC(nir_path_stds+'2M1632_L8.fits',hl8,/norm)
L9 =KREADSPEC(nir_path_stds+'2M0255-47_L9.fits',hl9,/norm)
;young=KREADSPEC(nir_path+'spex_prism_0141-4633_040905.fits',h0141,/norm)

Lstds={L0:L0,L0g:L0g,L1:L1,L2:L2,L3:L3,L4:L4,L5:L5,L5g:L5g,L6:L6,L7:L7,L8:L8,L9:L9}
@colors_kc ;load lots of colors
Lstds_color=[red,dkred,purple,blue,dkgreen,orange,red,dkred,purple,blue,dkgreen,orange]

;-----------

xsize=18.6267/1.45 ; 2 column wide 44 pica
ysize=xsize*1.45
scale=1.4
aspect_ratio=ysize/xsize

xsize_window=500 ;in pixels

if keyword_set(ps) then begin
    ext='.ps'
endif else begin
    if keyword_set(eps) then begin
        ps = 1
        ext='.eps'
    endif else begin
         set_plot,'x'
         device, Decomposed=0   ;make colors work for 24-bit display
         black=white ;exchange colors to work with default black backround
         @symbols_kc
    endelse
endelse

;-----------

RESTORE, root+input_file
;input_file is output .sav file from measure.pro
;ref,desig,sp_type,spectra_files,indices,indices_sigma,index_name

n_objects=n_elements(ref)

FOR i=0,n_objects-1 DO BEGIN
    H2O_A07=indices[i,where(index_name eq 'H2O_A07')]
    H2OJ=indices[i,where(index_name eq 'H2OJ')]
    H2OH=indices[i,where(index_name eq 'H2OH')]
    ;CH4K=indices[i,where(index_name eq 'CH4K')]
    sH2OJ=indices[i,where(index_name eq 'sH2OJ')]

    ;Allers07
    spt_H2O_A07=(H2O_A07-0.77)/0.04
    ;Burgasser07
    spt_H2OJ=1.949e1 -3.919e1*H2OJ + 1.312e2*H2OJ^2 -2.156e2*H2OJ^3 + 1.038e2*H2OJ^4
    spt_H2OH=2.708e1 - 8.45e1 * H2OH + 2.424e2 * H2OH^2 - 3.381e2 * H2OH^3 + 1.491e2 * H2OH^4
    ;spt_CH4K=1.885e1 - 2.246e1* CH4K + 2.534e1 * CH4K^2 - 4.734 * CH4K^3 - 1.259e1 * CH4K^4
    ;Testi
    spt_sH2OJ=10*((1.54 * sH2OJ) + 0.98)
    
    spt_avg=mean([spt_H2O_A07,10+spt_H2OJ,10+spt_H2OH,spt_sH2OJ],/double)
    spt_stddev=STDDEV([spt_H2O_A07,10+spt_H2OJ,10+spt_H2OH,spt_sH2OJ],/double)

    if ref[i]-10000 lt 10000 then outfile_root='U'+strn(uint(ref[i]))+'_'+strmid(desig[i],0,4)+strmid(desig[i],7,5) else outfile_root='U'+strn(uint(ref[i]))+'_'+strmid(desig[i],0,4)+strmid(desig[i],8,5)
;U1 vs U2 desig length

IF KEYWORD_SET(ps) THEN BEGIN
    !p.font=0 
    set_plot, 'ps'
    device, filename=root+'plots/'+outfile_root+ext, encapsulated=keyword_set(eps), $
            /helvetica,/isolatin1,$
            landscape=0, color=1, xsize=xsize, ysize=ysize, scale=scale,$
            xoffset=0.5, yoffset=0.5
    @symbols_ps_kc
ENDIF ELSE BEGIN
    window, 0, xsize=xsize_window, ysize=xsize_window*aspect_ratio ,xpos=0,ypos=0
ENDELSE

!p.multi=[0,2,0]
!p.thick=1
!x.thick=3
!y.thick=3

o1=2.0 ; top of offset
ltop=o1+1.35
lbot=0.5
micron = string(181B) + 'm'
plot, [0],[0],/nodata, xr=[0.8,1.5],  yr=[0,o1+1.5], $
      xstyle=1, ystyle=1,xmargin=[7,-2],xminor=4,xtickinterval=0.2,$
      xtitle='Wavelength ('+micron+')', ytitle='Normalized Flux + Constant'

; character sizes
cs=STR_SIZE('2M 2213+19',0.20)
cs2=cs/2
;label locations
lo=0.4 ; label offset from data
xt=0.8 ; x location of label

std_thick=1.0
std_ls=0
obj_thick=1.5

spec=KREADSPEC(nir_path+spectra_files[i],/norm)

Lstds_label=tag_names(Lstds)
Lstds_label[1]='L0'+delta
Lstds_label[7]='L5'+delta

oplot,L0[0,*],  L0[1,*]+o1, color=red,linestyle=std_ls,thick=std_thick,psym=10
oplot,spec[0,*], spec[1,*]+o1,thick=obj_thick,psym=10
xyouts, 1.4, o1+0.7,'L0', color=red

if keyword_set(young) then begin
	o1=o1-0.5
	oplot,L0g[0,*],  L0g[1,*]+o1, color=Lstds_color[1],linestyle=std_ls,thick=std_thick,psym=10
	oplot,spec[0,*], spec[1,*]+o1,thick=obj_thick,psym=10
	xyouts, 1.38, o1+0.7,Lstds_label[1], color=Lstds_color[1]
endif

o1=o1-0.5
oplot,L1[0,*],  L1[1,*]+o1, color=purple,linestyle=std_ls,thick=std_thick,psym=10
oplot,spec[0,*], spec[1,*]+o1,thick=obj_thick,psym=10
xyouts, 1.4, o1+0.7,'L1', color=purple

o1=o1-0.5
oplot,L2[0,*],  L2[1,*]+o1, color=blue,linestyle=std_ls,thick=std_thick,psym=10
oplot,spec[0,*], spec[1,*]+o1,thick=obj_thick,psym=10
xyouts, 1.4, o1+0.6,'L2', color=blue

o1=o1-0.5
oplot,L3[0,*],  L3[1,*]+o1, color=dkgreen,linestyle=std_ls,thick=std_thick,psym=10
oplot,spec[0,*], spec[1,*]+o1,thick=obj_thick,psym=10
xyouts, 1.4, o1+0.6,'L3', color=dkgreen

if ~keyword_set(young) then begin
	o1=o1-0.5
	oplot,L4[0,*],  L4[1,*]+o1, color=orange,linestyle=std_ls,thick=std_thick,psym=10
	oplot,spec[0,*], spec[1,*]+o1,thick=obj_thick,psym=10
	xyouts, 1.4, o1+0.6,'L4', color=orange
ENDIF

xyouts,1.07,0.30, 'Prev SpT:    '+strn(sp_type[i],format='(f0.1)') ,charsize=cs2
xyouts,1.07,0.25, 'Index SpT: '+strn(spt_avg,format='(f0.2)')+ pm + strn(spt_stddev,format='(f0.2)'),$
	charsize=cs2
xyouts, 1.09,0.20,$
        'sH2OJ: '+ strn(spt_sH2OJ,format='(f5.1)')+'!C'+ $
        'H2OJ:   '+ strn(10+spt_H2OJ,format='(f5.1)')+'!C'+ $
        'A07:     '+ strn(spt_H2O_A07,format='(f5.1)')+'!C'+ $
        'H2OH:  '+ strn(10+spt_H2OH,format='(f5.1)'),charsize=cs2/1.2

;--------
;NIR/MICRONS FEATURE LABELS
;--------

tk1=1.4
tk2=0.85

jlabels, tk1,cs2

;-------------------
;if ~keyword_set(ps) then window, 1, xsize=xsize_window, ysize=xsize_window*aspect_ratio,xpos=500,ypos=0

o1=2.0 ; top of offset
plot, [0],[0],/nodata, xr=[0.8,1.5],  yr=[0,o1+1.5], $
      xstyle=1, ystyle=1,xmargin=[5,0],xminor=4,xtickinterval=0.2,$
	ytickformat='(A1)',$
      xtitle='Wavelength ('+micron+')';, ytitle='Normalized Flux + Constant'

xyouts, 1.47,ltop,outfile_root,align=1,charsize=cs

oplot,L5[0,*],  L5[1,*]+o1, color=red,linestyle=std_ls,thick=std_thick,psym=10
oplot,spec[0,*], spec[1,*]+o1,thick=obj_thick,psym=10
xyouts, 1.4, o1+0.6,'L5', color=red

if keyword_set(young) then begin
	o1=o1-0.5
	oplot,L5g[0,*],  L5g[1,*]+o1, color=dkred,linestyle=std_ls,thick=std_thick,psym=10
	oplot,spec[0,*], spec[1,*]+o1,thick=obj_thick,psym=10
	xyouts, 1.4, o1+0.6,'L5'+delta, color=dkred
endif

o1=o1-0.5
oplot,L6[0,*],  L6[1,*]+o1, color=purple,linestyle=std_ls,thick=std_thick,psym=10
oplot,spec[0,*], spec[1,*]+o1,thick=obj_thick,psym=10
xyouts, 1.4, o1+0.6,'L6', color=purple

o1=o1-0.5
oplot,L7[0,*],  L7[1,*]+o1, color=blue,linestyle=std_ls,thick=std_thick,psym=10
oplot,spec[0,*], spec[1,*]+o1,thick=obj_thick,psym=10
xyouts, 1.4, o1+0.7,'L7', color=blue

o1=o1-0.5
oplot,L8[0,*],  L8[1,*]+o1, color=dkgreen,linestyle=std_ls,thick=std_thick,psym=10
oplot,spec[0,*], spec[1,*]+o1,thick=obj_thick,psym=10
xyouts, 1.4, o1+0.7,'L8', color=dkgreen

o1=o1-0.5
oplot,L9[0,*],  L9[1,*]+o1, color=orange,linestyle=std_ls,thick=std_thick,psym=10
oplot,spec[0,*], spec[1,*]+o1,thick=obj_thick,psym=10
xyouts, 1.4, o1+0.6,'L9', color=orange

;-------------------
;--------
;NIR/MICRONS FEATURE LABELS
;--------

tk1=1.4
tk2=0.85

jlabels,tk1,cs2

;-------------------

if ~keyword_set(ps) then window, 1, xsize=xsize_window, ysize=xsize_window*aspect_ratio,xpos=500,ypos=0
!p.multi=0

o1=2.0 ; top of offset
;ltop=o1+1.1
;lbot=0.5

plot, [0],[0],/nodata, xr=[0.8,2.5],  yr=[0,o1+1.5], $
      xstyle=1, ystyle=1,xmargin=[7,0],$
      xtitle='Wavelength ('+micron+')', ytitle='Normalized Flux + Constant'

xyouts, 2.4,ltop,outfile_root,align=1,charsize=cs

j=round(spt_avg)-10
if j ge 7 then j = 9
if j le 3 then j = 2

xloc=2.39

oplot,(Lstds.(j-2))[0,*],  (Lstds.(j-2))[1,*]+o1, color=Lstds_color[j-2],linestyle=std_ls,thick=std_thick,psym=10
oplot,spec[0,*], spec[1,*]+o1,thick=obj_thick,psym=10
xyouts, xloc, o1+0.4,Lstds_label[j-2], color=Lstds_color[j-2]

o1=o1-0.5
oplot,(Lstds.(j-1))[0,*],  (Lstds.(j-1))[1,*]+o1, color=Lstds_color[j-1],linestyle=std_ls,thick=std_thick,psym=10
oplot,spec[0,*], spec[1,*]+o1,thick=obj_thick,psym=10
xyouts, xloc, o1+0.4,Lstds_label[j-1], color=Lstds_color[j-1]

o1=o1-0.5
oplot,(Lstds.(j))[0,*],  (Lstds.(j))[1,*]+o1, color=Lstds_color[j],linestyle=std_ls,thick=std_thick,psym=10
oplot,spec[0,*], spec[1,*]+o1,thick=obj_thick,psym=10
xyouts, xloc, o1+0.4,Lstds_label[j], color=Lstds_color[j]

o1=o1-0.5
oplot,(Lstds.(j+1))[0,*],  (Lstds.(j+1))[1,*]+o1, color=Lstds_color[j+1],linestyle=std_ls,thick=std_thick,psym=10
oplot,spec[0,*], spec[1,*]+o1,thick=obj_thick,psym=10
xyouts, xloc, o1+0.4,Lstds_label[j+1], color=Lstds_color[j+1]

o1=o1-0.5
oplot,(Lstds.(j+2))[0,*],  (Lstds.(j+2))[1,*]+o1, color=Lstds_color[j+2],linestyle=std_ls,thick=std_thick,psym=10
oplot,spec[0,*], spec[1,*]+o1,thick=obj_thick,psym=10
xyouts, xloc, o1+0.4,Lstds_label[j+2], color=Lstds_color[j+2]

xyouts,1.37,0.30, 'Prev SpT:    '+strn(sp_type[i],format='(f0.1)') ,charsize=cs2
xyouts,1.37,0.25, 'Index SpT: '+strn(spt_avg,format='(f0.2)')+ pm + strn(spt_stddev,format='(f0.2)'),$
	charsize=cs2
xyouts, 1.39,0.20,$
        'sH2OJ: '+ strn(spt_sH2OJ,format='(f5.1)')+'!C'+ $
        'H2OJ:   '+ strn(10+spt_H2OJ,format='(f5.1)')+'!C'+ $
        'A07:     '+ strn(spt_H2O_A07,format='(f5.1)')+'!C'+ $
        'H2OH:  '+ strn(10+spt_H2OH,format='(f5.1)'),charsize=cs2/1.2

;--------
;NIR/MICRONS FEATURE LABELS
;--------

tk1=1.4
tk2=0.85

jlabels,tk1,cs2

;PaB

;H2O
;    oplot, [1.3,1.51],[2.1,2.1]+tk1 ;, linestyle=1
;    xyouts, 1.38, 2.15+tk1,'H2O', charsize=cs2

; ;K
;     oplot, [1.517,1.517],[1.35,1.5]+tk1, thick=obj_thick
;     xyouts, 1.51,1.55+tk1,'K !7I!X',align=0.5, charsize=cs2

;FeH
    oplot, [1.59,1.75],[2.05,2.05]+tk2, linestyle=1
    xyouts, 1.65,2.07+tk2,'FeH', charsize=cs2

;H2O
;    oplot, [1.75,2.05],[1.85,1.85]+tk2 ;, linestyle=1
;    xyouts, 1.85, 1.9+tk2,'H2O', charsize=cs2

; ;Ca
;     oplot, [1.93,1.99],[1.7,1.7]+tk2, linestyle=1
;     xyouts, 1.94, 1.9+tk2,'Ca', charsize=cs2

;Na
    oplot, [2.206,2.206],[1.7,1.8]+tk2, linestyle=1
    oplot, [2.2089,2.2089],[1.7,1.8]+tk2, linestyle=1
    xyouts, 2.207, 1.82+tk2,'Na !7I!X',align=0.5, charsize=cs2
;CO
    oplot, [2.29,2.35],[1.75,1.75]+tk2 , linestyle=1
    oplot, [2.29,2.29],[1.70,1.75]+tk2 , linestyle=0
    xyouts, 2.3, 1.77+tk2,'CO', charsize=cs2
;-------------------

if spt_avg gt 15 then begin
;Ch4
    oplot, [1.1,1.24],[1.45,1.45]+tk1, linestyle=1
    xyouts, 1.17,1.47+tk1,'CH4', charsize=cs2,align=0.5

 ;Ch4  
;    oplot, [1.6,1.8],[1.75,1.75]+tk1  ;, linestyle=1
;    xyouts, 1.67,1.8+tk1,'CH4', charsize=cs2

 ;Ch4
    oplot, [2.15,2.5],[1.4,1.4]+tk1, linestyle=1
    xyouts, 2.3, 1.45+tk1,'CH4', charsize=cs2
endif

;-------------------

IF KEYWORD_SET(ps) THEN BEGIN
	device,/close
	set_plot,'x'
	!p.font=-1 ;go back to default (Vector Hershey fonts)
	Message, 'WROTE: '+root+'plots/'+outfile_root+ext,/info
ENDIF ELSE BEGIN
    Print,'Press any key to continue.'
    tmp=get_kbrd()
ENDELSE

ENDFOR                           ;i over objects

!p.multi=0
loadct,0 ;go back to default greyscale color table

END

