PRO UVW_young

;calls gal_uvw

READCOL, '/scr2/kelle/young/goodbox/Young_goodbox.txt',ref,ra,dec,dis_kc,pmra,pmdec,$
comment='#', DELIMITER=',',FORMAT='ADDFDD'

n_objects=n_elements(ref)

u_arr=fltarr(5,11)
v_arr=fltarr(5,11)
w_arr=fltarr(5,11)

u_dis_kc=fltarr(11)
v_dis_kc=fltarr(11)
w_dis_kc=fltarr(11)

;loadct, 12 ; 16 level
;black=0
;green=32
;cyan=80
;blue=96
;purple=112
;pink=128
;red=192

loadct, 13 ;rainbow
black=0
green=158
cyan=110
blue=61
purple=29
orange=225
yellow=207
red=255

vrad=indgen(11)*10.-50. ; 11 rad vel from -50 -- 50 km/s
dis=indgen(5)*20.+10.

!p.font=0
set_plot,'ps'
device, file='/scr2/kelle/young/goodbox/plots.ps',/color,$
        xsize=9, ysize=9*2, xoffset=6, yoffset=8

!p.thick=3

dis_offset=5

FOR j = 0, n_objects-1 do begin
    
    FOR i=0,10  do begin ; loop over vrad
        for k=0,4 do begin ;loop over distance

            gal_uvw, u,v,w, ra=ra[j],dec=dec[j],pmra=pmra[j]*1e3, pmdec=pmdec[j]*1e3, vrad=vrad[i], dis=dis[k]

            u_arr[k,i]=-u
            v_arr[k,i]=v
            w_arr[k,i]=w

        endfor

        gal_uvw, u,v,w, ra=ra[j],dec=dec[j],pmra=pmra[j]*1e3, pmdec=pmdec[j]*1e3, vrad=vrad[i], dis=dis_kc[j]

        u_dis_kc[i]=-u
        v_dis_kc[i]=v
        w_dis_kc[i]=w

    ENDFOR

;if w_arr[0,0] lt w_arr[0,10] then w_min=0 else w_min=10
    w_min=0



;--------------
; BOTTOM PANEL
;-------------
    plotsym, 0, 0.5, /fill

    plot, u_arr,w_arr,psym=8, position=[0.15, 0.15, 0.9, 0.50], ytitle='W', xtitle='U',$
      xr=[-50,50], yr=[-60,40], xstyle=1, ystyle=1

    plots, [u_dis_kc[0],u_dis_kc[10]], [w_dis_kc[0], w_dis_kc[10]], linestyle=2, thick=5
   
    xyouts, u_arr[0,0],w_arr[0,0]+5, strn(fix(vrad[0]))+' km/s', align=0, charsize=0.8
    xyouts, u_arr[0,i/2],w_arr[0,i/2]+5, strn(fix(vrad[i/2]))+' km/s', align=0, charsize=0.8
    xyouts, u_arr[0,i-1],w_arr[0,i-1]+5, strn(fix(vrad[i-1]))+' km/s', align=0, charsize=0.8

    xyouts, u_arr[0,w_min],w_arr[0,w_min]-dis_offset, strn(fix(dis[0]))+' pc', charsize=0.8
    xyouts, u_arr[1,w_min],w_arr[1,w_min]-dis_offset, strn(fix(dis[1]))+' pc', charsize=0.8
    xyouts, u_arr[2,w_min],w_arr[2,w_min]-dis_offset, strn(fix(dis[2]))+' pc', charsize=0.8
    xyouts, u_arr[3,w_min],w_arr[3,w_min]-dis_offset, strn(fix(dis[3]))+' pc', charsize=0.8
    xyouts, u_arr[4,w_min],w_arr[4,w_min]-dis_offset, strn(fix(dis[4]))+' pc', charsize=0.8

    ;xyouts, 0.5,0.96, ref[j],/normal, charsize=2, align=0.5

    xyouts, 0.5,0.93, '!Ma!X= ' +strn(  fix(    sixty(ra[j]/15)  )   ) ,  align=0.5, /normal
    xyouts, 0.5,0.905, '!Md!X= '+strn(fix(sixty(dec[j]))) , align=0.5,/normal

    ;xyouts, 0.1,0.95, 'my d = '+strn(fix(dis_kc[j]))+' pc',/normal, charsize=1.5

;twhya
    IF ra[j]/15. gt 10 AND ra[j]/15 lt 13.5 AND dec[j] lt -20 then begin
        u1=-11
        w1=-5
        plots, [u1-10, u1+10, u1+10, u1-10, u1-10],[w1-10,w1-10,w1+10,w1+10, w1-10], color=red
        xyouts, u1,w1, 'TwHya', color=red
    ENDIF

;tuchor
    IF (ra[j]/15. gt 20 OR ra[j]/15 lt 7.5) AND dec[j] lt -20 THEN BEGIN
        u1=-11
        w1=0
        plots, [u1-10, u1+10, u1+10, u1-10, u1-10],[w1-10,w1-10,w1+10,w1+10, w1-10], color=green
        xyouts, u1,w1,'Tuc/Hor',color=green
    ENDIF

;cha near/eta cha
    IF ra[j]/15. gt 8 AND ra[j]/15 lt 13 AND dec[j] lt -70 THEN BEGIN
        u1=-11.5
        w1=-9
        plots, [u1-10, u1+10, u1+10, u1-10, u1-10],[w1-10,w1-10,w1+10,w1+10, w1-10], color=cyan
        xyouts, u1,w1,'Cha',color=green
    ENDIF

;beta pic
    u1=-11
    w1=-9
    plots, [u1-10, u1+10, u1+10, u1-10, u1-10],[w1-10,w1-10,w1+10,w1+10, w1-10], color=blue
    xyouts, -20,10,'!Mb!X Pic',color=blue, charsize=1, align=0.5
    plots, [-20,u1],[10,w1+10], color=blue

;ab Dor
    u1=-8
    w1=-14
    plots, [u1-10, u1+10, u1+10, u1-10, u1-10],[w1-10,w1-10,w1+10,w1+10, w1-10], color=red
    ;xyouts, u1,w1,'AB Dor',color=red, charsize=0.8, align=0.5
    xyouts, -20,-50,'AB Dor',color=red, charsize=1, align=0.5
    plots, [-20,u1],[-50+7, w1-10], color=red

;--------------
; TOP PANEL
;-------------

    plot, u_arr,v_arr,psym=8, Position=[0.15, 0.52, 0.9, 0.90],/NoErase, XTickformat='(A1)', ytitle='V' ,$
          xr=[-50,50], yr=[-70,30],xstyle=1, ystyle=1
    plots, [u_dis_kc[0],u_dis_kc[10]], [v_dis_kc[0], v_dis_kc[10]], linestyle=2, thick=5

    xyouts, u_arr[0,0],v_arr[0,0]-dis_offset, strn(fix(dis[0]))+' pc', charsize=0.8
    xyouts, u_arr[1,0],v_arr[1,0]-dis_offset, strn(fix(dis[1]))+' pc', charsize=0.8
    xyouts, u_arr[2,0],v_arr[2,0]-dis_offset, strn(fix(dis[2]))+' pc', charsize=0.8
    xyouts, u_arr[3,0],v_arr[3,0]-dis_offset, strn(fix(dis[3]))+' pc', charsize=0.8
    xyouts, u_arr[4,0],v_arr[4,0]-dis_offset, strn(fix(dis[4]))+' pc', charsize=0.8

    xyouts, u_arr[0,0],v_arr[0,0]+5, strn(fix(vrad[0]))+' km/s', align=0, charsize=0.8
    xyouts, u_arr[0,i/2],v_arr[0,i/2]+5, strn(fix(vrad[i/2]))+' km/s', align=0, charsize=0.8
    xyouts, u_arr[0,i-1],v_arr[0,i-1]+5, strn(fix(vrad[i-1]))+' km/s', align=0, charsize=0.8

;twhya 
    IF ra[j]/15. gt 10 AND ra[j]/15 lt 13.5 AND dec[j] lt -20 then begin
        u1=-11
        v1=-18
        plots, [u1-10, u1+10, u1+10, u1-10, u1-10],[v1-10,v1-10,v1+10,v1+10, v1-10], color=red
        ;xyouts, u1,v1, 'TwHya', color=red
    ENDIF

;tuchor 
    IF (ra[j]/15. gt 20 OR ra[j]/15 lt 7.5) AND dec[j] lt -20 THEN BEGIN
        u1=-11
        v1=-21
        plots, [u1-10, u1+10, u1+10, u1-10, u1-10],[v1-10,v1-10,v1+10,v1+10, v1-10], color=green
        ;xyouts, u1,v1,'Tuc/Hor',color=green
    ENDIF

;cha near/eta cha
    IF ra[j]/15. gt 11 AND ra[j]/15 lt 13 AND dec[j] lt -70 THEN BEGIN
        u1=-11.5
        v1=-17.5
        plots, [u1-10, u1+10, u1+10, u1-10, u1-10],[v1-10,v1-10,v1+10,v1+10, v1-10], color=purple
        ;xyouts, u1,v1,'Cha',color=green
    ENDIF

;beta pic

    u1=-11
    v1=-16
    plots, [u1-10, u1+10, u1+10, u1-10, u1-10],[v1-10,v1-10,v1+10,v1+10, v1-10], color=blue
    ;xyouts, u1,v1,'B Pic',color=blue, charsize=1, align=0.5
    xyouts, 0,10,'!Mb!X Pic',color=blue, charsize=1, align=0.5
    plots, [0,u1],[10,v1+10], color=blue

;ab Dor
    u1=-8
    v1=-27
    plots, [u1-10, u1+10, u1+10, u1-10, u1-10],[v1-10,v1-10,v1+10,v1+10, v1-10], color=red
    ;xyouts, u1,v1,'AB Dor',color=red, charsize=1, align=0.5    
    xyouts, -20,-60,'AB Dor',color=red, charsize=1, align=0.5
    plots, [-20,u1],[-60+7,v1-10], color=red



ENDFOR


device, /close
set_plot,'x'

END
