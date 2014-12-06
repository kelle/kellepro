RO UVW_young

;calls gal_uvw

ra=181.889447
dec=-39.548332

pmra=-78 ;mas/yr
pmdec=-24

;vrad=-15.4 ;km/s

dis=23.3 ;pc

u_arr=fltarr(16)
v_arr=fltarr(16)
w_arr=fltarr(16)

i=0

FOR vrad=-80,80,10, do begin

    gal_uvw, u,v,w, ra=ra,dec=dec,pmra=pmra, pmdec=pmdec, vrad=vrad, dis=dis


    u_arr[i]=u
    v_arr[i]=v
    w_arr[i]=w

    i=i+1

endfor

stop

END
