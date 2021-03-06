pro convert_kc
;original by mike cushing
;modified by kelle and mike 2006 Aug.

model_root='/data/hillary/1/kelle/Saumon_Models'
files=FILE_SEARCH(model_root,'*f?')

ranges = [0.6, 0.9,  1.1,  1.4,  1.8,  2.5, 4.2, 5.5, 7.5, 15.5]
res   = [2000, 2000, 2000, 2000, 2000, 300, 300, 60, 120]

;text file with one model file per line
;readcol,'../toopt/list',files,FORMAT='A'

nfiles = n_elements(files)
nrange = n_elements(ranges)-1

for i = 0,nfiles-1 do begin

;    rdfloat,'../toopt/'+files[i],x,y,SKIP=3
    rdfloat,files[i],x,y,SKIP=3
    print, files[i]
    s = sort(x)
    x = x[s]
    y = y[s]

;    ndat = (4.2-0.6)/1.5e-5
	ndat=(2.5-0.6)/1.5e-5
    nw = dindgen(double(ndat))*1.5e-5 + 0.6
    ;ndat = (15.0-4.2)/2e-3
    ;nw2 = dindgen(double(ndat))*3e-3 + 4.2+3e-3
    ;nw = [nw,nw2]

    linterp,x,y,nw,nf ;astrolib

;  Do the smoothing
    
    for k = 0,nrange-1 do begin
        
        z = where(nw gt ranges[k] and nw le ranges[k+1])
        
	mdisp=1.5e-5 ;resampled model dispersion delta lambda
        ;mdisp = (ranges[k] ge 4.2) ? (3e-3):(1.5e-5)
        ;ddisp = (ranges[k+1]/2.-ranges[k]/2. + ranges[k])/res[k]
        ddisp = 6.4e-3 ; 1.6/250 middle lambda/resolving power

        FWHM  = ddisp/mdisp
;        print, ranges[k],ranges[k+1],res[k],mdisp,ddisp,FWHM
        
        kernel = psf_gaussian(NPIX=FWHM*10.,NDIMEN=1,FWHM=fwhm,/NORM)
        ;cf = convol(nf,kernel)
	cf= convol(nf,kernel,/edge_wrap)       

        ;sw = (k eq 0) ? nw[z]:[sw,nw[z]]
        ;sf = (k eq 0) ? cf[z]:[sf,cf[z]]
            
    endfor


 mc_writespexfits, nw, cf, files[i]+'.fits',XUNITS='um', $
                     YUNITS='ergs-1cm-1Hz-1',WAVETYPE='Vacuum'

;    mc_writespexfits,sw,sf,files[i]+'.fits',XUNITS='um', $
;                     YUNITS='ergs-1cm-1Hz-1',WAVETYPE='Vacuum'


endfor

end

