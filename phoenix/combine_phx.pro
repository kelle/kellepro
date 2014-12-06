pro combine_phx, outfile, outspec,update=update;,ps=ps
; MODIFICATION HISTORY:	
;	2010 July 18: modified to use mc_meancomb instead of just average
;				gives variance array

if n_elements(outfile) eq 0 then newfitsfile='combined.fits' else newfitsfile=outfile

@colors_kc
device, decomposed=0
color=[blue,cyan,purple,dkpurple2,pink,red,purple,ltorange,dkorange,dkblue]

;use header info from first frame
fitsfiles=file_search('p?/*_200*.fits',count=n_fitsfiles)
tmp=readfits(fitsfiles[0],header)
obj_name=sxpar(header,'OBJECT')

datfiles=file_search('p?/tar.dat',count=n_datfiles)
print,datfiles
nspec=n_datfiles*2
norms=fltarr(n_datfiles)

;trim=10
trim=860
flux_arr=fltarr(trim,nspec)

; ;READ In FIRST dat file.  TWO spectra
; RDFLOAT,datfiles[0],flux_norm,flat_norm,skipline=1,/double,numline=1
; RDFLOAT,datfiles[0],pix,w,flux,n1,n2,skipline=3,/double
; 
; ;determine if flux = n1 +n2 or n1-n2
; if abs(flux[0] - (n1[0]-n2[0])/flux_norm[0]) lt $
; 	abs(flux[0] - (n1[0]+n2[0])/flux_norm[0]) then n2=n2*(-1.)
; 
; w=w[0:trim-1]
; flux=flux[0:trim-1]
; n1=n1[0:trim-1]
; n2=n2[0:trim-1]
; 
; j=0 ; loop over spectra
; i=0 ; loop over dat files
; 
; ;normalize both nods ta 1
; norms[i]=flux_norm[0]
; offset1 = 1.0-median(n1/norms[i])
; offset2 = 1.0-median(n2/norms[i])
; 
; flux_arr[*,j]=n1/norms[i]+offset1
; flux_arr[*,j+1]=n2/norms[i]+offset2

!p.multi=[0,1,3]
!p.charsize=2

j=0 ; loop over spectra
for i=0,n_datfiles-1 do begin
	; read in normaizations
	RDFLOAT,datfiles[i],flux_norm,flat_norm,skipline=1,/double,numline=1
	; read in spectra
    RDFLOAT,datfiles[i],pix,wdat,fdat,nod1dat,nod2dat,skipline=3,/double

	;determine if flux = n1 +n2 or n1-n2
	if abs(fdat[0] - (nod1dat[0]-nod2dat[0])/flux_norm[0]) lt $
		abs(fdat[0] - (nod1dat[0]+nod2dat[0])/flux_norm[0]) then $
		nod2dat=nod2dat*(-1.)
	;if (fdat[0] - (nod1dat[0]+nod2dat[0])/flux_norm[0]) lt $
	;	-1D*(fdat[0] - (nod1dat[0]+nod2dat[0])/flux_norm[0]) then begin 
	;		nod1dat=nod1dat*(-1.)
	;endif
	print, 'These should be the same number:'
	print, fdat[0], (nod1dat[0]+nod2dat[0])/flux_norm[0]

	;trim
	nod1dat=nod1dat[0:trim-1]
	nod2dat=nod2dat[0:trim-1]
	wdat=wdat[0:trim-1]
	
	;use wavelengeth scale of first file
	if i eq 0 then begin
		w = wdat
		nod1=nod1dat
		nod2=nod2dat
	endif else begin
		nod1=interpol(nod1dat,wdat,w)
		nod2=interpol(nod2dat,wdat,w)
	endelse
	
	norms[i]=flux_norm[0]
	offset1 = 1.0-median(nod1/norms[i])
	offset2 = 1.0-median(nod2/norms[i])

	flux_arr[*,j]=nod1/norms[i]+offset1
	flux_arr[*,j+1]=nod2/norms[i]+offset2
	
	if i eq 0 then plot, w,flux_arr[*,0],xr=[1.5515,1.55779],$
		xstyle=1,/ynozero, /nodata,yr=[-1.5,2.5]
	oplot,w,flux_arr[*,j],color=color[j]
	oplot,w,flux_arr[*,j+1],color=color[j+1]
	tmp=get_kbrd()
	
j=j+2 ; loop over spectra
ENDFOR

;keep=[0,1,2,3]
;nkeep=n_elements(keep)
;!p.multi=[0,1,3]
;for m=0,nkeep-1 do begin
 	;norm_flux_arr[*,keep[m]]=flux_arr[*,keep[m]] * norms2[keep[m]]*2
;         if m eq 0 then plot, w,norm_flux_arr[*,keep[m]],xr=[1.5515,1.55779],xstyle=1,/ynozero, /nodata,charsize=2
; 		oplot,w,norm_flux_arr[*,keep[m]],color=color[keep[m]]
;endfor 

mc_meancomb,flux_arr,mean_flux,mvar

data=dblarr(3,trim)
data[0,*]=w
data[1,*]=mean_flux
data[2,*]=sqrt(mvar);/median(mean_flux)

outspec=data

;MODIFY HEADER
sxdelpar, header, ''  ;delete blank lines from header
sxaddhist, 'REDSPEC: Spectra reduced and extracted.',header

fits_string1=strjoin(file_basename(fitsfiles),', ',/single)
n_lines=INDGEN(ceil(strlen(fits_string1)/75.))*75
fits_string=strmid(fits_string1,n_lines,75)

label = 'COMBINE_PHX:' + strmid(systime(),4,20)
;sxaddhist, label + ' Combined ' + strn(n_datfiles) + ' tar.dat files',header
;sxaddhist, label + ' Combined ' + strn(nkeep) + ' spectra',header
sxaddhist, label + ' Combined ' + strn(n_datfiles) + ' spectra',header
sxaddhist, 'Original Files = ',header
sxaddhist, fits_string, header

if keyword_set(update) then begin
	dateobs = ''
	read,'Enter the Date-Obs: ',dateobs
	read,'Enter the vhelio: ',vhelio
	sxaddpar, header, 'date-obs', dateobs
	sxaddpar, header, 'vhelio', vhelio
endif

writefits,newfitsfile, data,header

;message, 'Combined ' + strn(n_datfiles[keep]) + ' tar.dat files',/info
message, 'Wrote '+newfitsfile, /info
;if keyword_set(plot) then $
 ; oplot, data[0,*],data[1,*] ELSE $
plot, data[0,*],data[1,*],xr=[1.5515,1.55779],xstyle=1
oplot,data[0,*],data[2,*],color=grey
plot,w,data[1,*]/data[2,*],xr=[1.5515,1.55779],xstyle=1,yr=[0,30]

!p.multi=0
!p.charsize=1

END
