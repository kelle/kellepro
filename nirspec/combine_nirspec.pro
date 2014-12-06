pro combine_nirspec, outfile,plot=plot

if n_elements(outfile) eq 0 then newfitsfile='combined.fits' else newfitsfile=outfile

@colors_kc
device, decomposed=0
color=[red,blue,green,purple,orange]

;use header info from first frame
fitsfiles=file_search('*_a.fits',count=n_fitsfiles)
tmp=readfits(fitsfiles[0],header)

datfiles=file_search('tar.dat',count=n_datfiles)

RDFLOAT,datfiles[0],pix,w1,f1,skipline=3,/double
tot_flux=f1

if keyword_set(plot) then begin
    plot, w1,f1,xr=[1.545,1.568],xstyle=1,/ynozero, color=red
    print, 'press any key to continue'
    tmp=GET_KBRD()
endif

for i=1,n_datfiles-1 do begin

    RDFLOAT,datfiles[i],pix,w2,f2,skipline=3,/double
    g2=interpol(f2,w2,w1)
    tot_flux=tot_flux+g2
    

    if keyword_set(plot) then begin
        oplot,w1,g2,color=color[i];,xr=[1.5515,1.5582],xstyle=1,/ynozero
        print, 'press any key to continue'
        tmp=GET_KBRD()
    endif
ENDFOR

avg_flux=tot_flux/n_datfiles

;trim data
data=dblarr(2,1000)
data[0,*]=w1[0:999]
data[1,*]=avg_flux[0:999]

;MODIFY HEADER
sxdelpar, header, ''  ;delete blank lines from header
sxaddhist, 'REDSPEC: Spectra reduced and extracted.',header

fits_string1=strjoin(file_basename(fitsfiles),', ',/single)
n_lines=INDGEN(ceil(strlen(fits_string1)/75.))*75
fits_string=strmid(fits_string1,n_lines,75)


label = 'COMBINE_NIRSPEC:' + strmid(systime(),4,20)
sxaddhist, label + ' Combined ' + strn(n_datfiles) + ' tar.dat files',header
sxaddhist, 'Original Files = ',header
sxaddhist, fits_string, header

writefits,newfitsfile, data,header

message, 'Combined ' + strn(n_datfiles) + ' tar.dat files',/info
message, 'Wrote '+newfitsfile, /info
if keyword_set(plot) then $
  oplot, data[0,*],data[1,*] ELSE $
  plot, data[0,*],data[1,*],xr=[1.545,1.568],xstyle=1,/ynozero

END
