PRO coadd_phx

raw_dir='/scr6/kelle/nir_highres_raw/phoenix/GS-07B/'
work_dir='/scr3/kelle/nir_highres/phoenix/GS-07B/coadded/'


files=[file_search(raw_dir+'*5[4-9]*'),file_search(raw_dir+'*6[0-1]*')]
short_file=file_basename(files)
nfiles=(size(files,/dim))[0]
raw=fltarr(nfiles,256,1024)

;READ IN FILES
for i=0,nfiles-1 do begin
    raw[i,*,*]=readfits(files[i],h)
    if i eq 0 then h1=h
    if i eq 1 then h2=h
endfor

;SUM NODS
sum_a=raw[0,*,*]+raw[3,*,*]+raw[4,*,*]+raw[7,*,*]
sum_b=raw[1,*,*]+raw[2,*,*]+raw[5,*,*]+raw[6,*,*]


label = 'COADD_PHX: ' 
sxaddhist, label + strmid(systime(),4,20), h1
sxaddhist, label + 'IMAGE= ' + short_file[0]+ '+' + short_file[3] +'+' + short_file[4] +'+' + short_file[7], h1

sxaddhist, label + strmid(systime(),4,20) ,h2
sxaddhist, label + 'IMAGE= ' + short_file[1]+ '+' + short_file[2] +'+' + short_file[5] +'+' + short_file[6], h2

;ROTATE
direction=1
rot_sum_a=rotate(reform(sum_a),direction)
rot_sum_b=rotate(reform(sum_b),direction)

sxaddhist, label + ' Image = ROTATE(Image,' + strtrim(direction,2) + ')',h1
sxaddhist, label + ' Image = ROTATE(Image,' + strtrim(direction,2) + ')',h2


writefits, work_dir+'sum_a.fits',rot_sum_a,h1
writefits, work_dir+'sum_b.fits',rot_sum_b,h2

END
