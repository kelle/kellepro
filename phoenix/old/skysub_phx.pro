PRO skysub_phx

raw_dir='/scr6/kelle/nir_highres_raw/phoenix/GS-07B/'
work_dir='/scr3/kelle/nir_highres/phoenix/GS-07B/processed/'


files=[file_search(raw_dir+'*5[4-9]*'),file_search(raw_dir+'*6[0-1]*')]
short_files=FILE_BASENAME(files)
nfiles=8
npairs=nfiles/2

image=readfits(files[0],header)
label = 'COADD_PHX: ' 
sxaddhist, label + strmid(systime(),4,20) ,header

FOR i=0,nfiles-1,2 do begin
    image1=readfits(files[i],h1)
    image2=readfits(files[i+1],h2)
    if i eq 0 then h3=h1

    new1=image1-image2
    new2=image2-image1

    sxaddhist, label + 'Image=' + short_files[i]+'-'+short_files[i+1],h1
    sxaddhist, label + 'Image=' + short_files[i+1]+'-'+short_files[i],h2

;ROTATE
    direction=1
    rot_new1=rotate(new1,direction)
    rot_new2=rotate(new2,direction)
    if i eq 0 then arc=rotate(image1,direction)

   sxaddhist, label + ' Image = ROTATE(Image,' + strtrim(direction,2) + ')',h1
   sxaddhist, label + ' Image = ROTATE(Image,' + strtrim(direction,2) + ')',h2
   if i eq 0 then sxaddhist, label + ' Image = ROTATE(Image,' + strtrim(direction,2) + ')',h3

   writefits, work_dir+'sub_'+short_files[i],rot_new1,h1
   writefits, work_dir+'sub_'+short_files[i+1],rot_new2,h2
   if i eq 0 then writefits, work_dir+'arc_'+short_files[i],arc,h3

endfor



END
