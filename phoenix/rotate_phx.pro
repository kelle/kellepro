PRO rotate_phx, path=path


; Rotates Phoenix data 90 deg for reduction with REDSPEC
; Written Feb 23, 2009, Kelle Cruz

if n_elements(path) eq 0 then path=''
files=file_search(path+'*.fits',count=n_files)

direction=1

FOR i=0,n_files-1 DO BEGIN
    raw_image=readfits(files[i],header)
    rot_image=rotate(raw_image,direction)

;Add History to Header
    ;delete blank lines from header
    sxdelpar, header, ''

    label = 'ROTATE_PHX:' + strmid(systime(),4,20)
    sxaddhist, label + ' Image = ROTATE(Image,' + strtrim(direction,2) + ')',header

    newfile='rot_'+files[i]
    
    writefits, newfile,rot_image,header

ENDFOR

END
