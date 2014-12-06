pro sub_ab_phx, mid_line, swap=swap,path=path

;, a_file,b_file
if n_elements(path) eq 0 then path=''
files=FILE_SEARCH(path+'*.fits')
a_file=files[0]
b_file=files[1]

a=readfits(a_file)
b=readfits(b_file)

if keyword_set(swap) then begin
    j=a-b
    i=b-a
endif else begin
    i=a-b
    j=b-a
endelse

out=fltarr(1024,256)

out[*,0:mid_line]=j[*,0:mid_line]
out[*,mid_line+1:255]=i[*,mid_line+1:255]

writefits,path+'sub.fits',out

MESSAGE, 'Wrote sub.fits using '+ a_file +' ' + b_file,/info

;imdisp, out

loadct,39
tvscl, hist_equal(out)

;stop

END
