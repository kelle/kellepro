pro sub_ab_kc, mid_line

;, a_file,b_file

files=FILE_SEARCH('*.fits')
a_file=files[0]
b_file=files[1]

a=readfits(a_file)
b=readfits(b_file)

i=a-b
j=b-a

out=fltarr(1024,256)

out[*,0:mid_line]=j[*,0:mid_line]
out[*,mid_line+1:255]=i[*,mid_line+1:255]

writefits,'sub.fits',out

MESSAGE, 'Wrote sub.fits using '+ a_file +' ' + b_file,/info

imdisp, out

END
