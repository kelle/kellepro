PRO ab_avg, input, output
	
	; NEED TO ADD UNCERTAINTY

if n_params() lt 1 then begin
    print,"Syntax - ab_avg,['1[6,8]','2[0,2]','30'],output"
    print, 'will get you files 16, 18, 20, 22, and 30'
    return
endif

;ab_avg, '5[2,4,6],60'
;input = ['5[2,4,6]','59']

for i=0,n_elements(input)-1 DO BEGIN
    a=file_search('*'+input[i]+'*')
    if i eq 0 then fitsfiles = a else $
      fitsfiles=[fitsfiles,a]
endfor
nfiles=n_elements(fitsfiles)

sum=0

for j=0,nfiles-1 do begin
    print, fitsfiles[j]
    nod=readfits(fitsfiles[j],ah)
    if j eq nfiles/2 then begin
        header=ah
        print, 'HEADER '+fitsfiles[j]+'!C'
    endif
    sum=sum+nod
endfor

avg=sum/nfiles

fits_string1=strjoin(file_basename(fitsfiles),', ',/single)
n_lines=INDGEN(ceil(strlen(fits_string1)/75.))*75
fits_string=strmid(fits_string1,n_lines,75)

label = 'AB_AVG:' + strmid(systime(),4,20)
sxaddhist, label + ' Averaged ' + strn(nfiles) + ' files',header
sxaddhist, 'Original Files = ', header
sxaddhist, fits_string, header

newfitsfile=output+'.fits'
writefits,newfitsfile,avg,header

message, 'Combined ' + strn(nfiles) + ' files',/info
message, 'Wrote '+newfitsfile, /info

END
