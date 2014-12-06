PRO bmask_fix,x,y,bit,object

if N_params() lt 3 then begin
     print,'   Syntax -  BMASK_FITS, x, y, bit [,object'
     print, ''
     print,'   object is number'
     print,'   original bmask is overwritten.'
     return
endif

IF N_PARAMS() EQ 4 THEN object=strn(object) ELSE object='*'

;use x,y from DS9

path='/data/hillary/3/kelle/ir_spectra/irs/S12/skysub'

files=DIALOG_PICKFILE(/MULTIPLE_FILES,PATH=path, FILTER='*'+object+'*bmask.fits')

IF files[0] ne '' THEN BEGIN
 nfiles= n_elements(files)
ENDIF ELSE BEGIN
 RETURN
ENDELSE

FOR i = 0,nfiles-1 DO BEGIN

m=READFITS(files[i],h)

m[x-1,y-1] = 2^bit

hist1 = 'BMASK_FIX, ran ' + systime(0)
hist2 = 'BMASK_FIX, changed pixel' +strn(x) + ', ' + strn(y)
SXADDHIST, [hist1,hist2], h

WRITEFITS, files[i],m,h

ENDFOR

END
