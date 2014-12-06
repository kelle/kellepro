PRO HTM2FITS, FitsPath, Object, before=before, after=after

;expect two columns of wavelength and flux

;RDFLOAT,FitsPath,w,flux,silent=silent
READCOL,FitsPath,w,flux,silent=silent, comment='#',format='F,F'

pixel_size=w[2]-w[1]

MKHDR, h, flux
SXADDPAR, h, 'OBJECT', object, BEFORE='COMMENT'
SXADDPAR, h, 'TELESCOP', 'Keck I', BEFORE='COMMENT'
SXADDPAR, h, 'CRVAL1', w[0], BEFORE='COMMENT'
SXADDPAR, h, 'CD1_1', pixel_size, BEFORE='COMMENT'

SXADDHIST, 'Original file: '+FitsPath, h
SXADDHIST, 'Created with htm2fits.pro by Kelle Cruz', h

if n_elements(before) ne 0 then before=string(before)+'_' else before=''
if n_elements(after) ne 0 then after='_'+string(after) else after=''

newfitsfile=before+object+after+'.fits'

WRITEFITS, newfitsfile, flux, h
print, 'WROTE: ',newfitsfile

END
