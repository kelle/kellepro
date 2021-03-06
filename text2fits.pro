PRO TEXT2FITS, filename, object=Object, before=before, after=after

;calls KREADTEXT

;expect two columns of wavelength and flux
if N_params() lt 1 then begin
     print,"Syntax -  TEXT2FITS,filename [, Object='object', Before='before', after='after']"
;     print,'useful information'
     return
endif
openr, unit, filename,ERROR=error, /get_lun
if error NE 0 then begin
    message,/con,' ERROR - Unable to locate file ' + filename
    return
endif

if strpos(filename,'.') ne -1 then begin
	ext=(strsplit(filename,'.',/extract))[1]
	rootname=(strsplit(filename,'.',/extract))[0]
endif else begin
	ext = 'txt'
	rootname=filename
endelse
	
if ext ne 'txt' then message, 'text file expected',/info

;CHECK FOR FITS HEADER
first_line=''
READF, unit, first_line
if strmid(first_line,0,6) eq 'BITPIX' OR strmid(first_line,0,6) eq 'SIMPLE' $
  then begin
    header=1 
    message, 'FITS Header Detected',/info
endif else begin
    header=0
    message, 'No FITS Header Detected',/info
endelse
close, unit
free_lun,unit


IF header eq 1 then begin
    spec=KREADTEXT(filename,hdr)
    w=reform(spec[0,*])
    flux=reform(spec[1,*])
	data=fltarr(2,n_elements(w))
    data[0,*]=w
    data[1,*]=flux
	
	MKHDR, hdr2, [[w],[flux]]
endif

IF header eq 0 then begin
    READCOL,filename,w,flux,silent=silent, comment='#',format='F,F'
    data=fltarr(2,n_elements(w))
    data[0,*]=w
    data[1,*]=flux
    ;pixel_size=w[2]-w[1]
    
    MKHDR, hdr, [[w],[flux]]
    
    ;SXADDPAR, hdr, 'TELESCOP', 'Keck I', BEFORE='COMMENT'
    ;SXADDPAR, hdr, 'CRVAL1', w[0], BEFORE='COMMENT'
    ;SXADDPAR, hdr, 'CD1_1', pixel_size, BEFORE='COMMENT'

ENDIF

IF strn(FXPAR(hdr,'CTYPE1')) eq 'MULTISPE' then begin
    ;data=[[w],[flux]]
    data=fltarr(2,n_elements(w))
    data[0,*]=w
    data[1,*]=flux
    fxhclean,hdr
    mkhdr, hdr2,[[w],[flux]]
    SXADDHIST, 'Minmal Header keywords above created by text2fits.pro', hdr2,location='COMMENT'
    hdr2_end=(where(strmid(hdr2,0,7) eq 'COMMENT'))[0] -1
    hdr_top=hdr2[0:hdr2_end]
    newheader=[hdr_top,hdr]
    check_fits, [[w],[flux]],newheader
    hdr=newheader
ENDIF

;add object name to header
original= fxpar(hdr,'OBJECT')
print, 'Original Object Name: ',original
IF n_elements(object) ne 0 then BEGIN
    SXADDPAR, hdr, 'OBJECT', object, BEFORE='COMMENT'
    new=fxpar(hdr,'OBJECT')   
    print,'New Object Name: ', new 
ENDIF

SXADDHIST, 'Original file: '+filename, hdr
SXADDHIST, 'Created with text2fits.pro by Kelle Cruz', hdr

if n_elements(before) ne 0 then before=string(before)+'_' else before=''
if n_elements(after) ne 0 then after='_'+string(after) else after=''

;if n_elements(object) ne 0 then $
;  newfitsfile=before+object+'_'+rootname+after+'.fits' $
;  else 
newfitsfile=before+rootname+after+'.fits'

IF FILE_TEST(newfitsfile) ne 1 then begin	
	if n_elements(hdr2) ne 0 then WRITEFITS, newfitsfile, data, [hdr2,hdr] else WRITEFITS, newfitsfile, data, hdr
    message, 'WROTE: '+newfitsfile,/info
endif else begin
    message, 'would overwrite existing file: '+newfitsfile, /cont
    message, 'NO FILE CREATED',/info
endelse

plot, w,flux, xstyle=1,ystyle=1


END
