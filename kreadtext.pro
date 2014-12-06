FUNCTION KREADTEXT, filename, header, silent=silent



;object=(strsplit(data_file,'.txt',/extract,/regex))[0]
;IF ~keyword_set(silent) THEN print, object

n_lines=FILE_LINES(filename)

openr, unit, filename,ERROR=error, /get_lun
if error NE 0 then begin
    message,/con,' ERROR - Unable to locate file ' + filename
    return, -1
endif

;
; READ IN HEADER
;MODIFED FROM READFITS
;
doheader = arg_present(header)
hbuf=180
block = string(replicate(32b,80,36))
w = [-1]
header = strarr(hbuf)
headerblock = 0L
i = 0L   

while w[0] EQ -1 do begin

    if EOF(unit) then begin 
        message,/ CON, $
                'EOF encountered attempting to read extension '; + strtrim(ext,2)
        free_lun,unit
        return,-1
    endif

    readf, unit, block
    headerblock = headerblock + 1
    w = where(strlen(block) NE 80, Nbad)
    if (Nbad GT 0) then begin
        message,'Warning-Invalid characters in header',/INF,NoPrint=Silent
        message, 'Attempting to fix',/INF,NoPrint=Silent
        for j=0,nbad-1 do begin
            bad_header=block[w[j]]
            new_header=string(replicate(32b, 80))
            strput,new_header,bad_header
            block[w[j]]=new_header
        ENDFOR
    endif

    w = where(strmid(block, 0, 8) eq 'END     ', Nend)
    if (headerblock EQ 1) or doheader then begin
        if Nend GT 0 then  begin
            if headerblock EQ 1 then header = block[0:w[0]]   $
            else header = [header[0:i-1],block[0:w[0]]]
        endif else begin
            header[i] = block
            i = i+36
            if i mod hbuf EQ 0 then $
              header = [header,strarr(hbuf)]
        endelse   
    endif
endwhile


;
;END FROM READFITS
;

;Read in blanks + first line of data

;point_lun, unit, 0
;skip_lun, unit, n_headerlines, /lines
;comment=''
;blank=' '
;n_blanklines=0
;WHILE blank eq ' ' DO BEGIN
;    READF, unit, comment
;    blank = strmid(comment,0,1)
;    n_blanklines=++n_blanklines
;ENDWHILE
;n_blanklines=n_blanklines-1


;ENDIF; header exists

close, unit
free_lun,unit

n_headerlines=n_elements(header)
data_length = sxpar(header,'naxis1')
skiplines=n_lines-data_length

;read in two columns of data
READCOL, filename,  wavelength, flux, FORMAT='F,F',skipline=skiplines ;n_headerlines+n_blanklines

spectrum=DBLARR(2, data_length)
spectrum[0,*] = wavelength
spectrum[1,*] = flux

RETURN, spectrum

END
