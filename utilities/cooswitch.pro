pro cooswitch

; This routine converts decimal degree coordinates to
; sexigesimal format, or vice-versa, in double precision.
;
; SIRTF OST, July 2003, version 1.0 
; Spitzer OST, September 2004, version 1.1 (fixed bug in handling 
;    conversion of negative declinations > -1 deg)

print, ' '
print, 'This is COOSWITCH (v1.1, SSC OST)'
print, ' '
print, 'This utility converts celestial coordinates from decimal '
print, 'degrees to sexigesimal format, or vice-versa.  It does not '
print, 'apply any corrections for proper motions or precession, '
print, 'merely transforms given coordinates from one format to '
print, 'the other.'

starthere:

;*** get target coordinates
;*** (routine will accept either sexigesimal or decimal format)
print, ' '
print, 'Acceptable coordinate formats:'
print, '     - sexigesimal (hh mm ss +dd mm ss)'
print, '     - decimal degrees (xx.xxxxxxx +yy.yyyyyyy)'
print, '     - leave blank to exit

coords= ''
read, coords, prompt='Enter target coordinates: '

if (coords eq '') then begin
 print, ' '
 goto, bailout
endif

;*** parse string containing input coordinates
coosplit = strsplit(coords, ' ', /extract)

case (n_elements(coosplit)) of
 2 : begin
      ;*** coordinates entered in decimal degrees
      radeg  = double(coosplit[0])
      decdeg = double(coosplit[1])
      if (strpos(coords, '-') ge 0) then decneg = 'TRUE' else decneg = 'FALSE'
      decdeg = abs(decdeg)

      if (radeg lt 0.0d or radeg gt 360.0d or decdeg gt 90.0d) then goto, error

      ;*** convert decimal coordinates to sexigesimal format
      decd = double(fix(decdeg))
      decm = double(fix((decdeg - decd)*60.0d))
      decs = ((decdeg - decd)*60.0d - decm)*60.0d

      rahms = radeg/15.0d
      rah   = double(fix(rahms))
      ram   = double(fix((rahms - rah)*60.0d))
      ras   = ((rahms - rah)*60.0d - ram)*60.0d
     end
 6 : begin
      ;*** coordinates entered in sexigesimal
      rah   = double(coosplit[0])
      ram   = double(coosplit[1])
      ras   = double(coosplit[2])
      radeg = (rah + ram/60.0d + ras/3600.0d)*15.0d

      decd   = double(coosplit[3])
      if (strpos(coords, '-') ge 0) then decneg = 'TRUE' else decneg = 'FALSE'
      decd   = abs(decd)
      decm   = double(coosplit[4])
      decs   = double(coosplit[5])
      decdeg = decd + decm/60.0d + decs/3600.0d

      if (rah gt 24.0d or ram gt 60.0d or ras gt 60.0d or $
       decd gt 90.0d or decm gt 60.0d or decs gt 60.0d) then goto, error
     end
 else : begin
         goto, error
        end
endcase

;*** prepare strings for output
srah = strtrim(string(fix(rah)),2)
sram = strtrim(string(fix(ram)),2)
sras = strtrim(string(ras,format='(d6.3)'),2)
if (rah lt 10.0d) then srah = '0'+srah
if (ram lt 10.0d) then sram = '0'+sram
if (ras lt 10.0d) then sras = '0'+sras

if (decneg eq 'TRUE') then sdecsign = '-' else sdecsign = '+'
sdecd = strtrim(string(fix(decd)),2)
sdecm = strtrim(string(fix(decm)),2)
sdecs = strtrim(string(decs,format='(d5.2)'),2)
if (decd lt 10.0d) then sdecd = '0'+sdecd
if (decm lt 10.0d) then sdecm = '0'+sdecm
if (decs lt 10.0d) then sdecs = '0'+sdecs

outstr1de = '  DEC = '+sdecsign+strtrim(string(decdeg,format='(d11.7)'),2)
outstr2de = '  DEC = '+sdecsign+sdecd+' '+sdecm+' '+sdecs

spacer1 = ''
spacer2 = ''

outstr1ra = ''
outstr2ra = ' '

;*** make output strings pretty
while (strlen(outstr1ra) ne strlen(outstr2ra)) do begin
 outstr1ra = 'RA = '+strtrim(string(radeg,format='(d11.7)'),2)+spacer1
 outstr2ra = 'RA = '+srah+' '+sram+' '+sras+spacer2
 if (strlen(outstr1ra) gt strlen(outstr2ra)) then spacer2 = spacer2+' ' $
  else spacer1 = spacer1+' '
endwhile

;*** output coordinates 
print, ' '
if (n_elements(coosplit) gt 2) then begin
 print, outstr2ra+outstr2de
 print, outstr1ra+outstr1de
endif else begin
 print, outstr1ra+outstr1de
 print, outstr2ra+outstr2de
endelse

goto, starthere

error:

print, ' '
print, 'Check your coordinates!'
goto, starthere

bailout:

end
