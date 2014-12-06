FUNCTION GET_FILE, obj, dir,SILENT=silent
;v1

silent=keyword_set(SILENT)

use_file=1
file=''

IF strmid(obj,0,2) eq '2M' THEN obj_digits_start = 2 ELSE $
obj_digits_start=stregex(obj,'[0-9]',length=len,/subexpr)

obj_digits=strmid(obj,obj_digits_start)

if dir eq 'logs/' then begin
  files=file_search(dir,'*'+obj_digits+'*refdata*', count=nfiles)
ENDIF ELSE BEGIN
  files=file_search(dir,'*'+obj_digits+'*', count=nfiles)
ENDELSE

;stop

if dir eq 'ref_lis/' then begin 
 if nfiles ge 1 THEN files=[files,'ref_lis/allref_lis.tbl'] ELSE files='ref_lis/allref_lis.tbl'
 nfiles=nfiles+1
endif

CASE 1 of 
nfiles eq 0: BEGIN
	IF ~silent THEN PRINT, 'No files found'
	;READ, file, PROMPT='Enter path of file: '
	;file=''
	END
nfiles eq 1: BEGIN
	file=files[0]
	END
nfiles gt 1 and ~SILENT: BEGIN
	FOR m=0, nfiles-1 do begin
		print, strn(m)+') '+files[m]
	ENDFOR
	READ, use_file, PROMPT='Enter number of file to use: '
	file=files[use_file]
	END
nfiles gt 1 and SILENT: BEGIN
	file=files
	END	
ENDCASE

IF ~SILENT THEN PRINT, 'USING: '+file

RETURN, file

END
