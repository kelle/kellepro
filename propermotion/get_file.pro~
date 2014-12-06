FUNCTION GET_FILE, obj, dir,String=string,Silent=silent,Count=count
;v2
;use str='J' to get ref_num_J only, for example
;use /count to just return number of files that match

IF n_elements(string) EQ 0 THEN string='*'

use_file=1
file=''

IF strmid(obj,0,2) eq '2M' THEN obj_digits_start = 2 ELSE $
obj_digits_start=stregex(obj,'[0-9]',length=len,/subexpr)

obj_digits=strmid(obj,obj_digits_start)

if dir eq 'logs/' then begin
  files=file_search(dir,'*'+obj_digits+'*refdata*', count=nfiles)
ENDIF ELSE BEGIN
  files=file_search(dir,'*'+obj_digits+'*'+ string +'*', count=nfiles)
ENDELSE

;stop

if dir eq 'ref_lis/' then begin 
 tbl_files = file_search(dir,'*'+'ref_lis'+'*.tbl',count=tbl_nfiles)
 if nfiles ge 1 THEN files=[files,tbl_files] ELSE files=tbl_files
 nfiles=nfiles+tbl_nfiles
endif

CASE 1 of 
nfiles eq 0: BEGIN
	IF ~keyword_set(silent) THEN PRINT, 'No files found'
	;READ, file, PROMPT='Enter path of file: '
	;file=''
	END
nfiles eq 1: BEGIN
	file=files[0]
	END
nfiles gt 1 and ~keyword_set(silent): BEGIN
	FOR m=0, nfiles-1 do begin
		print, strn(m)+') '+files[m]
	ENDFOR
	READ, use_file, PROMPT='Enter number of file to use: '
	file=files[use_file]
	END
nfiles gt 1 and keyword_set(silent): BEGIN
	file=files
	END	
ENDCASE

IF ~keyword_set(silent) THEN PRINT, 'USING: '+file

IF keyword_set(count) THEN RETURN, nfiles ELSE RETURN, file

END
