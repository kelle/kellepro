;+
; NAME:
;     mc_filltable
;
; PURPOSE:
;     To create a text file with columns nicely aligned and with headers.
;
; CATEGORY:
;     I/O
;
; CALLING SEQUENCE:
;     mc_filltable,lun,space,v1,v2,v3,v4,...,v50,COLNUM=colnum,$
;                      REPEATLABEL=repeatlabel,CANCEL=cancel
;
; INPUTS:
;     lun   - The Logical Unit Number of file into which the data is written.
;     space - The number of spaces between columns.
;     vn    - A structure of the format, {l:'J',v:[10,11,12],f:'f4.1'}
;             where l = column label
;                   v = the array of values
;                   f = the format code
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     COLNUM      - Set to also put a column number under the column
;                   label.  
;     REPEATLABEL - The number of rows after which to repeat the column
;                   labels.  The default is 30.   Set to -1 to supress.
;     CANCEL      - Set on return if there is a problem.
;
; OUTPUTS:
;     None
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     None
;
; SIDE EFFECTS:
;     Makes viewing and writing text files easier!
;
; RESTRICTIONS:
;     Currently it must write to a file that is already opened.
;
; PROCEDURE:
;     Just some variable parsing.
;
; EXAMPLE:
;     x = findgen(10)
;     y = x+1
;     openw,lun,'test.dat',/GET_LUN
;     mc_filltable,lun,2,{l:'X',v:x,f:'I2'},{l:'Y',v:y,f:'I2'},/COLNUM
;     free_lun, lun
;
;     The text file looks like,
;     
;     #==========
;     #  X    Y
;     #(1)  (2)
;     #==========
;        0    1
;        1    2
;        2    3
;        3    4
;        4    5
;        5    6
;        6    7
;        7    8
;        8    9
;        9   10
;
; MODIFICATION HISTORY:
;     2008-08-21 - Written by M. Cushing, Institute for Astronomy,
;                  University of Hawai'i.
;-
pro mc_filltable,lun,space,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14, $
                 v15,v16,v17,v18,v19,v20,v21,v22,v23,v24,v25,v26,v27,v28, $
                 v29,v30,v31,v32,v33,v34,v35,v36,v37,v38,v39,v40,v41,v42,v43,$
                 v44,v45,v46,v47,v48,v49,v50,COLNUM=colnum, $
                 REPEATLABEL=repeatlabel,CANCEL=cancel

  cancel = 0

  nvar = n_params()-2
  nvals = n_elements(v1.v)

  if n_elements(REPEATLABEL) eq 0 then repeatlabel = 30.0
  if repeatlabel eq -1 then repeatlabel = nvals
  
  sformat = '(A1,'
  totlen = 0

  colnums = ''

  collabels = strarr(nvar)
  colnums   = strarr(nvar)
  formats   = strarr(nvar)

  for i = 0,nvar-1 do begin

;  Extract the label and the format code 

     str = 'val = v'+strtrim(i+1,2)+'.l'
     junk = execute(str)
     collabels[i] = val
     
     str = 'val = v'+strtrim(i+1,2)+'.f'
     junk = execute(str)
     formats[i] = val

;  Check the lengths

     collabel_l = strlen(collabels[i])
     colnum_l   = strlen(strtrim(i+1,2))+2
     format_l   = fix(strmid((strsplit(formats[i],'.',/EXTRACT))[0],1))

     length = collabel_l > colnum_l > format_l

;  Create the string format statment

     sformat = sformat+'A'+strtrim(length,2)+''

     if i eq nvar-1 then sformat = sformat+')' else $
        sformat = sformat+','+strtrim(space,2)+'X,'
     
     colnums[i] = '('+strtrim(i+1,2)+')'

;  Find the total length for the ==== bars.

     totlen = totlen+space+length 

  endfor

;  Now write to the file
  
  for i = 0,nvals-1 do begin

     if i mod repeatlabel eq 0 then begin
                
        printf, lun, '#'+strjoin(replicate('=',totlen))
        printf, lun, '#',collabels,FORMAT=sformat
        if keyword_set(COLNUM) then printf, lun, '#',colnums,FORMAT=sformat
        printf, lun, '#'+strjoin(replicate('=',totlen))
                
     endif

     vals = strarr(nvar)
     for j = 0,nvar-1 do begin

        str = 'val = string(v'+strtrim(j+1,2)+'.v['+ $
              strtrim(i,2)+"],FORMAT='("+formats[j]+")')"
        junk = execute(str)
        vals[j] = val


     endfor
     printf, lun, ' ',vals,FORMAT=sformat

  endfor
  
end