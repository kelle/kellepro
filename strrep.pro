function strrep, str, val1, val2

str2 = str
if (n_elements(str) eq 1) then str2 = [str2]

for i=long(0),n_elements(str2)-1 do begin
 flg = 0
 while (flg eq 0) do begin
  pos = strpos(str2(i),val1)
  if (pos eq -1) then flg=1 else $
   str2(i) = strmid(str2(i),0,pos)+val2+strmid(str2(i),pos+strlen(val1),strlen(str2(i)))
 endwhile
endfor

return, str2
end
