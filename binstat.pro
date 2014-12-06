function binstat, n, c, sig=sig, plot=plot

if N_params() lt 1 then begin
     print,'result = binstat(n,c [, sig=, /plot]'
     print,'n=sample size'
	print,'c=number of what your looking for (ie, binaries, M7s with J-K>1)'
	print,'sig=1,2, 3 for uncertanity estiamte. 1-sigma, 2sigma'
     return,0
endif

;written by Adam Burgasser
;n=sample size
;c=number of what your looking for (ie, binaries, M7s with J-K>1)
;sig=1,2, 3 for uncertanity estiamte. 1-sigma, 2sigma.

nsamp = 1000
if (n gt 150) then begin
 print, 'sorry, too large of a sample, factorial blows up'
 return, 0
endif
if (c gt n) then begin
 print, 'sample size must be larger than selected number'
 return, 0
endif
if (n_elements(sig) eq 0) then sig=1

ebm = double(1)*c/n
eb = dindgen(nsamp+1)/(1.*nsamp)

p2 = eb*0.
for i=0,c do p2=p2+(factorial(n+1)/(factorial(i)*factorial(n+1-i)))*eb^i*(1.-eb)^(n+1-i)
tmp = p2-gauss_pdf(sig)
w = where(tmp le 0, cnt)
ebl = eb(w(0))
tmp = p2-1+gauss_pdf(sig)
w = where(tmp ge 0, cnt)
ebu = eb(w(cnt-1))

if (keyword_set(plot)) then begin
 p = (factorial(n+1)/(factorial(c)*factorial(n-c)))*eb^c*(1.-eb)^(n-c)
 plot, eb, p, xtitle='!4e!3!db!N', ytitle='P(!4e!3!db!N)'
 oplot, [0,0]+ebl, [-100,100], linestyle=2
 oplot, [0,0]+ebm, [-100,100]
 oplot, [0,0]+ebu, [-100,100], linestyle=2
 nchar = fix(alog10(nsamp))
 xyouts, ebl, median(p), strmid(strtrim(string(ebl),2),0,nchar+2), align=0.5
 xyouts, ebm, median(p), strmid(strtrim(string(ebm),2),0,nchar+2), align=0.5
 xyouts, ebu, median(p), strmid(strtrim(string(ebu),2),0,nchar+2), align=0.5
endif
 
return, [ebl,ebm,ebu]
end
