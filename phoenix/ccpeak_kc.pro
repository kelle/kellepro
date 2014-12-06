;+
; NAME:
;           CCPEAK
;
; PURPOSE:
;       Locates the precise location of the peak in the
;       cross-correlation function between two vectors.
;       (Locates LAG_max)
;
; CALLING SEQUENCE:
;
;       LAG_max = CCPEAK(VEC1, VEC2 [, RADIUS ])
;
; INPUTS:
;
;       VEC1, VEC2 - Functions to be cross-correlated

; OPTIONAL INPUTS:
;
;       RADIUS -  How many array elements around the 
;                 nominal peak where polynomial fit 
;                 should be performed.
;
; OUTPUTS:
;
;       LAG_max - Lag at which cross-correlation 
;                 function is maximized
;
; RESTRICTIONS:
;
;       Uses my POLYFIT procedure, not IDL's
;       POLY_FIT
;
;       Uses C_CORRELATE to perform cross-correlation
;
; MODIFICATION HISTORY:
; Written sometime in December 2002 by JohnJohn
; 07 June 2003 JJ - Fixed bug where CF was being indexed out of
; range. Also limited the minimum and maximum lag returned to be
; min(lag) and max(lag), respectively.
; 26 June 2003 JJ - Default radius is now 50 rather than 10
; Dec 2009 K.Cruz - added higher order fit capability
;					plot polynomial
; Feb 2010, K. Cruz - rebinned by factor of r_factor to get subpixel shift
;						made radius and srad an input
;-

function ccpeak_kc,arr1, arr2, radius, sradnr,ccf=cf, lag=lag,plot=plot
on_error,2		;Return to caller if an error occurs
@colors_kc

n = n_elements(arr1)
r_factor=10
arr1_r=REBIN(arr1,n*r_factor)
arr2_r=REBIN(arr2,n*r_factor)
if n_elements(radius) eq 0 then radius = 50
radius=radius*r_factor
if n_elements(sradnr) eq 0 then sradnr = 3
srad=sradnr*r_factor
lag = fillarr(1,-radius,radius)
cf = c_correlate(arr1_r,arr2_r,lag)
dum = max(cf, ind)
dum_max= lag[where(cf eq dum)]

xmin=dum_max-radius/3
xmax=dum_max+radius/3
if xmax gt max(lag) then begin
	xmax = max(lag)
	xmin = xmax-2*radius/3
endif

if keyword_set(plot)then begin
	plot,lag,cf,psym=1,xr=[xmin,xmax],$
		xstyle=1,xmargin=[4,2],ymargin=[2,2],symsize=0.3,/ynozero
endif

sublag = lag[(ind-srad) > 0:(ind+srad) < (2*radius)]
subcf  = cf[(ind-srad) > 0:(ind+srad) < (2*radius)]
order= 2
a = polyfit(sublag,subcf,order,yfit)
a_prime=[ a[1], 2*a[2]];, 3*a[3] ,4*a[4],5*a[5],6*a[6]  ]

roots=fz_roots(a_prime)
max=max(poly(roots,a)); find root that maximizes function
max_loc=where((poly(roots,a)) eq max)
maxlag=real_part(roots[max_loc])
;print, maxlag
nlag = n_elements(lag)
if maxlag lt lag[0] then maxlag = lag[0]
;print, maxlag
if maxlag gt lag[nlag-1] then maxlag = lag[nlag-1]

if keyword_set(plot)then begin
	oplot, sublag, yfit, thick=2, color=red
	oplot, [maxlag,maxlag],[0,1]
	oplot, [dum_max,dum_max],[0,1],linestyle=1
	xyouts, maxlag,dum-dum/2.5,strn(maxlag)
	xyouts, dum_max,dum-dum/2,strn(dum_max),align=1
endif

;print,strn(maxlag) +'   ' + strn(dum_max)
answer=[maxlag/r_factor, dum_max/r_factor]
;return, maxlag
return, answer
end
