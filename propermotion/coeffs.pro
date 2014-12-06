;+
; NAME:
;	COEFFS
;
; PURPOSE:
;	Measure proper motions of stars given two images with several 
;	reference stars.
;
; CATEGORY:
;	Put a category (or categories) here.  For example:
;	Widgets.
;
; CALLING SEQUENCE:
;	Write the calling sequence here. Include only positional parameters
;	(i.e., NO KEYWORDS). For procedures, use the form:
;
;	ROUTINE_NAME, Parameter1, Parameter2, Foobar
;
;	Note that the routine name is ALL CAPS and arguments have Initial
;	Caps.  For functions, use the form:
; 
;	Result = FUNCTION_NAME(Parameter1, Parameter2, Foobar)
;
;	Always use the "Result = " part to begin. This makes it super-obvious
;	to the user that this routine is a function!
;
; INPUTS:
;	Parm1:	Describe the positional input parameters here. Note again
;		that positional parameters are shown with Initial Caps.
;
; OPTIONAL INPUTS:
;	Parm2:	Describe optional inputs here. If you don't have any, just
;		delete this section.
;	
; KEYWORD PARAMETERS:
;	KEY1:	Document keyword parameters like this. Note that the keyword
;		is shown in ALL CAPS!
;
;	KEY2:	Yet another keyword. Try to use the active, present tense
;		when describing your keywords.  For example, if this keyword
;		is just a set or unset flag, say something like:
;		"Set this keyword to use foobar subfloatation. The default
;		 is foobar superfloatation."
;
; OUTPUTS:
;	Describe any outputs here.  For example, "This function returns the
;	foobar superflimpt version of the input array."  This is where you
;	should also document the return value for functions.
;
; OPTIONAL OUTPUTS:
;	Describe optional outputs here.  If the routine doesn't have any, 
;	just delete this section.
;
; COMMON BLOCKS:
;	BLOCK1:	Describe any common blocks here. If there are no COMMON
;		blocks, just delete this entry.
;
; SIDE EFFECTS:
;	Describe "side effects" here.  There aren't any?  Well, just delete
;	this entry.
;
; RESTRICTIONS:
;	Describe any "restrictions" here.  Delete this section if there are
;	no important restrictions.
;
; PROCEDURE:
;	You can describe the foobar superfloatation method being used here.
;	You might not need this section for your routine.
;
; ROUTINES CALLED:
;	List all the routines and functions that your routine calls.
;
; EXAMPLE:
;	Please provide a simple example here. An example from the PICKFILE
;	documentation is shown below. Please try to include examples that
;       do not rely on variables or data files that are not defined in
;       the example code. Your example should execute properly if typed
;       in at the IDL command line with no other preparation.
;
;	Create a PICKFILE widget that lets users select only files with 
;	the extensions 'pro' and 'dat'.  Use the 'Select File to Read' title 
;	and store the name of the selected file in the variable F.  Enter:
;
;		F = PICKFILE(/READ, FILTER = ['pro', 'dat'])
;
; MODIFICATION HISTORY:
; 	Written by:	Your name here, Date.
;	July, 1994	Any additional mods get described here.  Remember to
;			change the stuff above if you add a new keyword or
;			something!
;       v2. got rid of unecessary pass of ra/dec_target and x/y_target
;	    got rid of stddev calculations for matrix
;-

FUNCTION COEFFS, ra_ref, dec_ref, x_ref, y_ref, obj, run, log=log_set, view=view_set

;if N_params() lt 6 then begin
;     print,'Syntax -  calling sequence'
;     print,'useful information'
;     return
;endif


n_ref_stars=n_elements(ra_ref)
n_combos=factorial(n_ref_stars)/factorial(3)/factorial(n_ref_stars-3)

;only cut datapoints if more than 3 ref stars
if n_ref_stars gt 3 THEN cut=1 else cut=0

;IF n_ref ne 5 then BEGIN 
; print, 'no good, only works with 5 now' 
; RETURN, 0
;endif

A=dblarr(n_combos)
B=A
C=A
D=A
E=A
F=A
ra_check=dblarr(n_ref_stars,n_combos)
dec_check=ra_check
ra_risiduals=ra_check
dec_risiduals=ra_check
combo=0

FOR i=0,n_ref_stars-3 do begin
 FOR j=1,n_ref_stars-2 do begin
  FOR k=2,n_ref_stars-1 do begin

   IF (i lt j) and (j lt k) THEN BEGIN
;	print,'COMBO', combo,':', i,j,k
	ra1=ra_ref[i] 
	ra2=ra_ref[j]
	ra3=ra_ref[k]

	dec1=dec_ref[i]  
	dec2=dec_ref[j]
	dec3=dec_ref[k]

	x1=x_ref[i]
	x2=x_ref[j]
	x3=x_ref[k]

	y1=y_ref[i]
	y2=y_ref[j]
	y3=y_ref[k]   

	A[combo]=((ra1-ra3)/(x1-x3) + (y3-y1)*(ra2-ra3)/(x1-x3)/(y2-y3))/(1-(y3-y1)*(x3-x2)/(x1-x3)/(y2-y3))

	B[combo]=(ra2-ra3 + A[combo]*(x3-x2))/(y2-y3)

	E[combo]=ra3-A[combo]*x3-B[combo]*y3

	C[combo]=((dec1-dec3)/(x1-x3) + (y3-y1)*(dec2-dec3)/(x1-x3)/(y2-y3))/(1-(y3-y1)*(x3-x2)/(x1-x3)/(y2-y3))

	D[combo]=(dec2-dec3 + C[combo]*(x3-x2))/(y2-y3)

	F[combo]=dec3-C[combo]*x3-D[combo]*y3

	ra_check[*,combo] =  A[combo]*x_ref + B[combo]*y_ref + E[combo]
	dec_check[*,combo] = C[combo]*x_ref + D[combo]*y_ref + F[combo]

	ra_risiduals[*,combo] = ra_ref[*]-ra_check[*,combo]
	dec_risiduals[*,combo]= dec_ref[*]-dec_check[*,combo]

	combo=combo+1
   ENDIF

  ENDFOR
 ENDFOR
ENDFOR

A_mean=mean(A,/NAN)
B_mean=mean(B,/NAN)
C_mean=mean(C,/NAN)
D_mean=mean(D,/NAN)
E_mean=mean(E,/NAN)
F_mean=mean(F,/NAN)

sig=1.

if cut eq 1 then begin

 A_std=STDDEV(A,/double,/NAN)
 A_cut=where(abs(A-A_mean) gt sig*A_std)
 A_keep=where(abs(A-A_mean) le sig*A_std)
 if A_cut[0] ne -1 then A_dirty=A[A_cut] else cut=0
 A_clean=A[A_keep]

 B_std=STDDEV(B,/double,/nan)
 B_cut=where(abs(B-B_mean) gt sig*B_std)
 B_keep=where(abs(B-B_mean) le sig*B_std)
 if b_cut[0]  ne -1 then B_dirty=B[B_cut] else cut=0
 B_clean=B[B_keep]

 C_std=STDDEV(C,/double,/nan)
 C_cut=where(abs(C-C_mean) gt sig*C_std)
 C_keep=where(abs(C-C_mean) le sig*C_std)
 if c_cut[0] ne -1 then C_dirty=C[C_cut] else cut=0
 C_clean=C[C_keep]

 D_std=STDDEV(D,/double,/nan)
 D_cut=where(abs(D-D_mean) gt sig*D_std)
 D_keep=where(abs(D-D_mean) le sig*D_std)
 if d_cut[0] ne -1 then D_dirty=D[D_cut] else cut=0
 D_clean=D[D_keep]

 E_std=STDDEV(E,/double,/nan)
 E_cut=where(abs(E-E_mean) gt sig*E_std)
 E_keep=where(abs(E-E_mean) le sig*E_std)
 if e_cut[0] ne -1 then E_dirty=E[E_cut] else cut=0
 E_clean=E[E_keep]

 F_std=STDDEV(F,/double,/nan)
 F_cut=where(abs(F-F_mean) gt sig*F_std)
 F_keep=where(abs(F-F_mean) le sig*F_std)
 if f_cut[0] ne -1 then  F_dirty=F[F_cut] else cut=0
 F_clean=F[F_keep]

 A_mean_clean=mean(A_clean)
 B_mean_clean=mean(B_clean)
 C_mean_clean=mean(C_clean)
 D_mean_clean=mean(D_clean)
 E_mean_clean=mean(E_clean) 
 F_mean_clean=mean(F_clean)

ENDIF ELSE BEGIN

 A_mean_clean=A_mean
 B_mean_clean=B_mean
 C_mean_clean=C_mean
 D_mean_clean=D_mean
 E_mean_clean=E_mean
 F_mean_clean=F_mean

ENDELSE

;Find Covariance terms
;sig_ab=1./n_combos*total((A_clean-A_mean_clean)*(B_clean-B_mean_clean))
;sig_ae=1./n_combos*total((A_clean-A_mean_clean)*(E_clean-E_mean_clean))
;sig_be=1./n_combos*total((B_clean-B_mean_clean)*(E_clean-E_mean_clean))

;A_std_clean=STDDEV(A_clean,/double,/NAN)
;B_std_clean=STDDEV(B_clean,/double,/NAN)
;C_std_clean=STDDEV(C_clean,/double,/NAN)
;D_std_clean=STDDEV(D_clean,/double,/NAN)
;E_std_clean=STDDEV(E_clean,/double,/NAN)
;F_std_clean=STDDEV(F_clean,/double,/NAN)

if n_ref_stars gt 10 then begin
  ny=5
ENDIF else begin
  ny=ceil(n_ref_stars/2.)
ENDELSE

IF keyword_set(log_set) THEN BEGIN
 
 x=indgen(n_combos)
 xr=[-0.5, (n_combos-1)+0.5]
 
 coeffs_file='logs/'+obj+'_coeffs_'+run+'.ps'
 set_plot,'ps'
 device, filename=coeffs_file, xsize=7.0, ysize=10.0,/inches,/portrait, yoffset=0.75
 !p.multi=[0,2,3]
 !Y.OMargin=[1,2]

 plot, x, A, psym=1, xr=xr, xstyle=1, /ynozero, xtitle='combo', ytitle='A'
 oplot, [xr[0],xr[1]],[A_mean,A_mean],linestyle=2
 oplot, [xr[0],xr[1]],[A_mean_clean,A_mean_clean],linestyle=0
 if cut eq 1 then oplot, A_cut, A_dirty, psym=7, thick=2

 plot, x, B, psym=1, xr=xr, xstyle=1, /ynozero, xtitle='combo', ytitle='B'
 oplot, [xr[0],xr[1]],[B_mean,B_mean],linestyle=2
 oplot, [xr[0],xr[1]],[B_mean_clean,B_mean_clean],linestyle=0
 if cut eq 1 then oplot, B_cut, B_dirty, psym=7, thick=2

 plot, x, C, psym=1, xr=xr, xstyle=1, /ynozero, xtitle='combo', ytitle='C'
 oplot, [xr[0],xr[1]],[C_mean,C_mean],linestyle=2
 oplot, [xr[0],xr[1]],[C_mean_clean,C_mean_clean],linestyle=0
 if cut eq 1 then oplot, C_cut, C_dirty, psym=7, thick=2

 plot, x, D, psym=1, xr=xr, xstyle=1, /ynozero, xtitle='combo', ytitle='D'
 oplot, [xr[0],xr[1]],[D_mean,D_mean],linestyle=2
 oplot, [xr[0],xr[1]],[D_mean_clean,D_mean_clean],linestyle=0
 if cut eq 1 then oplot, D_cut, D_dirty, psym=7, thick=2

 plot, x, E, psym=1, xr=xr, xstyle=1, /ynozero, xtitle='combo', ytitle='E'
 oplot, [xr[0],xr[1]],[E_mean,E_mean],linestyle=2
 oplot, [xr[0],xr[1]],[E_mean_clean,E_mean_clean],linestyle=0
 if cut eq 1 then oplot, E_cut, E_dirty, psym=7, thick=2

 plot, x, F, psym=1, xr=xr, xstyle=1, /ynozero, xtitle='combo', ytitle='F'
 oplot, [xr[0],xr[1]],[F_mean,F_mean],linestyle=2
 oplot, [xr[0],xr[1]],[F_mean_clean,F_mean_clean],linestyle=0
 if cut eq 1 then oplot, F_cut, F_dirty, psym=7, thick=2

 xyouts, 0.5,0.98, 'Coefficient Risiduals',/NORMAL, charsize=1.5, alignment=0.5
 
 device,/close
  
 ra_file='logs/'+obj+'_ra_residuals_'+run+'.ps'
 device, filename=ra_file, xsize=7.0, ysize=10.0,/inches,/portrait, yoffset=0.75
 !p.multi=[0,2,ny]
 !Y.OMargin=[1,2]
 FOR l=0, n_ref_stars-1 DO BEGIN
   if l eq 10 then !p.multi=[0,2,ny]
   plot, x, ra_risiduals[l,*], xr=xr, xstyle=1, /ynozero, psym=1, $
   title='Star # '+strn(l+1), xtitle='combo', ytitle='RA'
   oplot, [xr[0],xr[1]],[0,0]
   if (l eq 9) and (n_ref_stars gt 10) then xyouts, 0.5,0.975, 'RA Risiduals',/NORMAL, charsize=1.5, alignment=0.5
 ENDFOR
 xyouts, 0.5,0.975, 'RA Risiduals',/NORMAL, charsize=1.5, alignment=0.5
 device, /close

 dec_file='logs/'+obj+'_dec_residuals_'+run+'.ps'
 device, filename=dec_file, xsize=7.0, ysize=10.0,/inches,/portrait, yoffset=0.75
 !p.multi=[0,2,ny]
 !Y.OMargin=[1,2]
 FOR l=0, n_ref_stars-1 DO BEGIN
   if l eq 10 then !p.multi=[0,2,ny]
   plot, x, dec_risiduals[l,*], xr=xr, xstyle=1, /ynozero, psym=1, $
   title='Star # '+strn(l+1), xtitle='combo', ytitle='Dec'
   oplot, [xr[0],xr[1]],[0,0] 
   if (l eq 9) and (n_ref_stars gt 10) then xyouts, 0.5,0.975, 'Dec Risiduals',/NORMAL, charsize=1.5, alignment=0.5
 ENDFOR
 xyouts, 0.5,0.975, 'Dec Risiduals',/NORMAL, charsize=1.5, alignment=0.5
 device, /close

 set_plot,'x'
 !p.multi=0

ENDIF

IF keyword_set(view_set) THEN BEGIN

 x=indgen(n_combos)
 xr=[-0.5, (n_combos-1)+0.5]
 
 set_plot,'x'
 window, 0, xsize=512, ysize=800
 !p.multi=[0,2,3]
 !Y.OMargin=[1,2]

 plot, x, A, psym=1, xr=xr, xstyle=1, /ynozero, title='A', charsize=2
 oplot, [xr[0],xr[1]],[A_mean,A_mean],linestyle=2
 oplot, [xr[0],xr[1]],[A_mean_clean,A_mean_clean],linestyle=0
 if cut eq 1 then oplot, A_cut, A_dirty, psym=7, thick=2

 plot, x, B, psym=1, xr=xr, xstyle=1, /ynozero, title='B', charsize=2
 oplot, [xr[0],xr[1]],[B_mean,B_mean],linestyle=2
 oplot, [xr[0],xr[1]],[B_mean_clean,B_mean_clean],linestyle=0
 if cut eq 1 then oplot, B_cut, B_dirty, psym=7, thick=2

 plot, x, C, psym=1, xr=xr, xstyle=1, /ynozero, title='C', charsize=2
 oplot, [xr[0],xr[1]],[C_mean,C_mean],linestyle=2
 oplot, [xr[0],xr[1]],[C_mean_clean,C_mean_clean],linestyle=0
 if cut eq 1 then oplot, C_cut, C_dirty, psym=7, thick=2

 plot, x, D, psym=1, xr=xr, xstyle=1, /ynozero, title='D', charsize=2
 oplot, [xr[0],xr[1]],[D_mean,D_mean],linestyle=2
 oplot, [xr[0],xr[1]],[D_mean_clean,D_mean_clean],linestyle=0
 if cut eq 1 then oplot, D_cut, D_dirty, psym=7, thick=2

 plot, x, E, psym=1, xr=xr, xstyle=1, /ynozero, title='E', charsize=2
 oplot, [xr[0],xr[1]],[E_mean,E_mean],linestyle=2
 oplot, [xr[0],xr[1]],[E_mean_clean,E_mean_clean],linestyle=0
 if cut eq 1 then oplot, E_cut, E_dirty, psym=7, thick=2

 plot, x, F, psym=1, xr=xr, xstyle=1, /ynozero, title='F', charsize=2
 oplot, [xr[0],xr[1]],[F_mean,F_mean],linestyle=2
 oplot, [xr[0],xr[1]],[F_mean_clean,F_mean_clean],linestyle=0
 if cut eq 1 then oplot, F_cut, F_dirty, psym=7, thick=2

 xyouts, 0.5,0.98, 'Coefficient Risiduals',/NORMAL, charsize=1.5, alignment=0.5

 print, ''
 Print,'Press any key to continue'
 print, ' '
 anykey=get_kbrd(1)

 !p.multi=[0,2,ny]
 !Y.OMargin=[1,2]

 FOR l=0, n_ref_stars-1 DO BEGIN
   if l eq 10 then !p.multi=[0,2,ny]
   plot, x, ra_risiduals[l,*], xr=xr, xstyle=1, /ynozero, psym=1, $
   title='Star # '+strtrim(string(l),2), charsize=1.5
   oplot, [xr[0],xr[1]],[0,0]
   if (l eq 9) and (n_ref_stars gt 10) then BEGIN
    xyouts, 0.5,0.975, 'RA Risiduals',/NORMAL, charsize=1.5, alignment=0.5
    print, '' & Print,'Press any key to continue' & print, ' ' & anykey=get_kbrd(1)
   ENDIF
 ENDFOR

 xyouts, 0.5,0.975, 'RA Risiduals',/NORMAL, charsize=1.5, alignment=0.5

 print, ''
 Print,'Press any key to continue'
 print, ' '
 anykey=get_kbrd(1)

 !p.multi=[0,2,ny]
 !Y.OMargin=[1,2]

 FOR l=0, n_ref_stars-1 DO BEGIN
   if l eq 10 then !p.multi=[0,2,ny]
   plot, x, dec_risiduals[l,*], xr=xr, xstyle=1, /ynozero, psym=1, $
   title='Star # '+strtrim(string(l),2), charsize=1.5
   oplot, [xr[0],xr[1]],[0,0] 
   IF (l eq 9) and (n_ref_stars gt 10) then BEGIN
    xyouts, 0.5,0.975, 'Dec Risiduals',/NORMAL, charsize=1.5, alignment=0.5
    print, '' & Print,'Press any key to continue' & print, ' ' & anykey=get_kbrd(1)
   ENDIF
 ENDFOR
 
 xyouts, 0.5,0.975, 'Dec Risiduals',/NORMAL, charsize=1.5, alignment=0.5
 
 print, ''
 Print,'Press any key to continue'
 print, ' '
 anykey=get_kbrd(1)

 !p.multi=0

ENDIF

coeffs=dblarr(3,6)
coeffs[0,*]=[A_mean_clean,B_mean_clean,C_mean_clean,D_mean_clean,E_mean_clean,F_mean_clean]
;coeffs[1,*]=[A_std_clean,B_std_clean,C_std_clean,D_std_clean,E_std_clean,F_std_clean]
;coeffs[2,0:2]=[sig_ab,sig_ae,sig_be]

RETURN, coeffs
END




