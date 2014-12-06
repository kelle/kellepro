;+
; NAME:
;	ROUTINE_NAME
;
; PURPOSE:
;	Try to use the active, present tense. (Measure the foobar of...)
;
; CALLING SEQUENCE:
;	Write the calling sequence here. Put optional inputs in brackets.
;	For procedures, use the form:
;
;	ROUTINE_NAME, Parameter1, Foobar, [ Parm2, KEY1= , /KEY2 ]
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
;	Parm1:	Describe the positional input parameters here. 
;		Note data type expected. (String containing name of file...
;		Parameters use Initial Caps.
;
; OPTIONAL INPUTS:
;	Parm2:	Describe optional inputs here. If you don't have any, just
;		delete this section.
;	
; KEYWORD PARAMETERS:
;	KEY1:	Document keyword parameters like this. Note that the keyword
;		is shown in ALL CAPS!
;
;	KEY2:	Yet another keyword. 
;		is just a set or unset flag, say something like:
;		"If Set, foobar subfloatation is used. 
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
; FUTURE MODIFICATIONS:
;	List here anything that you want to add in the future.
;
; MODIFICATION HISTORY:
; 	Written by:	Your name here, Date.
;	July, 1994	Any additional mods get described here.  Remember to
;			change the stuff above if you add a new keyword or
;			something!
;-

PRO TEMPLATE, ps=ps,eps=eps

;if N_params() lt 6 then begin
;     print,'Syntax -  calling sequence'
;     print,'useful information'
;     return
;endif

  PRINT, "This is an example header file for documenting IDL routines"

;load lots of colors
@color_kc

if keyword_set(ps) then begin
    ext='.ps'
endif else begin
    if keyword_set(eps) then begin
        ps = 1
        ext='.eps'
    endif
endelse

IF KEYWORD_SET(ps) THEN begin
    device, Decomposed=0
    black=white    
ENDIF

IF KEYWORD_SET(ps) THEN BEGIN
    !p.font=0                   ;use postscript fonts
    set_plot, 'ps'
    @symbols_ps_kc ;load string symbols and greek letters for Postscript
    device, filename='template'+ext, encapsulated=eps, /helvetica,/isolatin1, landscape=0, color=1
    device, xsize=8.89, ysize=8.89 ;SQUARE one panel, one column figure square
                                ;device, xsize=18.6267, ysize=8.89
                                ;;RECTANGLE two column, two panel\
    cs=1 ;charcter size
ENDIF ELSE BEGIN
    device, Decomposed=0
    black=white
    @symbols_kc ;load string symbols and greek letters for Hershey
    cs=2 ;charcter size
ENDELSE

!p.thick=4
!x.thick=3
!y.thick=3

a=findgen(100)/10.
b=sin(a)

plot, a,b,xtitle='Wavelength' + ' ('+micron + ')',ytitle=Psi+'(x)',/nodata, $
      charsize=cs
oplot, a,b, color=red

test_string=Phi+Psi+Omega+'!C'+alpha+beta+delta+lambda+nu+pi+'!C'+$
            leq+geq+times+propto+approx+degr+pm+oplus

xyouts, 0.5,-0.5, test_string, color=blue,charsize=cs

IF KEYWORD_SET(ps) THEN BEGIN
 device,/close
 set_plot,'x'
 !p.font=-1 ;go back to using Hershey fonts
ENDIF

loadct,0 ;go back to greyscale color table

END

