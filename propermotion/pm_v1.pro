;+
; NAME:
;	pm
;
; PURPOSE:
;	Measures the proper motion of a star given pixel 
;	coords for the first epoch and ra,dec coords for the second epoch.
;	Designed using POSS II and 2MASS images.
;
; CALLING SEQUENCE:
;
;	PM, Coords_file, POSS_fits_image, 2M_fits_image, [ TVmag= , /LOG]
;
; INPUTS:
;	Coords_file: String containing the name of the text file with list 
;		     of ra,dec positions at the second
;		     epoch for many stars in the field. The first coordinate
;		     in the list is assumed to be that of the proper motion
;		     star.
;
;	POSS_fits_image:  String containing the name of the FITS image of the 
;		     field at first epoch (POSS II).
;
;	2M_fits_image:  String containing the name of the FITS image of the 
;		     field at second epoch (2MASS).
;		     
; OPTIONAL KEYWORD PARAMETERS:
;       TVmag: Default is 2.
;
;	/LOG:	  If set, postscript and text files created.
;
; OUTPUTS:
;	This procedure returns to the screen the proper motion. Set LOG 
;	keyword to write results to text file.
;
; OPTIONAL OUTPUTS:
;	If LOG keyword is set, then results will be written to a text file
;	and three postscript files will be created with solution risiduals.
;
; RESTRICTIONS:
;	Expects Coords_file to be formatted in a specific way.
;       no NULLS! 
;
; PROCEDURE:
;	You can describe the foobar superfloatation method being used here.
;	You might not need this section for your routine.
;
; ROUTINES CALLED:
;	COEFFS, GET_FILE
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
;	Add some error estimation
;	Calculate position angle
;	ADD 2MASS DESIGNATION or coords (at least) to log
;	get epoch of POSS I automatically
;       write final image and solution iamge to ps
;
; MODIFICATION HISTORY:
; 	Written by:	Kelle Cruz, September 2004
;-

PRO pm_v1;, coords_file, POSS_image_file, image_file_2M, tvmag=tvmag, log=log_set

coords_file=''
POSS_image_file=''
image_file_2M=''
obj=''
ans=''
use_file=0

PRINT, ''
READ, ans, PROMPT='Create log files? (y/n): '
IF ans eq 'n' then log_set=0 ELSE log_set=1 ; Default is to create logs

PRINT, ''
READ, obj, PROMPT='Enter name of pm target: '

PRINT, '' & PRINT, 'COORDS FILE:'
coords_file=GET_FILE(obj,'ref_lis/')
PRINT, '' & PRINT, 'POSS IMAGE FILE:'
POSS_image_file=GET_FILE(obj,'fits/POSS/')
PRINT, '' & PRINT, '2MASS IMAGE FILE:'
image_file_2M=GET_FILE(obj,'fits/2M/')

WHILE NOT file_test(coords_file) DO BEGIN 
 PRINT, coords_file+': Coords file does not exist, try again.'
 READ, coords_file, PROMPT='Enter path of coords file: '
ENDWHILE
WHILE NOT file_test(POSS_image_file) DO BEGIN
 PRINT, POSS_image_file+': POSS image does not exist, try again.'
 READ, POSS_image_file, PROMPT='Enter path of POSS image file: '
ENDWHILE
WHILE NOT file_test(image_file_2M) DO BEGIN 
 PRINT, image_file_2M+': 2MASS image does not exist, try again.'
 READ, image_file_2M, PROMPT='Enter path of 2MASS image file: '
ENDWHILE

;if N_params() lt 2 then begin
;     print,"Syntax -  pm, 'coords_file, 'POSS_image_file', '2M_image_file'"
;     print,'     coords_file: file with coords for 2MASS reference stars '
;     print,'     POSS_image_file: fits file of 2MASS image'
;     print,'     2M'
;     print,'     tvmag=
;     print,'     /log: create plots of residuals and a log file'
;     return
;endif

if not keyword_set(tvmag) then tvmag = 1

poss_image=readfits(poss_image_file,hdr_poss)
date_xy_head=sxpar(hdr_poss,'DATE-OBS')

lines=''

;EXPECT COORD FILE TO HAVE 12 COLUMNS - no nulls!
line=dblarr(12)

answer=''
date_xy=fltarr(3)

;max # of ref stars is num of lines in file
max_ref=file_lines(coords_file)
ra_ref=dblarr(max_ref)
dec_ref=dblarr(max_ref)
x_ref=dblarr(max_ref)
y_ref=dblarr(max_ref)
xy_tmp=dblarr(2)

;setup screen for display
DEVICE, Decomposed=0
;get color table GRN/WHT LINEAR and RAINBOW
loadct, 8, ncolors=240, bottom=0
loadct, 13, ncolors=255-240, bottom=240
top=239
black = 240
purple = 242
blue= 243
cyan= 246
yellow = 251
orange = 252
red=254
white=255

openr, lun_coords, coords_file, /GET_LUN

; GET THROUGH HEADER, IF ANY
READF, lun_coords, lines
char1=strmid(lines,0,1)
IF char1 eq '\' OR char1 eq '|' THEN BEGIN
 WHILE char1 eq '\' OR char1 eq '|' DO BEGIN
  POINT_LUN,-lun_coords, pos
  READF, lun_coords, lines
  char1=strmid(lines,0,1)
 ENDWHILE
 POINT_LUN, lun_coords, pos
 READF, lun_coords, line
ENDIF ELSE BEGIN
 point_lun,lun_coords,0 ;go back to beginning of file
 READF, lun_coords, line
ENDELSE

; ASSUME FIRST OBJECT iS PM TARGET
;obj_d = line[5] & obj='U'+strtrim(string(long(obj_d)),2)
ra_target = line[6]
dec_target = line[7]
j_mag = line[9]
date_target=line[11]
;print, 'PM TARGET: ',ra_target,dec_target,j_mag

;DISPLAY FITS IMAGE and MARK PM target

;READ 2MASS IMAGE AND FIX NANS
image=readfits(image_file_2M,hdr)
med_image=median(image)
w = where(finite(image,/nan) OR image eq 0,count)
if count gt 0 then image[w] = med_image
image=sigrange(image) ;pick out most significant data range (enhance contrast)

size1=size(image)
nx1=size1[1]
ny1=size1[2]

;CROP, IF NECESSARY
im_length=600 ;should be even
IF ny1 gt im_length THEN BEGIN
 adxy, hdr, ra_target, dec_target, x_target_2M, y_target_2M
 IF (y_target_2M gt im_length/2) AND (ny1-y_target_2M gt im_length/2) THEN BEGIN
     y1=y_target_2M[0]-(im_length/2-1)
     y2=y_target_2M[0]+im_length/2
 ENDIF 
 IF (y_target_2M le im_length/2) THEN BEGIN
     y1=1
     y2=im_length
 ENDIF 
 IF (ny1-y_target_2M le im_length/2) THEN BEGIN
     y1=ny1-im_length
     y2=ny1-1
 ENDIF
 HEXTRACT, image,hdr,0,nx1-1,y1,y2,/silent
ENDIF

size=size(image)
nx=size[1]
ny=size[2]
tk=nx*tvmag/50

;DISPLAY PM TARGET
;WDELETE ; closes any open windows
window, 0,xsize=nx*tvmag, ysize=ny*tvmag
wshow, 0 ;unhide window, if hidden
tvscl, rebin(image,nx*tvmag,ny*tvmag,/s),top=top  
adxy, hdr, ra_target, dec_target, x_display_pm, y_display_pm
tvcircle, tk, x_display_pm*tvmag, y_display_pm*tvmag, red, thick=2
;;plots, x_display_pm*tvmag-tk, y_display_pm*tvmag-tk, /device
;plots, x_display_pm*tvmag+tk, y_display_pm*tvmag+tk, /device,/continue, thick=2, color=red
;;plots, x_display_pm*tvmag-tk, y_display_pm*tvmag+tk, /device
;plots, x_display_pm*tvmag+tk, y_display_pm*tvmag-tk, /device,/continue, thick=2, color=red


;GET POSS INFO AND PIXEL COORD OF PM TARGET
PRINT, 'Date of POSS image from header: '  + date_xy_head
READ, date_xy, PROMPT='Enter date of xy coords for PM target as listed above (yyyy,mm,dd): '
READ, xy_tmp, PROMPT='Enter x and y coords of PM target: '
x_target=xy_tmp[0]
y_target=xy_tmp[1]

;READ REST OF COORD FILE AND IDENTIFY REFERENCE STARS
stop=0
i=0    ; number of ref stars
WHILE (stop eq 0) AND NOT EOF(lun_coords) DO BEGIN

  READF, lun_coords, line
  ra =    line[6]
  dec =   line[7]
  j_mag = line[9]
;  print, ra, dec, j_mag

  ;DETERMINE IF REF STAR IS VISIBLE  
  adxy, hdr, ra, dec, x_display, y_display
  IF x_display ge 1 AND x_display le nx AND $
     y_display ge 1 AND y_display le ny THEN BEGIN
        ask=1
        print, ra[0], dec[0], j_mag[0] 
  ENDIF ELSE ask=0

  ;MARK CANDIDATE REFERENCE STAR AND PM STAR 
  tvscl, rebin(image,nx*tvmag,ny*tvmag,/s),top=top
  
  tvcircle, tk, x_display_pm*tvmag, y_display_pm*tvmag, red, thick=2
;  plots, x_display_pm*tvmag-tk, y_display_pm*tvmag-tk, /device
 ; plots, x_display_pm*tvmag+tk, y_display_pm*tvmag+tk, /device,/continue, thick=2, color=red
 ; plots, x_display_pm*tvmag-tk, y_display_pm*tvmag+tk, /device
 ; plots, x_display_pm*tvmag+tk, y_display_pm*tvmag-tk, /device,/continue, thick=2, color=red

  tvcircle, tk, x_display*tvmag, y_display*tvmag, cyan, thick=2
;  plots, x_display*tvmag-tk, y_display*tvmag-tk, /device
;  plots, x_display*tvmag+tk, y_display*tvmag+tk, /device,/continue, thick=2, color=purple
;  plots, x_display*tvmag-tk, y_display*tvmag+tk, /device
;  plots, x_display*tvmag+tk, y_display*tvmag-tk, /device,/continue, thick=2, color=purple

  ;IF REF STAR VISIBLE, Query user if to use object as reference

  WHILE ask eq 1 DO BEGIN

	READ, answer, PROMPT='Use this object as reference or quit? (y/n/q): '

	IF answer eq 'y' THEN BEGIN
	  READ, xy_tmp, PROMPT='Enter x and y coords: '
	  x_ref[i]=xy_tmp[0]
	  y_ref[i]=xy_tmp[1]
	  ra_ref[i]=ra
	  dec_ref[i]=dec
	  i=i+1
	  PRINT, 'You have chosen '+strtrim(string(i),2)+' ref stars.' & Print, ''
	  ask=0
	ENDIF ELSE IF answer eq 'n' THEN BEGIN
	  ask=0
	ENDIF ELSE IF answer eq 'q' THEN BEGIN
	  stop=1
	  ask=0
	ENDIF ELSE BEGIN
	  PRINT, "Please answer 'y', 'n', or 'q'"
	  ask = 1 
	ENDELSE

   ENDWHILE

ENDWHILE
close, lun_coords
FREE_LUN, lun_coords

IF i lt 3 THEN BEGIN
 PRINT, '' & PRINT, 'Not enough reference stars were chosen, exiting...'  & Print, ''
 return
ENDIF

ra_ref=ra_ref[0:i-1]
dec_ref=dec_ref[0:i-1]
x_ref=x_ref[0:i-1]
y_ref=y_ref[0:i-1]

n_ref_stars=n_elements(ra_ref)

;Use COEFFS function to solve for transformation from pixel coord at first epoch 
;to ra,dec coord at second epoch.
coeffs1=coeffs(ra_target, dec_target ,x_target, y_target, ra_ref, dec_ref, x_ref, y_ref, obj, log=log_set)

A=coeffs1[0]
B=coeffs1[1]
C=coeffs1[2]
D=coeffs1[3]
E=coeffs1[4]
F=coeffs1[5]

;SOLVE FOR RA,DEC COORDS IN SECOND EPOCH COORD SYSTEM BUT AT FIRST EPOCH POSITION
ra_display =  A*x_ref + B*y_ref + E
dec_display = C*x_ref + D*y_ref + F

ra_target_poss = A*x_target + B*y_target + E
dec_target_poss = C*x_target + D*y_target + F

;DISPLAY REF STARS USING SOLUTION
tvscl, rebin(image,nx*tvmag,ny*tvmag,/s),top=top
FOR m=0,n_ref_stars-1 DO BEGIN 
 adxy,hdr,ra_display[m], dec_display[m], x_display, y_display 
 tvcircle, tk, x_display*tvmag, y_display*tvmag, orange, thick=2 
; plots, x_display*tvmag-tk, y_display*tvmag-tk, /device  
; plots, x_display*tvmag+tk, y_display*tvmag+tk, /device,/continue,thick=2, color=orange 
; plots, x_display*tvmag-tk, y_display*tvmag+tk, /device 
; plots, x_display*tvmag+tk, y_display*tvmag-tk, /device,/continue,thick=2, color=orange
ENDFOR

print, ''
Print,'Press any key to continue'
print, ' '
anykey=get_kbrd(1)


;CALCULATE PROPER MOTION
total_mu_ra = ra_target - ra_target_poss
total_mu_dec = dec_target - dec_target_poss

mu_ra_s=total_mu_ra*60*60
mu_dec_s=total_mu_dec*60*60

;convert 2MASS obs date to reduced Julian Date
date_target=date_target-2400000

juldate, date_xy, jul_xy  ;FIRST EPOCH
day_diff=date_target-jul_xy
year_diff=day_diff/365.25

mu_ra=mu_ra_s/year_diff
mu_dec=mu_dec_s/year_diff
total_mu=sqrt( mu_ra^2 +  mu_dec^2)

s_mura = fix(mu_ra/abs(mu_ra))
s_mudec = fix(mu_dec/abs(mu_dec))

year_diff_s=strmid(strn(round(year_diff*100)/100.),0,5) ; to 2 decimal places
mu_ra_s=strmid(strn(round(mu_ra*1000)/1000.),0,6)
mu_dec_s=strmid(strn(round(mu_dec*1000)/1000.),0,6)
total_mu_s=strmid(strn(round(total_mu*1000)/1000.),0,6)

print, 'baseline (yrs) = '+year_diff_s
print, 'mu_alpha (arcsec/yr) = '+mu_ra_s 
print, 'mu_dec (arcsec/yr) = '+mu_dec_s
print, 'total pm (arcsec/yr) = ' +total_mu_s

IF year_diff lt 1. then print, 'WARNING: BASELINE VERY SHORT'

;WORK WITH DATES

;EPOCH 1950
date_1950=[1950,1,0,0,0]
juldate,[date_1950[0],1,1],jdate_1950

;EPOCH 1980
date_1980=[1980,1,0,0,0]
juldate,[date_1980[0],1,1],jdate_1980

;FIRST EPOCH (I-band circa 1990)
e1_day=ymd2dn(date_xy[0],date_xy[1],date_xy[2])
date_first=[date_xy[0],e1_day,0,0,0]
;jdate_e1=jul_xy

;SECOND EPOCH (2MASS circa 1999)
daycnv,date_target+2400000, e2_yr, e2_mn, e2_mnday
e2_day=ymd2dn(e2_yr,e2_mn,e2_mnday)
date_second=[e2_yr,e2_day,0,0,0]
;jdate_e2=date_target

;EPOCH J2000
jdate_j2000=51545.0

;SPITZER EPOCH
date_spitzer=[2005,1,0,0,0]
juldate,[date_spitzer[0],1,1],jdate_spitzer

yr_diff=(date_target-[jdate_1950,jdate_1980,jdate_j2000,jdate_spitzer])/365.25
ra_offset=(mu_ra[0]*yr_diff)/60/60;cos(dec_target[0]*!DTOR)
dec_offset=(mu_dec[0]*yr_diff)/60/60

ra_epoch=ra_target[0]-ra_offset
dec_epoch=dec_target[0]-dec_offset

;radec, ra_epoch[2], dec_epoch[2], ra_h_spit, ra_m_spit, ra_s_spit, dec_d_spit,dec_m_spit, dec_s_spit
radec, ra_target[0], dec_target[0], ra_h_targ, ra_m_targ, ra_s_targ, dec_d_targ,dec_m_targ, dec_s_targ
ra_s_targ=round(ra_s_targ*100)/100.
dec_s_targ=round(dec_s_targ*100)/100.
radec, ra_epoch[2], dec_epoch[2], ra_h_j2000, ra_m_j2000, ra_s_j2000, dec_d_j2000,dec_m_j2000, dec_s_j2000
ra_s_j2000=round(ra_s_j2000*100)/100.
dec_s_j2000=round(dec_s_j2000*100)/100.

;DISPLAY PM STAR AT DIFFERENT EPOCHS
if total_mu gt 0.5 then zoom=2 else zoom=4

nx_z=nx/zoom
ny_z=ny/zoom

adxy, hdr, ra_target, dec_target, x_target_2M, y_target_2M
xz1 = x_target_2M[0] - nx_z/2
xz2 = x_target_2M[0] + nx_z/2
yz1 = y_target_2M[0] - ny_z/2
yz2 = y_target_2M[0] + ny_z/2

;print, xz1, xz2, yz1, yz2

CASE 1 OF
 (xz1 lt 0)  : BEGIN & xz1 = 0 & xz2 = nx_z & END
 (xz2 gt nx) : BEGIN & xz1 = nx - nx_z & xz2 = nx-1 & END
 ELSE        : BEGIN & xz1 = xz1 & xz2 = xz2 & END
ENDCASE
CASE 1 OF
 (yz1 lt 0)  : BEGIN & yz1 = 0 & yz2 = ny_z & END
 (yz2 gt ny) : BEGIN & yz1 = ny - ny_z & yz2 = ny-1 & END
 ELSE        : BEGIN & yz1 = yz1 & yz2 = yz2 & END
ENDCASE

HEXTRACT, image, hdr, image_z, hdr_z, xz1, xz2, yz1,yz2,/silent
sizez=size(image_z)
nx_z=sizez[1]
ny_z=sizez[2]
tk=nx_z*zoom/50

if abs(mu_ra) gt abs(mu_dec) then BEGIN
 txt_rot=90 
 xtk=tk
 ytk=tk/2
ENDIF else BEGIN
 txt_rot=0
 xtk=tk
 ytk=0
ENDELSE


window, 0, xsize=nx_z*zoom, ysize=ny_z*zoom
wshow, 0
tvscl, rebin(image_z,nx_z*zoom,ny_z*zoom,/s),top=top

;DISPLAY AT EPOCH 1950 POSITION
adxy,hdr_z,ra_epoch[0], dec_epoch[0], x50, y50
;tvcircle, tk, x50*tvmag, y50*tvmag, purple, thick=2
plots, x50*zoom-tk, y50*zoom-tk, /device
plots, x50*zoom+tk, y50*zoom+tk, /device,/continue, color=purple,thick=2
plots, x50*zoom+tk, y50*zoom-tk, /device
plots, x50*zoom-tk, y50*zoom+tk, /device,/continue, color=purple,thick=2
xyouts, x50*zoom+xtk, y50*zoom+ytk,'1950',/DEVICE, charsize=2, color=red, charthick=2, orient=txt_rot

;DISPLAY AT EPOCH 1980 POSITION
adxy,hdr_z,ra_epoch[1], dec_epoch[1], x80, y80
plots, (x80)*zoom-tk, (y80)*zoom-tk, /device
plots, (x80)*zoom+tk, (y80)*zoom+tk, /device,/continue, color=yellow,thick=2
plots, (x80)*zoom+tk, (y80)*zoom-tk, /device
plots, (x80)*zoom-tk, (y80)*zoom+tk, /device,/continue, color=yellow,thick=2
xyouts, (x80)*zoom+xtk,(y80)*zoom+ytk,'1980',/DEVICE, charsize=2, color=red, charthick=2, orient=txt_rot

;DISPLY AT FIRST EPOCH POSITION
adxy,hdr_z,ra_target_poss, dec_target_poss, x1, y1
plots, (x1)*zoom-tk, (y1)*zoom-tk, /device
plots, (x1)*zoom+tk, (y1)*zoom+tk, /device,/continue, color=black,thick=2
plots, (x1)*zoom+tk, (y1)*zoom-tk, /device
plots, (x1)*zoom-tk, (y1)*zoom+tk, /device,/continue, color=black,thick=2
xyouts, (x1)*zoom+xtk,(y1)*zoom+ytk,'POSS',/DEVICE, charsize=2, color=red, charthick=2, orient=txt_rot

;DISPLAY AT SECOND EPOCH POSITION
adxy,hdr_z,ra_target, dec_target, x2, y2
plots, (x2)*zoom-tk, (y2)*zoom-tk, /device
plots, (x2)*zoom+tk, (y2)*zoom+tk, /device,/continue, color=blue,thick=2
plots, (x2)*zoom+tk, (y2)*zoom-tk, /device
plots, (x2)*zoom-tk, (y2)*zoom+tk, /device,/continue, color=blue,thick=2
xyouts, (x2)*zoom+xtk,(y2)*zoom+ytk+(s_mudec*xtk),'2MASS',/DEVICE, charsize=2, color=red, charthick=2, orient=txt_rot

;DISPLAY AT SPITZER EPOCH POSITION
adxy,hdr_z,ra_epoch[3], dec_epoch[3], xs, ys
plots, (xs)*zoom-tk, (ys)*zoom-tk, /device
plots, (xs)*zoom+tk, (ys)*zoom+tk, /device,/continue, color=red,thick=2
plots, (xs)*zoom+tk, (ys)*zoom-tk, /device
plots, (xs)*zoom-tk, (ys)*zoom+tk, /device,/continue, color=red,thick=2
xyouts, (xs)*zoom+xtk,(ys)*zoom+ytk-xtk,'2005',/DEVICE, charsize=2, color=red, charthick=2, orient=txt_rot

IF keyword_set(log_set) THEN BEGIN
 comment=''
 READ, comment, PROMPT='Comments to be added to the log: '
 log_line=''
 logfile='logs/'+obj+'_pm_log.txt'
 date=systime()
 
 IF file_test(logfile) THEN BEGIN ;previous logfile exists
  old_file=1
  FILE_CHMOD, logfile, A_WRITE=1 ; Make file writable
  nloglines=FILE_LINES(logfile)
  logsarr=STRARR(nloglines)
  OPENR, lun_log, logfile, /GET_LUN
  READF, lun_log, logsarr
  CLOSE, lun_log
  FREE_LUN, lun_log
 ENDIF ELSE old_file=0
 
 OPENW, lun_rlog, logfile, /GET_LUN
 printf,lun_rlog, date
 printf,lun_rlog, 'This is data for the proper motion measurement of '+obj
 printf,lun_rlog, '2MASS Coordinates = '+strn(ra_h_targ) +' '+ strn(ra_m_targ) +' '+ strmid(strn(ra_s_targ),0,5) + ' ' $
                            + strn(dec_d_targ) + ' '+ strn(dec_m_targ) + ' '+ strmid(strn(dec_s_targ),0,5)
 printf,lun_rlog, ' '
 printf,lun_rlog, 'Coords File Used: '+coords_file
 printf,lun_rlog, 'POSS Image File Used: '+POSS_image_file
 printf,lun_rlog, '2MASS Image File Used: '+image_file_2M
 printf,lun_rlog, ' '
 printf,lun_rlog, 'Date of Epoch 1 = '+date_conv(date_first,'STRING')
 printf,lun_rlog, 'Date of Epoch 2 = '+date_conv(date_second,'STRING')
 printf,lun_rlog, 'Baseline (years) = '+year_diff_s
 IF year_diff lt 1. then printf, lun_rlog, 'WARNING: BASELINE VERY SHORT'
 printf,lun_rlog, ' '
 printf,lun_rlog, 'Number of Ref Stars used = '+strn(n_ref_stars);+'.'
 IF n_ref_stars le 4 then printf, lun_rlog, 'WARNING: USED SMALL NUMBER OF REF STARS'
 printf,lun_rlog, ' '
 printf,lun_rlog, 'mu_alpha (arcsec/yr) = '+mu_ra_s 
 printf,lun_rlog, 'mu_dec (arcsec/yr) = '+mu_dec_s 
 printf,lun_rlog, 'total pm (arcsec/yr) = ' +total_mu_s 
 printf,lun_rlog, ' '
 printf,lun_rlog, 'Predicted coord at Epoch J2000 used by Spitzer: ' + $
                  strn(ra_h_j2000) +' '+ strn(ra_m_j2000) +' '+ strmid(strn(ra_s_j2000),0,5) + ' '+ $
                  strn(dec_d_j2000) + ' '+ strn(dec_m_j2000) + ' '+ strmid(strn(dec_s_j2000),0,5)
 printf, lun_rlog, ' '
 printf, lun_rlog, 'COMMENT: '+comment
 IF old_file eq 1 THEN BEGIN
  printf,lun_rlog, '--------------------------------------------------------------------------------'
  FOR l=0, nloglines-1 DO printf, lun_rlog, logsarr[l]
 ENDIF
 CLOSE, lun_rlog
 FREE_LUN, lun_rlog
 FILE_CHMOD, logfile, A_WRITE=0, A_READ=1 ; Make file readonly
ENDIF

END