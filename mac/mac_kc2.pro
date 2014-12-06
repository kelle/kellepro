pro mac_kc2, parmfile, IMAGES=images, OUTIMAGE=outimage, 		     $
	DIR=dir, LABEL=label, FLATFIELD=flatfield, UNZIP=unzip,              $
	APERTURE=aperture, OBSCUR=obscur, TAU1=tau1, SIZE=ncells,            $
	PIXEL=pixel,  SUMMARY=summary, TVMAG=tvmag, SAVEPARMS=saveparms,     $ 
	SHOWPARMS=showparms, INTERACT=interact, NOTV=notv,                   $
	STOPATDIFFS=stopatdiffs, PEAKLOG = peaklog, FINDPEAKS=findpeaks

;
;+
; NAME:
;	MAC  (Match And Combine)
;
; PURPOSE:
;	Perform matched filtering on a set of images, and combine them using
;	shift-and-add.  The main steps are as follows:
;		(1) Subtract the sky background using subsequent images
;		(2) Flat field if user desires.
;		(3) Fill in bad pixels using a linear estimator.
;		(4) Detect source peaks using matched filter.
;		(5) Determine frame-to-frame offsets using peak positions.
;		(6) Combine the frames using shift-and-add.
;
; CALLING SEQUENCE:
;	mac, parmfile, IMAGES= , OUTIMAGE=, [ DIR= , LABEL= , UNZIP=, 
;		FLATFIELD= , APERTURE= , OBSCUR= , TAU1= , SIZE= , PIXEL= ,
;      (the previous words can be saved in the parmfile, the following cannot)'
;               SUMMARY= , SUBOUT= , MATCHOUT= , /STOPATDIFFS, 	
;		SAVEPARMS= , /SHOWPARMS, /INTERACT, /NOTV, /PEAKLOG,TVMAG=] 
;
; OPTIONAL INPUT PARAMETER:
;	PARMFILE    =	name of file containing the default values of input
;			input parameters.  Any optional keyword parameters 
;			entered	will override the values from this file.  
;
; OPTIONAL INPUT KEYWORDS:
;	DIR	    =	name of directory containing observed images
;
;	LABEL	    =	label used in data file names; default = 'data'
;			The data file names are then assumed to be of the form:
;				LABEL<4-digit #>.<a or b>[.gz]
;
;	IMAGES      =	ranges of image numbers, enclosed in square brackets.
;			Example:  to combine images 23-26, 33-72, and 107-134,
;			specify: [23, 26, 33, 72, 107, 134]
;
;	UNZIP	    =	allows reading of compressed FITS files. Recognizes
;			gzipped files [UNZIP='GZ'] (in which case, their
;                       file names must end in '.gz') and bzip2 
;                       [UNZIP='BZ2'] (extention is '.bz2'). Default is
;                       UNZIP='NO'. You must obtain Ressler's patched version
;                       of readfits.pro to use the bz2 option.
;
;	FLATFIELD   =	name of FITS file containing flat field image
;			(in local directory, not in DIR). 
;			Set to 0 to not flat field image.
;
;	APERTURE    =   telescope aperture diameter [m]
;	OBSCUR	    =	diameter of central obscuration [m]
;	TAU1	    =	optical depth of 1 air mass
;
;	SIZE	    =   size of output image (assumed square) in either 
;			dimension [pixels].  
;
;	PIXEL	    =	focal-plane pixel size in arcsec (if zero or negative, 
;			then use the value from FITS header).
;
;	OUTIMAGE    =   name of output FITS file containing shift-and-add image.
;
;	SUMMARY	    =   name of output summary file.
;			If not specified, then no file will be written.
;
;	SAVEPARMS   =	name of file to save the current parameter values at
;			termination of the procedure
;
;	SHOWPARMS   =	if set, then list the input parameters only (do not
;			run the program yet).
;
;	INTERACT    =   if set, then the user steps through each subtraction,
;			dynamically defining the search region with mouse
;			inputs. The user is then also shown with a crosshair 
;			the location of the peak found.  At the point, the
;			the user can accept this as the correct peak, search
;			again, or discard the image from the final output.
;
;	PEAKLOG	    =
;	NOTV	    =	if set, disable the TV display (necessary to run in
;			batch mode).
;
;	TVMAG	    =	magnification of displayed images (default = 3)
;
; OUTPUT FILE:
;	OUTIMAGE, as above.
;
; ADDITIONAL OUTPUT FILES (IF SPECIFIED):
;	(1) Summary file, listing various quantities for each subtracted image.
;	(2) Subtracted images, in FITS format (1 per nod).  The file names
;	    are of the form:  sub####.fits, where #### represents the image
;	    number corresponding to the beginning of the nod.
;	(3) Matched filter images, in FITS format (1 per nod).  The file names
;	    are of the form:  matched####.fits, where #### represents the image
;	    number corresponding to the beginning of the nod.
;	(4) Parameter save file.
;
; EXAMPLE:
;	Begin by listing the default input parameters on the terminal:
;
;		mac, /show
;
;	Now supply your own parameter values as needed, and save them in a 
;	file "mactest.inputs":
;
;		mac, dir='/home/test', images=[1,8], flat='flat.fits', $
;			out='shift.fits', save='mactest.inputs', /show
;
;	To actually run the program, either type the above line without the
;	"/show", or, instead, retrieve a previously-saved parameter file.  
;	In the example below, the output shift-and-add image will be in a 
;	file "shift.fits", the summary listing will be in "test.summary", and
;	a set of matched filter images will be outputted as well:
;
;		mac, 'mactest.inputs', summary='test.summary', /matched
;
;
; PROCEDURES CALLED:
;	apfilter
;	filler
;	fourint
;	get_nsfcam_band
;	get_orient
;	getpos
;	mkhdr
;	nextpower
;	pixresp
;	readfits
;	resmear
;	set_boxlimits
;	shiftimage
;	spacor
;	sxaddpar
;	sxpar
;	synpsf
;	writefits
;
; MODIFICATION HISTORY:
;	Written by K. A. Marsh,  1996 May 28. 
;	Modifications:  1997 Jun 23, by Peter Plavchan.
;                       1998 Jul 14, by M. Ressler ("mac2" diffs incorporated)
;			1998 Jul 24, by K. A. Marsh (rotation effects)
;			1999 Feb 10, by K. C. Cruz (to work with IRTF data)
;-

if n_params() eq 0 and not keyword_set(outimage) and $
  not keyword_set(showparms) then begin
    print,' '
    print,'Syntax:'
    print,'mac, parmfile,  IMAGES= ,OUTIMAGE= , SUMMARY= ,
    print,'     [DIR= , LABEL= ,UNZIP= , FLATFIELD= , APERTURE= ,'
    print,'     OBSCUR= , TAU1= ,SIZE= , PIXEL= , ] '
    print,'     SUBOUT= , MATCHOUT= ,SAVEPARMS= , TVMAG =, /SHOWPARMS,'
    print,'     /STOPATDIFFS, /INTERACT, /PEAKLOG, /NOTV, /FINDPEAKS'
    print,' '
    return
end

; Set default values of input parameters.
dir_in = '.'
label_in = 'data'
images_in = intarr(2)
unzip_in = 'NO'
flatfield_in = 'specify_me'
aperture_in = 3.0
obscur_in = 0.75
tau1_in = 0.2
npeaks_in = 1
ncells_in = 32
outimage_in = 'specify_me'
pixel_in = -1.
threshold_in = 1.
jcountno = 0

if keyword_set(stopatdiffs) then stopatdiffs=1 else stopatdiffs=0

; Read input parameters from file.
if(n_params() ne 0) then begin
    status = findfile(parmfile, count = found_file)
    
    if found_file eq 0 then begin
        message,/cont,' Unable to locate file '+parmfile
        return
    end

    unit = 1
    openr, unit, parmfile
    readf, unit, dir_in
    readf, unit, label_in
    readf, unit, flatfield_in
    readf, unit, unzip_in
    readf, unit, aperture_in, obscur_in 
    readf, unit, tau1_in
    readf, unit, ncells_in, pixel_in 
    close, unit
end

if not keyword_set(dir) then dir = dir_in
if not keyword_set(label) then label = label_in
if not keyword_set(flatfield) then flatfield = flatfield_in
if not keyword_set(unzip) then unzip = unzip_in
if not keyword_set(aperture) then aperture = aperture_in
if not keyword_set(obscur) then obscur = obscur_in
if not keyword_set(tau1) then tau1 = tau1_in
if not keyword_set(ncells) then ncells = ncells_in
if not keyword_set(pixel) then pixel = pixel_in

interact = keyword_set(interact)
if keyword_set(notv) then interact = 0
if not keyword_set(tvmag) then tvmag=3

if images(1) eq 0 then begin
    message,/cont," I can't proceed without a range of image numbers"
    return
endif

;Figure out how many files/peaks there are.
i = n_elements(images)
npeaks = intarr(i/2)
m = 0
for j=0,i-1,2 DO BEGIN
   npeaks_range = images(j+1) - images(j) + 1
   npeaks(m) = npeaks_range
   m = m + 1 
endfor
totpeaks=total(npeaks)
print, 'TOTAL PEAKS= ',totpeaks

; Print out the list of input parameters.
print, ' '
print, 'MAC inputs:'
print, '=========='
print, ' '
print, 'images    ..... ',images
print, 'npeaks    ..... ',npeaks
print, 'outimage  ..... ',outimage
print, 'dir       ..... ',dir
print, 'label     ..... ',label
print, 'flatfield ..... ',flatfield
print, 'unzip     ..... ',unzip
print, 'aperture  ..... ',aperture,' m'
print, 'obscur    ..... ',obscur,' m'
print, 'tau1      ..... ',tau1
print, 'size      ..... ',ncells,' pixels'
if pixel gt 0 then print, 'pixel     ..... ',pixel,' arcsec'  else $
    print, 'pixel     ..... FITS header value'
print, ' '

; Write out new parameters file.
if keyword_set(saveparms) then begin
    unit = 1
    openw, unit, saveparms
    printf, unit, dir
    printf, unit, label
    printf, unit, flatfield
    printf, unit, unzip
    printf, unit, aperture, obscur 
    printf, unit, tau1
    printf, unit, ncells, pixel
    close, unit
end

if keyword_set(showparms) then return

; Read the flat field file.
if flatfield ne '0'  then begin
	flat = readfits(flatfield,h)
	if flat(0) eq -1 then begin
	   message,/cont," The flatfield file specified is invalid"
	   return
	endif
	flat = float(flat)
endif

; Is is compressed?
case 1 of 
    (strpos(strupcase(unzip),'GZ') ge 0) : zip_ext = '.gz' 
    (strpos(strupcase(unzip),'BZ2') ge 0) : zip_ext = '.bz2' 
    else : zip_ext = ''
endcase

; Get parameters from first image.
image1 = dir + '/' + label + string(format='(i4.4)',images(0)) + '.a' + zip_ext
beam1 = readfits(image1,h1)
if beam1(0) eq -1 then return
nx = sxpar(h1,'NAXIS1')
ny = sxpar(h1,'NAXIS2')


if pixel le 0 then pixel = sxpar(h1,'ASEC_PIX') 
filtername = sxpar(h1,'FIL_WHEL')
camera=sxpar(h1,'INSTRUME')
band = get_nsfcam_band(h1)
wavelen = band(0)
bandwid = band(1)

object = sxpar(h1,'OBJECT')
date = sxpar(h1,'DATE_OBS')
telescope = sxpar(h1,'TELESCOP')
observer = sxpar(h1,'OBSERVER')
headline = 'Fr# Image  Pk#  A.M.   Peak     Sigma     Flux       ' +  $
  'Location'

; Open summary and rotation files.
if keyword_set(summary) then begin
    unit = 1
    openw, unit, summary
    printf, unit, ' '
    printf, unit, 'MAC inputs:'
    printf, unit, '=========='
    printf, unit, ' '
    printf, unit, 'dir       ..... ',dir
    printf, unit, 'label     ..... ',label
    printf, unit, 'images    ..... ',images
    printf, unit, 'unzip     ..... ',unzip
    printf, unit, 'flatfield ..... ',flatfield
    printf, unit, 'aperture  ..... ',aperture,' m'
    printf, unit, 'obscur    ..... ',obscur,' m'
    printf, unit, 'tau1      ..... ',tau1
    printf, unit, 'npeaks    ..... ',npeaks
    printf, unit, 'size      ..... ',ncells,' pixels'
    printf, unit, 'pixel     ..... ',pixel,' arcsec' 
    printf, unit, 'outimage  ..... ',outimage
    printf, unit, ' '
    printf, unit, 'Object = ',object
    printf, unit, ' '
    printf, unit, headline
    printf, unit, ' '
end	

; Calculate a synthetic PSF, and from it derive the covariance function to be
; used for filling in bad pixels, and also the filter kernel if necessary.
npsf = 16                   ; default size of PSF -- should be a power of 2
psf = synpsf(wavelen,aperture,obscur,npsf,pixel)
Cx = spacor(psf)
intbox = 5              ; size of interpolation box for filling in bad pixels
Cx = Cx(npsf/2-intbox:npsf/2+intbox-1, npsf/2-intbox:npsf/2+intbox-1)
kern = psf
kappa = kern/total(kern^2)	; Scale the matched filter kernel.

; Set aperture photometry window (halfwidth = 2.5 lambda/D)
iap = fix((2.5e-6*wavelen/aperture)*(3600./!DTOR)/pixel)

; Read in data.
nconfigs = 0           ; this initializes the set of estimators used in filler
bin = fltarr(ncells,ncells)     ; the bin used to sum images
newbin = fltarr(ncells,ncells)     ; the bin used to sum images
pixels = fltarr(5) ; vector of array values at a particular pixel in the saved images
goodpix = fltarr(4) ; vector of good array values
imcube = fltarr(ncells,ncells,totpeaks) ; saved images from the 5-box pattern
weight = fltarr(ncells,ncells)	; the weights used in the sums
totint = 0.                     ; total integration time
nranges = n_elements(images)/2
flipsign = 0

loadct,28
iframe = 0
frame = 0
special = 0

;IF (object eq '16 Tau' OR object eq '4 Lac') AND filtername eq 21 THEN BEGIN
;	npeaks(0) = 14
;        nranges = 1
;	diff = fltarr(nx,ny, 7)
;	special = 1
;ENDIF
;IF (object eq 'VB 10') AND (filtername eq 21) THEN BEGIN
;	npeaks(0) = 
;        nranges = 1
;	diff = fltarr(nx,ny, 7)
;	special = 1
;ENDIF  

FOR range = 0,nranges-1 do begin
   

   i = images(2*range)
  

   ; Do the differencing.
   IF special eq 0 then Begin 
        diff = fltarr(nx,ny,npeaks(range))  
	chopa = dir + '/' + label + string(format='(i4.4)',i) + '.a' + zip_ext        
	chopb = dir + '/' + label + string(format='(i4.4)',i+1) + '.a' + zip_ext
	beam1 = readfits(chopa,h,/SILENT)
	if beam1(0) eq -1 then begin
     	   message,/cont,' Unable to locate file '+chopa
     	   return
   	end
   	beam2 = readfits(chopb,hb,/SILENT)
  	if beam2(0) eq -1 then begin
     	   message,/cont,' Unable to locate file '+chopb
   	return
   	end
   	diff(*,*,0) = diff(*,*,0) + float(beam1 - beam2)
   ENDIF

   if(npeaks(range) eq 5) then begin
     chopa = dir + '/' + label + string(format='(i4.4)',i+2) + '.a' + zip_ext
     chopb = dir + '/' + label + string(format='(i4.4)',i+3) + '.a' + zip_ext
     chopc = dir + '/' + label + string(format='(i4.4)',i+4) + '.a' + zip_ext
     beam3 = readfits(chopa,h,/SILENT)
     if beam3(0) eq -1 then begin
	message,/cont,' Unable to locate file '+chopa
    	return
     end
     beam4 = readfits(chopb,hb,/SILENT)
     if beam4(0) eq -1 then begin
	message,/cont,' Unable to locate file '+chopb
	return
     end
     beam5 = readfits(chopc,hc,/SILENT)
     if beam5(0) eq -1 then begin
	message,/cont,' Unable to locate file '+chopc
 	return
     end
     diff(*,*,1) = diff(*,*,1) + float(beam2 - beam3)
     diff(*,*,2) = diff(*,*,2) + float(beam3 - beam4)
     diff(*,*,3) = diff(*,*,3) + float(beam4 - beam5)
     diff(*,*,4) = diff(*,*,4) + float(beam5 - beam4)
   endif 

   i = i + npeaks(range)
   
   ipeaknum = 0
   peaknum = 0
   if special eq 1 then npeaks(range) = npeaks(range)/2

   IF not stopatdiffs then begin
     FOR k = 0,npeaks(range)-1 DO BEGIN	;filter each differenced pair
	
	
	frame = frame + 1
     	peaknum = k 

	tint = sxpar(h,'ITIME')
     	coadd = sxpar(h,'CO_ADDS')
	IF special eq 1 then deltat = tint*coadd*4 else deltat = tint*coadd*2

; Apply flat-field and air mass corrections.
	if flatfield ne '0' then BEGIN
	   airmass = sxpar(h,'AIRMASS')
	   if ((ipos = strpos(strupcase(telescope),'PALOMAR'))) ne -1 then $
		    phi = 33.2 * !DTOR	$	; approximate latitude of Mt. Palomar
		    else phi = 19.8 * !DTOR	; approximate latitude of Mauna Kea

	   radecha = getpos(h)
	   delta = float(radecha(1))*!DTOR
	   ha = float(radecha(2))*!DTOR
	   cosz = sin(phi)*sin(delta) + cos(phi)*cos(delta)*cos(ha)
	   if airmass eq 0 then airmass = 1./cosz
       	   transpar = exp(-(airmass-1.)*tau1)
           nonblank = where(diff(nx,ny,k) eq diff(nx,ny,k) and flat gt 0.1, count)
           if count ne 0 then   $
           diff(nonblank) = diff(nonblank)/(transpar*flat(nonblank))
           lowgain = where(flat le 0.1, count)
           if count ne 0 then diff(lowgain) = !VALUES.F_NAN
	endif

; Fill in missing pixel values.
	SNR = 30.		; a nominal value for the SNR of a single image
        icent = ncells/2
        limits = intarr(4)
	search = intarr(4)
	search = [0 , nx-1, 0, ny-1]
        limits(0) = (search(0)-icent) > 0
        limits(1) = (search(1)+icent-1) < (nx-1)
        limits(2) = (search(2)-icent) > 0
        limits(3) = (search(3)+icent-1) < (ny-1)
        diff(*,*,k) = filler(diff(*,*,k),limits,Cx,SNR,nconfigs,configset,estset)


; Calculate matched filter.
        filter = fltarr(nx,ny)
        icw = npsf/2
        ilom = (search(0)-icw) > 0
        ihim = (search(1)+icw-1) < (nx-1)
        jlom = (search(2)-icw) > 0
        jhim = (search(3)+icw-1) < (ny-1)
        nvalues = float((ihim - ilom + 1)*(jhim - jlom + 1))
        dmean = total(diff(ilom:ihim, jlom:jhim,k))/nvalues
        filter(ilom:ihim, jlom:jhim) =    $
          convol((diff(ilom:ihim, jlom:jhim,k)-dmean), kappa)

; display differenced image
        if not keyword_set(notv) then begin
	   window,0,xsize=256*tvmag,ysize=256*tvmag,title="MAC DISPLAY"
		;loadct,5
           tvscl,rebin((diff(*,*,k)/deltat),256*tvmag,256*tvmag,/s)
		disprange=max(diff(*,*,k)/deltat)-min(diff(*,*,k)/deltat)
		low=median(diff(*,*,k)/deltat) - 0.01*disprange
		high= median(diff(*,*,k)/deltat) + 0.01*disprange
		;stretch,low,high
	endif

; Shift each differenced image to positive peak in the center.
        mintbox = (npsf/2) > 4 ; size of matched filter interpolation box
        hmint = mintbox/2
        intfactor = 16      ; interpolation factor, to find precise peak
        sampint = 1./intfactor
        nip = mintbox*intfactor

	include = 0 

; Interact, by Peter Plavchan (with modifications by K. A. Marsh)

	search:
        if interact and include EQ 2 then begin
	   print,'Click on upper left corner of search area.'
	   cursor,xa,ya,/device,/down
	   cursor,xa,ya,/device,/up
	   print,'Click on lower right corner of search area.'
	   cursor,xb,yb,/device,/down
	   cursor,xb,yb,/device,/up
	   plots,[xa,xb],[yb,yb],/device,color=12
	   plots,[xa,xb],[ya,ya],/device,color=12
	   plots,[xa,xa],[ya,yb],/device,color=12
	   plots,[xb,xb],[ya,yb],/device,color=12
	   xa=fix(xa/tvmag)
	   xb=fix(xb/tvmag)
	   ya=fix(ya/tvmag)
	   yb=fix(yb/tvmag)
	endif else if keyword_set(findpeaks) then begin

	   CASE peaknum OF
		0: BEGIN
		     xa = 105
		     xb = 130
		     yb = 105
		     ya = 130
		     ;xa = 122
		     ;xb = 128
		     ;yb = 127
		     ;ya = 133
		   END
		1: BEGIN
		     xa = 25 
		     xb = 60
                     yb = 25
		     ya = 60   
		     ;xa = 49 
		     ;xb = 55
		     ;yb = 52 
		     ;ya = 58
		   END
		2: BEGIN
		   if npeaks(range) eq 3 then begin & xa = 180 & xb = 210 & endif
		   if npeaks(range) eq 7 then begin & xa = 25 & xb = 60 & yb = 170 & ya=215 & endif
		   if npeaks(range) eq 9 then begin & xa = 180 & xb = 210 & endif
		    ; xa = 196 
		    ; xb = 202
		    ; yb = 204
		    ; ya = 210
		   END
		3: BEGIN
		    if npeaks(range) eq 7 then begin & xa = 170 & xb = 215 & yb = 170 & ya=215 & endif
		    if npeaks(range) eq 9 then begin & xa = 105 & xb = 130 & endif
		     ;xa = 122 
		     ;xb = 128
		     ;yb = 127
		     ;ya = 133
		   END	
		4: BEGIN
		    if npeaks(range) eq 7 then begin & xa = 170 & xb = 215 & yb = 25 & ya=60 & endif
		    if npeaks(range) eq 9 then begin & xa = 25 & xb = 60 & endif
		     ;xa = 49 
		     ;xb = 55
		     ;yb = 52 
		     ;ya = 58
		   END	
		5: BEGIN
		    if npeaks(range) eq 7 then begin & xa = 170 & xb = 215 & yb = 95 & ya=135 & endif
		    if npeaks(range) eq 9 then begin & xa = 180 & xb = 210 & endif
		     ;xa = 196 
		     ;xb = 202
		     ;yb = 204
		     ;ya = 210
		   END	
		6: BEGIN
		    if npeaks(range) eq 7 then begin & xa = 95 & xb = 135 & yb = 170 & ya=215 & endif
		    if npeaks(range) eq 9 then begin & xa = 105 & xb = 130 & endif
		     ;xa = 122 
		     ;xb = 128
		     ;yb = 127
		     ;ya = 133
		   END		   
		7: BEGIN
		     xa = 25 
		     xb = 60
		     ;xa = 49 
		     ;xb = 55
		     ;yb = 52 
		     ;ya = 58
		   END
		8: BEGIN
		     xa = 180 
		     xb = 210
		     ;xa = 196 
		     ;xb = 202
		     ;yb = 204
		     ;ya = 210
		   END
	   ENDCASE
	   IF npeaks(range) ne 7 then begin & yb = xa & ya = xb & endif

	endif else begin
	   xa = search(0)
	   xb = search(1)
	   yb = search(2)
	   ya = search(3)
        endelse
; end Interact

	include = 0
        abfilter = fltarr(nx,ny) + !values.f_nan
        abfilter(xa:xb,yb:ya) = filter(xa:xb,yb:ya)
        fabspeak = 0.
        remainder = where(abfilter eq abfilter, count)

	if count ne 0 then begin
	   abfilter(remainder) = abs(filter(remainder))
           fabspeak = max(abfilter, location, /NAN)
        endif
                
        IF fabspeak ne 0. then begin   

; Find the matched filter peak and corresponding data value.
	   ipeak = location mod nx
	   jpeak = location/nx
	   peak = diff(ipeak,jpeak,k)/deltat
		
; Interpolate to increase the accuracy of peak location.
	   set_boxlimits,ipeak,jpeak,hmint,hmint-1, nx,ny,ilom,ihim,jlom,jhim
	   ilop = ilom + hmint - ipeak
	   ihip = ihim + hmint - ipeak
	   jlop = jlom + hmint - jpeak
	   jhip = jhim + hmint - jpeak
	   portion = fltarr(mintbox,mintbox)
	   portion(ilop:ihip, jlop:jhip) = filter(ilom:ihim, jlom:jhim)
	   nanpart = where(portion ne portion, count)
	   if count ne 0 then portion(nanpart) = 0.
	   portint = fourint(portion,intfactor)
	   fabspeak = max(abs(portint), location)
	   ipeakint = location mod nip
	   jpeakint = location/nip
	   fpeak = portint(ipeakint,jpeakint)
	   peaksign = fpeak/fabspeak
	   fpeak = fpeak/deltat
	   xloc = ipeak + (ipeakint - nip/2)*sampint
	   yloc = jpeak + (jpeakint - nip/2)*sampint

; Apply sub-pixel shift to center of image.
	   xmove = (nx/2) - xloc
	   ymove = (ny/2) - yloc
	   diffshift = shiftimage(diff(*,*,k),xmove,ymove)

; Do aperture photometry.
	   set_boxlimits,ipeak,jpeak,5,5,nx,ny,iloa,ihia,jloa,jhia
	   flux = total(diff(iloa:ihia, jloa:jhia,k))/deltat
	   print, frame, flux

	   include = 1	


;begin code modification by Peter Plavchan

; show peak found with crosshair
           if not keyword_set(notv) then begin
		txloc=xloc*tvmag+tvmag/2 & tyloc=yloc*tvmag+tvmag/2
		plots,[txloc-6,txloc-1],[tyloc,tyloc],/device,color=12
		plots,[txloc+1,txloc+6],[tyloc,tyloc],/device,color=12
		plots,[txloc,txloc],[tyloc-6,tyloc-1],/device,color=12
		plots,[txloc,txloc],[tyloc+1,tyloc+6],/device,color=12
		plots,[txloc-6,txloc-1],[tyloc+1,tyloc+1],/device,color=255
		plots,[txloc+1,txloc+6],[tyloc+1,tyloc+1],/device,color=255
		plots,[txloc-6,txloc-1],[tyloc-1,tyloc-1],/device,color=255
		plots,[txloc+1,txloc+6],[tyloc-1,tyloc-1],/device,color=255
		plots,[txloc-1,txloc-1],[tyloc-6,tyloc-1],/device,color=255
		plots,[txloc-1,txloc-1],[tyloc+1,tyloc+6],/device,color=255
		plots,[txloc+1,txloc+1],[tyloc-6,tyloc-1],/device,color=255
		plots,[txloc+1,txloc+1],[tyloc+1,tyloc+6],/device,color=255
	   endif
	    

	   if interact then begin
		ask:
		ch=''
		print,' '

;interact with the user, do they like what the program's doing?

		read, Ch, PROMPT='Correct Peak (Yes/No/Discard)? ' 
		if (ch NE 'y') and (ch NE 'Y') and (ch NE 'n') and (ch NE 'N')$
			and (ch NE 'D') and (ch NE 'd') then begin 

;tell the user they can't type right

			print,ch,' is invalid input.'
			print,'Please enter Y,N, or D'
			goto, ask ; ask again
		endif else begin ; yes just continues on, so not cased here.
			if (ch EQ 'N') OR (ch EQ 'n') then begin
			   print,'MAC will now prompt you for the search area.'
			   include=2
			   goto, search ; user didn't like the peak, search again.
			endif 
			if (ch EQ 'D') or (ch EQ 'd') then begin
			   print,'Image will be discarded and excluded from the output.'
			   include=0 ; user didn't like data, discard.
                        endif
		endelse  ;end valid input
	   endif       ; end interact

;end code modification by Peter Plavchan


; Blank out the matched filter in neighborhood of peak.
           filter(iloa:ihia, jloa:jhia) = !VALUES.F_NAN

	ENDIF  ; if fabspeak ne 0.
		
	IF include eq 1 then begin
           iframe = iframe + 1		; keep track of INCLUDED total frames
           ipeaknum = ipeaknum + 1	; keep track of number of INCLUDED peaks in range
           totint = totint + deltat	; keep track of total int time of coadded image 

; Set up the weights, taking care of the edges if coverage is incomplete.
	   weightp = fltarr(ncells,ncells)
           weightp(*,*) = deltat
                    ;if ilop ne 0 then weightp(ilop,*) = 0.
                    ;if ihip ne ncells-1 then weightp(ihip,*) = 0.
                    ;if jlop ne 0 then weightp(*,jlop) = 0.
                    ;if jhip ne ncells-1 then weightp(*,jhip) = 0.
                    ;nzw = where(weightp ne 0.)

; Add to bin.
	   xlow = nx/2 - ncells/2
	   xhigh = nx/2 + (ncells/2 - 1)
	   ylow = ny/2 - ncells/2
	   yhigh = ny/2 + (ncells/2-1)
           print, 'beep', jcountno, xlow, xhigh, ylow, yhigh
           bin = bin + diffshift(xlow:xhigh,ylow:yhigh)
           imcube(*,*,jcountno) =  diffshift(xlow:xhigh,ylow:yhigh)
           jcountno=jcountno+1
           weight = weight + weightp

           if iframe eq 1 then begin
		print, ' '
                print, 'Object = ',object
                print, ' '
                print, headline
		print, ' '
           end

; Write peak log.

	   if keyword_set(peaklog) then begin
    		unita = 2
    		CASE npeaks(range) OF 
	 	  3: peaklog = '3peakslog'
 		  5: peaklog = '5peakslog'
  		  7: BEGIN
		    	if special eq 1 then begin
			   peaklog = '14peakslog' + object
			endif else begin
			   peaklog = '7peakslog'
			end
		     END
	 	  9: peaklog = '9peakslog'
		ENDCASE
		openu, unita, peaklog,/APPEND
		printf, unita, peaknum, xloc, yloc
		close, unita
	   ENDIF	

; Write summary record.
           if ipeaknum eq 1 then begin
              	nlab = label + string(format='(i4.4)',images(2*range))
              	print, format=   $
                   '(i3,a9,i2,f6.2,1x,3e9.2," (",f6.2,",",f6.2,")",f7.1,f8.4)',  $
                   frame,nlab,peaknum,0,peak,0,flux,xloc,yloc
              	if keyword_set(summary) then printf, unit, format=   $
                   '(i3,a9,i2,f6.2,1x,3e9.2," (",f6.2,",",f6.2,")",f7.1,f8.4)',  $
                   frame,nlab,peaknum,0,peak,0,flux,xloc,yloc
           endif else begin
              	print, format=   $
                   '(i3,9x,i2,f6.2,1x,3e9.2," (",f6.2,",",f6.2,")",f7.1,f8.4)',  $
                   frame,peaknum,0,peak,0,flux,xloc,yloc
              	if keyword_set(summary) then printf, unit, format=   $
                   '(i3,9x,i2,f6.2,1x,3e9.2," (",f6.2,",",f6.2,")",f7.1,f8.4)',  $
		   frame,peaknum,0,peak,0,flux,xloc,yloc
	   endelse   	  	; if ipeaknum
	endif           	; if include then begin
     endfor                   	; for k = 0, npeaks(range) (go through peaks in range)
   endif			; if not stopatdiffs		   	
endfor                          ; for range = 0, nranges-1 (go through ranges)

print,' '
print,'Output files:'
if keyword_set(summary) then print,'    ',summary

if not stopatdiffs then begin

; Here we could recombine the imcube stuff a replace bin with it!
for ix=0,ncells-1 DO BEGIN
  for iy=0,ncells-1 DO BEGIN
    for j=0,4 DO BEGIN
      pixels(j) = imcube(ix,iy,j)
    endfor
    for j=0,4 DO BEGIN
      if (pixels(j) - MEDIAN(pixels))^2 gt (5.*threshold_in)^2 then begin
        if pixels(j) lt -1. then begin
        icount=-1
        for i=0,4 do begin
          if i ne j then begin
            icount=icount+1
            goodpix(icount)=pixels(i)
          endif
        endfor
;       imcube(ix,iy,j) = imcube(33,33,j)
        imcube(ix,iy,j) = MEDIAN(goodpix)
;       imcube(ix,iy,j) = threshold_in
       endif
      endif
    endfor
  endfor
endfor
  
for j=0,4 DO BEGIN
  newbin = newbin + imcube(*,*,j)
endfor

;replace bin below with newbin ...

; Scale the summed image.
    outarr = fltarr(ncells,ncells)
    nonzero = where(weight ne 0, count)
    if count ne 0 then outarr(nonzero) = newbin(nonzero)/weight(nonzero)

; Find total counts/sec for summed image
    outpeak =  max(outarr, location)
    ioutpeak = location mod ncells
    joutpeak = location/ncells
    set_boxlimits, ioutpeak, joutpeak, 5, 5, ncells, ncells, ilob, ihib, jlob, jhib
    totalflux=total(outarr(ilob:ihib, jlob:jhib))
    print, 'TOTAL SCALED COUNTS OF OUTIMAGE IS ',totalflux
    if keyword_set(summary) then printf, unit, 'Total Counts=',totalflux

if keyword_set(summary) then close, unit

    print,'    ',outimage
    print,' '
    mkhdr,outhead,outarr
    sxaddpar,outhead,'ASEC_PIX',pixel
    sxaddpar,outhead,'OBJECT',object
    sxaddpar,outhead,'FILTER',filtername
    sxaddpar,outhead,'ITIME',totint
    sxaddpar,outhead,'DATE_OBS',date
    sxaddpar,outhead,'OBSERVER',observer
    sxaddpar,outhead,'COMMENT','Generated by MAC.PRO (KAM)'
    writefits,outimage,outarr,outhead
    print,' '

; More code edit by Peter Plavchan begins here

    if not keyword_set(notv) then begin
        window,1,xsize=384,ysize=384,title="Output image." 
        tvscl,congrid(outarr,384,384)
	wdelete,0
    endif

; More code edit by Peter Plavchan ends here
endif                           ; if not stopatdiffs 
return

end
	
