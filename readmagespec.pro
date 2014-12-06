FUNCTION readmagespec,filename, silent=silent      ;this function takes in the name of a sloan
                                        ;spectra fits file and returns a 3 by X array
                                        ;called datastructure, where X is the number
                                        ;of data points measured.  datastructure(0,*)
                                        ;gives the wavelength information in
                                        ;air wavelengths, (1,*) gives
                                        ;flux, and (2,*) gives noise information.
										; written by Jackie ~2009
										;modified by Kelle Nov 2013
basename = STRMID(filename, 0, (STRLEN(filename)-1-6)) 

data = MRDFITS(filename,0, header, silent=silent )      ;read data into data, header into header

if SXPAR(header,'NAXIS2') eq 3 then begin
	wave=data[*,0]
	flux=data[*,1]
	error=data[*,2]
	nsteps = SXPAR( header,'NAXIS1' )       ;find the # of pixels in the spectra
endif else begin
		
	IF file_test(basename+'_E.fits') eq 1 then $
  error = MRDFITS(basename+'_E.fits', 0, silent=silent)

	basewave = SXPAR( header,'CRVAL1' )     ;find the starting wavelength of the spectrum in weird sloan units
	step = SXPAR( header,'CDELT1' )         ;find how big the wavelength step is in weird sloan units
	nsteps = SXPAR( header,'NAXIS1' )       ;find the # of pixels in the spectra
	wave = 10.0^( basewave + FINDGEN( nsteps ) * step )    ;convert the wavelengths to air wavelengths
	
	flux=REFORM( data[*,1] )
endelse
								
if n_elements(error) ne 0 then datastructure = FLTARR(3,nsteps) else datastructure = FLTARR(2,nsteps)
datastructure[0,*] = wave               ;fill it with wavelength
datastructure[1,*] = flux

if n_elements(error) ne 0 then datastructure[2,*] = REFORM( error );noise
 
RETURN,datastructure            ;send it back up the pipe!

END
