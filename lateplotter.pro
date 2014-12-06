FUNCTION readnoerrorfitsfile,filename   ;this function takes in the name of a keck-style 
                                        ;spectra fits file and returns a 3 by X array
                                        ;called datastructure, where X is the number
                                        ;of data points measured.  datastructure(0,*)
                                        ;gives the wavelength information, (1,*) gives
                                        ;flux, and (2,*) gives noise information.
 
        data = READFITS(filename,header)        ;read data into data, header into header
        baselambda = SXPAR(header,'CRVAL1')     ;find the starting wavelength of the spectrum
        step = SXPAR(header,'CD1_1')            ;find how big the wavelength step is
        nsteps = N_ELEMENTS(data[*,0])          ;find the # of pixels in the spectra
mjd = SXPAR(header,'MJD') 
plate=SXPAR(header,'PLATEID') 
fiber=SXPAR(header,'FIBERID') 
        range = nsteps * step                   ;calculate the range of the spectra in angstroms
        idlnsteps = nsteps - 1                  ;calculate the trailing index of flux
        lambda = FLTARR(nsteps)                 ;now create an array for the wavelength values...
 
        FOR i=0l, idlnsteps DO BEGIN
                lambda[i] = baselambda + step * i       ;fill in lambda array
        ENDFOR
 
        flux = REFORM(data[*,0])                        ;pick off the flux values
        error = FLTARR(nsteps)          ;since our code is dependent on having values for the
        error[*] = 1                  ;noise, we'll just have to make some!
 ;error[*] = REFORM(data[*,3])                ;noise, we'll just have to make some!


        datastructure = FLTARR(3,nsteps)
        datastructure[0,*] = lambda
        datastructure[1,*] = flux
        datastructure[2,*] = error
 
        RETURN,datastructure
END
 
;---------------------------------------------------------

FUNCTION readapodatfile,filename        ;this function takes in the name of a file 
                                        ;with 4 columns of data stored in the APO 
                                        ;text file format and returns a 3 by X array
                                        ;called datastructure, where X is the number
                                        ;of data points measured.  datastructure(0,*)
                                        ;gives the wavelength information, (1,*) gives
                                        ;flux, and (2,*) gives noise information.
                                        
        CLOSE, 7
        OPENR, 7, filename              ;open specified file for reading
        
        count = 0                       ;make variable to count lines in file
        
        WHILE NOT EOF(7) DO BEGIN       ;count number of lines in file
                READF, 7, variable
                count = count + 1
        ENDWHILE
 
        wavelength = FLTARR(count)      ;create arrays to store wavelength                              
        signal = FLTARR(count)          ;flux, error, and signal to noise,
        noise = FLTARR(count)           ;as well as a dummy array for storing
        signoise = FLTARR(count)        ;data before reading it into those arrays.
        dummy = FLTARR(4)
        
        CLOSE, 7                        
        OPENR, 7, filename
 
        truesize = count - 1            ;get the final index for the arrays created above
 
        FOR i=0l,truesize DO BEGIN       ;read data into the arrays for wavelength, flux,
                                        ;error and signal to noise...
                READF, 7 ,dummy
                wavelength[i] = dummy[0]
                signal[i] = dummy[1]
                noise[i] = dummy[2]
                signoise[i] = dummy[3]
        ENDFOR
 
        datastructure = FLTARR(3,count) ;create a data structure to send 
        datastructure(0,*) = wavelength ;this info back in one piece.
        datastructure(1,*) = signal     ;then fill it up appropriately...
        datastructure(2,*) = noise
        
        RETURN,datastructure            ;throw it back up there
 
END
 
;--------------------------------------------------
 
FUNCTION readsloanfile,filename         ;this function takes in the name of a sloan 
                                        ;spectra fits file and returns a 3 by X array
                                        ;called datastructure, where X is the number
                                        ;of data points measured.  datastructure(0,*)
                                        ;gives the wavelength information, (1,*) gives
                                        ;flux, and (2,*) gives noise information.
 
        data = READFITS( filename,header )      ;read data into data, header into header
mjd = SXPAR(header,'MJD') 
plate=SXPAR(header,'PLATEID') 
fiber=SXPAR(header,'FIBERID')

        basewave = SXPAR( header,'COEFF0' )     ;find the starting wavelength of the spectrum in weird sloan units
        step = SXPAR( header,'COEFF1' )         ;find how big the wavelength step is in weird sloan units
        nsteps = SXPAR( header,'NAXIS1' )       ;find the # of pixels in the spectra
        wave = 10.0^( basewave + FINDGEN( nsteps ) * step )     ;now create an array for the wavelength values...
        ends = MINMAX( wave )                   ;find the limits of the spectra
        range = FIX(ends[1] - ends[0])          ;find the range of the spectra
        datastructure = FLTARR(6,nsteps)        ;create a data structure to send back all
                                                ;the info we've gathered.
        datastructure[0,*] = wave               ;fill it with wavelength
        datastructure[1,*] = REFORM( data(*,0) );flux
        datastructure[2,*] = REFORM( data(*,2) );and noise 
        datastructure[3,0] = plate
        datastructure[4,0] = mjd
        datastructure[5,0] = fiber
        RETURN,datastructure            ;send it back up the pipe!
END
 
;---------------------------------------------------------
 
FUNCTION normalize, rawlambda, rawflux          ;this function takes in arrays for flux and wavelength
                                                ;and normalizes the flux to the value at a given wavelength
                                                ;which, at this point, is 8350 angstroms...
 
        range = N_ELEMENTS(rawflux)
        fluxpastpoint = WHERE( rawlambda GT 8350)               ;grabs subscripts of newinlambda with lambda > 8350
        fluxval = ABS( rawflux(fluxpastpoint(0)))               ;set variable equal to the flux immediately past 8350 A
        fluxvalarr = REPLICATE(fluxval, 1, range)               ;creates a properly sized array with elements of fluxval
        normflux = rawflux / fluxvalarr
 
        RETURN, normflux
 
END
 
;--------------------------------------------------------
 
PRO lateplotter
 
SET_PLOT, 'X'
!p.multi=[0,1,1]
 
;Get all the files we need
 
type = ''
READ, type, PROMPT = 'Sloan data (s), apofits (f) or apodat (d)?'
sloan = 's'
apodat = 'd'
apofits = 'f'
 
need = ''
READ, need, PROMPT = 'do you need a datapath? (y or n)'
yes = 'y'
no = 'n'
 
datapath = ''
IF need EQ yes THEN READ, datapath, PROMPT = 'Datapath: '
 
filelist = ''
READ, filelist, PROMPT = 'Filelist: '
 
newfile = ''
READ, newfile, PROMPT = 'Name of the new file: '
 
start = 0
READ, start, PROMPT = 'Start on idl file #: '
 
;open the newfile for writing
 
CLOSE, 1
OPENW, 1, newfile
  
;read in the file list
 
filefmt = '(A)'
READCOL, filelist,F=filefmt,longname
 
;find out how many files we'll look at
 
numoffiles = N_ELEMENTS(longname)
idlnumoffiles = numoffiles - 1
 
specin = ''
ourtype = ''

newresultfile = ''
;read in file from template fitting program
READ, newresultfile, PROMPT = 'NewResultFile name: ' 

newresultformat = '(A,D,D,F,F,F,L,I,F,F,F,F,F,F,F,F)'
READCOL, newresultfile, F=newresultformat, file, ra, dec, rminusi, iminusz, imag, primtarget, specclass, tempfit, tempres, spread, indmfit, indmerr, indlfit, indlerr, tempminusind

;now cycle through all the files
 
FOR i = start, idlnumoffiles DO BEGIN
 
        IF need EQ yes THEN BEGIN
                specin = datapath + longname[i] 
        ENDIF ELSE BEGIN
                specin = longname[i]
        ENDELSE
 
        IF type EQ sloan THEN targetinfo = READSLOANFILE(specin) ELSE $
	IF type EQ apofits THEN targetinfo = READNOERRORFITSFILE(specin) ELSE targetinfo = READAPODATFILE(specin)
        lambda = targetinfo[0,*]
        flux = targetinfo[1,*]
        startend = MINMAX( lambda )
        range = FIX( startend[1] - startend[0] )
        baselambda = startend[0]
 
;       PRINT, 'raw data: halpha'
;       PLOT, lambda, flux, XRANGE=[6350,6750]
;       wait = GET_KBRD(1)
 
        newlambda = FINDGEN(range) + baselambda         ;creates point every angstrom to interpolate and shift
        newflux = INTERPOL(flux, lambda, newlambda)     ;interpolates flux to be defined every angstrom
        smoothflux = SMOOTH(newflux,3)                 ;smooths flux values over 47 angstrom window
        normflux = NORMALIZE(newlambda,smoothflux)      ;normalizes flux to value at a given lambda (specified in
                                                        ;NORMALIZE...)
        PRINT, specin
        PRINT, 'template fit = ',tempfit[i],', indices m fit = ',indmfit[i], ', indices l fit',indlfit[i]
        PLOT, newlambda, normflux, XRANGE = [6000,9200]
        READ, ourtype, PROMPT = 'Type assigned by eye : '
 
        IF ourtype EQ 99 THEN ourtype = fitendshere[i]
 
        IF ourtype EQ 1000 THEN BEGIN
                GOTO, JUMP1
        ENDIF
  
        spacer = '    '
 
        PRINTF, 1, longname[i], spacer, ourtype
 
ENDFOR
 
JUMP1: specin = 'dump'
 
        CLOSE, 1
 
END
