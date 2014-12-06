;+
; NAME:
;	Extract_interact.pro
;
; PURPOSE:
;	Follow the same steps as those in extract.pro, however as base
;	files use spectral fits files extracted by the SMART project manager.
;
; CALLING SEQUENCE:
;	Write the calling sequence here. Put optional inputs in brackets.
;	For procedures, use the form:
;
;	Extract_interact, ref, [Directory, Data_dir, Graph_dir, $
;	            Save_dir, /GRAPH_ALL, /NO_PS, /NO_SAVE, $
;                   /COMPARE_BURROWS, /UNBIN_BURROWS]
;                   
;
;	Note that the routine name is ALL CAPS and arguments have Initial
;	Caps.
;
; INPUTS:
;       Ref:        The reference number for the object.  Include
;                   single quotes around the name of the object.
;
; OPTIONAL INPUTS:
;	Directory:  String containing the complete name of the
;	            directory containing the 3-plane files to be 
;                   extracted.
;       Data_dir:   String containing the complete name of the
;                   directory of the spectral fits files to be
;                   extracted.  All files should be observations
;                   of the same object.  If not specified, Extract
;                   will assume that data is in the directory 
;                   '/data/hillary/1/mkirklan/home/spectral_data/'+ref+'/'.
;
;                   NOTE: the files in Data_dir should be named as follows:
;                         'reference #_000*_cycle #'  , where
;                         i=0 for nod A, SL2
;                         i=1 for nod B, SL2
;                         i=2 for nod A, SL1
;                         i=3 for nod B, SL1
;                   These naming conventions must be followed or the
;                   program will not operate properly.

;
;	Graph_dir:  String containing the complete name of the
;	            directory to which all postscript output will be
;	            sent.  If not specified, files will be sent to the
;	            directory
;	            '/data/hillary/1/mkirklan/home/graphs/'+ref+'/'. 
;       Save_dir:   String containing the complete name of the
;	            directory to which all IDL save files will be
;	            sent.  If not specified, files will be sent to the
;	            directory /data/hillary/1/mkirklan/home/saved/.
;	
; KEYWORD PARAMETERS:
;	GRAPH_ALL:  If set, program will create a postscript file with
;	            each page containing the spectrum from a different
;	            observation of the object.  These
;	            files will be placed in the directory specified
;	            by Graph_dir (or 
;                   '/data/hillary/1/mkirklan/home/graphs/'+ref+'/', if     
;                   Graph_dir is not included in the call to Extract).	
;
;	NO_PS:       If set, there will be no postscript output.
;       
;       NO_SAVE:     If set, the program will not produce a .sav
;                    file.  If not set, a .sav file will be created in
;                    the directory specified by Save_dir (or in
;                    /data/hillary/1/mkirklan/home/saved/, if
;                    Save_dir is not included in the call to Extract).
;
;     
;       COMPARE_BURROWS:   If set, Burrows graphs will be output to a
;                          postscript file in the directory specified
;                          by Graph_dir, or the default graph
;                          directory (see description of "Graph_dir"
;                          above). 
;
;       REBIN_BURROWS:     If set, rebinned Burrows graphs will be
;                          output to a postscript file entitled
;                          "compare_burrows_rebinned.ps" in Graph_dir
;                          or the default graph directory (see
;                          description of "Graph_dir" above).  Note:
;                          COMPARE_BURROWS must be set for
;                          REBIN_BURROWS graphs to be output. 
;
; OUTPUTS:
;	By default, the program creates:
;          1) A postscript file of the averaged spectrum
;             (averaged over all observations) to the directory given by
;             'Graph_dir' (or
;             '/data/hillary/1/mkirklan/home/graphs/'+ref+'/',
;             if Graph_dir is not included in the call to Extract).
;          2) A .sav file of the final averaged data.
;             Includes wave and av_flux.
;       The keyword /NO_PS will prevent output (1), and /NO_SAVE will
;       avoid output (2).
;
; OPTIONAL OUTPUTS:
;       Burrows Graphs:  If keyword COMPARE_BURROWS is set, data for
;                   the Burrows models will be read in from
;                   /data/hillary/1/kelle/Burrows_models/, and a graph
;                   will be produced in which the averaged nod B data
;                   is plotted against each of the Burrows models.
;                   The data will be normalized to the average flux in
;                   the wavelength range 9-9.75 microns.
;
;       Rebinned Burrows Graphs:  If keyword REBIN_BURROWS is set, 
;                   graphs identical to those described above (see
;                   "Burrows Graphs") will be created, with the
;                   Burrows data rebinned so that it is the same
;                   resolution as the irs observations.
;
;
; PROCEDURE:
;       * The 0002 and 0003 (1st order) and 0004 and 0005 (2nd order) 
;           wavelength and flux arrays are created.  Interpolation is 
;           used to remove error spikes, and the overlap of orders 1
;           and 2 is removed. 
;       * Take the average of all cycles for a given nod, then take
;           the average of composite nod A and nod B data.
;       * Plots and save files are output
;
; ROUTINES CALLED:
;       readcol
;       fixb_test
;
; EXAMPLE:
;       Extract spectra from the directory /data/kelle/irs/ which
;       contains all spectral fits files from a given observation.
;       Do not save the wavelength and flux arrays, but save plots of
;       each observation, as well as plots comparing the averaged data
;       to the Burrows models, to the directory
;       /data/kelle/irs/graphs/.
; 
;	Extract_interact, 'U10287', data_dir='/data/kelle/irs/', $
;	         graph_dir='/data/kelle/irs/graphs/',  $
;                /GRAPH_ALL, /NO_SAVE, /COMPARE_BURROWS
;
;
; FUTURE MODIFICATIONS:

;
; MODIFICATION HISTORY:
;
;       Written by: Margaret Kirkland 
;                   July, 2005 
;                   No additional modifications
;
;--------------------------------------------------------------------------

PRO EXTRACT_interact, ref, Data_dir=data_dir, Graph_dir=graph_dir, $
             Save_dir=save_dir, $
             graph_all=GRAPH_ALL, no_ps=NO_PS, no_save=NO_SAVE, $
             compare_burrows=COMPARE_BURROWS, unbin_burrows=UNBIN_BURROWS



;********************* READ IN Spectral FITS FILES *********************
; Set location of the directory
if keyword_set(data_dir) eq 0 then $
 data_dir='/data/hillary/1/mkirklan/home/spectral_data/'+ref+'/'


; Prompt the user to provide you with the number of nodA and nodB
; files in the directory.  

READ, numA, PROMPT='Please enter the number of nod A files in the directory '$
      +data_dir+' : '
READ, numB, PROMPT='Please enter the number of nod B files in the directory '$
      +data_dir+' : '

; Create data arrays.  Assume that spectral fits
data0002=fltarr(numA, 12, 128)
data0003=fltarr(numB, 12, 128)
data0004=fltarr(numA, 12, 131)
data0005=fltarr(numB, 12, 131)

; Read in spectral fits files
for i=0, numA-1 do begin
    data0002(i, *, *)=readfits(data_dir+ref+'_0002_000'+strn(i)+'.fits')
endfor
for i=0, numB-1 do begin
    data0003(i, *, *)=readfits(data_dir+ref+'_0003_000'+strn(i)+'.fits')
endfor

for j=0, numA-1 do begin
    data0004(j, *, *)=readfits(data_dir+ref+'_0004_000'+strn(j)+'.fits')
endfor
for j=0, numB-1 do begin
    data0005(j, *, *)=readfits(data_dir+ref+'_0005_000'+strn(j)+'.fits')
endfor


;********************* Create Flux and Wavelength Arrays *********************

;--------------------------------
; Create 0002 arrays, removing "bonus" (3rd) order data.

maxindex2=fltarr(numA)
for i=0,numA-1 do begin
    max2=-1
    maxindex2(i)=-1
; Find final wavelength of 1st order data
    j=0
    while data0002(i,0,j) gt max2 do begin
        max2=data0002(i,0,j)
        maxindex2(i)=j
        j=j+1
    endwhile
endfor

firstsize0002=maxindex2(0)


; Make wave and flux arrays for 0002 data
firstflux0002=fltarr(firstsize0002+1, numA)
; All of the wavelength arrays are the same, so you only need to make
; one of them.
firstwave0002=fltarr(firstsize0002+1)

for i=0,numA-1 do begin
    for j=0,firstsize0002 do begin
        firstwave0002(j)=data0002(i,0,j)
        firstflux0002(j, i)=data0002(i,1,j)
    endfor
endfor

; Use linear interpolation to remove any error points
for i=0, numA-1 do begin
    firstflux0002(*,i)=interpolflux(firstflux0002(*,i), firstwave0002)
endfor


; A NOTE ABOUT PTR_AAR: The odd elements of ptr_aar contain the
; off-source data from that observation (i.e. noise).  For example,
; the first observation (_0000) taken of the 1st order (0003_) would have
; the desired 1st order data in (*ptr_aar(00)).irs.data.  During that
; observation, the 2nd order slit will be open, just observing the
; sky.  The data collected from the 2nd order slit will be stored in
; (*ptr_aar(01)).irs.data.  
;

; Shrink wave and flux arrays so that they include only data from
; wavelengths less than or equal to 7.5 microns
good0002=where(firstwave0002 le 7.5)
wave0002=firstwave0002(good0002)

size0002=size(good0002, /n_elements)-1
flux0002=fltarr(size0002+1, numA)
for i=0, numA-1 do begin
    flux0002(*,i)=firstflux0002(good0002, i)
endfor

;----------------
; Create 0003 arrays, removing "bonus" (3rd) order data.

maxindex3=fltarr(numB)
for i=0,numB-1 do begin
    max3=-1
    maxindex3(i)=-1
; Find final wavelength of 2nd order data
    j=0
    while data0003(i,0,j) gt max3 do begin
        max3=data0003(i,0,j)
        maxindex3(i)=j
        j=j+1
    endwhile
endfor

firstsize0003=maxindex3(0)


; Make wave and flux arrays for 0003 data
firstflux0003=fltarr(firstsize0003+1, numB)
; All of the wavelength arrays are the same, so you only need to make
; one of them.
firstwave0003=fltarr(firstsize0003+1)

for i=0,numB-1 do begin
    for j=0,firstsize0003 do begin
        firstwave0003(j)=data0003(i,0,j)
        firstflux0003(j, i)=data0003(i,1,j)
    endfor
endfor

; Use linear interpolation to remove any negative data points.
for i=0, numB-1 do begin
    firstflux0003(*,i)=interpolflux(firstflux0003(*,i), firstwave0003)
endfor

; Shrink wave and flux arrays so that they include only data from
; wavelengths less than or equal to 7.5 microns
good0003=where(firstwave0003 le 7.5)
wave0003=firstwave0003(good0003)

size0003=size(good0003, /n_elements)-1
flux0003=fltarr(size0003+1, numB)
for i=0, numB-1 do begin
    flux0003(*,i)=firstflux0003(good0003, i)
endfor

;-----------------
; Create 0004 arrays.

; Find the size of the 0004 arrays
firstsize0004=130

; Create wavelength and flux arrays for 0004 data
firstflux0004=fltarr(firstsize0004+1,numA)
; Again, the wavelength arrays are identical, so you only need one.
firstwave0004=fltarr(firstsize0004+1)

firstwave0004=data0004(0,0,*)

; Use linear interpolation to remove any negative error points
for i=0,numA-1 do begin
    firstflux0004(*,i)= interpolflux(data0004(i,1,*), firstwave0004)
endfor

; Shrink wave and flux arrays so that they include only data from
; wavelengths greater than or equal to 7.5 microns
good0004=where(firstwave0004 ge 7.5)
wave0004=firstwave0004(good0004)

size0004=size(good0004, /n_elements)-1
flux0004=fltarr(size0004+1, numA)
for i=0, numA-1 do begin
    flux0004(*,i)=firstflux0004(good0004, i)
endfor

;-----------------
; Create 0005 arrays.

; Find the size of the 0005 arrays
firstsize0005=130

; Create wavelength and flux arrays for 0005 data
firstflux0005=fltarr(firstsize0005+1,numB)
; Again, the wavelength arrays are identical, so you only need one.
firstwave0005=fltarr(firstsize0005+1)

firstwave0005=data0005(0,0,*)

; Use linear interpolation to remove any negative error points
for i=0,numB-1 do begin
    firstflux0005(*,i)= interpolflux(data0005(i,1,*), firstwave0005)
endfor

; Shrink wave and flux arrays so that they include only data from
; wavelengths greater than or equal to 7.5 microns
good0005=where(firstwave0005 ge 7.5)
wave0005=firstwave0005(good0005)

size0005=size(good0005, /n_elements)-1
flux0005=fltarr(size0005+1, numB)
for i=0, numB-1 do begin
    flux0005(*,i)=firstflux0005(good0005, i)
endfor


;------------------


;********************* Take Average of Observations *********************
; AVERAGE NOD A DATA

; We want to take the average of flux0002(*,i) and flux 0004(*,i)
; where each i represents a different observation.

av_flux0002=fltarr(size0002+1)
av_flux0004=fltarr(size0004+1)

for j=0, size0002 do begin
    for i=0,numA-1 do begin
        av_flux0002(j)+=flux0002(j,i)
    endfor
    av_flux0002(j)=(av_flux0002(j))/numA
endfor

for j=0, size0004 do begin
    for i=0,numA-1 do begin
        av_flux0004(j)+=flux0004(j,i)
    endfor
    av_flux0004(j)=(av_flux0004(j))/numA
endfor
;------------------
; AVERAGE NOD B DATA

; We want to take the average of flux0003(*,i) and flux 0005(*,i)
; where each i represents a different observation.

av_flux0003=fltarr(size0003+1)
av_flux0005=fltarr(size0005+1)

for j=0, size0003 do begin
    for i=0,numB-1 do begin
        av_flux0003(j)+=flux0003(j,i)
    endfor
    av_flux0003(j)=(av_flux0003(j))/numB
endfor

for j=0, size0005 do begin
    for i=0, numB-1 do begin
        av_flux0005(j)+=flux0005(j,i)
    endfor
    av_flux0005(j)=(av_flux0005(j))/numB
endfor
;----------------
; AVERAGE TOGETHER NOD A AND NOD B DATA

; Take the average of av_flux0002 and av_flux0003, as well as of
; av_flux0004 and av_flux0005.  

; wave1 and wave2 are arrays of 1st order and 2nd order wavelengths
; respectively.
wave1=wave0002
wave2=wave0004

; flux1 and flux2 are the averaged data for orders 1 and 2
; respectively.
flux1=(av_flux0002+av_flux0003)/2
flux2=(av_flux0004+av_flux0005)/2



;********************* Output .sav File *********************

; Save arrays to extract.sav, unless keyword /NO_SAVE was set
if keyword_set(NO_SAVE) eq 0 then begin
    if keyword_set(save_dir) eq 0 then $
         save_dir='/data/hillary/1/mkirklan/home/saved/'
    save, wave1, av_flux0002, av_flux0003, flux1, wave2, av_flux0004, $
      av_flux0005, flux2, flux0002, flux0003, flux0004, flux0005, $
      filename=save_dir+ref+'.sav'
endif

;********************* Output Final Spectrum to Screen *********************

mean_all=mean(flux2(where(wave2 gt 9 and wave2 lt 9.75)))
meanA=mean(av_flux0004(where(wave2 gt 9 and wave2 lt 9.75)))
meanB=mean(av_flux0005(where(wave2 gt 9 and wave2 lt 9.75)))

max_all=max(flux1/mean_all)+0.1
max_ab=(max(av_flux0002/meanA)>$
         max(av_flux0003/meanB))+0.1

    set_plot, 'x'
    plot, wave1, flux1/mean_all, xtitle='Wavelenth (microns)', $
      ytitle='Flux (relative to average flux at 9-9.75 microns)', $
      xrange=[4.5, 16], yrange=[0, max_all], xstyle=1, $
      ystyle=1, title='Average flux from all observations of '+ref
    oplot, wave2, flux2/mean_all

;********************* Output Postscript Files *********************
; Create graph output, unless keyword /NO_PS was set
if keyword_set(NO_PS) eq 0 then begin
    set_plot, 'ps'
    if keyword_set(graph_dir) eq 0 then $
        graph_dir='/data/hillary/1/mkirklan/home/graphs/'+ref+'/'

    device, filename=graph_dir+ref+'_average.ps', /landscape
    plot, wave1, flux1/mean_all, xtitle='Wavelenth (microns)', $
      ytitle='Flux (relative to flux at 9-9.75 microns)', $
      xrange=[4.5, 16], yrange=[0, max_all], xstyle=1, $
      ystyle=1, title='Average flux from all observations of '+ref
    oplot, wave2, flux2/mean_all
    device, /close
            
    ; If keyword /GRAPH_ALL was set, create plots of each observation and
    ; average graphs for nods A and B. 
    if keyword_set(GRAPH_ALL) eq 1 then begin

        device, filename=graph_dir+ref+'_average_nodA.ps', /landscape
        plot, wave1, av_flux0002/meanA, xtitle='Wavelenth (microns)', $
      ytitle='Flux (relative to flux at 9-9.75 microns)', $
          xrange=[4.5, 16], yrange=[0, max_ab], xstyle=1, $
          ystyle=1, title='Average flux from '+ref+' nod A'
        oplot, wave2, av_flux0004/meanA
        device, /close

        device, filename=graph_dir+ref+'_average_nodB.ps', /landscape
        plot, wave1, av_flux0003/meanB, xtitle='Wavelenth (microns)', $
      ytitle='Flux (relative to flux at 9-9.75 microns)', $
          xrange=[4.5, 16], yrange=[0, max_ab], xstyle=1, $
          ystyle=1, title='Average flux from '+ref+' nod B'
        oplot, wave2, av_flux0005/meanB
        device, /close
       

        device, filename=graph_dir+ref+'_nodA_spectra.ps',/landscape
        for i=0,numA-1 do begin
            plot, wave1, flux0002(*,i), xtitle= 'Wavelength (microns)',$
              ytitle='Flux (Jy)', xrange=[4.5, 16], $
              xstyle=1, title= 'Flux from '+ref+'_nodA_000'+strn(i)
            oplot, wave2, flux0004(*,i)
        endfor
        device,/close

        device, filename=graph_dir+ref+'_nodB_spectra.ps', /landscape
        for i=0,numB-1 do begin            
            plot, wave1, flux0003(*,i), xtitle= 'Wavelength (microns)',$
              ytitle='Flux (Jy)', xrange=[4.5, 16], $
              xstyle=1, title= 'Flux from '+ref+'_nodB_000'+strn(i)
            oplot, wave2, flux0005(*,i)
        endfor
        device,/close

    endif

;********************* Output Burrows Graphs *********************
    ; If keyword /COMPARE_BURROWS was set, create postscript file of graphs
    ; comparing the averaged data to the Burrows models in the directory
    ; /data/hillary/1/kelle/Burrows_models/. 
    if keyword_set(COMPARE_BURROWS) eq 1 then begin

        burrows, ref, wave1, flux1, wave2, flux2, max_all, $
                 unbin_burrows=unbin_burrows, graph_dir=graph_dir

    endif

set_plot, 'x'

endif

!P.MULTI=0

END
