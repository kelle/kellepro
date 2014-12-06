;+
; NAME: INTERPOLFLUX
;
;
; PURPOSE: To interpolate over points with negative flux.
;
;
; CATEGORY: Function
;
;
; CALLING SEQUENCE: INTERPOLATE, flux, wav
;
;
; INPUTS: flux - the flux array that you want to have interpolated
;         wav - the wavelength array corresponding to the flux array
;
;
; OUTPUTS: newflux - the flux array with error points corrected
;
;
; PROCEDURE: Program run through all wavelength points.  Where it
;            finds a group of negative error points (flux values less
;            than -0.5), it replaces those points using a linear
;            interpolation between the first pixel to the left of the
;            negative points and the first pixel to the right of the
;            last consecutive negative point in that group. 
;
;
; EXAMPLE: answer=INTERPOLFLUX(FLUX, WAV)
;
;
; MODIFICATION HISTORY: Created Jul 14 by Margaret Kirkland
;
;-

function interpolflux, flux, wav

newflux=flux

wavedim=fltarr(1)
wavedim(0)=size(wav, /n_elements)-1.0


; Find out the resolution of the wavelength array
res=(max(wav)-min(wav))/wavedim

; If the first pixel has a value below -0.5, set it equal to the flux
; value of the second pixel.
if flux(0) lt -0.5 then newflux(0)=flux(1)

; Start with the second pixel---if the first has a negative value,
; there will be no way to 
i=1
while wav(i) lt max(wav) do begin
    if flux(i) lt -0.5 then begin
; Leftpt is the point just to the left of the negative points.
        leftpt=i-1

; Width yields the number of consecutive negative points in the spike.
        width=1
        while flux(i) lt -0.5 do begin
            width=width+1
            i=i+1
        endwhile    
         
; The first point after the end of the negative points is Rightpt.        
        rightpt=i

        m=(flux(rightpt)-flux(leftpt))/(wav(rightpt)-wav(leftpt))

        for j= leftpt+1, rightpt-1 do begin
            newflux(j)=flux(leftpt)+m*res*(j-leftpt)
        endfor
    endif
    
    if wav(i) lt max(wav) then i=i+1
endwhile

return, newflux

end
