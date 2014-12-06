FUNCTION area, glon, glat,min1,max1,min2,max2,bin,name,lun

printf, lun,'---------'
printf, lun, name

IF min1 LT 0 THEN min1 = min1+360.
IF max1 LT 0 THEN max1 = max1+360.

min1=double(min1)
max1=double(max1)
min2=double(min2)
max2=double(max2)


bin=double(bin)

red=192
green=32
blue = 96

dec=mean([min2,max2])

bin2=bin                  ;width of lat
bin1=bin2/cos(dec/!radeg) ; width of lon
;hopefully the same size

printf,lun, 'binsize: '+strn(bin1)+'  '+ strn(bin2)

;change max1 and max2 to accomodate bin size
;from HISTOGRAM binszie=(max-min)/(nbins-1)
;nbins-1=max-min/binszie
nbins1=CEIL((max1-min1)/bin1)
nbins2=CEIL((max2-min2)/bin2)

printf,lun, 'nbins: '+ strn(nbins1)+'  '+ strn(nbins2)

printf,lun, 'oldmax: '+ strn(max1)+'  ' + strn(max2)

new_max1=min1+(nbins1*bin1)
new_max2=min2+(nbins2*bin2)
printf,lun, 'ra/glon min/max: '+ strn(min1)+'  ' + strn(new_max1)
printf,lun, 'dec/glat min/max: '+ strn(min2)+'  ' + strn(new_max2)

area=abs((cos((min2+90)/!radeg)-cos((new_max2+90)/!radeg)))*!radeg*(new_max1-min1)
;see p.155 of labnotebook

printf,lun, 'area of '+name+': ' + strn(area) + ' sq. deg.'

reg=where(glon ge min1 and glon le new_max1 AND glat ge min2 AND glat Le new_max2 )
reg_lon=glon[reg]
reg_lat=glat[reg]


;reg_dist=HIST_2D(reg_lon, reg_lat, bin1=bin1, bin2=bin2,min1=min1, max1=max1, min2=min2, max2=max2)

input=dblarr(2,n_elements(reg_lon))
input[0,*]=reg_lon
input[1,*]=reg_lat

reg_dist=HIST_ND(input,min=[min1,min2],max=[new_max1,new_max2],nbins=[nbins1,nbins2])

lon=findgen(nbins1)*bin1+min1
lat=findgen(nbins2)*bin2+min2

;oplot, reg_lon,reg_lat,psym=4

t=where(reg_dist ne 0)
test=reg_dist
test[t]=10000
contour, test,lon,lat,/overplot, color=red

;make box
oplot, rebin([min1,new_max1],6),rebin([min2],6),color=green
oplot, rebin([min1,new_max1],6),rebin([new_max2],6),color=green
oplot, rebin([min1],6),rebin([min2,new_max2],6),color=green
oplot, rebin([new_max1],6),rebin([min2,new_max2],6),color=green

IF (where(reg_dist eq 0))[0] NE -1 THEN $
  percent_unfilled = n_elements(where(reg_dist eq 0))*1d/n_elements(reg_dist) ELSE $
  percent_unfilled = 0

percent_filled = n_elements(where(reg_dist ne 0))*1d/n_elements(reg_dist)

printf,lun,'fraction of region removed: ',strn(percent_filled)

IF percent_unfilled+percent_filled NE 1 THEN print, "WARNING, something's wrong."

area_removed=area*percent_filled

printf,lun, 'area removed from '+name+' : ' + strn(area_removed)+ ' sq. deg.'

RETURN, area_removed

END
