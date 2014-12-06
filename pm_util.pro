PRO pm_util, pm_tot, theta, pm_ra, pm_dec

phi = 90 + theta

pm_ra = -pm_tot * cos(phi/!radeg)
pm_dec = pm_tot * sin(phi/!radeg)

print,'pm ra,dec: ', pm_ra, pm_dec

;theta=(atan(mu_dec,-mu_ra)*!radeg)-90

;IF theta lt 0 then theta=theta+360
;total_mu_sig=sqrt(mu_ra_sig^2*abs(mu_ra)/total_mu + mu_dec_sig^2*abs(mu_dec)/total_mu)

END
