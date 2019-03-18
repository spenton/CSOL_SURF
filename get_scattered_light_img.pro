;+
; NAME:
;  get_scattered_light_img
;
; PURPOSE:
;   A function that calculates the scattered light contribution by fitting a 4th degree polynomial across every cross dispersion pixel
;   that are outside of the FUV and MUV slit areas as determined by the function get_slith_info. The polynomial is evalued for the full
;   range of the cross dispersion pixels (i.e. 1504 pixels).
;
; INPUTS:
;   A CSOL image represented as a 2000x1504 float array
;
; OPTIONAL INPUTS:
; None
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   An array of 2000x1504 pixels of the interpolated scattered light contribution.  This can be subtracted from the image.
;
; OPTIONAL OUTPUTS:
;   None
;
; PROCEDURES/FUNCTIONS CALLED:
;   None
;
; RESTRICTIONS:
;   None
;
; EXAMPLE:scat=get_scattered_light_img(img)
;
; MODIFICATION HISTORY: RCS_ID="$Id: get_scattered_light_img.pro,v 1.2 2019/01/02 18:14:15 spenton Exp spenton $"
;   2018-05-11: JWH
;-
function get_scattered_light_img , img, n_xpix=n_xpix, n_ypix=n_ypix, do_plot=do_plot
	if n_elements(do_plot) ne 1 then do_plot=1
	if n_elements(n_xpix) ne 1 then n_xpix=2000
	if n_elements(n_ypix) ne 1 then n_ypix=1504

  xpix_counter=findgen(n_xpix)
  ypix_counter=findgen(n_ypix)

  scattered_light=fltarr(n_xpix,n_ypix)

  slith_info=get_slith_info(img)

  for i=0,n_xpix-1 do begin
    on_slit=where(ypix_counter ge slith_info.(0) and ypix_counter le slith_info.(1) or $
      ypix_counter ge slith_info.(3) and ypix_counter le slith_info.(4),complement=off_slit)
    if do_plot then plot, ypix_counter[off_slit],img[i,off_slit],psym=7,symsize=0.5,title=string(i)
    polyco=robust_poly_fit(ypix_counter[off_slit],img[i,off_slit],4)
    if do_plot then begin
    	oplot, ypix_counter,poly(ypix_counter,polyco),color=250
    	wait,0.2
    endif
    scattered_light[i,*]=poly(ypix_counter,polyco)
  endfor
  return, scattered_light
end
