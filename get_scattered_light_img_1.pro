;+
; NAME:
;  get_scattered_light_img_1
;
; PURPOSE:
;   Similar to the function get_scattered_light_img_1
;   ;2018/09/05  JWH
;   corrects the scattered light assuming that only the first and last 200 points of the data represent the true scattered
;   light contribution to the image, particulary valuable since scattered light appears to be over corrected for the
;   weakest signals, routine produces similar scattered light contributions for stronger signals as get_scattered_light_img_1.pro.
;
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
; EXAMPLE:scat_1=get_scattered_light_img_1(img)
;   ;Recommend that generating  the fuv_line array that negative values of signal be replaced by their standard deviation.
;   uu=img-scat_1
;   for i=0,1999 do begin resistant_mean, uu[i,slith_info.fuv_bot[i]:slith_info.fuv_top[i]],2.5,mymean
;   if mymean gt 0. then fuv_line_1[i]=mymean else $
;      fuv_line[i]=robust_sigma(uu[i,slith_info.fuv_bot[i]:slith_info.fuv_top[i]])
;   endfor
;
; MODIFICATION HISTORY: RCS_ID="$Id: get_scattered_light_img_1.pro,v 1.2 2019/01/02 18:14:15 spenton Exp spenton $"
;   2018-05-11: JWH
;-
function get_scattered_light_img_1 , img
  n_xpix=2000
  n_ypix=1504

  xpix_counter=findgen(n_xpix)
  ypix_counter=findgen(n_ypix)

  scattered_light=fltarr(n_xpix,n_ypix)

  y_line=[findgen(200),(1300.+findgen(1504-1300))]

  for i=0,n_xpix-1 do begin
    img_line=[reform(img[i,0:200]),reform(img[i,1301:1503])]
    polyco=robust_poly_fit(y_line,img_line,4,yfit)

    scattered_light[i,*]=poly(ypix_counter,polyco)
  endfor
  scattered_light=smooth(scattered_light,[0,4],/edge_truncate)
  return, scattered_light
end
