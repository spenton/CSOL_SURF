;+
; NAME: get_muv_slitpos
;
; PURPOSE:
;   A function that generates the location and size of a slit image from a laser line or isolated atomic line
;   use mostly for laboratory wavelength calibration and instrument focusing  Specifed for the CSOL MUV slit

;
; INPUTS:
;   A predifined wavelength for the slit image
;   A CSOL image represented as a 2000 by 1504 array of floats

;
; OPTIONAL INPUTS:
; None
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   Returns a structure of the slit specification
;
; OPTIONAL OUTPUTS:
;   None
;
; PROCEDURES/FUNCTIONS CALLED:
;   bspline_iterfit
;   bspline_valu
;
; RESTRICTIONS:
;   None
;
; EXAMPLE:
;   slit_info=get_muv_slitpos(wvl,img)
;
; MODIFICATION HISTORY: RCS_ID="$Id: get_muv_slitpos.pro,v 1.2 2019/03/19 19:40:15 spenton Exp spenton $"
;   2018-04-11: JWH
;-

function get_muv_slitpos,wvl,img

  ;example
  ;slit_info=get_muv_slitpos(wvl,img)

  slitpos=CREATE_STRUCT('wvl', float(0.), 'slitw_start',float(0.), 'slitw_stop',float(0.),'slitw',float(0.),'center_w',float(0.),$
    'slith_start',float(0.),'slith_stop',float(0.),'slith',float(0.),'center_h',float(0.) )
  slitpos.(0)=wvl

  y1=850                                       ;MUV slit central part of slit as initial guess
  y2=1050
  y_tot=(y2-y1)+1
  muvline_x=total(img[*,y1:y2],2)/y_tot
  mnmx_x=minmax(deriv(muvline_x),mnmx_xind)
  mnmx_xind=reverse(mnmx_xind)
  ;print, mnmx_xind

  x1=mnmx_xind[0]
  x2=mnmx_xind[1]
  x_tot=(x2-x1)+1
  muvline_y=total(img[x1:x2,*],1)/x_tot
  mnmx_y=minmax(deriv(muvline_y),mnmx_yind)
  mnmx_yind=reverse(mnmx_yind)
  ;print, mnmx_yind
  slitpos.(5)=mnmx_yind[0]
  slitpos.(6)=mnmx_yind[1]
  slitpos.(7)=abs(mnmx_yind[1]-mnmx_yind[0])
  slitpos.(8)=mean(mnmx_yind)


  y1=mnmx_yind[0]
  y2=mnmx_yind[1]
  y_tot=(y2-y1)+1
  muvline_x=total(img[*,y1:y2],2)/y_tot

  pix=findgen(2000)
  subpix=0.25*findgen(8000)
  sset=bspline_iterfit(pix,muvline_x,maxiter=10,requiren=0,bkspace=2)
  muvline_intp=bspline_valu(subpix,sset)
  ;svp_lineplot, subpix,deriv(subpix,muvline_intp)
  ;svp_lineplot, pix,deriv(pix,muvline_x)
  mnmx_x=minmax(deriv(muvline_intp),mnmx_xind)
  mnmx_xind=reverse(mnmx_xind)
  ;print, mnmx_xind
  slitpos.(1)=subpix[mnmx_xind[1]]
  slitpos.(2)=subpix[mnmx_xind[0]]
  slitpos.(3)=abs(subpix[mnmx_xind[1]]-subpix[mnmx_xind[0]])
  slitpos.(4)=mean([subpix[mnmx_xind[1]],subpix[mnmx_xind[0]]])

  return,slitpos
end
