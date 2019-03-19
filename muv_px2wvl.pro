;+
; NAME: muv_px2wvl
;
; PURPOSE:
;   A function that generates a FUV wavelength for a given CSOL pixel number (reanges from 0 to 1999) for the MUV slit
;  coefficients produced in routine '/Users/harder/Documents/LASP documents/2018/CSOL_2018/IDLanalysisCode/generate_fuv_wvlscale.pro'
;
; INPUTS:
;   CSOL pixel number
;
; OPTIONAL INPUTS:
; None
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   Returns the wavelength (nm vacuum) from the pixel number
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
; EXAMPLE:
;   muv_wvl=muv_px2wvl(findgen(2000)) ; give full MUV wavlength scale
;
; MODIFICATION HISTORY: RCS_ID="$Id: muv_px2wvl.pro,v 1.2 2019/03/13 10:55:42 spenton Exp $"
;   2018-04-11: JWH
;-
function muv_px2wvl,pixno

  muv_polyco=double([3.01530670E+02 , -6.16126619E-02 , -7.14143368E-07 , -5.83589066E-10 ,  1.58139249E-13])
  lambda_out=poly(pixno,muv_polyco)

  return,lambda_out
end
