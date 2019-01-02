;+
; NAME: fuv_px2wvl
;
; PURPOSE:
;   A function that generates a FUV wavelength for a given CSOL pixel number (reanges from 0 to 1999) for the FUV slit
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
;   fuv_wvl=fuv_px2wvl(findgen(2000)    ; gives the full FUV wavelength scale
;
; MODIFICATION HISTORY: RCS_ID="$Id: fuv_px2wvl.pro,v 1.1 2018/12/30 00:27:42 spenton Exp $"
;   2018-04-11: JWH
;-

function fuv_px2wvl,pixno

  fuv_polyco=double([2.39853775E+02 , -6.18005060E-02 , -9.44819703E-07 , -1.88840915E-10 , 2.01994776E-14])
  lambda_out=poly(pixno,fuv_polyco)

  return,lambda_out

end
