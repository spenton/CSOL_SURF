;+
; NAME: csol_dispersion
;
; PURPOSE:
;   A function that generates the dispersion (i.e. nm/px) for eith the FUV or the MUV channel
;
; INPUTS:
;   pxno = Pixel number, or range of pixel numbers, can be the full 2000 element array
;   delta_px = the number of pixels that span the range of interest. i.e. delta_px = 7. give the wavelength range spanned by the 50 micron entrance slit
;
; OPTIONAL INPUTS:
; None
;
; KEYWORD PARAMETERS:
;   which_channel:  can be either 'muv' or 'fuv'  case insensitive
;
; OUTPUTS:
;   Returns the number of nm spanned for a given pixel with a selected delta_px range, can be an array or a single value.
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
;   pxno=csol_wvl2px(121.6,which_channel='fuv')  ;returns the pixel number for Ly-a for the FUV channel
;
; MODIFICATION HISTORY: RCS_ID="$Id: csol_dispersion.pro,v 1.1 2018/12/30 00:27:42 spenton Exp $"
;   2018-04-11: JWH
;-

FUNCTION csol_dispersion, pixno, delta_px,which_channel=which_channel
  ;July 25, corrected to match wavelength scale of SOLSTICE at the short end of the scale
  sz=n_elements(pixno)

  IF KEYWORD_SET(which_channel) THEN BEGIN

    which_channel=strlowcase(which_channel)

    CASE which_channel OF
      'muv': begin
        polyco=double([3.01530670E+02 , -6.16126619E-02 , -7.14143368E-07 , -5.83589066E-10 ,  1.58139249E-13])
        dispersion = polyco[1] + 2.*polyco[2]*pixno + 3.*polyco[3]*pixno^(2.) + 4.*polyco[4]*pixno^(3.)
        dispersion*=delta_px
      end

      'fuv': begin
        polyco= double([2.39853775E+02 , -6.18005060E-02 , -9.44819703E-07 , -1.88840915E-10 , 2.01994776E-14])
        dispersion = polyco[1] + 2.*polyco[2]*pixno + 3.*polyco[3]*pixno^(2.) + 4.*polyco[4]*pixno^(3.)
        dispersion*=delta_px
      end
    ENDCASE
  ENDIF ELSE BEGIN
    print, 'Channel keyword not identified use string variables fuv or muv (case insensitive)'
    return, -1
  ENDELSE

  return,dispersion

END
