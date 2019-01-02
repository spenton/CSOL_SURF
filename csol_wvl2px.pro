
;+
; NAME: csol_wvl2px
;
; PURPOSE:
;   A function that generates the pixel number for a give wavelength, uses the polynoimal coefficients derived from the wavelength scales
;   and uses FZ_ROOTS.pro to solve the equation for the real root. Keywords which_channel = 'FUV' or 'MUV' are used to determine which channel
;   is requested
;
; INPUTS:
;   A valid wavelength in nanometers

;
; OPTIONAL INPUTS:
; None
;
; KEYWORD PARAMETERS:
;   which_channel:  can be either 'muv' or 'fuv'  case insensitive
;
; OUTPUTS:
;   Returns the pixel number from a given wavelength for a selected channel, can be an array or a single value.
;   Values outside of the 0-1999 range are returned as -1
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
; MODIFICATION HISTORY: RCS_ID="$Id: csol_wvl2px.pro,v 1.1 2018/12/30 00:27:42 spenton Exp $"
;   2018-04-11: JWH
;-

function csol_wvl2px, targetwvl, which_channel=which_channel
  ;July 25, corrected to match wavelength scale of SOLSTICE at the short end of the scale
  sz=n_elements(targetwvl)

  IF KEYWORD_SET(which_channel) THEN BEGIN

    which_channel=strlowcase(which_channel)

    CASE which_channel OF
      'muv': polyco = double([3.01530670E+02 , -6.16126619E-02 , -7.14143368E-07 , -5.83589066E-10 ,  1.58139249E-13])
      'fuv': polyco = double([2.39853775E+02 , -6.18005060E-02 , -9.44819703E-07 , -1.88840915E-10 ,  2.01994776E-14])
    ENDCASE
  ENDIF ELSE BEGIN
    print, 'Channel keyword not identified use string variables fuv or muv (case insensitive)'
    return, -1
  ENDELSE

  if sz gt 1 then begin
    px=fltarr(sz)
    for i=0,sz-1 do begin
      vec=polyco
      vec[0]-=targetwvl[i]
      roots=fz_roots(vec)
      ok=where(imaginary(roots) eq 0.,root_cnt)
      if root_cnt eq 1 then real_root=float(roots[ok]) else real_root=float(roots[ok[0]])
      if real_root ge 0D and real_root lt 1999.D then px=real_root else px = -1.
    endfor
  endif else begin
    vec=polyco
    vec[0]-=targetwvl
    roots=fz_roots(vec)
    ;print, roots
    ok=where(imaginary(roots) eq 0.,root_cnt)
    if root_cnt eq 1 then real_root=float(roots[ok]) else real_root=float(roots[ok[0]])
    if real_root ge 0D and real_root lt 1999.D then px=real_root else px = -1.
  endelse

  return,px

end
