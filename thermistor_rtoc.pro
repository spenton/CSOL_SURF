;+
; NAME: Thermistor_RtoC
;
; PURPOSE:
  ; Function to convert input thermistor resistance [*] to temperature [C]
;
; INPUTS:
;   Thermistor Resistance
;
; OPTIONAL INPUTS:
;  Temperature (C)
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
;   Tc=Thermistor_RtoC(R)
;
; MODIFICATION HISTORY: RCS_ID="$Id: thermistor_rtoc.pro,v 1.2 2019/03/13 10:55:42 spenton Exp $"
;   2018-04-11: JWH
;-
function Thermistor_RtoC, R
  ; From George
  Tk = 273.15d
  a = 1.129241D-3           ; manufacturer's values for a 10K resistor
  b = 2.341077D-4
  c = 8.775468D-8

  degrees_C = 1d / (a + b*alog (R) + c*alog (R)^3) - Tk
  ; is this the solution from the Steinhart & Hart Eqn ignoring T^5 term?
  return, degrees_C
END
