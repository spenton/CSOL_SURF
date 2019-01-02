FUNCTION Thermistor_RtoC, R
  ; Function to convert input thermistor resistance [*] to temperature [C]
  ; From George
  ; Initialize
  Tk = 273.15d
  a = 1.129241D-3           ; manufacturer's values for a 10K resistor
  b = 2.341077D-4
  c = 8.775468D-8

  degrees_C = 1d / (a + b*alog (R) + c*alog (R)^3) - Tk
  ; is this the solution from the Steinhart & Hart Eqn ignoring T^5 term?
  return, degrees_C
END