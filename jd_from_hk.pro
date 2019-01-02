;***** Utilites *****
;+
; NAME: jd_from_hk
;
; PURPOSE:
;   A function that extracts xtract a Julian day from a CSOL housekeeping structure

;
; INPUTS:
;   An array of structures of CSOL housekeeping data (produced in the procedure read_csol_metadatalog
;   the size of the CSOL housekeeing record ( an output from read_csol_metadatalog
;
; OPTIONAL INPUTS:
; None
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   Returns an array of Julian days with the same number of elements as the CSOL housekeeping record
;
; OPTIONAL OUTPUTS:
;   None
;
; PROCEDURES/FUNCTIONS CALLED:
;   datetime_adapt.pro
;
; RESTRICTIONS:
;   None
;
; EXAMPLE:
;   csol_jd=jd_from_hk(csol_hk, n_hkrec)
;
; MODIFICATION HISTORY: RCS_ID="$Id: jd_from_hk.pro,v 1.1 2018/12/30 00:27:42 spenton Exp $"
;   2018-04-11: JWH
;-

function jd_from_hk, hk_struct, n_hkrec
  ;example
  ;juld=jd_from_hk(csol_hk,n_hkrec)

  outarr=dblarr(6,n_hkrec)
  uu=long(hk_struct.(0))
  uu=strtrim(string(uu),2)
  outarr[0,*]=double(strmid(uu,0,4))
  outarr[1,*]=double(strmid(uu,4,2))
  outarr[2,*]=double(strmid(uu,6,2))
  uu=long(hk_struct.(1))
  uu=strtrim(string(uu),2)
  uu='000000'+ uu
  uu = strmid(uu, 5 , /REVERSE_OFFSET )
  outarr[3,*]=double(strmid(uu,0,2))
  outarr[4,*]=double(strmid(uu,2,2))
  outarr[5,*]=double(strmid(uu,4,2))
  juld=datetime_adapt(outarr,from='ymdhms',to='jd')

  return, juld
end
