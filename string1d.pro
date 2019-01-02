;+
; NAME:   STRING1D
;
; PURPOSE:
;    This function returns a converts the input to a floating point number and
;    returns it as a compressed (white space removed) string or string array.
;
; CALLING SEQUENCE:
;    output_string=string1d(number_of_any_type,format=format)
;
; OPTIONAL INPUT PARAMETERS:
;    format - Input the desired format code, default is the generic "(F)"
;
; OUTPUT PARAMETERS:
;   none
;
; OPTIONAL OUTPUT PARAMETERS:
;      NONE
;
; EXAMPLE:
;	help,string1d(!dpi,format='(D10.4)')
;	    STRING    = '3.1416'
;
;	help,string1d(lindgen(10))
;	   STRING    = Array[10]
;
;	print,string1d(lindgen(5),format='(D5.1)')
;	   0.0 1.0 2.0 3.0 4.0
;
; COMMON BLOCKS:
;      NONE
;
; NOTES:
;      Works with scalar or array inputs.
;
; REVISION HISTORY:
;   Revision: $Id: string1d.pro,v 1.1 2018/12/30 00:27:42 spenton Exp $
;
;	SVP: Original Version
;-
;*****************************************************************************
function string1d,f,format=format
	if not keyword_set(format) then format='(D)'
	s_out=string(double(f))
	nf=ulong64(n_elements(f))
	for j=ulong64(0),ulong64(nf-1) do $
		s_out[j]=strtrim(string(f[j],format=format),2)
	return,s_out.compress()
end
