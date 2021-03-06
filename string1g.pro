;+
; NAME:   STRING1G
;
; PURPOSE:
;    This function returns a converts the input to a floating point number and
;    returns it as a compressed (white space removed) string or string array.
;
; CALLING SEQUENCE:
;    output_string=string1g(number_of_any_type,format=format)
;
; OPTIONAL INPUT PARAMETERS:
;    format - Input the desired format code, default is the generic "(G)"
;
; OUTPUT PARAMETERS:
;   none
;
; OPTIONAL OUTPUT PARAMETERS:
;      NONE
;
; EXAMPLE:
;	help,string1g(!dpi,format='(G)')
;	    STRING    = '3.141592653589793'
;
;	help,string1g(lindgen(10))
;	   STRING    = Array[10]
;
;	print,string1g(lindgen(5),format='(G8.4)')
;	0.000 1.000 2.000 3.000 4.000
;
; COMMON BLOCKS:
;      NONE
;
; NOTES:
;      Works with scalar or array inputs.
;
; REVISION HISTORY:
;   Revision: $Id: string1g.pro,v 1.1 2018/12/30 00:27:42 spenton Exp $
;
;	SVP: Original Version
;-
;*****************************************************************************
function string1g,f,format=format
	if not keyword_set(format) then format='(G)'
	s_out=string(double(f))
	nf=ulong64(n_elements(f))
	for j=ulong64(0),ulong64(nf-1) do $
		s_out[j]=strtrim(string(f[j],format=format),2)
	return,s_out.compress()
end
