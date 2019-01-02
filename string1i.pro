;+
; NAME:   STRING1I
;
; PURPOSE:
;    This function returns a converts the input to an integer and returns it as a
;    compressed (white space removed) string or string array.
;
; CALLING SEQUENCE:
;    output_string=string1i(number_of_any_type,format=format)
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
;	help,string1i(!dpi,format='(I4)')
;	    STRING    = '3.1416'
;
;	help,string1i(lindgen(10))
;	   STRING    = Array[10]
;
;	print,string1i(lindgen(5),format='(F5.1)')
;	   0.0 1.0 2.0 3.0 4.0
;
; COMMON BLOCKS:
;      NONE
;
; NOTES:
;      Works with scalar or array inputs.
;
; REVISION HISTORY:
;   Revision: $Id: string1i.pro,v 1.1 2018/12/30 00:27:42 spenton Exp $
;
;	SVP: Original Version
;-
;*****************************************************************************
function string1i,f,format=format,fix=fix,round=round
	if n_elements(fix) ne 1 then fix=0
	if n_elements(round) ne 1 then round=(fix ? 0 : 1) ; round is the default
	if not keyword_set(format) then format='(I)';
	; use double() and the /L64 keyword just in case its a really big integer
	s_out=strtrim(string((fix ? fix(double(f)) : round(double(f),/L64)),format=format),2)
	return,s_out.compress()
end
