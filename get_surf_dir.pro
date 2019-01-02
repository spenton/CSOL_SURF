;+
;
; MODIFICATION HISTORY: RCS_ID="$Id: get_surf_dir.pro,v 1.1 2018/12/30 00:27:42 spenton Exp $" RCS_ID=$Id: get_surf_dir.pro,v 1.1 2018/12/30 00:27:42 spenton Exp $
;-
function get_surf_dir,txt=txt,logs=logs,basedir=basedir,data=data,init=init
	if n_elements(init) ne 1 then init=0
	if n_elements(basedir) ne 1 then basedir='/Volumes/LASP_DTrans/SURF/'
	if n_elements(txt) ne 1 then txt=0
	if n_elements(logs) ne 1 then logs=0
	if n_elements(data) ne 1 then data=0
	basedir+=(data ? 'Data/' :'')
	compile_opt idl2
	;
	if init then begin
		; test for a few directories, and make them if the do not exist
		;
	endif
	return,basedir
end
