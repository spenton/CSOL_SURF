;+
;
; MODIFICATION HISTORY: RCS_ID="$Id: get_surf_dir.pro,v 1.3 2019/03/18 19:30:02 spenton Exp spenton $" RCS_ID=$Id: get_surf_dir.pro,v 1.3 2019/03/18 19:30:02 spenton Exp spenton $
;-
function get_surf_dir,txt=txt,logs=logs,basedir=basedir,data=data,init=init,dropbox=dropbox,verbose=verbose,laspstore=laspstore
	if n_elements(dropbox) ne 1 then dropbox=1
	if n_elements(laspstore) ne 1 then laspstore=(dropbox ? 0 : 1)
	if n_elements(verbose) ne 1 then verbose=0
	if n_elements(init) ne 1 then init=0
	CSD=getenv("CSOL_SURF_DIR")
	laspstore_dir="/Volumes/projects/Phase_Development/SOLSTICE_RI/CSOL_SURF/2019_Mar_SURF/SURF_2019/Data/CSOL/"
	dropbox_dir="~/Dropbox_CSOL_SURF/CSOL_SURF Dropbox/CSOL_SURF/2019_Mar_SURF/SURF_2019/Data/CSOL/"
	laspdir=(dropbox ? dropbox_dir : laspstore_dir)
	if n_elements(basedir) ne 1 then $
		basedir=(CSD ne "" ? CSD : laspdir)

	if n_elements(txt) ne 1 then txt=0
	if n_elements(logs) ne 1 then logs=0
	if n_elements(data) ne 1 then data=0
	if basedir eq "" then message,/info,'The environment variable CSOL_SURF_DIR must be set, or the BASEDIR keyword must be specified'
	if verbose then message,/info,'The baseline directory has been set to '+basedir
	basedir+=(data ? 'Data/' :'')
	compile_opt idl2
	;
	if init then begin
		; test for a few directories, and make them if they do not exist
		;
	endif
	return,basedir
end
