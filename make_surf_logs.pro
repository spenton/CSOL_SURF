;+
; NAME:
;   make_surf_logs
;
; PURPOSE:
;   A procedure to convert SURF text logs into an array of structures and save the result in a .SAV format in a hard coded location
;
; INPUTS:
;   Currently no inputs, but routine needs to be made to test for the existence of a directory or create it if it does not exist
;
; OPTIONAL INPUTS:
; None
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   none
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
;
; MODIFICATION HISTORY: RCS_ID="$Id: make_surf_logs.pro,v 1.1 2018/12/30 00:27:42 spenton Exp $"
;   2018-05-11: JWH
;-

pro make_surf_logs,indir,outdir
  datadir=get_surf_dir(/data)
  rootdir=datadir+'SURFER/'
  outdir=datadir+'IDL_structuure_files/SURF_logs/'

  cd,rootdir
  surf_log_filenm=file_search('*.txt',count=surf_log_cnt)

  for i=0,surf_log_cnt-1 do begin
    surf_log=read_surf_log_jwh(surf_log_filenm[i])
    outfilenm=outdir+repstr(surf_log_filenm[i],'txt','sav')
    save,file=outfilenm,surf_log
  endfor

end
