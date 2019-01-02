;+
; NAME:
;   make_csol_logs
;
; PURPOSE:
;   A prceedure to convert CSOL .CVS files into an array of structures and save the result in a .SAV format in a hard coded location
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
; MODIFICATION HISTORY: RCS_ID="$Id: make_csol_logs.pro,v 1.1 2018/12/30 00:27:42 spenton Exp $"
;   2018-05-11: JWH
;-

pro make_csol_logs
  ;data paths are hard-coded

  rootdir=get_surf_dir(/data)+'CSOL/'
  outdir=get_surf_dir(/data)+'/IDL_structure_files/CSOL_logs/'
  rootlen=strlen(rootdir)
  suffixlen=strlen('/MetaDataLog.csv')
  cd,rootdir
  csol_log_filenm=file_search(rootdir,'MetaDataLog.csv',count=csol_log_cnt)

  for i=0,csol_log_cnt-1 do begin
    read_csol_metadatalog,csol_log_filenm[i], csol_hk,n_hkrecs,n_hktags

    outfilenm=strmid(csol_log_filenm[i],rootlen)
    outlen=strlen(outfilenm)
    outfilenm=outdir + strcompress(strmid(outfilenm,0,outlen-suffixlen),/remove_all) + '_csolhk.sav'

    save,file=outfilenm,csol_hk
  endfor

end
