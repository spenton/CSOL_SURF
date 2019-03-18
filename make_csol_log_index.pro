;+
; NAME:
;  make_csol_log_index
;
; PURPOSE:
;   A procedure to construct an index of CSOL housekeeping files stored as .SAV files.  Output is an array of structures that gives the
;   start and stop times of the housekeeping files and a complete path to the target image
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
; MODIFICATION HISTORY: RCS_ID="$Id: make_csol_log_index.pro,v 1.1 2018/12/30 00:27:42 spenton Exp $"
;   2018-05-11: JWH
;-

pro make_csol_log_index
  ;DATA PATHS ARE CURRENTLY HARD CODED
  mypath=get_surf_dir(/data)+'IDL_structure_files/CSOL_logs/'
  cd, mypath
  file_arr=file_search(mypath,'*.sav',count=filecnt)
  csol_log_index=[]
  rng_file= create_struct('start_time',0.D0 ,'stop_time',0.D0 , 'log_file','')
  for i=0,filecnt-1 do begin
    restore,file_arr[i]
    n_hkrec=n_elements(csol_hk)
    juld=jd_from_hk( csol_hk, n_hkrec)
    rng_file.start_time = juld[0]
    rng_file.stop_time = juld[-1]
    rng_file.log_file = file_arr[i]
    csol_log_index=[csol_log_index,rng_file]
  endfor

  filenm=get_surf_dir(/data)+'IDL_structure_files/csol_log_index.sav'
  save,file=filenm,csol_log_index
end
