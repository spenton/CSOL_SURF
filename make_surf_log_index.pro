;+
; NAME:
;  make_surf_log_index
;
; PURPOSE:
;   A prceedure to construct an index of SURF logs stored as .SAV files.  An array of structures that gives the
;   start and stop times of the log file and a complete path to the target image
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
; MODIFICATION HISTORY: RCS_ID="$Id: make_surf_log_index.pro,v 1.1 2018/12/30 00:27:42 spenton Exp $"
;   2018-05-11: JWH
;-
pro make_surf_log_index
  ;data paths are hard-coded
  mypath=get_surf_dir(/data)+'IDL_structure_files/SURF_logs/'
  cd, mypath
  file_arr=file_search(mypath,'*.sav',count=filecnt)
  surf_log_index=[]
  rng_file= create_struct('start_time',0.D0 ,'stop_time',0.D0 , 'log_file','')
  for i=0,filecnt-1 do begin
    restore,file_arr[i]
    mnmx=minmax(surf_log.date_julian)
    rng_file.start_time = mnmx[0]
    rng_file.stop_time = mnmx[1]
    rng_file.log_file = file_arr[i]
    surf_log_index=[surf_log_index,rng_file]
  endfor

  filenm=get_surf_dir(/data)+'/IDL_structure_files/surf_log_index.sav'
  save,file=filenm,surf_log_index

end
