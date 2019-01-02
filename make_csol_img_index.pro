;***** Contruct indices and file lists *****
;+
; NAME:
;   make_csol_img_index
;
; PURPOSE:
;   A prceedure to construct an index of a CSOL image files.  An array of structures that gives the image timestamp and a complete path to the target image
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
; MODIFICATION HISTORY: RCS_ID="$Id: make_csol_img_index.pro,v 1.1 2018/12/30 00:27:42 spenton Exp $"
;   2018-05-11: JWH
;-
pro make_csol_img_index
  ;DATA PATHS ARE CURRENTLY HARD CODED
  rootdir=get_surf_dir(/data)
  log_path='IDL_structure_files/CSOL_logs/'
  cd,rootdir+log_path
  csol_hk_filenm=file_search('*.sav',count=n_groups)

  csol_img_index=[]
  date_fileinfo =create_struct('juld',0.D0 , 'img_file','')

  for i=0,n_groups-1 do begin
    restore, csol_hk_filenm[i]
    n_hkrec=n_elements(csol_hk)
    juld=jd_from_hk(csol_hk,n_hkrec)
    target_path=repstr(csol_hk_filenm[i],'_csolhk.sav','/')
    img_filenm=filenm_from_hk('',csol_hk)

    for j=0,n_hkrec-1 do begin
      target_filenm=rootdir+'csol/'+target_path+img_filenm[j]
      valid=file_test(target_filenm)
      if valid eq 1 then begin
        date_fileinfo.juld=juld[j]
        date_fileinfo.img_file=target_filenm
        csol_img_index=[csol_img_index,date_fileinfo]
      endif else bad_filenm=[bad_filenm,target_filenm]
    endfor ;on j
  endfor ;on i

  srt             = sort(csol_img_index.juld)
  csol_img_index = csol_img_index[srt]

  filenm=get_surf_dir(/data)+'IDL_structure_files/csol_img_index.sav'
  save,file=filenm,csol_img_index

end
