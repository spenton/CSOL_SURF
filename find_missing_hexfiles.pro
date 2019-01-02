function find_missing_hexfiles, file_path, filelist_hk
  ;utility used for finding mission hex files from a directory with images in it based on a filelist genereated from housekeeping entries
  n_hkrecs=n_elements(filelist_hk)
  file_list=file_search(file_path,'*.hex')
  missing_files=[]
  for i=0,n_hkrecs-1 do begin
    ok=where(strmatch(file_list, filelist_hk[i], /FOLD_CASE) eq 1)
    if ok eq -1 then missing_files=[missing_files,filelist_hk[i]]
  endfor

  sz_missing=n_elements(missing_files)
  if sz_missing eq 0 then sz_missing =-1
  return, missing_files

end
