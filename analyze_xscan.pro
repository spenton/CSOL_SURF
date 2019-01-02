;+
; RCS_ID = $ID$
;-
pro analyze_xscan,verbose=verbose,n_xpix=n_xpix,n_ypix=n_ypix,xscan_savefile=xscan_savefile,$
	surf_log_indexfile=surf_log_indexfile,wait_time=wait_time
	if n_elements(wait_time) ne 1 then wait_time=0.25
	if n_elements(surf_log_indexfile) ne 1 then surf_log_indexfile='surf_log_index.sav'
	if n_elements(csol_img_indexfile) ne 1 then csol_img_indexfile='csol_img_index.sav'

	if n_elements(xscan_savefile) ne 1 then xscan_savefile='xscan_20180418_csolhk.sav'
	if n_elements(n_ypix) ne 1 then n_ypix=1504
	if n_elements(n_xpix) ne 1 then n_xpix=2000
	if n_elements(verbose) ne 1 then verbose=1

	;retrieve the specific CSOL housekeeping log file to be analyzed
	restore,get_surf_dir(/data)+'IDL_structure_files/CSOL_logs/'+xscan_savefile,verbose=verbose

	;retrieve the index of all image files
	restore, get_surf_dir(/data)+'IDL_structure_files/'+csol_img_indexfile,verbose=verbose
	n_hkrec=n_elements(csol_hk)
	csol_juld=jd_from_hk(csol_hk,n_hkrec)    ;get julian day from columns in CSOL housekeeping file

	ok=where(csol_img_index.juld ge csol_juld[0] and $
	  csol_img_index.juld le csol_juld[-1],imgcnt)
	img_filelist=csol_img_index[ok]                           ;specif set of image files covered in housekeeping record
	restore, get_surf_dir(/data)+'IDL_structure_files/'+surf_log_indexfile,verbose=verbose

	;get the SURF log corresponding to CSOL housekeeping record
	val=min(abs(surf_log_index.start_time - csol_juld[0]),ind)
	restore, surf_log_index[ind].log_file,verbose=verbose

	surf_aligned = align_surf2img(surf_log , csol_hk)     ; get SURF data for each explicit image in img_filelist
	;help, surf_aligned,/struct

	lgt_ind=where(surf_aligned.v20_21_22 eq 7.,complement=drk_ind,lgt_cnt,ncomplement=drk_cnt)

	drk=fltarr(n_xpix,n_ypix)
	for i=0,drk_cnt-1 do begin
		drk+=csol_hex2img(img_filelist[drk_ind[i]].img_file,1)
		;im=image(csol_hex2img(img_filelist[drk_ind[i]].img_file,1),rgb_table=46,title=string(drk_ind[i]))
	endfor
	drk/=float(drk_cnt)

	fuv_line=fltarr(lgt_cnt,n_xpix)
	for i=0,lgt_cnt-1 do begin
		lgt=csol_hex2img(img_filelist[lgt_ind[i]].img_file,1)-drk
		slith_info=get_slith_info(lgt)
		for jj=0,n_xpix-1 do begin
			resistant_mean,lgt[jj,slith_info.fuv_bot[jj]:slith_info.fuv_top[jj]],2.5,mymean
			fuv_line[i,jj]=mymean
		endfor
		fuv_line[i,*]/=surf_aligned[lgt_ind[i]].bc
		wait,wait_time & message,/info,string1i(i)
	endfor

	qq=surf_aligned[lgt_ind].xpos[1000]
	edge_ind=where(qq[1:-1]-qq[0:-2] gt 0.025)
	edge_ind=[edge_ind,lgt_cnt-1]
	n_edge=n_elements(edge_ind)
	haystack=fltarr(n_edge,n_xpix)
	cardinalpos=surf_aligned[edge_ind].xpos[1000]
	strt_ind = 0

	for i=0,n_edge-1 do begin
		haystack[i,*]=median(fuv_line[strt_ind:edge_ind[i],*],/even,dimension=1)
		strt_ind=edge_ind[i]+1
	endfor

	haystack=haystack[2:-1,*]
	cardinalpos=cardinalpos[2:-1]
	mnmx=minmax(cardinalpos) & n_intp=ceil((mnmx[1]-mnmx[0])/0.005)
	cardinalpos_intp=cardinalpos[0]+0.005*findgen(n_intp)

	midpnt_cntr=fltarr(n_xpix)
	zedpnt_cntr=fltarr(n_xpix)

	for i=0,n_xpix-1 do begin
		haystack_intp=interpol(binomialsmooth(haystack[*,i],4),cardinalpos,cardinalpos_intp,/spline)
		mnmx=minmax(deriv(haystack_intp),mnmx_ind)
		midpnt=(cardinalpos_intp[mnmx_ind[0]]+cardinalpos_intp[mnmx_ind[1]])/2.
		midpnt_cntr[i]=midpnt
		val=min(abs(cardinalpos_intp-midpnt),midind)
		linco=linfit(cardinalpos_intp[midind-10:midind+10],deriv(haystack_intp[midind-10:midind+10]))
		zedpnt=(-1.)*linco[0]/linco[1]
		zedpnt_cntr[i]=zedpnt
	endfor

	message,/info, 'xscan center = ',median((midpnt_cntr+zedpnt_cntr)/2.,/even)

end
