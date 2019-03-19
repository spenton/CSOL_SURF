pro csol_look,verbose=verbose,histo=histo,debug=debug
	if n_elements(debug) ne 1 then debug=0
	if n_elements(histo) ne 1 then histo=0
	if n_elements(verbose) ne 1 then verbose=0
	file=csol_pickfile()
	image=csol_hex2img(file)
	;csol_viewer,image,verbose=1
	csol2_viewer,image,verbose=1
	if histo then begin
		plothist,image,counts,n
		FDECOMP, file, disk, dir, name
 		help,file,disk,dir,name
		p=plot(counts,n,font_name='Times',font_size=18,dimensions=[1000,800],xtitle='Counts',ytitle='N (p)',$
		title='Histogram of counts in the CSOL image '+name)
	endif
end
