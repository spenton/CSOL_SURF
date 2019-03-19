function csol_pickfile,verbose=verbose,dropbox=dropbox,laspstore=laspstore
	pickpath=get_surf_dir(dropbox=dropbox,laspstore=laspstore)
	a=dialog_pickfile(path=pickpath,filter="FrameData*.hex",title="Please Select a CSOL .hex file",/read,/must_exist,/fix_filter)
	return,a
end
