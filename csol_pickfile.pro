function csol_pickfile,verbose=verbose,dropbox=dropbox,laspstore=laspstore
	a=pickfile(path=get_surf_dir(/drop),filter=".hex",title="Please Select a CSOL .hex file")
	return,a
end
