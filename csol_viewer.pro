;$Id: csol_viewer.pro,v 1.2 2019/03/19 19:40:15 spenton Exp spenton $
;
;+
; csol_viewer
;
; Routine to interactively examine raw CSOL (HEX) data files
;
; CALLING SQUENCE:
;   csol_viewer
;      or
;   csol_viewer,data,header
;      or
;   csol_viewer,file=filename
;
; OPTIONAL INPUTS:
;   data - data array
;   header - header (STRARR)
;   filename - full path to file to open (expects HEX)
;
; INTERACTIVE INPUT:
;   Image Windows:
;   lower - display of whole image rebinned to 2000 x 1504 size
;   upper left - scrollable display of whole image without any rebinning.
;   upper right - zoomed image display
;
;   To select region viewed in upper right image use the scroll bars
;   to the botton and right of the image or place cursor in bottom image
;   and push any mouse button.
;
;   To select region for the zoomed image. Place cursor in the upper
;   left window and push any mouse button.
;
;   MENU BUTTONS:
;    FILE/READ - to specify and read a CSOL fits or RAW file
;    FILE/PS output - to write postscript file of screen contents
;    FILE/EXIT - to exit the program
;
;    COLORS - to change color table
;    CONTRAST - to change intensity scaling function (linear, log, square
;      root, or histogram equalization)
;    HISTOGRAM - to plot histogram of data values
;    STATS - to generate statistics of selected region. To generate
;      statistics for a box or draw region, follow directions
;      given above upper left window.
;    PLOT - to plot rows, columns, row sums, column sums, or cross
;      sectional plots. After selecting type of plot, follow the
;      directions given above the upper left window.  If a second
;      plot is made without closing the first plot window, it will
;      be overplotted on the existing plot.
;    CLEAR OVERLAY - to clear overlay lines written on the image
;    HEADER - to display contents of the FITS header
;    SURFACE - to generate surface plot of zoomed region
;   CONTOUR - to generate a contour plot of the zoomed region
;   GAUSSFIT - to fit gaussians in both directions to zoomed region
;   TTAG - time tag processing, plots/movies/good time editing
;   ZOOM - to change the zoom factor of the zoom window
;
;    TEXT Boxes
;   Min - image minimum - used to set background level for display and
;      for computing statistics of selected regions
;   Max - image maximum - used to set maximum for image scaling
;   Reset Min/Max - sets Min and Max to the original image Min/Max
;   X - displays x position of cursor in image pixel coordinates
;   y - displays y position of cursor in image pixel coordinates
;   Val - displays data value for the pixel at the specified x/y
;
; HISTORY:
;       modified from Lindler's FUSE_SCAN, SBeland CASA, Apr. 2000
;   version 1 D. Lindler, Sept, 1999
;   too many changes to keep track here: look at the rcslog
;   SBeland Aug. 2008: will prompt user for wich X, Y variable to display if many
;   SBeland Aug. 2008: as part of the stats, will now display rates (estimated and actual (from events))
;   SBeland Sept 2008: modified to switch between USER and DETECT coord
;-
; ===========================================================
;

@xttag_CSOL

; =========================================================== readHEX
;
function readHEX,filename, image=image, dtype=dtype,whichrot=whichrot
	if n_elements(whichrot) ne 1 then whichrot=1
	image=csol_hex2img(filename,whichrot=whichrot)
	status=1
	message,/info,'Reading HEX'
	dtype=size(image,/type)
	return, status
end
function HEX_openCSOL, info, coord=CSOLcoord,verbose=verbose
   if n_elements(verbose) ne 1 then verbose=0
   if n_elements(info) eq 0 then return, 'Missing info variable'

   fdecomp, info.filename, disk, dir, fname, ext
   msg=''

   ; select the image sequence
   img_seq = 1
   num_img = 1

   instrument = 'CSOL'   ;CSOL
   status = readHEX(info.filename,image=image, dtype=dtype)
   IF status ne '' THEN BEGIN
      ; problem reading the image sequence from the file
      print,string(7b)
      return, 'Error reading '+fname+'.'+ext+' ('+$
         strtrim(string(img_seq),2)+') :'+STRING(10B)+STRING(13B)+status
   ENDIF
   orig=image


   ; These lines were moved from line 114 above
   ; It is not the most memory efficient way of doing this since the new and
   ; the previous images are saved in memory but we need to keep the old all
   ; the way here in case the user Cancels when loading a raw file.
   ; This way the old images stays on screen

   info.HEX_imgseq=img_seq

   IF PTR_VALID(info.orig) THEN PTR_FREE,info.orig
   if PTR_VALID(info.data) then PTR_FREE, info.data
   if PTR_VALID(info.header) then PTR_FREE, info.header

	info.little_nx=240.0
	info.little_ny=240.0
	; destroy previous instance of window before creating a new one
	if info.little_window ne -1 then widget_control, info.little_window, /DESTROY
	info.little_window = widget_draw(info.base_nuv,uvalue='LITTLE_WINDOW',retain=2, $
	 xsize=info.little_nx,ysize=info.little_ny,/button_events,/motion)
	widget_control,info.little_window,get_value=little_id
	info.little_id = little_id

   ; form the data structure
   info.header = PTR_NEW('')
   info.orig = PTR_NEW(orig, /NO_COPY)
   info.otherseg = PTR_NEW(otherseg, /no_copy)
   info.data = PTR_NEW({  $
      xttag:PTR_NEW(), $
      yttag:PTR_NEW(), $
      timetag:PTR_NEW(), $
      xbin: 1, $
      ybin: 1, $
      gtime1: PTR_NEW(), $
      gtime2: PTR_NEW(), $
      pha: PTR_NEW(), $
      phamask: PTR_NEW(), $
      xname:'RAWX', $
      yname:'RAWY' $
   })

   (*info.data).xttag = PTR_NEW(xttag, /no_copy)
   (*info.data).yttag = PTR_NEW(yttag, /no_copy)
   (*info.data).timetag = PTR_NEW(timetag, /no_copy)
   (*info.data).xbin = xbin
   (*info.data).ybin = ybin
   (*info.data).gtime1 = PTR_NEW(gtime1, /no_copy)
   (*info.data).gtime2 = PTR_NEW(gtime2, /no_copy)
   (*info.data).pha = PTR_NEW(pha, /no_copy)
   (*info.data).phamask = PTR_NEW([-1], /no_copy)
   info.timerange=timerange

   info.xoff=0.0
   info.yoff=0.0
   info.wtitle = fname+'.'+ext+' ('+strtrim(string(info.HEX_imgseq),2)+') '+segment
      widget_control, info.menuDispDetCoord,  sensitive=0
      widget_control, info.menuDispUserCoord, sensitive=1
   info.wtitle=info.wtitle+'  '+(*info.data).xname+' '+(*info.data).yname
   widget_control,info.top_base,tlb_set_title=info.wtitle
   csol_viewer_display, info.orig, info=info

   widget_control, info.menuDispReload, sensitive=1

END

; =========================================================== HEX_openCSOL
;
function HEX_open,info,whichrot=whichrot,nrows=nrows,ncols=ncols,verbose=verbose
		if n_elements(verbose) ne 1 then verbose=1
		if n_elements(nrows) ne 1 then nrows=1504L
		if n_elements(ncols) ne 1 then ncols=2000L
		if n_elements(whichrot) ne 1 then whichrot=1

;   whichrot is the orientation of the final image, for subsequent processing use whichrot =1
;  whichrot=6 make the image match what appears in Alan Sims' MATLAB images

   if n_elements(info) eq 0 then return, 'Missing info variable'

	instrument='CSOL'
	message,'MADE it to HEX_open'
	help,info,/str
	stop

	status=readHEX(info.filename,image=orig,whichrot=whichrot)
 ; orig is already a 2D image, clear the ttag variable
		maxx = n_elements(orig[*,0])
		maxy = n_elements(orig[0,*])
		info.xoff=0.0
		info.yoff=0.0

      ;transform orig to a pointer
      info.orig = PTR_NEW(orig, /NO_COPY)
      if SIZE(ttag, /TNAME) eq 'STRUCT'then begin
         PTR_FREE, info.data
         info.data = PTR_NEW(ttag, /NO_COPY)
      endif

      ; disable the other segment menu for all
      info.HEXseg=''
      widget_control, info.menuDispOtherSeg,  sensitive=0
      widget_control, info.menuDispReload,  sensitive=0

      sz=size(*info.orig)

         info.little_nx=200.0
         info.little_ny=150.4
         ; destroy previous instance of window before creating a new one
         if info.little_window ne -1 then widget_control, info.little_window, /DESTROY
         info.little_window = widget_draw(info.base_nuv,uvalue='LITTLE_WINDOW',retain=2, $
            xsize=info.little_nx,ysize=info.little_ny,/button_events,/motion)
         widget_control,info.little_window,get_value=little_id
         info.little_id = little_id

       info.header = PTR_NEW(header, /no_copy)

      ; check if we have a CSOL file and enable USER/DETECT coord function
;	   widget_control, info.menuDispDetCoord,  sensitive=1
;	   widget_control, info.menuDispUserCoord, sensitive=0
      ttnames=tag_names(*info.data)
      tpos = where(strpos(ttnames,'XNAME') ge 0, count)
      if count gt 0 then $
         info.wtitle=info.wtitle+'  '+(*info.data).xname+' '+(*info.data).yname
      widget_control,info.top_base,tlb_set_title=info.wtitle

      csol_viewer_display, info.orig, info=info
      widget_control, info.menuDispReload, sensitive=1
	help,info,/str
   return,msg
end

;
; csol_viewer Event Handler
; =========================================================== csol_viewer_EVENT
;
; csol_viewer Event Handler
;
pro csol_viewer_event,event

common csol_viewer_path, hex_path, raw_path,verbose=verbose
   if n_elements(verbose) ne 1 then verbose=0

   widget_control, event.top, get_uvalue=info, /no_copy
   widget_control,event.id,get_uvalue=uvalue

   ; tvlct,info.rsave,info.gsave,info.bsave

   if TAG_NAMES(event, /STRUCTURE_NAME) eq 'WIDGET_BASE' AND $
      event.id EQ info.top_base then begin       ; The window has been sized

     ; Determine the change in the window size
     WIDGET_CONTROL, info.top_base, TLB_GET_SIZE=windowSize
     deltaX = windowSize[0] - info.top_base_windowSize[0]
     deltaY = windowSize[1] - info.top_base_windowSize[1]
     if deltax eq 0.0 and deltay eq 0.0 then GOTO, SKIP

     ; determine the size of screen to make widget fit on screen
     ; specially for 1024x768 laptops
     device,get_screen_size=screen_size
     if screen_size[1] ge 1024 then begin
        min_ysize=542
     endif else begin
        min_ysize = screen_size[1] / 2
     endelse

     ; resize the big window only
     geom = WIDGET_INFO(info.big_window, /GEOMETRY)
     newXSize = ((geom.scr_xsize + deltaX) > 750) < (info.ns+30)
     newYSize = ((geom.scr_ysize + deltaY) > min_ysize) < (info.nl+30)
     ; Resize the text widget accordingly
     widget_control,info.big_window, scr_xsize=newXsize,scr_ysize=newYsize
    info.draw_window_size = [newXsize, newYsize]

     ; get the size of the new window
     WIDGET_CONTROL, info.top_base, TLB_GET_SIZE=windowSize
     info.top_base_windowSize=windowSize
     GOTO, SKIP
  ENDIF

  case uvalue of

   'OPENCSOL': begin
		 message,/info,'UVALUE = OPENCSOL'
         widget_control,/hourglass
         if n_elements(csol_path) eq 0 then  begin
         	csol_path=get_surf_dir(dropbox=dropbox,laspstore=laspstore)
        endif
         file = dialog_pickfile(title='Select CSOL/SURF HEX file(s)',/must_exist, path=csol_path, $
                 filter='*.hex*', get_path=path)
         if file[0] eq '' THEN GOTO, SKIP
         csol_path = path
         info.filename = file[0]
         instrument='CSOL'
         ; disable the event handler
         WIDGET_CONTROL, info.top_base, EVENT_FUNC=''
         status = HEX_openCSOL(info,verbose=verbose)
         ; re-enable the event handler
         WIDGET_CONTROL, info.top_base, EVENT_PRO='csol2_viewer_EVENT'
         if status ne '' then r=dialog_message(dialog_parent=info.top_base, status, /error)
         widget_control,/hourglass
      END

   'PRINT':   BEGIN
         csol_viewer_ps, /PRINT, /REVERSED, info=info
         END

   'PS_BW':   BEGIN
         csol_viewer_ps, info=info
         END

   'PS_BWR':  BEGIN
         csol_viewer_ps, /REVERSED, info=info
         END

   'PS_COLOR': BEGIN
         csol_viewer_ps, /COLOR, info=info
         END

   'MK_PNG': BEGIN
         csol_viewer_ps, /COLOR, /PNG, /REVERSED, info=info
         END

   'MK_JPG': BEGIN
         csol_viewer_ps, /COLOR, /JPG, /REVERSED, info=info
         END

   'EXIT': BEGIN
         IF PTR_VALID(info.orig) THEN PTR_FREE,info.orig
         if PTR_VALID(info.data) then PTR_FREE, info.data
         if PTR_VALID(info.big_image) then PTR_FREE, info.big_image
         if PTR_VALID(info.little_image) then PTR_FREE, info.little_image
         if PTR_VALID(info.zoom) then PTR_FREE, info.zoom
         if PTR_VALID(info.stars) then PTR_FREE, info.stars
         ; clean up anything else
         HEAP_GC, /PTR
         widget_control,event.top,/destroy
         RETURN
         END

   'DISP_COLOR': begin
      xloadct,/modal,group=event.top
      tek_color, 0, 8
      tvlct,rsave,gsave,bsave,/get
      info.rsave=rsave & info.gsave=gsave & info.bsave=bsave
      end

   'DISP_LINEAR': begin
      info.scale_type = 'Linear'
      csol_viewer_display, info=info
      end

   'DISP_LOG': begin
      info.scale_type = 'Log'
      csol_viewer_display, info=info
      end

   'DISP_SQRT': begin
      info.scale_type = 'Sqrt'
      csol_viewer_display, info=info
      end

   'DISP_HISTEQ': begin
      info.scale_type = 'Hist. Eq.'
      csol_viewer_display, info=info
      end

   'DISP_RELOAD' : BEGIN
         WIDGET_CONTROL, info.top_base, EVENT_FUNC=''
         status = HEX_openCSOL(info,verbose=verbose)
         ; re-enable the event handler
         WIDGET_CONTROL, info.top_base, EVENT_PRO='csol_viewer_EVENT'
         if status ne '' then r=dialog_message(dialog_parent=info.top_base, status, /error)
         widget_control,/hourglass
      END

   'DISP_ORIENTATION' : BEGIN
      csol_viewer_plot_orient, info=info, /orient
      END

   'DISP_ZOOM': begin
      widget_control,event.id,get_value=val
      val = FIX(val)
      x = info.xoffzoom + (info.zoom_width/2)/info.zoom_factor
      y = info.yoffzoom + (info.zoom_width/2)/info.zoom_factor
      info.zoom_factor = val
      info.xoffzoom = (x - (info.zoom_width/2)/info.zoom_factor)>0
      info.yoffzoom = (y - (info.zoom_width/2)/info.zoom_factor)>0
      csol_viewer_zoom, info.orig, zoom_img, info=info
      if PTR_VALID(info.zoom) then PTR_FREE,info.zoom
      info.zoom = PTR_NEW(zoom_img, /NO_COPY)
      csol_viewer_scale, info.zoom_id, info.zoom, imin=info.omin, imax=info.omax, scale=info.scale_type, ncolors=info.ncolors
      end

   'DISP_REGION': csol_viewer_set_region,group=event.top,info=info

   'DATA_HIST' : csol_viewer_histogram,info.orig,TITLE=info.filename

   'MIN_FIELD': begin
      widget_control,info.min_field,get_value=omin
      widget_control,info.max_field,get_value=omax
      info.omin = float(omin)
      info.omax = float(omax)
      csol_viewer_display, info=info
      end

   'MAX_FIELD': begin
      widget_control,info.min_field,get_value=omin
      widget_control,info.max_field,get_value=omax
      info.omin = float(omin)
      info.omax = float(omax)
      csol_viewer_display, info=info
      end

   'FREEZE': begin
      widget_control,info.menuDispFreeze,get_value=v
      if STRUPCASE(v) eq 'FREEZE' then $
         newv = 'UnFreeze' $
      else $
         newv = 'Freeze'
      widget_control,info.menuDispFreeze,set_value=newv
      end

   'RESET': begin
      omin=min(*info.orig,max=omax)
      info.omin = omin
      info.omax = omax
      widget_control,info.min_field,set_value=float(info.omin)
      widget_control,info.max_field,set_value=float(info.omax)
      csol_viewer_display, info=info
      end

   'BIG_WINDOW': begin
      x = event.x
      y = event.y
      if event.type eq 3 then begin
         csol_viewer_position, info=info
         GOTO,SKIP
      endif

      csol_viewer_xyval,info.orig,x,y,1,1,0,0,info=info
      if (info.csol_viewer_state[0] eq 0 and event.type eq 0 and event.press eq 4)  then begin
          ; the right mouse button was pressed to adjust the brightness/contrast
          info.csol_viewer_state[0] = 12
          widget_control,info.big_window,get_draw_view=v
          xr = ((x-v[0])>0)<(info.draw_window_size[0]-1)
          yr = ((y-v[1])>0)<(info.draw_window_size[1]-1)
          csol_viewer_stretchct, xr, yr, info=info, /getmouse
          GOTO,SKIP
      endif

      if (info.csol_viewer_state[0] eq 12 and event.type eq 1 and event.release eq 4)  then begin
          ; the right mouse button was released to stop adjustment of bright/contrast
          info.csol_viewer_state[0] = 0
          GOTO, SKIP
      endif

      if (info.csol_viewer_state[0] eq 12 and event.type eq 2)  then begin
          ; the right mouse button is still depressed and the mouse has moved
          widget_control,info.big_window,get_draw_view=v
          xr = ((x-v[0])>0)<(info.draw_window_size[0]-1)
          yr = ((y-v[1])>0)<(info.draw_window_size[1]-1)
          csol_viewer_stretchct, xr, yr, info=info, /getmouse
          GOTO, SKIP
      endif

      if (info.csol_viewer_state[0] eq 1) or (info.csol_viewer_state[0] eq 2) or $
         (info.csol_viewer_state[0] eq 9) or (info.csol_viewer_state[0] eq 10) or $
         (info.csol_viewer_state[0] eq 11) then  begin
            csol_viewer_defroi,event,info.big_id, info=info
      endif

      if ((info.csol_viewer_state[0] ge 3) and (info.csol_viewer_state[0] le 8)) or $
         (info.csol_viewer_state[0] eq 19) then begin
         csol_viewer_lineplot,event,'BIG', info=info
      endif

      if (event.press gt 1) or ((info.csol_viewer_state[0] eq 0) and $
         (event.press eq 1)) then begin
          info.xoffzoom = round(x - (info.zoom_width/2)/info.zoom_factor)>0
          info.yoffzoom = round(y - (info.zoom_width/2)/info.zoom_factor)>0
          if PTR_VALID(info.big_image) then $
             csol_viewer_zoom, info.big_image, zoom_img, info=info $
          else $
             csol_viewer_zoom, info.orig, zoom_img, info=info
          if PTR_VALID(info.zoom) then PTR_FREE,info.zoom
          info.zoom = PTR_NEW(zoom_img, /NO_COPY)
          csol_viewer_scale, info.zoom_id, info.zoom, imin=info.omin, imax=info.omax, $
             scale=info.scale_type, ncolors=info.ncolors
      endif
      end

   'LITTLE_WINDOW': begin
      x = event.x
      y = event.y
      factorx = info.little_nx/info.ns
      factory = info.little_ny/info.nl
      csol_viewer_xyval,info.orig,x,y,factorx,factory,0,0,info=info

      if (info.csol_viewer_state[0] eq 1) or (info.csol_viewer_state[0] eq 2) or $
         (info.csol_viewer_state[0] eq 9) or (info.csol_viewer_state[0] eq 10) or $
         (info.csol_viewer_state[0] eq 11) then  $
            csol_viewer_defroi,event,info.little_id,info=info
      if ((info.csol_viewer_state[0] ge 3) and (info.csol_viewer_state[0] le 8)) or $
         (info.csol_viewer_state[0] eq 19) then $
         csol_viewer_lineplot,event,'LITTLE',info=info
      if (info.csol_viewer_state[0] eq 0) then begin
         if event.press ge 1 then info.little_down=1
         if event.release ge 1 then info.little_down=0
         if info.little_down then begin
            factorx = info.ns/info.little_nx
            factory = info.nl/info.little_ny
            x1 = ((x*factorx)-350)>0
            y1 = ((y*factory)-256)>0
            widget_control,info.big_window,set_draw_view=[x1,y1]
            csol_viewer_position, info=info
         endif
      endif
      end

   'ZOOM_WINDOW': begin
      x = event.x
      y = event.y
      if event.key ne 0 and event.press eq 1 then begin
         ; an arrow key was pressed
         if event.key eq 5 then x-=info.zoom_factor else $
         if event.key eq 6 then x+=info.zoom_factor else $
         if event.key eq 7 then y+=info.zoom_factor else $
         if event.key eq 8 then y-=info.zoom_factor
      endif

      csol_viewer_xyval,info.orig,x,y,info.zoom_factor,info.zoom_factor,$
         info.xoffzoom, info.yoffzoom,info=info
      if (info.csol_viewer_state[0] eq 0) and (event.press ne 0) then begin
         if (event.modifiers eq 0) then begin
            widget_control,info.x_field,get_value=xx
            widget_control,info.y_field,get_value=yy
            csol_viewer_xyttag,xx,yy,/to_binned,xbin=(*info.data).xbin,ybin=(*info.data).ybin
            if event.key eq 0 then begin
               ; recenter the zoom window if mouse button was pressed
               info.xoffzoom = round(xx - (info.xoff/(*info.data).xbin) - (info.zoom_width/2)/info.zoom_factor)>0
               info.yoffzoom = round(yy - (info.yoff/(*info.data).ybin) - (info.zoom_width/2)/info.zoom_factor)>0
            endif else begin
               ; move the zoom window one pixel if an arrow key was press
               if event.key eq 5 then info.xoffzoom -= 1 else $
               if event.key eq 6 then info.xoffzoom += 1 else $
               if event.key eq 7 then info.yoffzoom += 1 else $
               if event.key eq 8 then info.yoffzoom -= 1
            endelse
            if PTR_VALID(info.big_image) then $
               csol_viewer_zoom, info.big_image, zoom_img, info=info $
            else $
               csol_viewer_zoom, info.orig, zoom_img, info=info
            if PTR_VALID(info.zoom) then PTR_FREE,info.zoom
            info.zoom = PTR_NEW(zoom_img, /NO_COPY)
            csol_viewer_scale, info.zoom_id, info.zoom, imin=info.omin, imax=info.omax, scale=info.scale_type, ncolors=info.ncolors
      endif
      if (info.csol_viewer_state[0] eq 1) or (info.csol_viewer_state[0] eq 2) or $
         (info.csol_viewer_state[0] eq 9) or (info.csol_viewer_state[0] eq 10) or $
         (info.csol_viewer_state[0] eq 11) then  $
            csol_viewer_defroi,event,info.zoom_id,info=info
      if ((info.csol_viewer_state[0] ge 3) and (info.csol_viewer_state[0] le 8)) or $
         (info.csol_viewer_state[0] eq 19)  then $
            csol_viewer_lineplot,event,'ZOOM',info=info
      endif
      end

   'X_FIELD': begin
      widget_control,info.x_field,get_value=x
      widget_control,info.y_field,get_value=y
      csol_viewer_xyval,info.orig,x,y,1,1,0,0,info=info
      end

   'Y_FIELD': begin
      widget_control,info.x_field,get_value=x
      widget_control,info.y_field,get_value=y
      csol_viewer_xyval,info.orig,x,y,1,1,0,0,info=info
      end

   'PLOT_SURFACE': begin
      if (info.xoffzoom ge 0) and (info.yoffzoom ge 0) and $
         (info.xoffzoom le info.ns-1) and (info.yoffzoom le info.nl-1) then begin
         xend = (info.xoffzoom + (info.zoom_width+info.zoom_factor-1)/$
            info.zoom_factor-1)<(info.ns-1)
         yend = (info.yoffzoom + (info.zoom_width+info.zoom_factor-1)/$
            info.zoom_factor-1)<(info.nl-1)
         if NOT PTR_VALID(info.big_image) then $
             data = (*info.orig)[info.xoffzoom:xend,info.yoffzoom:yend] $
         else $
            data = (*info.big_image)[info.xoffzoom:xend,info.yoffzoom:yend]
         x = findgen(xend-info.xoffzoom+1)+info.xoffzoom
         y = findgen(yend-info.yoffzoom+1)+info.yoffzoom
         widget_control,info.min_field,get_value=imin
         widget_control,info.max_field,get_value=imax
         case info.scale_type of
         'Linear': data = data>imin<imax
         'Log':    begin
            tmin=imax/1e4
            data = alog10((data-imin)>tmin<(imax-tmin))
            end
         'Sqrt': data = sqrt(data>0>imin<imax)
         'Hist. Eq.': data = hist_equal(data)
         endcase
         ; xsurface,data
         isurface,data
      endif
      end

   'PLOT_CONTOUR': begin
      if (info.xoffzoom ge 0) and (info.yoffzoom ge 0) and $
         (info.xoffzoom le info.ns-1) and (info.yoffzoom le info.nl-1) then begin
         xend = (info.xoffzoom + (info.zoom_width+info.zoom_factor-1)/$
            info.zoom_factor-1)<(info.ns-1)
         yend = (info.yoffzoom + (info.zoom_width+info.zoom_factor-1)/$
            info.zoom_factor-1)<(info.nl-1)
         if NOT PTR_VALID(info.big_image) then $
             data = (*info.orig)[info.xoffzoom:xend,info.yoffzoom:yend] $
         else $
            data = (*info.big_image)[info.xoffzoom:xend,info.yoffzoom:yend]
         x = findgen(xend-info.xoffzoom+1)+info.xoffzoom
         y = findgen(yend-info.yoffzoom+1)+info.yoffzoom
         csol_viewer_xyttag,x,y,/frac, xbin=(*info.data).xbin, ybin=(*info.data).ybin, $
            xoff=info.xoff,yoff=info.yoff
         live_contour,data,xindep=x,yindep=y
         ; tvlct,ctbl,/get
         ; icontour, data, x, y, /insert_legend, n_levels=10,rgb_table=ctbl ; keeps hanging X11
      endif
      end

   'PLOT_ENCENERGY': begin
      if (info.xoffzoom ge 0) and (info.yoffzoom ge 0) and $
         (info.xoffzoom le info.ns-1) and (info.yoffzoom le info.nl-1) then begin
         xend = (info.xoffzoom + (info.zoom_width+info.zoom_factor-1)/$
            info.zoom_factor-1)<(info.ns-1)
         yend = (info.yoffzoom + (info.zoom_width+info.zoom_factor-1)/$
            info.zoom_factor-1)<(info.nl-1)
         if NOT PTR_VALID(info.big_image) then $
             data = (*info.orig)[info.xoffzoom:xend,info.yoffzoom:yend] $
         else $
            data = (*info.big_image)[info.xoffzoom:xend,info.yoffzoom:yend]
         x = findgen(xend-info.xoffzoom+1)+info.xoffzoom
         y = findgen(yend-info.yoffzoom+1)+info.yoffzoom
         csol_viewer_xyttag,x,y,/frac, xbin=(*info.data).xbin, ybin=(*info.data).ybin, $
            xoff=info.xoff,yoff=info.yoff
         enc=enc_energy(data,xcenter=xc,ycenter=yc,/getcent)
         xc=float(long((xc+info.xoffzoom)*10.0)/10.0)
         yc=float(long((yc+info.yoffzoom)*10.0)/10.0)
         plot_cmd='plot,/xstyle,/ystyle,xtitle="Radius (pixels)",ytitle="Encircled Energy"'
         plot_cmd = plot_cmd + ',title="'+info.filename+' Center at: '+$
           strtrim(string(xc),2)+', '+ strtrim(string(yc),2)+'"'
         title=info.filename+' Center at: '+strtrim(string(xc),2)+', '+ strtrim(string(yc),2)
         plot_data,'plot',/xstyle,/ystyle,xtitle="Radius (pixels)",ytitle="Encircled Energy",title=title,$
            enc.radius,enc.energy,wtitle=info.filename
      endif
      end

   'GAUSS_EM': begin
      if (info.xoffzoom ge 0) and (info.yoffzoom ge 0) and $
         (info.xoffzoom le info.ns-1) and (info.yoffzoom le info.nl-1) then begin
         xend = (info.xoffzoom + (info.zoom_width+info.zoom_factor-1)/$
            info.zoom_factor-1)<(info.ns-1)
         yend = (info.yoffzoom + (info.zoom_width+info.zoom_factor-1)/$
            info.zoom_factor-1)<(info.nl-1)
         if NOT PTR_VALID(info.big_image) then $
             data = (*info.orig)[info.xoffzoom:xend,info.yoffzoom:yend] $
         else $
            data = (*info.big_image)[info.xoffzoom:xend,info.yoffzoom:yend]
         x = findgen(xend-info.xoffzoom+1)+info.xoffzoom+info.xoff
         y = findgen(yend-info.yoffzoom+1)+info.yoffzoom+info.yoff
         xbin=(*info.data).xbin & ybin=(*info.data).ybin
         ;title=info.filename+'  '+info.HEXseg
         title=info.wtitle
         csol_viewer_gfit,data,x,y,1,title=title,group=event.top,xbin=(*info.data).xbin,ybin=(*info.data).ybin,$
            filename=info.filename, info=info
      endif
      end

   'GAUSS_AB': begin
      if (info.xoffzoom ge 0) and (info.yoffzoom ge 0) and $
         (info.xoffzoom le info.ns-1) and (info.yoffzoom le info.nl-1) then begin
         xend = (info.xoffzoom + (info.zoom_width+info.zoom_factor-1)/info.zoom_factor-1)<(info.ns-1)
         yend = (info.yoffzoom + (info.zoom_width+info.zoom_factor-1)/info.zoom_factor-1)<(info.nl-1)
         if NOT PTR_VALID(info.big_image) then $
             data = (*info.orig)[info.xoffzoom:xend,info.yoffzoom:yend] $
         else $
            data = (*info.big_image)[info.xoffzoom:xend,info.yoffzoom:yend]
         x = findgen(xend-info.xoffzoom+1)+info.xoffzoom+info.xoff
         y = findgen(yend-info.yoffzoom+1)+info.yoffzoom+info.yoff
         xbin=(*info.data).xbin & ybin=(*info.data).ybin
         ;title=info.filename+'  '+info.HEXseg
         title=info.wtitle
         csol_viewer_gfit,data,x,y,0,title=title,group=event.top,xbin=(*info.data).xbin,ybin=(*info.data).ybin,$
            filename=info.filename, info=info
      endif
      end

   'STATS': begin
       widget_control,event.id,get_value=val
       val = STRUPCASE(STRTRIM(STRCOMPRESS(val),2))

       ; if the data is TTAG, determine the exact time between first and last event
       ; (useful in the case where an exposure is split between many HEX files that all
       ; have the same total exposure time in them
       case val of

           'WHOLE IMAGE':  begin
              widget_control,/hourglass
              widget_control,info.min_field,get_value=minv
              ; check if we have TTAG info
              ttag_time = 0.0

              region=[0,0,info.ns,info.nl]

              if NOT PTR_VALID(info.big_image) then begin
                 csol_viewer_stats,info.orig,minv,title='Image Statistics', group=event.top, info=info, $
                    filename=info.filename, exptime=info.exptime, ttag_time=ttag_time,region=region
              endif else  begin
                 csol_viewer_stats,info.big_image,minv,title='Image Statistics', group=event.top, info=info, $
                    filename=info.filename, exptime=info.exptime, ttag_time=ttag_time,region=region
              endelse
              widget_control,/hourglass
              end

           'ZOOMED IMAGE':  begin
              ttag_time=0.0
              if (info.xoffzoom ge 0) and (info.yoffzoom ge 0) and $
                 (info.xoffzoom le info.ns-1) and (info.yoffzoom le info.nl-1) then begin
                 xend = (info.xoffzoom + (info.zoom_width+info.zoom_factor-1)/$
                    info.zoom_factor-1 ) < (info.ns-1)
                 yend = (info.yoffzoom + (info.zoom_width+info.zoom_factor-1)/$
                    info.zoom_factor-1 ) < (info.nl-1)
                 if NOT PTR_VALID(info.big_image) then begin
                    data = (*info.orig)[info.xoffzoom:xend,info.yoffzoom:yend]
                 endif else begin
                    data = (*info.big_image)[info.xoffzoom:xend,info.yoffzoom:yend]
                 endelse

                 widget_control,/hourglass
                 widget_control,info.min_field,get_value=minv
                 title='Region ['+strtrim(info.xoffzoom+info.xoff,2)+':'+ $
                       strtrim(xend+info.xoff,2)+', '+strtrim(info.yoffzoom+info.yoff,2)+':'+ $
                       strtrim(yend+info.yoff,2)+']'
                 region=[info.xoffzoom,info.yoffzoom,xend,yend]
                 csol_viewer_stats,data,minv,title=title,group=event.top,filename=info.filename, $
                    exptime=info.exptime, ttag_time=ttag_time,region=region, info=info
                 widget_control,/hourglass
              endif
              end
          else: begin   ;SubArray
              if val eq 'BOX' then val=1 $
              else if val eq 'DRAW REGION' then val=2
              info.csol_viewer_state = [val,0,-1,-1]
              if info.csol_viewer_state[0] ne 2 then $
                 mess = 'Place cursor on first corner and push left button' $
              else $
                 mess = 'Push mouse button, hold down, trace region, then release'
              widget_control,info.HEXmessage,set_value=mess
          end
       endcase
       end
;

   'PLOT' : BEGIN
       WIDGET_CONTROL, event.id, GET_VALUE=val
       val = STRUPCASE(STRTRIM(STRCOMPRESS(val),2))
       if val eq 'ROW' then val=1 $
       else if val eq 'COLUMN' then val=2 $
       else if val eq 'ROW SUM' then val=3 $
       else if val eq 'COLUMN SUM' then val=4 $
       else if STRMID(val,0,2) eq '1' then val=6 $
       else if STRMID(val,0,2) eq '3' then val=7 $
       else if STRMID(val,0,2) eq '5' then val=8 $
       else if STRMID(val,0,2) eq '7' then val=9 $
       else if STRMID(val,0,2) eq '9' then val=10 $
       else if STRMID(val,0,2) eq '11' then val=11 $
       else if STRMID(val,0,2) eq '15' then val=12 $
       else if STRMID(val,0,2) eq '25' then val=13 $
       else if STRMID(val,0,2) eq '35' then val=14 $
       else if STRMID(val,0,2) eq '55' then val=15 $
       else if STRMID(val,0,2) eq '75' then val=16

       info.csol_viewer_State = [(val+2)<8,0,-1,-1,0]
       case info.csol_viewer_state[0] of
         3: mess = 'Select row and click left mouse button'
         4: mess = 'Select column and click left mouse button'
         5: mess = 'Select first row and click left button'
         6: mess = 'Select first column and click left button'
         7: mess = 'Select first row and click left button'
         8: begin
            mess = 'Select first point and click left button'
            widths = [1,3,5,7,9,11,15,25,35,55,75]
            info.csol_viewer_state[4] = widths[val-6]
            info.xregion[0:1] = [-1,-1]
            end
       endcase
       widget_control,info.HEXmessage,set_value=mess
       END
   else:
   endcase

SKIP:
   ; Reset the windows user value to the updated state structure
   WIDGET_CONTROL, event.top, SET_UVALUE=info, /NO_COPY

end
;=========================================================== csol_viewer_POSITION
;
; Routine to report regions displayed
;
pro csol_viewer_position, info=info

   if n_elements(info) eq 0 then return
;
; big window
;
   widget_control,info.big_window,get_draw_view=v

   xr = [v[0],(v[0]+744)<(info.ns-1)]
   yr = [v[1],(v[1]+514)<(info.nl-1)]
   csol_viewer_xyttag,xr,yr,xbin=(*info.data).xbin,ybin=(*info.data).ybin, $
            xoff=info.xoff, yoff=info.yoff
   widget_control,info.big_position,set_value=strtrim(xr[0],2)+':'+ $
      strtrim(xr[1],2)+'  '+strtrim(yr[0],2)+':'+ strtrim(yr[1],2)
;
; zoom window
;
   xend = (info.xoffzoom + (info.zoom_width+info.zoom_factor-1)/info.zoom_factor-1)<(info.ns-1)
   yend = (info.yoffzoom + (info.zoom_width+info.zoom_factor-1)/info.zoom_factor-1)<(info.nl-1)
   xr = [info.xoffzoom,xend]
   yr = [info.yoffzoom,yend]
   csol_viewer_xyttag,xr,yr,xbin=(*info.data).xbin,ybin=(*info.data).ybin, $
            xoff=info.xoff, yoff=info.yoff
   widget_control,info.zoom_position,set_value=strtrim(xr[0],2)+':'+ $
      strtrim(xr[1],2)+'  '+strtrim(yr[0],2)+':'+ strtrim(yr[1],2)
   return
   end
;============================================================ csol_viewer_PLOTS
pro csol_viewer_plots,xx,yy,color=color,overlay=overlay,$
    linestyle=linestyle, psym=psym, symsize=symsize, info=info

; plot vector in all three windows
;
;
; convert to window coordinates for all three windows and plot
;
   if n_elements(info) eq 0 then return

   if n_elements(color) eq 0 then color = !d.n_colors-1
   if keyword_set(overlay) then color=!d.n_colors-1
   if n_elements(linestyle) eq 0 then linestyle=0
   if n_elements(psym) eq 0 then psym=0
   if n_elements(symsize) eq 0 then symsize=1.0

   wset,info.big_id
   if keyword_set(overlay) then device,set_graphic=6
   plots,xx,yy,/dev,color=color,line=linestyle, psym=psym, symsize=symsize

   wset,info.little_id
   if keyword_set(overlay) then device,set_graphic=6
   factorx = info.little_nx/info.ns
   factory = info.little_ny/info.nl
   plots,long(xx*factorx),long(yy*factory),/dev,color=color,$
         line=linestyle,psym=psym, symsize=symsize

   wset,info.zoom_id
   if keyword_set(overlay) then device,set_graphic=6
   xpos = (xx-info.xoffzoom)*long(info.zoom_factor) + info.zoom_factor/2
   ypos = (yy-info.yoffzoom)*long(info.zoom_factor) + info.zoom_factor/2
   xend = info.zoom_width*info.zoom_factor
   yend = info.zoom_width*info.zoom_factor
   indx = where(xpos ge 0.0 and xpos le xend and $
                ypos ge 0.0 and ypos le yend, count)
   if count gt 0 then begin
      plots,xpos[indx],ypos[indx], /dev, $
            color=color,line=linestyle,psym=psym, symsize=symsize
   endif
   device,set_graphic=3
return
end

;
;=============================================================
;
pro csol_viewer_set_region_event,event

   widget_control,event.top,get_uvalue=uvalue

   CASE event.id OF

      (*uvalue).ok_button: BEGIN
         widget_control,(*uvalue).xlarge,get_value=x
         (*uvalue).xlval=x[0]
         widget_control,(*uvalue).ylarge,get_value=y
         (*uvalue).ylval=y[0]
         widget_control,(*uvalue).xzoom,get_value=x
         (*uvalue).xzval=x[0]
         widget_control,(*uvalue).yzoom,get_value=y
         (*uvalue).yzval=y[0]
         widget_control,(*uvalue).fzoom,get_value=v
         (*uvalue).fzval=v[0]
         widget_control,event.top,/destroy
         return
      END

      (*uvalue).cancel_button: BEGIN
         ; temporarily set zoom factor to zero to indicate cancel
         (*uvalue).fzval=0
         widget_control,event.top,/destroy
         return
      END

      ELSE:
         ; don't do anything for un-interresting events
   ENDCASE

end

;
;========================================================== csol_viewer_SET_REGION
;
; widget to set display regions
;
pro csol_viewer_set_region,group=group, info=info

   if n_elements(info) eq 0 then return

   base = widget_base(/col,group=group,/modal)
   label = widget_label(base,value='Large Window')
   widget_control,info.big_window,get_draw_view=v
   xlval = v[0] & ylval = v[1]
   csol_viewer_xyttag,xlval,ylval,xbin=(*info.data).xbin,ybin=(*info.data).ybin,$
      xoff=info.xoff, yoff=info.yoff
   xlarge = cw_field(base,/row,title='XMIN:',/long,/return_events, $
      value=xlval, xsize=13)
   ylarge = cw_field(base,/row,title='YMIN:',/long,/return_events, $
      value=ylval, xsize=13)
   label = widget_label(base,value='Zoom Window')
   xzval = info.xoffzoom
   yzval = info.yoffzoom
   csol_viewer_xyttag,xzval,yzval,xbin=(*info.data).xbin,ybin=(*info.data).ybin, $
      xoff=info.xoff, yoff=info.yoff
   xzoom=cw_field(base,/row,title='XMIN:',/long,/return_events, $
      value=xzval,xsize=13)
   yzoom=cw_field(base,/row,title='YMIN:',/long,/return_events, $
      value=yzval,xsize=13)
   fzoom=cw_field(base,/row,title='Zoom Factor:',/long, $
      /return_events,xsize=7,value=info.zoom_factor)

   base2 = widget_base(base, /ROW, SPACE=30, XPAD=30, YPAD=10, /ALIGN_CENTER)
   cancel_button = widget_button(base2, VALUE='Cancel', UVALUE='CANCEL', XSIZE=80)
   ok_button = widget_button(base2, VALUE='Ok', UVALUE='OK',XSIZE=80)

   widget_control,base,/realize
   xbase = {ok_button:ok_button, $
            cancel_button:cancel_button, $
            xlarge:xlarge,$
            ylarge:ylarge,$
            xzoom:xzoom, $
            yzoom:yzoom,$
            fzoom:fzoom,$
            xlval:xlval,$
            ylval:ylval, $
            xzval:xzval,$
            yzval:yzval, $
            fzval:info.zoom_factor}
   uvalue = ptr_new(xbase)
   widget_control,base,set_uvalue=uvalue
   xmanager,'csol_viewer_set_region',base,/no_block

   xlval=(*uvalue).xlval
   ylval=(*uvalue).ylval
   xzval=(*uvalue).xzval
   yzval=(*uvalue).yzval
   fzval=(*uvalue).fzval
   PTR_FREE, uvalue

   csol_viewer_xyttag,xzval,yzval,/to_binned, xbin=(*info.data).xbin, ybin=(*info.data).ybin
   info.zoom_factor = fzval>1<32
   info.xoffzoom = xzval>0<(info.ns-info.zoom_width/info.zoom_factor)
   info.yoffzoom = yzval>0<(info.nl-info.zoom_width/info.zoom_factor)
   csol_viewer_zoom,info.orig,zoom_img,info=info
   if PTR_VALID(info.zoom) then PTR_FREE,info.zoom
   info.zoom = PTR_NEW(zoom_img, /NO_COPY)
   csol_viewer_scale,info.zoom_id,info.zoom,imin=info.omin,imax=info.omax, scale=info.scale_type, ncolors=info.ncolors
   csol_viewer_xyttag,xlval,ylval,/to_binned, xbin=(*info.data).xbin, ybin=(*info.data).ybin
   x = xlval>0<(info.ns-745)
   y = ylval>0<(info.nl-515)
   widget_control,info.big_window,set_draw_view=[x,y]
   csol_viewer_position, info=info

end

;
;============================================================ csol_viewer_DISPLAY
;
; Routine to set up and display image all three windows
;
pro csol_viewer_display, image, select=select, info=info, reset=reset

   if n_elements(info) eq 0 then return

   widget_control,/hourglass
;
; process new image?
;
   if size(image,/TNAME) ne 'UNDEFINED' then begin      ;new image?
      if size(image,/TNAME) ne 'POINTER' then begin
         if PTR_VALID(info.orig) then PTR_FREE,info.orig
         info.orig=PTR_NEW(image,/NO_COPY)
      endif else begin
         if PTR_VALID(info.orig) then begin
            ;if info.orig previously defined and not the same as image
            if info.orig ne image then PTR_FREE,info.orig
         endif
         info.orig=image
      endelse
      if PTR_VALID(info.big_image) then PTR_FREE, info.big_image
      omin = min(*info.orig, max=omax)
      info.omin=omin & info.omax=omax
      widget_control,info.menuDispFreeze,get_value=v
      if STRUPCASE(v) eq 'FREEZE' then begin
         widget_control,info.min_field,set_value=float(omin)
         widget_control,info.max_field,set_value=float(omax)
      endif
      s = size(*info.orig) & ns = s[1] & nl = s[2]
      info.ns=ns & info.nl=nl
      if info.xoffzoom gt (ns - (info.zoom_width/2)/info.zoom_factor) then $
         info.xoffzoom = (ns/2 - (info.zoom_width/2)/info.zoom_factor) > 0
      if info.yoffzoom gt (nl - (info.zoom_width/2)/info.zoom_factor) then $
         info.yoffzoom = (nl/2 - (info.zoom_width/2)/info.zoom_factor) > 0
      little_image = frebin(*info.orig,info.little_nx, info.little_ny)
      if PTR_VALID(info.little_image) then PTR_FREE, info.little_image
      info.little_image = PTR_NEW(little_image, /no_copy)
      ; determine the size of screen to make widget fit on screen
      ; specially for 1024x768 laptops
      device,get_screen_size=screen_size
      if screen_size[1] ge 1024 then begin
         min_ysize=542
      endif else begin
         min_ysize = screen_size[1] / 2
      endelse
      max_ysize=(screen_size[1]-380) < (nl+30)

      ; resize the big window only
      geom = WIDGET_INFO(info.big_window, /GEOMETRY)
      newXSize = (geom.scr_xsize > 750) < (ns+30)
      newYSize = (geom.scr_ysize > min_ysize) < max_ysize
      ; Resize the text widget accordingly
      widget_control,info.big_window,draw_xsize=ns,draw_ysize=nl, $
         scr_xsize=newxsize,scr_ysize=newysize
   endif

   if keyword_set(select) then begin
      ; use a selected subset of the original array
      ; select is array indices to keep
      ; big_image is used only when a subset of the data
      ; is being used (using the PHA filter)
      ; This avoids keeping multiple copies of the image in memory
      big_image = hist_2d(round((*(*info.data).xttag)[select]/(*info.data).xbin), $
         round((*(*info.data).yttag)[select]/(*info.data).ybin), $
         min1=0,min2=0, max1=info.ns-1, max2=info.nl-1)
      if PTR_VALID(info.big_image) then PTR_FREE, info.big_image
      info.big_image=PTR_NEW(big_image, /no_copy)
      omin = min(*info.big_image, max=omax)
      widget_control,info.menuDispFreeze,get_value=v
      if STRUPCASE(v) eq 'FREEZE' then begin
         widget_control,info.min_field,set_value=float(omin)
         widget_control,info.max_field,set_value=float(omax)
      end
      little_image = frebin(*info.big_image, info.little_nx, info.little_ny)
      if PTR_VALID(info.little_image) then PTR_FREE, info.little_image
      info.little_image = PTR_NEW(little_image, /no_copy)
      info.omin=omin & info.omax=omax
   endif

   widget_control, info.HEXmessage, set_value=info.scale_type+' Display'
;
; display big image
;
   if PTR_VALID(info.big_image) then $
      csol_viewer_scale, info.big_id, info.big_image, imin=info.omin, imax=info.omax, scale=info.scale_type, ncolors=info.ncolors $
   else $
      csol_viewer_scale, info.big_id, info.orig, imin=info.omin, imax=info.omax, scale=info.scale_type, ncolors=info.ncolors
;
; display little image
;
   csol_viewer_scale,info.little_id,info.little_image,imin=info.omin,imax=info.omax,$
       scale=info.scale_type, ncolors=info.ncolors
;
; display zoom image
;
   if PTR_VALID(info.big_image) then begin
      csol_viewer_zoom, info.big_image, zoom_img, info=info
   endif else begin
      csol_viewer_zoom, info.orig, zoom_img, info=info
   endelse
   if PTR_VALID(info.zoom) then PTR_FREE,info.zoom
   info.zoom = PTR_NEW(zoom_img, /NO_COPY)

   csol_viewer_scale, info.zoom_id, info.zoom, imin=info.omin, imax=info.omax, scale=info.scale_type, ncolors=info.ncolors
;
; update positions
;
   csol_viewer_position, info=info

   wset,info.big_id
   ; get the size of the new window
   WIDGET_CONTROL, info.top_base, TLB_GET_SIZE=windowSize
   info.top_base_windowSize = windowSize

   if keyword_set(reset) then begin
      ; user requested to reset the uvalue of the top_base
      ; this is used by xttag_CSOL when filtering and updating the data
      widget_control, info.top_base, set_uvalue=info
   endif


   widget_control,/hourglass
   return
end
; ============================================================ csol_viewer_SCALE
;
; Routine to scale and display an image
;
pro csol_viewer_scale, window_id, image, xoff=xoff, yoff=yoff, noerase=noerase, $
   imin=imin, imax=imax, scale=scale_type, ncolors=ncolors

   if n_elements(imin) eq 0 then begin
      if size(image,/TNAME) eq 'POINTER' then begin
         imin=min(*image, max=imax)
      endif else begin
         imin=min(image, max=imax)
      endelse
   endif
   imin=float(imin)
   imax=float(imax)

   case scale_type of
      'Linear': begin
         if size(image,/TNAME) eq 'POINTER' then begin
            pic = bytscl(*image,min=imin,max=imax,top=ncolors-1) + 12
         endif else begin
            pic = bytscl(image,min=imin,max=imax,top=ncolors-1) + 12
         endelse
         end
      'Log':    begin
         tmin=imax/1e4
         if size(image,/TNAME) eq 'POINTER' then begin
            pic = bytscl(alog10((*image-imin)>tmin), min=alog10(tmin), $
               max=alog10(imax-imin),top=ncolors-1) + 12
         endif else begin
            pic = bytscl(alog10((image-imin)>tmin), min=alog10(tmin), $
               max=alog10(imax-imin),top=ncolors-1) + 12
         endelse
         end
      'Sqrt': begin
         if size(image,/TNAME) eq 'POINTER' then begin
            pic = bytscl(sqrt((*image-imin)>0),min=0,max=sqrt(imax-imin),top=ncolors-1) + 12
         endif else begin
            pic = bytscl(sqrt((image-imin)>0),min=0, max=sqrt(imax-imin),top=ncolors-1) + 12
         endelse
         end
      'Hist. Eq.': BEGIN
         if size(image,/TNAME) eq 'POINTER' then begin
            IF (imin EQ imax) THEN BEGIN
               pic = bytscl(*image,min=imin,max=imax,top=ncolors-1) + 12
            ENDIF ELSE BEGIN
               pic = hist_equal(*image,minv=imin,maxv=imax, top=ncolors-1) + 12
            ENDELSE
         endif else begin
            IF (imin EQ imax) THEN BEGIN
               pic = bytscl(image,min=imin,max=imax,top=ncolors-1) + 12
            ENDIF ELSE BEGIN
               pic = hist_equal(image,minv=imin,maxv=imax, top=ncolors-1) + 12
            ENDELSE
         endelse
       end
   endcase

   wset,window_id
   if not keyword_set(xoff) then xoff=0
   if not keyword_set(yoff) then yoff=0
   if not keyword_set(noerase) then erase
   tv,pic, xoff, yoff
   return
end
;
;============================================================== csol_viewer_ZOOM
;
; ROUTINE TO CREATE ZOOMED IMAGE -
;
pro csol_viewer_zoom,image,zoom_img,info=info

   if n_elements(info) eq 0 then return

   if size(image,/TNAME) eq 'POINTER' then begin
      s = size(*image) & ns = s[1] & nl = s[2]
   endif else begin
      s = size(image) & ns = s[1] & nl = s[2]
   endelse
   xoff=info.xoffzoom
   yoff=info.yoffzoom
   if (xoff lt 0) or (yoff lt 0) or (xoff ge ns-1) or (yoff ge nl-1) then begin
         zoom_img = fltarr(info.zoom_width,info.zoom_width)
      return
   endif
   xend = (xoff + (info.zoom_width+info.zoom_factor-1)/info.zoom_factor-1)<(ns-1)
   yend = (yoff + (info.zoom_width+info.zoom_factor-1)/info.zoom_factor-1)<(nl-1)
   if size(image,/TNAME) eq 'POINTER' then begin
      zoom_img = rebin((*image)[xoff:xend,yoff:yend],(xend-xoff+1)*info.zoom_factor, $
                  (yend-yoff+1)*info.zoom_factor,/samp)
   endif else begin
      zoom_img = rebin(image[xoff:xend,yoff:yend],(xend-xoff+1)*info.zoom_factor, $
                  (yend-yoff+1)*info.zoom_factor,/samp)
   endelse
   csol_viewer_position, info=info
   return
end
;
; ============================================================== csol_viewer_XVAL
;
; ROUTINE TO UPDATE X/Y/Value fields
;
pro csol_viewer_xyval,image,x,y,zoomfactx,zoomfacty,xoff,yoff,info=info

   if n_elements(info) eq 0 then return

   if size(image,/TNAME) eq 'POINTER'  and NOT PTR_VALID(image) then begin
      ; bad pointer
      return
   endif else if PTR_VALID(image) then begin
      s = size(*image) & ns = s[1] & nl = s[2]
   endif else begin
      s = size(image) & ns = s[1] & nl = s[2]
   endelse
   xx = xoff + fix(x/zoomfactx)
   yy = yoff + fix(y/zoomfacty)
   if (xx ge 0) and (xx le ns-1) and (yy ge 0) and (yy le nl-1) then begin
      if size(image,/TNAME) eq 'POINTER' then begin
         widget_control,info.val_field,set_value=float((*image)[xx,yy])
      endif else begin
         widget_control,info.val_field,set_value=float(image[xx,yy])
      endelse
      csol_viewer_xyttag,xx,yy, xbin=(*info.data).xbin, ybin=(*info.data).ybin, $
         xoff=info.xoff, yoff=info.yoff
      widget_control,info.x_field,set_value=xx
      widget_control,info.y_field,set_value=yy
   endif
end

;=========================================================== csol_viewer_XYTTAG
; Routine to convert back and forth from binned and raw timetag positions
;
pro csol_viewer_xyttag,x,y,to_binned=to_binned,frac=frac,xbin=xbin,ybin=ybin,xoff=xoff,yoff=yoff
;
   if n_elements(to_binned) eq 0 then to_binned = 0
   if n_elements(frac) eq 0 then frac = 0
   if n_elements(xbin) eq 0 then xbin = 1
   if n_elements(ybin) eq 0 then ybin = 1
   if n_elements(xoff) eq 0 then xoff = 0
   if n_elements(yoff) eq 0 then yoff = 0
   if to_binned then begin
      if frac then begin
         x = (x + xoff - (xbin-1)/2.0)/xbin
         y = (y + yoff - (ybin-1)/2.0)/ybin
      endif else begin
         x = long(x+xoff)/xbin
         y = long(y+yoff)/ybin
      endelse
   endif else begin
      if frac then begin
         x = float(x)*xbin + (xbin-1)/2.0 + xoff
         y = float(y)*ybin + (ybin-1)/2.0 + yoff
      endif else begin
         x = long(x)*xbin + xoff
         y = long(y)*ybin + yoff
      endelse
   endelse
end

;
; ======================================================== csol_viewer_HISTOGRAM
;
; Main Widget Driver for Histograms
;
pro csol_viewer_histogram,img,title=title
   ; ignore the pixels with a value of 0 (overwhelms the plot)
   if ptr_valid(img) then begin
      hh=histogram((*img),min=1,omin=omin,omax=omax)
   endif else begin
      hh=histogram(img,min=1,omin=omin,omax=omax)
   endelse
   plot_data,'plot',xrange=[omin,omax],/ystyle,title=title,xtitle="Pixel Values",ytitle="Counts",$
      psym=10,lindgen(omax-omin+1L)+omin,hh

end
;
; ============================================================= csol_viewer_GFIT
;
;  Integrated Gaussian Fit event driver
;
pro csol_viewer_gfit_event,event
   ; only button is "done" so clear the plots

   widget_control, event.top, get_uvalue=state, /no_copy
   widget_control, event.id,  get_uvalue=uvalue

   case uvalue of
   'CLOSE': BEGIN   ; an option from the menubar was selected
      widget_control,event.top,/destroy
      return
   END

   'PRINT': BEGIN
      ; send output to printer
      file='gaussfit.ps'
      thisDevice=!d.name
      set_plot,'ps'
      xsize = 10.0
      ysize = 6.5
      device,/land,xsize=xsize,ysize=ysize,xoff=0.5,yoff=10.5,color=0, $
         file=file,bits=8,/inches
      !p.font = 0
      !p.position = 0
      !p.region = 0
      !p.multi = [1,2,1,0,0]
      xtitle= '  Center =' + strtrim(string(state.coef1[1],'(F8.2)'),2) + $
              '    FWHM =' + strtrim(string(state.coef1[2]*2.3548,'(F8.2)'),2)
      xtitle= xtitle+'!C2DCenter =' + strtrim(string((state.coef2d[4]*state.xbin+state.x[0]),'(F8.2)'),2) + $
              '  2DFWHM =' + strtrim(string(state.coef2d[2]*state.xbin*2.3548,'(F8.2)'),2)
      plot,state.x,state.profile1,psym=10,xstyle=1, title='Horizontal Profile', $
         yrange=[min(state.yy1)<min(state.profile1),max(state.yy1)>max(state.profile1)], $
         xtitle=xtitle
      oplot,state.x,state.fit1,psym=10,line=2
      oplot,state.xx1,state.yy1,thick=2
      oplot,state.x,state.back1,line=1

      !p.multi = [2,2,1,0,0]
      xtitle= 'Center =' + strtrim(string(state.coef2[1],'(F8.2)'),2) + $
              '  FWHM =' + strtrim(string(state.coef2[2]*2.3548,'(F8.2)'),2)
      xtitle= xtitle+'!C2DCenter =' + strtrim(string((state.coef2d[5]*state.ybin+state.y[0]),'(F8.2)'),2) + $
              '  2DFWHM =' + strtrim(string(state.coef2d[3]*state.ybin*2.3548,'(F8.2)'),2)
      plot,state.y,state.profile2,psym=10,xstyle=1, title='Vertical Profile', $
         yrange=[min(state.yy2)<min(state.profile2),max(state.yy2)>max(state.profile2)], $
         xtitle=xtitle
      oplot,state.y,state.fit2,psym=10,line=2
      oplot,state.xx2,state.yy2,thick=2
      oplot,state.y,state.back2,line=1
      xyouts,0.5,1.1,state.title,/norm,align=0.5,charsize=1.5
      device,/close
      set_plot,thisDevice
      tvlct,state.info.rsave,state.info.gsave,state.info.bsave
      !p.background=1
      !p.color=0
      status=send2printer(file)
      !p.multi=0
   END

   'SAVE': BEGIN
      ; get output file
      fdecomp, state.filename, disk,dir,fname,ext
      if fname eq '' then fname='gaussfit'
      path=CSOL_GETENV('CSOL_RESULTS_PATH')
      ; call this again in case the file had 2 extensions (as in *.fits.gz)
      fdecomp, fname,disk,dir,fname,ext
      file = fname+'_gfit.txt'
      file = dialog_pickfile(title='Select file to save ASCII fit',path=path, file=file,filter='*.txt',/write)
      if file eq '' then begin
         ;no file selected
         widget_control, event.top, set_uvalue=state, /no_copy
         return
      endif

      OPENW, unit, file, /GET_LUN
      printf, unit, systime()
      printf, unit, state.title

      printf, unit, ''
      printf, unit, 'X Fit:'
      printf, unit, 'Center =' + strtrim(string(state.coef1[1],'(F8.2)'),2) + $
              '  FWHM =' + strtrim(string(state.coef1[2]*2.3548,'(F8.2)'),2)
      printf, unit, ''
      printf, unit, '      X               Counts             GaussFit   '
      fmt='(F,2x,F,2x,F)'
      for i=0L,n_elements(state.x)-1L do begin
         printf,unit,state.x[i], state.profile1[i],state.fit1[i],format=fmt
      endfor

      printf, unit, ''
      printf, unit, 'Y Fit:'
      printf, unit, 'Center =' + strtrim(string(state.coef2[1],'(F8.2)'),2) + $
              '  FWHM =' + strtrim(string(state.coef2[2]*2.3548,'(F8.2)'),2)
      printf, unit, ''
      printf, unit, '       Y               Counts             GaussFit   '
      for i=0L,n_elements(state.y)-1L do begin
         printf,unit,state.y[i], state.profile2[i],state.fit2[i],format=fmt
      endfor
      free_lun,unit

   END

   'PS': BEGIN
      ; get output file
      fdecomp, state.filename, disk,dir,fname,ext
      if fname eq '' then fname='gaussfit'
      path=CSOL_GETENV('CSOL_RESULTS_PATH')
      fdecomp, fname,disk,dir,fname,ext
      file = fname+'_gfit.ps'
      file = dialog_pickfile(path=path, file=file,filter='*.ps',/write)
      if file eq '' then begin
         ;no file selected
         widget_control, event.top, set_uvalue=state, /no_copy
         return
      endif
      thisDevice=!d.name
      set_plot,'ps'
      xsize = 10.0
      ysize = 6.5
      !p.position = 0
      !p.region = 0
      !p.multi = [1,2,1,0,0]
      device,/land,xsize=xsize,ysize=ysize,xoff=0.5,yoff=10.5,color=0, $
         file=file,bits=8,/inches
      !p.font = 0
      xtitle= '  Center =' + strtrim(string(state.coef1[1],'(F8.2)'),2) + $
              '     FWHM =' + strtrim(string(state.coef1[2]*2.3548,'(F8.2)'),2)
      xtitle= xtitle+'!C2DCenter =' + strtrim(string((state.coef2d[4]*state.xbin+state.x[0]),'(F8.2)'),2) + $
              '  2DFWHM =' + strtrim(string(state.coef2d[2]*state.xbin*2.3548,'(F8.2)'),2)
      plot,state.x,state.profile1,psym=10,xstyle=1, title='Horizontal Profile', $
         yrange= [ min(state.yy1)<min(state.profile1) , max(state.yy1)>max(state.profile1) ], $
         xtitle=xtitle
      oplot,state.x,state.fit1,psym=10,line=2
      oplot,state.xx1,state.yy1,thick=2
      oplot,state.x,state.back1,line=1

      !p.multi = [2,2,1,0,0]
      xtitle= '  Center =' + strtrim(string(state.coef2[1],'(F8.2)'),2) + $
              '     FWHM =' + strtrim(string(state.coef2[2]*2.3548,'(F8.2)'),2)
      xtitle= xtitle+'!C2DCenter =' + strtrim(string((state.coef2d[5]*state.ybin+state.y[0]),'(F8.2)'),2) + $
              '  2DFWHM =' + strtrim(string(state.coef2d[3]*state.ybin*2.3548,'(F8.2)'),2)
      plot,state.y,state.profile2,psym=10,xstyle=1, title='Vertical Profile', $
         yrange = [ min(state.yy2)<min(state.profile2) , max(state.yy2)>max(state.profile2) ], $
         xtitle=xtitle
      oplot,state.y,state.fit2,psym=10,line=2
      oplot,state.xx2,state.yy2,thick=2
      oplot,state.y,state.back2,line=1
      xyouts,0.5,1.1,state.title,/norm,align=0.5,charsize=1.5
      device,/close
      set_plot,thisDevice
      tvlct,state.info.rsave,state.info.gsave,state.info.bsave
      !p.background=1
      !p.color=0
      !p.multi=0
   END

   else:
   endcase

   widget_control, event.top, set_uvalue=state, /no_copy

end
;
; Integrated Gaussian Fit Widget
;
pro csol_viewer_gfit,image,inx,iny,type,group=group,restore=restore,$
   title=ptitle,xbin=xbin,ybin=ybin,filename=filename, info=info

;
; compute fits
;
   if n_elements(ptitle) eq 0 then $
      title='Zoomed Window Profile' $
   else $
      title=ptitle

   if n_elements(filename) eq 0 then filename=''

   x=inx
   y=iny
   profile1 = total(image,2)
   profile2 = total(image,1)
   csol_viewer_xyttag,x,y,/frac, xbin=xbin, ybin=ybin
   ; type=1 for emission,  type=0 for absorption
   fit1 = gaussint_fit(x,profile1,coef1,sigma1,nterms=4,absorption=type-1,chisq=chisq1)
   fit2 = gaussint_fit(y,profile2,coef2,sigma2,nterms=4,absorption=type-1,chisq=chisq2)
   ;pad coef1 and coef2 to 6 elements
   coef1=[coef1,0.0d,0.0d,0.0d,0.0d,0.0d,0.0d]
   coef1=coef1[0:5]
   coef2=[coef2,0.0d,0.0d,0.0d,0.0d,0.0d,0.0d]
   coef2=coef2[0:5]

   ; calculate the center with gauss2dfit
   if type eq 1 then begin
      ; emission
      result = gauss2dfit(image,coef2d,/tilt)
   endif else begin
      ; absorption
      result = gauss2dfit(image,coef2d,/tilt,/negative)
   endelse

;
; create widget layout
;
   base = widget_base(/col,group=group,title=title, $
                      uvalue='MAIN', mbar=menubar)
   file_menu  = widget_button(menubar, value='File', /menu)
   ;file_print = widget_button(file_menu, uvalue='PRINT', VALUE='Print')
   file_save = widget_button(file_menu, uvalue='SAVE', VALUE='Save ...')
   file_ps    = widget_button(file_menu, uvalue='PS', VALUE='Postscript')
   file_quit  = widget_button(file_menu, uvalue='CLOSE', VALUE='Close')
   basex = widget_base(base,/row)
   base1 = widget_base(basex,/col)
   base2 = widget_base(basex,/col)

   lab = widget_label(base1,value='Horizontal Profile')
   lab = widget_label(base2,value='Vertical Profile')

   draw1 = widget_draw(base1,xsize=450,ysize=300)
   draw2 = widget_draw(base2,xsize=450,ysize=300)

   str1 = string(coef1[1],sigma1[0],coef1[2]*2.3548,sigma1[1]*2.3548,format="('  X-center =',F8.2,' (',F4.2,')   X-FWHM =',F8.2,' (',F4.2,')')")
   lab = widget_label(base1,value=str1)
   str1 = string(coef2d[4]*xbin+inx[0], coef2d[2]*xbin*2.3548, format="('2DX-center =',F8.2,'       2DX-FWHM =',F8.2,'      ')")
   lab = widget_label(base1,value=str1)

   str2 = string(coef2[1],sigma2[0],coef2[2]*2.3548,sigma2[1]*2.3548,format="('  Y-center =',F8.2,' (',F4.2,')    Y-FWHM =',F8.2,' (',F4.2,')')")
   lab = widget_label(base2,value=str2)
   str2 = string(coef2d[5]*ybin+iny[0], coef2d[3]*ybin*2.3548, format="('2DY-center =',F8.2,'       2DY-FWHM =',F8.2,'      ')")
   lab = widget_label(base2,value=str2)

;
; create widget
;
   widget_control,base,/realize
   widget_control,draw1,get_value=window1
   widget_control,draw2,get_value=window2
;
; Plot Profiles
;
   back1 = coef1[3] + coef1(4)*x
   xx1 = congrid(x,500,/interp)
   gaussx,xx1,coef1[1],coef1[2],coef1[0],yy1
   yy1 = yy1 + coef1[3] + coef1[4]*xx1
   wset,window1

   plot,x,profile1,psym=10,xstyle=1, color=0, $
      yrange= [ min(yy1)<min(profile1) , max(yy1)>max(profile1) ]
   oplot,x,fit1,psym=10,line=2, color=2
   oplot,xx1,yy1,thick=2, color=4
   oplot,x,back1,line=1, color=8
   ;WSHOW, window1, ICONIC=0
   plot1 = TVRD()

   back2 = coef2[3] + coef2[4]*y

   xx2 = congrid(y,500,/interp)
   gaussx,xx2,coef2[1],coef2[2],coef2[0],yy2
   yy2 = yy2 + coef2[3] + coef2[4]*xx2

   wset,window2

   plot,y,profile2,psym=10,xstyle=1, color=0, $
      yrange = [ min(yy2)<min(profile2) , max(yy2)>max(profile2) ]
   oplot,y,fit2,psym=10,line=2, color=2
   oplot,xx2,yy2,thick=2, color=4
   oplot,y,back2,line=1, color=8
   ;WSHOW, window2, ICONIC=0
   plot2 = TVRD()

   state = { coef1:coef1, $
             coef2:coef2, $
             coef2d:coef2d, $
             plot1:plot1, $
             plot2:plot2, $
             title:title, $
             x:x, $
             y:y, $
             xx1:xx1, $
             yy1:yy1, $
             xx2:xx2, $
             yy2:yy2, $
             profile1:profile1, $
             profile2:profile2, $
             fit1:fit1, $
             fit2:fit2, $
             back1:back1, $
             back2:back2, $
             xbin:xbin, $
             ybin:ybin, $
             info:info, $
             filename:filename $
          }
   WIDGET_CONTROL, base, SET_UVALUE=state

   xmanager,'csol_viewer_gfit',base,/no_block
   return
end
;
; ============================================================= csol_viewer_STATS
;
; Routine to compute statitics and print statistics
;

pro csol_viewer_stats,indata,background,group=group,title=title,filename=filename,$
                     exptime=exptime,ttag_time=ttag_time,region=region,info=info

;
; compute statistics
;
   if n_elements(title) eq 0 then title='Statistics'
   if n_elements(background) eq 0 then background=0.0
   ; print,'region=',region
   if size(indata,/TNAME) eq 'POINTER' then begin
      if size(*indata,/n_dimension) eq 1 then begin
         *indata = reform(*indata,region[2]-region[0],region[3]-region[1])
      endif
      n=n_elements(*indata)
      minv = min(*indata,max=maxv)
      med = median(*indata)
      sig = stdev(*indata,meanval)
      v=max(*indata,p)
      s=size(*indata) & IX = p MOD s[1] & IY = p/s[1]
      brghtpos = [ix+region[0],iy+region[1]]
      if n_elements(region) eq 4 then begin
         ; limit the size of the data that will go through accum2ttag to 32M
         if size(*indata,/tname) eq 'FLOAT' or size(*indata,/tname) eq 'DOUBLE' or total(indata) gt 2.0^25.0 then begin
            ; can't use accum2ttag for floating point data like flux
            totx=total(*indata,2)
            toty=total(*indata,1)
            avgx = total((findgen(region[2]-region[0])+1.0)*totx)/total(totx) + region[0] - 1.0
            avgy = total((findgen(region[3]-region[1])+1.0)*toty)/total(toty) + region[1] - 1.0
            medx=0.0 & medy=0.0
         endif else begin
            res=accum2ttag(*indata)
            avgx = mean([res.x]+region[0])
            avgy = mean([res.y]+region[1])
            medx = median([res.x]+region[0])
            medy = median([res.y]+region[1])
         endelse
      endif
   endif else begin
      if size(indata,/n_dimension) eq 1 then begin
         indata = reform(indata,region[2]-region[0],region[3]-region[1])
      endif
      n=n_elements(indata)
      minv = min(indata,max=maxv)
      med = median(indata)
      sig = stdev(indata,meanval)
      v=max(indata,p)
      s=size(indata) & IX = p MOD s[1] & IY = p/s[1]
      brghtpos = [ix+region[0],iy+region[1]]
      if n_elements(region) eq 4 then begin
         ; limit the size of the data that will go through accum2ttag to 16M
         if size(indata,/tname) eq 'FLOAT' or size(indata,/tname) eq 'DOUBLE' or total(indata) gt 2.0^24.0 then begin
            totx=total(indata,2)
            toty=total(indata,1)
            avgx = total((findgen(region[2]-region[0])+1.0)*totx)/total(totx) + region[0] - 1.0
            avgy = total((findgen(region[3]-region[1])+1.0)*toty)/total(toty) + region[1] - 1.0
            medx=0.0 & medy=0.0
         endif else begin
            res=accum2ttag(indata)
            avgx = mean(res.x+region[0])
            avgy = mean(res.y+region[1])
            medx = median(res.x+region[0])
            medy = median(res.y+region[1])
         endelse
      endif
   endelse
   tot = meanval*n
   medback = med-background
   totback = tot - background*n
   meanback= meanval-background
   minvb = minv - background
   maxvb = maxv - background
   if n_elements(exptime) gt 0 and exptime gt 0.0  then begin
      rate = float(tot) / float(exptime)
      rateback = float(totback) / float(exptime)
   endif else begin
      rate = 0.0
      rateback = 0.0
   endelse
   ratestring = +strtrim(rate,2)+'  ('+strtrim(string(tot,format="(F10.0)"),2)+$
      ' counts / '+strtrim(string(exptime,format="(F10.2)"),2)+' secs)'
   ratebackstring = strtrim(rateback,2)+'  ('+strtrim(string(totback,format="(F10.0)"),2)+$
      ' counts / '+strtrim(string(exptime,format="(F10.2)"),2)+' secs)'

   if ttag_time gt 0.0 then begin
      rate = float(tot) / float(ttag_time)
      ratettag = +strtrim(rate,2)+'  ('+strtrim(string(tot,format="(F10.0)"),2)+$
         ' counts / '+strtrim(string(ttag_time,format="(F10.2)"),2)+' secs)'
      rateback = float(totback) / float(ttag_time)
      ratebackttag = strtrim(rateback,2)+'  ('+strtrim(string(totback,format="(F10.0)"),2)+$
         ' counts / '+strtrim(string(ttag_time,format="(F10.2)"),2)+' secs)'
   endif

;
; create the text to display
;
  text = [filename, '', $
          title, $
          '  NPoints   = '+strtrim(n,2), $
          '  Minimum   = '+strtrim(minv,2), $
          '  Maximum   = '+strtrim(maxv,2), $
          '  Total     = '+strtrim(tot,2), $
          '  Median    = '+strtrim(med,2), $
          '  Mean      = '+strtrim(meanval,2), $
          '  StDev     = '+strtrim(sig,2), $
          '  Brightest = ['+strtrim(brghtpos[0]+info.xoff,2)+', '+strtrim(brghtpos[1]+info.yoff,2)+']', $
          '  Rate      = '+ratestring]
  if ttag_time gt 0.0 then begin
     text = [text, $
          '  RateTTAG = '+ratettag]
  endif
  if n_elements(region) eq 4 then begin
     text = [text, $
          '  Median Pos  = ['+strtrim(medx+info.xoff,2)+','+strtrim(medy+info.yoff,2)+']', $
          '  Average Pos = ['+strtrim(avgx+info.xoff,2)+','+strtrim(avgy+info.yoff,2)+']' ]
  endif

  text = [text, $
          ' ', $
          'Results after background subtraction', $
          '  Background = '+strtrim(background,2), $
          '  Minimum = '+strtrim(minvb,2), $
          '  Maximum = '+strtrim(maxvb,2), $
          '  Total = '+strtrim(totback,2), $
          '  Median = '+strtrim(medback,2), $
          '  Mean = '+strtrim(meanback,2), $
          '  Rate = '+ratebackstring $
         ]
   if ttag_time gt 0.0 then begin
      text = [text, $
           '  RateTTAG = '+ratebackttag]
   endif

   ShowText, filename, GROUP=group, INFOTEXT=text, $
             HEIGHT=20, WIDTH=60,font='6X13', PRINTCMD=CSOL_GETENV('CSOL_PRINT_CMD')

end

;
; ===========================================================  csol_viewer_DEFROI
;
; Routine to define region of interest
;
pro csol_viewer_defroi,event,window_id, info=info

;
; csol_viewer_State Vector
;   csol_viewer_State(0) = 0 not processing stats
;         1 Processing Box
;         2 Processing Defroi
;         9 Box PHA
;        10 Box X_WALK
;        11 Box Y_WALK
;        12 Processing Brightness/Contrast
;   csol_viewer_State(1) = 0 Ready to begin processing
;       = 1 Button Pressed for first position of box or button
;         Pressed for defroi
;       = 2 Button pressed for second position of box or button
;         released for defroi
;   csol_viewer_State(2) = window id of window being processed
;
;
; convert x and y to data coordinates
;
   x = event.x
   y = event.y
   case window_id of
      info.big_id: begin
         xdata = x
         ydata = y
         end
      info.little_id: begin
         factorx = info.little_nx/info.ns
         factory = info.little_ny/info.nl
         xdata = long(x/factorx)
         ydata = long(y/factory)
         end
      info.zoom_id: begin
         xdata = long(x/info.zoom_factor) + info.xoffzoom
         ydata = long(y/info.zoom_factor) + info.yoffzoom
         end
   endcase
;
; BOX Processing
;
   if info.csol_viewer_state[0] ne 2 then begin
;
; erase previous overlay
;
       if (info.csol_viewer_state[1] eq 0) or $
          ((info.csol_viewer_state[1] eq 1) and (info.xregion[1] eq -1)) then begin
          csol_viewer_plots,[info.csol_viewer_state[2],info.csol_viewer_state[2]],$
             [0,info.nl],color=255,/o,info=info
          csol_viewer_plots,[0,info.ns],[info.csol_viewer_state[3],info.csol_viewer_state[3]],$
             color=255,/o,info=info
          info.csol_viewer_state[2] = -1
          info.csol_viewer_state[3] = -1
       endif else begin
          xx = [info.xregion[0],info.xregion[1],info.xregion[1], info.xregion[0],info.xregion[0]]
          yy = [info.yregion[0],info.yregion[0],info.yregion[1], info.yregion[1],info.yregion[0]]
          csol_viewer_plots,xx,yy,color=255,/o,info=info
       endelse
       if event.press eq 0 then begin
          if info.csol_viewer_state[1] eq 0 then begin
             info.csol_viewer_state[2] = xdata
             info.csol_viewer_state[3] = ydata
             csol_viewer_plots,[info.csol_viewer_state[2],info.csol_viewer_state[2]],$
                [0,info.nl],color=255,/o,info=info
             csol_viewer_plots,[0,info.ns],[info.csol_viewer_state[3],info.csol_viewer_state[3]],$
                color=255,/o,info=info
          endif else begin
             info.xregion[1] = xdata
             info.yregion[1] = ydata
             xx = [info.xregion[0],info.xregion[1],info.xregion[1], info.xregion[0],info.xregion[0]]
             yy = [info.yregion[0],info.yregion[0],info.yregion[1], info.yregion[1],info.yregion[0]]
             csol_viewer_plots,xx,yy,color=255,/o,info=info
          endelse
          return
       endif

       case info.csol_viewer_state[1] of
       0: begin         ;processing not started
         info.csol_viewer_state[1] = 1
         info.xregion[0] = xdata
         info.yregion[0] = ydata
         info.xregion[1] = -1
         info.yregion[1] = -1
         info.nregion = 1
         widget_control,info.HEXmessage,set_value = 'Place cursor on ' + $
               'opposite corner and press left button'
         return
         end
      1: begin         ;waiting for second corner
         info.csol_viewer_state[1] = 2
         info.xregion[1] = xdata
         info.yregion[1] = ydata
         info.xregion[0:4] = [info.xregion[0],info.xregion[1],info.xregion[1], $
            info.xregion[0],info.xregion[0]]
         info.yregion[0:4] = [info.yregion[0],info.yregion[0],info.yregion[1], $
            info.yregion[1],info.yregion[0]]
         info.nregion = 5
         end
       endcase
   endif else begin
;
; Draw Processing
;
       case info.csol_viewer_state[1] of
          0: begin         ;not yet started
             if event.press eq 1 then begin
                info.csol_viewer_state[1] = 1
                info.xregion[0] = xdata
                info.yregion[0] = ydata
                info.nregion = 1
             endif
             return
         end
         1: begin
            info.xregion[info.nregion] = xdata
            info.yregion[info.nregion] = ydata
            info.nregion = info.nregion + 1
            if event.release gt 0 then info.csol_viewer_state[1] = 2
         end
       endcase
   endelse
;
; Draw region in all three windows
;
   i1 = info.nregion-2
   i2 = info.nregion-1
   if (info.csol_viewer_state[0] eq 1)  or (info.csol_viewer_state[0] eq 9)  or $
      (info.csol_viewer_state[0] eq 10) or (info.csol_viewer_state[0] eq 11) then i1 = 0   ;draw entire box
   xx = info.xregion[i1:i2]
   yy = info.yregion[i1:i2]
   if (info.csol_viewer_state[0] eq 2) and (info.csol_viewer_state[1] eq 2) then begin  ;back to first point
      xx = [xx,info.xregion[0]]
      yy = [yy,info.yregion[0]]
   endif
;
; convert to window coordinates for all three windows and plot
;
   csol_viewer_plots,xx,yy,info=info
;
; process statistics
;
   if info.csol_viewer_state[1] lt 2 then return
   widget_control,info.HEXmessage,set_value = ' '
   if info.csol_viewer_state[0] eq 10 or info.csol_viewer_state[0] eq 11 then begin
      ; requested a WALK PLOT
      widget_control,/hourglass
      if NOT PTR_VALID((*info.data).pha) then begin
         r = dialog_message('Pulse Height data not available', dialog_parent=event.top,/error)
      endif else begin
         csol_viewer_xyttag,info.xregion,info.yregion, xbin=(*info.data).xbin, ybin=(*info.data).ybin
         x1 = min(info.xregion[0:4])
         x2 = max(info.xregion[0:4])
         y1 = min(info.yregion[0:4])
         y2 = max(info.yregion[0:4])

         timegood = xttag_goodmask(*(*info.data).timetag,*(*info.data).gtime1,*(*info.data).gtime2)
         good = where(timegood,ng)
         if ng eq 0 then begin
            r=dialog_message('No events found',/error,dialog_parent=event.top)
            return
         endif
         nbin = max((*(*info.data).pha)[good]) + 1
         ; if phamask has been reset get a fresh mask
         if NOT PTR_VALID((*info.data).phamask) then begin
            (*info.data).phamask = PTR_NEW(replicate(1b,nbin),/no_copy)
         endif

         if PTR_VALID((*info.data).phamask) gt 0 then begin
            good = where((*(*info.data).xttag ge x1) and (*(*info.data).xttag le x2) and $
                 (*(*info.data).yttag ge y1) and (*(*info.data).yttag le y2) and $
                 xttag_goodmask(*(*info.data).timetag,*(*info.data).gtime1,*(*info.data).gtime2) and $
                 xttag_CSOL_phamask(*(*info.data).pha, *(*info.data).phamask), ngood)
         endif else begin
            good = where((*(*info.data).xttag ge x1) and (*(*info.data).xttag le x2) and $
                 (*(*info.data).yttag ge y1) and (*(*info.data).yttag le y2) and $
                 xttag_goodmask(*(*info.data).timetag,*(*info.data).gtime1,*(*info.data).gtime2), ngood)
         endelse

         if ngood lt 1 then begin
            r = dialog_message('No events in box',/error,dialog_parent = event.top)
         endif else begin
            ; generate the WALK image
            if info.csol_viewer_state[0] eq 10 then begin
               img = hist_2d((*(*info.data).xttag)[good],(*(*info.data).pha)[good],$
                  min1=x1,max1=x2,min2=0)
            endif else if info.csol_viewer_state[0] eq 11 then begin
               img = hist_2d((*(*info.data).yttag)[good],(*(*info.data).pha)[good],$
                  min1=y1,max1=y2,min2=0)
            endif
            result=dialog_input(wtitle='Walk Plot Zoom Factors', $
               prompt=['X or Y Zoom:', 'PHA Zoom:'], initial=[1,4], $
               nfields=2, dialog_parent=event.top)
            if result[0] ne '' then begin
               if strnumber(result[0]) eq 1 and strnumber(result[1]) eq 1 then begin
                  zoom1 = FLOAT(result[0])
                  zoom2 = FLOAT(result[1])
                  sz = size(img)
                  img = frebin(img, sz[1]*zoom1, sz[2]*zoom2)
               endif
            endif
            csol_viewer,img, wtitle=info.wtitle+'  Walk Plot'
            img=0b
         endelse
      endelse
   endif else if info.csol_viewer_state[0] eq 9 then begin   ;pha
      widget_control,/hourglass
      if NOT PTR_VALID((*info.data).pha) then begin
         r = dialog_message('Timetag events not available',dialog_parent=event.top,/error)
      endif else begin
         csol_viewer_xyttag,info.xregion,info.yregion,xbin=(*info.data).xbin,ybin=(*info.data).ybin
         x1 = min(info.xregion[0:4], max=x2)
         y1 = min(info.yregion[0:4], max=y2)

         timegood = xttag_goodmask(*(*info.data).timetag,*(*info.data).gtime1,*(*info.data).gtime2)
         good = where(timegood,ng)
         if ng eq 0 then begin
            r=dialog_message('No events found',/error,dialog_parent=event.top)
            return
         endif
         nbin = max((*(*info.data).pha)[good]) + 1
         ; if phamask has been reset get a fresh mask
         if NOT PTR_VALID((*info.data).phamask) then begin
            (*info.data).phamask = PTR_NEW(replicate(1b,nbin), /no_copy)
         endif

         if PTR_VALID((*info.data).phamask) then begin
            good = where((*(*info.data).xttag ge x1) and (*(*info.data).xttag le x2) and $
                 (*(*info.data).yttag ge y1) and (*(*info.data).yttag le y2) and $
                 xttag_goodmask(*(*info.data).timetag,*(*info.data).gtime1,*(*info.data).gtime2) and $
                 xttag_CSOL_phamask(*(*info.data).pha, *(*info.data).phamask), ngood)
         endif else begin
            good = where((*(*info.data).xttag ge x1) and (*(*info.data).xttag le x2) and $
                 (*(*info.data).yttag ge y1) and (*(*info.data).yttag le y2) and $
                 xttag_goodmask(*(*info.data).timetag,*(*info.data).gtime1,*(*info.data).gtime2), ngood)
         endelse

         if ngood lt 1 then begin
            r = dialog_message('No events in box',/error,dialog_parent = event.top)
         endif else begin
            ;phahist = histogram(pha(good),min=1,max=31)
            phahist = histogram((*(*info.data).pha)[good])  ;no limit on pha
            fdecomp,info.filename,disk,dir,fname,ext
            title=fname+'.'+ext
            title = title + '  ['+strtrim(x1,2)+':'+ $
               strtrim(x2,2)+','+strtrim(y1,2)+ ':'+strtrim(y2,2)+']'
            nbin = max(where(phahist gt 0.0)) + 1
            plotcmd='plot,xtitle="PHA",ytitle="Fraction of events",title="'+title+'"'
            plotcmd=plotcmd + ',/xstyle,/ystyle,psym=10'
            plot_data,'plot',xtitle="PHA",ytitle="Fraction of events",title=title,/xstyle,/ystyle,psym=10,$
               findgen(nbin),phahist/total(phahist),wtitle=info.filename
         endelse
      endelse
   endif else begin
      if info.nregion gt 2 then begin
         index = polyfillv(info.xregion[0:info.nregion-1], $
                  info.yregion[0:info.nregion-1],info.ns,info.nl)
         n = n_elements(index)
         widget_control,info.min_field,get_value=minv
         ttag_time=0.0
         if info.csol_viewer_state[0] eq 1 then begin
            title='Statistics in Box'
            x1=min(info.xregion[[0,1]],max=x2)
            y1=min(info.yregion[[0,2]],max=y2)
            title=title+' ['+strtrim(x1+info.xoff,2)+':'+ strtrim(x2+info.xoff,2)+', '+$
               strtrim(y1+info.yoff,2)+':'+strtrim(y2+info.yoff,2)+']'
            ; create a temporary image to pass to csol_viewer_stats
            areadata = (*info.orig)[x1:x2,y1:y2]
            if ptr_valid((*info.data).timetag) then begin
               if n_elements(*(*info.data).timetag) ge 2 then begin
                  ; calculate the actual time between first and last event
                  pos = where((*(*info.data).xttag) ge x1 and (*(*info.data).xttag) le x2 and $
                     (*(*info.data).yttag) ge y1 and (*(*info.data).yttag) le y2, count)
                  if count gt 2 then ttag_time = (*(*info.data).timetag)[pos[count-1L]] - (*(*info.data).timetag)[pos[0]]
               endif
            endif
         endif else begin
            title='Statistics in Drawn Region'
            ; create a temporary image to pass to csol_viewer_stats
            posx=index MOD info.ns
            posy=index/info.ns
            x1=min(posx,max=x2)
            y1=min(posy,max=y2)
            areadata=(*info.orig)[x1:x2,y1:y2] * 0
            areadata[posx-x1,posy-y1]=(*info.orig)[index]
         endelse
         if n gt 1 then begin
            widget_control,/hourglass

            region = [x1,y1,x2,y2]
            csol_viewer_stats,areadata,minv,group=event.top, title=title, info=info, $
               filename=info.filename, exptime=info.exptime, ttag_time=ttag_time,region=region
            widget_control,/hourglass
         endif
      endif
   endelse
   info.nregion = 0
   info.csol_viewer_state = [0,0,-1,-1]
return
end
;
; =========================================================  csol_viewer_LINEPLOT
;
; Routine to plot row/column sums
;
pro csol_viewer_lineplot,event,window,info=info
;

; csol_viewer_State Vector
;
;   csol_viewer_State(0) = 3 Row
;                  4 Column
;         5 Row Sum
;         6 Column Sum
;         7 Linefind rowsum
;         8 Cross Section
;   csol_viewer_State(1) = Number of points measured
;
;
; If window not supplied, initialize csol_viewer_state vector, and print instructions
;
;
   fdecomp,info.filename,disk,dir,file
;
; erase previous overlay
;
   if info.csol_viewer_state[3] ge 0 then begin
       case info.csol_viewer_state(0) of
      3: csol_viewer_plots,[0,info.ns],[info.csol_viewer_state(3),info.csol_viewer_state(3)],$
         /o,color=255,info=info
      4: csol_viewer_plots,[info.csol_viewer_state(2),info.csol_viewer_state(2)],[0,info.nl],$
         /o,color=255,info=info
      5: csol_viewer_plots,[0,info.ns],[info.csol_viewer_state(3),info.csol_viewer_state(3)],$
         /o,color=255,info=info
      6: csol_viewer_plots,[info.csol_viewer_state(2),info.csol_viewer_state(2)],[0,info.nl],$
         /o,color=255,info=info
      7: csol_viewer_plots,[0,info.ns],[info.csol_viewer_state(3),info.csol_viewer_state(3)],$
         /o,color=255,info=info
      8: if info.xregion(1) ne -1 then begin
            xsection_corners,info.xregion(0),info.yregion(0), $
               info.xregion(1),info.yregion(1),info.csol_viewer_state(4),xcorn,ycorn,dist
            csol_viewer_plots,[xcorn,xcorn(0)],[ycorn,ycorn(0)],/o,color=255,info=info
         endif
      19: csol_viewer_plots,[info.csol_viewer_state(2),info.csol_viewer_state(2)],[0,info.nl],$
            /o,color=255,info=info
       endcase
   endif

;
; convert x and y to data coordinates
;
   csol_viewer_convert,event.x,event.y,window,x,y,info=info
   info.xregion[info.csol_viewer_state[1]] = x
   info.yregion[info.csol_viewer_state[1]] = y
   info.csol_viewer_state[2:3] = [x,y]
;
; If button not pressed then plot new overlay and return
;
   if event.press ne 1 then begin
       case info.csol_viewer_state[0] of
      3: csol_viewer_plots,[0,info.ns],[info.csol_viewer_state(3),info.csol_viewer_state(3)],$
         /o,color=255,info=info
      4: csol_viewer_plots,[info.csol_viewer_state(2),info.csol_viewer_state(2)],[0,info.nl],$
         /o,color=255,info=info
      5: csol_viewer_plots,[0,info.ns],[info.csol_viewer_state(3),info.csol_viewer_state(3)],$
         /o,color=255,info=info
      6: csol_viewer_plots,[info.csol_viewer_state(2),info.csol_viewer_state(2)],[0,info.nl],$
         /o,color=255,info=info
      7: csol_viewer_plots,[0,info.ns],[info.csol_viewer_state(3),info.csol_viewer_state(3)],$
         /o,color=255,info=info
      8: begin
         if info.csol_viewer_state[1] eq 1 then begin
            xsection_corners,info.xregion[0],info.yregion[0], $
                 info.xregion[1],info.yregion[1],info.csol_viewer_state[4],xcorn,ycorn,dist
            csol_viewer_plots,[xcorn,xcorn[0]],[ycorn,ycorn[0]],/o,color=255,info=info
         endif
         end
      19: csol_viewer_plots,[info.csol_viewer_state[2],info.csol_viewer_state[2]],[0,info.nl],$
         /o,color=255,info=info
       endcase
       return
   endif

;
; button pressed
;
   info.csol_viewer_state[1] = info.csol_viewer_state[1] + 1
;
; Row Plot
;
   if info.csol_viewer_state[0] eq 3 then begin
      csol_viewer_plots,[0,info.ns],[info.csol_viewer_state[3],info.csol_viewer_state[3]],line=1,info=info
      xrange = [0,info.ns]
      if window eq 'ZOOM' then $
            xrange = [info.xoffzoom,info.xoffzoom+info.zoom_width/info.zoom_factor]
      if window eq 'BIG' then begin
         widget_control,info.big_window,get_draw_view=v
         xrange = [v[0],(v[0]+720)<(info.ns-1)]
      end
      csol_viewer_xyttag,xrange,0,/frac, xbin=(*info.data).xbin, ybin=(*info.data).ybin, $
         xoff=info.xoff, yoff=info.yoff
      xv = findgen(info.ns)
      csol_viewer_xyttag,xv,0,/frac, xbin=(*info.data).xbin, ybin=(*info.data).ybin, $
         xoff=info.xoff, yoff=info.yoff
      ptitle=info.wtitle+'  Row '+ strtrim((y+info.yoff)*(*info.data).ybin,2)
      title=file+'  Row '+ strtrim((y+info.yoff)*(*info.data).ybin,2)
      ; use the big_image instead of orig in case we filtered data with time or pha
      if NOT PTR_VALID(info.big_image) then begin
         svp_lineplot,xv,(*info.orig)[*,y],title=title,xrange=xrange,ptitle=ptitle,$
            header=*info.header,outfile=file
      endif else begin
         svp_lineplot,xv,(*info.big_image)[*,y],title=title,xrange=xrange,ptitle=ptitle,$
            header=*info.header,outfile=file
      endelse
   end
;
; Column Plot
;
   if info.csol_viewer_state[0] eq 4 then begin
      csol_viewer_plots,[info.csol_viewer_state(2),info.csol_viewer_state(2)],[0,info.nl],line=1,info=info
      xrange = [0,info.nl]
      if window eq 'ZOOM' then $
            xrange = [info.yoffzoom,info.yoffzoom+info.zoom_width/info.zoom_factor]
      if window eq 'BIG' then begin
         widget_control,info.big_window,get_draw_view=v
         xrange = [v[1],(v[1]+512)<(info.nl-1)]
      end
      csol_viewer_xyttag,0,xrange,/frac, xbin=(*info.data).xbin, ybin=(*info.data).ybin, $
         xoff=info.xoff, yoff=info.yoff
      xv = findgen(info.nl)
      csol_viewer_xyttag,0,xv,/frac, xbin=(*info.data).xbin, ybin=(*info.data).ybin, $
         xoff=info.xoff, yoff=info.yoff
      ptitle=info.wtitle+'  Column '+strtrim((x+info.xoff)*(*info.data).xbin,2)
      title=file+'  Column '+strtrim((x+info.xoff)*(*info.data).xbin,2)
      ; use the big_image instead of orig in case we filtered data with time or pha
      if NOT PTR_VALID(info.big_image) then begin
         svp_lineplot,xv,reform((*info.orig)[x,*]),title=title,xrange=xrange,ptitle=ptitle,$
            header=*info.header,outfile=file
      endif else begin
         svp_lineplot,xv,reform((*info.big_image)[x,*]),title=title,xrange=xrange,ptitle=ptitle,$
            header=*info.header,outfile=file
      endelse
   end
;
; Row Sum
;
   if (info.csol_viewer_state(0) eq 5) or (info.csol_viewer_state(0) eq 7) then begin
      if (info.csol_viewer_state(1) eq 2) then begin
         csol_viewer_plots,[0,info.ns],[info.yregion(0),info.yregion(0)],/o,color=255,info=info
         csol_viewer_plots,[0,info.ns],[info.csol_viewer_state(3),info.csol_viewer_state(3)],line=1,info=info
         xrange = [0,info.ns]
         if window eq 'ZOOM' then $
            xrange = [info.xoffzoom,info.xoffzoom+info.zoom_width/info.zoom_factor]
         if window eq 'BIG' then begin
            widget_control,info.big_window,get_draw_view=v
            xrange = [v(0),(v(0)+720)<(info.ns-1)]
         end
         y1 = info.yregion(0)<info.yregion(1)
         y2 = info.yregion(0)>info.yregion(1)
         if NOT PTR_VALID(info.big_image) then begin
            if y1 eq y2 then data = (*info.orig)[*,y1] $
            else data = total((*info.orig)[*,y1:y2],2)
         endif else begin
            if y1 eq y2 then data = (*info.big_image)[*,y1] $
            else data = total((*info.big_image)[*,y1:y2],2)
         endelse
         ptitle = info.wtitle+'  Rows '+strtrim((y1+info.yoff)*(*info.data).ybin,2)+' to '+$
            strtrim((y2+info.yoff)*(*info.data).ybin,2)
         title = file+'  Rows '+strtrim((y1+info.yoff)*(*info.data).ybin,2)+' to '+$
            strtrim((y2+info.yoff)*(*info.data).ybin,2)
         csol_viewer_xyttag,xrange,0,/frac, xbin=(*info.data).xbin, ybin=(*info.data).ybin, $
            xoff=info.xoff, yoff=info.yoff
         xv = findgen(info.ns)
         csol_viewer_xyttag,xv,0,/frac, xbin=(*info.data).xbin, ybin=(*info.data).ybin, $
            xoff=info.xoff, yoff=info.yoff

         if info.csol_viewer_state(0) eq 5 then begin
            svp_lineplot,xv,data,title=title,xrange=xrange,ptitle=ptitle,header=*info.header,outfile=file
         endif else begin
            mn=min(xv,max=mx)
            findlines, xv, data, /gui, title=title, xrange=xrange, yrange=[y1,y2],$
               baseline=[mn,0.0,mx,0.0], filename=info.filename, minheight=8, $
               image=info.orig, header=*info.header, /silent
         endelse
      endif else $
         csol_viewer_plots,[0,info.ns],[info.csol_viewer_state(3),info.csol_viewer_state(3)],$
         line=1,info=info
   endif
;
; Column Sum
;
   if (info.csol_viewer_state(0) eq 6) or (info.csol_viewer_state(0) eq 19) then begin
      if (info.csol_viewer_state(1) eq 2) then begin
         csol_viewer_plots,[info.xregion(0),info.xregion(0)],[0,info.nl],/o,color=255,info=info
         csol_viewer_plots,[info.csol_viewer_state(2),info.csol_viewer_state(2)],[0,info.nl],$
            line=1,info=info
         xrange = [0,info.nl]
         if window eq 'ZOOM' then $
            xrange = [info.yoffzoom,info.yoffzoom+info.zoom_width/info.zoom_factor]
         if window eq 'BIG' then begin
            widget_control,info.big_window,get_draw_view=v
            xrange = [v(1),(v(1)+512)<(info.nl-1)]
         end
         x1 = info.xregion(0)<info.xregion(1)
         x2 = info.xregion(0)>info.xregion(1)
         if NOT PTR_VALID(info.big_image) then begin
            if x1 eq x2 then data = reform((*info.orig)[x1,*]) $
            else data = total((*info.orig)[x1:x2,*],1)
         endif else begin
            if x1 eq x2 then data = reform((*info.big_image)[x1,*]) $
            else data = total((*info.big_image)[x1:x2,*],1)
         endelse
         ptitle=info.wtitle+'  Columns '+strtrim((x1+info.xoff)*(*info.data).xbin,2)+' to '+$
            strtrim((x2+info.xoff)*(*info.data).xbin,2)
         title=file+'  Columns '+strtrim((x1+info.xoff)*(*info.data).xbin,2)+' to '+$
            strtrim((x2+info.xoff)*(*info.data).xbin,2)
         csol_viewer_xyttag,0,xrange,/frac, xbin=(*info.data).xbin, ybin=(*info.data).ybin, $
            xoff=info.xoff, yoff=info.yoff
         xv = findgen(info.nl)
         csol_viewer_xyttag,0,xv,/frac, xbin=(*info.data).xbin, ybin=(*info.data).ybin, $
            xoff=info.xoff, yoff=info.yoff

         if info.csol_viewer_state(0) eq 6 then begin
            svp_lineplot,xv,data,title=title,xrange=xrange,header=*info.header,outfile=file,ptitle=ptitle
         endif else begin
            mn=min(xv,max=mx)
            findlines, xv, data, /gui, title=title, xrange=xrange, yrange=[x1,x2],$
               baseline=[mn,0.0,mx,0.0], filename=info.filename, minheight=8, $
               image=info.orig,/COL, header=*info.header, /silent
         endelse
       endif else $
         csol_viewer_plots,[info.csol_viewer_state(2),info.csol_viewer_state(2)],[0,info.nl],$
            line=1,info=info
   endif
;
; x-section plot
;
   if (info.csol_viewer_state[0] eq 8) and (info.csol_viewer_state[1] eq 2) then begin
      xsection_corners,info.xregion[0],info.yregion[0], $
            info.xregion[1],info.yregion[1],info.csol_viewer_state[4],xcorn,ycorn,dist
      csol_viewer_plots,[xcorn,xcorn[0]],[ycorn,ycorn[0]],info=info
      if NOT PTR_VALID(info.big_image) then begin
         xsection_compute,info.orig,xcorn,ycorn,dist,info.csol_viewer_state[4],data
      endif else begin
         xsection_compute,info.big_image,xcorn,ycorn,dist,info.csol_viewer_state[4],data
      endelse
      csol_viewer_xyttag,info.xregion,info.yregion,/frac, xbin=(*info.data).xbin, ybin=(*info.data).ybin, $
         xoff=info.xoff, yoff=info.yoff
      dist = sqrt((info.xregion[1]-info.xregion[0])^2. + (info.yregion[1]-info.yregion[0])^2.)
      title='X '+strtrim(info.xregion[0]+info.xoff,2)+' '+strtrim(info.yregion[0]+info.yoff,2)+ $
         ' to '+strtrim(info.xregion[1]+info.xoff,2)+' '+strtrim(info.yregion[1]+info.yoff,2)
      xv = findgen(n_elements(data))
      xv = xv/max(xv)*dist
      svp_lineplot,xv,data,title=file+'  '+title,ptitle=info.wtitle+'  '+title,xrange=[0,dist],header=*info.header,outfile=file

   end

;
; Do we need second point?
;
   if (info.csol_viewer_state[0] gt 4) and (info.csol_viewer_state[1] eq 1) then begin
      if (info.csol_viewer_state[0] eq 5) or (info.csol_viewer_state[0] eq 7) then $
         mess = 'Select last row and click left button' $
      else mess = 'Select last column and click left button'
      if info.csol_viewer_state[0] eq 8 then $
         mess = 'Select ending point and click left button'
      widget_control,info.HEXmessage,set_value=mess
   endif else begin
      widget_control,info.HEXmessage,set_value=info.scale_type + ' Display'
      info.csol_viewer_state = [0,0,-1,-1,0]
   endelse

end
; ================================================================ XSECTION_*
;
; Routines for Cross section plots
;
pro xsection_corners,x1,y1,x2,y2,width,xcorners,ycorners,dist
;
; Compute corners for cross section plot from (x1,y1) to (x2,y2)
; with a width of WIDTH
;
   dist = sqrt(float(x2-x1)^2.+float(y2-y1)^2.)
   theta = atan(y2-y1,x2-x1)
   if (x2 eq x1) and (y2 eq y1) then x2 = x1+1

   dx = (width/2.0)*sin(theta)
   dy = (width/2.0)*cos(theta)
   xcorners = [x1+dx,x2+dx,x2-dx,x1-dx]
   ycorners = [y1-dy,y2-dy,y2+dy,y1+dy]
   return
   end

pro xsection_compute,image,xcorners,ycorners,dist,width,xsection
;
; routine to compute cross-section using four corners of box computed by
; xsection_corners
;
   x0 = [0,dist,dist,0]
   y0 = [0,0,width,width]
   polywarp,xcorners,ycorners,x0,y0,1,kx,ky
   if size(image,/TNAME) eq 'POINTER' then begin
      pic = poly_2d(*image,kx,ky,1,round(dist+0.5)>2,width)
   endif else begin
      pic = poly_2d(image,kx,ky,1,round(dist+0.5)>2,width)
   endelse
   if width eq 1 then xsection = pic else xsection = total(pic,2)
return
end

;============================================================  csol_viewer_PS
;
; Routine to generate postscript output files
;
pro csol_viewer_ps,color=color,reversed=reversed,print=print,png=png,jpg=jpg,info=info

   if n_elements(color) eq 0 then color=0
;
; get output file
;
   xsize=10.0  & ysize=7.5

   path=CSOL_GETENV('CSOL_RESULTS_PATH')
   fdecomp,info.filename,disk,directory,name,ext
   if path eq '' then path=directory

   if KEYWORD_SET(print) THEN BEGIN
      file='csol_viewer.ps'
   endif else if keyword_set(png) or keyword_set(jpg) then begin
      ; create a pixmap window to draw everything
      !p.background=255
      !p.color=0
      xsize=1000 & ysize=750
      thisWindow=!d.window
      window, xs=xsize, ys=ysize, /pixmap, /free
      pixmap=!d.window
      if keyword_set(png) then file=name+'.png' else file=name+'.jpg'
      file = dialog_pickfile(file=file, path=path,/write)
      if file eq '' then return   ;no file selected
   endif else begin
      fdecomp,info.filename,disk,directory,name,ext
      file = name+'.ps'
      file = dialog_pickfile(file=file, path=path,filter='*.ps',/write)
      if file eq '' then return   ;no file selected
   endelse
;
; get images to be displayed
;
   wset,info.little_id & pic1 = tvrd()
   wset,info.zoom_id & pic2 = tvrd()
   wset,info.big_id
   widget_control,info.big_window,get_draw_view=v
   pic3 = tvrd(v[0],v[1],745<(info.ns-v[0]),515<(info.nl-v[1]))
;
; rescale to 0 to 255
;
   nc = (!d.n_colors-1) < 255
   scale = 255/float(nc)
   pic1 = byte(pic1*scale+0.5)
   pic2 = byte(pic2*scale+0.5)
   pic3 = byte(pic3*scale+0.5)
;
; reverse
;
   if keyword_set(reversed) then begin
      pic1 = 255b-pic1
      pic2 = 255b-pic2
      pic3 = 255b-pic3
   end
;
; get color table
;
   tvlct,r,g,b,/get
   my_p=!p
;
; load color table
;
   if color then tvlct,frebin(r[12:*],255),frebin(g[12:*],255),frebin(b[12:*],255),12
;
; set up postscript file
;
   !p.font = 0
	xpos1=0.7*xsize & ypos1=ysize/15.0 & xsize1=0.3*xsize & ysize1=0.3*xsize
	xpos2=0.7*xsize & ypos2=0.53*ysize & xsize2=0.3*xsize & ysize2=0.3*xsize
	xpost=0.12 & ypost=0.2 & yofft=0.02667
   s = size(pic3) & nx = float(s(1)) & ny = float(s(2))
   rat = nx/ny
   if rat gt 745.0/515.0 then begin
      xsize3 = 0.694*xsize
      ysize3 = xsize3/rat
   end else begin
      xsize3 = (0.48*xsize)*rat
      ysize3 = xsize3/rat
   end
   xpos3=0.0 & ypos3=(0.7*xsize)-ysize3
   bar = bindgen(244,1)
   if keyword_set(reversed) then bar = reverse(bar)
   xposb=0.125*xsize & yposb=0.2*ysize & xsizeb=0.425*xsize & ysizeb=0.04*ysize
;
; display images
;
   if keyword_set(png) or keyword_set(jpg) then begin
      wset,pixmap
      tv,frebin(pic1,xsize1,ysize1),xpos1,ypos1,xsize=xsize1,ysize=ysize1
      tv,frebin(pic2,xsize2,ysize2),xpos2,ypos2,xsize=xsize2,ysize=ysize2
      tv,frebin(pic3,xsize3,ysize3),xpos3,ypos3,xsize=xsize3,ysize=size3
      tv,frebin(bar,xsizeb,ysizeb),xposb,yposb,xsize=xsizeb,ysize=ysizeb
   endif else begin
      thisDevice=!d.name
      set_plot,'ps'
      device,/land,xsize=xsize,ysize=ysize,xoff=0.5,yoff=10.5,color=color, $
         file=file,bits=8,/inches
      tv,pic1,xpos1,ypos1,xsize=xsize1,ysize=ysize1,/inches
      tv,pic2,xpos2,ypos2,xsize=xsize2,ysize=ysize2,/inches
      tv,pic3,0,ypos3,xsize=xsize3,ysize=size3,/inches
      tv,bar,xposb,yposb,xsize=xsizeb,ysize=ysizeb,/inches
   endelse

;
; write image scaling information
;
   widget_control,info.min_field,get_value=imin
   widget_control,info.max_field,get_value=imax
   case info.scale_type of
      'Linear': title = 'Linear Display'
      'Log': title = 'Logarithmic Display'
      'Sqrt': title = 'Square Root Display'
      'Hist. Eq.': title = 'Histogram Equalized Display'
   end
   xyouts,(xposb+xsizeb/2.0)/xsize,(yposb+ysizeb*1.2)/ysize,title,/norm,align=0.5
   xyouts,(xposb*0.99)/xsize,(yposb+ysizeb/2.2)/ysize,strtrim(imin,2),align=1.0,/norm
   xyouts,(xposb+xsizeb*1.01)/xsize,(yposb+ysizeb/2.2)/ysize,strtrim(imax,2),align=0.0,/norm
;
; draw location of pic2 in pic3
;
   x1 = info.xoffzoom
   y1 = info.yoffzoom
   x2 = x1 + info.zoom_width/info.zoom_factor
   y2 = y1 + info.zoom_width/info.zoom_factor
   if (x1 ge v(0)) and (y1 ge v(1)) and (x2 le (v(0)+nx-1)) $
      and (y2 le (v(1)+ny-1)) then begin
      x1 = (x1 - v(0))/nx*xsize3/xsize
      x2 = (x2 - v(0))/nx*xsize3/xsize
      y1 = ((y1 - v(1))/ny*ysize3+ypos3)/ysize
      y2 = ((y2 - v(1))/ny*ysize3+ypos3)/ysize
      plots,[x1,x2,x2,x1,x1],[y1,y1,y2,y2,y1],/norm,thick=2
      plots,[x1,x2,x2,x1,x1],[y1,y1,y2,y2,y1],/norm,thick=2, color=255,line=2
   endif
;
; draw location of pic3 in pic1
;
   x1 = v(0)/float(info.ns)*xsize1/xsize + xpos1/xsize
   x2 = (v(0) + nx - 1)/float(info.ns)*xsize1/xsize + xpos1/xsize
   y1 = v(1)/float(info.nl)*ysize1/ysize + ypos1/ysize
   y2 = (v(1) + ny - 1)/float(info.nl)*ysize1/ysize + ypos1/ysize
   plots,[x1,x2,x2,x1,x1],[y1,y1,y2,y2,y1],/norm,thick=2
   plots,[x1,x2,x2,x1,x1],[y1,y1,y2,y2,y1],/norm,thick=2,color=255,line=2
;
; plot boxes around images
;
   x = [0,1,1,0,0]
   y = [0,0,1,1,0]
   plots,(x*xsize1+xpos1)/xsize,(y*ysize1+ypos1)/ysize,/norm,thick=2
   plots,(x*xsize2+xpos2)/xsize,(y*ysize2+ypos2)/ysize,/norm,thick=2
   plots,(x*xsize3+xpos3)/xsize,(y*ysize3+ypos3)/ysize,/norm,thick=2
   plots,(x*xsizeb+xposb)/xsize,(y*ysizeb+yposb)/ysize,/norm,thick=2
;
; write position information
;
   widget_control,info.big_position,get_value=v
   xyouts,0.01,(ypos3/ysize)-0.02,v(0),/norm,charsize=0.8
   widget_control,info.zoom_position,get_value=v
   xyouts,(xpos2/xsize)+0.01,(ypos2/ysize)-0.02,v(0)+'  zoom = '+strtrim(info.zoom_factor,2) $
            ,/norm,charsize=0.8
;
; write header information
;
   ; fdecomp,info.filename,disk,dir,name,ext
   ; title=name+'.'+ext
   title=info.wtitle
   xyouts,0.5,0.946667,title,/norm,align=0.5
   ; skip if no header
   if n_elements(*info.header) gt 1 then begin
      xyouts,xpost,ypost-yofft,/norm, $
         'Targname = '+strtrim(sxpar(*info.header,'targname',/silent),2)
      xyouts,xpost,ypost-yofft*2,/norm,strtrim(sxpar(*info.header,'dateobs',/silent),2) + $
            ' '+ strtrim(sxpar(*info.header,'timeobs',/silent),2)
      xyouts,xpost,ypost-yofft*3,/norm, $
            'Detector = '+strtrim(sxpar(*info.header,'detector',/silent),2)
      xyouts,xpost,ypost-yofft*4,/norm, $
            'Aperture = '+strtrim(sxpar(*info.header,'aperture',/silent),2)
   endif

   if keyword_set(png) or keyword_set(jpg) then begin
      wset,pixmap
      img=tvrd()
      wset,thisWindow
      if keyword_set(png) then write_image, file, 'PNG', img, r, g, b
      if keyword_set(jpg) then write_image, file, 'JPEG', img, r, g, b
      wdelete, pixmap
   endif else begin
      device,/close
      set_plot,thisDevice,/copy
      tvlct,r,g,b
      !p = my_p

      if KEYWORD_SET(print) then status=send2printer(file)
   endelse

return
end


;
;============================================================ csol_viewer_CONVERT
;
; Routine to convert x,y coordinates from screen to data and vice versa
;
pro csol_viewer_convert,xin,yin,window,xout,yout,to_screen=to_screen,info=info
;
; Inputs: xin, yin, window
;   window = 'big','little', or 'zoom'
; Outputs: xout, yout
; Keyword: /to_screen - if supplied coordinates are converted from
;         data to screen coord, otherwise conversion is
;         for screen to data.
;
; conversion from screen to data
;
   if keyword_set(to_screen) then begin

      case window of
         'BIG': begin
            xout = xin
            yout = yin
            end
         'LITTLE': begin
            xfactor = info.ns/info.little_nx
            yfactor = info.nl/info.little_ny
            xout = fix(xin/info.xfactor)
            yout = fix(yin/info.yfactor)
            end
         'ZOOM': begin
            xout = (xin-info.xoffzoom)*info.zoom_factor + info.zoom_factor/2
            yout = (yin- nfo.yoffzoom)*info.zoom_factor + info.zoom_factor/2
            end
       endcase
   endif else begin
;
; conversion for screen to data
;
      case window of
         'BIG': begin
            xout = xin
            yout = yin
            end
         'LITTLE': begin
            xfactor = info.ns/info.little_nx
            yfactor = info.nl/info.little_ny
            xout = fix(xin*xfactor+xfactor/2)
            yout = fix(yin*yfactor+yfactor/2)
            end
         'ZOOM': begin
            xout = long(xin/info.zoom_factor) + info.xoffzoom
            yout = long(yin/info.zoom_factor) + info.yoffzoom
            end
      end
      xout = xout>0<(info.ns-1)
      yout = yout>0<(info.nl-1)
   end
   return
end

;--------------------------------------------------------- show_table
;
pro show_table, info, data

   if info.wtable ne 0L then widget_control, info.wtable, /DESTROY

   WIDGET_CONTROL, info.table_main, TLB_SET_TITLE=info.title

   info.wtable = WIDGET_TABLE(info.table_main, VALUE=data, FORMAT='(F12.3)', $
                 COLUMN_LABELS=info.col_labels, FONT='6x13', /SCROLL, $
                 SCR_XSIZE=info.tbl_scr_x, SCR_YSIZE=info.tbl_scr_y, $
                 UVALUE='TABLE', /RESIZEABLE_COL, /ALL_EVENTS)

   widget_control, info.table_main, map=1
   WIDGET_CONTROL, info.table_main, TLB_GET_SIZE=a
   info.windowSize=a
   WIDGET_CONTROL, info.table_main, SET_UVALUE=info
   WIDGET_CONTROL, info.wtable, SET_TABLE_SELECT=[-1, -1, -1, -1]

end

;--------------------------------------------------------- stretch color table
pro csol_viewer_stretchct, brightness, contrast,  getmouse = getmouse, info=info

; routine to change color stretch for given values of
; brightness and contrast. Borrowed from ATV.

; if GETMOUSE then assume mouse positoin passed; otherwise ignore
; inputs

   if (keyword_set(getmouse)) then begin
      info.brightness = brightness/float(info.draw_window_size[0])
      info.contrast = contrast/float(info.draw_window_size[1])
   endif

   x = info.brightness*(info.ncolors-1)
   y = info.contrast*(info.ncolors-1) > 2
   high = x+y & low = x-y
   diff = (high-low) > 1

   slope = float(info.ncolors-1)/diff ;Scale to range of 0 : nc-1
   intercept = -slope*low
   p = long(findgen(info.ncolors)*slope+intercept) ;subscripts to select
   tvlct, info.rsave[p], info.gsave[p], info.bsave[p], 8
   if (!d.n_colors GT 256) then begin
       if PTR_VALID(info.big_image) then begin
          csol_viewer_scale, info.big_id, info.big_image, imin=info.omin, imax=info.omax, $
              scale=info.scale_type, ncolors=info.ncolors, /noerase
       endif else begin
          csol_viewer_scale, info.big_id, info.orig, imin=info.omin, imax=info.omax, $
              scale=info.scale_type, ncolors=info.ncolors, /noerase
      endelse
      csol_viewer_scale,info.little_id,info.little_image,imin=info.omin,imax=info.omax,$
          scale=info.scale_type, ncolors=info.ncolors, /noerase
      csol_viewer_scale, info.zoom_id, info.zoom, imin=info.omin, imax=info.omax, $
          scale=info.scale_type, ncolors=info.ncolors, /noerase
   endif

end

; ================================================================== csol_viewer
;
;
; Main Routine
;
;
; ==================================================================
;
pro csol_viewer,image,h, file=file, PARENT=parent, RESTORE=restore, $
    CONTRAST=contrast, wtitle=wtitle, verbose=verbose
    if n_elements(verbose) ne 1 then verbose=1

   ; define a non-existing CSOL_DEF_FILE in case none were previously defined
   ; and we are running csol_viewer in "Virtual Machine" mode to prevent popup
   ; for request of cedar.def file all the time.
   defsysv, '!CSOL_DEF_FILE', exists=exists
   if exists eq 0 then begin
      if lmgr(/runtime) then begin
         ; define the stuff that would normaly be in the CSOL_startup in case we're running VM
		defsysv,'!debug',exist=exist	&	if exist eq 0 then defsysv,'!debug',0
		defsysv,'!priv',exist=exist		&	if exist eq 0 then defsysv,'!priv',0
		defsysv,'!quiet',exist=exist	&	if exist eq 0 then defsysv,'!quiet',0
         defsysv,'!TEXTOUT',1
         defsysv,'!TEXTUNIT',0
         DEVICE, DECOMPOSED=0
         if !version.os_family ne "Windows" then DEVICE, PSEUDO_COLOR=8
      endif
      ; if the system variable is not defined we have not read a cedar.def file
      ; temp=csol_getenv("CSOL_DEF_FILE",/file)
   endif

    ;print,'CSOL_DEF_FILE is '+!CSOL_DEF_FILE

   ;
   ; initialization
   ;
   if size(image,/TNAME) eq 'UNDEFINED' then begin
      ; create a dummy image
      orig = PTR_NEW(fltarr(2000, 1504), /NO_COPY)
   endif else if size(image,/TNAME) ne 'POINTER' then begin
      ; transform image to a pointer
      orig = PTR_NEW(image)
   endif else begin
      ; image is already a pointer
      orig = *image
      orig = PTR_NEW(orig,/no_copy)
   endelse

    ; put some code here in case csol_viewer is used as a standalone program
    defsysv,'!debug',exist=exist	&	if exist eq 0 then defsysv,'!debug',0
    defsysv,'!priv',exist=exist		&	if exist eq 0 then defsysv,'!priv',0
    defsysv,'!quiet',exist=exist	&	if exist eq 0 then defsysv,'!quiet',0
   if n_elements(wtitle) eq 0 then wtitle='csol_viewer'
   if n_elements(h) gt 0 then header=h else header=['END      ']
   filename = ''
   s = size(*orig) & ns = s[1] & nl = s[2]
   omin=min(*orig,max=omax)
   zoom_factor = 4
   zoom_width = 200
   xoffzoom = ns/2 - (zoom_width/2)/zoom_factor
   yoffzoom = nl/2 - (zoom_width/2)/zoom_factor
   little_down = 0
   little_window=-1
   little_nx=240.0
   little_ny=240.0
   if keyword_set(CONTRAST) then begin
      contrast = strupcase(contrast)
      if strpos(contrast,'LIN') ge 0 then begin
         scale_type='Linear'
      endif else if strpos(contrast,'LOG') ge 0 then begin
         scale_type='Log'
      endif else if strpos(contrast,'SQR') ge 0 then begin
         scale_type='Sqrt'
      endif else begin
         scale_type='Hist. Eq.'
      endelse
   endif else begin
      scale_type = 'Hist. Eq.'
   endelse

   csol_viewer_state = [0,0,-1,-1,0]
   xregion = intarr(20000)
   yregion = intarr(20000)
   nregion = 0
   ;widget_control,default_font  = $
   ;    '-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1'
   widget_control,default_font  ='6x13'

   ;set color table to greyscale and tek_color for first 12
   ncolors = !d.table_size - 13

   loadct, 0
   tek_color, 0, 12
   tvlct,rsave,gsave,bsave,/get

; create widget layout
;
   IF KEYWORD_SET(PARENT) THEN $
      top_base = widget_base(GROUP=parent,/col,uvalue='MAIN', $
                 MBAR=menuBar, /TLB_SIZE_EVENTS, TITLE=wtitle) $
   ELSE $
      top_base = widget_base(GROUP=0,/col,uvalue='MAIN',$
                 MBAR=menuBar, /TLB_SIZE_EVENTS, TITLE=wtitle)

   widget_control,top_base,/managed
;
; Button Bar  (Using cw_pdmenu is easier but looks weird for top level menu)
;

   menuFile         = WIDGET_BUTTON(menuBar, Value="File", /MENU)
   menuFileOpenHEX  = WIDGET_BUTTON(menuFile, VALUE="Open CSOL...", UVALUE="OPENCSOL")
   menuFileSave     = WIDGET_BUTTON(menuFile, VALUE="Save as...", /MENU, /SEPARATOR)
   menuFilePS       = WIDGET_BUTTON(menuFileSave, VALUE="PostScript", /MENU, /SEPARATOR)
   menuFilePS_BW    = WIDGET_BUTTON(menuFilePS, VALUE="B/W", UVALUE="PS_BW")
   menuFilePS_BWR   = WIDGET_BUTTON(menuFilePS, VALUE="B/W Reversed", UVALUE="PS_BWR")
   menuFilePS_Color = WIDGET_BUTTON(menuFilePS, VALUE="Color", UVALUE="PS_COLOR")
   menuFilePNG      = WIDGET_BUTTON(menuFileSave, VALUE="PNG",  UVALUE="MK_PNG")
   menuFileJPG      = WIDGET_BUTTON(menuFileSave, VALUE="JPG",  UVALUE="MK_JPG")
   menuFilePRINT    = WIDGET_BUTTON(menuFile, VALUE="Print",  UVALUE="PRINT")
   menuFileExit     = WIDGET_BUTTON(menuFile, VALUE="Exit",  UVALUE="EXIT")

   menuDisp         = WIDGET_BUTTON(menuBar, Value="Display", /MENU)
   menuDispColor    = WIDGET_BUTTON(menuDisp, VALUE="Colors...", UVALUE="DISP_COLOR")
   menuDispContrast = WIDGET_BUTTON(menuDisp, VALUE="Contrast", /MENU)
   menuDispLinear   = WIDGET_BUTTON(menuDispContrast, VALUE="Linear", UVALUE="DISP_LINEAR")
   menuDispLog      = WIDGET_BUTTON(menuDispContrast, VALUE="Log", UVALUE="DISP_LOG")
   menuDispSqrt     = WIDGET_BUTTON(menuDispContrast, VALUE="Square Root", UVALUE="DISP_SQRT")
   menuDispHistEq   = WIDGET_BUTTON(menuDispContrast, VALUE="Hist. Eq.", UVALUE="DISP_HISTEQ")
   menuDispZoom     = WIDGET_BUTTON(menuDisp, VALUE="Zoom", /MENU)
   menuDispZoom1    = WIDGET_BUTTON(menuDispZoom, VALUE=" 1", UVALUE="DISP_ZOOM")
   menuDispZoom2    = WIDGET_BUTTON(menuDispZoom, VALUE=" 2", UVALUE="DISP_ZOOM")
   menuDispZoom3    = WIDGET_BUTTON(menuDispZoom, VALUE=" 3", UVALUE="DISP_ZOOM")
   menuDispZoom4    = WIDGET_BUTTON(menuDispZoom, VALUE=" 4", UVALUE="DISP_ZOOM")
   menuDispZoom5    = WIDGET_BUTTON(menuDispZoom, VALUE=" 5", UVALUE="DISP_ZOOM")
   menuDispZoom6    = WIDGET_BUTTON(menuDispZoom, VALUE=" 6", UVALUE="DISP_ZOOM")
   menuDispZoom7    = WIDGET_BUTTON(menuDispZoom, VALUE=" 7", UVALUE="DISP_ZOOM")
   menuDispZoom8    = WIDGET_BUTTON(menuDispZoom, VALUE=" 8", UVALUE="DISP_ZOOM")
   menuDispZoom9    = WIDGET_BUTTON(menuDispZoom, VALUE=" 9", UVALUE="DISP_ZOOM")
   menuDispZoom10   = WIDGET_BUTTON(menuDispZoom, VALUE=" 10", UVALUE="DISP_ZOOM")
   menuDispZoom16   = WIDGET_BUTTON(menuDispZoom, VALUE=" 16", UVALUE="DISP_ZOOM")
   menuDispZoom32   = WIDGET_BUTTON(menuDispZoom, VALUE=" 32", UVALUE="DISP_ZOOM")
   menuDispMinMax   = WIDGET_BUTTON(menuDisp, VALUE="Min / Max", /MENU)
   menuDispFreeze   = WIDGET_BUTTON(menuDispMinMax, VALUE="Freeze", UVALUE="FREEZE",/dynamic)
   menuDispReset    = WIDGET_BUTTON(menuDispMinMax, VALUE="Reset", UVALUE="RESET",/dynamic)
   menuDispOverlay  = WIDGET_BUTTON(menuDisp, VALUE="Overlay", /MENU)
   menuDispX1D      = WIDGET_BUTTON(menuDispOverlay, VALUE="Aperture Boxes", /MENU)
   menuDispX1DREF   = WIDGET_BUTTON(menuDispX1D, VALUE="from XTRACTTAB", UVALUE="DISP_X1DREF")
   menuDispX1DDATA  = WIDGET_BUTTON(menuDispX1D, VALUE="from *_x1d file", UVALUE="DISP_X1DDATA")
   menuDispX1DUSER  = WIDGET_BUTTON(menuDispX1D, VALUE="select a reffile", UVALUE="DISP_X1DUSER")
   menuDispRegion   = WIDGET_BUTTON(menuDisp, VALUE="Set Region...", UVALUE="DISP_REGION")
   menuDispOtherSeg = WIDGET_BUTTON(menuDisp, VALUE="Other Segment", UVALUE="DISP_OTHERSEG")
   menuDispUserCoord= WIDGET_BUTTON(menuDisp, VALUE="USER coord", UVALUE="DISP_USERCOORD")
   menuDispDetCoord = WIDGET_BUTTON(menuDisp, VALUE="DETECT coord", UVALUE="DISP_DETCOORD")
   menuDispReload   = WIDGET_BUTTON(menuDisp, VALUE="Reload File", UVALUE="DISP_RELOAD")
   menuDispRefresh  = WIDGET_BUTTON(menuDisp, VALUE="Refresh", UVALUE="DISP_REFRESH")

   menuData         = WIDGET_BUTTON(menuBar,  Value="Data", /MENU)
   menuDataHeader   = WIDGET_BUTTON(menuData, VALUE="Header", UVALUE="DATA_HEADER")
   menuDataHistog   = WIDGET_BUTTON(menuData, VALUE="Histogram", UVALUE="DATA_HIST")
   menuDataEvents   = WIDGET_BUTTON(menuData, VALUE="Events", /MENU)
   menuDataEvAll    = WIDGET_BUTTON(menuDataEvents, VALUE="Whole Image", UVALUE='DATA_EVENTS')
   menuDataEvWin    = WIDGET_BUTTON(menuDataEvents, VALUE="Big Window", UVALUE='DATA_EVENTS')
   menuDataEvZoom   = WIDGET_BUTTON(menuDataEvents, VALUE="Zoomed Window", UVALUE='DATA_EVENTS')

   menuTools        = WIDGET_BUTTON(menuBar,  Value="Tools", /MENU)
   menuToolsStats   = WIDGET_BUTTON(menuTools, VALUE="Statistics", /MENU)
   menuStatsBox     = WIDGET_BUTTON(menuToolsStats, VALUE="Box", UVALUE="STATS")
   menuStatsROI     = WIDGET_BUTTON(menuToolsStats, VALUE="Draw Region", UVALUE="STATS")
   menuStatsIMG     = WIDGET_BUTTON(menuToolsStats, VALUE="Whole Image", UVALUE="STATS")
   menuStatsZOOM    = WIDGET_BUTTON(menuToolsStats, VALUE="Zoomed Image", UVALUE="STATS")

   menuToolsPlot    = WIDGET_BUTTON(menuTools, VALUE="Plot", /MENU)
   menuPlotRow      = WIDGET_BUTTON(menuToolsPlot, VALUE="Row", UVALUE="PLOT")
   menuPlotCol      = WIDGET_BUTTON(menuToolsPlot, VALUE="Column", UVALUE="PLOT")
   menuPlotRowSum   = WIDGET_BUTTON(menuToolsPlot, VALUE="Row Sum", UVALUE="PLOT")
   menuPlotColSum   = WIDGET_BUTTON(menuToolsPlot, VALUE="Column Sum", UVALUE="PLOT")
   menuPlotXSec     = WIDGET_BUTTON(menuToolsPlot, VALUE="X-Section", /MENU)
   menuPlotXSec1    = WIDGET_BUTTON(menuPlotXSec, VALUE=" 1 pixel wide", UVALUE="PLOT")
   menuPlotXSec3    = WIDGET_BUTTON(menuPlotXSec, VALUE=" 3", UVALUE="PLOT")
   menuPlotXSec5    = WIDGET_BUTTON(menuPlotXSec, VALUE=" 5", UVALUE="PLOT")
   menuPlotXSec7    = WIDGET_BUTTON(menuPlotXSec, VALUE=" 7", UVALUE="PLOT")
   menuPlotXSec9    = WIDGET_BUTTON(menuPlotXSec, VALUE=" 9", UVALUE="PLOT")
   menuPlotXSec11   = WIDGET_BUTTON(menuPlotXSec, VALUE=" 11", UVALUE="PLOT")
   menuPlotXSec15   = WIDGET_BUTTON(menuPlotXSec, VALUE=" 15", UVALUE="PLOT")
   menuPlotXSec25   = WIDGET_BUTTON(menuPlotXSec, VALUE=" 25", UVALUE="PLOT")
   menuPlotXSec35   = WIDGET_BUTTON(menuPlotXSec, VALUE=" 35", UVALUE="PLOT")
   menuPlotXSec55   = WIDGET_BUTTON(menuPlotXSec, VALUE=" 55", UVALUE="PLOT")
   menuPlotXSec75   = WIDGET_BUTTON(menuPlotXSec, VALUE=" 75", UVALUE="PLOT")
   menuPlotSurface  = WIDGET_BUTTON(menuToolsPlot, VALUE="Surface", UVALUE="PLOT_SURFACE")
   menuPlotContour  = WIDGET_BUTTON(menuToolsPlot, VALUE="Contour", UVALUE="PLOT_CONTOUR")
   menuPlotEncEnergy= WIDGET_BUTTON(menuToolsPlot, VALUE="Encircled Energy", UVALUE="PLOT_ENCENERGY")
   ;menuPlotPHASec   = WIDGET_BUTTON(menuToolsPlot, VALUE="FUV Pulse Height", /MENU)
   ;menuPlotPHAIMG   = WIDGET_BUTTON(menuPlotPHASec, VALUE="Whole Image", UVALUE="PLOT_PHA")
   ;menuPlotPHABOX   = WIDGET_BUTTON(menuPlotPHASec, VALUE="Box", UVALUE="PLOT_PHA")

   menuToolsGauss   = WIDGET_BUTTON(menuTools, VALUE="Gaussfit", /MENU)
   menuToolsGaussEM = WIDGET_BUTTON(menuToolsGauss, VALUE="Emission", UVALUE="GAUSS_EM")
   menuToolsGaussAB = WIDGET_BUTTON(menuToolsGauss, VALUE="Absorption", UVALUE="GAUSS_AB")

   menuToolsFind    = WIDGET_BUTTON(menuTools, VALUE="Find Lines", /MENU)
   menuToolsFindMan = WIDGET_BUTTON(menuToolsFind, VALUE="Interactive", /MENU)
   menuFindRowMan   = WIDGET_BUTTON(menuToolsFindMan, VALUE="Row Sum", UVALUE="FINDLINES")
   menuFindColMan   = WIDGET_BUTTON(menuToolsFindMan, VALUE="Column Sum", UVALUE="FINDLINES")
   menuToolsFindAuto= WIDGET_BUTTON(menuToolsFind, VALUE="Automatic", /MENU)
   menuToolsFAR     = WIDGET_BUTTON(menuToolsFindAuto, VALUE="Row Sum", UVALUE="FINDLINES_AUTO")
   menuToolsFAC     = WIDGET_BUTTON(menuToolsFindAuto, VALUE="Column Sum", UVALUE="FINDLINES_AUTO")
   menuToolsFARC    = WIDGET_BUTTON(menuToolsFindAuto, VALUE="Rows+Columns", UVALUE="FINDLINES_AUTO")

   lbl_font = '6x13'
   basewindows = widget_base(top_base,/row)
   basebig = widget_base(basewindows,/col)
   basex = widget_base(basebig,col=3)
   base1x = widget_base(basex, /col,/frame)
   HEXmessage = widget_label(base1x,value = '    ',/align_left,font=lbl_font,xsize=300)

   base2x = widget_base(base1x,/row)
   x_field = cw_field(base2x,/row,uvalue='X_FIELD',value=-1, $
                title='X: ',xsize=6,/return_events,/long,/noedit,fieldfont=lbl_font)
   y_field = cw_field(base2x,/row,uvalue='Y_FIELD',value=-1, $
                title='Y: ',xsize=6,/return_events,/long,/noedit,fieldfont=lbl_font)
   val_field = cw_field(base2x,/row,uvalue='VAL_FIELD',value=-1.0, $
                title='Val: ',xsize=10,/return_events,/float,/noedit,fieldfont=lbl_font)

   base3x = widget_base(base1x, /row)
   big_pos_lbl  = widget_label(base3x,value='Main Area:   ')
   big_position = widget_label(base3x,value=' XXXXX:XXXX XXXXX:XXXX', /frame,$
                        /align_left,font=lbl_font,xsize=160,ysize=25)
   base4x = widget_base(base1x, /row)
   zoom_pos_lbl  = widget_label(base4x,value='Zoom Area: ')
   zoom_position = widget_label(base4x,value='XXXXX:XXXX XXXXX:XXXX', /frame,$
                        /align_left,font=lbl_font,xsize=160,ysize=25)

   base2 = widget_base(basex,col=4)
   base2a = widget_base(base2,row=1,/frame)

   min_field = cw_field(base2a,/row,uvalue='MIN_FIELD',value=omin, $
                title='Min: ',xsize=13,/return_events,/float,fieldfont=lbl_font)
   max_field = cw_field(base2a,/row,uvalue='MAX_FIELD',value=omax, $
                title='Max: ',xsize=13,/return_events,/float,fieldfont=lbl_font)

   base5x = widget_base(basex, /col)
   zoom_window = widget_draw(base5x,uvalue='ZOOM_WINDOW',retain=2,  xsize=zoom_width,$
      ysize=zoom_width,/motion,/button_events,/keyboard_events)

   base_nuv = widget_base(basex, /col)
   little_window = widget_draw(base_nuv,uvalue='LITTLE_WINDOW',retain=2, $
      xsize=little_nx,ysize=little_ny,/button_events,/motion)

   ; determine the size of screen to make widget fit on screen
   ; specially for 1024x768 laptops
   xsize=750
   x_scroll_size=700
   device,get_screen_size=screen_size
   if screen_size[1] ge 1024 then begin
      ysize=500
      y_scroll_size=500
   endif else begin
      ysize = screen_size[1] / 2
      y_scroll_size = ysize + 30
   endelse

   big_window = widget_draw(basebig,uvalue='BIG_WINDOW', retain=2, $
         xsize=xsize,ysize=ysize,x_scroll_size=x_scroll_size, $
         y_scroll_size=y_scroll_size,/button_events,/motion, $
         /viewport_events)

   base_fuv = widget_base(top_base)
;
; create widget
;
   widget_control,top_base,/realize
   widget_control, menuDispOtherSeg,  sensitive=0
   widget_control, menuDispUserCoord, sensitive=0
   widget_control, menuDispDetCoord,  sensitive=0
   widget_control, menuDispReload,  sensitive=0
   widget_control, menuDispOverlay,  sensitive=1

   widget_control,big_window,get_value=big_id
   widget_control,zoom_window,get_value=zoom_id
   widget_control,little_window,get_value=little_id
   WIDGET_CONTROL, top_base, TLB_GET_SIZE=top_base_windowSize
   ;
   ; the variable data will replace the old CSOL_timetag_data COMMON BLOCK
   data = { $
      xttag:PTR_NEW(), $
      yttag:PTR_NEW(), $
      timetag:PTR_NEW(), $
      xbin:1, $
      ybin:1, $
      gtime1:PTR_NEW(), $
      gtime2:PTR_NEW(), $
      pha:PTR_NEW(), $
      phamask:PTR_NEW() $
      }
   ;
   ; the variable info will replace the old csol_viewer_common COMMON BLOCK
   info = { $
      top_base: top_base,$
      base_fuv: base_fuv,$
      base_nuv: base_nuv,$
      menuDispFreeze: menuDispFreeze,$
      menuDataEvents: menuDataEvents,$
      menuDispReload: menuDispReload,$
      menuDispOtherSeg: menuDispOtherSeg,$
      menuDispUserCoord: menuDispUserCoord,$
      menuDispDetCoord: menuDispDetCoord,$
      HEXmessage: HEXmessage,$
      disp_orient:0, $
      disp_xtractab:0, $
      disp_x1dfile:0, $
      disp_x1duser:0, $
      HEX_imgseq: 0,$
      wtitle:wtitle, $
      orig: orig,$
      ns: ns,$
      nl: nl,$
      xoff: 0, $
      yoff: 0, $
      omin:omin ,$
      omax: omax,$
      filename: filename,$
      HEXseg: '',$
      big_window: big_window,$
      big_id: big_id,$
      csol_viewer_state: csol_viewer_state,$
      xregion: xregion,$
      yregion: yregion,$
      nregion: nregion,$
      big_position: big_position,$
      zoom_id: zoom_id,$
      zoom_width: zoom_width,$
      zoom_factor: zoom_factor,$
      xoffzoom: xoffzoom,$
      yoffzoom: yoffzoom,$
      zoom_position: zoom_position,$
      little_id: little_id,$
      little_down: little_down,$
      ncolors: ncolors, $
      rsave: rsave,$
      gsave: gsave,$
      bsave: bsave,$
      brightness: 0.5, $
      contrast: 0.5, $
      scale_type: scale_type,$
      min_field: min_field,$
      max_field: max_field,$
      x_field: x_field,$
      y_field: y_field,$
      val_field: val_field,$
      little_nx: little_nx,$
      little_ny: little_ny,$
      little_window: little_window,$
      top_base_windowsize: top_base_windowsize,$
      draw_window_size: [xsize,ysize],$
      exptime: 0.0, $
      header: PTR_NEW(header,/no_copy),$
      little_image: PTR_NEW(),$
      zoom: PTR_NEW(),$
      otherseg: PTR_NEW(),$
      big_image: PTR_NEW(),$
      data: PTR_NEW(data,/no_copy), $
      timerange: [0.0d,0.0d], $
      stars: PTR_NEW(), $
      CSOLcoord:-1}

   WIDGET_CONTROL, top_base, TLB_GET_SIZE=windowSize
   info.top_base_windowSize=windowSize
   WIDGET_CONTROL, top_base, SET_UVALUE=info
   xmanager,'csol_viewer',top_base,/no_block
   ; if filename was passed, open file and display data
   if n_elements(file) ne 0 then begin
      filename = file
      info.filename=file
      ; check if fits file
      msg=''
      fxread,filename,data,header,/nodata,exten=0,errmsg=msg
      if strlen(msg) eq 0 then begin
         ; we have a FITS file
         ; disable the event handler
         WIDGET_CONTROL, info.top_base, EVENT_FUNC=''
         status = HEX_openraw(info)
         ; re-enable the event handler
         WIDGET_CONTROL, info.top_base, EVENT_PRO='cos_viewer_EVENT'
         if status ne '' then r=dialog_message(dialog_parent=info.top_base, status, /error)
         widget_control,/hourglass
         WIDGET_CONTROL, info.top_base, SET_UVALUE=info
      ENDIF ELSE BEGIN
         ; we do not have a fits file, assume COS_RAW file
         status = HEX_openraw(info)
         if status ne '' then r=dialog_message(dialog_parent=top_base,status,/error)
      ENDELSE
   endif else if PTR_VALID(orig) then begin
      ; displaying a new image
      s=size(*orig,/type)
      if s ge 7 and s le 11 then begin
          r=dialog_message(dialog_parent=top_base, 'Wrong datatype passed to the program',/error)
      endif else begin
         s=size(*orig)
		little_nx=240.0
		little_ny=240.0
		; destroy previous instance of window before creating a new one
		if little_window ne -1 then widget_control, little_window, /DESTROY
		little_window = widget_draw(base_nuv,uvalue='LITTLE_WINDOW',retain=2, $
		   xsize=little_nx,ysize=little_ny,/button_events,/motion)
		widget_control,little_window,get_value=little_id
         info.little_nx=little_nx
         info.little_ny=little_ny
         info.little_window=little_window
         info.little_id=little_id
         info.exptime=0.0
         csol_viewer_display,orig, info=info
         WIDGET_CONTROL, info.top_base, SET_UVALUE=info
      endelse
   endif

   return
end
