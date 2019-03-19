; $Id: svp_lineplot.pro,v 1.1 2019/03/19 19:40:15 spenton Exp spenton $
;+
;				lineplot
; Widget line plot tool
;
; CALLING SEQUENCE:
;   svp_lineplot,x,y,xsize=1000,ysize=800
;      or
;   svp_lineplot,y
;
; INPUTS
;   x - input x vector
;   y - input y vector
; OPTIONAL INPUT PARAMETERS
;   ptitle - main plot title
;   title - title of the plot or overplot vectors
;   xtitle - title for the xaxis
;   ytitle - title for the yaxis
;   xrange - initial xrange for the plot
;   yrange - initial yrange for the plot
;   group - group id of calling widget
;
; INTERACTIVE INPUTS:
;   In addition to the widget parameters controlled by buttons and
;   text field inputs:
;   1) Use the left mouse button to measure a feature location in the
;      plot.
;   2) Push the center mouse button and hold it down to define
;      two corners of a region to be zoomed.
;
; The large text box containing may be edited before writing to a file
;
; HISTORY:
;   version 1  D. Lindler  Aug 1999
;   version 1.1  SBeland June 2003:  Added header parameter for writing FITS
;-
;----------------------------------------------------------------------------

;====================================================== LINEPLOT_EVENT
;
; Main event driver for svp_lineplot
;
pro svp_lineplot_event,event
   common svp_lineplot_common,info,xarray,yarray,lineplot_r,lineplot_g,lineplot_b,$
               svp_lineplot_rsave,lineplot_gsave,lineplot_bsave,cwd

  ;tvlct,lineplot_r,lineplot_g,lineplot_b

  if TAG_NAMES(event, /STRUCTURE_NAME) eq 'WIDGET_BASE' AND $
     event.id EQ info.main then begin ; The window has been sized

     ;; Get the new size of the window
     WIDGET_CONTROL, info.main, TLB_GET_SIZE=windowSize

     ;; Determine the change in the window size
     deltaX = windowSize[0] - info.windowSize[0]
     deltaY = windowSize[1] - info.windowSize[1]

     IF not (deltaX EQ 0.0 AND deltaY EQ 0.0) THEN begin
        ;; widget_control, info.main, map=0
        ;; Get the pixel size of the draw widget
        plotGeometry = WIDGET_INFO(info.plot1, /GEOMETRY)

        ;; Determine the new size based on the amount the window grew
        newXSize = (plotGeometry.scr_xsize + deltaX) > info.wMinXsize
        newYSize = (plotGeometry.scr_ysize + deltaY) > info.wMinYsize

        ;; delete the pixmap and create a new one
        wdelete, info.pixid
        window, xs=newXSize, ys=newYSize, /pixmap, /free
        info.pixid = !d.window
        info.wXsize = newXsize
        info.wYsize = newYsize

        widget_control, info.plot1, xsize=newXsize, ysize=newYsize

        ;; widget_control, info.main, map=1

        ;; redraw the plot
        svp_lineplot_plot,info,xarray,yarray

        WIDGET_CONTROL, info.main, TLB_GET_SIZE=windowSize
        info.windowSize = windowSize
     endif
     return
  ENDIF


   widget_control,event.id,get_uvalue=uvalue
   case uvalue of
   'EXIT': begin
      xarray = 0
      yarray = 0
      wdelete,info.pixid
      tvlct,lineplot_rsave,lineplot_gsave,lineplot_bsave
      widget_control,event.top,/destroy
      end
   'Postscript File': begin
     file=info.outfile+'.ps'
     file = dialog_pickfile(path=cwd,get_path=npath,file=file,filter='*.ps',/write)
     if file eq '' then return   ;no file selected
     if npath ne cwd then cwd=npath
     thisDevice = !d.name
     set_plot,'ps'
     !p.background=255
     !p.color=0
     ;!p.font = 0
     !p.font = 1   ; this was changed to address the problem with postscript files in IDL8.3
     device,/port,xoff=1,yoff=1,xsize=7,ysize=9,bits=8, /color,/inches,file=file
     ;set_viewport,0.08,0.98,0.4,0.9
     mypos=!p.position
     !p.position=[0.08,0.4,0.98,0.9]
     svp_lineplot_plot,info,xarray,yarray,/ps
     svp_lineplot_annotate,info,/ps
     device,/close
     set_plot, thisDevice
     !p.background=255
     !p.color=0
     !p.font = -1
     !p.position=mypos
     end
  'PNG File': begin
     file=info.outfile+'.png'
     file = dialog_pickfile(path=cwd,get_path=npath,file=file,filter='*.png',/write)
     if file eq '' then return   ;no file selected
     if npath ne cwd then cwd=npath
     thisDevice = !d.name
     set_plot,'Z',/copy
     device, set_resolution=[info.wXsize, info.wYsize]
     !p.background=255
     !p.color=0
     tvlct,r,g,b,/get
     !p.font = 0
     ;set_viewport,0.08,0.98,0.4,0.9
     mypos=!p.position
     ;!p.position=[0.08,0.4,0.98,0.9]
     !p.position=[0.1,0.4,0.9,0.9]
     svp_lineplot_plot,info,xarray,yarray,/ps
     svp_lineplot_annotate,info,/ps
     img=tvrd()
     set_plot, thisDevice
     !p.background=255
     !p.color=0
     !p.font = -1
     write_png,file,img,r,g,b
     image=0b
     !p.position=mypos
     end
  'ASCII Table': begin
     svp_lineplot_select,info,'Select Which Plot to Write', isel,group=event.top, /all
     file=info.outfile+'.txt'
     if n_elements(info.title) gt isel then begin
        if strlen(info.title[isel]) gt 0 then file=info.title[isel]
     endif else if n_elements(info.title) eq isel then begin
         ; the user selected o save data from all plots
         file=info.outfile+'_ALL.txt'
     endif
     file = dialog_pickfile(path=cwd,get_path=npath,file=file,filter='*.txt',/write)
     if file eq '' then return   ;no file selected
     if npath ne cwd then cwd=npath
     openw,unit,file,/get_lun
     printf,unit,';'+info.ptitle
     printf,unit,';'+info.ytitle
     printf,unit,';'+info.xtitle
     if isel eq info.n then begin
         ; the user selected o save data from all plots
         i1=0
         for k=0,n_elements(info.title)-1 do begin
             printf,unit,';'+info.title(k)
             i2 = i1 + info.ns(k)-1
             for i=i1,i2 do printf,unit,k+1,xarray(i),yarray(i),format='(I,"  ",F,"  ",F)'
             i1 = i2+1L
         endfor
     endif else begin
         printf,unit,';'+info.title(isel)
         if isel ne 0 then i1 = round(total(info.ns(0:isel-1))) else i1 = 0
         i2 = i1 + info.ns(isel)-1
         for i=i1,i2 do printf,unit,xarray(i),yarray(i)
     endelse
     free_lun,unit
     end
  'FITS Table': begin
     svp_lineplot_select,info,'Select Which Plot to Write', $
        isel,group=event.top
     file=info.outfile+'_line.fits'
     file = dialog_pickfile(path=cwd,get_path=npath,file=file,filter='*.fits',/write)
     if file eq '' then return   ;no file selected
     if npath ne cwd then cwd=npath
     mkhdr,h0,1,0   ; create a dummy primary header
     if PTR_VALID(info.header[isel]) then begin
        ; a valid header exists
        ; find the primary header
        pos=where(strpos((*info.header[isel]),'END') EQ 0, count)
        if count ge 2 then begin
           h0 = (*info.header[isel])[0:pos[0]]
           h1 = (*info.header[isel])[pos[0]+1:*]
        endif else if n_elements(*info.header[isel]) gt 1 then begin
           h1 = (*info.header[isel])
        endif else begin
           h1=['END']
        endelse
     endif
     sxaddpar,h1,'PTITLE',info.ptitle
     sxaddpar,h1,'XTITLE',info.xtitle
     sxaddpar,h1,'YTITLE',info.ytitle
     sxaddpar,h1,'TITLE',info.title(isel)
     sxdelpar,h1,'TUNIT1'
     sxdelpar,h1,'TFORM1'
     sxdelpar,h1,'TTYPE1'
     sxdelpar,h1,'TSCAL1'
     sxdelpar,h1,'TZERO1'
     sxdelpar,h1,'TCTYP1'
     sxdelpar,h1,'TCRVL1'
     sxdelpar,h1,'TCDLT1'
     sxdelpar,h1,'TCRPX1'
     sxdelpar,h1,'TALEN1'
     sxdelpar,h1,'TUNIT2'
     sxdelpar,h1,'TFORM2'
     sxdelpar,h1,'TTYPE2'
     sxdelpar,h1,'TSCAL2'
     sxdelpar,h1,'TZERO2'
     sxdelpar,h1,'TCTYP2'
     sxdelpar,h1,'TCRVL2'
     sxdelpar,h1,'TCDLT2'
     sxdelpar,h1,'TCRPX2'
     sxdelpar,h1,'TALEN2'
     sxdelpar,h1,'TUNIT3'
     sxdelpar,h1,'TFORM3'
     sxdelpar,h1,'TTYPE3'
     sxdelpar,h1,'TSCAL3'
     sxdelpar,h1,'TZERO3'
     sxdelpar,h1,'TCTYP3'
     sxdelpar,h1,'TCRVL3'
     sxdelpar,h1,'TCDLT3'
     sxdelpar,h1,'TCRPX3'
     sxdelpar,h1,'TALEN3'
     sxdelpar,h1,'TUNIT4'
     sxdelpar,h1,'TFORM4'
     sxdelpar,h1,'TTYPE4'
     sxdelpar,h1,'TSCAL4'
     sxdelpar,h1,'TZERO4'
     sxdelpar,h1,'TCTYP4'
     sxdelpar,h1,'TCRVL4'
     sxdelpar,h1,'TCDLT4'
     sxdelpar,h1,'TCRPX4'
     sxdelpar,h1,'TALEN4'
     sxaddpar,h1,'EXTNAME','SCI'
     if isel ne 0 then i1=round(total(info.ns(0:isel-1))) else i1=0
     i2 = i1 + info.ns(isel)-1
     a = {x:xarray(i1:i2),y:yarray(i1:i2)}
     mwrfits,nothing,file,h0,/create,/NO_COMMENT
     mwrfits,a,file,h1
     end

  'Log File': begin
     file=info.outfile+'.log'
     file = dialog_pickfile(path=cwd,get_path=npath,file=file,filter='*.log',/write)
     if file eq '' then return   ;no file selected
     if npath ne cwd then cwd=npath
     widget_control,info.log,get_value=v
     openw,unit,file,/get_lun
     for i=0,n_elements(v)-1 do printf,unit,v(i)
     free_lun,unit
     end
   'MAIN_LINEPLOT':
   'ZOOM': begin
      wset,info.plot2_id
      erase
      xyouts,10,150,/dev,'Place Cursor on first corner',$
            charsize=2.2,charthick=2
      xyouts,10,110,/dev,'and push left mouse button', $
            charsize=2.2,charthick=2
      info.state = 'ZOOM1'
      end
   'UNZOOM_ALL': begin
      yoff = yarray*0
      i1 = 0L
      widget_control,info.yoff_base,get_value=yoffval
      info.yoffval = yoffval
      good = bytarr(n_elements(yarray))
      for i=0,info.n-1 do begin
         i2 = i1+info.ns[i]-1
         good[i1:i2] = (yarray[i1:i2] ge info.min_val[i]) and $
                  (yarray[i1:i2] le info.max_val[i])
         yoff[i1:i2] = i*double(yoffval)
         i1 = i2+1
      end
      xmin = MIN(xarray, MAX=xmax)
      widget_control,info.xmin_base,set_value = string(xmin,format='(G)')
      widget_control,info.xmax_base,set_value = string(xmax,format='(G)')
      good = where(good)
      ygood = yarray[good] + yoff[good]
      ymin = MIN(ygood, MAX=ymax)
      widget_control,info.ymin_base,set_value = string(ymin,format='(G)')
      widget_control,info.ymax_base,set_value = string(ymax,format='(G)')
      info.xmin = xmin
      info.xmax = xmax
      info.ymin = ymin
      info.ymax = ymax
      svp_lineplot_plot,info,xarray,yarray
      end
   'UNZOOM': begin
      yoff = yarray*0
      i1 = 0
      widget_control,info.yoff_base,get_value=yoffval
      info.yoffval = yoffval
      good = bytarr(n_elements(yarray))
      for i=0,info.n-1 do begin
         i2 = i1+info.ns(i)-1
         good[i1:i2] = (yarray[i1:i2] gt info.min_val[i]) and $
                  (yarray[i1:i2] lt info.max_val[i])
         yoff[i1:i2] = i*double(yoffval)
         i1 = i2+1
      end
      good = where(good)
      ygood = yarray[good] + yoff[good]
      widget_control,info.xmin_base,get_value = xmin
      widget_control,info.xmax_base,get_value = xmax
      widget_control,info.ymin_base,get_value = ymin
      widget_control,info.ymax_base,get_value = ymax
      xmin=double(xmin) & xmax=double(xmax)
      ymin=double(ymin) & ymax=double(ymax)
      xrange = (xmax-xmin)
      yrange = (ymax-ymin)
      xmin = (xmin - xrange*0.25) > min(xarray) < xmin
      xmax = (xmax + xrange*0.25) < max(xarray) > xmax
      ymin = (ymin - yrange*0.25) > min(ygood) < ymin
      ymax = (ymax + yrange*0.25) < max(ygood) > ymax
      widget_control,info.xmin_base,set_value = string(xmin,format='(G)')
      widget_control,info.xmax_base,set_value = string(xmax,format='(G)')
      widget_control,info.ymin_base,set_value = string(ymin,format='(G)')
      widget_control,info.ymax_base,set_value = string(ymax,format='(G)')
      info.xmin = xmin
      info.xmax = xmax
      info.ymin = ymin
      info.ymax = ymax
      svp_lineplot_plot,info,xarray,yarray
      end
   'UNZOOM_X': begin
      yoff = yarray*0
      i1 = 0
      widget_control,info.yoff_base,get_value=yoffval
      info.yoffval = yoffval
      good = bytarr(n_elements(yarray))
      for i=0,info.n-1 do begin
         i2 = i1+info.ns(i)-1
         good[i1:i2] = (yarray[i1:i2] gt info.min_val[i]) and $
                  (yarray[i1:i2] lt info.max_val[i])
         yoff[i1:i2] = i*double(yoffval)
         i1 = i2+1
      endfor
      xmin = MIN(xarray, MAX=xmax)
      widget_control,info.xmin_base,set_value = string(xmin,format='(G)')
      widget_control,info.xmax_base,set_value = string(xmax,format='(G)')
      info.xmin = xmin
      info.xmax = xmax
      svp_lineplot_plot,info,xarray,yarray
      end
   'UNZOOM_Y': begin
      yoff = yarray*0
      i1 = 0
      widget_control,info.yoff_base,get_value=yoffval
      info.yoffval = yoffval
      good = bytarr(n_elements(yarray))
      for i=0,info.n-1 do begin
         i2 = i1+info.ns(i)-1
         good[i1:i2] = (yarray[i1:i2] gt info.min_val[i]) and $
                  (yarray[i1:i2] lt info.max_val[i])
         yoff[i1:i2] = i*double(yoffval)
         i1 = i2+1
      endfor
      good = where(good)
      ygood = yarray(good) + yoff(good)
      ymin = MIN(ygood, MAX=ymax)
      widget_control,info.ymin_base,set_value = string(ymin,format='(G)')
      widget_control,info.ymax_base,set_value = string(ymax,format='(G)')
      info.ymin = ymin
      info.ymax = ymax
      svp_lineplot_plot,info,xarray,yarray
      end

   'RANGE': svp_lineplot_plot,info,xarray,yarray
   'LINESTYLES': svp_lineplot_plotpar,group=event.top
   'PAN_R1': begin
      ; move 1/10 of screen
      widget_control,info.xmin_base,get_value = xmin
      widget_control,info.xmax_base,get_value = xmax
      xmin=double(xmin) & xmax=double(xmax)
      xrange=xmax-xmin
      m1=min(xarray,max=m2)
      xmin = (xmin - xrange*0.1d) > m1 < m2
      xmax = (xmin + xrange) > m1 < m2
      widget_control,info.xmin_base,set_value = string(xmin,format='(G)')
      widget_control,info.xmax_base,set_value = string(xmax,format='(G)')
      info.xmin = xmin
      info.xmax = xmax
      svp_lineplot_plot,info,xarray,yarray
      end
   'PAN_R2': begin
      ; move a full screen
      widget_control,info.xmin_base,get_value = xmin
      widget_control,info.xmax_base,get_value = xmax
      xmin=double(xmin) & xmax=double(xmax)
      xrange=xmax-xmin
      m1=min(xarray,max=m2)
      xmin = (xmin - xrange) > m1 < m2
      xmax = (xmin + xrange) > m1 < m2
      widget_control,info.xmin_base,set_value = string(xmin,format='(G)')
      widget_control,info.xmax_base,set_value = string(xmax,format='(G)')
      xmin=double(xmin)
      xmax=double(xmax)
      info.xmin = xmin
      info.xmax = xmax
      svp_lineplot_plot,info,xarray,yarray
      end
   'PAN_L1': begin
      widget_control,info.xmin_base,get_value = xmin
      widget_control,info.xmax_base,get_value = xmax
      xmin=double(xmin) & xmax=double(xmax)
      xmin=double(xmin)
      xmax=double(xmax)
      xrange=xmax-xmin
      m1=min(xarray,max=m2)
      xmax = (xmax + xrange*0.1d) < m2 > m1
      xmin = (xmax - xrange) < m2 > m1
      widget_control,info.xmin_base,set_value = string(xmin,format='(G)')
      widget_control,info.xmax_base,set_value = string(xmax,format='(G)')
      info.xmin = xmin
      info.xmax = xmax
      svp_lineplot_plot,info,xarray,yarray
      end
   'PAN_L2': begin
      ; move a full screen
      widget_control,info.xmin_base,get_value = xmin
      widget_control,info.xmax_base,get_value = xmax
      xmin=double(xmin) & xmax=double(xmax)
      xrange=xmax-xmin
      m1=min(xarray,max=m2)
      xmax = (xmax + xrange) < m2 > m1
      xmin = (xmax - xrange) < m2 > m1
      widget_control,info.xmin_base,set_value = string(xmin,format='(G)')
      widget_control,info.xmax_base,set_value = string(xmax,format='(G)')
      info.xmin = xmin
      info.xmax = xmax
      svp_lineplot_plot,info,xarray,yarray
      end
   'PAN_U1': begin
      ; move up 1/4 of screen
      widget_control,info.ymin_base,get_value = ymin
      widget_control,info.ymax_base,get_value = ymax
      ymin=double(ymin) & ymax=double(ymax)
      yrange=ymax-ymin
      m1=min(yarray,max=m2)
      ymin = (ymin + yrange*0.1d) > m1 < m2
      ymax = (ymin + yrange) > m1 < m2
      widget_control,info.ymin_base,set_value = string(ymin,format='(G)')
      widget_control,info.ymax_base,set_value = string(ymax,format='(G)')
      info.ymin = ymin
      info.ymax = ymax
      svp_lineplot_plot,info,xarray,yarray
      end
   'PAN_D1': begin
      ; move up 1/4 of screen
      widget_control,info.ymin_base,get_value = ymin
      widget_control,info.ymax_base,get_value = ymax
      ymin=double(ymin) & ymax=double(ymax)
      yrange=ymax-ymin
      m1=min(yarray,max=m2)
      ymin = (ymin - yrange*0.1d) < m2 > m1
      ymax = (ymin + yrange) < m2 > m1
      widget_control,info.ymin_base,set_value = string(ymin,format='(G)')
      widget_control,info.ymax_base,set_value = string(ymax,format='(G)')
      info.ymin = ymin
      info.ymax = ymax
      svp_lineplot_plot,info,xarray,yarray
      end
   'XLOG': begin
      info.xlog = 1 - info.xlog
      if info.xlog eq 1 then v='X Linear' else v='X Log'
      widget_control,info.xbutton,set_value=v
      svp_lineplot_plot,info,xarray,yarray
      end
    'YLOG': begin
      info.ylog = 1 - info.ylog
      if info.ylog eq 1 then v='Y Linear' else v='Y Log'
      widget_control,info.ybutton,set_value=v
      svp_lineplot_plot,info,xarray,yarray
      end
  'Statistics': begin
      ; print the statistics from the current zoom in the log window
      widget_control,info.xmin_base,get_value = xmin
      widget_control,info.xmax_base,get_value = xmax
      xmin=double(xmin) & xmax=double(xmax)
      svp_lineplot_select,info,'Select Which Plot to get STATS',isel,group=event.top
      if isel ne 0 then i1=round(total(info.ns[0:isel-1])) else i1=0
      i2 = i1 + info.ns[isel]-1
      x = xarray[i1:i2]
      y = yarray[i1:i2]
      xrange = info.xsave.crange
      good = where((x ge xrange[0]) and (x le xrange[1]),n)
      if n eq 0 then begin
         widget_control,info.log,set_v='No Points found in Xrange'
         return
      endif
      x = x[good]
      y = y[good]
      widget_control,info.log,/append,set_v='  '
      widget_control,info.log,/append,set_v=info.title[isel]
      val=strcompress(string(min(x,max=m)))
      widget_control,info.log,/append,set_v='X-Min:    '+val
      val=strcompress(string(m))
      widget_control,info.log,/append,set_v='X-Max:    '+val
      m=min(y,pos)
      val=strcompress(string(m)+' at '+string(x[pos]))
      widget_control,info.log,/append,set_v='Y-Min:    '+val
      m=max(y,pos)
      val=strcompress(string(m)+' at '+string(x[pos]))
      widget_control,info.log,/append,set_v='Y-Max:    '+val
      result=moment(y,/double,sdev=sdev)
      val=strcompress(string(result[0]))
      widget_control,info.log,/append,set_v='Y-Mean:   '+val
      val=strcompress(string(median(y)))
      widget_control,info.log,/append,set_v='Y-Median: '+val
      val=strcompress(string(result[1]))
      widget_control,info.log,/append,set_v='Variance: '+val
      val=strcompress(string(sdev))
      widget_control,info.log,/append,set_v='Std Dev:  '+val
      val=strcompress(string(result[2]))
      widget_control,info.log,/append,set_v='Skewness: '+val
      val=strcompress(string(result[3]))
      widget_control,info.log,/append,set_v='Kurtosis: '+val
      val=strcompress(string(result[0]/sdev))
      widget_control,info.log,/append,set_v='S/N: '+val
      widget_control,info.log,/append,set_v='  '
      end

  'No Baseline': begin
      svp_lineplot_gfit,info,xarray,yarray,uvalue,group=event.top
      end
  'Constant Baseline': begin
      svp_lineplot_gfit,info,xarray,yarray,uvalue,group=event.top
      end
  'Linear Baseline': begin
      svp_lineplot_gfit,info,xarray,yarray,uvalue,group=event.top
      end
  'Interactive/Multiple': begin
      svp_lineplot_gfit,info,xarray,yarray,uvalue,group=event.top
      end

  'Drag': begin
      svp_lineplot_select,info,'Select Which Plot to DRAG', isel,group=event.top
      info.dragid=isel
      wset,info.plot2_id
      erase
      xyouts,10,150,/dev,'Place Cursor on ORIGIN point', charsize=2.2,charthick=2
      xyouts,10,110,/dev,'and push left mouse button', charsize=2.2,charthick=2
      info.state = 'DRAG1'
      end
  'Drag Reset': begin
      yoff = yarray*0
      i1 = 0L
      widget_control,info.yoff_base,get_value=yoffval
      info.yoffval = yoffval
      good = bytarr(n_elements(yarray))
      for i=0,info.n-1 do begin
         i2 = i1+info.ns[i]-1
         xarray[i1:i2] -= info.dragx[i]
         yarray[i1:i2] -= info.dragy[i]
         info.dragx[i]=0.0
         info.dragy[i]=0.0
         info.min_val[i] = min(yarray[i1:i2], max=mx)
         info.max_val[i] = mx
         i1 = i2+1
      endfor
      svp_lineplot_plot,info,xarray,yarray
      end
   'PLOT1': begin
      xd = event.x   ;device coordinates
      yd = event.y
      wset,info.plot1_id
      device,copy=[0,0,info.wXsize,info.wYsize,0,0,info.pixid]
      if (info.state eq 'X/Y') then begin
          plots,[xd,xd],[0,info.wYsize],/dev
          plots,[0,info.wXsize],[yd,yd],/dev
          if event.press eq 1 then begin
             !x = info.xsave
             !y = info.ysave
             v = convert_coord(xd,yd,/dev,/to_data,/double)
             widget_control,info.log,/append,set_value='X = '+strtrim(string(v[0],format='(G)'),2)+$
                 '    Y = '+strtrim(string(v[1],format='(G)'),2)
             p0=0L
             for i=0,info.n-1 do begin
                 p1=p0+info.ns[i]-1L
                 yval = interpol(yarray[p0:p1],xarray[p0:p1], v[0])
                 widget_control,info.log,/append,set_value='    Plot id'+strtrim(string(i+1,format='(I02)'),2)+$
                     '    Y = '+strtrim(string(yval,format='(G)'),2)
                 p0=p1+1L
             endfor
          endif
          if event.press eq 2 then begin
            info.x1 = xd
            info.y1 = yd
            info.state = 'ZOOM2'
            return
          endif
      end
      if info.state eq 'ZOOM1' and (event.press eq 1) then begin
         plots,[xd,xd],[0,info.wYsize],/dev
         plots,[0,info.wXsize],[yd,yd],/dev
         info.x1 = xd
         info.y1 = yd
         wset,info.plot2_id
         erase
         xyouts,10,150,/dev,'Position at second corner and', charsize=2.2,charthick=2
         xyouts,10,110,/dev,'push left mouse button', charsize=2.2,charthick=2
         info.state = 'ZOOM2'
         return
      endif
      if (info.state eq 'ZOOM2') then begin
          x = [info.x1,xd]
          y = [info.y1,yd]
          plots,[x(0),x(1),x(1),x(0),x(0)], [y(0),y(0),y(1),y(1),y(0)],/dev

          if (event.release eq 2) or (event.press eq 1) then begin
             !x = info.xsave
             !y = info.ysave
             v = convert_coord(x,y,/dev,/to_data,/double)
             x = v(0,*)
             y = v(1,*)
             xmin = MIN(x, MAX=xmax)
             ymin = MIN(y, MAX=ymax)
             widget_control,info.xmin_base,set_value = string(xmin,format='(G)')
             widget_control,info.xmax_base,set_value = string(xmax,format='(G)')
             widget_control,info.ymin_base,set_value = string(ymin,format='(G)')
             widget_control,info.ymax_base,set_value = string(ymax,format='(G)')
             info.xmin = xmin
             info.xmax = xmax
             info.ymin = ymin
             info.ymax = ymax
             svp_lineplot_plot,info,xarray,yarray
             svp_lineplot_annotate,info
             return
          endif
      endif

      if info.state eq 'DRAG1' and (event.press eq 1) then begin
         plots,[xd,xd],[0,info.wYsize],/dev
         plots,[0,info.wXsize],[yd,yd],/dev
         info.x1 = xd
         info.y1 = yd
         wset,info.plot2_id
         erase
         xyouts,10,150,/dev,'Position at DESTINATION point and', charsize=2.2,charthick=2
         xyouts,10,110,/dev,'push left mouse button', charsize=2.2,charthick=2
         info.state = 'DRAG2'
         return
      endif
      if (info.state eq 'DRAG2') then begin
          if event.modifiers eq 1 then begin
              ; if Shift key was pressed move vertically only
              x = [info.x1,info.x1]
              y = [info.y1,yd]
          endif else if event.modifiers eq 2 then begin
              ; if Shift key was pressed move horizontally only
              x = [info.x1,xd]
              y = [info.y1,info.y1]
          endif else begin
              x = [info.x1,xd]
              y = [info.y1,yd]
          endelse
          plots,[x(0),x(1)], [y(0),y(1)],/dev

          if (event.release eq 2) or (event.press eq 1) then begin
             !x = info.xsave
             !y = info.ysave
             v = convert_coord(x,y,/dev,/to_data,/double)
             x = v(0,*)
             y = v(1,*)
             deltax = x[1] - x[0]
             deltay = y[1] - y[0]
             if info.dragid gt 0 then i1 = round(total(double(info.ns[0:info.dragid-1]))) else i1 = 0L
             i2 = i1 + info.ns[info.dragid]-1
             info.dragx[info.dragid] += deltax
             info.dragy[info.dragid] += deltay
             xarray[i1:i2] += deltax
             yarray[i1:i2] += deltay
             info.min_val[info.dragid] = min(yarray[i1:i2], max=mx)
             info.max_val[info.dragid] = mx
             svp_lineplot_plot,info,xarray,yarray
             svp_lineplot_annotate,info
             widget_control,info.log,/append,set_v='  '
             widget_control,info.log,/append,set_v=info.title[info.dragid]
             widget_control,info.log,/append,set_value='Drag_X = '+strtrim(info.dragx[info.dragid],2)
             widget_control,info.log,/append,set_value='Drag_Y = '+strtrim(info.dragy[info.dragid],2)
             info.dragid=-1
             return
          endif
      endif

      end
   'PLOT2':
   'YOFF': svp_lineplot_plot,info,xarray,yarray
   'NORMALIZE': svp_lineplot_plot,info,xarray,yarray
   else:
   endcase
   return
   end
;=========================================================== LINEPLOT_SELECT
;
; Select which vector to use
;
pro svp_lineplot_select,info,title,isel,group=group,all=all

   n = info.n
   if info.n eq 1 then begin
      isel = 0
      return
   end

   main = widget_base(/col,title=title,xsize=400,/modal,group=group)
   buttons = lonarr(n)
   for i=0,n-1 do buttons[i] = widget_button(main,value = 'PLOT '+strtrim(i+1,2)+': '+ info.title[i])

   ; add option to save all plots
   if keyword_set(all) then begin
       buttons = [buttons,widget_button(main,value = 'ALL PLOTS')]
   endif

   widget_control,main,/realize
   ptr = ptr_new({buttons:buttons,isel:0})
   widget_control,main,set_uvalue=ptr
   xmanager,'lineplot_select',main
   isel = (*ptr).isel
   ptr_free,ptr
   return
end
pro svp_lineplot_select_event,event
   widget_control,event.top,get_uvalue=ptr
   good = where((*ptr).buttons eq event.id)
   (*ptr).isel = good[0]
   widget_control,event.top,/destroy
   return
end
;============================================================ LINEPLOT_PLOT
;
; Routine to generate the plot
;
pro svp_lineplot_plot,info,xarray,yarray,ps=ps

   if not keyword_set(ps) then begin
      wset,info.plot1_id
      ;set_viewport,0.1,0.9,0.1,0.9
      mypos=!p.position
      !p.position=[0.1,0.1,0.9,0.9]
   end
   widget_control,info.xmin_base,get_value=xmin
   widget_control,info.xmax_base,get_value=xmax
   widget_control,info.ymin_base,get_value=ymin
   widget_control,info.ymax_base,get_value=ymax
   widget_control,info.yoff_base,get_value=yoff
   xmin=double(xmin) & xmax=double(xmax)
   ymin=double(ymin) & ymax=double(ymax)
   normalize=widget_info(info.normal_base,/button_set)
   info.xmin = xmin
   info.xmax = xmax
   info.ymin = ymin
   info.ymax = ymax
   info.yoffval = yoff
   info.normalize=normalize
   plot,[xmin,xmax],[ymin,ymax],/nodata,ytitle=info.ytitle, $
      xtitle=info.xtitle,title=info.ptitle,xstyle=1,ystyle=1, $
      xlog=info.xlog,ylog=info.ylog,charsize=info.charsize,_extra=*info.plotkw
   i1 = 0
   ; mn=min(info.min_val[0:info.n-1])
   ; mx=max(info.max_val[0:info.n-1])
   for i=0,info.n-1 do begin
      i2 = i1+info.ns(i)-1
      if normalize then begin
         ; bring this plot to same scale  of the current window
         p1=where(xarray[i1:i2] ge xmin[0] and xarray[i1:i2] le xmax[0],count)
         if count eq 0 then continue
         m1=min((yarray[i1:i2])[p1],max=m2)
         scale = double(ymax-ymin)/double(m2-m1)
         temp = scale[0] * double(yarray[i1:i2]-m1)
         temp = temp + ymin[0] + yoff*i
      endif else begin
         temp = yarray[i1:i2] + yoff*i
      endelse
      oplot,xarray[i1:i2],temp, $
         color=i+2,psym=info.psym[i], $
         line=info.linestyle(i),thick=info.thick(i), $
         symsize=info.symsize(i),nsum=info.nsum(i), _extra=*info.plotkw ;, $
         ; min_val = info.min_val(i)+yoff*i, $
         ; max_val = info.max_val(i)+yoff*i
      i1  = i2+1
   endfor
   if info.ylog ne 1 then oplot,!x.crange,[0,0],line=2
   info.xsave = !x
   info.ysave = !y
   xrange = !x.crange
   if info.xlog then xrange = 10^xrange
   yrange = !y.crange
   if info.ylog then yrange = 10^yrange
   widget_control,info.xmin_base,set_value=string(xrange[0],format='(G)')
   widget_control,info.xmax_base,set_value=string(xrange[1],format='(G)')
   widget_control,info.ymin_base,set_value=string(yrange[0],format='(G)')
   widget_control,info.ymax_base,set_value=string(yrange[1],format='(G)')
   if not keyword_set(ps) then begin
      wset,info.pixid
      device,copy=[0,0,info.wXsize,info.wYsize,0,0,info.plot1_id]
   endif
   info.state = 'X/Y'
   if n_elements(mypos) eq 4 then !p.position=mypos
return
end
;============================================================= LINEPLOT_ANNOTATE
;
; Routine to fill annotation box
;
pro svp_lineplot_annotate,info,ps=ps

   if keyword_set(ps) then begin
      ;set_viewport,0,1.0,0,1.0
      mypos=!p.position
      !p.position=[0.0,0.0,1.0,1.0]
      y1 = 0.25
      dy = 0.03
      x = [0.2,0.3,0.33]
      charsize = 1.4
   endif else begin
      y1 = 0.95
      dy = 0.04
      x = [0.05,0.14,0.18]
      charsize = 0.9
      wset,info.plot2_id
      erase
   endelse
   for i=0,info.n-1 do begin
      psym = info.psym[i]
      if psym eq 10 then psym=0
      plots,x[0:1],[y1,y1]-i*dy,psym=psym,color=i+2, line=info.linestyle[i],$
         thick=info.thick[i], symsize=info.symsize[i],/norm
      xyouts,x[2],y1-dy*0.25-i*dy,info.title[i],/norm,color=i+2, charsize=charsize
   endfor
   if n_elements(mypos) eq 4 then !p.position=mypos
   return
   end

;=========================================================== LINEPLOT_PLOTPAR
;
; Routine to adjust the plotting parameters
;
pro svp_lineplot_plotpar,group=group
   common svp_lineplot_common,info,xarray,yarray,lineplot_r,lineplot_g,lineplot_b,$
               svp_lineplot_rsave,lineplot_gsave,lineplot_bsave


;
; if already active, return
;
   if xregistered('lineplot_plotpar') then return
;
; set up widget layout
;

   mainbase = widget_base(/col,title='Plot Parameter Adjustment', $
      group=group)
   button = widget_button(mainbase,uvalue='DONE',value='Done')
   basex = widget_base(mainbase,/row)
   plot_select = cw_bgroup(basex,info.title+' ', $
         /col,set_value=0,uvalue='PLOT_SELECT', $
         /exclusive)
   base = widget_base(basex,/col,/frame)
   window = widget_draw(base,xsize=300,ysize=50)
;
; color sliders
;
   red_slider = widget_slider(base,min=0,max=255,/drag,uvalue='RED', $
         title='Red',value=0,xsize=280)
   green_slider = widget_slider(base,min=0,max=255,/drag,uvalue='GREEN', $
         title='Green',value=0,xsize=280)
   blue_slider = widget_slider(base,min=0,max=255,/drag,uvalue='BLUE', $
         title='Blue',value=0,xsize=280)
;
; linestype
;
   base1 = widget_base(base,/row)
   label = widget_label(base1,value='Linestyle',xsize=80,/align_left)
   linestyle = widget_droplist(base1,uvalue='LINESTYLE', $
         value=['Solid','Dotted','Short Dash','Dash Dot Dash', $
            'Dash Dot Dot Dot Dash','Long Dash'])
   widget_control,linestyle,set_droplist_select=0
;
; Psym
;
   base2 = widget_base(base,/row)
   label = widget_label(base2,value='Psym',xsize=80,/align_left)
   psym = widget_droplist(base2,uvalue='PSYM', $
      value =["Connected X's","Connected Squares", $
         "Connected Triangles","Connected Diamonds", $
         "Connected Asterisks","Connected Plus Signs", $
         "No Symbol","Plus Signs","Asterisks","Dots", $
         "Diamonds","Triangles","Squares","X's","Histogram"])
   psyms = [-7,-6,-5,-4,-2,-1,0,1,2,3,4,5,6,7,10]
;
; Psymsize, pthick
;
   size_field = cw_field(base,uvalue='SIZE_FIELD',xsize=8, $
         value=0, /float,title='Symsize       ',/return_events)

   thick_field = cw_field(base,uvalue='THICK_FIELD',xsize=8, $
         value=0, /float,title='Line Thickness',/return_events)
   nsum_field = cw_field(base,uvalue='NSUM',value=1,/long, $
         xsize=5,title='NSUM',/return_events)
   button = widget_button(base,uvalue='APPLY',value='APPLY')
   label = widget_label(base,value='   ',xsize=10,/align_left)
   if info.n gt 1 then sensitive=1 else sensitive=0
   button = widget_button(base,uvalue='DELETE',value='DELETE',sensitive=sensitive)
;
; create and run the widget
;

   widget_control,mainbase,/realize
   widget_control,window,get_value=window_id
   base = {window_id:window_id, red_slider:red_slider, $
         green_slider:green_slider,blue_slider:blue_slider, $
         linestyle:linestyle,psym:psym,size_field:size_field, $
         thick_field:thick_field, $
         plot_select:plot_select,nsum_field:nsum_field}
   point = ptr_new({base:base})
   widget_control,mainbase,set_uvalue=point
   svp_lineplot_plotpar_set,base,info,lineplot_r,lineplot_g,lineplot_b
   xmanager,'lineplot_plotpar',mainbase,/no_block

   return
end

pro svp_lineplot_plotpar_event,event
   common svp_lineplot_common,info,xarray,yarray,lineplot_r,lineplot_g,lineplot_b,$
               svp_lineplot_rsave,lineplot_gsave,lineplot_bsave


   widget_control,event.id,get_uvalue=uvalue
   widget_control,event.top,get_uvalue=point
   base = (*point).base
   widget_control,base.plot_select,get_value=i   ;current selection
   apply = 0

   case uvalue of
   'DONE': begin
      svp_lineplot_plot,info,xarray,yarray
      svp_lineplot_annotate,info
      if ptr_valid(point) then ptr_free,point
      widget_control,event.top,/destroy
      return
      end
   'PLOT_SELECT': begin
      if event.select eq 1 then begin
         svp_lineplot_plotpar_set,base,info,lineplot_r,lineplot_g,lineplot_b
         return
      end
      end
   'RED': svp_lineplot_r[i+2] = event.value
   'BLUE': svp_lineplot_b[i+2] = event.value
   'GREEN': svp_lineplot_g[i+2] = event.value
   'LINESTYLE': info.linestyle[i] = event.index
   'PSYM': begin
      psyms = [-7,-6,-5,-4,-2,-1,0,1,2,3,4,5,6,7,10]
      info.psym[i] = psyms[event.index]
      end
   'APPLY': apply = 1
   'DELETE': begin
      ; prompt user to make sure the plot is to be deleted
      msg = 'Delete plot '+strtrim(string(i),2)+' '+info.title[i]+' ?'
      res=dialog_message(msg,dialog_parent=base.plot_select,/question,title='Deleting plot')
      if res eq 'Yes' then begin
         if i eq 0 then begin
             xarray=xarray[info.ns[i]:-1]
             yarray=yarray[info.ns[i]:-1]
         endif else if i eq info.n-1 then begin
             xarray=xarray[0:-info.ns[i]-1]
             yarray=yarray[0:-info.ns[i]-1]
         endif else begin
             p1 = total(info.ns[0:i-1])
             xarray=[xarray[0:p1-1L],xarray[p1+info.ns[i]:-1]]
             yarray=[yarray[0:p1-1L],yarray[p1+info.ns[i]:-1]]
         endelse
         info.ns[i]=0
         info.title[i]=''
         info.min_val[i]=0 & info.max_val[i]=0
         if ptr_valid(info.header[i]) then ptr_free,info.header[i]
         info.psym[i]=-3 & info.linestyle[i]=0 & info.thick[i]=0
         ; now shift all plots
         for j=i+1,n_elements(info.ns)-1 do begin
             info.ns[j-1]=info.ns[j]
             info.psym[j-1]=info.psym[j]
             info.thick[j-1]=info.thick[j]
             info.symsize[j-1]=info.symsize[j]
             info.linestyle[j-1]=info.linestyle[j]
             info.title[j-1]=info.title[j]
             info.nsum[j-1]=info.nsum[j]
             info.min_val[j-1]=info.min_val[j]
             info.max_val[j-1]=info.max_val[j]
             if ptr_valid(info.header[j-1]) then begin
                 ptr_free,info.header[j-1]
                 info.header[j-1]=info.header[j]
             endif
             svp_lineplot_r[j-1+2]=lineplot_r[j+2]
             svp_lineplot_g[j-1+2]=lineplot_g[j+2]
             svp_lineplot_b[j-1+2]=lineplot_b[j+2]
         endfor
         info.n -= 1
         svp_lineplot_plotpar_set,base,info,lineplot_r,lineplot_g,lineplot_b
         svp_lineplot_plot,info,xarray,yarray
         svp_lineplot_annotate,info
         if ptr_valid(point) then ptr_free,point
         widget_control,event.top,/destroy
         return
      endif
      end

   else:
   endcase

   widget_control,base.thick_field,get_value=v
   info.thick[i] = v
   widget_control,base.size_field,get_value=v
   info.symsize[i] = v
   widget_control,base.nsum_field,get_value=v
   info.nsum[i] = v

   svp_lineplot_plotpar_set,base,info,lineplot_r,lineplot_g,lineplot_b
   if apply then begin
      svp_lineplot_plot,info,xarray,yarray
      svp_lineplot_annotate,info
   endif

    return
end
;======================================================== LINEPLOT_PLOTPAR_SET
;
; Routine to set plot paramters for the selected plot
;
pro svp_lineplot_plotpar_set,base,info,red,green,blue

   widget_control,base.plot_select,get_value=isel
   widget_control,base.red_slider,set_value=red(isel+2)
   widget_control,base.green_slider,set_value=green(isel+2)
   widget_control,base.blue_slider,set_value=blue(isel+2)
   widget_control,base.linestyle,set_droplist_select=info.linestyle(isel)
   psyms = [-7,-6,-5,-4,-2,-1,0,1,2,3,4,5,6,7,10]
   good = where(psyms eq info.psym(isel))
   widget_control,base.psym,set_droplist_select=good(0)
   widget_control,base.size_field,set_value=info.symsize(isel)
   widget_control,base.thick_field,set_value=info.thick(isel)
   widget_control,base.nsum_field,set_value=info.nsum(isel)
;
; update plot
;
   wset,base.window_id
   erase
   tvlct,red,green,blue
   ps = info.psym(isel)
   if ps eq 10 then ps=0
       plots,indgen(11)*30,replicate(25,11),/dev,psym=ps, $
      color=isel+2,line=info.linestyle(isel),thick=info.thick(isel), $
      symsize = info.symsize(isel)

return
end
;========================================================== LINEPLOT_GFIT
;
; Gaussian Fit Routine for line plot
;
pro svp_lineplot_gfit,info,xarray,yarray,typename,group=group
;
; Extract data region to fit
;
   svp_lineplot_select,info,'Select Which Plot to FIT',isel,group=group
   svp_lineplot_plot,info,xarray,yarray
   if isel ne 0 then i1 = round(total(info.ns[0:isel-1])) $
              else i1 = 0
   i2 = i1 + info.ns[isel]-1
   x = xarray[i1:i2]
   y = yarray[i1:i2]
   xrange = info.xsave.crange
   good = where((x ge xrange[0]) and (x le xrange[1]),n)
   if n eq 0 then begin
      widget_control,info.log,set_v='No Points found in Xrange'
      return
   end
   x = double(x[good])
   y = double(y[good])
   widget_control,info.yoff_base,get_v=yoffset
   yoffset = yoffset*isel
;
; emission or absorption
;
   if strpos(strupcase(typename),'NO BASELINE') ge 0 then type=1
   if strpos(strupcase(typename),'CONSTANT') ge 0 then type=2
   if strpos(strupcase(typename),'LINEAR') ge 0 then type=3
   if strpos(strupcase(typename),'INTERACTIVE') ge 0 then type=4

   widget_control,info.log,/append,set_v='-----  Gaussian Fit'
   widget_control,info.log,/append,set_v=info.title[isel]
   widget_control,info.log,/append,set_v='Xrange = '+strtrim(min(x),'2')+ $
         ' to '+strtrim(max(x),2)
   if strpos(strupcase(typename),'INTERACTIVE') ge 0 then begin
       xgaussfit,x,y,bcoef,gcoef,fit,title=info.title(isel)
       if n_elements(gcoef) gt 0 then begin
          widget_control,info.log,/append,set_v= $
                '       Peak                 Center             FWHM'
          n = n_elements(gcoef)/3
          for i=0,n-1 do begin
             widget_control,info.log,/append,set_v=string(gcoef[0,i],'(G14.8)')+'   '+ $
               string(gcoef[1,i],'(G14.8)')+ '   '+string(gcoef[2,i]*2.3548,'(G14.8)')
          endfor
      endif
   endif else begin
       ; normalize the data before fitting -> works best when
       ; wave and flux scales are very different
       xmin=min(x,max=xmax)
		 xrange = xmax-xmin
		 ymax = max(y)
		 xx = double((x - xmin)/xrange)
		 yy = double(y/ymax)
       fit = gaussfit(xx,yy,coef,nterms=2+type)   ;peak,center,sigma
       ; re-evalute the peak with the fit
       if strpos(strupcase(typename),'NO BASELINE') ge 0 then begin
          coef[0] *= ymax
       endif else if strpos(strupcase(typename),'CONSTANT') ge 0 then begin
          coef[0] = (coef[0]+coef[3])*ymax
       endif else if strpos(strupcase(typename),'LINEAR') ge 0 then begin
          coef[0] = (coef[0]+coef[3]+coef[1]*coef[4])*ymax
       endif
       coef[1] = coef[1]*xrange + xmin
       coef[2] *= xrange
       fit *= ymax
       a=coef
       wset,info.plot1_id
       oplot,x,fit+yoffset,thick=2
       widget_control,info.log,/append,set_v='  Peak  = '+strtrim(a[0],2)
       widget_control,info.log,/append,set_v='  Center = '+strtrim(string(a[1],format='(F0.5)'),2)
       widget_control,info.log,/append,set_v='  FWHM = '+ $
                      strtrim(a[2]*2.3548,2)
   endelse

   wset,info.pixid
   device,copy=[0,0,info.wXsize,info.wYsize,0,0,info.plot1_id]
   return
end
;==================================================================== LINEPLOT
;
; Plot widget main routine

pro svp_lineplot,xin,yin,title=title,xtitle=xtitle,ytitle=ytitle, $
   ptitle=ptitle,group=group,xrange=xrange,yrange=yrange, $
   min_val=min_val,max_val=max_val,restore=restore, header=header, $
   outfile=outfile, psym=psym, color=color, charsize=charsize, $
   linestyle=linestyle, thick=thick, nsum=nsum, unzoom=unzoom,$
   xsize=xsize,ysize=ysize,_extra=extra

   common svp_lineplot_common,info,xarray,yarray,lineplot_r,lineplot_g,lineplot_b,$
          svp_lineplot_rsave,lineplot_gsave,lineplot_bsave, cwd

   MAX_NUM_PLOTS = 24

IF NOT KEYWORD_SET(restore)  THEN BEGIN
   ;
   ; not restoring from previous session -> normal startup
   ;
   if n_elements(xsize) ne 1 then xsize=1200
   if n_elements(ysize) ne 1 then ysize=450

   if n_elements(title) eq 0 then title=''
   if n_elements(xtitle) eq 0 then xtitle=''
   if n_elements(ytitle) eq 0 then ytitle=''
   if n_elements(ptitle) eq 0 then ptitle=''
   if n_elements(charsize) eq 0 then charsize=1.0
   ; if n_elements(cwd) eq 0 then cwd=CEDAR_GETENV('CEDAR_RESULTS_PATH')
   if n_elements(cwd) eq 0 then cd,current=cwd
   if n_elements(outfile) eq 0 then begin
      outfname=''
   endif else begin
      ;extract the filename from the input string
      fdecomp,outfile,disk,dir,outfname,ext
   endelse
   if n_params(0) eq 1 then begin
      y = xin
      x = findgen(n_elements(xin))
   endif else begin
      x = xin
      y = yin
   endelse
   ;
   ; add new x and y to common block
   ;
   ns = n_elements(x)
   if xregistered('lineplot') then begin
      if info.n ge MAX_NUM_PLOTS then return
      if n_elements(xrange) gt 0 then begin
         widget_control,info.xmin_base,set_v=string(xrange[0],format='(G)')
         widget_control,info.xmax_base,set_v=string(xrange[1],format='(G)')
         info.xmin = xrange[0]
         info.xmax = xrange[1]
      end
      if n_elements(yrange) gt 0 then begin
         widget_control,info.ymin_base,set_v=string(yrange[0],format='(G)')
         widget_control,info.ymax_base,set_v=string(yrange[1],format='(G)')
         info.ymin = yrange[0]
         info.ymax = yrange[1]
      end
      xarray = [xarray,x]
      yarray = [yarray,y(0:ns-1)]
      info.ns[info.n] = ns
      info.title[info.n] = title
      if n_elements(min_val) gt 0 then info.min_val[info.n] = min_val else info.min_val[info.n]=min(y)
      if n_elements(max_val) gt 0 then info.max_val[info.n] = max_val else info.max_val[info.n]=max(y)
      widget_control,info.log,/append,set_v=title
      if n_elements(header) gt 0 then info.header[info.n] = PTR_NEW(header)
      if keyword_set(psym) then info.psym[info.n]=psym
      if keyword_set(linestyle) then info.linestyle[info.n]=linestyle
      if keyword_set(thick) then info.thick[info.n]=thick
      if keyword_set(nsum) then info.nsum[info.n]=nsum
      if n_elements(color) eq 3 then begin
          svp_lineplot_r[info.n+2] = color[0]
          svp_lineplot_g[info.n+2] = color[1]
          svp_lineplot_b[info.n+2] = color[2]
          tvlct,lineplot_r,lineplot_g,lineplot_b
      endif
      info.n = info.n + 1
      if xtitle ne '' then info.xtitle=xtitle
      if ytitle ne '' then info.ytitle=ytitle
      if ptitle ne '' then begin
         info.ptitle=ptitle
         widget_control,info.log,/append,set_v=ptitle
      end
      svp_lineplot_plot,info,xarray,yarray
      svp_lineplot_annotate,info

       if keyword_set(unzoom) then widget_control,info.unzoom_id, send_event={ID:info.unzoom_id, TOP:info.main, HANDLER:info.main, SELECT:1}

      return
   endif
   ;
   ; initilization
   ;
   xarray = reform(x)
   yarray = reform(y)
   tvlct,lineplot_r,lineplot_g,lineplot_b,/get
   svp_lineplot_rsave = svp_lineplot_r
   svp_lineplot_gsave = svp_lineplot_g
   svp_lineplot_bsave = svp_lineplot_b
   ; black on white background
   xmin_val = MIN(x, MAX=xmax_val)
   ymin_val = MIN(y, MAX=ymax_val)

ENDIF ELSE BEGIN
   ;
   ; restoring from a previously saved session
   ;
   xmin_val = info.xmin
   xmax_val = info.xmax
   ymin_val = info.ymin
   ymax_val = info.ymax
ENDELSE

   loadct,0
   tek_color,0,25
   !p.background=255
   !p.color=0
   ; replace the almost invisible yellow on white background with a darker color
   tvlct,160,60,0,7
   tvlct,lineplot_r,lineplot_g,lineplot_b,/get

;;   widget_control,default_font  = $
;;       '-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1'
   main_base = widget_base(/col,group=group,/tracking,uvalue='MAIN_LINEPLOT',/TLB_SIZE_EVENTS)
   menu = widget_base(main_base,/row,/frame)
   exit = widget_button(menu,value='EXIT',uvalue='EXIT')
   buttonWrite = widget_button(menu,menu=2, value='Write', uvalue='WRITE')
   button = widget_button(buttonWrite, value='Postscript File', uvalue='Postscript File')
   button = widget_button(buttonWrite, value='PNG File', uvalue='PNG File')
   button = widget_button(buttonWrite, value='ASCII Table', uvalue='ASCII Table')
   button = widget_button(buttonWrite, value='FITS Table', uvalue='FITS Table')
   button = widget_button(buttonWrite, value='Log File', uvalue='Log File')
   unzoom_id = widget_button(menu,uvalue='UNZOOM_ALL',value='UnZoom All')
   unzoomx = widget_button(menu,uvalue='UNZOOM_X',value='UnZoom_X')
   unzoomy = widget_button(menu,uvalue='UNZOOM_Y',value='UnZoom_Y')
   unzoomxy = widget_button(menu,uvalue='UNZOOM',value='UnZoom')
   zoom = widget_button(menu,uvalue='ZOOM',value='Zoom')
   button = widget_button(menu,uvalue='LINESTYLES',value='Linestyles')
   xbutton = widget_button(menu,uvalue='XLOG',value='X Log     ')
   ybutton = widget_button(menu,uvalue='YLOG',value='Y Log     ')
   buttonTools = widget_button(menu,menu=2,value='Tools',uvalue='TOOLS')
   button = widget_button(buttonTools, value='Statistics', uvalue='Statistics')
   buttonGauss = widget_button(buttonTools, menu=2, value='GAUSSFIT', uvalue='GAUSSFIT')
   button = widget_button(buttonGauss, value='No Baseline', uvalue='No Baseline')
   button = widget_button(buttonGauss, value='Constant Baseline', uvalue='Constant Baseline')
   button = widget_button(buttonGauss, value='Linear Baseline', uvalue='Linear Baseline')
   button = widget_button(buttonGauss, value='Interactive/Multiple', uvalue='Interactive/Multiple')
   button = widget_button(buttonTools, value='Drag', uvalue='Drag')
   button = widget_button(buttonTools, value='Drag Reset', uvalue='Drag Reset')
   button = widget_button(menu,uvalue='PAN_R2',value=' << ')
   button = widget_button(menu,uvalue='PAN_R1',value=' < ')
   button = widget_button(menu,uvalue='PAN_L1',value=' > ')
   button = widget_button(menu,uvalue='PAN_L2',value=' >> ')
   button = widget_button(menu,uvalue='PAN_U1',value=' ^ ')
   button = widget_button(menu,uvalue='PAN_D1',value=' v ')
;
; draw window 1
;
   wMinXsize=xsize ; 1000
   wMinYsize=ysize ; 450
   wXsize=wMinXsize
   wYsize=wMinYsize
   base1 = widget_base(main_base,/row)
   plot1 = widget_draw(base1,uvalue='PLOT1',retain=2, $
            xsize=wXsize,ysize=wYsize,/button_events,/motion)
;
; draw window 2
;
   base = widget_base(main_base,/row)
   plot2 = widget_draw(base,xsize=400,ysize=300,/button_events, $
      uvalue='PLOT2',/motion)
   log = widget_text(base,xsize=55,ysize=8,/scroll,uvalue='MESSAGE',/edit)
   basex = widget_base(base,/col)
   xmin_base = cw_field(basex,/row,uvalue='RANGE',value=string(xmin_val,format='(G)'), $
          title='X Min: ',xsize=18,/return_events,/string)
   xmax_base = cw_field(basex,/row,uvalue='RANGE',value=string(xmax_val,format='(G)'), $
          title='X Max: ',xsize=18,/return_events,/string)
   ymin_base = cw_field(basex,/row,uvalue='RANGE',value=string(ymin_val,format='(G)'), $
          title='Y Min: ',xsize=18,/return_events,/string)
   ymax_base = cw_field(basex,/row,uvalue='RANGE',value=string(ymax_val,format='(G)'), $
          title='Y Max: ',xsize=18,/return_events,/string)
   yoff_base = cw_field(basex,/row,uvalue='YOFF',value=0.0, $
      title='Y Offsets:',xsize=11,/return_events,/float)

   base1a      = widget_base(basex, /ROW, /NONEXCLUSIVE)
   normal_base = Widget_button(base1a, UVALUE='NORMALIZE', VALUE='Scale All Plots')

;
; create pixmap
;
   window,xs=wXsize,ys=wYsize,/pixmap,/free
   pixid = !d.window
;
; save widget info in structure
;
   widget_control,main_base,/realize
   widget_control,plot1,get_value=plot1_id
   widget_control,plot2,get_value=plot2_id
   WIDGET_CONTROL, main_base, TLB_GET_SIZE=windowSize

IF NOT(KEYWORD_SET(restore)) THEN BEGIN
   ;
   ; not restoring -> normal process
   ;
   ns = lonarr(MAX_NUM_PLOTS)
   ns(0) = n_elements(x)

   info = {n:1, $
      ns:ns, $
      charsize:charsize, $
      psym:intarr(MAX_NUM_PLOTS)-3, $
      thick:intarr(MAX_NUM_PLOTS), $
      symsize:replicate(1.0,MAX_NUM_PLOTS), $
      linestyle:intarr(MAX_NUM_PLOTS), $
      title:strarr(MAX_NUM_PLOTS), $
      dragx:dblarr(MAX_NUM_PLOTS), $
      dragy:dblarr(MAX_NUM_PLOTS), $
      dragid:-1, $
      pixid:pixid, $
      xtitle:xtitle, $
      ytitle:ytitle, $
      ptitle:ptitle, $
      plot1:plot1, $
      yoff_base:yoff_base, $
      main:main_base, $
      unzoom_id:unzoom_id, $
      log:log, $
      plot2:plot2, $
      xmin:double(xmin_val), $
      ymin:double(ymin_val), $
      xmax:double(xmax_val), $
      ymax:double(ymax_val), $
      xlog:0, $
      ylog:0, $
      xmin_base:xmin_base, $
      ymin_base:ymin_base, $
      xmax_base:xmax_base, $
      ymax_base:ymax_base, $
      xsave:!x, $
      ysave:!y, $
      state:'X/Y', $
      xbutton:xbutton, $
      ybutton:ybutton, $
      x1:0, $
      y1:0, $
      plot1_id:plot1_id, $
      plot2_id:plot2_id, $
      nsum:intarr(MAX_NUM_PLOTS), $
      min_val:replicate(1d37,MAX_NUM_PLOTS), $
      max_val:replicate(-1d37,MAX_NUM_PLOTS), $
      yoffval:0.0d, $
      normal_base:normal_base, $
      normalize:0, $
      outfile:outfname, $
      header: ptrarr(MAX_NUM_PLOTS) ,$
      windowSize:windowSize, $
      wXsize:wXsize, $
      wYsize:wYsize, $
      wMinXsize:wMinXsize, $
      wMinYsize:wMinYsize, $
      plotkw:ptr_new(/allocate_heap) }

   if n_elements(extra) gt 0 then *info.plotkw=extra

   if n_elements(corrwidth) eq 0 then corrwidth=40
   if n_elements(min_val) gt 0 then info.min_val[0]=min_val else info.min_val[0]=min(y)
   if n_elements(max_val) gt 0 then info.max_val[0]=max_val else info.max_val[0]=max(y)
   info.title(0) = title
   if n_elements(header) gt 0 then info.header[0] = PTR_NEW(header)
   ;
   ; set initial range
   ;
   if n_elements(xrange) gt 0 then begin
      widget_control,info.xmin_base,set_v=string(xrange[0],format='(G)')
      widget_control,info.xmax_base,set_v=string(xrange[1],format='(G)')
   end
   if n_elements(yrange) gt 0 then begin
      widget_control,info.ymin_base,set_v=string(yrange[0],format='(G)')
      widget_control,info.ymax_base,set_v=string(yrange[1],format='(G)')
   end
   if keyword_set(psym) then info.psym[0]=psym
   if keyword_set(linestyle) then info.linestyle[0]=linestyle
   if keyword_set(thick) then info.thick[0]=thick
   if keyword_set(nsum) then info.nsum[0]=nsum
   if n_elements(color) eq 3 then begin
      svp_lineplot_r[2] = color[0]
      svp_lineplot_g[2] = color[1]
      svp_lineplot_b[2] = color[2]
      tvlct,lineplot_r,lineplot_g,lineplot_b
   endif

ENDIF ELSE BEGIN
   ;
   ; restoring
   ;
   info.pixid = pixid
   info.main = main_base
   info.xmin_base = xmin_base
   info.xmax_base = xmax_base
   info.ymin_base = ymin_base
   info.ymax_base = ymax_base
   info.yoff_base = yoff_base
   info.normal_base = normal_base
   info.normalize = normalize
   info.plot1 = plot1
   info.plot2 = plot2
   info.plot1_id = plot1_id
   info.plot2_id = plot2_id
   info.xbutton = xbutton
   info.ybutton = ybutton
   info.log = log
ENDELSE

   widget_control,info.log,/append,set_v=info.ptitle
   widget_control,info.log,/append,set_v=info.title(0)
   svp_lineplot_plot,info,xarray,yarray
   svp_lineplot_annotate,info
   xmanager,'lineplot',main_base,/no_block

   if keyword_set(unzoom) then widget_control,info.unzoom_id, send_event={ID:info.unzoom_id, TOP:info.main, HANDLER:info.main, SELECT:1}

   return
end
