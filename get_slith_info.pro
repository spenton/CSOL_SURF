;+
; NAME:
;  get_slith_info
;
; PURPOSE:
;   A function that determines the top and bottom of the MUV and FUV slits was well as the number of pixels in the channel height
;   and these values are stored in a structure for all 2000 pixels in the image.  The case where the signal is too weak to detect
;   the edge is replace with a proxy for what the slit values should be.  A score is given that indicates how many indeterminant
;   pixels appear in the image.  For images where a sufficient number of pixels are present to determine an edge are then used to
;   linear fit across the image to give where the top and bottom of the channels are located.
;
; INPUTS:
;   A CSOL image represented as a 2000x1504 float array
;
; OPTIONAL INPUTS:
; None
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   a structure with parameters fuv_bot,fuv_top,fuv_npix,muv_bot,muv_top,muv_npix - all these are array of 2000 elements
;   fuv_score & muv_score report the number of valid slith determinations
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
; EXAMPLE: slith_info=get_slith_info(valid CSOL image array)
;
; MODIFICATION HISTORY: RCS_ID="$Id: get_slith_info.pro,v 1.1 2018/12/30 00:27:42 spenton Exp $"
;   2018-05-11: JWH
;-

function get_slith_info, img

  uu=edge_dog(img,radius1=7,radius2=52,threshold=80.)
  ;im=image(uu)

  n_xpix=2000
  n_ypix=1504

  fuvbot=0
  fuvmid=550
  imgcnt=752
  muvmid=930
  muvtop=1503

  ;dummy values if edge detection cannot produce a viable result set to give a 2mm tall image slit
  dummy_fuvbot=405
  dummy_fuvtop=694
  dummy_muvbot=825
  dummy_muvtop=1108

  threshold=100.

  pixno=findgen(n_ypix)

  edge=fltarr(4,n_xpix)


  for i=0,n_xpix-1 do begin
    top_bot=minmax(uu[i,fuvbot:fuvmid],top_bot_ind)
    top_bot_ind=fuvbot+top_bot_ind
    in_order=sign(top_bot_ind[0]-top_bot_ind[1])
    if abs(top_bot[0]) gt threshold and abs(top_bot[1]) gt threshold and in_order eq -1 then begin
      wave_x=pixno[top_bot_ind[0]:top_bot_ind[1]]
      wave_y=reform(uu[i,top_bot_ind[0]:top_bot_ind[1]])
      linco=robust_linefit(wave_x,wave_y,yfit)
      edge[0,i]=(-1.)*linco[0]/linco[1]
    endif

    top_bot=minmax(uu[i,fuvmid:imgcnt],top_bot_ind)
    top_bot_ind=fuvmid+reverse(top_bot_ind)
    in_order=sign(top_bot_ind[0]-top_bot_ind[1])
    if abs(top_bot[0]) gt threshold and abs(top_bot[1]) gt threshold and in_order eq -1 then begin
      wave_x=pixno[top_bot_ind[0]:top_bot_ind[1]]
      wave_y=reform(uu[i,top_bot_ind[0]:top_bot_ind[1]])
      linco=robust_linefit(wave_x,wave_y)
      edge[1,i]=(-1.)*linco[0]/linco[1]
    endif

    top_bot=minmax(uu[i,imgcnt:muvmid],top_bot_ind)
    top_bot_ind=imgcnt+top_bot_ind
    in_order=sign(top_bot_ind[0]-top_bot_ind[1])
    if abs(top_bot[0]) gt threshold and abs(top_bot[1]) gt threshold and in_order eq -1 then begin
      wave_x=pixno[top_bot_ind[0]:top_bot_ind[1]]
      wave_y=reform(uu[i,top_bot_ind[0]:top_bot_ind[1]])
      linco=robust_linefit(wave_x,wave_y,yfit)
      edge[2,i]=(-1.)*linco[0]/linco[1]
    endif

    top_bot=minmax(uu[i,muvmid:muvtop],top_bot_ind)
    top_bot_ind=muvmid+reverse(top_bot_ind)
    in_order=sign(top_bot_ind[0]-top_bot_ind[1])
    if abs(top_bot[0]) gt threshold and abs(top_bot[1]) gt threshold and in_order eq -1 then begin
      wave_x=pixno[top_bot_ind[0]:top_bot_ind[1]]
      wave_y=reform(uu[i,top_bot_ind[0]:top_bot_ind[1]])
      val=min(abs(wave_y),ind)
      linco=robust_linefit(wave_x,wave_y,yfit)
      edge[3,i]=(-1.)*linco[0]/linco[1]
    endif

  endfor

  slith_info=create_struct('fuv_bot',intarr(n_xpix),'fuv_top',intarr(n_xpix),'fuv_npix',intarr(n_xpix),$
    'muv_bot',intarr(n_xpix),'muv_top',intarr(n_xpix),'muv_npix',intarr(n_xpix),'fuv_score',0,'muv_score',0)
  xpix=findgen(n_xpix)
  ok=where(finite(edge),complement=notok,ncomplement=n_notok)
  if n_notok gt 0 then edge[notok]=0.
  ok=where(edge[0,*] ne 0. and edge[1,*] ne 0.,n_validedge)
  if n_validedge gt 500 then begin
    linco=robust_linefit(xpix[ok],edge[0,ok])
    slith_info.fuv_bot=round(poly(xpix,linco))
    linco=robust_linefit(xpix[ok],edge[1,ok])
    slith_info.fuv_top=round(poly(xpix,linco))
    slith_info.fuv_npix=slith_info.fuv_top-slith_info.fuv_bot+1
    slith_info.fuv_score=n_validedge
  endif else begin
    slith_info.fuv_bot=replicate(dummy_fuvbot,n_xpix)
    slith_info.fuv_top=replicate(dummy_fuvtop,n_xpix)
    slith_info.fuv_npix=slith_info.fuv_top-slith_info.fuv_bot+1
    slith_info.fuv_score=n_validedge
  endelse

  ok=where(edge[2,*] ne 0. and edge[3,*] ne 0.,n_validedge)
  if n_validedge gt 500 then begin
    linco=robust_linefit(xpix[ok],edge[2,ok])
    slith_info.muv_bot=round(poly(xpix,linco))
    linco=robust_linefit(xpix[ok],edge[3,ok])
    slith_info.muv_top=round(poly(xpix,linco))
    slith_info.muv_npix=slith_info.muv_top-slith_info.muv_bot+1
    slith_info.muv_score=n_validedge
  endif else begin
    slith_info.muv_bot=replicate(dummy_muvbot,n_xpix)
    slith_info.muv_top=replicate(dummy_muvtop,n_xpix)
    slith_info.muv_npix=slith_info.muv_top-slith_info.muv_bot+1
    slith_info.muv_score=n_validedge
  endelse

  return, slith_info
end
