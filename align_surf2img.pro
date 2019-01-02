;+
; NAME:align_surf2img.pro
;
;
; PURPOSE:
;   A function to provide SURF data specifically within the time stamps of each invidual CSOL image
;
; INPUTS:
;   arrays of structures corresponding to the SURF log,and the data log for CSOL housekeeping
;
; OPTIONAL INPUTS:
; None
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   Returns an array of structure reproducing the SURF data pertinent to each image file
;
; OPTIONAL OUTPUTS:
;   None
;
; PROCEDURES/FUNCTIONS CALLED:
;   best usage of this function follows calls from three procedures that locate the CSOL housekeeping, image list, and the nearest
;   time concurrentl SURF log file
;   ;retrieve the specific CSOL housekeeping log file to be analyzed
;restore,get_surf_dir(/data)+'IDL_structure_files/CSOL_logs/FOV_fuv_csolhk.sav'
;
;retrieve the index of all image files
;restore, get_surf_dir(/data)+'IDL_structure_files/csol_img_index.sav'
;n_hkrec=n_elements(csol_hk)
;csol_juld=jd_from_hk(csol_hk,n_hkrec)    ;get julian day from columns in CSOL housekeeping file
;
;ok=where(csol_img_index.juld ge csol_juld[0] and $
;  csol_img_index.juld le csol_juld[-1],imgcnt)
;img_filelist=csol_img_index[ok]                           ;specif set of image files covered in housekeeping record
;restore, get_surf_dir(/data)+'IDL_structure_files/surf_log_index.sav'

;get the SURF log corresponding to CSOL housekeeping record
;val=min(abs(surf_log_index.start_time - csol_juld[0]),ind)
;restore, surf_log_index[ind].log_file

;surf_aligned = align_surf2img(surf_log , csol_hk)     ; get SURF data for each explicit image in img_filelist

;
; RESTRICTIONS:
;   None
;
; EXAMPLE:
;   see discussion in PROCEDURES/FUNCTIONS CALLED:
;
; MODIFICATION HISTORY: RCS_ID="$Id: align_surf2img.pro,v 1.1 2018/12/30 00:27:42 spenton Exp $"
;   2018-12-20: JWH
; RCS_ID=$Id: align_surf2img.pro,v 1.1 2018/12/30 00:27:42 spenton Exp $
;-

function align_surf2img, surflog,csolhk

  n_hkrec=n_elements(csolhk)

  csol_juld=jd_from_hk(csolhk,n_hkrec)

  surf_aligned=[]         ;final output to be returned

  n_xpix=2000             ;number of pixels in the dispersion direction

  surflog_info=create_struct('date_julian',0.d,$
    'bc',fltarr(n_xpix),$
    'xpos',fltarr(n_xpix),$
    'ypos',fltarr(n_xpix),$
    'pitch',fltarr(n_xpix),$
    'yaw',fltarr(n_xpix),$
    'energy',0.e0,$
    'height',0.e0,$
    'gain',0.e0,$
    'bc_mult',0.e0,$
    'bc_time',0.e0,$
    'fuzzfactor',0.e0,$
    'v20_21_22',0.e0)

  for i=0,n_hkrec-1 do begin

    val=min(abs(surflog.date_julian-csol_juld[i]),ind)

    intg_period=csolhk[i].SETINTGPRD/86400.
    dwell=intg_period/n_xpix
    readout_time=(dwell*findgen(n_xpix))

    inrng=where(surflog.date_julian ge csol_juld[i] and surflog.date_julian le csol_juld[i]+intg_period,cnt)

    case 1 of
      (cnt eq 0): begin         ;case of no SURF data within the length of a CSOL integration
        surflog_info.date_julian = csol_juld[i]
        surflog_info.bc          = replicate(-1.,n_xpix)
        surflog_info.xpos        = replicate(-1.,n_xpix)
        surflog_info.ypos        = replicate(-1.,n_xpix)
        surflog_info.pitch       = replicate(-1.,n_xpix)
        surflog_info.yaw         = replicate(-1.,n_xpix)
        surflog_info.energy      = -1.
        surflog_info.height      = -1.
        surflog_info.gain        = -1.
        surflog_info.bc_mult     = -1.
        surflog_info.bc_time     = -1.
        surflog_info.fuzzfactor  = -1.
        surflog_info.v20_21_22   = -1.

      end
      (cnt eq 1): begin          ;case of only one SURF data point within the length of a CSOL integration
        surflog_info.date_julian = csol_juld[i]
        surflog_info.bc          = replicate(surflog[inrng].bc,n_xpix)
        surflog_info.xpos        = replicate(surflog[inrng].xpos,n_xpix)
        surflog_info.ypos        = replicate(surflog[inrng].ypos,n_xpix)
        surflog_info.pitch       = replicate(surflog[inrng].pitch,n_xpix)
        surflog_info.yaw         = replicate(surflog[inrng].yaw,n_xpix)
        surflog_info.energy      = surflog[inrng].energy
        surflog_info.height      = surflog[inrng].height
        surflog_info.gain        = surflog[inrng].gain
        surflog_info.bc_mult     = surflog[inrng].bc_mult
        surflog_info.bc_time     = surflog[inrng].bc_time
        surflog_info.fuzzfactor  = surflog[inrng].fuzzfactor
        surflog_info.v20_21_22   = surflog[inrng].v20_21_22
      end
      (cnt gt 1): begin          ;case of SURF data spanning the length of a CSOL integration
        surflog_info.date_julian = csol_juld[i]
        linco                    = robust_linefit(surflog[inrng].date_julian,surflog[inrng].bc)
        surflog_info.bc          = poly(csol_juld[i]+readout_time, linco)
        surflog_info.xpos        = interpol(surflog[inrng].xpos,surflog[inrng].date_julian,csol_juld[i]+readout_time)
        surflog_info.ypos        = interpol(surflog[inrng].ypos,surflog[inrng].date_julian,csol_juld[i]+readout_time)
        surflog_info.pitch       = interpol(surflog[inrng].pitch,surflog[inrng].date_julian,csol_juld[i]+readout_time)
        surflog_info.yaw         = interpol(surflog[inrng].yaw,surflog[inrng].date_julian,csol_juld[i]+readout_time)
        surflog_info.energy      = median(surflog[inrng].energy)
        surflog_info.height      = median(surflog[inrng].height)
        surflog_info.gain        = median(surflog[inrng].gain)
        surflog_info.bc_mult     = median(surflog[inrng].bc_mult)
        surflog_info.bc_time     = median(surflog[inrng].bc_time)
        surflog_info.fuzzfactor  = median(surflog[inrng].fuzzfactor)
        surflog_info.v20_21_22   = median(surflog[inrng].v20_21_22)
      end
    endcase

    surf_aligned=[surf_aligned,surflog_info]

  endfor

  return, surf_aligned

end
