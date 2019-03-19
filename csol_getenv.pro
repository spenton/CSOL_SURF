; $Id: csol_getenv.pro,v 1.1 2019/03/19 19:40:15 spenton Exp spenton $
;
; Function to get the environment variables from the system variables
; Making this call is easier then checking if the system variable
; exists every time we try to access it.
;
; Modified to use system variables instead of environment variables
; to make it easier to port on Windows machines.
;
;********************************************************************
pro csol_getenv_read

   ; read the file defined by CSOL_DEF_FILE
   ; If a corresponding environment variable exists, it has precedence
   file = getenv('CSOL_DEF_FILE')
   res=file_search(file,count=count)
   if file eq '' or count eq 0 then begin
      ; not a valid env var -> look in the system variables
      file=''
      defsysv, '!CSOL_DEF_FILE', exists=exists
      if exists eq 1 then begin
         ; get the name of CSOL_DEF_FILE
         res=file_search(!CSOL_DEF_FILE,count=count)
         if count gt 0 then begin
            setenv,'CSOL_DEF_FILE='+!CSOL_DEF_FILE
            file=!CSOL_DEF_FILE
         endif
      endif
      if file eq '' then begin
         ; first check in the current directory for the file csol.def before prompting
         file=file_search('csol.def',count=count)
      endif

      if file eq '' then begin
         ; prompt the user for the CSOL_DEF_FILE (csol.def)
         file=dialog_pickfile(filter="csol.def", title="Select the CSOL Properties file",$
            /must_exist)
         if file eq '' then begin
            ; create and empty environment variable
            setenv, 'CSOL_DEF_FILE='
            defsysv, '!CSOL_DEF_FILE', 'no_file_defined'
            return
         endif
      endif
   endif
   defsysv, '!CSOL_DEF_FILE',strtrim(file[0],2)
   setenv, 'CSOL_DEF_FILE='+!CSOL_DEF_FILE

   ; expand the file to its full path
   file=expand_path(file)
   result=findfile(file, count=count)
   if count eq 0 then return

   ; read the content of CSOL_DEF_FILE
   OPENR, unit, file, ERROR=err, /GET_LUN
   IF err NE 0 THEN BEGIN
      ; opening file failed
      return
   ENDIF ELSE BEGIN
      ; no errors - proceed
      num_lines=200
      list_count=0L
      sysvar=strarr(num_lines)
      value=strarr(num_lines)

      WHILE NOT EOF(unit) DO BEGIN
         one_line=' '
         READF, unit, one_line, FORMAT='(A100)'
         text=strtrim(one_line,2)
         c=strmid(one_line,0,1)
         ; skip over empty and comment lines
         if c ne ';' and c ne '#' and c ne '' then begin
            keyword = STRUPCASE(STRTRIM(GETTOK(text,' '),2))
            ; the following line is there to handle previous format of csol.def
            if keyword ne 'IF' and keyword ne 'ENDIF' then begin
               if keyword eq 'SETENV' then keyword=STRUPCASE(STRTRIM(GETTOK(text,' '),2))
               sysvar[list_count] = keyword
               value[list_count] = STRTRIM(text,2)
               list_count = list_count + 1
            endif
         endif
      ENDWHILE
      FREE_LUN, unit
      sysvar=sysvar[0:list_count-1L]
      value=value[0:list_count-1L]
   ENDELSE

   ; now generate the system variables from the content of the file
   if !version.os_family eq "Windows" then separator="\" else separator="/"
   for i=0,n_elements(sysvar)-1 do begin
      ; first expand the value if it contains "$" from previous format of csol.def
      pos0=strpos(value[i], '$')
      if pos0 ge 0 then begin
         ; get the actual value of the variable
         text=strmid(value[i],pos0+1)
         var=gettok(text,separator)
         ;assume the system variable is already defined
         var = getenv(var)
         if text eq '' then value[i]=var else value[i]=var+separator+text
      endif
      ; expand the file names (in case ~ was used)
      if strpos(value[i],"~") ge 0 then value[i]=expand_path(value[i])
      setenv, strtrim(sysvar[i],2)+'='+strtrim(string(value[i]),2)
   endfor

   ; redefine CSOL_DEF_FILE from the system variable defined above
   ; in case the csol.def file had a bad entry for CSOL_DEF_FILE in it
   setenv, 'CSOL_DEF_FILE='+!CSOL_DEF_FILE


end
;
;********************************************************************
;
function csol_getenv,arg,file=file

   ; make sure arg is a string
   arg = STRING(arg)

   if keyword_set(file) then csol_getenv_read

   result=getenv(arg)
   if result ne '' then return,result

   ; if here than system variable not found
   ; read the csol_def_file in case it has never been read
   defsysv, '!CSOL_DEF_FILE', exists=exists
   if exists eq 0 then begin
      csol_getenv_read
      return, getenv(arg)
   endif else return,''

end
