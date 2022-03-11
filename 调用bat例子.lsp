(defun TT (/ a ff file2 line) 
  (setvar "CMDECHO" 0)
  (setq file2 (vl-filename-mktemp "time.txt"))
  (setq a (strcat 
            "@echo off&&cd /d C:\\&&dir /s /b dynwrapx.bat>"
            file2
            "&&exit"
          )
  )
  (command "SHELL" a)
  (command "DELAY" 32767 "DELAY" 32767)
  (setq ff (open file2 "r"))
  (setq line (read-line ff))
  (close ff)
  (vl-file-delete file2)
  line
)
(vlax-invoke-method (Vlax-Get-Or-Create-Object "Shell.Application") 
                    'Open
                    (vl-filename-directory (TT))
)
(princ)
;;另类写法1
(setq a (strcat 
            "@echo off"
            "&for /r C:\\ %i in (dynwrapx.dll) do if exist %i echo %i>D:\\61.txt"
            "&exit"
          )
  )
;;另类写法2(剪贴板clip版)
(setq a (strcat 
            "@echo off&&cd /d C:\\&&dir /s /b dynwrapx.dll|clip"
            "&&exit"
          )
  )
