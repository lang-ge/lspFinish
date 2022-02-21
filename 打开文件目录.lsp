; 现在已经实现这两个功能，但是如果两者能柔和一下就好了！
;打开文件目录并定位文件
(startapp "explorer" "/select,C:\\Windows\\notepad.exe")

;打开本文件的路径的时候如果已经打开过了，就弹出，如果没有，就打开！
(command "shell" "start d:\\")

; 想要的效果就像这样：不管你点多少次文件夹，始终只打开一个目录，并定位到文件



;;;这个基本可以实现
(defun test ( ext / shell)
  (vl-load-com)
  (setq shell (vlax-create-object "shell.application"))
  (vlax-invoke shell 'open ext)
  (vlax-release-object shell)
)

;;(test "C:\\Windows")

(SETQ path (strcat (GETVAR "DWGPREFIX") "pdf" "\\"))
(startapp "explorer" "/select,E:\\郑工\\昂纳科技\\DEV\\20200529\\CQJ001-004-01A 机架总图.dwg")

(if path (startapp "explorer" (strcat "/open," path)))