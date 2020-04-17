;;; World类型库初始化!
(defun jinn-get-word-Lib () 
  (setq office:dir "C:\\Program Files (x86)\\Microsoft Office\\")
  (cond 
    ((setq mwlib (findfile (strcat office:dir "office11\\" "MSWORD.olb"))))
    ((setq mwlib (findfile (strcat office:dir "office12\\" "MSWORD.olb"))))
    (t (setq mwlib nil))
  )
  (if mwlib 
    (vlax-import-type-library :tlb-filename mwlib :methods-prefix "wd-" 
                              :properties-prefix "wp-" :constants-prefix "wm-"
    )
    (alert "Word typelib 文件不存在")
  )
)
;;; excel类型库初始化!
(if (equal exc-xlScalelinear nil)
  (vlax-import-type-library
    :tlb-filename
    "C:/Program Files (x86)/Microsoft Office/Office12/Excel.exe"
    :methods-prefix
    "exm-"
    :properties-prefix
    "exp-"
    :constants-prefix
    "exe-"
   )
)

;;; 与word建立联接
(setq myword (vlax-get-object "word.application.12"))

;;; 与excel建立联接
(setq myexcel (vlax-get-object "excel.application.12"))

;;; 先检测后建立word新实例并打开
(setq myword (vlax-get-or-create-object "word.application.12"))
(vla-put-Visible myword :vlax-true)

;;; 直接建立word新实例
(setq myword (vlax-create-object "word.application.12"))

vlax-read-enabled-p ;判断对象是否可以被存取
vlax-write-enabled-p ;判断对象是否可以被更改
vlax-erased-p ;判断对象是否已经被删除
(vla-put-color vla-circle acred);类型参数常量"acred"