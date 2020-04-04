;;命令反应器
  (or *cmdvlr*
      (setq *cmdvlr* (vlr-command-reactor nil '((:vlr-commandWillStart . cmdstart) (:VLR-commandEnded . cmdEnd))))
      )
  ;;数据库反应器
  (or *acdbvlr*
      (setq *acdbvlr* (vlr-acdb-reactor nil
       '((::VLR-objectModified . acdbstart))))
      )
;;命令开始反应器回调函数
(defun cmdstart (vlr lst)
  (if (= "GRIP_STRETCH" (car lst))
    (setq *GRIP_STRETCH_Start* t ;_ 设置启动夹点拖拽命令标志
          *GRIP_STRETCH_Obj* nil ;_ 清空夹点拖拽的对象图元名列表
          )
    )
  )
;;命令结束反应器回调函数
(defun cmdEnd (vlr lst )
  (if (= "GRIP_STRETCH" (car lst))
    (progn
      (setq *GRIP_STRETCH_Start* nil) ;_ 清除启动夹点拖拽命令标志
      ;;此处可自行添加对 *GRIP_STRETCH_Obj* 处理的代码,如将夹点拖拽物体颜色变红
     (mapcar '(lambda (x) (vla-put-color (vlax-ename->vla-object x) 1)) *GRIP_STRETCH_Obj*)
    )
  )
)
;;数据库反应器回调函数
(defun acdbstart (vlr obj)
  (if *GRIP_STRETCH_Start*
    (setq *GRIP_STRETCH_Obj*
           (append *GRIP_STRETCH_Obj* (cdr obj)) ;_ 夹点拖拽的对象图元名列表
    )
  )
)