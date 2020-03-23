(defun c:235 (/ myacad mydoc myms p1 p2) 
  (vl-load-com)
  (setq myacad (vlax-get-acad-object)) ;获取AutoCAD应用程序本身
  (setq mydoc (vla-get-ActiveDocument myacad)) ;获取活动文档
  (setq myms (vla-get-ModelSpace mydoc)) ;获取模型空间
  (setq p1 (getpoint "\n输入直线的起点:"))
  (setq ec (car (entsel "\n选择一个圆:"))) ;获取圆的图元名
  (setq v_c (vlax-ename->vla-object ec)) ;将圆的图元名转换为VLA对象
  (setq p3 (vla-get-Center v_c))
  ;;;  (setq p2 (getpoint p1 "\n输入直线的终点:"))
  (setq myline (vla-addline myms (vlax-3d-point p1) p3))
  ;;;创建对象反应器，回调事件是图元被移动，回调函数是c-2l
  (setq rlt (list v_c)) ;圆的图元名转换为VLA对象
  (setq vrl (vlr-pers 
              (vlr-object-reactor rlt p3 '((:vlr-modified . c-2l)))
            )
  ) ;反应器链接到圆上，当发生修改该圆的事件时，调用c-2l函数
  (princ)
)

;;;定义回调函数
(defun c-2l (notifier-object reactor-object parameter-list) 
  (vl-load-com)
  (cond 
    ((vlax-property-available-p notifier-object "Center")
     (progn 
       (setq p3 (vla-get-Center v_c))
       (vla-put-EndPoint myline p3)
     )
    )
  )
)