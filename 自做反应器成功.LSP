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
;;;==================================================*其它实例
;;;经典的反应器例子
(vl-load-com)
(defun c:nm (/ r1 h1 oce p2 p3 t1 v_c v_l1 vrl) 
  (setq r1 (* 4 (GETVAR "DIMSCALE"))
        h1 (* 2.5 (GETVAR "DIMSCALE"))
  )
  (graphscr)
  (setq oce (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (setvar "osmode" 0)
  (setvar "CLAYER" "mark")
  (setq p1 (getpoint "\nfrom point: ")
        p2 (getpoint p1 "\nbubber center point: ")
        a1 (angle p2 p1)
        p3 (polar p2 a1 r1)
  )
  ;;; (COMMAND "_LEADER" p1 p3 "" "" "NONE")
  (command "line" p1 p3 "")
  (setq v_l1 (vlax-ename->vla-object (entlast))) ;第一条直线转换为VLA对象
  (setq t1 (getstring "\npart number: "))
  (command ".text" "m" p2 h1 0 t1)
  (command ".circle" p2 r1)
  (setq v_c (list (vlax-ename->vla-object (entlast)))) ;圆的图元名转换为VLA对象
  (setq vrl (vlr-pers 
              (vlr-object-reactor v_c (list v_l1) '((:vlr-modified . c-2l)))
            )
  )
  ;;;反应器链接到圆上，直线的VLA对象表为关联数据，当发生修改该圆的事件时，调用c-2l函数
  (setvar "cmdecho" oce)
  (princ)
)
;定义c-2l回调函数
(defun c-2l (notifier-object reactor-object parameter-list / r1 a1 p2 p3 v_l1) 
  (setq p2 (VLA-get-center notifier-object)) ;获取圆的圆心，p2是变体
  (setq r1 (VLA-get-radius notifier-object)) ;获取圆的半径，r1是变体
  (setq p2 (vlax-safearray->list (vlax-variant-value p2))) ;将安全数组转换为表
  (setq a1 (angle p2 p1)
        p3 (polar p2 a1 r1)
  )

  (setq v_l1 (car (vlr-data reactor-object))) ;第一条直线的VLA对象
  (vla-put-endpoint v_l1 (vlax-3d-point p3)) ;更新直线1的终点
)
;;; 本程序中，与反应器相关联的数据是直线的VLA对象。