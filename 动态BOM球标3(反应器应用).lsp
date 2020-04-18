;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;;
;;                                                                               ;;
;;  AUTHOR:  lan-ge, April 2020.   VERSION:   v 1.0                              ;;
;;...............................................................................;;
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;;
(defun c:235 (/ r1 h1 oce p1 p2 p3 t1 v_c v_l1 vrl) 
  (vl-load-com)
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
        t1 (getstring "\npart number: ")
  )


  (COMMAND "_LEADER" p1 p3 "" "" "NONE")
  (setq v_l1 (vlax-ename->vla-object (entlast))) ;箭头线转换为VLA对象
  (command ".text" "m" p2 h1 0 t1)
  (setq v_l2 (vlax-ename->vla-object (entlast))) ;球标号转换为VLA对象
  (command ".circle" p2 r1)
  (setq v_c (list (vlax-ename->vla-object (entlast)))) ;圆的图元名转换为VLA对象
  (setq vrl (vlr-object-reactor v_c (list v_l1 v_l2) '((:vlr-modified . c-2l))))
  ;;;反应器链接到圆上，箭头线的VLA对象和球标号的VLA对象关联表为关联数据，当发生修改该圆的事件时，调用c-2l函数
  (setvar "cmdecho" oce)
  (princ)
)
;定义c-2l回调函数
(defun c-2l (notifier-object reactor-object parameter-list / r1 p2 p3 v_l1 v_l2 points 
             myacad mydoc
            ) 
  (if (vlax-property-available-p notifier-object "radius") 
    (progn 
      (setq p2 (VLA-get-center notifier-object)) ;获取圆的圆心，p2是变体
      (setq r1 (VLA-get-radius notifier-object)) ;获取圆的半径，r1是变体
    )
  )
  ;;;   (setq p2 (vlax-safearray->list (vlax-variant-value p2))) ;将安全数组转换为表
  (setq v_l1 (car (vlr-data reactor-object))) ;箭头线的VLA对象
  (setq v_l2 (cadr (vlr-data reactor-object))) ;球标号的VLA对象
  ;;;   (setq P1 (vlax-safearray->list
  ;;;              (vlax-variant-value
  ;;;                (vlax-get-property v_l1 'Coordinate 0);0表示第一个端点
  ;;;              )
  ;;;            )
  ;;;   ) ;将安全数组转换为表
  ;;;   (setq P1 (cdr (assoc 10 (entget (vlax-vla-object->ename v_l1)))))
  ;;;   (setq a1 (angle p2 p1)
  ;;;         p3 (polar p2 a1 r1)
  ;;;   )
  (setq myacad (vlax-get-acad-object)) ;获取AutoCAD应用程序本身
  (setq mydoc (vla-get-ActiveDocument myacad)) ;获取活动文档
  (setq a1 (vla-AngleFromXAxis (vla-get-Utility mydoc) 
                               p2
                               (vla-get-Coordinate v_l1 0)
           )
  ) ;获取角度
  (setq P3 (vla-PolarPoint (vla-get-Utility mydoc) p2 a1 r1)) ;获取终点
  ;;;   点模式更新VLA对象
  (vla-put-Coordinate v_l1 1 P3) ;更新指定点。简单直接
  (vla-put-TextAlignmentpoint v_l2 p2) ;更新文本对齐点
  ;;;   (setq points (append P1 P3))
  ;;;   (vlax-put v_l1 'Coordinates points)

  ;;;   点坐标模式更新VLA对象/(vlax-get-property v_l1 'Coordinate 0)/(vla-get-Coordinate v_l1 0)
  ;;;   (setq points (vlax-variant-value (vla-get-Coordinates v_l1)))
  ;;;   (vlax-safearray-put-element points 3 (car p3))
  ;;;   (vlax-safearray-put-element points 4 (cadr p3))
  ;;;   (vlax-safearray-put-element points 5 (caddr p3))
  ;;;   (vla-put-Coordinates v_l1 points)

  (vla-Regen mydoc :vlax-true) ;再生文档
  (setq a1 nil)
  ;;;   (vla-put-endpoint v_l1 (vlax-3d-point p3)) ;更新直线1的终点
  ;;;   (if (and v_l1 (not (vlax-object-released-p v_l1))) (vlax-release-object v_l1))
)