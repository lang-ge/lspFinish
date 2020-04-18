;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;;
;;                                                                               ;;
;;  AUTHOR:  lan-ge, April 2020.   VERSION:   v 1.0                              ;;
;;...............................................................................;;
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;;
(defun c:nm (/ r1 h1 oce p1 p2 p3 t1 v_c v_le v_l2 loop1 loop2 source Points vrl kw 
             ArroType *ModelSpace*
            ) 
  (vl-load-com)
  (setq r1    (* 4 (GETVAR "DIMSCALE"))
        h1    (* 2.5 (GETVAR "DIMSCALE"))
        loop1 T
  )
  (graphscr)
  (setq oce (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (setvar "osmode" 0)
  (setvar "CLAYER" "mark")

  ;;; 箭头选项
  (if (not #def) (setq #def "D默认"))
  (initget "D默认 S小点 O'90° B方块")
  (setq kw (getkword 
             (acet-str-format "Enter an option [D默认/S小点/O'90°/B方块] <%1>: " #def)
           )
  )
  (if (or (not kw) (equal kw "")) 
    (setq kw #def)
    (setq #def kw)
  )
  (cond 
    ((= kw "D默认")
     (setq ArroType acArrowDefault)
    )
    ((= kw "S小点")
     (setq ArroType acArrowDotSmall)
    )
    ((= kw "O'90°")
     (setq ArroType acArrowOpen90)
    )
    ((= kw "B方块")
     (setq ArroType acArrowBoxBlank)
    )
  )
  ;;; 结束

  (while loop1 
    (if 
      (setq t1 (getstring "\npart number: ")
            p1 (getpoint "\nfrom point: ")
      )
      (progn 
        (command ".text" "m" p1 h1 0 t1)
        (setq v_l2 (vlax-ename->vla-object (entlast))) ;球标号转换为VLA对象
        (setq *ModelSpace* (vla-get-ModelSpace (vla-get-activedocument (vlax-get-acad-object ) ) ))
        (setq v_c (vla-addcircle *ModelSpace* (vlax-3d-point p1) r1))
        (setq loop2 T
              v_le  (draw_leader p1 (polar p1 0 0.001))
        )
        (prompt "\n确定序号放置位置:")
        (while loop2 
          (setq p2     (grread T)
                source (car p2)
                p2     (cadr p2)
          )
          (cond 
            ((= source 5)
             (vla-put-TextAlignmentpoint v_l2 (vlax-3d-point p2))
             (vla-put-center v_c (vlax-3d-point p2))
             (setq Points (vlax-safearray-fill 
                            (vlax-make-safearray 
                              vlax-vbdouble
                              '(0 . 5)
                            )
                            (append p1 
                                    (polar p2 (angle p2 p1) r1)
                            )
                          )
             )
             (if (> (distance p2 p1) r1) 
               (vla-put-coordinates v_le Points)
             )
            )
            (t
             (progn 
               (setq loop2 nil)
               (vlax-ldata-put v_c "radius" r1)
               (function c-2l)
               (setq vrl (vlr-object-reactor (list v_c) 
                                             (list v_le v_l2)
                                             '((:vlr-modified . c-2l))
                         )
               )
             )
            )
          )
        )
      )
      (setq loop1 nil)
    )
  )
  ;;;反应器链接到圆上，箭头线的VLA对象和球标号的VLA对象关联表为关联数据，当发生修改该圆的事件时，调用c-2l函数
  (setvar "cmdecho" oce)
  (princ)
)
;定义c-2l回调函数
(defun c-2l (notifier-object reactor-object parameter-list / r1 p2 p3 v_le points 
             *acdocument*
            ) 
  (if (vlax-property-available-p notifier-object "radius") 
    (progn 
      (setq p2 (VLA-get-center notifier-object)) ;获取圆的圆心，p2是变体
      (setq r1 (VLA-get-radius notifier-object)) ;获取圆的半径，r1是变体
    )
  )
  ;;;   (setq p2 (vlax-safearray->list (vlax-variant-value p2))) ;将安全数组转换为表
  (setq v_le (car (vlr-data reactor-object))) ;箭头线的VLA对象
  (setq v_l2 (cadr (vlr-data reactor-object))) ;球标号的VLA对象
  (setq *acdocument* (vla-get-activedocument (vlax-get-acad-object)))
  (setq a1 (vla-AngleFromXAxis (vla-get-Utility *acdocument*) 
                               p2
                               (vla-get-Coordinate v_le 0)
           )
  ) ;获取角度
  (setq P3 (vla-PolarPoint (vla-get-Utility *acdocument*) p2 a1 r1)) ;获取终点
  ;;;   点模式更新VLA对象
  (vla-put-Coordinate v_le 1 P3) ;更新指定点。简单直接
  (vla-put-TextAlignmentpoint v_l2 p2) ;更新文本对齐点
  (vla-Regen *acdocument* :vlax-true) ;再生文档
  (setq a1 nil)
)
;;; 绘制箭头线
(defun draw_leader (pt1 pt2 / v_le)  ;draw leader
  (command "leader" pt1 pt2 "f" "st" "f" "a" "" "" "n")
  (setq v_le (vlax-ename->vla-object (entlast)))
  (vla-put-ArrowheadType v_le ArroType)
  (vla-Update v_le)
  v_le
)