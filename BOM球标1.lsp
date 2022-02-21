(defun c:267 (/ r1 h1 sca loop1 oce p1 loop1 loop2 vla-blkref att_lst p2 source t1 
              v_le Points obj-lst kw ArroType
             ) 
  (vl-load-com)
  (setq r1    (* 4 (GETVAR "DIMSCALE"))
        h1    (* 2.5 (GETVAR "DIMSCALE"))
        sca   1
        loop1 T
  )
  (graphscr)
  (setq oce (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (setvar "osmode" 0)
  (setvar "CLAYER" "mark")

  ;;; 箭头选项
<<<<<<< HEAD
  (if (not #def) (setq #def "D默认"))
=======
;   (if (not #def) (setq #def "D默认"))
>>>>>>> ec99df1 (done)
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
  (if (not (check_num_block)) 
    (Creat_Num_block h1 r1)
  )
  (while loop1 
    (if 
      (setq t1 (getstring "\npart number: ")
            p1 (getpoint "\nfrom point: ")
      )
      (progn 
        (setq vla-blkref (vla-InsertBlock 
                           *ModelSpace*
                           (vlax-3d-point p1)
                           "num"
                           sca
                           sca
                           sca
                           0
                         )
              att_lst    (list (cons ' "nums" t1))
              loop2      T
        )
        (SetAtts vla-blkref att_lst)
        (setq v_le (draw_leader p1 (polar p1 0 0.001)))

        (prompt "\n确定序号放置位置:")

        (while loop2 
          (setq p2     (grread T)
                source (car p2)
                p2     (cadr p2)
          ) ;_ end setq
          (cond 
            ((= source 5)
             (vla-put-InsertionPoint vla-blkref (vlax-3d-point p2))
             (setq Points (vlax-safearray-fill 
                            (vlax-make-safearray 
                              vlax-vbdouble
                              '(0 . 5)
                            )
                            (append p1 
                                    (polar p2 (angle p2 p1) (* r1 sca))
                            )
                          )
             )

             (if (> (distance p2 p1) (* r1 sca)) 
               (vla-put-coordinates v_le Points)
             )
            )
            (t
             (progn 
               (setq loop2   nil
                     obj-lst (list vla-blkref v_le)
               )
               (vlax-ldata-put vla-blkref "radius" r1)
               (function c-2l)
               (setq reactor (VLR-Object-reactor 
                               obj-lst
                               obj-lst
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
  (setvar "cmdecho" oce)
  (princ)
)

;定义c-2l回调函数
(defun c-2l (notifier reactor arg-list) 
  (update-leader notifier (vlr-data reactor))
)

(defun update-leader (notifier obj-list / vla-blk_bef vla-leader) 
  (setq vla-blk_bef (nth 0 obj-list)
        vla-leader  (nth 1 obj-list)
  )
  (if (set-params-obj vla-blk_bef vla-leader) 
    (update-parameter-mdim vla-leader "Coordinates" Coordinates)
  )
)

(vl-load-com)
(setq *ActiveDocument* (vla-get-ActiveDocument 
                         (vlax-get-acad-object)
                       )
)
(setq *ModelSpace* (vla-get-ModelSpace 
                     *ActiveDocument*
                   )
)
(setq *AcadBlock* (vla-get-blocks 
                    *ActiveDocument*
                  )
)

;;;创建带有属性的数字块
(defun Creat_Num_block (h1 r1 / cirPnt blockObj circleObj attributeObj) 
  (setq cirPnt (list r1 r1 0.0))
  (setq blockObj     (vla-add *AcadBlock* 
                              (vlax-3d-point cirPnt)
                              "num"
                     )
        circleObj    (vla-AddCircle 
                       blockObj
                       (vlax-3d-point cirPnt)
                       r1
                     )
        attributeObj (vla-AddAttribute 
                       blockObj
                       h1
                       acAttributeModeVerify
                       ""
                       (vlax-3d-point cirPnt)
                       "nums"
                       ""
                     )
  )
  (vla-put-Alignment attributeObj acAlignmentMiddle)
  (if (> (setq v (distof (substr (getvar "acadver") 1 3))) 16.2) 
    (vla-put-TextAlignmentPoint 
      attributeObj
      (vlax-3d-point cirPnt)
    )
  )
  (vla-Update attributeObj)
)
;;; 绘制箭头线
(defun draw_leader (pt1 pt2 / v_le)  ;draw leader
  (command "leader" pt1 pt2 "f" "st" "f" "a" "" "" "n")
  (setq v_le (vlax-ename->vla-object (entlast)))
  (vla-put-ArrowheadType v_le ArroType)
  (vla-Update v_le)
  v_le
)

;;;设置属性
(defun SetAtts (Obj Lst / AttVal) 
  (mapcar 
    '(lambda (Att) 
       (if (setq AttVal (cdr (assoc (vla-get-TagString Att) Lst))) 
         (vla-put-TextString Att AttVal)
       )
     )
    (vlax-invoke Obj "GetAttributes")
  )
  (vla-update Obj)
  (princ)
)
;;; 负责更新箭头
(defun update-parameter-mdim (vla-obj par-name par-value) 
  (if 
    (and (= (type vla-obj) 'VLA-OBJECT) 
         (vlax-write-enabled-p vla-obj)
         (not (equal (vlax-get vla-obj par-name) par-value))
    )
    (vlax-put vla-obj par-name par-value)
  )
)

;;; 关联形参更新对象
(defun set-params-obj (vla-blk_bef v_le / ins-pnt lead-pnt1 lead-pnt2 sca rad) 
  (if 
    (and (= (type vla-blk_bef) 'VLA-OBJECT) 
         (vlax-read-enabled-p vla-blk_bef)
    )
    (progn 
      (setq ins-pnt (vla-get-InsertionPoint vla-blk_bef))
      (if (eq (type ins-pnt) 'VARIANT) 
        (if (> (vlax-variant-type ins-pnt) 8192) 
          (setq ins-pnt (vlax-safearray->list (vlax-variant-value ins-pnt)))
        )
      )
      (setq sca         (vla-get-XScaleFactor vla-blk_bef)
            rad         (vlax-ldata-get vla-blk_bef "radius")
            lead-pnt1   (vlax-curve-getstartpoint v_le)
            lead-pnt2   (polar ins-pnt (angle ins-pnt lead-pnt1) (* sca rad))
            Coordinates (append lead-pnt1 lead-pnt2)
      )
    )
  )
)
;;;检查是否存在“ num”块
(defun check_num_block (/) 
  (if 
    (ssget "X" 
           '((0 . "INSERT")
             (2 . "num")
            )
    )
    T
    nil
  )
)