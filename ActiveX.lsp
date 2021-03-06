;;; 用ActiveX方法生成直线
(defun c:367 (/ myacad mydoc myms p1 p2 myline) 
  (vl-load-com)
  (setq myacad (vlax-get-acad-object)) ;获取AutoCAD应用程序本身
  (setq mydoc (vla-get-ActiveDocument myacad)) ;获取活动文档
  (setq mspace (vla-get-ModelSpace mydoc)) ;获取模型空间
  (setq p1 (getpoint "\n输入直线的起点:"))
  (setq p2 (getpoint p1 "\n输入直线的终点:"))
  ;将普通的三维点转换为ActiveX的变体，再调用填加直线的方法
  (setq myline (vla-addline myms (vlax-3d-point p1) (vlax-3d-point p2)))
  (princ)
)
;;;==================================================*
;;;用ActiveX方法定义将选到的圆改变为指定面积的命令。
(defun c:236 (/ ec area v_c) 
  (vl-load-com)
  (setq ec (car (entsel "\n选择一个圆:"))) ;获取圆的图元名
  (setq area (getreal "\n输入圆的新的面积:")) ;
  (setq v_c (vlax-ename->vla-object ec)) ;将圆的图元名转换为VLA对象
  (vla-put-area v_c area) ;更新圆的面积
  (princ)
)
;;; 定义 Mtext对象
(vl-load-com)
(setq myacad (Vlax-get-acad-object) ;获取AutoCAD应用程序本身
      mspace (vla-get-modelspace (Vla-get-activedocument myacad)) ;获取模型空间
)
;;;==================================================*
;;;以给定的坐标值建立引线
(setq corner (vlax-make-safearray vlax-vbdouble '(0 . 2)))
(vlax-safearray-fill corner '(0 10 0))
(setq width 10)
(setq text "这是多行对象的文字字符串")
;;;建立 Mtext对象
(setq Mtextobj (Vla-addmtext mspace corner width text))
;;;在模型空间中建立 Leader对象
(setq pnts (vlax-make-safearray vlax-vbdouble '(0 . 5)))
(vlax-safearray-fill pnts '(2 2 0 4 4 0))
(setq leadertype aclinewitharrow)
(setq leaderobj (vla-addleader mspace pnts Mtextobj leadertype))
(vla-zoomextents myacad)
(princ)
)
;;;==================================================*
;;; VLA点转换成lisp点
(setq p0 (vlax-variant-value p0)) ;将变体转换为安全数组
(setq p0 (vlax-safearray->list p0)) ;将安全数组转换为点表
;;;==================================================*
;;; 将VLA对象转换成lisp对象名
(setq P1 (cdr (assoc 10 (entget (vlax-vla-object->ename v_l1)))))
;;;==================================================*
;;; 获取VLA对象指定第一个端点(1为每二个)
(setq P1 (vlax-get-property v_l1 'Coordinate 0));格式1
(setq P1 (vla-get-Coordinate v_l1 0));格式2
  ;;;==================================================*
;;; 更新VLA对象指定第一个端点(1为每二个)
(vla-put-Coordinate v_l1 0 (vlax-3d-point P3));格式1
(vlax-put-property v_l1 'Coordinate 0 (vlax-3d-point P3));格式2
(vla-put-endpoint v_l1 (vlax-3d-point p3)) ;更新直线1的终点
;;;==================================================*
;;; 点坐标模式更新VLA对象/(vlax-get-property v_l1 'Coordinate 0)/(vla-get-Coordinate v_l1 0)
(setq points (vlax-variant-value (vla-get-Coordinates v_l1)))
(vlax-safearray-put-element points 3 (car p3))
(vlax-safearray-put-element points 4 (cadr p3))
(vlax-safearray-put-element points 5 (caddr p3))
(vla-put-Coordinates v_l1 points)
;;;==================================================*
;;;点模式更新VLA对象
(vla-put-Coordinate v_l1 1 (vlax-3d-point P3)) ;更新指定点。简单直接
(setq points (append P1 P3))
(vlax-put v_l1 'Coordinates points)
;;;==================================================*
(vlr-added-p vrl);判断反应器是否活动
;;;==================================================* 
;;;vla-sendcommand(精简版)
(vla-sendcommand (vlax-get-property (vlax-get-acad-object) 'activedocument) 
                 (strcat "(load " "\"" FileName "\"" ")")
)
;;;==================================================* 
;;; 关于vla-AddLeader的例子1
(setq myms (vla-get-modelspace 
             (vla-get-ActiveDocument (vlax-get-acad-object))
           )
      P1   (getpoint "\n点1:")
      P2   (getpoint P1 "\n点2:")
)
(setq LDOBJ (vlax-invoke 
              myms
              'addleader
              (setq CI (append P1 P2))

              (setq TXTOBJ (vlax-invoke myms 'addmtext P2 0 ""))
              2
            )
)
(vla-delete TXTOBJ)
;;修改点位置
;;; (vlax-put LDOBJ 'Coordinates CI)
;;添加第三点
;;; (vlax-put LDOBJ 'Coordinates (append CI (getpoint P2 "\n点3:")))
;;;==================================================*
;;; 关于vla-AddLeader的例子2
(setvar 'osmode 0)
(setq pt1   (getpoint "\nFirst point: ")
      pt2   (getpoint pt1 "\nNext point: ")
      space (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object)))
)

(setq ptlist (vlax-make-safearray vlax-vbDouble '(0 . 5)))
(vlax-safearray-fill ptlist (append pt1 pt2))
(setq txtobj (vla-addmtext space (vlax-3d-point pt2) 0 ""))
(setq ldrobj (vla-addleader space ptlist txtobj acLineWithArrow))
;;; (vla-addpolyline space ptlist)
;;; (vla-delete txtobj)
;;;==================================================*
;;; 官方关于vla-AddLeader的例子3
(vl-load-com)
(defun c:257 () 
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))

  (setq points (vlax-make-safearray vlax-vbDouble '(0 . 5)))
  (vlax-safearray-fill points '(1 1 0 4 4 0))
  (setq i 0)

  (setq modelSpace (vla-get-ModelSpace doc))
  (setq oML (vla-AddMLeader modelSpace points i))

  (setq r (vla-AddLeader oML))

  (vlax-safearray-put-element points 4 10)
  (vla-AddLeaderLine oML r points)

  (alert (strcat "LeaderCount = " (itoa (vla-get-LeaderCount oML))))
  (vla-ZoomExtents acadObj)
)
;;;==================================================*
;;; 个人精简关于vla-AddLeader的例子3
(vl-load-com)
(defun c:257 () 
  (setq pt1 (getpoint "\nFirst point: ")
        pt2 (getpoint pt1 "\nNext point: ")
  )
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))

  ;; 定义引导点
  (setq points (vlax-make-safearray vlax-vbDouble '(0 . 5)))
  (vlax-safearray-fill points (append pt1 pt2))

  ;; 将mleader对象添加到模型空间
  (setq modelSpace (vla-get-ModelSpace doc))
  (setq mLeader (vla-AddMLeader modelSpace points 0))
)
;;;==================================================*
;;; 旋转标注
(vl-load-com)
(defun c:232 () 
  (setq pt1 (getpoint "\nFirst point: ")
        pt2 (getpoint pt1 "\nNext point: ")
        pt3 (getpoint "\n文字位置: ")
  )

  ;; 本示例在模型空间中创建旋转尺寸.
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))

  ;;定义尺寸
  (setq rotAngle (/ (* 180 3.141592) 180))

  ;; 在模型空间中创建旋转尺寸
  (setq modelSpace (vla-get-ModelSpace doc))
  (setq dimObj (vla-AddDimRotated modelSpace 
                                  (vlax-3d-point pt1)
                                  (vlax-3d-point pt2)
                                  (vlax-3d-point pt3)
                                  rotAngle
               )
  )
  ;;;  (vla-ZoomAll acadObj)
)
