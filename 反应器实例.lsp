;;;生成一个圆的VLA对象。
(vl-load-com)
(setq myCircle
       (progn
	 (setq ctrPt  (getpoint "\n输入圆心: ")
	       radius (distance ctrPt (getpoint ctrpt "\n输入半径 : "))
	 )
	 (vla-addCircle
	   (vla-get-ModelSpace		;将圆加入到图形模型空间
	     (vla-get-ActiveDocument (vlax-get-acad-object))
	   )
	   (vlax-3d-point ctrPt)
	   radius
	 )
       )
)

;;;该函数用来打印圆的半径
(defun print-radius (notifier-object reactor-object parameter-list)
  (vl-load-com)
  (cond
    ((vlax-property-available-p notifier-object "Radius")
     (alert
       (strcat "这个圆的半径是:" (rtos (vla-get-radius notifier-object) 2))
     )

    )
  )
)

:vlr-docframemovedorresized
;;;mycircle对象反应器
(setq circleReactor
       (vlr-object-reactor
	 (list myCircle)
	 "Circle Reactor"
	 '((:vlr-modified . print-radius))
       )
)

;;;==================================================*
;;;定义将对象反应器链接到指定直线的命令。
(vl-load-com)
(defun c:258 (/ el rlt vrl)
  (setq el (car (entsel "\n选择一条链接反应器的直线:")))
  (setq rlt (list (vlax-ename->vla-object el))) ;将图元名转换为VLA对象

;;;创建对象反应器，回调事件是图元被修改，回调函数是show-l
  (setq	vrl (vlr-pers
	      (vlr-object-reactor rlt nil '((:vlr-modified . show-l)))
	    )
  )
  (princ)
)

;;;定义回调函数
(defun show-l (notifier-object reactor-object parameter-list / l)
  (setq l (vla-get-length notifier-object)) ;将被选直线的长度赋给变量l
  (setq l (rtos l 2 4))			;将数值转换为字符串
  (alert (strcat "直线的长度是：" l))	;调用信息对话框
)

;;;==================================================*
;;;定义将对象反应器链接到圆上的命令，如果圆被修改，两条直线与圆的相对位置和相对比例不变。
(vl-load-com)
(defun c:257( / p0 p1 p2 p3 p4 r r1 eh1 eh2 l1-l2 rlt vrl)
  (setq p0(getpoint "\n输入圆心："))
  (setq r(getdist p0 "\n输入半径："))
  (command "circle" p0 r)
  (setq r1(* 1.25 r))
  (setq ec(entlast))
  (setq p1(polar p0 0 r1))
  (setq p2(polar p0 (* 0.5 pi) r1))
  (setq p3(polar p0 pi r1))
  (setq p4(polar p0 (* -0.5 pi) r1))
  (command "line" p1 p3 "")
  (setq eh1(cdr (assoc 5 (entget (entlast)))));第一条直线的句柄
  (command "line" p2 p4 "")
  (setq eh2(cdr (assoc 5 (entget(entlast)))));第二条直线的句柄
  (setq l1-l2(list eh1 eh2));两条直线的句柄表
  (setq rlt(list (vlax-ename->vla-object ec)));圆的图元名转换为VLA对象
  (setq vrl (vlr-pers(vlr-object-reactor rlt  l1-l2 '((:vlr-modified . c-2l)))));反应器链接到圆上，两条直线的句柄表为关联数据，当发生修改该圆的事件时，调用c-2l函数
  (princ);静默退出
)

;定义c-2l函数
(defun c-2l(notifier-object reactor-object parameter-list / ec ec_l el1 el2 ell_1 ell_2 p0 p1 p2 p3 p4 p0x p0y p0z)
  (setq ec(vlax-vla-object->ename notifier-object);VLA对象的圆转换为图元名
     ec_l(entget ec);圆的图元表
     p0(cdr(assoc 10 ec_l));获取圆心的坐标
     r(* 1.25 (cdr(assoc 40 ec_l)));获取圆的半径之后×1.25
  )
  (setq el1(handent (car (vlr-data reactor-object))));第一条直线的图元名
  (setq el2(handent (cadr (vlr-data reactor-object))));第二直条线的图元名
(setq p0x (car p0));获取圆心的X坐标
 (setq p0y (cadr p0));获取圆心的Y坐标
 (setq p0z (caddr p0));获取圆心的Z坐标
 (setq p1 (list (+ p0x r) p0y p0z));修改直线端点的坐标
 (setq p2 (list p0x (+ p0y r) p0z));修改直线端点的坐标
 (setq p3 (list (- p0x r) p0y p0z));修改直线端点的坐标
 (setq p4 (list p0x (- p0y r) p0z));修改直线端点的坐标
 (setq ell_1(entget el1));第一条直线的图元表
 (setq ell_1(subst (vl-list* 10 p1)(assoc 10 ell_1)ell_1));直线的新端点替换直线的老端点
 (setq ell_1(subst (vl-list* 11 p3)(assoc 11 ell_1)ell_1));直线的新端点替换直线的老端点
(entmod ell_1);更新第一条直线
  (setq ell_2(entget el2));第二条直线的图元表
  (setq ell_2(subst (vl-list* 10 p2)(assoc 10 ell_2)ell_2));直线的新端点替换直线的老端点
  (setq ell_2(subst (vl-list* 11 p4)(assoc 11 ell_2)ell_2));直线的新端点替换直线的老端点
  (entmod ell_2);更新第二条直线
)






