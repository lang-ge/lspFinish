;;; 工艺孔自动生成程序
(defun c:fc (/ r r1 r2 m ss ss1 ent1 ent2 obj p_s1 p_E1 p_s2 p_E2 mpt pt1 pt2 pt3 pt4 pt5)
  (vl-load-com)
  (setvar "cmdecho" 0)			;关闭回显
  (setvar "osmode" 0)			;关闭对象捕捉
  (setq r1 (getvar "filletrad"))
  (prompt "\n此程序用于生成折弯工艺孔!无输入可两线会合!")
  (SETQ r (getreal (strcat "\n输入孔半径或[材料厚度]:(" (rtos r1 2 1) "):")))
  (if (null r) (setq r r1))
  (prompt "\n请选择一个角!")
;;; (if (null (type c:cal))
;;;   (arxload "geomcal.arx")
;;; )
  (setq m 0)
  (while (setq ss (ssget '((0 . "*LINE,ARC,XLINE"))))
    (setq ent1 (ssname ss 0)
	  obj  (vlax-ename->vla-object ent1)
	  p_s1 (vlax-curve-getPointAtParam obj (* (vlax-curve-getEndParam obj) 0.5));曲线中点座标
	  p_E1 (vlax-curve-GetEndPoint obj)
	  ent2 (ssname ss 1)
	  obj  (vlax-ename->vla-object ent2)
	  p_s2 (vlax-curve-getPointAtParam obj (* (vlax-curve-getEndParam obj) 0.5));曲线中点座标
	  p_E2 (vlax-curve-GetEndPoint obj)
    )
;;; (setq p_s1 (c:cal "(p_s1+p_E1)/2"))
;;; (setq p_s2 (c:cal "(p_s2+p_E2)/2"))
    (setq mpt (inters p_s1 p_E1 p_s2 p_E2 nil))
    (setq pt1 (GEO:1 0.0 mpt p_s1 p_s2 (- r 0.1)))
    (setq pt2 (GEO:1 1.0 mpt p_s1 p_s2 (- r 0.1)))
    (setq r2 (* r 2))
    (setq pt3 (GEO:1 0.5 mpt p_s1 p_s2 r2))
    (setq pt4 (polar mpt (* pi 0.25) r2))
    (setq pt5 (polar mpt (* pi 1.25) r2))
;;;    (SETQ r1 (* 5 r))
;;;    (setq pt1 (c:cal "pld(mpt,p_s1,r1)"))
;;;    (setq pt2 (c:cal "pld(mpt,p_s2,r1)"))
;;;    (setq pt3 (c:cal "(pt1+pt2)/2"))  
;;;    (SETQ r1 (- r 0.1))
;;;    (setq pt1 (c:cal "pld(mpt,p_s1,r1)"))
;;;    (setq pt2 (c:cal "pld(mpt,p_s2,r1)"))旧算法做废！！！
    (setvar "FILLETRAD" 0)
    (command "_fillet" ent1 ent2)
    (if (setq ss1 (ssget "_W" pt5 pt4))
	       (command "eRase" ss1 "" "")
	     )
    (if	(/= r 0.0)
      (progn (command "circle" mpt r)      
	           (command "_trim" "" "f" mpt pt3 pt1 pt2 "" "")
      ;;; (setvar "cmdecho" 1)
      ;;; (command "_trim" "" "f" mpt pt3 pt1 pt2 "" "" "eRase" (SSGET) "" "")旧功能
      )
    )
    (setq m (1+ m))
  )
  (setvar "filletrad" r)
  (prompt (strcat "\n已选择" (itoa m) "个"))
  (prompt "\n★请选择下一个角！")
)

;;;功能: 定比点P,使得在三点构角平分线上                 ;
(defun GEO:1 (k po p1 p2 dist / ag1 ag2)
  (SETQ ag1 (angle po p1))
  (SETQ ag2 (angle po p2))
  (SETQ ag (- ag2 ag1))
  (if (and (> ag pi) (/= K 0.0) (/= K 1.0))
    (polar po (+ pi ag1 (* ag K)) dist)
    (polar po (+ ag1 (* ag K)) dist)
  )
)