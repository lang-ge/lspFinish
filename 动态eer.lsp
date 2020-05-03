(defun c:253 (/ p1 p2 entity pt1 pt2 mx1 my1) 
  (vl-load-com)
  (vl-cmdf "_ucs" "world")
  (prompt "\n标注外形尺寸")
  (setvar "osmode" 0)
  (setvar "cmdecho" 0)
  (setq p1 (getpoint "\nPlease pick upper-left corner:"))
  (setq p2 (getpoint "\nPlease pick lower-right corner:"))
  ;;; (vl-cmdf "_layer" "t" "*" "")
  (vl-cmdf "_layer" "s" "dim" "")
  (setq entity (ssget "w" p1 p2))
  (vl-cmdf "_explode" entity "")
  (setq entity (ssget "w" p1 p2))

  (setq  ;;;mx1┏━a1 ━━┓pt2
         ;;;   ┃         ┃
         ;;;    b1 主视图  b1
         ;;;   ┃         ┃
         ;;;   ┃         ┃
         ;;;pt1┗━a1 ━━┛my1
        shulist (minmm_ssbox entity)
        pt1     (car shulist)
        pt2     (cadr shulist)
        mx1     (list (car pt1) (cadr pt2)) ;;;求得mx1坐标值
        my1     (list (car pt2) (cadr pt1)) ;;;求得my1坐标值
  )

  (if (< (angle pt2 p2) (angle pt2 mx1)) 
    (vl-cmdf "_dimlinear" mx1 pt2 p2)
    (vl-cmdf "_dimlinear" pt1 my1 p2)
  )

  (if (and (< (angle mx1 p2) (angle mx1 pt1)) (> (angle mx1 p2) (* pi 0.5))) 
    (vl-cmdf "_dimlinear" mx1 pt1 p2)
    (vl-cmdf "_dimlinear" pt2 my1 p2)
  )
  (vl-cmdf "_layer" "t" "*" "")
)

