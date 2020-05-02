(defun c:253 () 
  (vl-load-com)
  (vl-cmdf "_ucs" "world")
  (prompt "\n��ע���γߴ�")
  (setvar "osmode" 0)
  (setvar "cmdecho" 0)
  (setq pt1 (getpoint "\nPlease pick upper-left corner:"))
  (setq pt2 (getpoint "\nPlease pick lower-right corner:"))
  (vl-cmdf "_layer" "t" "*" "")
  (vl-cmdf "_layer" "s" "dim" "")
  (setq entity (ssget "w" pt1 pt2))
  (vl-cmdf "_explode" entity "")
  (setq entity (ssget "w" pt1 pt2))

  (setq  ;;;mx1����a1 ������pt2
         ;;;   ��         ��
         ;;;    b1 ����ͼ  b1
         ;;;   ��         ��
         ;;;   ��         ��
         ;;;pt1����a1 ������my1
        shulist (minmm_ssbox entity)
        pt1     (car shulist)
        pt2     (cadr shulist)
        px1     (car pt1) ;;;ȡ��pt1�����xֵ
        py1     (cadr pt1) ;;;ȡ��pt1�����yֵ
        px2     (car pt2) ;;;ȡ��pt2�����xֵ
        py2     (cadr pt2) ;;;ȡ��pt2�����yֵ
        mx1     (list px1 py2) ;;;���mx1����ֵ
        my1     (list px2 py1) ;;;���my1����ֵ
        a1      (distance mx1 pt2) ;;;mx1��,pt2��֮��ľ��룺����ľ���ֵ����ͼ(��)
        b1      (distance pt1 mx1) ;;;pt1��,mx1��֮��ľ��룺����ľ���ֵ����ͼ(��)
  )

  (vl-cmdf "_.dimaligned" mx1 pt2 (cadr (grread T 12 1)))
  (setq dimx (vlax-ename->vla-object (entlast)))

  (vl-cmdf "_.dimaligned" mx1 pt1 (cadr (grread T 12 1)))
  (setq dimy (vlax-ename->vla-object (entlast)))

  (setq atx (vlax-safearray->list (vlax-variant-value (vla-get-TextPosition dimx))))
  (setq aty (vlax-safearray->list (vlax-variant-value (vla-get-TextPosition dimy))))


  (setq angx (angle pt2 mx1))
  (setq angm (angle pt2 atx))

  (setq angy (angle mx1 pt1))
  (setq angn (angle mx1 aty))

  (if (> angm angx) 
    (progn 
      (vla-put-ExtLine1Point dimx (vlax-3d-point my1))
      (vla-put-ExtLine2Point dimx (vlax-3d-point pt1))
      (vla-put-TextPosition dimx (vlax-3d-point atx))
    )
  )
  (if (> angn angy) 
    (progn 
      (vla-put-ExtLine1Point dimy (vlax-3d-point pt2))
      (vla-put-ExtLine2Point dimy (vlax-3d-point my1))
      (vla-put-TextPosition dimy (vlax-3d-point aty))
    )
  )

  (vl-cmdf "_layer" "t" "*" "")
  (setvar "cmdecho" 1)
  (setvar "osmode" 703)
)