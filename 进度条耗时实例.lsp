(vl-load-com)
(defun c:357 (/ a1 b1 layer_old osmode_old po pt1 pt2 pt3 ss enn zyd zd) 
  (setvar "cmdecho" 0)
  (setq osmode_old (getvar "OSMODE")) ;;��׽����
  (setq layer_old (getvar "CLAYER")) ;;ȡ��ǰͼ��
  (setvar "osmode" 0) ;;�رն���׽
  (princ "\\n��ѡ�����...")
  (if (setq SS (ssget)) 
    (progn 
      ;;;��ʼ��
      (setq stime (getvar "date")
            I     0
      )
      ;;;������������ȡ
      (repeat (setq nm (sslength SS)) 
        (cs_pross nm I)
        (setq OBJ (vlax-ename->vla-object (ssname SS I)))
        (setq p2 (vlax-safearray->list 
                   (vlax-variant-value (VLA-get-center OBJ))
                 )
        )
        (SETQ SC (RTOS (car p2) 2 0))
        (alert SC)
        (setq I (1+ I))
      )
      (setq etime (getvar "date"))
      (setq Htime (* 86400.0 (- (- etime stime) (fix (- etime stime)))))
      (grtext -2 (strcat "������ɣ���ʱ" (rtos Htime 2 2) "��..."))
    )
  )

  ;;;ϵͳ������ԭ
  (setvar "OSMODE" osmode_old)
  (setvar "CLAYER" layer_old)
  (princ)
)

(defun cs_pross (to I / CS_TEXT MYI) 
  (setq cs_text ">>>>>>>>>>>>>>>>>>>>>>>>>>>>")
  (setq myi     (fix (/ (* (strlen cs_text) I) to))
        cs_text (substr cs_text 1 myi)
  )
  (grtext -2 cs_text)
)

