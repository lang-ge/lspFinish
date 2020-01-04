(defun Example_rotation	()
  (VL-LOAD-COM)
  (setq	Acadobject (Vlax-get-acad-object)
	mspace	   (vla-get-modelspace (Vla-get-activedocument Acadobject))
  )


;;;  (setq textstring "��,��á�")
  
  (setq textstring (ssget))
  (setq insertionpnt (vlax-make-safearray vlax-vbdouble '(0 . 2)))
  (vlax-safearray-fill insertionpnt '(30 30 0))
  (setq alignmentpnt (vlax-make-safearray vlax-vbdouble '(0 . 2)))
  (vlax-safearray-fill alignmentpnt '(30 30 0))
  (setq height 5)
  ;;��ģ�Ϳռ��н������ֶ���
  (setq	Textobj
	(vla-addtext mspace textstring insertionpnt height)
  )
  (vla-zoomextents Acadobject)
  (princ (STRCAT "Rotation Ϊ��"
		 (rtos (vla-get-rotation Textobj))
		 "Rotation����\n"
	 )
  )
  (prompt "<Enter>����:" )
  (vl-cmdf pause)
  ;;�� Rotationֵ�ı�Ϊ45��(.707����
  (vla-put-rotation Textobj 0.707)
  (vla-zoomextents Acadobject)
  (princ (STRCAT " Rotation�趨Ϊ:"
		 (RTOS (vla-get-Rotation Textobj))
		 "Rotation����\n"
	 )
  )
  (princ)
)