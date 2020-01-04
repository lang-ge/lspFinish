(defun Example_rotation	()
  (VL-LOAD-COM)
  (setq	Acadobject (Vlax-get-acad-object)
	mspace	   (vla-get-modelspace (Vla-get-activedocument Acadobject))
  )


;;;  (setq textstring "嗨,你好。")
  
  (setq textstring (ssget))
  (setq insertionpnt (vlax-make-safearray vlax-vbdouble '(0 . 2)))
  (vlax-safearray-fill insertionpnt '(30 30 0))
  (setq alignmentpnt (vlax-make-safearray vlax-vbdouble '(0 . 2)))
  (vlax-safearray-fill alignmentpnt '(30 30 0))
  (setq height 5)
  ;;在模型空间中建立文字对象
  (setq	Textobj
	(vla-addtext mspace textstring insertionpnt height)
  )
  (vla-zoomextents Acadobject)
  (princ (STRCAT "Rotation 为："
		 (rtos (vla-get-rotation Textobj))
		 "Rotation范例\n"
	 )
  )
  (prompt "<Enter>进行:" )
  (vl-cmdf pause)
  ;;将 Rotation值改变为45度(.707弧度
  (vla-put-rotation Textobj 0.707)
  (vla-zoomextents Acadobject)
  (princ (STRCAT " Rotation设定为:"
		 (RTOS (vla-get-Rotation Textobj))
		 "Rotation范例\n"
	 )
  )
  (princ)
)