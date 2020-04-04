;;���Ӧ��
  (or *cmdvlr*
      (setq *cmdvlr* (vlr-command-reactor nil '((:vlr-commandWillStart . cmdstart) (:VLR-commandEnded . cmdEnd))))
      )
  ;;���ݿⷴӦ��
  (or *acdbvlr*
      (setq *acdbvlr* (vlr-acdb-reactor nil
       '((::VLR-objectModified . acdbstart))))
      )
;;���ʼ��Ӧ���ص�����
(defun cmdstart (vlr lst)
  (if (= "GRIP_STRETCH" (car lst))
    (setq *GRIP_STRETCH_Start* t ;_ ���������е���ק�����־
          *GRIP_STRETCH_Obj* nil ;_ ��ռе���ק�Ķ���ͼԪ���б�
          )
    )
  )
;;���������Ӧ���ص�����
(defun cmdEnd (vlr lst )
  (if (= "GRIP_STRETCH" (car lst))
    (progn
      (setq *GRIP_STRETCH_Start* nil) ;_ ��������е���ק�����־
      ;;�˴���������Ӷ� *GRIP_STRETCH_Obj* ����Ĵ���,�罫�е���ק������ɫ���
     (mapcar '(lambda (x) (vla-put-color (vlax-ename->vla-object x) 1)) *GRIP_STRETCH_Obj*)
    )
  )
)
;;���ݿⷴӦ���ص�����
(defun acdbstart (vlr obj)
  (if *GRIP_STRETCH_Start*
    (setq *GRIP_STRETCH_Obj*
           (append *GRIP_STRETCH_Obj* (cdr obj)) ;_ �е���ק�Ķ���ͼԪ���б�
    )
  )
)