(defun c:nm (/ r1 h1 oce p2 p3 t1 v_c v_l1 vrl) 
  (vl-load-com)
  (setq r1 (* 4 (GETVAR "DIMSCALE"))
        h1 (* 2.5 (GETVAR "DIMSCALE"))
  )
  (graphscr)
  (setq oce (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (setvar "osmode" 0)
  (setvar "CLAYER" "mark")
  (setq p1 (getpoint "\nfrom point: ")
        p2 (getpoint p1 "\nbubber center point: ")
        a1 (angle p2 p1)
        p3 (polar p2 a1 r1)
        t1 (getstring "\npart number: ")
  )


  (COMMAND "_LEADER" p1 p3 "" "" "NONE")
  ;;;   (command "line" p1 p3 "")
  (setq v_l1 (vlax-ename->vla-object (entlast))) ;��һ��ֱ��ת��ΪVLA����
  (command ".text" "m" p2 h1 0 t1)
  (command ".circle" p2 r1)
  (setq v_b (list (vlax-ename->vla-object (entlast)))) ;Բ��ͼԪ��ת��ΪVLA����
  (setq vrl (vlr-object-reactor v_c (list v_l1) '((:vlr-modified . c-2l))))
  ;;;��Ӧ�����ӵ�Բ�ϣ�ֱ�ߵ�VLA�����Ϊ�������ݣ��������޸ĸ�Բ���¼�ʱ������c-2l����
  (setvar "cmdecho" oce)
  (princ)
)
;����c-2l�ص�����
(defun c-2l (notifier-object reactor-object parameter-list / r1 a1 p2 p3 v_l1 points 
             myacad mydoc list1
            ) 
  (setq p2 (VLA-get-center notifier-object)) ;��ȡԲ��Բ�ģ�p2�Ǳ���
  (setq r1 (VLA-get-radius notifier-object)) ;��ȡԲ�İ뾶��r1�Ǳ���
  (setq p2 (vlax-safearray->list (vlax-variant-value p2))) ;����ȫ����ת��Ϊ��
  (setq v_l1 (car (vlr-data reactor-object))) ;��һ��ֱ�ߵ�VLA����
  ;;; (setq list1 (vlax-safearray->list 
  ;;;               (vlax-variant-value 
  ;;;                 (vla-get-Coordinates 
  ;;;                   v_l1
  ;;;                 )
  ;;;               )
  ;;;             ) ;����ȫ����ת��Ϊ��
  ;;; )
  ;;; (setq P1 (list (car list1) (cadr list1) (caddr list1)))
  (setq P1 (cdr (assoc 10 (entget (vlax-vla-object->ename v_l1)))))
  (setq a1 (angle p2 p1)
        p3 (polar p2 a1 r1)
  )

  ;;;   ��ģʽ����VLA����
  (setq points (append P1 P3))
  (vlax-put v_l1 'Coordinates points)

  ;;;   ������ģʽ����VLA����
  ;;;   (setq points (vlax-variant-value (vla-get-Coordinates v_l1)))
  ;;;   (vlax-safearray-put-element points 3 (car p3))
  ;;;   (vlax-safearray-put-element points 4 (cadr p3))
  ;;;   (vlax-safearray-put-element points 5 (caddr p3))
  ;;;   (vla-put-Coordinates v_l1 points)
  (setq myacad (vlax-get-acad-object)) ;��ȡAutoCADӦ�ó�����
  (setq mydoc (vla-get-ActiveDocument myacad)) ;��ȡ��ĵ�
  (vla-Regen mydoc :vlax-true)
  ;;;(vla-put-endpoint v_l1 (vlax-3d-point p3)) ;����ֱ��1���յ�
)

