;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;;
;;                                                                               ;;
;;  AUTHOR:  lan-ge, April 2020.   VERSION:   v 1.0                              ;;
;;...............................................................................;;
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;;
(defun c:235 (/ r1 h1 oce p1 p2 p3 t1 v_c v_l1 vrl) 
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
  (setq v_l1 (vlax-ename->vla-object (entlast))) ;��ͷ��ת��ΪVLA����
  (command ".text" "m" p2 h1 0 t1)
  (setq v_l2 (vlax-ename->vla-object (entlast))) ;����ת��ΪVLA����
  (command ".circle" p2 r1)
  (setq v_c (list (vlax-ename->vla-object (entlast)))) ;Բ��ͼԪ��ת��ΪVLA����
  (setq vrl (vlr-object-reactor v_c (list v_l1 v_l2) '((:vlr-modified . c-2l))))
  ;;;��Ӧ�����ӵ�Բ�ϣ���ͷ�ߵ�VLA��������ŵ�VLA���������Ϊ�������ݣ��������޸ĸ�Բ���¼�ʱ������c-2l����
  (setvar "cmdecho" oce)
  (princ)
)
;����c-2l�ص�����
(defun c-2l (notifier-object reactor-object parameter-list / r1 p2 p3 v_l1 v_l2 points 
             myacad mydoc
            ) 
  (if (vlax-property-available-p notifier-object "radius") 
    (progn 
      (setq p2 (VLA-get-center notifier-object)) ;��ȡԲ��Բ�ģ�p2�Ǳ���
      (setq r1 (VLA-get-radius notifier-object)) ;��ȡԲ�İ뾶��r1�Ǳ���
    )
  )
  ;;;   (setq p2 (vlax-safearray->list (vlax-variant-value p2))) ;����ȫ����ת��Ϊ��
  (setq v_l1 (car (vlr-data reactor-object))) ;��ͷ�ߵ�VLA����
  (setq v_l2 (cadr (vlr-data reactor-object))) ;���ŵ�VLA����
  ;;;   (setq P1 (vlax-safearray->list
  ;;;              (vlax-variant-value
  ;;;                (vlax-get-property v_l1 'Coordinate 0);0��ʾ��һ���˵�
  ;;;              )
  ;;;            )
  ;;;   ) ;����ȫ����ת��Ϊ��
  ;;;   (setq P1 (cdr (assoc 10 (entget (vlax-vla-object->ename v_l1)))))
  ;;;   (setq a1 (angle p2 p1)
  ;;;         p3 (polar p2 a1 r1)
  ;;;   )
  (setq myacad (vlax-get-acad-object)) ;��ȡAutoCADӦ�ó�����
  (setq mydoc (vla-get-ActiveDocument myacad)) ;��ȡ��ĵ�
  (setq a1 (vla-AngleFromXAxis (vla-get-Utility mydoc) 
                               p2
                               (vla-get-Coordinate v_l1 0)
           )
  ) ;��ȡ�Ƕ�
  (setq P3 (vla-PolarPoint (vla-get-Utility mydoc) p2 a1 r1)) ;��ȡ�յ�
  ;;;   ��ģʽ����VLA����
  (vla-put-Coordinate v_l1 1 P3) ;����ָ���㡣��ֱ��
  (vla-put-TextAlignmentpoint v_l2 p2) ;�����ı������
  ;;;   (setq points (append P1 P3))
  ;;;   (vlax-put v_l1 'Coordinates points)

  ;;;   ������ģʽ����VLA����/(vlax-get-property v_l1 'Coordinate 0)/(vla-get-Coordinate v_l1 0)
  ;;;   (setq points (vlax-variant-value (vla-get-Coordinates v_l1)))
  ;;;   (vlax-safearray-put-element points 3 (car p3))
  ;;;   (vlax-safearray-put-element points 4 (cadr p3))
  ;;;   (vlax-safearray-put-element points 5 (caddr p3))
  ;;;   (vla-put-Coordinates v_l1 points)

  (vla-Regen mydoc :vlax-true) ;�����ĵ�
  (setq a1 nil)
  ;;;   (vla-put-endpoint v_l1 (vlax-3d-point p3)) ;����ֱ��1���յ�
  ;;;   (if (and v_l1 (not (vlax-object-released-p v_l1))) (vlax-release-object v_l1))
)