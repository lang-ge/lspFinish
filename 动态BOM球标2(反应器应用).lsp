;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;;
;;                                                                               ;;
;;  AUTHOR:  lan-ge, April 2020.   VERSION:   v 1.0                              ;;
;;...............................................................................;;
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;;
(defun c:nm (/ r1 h1 oce p1 p2 p3 t1 v_c v_le v_l2 loop1 loop2 source Points vrl kw 
             ArroType *ModelSpace*
            ) 
  (vl-load-com)
  (setq r1    (* 4 (GETVAR "DIMSCALE"))
        h1    (* 2.5 (GETVAR "DIMSCALE"))
        loop1 T
  )
  (graphscr)
  (setq oce (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (setvar "osmode" 0)
  (setvar "CLAYER" "mark")

  ;;; ��ͷѡ��
  (if (not #def) (setq #def "DĬ��"))
  (initget "DĬ�� SС�� O'90�� B����")
  (setq kw (getkword 
             (acet-str-format "Enter an option [DĬ��/SС��/O'90��/B����] <%1>: " #def)
           )
  )
  (if (or (not kw) (equal kw "")) 
    (setq kw #def)
    (setq #def kw)
  )
  (cond 
    ((= kw "DĬ��")
     (setq ArroType acArrowDefault)
    )
    ((= kw "SС��")
     (setq ArroType acArrowDotSmall)
    )
    ((= kw "O'90��")
     (setq ArroType acArrowOpen90)
    )
    ((= kw "B����")
     (setq ArroType acArrowBoxBlank)
    )
  )
  ;;; ����

  (while loop1 
    (if 
      (setq t1 (getstring "\npart number: ")
            p1 (getpoint "\nfrom point: ")
      )
      (progn 
        (command ".text" "m" p1 h1 0 t1)
        (setq v_l2 (vlax-ename->vla-object (entlast))) ;����ת��ΪVLA����
        (setq *ModelSpace* (vla-get-ModelSpace (vla-get-activedocument (vlax-get-acad-object ) ) ))
        (setq v_c (vla-addcircle *ModelSpace* (vlax-3d-point p1) r1))
        (setq loop2 T
              v_le  (draw_leader p1 (polar p1 0 0.001))
        )
        (prompt "\nȷ����ŷ���λ��:")
        (while loop2 
          (setq p2     (grread T)
                source (car p2)
                p2     (cadr p2)
          )
          (cond 
            ((= source 5)
             (vla-put-TextAlignmentpoint v_l2 (vlax-3d-point p2))
             (vla-put-center v_c (vlax-3d-point p2))
             (setq Points (vlax-safearray-fill 
                            (vlax-make-safearray 
                              vlax-vbdouble
                              '(0 . 5)
                            )
                            (append p1 
                                    (polar p2 (angle p2 p1) r1)
                            )
                          )
             )
             (if (> (distance p2 p1) r1) 
               (vla-put-coordinates v_le Points)
             )
            )
            (t
             (progn 
               (setq loop2 nil)
               (vlax-ldata-put v_c "radius" r1)
               (function c-2l)
               (setq vrl (vlr-object-reactor (list v_c) 
                                             (list v_le v_l2)
                                             '((:vlr-modified . c-2l))
                         )
               )
             )
            )
          )
        )
      )
      (setq loop1 nil)
    )
  )
  ;;;��Ӧ�����ӵ�Բ�ϣ���ͷ�ߵ�VLA��������ŵ�VLA���������Ϊ�������ݣ��������޸ĸ�Բ���¼�ʱ������c-2l����
  (setvar "cmdecho" oce)
  (princ)
)
;����c-2l�ص�����
(defun c-2l (notifier-object reactor-object parameter-list / r1 p2 p3 v_le points 
             *acdocument*
            ) 
  (if (vlax-property-available-p notifier-object "radius") 
    (progn 
      (setq p2 (VLA-get-center notifier-object)) ;��ȡԲ��Բ�ģ�p2�Ǳ���
      (setq r1 (VLA-get-radius notifier-object)) ;��ȡԲ�İ뾶��r1�Ǳ���
    )
  )
  ;;;   (setq p2 (vlax-safearray->list (vlax-variant-value p2))) ;����ȫ����ת��Ϊ��
  (setq v_le (car (vlr-data reactor-object))) ;��ͷ�ߵ�VLA����
  (setq v_l2 (cadr (vlr-data reactor-object))) ;���ŵ�VLA����
  (setq *acdocument* (vla-get-activedocument (vlax-get-acad-object)))
  (setq a1 (vla-AngleFromXAxis (vla-get-Utility *acdocument*) 
                               p2
                               (vla-get-Coordinate v_le 0)
           )
  ) ;��ȡ�Ƕ�
  (setq P3 (vla-PolarPoint (vla-get-Utility *acdocument*) p2 a1 r1)) ;��ȡ�յ�
  ;;;   ��ģʽ����VLA����
  (vla-put-Coordinate v_le 1 P3) ;����ָ���㡣��ֱ��
  (vla-put-TextAlignmentpoint v_l2 p2) ;�����ı������
  (vla-Regen *acdocument* :vlax-true) ;�����ĵ�
  (setq a1 nil)
)
;;; ���Ƽ�ͷ��
(defun draw_leader (pt1 pt2 / v_le)  ;draw leader
  (command "leader" pt1 pt2 "f" "st" "f" "a" "" "" "n")
  (setq v_le (vlax-ename->vla-object (entlast)))
  (vla-put-ArrowheadType v_le ArroType)
  (vla-Update v_le)
  v_le
)