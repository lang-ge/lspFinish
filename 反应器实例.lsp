;;;����һ��Բ��VLA����
(vl-load-com)
(setq myCircle
       (progn
	 (setq ctrPt  (getpoint "\n����Բ��: ")
	       radius (distance ctrPt (getpoint ctrpt "\n����뾶 : "))
	 )
	 (vla-addCircle
	   (vla-get-ModelSpace		;��Բ���뵽ͼ��ģ�Ϳռ�
	     (vla-get-ActiveDocument (vlax-get-acad-object))
	   )
	   (vlax-3d-point ctrPt)
	   radius
	 )
       )
)

;;;�ú���������ӡԲ�İ뾶
(defun print-radius (notifier-object reactor-object parameter-list)
  (vl-load-com)
  (cond
    ((vlax-property-available-p notifier-object "Radius")
     (alert
       (strcat "���Բ�İ뾶��:" (rtos (vla-get-radius notifier-object) 2))
     )

    )
  )
)

:vlr-docframemovedorresized
;;;mycircle����Ӧ��
(setq circleReactor
       (vlr-object-reactor
	 (list myCircle)
	 "Circle Reactor"
	 '((:vlr-modified . print-radius))
       )
)

;;;==================================================*
;;;���彫����Ӧ�����ӵ�ָ��ֱ�ߵ����
(vl-load-com)
(defun c:258 (/ el rlt vrl)
  (setq el (car (entsel "\nѡ��һ�����ӷ�Ӧ����ֱ��:")))
  (setq rlt (list (vlax-ename->vla-object el))) ;��ͼԪ��ת��ΪVLA����

;;;��������Ӧ�����ص��¼���ͼԪ���޸ģ��ص�������show-l
  (setq	vrl (vlr-pers
	      (vlr-object-reactor rlt nil '((:vlr-modified . show-l)))
	    )
  )
  (princ)
)

;;;����ص�����
(defun show-l (notifier-object reactor-object parameter-list / l)
  (setq l (vla-get-length notifier-object)) ;����ѡֱ�ߵĳ��ȸ�������l
  (setq l (rtos l 2 4))			;����ֵת��Ϊ�ַ���
  (alert (strcat "ֱ�ߵĳ����ǣ�" l))	;������Ϣ�Ի���
)

;;;==================================================*
;;;���彫����Ӧ�����ӵ�Բ�ϵ�������Բ���޸ģ�����ֱ����Բ�����λ�ú���Ա������䡣
(vl-load-com)
(defun c:257( / p0 p1 p2 p3 p4 r r1 eh1 eh2 l1-l2 rlt vrl)
  (setq p0(getpoint "\n����Բ�ģ�"))
  (setq r(getdist p0 "\n����뾶��"))
  (command "circle" p0 r)
  (setq r1(* 1.25 r))
  (setq ec(entlast))
  (setq p1(polar p0 0 r1))
  (setq p2(polar p0 (* 0.5 pi) r1))
  (setq p3(polar p0 pi r1))
  (setq p4(polar p0 (* -0.5 pi) r1))
  (command "line" p1 p3 "")
  (setq eh1(cdr (assoc 5 (entget (entlast)))));��һ��ֱ�ߵľ��
  (command "line" p2 p4 "")
  (setq eh2(cdr (assoc 5 (entget(entlast)))));�ڶ���ֱ�ߵľ��
  (setq l1-l2(list eh1 eh2));����ֱ�ߵľ����
  (setq rlt(list (vlax-ename->vla-object ec)));Բ��ͼԪ��ת��ΪVLA����
  (setq vrl (vlr-pers(vlr-object-reactor rlt  l1-l2 '((:vlr-modified . c-2l)))));��Ӧ�����ӵ�Բ�ϣ�����ֱ�ߵľ����Ϊ�������ݣ��������޸ĸ�Բ���¼�ʱ������c-2l����
  (princ);��Ĭ�˳�
)

;����c-2l����
(defun c-2l(notifier-object reactor-object parameter-list / ec ec_l el1 el2 ell_1 ell_2 p0 p1 p2 p3 p4 p0x p0y p0z)
  (setq ec(vlax-vla-object->ename notifier-object);VLA�����Բת��ΪͼԪ��
     ec_l(entget ec);Բ��ͼԪ��
     p0(cdr(assoc 10 ec_l));��ȡԲ�ĵ�����
     r(* 1.25 (cdr(assoc 40 ec_l)));��ȡԲ�İ뾶֮���1.25
  )
  (setq el1(handent (car (vlr-data reactor-object))));��һ��ֱ�ߵ�ͼԪ��
  (setq el2(handent (cadr (vlr-data reactor-object))));�ڶ�ֱ���ߵ�ͼԪ��
(setq p0x (car p0));��ȡԲ�ĵ�X����
 (setq p0y (cadr p0));��ȡԲ�ĵ�Y����
 (setq p0z (caddr p0));��ȡԲ�ĵ�Z����
 (setq p1 (list (+ p0x r) p0y p0z));�޸�ֱ�߶˵������
 (setq p2 (list p0x (+ p0y r) p0z));�޸�ֱ�߶˵������
 (setq p3 (list (- p0x r) p0y p0z));�޸�ֱ�߶˵������
 (setq p4 (list p0x (- p0y r) p0z));�޸�ֱ�߶˵������
 (setq ell_1(entget el1));��һ��ֱ�ߵ�ͼԪ��
 (setq ell_1(subst (vl-list* 10 p1)(assoc 10 ell_1)ell_1));ֱ�ߵ��¶˵��滻ֱ�ߵ��϶˵�
 (setq ell_1(subst (vl-list* 11 p3)(assoc 11 ell_1)ell_1));ֱ�ߵ��¶˵��滻ֱ�ߵ��϶˵�
(entmod ell_1);���µ�һ��ֱ��
  (setq ell_2(entget el2));�ڶ���ֱ�ߵ�ͼԪ��
  (setq ell_2(subst (vl-list* 10 p2)(assoc 10 ell_2)ell_2));ֱ�ߵ��¶˵��滻ֱ�ߵ��϶˵�
  (setq ell_2(subst (vl-list* 11 p4)(assoc 11 ell_2)ell_2));ֱ�ߵ��¶˵��滻ֱ�ߵ��϶˵�
  (entmod ell_2);���µڶ���ֱ��
)






