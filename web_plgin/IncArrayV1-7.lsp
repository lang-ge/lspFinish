;; -----------------------== {��������} == ----------------------;;
;; ;;
;;�ó���������;;��ͬʱ���ж����ѡ��
;; ;;�е�ע�Ͷ������ҵ����κ���������
;;ѡ�� ;;
;; ;;
;;�ó���������ֲ���ģʽ����׼�Ͷ�̬�� ;;
;;��׼���'incarray'������ʾ��̬Ԥ����
;;���ֻ�ȶ�̬�汾���еø��죬��������
;; -���Խ������²���ʱ�����ֲ����������ԣ�
;;���д������� ;;
;; ;;
;;��̬ģʽ���� incarrayd������ʾ;;��Ԥ����
;;�������Ļ���϶�ʱ���еĶ��󡣵��ǣ�
;;�����������ɴ�Ԥ���ķ�������ģʽ��Ϊ;;
;;������ʹ�øó��������������������� ;;
;; ;;
;;�����������ʾ�û�ָ��;;��
;;����ֵ��Ȼ����ʾ��ѡ�����
;;���С���ѡ����԰�������;;���κ�ͼ�ζ���
;;�ӿڳ��⡣ ;;
;; ;;
;;�ڽ�����Чѡ��֮���û�Ӧָ��һ�����㣻
;;������ڻ���������������ǶȺͳ���
;;�����ķ���ȷ��;;�ķ�����ܶȡ�
;;�ֱ���������Խ�̣�����Խ�ܼ��� ;;
;; ;;
;;���ڿ���ͨ����;;���϶�������������顣
;;��Ļ��ֱ�����дﵽ����Ĵ�С���������
;;ѡ������ı����������֣����Զ��壬�ߴ磬;;
;;��Multileader�������ı����ҵ����κ��������ݣ�
;;��Щ��������ݽ���;;�Զ����ӡ�
;;��������ֵ��������������һ�� ;;
;; ----------------------------------------------------------------------;;
;;���ߣ�Lee Mac����Ȩ����?2014-www.lee-mac.com ;;
;; ----------------------------------------------------------------------;;
;;�汾1.0-2011-07-27 ;;
;; ;;
;; -�״η����� ;;
;; ----------------------------------------------------------------------;;
;;�汾1.1-2011-07-29 ;;
;; ;;
;; -ͨ�����û����ʽ���λ�Ʊ�����޸�UCS���� ;;
;;��лSwamp�û�HighflyingBird�ҵ�������� ;;
;; ----------------------------------------------------------------------;;
;;�汾1.2-2011-07-29 ;;
;; ;;
;; -����˳���ķǶ�̬�汾�� ;;
;; -�������������Զ����ǩ����ʾ���ı��Ĺ��ܡ� ;;
;; -�Ľ����������ܿɱ�������ǰ����;;
;;β���㡣 ;;
;; ----------------------------------------------------------------------;;
;;�汾1.3-2011-08-05 ;;
;; ;;
;; -�������������ж���Ĺ��ܣ�����������ע�Ͷ��� ;;
;;ѡ���а�������;;���κ�ע�Ͷ���
;;�����Խ����ӡ� ;;
;; -���黯���Կ��е������������ӡ� ;;
;; -MLeader���ֺͳߴ�������ֻ����ӡ� ;;
;; ----------------------------------------------------------------------;;
;;�汾1.4-2011-09-30 ;;
;; ;;
;; -�޸���������ͼ������������ʱ�Ĵ��� ;;
;; ----------------------------------------------------------------------;;
;;�汾1.5-2014-04-13 ;;
;; ;;
;; -������ȫ��д�� ;;
;; -��������ʾ��������ֵ�� ;;
;; ----------------------------------------------------------------------;;
;;�汾1.6-2014-04-13 ;;
;; ;;
;; -�޸���AutoCAD 2006�д��ڵı�������ͻ��grvecs����
;;��Swamp�û�CAB������-�ǳ���л�� ;;
;; ----------------------------------------------------------------------;;
;;�汾1.7-2014-06-07 ;;
;; ;;
;; -�������ۻ������������;;֮�������44��
;;��ֵ1����1��
;; ----------------------------------------------------------------------;;

(defun c:t1  nil (LM:incarray nil)) ;; Standard version
(defun c:t2 nil (LM:incarray  t )) ;; Dynamic  version

;;----------------------------------------------------------------------;;

(defun LM:incarray ( dyn / *error* bpt dim dis ept inc lst obl qty tmp vxu vxw )

    (defun *error* ( msg )
        (if (= 'int (type dim))
            (setvar 'dimzin dim)
        )
        (foreach obj obl
            (if (and (= 'vla-object (type obj)) (not (vlax-erased-p obj)) (vlax-write-enabled-p obj))
                (vla-delete obj)
            )
        )
        (incarray:endundo (incarray:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (redraw) (princ)
    )

    (if (not (and (setq inc (getenv "LMac\\incarray")) (setq inc (distof inc))))
        (setq inc 1)
    )
    (if (setq tmp (getreal (strcat "\nָ������ <" (incarray:num->str inc) ">: ")))
        (setenv "LMac\\incarray" (incarray:num->str (setq inc tmp)))
    )
    (incarray:startundo (incarray:acdoc))
    (setq dim (getvar 'dimzin))
    (setvar 'dimzin 0)
    (cond
        (   (not
                (and
                    (setq lst (incarray:selection->list (ssget "_:L" '((0 . "~VIEWPORT")))))
                    (setq bpt (getpoint "\nָ������: "))
                    (progn
                        (while
                            (and
                                (setq vxu (getpoint "\nָ����������: " bpt))
                                (equal bpt vxu 1e-8)
                            )
                            (princ "\n��Ч����������.")
                        )
                        vxu
                    )
                    (setq vxu (mapcar '- vxu bpt)
                          vxw (trans vxu 1 0 t)
                          dis (distance '(0.0 0.0 0.0) vxw)
                    )
                )
            )
        )
        (   dyn
            (princ "\nָ�������յ�: ")
            (while (= 5 (car (setq ept (grread t 13 0))))
                (redraw)
                (foreach obj obl (vla-delete obj))
                (setq qty (/ (caddr (trans (mapcar '- (cadr ept) bpt) 1 vxw t)) dis)
                      obl (incarray:copyvector lst (mapcar (if (minusp qty) '- '+) vxw) (abs (fix qty)) inc)
                )
                (grvecs (list -3 bpt (mapcar '(lambda ( a b ) (+ (* a qty) b)) vxu bpt)))
            )
        )
        (   (setq ept (getpoint bpt "\nָ�������յ�: "))
            (setq qty (fix (/ (caddr (trans (mapcar '- ept bpt) 1 vxw t)) dis)))
            (incarray:copyvector lst (mapcar (if (minusp qty) '- '+) vxw) (abs (fix qty)) inc)
        )
    )
    (setvar 'dimzin dim)
    (incarray:endundo (incarray:acdoc))
    (redraw) (princ)
)

;;----------------------------------------------------------------------;;

(defun incarray:num->str ( x / dim rtn )
    (if (equal x (atof (rtos x 2 0)) 1e-8)
        (rtos x 2 0)
        (progn
            (setq dim (getvar 'dimzin))
            (setvar 'dimzin 8)
            (setq rtn (vl-catch-all-apply 'rtos (list x 2 15)))
            (setvar 'dimzin dim)
            (if (not (vl-catch-all-error-p rtn)) rtn)
        )
    )
)

;;----------------------------------------------------------------------;;

(defun incarray:copyvector ( lst vec qty inc / cnt obj obl org )
    (setq org (vlax-3D-point 0 0)
          cnt 1
    )
    (repeat qty
        (foreach itm lst
            (setq obj (vla-copy (car itm))
                  obl (cons obj obl)
            )
            (vla-move obj org (vlax-3D-point (mapcar '* vec (list cnt cnt cnt))))
            (if (= "AcDbBlockReference" (vla-get-objectname obj))
                (mapcar
                    (function
                        (lambda ( att prp )
                            (vl-catch-all-apply 'vlax-put-property
                                (list att (car prp)
                                    (apply 'strcat
                                        (mapcar '(lambda ( x ) (incarray:increment x (* cnt inc)))
                                            (cdr prp)
                                        )
                                    )
                                )
                            )
                        )
                    )
                    (vlax-invoke obj 'getattributes)
                    (cdr itm)
                )
                (foreach prp (cdr itm)
                    (vlax-put-property obj (car prp)
                        (apply 'strcat
                            (mapcar '(lambda ( x ) (incarray:increment x (* cnt inc)))
                                (cdr prp)
                            )
                        )
                    )
                )
            )
        )
        (setq cnt (1+ cnt))
    )
    obl
)

;;----------------------------------------------------------------------;;

(defun incarray:selection->list ( sel / idx lst obj obn )
    (if sel
        (repeat (setq idx (sslength sel))
            (setq obj (vlax-ename->vla-object (ssname sel (setq idx (1- idx))))
                  obn (vla-get-objectname obj)
            )
            (if (and (= "AcDbBlockReference" obn) (= :vlax-true (vla-get-hasattributes obj)))
                (setq lst
                    (cons
                        (cons obj
                            (mapcar '(lambda ( a ) (vl-list* 'textstring (incarray:splitstring (vla-get-textstring a))))
                                (vlax-invoke obj 'getattributes)
                            )
                        )
                        lst
                    )
                )
                (setq lst
                    (cons
                        (cons obj
                            (mapcar '(lambda ( p ) (vl-list* p (incarray:splitstring (vlax-get-property obj p))))
                                (cond
                                    (   (wcmatch obn "AcDb*Text,AcDbMLeader") '(textstring))
                                    (   (wcmatch obn "AcDb*Dimension")        '(textoverride))
                                    (   (= "AcDbAttributeDefinition" obn)     '(tagstring promptstring textstring))
                                )
                            )
                        )
                        lst
                    )
                )
            )
        )
    )
)

;;----------------------------------------------------------------------;;

(defun incarray:splitstring ( str / lst )
    (setq lst (vl-string->list str))
    (read (vl-list->string (vl-list* 40 34 (incarray:split lst (< 47 (car lst) 58)))))
)

;;----------------------------------------------------------------------;;

(defun incarray:split ( lst flg )
    (cond
        (   (null lst) '(34 41))
        (   (= 92 (car lst))
            (if flg
                (vl-list* 34 32 34 92 92 (incarray:split (cdr lst) nil))
                (vl-list* 92 92 (incarray:split (cdr lst) flg))
            )
        )
        (   (or (< 47 (car lst) 58) (and (= 46 (car lst)) flg (< 47 (cadr lst) 58)))
            (if flg
                (vl-list* (car lst) (incarray:split (cdr lst) flg))
                (vl-list* 34 32 34 (car lst) (incarray:split (cdr lst) t))
            )
        )
        (   flg (vl-list* 34 32 34 (car lst) (incarray:split (cdr lst) nil)))
        (   (vl-list* (car lst) (incarray:split (cdr lst) nil)))
    )
)

;;----------------------------------------------------------------------;;

(defun incarray:increment ( str inc / dci dcs len num )
    (if (numberp (read str))
        (progn
            (setq num (+ (distof str) inc)
                  inc (incarray:num->str inc)
                  str (vl-string-left-trim "-" str)
                  inc (vl-string-left-trim "-" inc)
                  dci (incarray:decimalplaces inc)
                  dcs (incarray:decimalplaces str)
                  len (strlen str)
                  str (vl-string-left-trim "-" (rtos num 2 (max dci dcs)))
            )
            (cond
                (   (< 0 dcs) (setq len (+ (- len dcs) (max dci dcs))))
                (   (< 0 dci) (setq len (+ dci len 1)))
            )
            (repeat (- len (strlen str))
                (setq str (strcat "0" str))
            )
            (if (minusp num)
                (strcat "-" str)
                str
            )
        )
        str
    )
)

;;----------------------------------------------------------------------;;

(defun incarray:decimalplaces ( str / pos )
    (if (setq pos (vl-string-position 46 str))
        (- (strlen str) pos 1)
        0
    )
)

;;----------------------------------------------------------------------;;

(defun incarray:startundo ( doc )
    (incarray:endundo doc)
    (vla-startundomark doc)
)

;;----------------------------------------------------------------------;;

(defun incarray:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;;----------------------------------------------------------------------;;

(defun incarray:acdoc nil
    (eval (list 'defun 'incarray:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (incarray:acdoc)
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: IncArray.lsp | Version 1.7 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: \"incarray\" - Standard | \"incarrayd\" - Dynamic ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;