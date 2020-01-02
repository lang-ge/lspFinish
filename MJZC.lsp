;;;--------------------------------------------------------
;;;��ȡ���󳤶���Ϣ
(vl-load-com)
(defun C:Q1 (/ SS SUM I OBJ TMP LEN)
    (princ "\\n��ѡ�����...")
    (if    (setq SS (ssget))
    (progn
        ;;��ʼ��
        (setq SUM 0
          I 0
        )
        ;;������������ȡ
        (repeat (sslength SS)
        (setq OBJ (vlax-ename->vla-object (ssname SS I)))
        (if (setq TMP (vlax-curve-getendparam OBJ))
            (setq LEN (vlax-curve-getdistatparam OBJ TMP)
              SUM (+ SUM LEN)
            )
            (princ (strcat "\\n��" (itoa (1+ I)) "������������")
            )
        )
        (setq I (1+ I))
        )
        ;;��ʾ���
        (if    (= SUM 0)
        (princ "\\nû����ȡ�����ȡ�")
        (alert
            (princ (strcat "��ѡʵ����ܳ���Ϊ��" (rtos SUM 2 3))
            )
        )
        )
    )
    )
    (princ)
)


;;;--------------------------------------------------------
;;;��ȡ���������Ϣ
(vl-load-com)
(defun c:Q2 (/ ENT sc sek sel jd th)
;(setq sc (cdr (assoc 3 (entget (car (entsel "\n���ѡͼ���ڳߴ�"))))))
;(COMMAND "._DIMSTYLE" "R" SC)
;(setq sel (getint "\n��ѡ��������� :<ƽ���>\n(1)ƽ���;(2)�����(������);"))
   
  (Dcl_data)
  
  (if (or (= sek 1))
    (Q11)
  )

  (if (= sek 2)
    (Q22)
  )
(princ)
)
;;;--------------------------------------------------------
;;;;DCL�ӳ��� 
(DEFUN Dcl_data	(/ Cal_id)
 (setq Cal_id (load_dialog "MJZC.dcl"))
 (if (not (new_dialog "q2" Cal_id ""))
  (exit)
 )
 (disableCtrls '("PT"))
 (ACTION_TILE "PS" "(disableCtrls '( \"PT\"))")
 (ACTION_TILE "FS" "(EnableCtrls '( \"PT\"))")
 (ACTION_TILE "MM" "(SET_TILE \"Acy\" \"1\")")
 (ACTION_TILE "M" "(SET_TILE \"Acy\" \"3\")")
 (ACTION_TILE "accept" "(Cal_data)(DONE_DIALOG)")
 (start_dialog)
 (unload_dialog Cal_id)
)

(DEFUN Cal_data	()
 (cond ((= (GET_TILE "PS") "1") (setq sek 1))
       ((= (GET_TILE "FS") "1") (setq sek 2))
 )
 (cond ((= (GET_TILE "MM") "1") (setq sel 1))
       ((= (GET_TILE "M") "1") (setq sel 2) )
 )

 (SETQ	jd (1+ (ATOI (GET_TILE "Acy")))
	th (ATOF (nth (ATOI (GET_TILE "PT")) '("0.8" "1.0" "1.2" "1.5" "2.0" "2.5" "3.0")))
  )
)

;;;--------------------------------------------------------
;;;;�Ӻ����� Q11
(defun Q11 (/ olderr oldcmdecho errexit undox restore ss1 nr en tot_area)
  (defun errexit (s)
    (restore)
  )

  (defun undox ()
    (command "._undo" "_E")
    (setvar "cmdecho" oldcmdecho)
    (setq *error* olderr)
    (princ)
  )

  (setq olderr  *error*
        restore undox
        *error* errexit
  )
  (setq oldcmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (command "._UNDO" "_BE")
  (if (setq ss1 (ssget '((-4 . "<OR")
                         (0 . "POLYLINE")
                         (0 . "LWPOLYLINE")
                         (0 . "CIRCLE")
                         (0 . "ELLIPSE")
                         (0 . "SPLINE")
                         (0 . "REGION")
                         (-4 . "OR>")
                        )
                )
      )
    (progn
      (setq nr 0)
      (setq tot_area 0.0)
      (setq en (ssname ss1 nr))
      (while en
        (command "._area" "_O" en)
;(setq sel (getint "\n��ѡ��λ :<��>\n(1)����;(2)��;"))

 (if (= sel 1)
 (setq v (strcat (rtos  (setq tot_area (+ tot_area (getvar "area"))) 2 jd) "ƽ������"))
 )


 (if (= sel 2)
 (setq v (strcat (rtos  (setq tot_area (/ (+ tot_area (getvar "area")) 1000000)) 2 jd) "ƽ����"))
 )
        (setq nr (1+ nr))
        (setq en (ssname ss1 nr))
      )
      
      (princ "\nƽ��� = ")
      (princ v)
    )
  )
  (restore)
)

;;;--------------------------------------------------------
;;;;�Ӻ����� Q22
(defun Q22 (/ ss en lis es v)
 (setvar "CMDECHO" 0)
 (princ "\n\n��ѡ��������(Բ������ε�)....")
 (setq ss (ssget '((0 . "CIRCLE,LWPOLYLINE"))))
 (setq i -1  ES 0 PS 0 lis '())
 (repeat (sslength ss)
  (setq en (ssname ss (setq i (1+ i))))
  (command "AREA" "O" EN)
  (setq lis (append (list (getvar "AREA")) lis)
        ps (+ (getvar "PERIMETER") ps))
 )
 (setq lis (vl-sort lis '>))
 (setq es (car lis)  k 0)
 (repeat (1- (length lis))
  (setq es (- es (nth (setq k (1+ k)) lis)))
 )
; (setq th (getdist "\n����������Ϻ� :"))
;(setq jd (getint "\n�����뾫��<4>:"))
;(if (= jd nil)
;  (setq jd 4)
;)
; (setq sel (getint "\n��ѡ��λ :<��>\n(1)����;(2)��;"))

 (if (= sel 1)
 (setq v (strcat (rtos  (setq es (+ (* ps th) es es)) 2 jd) "ƽ������"))
 )

 (if (= sel 2)
 (setq v (strcat (rtos  (setq es (/ (+ (* ps th) es es) 1000000)) 2 jd) "ƽ����"))
 )
 (princ (strcat "\n�������Ϣ(��������)Ϊ��" v))
 (setq po (getpoint "\nָ����������д���:"))
 (if (not (tblsearch "style" "CHINESE"))
 (command "_.style" "CHINESE" "PMingLiU" "" "0.8" "" "" "")
 )
 (command "_layer" "s" "MARK" "")
 (COMMAND "_.TEXT" "S" "CHINESE" po (* 3.5 (GETVAR "DIMSCALE")) ""v)
 (princ)
  )


;;;--------------------------------------------------------
;;;����:Q3
;;;--------------------------------------------------------
;;;��������:2013.02.05
;;;����  :֣�Ѷ�
;;;˵��:��ȡ����ɱ���Ϣ
;;;--------------------------------------------------------

;;;����������ָ������Ƶ�ļ�
(defun Play (file /)
;;; (Vlax-Put-Property (vlax-Create-Object "WMPlayer.OCX") 'URL file)
 (findfile file)
)
;;;���������ı��ļ�ת��Ϊ���ݱ� 
(defun fn-lst (file / fn ftext lst)
 (setq fn (open (findfile file) "r"))
 (while	(setq ftext (read-line fn))
  (setq lst (append lst (list ftext)))
 )
 (close fn)
 lst
)

;;;�����������ݱ�ת��Ϊ�ı��ļ�
(defun lst-fn (file lst mod / fn)
 (setq fn (open file mod))
 (foreach x lst
  (write-line x fn)
 )
 (close fn)
)

;;;�����������ݱ����DCL�б��
(defun lst-dcl (key lst Mod No /)
  (start_list key Mod No)
  (mapcar 'add_list
	  (mapcar '(lambda (x)
		     (strcat " " x)
		   )
		  lst
	  )
  )
  (end_list)
)

;;;������ʸ��ͼ��������
(defun zd-image	(Dkey Dmod Result /)
  (START_IMAGE Dkey)
  (FILL_IMAGE 0 0 (DIMX_TILE Dkey) (DIMY_TILE Dkey) Dmod)
  (mapcar 'eval
	  (mapcar 'cons
		  (mapcar '(lambda (x) 'vector_image) Result)
		  Result
	  )
  )
  (END_IMAGE)
)

;;;;ͨ�ú���A
(defun minmm_ss->enlist (ss / lst n en)
  (setq n -1)
  (while (setq en (ssname ss (setq n (1+ n))))
    (setq lst (cons en lst))
  )
)
(defun minmm_objBox (obj / minpoint maxpoint)
  (vla-GetBoundingBox obj 'minpoint 'maxpoint)
					;ȡ�ð���ͼԪ���������С��
  (setq minpoint (vlax-safearray->list minpoint)) ;�ѱ�������ת��Ϊ��
  (setq maxpoint (vlax-safearray->list maxpoint)) ;�ѱ�������ת��Ϊ��
  (setq minpoint (trans minpoint 0 1))
  (setq maxpoint (trans maxpoint 0 1))
(setq obj (list minpoint maxpoint));;;��ϳɵ��
)
(defun minmm_ssbox (ss / boxlst maxlst minlst objlst)
  (setq objlst (mapcar 'vlax-ename->vla-object (minmm_ss->enlist ss)))
  (setq boxlst (mapcar 'minmm_objBox objlst))
  (setq minlst (mapcar 'car boxlst))
  (setq maxlst (mapcar 'cadr boxlst))
(list
   (apply 'mapcar (cons 'min minlst))
   (apply 'mapcar (cons 'max maxlst))
  )
)
;;;ͨ�ú���A����

;;;;ͨ�ú���B
;;____________________________________________
;; �� (lt:ss-entnext en)
;; [����] ��ȡ��ͼԪ en ֮�������ͼԪ��ѡ��
;; [����] en----ͼԪ��
;; [����] ѡ��
;; [����]1.(setq en (entlast))
;;         ִ�д���ͼԪ������� LINE,BOUNDARY
;;         (setq ss (lt:ss-entnext en))
;;       2.(setq ss (lt:ss-entnext (car(entsel))))
(defun lt:ss-entnext (en / ss)
   (if en
     (progn
       (setq ss (ssadd))
       (while (setq en (entnext en))
         (if (not (member (cdr (assoc 0 (entget en)))
                          '("ATTRIB" "VERTEX" "SEQEND")
                  )
             )
           (ssadd en ss)
         )
       )
       (if (zerop (sslength ss)) (setq ss nil))
       ss
     )
     (ssget "_x")
   )
)
;;;ͨ�ú���B����

;;;;ͨ�ú���C
;;____________________________________________
;; �� (MJ:GetAttributes ent)
;; [����] ��ȡͼ�� ent ֮�����������
;; [����] ent----ͼ����
;; [����] ����
;; [����]1.(setq shujlb (MJ:GetAttributes (car (entsel))))
;;       2.(setq sheji  (nth 1 (nth 0 shujlb) );;;0Ϊ���һ������
(defun MJ:GetAttributes	(ent / lst) 
(setq *Obj2En*  vlax-vla-object->ename )
  (if (safearray-value
	(setq lst
	       (vlax-variant-value
		 (vla-getattributes
		   (vlax-ename->vla-object ent)
		 )
	       )
	)
      )
    (mapcar
      '(lambda (x)
	 (list
	   (vla-get-tagstring x)
	   (vla-get-textstring x)
	   (*Obj2En* x)
	 )
       )
      (vlax-safearray->list lst)
    )
  )
)
;;;;ͨ�ú���C����

;;;==================================================*
;;;���ܣ�����ʸ��ͼ��ͼ��                            *
;;;������ lst     -----�ؼ����                      *
;;;������ key     -----�ؼ��ؼ���                    *
;;;���أ� mode    -----��ʾģʽ                      *
;;;���ԣ�((mode_image '("ina" "idw")) "ina" 0)
(defun mode_image (lst / i j x y mode_image)
  ((lambda (/ i j x y)
     (repeat (setq i 20)
       (setq j 1)
       (repeat 20
	 (setq x (cons j x)
	       y (cons i y)
	       j (1+ j)
	 )
       )
       (setq i (1- i))
     )
     (setq mode_image
	    (eval
	      (list
		'lambda
		'(key mode)
		(list
		  'cond
		  '((= 1 mode)
		    (start_image key)
		    (fill_image 0 0 (dimx_tile key) (dimy_tile key) -15)
		    (end_image)
		    (mode_tile key mode)
		   )
		  (list	't
			'(start_image key)
			'(fill_image 0 0 (dimx_tile key) (dimy_tile key) -15)
			(list 'mapcar
			      ''vector_image
			      (list 'quote x)
			      (list 'quote y)
			      (list 'quote x)
			      (list 'quote y)
			      '(cond
                                                (   (member key lst)
                                                   '(
                                                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                                                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                                                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                                                        -15 -15 -15 -15 -15 -15 095 096 096 096 096 096 096 096 096 095 -15 -15 -15 -15
                                                        -15 -15 -15 -15 -15 -15 096 254 254 254 254 254 254 254 254 096 -15 -15 -15 -15
                                                        -15 -15 -15 -15 -15 -15 096 063 063 -15 063 063 063 063 063 096 -15 -15 -15 -15
                                                        -15 -15 -15 -15 -15 -15 096 063 -15 250 -15 063 063 063 063 096 -15 -15 -15 -15
                                                        -15 -15 -15 -15 -15 -15 096 -15 250 250 -15 063 063 063 063 096 -15 -15 -15 -15
                                                        -15 -15 -15 -15 -15 -15 254 250 -15 250 -15 063 063 063 063 096 -15 -15 -15 -15
                                                        254 254 254 254 254 254 250 -15 -15 250 -15 063 063 063 063 096 254 254 254 254
                                                        254 254 254 254 254 250 -15 -15 -15 250 -15 063 063 063 063 096 254 254 254 254
                                                        254 254 254 254 250 -15 -15 -15 -15 250 -15 063 063 063 063 096 254 254 254 254
                                                        254 254 254 250 250 250 -15 -15 -15 250 -15 063 063 063 063 096 254 254 254 254
                                                        254 254 254 254 254 250 -15 250 -15 250 -15 063 063 063 063 096 254 254 254 254
                                                        254 254 254 254 254 250 -15 250 250 250 -15 063 063 063 063 096 254 254 254 254
                                                        254 254 254 254 250 -15 250 254 254 250 254 096 096 096 096 095 254 254 254 254
                                                        254 254 254 254 250 -15 250 254 254 254 254 254 254 254 254 254 254 254 254 254
                                                        254 254 254 254 254 250 254 254 254 254 254 254 254 254 254 254 254 254 254 254
                                                        254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254
                                                        -15 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 -15
                                                    )
                                                )
                                                (
                                                   '(
                                                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                                                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                                                        -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15 -15
                                                        -15 -15 -15 -15 -15 -15 -15 095 096 096 096 096 096 095 -15 -15 -15 -15 -15 -15
                                                        -15 -15 -15 -15 -15 -15 -15 096 063 063 063 063 063 096 -15 -15 -15 -15 -15 -15
                                                        -15 -15 -15 -15 -15 -15 -15 096 254 254 063 063 063 096 -15 -15 -15 -15 -15 -15
                                                        -15 -15 -15 -15 -15 -15 -15 253 250 254 063 063 063 096 254 008 254 -15 -15 -15
                                                        -15 -15 -15 -15 -15 -15 -15 250 250 254 063 063 063 096 -15 252 251 254 -15 -15
                                                        -15 -15 -15 -15 -15 -15 250 -15 250 254 063 063 063 096 -15 254 252 008 -15 -15
                                                        254 254 254 254 254 250 -15 -15 250 253 096 096 096 095 -15 254 254 149 254 254
                                                        254 254 254 254 250 -15 -15 -15 250 -15 254 -15 -15 -15 -15 254 254 149 254 254
                                                        254 254 254 250 -15 -15 -15 -15 250 -15 008 253 -15 -15 -15 -15 253 008 254 254
                                                        254 254 250 250 250 -15 -15 -15 250 254 254 251 253 -15 -15 253 251 254 254 254
                                                        254 254 254 254 250 -15 250 -15 250 254 254 254 008 149 149 008 254 254 254 254
                                                        254 254 254 254 250 -15 250 250 250 254 254 254 254 254 254 254 254 254 254 254
                                                        254 254 254 250 -15 250 254 254 250 254 254 254 254 254 254 254 254 254 254 254
                                                        254 254 254 250 -15 250 254 254 254 254 254 254 254 254 254 254 254 254 254 254
                                                        254 254 254 254 250 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254
                                                        254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254
                                                        -15 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 254 -15
                                                    )
                                                )
                                            )
			)
			'(end_image)
			'(mode_tile key mode)
		  )
		)
		'mode
	      )
	    )
     )
   )
  )
)


;;;;;;;;;;�����ɽ��й�������Ԫ��
;;;  ;;���ñ���ֵ
;;;(defun SetValue (key value)
;;; (if (assoc key DCLValues)
;;;  (setq DCLValues (subst
;;;                   (cons key value)
;;;                   (assoc key DCLValues)
;;;                   DCLValues
;;;                  )
;;;  )
;;;  (setq DCLValues (cons (cons key value) DCLValues))
;;; )
;;;)
;;;  ;;��ȡ����ֵ
;;;(defun GetValue (key / r)
;;; (if (assoc key DCLValues)
;;;  (cdr (assoc key DCLValues))
;;;  (cdr (assoc key default_dclvalues))
;;; )
;;;)
;;���ÿؼ�״̬��ʱ��֪������ô��о�������
;;;(defun SetCtrlStatus ()
;;; (EnableCtrls '("PN_1" 
;;;                "PN_2"
;;;              "SEL_PRT"
;;;                "PN_3_1"
;;;               "PN_3_2" "PN_3_3"
;;;               "lh" "wh"
;;;               "prtph" "Ah"
;;;               "makDate" "modDate"
;;;               "simDate"
;;;              )
;;; )
;;; (if (= "0" (get_tile "PN_1"))
;;;  (disableCtrls '("a1" "b1"))
;;; )
;;; (if (= "0" (get_tile "PN_2"))
;;;  (disableCtrls '("prtna" "prtma" "prtth"))
;;; )
;;; (if (= "1" (get_tile "PN_3_1"))
;;;  (disableCtrls '("lh" "wh"))
;;; )
;;; (if (= "1" (get_tile "PN_3_2"))
;;;  (disableCtrls '("lh" "wh"))
;;; )
;;;)

;;;��������
(defun batchplot_dcl_help (/ dcl_id helpcontext parse mc_getfile)
 (defun parse (str delim / lst pos)
  (setq pos (vl-string-search delim str));���ַ���������ָ���Ӵ�
  (while pos
   (setq lst (cons (substr str 1 pos) lst)
         str (substr str (+ pos 2))
         pos (vl-string-search delim str)
   )
  )
  (if (> (strlen str) 0)
   (setq lst (cons str lst))
  )
  (reverse lst)
 )
 (defun mc_getfile (files / tmplst x fn)
  (setq files (findfile files))
  (if files
   (progn
    (setq fn (open files "r"))
    (while (setq x (read-line fn))
     (setq tmplst (append
                   tmplst
                   (list x)
                  )
     )
    )
    (close fn)
    tmplst
   )
   nil
  )
 )  ;; main
 (setq helpcontext (vl-get-resource "Q3_Help"))
 (if helpcontext
  (progn
   (setq helpcontext (parse helpcontext "\n"))
   (foreach str helpcontext
    (setq nlist (cons (vl-string-subst "" "\r" str) nlist))
   )
   (setq helpcontext (reverse nlist))
  )    ;; else no resource found
  (setq helpcontext (mc_getfile "Q3_Help.txt"))
 )  ;; init dcl
 (setq dcl_id (load_dialog "MJZC.dcl"))
 (if (< dcl_id 0)
  (progn
   (alert "�����޷����ضԻ����ļ���")
   (exit)
  )
 )
 (new_dialog "q3_help" dcl_id)
 (start_list "HelpList")
 (mapcar
  'add_list
  helpcontext
 )
 (end_list)
 (ACTION_TILE "htm" "(Play (findfile \"star_collect.wav\"))(batchplot_help)")
 (start_dialog)
 (unload_dialog dcl_id)
)


(defun batchplot_help (/ filename fd str oIE)
 (setq oIE (vlax-create-object "InternetExplorer.Application"));����Ӧ�ó���������ʵ��
 (if oIE
  (progn
   (if (setq str (vl-get-resource "Q3_Help"));���� VLX �б���� .txt �ļ��е�����
    (progn
     (setq filename (vl-filename-mktemp "Q3.htm"));Ϊ��ʱ�ļ�����Ψһ���ļ���
     (setq fd (open filename "w"))
     (princ str fd)
     (close fd)
    )
    (setq filename (findfile "Q3_HELP.htm"))
   )
   (vlax-invoke-method oIE "navigate" filename);����ָ���� ActiveX ����
   (vlax-put-property oIE "visible" :vlax-true);���� ActiveX ���������
   (if str
    (vl-file-delete filename)	; delete temp file
   )
  )
  (batchplot_dcl_help)
 )
 (princ)
)

;;;;;;;;;;����Ԫ��
  ;;���Կؼ�
(defun disableCtrls (keylist / key)
 (foreach key keylist
  (mode_tile key 1)
 )
)
  ;;����ؼ�
(defun EnableCtrls (keylist / key)
 (foreach key keylist
  (mode_tile key 0)
 )
)

;;;���ݵ������Ϣ
(defun Bak_PRT (/ px1 px2 py2 shujlb)
(setq ss bak_ss)
(if (/= ss nil)
(setq 

;;;mx1����a1 ������pt2    
;;;   ��         ��         
;;;    b1 ����ͼ  b1       
;;;   ��         ��         
;;;   ��         ��         
;;;pt1����a1 ������my1  

shulist (minmm_ssbox ss)
pt1 (car shulist )
pt2 (cadr shulist )
px1 (car pt1)		      ;;;ȡ��pt1�����xֵ
py1 (cadr pt1)	              ;;;ȡ��pt1�����yֵ
px2 (car pt2)		      ;;;ȡ��pt2�����xֵ
py2 (cadr pt2)		      ;;;ȡ��pt2�����yֵ
mx1  (list px1 py2)	      ;;;���mx1����ֵ
my1  (list px2 py1)	      ;;;���my1����ֵ  
a1   (distance mx1 pt2)       ;;;mx1��,pt2��֮��ľ��룺����ľ���ֵ����ͼ(��)
b1   (distance pt1 mx1)       ;;;pt1��,mx1��֮��ľ��룺����ľ���ֵ����ͼ(��) 
));;;end setq  
(princ)
)

;;;��ȡ�������Ϣ
(defun Get_PRT (/ zyd px1 px2 py2 shujlb) 
(setq zyd (ssget))
(setq ss (SSGET "p"  '((8 . "NCT"))));;;�б�עʱ,���˶���
(while (not ss)
 (alert "Sorry! Please select again!") 
 (command "change" zyd "" "p" "LA" "NCT" "")
 (setq zyd (ssget))
 (setq ss (SSGET "p" '((8 . "NCT"))));;;ͼ����
)
(setq bak_ss ss)
(command "_.select" zyd "")          ;;;ѡ�񡰱���ѡ�񼯡�
(setq zd (SSGET "P" '((2 . "a3"))))  ;;;�˴���ֵ����Ч

;;;��������Ϣ��
(if (/= ss nil)
(setq 

;;;mx1����a1 ������pt2    
;;;   ��         ��         
;;;    b1 ����ͼ  b1       
;;;   ��         ��         
;;;   ��         ��         
;;;pt1����a1 ������my1  

shulist (minmm_ssbox ss)
pt1 (car shulist )
pt2 (cadr shulist )
px1 (car pt1)		      ;;;ȡ��pt1�����xֵ
py1 (cadr pt1)	              ;;;ȡ��pt1�����yֵ
px2 (car pt2)		      ;;;ȡ��pt2�����xֵ
py2 (cadr pt2)		      ;;;ȡ��pt2�����yֵ
mx1  (list px1 py2)	      ;;;���mx1����ֵ
my1  (list px2 py1)	      ;;;���my1����ֵ  
a1   (distance mx1 pt2)       ;;;mx1��,pt2��֮��ľ��룺����ľ���ֵ����ͼ(��)
b1   (distance pt1 mx1)       ;;;pt1��,mx1��֮��ľ��룺����ľ���ֵ����ͼ(��) 
));;;end setq  

(if (/= zd nil)
(progn
  (setq shujlb (MJ:GetAttributes (cdar (entget (ssname zd 0)))))
  (setq
    NA (nth 1 (nth 5 shujlb));;;�������4
    MA (nth 1 (nth 11 shujlb));;;�������9
    TH (nth 1 (nth 12 shujlb));;;������10
  )))
  (princ)
  )

;;;������������
(defun Nu_PRT (/ ss)
  (princ "\n>>>�Ű�������鹤��...")
  (while (setq ss (ssget (list (cons 0 "INSERT"))))
    (alert
      (princ (strcat "��ѡʵ��������Ϊ��" (rtos (sslength ss) 2 0)))
    )
  )
  (princ)
)

;;;�������㣡
(defun sumDA (/ ss i lis ent en s es th_Y)
  (prompt "\n>>>���������ʽ?(�ı��ࡢ��ע��������־��ɣ�)") 
  (setq ss (ssget '((-4 . "<OR")(0 . "DIMENSION")(0 . "MTEXT")(-4 . "OR>"))))
  (setq	i   0
	lis '()
  )
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (if (and (= (cdr (assoc 0 (entget ent))) "DIMENSION")
	     (= (cdr (assoc 1 (entget ent))) "")
	)
    (setq s (list (cdr (assoc 42 (entget ent)))))
    (setq s (list (atof (cdr (assoc 1 (entget ent))))))          ;�����㷨?
;    (setq s (list (atof (str��No (cdr (assoc 1 (entget ent))))))) �����㷨
    )
;    (setq en (cdr (assoc 1 (entget ent))))
;    (setq s (list (atof (str��No en))));(read (str��No en))������
    (setq lis (append s lis))
    (setq i (1+ i))
  )
  (setq lis (vl-sort lis '>))
  (setq es (eval (cons '+ lis)))
  (setq th_Y (getreal "\n��������۳�����չ��...\n<�ո�Ϊ���ֺ�ͳ��...>"))
  (if th_Y (setq es (- es (* th_Y (1- (sslength ss))))))
  (alert
    (princ (strcat "\n���Ϊ��" (rtos es 2 2)))
  )
)

;;;��������ȡ�ַ����е�����
(defun str��No (str /)
  (vl-list->string
    (vl-remove-if-not
      '(lambda (x) (and (> x 45) (< x 58)))
      (vl-string->list str)
    )
  )
)

;;;����������д����Ļ��(/ pt5 pt6 obj es)
(defun Write_txt (/ pt5 obj es enn i lis ent s new_prtth)
  (princ "\n>>>����ͼ��(�����/����)����...")
;;;  (while (progn
 
  (princ "\n��ͼ�������ڵ��һ��:")
;;;  (command "-BHATCH" pause "")
;;;  (setq pt5 (cadr (grread T 12 1)))
;;;  (setq pt6 (polar pt5 (/ pi -2.0) (* (* 3.5 (GETVAR "DIMSCALE")) 2)))
;;;  (setq obj (vlax-ename->vla-object (entlast)))
;;;  (setq es (vla-get-Area obj))
;;;  (entdel (entlast))
;;;--------------------------------------------  
  (setq enn (entlast)) ;;�������Ҫѡ��Ķ���
  (command "-BOUNDARY" pause "")
  (setq ss (lt:ss-entnext enn))
;;;  )
  (setq	i   0
	lis '()
  )
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (setq obj (vlax-ename->vla-object ent))
    (setq s (list (vla-get-Area obj)))
    (setq lis (append s lis))
    (setq i (1+ i))
  )
  (setq lis (vl-sort lis '>))
  (setq es (abs (eval (cons '- lis))))
  (setq pt5 (cadr (grread T 12 1)))
  (command "change" ss "" "p" "C" "T" "255,0,255" "")
;  (command "erase" ss "")
;;;-------------------------------------------- 
;;;  (princ "\n ��ǰ���Ϊ��<")
;;;  (princ prtth)
;;;  (setq new_prtth (getstring "\n�������µİ��<�ո�Ĭ�ϵ�ǰ...>:"))
;;;  (if (= new_prtth "")(setq new_prtth prtth))
;;;  (setq prtth new_prtth)
  
  (if (not (tblsearch "style" "CHINESE"))
    (command "_.style" "CHINESE" "΢���ź�" "" "0.8" "" "" "")
  )
  (command "_layer" "s" "MARK" "")
  (command "_.TEXT" "S" "CHINESE" pt5 (* 3.5 (GETVAR "DIMSCALE")) "" (princ (strcat "\n����< " MA "," " T = " TH)))
  (command "_.TEXT" "S" "CHINESE" "" (princ (strcat "\n������ = " (rtos es 2 2)"mm\U+00B2")))
  (COMMAND "_.TEXT" "S" "CHINESE" "" (princ (strcat "\n������� = " (rtos (* es 0.000001 (atof TH) midu) 2 8) "Kg")))
  (princ);;;)
)

;;;DCL�Ի����Ű���Ϣ
(DEFUN Dcl_MAK (/ cad D5 N1 N2 N3 N4 alpr spcpr suspr fpr P_S P_G)
(SETQ PH    (ATOF (GET_TILE "prtph"))
      enx   (ATOF (GET_TILE "EnX"))
      eny   (ATOF (GET_TILE "EnY"))
      str3  (ATOF (GET_TILE "Ah"))
      alpr  (ATOF (nth 8 D4))
      spcpr (ATOF (nth 10 D4))
      suspr (ATOF (nth 12 D4))
      fpr   (ATOF (nth 14 D4))
      nss   (GET_TILE "OPS")
      PN_11 (GET_TILE "PN_3_1")
      PN_22 (GET_TILE "PN_3_2")
      PN_33 (GET_TILE "PN_3_3")
)

(cond ((= (setq PN_11 (GET_TILE "PN_3_1")) "1") (setq str1 1220 str2 1220))
      ((= (setq PN_22 (GET_TILE "PN_3_2")) "1") (setq str1 2440 str2 1220))
      ((= (setq PN_33 (GET_TILE "PN_3_3")) "1") (setq str1 (ATOF (GET_TILE "lh"))
					              str2 (ATOF (GET_TILE "wh"))
 )))
 (if (and (/= bak_ss nil) (= nsn "")) (Bak_PRT))				   
 (if (or (not a1) (not b1) (not ss))
  (SETQ	a1 (ATOF (GET_TILE "A1")) b1 (ATOF (GET_TILE "B1"))))
 (if (or (= MA "") (= TH "") (not MA) (not zd))
  (setq NA (GET_TILE "prtna")
	MA (GET_TILE "prtma")
	TH (GET_TILE "prtth")))
;(if (or (= MA "AL") (= MA "AL6063") (= MA "AL5052"))   
;  (setq midu 2.7 vle alpr))     
;(if (or (= MA "SPCC") (= MA "SGCC") (= MA "SECC"))
;  (setq midu 7.85 vle spcpr))
;(if (or (= MA "SUS304") (= MA "SUS201") (= MA "SUS"))
;  (setq midu 7.9 vle suspr))
 (if (wcmatch MA "*��*,*AL*")   
   (setq midu 2.7 vle alpr))     
 (if (wcmatch MA "*Q*,*��*,*CC*")
   (setq midu 7.85 vle spcpr))
 (if (wcmatch MA "*��*,*SUS*")
   (setq midu 7.9 vle suspr))

(if (or (not MA) (not TH) (= TH "") (not vle))
  (setq	wenzi " ������ȱʧ�����Ϻ�����ֹ��"
	midu  0
	vle   0
	NA    ""
	MA    ""
	TH    ""
  )
  (setq wenzi (strcat " #���ϳɱ�����:(" MA "/" (rtos vle 2 1) "Ԫ)"))
)

(setq
 N1 (fix (/ (- str1 enx) (+ a1 PH)));;;����
 N2 (fix (/ (- str2 str3) (+ b1 PH)));;;����
 N3 (fix (/ (- str1 enx) (+ b1 PH)));;;����
 N4 (fix (/ (- str2 str3) (+ a1 PH)));;;����
 v1 (* 1.3 (/ (* a1 b1 ) 1000000 ) (atof TH) midu vle);;;���ϳɱ����㹫ʽ 
 v2 (* fpr (/ (* a1 b1 ) 1000000 ));;;��ĩ�������㹫ʽ  
 v3 (* N1 N2);;;�����Ű���㹫ʽ
 v4 (* N3 N4);;;�����Ű���㹫ʽ 
 wenza (strcat "�����ɱ�����:[��۵���:" (rtos fpr 2 1) "Ԫ]")
)
   
(if (>= v3 v4)
 (setq propose "��OK! ��ǰλ���Ű���ʡ��Ŷ!"
       xn      N1
       yn      N2
 )
 (setq propose "�ｨ�齫�����ת90���Ű�!"
       xn      N3
       yn      N4
 ));;;����Ļ����ʾ��ʾ��

(if (= ns nil)
 (progn
  (set_tile "XN" (rtos xn 2 0))
  (set_tile "YN" (rtos yn 2 0))
  (set_tile "A1" (rtos a1 2 2))
  (set_tile "B1" (rtos b1 2 2))
 )
)

 (SETQ xn  (atoi (GET_TILE "XN"))
       yn  (atoi (GET_TILE "YN"))
       P_S (/ (* a1 b1 xn yn) 1000000)
       P_G (* P_S (atof TH) midu))
 (set_tile "PS" (rtos P_S 2 5))
 (set_tile "PG" (rtos P_G 2 5))
 (setq cad (vl-filename-directory (findfile "acad.exe"))
       D5  (list (rtos str1 2 0)(rtos str2 2 0)(rtos str3 2 0) NA MA TH (rtos PH 2 0)(rtos enx 2 0)(rtos eny 2 0) PN_11 PN_22 PN_33 nss))
 (lst-fn (vl-string-translate "\\""/" (strcat cad "\\bak_Set.ini")) D5 "w")
)

;;;����Ű�ģ�⹦�ܲ���
(DEFUN PRT_MAK (/ bn DU shulist)
(if (not ss)
 (progn
  (setq	pt1 (list 0 0)
	my1 (polar pt1 0 a1)
	pt2 (polar my1 (angtof "90") b1)
	mx1 (polar pt1 (angtof "90") b1))
  (command "RECTANGLE" pt1 pt2)
  (setq ss (entlast)))
)
(if (not (tblsearch "style" "CHINESE"))
(command "_.style" "CHINESE" "PMingLiU" "" "0.8" "" "" "") )
(if (not (tblsearch "LAYER" "HATCH"))
(command "_.layer" "n" "HATCH" "c" "42" "HATCH" ""))
(command "_.layer" "s" "HATCH" "")

(setq enn (entlast));;�������Ҫѡ��Ķ���
(setq bn (rtos (getvar "cdate") 2 6))
(COMMAND "_.block" bn mx1 ss "")
(COMMAND "_.insert" bn mx1 "" "" "")
(setq ss (ENTLAST))
(if (>= v3 v4)
 (progn
  (setq DU 0)
  (if (= 1 yn)
   (command "_.ARRAY" ss "" "r" yn xn (rtos (+ a1 PH) 2 2))
   (command "_.ARRAY" ss "" "r" yn xn (rtos (- (+ b1 PH)) 2 2) (rtos (+ a1 PH) 2 2))
  ))
 (progn
  (setq	mx1 (polar mx1 0 a1) DU 90)
  (command "_.ARRAY" ss "" "r" xn yn (rtos (- (+ b1 PH)) 2 2) (rtos (- (+ a1 PH)) 2 2))
  )
)
(setq ss (lt:ss-entnext enn));;ѡ����ú��ʵ��
(if (= nsn "") (command "oops"))
(setq bn (rtos (GETVAR "TDINDWG") 2 10))
(command "block" bn mx1 ss "")
(command "_.insert" bn "R" DU pause "" "" "_EXPLODE" (ENTLAST))
(princ (strcat "\n" "*�Ű�����" (rtos (* xn yn) 2 0) "��*"))

(if (= "1" nss)
(progn
(setq pt3 (cadr (grread T 12 1)))
(setq P1 (LIST (- (car pt3) enx) (- (cadr pt3) eny)))
(command "RECTANGLE"
	 P1
	 (LIST (+ (CAR P1) str1) (- (CADR P1) str2))
	 "PLINE"
	 (LIST (CAR P1) (- (CADR P1) (- str2 str3)))
	 (LIST (+ (CAR P1) str1) (- (CADR P1) (- str2 str3)))
	 "")
(COMMAND "_.TEXT" "S" "CHINESE" P1 (* 20 (GETVAR "DIMSCALE")) ""(strcat "\n "NA" "MA" T= "TH" ""��"(ITOA xn)"*"(ITOA yn)"��" "�� ")))
);;;end if��
(princ)
)

;;;DCL�Ű���Ϣ���
(defun Dcl_out (/ out_id)
 (setq out_id (load_dialog "MJZC.dcl"))
 (if (not (new_dialog "q3_output" out_id "" '(1305 485)))
  (exit)
 )
 (set_tile "wenzi" wenzi)
 (set_tile "wenzA" wenza)
 (set_tile "v1" (rtos v1 2 1))
 (set_tile "v2" (rtos v2 2 1))
 (set_tile "str1" (rtos str1 2 1))
 (set_tile "str2" (rtos str2 2 1))
 (set_tile "v3" (rtos v3 2 0))
 (set_tile "v4" (rtos v4 2 0))
 (set_tile "propose" propose)
 (start_dialog)
 (unload_dialog out_id)
)

;;;�����������϶��壡
(defun PRT_DATE	(/ prt_id dd I_lst PN_11 PN_22 PN_33 P1 P2 P3 P4)
 (setq prt_id (load_dialog "MJZC.dcl"))
 (setq dd 1)
 (IF (findfile "bak_Set.ini")
  (progn
   (setq D5 (fn-lst "bak_Set.ini"))
   (setq PN_11 (nth 9 D5)
	 PN_22 (nth 10 D5)
	 PN_33 (nth 11 D5)
         nss   (nth 12 D5)   
	 NA    (nth 3 D5)  
	 MA    (nth 4 D5)
	 TH    (nth 5 D5)
	 PH    (ATOF (nth 6 D5))
	 str1  (ATOF (nth 0 D5))
	 str2  (ATOF (nth 1 D5))
	 str3  (ATOF (nth 2 D5))
	 enx   (ATOF (nth 7 D5))
	 eny   (ATOF (nth 8 D5))
   )
  )
  (setq PN_11 "1" PN_22 "0" PN_33 "0" nss "0" NA "" MA "" TH "" PH 5 str1 2500 str2 1250 str3 80 enx 20 eny -10)
 )
 (while	(< dd 8);whileѭ����ʼ
 (if (not (new_dialog "q3_date" prt_id ""))
  (exit)
 )

 (zd-image "makDate" 3 '((9 11 13 11 7) (13 11 14 11 7) (14 11 15 10 7) (15 10 15 4 7) (15 4 14 3 7)
  (14 3 13 3 7) (13 3 3 3 7) (3 3 2 3 7) (2 3 2 4 7) (2 4 2 10 7) (2 10 2 11 7)
  (2 11 3 11 7) (3 11 4 11 7) (4 11 4 15 7) (9 11 4 15 7) (4 5 12 5 7) (12 5 12 6 7)
  (12 6 4 6 7) (4 6 4 5 7) (4 7 9 7 7) (9 7 4 7 7) (0 1 17 1 7) (17 1 17 18 7) (17 18 0 18 7)
  (0 18 0 1 7)))

 (start_image "Display")
 (slide_image 0 0 (dimx_tile "Display") (dimy_tile "Display") "Q3_D.sld")
 (end_image)

 ;���ñ༭��ؼ���ֵ
  (setq D4 (fn-lst "Pr_Set.ini"))
;  (setq D1 (read (nth 1 D4)))
;  (setq D2 (read (nth 3 D4)))
;  (setq D3 (read (nth 5 D4)))
;  (lst-dcl "prtna" D1 3 0)
;  (lst-dcl "prtma" D2 3 0)
;  (lst-dcl "prtth" D3 3 0)
;  (set_tile "prtna" (itoa NA))
;  (set_tile "prtma" (itoa MA))
;  (set_tile "prtth" (itoa TH))
  (if (= typ 1)(get_2 "prtna")(set_tile "prtna" NA))
  (if (= typ 2)(get_2 "prtma")(set_tile "prtma" MA))
  (if (= typ 3)(get_2 "prtth")(set_tile "prtth" TH))
  (SETQ typ nil)
  (set_tile "OPS" nss)
  (set_tile "PN_3_1" PN_11)
  (set_tile "PN_3_2" PN_22)
  (set_tile "PN_3_3" PN_33)
  (set_tile "prtph" (rtos PH 2 0))
  (set_tile "lh" (rtos str1 2 0))
  (set_tile "wh" (rtos str2 2 0))
  (set_tile "Ah" (rtos str3 2 0))
  (set_tile "EnX" (rtos enx 2 0))
  (set_tile "EnY" (rtos eny 2 0))

 (if (not ss)
  (EnableCtrls '("PN_1"))
  (progn
   (disableCtrls '("PN_1"))
   (set_tile "A1" (rtos a1 2 2))
   (set_tile "B1" (rtos b1 2 2))
  ))
 
 (if (or (not NA) (not MA) (not TH) (not zd) (= MA "") (= TH ""))
  (EnableCtrls '("PN_2"))
  (disableCtrls '("PN_2"))
 )
 
 (if (= "0" PN_33)
  (disableCtrls '("lh" "wh"))
 )

 (ACTION_TILE "A1" "(setq nsn $value)")
 (ACTION_TILE "XN" "(setq ns $value)")                                                                                                                  ;;;X��������
 (ACTION_TILE "YN" "(setq ns $value)")                                                                                                                  ;;;y��������
 (ACTION_TILE "prtna" "(Play (findfile \"button.wav\"))(setq NA $value)")                                                                                               ;;;��������
 (ACTION_TILE "prtma" "(Play (findfile \"button.wav\"))(setq MA $value)")                                                                                               ;;;��������
 (ACTION_TILE "prtth" "(Play (findfile \"button.wav\"))(setq TH $value)")                                                                                               ;;;�����Ϻ�
 (ACTION_TILE "PN_3_1" "(Play (findfile \"button.wav\"))(disableCtrls '(\"lh\" \"wh\"))")                                                                               ;;;��������1
 (ACTION_TILE "PN_3_2" "(Play (findfile \"button.wav\"))(disableCtrls '(\"lh\" \"wh\"))")                                                                               ;;;��������2
 (ACTION_TILE "PN_3_3" "(Play (findfile \"button.wav\"))(EnableCtrls '(\"lh\" \"wh\"))(mode_tile \"lh\" 2)(DONE_DIALOG 6)")                                             ;;;��������3
 (ACTION_TILE "OP" "(Play (findfile \"button.wav\"))(startapp \"notepad.exe\" (findfile \"Pr_Set.ini\"))")                                              ;;;�ײ���������
 (ACTION_TILE "OPS" "(Play (findfile \"button.wav\"))")                                                                                                 ;;;������ʾ��
 (ACTION_TILE "makDate" "(Play (findfile \"Change.wav\"))(Dcl_MAK)(Dcl_out)")                                                                           ;;;�Ű���Ϣ��ʾ
 (ACTION_TILE "picna" "(DONE_DIALOG 1)")
 (ACTION_TILE "picma" "(DONE_DIALOG 2)")
 (ACTION_TILE "picth" "(DONE_DIALOG 3)")
 (ACTION_TILE "SEL_PRT" "(Play (findfile \"menu back.wav\"))(Dcl_MAK)(DONE_DIALOG 4)")                                                                  ;;;ѡȡ����(1)
 (ACTION_TILE "pick1" "(Play (findfile \"button.wav\"))(DONE_DIALOG 5)")                                                                                ;;;����������(2)
 (ACTION_TILE "pick2" "(Play (findfile \"button.wav\"))(Dcl_MAK)(DONE_DIALOG 8)")                                                                       ;;;�������д����Ļ(5)
 (ACTION_TILE "simDate" "(Play (findfile \"menu back.wav\"))(Dcl_MAK)(DONE_DIALOG 9)")                                                                  ;;;����Ű�ģ�⹦�ܲ���(6)
 (ACTION_TILE "Display" "(Play (findfile \"button.wav\"))(DONE_DIALOG 10)")                                                                              ;;;���ⱨ�۳�ʽ(7)
 (ACTION_TILE "pick3" "(Play (findfile \"button.wav\"))(Dcl_MAK)(DONE_DIALOG 11)")                                                                       ;;;���ֺ�(8)
 (ACTION_TILE "cancel" "(DONE_DIALOG 12)")                                                                                                               ;;;�������ȡ��
 (ACTION_TILE "modDate" "(batchplot_dcl_help)")                                                                                                         ;;; �������
 (setq dd (start_dialog))
 (cond ((= dd 1) (setq typ 1) (get_1))                                                                                                                  ;;;ѡȡ����(1)
       ((= dd 2) (setq typ 2) (get_1))                                                                                                                  ;;;����������(2)
       ((= dd 3) (setq typ 3) (get_1))                                                                                                                  ;;;����������(2)
       
       ((= dd 4) (Get_PRT))                                                                                                                             ;;;ѡȡ����(1)
       ((= dd 5) (Nu_PRT))                                                                                                                              ;;;����������(2)
       
       ((= dd 6) (SETQ P1    (GETPOINT "\nѡ���ĵ�һ�ǵ�:")
		       p2    (getcorner p1 "\nѡ���ĵڶ��ǵ�:")
		       p3    (list (car p2) (cadr p1))
		       p4    (list (car p1) (cadr p2))
		       str1  (distance p1 p3)
		       str2  (distance p1 p4)
		       PN_33 "1"))                                                                                                                      ;;;�Զ���XY������ϳߴ�(4)
       
       ));whileѭ������
 
 (cond ((= dd 8) (Write_txt))                                                                                                                           ;;;�������д����Ļ(5)
       ((= dd 9) (PRT_MAK))                                                                                                                             ;;;����Ű�ģ�⹦�ܲ���(6)
       ((= dd 10) (laser))                                                                                                                               ;;;���ⱨ�۳�ʽ(7)
       ((= dd 11) (sumDA))                                                                                                                               ;;;���ֺ�(8)
       ((= dd 12) (if (findfile "bak_Set.ini") (vl-file-delete (findfile "bak_Set.ini")))(setq bak_ss nil)(PRINC "\n��л��ʹ�ñ���ʽ��ף��������죡")))
       
 (unload_dialog prt_id)
 (princ)
)

;;;���������
(vl-load-com)
(defun c:Q3( / lh wh Ah v1 v2 v3 v4 D1 D2 D3 D4 N1 N2 N3 N4 NA MA TH PH P1 a1 b1 kw mx1 my1 midu ns nss nsn vle layer_old osmode_old po pt1 pt2 pt3 px1 px2 py1 py2 shulist ss enn enx eny xn yn wenzi wenza zyd zd)
(setvar "cmdecho" 0)
(setq osmode_old (getvar "OSMODE")) ;;��׽����   
(setq  layer_old (getvar "CLAYER")) ;;ȡ��ǰͼ��
(setvar "osmode" 0)                 ;;�رն���׽ 
(if (or (not (findfile "MJZC.dcl"))(not (tblsearch "LAYER" "NCT")))
 (progn
  (alert "��ϵͳ��⵽����һ��ʹ��!!!\n�ｫ��ʼ����ʽ!��Ҫ�޸�����ϵͳ����\n��------------��ȷ���Լ���------------")
  (LOAD "Q3_Set.LSP")
 )
)
(PRT_DATE)

;;;ϵͳ������ԭ
(setvar "OSMODE" osmode_old)
(setvar "CLAYER" layer_old)
(princ) 
)

;;;--------------------------------------------------------
;;;;�����и�۳�ʽ�� 
(DEFUN laser (/ osmode_old layer_old Cal_id dd D2 D3 v1 v2 v3 v4 v5 MA TH prtma prtth NU al ss)	      
  (PRINC "\n�����и�۹���<<<")
  (setvar "cmdecho" 0)
  (setq osmode_old (getvar "OSMODE"));;��׽����   
  (setq layer_old (getvar "CLAYER")) ;;ȡ��ǰͼ��
  (setvar "osmode" 0)                ;;�رն���׽
  (setq Cal_id (load_dialog "MJZC.dcl"))
  (setq dd 1)
  (while (< dd 2) ;whileѭ����ʼ    
    (if	(not (new_dialog "q3_las" Cal_id ""))
      (exit)
    )
;    (setq D4 (fn-lst "Pr_Set.ini"))
;    (setq D2 (read (nth 3 D4)))
;    (setq D3 (read (nth 5 D4)))
    (setq D2 '("��" "�����" "ͭ��"))
    (setq D3 '("1.0\U+2264" "1.2" "1.5" "2.0" "2.5" "3.0" "4.0" "5.0" "8.0" "10.0"))
    (lst-dcl "prtma" D2 3 0)
    (lst-dcl "prtth" D3 3 0)
    (if	v1
      (progn
	(set_tile "BaX" (rtos v1 2 1))
	(set_tile "BaY" (rtos v2 2 2))
	(set_tile "EnX" (rtos v3 2 3))
	(set_tile "EnY" (rtos v4 2 0))
	(set_tile "Av" (rtos v5 2 1))
	(set_tile "prtma" (itoa MA))
	(set_tile "prtth" (itoa TH))
      ))
    (ACTION_TILE "prtma" "(LAS_1)")
    (ACTION_TILE "prtth" "(LAS_1)")
    (ACTION_TILE "all" "(LAS_1)")
    (ACTION_TILE "makDate" "(LAS_1)(Dcl_data7)(DONE_DIALOG 1)")
    (ACTION_TILE "cancel" "(DONE_DIALOG 2)")
    (setq dd (start_dialog))
    (cond ((= dd 1) (Cal_data7)))
  );whileѭ������
  (unload_dialog Cal_id)
;;;ϵͳ������ԭ
  (setvar "OSMODE" osmode_old)
  (setvar "CLAYER" layer_old)
  (princ)
)

;;;Dcl����
(DEFUN Dcl_data7 (/)
  (SETQ
    al (GET_TILE "all")
    MA (ATOI (GET_TILE "prtma"))
    TH (ATOI (GET_TILE "prtth"))
    v1 (atof (GET_TILE "BaX"))
    v2 (atof (GET_TILE "BaY"))
  )
  (setq prtma (nth MA D2))
  (setq prtth (nth TH D3))

;  (if (wcmatch prtma "SPCC,SGCC,SECC,AL,AL6063,AL5052")
;  (progn
;    (setq NU 1)
;    (cond ((wcmatch prtth "1.0,1.2,1.5,2.0,2.5,3.0") (setq v2 0.15))
;	  ((wcmatch prtth "4.0,5.0") (setq v2 0.2))
;	  ((wcmatch prtth "6.0,8.0") (setq v2 0.3))
;	  ((wcmatch prtth "10.0") (setq v2 0.5)))))
;   (if (wcmatch prtma "SUS304,SUS201,SUS")
;  (progn
;    (setq NU 1.5)
;    (cond ((wcmatch prtth "1.0,1.2,1.5,2.0,2.5,3.0") (setq v2 0.5))
;	  ((wcmatch prtth "4.0") (setq v2 1))
;	  ((wcmatch prtth "5.0,6.0") (setq v2 1.5)))))
;  (setq v1 (* NU (atof prtth)))
)


;;;���ݻ�ȡ����
(DEFUN Cal_data7 (/ OBJ I TMP LEN SUM)
  (if (= "1" al)
    (progn
      (princ "\n���������۵�ȫ��ͼ�εİ��ݿ�:")
      (command "RECTANGLE" pause pause)
      (Cal_psg)
      (ssdel (ssname ss (1- (sslength ss))) ss)
    )
    (Cal_psg)
  )
  (if ss
    (progn ;;��ʼ��
	   (setq v4 (sslength ss))
	   (setq SUM 0
		 I 0
	   )
	   ;;������������ȡ
	   (repeat (sslength ss)
	     (setq OBJ (vlax-ename->vla-object (ssname ss I)))
	     (if (setq TMP (vlax-curve-getendparam OBJ))
	       (setq LEN (vlax-curve-getdistatparam OBJ TMP)
		     SUM (+ SUM LEN)
	       )
	       (princ (strcat "\\n��" (itoa (1+ I)) "������������")
	       )
	     )
	     (setq I (1+ I))
	   )
	   ;;��ʾ���
	   (if (= SUM 0)
	     (princ "\\nû����ȡ�����ȡ�")
	   )))
  (command "change" ss "" "p" "C" "T" "255,0,255" "")
  (setq v3 (* SUM 0.001)) ;;;����ת��Ϊ��
  (setq v5 (+ (* v1 v3) (* v2 v4)))
  (if (not (tblsearch "style" "CHINESE"))
    (command "_.style" "CHINESE" "΢���ź�" "" "0.8" "" "" "")
  )
  (command "_layer" "s" "MARK" "")
  (command "_.TEXT" "S" "CHINESE" (cadr (grread T 12 1))(* 3.5 (GETVAR "DIMSCALE"))""(princ (strcat "\n����< " prtma "," " T = " prtth)))
  (command "_.TEXT" "S" "CHINESE" ""(princ (strcat "\n�и��ܱ��� = " (rtos v5 2 1) "Ԫ")))
;  (command "_.TEXT" "S" "CHINESE" (cadr (grread T 12 1))(* 3.5 (GETVAR "DIMSCALE"))""(strcat "�и��ܱ��� = " (rtos v5 2 1) "Ԫ"))
  (princ)
)


;;;���ݶ���Ԥ����
(DEFUN Cal_psg (/ enn DATA_RESULT)
  (princ "\n�ڰ��ݿ������ڵ��һ��:")
  (setq enn (entlast)) ;;�������Ҫѡ��Ķ���
  (command "-BOUNDARY" pause "")
  (setq ss (lt:ss-entnext enn))
  (PRINC)
)
;;;Laser���ۼ������
(DEFUN LAS_1 (/ X Y DBFILE FP RECODE GRP) 
  (SETQ DBFILE (findfile "LAST.LDB"))
  (SETQ FP (OPEN DBFILE "r"))
  (SETQ GRP '())
  (READ-LINE FP)
  (READ-LINE FP)
  (SETQ RECODE (READ-LINE FP))
  (WHILE (/= RECODE NIL)
    (SETQ GRP (CONS (READ RECODE) GRP))
    (SETQ RECODE (READ-LINE FP))
  )
  (CLOSE FP)
  (SETQ GRP (REVERSE GRP))		;��ȡDATAFILE

  (SETQ	X (ATOI (GET_TILE "prtma"))
	Y (ATOI (GET_TILE "prtth"))
  )					;ʹ��ATOF����ʹ�÷�Χ
  (SETQ X (+ X 1))			;��������ṹ����ָ��
  (SETQ DATA_RESULT (NTH X (NTH Y GRP)))
  (IF (NUMBERP DATA_RESULT)
    (SETQ DATA_RESULT (RTOS DATA_RESULT 2 2))
  )
  (IF (= DATA_RESULT "F")
    (SETQ DATA_RESULT "�޴����¼")
  )
  (SET_TILE "BaX" (substr DATA_RESULT 1 4))
  (SET_TILE "BaY" (substr DATA_RESULT 6))
)

;;;--------------------------------------------------------
;;;����:Q4
;;;--------------------------------------------------------
;;;��������:2015.06.27
;;;����  :֣�Ѷ�
;;;˵��:������ƥ�����
;;;--------------------------------------------------------
(vl-load-com)
(defun c:Q4(/ typ osmode_old prt_id dd I_lst)	       
(setvar "cmdecho" 0)
(setq osmode_old (getvar "OSMODE")) ;;��׽����
(setvar "osmode" 0)   ; �رն���׽  
(setq prt_id (load_dialog "MJZC.dcl"))
  (setq	dd  1 I_lst '("ina" "idw" "ive" "ino" "ima" "ith" "isu"))
  (while (< dd 8)			
    (if	(not (new_dialog "q4" prt_id ""))
      (exit)
    )

    ((mode_image I_lst) "ina" 0)
    ((mode_image I_lst) "idw" 0)
    ((mode_image I_lst) "ive" 0)
    ((mode_image I_lst) "ino" 0)
    ((mode_image I_lst) "ima" 0)
    ((mode_image I_lst) "ith" 0)
    ((mode_image I_lst) "isu" 0)

    (if(not da1)(setq da1 ""))
    (if(not da2)(setq da2 ""))
    (if(not da3)(setq da3 ""))
    (if(not da4)(setq da4 ""))
    (if(not da5)(setq da5 ""))
    (if(not da6)(setq da6 ""))
    (if(not da7)(setq da7 ""))
    (if	(= typ 1)(get_2 "na")(set_tile "na" da1))
    (if	(= typ 2)(get_2 "dw")(set_tile "dw" da2))
    (if	(= typ 3)(get_2 "ve")(set_tile "ve" da3))
    (if	(= typ 4)(get_2 "no")(set_tile "no" da4))
    (if	(= typ 5)(get_2 "su")(set_tile "su" da5))
    (if	(= typ 6)(get_2 "ma")(set_tile "ma" da6))
    (if	(= typ 7)(get_2 "th")(set_tile "th" da7))
    
    (ACTION_TILE "na" "(setq da1 $value)")
    (ACTION_TILE "dw" "(setq da2 $value)")
    (ACTION_TILE "ve" "(setq da3 $value)")
    (ACTION_TILE "no" "(setq da4 $value)")
    (ACTION_TILE "su" "(setq da5 $value)")
    (ACTION_TILE "ma" "(setq da6 $value)")
    (ACTION_TILE "th" "(setq da7 $value)")
    (ACTION_TILE "ina" "(DONE_DIALOG 1)")
    (ACTION_TILE "idw" "(DONE_DIALOG 2)")
    (ACTION_TILE "ive" "(DONE_DIALOG 3)")
    (ACTION_TILE "ino" "(DONE_DIALOG 4)")
    (ACTION_TILE "isu" "(DONE_DIALOG 5)")
    (ACTION_TILE "ima" "(DONE_DIALOG 6)")
    (ACTION_TILE "ith" "(DONE_DIALOG 7)") 
    (ACTION_TILE "accept" "(Play (findfile \"menu back.wav\"))(DONE_DIALOG 8)")
    (ACTION_TILE "cancel" "(DONE_DIALOG 9)")

    (setq dd (start_dialog))
    (cond ((= dd 1) (setq typ 1) (get_1))
	  ((= dd 2) (setq typ 2) (get_1))
	  ((= dd 3) (setq typ 3) (get_1))
	  ((= dd 4) (setq typ 4) (get_1))
	  ((= dd 5) (setq typ 5) (get_1))
	  ((= dd 6) (setq typ 6) (get_1))
	  ((= dd 7) (setq typ 7) (get_1))
    )
  )					;whileѭ������

  (cond	((= dd 8) (E_date))
	((= dd 9) (setq da1 nil da2 nil da3 nil da4 nil da5 nil da6 nil da7 nil)(PRINC "\n��л��ʹ�ñ���ʽ��ף��������죡"))
  )

  (unload_dialog prt_id)
;;;ϵͳ������ԭ
  (setvar "OSMODE" osmode_old)
  (command "ucs" "w")
  (princ)
)
;;;--------------------------------------------------------
(defun get_1 (/ esel edata)
  (if (setq esel (nentsel "\nѡ������"))
    (progn
      (setq edata (entget (car esel)))
      (if (and (= (length esel) 2) (= (cdr (assoc 0 edata)) "ATTDEF"))
	(setq date (cdr (assoc 2 edata)))
	(setq date (cdr (assoc 1 edata)))
      )
    )
    (cond ((= typ 1) (setq date "Name-?"))
	  ((= typ 2) (setq date "SPCC-?"))
	  ((= typ 3) (setq date "1.00-?"))
    )
  )
)

;;;--------------------------------------------------------
(defun get_2 (edit / )
  (set_tile edit date)
  (mode_tile edit 2)
)
;;;--------------------------------------------------------
(defun G_date( / zyd ss zd new_na new_no new_si new_nu new_su new_su_len new_ma en_ma new_th shulist a1 b1 shujlb vle1 vle2 vle3 vle4 vle5 vle6 vle7 vle8 vle9 texth)
;(prompt "\n����ͼ����Ϣ�Ŀ���ƥ��!")
;(princ "\n�����ѡ����!")
(setq zyd (ssget))
(setq ss (SSGET "p"  '((8 . "NCT"))));;;�б�עʱ,���˶���
(command "_.select" zyd "")          ;;;ѡ�񡰱���ѡ�񼯡�
(setq zd (SSGET "P" '((2 . "a3,a4"))))  ;;;�˴���ֵ����Ч
(if (/= ss nil)
(setq 

;;;mx1����a1 ������pt2    
;;;   ��         ��         
;;;    b1 ����ͼ  b1       
;;;   ��         ��         
;;;   ��         ��         
;;;pt1����a1 ������my1  

shulist (minmm_ssbox ss)
pt1 (car shulist )
pt2 (cadr shulist )
px1 (car pt1)		      ;;;ȡ��pt1�����xֵ
py1 (cadr pt1)	              ;;;ȡ��pt1�����yֵ
px2 (car pt2)		      ;;;ȡ��pt2�����xֵ
py2 (cadr pt2)		      ;;;ȡ��pt2�����yֵ
mx1  (list px1 py2)	      ;;;���mx1����ֵ
my1  (list px2 py1)	      ;;;���my1����ֵ  
a1   (distance mx1 pt2)       ;;;mx1��,pt2��֮��ľ��룺����ľ���ֵ����ͼ(��)
b1   (distance pt1 mx1)       ;;;pt1��,mx1��֮��ľ��룺����ľ���ֵ����ͼ(��) 
));;;end setq

;;;������������

(progn
  (setq shujlb (cdar (entget (ssname zd 0))))
  (repeat 5
    (setq shujlb (entnext shujlb))
  )
  (setq prtna (entnext shujlb));;;�������
  (setq na (entget prtna))
  (setq vle1 (assoc 1 na))

  (setq prtno (entnext (entnext prtna)));;;���ͼ��
  (setq no (entget prtno))
  (setq vle2 (assoc 1 no))

  (setq prtsi (entnext prtno));;;�ͻ��汾
  (setq si (entget prtsi))
  (setq vle3 (assoc 1 si))
   
  (setq prtnu (entnext prtsi));;;��������
  (setq nu (entget prtnu))
  (setq vle4 (assoc 1 nu))
  
  (setq prtsu (entnext prtnu));;;���洦��
  (setq su (entget prtsu))
  (setq vle5 (assoc 1 su))
  
  (setq prtma (entnext prtsu));;;�������
  (setq ma (entget prtma))
  (setq vle6 (assoc 1 ma))
  
  (setq prtth (entnext prtma));;;������
  (setq th (entget prtth))
  (setq vle7 (assoc 1 th))

  (repeat 8
    (setq shujlb (entnext shujlb))
  )
  (setq prtlh (entnext shujlb));;;չ����(7)
  (setq lh (entget prtlh))
  (setq vle8 (assoc 1 lh))

  (setq prtwh (entnext prtlh));;;չ����
  (setq wh (entget prtwh))
  (setq vle9 (assoc 1 wh))

(if (and (/= da1 nil) (/= da1 ""))
(progn
  (setq new_na (cons 1 da1))
  (entmod (subst new_na vle1 na))
  ));;;�������

(if (and (/= da2 nil) (/= da2 ""))
(progn
  (setq new_no (cons 1 da2))
  (entmod (subst new_no vle2 no))
  ));;;���ͼ��

(if (and (/= da3 nil) (/= da3 ""))
(progn
  (setq new_si (cons 1 da3))
  (entmod (subst new_si vle3 si))
  ));;;�ͻ��汾

(if (and (/= da4 nil) (/= da4 ""))
(progn
  (setq new_nu (cons 1 da4))
  (entmod (subst new_nu vle4 nu))
  ));;;��������

(if (and (/= da5 nil) (/= da5 ""))
(progn
  (setq new_su (cons 1 da5))
  (entmod (subst new_su vle5 su))
  ));;;���洦��

(if (and (/= da6 nil) (/= da6 ""))
(progn
  (setq new_ma (cons 1 da6))
  (entmod (subst new_ma vle6 ma))
  ));;;�������

(if (and (/= da7 nil) (/= da7 ""))
(progn
  (setq new_th (cons 1 da7))
  (entmod (subst new_th vle7 th))
  ));;;������
(if a1
  (progn
  (entmod (subst (cons 1 (rtos a1 2 1)) vle8 lh))  ;;;չ����
  (entmod (subst (cons 1 (rtos b1 2 1)) vle9 wh))));;;չ����
  (entupd shujlb)
)
(prin1)
)
;;;--------------------------------------------------------
(defun E_date (/ wxwez dwg path str filename f1)
 (prompt "\n�˳�������ͼ����Ϣ�Ŀ���ƥ��!")
 (princ "\n�����ѡ����!�������ַ����жϳ���!")
 (if (= (getstring) "")
 (G_date)
 (progn 
  (setq	wxwez (STRCAT da1 "\t" da2 "\t" da3 "\t" da4 "\t" da5 "\t" da6 "\t" da7 "\n")
	dwg   (vl-filename-base (getvar "DWGNAME")); ȡ��ǰ�ĵ���
	path  (getvar "dwgprefix")                 ; ȡ��ǰ�ĵ�·��
	str   "")	           
  (setq filename (strcat path dwg ".xlsx"))
  (setq f1 (open filename "a"))                    ; ѡ��дģʽ��һ�ı��ļ���
  (princ str f1)
  (close f1)
  (setq str (strcat str wxwez))
  (dos_speaker 3000 100)
  (SET-CLIP-STRING str)
  (vl-file-delete filename)
  (prompt "\n������ж�!�����ѱ��ݿ�ճ����BOM��!") 
)))


;;;--------------------------------------------------------
;;;����:Q44
;;;--------------------------------------------------------
;;;��������:2013.03.19
;;;����  :֣�Ѷ�
;;;˵��:��ϵͳ������д�빤��ָ��

;;;���ݲɼ��ӳ���A
(defun tolia (/ n ent crtlst x)
 (setq n 0 ent "")
 (repeat (sslength ss)
  (setq crtlst (append
                crtlst
                (list (cdr (assoc 1 (entget (ssname ss n)))))
               )
        n (1+ n)
  )
 )
 (foreach x (reverse crtlst)
  (setq ent (strcat ent (strcat x "\t")))
  )
 (setq tep10 (vl-string-trim "\t" ent))
)
;;;���ݲɼ��ӳ���B
(defun tolib (/ n ent crtlst x)
 (setq n 0 ent "")
 (repeat (sslength ss)
  (setq crtlst (append
                crtlst
                (list (cdr (assoc 1 (entget (ssname ss n)))))
               )
        n (1+ n)
  )
 )
 (foreach x (reverse crtlst)
  (setq ent (strcat ent (strcat x "-")))
  )
 (setq tep10 (vl-string-trim "-" ent))
)

;;;ͨ�ú��� 
(defun SET-CLIP-STRING (STR / HTML RESULT)
  (and
    (= (type STR) 'STR)
    (setq HTML (vlax-create-object "htmlfile"))
    (setq
      RESULT (vlax-invoke
	       (vlax-get (vlax-get HTML 'PARENTWINDOW) 'CLIPBOARDDATA)
	       'SETDATA
	       "Text"
	       STR
	     )
    )
    (vlax-release-object HTML)
  )
)
;;;;ͨ�ú�������
(vl-load-com)
(defun c:Q44 (/ osmode_old zyd ss zd shulist prtna tep1 tep2 tep3 tep4 tep5 tep6 tep7 tep8 tep9 tep10 wxwez filename str)
; (load "MJZC.lsp")
 (setvar "cmdecho" 0)                   ;�رջ���
 (setq osmode_old (getvar "OSMODE"))	;��׽����
 (setvar "osmode" 0)			;�رն���׽
 (prompt "\n�˳�������BOM���ݵĿ��ٸ���!")
 (prompt "\n��ѡ��ͼ��͹�������!")
 (setq str "")
 (setq m 0)
 (while (setq zyd (ssget))
  (setq ss (SSGET "p" '((0 . "text"))))
  (command "_.select" zyd "")
  (setq zd (SSGET "P" '((2 . "*a[345]*"))))
  (if zd
   (setq shujlb (MJ:GetAttributes (cdar (entget (ssname zd 0))))
         tep1 (nth 1 (nth 5 shujlb))	;�������
         tep2 (nth 1 (nth 7 shujlb))	;���ͼ��
         tep3 (nth 1 (nth 8 shujlb))	;����汾
         tep4 (nth 1 (nth 9 shujlb))	;�������
         tep5 (nth 1 (nth 10 shujlb))	;���洦��
         tep6 (nth 1 (nth 11 shujlb))	;�������
         tep7 (nth 1 (nth 12 shujlb))	;������
         tep8 (nth 1 (nth 13 shujlb))	;�������
         tep9 (nth 1 (nth 14 shujlb))	;������

   )
   (setq tep1 "" tep2 "" tep3 "" tep4 "" tep5 "" tep6 "" tep7 "" tep8 "" tep9 "")
  )
  (if ss
   (progn
    (if (= (cdr (assoc 8 (entget (ssname ss 0)))) "MARK")
     (tolia)
    )
    (if (= (cdr (assoc 8 (entget (ssname ss 0)))) "FORM")
     (tolib)
    )
   )
   (setq tep10 "")
  )
  (setq wxwez (STRCAT tep1 "\t" tep2 "\t" tep3 "\t" tep4 "\t" tep5 "\t" tep6 "\t" tep7 "\t" tep8 "\t" tep9 "\t" "\t" "\t" "\t" "\t" "\t" tep10 "\n"))
  (setq m (1+ m))
 ;(setq str (strcat (itoa m) "\t" prtna "\t" wxwez))  
  (setq dwg (vl-filename-base (getvar "DWGNAME")) ; ȡ��ǰ�ĵ���
        path (getvar "dwgprefix")        ; ȡ��ǰ�ĵ�·��

  )
  (setq filename (strcat path dwg ".xlsx"))
  (setq f1 (open filename "a"));;; ѡ��дģʽ��һ�ı��ļ���
 ;(write-line str f1)
  (princ str f1)
  (close f1)
  (setq str (strcat str wxwez))
  (dos_speaker 3000 100)
  (prompt (strcat "\n��ѡ��" (itoa m) "��"))
  (prompt "\n����ѡ����һ��ͼ��͹������֣�")
 )
  (SET-CLIP-STRING str)
 ;(startapp "C:/Program Files/Microsoft Office/OFFICE11/EXCEL.EXE" filename) ;;;ϵͳ������ԭ
  (vl-file-delete filename)
  (setvar "OSMODE" osmode_old)
  (prompt "\n���˳�ѡȡ������BOM��༭��!")
  (princ);��Ĭ�˳�
 )
;;--------------------------------------------------------
;;;ͼԪ�ϲ�����ɾ���ظ�ͼԪ
(vl-load-com)
(defun c:Q5 (/ ARC_LIST ENT I LINE_LIST SS)
  (while (and
    (setq ss (ssget (list (cons -4 "<or")
     (cons 0 "arc")
     (cons 0 "CIRCLE")
     (cons 0 "line")
     (cons -4 "or>")
      )
      )
    )
    (> (sslength ss) 0)
  )
    (hbzhx ss)
    )
  (princ)
  )
(defun cs_pross (to i / CS_TEXT MYI)
  (setq cs_text ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
  (setq myi (fix (/ (* (strlen cs_text) i) to))
 cs_text (substr cs_text 1 myi)
 )
  (grtext -2 cs_text)
  )

(defun hbzhx ( ss / ARC_LIST ENT I LINE_LIST SS jd)
;;;     תΪ���ݱ�
     (grtext -2 "������������")
     (setq i 0 jd 1e-5
    line_list ' ()
    arc_list '()
    )
     (repeat (sslength ss)
       (setq ent (ssname ss i)
      i (1+ i)
      )
       (if (= "LINE" (cdr (assoc 0 (entget ent))))
  (setq line_list (cons (line_data ent) line_list))
  (setq arc_list (cons (arc_data ent) arc_list))
  )
       )
     (setq line_list (vl-sort line_list
        '(lambda (e1 e2)
    (if (equal (car e1) (car e2) jd)
      (if (equal (cadr e1) (cadr e2) jd)
        (if (equal (car (caddr e1)) (car (caddr e2)) jd)
          (< (cadr (caddr e1)) (cadr (caddr e2)))
          (< (car (caddr e1)) (car (caddr e2)))
          )
        (< (cadr e1) (cadr e2))
        )
      (< (car e1) (car e2))
    )
         )
      )
    )
     (setq arc_list (vl-sort arc_list
        '(lambda (e1 e2)
    (if (equal (car e1) (car e2) jd)
      (if (equal (cadr e1) (cadr e2) jd)
        (if (equal (caddr e1) (caddr e2) jd)
          (< (cadddr e1) (cadddr e2))
          (< (caddr e1) (caddr e2))
          )
        (< (cadr e1) (cadr e2))
        )
      (< (car e1) (car e2))
    )
         )
      )
    )
     (if line_list (hb_line line_list jd))
     (if arc_list (hb_arc arc_list jd))
     (grtext)
  (princ)
  )
(defun hb_line (line_list jd / B BIAOJI DATA ENT K LINE_A LINE_B P1 P2 P3 P4 P5 JD XUHAO ZONGSHU i lay)
  (setq zongshu (length line_list)
 i 0
 xuhao 0)
  (princ (strcat "\n������" (rtos zongshu) "��ʵ��"))
  (grtext -1 "�ϲ�ֱ��")
  (while (> (length line_list) 0)    
       (setq xuhao (1+ xuhao))
       (cs_pross zongshu xuhao)
    (setq line_a (car line_list)
   line_list (cdr line_list)
   biaoji t
   k (car line_a)
   b (cadr line_a)
   p1 (caddr line_a)
   p2 (cadddr line_a)
   ent (last line_a)
   lay (cdr (assoc 8 (entget ent)))
   )
    (while (and biaoji
  (> (length line_list) 0)
  )
      (setq line_b (car line_list)
     )
      (cond
 ((and (equal k (car line_b) jd)
       (equal b (cadr line_b) jd)
       (= lay (cdr (assoc 8 (entget (last line_b)))))
       )
  (setq p3 (caddr line_b)
        p4 (cadddr line_b)
        p5 (vl-sort (list p1 p2 p3 p4)
      '(lambda (e1 e2)
         (if (equal (car e1) (car e2) jd)
    (< (cadr e1) (cadr e2))
    (< (car e1) (car e2))
         )
       )
    )
        p4 (cadr p5)
  )
  (if (or (equal p1 p4 jd)
   (equal p3 p4 jd)
      )
    (progn
      (setq p1      (car p5)
     p2      (last p5)
     line_list (cdr line_list)
      )
      (entdel (last line_b))
      (setq xuhao (1+ xuhao))
      (cs_pross zongshu xuhao)
      (setq i (1+ i))
    )
    (setq biaoji nil)
  )
  )
 (t (setq biaoji nil))
 )
      )
    (setq data (entget ent)
   data (subst (cons 10 p1) (assoc 10 data) data)
   data (subst (cons 11 p2) (assoc 11 data) data)
   )
    (entmod data)
    )
  (princ (strcat "��ɾ����" (rtos i) "��ʵ��"))
  (princ)
  )
(defun hb_arc (arc_list jd / i ARC_A ARC_B BIAOJI BJ DATA EANGL EANGL1 ENT JD LINE_LIST P5 PC SANGL SANGL1 XUHAO ZONGSHU lay)
  (setq zongshu (length arc_list)
 xuhao 0
 i 0)
  (princ (strcat "\n������" (rtos zongshu) "��ʵ��"))
  (grtext -1 "�ϲ�Բ��")
  (while (> (length arc_list) 0)
       (setq xuhao (1+ xuhao))
       (cs_pross zongshu xuhao)
    (setq arc_a (car arc_list)
   arc_list (cdr arc_list)
   biaoji t
   bj (car arc_a)
   pc (list (cadr arc_a) (caddr arc_a))
   sangl (cadddr arc_a)
   eangl (nth 4 arc_a)
   ent (last arc_a)
   lay (cdr (assoc 8 (entget ent)))
   )
    (while (and biaoji
  (> (length arc_list) 0)
    )
      (setq arc_b (car arc_list)
      )
      (cond
 ((and (equal bj (car arc_b) jd)
       (equal pc (list (cadr arc_b) (caddr arc_b)) jd)
       (= lay (cdr (assoc 8 (entget (last arc_b)))))
  )
  (setq sangl1 (cadddr arc_b)
        eangl1 (nth 4 arc_b)
        p5     (vl-sort (list sangl eangl sangl1 eangl1)
          '(lambda (e1 e2)
      (< e1 e2)
    )
        )
        sangl1 (nth (- (length p5) 2) p5)
  )
  (if (or (equal eangl sangl1 jd)
   (equal eangl1 sangl1 jd)
      )
    (progn
      (setq sangl    (car p5)
     eangl    (last p5)
     arc_list (cdr arc_list)
      )
      (entdel (last arc_b))
      (setq xuhao (1+ xuhao))
      (cs_pross zongshu xuhao)
      (setq i (1+ i))
    )
    (setq biaoji nil)
  )
 )
 (t (setq biaoji nil))
      )
    )
    (setq data (entget ent)
   data (subst (cons 50 sangl) (assoc 50 data) data)
   data (subst (cons 51 eangl) (assoc 51 data) data)
   )
    (entmod data)
    )
  (princ (strcat "��ɾ����" (rtos i) "��ʵ��"))
  (princ)
  )
(defun arc_data (ent / BJ DATA EANGL PC SANGL)
  (setq data (entget ent))
  (setq bj (cdr (assoc 40 data)))
  (setq pc (cdr (assoc 10 data)))
  (setq sangl (cdr (assoc 50 data)))
  (setq eangl (cdr (assoc 51 data)))
  (if (not sangl)
    (setq sangl 0.0
   eangl (+ pi pi)
    )
  )
  (if (< eangl sangl)
    (setq eangl (+ eangl (+ pi pi)))
  )
  (list bj (car pc) (cadr pc) sangl eangl ent)
)

  (defun line_data (ent / B K P1 P2 jd)
    (setq p1 (vlax-curve-getstartpoint ent)
   p2 (vlax-curve-getendpoint ent)
   jd 1e-5
    )
    (if (equal (car p1) (car p2) jd)
      (setq k nil
     b (car p1)
      )
      (setq k (/ (- (cadr p2) (cadr p1))
   (- (car p2) (car p1))
       )
     b (- (cadr p1) (* (car p1) k))
      )
    )
    (setq p2 (vl-sort (list p1 p2)
        '(lambda (e1 e2)
    (if (equal (car e1) (car e2) jd)
      (< (cadr e1) (cadr e2))
      (< (car e1) (car e2))
    )
         )
      )
   p1 (car p2)
   p2 (cadr p2)
   )
    (list k
   b
   (list (car p1) (cadr p1))
   (list (car p2) (cadr p2))
   ent
    )
  )


---------------------------------------------------------------
;;;�ڽ��㴦���ͼԪ
  (defun c:Q6 (/ elist ssg n t0)
  (VL-LOAD-COM)
  (setq t0 (xdl-getutime))
  (if (setq ssg (ssget '((0 . "line,arc,circle,ellipse"))))
    (vlax-for obj (vla-get-activeselectionset
      (vla-get-activedocument (vlax-get-acad-object))
    )
      (setq elist (cons obj elist)) ; ssg->elist
    )
  )
  (DoEntMake (InterSort (ssinter elist)))
  (princ (strcat "\n*****�ҵ�����"
   (itoa n)
   "��������Ͽ���������ʱ"
   (rtos (- (xdl-getutime) t0) 2 3)
   "�롣*****"
  )
  )
  (princ)
)
;;�󽻵㼯������nth
;;�������ԣ�nth��������assoc������һ��㡣
;;�ʴ˺���Ҳ��ȡ��i��j������ֱ��ʹ��assoc����
(defun ssinter (el / el1 obj1 obj2 ipts pts list1 outlst i j)
  (setq outlst (mapcar 'list el)
 i      -1   ;obj1λ��ָ�� 
 n      0   ;������������
  )
  (while el
    (setq obj1 (car el)
   list1 (nth (setq i (1+ i)) outlst) ;obj1���еĽ����б�
   el (cdr el)
   el1 el
   j i   ;obj2λ��ָ��
    )
    (while el1
      (setq obj2 (car el1)
     el1  (cdr el1)
     j  (1+ j)
      )
      ;;ȡ����
      (if (and (setq ipts (vla-intersectwith obj1 obj2 0))
        (setq ipts (vlax-variant-value ipts))
        (> (vlax-safearray-get-u-bound ipts 1) 0)
   )
 (progn
   (setq ipts (vlax-safearray->list ipts)
  pts  '()  ;obj1,obj2������ʱ�б����
   )
   (while (> (length ipts) 0)
     (setq pts  (cons (list (car ipts)
       (cadr ipts)
       (caddr ipts)
        )
        pts
         )
    ipts (cdddr ipts)
     )
   )
   (setq list1 (append list1 pts) ;��obj1�����ѭ���������ٸ���
  n     (+ n (length pts)) ;��������ۼ�
   )
   ;;obj2�Ľ����б���������
   (setq
     outlst (subst (append (nth j outlst) pts)
     (nth j outlst)
     outlst
     )
   )
 )
      )
    )
    ;;��obj1���ڽ��㣬�ҷǷ�����ߣ�������˵�
    (if (and (cdr list1) (not (vlax-curve-isClosed obj1)))
      (setq list1 (append list1
     (list (vlax-curve-getEndPoint obj1))
     (list (vlax-curve-getStartPoint obj1))
    )
      )
    )
    (setq outlst (subst list1 (nth i outlst) outlst)) ;����obj1�����б�
  )
  outlst
)
;;�㼯����ɾ���ظ��㺯��
(defun InterSort (el / obj1 pts plst outlst)
  (setq outlst '())   ;empty list
  (foreach item el
    (setq obj1 (car item)
   pts  (cdr item)
   plst '()   ;empty list
    )
    (if pts    ;���޽��㣬���޸ĸ�ʵ��
      (progn
 ;;���������б�Ϊ����
 (setq
   pts (vl-sort
  pts
  (function (lambda (p1 p2)
       (< (vlax-curve-getParamAtPoint obj1 p1)
          (vlax-curve-getParamAtPoint obj1 p2)
       )
     )
  )
       )
 )
 ;;�޳��ظ��㲢���б�˳��ת��
 (foreach p pts
   (if plst
     (if (not (equal p (car plst) 0.00001))
       (setq plst (cons p plst))
     )
     (setq plst (cons p plst))
   )
 )
 ;;�պ�������������׸�������ʹ��ʵ����ȫ���
 (if (vlax-curve-isClosed obj1)
   (setq plst (cons (last plst) plst))
 )
 (setq plst   (cons (vlax-vla-object->ename obj1) plst)
       outlst (cons plst outlst)
 )
      )
    )
  )
  outlst
)
;;����entmake������ʵ��
(defun DoEntMake (el / obj objlst objname objcen objratio objaxis)
  (foreach e el
    (setq obj   (car e)
   objlst  (entget obj)
   objlst  (vl-remove (assoc -1 objlst) objlst) ;ȥ��ͼԪ��
   objlst  (vl-remove (assoc 330 objlst) objlst) ;ȥ��id
   objlst  (vl-remove (assoc 5 objlst) objlst) ;ȥ�����
   objname (cdr (assoc 0 objlst))
    )
    (cond
      ((= objname "LINE")
       (repeat (- (length e) 2)
  (setq e (cdr e))
  (setq objlst (subst (cons 10 (car e)) (assoc 10 objlst) objlst))
  (setq objlst (subst (cons 11 (cadr e)) (assoc 11 objlst) objlst))
  (entmake objlst)
       )
       (entdel obj)
      )
      ((= objname "CIRCLE")
       (setq objcen (cdr (assoc 10 objlst)))
       (setq objlst (subst (cons 0 "ARC") (assoc 0 objlst) objlst))
       (setq objlst (append objlst
       (list (cons 100 "AcDbArc")
      (cons 50 0.0)
      (cons 51 0.0)
       )
      )
       )
       (repeat (- (length e) 2)
  (setq e (cdr e))
  (setq objlst (subst (cons 50 (angle objcen (cadr e)))
        (assoc 50 objlst)
        objlst
        )
  )
  (setq objlst (subst (cons 51 (angle objcen (car e)))
        (assoc 51 objlst)
        objlst
        )
  )
  (entmake objlst)
       )
       (entdel obj)
      )
      ((= objname "ARC")
       (setq objcen (cdr (assoc 10 objlst)))
       (repeat (- (length e) 2)
  (setq e (cdr e))
  (setq objlst (subst (cons 50 (angle objcen (cadr e)))
        (assoc 50 objlst)
        objlst
        )
  )
  (setq objlst (subst (cons 51 (angle objcen (car e)))
        (assoc 51 objlst)
        objlst
        )
  )
  (entmake objlst)
       )
       (entdel obj)
      )
      ((= objname "ELLIPSE")
       ;;��ԲԲ��
       (setq objcen (cdr (assoc 10 objlst)))
       ;;��������ĵĳ���ʸ��
       (setq objaxis (cdr (assoc 11 objlst)))
       ;;�����볤��ı���
       (setq objratio (cdr (assoc 40 objlst)))
       (repeat (- (length e) 2)
  (setq e (cdr e))
  (setq objlst (subst (cons 41 (pt->param (cadr e) objcen objaxis objratio))
        (assoc 41 objlst)
        objlst
        )
  )
  (setq objlst (subst (cons 42 (pt->param (car e) objcen objaxis objratio))
        (assoc 42 objlst)
        objlst
        )
  )
  (entmake objlst)
       )
       (entdel obj)
      )
    )
  )
)
;;�����ʱ
(defun xdl-getutime ()
  (* 86400 (getvar "tdusrtimer"))
)
;;����Բ���߲���
(defun pt->param (pt cen axis ratio / ang param)
  (setq ang (- (angle cen pt) (angle '(0. 0. 0.) axis)))
  (cond ((= (cos ang) 0.0)  ;��ֹ��ĸcosΪ�����
  (if (> (sin ang) 0.0)
    (setq param (* 0.5 PI))
    (setq param (* 1.5 PI))
  )
 )
 ((= (sin ang) 0.0)
  (if (> (cos ang) 0.0)
    (setq param 0.0)
    (setq param PI)
  )
 )
 (T
  (setq param (atan (/ (sin ang) (* (cos ang) ratio))))
  (if (< (cos ang) 0.0)
    (setq param (+ pi param))
  )
 )
  )
  param
)
(princ)



;;;--------------------------------------------------------
;;;;CAD��CREOϵ��ת�����ߣ�
(defun c:Q7 (/ Y1 Y2 th r ra l)
  (princ "\n ALL PROGRAM WRITTEN BY Z.Y.D 2015/1/8 PM 17:04  \n")
  (princ "\n you can copy it anytimes, but not modify it!!")
  (PRINC "\n �����ڳߴ�ϵ����proeY����\(VERSION 0.1 \) . \n")
  (SETQ Y1 (getreal "\n�����������ڳߴ�ϵ��"))
  (SETQ th (getreal "\n�������Ϻ�!"))
  (SETQ r (getreal "\n�����������ڰ뾶Ĭ��0.1!"))
  (if (null r)
    (setq r 0.1)
  )
  (SETQ ra (getreal "\n������Բ�Ľ�Ĭ��90��!"))
  (if (null ra)
    (setq ra 90)
  )

  (setq l (+ (* Y1 th) (* r 2)))
  (setq Y2 (/ (- (* l (/ 90 ra)) (* 0.5 pi r)) th))
  (princ (strcat "\nת����Y����Ϊ~" (rtos Y2 2 5)))
  (princ)
)

;;;��ȡ�������Ϣ
(defun C:Q8 (/ zyd ss pt1 pt2 px1 px2 py2 mx1 my1 a1 b1 v po shujlb) 
(setq zyd (ssget))
(setq ss (SSGET "p"  '((8 . "NCT"))));;;�б�עʱ,���˶���
(while (not ss)
 (alert "Sorry! Please select again!") 
 (command "change" zyd "" "p" "LA" "NCT" "")
 (setq zyd (ssget))
 (setq ss (SSGET "p" '((8 . "NCT"))));;;ͼ����
)
;;;(setq bak_ss ss)
;;;(command "_.select" zyd "")          ;;;ѡ�񡰱���ѡ�񼯡�
;;;(setq zd (SSGET "P" '((2 . "a3"))))  ;;;�˴���ֵ����Ч

;;;��������Ϣ��
(if (/= ss nil)
(setq 

;;;mx1����a1 ������pt2    
;;;   ��         ��         
;;;    b1 ����ͼ  b1       
;;;   ��         ��         
;;;   ��         ��         
;;;pt1����a1 ������my1  

shulist (minmm_ssbox ss)
pt1 (car shulist )
pt2 (cadr shulist )
px1 (car pt1)		      ;;;ȡ��pt1�����xֵ
py1 (cadr pt1)	              ;;;ȡ��pt1�����yֵ
px2 (car pt2)		      ;;;ȡ��pt2�����xֵ
py2 (cadr pt2)		      ;;;ȡ��pt2�����yֵ
mx1  (list px1 py2)	      ;;;���mx1����ֵ
my1  (list px2 py1)	      ;;;���my1����ֵ  
a1   (distance mx1 pt2)       ;;;mx1��,pt2��֮��ľ��룺����ľ���ֵ����ͼ(��)
b1   (distance pt1 mx1)       ;;;pt1��,mx1��֮��ľ��룺����ľ���ֵ����ͼ(��) 
));;;end setq

  

(setq v (strcat (rtos a1 2 2) "*" (rtos b1 2 2)))
(SET-CLIP-STRING v)
(princ)
)


;;;(setq po (getpoint "\nָ����������д���:"))
;;;(if (not (tblsearch "style" "CHINESE"))
;;;  (command "_.style" "CHINESE" "PMingLiU" "" "0.8" "" "" "")
;;;)
;;;(command "_layer" "s" "MARK" "")
;;;(COMMAND "_.TEXT" "S" "CHINESE" po (* 3.5 (GETVAR "DIMSCALE")) ""v)

;;;--------------------------------------------------------
;;;;ͨ�ú���
;;____________________________________________
;; �� (Paper2Model en)
;; [����] ��ͼֽ�ռ�ͼԪ���Ƶ�ģ�Ϳռ�
;; [����] en----ͼֽ�ռ䲼����
;; [����] ģ�Ϳռ�ͼԪ
;; [����]1.(Paper2Model "����1") 
 (defun Paper2Model(strLayoutName / intCount i ents blk acadDoc Layout strPaperName)
  (setq acadDoc(vlax-get-property (vlax-get-acad-object) 'ActiveDocument))
  (setq Layout(vlax-invoke-method (vlax-get-property acadDoc 'layouts) 'Item strLayoutName))
  (setq strPaperName (vlax-get-property (vlax-get-property Layout 'Block) 'Name))
  (setq blk(vlax-invoke-method (vlax-get-property acadDoc 'Blocks) 'Item strPaperName))
  (setq intCount (- (vlax-get-property blk 'count) 1))
  (setq ents(vlax-make-safearray vlax-vbObject (vl-list* 0  (- intCount 1))))

  (setq i 1)
  (while (<= i intCount)
    (vlax-safearray-put-element ents (- i 1) (vlax-invoke-method blk 'Item i))
    (setq i(+ i 1))
  )
  (vlax-invoke-method acadDoc 'CopyObjects ents (vlax-get-property acadDoc 'ModelSpace))
)

;; [����] ���ص�ǰͼ���а���������������ʽ�ı�
(defun get-textstyle ( )
  (if (null vlax-dump-object) (vl-load-com) )
  (setq txts (vla-get-textstyles (vla-get-activedocument (vlax-get-acad-object))))
  (setq txtl '())
  (vlax-for txt txts
   (setq txtl (cons (vla-get-name txt) txtl))
  )
  (reverse txtl)
)

;;;ͨ�ú�������

(DEFUN HW (OBJ / shulist pt1 pt2 px1 py2 mx1)
(if (/= OBJ nil)
(setq 

;;;mx1����a1 ������pt2    
;;;   ��         ��         
;;;    b1 ����ͼ  b1       
;;;   ��         ��         
;;;   ��         ��         
;;;pt1����a1 ������my1  

shulist (minmm_ssbox OBJ)
pt1 (car shulist )
pt2 (cadr shulist )
px1 (car pt1)		      ;;;ȡ��pt1�����xֵ
py2 (cadr pt2)		      ;;;ȡ��pt2�����yֵ
mx1  (list px1 py2)	      ;;;���mx1����ֵ
a1   (distance mx1 pt2)       ;;;mx1��,pt2��֮��ľ��룺����ľ���ֵ����ͼ(��)
)));;;end setq

(defun set_txt (/ a1 n index shujlb en b1 ea ec ed ef)
  (PROMPT "\n �������гߴ��ע������")
  (setq al (SSGET "_x" '((0 . "dimension"))))
  (setq n (SSLENGTH al))
  (setq index 0)
  (repeat n
    (setq shujlb (ssname al index))
    (setq en (cdr (assoc 3 (entget shujlb))))
    (setq b1 (entget (tblobjname "dimstyle" en)))
    (setq ea (entget (cdr (assoc 340 b1))))
    (entmod (subst (cons 144 1.0) (assoc 144 b1) b1))
    (entmod (setq ec (subst (cons 41 0.8) (assoc 41 ea) ea)))
    (entmod (subst (cons 3 "simsun.ttf") (assoc 3 ec) ec))
    (entupd shujlb)
    (setq index (+ index 1))
  )
  (setq ed (entget (tblobjname "style" "standard")))
  (entmod (setq ef (subst (cons 41 0.8) (assoc 41 ed) ed)))
  (entmod (subst (cons 3 "simsun.ttf") (assoc 3 ef) ef))
  (command "_.move" (ssget "_x") "" '(0 0) '(0.1 0))
  (princ)
)

;;;--------------------------------------------------------
;;;����:Q9
;;;--------------------------------------------------------
;;;��������:2017.06.20
;;;����  :֣�Ѷ�
;;;˵��:��ͼֽ�ռ�ͼ�����Ƶ�ģ�Ϳռ�
;;;--------------------------------------------------------
(DEFUN c:Q9 (/ wn n enn OBJ pt1 pt2 px1 py2 mx1 a1 p)
(if (null vlax-dump-object)(vl-load-com))
(setq wn (vl-list-length (layoutlist)))
(setq ProgressBar (vlax-create-object "ProgressBar.CProgressBar"))
(vlax-invoke-method ProgressBar 'init (vlax-get-acad-object))
(vlax-put-property ProgressBar 'Min 0)
(vlax-put-property ProgressBar 'Max 100)
(vlax-put-property ProgressBar 'Progress 1)
(HW (ssget "_x"))
(if (= a1 nil)(setq a1 1))
(setq j (/ 100 (1+ wn)))
(setq p (list (+ a1 50) 0))
(setq n 0)
(while (< n wn)
(setq enn (entlast));;�������Ҫѡ��Ķ���
(Paper2Model (nth n (layoutlist)))
(setq OBJ (lt:ss-entnext enn))
(HW OBJ)
(command "_.move" OBJ "" '(0 0) p)
(setq p (list (+ (car p) a1 50) (cadr p)))
(setq n (1+ n))
(vlax-put-property ProgressBar 'Progress (* j n))
(if (= n wn)
  (vlax-release-object ProgressBar)
))
(FOREACH layout_name (LAYOUTLIST) (COMMAND "layout" "d" layout_name))
(princ)(set_txt)
)

;;;*****�������� ����ʼ*****
(defun C:T1 (/ ss ptBase entnam obj ptStart ptEnd pt ptList ptNo)
  (princ "\n�﹦�ܣ��������ҶϿ����߶μ�\n")  
  (alert "�����֮ǰ����ɾ�����ߺͱ�����߲���!\n���������ش���......")
  (setvar "pickadd" 1)
  (setvar "osmode" 15359)
  (setvar "PICKDRAG" 0)
  (setvar "cmdecho" 0)
                                        ;  (wdy_timeset1)
  (command "undo" "be")
  (princ "\n��ѡȡֱ�ߡ�����ߡ��������ߡ�Բ����")
  (if (not (setq ss (ssget '((0 . "*LINE,ARC")))))
    (progn (princ "\δѡ�ж��󡣳����˳���") (exit))
  )
  (initget 1)
  (setq ptBase (getpoint "\nָ����������ߵ�λ�õ㣺"))
  (command "LAYER" "M" "����-������" "C" "1" "����-������" "")
  (setvar "osmode" 0)
  (vl-load-com)
  (setq        i -1
        ptList nil
        ptNo nil
  )
  (repeat (sslength ss)
    (setq entnam  (ssname ss (setq i (1+ i)))
          obj          (vlax-ename->vla-object entnam)
          ptStart (vlax-curve-GetStartPoint obj)
          ptEnd          (vlax-curve-GetEndPoint obj)
    )
    (if        (not (vlax-curve-isclosed obj))
      (progn
        (setq ptList (cons ptStart ptList))
        (setq ptList (cons ptEnd ptList))
      )
    )
  )
  (prin1 ptList)
  (while (setq pt     (car ptList)
               ptList (cdr ptList)
         )
    (if        (wdy_cxgx_duibi pt ptList)
      (setq ptList (vl-remove pt ptList))
      (setq ptNo (cons pt ptNo))
    )
  )
  ;| (while (setq pt     (car ptList)
               ptList (cdr ptList)
         )
    (if        (member pt ptList)
      (setq ptList (vl-remove pt ptList))
      (setq ptNo (cons pt ptNo))
    )
  )|;                                        ;��һ��д�����޾����жϣ��㷨�ϲ�
  (if (not ptNo)
    (alert "��ʾ��\n��ϲ�㣡û�з��������߶���\n")
    (progn
      (foreach pt ptNo
        (command "LINE" pt ptBase "")
      )
      (alert
        "��ʾ��\n�����������߶���\n\n����ݡ�����-�����ߡ�ͼ���е�������λ�õ���в鿴������λ�õ㡣\n"
      )
    )
  )
  (command "undo" "e")
  (setvar "osmode" 15359)
  (princ)
)

(defun wdy_cxgx_duibi (pt0 lst / TorF x)
  (setq TorF nil)
  (foreach x lst
    (if        (equal pt0 x 0.001)
      (setq TorF T)
    )
  )
  TorF
)
;;;*****�������� �������*****

(defun c:T2 (/ os cen rr pp_12 pp_3 pp_6 pp_9)
  (setvar "cmdecho" 0)
  (setq os (getvar "osmode"))
  (setvar "osmode" 0)
  (setq cen (getpoint "\n ���ĵ㣺"))
  (setq rr (* (getdist cen "\n �뾶��")))
  (lsp_22a)
  (lsp_22b)
  (lsp_22c)
  (setvar "osmode" os)
)
;����ʱ���ӳ���
(defun lsp_22a(/ ti tt dd mm en3 en1 en2)
  (setvar "cecolor" "3")
  (command "donut" (/ (* rr 39) 40) rr cen "")
  (command "donut" 0 (/ rr 39) cen "")
  (setvar "cecolor" "1")
  (setq pp_12 (polar cen (/ pi 2) (/ rr 2)))
  (setq pp_3 (polar cen 0 (/ rr 2)))
  (setq pp_6 (polar cen (* pi 1.5) (/ rr 2)))
  (setq pp_9 (polar cen pi (/ rr 2)))
  (command "donut" 0 (/ rr 12) pp_12 pp_3 pp_6 pp_9  "")
  (setvar "cecolor" "7")
  (command "pline" cen "w" (/ rr 40) "" cen (polar cen (/ pi 2) (* (/ rr 20) 7)) "")
  (setq en3 (entlast))
  (setvar "cecolor" "4")
  (command "pline" cen "w" (/ rr 50) "" cen (polar cen (/ pi 2) (* (/ rr 20) 8)) "")
  (setq en1 (entlast))
  (setvar "cecolor" "5")
  (command "pline" cen "w" 0 0 cen (polar cen (/ pi 2) (* (/ rr 20) 9)) "")
  (setq en2 (entlast))
  (setvar "cecolor" "bylayer")
  (setq ti (rtos (getvar "cdate") 2 6))	;ȡ��Ŀǰϵͳ��ʱ��ֵ
  (setq tt (substr ti 10 2))		;ȡ��"ʱ"
  (setq dd (substr ti 12 2))		;ȡ��"��"
  (setq mm (substr ti 14 2))		;ȡ��"��"
  
  ;��ȡ�õ�ʱ�����ʱ�����λ��
  (command "rotate" en3 "" cen (* (atoi tt) -30))
  (command "rotate" en2 "" cen (* (atoi mm) -6))
  (command "rotate" en1 "" cen (* (atoi dd) -6))
  (command "rotate" en3 "" cen (* (atoi mm) -0.5))
  )
;����ʱ�������ӳ���
(defun lsp_22b (/ txt_3 txt_6 txt_9 txt_12)
  (setq txt_12 (polar pp_12 (* pi 1.5) (/ rr 10)))
  (setq txt_3 (polar pp_3 pi (/ rr 10)))
  (setq txt_6 (polar pp_6 (/ pi 2) (/ rr 10)))
  (setq txt_9 (polar pp_9 0 (/ rr 10)))
  (command "text" "m" txt_12 (/ rr 12) 0 "12")
  (command "text" "m" txt_3 (/ rr 12) 0 "3")
  (command "text" "m" txt_6 (/ rr 12) 0 "6")
  (command "text" "m" txt_9 (/ rr 12) 0 "9")
)


;����ʱ��װ���� (gcd int1 int2)���������������Լ��
(defun lsp_22c (/ i pt ptStart ptEnd)
  (setq i 0)
  (repeat 60
    (progn
      (if (wcmatch (rtos i 2 0)
		   "0,5,10,15,20,25,30,35,40,45,50,55,60"
	  )
	(setq pt (/ rr 2.3))
	(setq pt (/ rr 2.2))
      )
      (setq ptStart (polar cen (* (/ pi 30) i) pt))
      (setq ptEnd (polar cen (* (/ pi 30) i) (/ rr 2.06)))
      (command "pline" ptStart "w" (/ rr 200) "" ptStart ptEnd "")
      (setq i (1+ i))
    )
  )
)

;;;�������������ļ���VsCode
(defun c:T3 (/ na d1 d2 nu en em)
  (alert "�������ǰ�������е�VsCode�༭������ ")
  (setq na (getstring "������༭���ļ������������------"))
  (setq d1 (ZL-TXTFILE-READTOLIST (findfile "hymcad.mnl")))
  (setq d2 (cadr (member (strcat "(defun c:" na "()") d1)))
  (if (/= d2 nil)
    (setq nu (- (vl-string-search "lsp" d2) (vl-string-search "\"" d2))
	  en (substr d2 (+ (vl-string-search "\"" d2) 2) (+ nu 2))
	  em (findfile en)
    )
    (setq em
	   (strcat
	     "E:\\ZydZax\\Documents\\My_file\\Work Area\\lisp works pace\\lsptest\\20191113\\"
	     na
	     ".lsp"
	   )
    )
  )

  (startapp
    "E:\\ZydZax\\Downloads\\�������\\Microsoft VS Code\\Code.exe"
    (strcat "\"" em)
  )
  (princ)
)


;;;���ԣ�(ZL-TXTFILE-READTOLIST "D:\\TEST.TXT")
(defun ZL-TXTFILE-READTOLIST (TXTFILE / LST_JG F TMP)
  (setq LST_JG '())
  (if (setq F (open TXTFILE "r"))
    (progn
      (while (setq TMP (read-line F))
	(setq LST_JG (cons TMP LST_JG))
      )
      (close F)
    )
  )
  ;;����
  (reverse LST_JG)
)













