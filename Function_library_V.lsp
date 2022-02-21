;;; Function library

;;;VsCode��������A
;;; ***************************************************************************
;; �� (Sendcommand)
;; [����] ��������VScode��CADͨ��
;; [����] ����·��
;; [����] ��
;; [����]1.(Sendcommand FileName)
;;; (defun Sendcommand (FileName / F str nm nu en wcm myacad document) 
;;;   (vl-load-com)
;;;   (if (setq F (open FileName "r")) 
;;;     (progn 
;;;       (while (and (setq str (read-line F)) (not wcm)) 
;;;         (if (wcmatch (strcase str T) "*c:*") 
;;;           (setq nm  (vl-string-search "c:" str)
;;;                 nu  (- (vl-string-search " (" str) nm)
;;;                 en  (substr str (+ nm 3) (- nu 2))
;;;                 wcm T
;;;           )
;;;         )
;;;       )
;;;       (close F)
;;;     )
;;;   )
;;;   (if (not en) 
;;;     (setq en "No command found.")
;;;   )
;;;   (setq myacad   (Vlax-get-acad-object) ;;��ȡAutoCADӦ�ó�����
;;;         document (Vla-get-activedocument myacad) ;;;��ȡ��ĵ�
;;;   )
;;;   (vla-sendcommand document (strcat "(load " "\"" FileName "\"" ")"))
;;;   (princ (princ "\n������: ^" en "^ �������-^o^-^o^"))
;;;   ;;; (vla-sendcommand document en)
;;;   (princ)
;;; )

;;;VsCode��������B
;;; ***************************************************************************
;; �� (Sendcommand)
;; [����] ��������VScode��CADͨ��
;; [����] ����·��
;; [����] ��
;; [����]1.(Sendcommand FileName)
;;; (defun Sendcommand (FileName /)  ;;VsCode��������
;;;   (vla-sendcommand (vlax-get-property (vlax-get-acad-object) 'activedocument) 
;;;                    (strcat "(load " "\"" FileName "\"" ")")
;;;   )
;;;   (princ)
;;; )

;;;ͨ�ú���A
;;; ***************************************************************************
;; �� (minmm_ss->enlist)
;; [����] �ѱ�������ת��Ϊ��
;; [����] ѡ��
;; [����] ��
;; [����]1.(minmm_ss->enlist ss)
(defun minmm_ss->enlist (ss / lst n en) 
  (setq n -1)
  (while (setq en (ssname ss (setq n (1+ n)))) 
    (setq lst (cons en lst))
  )
)
;;; (defun minmm_objBox (obj / minpoint maxpoint) 
;;;   (vla-GetBoundingBox obj 'minpoint 'maxpoint)
;;;   ;ȡ�ð���ͼԪ���������С��
;;;   (setq minpoint (vlax-safearray->list minpoint)) ;�ѱ�������ת��Ϊ��
;;;   (setq maxpoint (vlax-safearray->list maxpoint)) ;�ѱ�������ת��Ϊ��
;;;   (setq minpoint (trans minpoint 0 1))
;;;   (setq maxpoint (trans maxpoint 0 1))
;;;   (setq obj (list minpoint maxpoint)) ;;;��ϳɵ��
;;; )
;;; ȡ�ð���ͼԪ���������С��
(defun minmm_objBox (obj / Minp Maxp) 
  (vla-GetBoundingBox obj 'Minp 'Maxp)
  (mapcar 'vlax-safearray->list (list Minp Maxp))
)
(defun minmm_ssbox (ss / boxlst maxlst minlst objlst) 
  (vl-load-com)
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

;;;ͨ�ú���B
;;; ***************************************************************************
;; �� (lt:ss-entnext)
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
        (if 
          (not 
            (member (cdr (assoc 0 (entget en))) 
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

;;;ͨ�ú���C
;;; ***************************************************************************
;; �� (MJ:GetAttributes)
;; [����] ��ȡͼ�� ent ֮�����������
;; [����] ent----ͼ����
;; [����] ����
;; [����]1.(setq shujlb (MJ:GetAttributes (car (entsel))))
;;       2.(setq sheji  (nth 1 (nth 0 shujlb) );;;0Ϊ���һ������
(defun MJ:GetAttributes (ent / lst) 
  (setq *Obj2En* vlax-vla-object->ename)
  (if 
    (safearray-value 
      (setq lst (vlax-variant-value 
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

;;;ͨ�ú���
;;; ***************************************************************************
;; �� (ZL-TXTFILE-READTOLIST)
;; [����] ��ȡ�ļ���ת��Ϊ��
;; [����] �ļ���·��
;; [����] ��
;; [����](ZL-TXTFILE-READTOLIST "D:\\TEST.TXT")
;;; (defun ZL-TXTFILE-READTOLIST (TXTFILE / LST_JG F TMP) 
;;;   (setq LST_JG '())
;;;   (if (setq F (open TXTFILE "r")) 
;;;     (progn 
;;;       (while (setq TMP (read-line F)) 
;;;         (setq LST_JG (cons TMP LST_JG))
;;;       )
;;;       (close F)
;;;     )
;;;   )
;;;   ;;����
;;;   (reverse LST_JG)
;;; )

;;;ͨ�ú���(�ı���)
;;; ***************************************************************************
;; �� (fn-lst)
;; [����] ���ı��ļ�ת��Ϊ�ַ������ݱ�
;; [����] file----�ļ���(��·��)
;; [����] �ַ���
;; [����](fn-lst "note_bak.ini")
(defun fn-lst (file / fn ftext lst) 
  (if (setq fn (open (findfile file) "r")) 
    (progn 
      (while (setq ftext (read-line fn)) 
        (setq lst (append lst (list ftext)))
      )
      (close fn)
    )
    (alert "�ļ�д��ʧ��~~ ����Ŀ���ļ��еĹ���Ȩ��!")
  )
  lst
)

;; [����] ���ַ������ݱ�ת��Ϊ�ı��ļ�
(defun lst-fn (file lst mod / fn) 
  (setq fn (open file mod))
  (foreach x lst 
    (if (/= 'str (type x)) 
      (setq x (vl-prin1-to-string x)) ;��ת��Ϊ�ַ�������
    )
    (write-line x fn)
  )
  (close fn)
)

;; [����] �����ݱ����DCL�б��
(defun lst-dcl (key lst Mod No /) 
  (start_list key Mod No)
  (mapcar 'add_list lst)
  (end_list)
)

;; [����] ɾ������ָ���е��ı�����
(defun DelLine (file NumberOfLine / cnt line lst f) 
  (setq lst '()
        cnt 0
  )
  (setq f (open file "r"))
  (while (setq line (read-line f)) 
    (setq cnt (1+ cnt))
    (if (/= cnt NumberOfLine) 
      (setq lst (append lst (list line)))
    )
  )
  (close f)
  (setq f (open file "w"))
  (foreach atm lst (write-line atm f))
  (close f)
)

;; [����] �滻����ָ���е��ı�����
(defun RelLine (file NumberOfLine new_line / cnt line lst f) 
  (setq lst '()
        cnt 0
  )
  (setq f (open file "r"))
  (while (setq line (read-line f)) 
    (setq cnt (1+ cnt))
    (if (/= cnt NumberOfLine) 
      (setq lst (append lst (list line)))
      (setq lst (append lst (list new_line)))
    )
  )
  (close f)
  (setq f (open file "w"))
  (foreach atm lst (write-line atm f))
  (close f)
)

;;;ͨ�ú���
;;; ***************************************************************************
;; �� (vs-f-path)
;; [����] ת���ļ�·���ַ���ʽ
;; [����] �ַ���
;; [����] ��ʽ���ַ���
;; [����]1.(vs-f-path string)
(Defun vs-f-path (string /) 
  (while (vl-string-search "\\" string) 
    (setq string (vl-string-subst "/" "\\" string))
  )
  string
)

;;;ͨ�ú���
;;; ***************************************************************************
;; �� (SET-CLIP-STRING)
;; [����] ������д��ϵͳ������
;; [����] �ַ���
;; [����] ��
;; [����]1.(SET-CLIP-STRING STR)
(defun SET-CLIP-STRING (STR / HTML RESULT) 
  (and 
    (= (type STR) 'STR)
    (setq HTML (vlax-create-object "htmlfile"))
    (setq RESULT (vlax-invoke 
                   (vlax-get (vlax-get HTML 'PARENTWINDOW) 'CLIPBOARDDATA)
                   'SETDATA
                   "Text"
                   STR
                 )
    )
    (vlax-release-object HTML)
  )
)

;;;ͨ�ú���
;;; ***************************************************************************
;; �� (GET-CLIP-STRING)
;; [����] ��ϵͳ�������ȡ�ַ���
;; [����] ��
;; [����] ��
;; [����]1.(GET-CLIP-STRING)
(defun GET-CLIP-STRING (/ HTML RESULT) 
  (and (setq HTML (vlax-create-object "htmlfile")) 
       (setq RESULT (vlax-invoke 
                      (vlax-get (vlax-get HTML 'PARENTWINDOW) 
                                'CLIPBOARDDATA
                      )
                      'GETDATA
                      "Text"
                    )
       )
       (vlax-release-object HTML)
  )
  RESULT
)
;;;ͨ�ú���
;;; ***************************************************************************
;; �� (cs_pross)
;; [����] ������
;; [����] ����/����
;; [����] ������ʾ
;; [����](cs_pross to I)
(defun cs_pross (to I / cs_txt myi) 
  ;;; (setq cs_txt "����������������������������")
  (setq cs_txt ">>>>>>>>>>>>>>>>>>>>>>>>>>>>")
  (setq myi    (fix (/ (* (strlen cs_txt) I) to))
        cs_txt (substr cs_txt 1 myi)
  )
  (grtext -2 cs_txt)
)
;;; cs_pross����"��Ӻ�ʱ����"
;;; (setq stime (getvar "date"))
;;; (cs_pross to I);��������
;;; (setq etime (getvar "date"))
;;; (setq Htime (* 86400.0 (- (- etime stime) (fix (- etime stime)))))
;;; (grtext -2 (strcat "������ɣ���ʱ" (rtos Htime 2 2) "��..."))

;;;ͨ�ú���
;;; ***************************************************************************
;; �� (Scr_pross)
;; [����] ����������
;; [����] ����
;; [����] ����������ʾ
;; [����](Scr_pross I)
(defun Scr_pross (I / cs_txt) 
  (setq cs_txt '("��" "����" "������" "��������" "����������" "������������" "��������������" "����������������" 
                 "������������������" "��������������������" "����������������������" "������������������������" "��������������������������" 
                 "����������������������������"
                )
  )
  (grtext -2 (nth (rem (1+ I) 14) cs_txt)) ;�������������Ĵ���
)

;;;ͨ�ú���
;;; ***************************************************************************
;; �� (disableCtrls)
;; [����] �ؼ�����
;; [����] keylst
;; [����] ��
;; [����](disableCtrls '("T3" "T4"))

  ;;���Կؼ�
(defun disableCtrls (keylst / key) 
  (foreach key keylst 
    (mode_tile key 1)
  )
)
  ;;����ؼ�
(defun EnableCtrls (keylst / key) 
  (foreach key keylst 
    (mode_tile key 0)
  )
)

;;;ͨ�ú���
;;; ***************************************************************************
;; �� (setq t0 (get-utime))
;; [����] ʱ�亯��
;; [����] ��
;; [����] ��ǰʱ��,��
;; [����](setq t0 (get-utime))
;;; (defun get-utime ()
;;;   (* 86400 (getvar "tdusrtimer"))
;;; )
;;; (getvar "date")


;;;ͨ�ú���
;;; ***************************************************************************
;;;���������ϳ�֣�Ѷ����� 2015��6��13��
;;ssPts: 1 ѡ�񼯣����������ѡ��
;;Key: "xyzXYZ"�������,����"yX",y��ǰ��ʾy�������ȣ�Сy��ʾ��С����(ע:��ά��ʱ��������z)
;;FUZZ: �������
;;ʾ�� (HH:ssPts:Sort zd "XYZ" 3.5) ;����������ѡ��<Selection set: fb3>
(defun HH:ssPts:Sort (ssPts KEY FUZZ / E EN FUN LST N SORTPTS SORTSS) 
  ;;1 ѡ��ͼԪ����
  (defun sortSS (PTS FUN F FUZZ) 
    (vl-sort pts 
             '(lambda (a b) 
                (if (not (equal (F (car a)) (F (car b)) fuzz)) 
                  (fun (F (car a)) (F (car b)))
                )
              )
    )
  )
  ;;2 ����
  (defun sortSS1 (myfun PTS KEY FUZZ) 
    (setq Key (vl-string->list Key))
    (foreach xyz (reverse Key) 
      (cond 
        ((< xyz 100)
         (setq fun >)
         (setq xyz (nth (- xyz 88) (list car cadr caddr)))
        )
        (T
         (setq fun <)
         (setq xyz (nth (- xyz 120) (list car cadr caddr)))
        )
      )
      (setq Pts (myfun Pts fun xyz fuzz))
    )
  )
  ;;3 ������������
  (cond 
    ((= (type ssPts) 'PICKSET)
     (repeat (setq n (sslength ssPts)) 
       (if 
         (and (setq e (ssname ssPts (setq n (1- n)))) 
              (setq en (entget e))
         )
         (setq lst (cons (cons (cdr (assoc 10 en)) e) lst))
       )
     )
     (setq SS (ssadd))
     (foreach CN (mapcar 'cdr (sortSS1 sortSS lst KEY FUZZ)) 
       (if (= (type CN) 'ENAME) 
         (ssadd CN SS)
       )
     )
     (if (= (sslength SS) 0) 
       NIL
       SS
     )
    )
  )
)

;;;ͨ�ú���
;;; ***************************************************************************
;; �� (defultprint)
;; [����] ��ע����ȡĬ�ϴ�ӡ��
;; [����] ��
;; [����] Ĭ�ϴ�ӡ��
;; [����](defultprint)
(defun defultprint (/ device)
 (vl-load-com)
 (substr (setq device (vl-registry-read "HKEY_CURRENT_USER\\Software\\Microsoft\\Windows NT\\CurrentVersion\\Windows" "Device"))
         1 (vl-string-search "," device)
 )
)

(load "wubi.lsp") ;;; �������뷨�л���ʽ