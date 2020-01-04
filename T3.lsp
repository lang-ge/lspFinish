;;;�������������ļ���VsCode
(defun c:T3 (/ na d1 d2 nu en em)
  (alert "�������ǰ�������е�VsCode�༭������ ")
  (setq	na (getstring "�����ļ������������~~~~!\nĬ�Ͽɽ���Git�ļ�·����ʽת��!"))
  (if (= na "")
    (progn
      (alert "Դ�ļ�·��ѡ������?ȷ�ϼ���~~~")
      (setq em (strcat "\"" (vs-f-path (vl-filename-directory (GET-CLIP-STRING))) "\""))
      (SET-CLIP-STRING em)
      (if (= em "\"\"")
      (alert " Ŷ��^.^�߶���^o^~~^o^")
      (alert em))	
    )
    (progn
      (setq d1 (ZL-TXTFILE-READTOLIST (findfile "hymcad.mnl")))
      (setq d2 (cadr (member (strcat "(defun c:" na "()") d1)))
      (if (/= d2 nil)
	(setq nu (- (vl-string-search "p\"" d2) (vl-string-search "\"" d2)
	      en (substr d2 (+ (vl-string-search "\"" d2) 2) nu)
	)
	(setq en (strcat na ".lsp"))
      )

      (startapp
	"E:\\ZydZax\\Downloads\\�������\\Microsoft VS Code\\Code.exe"
	(strcat "\"" (findfile en))
      )
    )
  )
  (princ)
)


;;;���ܣ���ȡ�ļ���ת��Ϊ��
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


;;���ܣ�ת���ļ�·���ַ���ʽ
(Defun vs-f-path (string /)
  (while (vl-string-search "\\" string)
    (setq string (vl-string-subst "/" "\\" string))
  )
  string
)

;;;=================================================================*
;;;���ܣ�Write text to the system clipboard                                       *
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
;;;���ܣ�Read string from system clipboard
(defun GET-CLIP-STRING (/ HTML RESULT)
  (and (setq HTML (vlax-create-object "htmlfile"))
       (setq RESULT (vlax-invoke
		      (vlax-get	(vlax-get HTML 'PARENTWINDOW)
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
