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