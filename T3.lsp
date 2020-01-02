;;;输入命令发送相关文件至VsCode
(defun c:T3 (/ na d1 d2 nu en em)
  (alert "☆☆☆即将打开前端最流行的VsCode编辑器☆☆☆ ")
  (setq na (getstring "输入需编辑的文件名或命令继续------"))
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
    "E:\\ZydZax\\Downloads\\软件备份\\Microsoft VS Code\\Code.exe"
    (strcat "\"" em)
  )
  (princ)
)


;;;测试：(ZL-TXTFILE-READTOLIST "D:\\TEST.TXT")
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
  ;;返回
  (reverse LST_JG)
)