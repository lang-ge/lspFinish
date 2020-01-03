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


;;;功能：读取文件并转换为表
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

;;;==================================================*
;;;Vscode文件路径转换
(defun c:T4 (/)
  (vl-load-com)
  (SET-CLIP-STRING
    (vs-f-path (vl-filename-directory (GET-CLIP-STRING)))
  )
  (princ "格式转换完成!")
)


;;返回值:转换后的字符串 或者  None
(Defun vs-f-path (string /)
  (while (vl-string-search "\\" string)
    (setq string (vl-string-subst "/" "\\" string))
  )
  string
)

;;;=================================================================*
;;;功能：向系统剪贴板写入文字                                       *
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
;;;功能：读取系统剪贴板中字符串
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
