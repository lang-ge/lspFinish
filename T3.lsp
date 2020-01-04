;;;输入命令发送相关文件至VsCode
(defun c:T3 (/ na d1 d2 nu en em)
  (alert "☆☆☆即将打开前端最流行的VsCode编辑器☆☆☆ ")
  (setq	na (getstring "输入文件名或命令继续~~~~!\n默认可进行Git文件路径格式转换!"))
  (if (= na "")
    (progn
      (alert "源文件路径选择了吗?确认继续~~~")
      (setq em (strcat "\"" (vs-f-path (vl-filename-directory (GET-CLIP-STRING))) "\""))
      (SET-CLIP-STRING em)
      (if (= em "\"\"")
      (alert " 哦豁^.^走丢了^o^~~^o^")
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
	"E:\\ZydZax\\Downloads\\软件备份\\Microsoft VS Code\\Code.exe"
	(strcat "\"" (findfile en))
      )
    )
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


;;功能：转换文件路径字符格式
(Defun vs-f-path (string /)
  (while (vl-string-search "\\" string)
    (setq string (vl-string-subst "/" "\\" string))
  )
  string
)

;;;=================================================================*
;;;功能：Write text to the system clipboard                                       *
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
;;;功能：Read string from system clipboard
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
