;;;输入命令发送相关文件至VsCode
(defun c:21 (/ na d1 d2 nu en em ez x)
  (alert "☆☆☆即将开始VsCode编辑器工作之旅☆☆☆~^o^ ")
  (setq	na (getstring "输入目标文件名或命令继续~~~!\n默认可进行Git文件路径格式转换!"))
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
	(setq nu (- (vl-string-search "p\"" d2) (vl-string-search "\"" d2))
	      en (substr d2 (+ (vl-string-search "\"" d2) 2) nu)
	)
	(setq en (strcat na ".lsp"))
      )
      (if (findfile en)
	(setq ez (findfile en))
	(foreach x (dos_lisplist T);;;返回已加载的LISP文件和路径列表
	  (if (wcmatch x (strcat "*" en))
	    (setq ez x)
	  )
	)
      )
      (startapp
	"E:\\ZydZax\\Downloads\\软件备份\\Microsoft VS Code\\Code.exe"
	(strcat "\"" ez)
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



;;; ===================================================
 ;;; 功能：动态轴测图直线
 ;;;  
 ;;; ===================================================
 (defun c:zline (/ #errzline $orr name pt1)
   (defun #errzline (s)
     (if name
       (entdel name)
     )
     (redraw)
     (command ".UNDO" "E")
     (setq *error* $orr)
   )
   (setq $orr *error*)
   (setq *error* #errzline)
   (setvar "cmdecho" 0)
   (if (setq pt1 (getpoint "\n指定第一点:"))
     (zct01 pt1)
   )
   (setq *error* $orr)
   (princ)
 )
 ;;; ===================================================
 ;;; 功能：动态轴测图文字
 ;;;         命令:ztext      
 ;;; ===================================================
 (defun c:ztext (/ #errztext $orr bili dcl_re dclname dlg dsy ent filen i name pt1 stream tempname tsy txt xunh zigao)
   (defun #errztext (s)
     (if name
       (entdel name)
     )
     (redraw)
     (command ".UNDO" "E")
     (setq *error* $orr)
   )
   (setq $orr *error*)
   (setq *error* #errztext)
   (setvar "cmdecho" 0)
   (setq zigao (getvar "TEXTSIZE")
 bili (getvar "DIMSCALE")
   )
   (setq i (zct02))
   (setq tsy (car i))
   (setq dsy (cadr i))
   (setq dclname (cond
     ((setq tempname (vl-filename-mktemp "re-dcl-tmp.dcl")
     filen (open tempname "w")
      )
       (foreach stream '("\n" "RENAME:dialog {\n"
          "    label = \"轴测图文字\" ;\n" "    :row {\n"
          "        :edit_box {\n" "            key = \"e01\" ;\n"
          "            width = 30 ;\n" "  height = 1.2 ;    }\n"
          "    }\n" "    :row {\n"
          "        :button {\n" "            key = \"e02\" ;\n"
          "            label = \"确认\" ;\n" "        }\n"
          "        :button {\n" "            is_cancel = true ;\n"
          "            key = \"btn_cancle\" ;\n" "            label = \"取消\" ;\n"
          "        }\n" "    }\n"
          "}\n"
         )
         (princ stream filen)
       )
       (close filen)
       tempname
     )
   )
   )
   (setq dcl_re (load_dialog dclname))
   (if (not (new_dialog "RENAME" dcl_re))
     (vl-exit-with-error "")
   )
   (if texbak
     (set_tile "e01" texbak)
   )
   (action_tile "e02" "(setq TXT  (get_tile \"e01\"))(done_dialog )")
   (setq dlg (start_dialog))
   (unload_dialog dcl_re)
   (vl-file-delete dclname)
   (if (or
 (= txt "")
 (null txt)
       )
     (setq xunh nil)         ;    (vl-exit-with-error "")
     (setq texbak txt)
   )
   (if (setq pt1 (getpoint "\n指定文字的起点:"))
     (progn
       (command "Undo" "BE")
       (entmake (list '(0 . "TEXT") (cons 1 txt) (cons 10 pt1) (cons 40 (* zigao bili))))
       (setq name (entlast)
      ent (entget name)
       )
       (zct03 name ent pt1 tsy t)
     )
   )
   (redraw)
   (command ".UNDO" "E")
   (setq *error* $orr)
   (princ)
 )
 ;;; ===================================================
 ;;; 功能：动态轴测图标注
 ;;;         命令:zdim       
 ;;; ===================================================
 (defun c:zdim (/ ang bili data dm ds dsy ent i inf kk lst na name name1 obj pd pdzc pt1 pt10 pt14 pt2 pts ptv tsy uu x zhigao zz)
   (defun *error* (inf)
     (setq inf (strcase inf t))
     (cond
       ((wcmatch inf "*break*,*cancel*,*exit*,*取消*,*中断*") ; 按了<esc>键出错处理
 (if (and
        name
        (/= pdzc "Y")
      )
    (entdel name)
 )
 (if dsy
    (command "dimstyle" "r" dsy)
 )
 (redraw)
 (command ".UNDO" "E")
       )
       (t
 (princ (strcat "\n" inf))
       )
     )
     (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
     (princ)
   )
   (defun zctbz01 (pt1 pt2 / bili1 dsy i tsy zhigao) ; 重要计算程序
     (command ".UNDO" "BE")
     (setq bili1 (getvar "DIMSCALE"))
     (setq i (zct02))
     (setq tsy (car i))
     (setq dsy (cadr i))
     (if (or
    (null panybak1)
    (/= (car panybak1) bili1)
 )
       (progn
 (setq zhigao (getvar "DIMTXT")
        panybak1 (list bili1 (* zhigao bili1 2.2))
 )
 (command "dimstyle" "r" "+30")
 (setvar "DIMSCALE" bili1)
 (command "DIMSTYLE" "S" "+30" "Y")
 (command "dimstyle" "r" "-30")
 (setvar "DIMSCALE" bili1)
 (command "DIMSTYLE" "S" "-30" "Y")
 (command "dimstyle" "r" dsy)
 (setvar "DIMSCALE" bili1)
       )
     )
     (setvar "DIMSCALE" bili1)
     (setq panybak (cadr panybak1))
     (zct04 pt1 pt2 dsy t nil)
     (command ".UNDO" "E")
   )
   (defun zctbz02 (name dm / ds ent pt10 pt14 pts) ; 更新标注
     (redraw)
     (setq ent (entget name)
    pt10 (cdr (assoc 10 ent))
    pt14 (cdr (assoc 14 ent))
    ds (angle pt14 pt10)
    pts (polar pt14 ds panybak)
     )
     (if dm
       (setq ent (subst
     (cons 1 dm)
     (assoc 1 ent)
     ent
   )
       )
     )
     (entmod (subst
        (cons 10 pts)
        (assoc 10 ent)
        ent
      )
     )
     (if dsy
       (command "dimstyle" "r" dsy)
     )
     (command ".UNDO" "E")
     (vl-exit-with-error "")
   )
   (defun zctbz03 (ang)         ; 弧度转为角度
     (/ (* ang 180) pi)
   )
   (defun zctbz04 (uu kk zz dm /)       ; 创建标注
     (if name
       (entdel name)
     )
     (if dm
       (entmake (list '(0 . "DIMENSION") '(100 . "AcDbEntity") '(100 . "AcDbDimension") (cons 10 data) '(70 . 33) (cons 1 dm) '
        (100 . "AcDbAlignedDimension") (cons 13 pt1) (cons 14 pt2)
         )
       )
       (entmake (list '(0 . "DIMENSION") '(100 . "AcDbEntity") '(100 . "AcDbDimension") (cons 10 data) '(70 . 33) '(1 . "") '
        (100 . "AcDbAlignedDimension") (cons 13 pt1) (cons 14 pt2)
         )
       )
     )
     (setq name (entlast))
     (command ".dimedit" "o" name "" uu)
     (vla-put-textstyle (vlax-ename->vla-object name) kk)
     (setq ent (entget name)
    pd zz
     )
     (setq ent (subst
   (cons 3 kk)
   (assoc 3 ent)
   ent
        )
     )
     (entmod ent)
   )
   (defun zctbz05 (data ent)        ; 更新标注
     (entmod (subst
        (cons 10 data)
        (assoc 10 ent)
        ent
      )
     )
     ent
   )
   (setvar "cmdecho" 0)         ; 主程序
   (if (setq pt1 (getpoint "\n指定第一条尺寸界线原点或 <选择对象>:"))
     (if (setq pt2 (getpoint pt1 "\n指定第二条尺寸界线原点:"))
       (zctbz01 pt1 pt2)
     )
     (progn
       (while (not (and
       (setq na (entsel "\n选择标注对象:"))
       (setq name1 (car na))
       (setq ptv (cadr na))
       (setq ent (entget name1))
       (or
         (= (cdr (assoc 0 ent)) "LINE")
         (= (cdr (assoc 0 ent)) "LWPOLYLINE")
       )
     )
       )
 (if (= 52 (getvar "errno"))
    (vl-exit-with-error "")
 )
       )
       (if (= (cdr (assoc 0 ent)) "LINE")
 (setq pt1 (cdr (assoc 10 ent))
        pt2 (cdr (assoc 11 ent))
 )
       )
       (if (= (cdr (assoc 0 ent)) "LWPOLYLINE")
 (progn
    (setq lst '()
   obj (vlax-ename->vla-object name1)
   i (fix (vlax-curve-getparamatpoint obj (vlax-curve-getclosestpointto obj ptv)))
    )
    (foreach x ent
      (if (= (car x) 10)
        (setq lst (cons (cdr x) lst))
      )
    )
    (setq lst (reverse lst)
   pt1 (nth i lst)
   pt2 (nth (if (< (1+ i) (length lst))
       (1+ i)
       0
     ) lst
       )
    )
 )
       )
       (zctbz01 pt1 pt2)
     )
   )
   (princ)
 )
 ;;; ===================================================
 ;;; 功能：动态轴测图更改
 ;;;         命令:zch        
 ;;; ===================================================
 (defun c:zch (/ #errzch $orr bili dm dsy ent i name ob pt pt1 pt2 tsy zhigao)
   (defun #errzch (s)
     (setvar "nomutt" 0)
     (redraw)
     (command ".UNDO" "E")
     (setq *error* $orr)
   )
   (setq $orr *error*)
   (setq *error* #errzch)
   (setvar "cmdecho" 0)
   (if (null panybak)
     (setq zhigao (getvar "DIMTXT")
    bili (getvar "DIMSCALE")
    panybak (* zhigao bili 2.5)
     )
   )
   (setq i (zct02))
   (setq tsy (car i))
   (setq dsy (cadr i))
   (while (not (and
   (setq ob (entsel "\n选择直线、文字或标注:"))
   (setq name (car ob))
   (setq pt (cadr ob))
   (setq ent (entget name))
   (member (cdr (assoc 0 ent)) '("TEXT" "MTEXT"
     "DIMENSION" "LINE"
    )
   )
        )
   )
     (if (= 52 (getvar "errno"))
       (vl-exit-with-error "")
     )
   )
   (command ".UNDO" "BE")
   (if (= (cdr (assoc 0 ent)) "DIMENSION")
     (progn
       (setq pt1 (cdr (assoc 13 ent))
      pt2 (cdr (assoc 14 ent))
      dm (cdr (assoc 1 ent))
       )
       (zct04 pt1 pt2 dsy nil dm)
     )
   )
   (if (member (cdr (assoc 0 ent)) '("TEXT" "MTEXT"))
     (progn
       (setq pt1 (cdr (assoc 10 ent)))
       (zct03 name ent pt1 tsy nil)
     )
   )
   (if (= (cdr (assoc 0 ent)) "LINE")
     (progn
       (setq pt1 (cdr (assoc 10 ent))
      pt2 (cdr (assoc 11 ent))
      strbak (rtos (distance pt1 pt2) 2 2)
       )
       (if (< (distance pt1 pt) (distance pt2 pt))
 (setq pt1 pt2)
       )
       (entdel name)
       (zct01 pt1)
     )
   )
   (setq *error* $orr)
   (princ)
 )
 (defun zct01 (pt1 / ang1 code code1 color d data ent ent1 gr gr1 i k l lstpt lx name nearpt nearpt2 osmo pd ptls10 ptls11 s ss stl x
     x0 x1 xunh y0 y1 z1
       )
   (if (null strbak)
     (setq strbak "100")
   )
   (setq color (vla-get-autosnapmarkercolor (vla-get-drafting (vla-get-preferences (vlax-get-acad-object)))))
   (setq xunh t)
   (while (progn
     (setq gr (grread t 15 0)
    code (car gr)
    data (cadr gr)
     )
     (cond
       ((= code 2)        ; 键盘区域
         (if (member data '(13 32))
    (progn
      (setq pt (polar pt1 (angle pt1 pt) (atof strbak)))
      (entmod (subst
         (cons 11 pt)
         (assoc 11 ent)
         ent
       )
      )
      (setq pt1 pt)
      (setq pd "Y")
    )
         )
         (if (member data '(48 49 50 51 52 53 54 55 56 57))
    (progn
      (setq s (chr data))
      (princ (strcat s))
      (while (progn
        (setq gr1 (grread)
       code1 (car gr1)
       lx (cadr gr1)
        )
        (if (member lx '(46 48 49 50 51 52 53 54 55 56 57 8))
          (progn
     (if (and
           (> (setq stl (strlen s))
       0
           )
           (= lx 8)
         )  ; 当有键盘输入按了退格
       (progn
         (setq s (substr s 1 (1- stl))) ; 删除一个字
         (princ (strcat "\n距离:" s))
       )    ; 符并换行
     )
     (if (not (member lx '(8 13 32)))
       (progn
         (setq s (strcat s (chr lx)))
         (princ (strcat (chr lx)))
       )
     )      ; 当有键盘输入按了退格
     (if (= (strlen s) 0)
       (princ (strcat "\n距离:<" strbak ">"))
     )
          )
        )
        (and
          (not (member lx '(13 32)))
          (not (member code1 '(11 25)))
        )
      )
      )
      (if (> (strlen s) 0)
        (setq strbak s)
      )
      (setq pt (polar pt1 (angle pt1 pt) (atof strbak)))
      (entmod (subst
         (cons 11 pt)
         (assoc 11 ent)
         ent
       )
      )
      (setq pt1 pt)
      (setq pd "Y")
    )
         )
       )
       ((= code 3)        ; 鼠标左击
         (command ".UNDO" "E")
         (redraw)
         (setq strbak (rtos (distance pt1 pt) 2 2))
         (princ strbak)
         (setq pt1 pt)
         (setq pd "Y")
       )
       ((= code 5)        ; 鼠标移动
         (if name
    (entdel name)
         )         ; 删除插入的图块
         (if (>= (getvar "osmode") 16384)
    (setq nearpt nil)
    (progn
      (setq k (* 1.5 (getvar "pickbox") (/ (getvar "viewsize") (cadr (getvar "screensize")))))
      (if (setq nearpt (osnap data "_END,_MID,_CEN,_NOD,_QUA,_INT,_INS,_TAN,_EXT")) ; 取得最近的捕捉点
        (setq osmo 1)
      )
      (if (and
     (setq nearpt2 (osnap data "_NEA")) ; 取得最近的捕捉点
     (not (equal nearpt nearpt2 k))
          )
        (setq osmo 2
       nearpt nearpt2
        )
      )
      (if (and
     (setq nearpt2 (osnap data "_MID")) ; 取得最近的捕捉点
     (equal nearpt nearpt2 k)
          )
        (setq osmo 3
       nearpt nearpt2
        )
      )
    )
         )
         (if name
    (entdel name)
         )         ; 恢复刚才删除的图块
         (if (= nearpt nil)
    (progn
      (redraw)
      (if (/= pd "N")
        (progn
          (command "Undo" "BE")
          (entmake (list '(0 . "LINE") (cons 10 pt1) (cons 11 pt1)))
          (setq name (entlast)
         ent (entget name)
         pd "N"
          )
          (princ (strcat "\n指定下一点,或输入距离:空格默认<" strbak ">"))
        )
      )
      (setq l (distance pt1 data))
      (setq d (* (setq ang1 (angle pt1 data))
          (/ 180 pi)
       )
      )
      (setq x0 (car pt1)
     y0 (cadr pt1)
     x1 (car data)
     y1 (cadr data)
     z1 (caddr data)
      )
      (if (or
     (and
       (> d 0)
       (< d 60)
     )
     (and
       (> d 180)
       (< d 240)
     )
          )
        (progn
          (setq pt (list (- x1 (* l (sin (- (* 30 (/ pi 180)) ang1)) (sin (* 30 (/ pi 180))))) (+ y1 (* l (sin
                    (-
                       (* 30
                   (/ pi 180)
                       ) ang1
                    )
                      )
                    (cos (* 30
                     (/ pi
                 180
                     )
                  )
                    )
                 )
                  ) z1
     )
          )
          (entmod (subst
      (cons 11 pt)
      (assoc 11 ent)
      ent
           )
          )
        )
      )
      (if (or
     (and
       (> d 120)
       (< d 180)
     )
     (and
       (> d 300)
       (< d 360)
     )
          )
        (progn
          (setq pt (list (+ x1 (* l (sin (- ang1 (* 150 (/ pi 180)))) (sin (* 30 (/ pi 180))))) (+ y1 (* l (sin
                     (- ang1
                 (* 150
                    (/ pi 180)
                 )
                     )
                )
                     (cos
                   (* 30
                      (/ pi
                  180
                      )
                   )
                     )
                  )
                   ) z1
     )
          )
          (entmod (subst
      (cons 11 pt)
      (assoc 11 ent)
      ent
           )
          )
        )
      )
      (if (or
     (and
       (> d 60)
       (< d 120)
     )
     (and
       (> d 240)
       (< d 300)
     )
          )
        (progn
          (setq pt (list x0 y1 z1))
          (entmod (subst
      (cons 11 pt)
      (assoc 11 ent)
      ent
           )
          )
        )
      )
    )
    (progn
      (setq ent1 nil)
      (if (setq ss (ssget "C" nearpt nearpt '((0 . "LINE"))))
        (repeat (setq i (sslength ss))
          (setq x (ssname ss (setq i (1- i))))
          (if (/= x name)
     (setq ent1 x)
          )
        )
      )
      (if ent1
        (progn
          (setq ent1 (entget ent1)
         ptls10 (cdr (assoc 10 ent1))
         ptls11 (cdr (assoc 11 ent1))
          )
          (setq lstpt (mapcar
          '(lambda (x)
      (inters
        pt1
        (polar pt1 (* x (/ pi 180)) 100)
        ptls10
        ptls11
        nil
      )
           )
          '(30 150 90)
        )
          )
          (setq k (* 1.5 (getvar "pickbox") (/ (getvar "viewsize") (cadr (getvar "screensize")))))
          (foreach x lstpt
     (if x
       (if (< (distance nearpt x) k)
         (setq nearpt x
        osmo 4
         )
       )
     )
          )
        )
      )
      (setq pt nearpt)
      (if (/= pd "N")
        (progn
          (command "Undo" "BE")
          (entmake (list '(0 . "LINE") (cons 10 pt1) (cons 11 pt1)))
          (setq name (entlast)
         ent (entget name)
         pd "N"
          )
          (princ (strcat "\n指定下一点,或输入距离:空格默认<" strbak ">"))
        )
      )
      (if pt
        (entmod (subst
           (cons 11 pt)
           (assoc 11 ent)
           ent
         )
        )
      )
      (redraw)
      (zct05 pt color osmo)
    )
         )
       )
       ((or
   (= code 11)
   (= code 25)
        )          ; 鼠标右击
         (if name
    (entdel name)
         )
         (command ".UNDO" "E")
         (redraw)
         (setq xunh nil)        ;      (vl-exit-with-error "")
       )
       (t
       )
     )
     xunh
   )
   )
 )
 ;;; 以下部分是共用子函数
 (defun zct02 (/ ctrview dsy name pt0 pt1 pt2 pt3 pt4 pt5 sizeview snap thr tsy tsyent)
   (setvar "cmdecho" 0)
   (setq tsy (getvar "TEXTSTYLE"))
   (setq dsy (getvar "DIMSTYLE"))
   (setq tsyent (tblsearch "style" tsy))
   (setq thr (strcat (cdr (assoc 3 tsyent)) "," (cdr (assoc 4 tsyent))))
   (if (not (tblsearch "style" "+30"))  ; 文字样式"+30"，无则创建
     (progn
       (command "_style" "+30" thr 0 0.8 30 "N" "N")
       (if (/= 0 (getvar "cmdactive"))
 (command "N")
       )
     )
   )
   (if (not (tblsearch "style" "-30"))  ; 文字样式"-30"，无则创建
     (progn
       (command "_style" "-30" thr 0 0.8 -30 "N" "N")
       (if (/= 0 (getvar "cmdactive"))
 (command "N")
       )
     )
   )
   (if (not (tblsearch "dimstyle" "+30")) ; 标注样式"+30"，无则创建
     (progn
       (setq snap (getvar "osmode"))
       (setvar "osmode" 0)
       (setvar "cecolor" (itoa (getvar "DIMCLRD")))
       (setq sizeview (getvar "viewsize"))
       (setq ctrview (getvar "viewctr"))
       (setq pt0 '(0.0 0.0 0.0)
      pt1 '(-1.10647 0.184412 0.0)
      pt2 '(-0.89353 -0.184412 0.0)
      pt3 '(-1.10647 -0.184412 0.0)
      pt4 '(-0.89353 0.184412 0.0)
      pt5 '(-0.7 0.0 0.0)
       )
       (command "zoom" "c" pt0 3)
       (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 90 4) (cons 10 pt0) (cons 10 pt1)
        (cons 10 pt2) (cons 10 pt0)
         )
       )
       (setq name (entlast))
       (command "bhatch" "p" "SOLID" pt5 "")
       (entdel name)
       (command "block" "箭头2" pt0 (entlast) "")
       (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 90 4) (cons 10 pt0) (cons 10 pt3)
        (cons 10 pt4) (cons 10 pt0)
         )
       )
       (setq name (entlast))
       (command "bhatch" "p" "SOLID" pt5 "")
       (entdel name)
       (command "block" "箭头1" pt0 (entlast) "")
       (command "zoom" "c" ctrview sizeview)
       (setvar "cecolor" "bylayer")
       (setvar "osmode" snap)
       (command "DIMTXSTY" "+30" "DIMBLK1" "箭头1" "DIMBLK2" "箭头1" "DIMSAH" 1 "DIMSTYLE" "S" "+30")
     )
   )
   (if (not (tblsearch "dimstyle" "-30")) ; 标注样式"-30"，无则创建
     (progn
       (command "DIMTXSTY" "-30" "DIMBLK1" "箭头2" "DIMBLK2" "箭头2" "DIMSAH" 1 "DIMSTYLE" "S" "-30")
     )
   )
   (setvar "TEXTSTYLE" tsy)
   (command "dimstyle" "r" dsy)
   (list tsy dsy)
 )
 (defun zct03 (name ent pt1 tsy ch / code color d data gr pd xunh)
   (setq xunh t)
   (setq color (vla-get-autosnapmarkercolor (vla-get-drafting (vla-get-preferences (vlax-get-acad-object)))))
   (princ "\n移动鼠标改变文字方向:")
   (while (progn
     (setq gr (grread t 15 0)
    code (car gr)
    data (cadr gr)
     )
     (cond
       ((= code 2)        ; 当键盘输入且不是空格或回车
         (if (member data '(13 32))
    (progn
      (command ".UNDO" "E")
      (redraw)
      (setq xunh nil)
    )
         )
       )
       ((= code 3)        ; 鼠标左击
         (command ".UNDO" "E")
         (redraw)
         (setq xunh nil)        ;         (vl-exit-with-error "")
       )
       ((= code 5)        ; 鼠标移动
         (redraw)
         (zct05 pt1 (getvar "GRIPCOLOR") 1)
         (setq d (* (angle pt1 data) (/ 180 pi)))
         (cond
    ((and
       (> d 300)
       (< d 330)
     )
      (if (/= pd 1)
        (progn
          (entmod (subst
      (cons 50 (* 330 (/ pi 180)))
      (assoc 50 ent)
      ent
           )
          )
          (command ".change" name "" "" "" "+30" "" "" "")
          (setq pd 1)
        )
      )
    )
    ((and
       (> d 330)
       (< d 360)
     )
      (if (/= pd 2)
        (progn
          (entmod (subst
      (cons 50 (* 330 (/ pi 180)))
      (assoc 50 ent)
      ent
           )
          )
          (command ".change" name "" "" "" "-30" "" "" "")
          (setq pd 2)
        )
      )
    )
    ((and
       (>= d 0)
       (< d 30)
     )
      (if (/= pd 3)
        (progn
          (entmod (subst
      (cons 50 (* 30 (/ pi 180)))
      (assoc 50 ent)
      ent
           )
          )
          (command ".change" name "" "" "" "+30" "" "" "")
          (setq pd 3)
        )
      )
    )
    ((and
       (> d 30)
       (< d 60)
     )
      (if (/= pd 4)
        (progn
          (entmod (subst
      (cons 50 (* 30 (/ pi 180)))
      (assoc 50 ent)
      ent
           )
          )
          (command ".change" name "" "" "" "-30" "" "" "")
          (setq pd 4)
        )
      )
    )
    ((and
       (> d 60)
       (< d 90)
     )
      (if (/= pd 5)
        (progn
          (entmod (subst
      (cons 50 (* 90 (/ pi 180)))
      (assoc 50 ent)
      ent
           )
          )
          (command ".change" name "" "" "" "+30" "" "" "")
          (setq pd 5)
        )
      )
    )
    ((and
       (> d 90)
       (< d 120)
     )
      (if (/= pd 6)
        (progn
          (entmod (subst
      (cons 50 (* 90 (/ pi 180)))
      (assoc 50 ent)
      ent
           )
          )
          (command ".change" name "" "" "" "-30" "" "" "")
          (setq pd 6)
        )
      )
    )
    ((and
       (> d 120)
       (< d 210)
     )
      (if (/= pd 7)
        (progn
          (entmod (subst
      (cons 50 (/ pi 2))
      (assoc 50 ent)
      ent
           )
          )
          (command ".change" name "" "" "" tsy "" "" "")
          (setq pd 7)
        )
      )
    )
    (t
      (if (/= pd 8)
        (progn
          (entmod (subst
      (cons 50 0)
      (assoc 50 ent)
      ent
           )
          )
          (command ".change" name "" "" "" tsy "" "" "")
          (setq pd 8)
        )
      )
    )
         )
       )
       ((or
   (= code 11)
   (= code 25)
        )          ; 鼠标右击
         (if (and
        name
        ch
      )
    (entdel name)
         )
         (redraw)
         (command ".UNDO" "E")
         (setq xunh nil)        ;         (vl-exit-with-error "")
       )
       (t
       )
     )
     xunh
   )
   )
 )
 (defun zct04 (pt1 pt2 dsy ch dm / code color d data ent gr jd nearpt pd pdzc s)
   (setq jd (fix (+ (/ (zctbz03 (angle pt1 pt2)) 10) 0.5)))
   (setq color (vla-get-autosnapmarkercolor (vla-get-drafting (vla-get-preferences (vlax-get-acad-object)))))
   (if (member jd '(33 15 3 21 9 27))
     (progn
       (princ (strcat "\n空格默认尺寸距线[重设(S)]:< " (rtos panybak) " >"))
       (while (progn
         (setq gr (grread t 15 0)
        code (car gr)
        data (cadr gr)
         )
         (cond
    ((= code 2)        ; 键盘区域
      (if (= data 115)
        (if (setq s (getreal (strcat "\n<" (rtos panybak) ">")))
          (setq panybak s
         panybak1 (list (getvar "DIMSCALE") panybak)
          )
        )
      )
      (if (member data '(13 32))
        (zctbz02 name nil)
      )
      (princ (strcat "\n空格默认尺寸距线[重设(S)]:< " (rtos panybak) " >"))
    )
    ((= code 3)        ; 鼠标左击
      (redraw)
      (command ".UNDO" "E")
      (vl-exit-with-error "") ;      (zctbz02 name nil)
    )
    ((= code 5)        ; 鼠标移动
      (if name
        (entdel name)
      )         ; 删除插入的图块
      (if (>= (getvar "osmode") 16384)
        (setq nearpt nil)
        (progn
          (setq k (* 1.5 (getvar "pickbox") (/ (getvar "viewsize") (cadr (getvar "screensize")))))
          (if (setq nearpt (osnap data "_END,_CEN,_MID,_NOD,_QUA,_INT,_INS,_TAN,_EXT")) ; 取得最近的捕捉点
     (setq osmo 1)
          )
          (if (and
         (setq nearpt2 (osnap data "_NEA")) ; 取得最近的捕捉点
         (not (equal nearpt nearpt2 k))
       )
     (setq osmo 2
           nearpt nearpt2
     )
          )
          (if (and
         (setq nearpt2 (osnap data "_MID")) ; 取得最近的捕捉点
         (equal nearpt nearpt2 k)
       )
     (setq osmo 3
           nearpt nearpt2
     )
          )
        )
      )
      (if name
        (entdel name)
      )         ; 恢复刚才删除的图块
      (if (= nearpt nil)
        (progn
          (redraw)
          (zct05 pt1 (getvar "GRIPCOLOR") 1)
          (setq d (zctbz03 (angle pt1 data)))
          (if (or
         (= jd 33)
         (= jd 15)
       )
     (if (or
           (and
      (> d 60)
      (< d 150)
           )
           (and
      (> d 240)
      (< d 330)
           )
         )
       (progn
         (if (/= pd "Y")
           (zctbz04 "-90" "-30" "Y" dm)
         )
         (setq ent (zctbz05 data ent))
       )
       (progn
         (if (/= pd "N")
           (zctbz04 "30" "+30" "N" dm)
         )
         (setq ent (zctbz05 data ent))
       )
     )
          )
          (if (or
         (= jd 3)
         (= jd 21)
       )
     (if (or
           (and
      (> d 30)
      (< d 120)
           )
           (and
      (> d 210)
      (< d 300)
           )
         )
       (progn
         (if (/= pd "Y")
           (zctbz04 "-90" "+30" "Y" dm)
         )
         (setq ent (zctbz05 data ent))
       )
       (progn
         (if (/= pd "N")
           (zctbz04 "-30" "-30" "N" dm)
         )
         (setq ent (zctbz05 data ent))
       )
     )
          )
          (if (or
         (= jd 9)
         (= jd 27)
       )
     (if (or
           (and
      (> d 90)
      (< d 180)
           )
           (> d 270)
         )
       (progn
         (if (/= pd "N")
           (zctbz04 "-30" "+30" "N" dm)
         )
         (setq ent (zctbz05 data ent))
       )
       (progn
         (if (/= pd "Y")
           (zctbz04 "30" "-30" "Y" dm)
         )
         (setq ent (zctbz05 data ent))
       )
     )
          )
        )
        (progn
          (setq pt nearpt)
          (setq ent (zctbz05 pt ent))
          (redraw)
          (zct05 pt color osmo)
          (zct05 pt1 (getvar "GRIPCOLOR") 1)
        )
      )
    )
    ((or
       (= code 11)
       (= code 25)
     )         ; 鼠标右击
      (if (and
     name
     ch
          )
        (entdel name)
      )
      (redraw)
      (command ".UNDO" "E")
      (vl-exit-with-error "")
    )
    (t
    )
         )
         t
       )
       )
     )
     (progn
       (princ "\n指定尺寸线位置:")
       (if (and
      (not ch)
      name
    )
 (entdel name)
       )
       (setq pdzc "Y")
       (command "dimstyle" "r" dsy)
       (command "dimaligned" pt1 pt2 pause)
       (setq name (entlast))
       (setq ent (entget name))
       (if dm
 (entmod (subst
     (cons 1 dm)
     (assoc 1 ent)
     ent
   )
 )
       )
     )
   )
 )
 (defun zct05 (pt color osmo / d h lst pt1 pt2 pt3 pt4 ptx pty x) ; 显示捕捉方框
   (setq h (/ (getvar "viewsize") (cadr (getvar "screensize")))
 d (getvar "pickbox")
 lst (list (* d h) (* (- d 0.5) h) (* (+ d 0.5) h))
 ptx (car pt)
 pty (cadr pt)
   )
   (foreach x lst
     (setq pt1 (list (- ptx x) (- pty x))
    pt2 (list (+ ptx x) (- pty x))
    pt3 (list (+ ptx x) (+ pty x))
    pt4 (list (- ptx x) (+ pty x))
    pt5 (list ptx (+ pty x))
     )
     (if (= osmo 1)
       (grvecs (list color pt1 pt2 pt2 pt3 pt3 pt4 pt4 pt1))
     )
     (if (= osmo 2)
       (grvecs (list color pt1 pt2 pt2 pt4 pt3 pt4 pt3 pt1))
     )
     (if (= osmo 3)
       (grvecs (list color pt1 pt2 pt2 pt5 pt5 pt1))
     )
     (if (= osmo 4)
       (grvecs (list color pt1 pt2 pt1 pt4 pt (list (- ptx x) pty) pt (list ptx (- pty x))))
     )
   )
 )
 ;;; 自动创建"动态轴测图"工具栏子程序
 (defun create_toolbar (/ file lst x)
   (setvar "cmdecho" 0)
   (vl-load-com)
   (if (not (menugroup "TEMPTOOLBAR"))  ; 判断cad是否加载temptoolbar工具栏
     (progn          ; 如果没加载，则自动执行下面程序
       (alert "\n程序自动创建\"动态轴测图\"工具栏，请稍后")
       (setq file (open "TEMPTOOLBAR.mnu" "W")) ; 在cad搜索目录创建一个菜单mnu文件,写状态打开
       (foreach x '("***MENUGROUP=TEMPTOOLBAR\n\n***TOOLBARS\n**TEMPTOOLBAR" ; 下面定义工具栏内容，最好采用cad自带图标
   "ID_ZCT001   [_Toolbar(\"动态轴测图\", _Floating, _Hide, 10, 340, 1)]"
   "ID_ZCT011   [_Button(\"轴测图直线\", \"RCDATA_16_OSNEND\", \"RCDATA_16_OSNEND\")]^C^Czline"
   "ID_ZCT012   [_Button(\"轴测图标注\", \"RCDATA_16_DIMOBL\", \"RCDATA_16_DIMOBL\")]^C^Czdim"
   "ID_ZCT013   [_Button(\"轴测图文字\", \"RCDATA_16_TEXEDI\", \"RCDATA_16_TEXEDI\")]^C^Cztext"
   "ID_ZCT014   [_Button(\"轴测图更改\", \"RCDATA_16_DIMOVE\", \"RCDATA_16_DIMOVE\")]^C^Czch"
 )
 (princ (strcat x "\n") file)   ; 将定义工具栏内容写入mnu文件
       )
       (close file)         ; 关闭mnu文件
       (command "menuload" "TEMPTOOLBAR.mnu") ; 加载temptoolbar工具栏
     )
   )
   (command "TOOLBAR" "动态轴测图" "S") ; 显示工具栏
   (princ)
 )
 (create_toolbar)
 ;;; 本句的作用是加载本lisp就自动执行create_toolbar子程序
 (princ "\n    《动态轴测图》\n   轴测图直线: zline\n   轴测图标注: zdim\n   轴测图文字: ztext\n   轴测图更改: zch\n           by: langjs")
 ;;; 下面是命令反应器,程序监测到quit关闭cad命令则卸载临时菜单
 (defun vlr_command_l ()
   (vl-load-com)
   (vlr-command-reactor nil '((:vlr-commandwillstart . startcommand_l)))
 )
 (defun startcommand_l (calling-reactor startcommandinfo)
   (if (= (car startcommandinfo) "QUIT")
     (progn
       (vl-cmdf "menuunload" "TEMPTOOLBAR")
       (if (vl-file-delete "TEMPTOOLBAR.mnu")
 (princ "\n 已删除TEMPTOOLBAR.mnu文件")
       )
       (if (vl-file-delete "TEMPTOOLBAR.cui")
 (princ "\n 已删除TEMPTOOLBAR.cui文件")
       )
       (if (vl-file-delete "TEMPTOOLBAR.mnr")
 (princ "\n 已删除TEMPTOOLBAR.mnr文件")
       )
       (if (vl-file-delete "TEMPTOOLBAR.mns")
 (princ "\n 已删除TEMPTOOLBAR.mns文件")
       )
       (if (vl-file-delete "TEMPTOOLBAR.mnc")
 (princ "\n 已删除TEMPTOOLBAR.mnc文件")
       )
     )
   )
   (princ)
 )
 (vlr_command_l)
 (princ)