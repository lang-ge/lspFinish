(vl-load-com)
(setq *acad* (vlax-get-acad-object))
(setq *doc* (vla-get-ActiveDocument *acad*))
;字体更换程序
;================================================================
(defun c:kk (/ $fontlist$ $sydzt$ addlist av:changefontsdcl dclid desetpopx-0 desetpopx-1 fname getpop key setcusfontstype setpop1-0 setpop1-1 setpop2-0 setpop2-1)
  (defun av:changefontsdcl (fname / dcls fn)
    (setq dcls
			(list
				"fonts:dialog {"
				"    initial_focus = \"13\" ;"
				"    children_alignment = left ;"
				"    children_fixed_width = true ;"
				"    key = \"k00\" ;"
				"    label = \"【A维制造】字体乱码整体解决方案\" ;"
				"    :row {"
				"        :button {"
				"            key = \"11\" ;"
				"            label = \"1.空字填充(&K)\" ;"
				"        }"
				"        :text {"
				"            label = \"空缺字体填充为方正与仿宋字型\" ;"
				"        }"
				"    }"
				"    :row {"
				"        :button {"
				"            key = \"12\" ;"
				"            label = \"2.复合字型(&F)\" ;"
				"        }"
				"        :text {"
				"            label = \"方正字型与粗体字型按需加载\" ;"
				"        }"
				"    }"
				"    :row {"
				"        :button {"
				"            key = \"13\" ;"
				"            label = \"3.方正字型(&Z)\" ;"
				"        }"
				"        :text {"
				"            label = \"全部字型调整为tssdeng与hztxt\" ;"
				"        }"
				"    }"
				"    :row {"
				"        :button {"
				"            key = \"14\" ;"
				"            label = \"4.国标字型(&G)\" ;"
				"        }"
				"        :text {"
				"            label = \"全部字型调整为gbenor与gbcbig\" ;"
				"        }"
				"    }"
				"    :row {"
				"        :button {"
				"            key = \"15\" ;"
				"            label = \"5.仿宋字型(&S)\" ;"
				"        }"
				"        :text {"
				"            label = \"全部字型调整为仿宋字体\" ;"
				"        }"
				"    }"
				"    :row {"
				"        :button {"
				"            key = \"16\" ;"
				"            label = \"6.局部生成(&R)\" ;"
				"        }"
				"        :text {"
				"            label = \"解决字体调整后无法刷新的问题\" ;"
				"        }"
				"    }"
				"    spacer;"
				"    :boxed_column {"
				"        fixed_width = true ;"
				"        label = \"-7.自定义字型\" ;"
				"        :row {"
				"            :column {"
				"                :row {"
				"                    :text {"
				"                        fixed_width = true ;"
				"                        key = \"pop1-t\" ;"
				"                        label = \"字体1:\" ;"
				"                        width = 12 ;"
				"                    }"
				"                    :popup_list {"
				"                        fixed_width = true ;"
				"                        key = \"pop1\" ;"
				"                        width = 17 ;"
				"                    }"
				"                }"
				"                :row {"
				"                    :text {"
				"                        fixed_width = true ;"
				"                        key = \"pop2-t\" ;"
				"                        label = \"字体2:\" ;"
				"                        width = 12 ;"
				"                    }"
				"                    :popup_list {"
				"                        fixed_width = true ;"
				"                        key = \"pop2\" ;"
				"                        width = 17 ;"
				"                    }"
				"                }"
				"            }"
				"            :column {"
				"                fixed_width = true ;"
				"                width = 10 ;"
				"                spacer;"
				"                :toggle {"
				"                    key = \"sydzt\" ;"
				"                    label = \"使用大字体\" ;"
				"                }"
				"                :button {"
				"                    key = \"ok\" ;"
				"                    label = \"修改\" ;"
				"                }"
				"            }"
				"        }"
				"    }"
				"    :button {"
				"        is_cancel = true ;"
				"        is_enabled = false ;"
				"        label = \"提示：方正字型与国标字型，图形缩放最为顺滑！\" ;"
				"    }"
				"    spacer;"
				"}"
	    )
    )
    (if	(setq fn (open fname "w"))
      (progn
				(foreach dcl dcls (write-line dcl fn))
				(close fn)
				fname
      )
    )
  )
	
	;AutoCAD-86 bigfont 1.0大字体
	;AutoCAD-86 unifont 1.0常规字体
	;AutoCAD-86 shapes 1.0形文件
	;取得特定类型字体列表
	(defun $fontlist$ (txt / $fonttype$ delsame dir1 dirlist fontlist fonts getfontlist)
		;判断字体类型
		(defun $fonttype$ (fn txt / cnt dv fh fonttype inp)
			(setq cnt 22 dv  "")
			(if	(setq fh (open fn "r"))
				(progn
					(while
						(and
							(> (setq cnt (1- cnt)) 0)
							(setq inp (read-char fh))
							(> inp 0)
						)
						(setq dv (strcat dv (chr inp)))
					)
					(close fh)
					(and dv (setq fonttype (WCMATCH (strcase dv t) txt)))
				)
			)
			fonttype
		)
		;得到文件夹内对应字型列表
		(defun getfontlist(dir txt / files fontlist)
			(setq files (vl-directory-files dir "*.shx"))
			(setq fontlist
				(vl-remove-if-not
					(function
						(lambda(a)($fonttype$ (strcat dir "\\" a) txt))
					)
					files
				)
			)
		)
		;消除列表内重复内容
		(defun delsame(biao)
			(if biao
				(setq biao (cons (car biao) (delsame (vl-remove (car biao) (cdr biao)))))
			)
			biao
		)
		;查询多个文件夹内字体列表
		(and *fstl_dir* (setq dir1 (findfile (strcat *fstl_dir* "\\support"))))
		(setq dirlist (list dir1 (findfile "fonts")))
		(foreach dir dirlist
			(setq fonts (getfontlist dir txt))
			(setq fontlist (append fontlist fonts))
		)
		(setq fontlist (delsame fontlist));列表去重
		;(setq fontlist (acad_strlsort fontlist));列表排序
	)
	;对话框控件填充
  (defun AddList (key lst)
    (IF	(AND key lst)
      (PROGN
				(if (= (type lst) 'str)(setq lst (list lst)))
				(start_list key)
				(foreach x lst (AND X (= (type x) 'str) (add_list x)))
				(end_list)
      )
      (PROGN
				(start_list key)
				(end_list)
      )
    )
    lst
  )
	;写入列表的简便写法
	;(defun setpop(fonts)
	;	(start_list "pop1")
	;	(mapcar 'add_list fonts)
	;	(end_list)
	;)
	;设置按钮类型
	(defun setpop1-1(/ unis)
		(setq unis ($fontlist$ "*unifont*"));常规字形
		(mode_tile "pop1" 0)
		(AddList "pop1" unis)
		(set_tile "pop1-t" "SHX字体(X):")
		unis
	)
	(defun setpop2-1(/ bigs)
		(setq bigs ($fontlist$ "*bigfont*"));大字形
		(mode_tile "pop2" 0)
		(AddList "pop2" bigs)
		(set_tile "pop2-t" "大字体(B):")
		bigs
	)
	(defun setpop1-0(/ ttfs)
		(set_tile "pop1-t" "字体名(F):")
		(setq ttfs (list "微软雅黑" "仿宋" "黑体" "楷体" "宋体" "新宋体"))
		(AddList "pop1" ttfs)
		ttfs
	)
	(defun setpop2-0()
		(set_tile "pop2-t" "字体样式(Y):")
		(mode_tile "pop2" 1)
		(AddList "pop2" nil)
	)
	(defun desetpopx-1()(setq unis (setpop1-1))(setq bigs (setpop2-1)))
	(defun desetpopx-0()(setq ttfs (setpop1-0))(setpop2-0))
  (defun $sydzt$ (/ pick)
    (setq pick (get_tile "sydzt"))
    (if(= pick "1")(desetpopx-1)(desetpopx-0))
  )
	;取得列表字体
	(defun getpop(pop fontlist / font pick)
		(setq pick (atoi(get_tile pop)))
    (if fontlist (setq font (nth pick fontlist)))
    font
	)
	;设置自定义字形样式
	(defun setcusfontstype(/ pop1-0 pop1-1 pop2-1 sydzt)
		(setq pop1-1 (getpop "pop1" unis))
		(setq pop1-0 (getpop "pop1" ttfs))
		(setq pop2-1 (getpop "pop2" bigs))
		(setq sydzt (get_tile "sydzt"))
		(cond
			((= sydzt "0")(av:fontstoshx nil nil pop1-0))
			((= sydzt "1")(av:fontstoshx pop1-1 pop2-1 nil))
			(t nil)
		)
	)
	;开始奔跑
	(vl-load-com)
  (setvar "cmdecho" 0)
	;(setq *acad* (vlax-get-acad-object))
	;(setq *doc* (vla-get-ActiveDocument *acad*))
  (and
		(setq fname (vl-filename-mktemp nil nil ".dcl"))
		(av:changefontsdcl fname)
		;(setq fname (findfile "fontsdcl.dcl"))
		(setq dclid (load_dialog fname))
  )
	(and dclid (new_dialog "fonts" dclid ""))
	(action_tile "11" "(done_dialog 11)")
	(action_tile "12" "(done_dialog 12)")
	(action_tile "13" "(done_dialog 13)")
	(action_tile "14" "(done_dialog 14)")
	(action_tile "15" "(done_dialog 15)")
	(action_tile "16" "(c:regenlocal)(done_dialog 0)")
	(action_tile "sydzt" "($sydzt$)")
	(action_tile "ok" "(setcusfontstype)(done_dialog 1)")
	(desetpopx-0);默认不使用大字体时的显示样式
	(setq key (start_dialog))
	(unload_dialog dclid)
	(cond
		((= key 11) (av:nulltoFonts "tssdeng" "hztxt" "仿宋"))
		((= key 12) (av:fontstoshx "tssdeng" "hztxt" t))
		((= key 13) (av:fontstoshx "tssdeng" "hztxt" nil))
		((= key 14) (av:fontstoshx "gbenor.shx" "gbcbig.shx" nil))
		((= key 15) (av:fontstoshx nil nil "仿宋"))
		((= key 1) nil)
		(t (setq key nil))
	)
	(if key (repeat 1 (vla-regen *doc* 0)))
  (vl-file-delete fname)
  (setvar "cmdecho" 1)
  (princ)
)


;================================================================


(defun av:fontstoshx (shxx shxb ttf / a1 a2 info toshx tottf)
	(defun toshx (shxx shxb / a3)
		(setq a3 (entget (tblobjname "style" a2))) ;取出字体的数据串行
		(setq a3 (subst (cons 3 shxx) (assoc 3 a3) a3))
		;将字体字型改成新字型
		(setq a3 (subst (cons 4 shxb) (assoc 4 a3) a3))
		;将字体字型改成新字型
		(entmod a3)			;更新字体
	)
	(defun tottf (ttf / font_obj obj)
		(setq font_obj (vla-get-TextStyles *doc*))
		(setq obj (vla-add font_obj a2));此处容易出错？？？
		;(vla-getfont obj 'a 'b 'c 'd 'e) 
		(vla-setFont obj ttf 0 0 134 2)
	)
	(setq a1 (tblnext "style" t))	;将指针移到第一个字体
	(while a1
		(setq a2 (cdr (assoc 2 a1)))	;取出字体名称
		(cond
			((and (/= shxx nil) (/= shxb nil) (/= ttf nil))
				(cond
					((wcmatch a2 "*仿宋*") (tottf "仿宋"))
					((wcmatch a2 "*宋体*") (tottf "宋体"))
					((wcmatch a2 "*黑体*") (tottf "黑体"))
					((wcmatch a2 "*楷体*") (tottf "楷体"))
					(t (toshx "tssdeng.shx" "hztxt.shx"))
				)
			)
			((and (/= shxx nil) (/= shxb nil) (= ttf nil))
				(toshx shxx shxb)
			)
			((and (= shxx nil) (= shxb nil) (/= ttf nil))
				(tottf ttf)
			)
		)
		(setq a1 (tblnext "style"))	;找出下一个字体
	)
	(cond
		((and (/= shxx nil) (/= shxb nil) (/= ttf nil))
			(setq info (strcat "shx字体和True字体"))
		)
		((and (/= shxx nil) (/= shxb nil) (= ttf nil))
			(setq info (strcat shxx "、" shxb))
		)
		((and (= shxx nil) (= shxb nil) (/= ttf nil))
			(setq info ttf)
		)
	)
	(princ (strcat "\n>>>字型替换为" info "。"))
	(princ)
)
(defun av:nulltoFonts	(shxx shxb ttf / err font_obj)
	;"不存在"的空字体批量处理程序。
	;(nulltoFonts "tssdeng" "tssdchn" "仿宋")
	(vl-load-com)
	(setq font_obj (vla-get-TextStyles *doc*))
	(vlax-for x	font_obj		;单独分离函数时需重新定义font_obj
		(vla-getfont x 'a 'b 'c 'd 'e)
		(if (= a "")
			(progn
				(if (and
							(not (findfile (vla-get-fontfile x)))
							(not (findfile (strcat (vla-get-fontfile x) ".shx")))
						)
					(vla-put-fontfile x shxx);此处容易出错？？？
				)
				(if
					(and
						(/= (vla-get-bigfontfile x) "")
						(not (findfile (vla-get-bigfontfile x)))
						(not (findfile (strcat (vla-get-bigfontfile x) ".shx")))
					)
					(vla-put-bigfontfile x shxb)
				)
				;(vla-put-Height font_obj 0);设置字高
				;(vla-put-width font_obj 0.8);设置字宽
			)
			(progn
				(setq
					err	(vl-catch-all-apply 'vla-setfont (list x a b c d e))
				)
				(if (vl-catch-all-error-p err)
					(vla-setfont x ttf b c d e)
					;(vla-setFont x ttf :vlax-false :vlax-false 134 2)
				)
			)
		)
	)
	(princ
		(strcat "\n>>>空字型分别替换为" shxx "、" shxb "、" ttf)
	)
)

;================================================================

;局部刷新重生成
(defun c:r()
	(princ "-->局部重生成\n")
	(c:regenlocal)
)
(defun c:regenlocal (/ $screen atio ce ch ch2 diftime hh hh2 immtime p1 p2 re1 re2 ss)
	(setvar "cmdecho" 0)
	(setq $screen (getvar "SCREENSIZE")) 
	(setq ch (getvar "viewsize")) 
	(setq ch2 (/ ch 2)) 
	(setq ce (getvar "viewctr")) 
	(setq atio (/ (car $screen) (cadr $screen))) 
	(setq hh (* atio ch)) (setq hh2 (/ hh 2))
	(setq p1 (polar (polar ce 0 hh2) (* 1.5 pi) ch2))
	(setq p2 (polar (polar ce pi hh2) (* 0.5 pi) ch2))
	(setq ss (ssget "C" p1 p2))
	(setq immtime (getvar "tdusrtimer"))
	(if oldimmtime (setq diftime (* (- immtime oldimmtime) 86400)))
	(defun re1()((lambda (i / e)(while (setq e (ssname ss (setq i (1+ i))))(entupd e)))-1))
	(defun re2()(vl-cmdf "MOVE" ss "" "0,0,0" "0,0,0"))
	(if ss (progn(cond
								 ((null oldimmtime)(setq oldimmtime (getvar "tdusrtimer"))(re1))
								 ((> diftime 0.2)(setq oldimmtime immtime)(re1))
								 (t (setq oldimmtime immtime)(re2))
							 )
					 ;(princ ">>>局部重生成")
				 )
		(princ "屏幕区域内无内容！")
	)
	(setvar "cmdecho" 1)
	(princ)
)

;================================================================
(defun ChangeFonts (/ file path)
	(setvar "cmdecho" 0)
	(if(null(and
						*fstl_dir*
						(setq path (strcat *fstl_dir* "\\Program\\ChangeFonts\\"))
						(setq file (findfile(strcat path "ChangeFonts2012.dll")))
						(vl-cmdf "netload" file)
						;(if (vl-cmdf "netload" file) t (progn(command-s "netload" file)t))
					))
		(av:nulltoFonts "tssdeng" "hztxt" "仿宋")
	)
	(setvar "cmdecho" 1)
	(vl-acad-undefun 'ChangeFonts)
	(princ)
)
(ChangeFonts)



(princ)


