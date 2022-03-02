
;�����������
;================================================================
(defun c:kk (/ $fontlist$ $sydzt$ *acad* *doc* addlist av:changefontsdcl dclid desetpopx-0 desetpopx-1 deshow fname getpop key setcusfontstype setpop1-0 setpop1-1 setpop2-0 setpop2-1)
  (defun av:changefontsdcl (fname / dcls fn)
    (setq dcls
			(list
				"fonts:dialog {"
				"    initial_focus = \"13\" ;"
				"    children_alignment = left ;"
				"    children_fixed_width = true ;"
				"    key = \"k00\" ;"
				"    label = \"��Aά���졿������������������\" ;"
				"    :row {"
				"        :button {"
				"            key = \"11\" ;"
				"            label = \"1.�������(&K)\" ;"
				"        }"
				"        :text {"
				"            label = \"��ȱ�������Ϊ�������������\" ;"
				"        }"
				"    }"
				"    :row {"
				"        :button {"
				"            key = \"12\" ;"
				"            label = \"2.��������(&F)\" ;"
				"        }"
				"        :text {"
				"            label = \"����������������Ͱ������\" ;"
				"        }"
				"    }"
				"    :row {"
				"        :button {"
				"            key = \"13\" ;"
				"            label = \"3.��������(&Z)\" ;"
				"        }"
				"        :text {"
				"            label = \"ȫ�����͵���Ϊtssdeng��hztxt\" ;"
				"        }"
				"    }"
				"    :row {"
				"        :button {"
				"            key = \"14\" ;"
				"            label = \"4.��������(&G)\" ;"
				"        }"
				"        :text {"
				"            label = \"ȫ�����͵���Ϊgbenor��gbcbig\" ;"
				"        }"
				"    }"
				"    :row {"
				"        :button {"
				"            key = \"15\" ;"
				"            label = \"5.��������(&S)\" ;"
				"        }"
				"        :text {"
				"            label = \"ȫ�����͵���Ϊ��������\" ;"
				"        }"
				"    }"
				"    :row {"
				"        :button {"
				"            key = \"16\" ;"
				"            label = \"6.�ֲ�����(&R)\" ;"
				"        }"
				"        :text {"
				"            label = \"�������������޷�ˢ�µ�����\" ;"
				"        }"
				"    }"
				"    spacer;"
				"    :boxed_column {"
				"        fixed_width = true ;"
				"        label = \"-7.�Զ�������\" ;"
				"        :row {"
				"            :column {"
				"                :row {"
				"                    :text {"
				"                        fixed_width = true ;"
				"                        key = \"pop1-t\" ;"
				"                        label = \"����1:\" ;"
				"                        width = 8 ;"
				"                    }"
				"                    :popup_list {"
				"                        fixed_width = true ;"
				"                        key = \"pop1\" ;"
				"                        width = 21 ;"
				"                    }"
				"                }"
				"                :row {"
				"                    :text {"
				"                        fixed_width = true ;"
				"                        key = \"pop2-t\" ;"
				"                        label = \"����2:\" ;"
				"                        width = 8 ;"
				"                    }"
				"                    :popup_list {"
				"                        fixed_width = true ;"
				"                        key = \"pop2\" ;"
				"                        width = 21 ;"
				"                    }"
				"                }"
				"            }"
				"            :column {"
				"                fixed_width = true ;"
				"                width = 10 ;"
				"                spacer;"
				"                :toggle {"
				"                    key = \"sydzt\" ;"
				"                    label = \"ʹ�ô�����\" ;"
				"                }"
				"                :button {"
				"                    key = \"ok\" ;"
				"                    label = \"�޸�\" ;"
				"                }"
				"            }"
				"        }"
				"    }"
				"    :button {"
				"        is_cancel = true ;"
				"        is_enabled = false ;"
				"        label = \"��ʾ������������������ͣ�ͼ��������Ϊ˳����\" ;"
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
	
	;AutoCAD-86 bigfont 1.0������
	;AutoCAD-86 unifont 1.0��������
	;AutoCAD-86 shapes 1.0���ļ�
	;ȡ���ض����������б�
	(defun $fontlist$ (txt / $fonttype$ delsame dir1 dirlist fontlist fonts getfontlist)
		;�ж���������
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
		;�õ��ļ����ڶ�Ӧ�����б�
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
		;�����б����ظ�����
		(defun delsame(biao)
			(if biao
				(setq biao (cons (car biao) (delsame (vl-remove (car biao) (cdr biao)))))
			)
			biao
		)
		;��ѯ����ļ����������б�
		(and *fstl_dir* (setq dir1 (findfile (strcat *fstl_dir* "\\support"))))
		(setq dirlist (list dir1 (findfile "fonts")))
		(foreach dir dirlist
			(setq fonts (getfontlist dir txt))
			(setq fontlist (append fontlist fonts))
		)
		(setq fontlist (delsame fontlist));�б�ȥ��
		;(setq fontlist (acad_strlsort fontlist));�б�����
	)
	;�Ի���ؼ����
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
	;д���б�ļ��д��
	;(defun setpop(fonts)
	;	(start_list "pop1")
	;	(mapcar 'add_list fonts)
	;	(end_list)
	;)
	;���ð�ť����
	(defun setpop1-1(/ npop1 shx shx1 shx2)
		(setq shx1 ($fontlist$ "*unifont*"));��������
		(setq shx2 ($fontlist$ "*shapes*"));������
		(setq shx (acad_strlsort(append shx1 shx2)))
		(mode_tile "pop1" 0)
		(AddList "pop1" shx)
		(setq npop1 (vl-position "txt.shx" shx))
		(and npop1 (set_tile "pop1" (itoa npop1)))
		(set_tile "pop1-t" "SHX����:")
		shx
	)
	(defun setpop2-1(/ bigs npop2)
		(setq bigs ($fontlist$ "*bigfont*"));������
		(mode_tile "pop2" 0)
		(AddList "pop2" bigs)
		(setq npop2 (vl-position "tssdchn.shx" bigs))
		(and npop2 (set_tile "pop2" (itoa npop2)))
		(set_tile "pop2-t" "������:")
		bigs
	)
	(defun setpop1-0(/ npop1 ttfs)
		(set_tile "pop1-t" "������:")
		(setq ttfs (list "΢���ź�" "����" "����" "����" "����" "������"))
		(AddList "pop1" ttfs)
		(setq npop1 (vl-position "΢���ź�" ttfs))
		(and npop1 (set_tile "pop1" (itoa npop1)))
		ttfs
	)
	(defun setpop2-0()
		(set_tile "pop2-t" "��ʽ��:")
		(mode_tile "pop2" 1)
		(AddList "pop2" nil)
	)
	(defun desetpopx-1()(setq unis (setpop1-1))(setq bigs (setpop2-1)))
	(defun desetpopx-0()(setq ttfs (setpop1-0))(setpop2-0))
  (defun $sydzt$ (/ pick)
    (setq pick (get_tile "sydzt"))
    (if(= pick "1")(desetpopx-1)(desetpopx-0))
  )
	;����Ĭ����ʾ���
	(defun deshow()
		(progn(set_tile "sydzt" "0")(desetpopx-0)) ;������
		;(progn(set_tile "sydzt" "1")(desetpopx-1)) ;ϸ����
	)
	;ȡ���б�����
	(defun getpop(pop fontlist / font pick)
		(setq pick (atoi(get_tile pop)))
    (if fontlist (setq font (nth pick fontlist)))
    font
	)
	;�����Զ���������ʽ
	(defun setcusfontstype(/ pop1-0 pop1-1 pop2-1 sydzt)
		(setq pop1-1 (getpop "pop1" unis))
		(setq pop1-0 (getpop "pop1" ttfs))
		(setq pop2-1 (getpop "pop2" bigs))
		(setq sydzt (get_tile "sydzt"))
		(cond
			((= sydzt "0")(av:fontstottf pop1-0))
			((= sydzt "1")(av:fontstoshx pop1-1 pop2-1))
			(t nil)
		)
	)
	;��ʼ����
	(vl-load-com)
  (setvar "cmdecho" 0)
	(explode-screen-mtext);�ֽ⵱ǰ��Ļ��������ı�
	(setq *acad* (vlax-get-acad-object))
	(setq *doc* (vla-get-ActiveDocument *acad*))
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
	(action_tile "16" "(regenlocal)(done_dialog 0)")
	(action_tile "sydzt" "($sydzt$)")
	(action_tile "ok" "(setcusfontstype)(done_dialog 1)")
	(deshow);����Ĭ����ʾ���
	(setq key (start_dialog))
	(unload_dialog dclid)
	(cond
		((= key 11) (av:0totc "tssdeng" "hztxt" "����"))
		((= key 12) (av:fontstofh "tssdeng" "hztxt"))
		((= key 13) (av:fontstoshx "tssdeng" "hztxt"))
		((= key 14) (av:fontstoshx "gbenor.shx" "gbcbig.shx"))
		((= key 15) (av:fontstottf "����"))
		((= key 1) nil)
		(t (setq key nil))
	)
	(if key (repeat *n-regen* (vla-regen *doc* 0)))
  (vl-file-delete fname)
  (setvar "cmdecho" 1)
  (princ)
)

(vl-load-com)
(setq *acad* (vlax-get-acad-object))
(setq *doc* (vla-get-ActiveDocument *acad*))
(setq *n-regen* 1)
;================================================================

(defun av:toshx (x shxx shxb)
	(vla-put-fontfile x shxx)
	(vla-put-bigfontfile x shxb)
)
(defun av:tottf (x ttf)
	(vla-setfont x ttf b c d e)
)
(defun c:fontstofh()
	(setvar "cmdecho" 0)
	(av:fontstofh "tssdeng" "hztxt")
	(repeat *n-regen* (vla-regen *doc* 0))
  (setvar "cmdecho" 1)
  (princ)
)
(defun av:fontstofh(shxx shxb / xn)
	(vlax-for x (vla-get-textstyles *doc*)
		(vla-getfont x 'a 'b 'c 'd 'e)
		(setq xn (vla-get-name x))
		(cond
			((wcmatch xn "*����*") (av:tottf x "����"))
			((wcmatch xn "*����*") (av:tottf x "����"))
			((wcmatch xn "*����*") (av:tottf x "����"))
			((wcmatch xn "*����*") (av:tottf x "����"))
			(t (av:toshx x "tssdeng.shx" "hztxt.shx"))
		)
  )
	(princ (strcat "\n>>>�����滻Ϊshx����ͷ�������"))
)
(defun c:fontstofz()
	(setvar "cmdecho" 0)
	(av:fontstoshx "tssdeng" "hztxt")
	(repeat *n-regen* (vla-regen *doc* 0))
  (setvar "cmdecho" 1)
  (princ)
)
(defun c:fontstogb()
	(setvar "cmdecho" 0)
	(av:fontstoshx "gbenor.shx" "gbcbig.shx")
	(repeat *n-regen* (vla-regen *doc* 0))
  (setvar "cmdecho" 1)
  (princ)
)
(defun av:fontstoshx(shxx shxb)
	(vlax-for x (vla-get-textstyles *doc*)
		(av:toshx x shxx shxb)
		;(princ (entmod(entget(tblobjname "style" (vla-get-name x)))))
  )
	(princ (strcat "\n>>>�����滻Ϊ" shxx "��" shxb))
)
(defun c:fontstofs()
	(setvar "cmdecho" 0)
	(av:fontstottf "����")
	(repeat *n-regen* (vla-regen *doc* 0))
  (setvar "cmdecho" 1)
  (princ)
)
(defun av:fontstottf(ttf)
	(vlax-for x (vla-get-textstyles *doc*)
		(vla-getfont x 'a 'b 'c 'd 'e)
		(av:tottf x ttf)
  )
	(princ (strcat "\n>>>�����滻Ϊ" ttf))
)
(defun av:0totc	(shxx shxb ttf / big err shx)
	(vlax-for x	(vla-get-TextStyles *doc*)
		(vla-getfont x 'a 'b 'c 'd 'e)
		(if (= a "")
			(progn
				(cond
					((findfile (setq shx (strcat (vla-get-fontfile x) ".shx")))
						(vla-put-fontfile x shx)
					)
					((findfile (setq shx (vla-get-fontfile x)))
						(vla-put-fontfile x shx)
					)
					(t (vla-put-fontfile x shxx))
				)
				(cond
					((findfile (setq big (strcat (vla-get-bigfontfile x) ".shx")))
						(vla-put-bigfontfile x big)
					)
					((findfile (setq big (vla-get-bigfontfile x)))
						(vla-put-bigfontfile x big)
					)
					(t (vla-put-bigfontfile x shxb))
				)
			)
			(progn
				(setq err	(vl-catch-all-apply 'vla-setfont (list x a b c d e)))
				(if (vl-catch-all-error-p err) (vla-setfont x ttf b c d e))
			)
		)
	)
	(princ(strcat "\n>>>�����ͷֱ��滻Ϊ" shxx "��" shxb "��" ttf))
)

;================================================================

;�ֲ�ˢ��������
(defun c:r()
	(princ "-->�ֲ�������\n")
	(regenlocal)
)
(defun regenlocal (/ re1 re2 ss)
	(defun re1()(vl-cmdf "MOVE" ss "" "0,0,0" "0,0,0"))
	(defun re2()((lambda (i / e)(while (setq e (ssname ss (setq i (1+ i))))(entupd e)))-1))
	(setvar "cmdecho" 0)
	(if (setq ss (ssscreen))
		(if(null(re1))(re2))
		(princ "��Ļ�����������ݣ�")
	)
	(setvar "cmdecho" 1)
	(princ)
)
;ѡ��ǰ��Ļ��������
(defun ssscreen(/ $screen atio ce ch ch2 hh hh2 p1 p2 ss)
	(setq $screen (getvar "SCREENSIZE")) 
	(setq ch (getvar "viewsize")) 
	(setq ch2 (/ ch 2)) 
	(setq ce (getvar "viewctr")) 
	(setq atio (/ (car $screen) (cadr $screen))) 
	(setq hh (* atio ch)) 
	(setq hh2 (/ hh 2))
	(setq p1 (polar (polar ce 0 hh2) (* 1.5 pi) ch2))
	(setq p2 (polar (polar ce pi hh2) (* 0.5 pi) ch2))
	(setq ss (ssget "C" p1 p2))
)
;�ֽ⵱ǰ��Ļ��������ı�
(defun explode-screen-mtext (/ ss)
	(setvar "qaflags" 1)
	(and
		(setq ss (ssscreen))
		(setq ss (ssget "p" '((0 . "mtext"))))
		;(sssetfirst nil ss)
		(command "explode" ss "")
	)
	(setvar "qaflags" 0)
)
;=======================================================
;�����Զ���������
(defun ChangeFonts (/ file path)
	(setvar "cmdecho" 0)
	(if(null(and
						*fstl_dir*
						(setq path (strcat *fstl_dir* "\\Program\\ChangeFonts\\"))
						(setq file (findfile(strcat path "ChangeFonts2012.dll")))
						(vl-cmdf "netload" file)
						;(if (vl-cmdf "netload" file) t (progn(command-s "netload" file)t))
					))
		(progn
			(av:0totc "tssdeng" "hztxt" "����")
			(repeat *n-regen* (vla-regen *doc* 0))
		)
	)
	(setvar "cmdecho" 1)
	(vl-acad-undefun 'ChangeFonts)
	(princ)
)
(ChangeFonts)


(princ)


