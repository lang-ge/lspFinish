(defun c:sldview ()
;;;xshrimp 2011.8 
;;;选择跟目录有\\
(defun xzj-browsedir (msg / WinShell shFolder path catchit rtn)
  (if (null msg) (setq msg "选择目录"))
  (if (setq winshell (vlax-create-object "Shell.Application"))
    (progn
      (setq shFolder
                     (vlax-invoke-method WinShell 'BrowseForFolder 0 msg 1)
            catchit
                     (vl-catch-all-apply
                       '(lambda ()
                          (setq shFolder (vlax-get-property shFolder 'self))
                          (setq path     (vlax-get-property shFolder 'path))
                        )
                     )
      )
      (if shFolder (vlax-release-object shFolder))      
      (vlax-release-object winshell)
      (if (vl-catch-all-error-p catchit)
        (setq rtn nil)
        (setq rtn (vl-string-trim "\\/" path))        
      )
    )
  )
  rtn
)

(Defun xzj-sld-Zoom (zx / zx0 zy0)
       (start_image "image")
       (Fill_Image 0 0 dimx dimy -2)
       (End_Image)
       (SetQ zx0 (Fix (* 0.5 (- xicon zx)))
             zy0 (Fix (* 0.5 0.6725 (- xicon zx)))
           xicon zx
       )
       (xzj-sld-Fill "image" (SetQ x0 (+ x0 zx0)) (SetQ y0 (+ y0 zy0)) xicon slbfullname$)
)
(Defun xzj-sld-Pan (px py)
       (Start_Image "image")
       (Fill_Image 0 0 dimx dimy -2)
       (End_Image)
       (xzj-sld-Fill "image" (SetQ x0 (+ x0 px)) (SetQ y0 (+ y0 py)) xicon slbfullname$)
)

(Defun xzj-sld-Fill (fkey fx0 fy0 fx fsld)
       (Start_Image fkey)
       (Slide_Image fx0 fy0 (Fix fx) (Fix (* 0.6725 fx)) fsld)      
       ;(mapcar '(lambda(x) (alert (itoa x)) ) (list fx0 fy0 (Fix fx) (Fix (* 0.6725 fx))))
       (End_Image)
)

;;取得幻灯片库中幻灯片名称
(defun xzj-slb-allname(slb / flst i len newstr sstr str tmpstr)
  (if (findfile slb)
    (progn    
    (setq str (binaryToText slb))
    (setq sstr (substr str 33))
    (setq newstr (substr sstr 1 (vl-string-search "AutoCAD Slide" sstr)))    
    (setq i 1 len (strlen newstr) flst nil )
    (while (< i len)
    (setq tmpstr (substr newstr i 31))
    ;;;每条数据长度为36 后面说明为4  
    ;;;(= (vl-list->string  '(0)) "\000") 等价.为什么下面不等价.>?
    ;;;(setq tmpstr (vl-string-right-trim  "\000"    tmpstr))
    (setq tmpstr (vl-string-right-trim (vl-list->string '(0)) tmpstr))
    (if (= "" tmpstr) 
       nil   
      (setq flst (append flst (list tmpstr)))
    )
    (setq i (+ 36 i))
    )    
    flst      
    )
  )
)
;Subject:binary conversion to text by lisp 二进制文件转文本
;Writen:nonsmall(不死猫)
;Date:2009 10 18
;All Rights Reserved 版权所有 nonsmall(不死猫)
;Contact: QQ:43797405 Email:nonsmall@163.com
(defun binaryToText(FileName / ADODB.Stream)
	(Setq ADODB.Stream (Vlax-Get-Or-Create-Object "ADODB.Stream" ))
	(Vlax-Put-Property ADODB.Stream 'Type 1 )
	(Vlax-Invoke ADODB.Stream 'Open )
	(Vlax-Invoke-Method ADODB.Stream 'LoadFromFile FileName )
	(Vlax-Put-Property ADODB.Stream 'Position 0 )
	(Vlax-Invoke-Method ADODB.Stream 'Write (car (list (Vlax-Invoke-Method ADODB.Stream 'Read (Vlax-Get ADODB.Stream 'Size )) (Vlax-Put-Property ADODB.Stream 'Position 0 ))))
	(Vlax-Put-Property ADODB.Stream 'Position 0 )
	(Vlax-Put-Property ADODB.Stream 'Type 2 )	
	(Vlax-Put-Property ADODB.Stream 'CharSet "us-ascii" )
	(Vlax-Invoke ADODB.Stream 'ReadText)	
)
(defun xzj-dclst-show(key lst)
  (start_list key)
  (mapcar 'add_list lst) 
  (end_list) 
)
(defun xzj-dcl-disablectrls (keylist / key)
  (foreach key keylist (mode_tile key 1))
)
(defun xzj-dcl-enablectrls (keylist / key)
  (foreach key keylist (mode_tile key 0))
)

  
(setq dclname 
(cond  
((setq tempname (vl-filename-mktemp "gps-dcl-tmp.dcl") filen (open tempname "w")) 
(foreach stream  
'(  
"ibt:button {\n"
"    width = 4 ;\n"
"    aspect_ratio = 1 ;\n"
"    fixed_width = true ;\n"
"    fixed_high = true ;\n"
"    color = 253 ;\n"
"}\n"
"ispacer:spacer {    width = 7 ;}\n"
"timage:dialog {\n"
"    label = \"幻灯片查看工具\" ;\n"
"    :row {\n"
"        :column {\n"
"            :button {\n"
"                label = \"浏览...\" ;\n"
"                key = \"path\" ;\n"
"            }\n"
"            :text {\n"
"                key = \"txt\" ;\n"
"                label = \"文件路径\" ;\n"
"            }\n"
"            :list_box {\n"
"                fixed_height = true ;\n"
"                fixed_width = true ;\n"
"                height = 15 ;\n"
"                key = \"sldslb\" ;\n"
"                width = 25 ;\n"
"            }\n"
"            :list_box {\n"
"                key = \"slb\" ;\n"
"            }\n"
"        }\n"
"        :column {\n"
"            :image {\n"
"                aspect_ratio = 0.7 ;\n"
"                color = black ;\n"
"                fixed_width = true ;\n"
"                key = \"image\" ;\n"
"                width = 50 ;\n"
"            }\n"
"            :column {\n"
"                :row {\n"
"                    ispacer;\n"
"                    :ibt {\n"
"                        label = \" ━ \" ;\n"
"                        key = \"-\" ;\n"
"                        color = 143 ;\n"
"                    }\n"
"                    :ibt {\n"
"                        label = \" ↑ \" ;\n"
"                        key = \"up\" ;\n"
"                    }\n"
"                    :ibt {\n"
"                        label = \" ╋ \" ;\n"
"                        key = \"+\" ;\n"
"                        color = 143 ;\n"
"                    }\n"
"                    ispacer;\n"
"                }\n"
"                :row {\n"
"                    ispacer;\n"
"                    :ibt {\n"
"                        label = \" ← \" ;\n"
"                        key = \"left\" ;\n"
"                    }\n"
"                    :ibt {\n"
"                        label = \" ↓ \" ;\n"
"                        key = \"down\" ;\n"
"                    }\n"
"                    :ibt {\n"
"                        label = \" → \" ;\n"
"                        key = \"right\" ;\n"
"                    }\n"
"                    ispacer;\n"
"                }\n"
"                cancel_button;\n"
"            }\n"
"        }\n"
"    }\n"
"}\n"
)
(princ stream filen)
) 
(close filen)
tempname
)))
(setq dclid (load_dialog dclname))  
(setq loop T) 
(while loop
(if (not (new_dialog "timage" dclid)) (progn (alert "dcl对话框加载失败.")(exit)))
(setq dimx (1- (dimx_tile "image")) dimy (1- (dimy_tile "image")) xicon dimx x0 0 y0 0) 
;;;浏览
(action_tile "path" (vl-prin1-to-string 
  '(progn
    (if (setq sldpath$ (xzj-browsedir "选择目录"))
      (progn
	(set_tile "txt" sldpath$)
	(setq sldlst$ (append (vl-directory-files sldpath$ "*.sld")  (vl-directory-files sldpath$ "*.slb")))
	(xzj-dclst-show "sldslb" sldlst$) 
	(xzj-dclst-show "slb" nil)     
      )
    )
  )
))
;;;sldslb
(action_tile "sldslb" (vl-prin1-to-string 
  '(progn
    ;;;cls...
    (start_image "image")
    (fill_image 0 0 dimx dimy -2)
    (end_image)
    (setq sldname$ (nth (atoi $value) sldlst$))
    (if (wcmatch (strcase sldname$) "*.SLD")
	(progn
	  (xzj-dclst-show "slb" nil)
	  (xzj-dcl-disablectrls '("slb"))
	  (setq slbfullname$ (strcat sldpath$ "\\" sldname$))      
	  (start_image "image")
	  (slide_image 0 0 dimx dimy slbfullname$)
	  (end_image)        
	)
      (progn ;幻灯片库
	  (xzj-dcl-enablectrls '("slb" ))
	  (setq slblst$  (xzj-slb-allname (strcat sldpath$ "\\" sldname$)))
	  (xzj-dclst-show "slb" slblst$)
	  (set_tile "slb" "0")
	  (setq dimx (1- (dimx_tile "image")) dimy (1- (dimy_tile "image")))
	  (start_image "image")
	  (fill_image 0 0 dimx dimy -2)
	  (end_image)
	  (setq slbname (nth (atoi $value) slblst$))
	  (setq slbfullname$ (strcat sldpath$ "\\" sldname$ "(" slbname ")"  ))
	  (start_image "image")
	  (slide_image 0 0 dimx dimy slbfullname$)
	  (end_image) 
      )  
    )    
  )
))
(action_tile "slb" (vl-prin1-to-string 
  '(progn    
    (setq dimx (1- (dimx_tile "image")) dimy (1- (dimy_tile "image")))
    (start_image "image")
    (fill_image 0 0 dimx dimy -2)
    (end_image)
    (setq slbname (nth (atoi $value) slblst$))
    (setq slbfullname$ (strcat sldpath$ "\\" sldname$ "(" slbname ")"  ))
    (start_image "image")
    (slide_image 0 0 dimx dimy slbfullname$)
    (end_image) 
  )
))
(Action_Tile "-"     "(xzj-sld-Zoom (* 0.8 xicon))")
(Action_Tile "+"     "(xzj-sld-Zoom (* 1.2 xicon))")
(Action_Tile "left"  "(xzj-sld-Pan -20 0)")
(Action_Tile "right" "(xzj-sld-Pan  20 0)")
(Action_Tile "up"    "(xzj-sld-Pan   0 -20)")
(Action_Tile "down"  "(xzj-sld-Pan   0  20)")
(action_tile "accept" "(done_dialog 1)")
(action_tile "cancel" "(done_dialog 0)")
(setq dd (start_dialog))
(cond((= 0 dd) (setq Loop nil))
))(unload_dialog dclid)
(vl-file-delete dclname) 
)


