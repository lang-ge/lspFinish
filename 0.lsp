;;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;批量打印开始


;;;--------------------------------------------------------------------------
;;;   0.LSP
;;;    版权(C) 2020-2025  Lang_ge
;;;
;;; 模型空间的批量打印程序 第一版
;;; Lang_ge, 2020年8月3日
;;; 最新更新：2020年8月3日12时39分

;;;   本程序免费可供进行任何用途需求的拷贝、修改及发行, 但请遵循下述原则:
;;;
;;;   1)  上列的版权通告必须出现在每一份拷贝里.
;;;   2)  相关的说明文档也必须载有版权通告及本项许可通告.

;;;   本程序仅提供作为应用上的参考, 而未声明或隐含任何保证; 对于任何特殊
;;;   用途之适应性, 以及商业销售所隐含作出的保证, 在此一概予以否认.

;;; 主要功能从这里开始
(defun c:QA (/ *Dwg* *Dwn* *error* *Pdf* ax:2DPoint CopyLayout Dcl_b Dcl_c Dcl_d 
             Dcl_M Dcl_r defultprint dlg_recf doBatchLayout doPlotLayoutsInTabOrder 
             expandbounding G_Folder G_tag GetBlock GetBlockList Getblpe GetLayer 
             GetLayerList HiLiShow Is_dcl islandscape Isvt layout-tab-list Mwp N003 
             N003A Nava one OrderFrames Play PlotLayoutsInTabOrder Preng Priew Selena 
             Spblpe Specla two acaddoc ocho kk prna colorall prt_id s word sel sen 
             N_list rn_b
            )
 (vl-load-com)
 
;;; 通用函数:从注册表获取默认打印机开始
(defun defultprint (/ device)
 (vl-load-com)
 (substr (setq device (vl-registry-read "HKEY_CURRENT_USER\\Software\\Microsoft\\Windows NT\\CurrentVersion\\Windows" "Device"))
         1 (vl-string-search "," device)
 )
)
;;; 通用函数:从注册表获取默认打印机结束

;;;函数：播放指定的音频文件
(defun Play (file /)
 ;;;(Vlax-Put-Property (vlax-Create-Object "WMPlayer.OCX") 'URL file)
 (findfile file)
)

;;; 出错定义
(defun *error* (ERROR)
 (princ "\n>>>ZYD设计工作室:")
 (princ "打印功能被取消!!")
 (setvar "osmode" 703)
)

;;; 选择文件夹对话框函数
(defun G_Folder (/ get) 
  (if (setq get (Vlax-Invoke-Method Shel 'BrowseForFolder 0 "选择保存目录" 16 0)) 
    (setq get (vlax-get-property (vlax-get-property get 'self) 'path))
  )
  (if (vl-file-directory-p get) get)
  get
)

;; 选择图块与图层
(defun Selena (/ ename ena) 
  (setq ret nil)
  (setq ename (car (entsel "\n指定图框所在的样板物体:")))
  (setq ena (vlax-ename->vla-object ename))
  (if (null ename) 
    (exit)
  )
  (if (= (cdr (assoc 0 (entget ename))) "INSERT") 
    (progn 
      (setq rsh T)
      (vla-get-effectivename ena)
    )
    (vla-get-Layer ena)
  )
)

;;;--------------------------------------------------------
;; 插入智能变量标记
(defun Isvt (/ dcl_id sek) 
  (setq dcl_id (load_dialog "N003.dcl"))
  (if (not (new_dialog "qa_k" dcl_id "")) 
    (exit)
  )
  (set_tile "ssat" "1")
  (action_tile "accept" "(Is_dcl)(DONE_DIALOG)")
  (start_dialog)
  (unload_dialog dcl_id)
  (if (= sek 1) 
    (alert "常用智能标记")
  )
  (if (= sek 2) 
    (alert "AutoCAD系统变量")
  )
  (if (= sek 3) 
    (alert "从指定数字自动增量编号")
  )
  (if (= sek 4) 
    (progn 
      (setq rep (Specla))
      (if (not baa) (setq baa ""))
      (if rep (setq baa (strcat baa rep)))
    )
  )
  (if (= sek 5) 
    (progn 
      (setq rep (Getblpe))
      (if (not baa) (setq baa ""))
      (if rep (setq baa (strcat baa rep)))
    )
  )
  (if (= sek 6) 
    (setq baa "")
  )
  (princ)
)

;; 智能变量DCL子程序：
(DEFUN Is_dcl () 
  (cond 
    ((= (GET_TILE "cust") "1") (setq sek 1))
    ((= (GET_TILE "acsv") "1") (setq sek 2))
    ((= (GET_TILE "ansn") "1") (setq sek 3))
    ((= (GET_TILE "sstl") "1") (setq sek 4))
    ((= (GET_TILE "ssat") "1") (setq sek 5))
    ((= (GET_TILE "aawn") "1") (setq sek 6))
  )
)

;;;--------------------------------------------------------
;; 有名图块列表
(defun GetBlockList (/ blklst blk) 
  (setq blklst nil)
  (if (tblnext "BLOCK" T) 
    (progn 
      (setq blklst (cons (cdr (assoc 2 (tblnext "BLOCK" T))) blklst))
      (while (setq blk (tblNext "BLOCK")) 
        (setq blklst (cons (cdr (assoc 2 blk)) blklst))
      )
    )
  )
  (vl-remove-if '(lambda (x) (= "*" (substr x 1 1))) blklst)
)

;; 获取有名图块列表
(defun GetBlock (/ res b_list dcl_id x c_list) 
  (setq dcl_id (load_dialog "N003.dcl"))
  (if (not (new_dialog "qa_b" dcl_id "")) 
    (exit)
  )
  (setq b_list (GetBlockList))
  (lst-dcl "bl_list" b_list 3 0)
  (action_tile "bl_list" "(setq res $value)")
  (start_dialog)
  (unload_dialog dcl_id)
  (if (not c_list) (setq c_list ""))
  (foreach x (read (strcat "(" res ")")) 
    (setq c_list (strcat c_list "," (nth x b_list)))
  )
  (vl-string-left-trim "," c_list)
)

;; 图层列表
(defun GetLayerList (/ laylst lay) 
  (setq laylst nil)
  (if (tblnext "LAYER" T) 
    (progn (setq laylst (cons (cdr (assoc 2 (tblnext "LAYER" T))) laylst)) 
           (while (setq lay (tblNext "LAYER")) 
             (setq laylst (cons (cdr (assoc 2 lay)) laylst))
           )
    )
  )
  (vl-sort laylst '<)
)

;; 获取图层列表
(defun GetLayer (/ reh l_list dcl_id x c_list) 
  (setq dcl_id (load_dialog "N003.dcl"))
  (if (not (new_dialog "qa_l" dcl_id "")) 
    (exit)
  )
  (setq l_list (GetLayerList))
  (lst-dcl "la_list" l_list 3 0)
  (action_tile "la_list" "(setq reh $value)")
  (start_dialog)
  (unload_dialog dcl_id)
  (if (not c_list) (setq c_list ""))
  (foreach x (read (strcat "(" reh ")")) 
    (setq c_list (strcat c_list "," (nth x l_list)))
  )
  (vl-string-left-trim "," c_list)
)

;; 图块中的属性
(defun Getblpe (/ tmp_s t_a3 t_ag TMP_l TMP_M dcl_id repn) 
  (setq tmp_s (car (entsel "\n请从图中拾取样板图块与属性:")))
  (setq t_a3 (vla-get-effectivename (vlax-ename->vla-object tmp_s)))
  (setq TMP_l (MJ:GetAttributes (cdar (entget tmp_s))))
  (setq TMP_M (mapcar '(lambda (x) (strcat (car x) ": " (cadr x))) TMP_l))
  (setq dcl_id (load_dialog "N003.dcl"))
  (if (not (new_dialog "qa_v" dcl_id "")) 
    (exit)
  )
  (lst-dcl "va_list" TMP_M 3 0)
  (action_tile "va_list" "(setq repn $value)")
  (action_tile "accept" "(DONE_DIALOG)")
  (start_dialog)
  (unload_dialog dcl_id)
  (setq t_ag (car (nth (atoi repn) TMP_l)))
  (Spblpe)
  (strcat "<Att:" t_a3 ":" t_ag ">")
)

;; 指定图块特定的属性
(defun Spblpe (/ dcl_id) 
  (setq dcl_id (load_dialog "N003.dcl"))
  (if (not (new_dialog "qa_s" dcl_id "")) 
    (exit)
  )
  (set_tile "bna" t_a3)
  (set_tile "ata" t_ag)
  (action_tile "accept" "(setq t_a3 (GET_TILE \"bna\") t_ag (GET_TILE \"ata\"))(DONE_DIALOG)")
  (start_dialog)
  (unload_dialog dcl_id)
)

;; 获取图块特定标签的属性值
(defun G_tag ( blk tag )
    (setq tag (strcase tag))
    (vl-some '(lambda (x) (if (= tag (strcase (vla-get-tagstring x))) (vla-get-textstring x)))
        (vlax-invoke (vlax-ename->vla-object blk) 'getattributes)
    )
)

;; 图层上的特定文字 [样式名:vla-get-StyleName (cons 7 tyNa)]
(defun Specla (/ lent ena lame ltxt p0 ss m enn dcl_id) 
  (setq lent (car (entsel "请在最右侧选择一个样板对象以确定特定的图层名:")))
  (setq ena (vlax-ename->vla-object lent))
  (if (null lent) (exit))
  (setq lame (vla-get-Layer ena)) ;图层名
  (setq ltxt (vla-get-textstring ena)) ;图层文字
  (setvar "osmode" 703)
  (setq p0 (getpoint "请框选一个典型较大的图框范围，应包含刚才选择的样板对象:"))
  (setq ss (ssget "_w" p0 (getcorner p0 "对角点:") (list '(-4 . "<OR") '(0 . "TEXT") '(0 . "MTEXT") '(-4 . "OR>") (cons 8 lame))))
  (setq m   0
        wcm nil
  )
  (while (not wcm) 
    (setq m (1+ m))
    (setq enn (cdr (assoc 1 (entget (ssname ss m)))))
    (if (wcmatch ltxt enn) 
      (setq wcm T)
    )
  )
  (setq dcl_id (load_dialog "N003.dcl"))
  (if (not (new_dialog "qa_t" dcl_id "")) 
    (exit)
  )
  (set_tile "lan" lame)
  (set_tile "lat" (rtos m 2 0))
  (action_tile "accept" 
               "(setq lame (GET_TILE \"lan\") m (GET_TILE \"lat\"))(DONE_DIALOG)"
  )
  (start_dialog)
  (unload_dialog dcl_id)
  (strcat "<Txt:" lame ":" m ">")
)

;; 多行文字处理
(defun Mwp (ment /) 
  (if (= (cdr (assoc 0 (entget ment))) "MTEXT") 
    (progn 
      (command "_explode" ment "")
      (vl-string-trim " " (cdr (assoc 1 (entget (entlast)))))
    )
    (cdr (assoc 1 (entget ment)))
  )
)

;; DCL对话框选项信息
(DEFUN Dcl_M (/) 
  (cond 
    ((= (GET_TILE "PlotRa") "1") (progn (princ "\n☆开始批量打印") (setq sen 1)))
    ((= (GET_TILE "LautRa") "1") (progn (princ "\n☆开始批量生成布局") (setq sen 2)))
    ((= (GET_TILE "PPdfRa") "1") (progn (princ "\n☆开始批量打印PDF文档") (setq sen 3)))
    ((= (GET_TILE "PDwgRa") "1") (progn (princ "\n☆开始批量切分Dwg~文档") (setq sen 4)))
    ((= (GET_TILE "PLautRa") "1") (progn (princ "\n☆开始批量打印已有布局") (setq sen 5)))
  )
)

;; 输出选项框状态值
(defun Dcl_b () 
  (setq Plo (GET_TILE "PlotRa")
        PLa (GET_TILE "PLautRa")
        Lau (GET_TILE "LautRa")
        PPd (GET_TILE "PPdfRa")
        Pdw (GET_TILE "PDwgRa")
        ban (GET_TILE "banb")
        odn (GET_TILE "Odn")
        bdn (GET_TILE "Bdn")
        aun (GET_TILE "Aun")
        baa (GET_TILE "bana")
        res (GET_TILE "tub")
        reh (GET_TILE "tut")
        ler (GET_TILE "leri")
        tob (GET_TILE "tobo")
        reo (GET_TILE "reor")
        wri (GET_TILE "writb")
        cle (GET_TILE "clesa")
  )
)
;; 图框选项切换
(defun Dcl_c () 
  (cond 
    (sel
     (if (= sel 1) 
       (progn 
         (setq s (ssget '((0 . "INSERT") (2 . "*a[345]*"))))
         (one s)
       )
     )
     (if (= sel 2) 
       (progn 
         (setq word (strcase (getstring "\n请输入匹配字符:")))
         (setq s (ssget (list (cons 0 "INSERT") (cons 2 (strcat "*" word "*")))))
         (one s)
       )
     )
     (if (= sel 3) 
       (two '((0 . "line,arc,lwpolyline,circle,3dsolid,DIMENSION")))
     )
    )
    ((and res (/= res "*图框,*TITLE"))
     (setq s (ssget (list (cons 0 "INSERT") (cons 2 res))))
     (one s)
    )
    ((and reh (/= reh "*图框,*TITLE"))
     (two (list (cons 0 "line") (cons 8 reh)))
    )
  )
)

;; 打印初始化
(defun Dcl_d (/ x m) 
  (setq m 0)
  (if bdlist 
    (foreach x (OrderFrames bdlist) 
      (setq p1 (car x) p2 (last x))
      (princ (strcat "\n正在进行: 第" (itoa (setq m (1+ m))) "/" (itoa n) "页"))
      (Preng)
    )
    (Preng)
  )
  (if n (princ (strcat "\nOK!此次共完成:" (itoa n) " 份图纸")))
  (if path (vlax-invoke-method Shel 'Open path))
)

;; 打印输出引擎
(defun Preng (/ dwg)
  (cond 
    ((= sen 1)
     (if (> a b) 
       (command "-plot" "yes" "" prna "A4" "m" "l" "" "w" p1 p2 "f" "c" "yes" "acad.ctb" "yes" "" "no" "yes" "yes")
       (command "-plot" "yes" "" prna "A4" "m" "p" "" "w" p1 p2 "f" "c" "yes" "acad.ctb" "yes" "" "no" "yes" "yes")
     )
    )
    ((= sen 2) (doBatchLayout (list x) "ScaleToFit" "1" T T))
    ((= sen 3) (*Pdf*))
    ((and (= sen 4) (= cle "1")) (c:ea) (command "_.QSAVE"))
    ((= sen 4) (*Dwg*))
    ((= sen 5) (doPlotLayoutsInTabOrder))
  )
)

;; 重文档名处理!
(DEFUN *Dwn* (path fold dwg sufix / x) 
  (if (not N_list) (setq N_list '()))
  (foreach x N_list 
    (if (wcmatch x (strcat dwg "*")) 
      (setq dwg (strcat dwg (rtos (1- (atof (vl-string-subst "" dwg x))) 2 0)))
    )
  )
  (setq N_list (cons dwg N_list))
  (vl-mkdir (strcat path fold)) ;vl-file-directory-p(判断文件夹是否存在)
  (strcat path fold "\\" dwg sufix)
)

;; 文件名解析!
(DEFUN Nava (rn_b / dwg dwlist x symb syml zd) 
  (if (and rn_b (/= rn_b "")) 
    (progn 
      (setq dwg "")
      (setq dwlist (read (vl-string-translate "<:>" "( )" (strcat "(" rn_b ")"))))
      (foreach x dwlist 
        (setq symb (vl-symbol-name (car x)))
        (setq syml (vl-princ-to-string (cadr x)))
        (cond 
          ((= symb "ATT")
           (if (setq zd (ssget "_w" p1 p2 (list (cons 2 syml)))) 
             (setq dwg (strcat dwg (G_tag (ssname zd 0) (vl-princ-to-string (last x)))))
             (alert "数据错误,请检查名称变量!")
           )
          )
          ((= symb "TXT")
           (setq zd (ssget "_w" 
                           p1
                           p2
                           (list '(-4 . "<OR") 
                                 '(0 . "TEXT")
                                 '(0 . "MTEXT")
                                 '(-4 . "OR>")
                                 (cons 8 syml)
                           )
                    )
           )
           (setq dwg (strcat dwg (Mwp (ssname zd (last x)))))
          )
        )
      )
    )
    (setq dwg (vl-filename-base (getvar "DWGNAME")))
  )
)

;; 打印PDF文档!
(DEFUN *Pdf* (/ dwg dwna) 
  (setvar "FILEDIA" 0)
  (setq dwg (Nava rn_b))
  (setq dwna (*Dwn* fold "\\pdf" dwg ""))
  (setq path (vl-filename-directory dwna))
  ; (alert dwna) ;;测试用
  (command "-EXPORT" "P" "w" p1 p2 "y" "" "" "l" "f" "y" "monochrome.ctb" "y" dwna) 
  (princ)
)

;; 切分DWG文档!
(DEFUN *Dwg* (/ GRP dwg dwna) 
  (setvar "FILEDIA" 0)
  (setq GRP (ssget "_w" p1 p2))
  (command "_copy" GRP "" "0,0,0" "0,0,0")
  (setq dwg (Nava rn_b))
  (setq dwna (*Dwn* fold "\\dwg" dwg ".dwg"))
  (setq path (vl-filename-directory dwna))
  (command "_EXPORT" dwna "" "0,0,0" GRP "")
  (princ)
)

;; 批量生成布局
  (defun doBatchLayout (bdlist plotscale ltscale autoRotate UpsideDown / 
                               landscapeList layoutprefix portraitList NoRotationList 
                               RotationList bd bd2 layouts layout ll ur MarginLL MarginUR 
                               prdisplay crtvp showpg pwidth pHeight PaperWidth item clayout catch
                       ) 
  (setq prdisplay (vla-get-display 
                    (vla-get-preferences (vlax-get-acad-object))
                  )
  )
  (setq layouts (vla-get-layouts acaddoc))
  (setq bdlist (mapcar (function (lambda (v) (expandbounding v 1e6))) bdlist))
  
   ;; 取得布局名
  (setvar "FILEDIA" 0)
  (setq layoutprefix (vl-string-left-trim "\\" (*Dwn* "" "" (Nava rn_b) "")))

  ;;增量，用于防止打印时打不出边线
  (setq portraitList (vl-remove-if 'islandscape bdlist))
  (setq landscapelist (vl-remove-if-not 'islandscape bdlist))
  (setq clayout (vla-get-activelayout acaddoc))
  (vla-getpapersize clayout 'pWidth 'pHeight)

  ;;页面设置错误处理
  (setq catch (vl-catch-all-apply 'vla-put-paperunits (list clayout acMillimeters)))
  (if (vl-catch-all-error-p catch) 
    (progn 
      (vla-put-configname clayout "无")
      (vla-put-paperunits clayout acMillimeters)
    )
  )
  ;; 取得当前纸张的长边长度
  (if (< pwidth pheight) 
    (setq PaperWidth pHeight)
    (setq PaperWidth pWidth)
  )
  ;; 决定旋转什么
  (setq NoRotationList (if (> pwidth pHeight) landscapeList portraitList))
  (setq RotationList (if (> pwidth pHeight) portraitList landscapeList))

  (if (null AutoRotate) 
    (progn 
      (setq NoRotationList bdlist)
      (setq RotationList nil)
    )
  )

  (setq crtvp  (vla-get-LayoutCreateViewport prdisplay)
        showpg (vla-get-LayoutShowPlotSetup prdisplay)
  )
  (vla-put-layoutcreateviewport prdisplay :vlax-false)
  (vla-put-layoutshowplotsetup prdisplay :vlax-false)

  ;; 创建0度模板
  (if NoRotationList 
    (progn 
      (vla-put-layoutcreateviewport prdisplay :vlax-false)
      (vla-put-layoutshowplotsetup prdisplay :vlax-false)
      (vl-cmdf "_.Layout" "_New" "$temp_No_Rotation")
      (vl-cmdf "_.Layout" "_Set" "$temp_No_Rotation")
      (vla-put-layoutcreateviewport prdisplay crtvp)
      (vla-put-layoutshowplotsetup prdisplay showpg)
      ;(alert ltscale)
      (if (= "1" ltscale) 
        (setvar "psltscale" 0)
      )
      (setq layout (vla-item layouts "$temp_No_Rotation"))
      (vla-put-ConfigName 
        layout
        (vla-get-ConfigName (vla-item layouts "Model"))
      )
      (vla-put-CanonicalMediaName 
        layout
        (vla-get-CanonicalMediaName (vla-item layouts "Model"))
      )
      (vla-put-stylesheet 
        layout
        (vla-get-stylesheet (vla-item layouts "Model"))
      )
      (vla-put-showplotstyles layout :vlax-true)
      (vla-getpapermargins layout 'MarginLL 'MarginUR)
      (setq marginll (vlax-safearray->list marginll))
      (setq marginur (vlax-safearray->list marginur))
      (setq ll (mapcar '- MarginLL))
      (setq ur (mapcar '+ ll (list pwidth pHeight)))
      (cond 
        ((= plotscale "ScaleToFit")
         (vl-cmdf "_.MView" 
                  '(0 0)
                  (mapcar '- 
                          (list pwidth pHeight)
                          (list (car marginll) (cadr marginll))
                          (list (car marginur) (cadr marginur))
                  )
         )
        )
        ((= plotscale "Auto") (vl-cmdf "_.Mview" ll ur))
        ('T (vl-cmdf "_.Mview" ll ur))
      )
      (vla-put-paperunits layout acMilliMeters)
      (vla-put-standardscale layout acVpCustomScale)
      (vla-setcustomscale layout 1 1)
      (vla-put-plotrotation layout (if upsidedown ac180degrees ac0degrees))
      ;(vla-regen acaddoc acAllViewports)
      (vl-cmdf "_.zoom" "_all")
      (foreach bd NoRotationList 
        (setq bd2 (expandbounding bd 1e2))
        (copylayout "$temp_No_Rotation_Ctab")
        (vl-cmdf "_.MSPACE")
        (vl-cmdf "_.zoom" "_window" "_non" (car bd2) "_non" (cadr bd2))
        (vl-cmdf "_.Zoom" "_Window" "_non" (car bd) "_non" (cadr bd))
        (if (numberp plotscale) 
          (vl-cmdf "_.zoom" 
                   "_Scale"
                   (strcat (rtos (/ 1.0 plotscale) 2 100) "xp")
          )
        )
        ;; 恢复到图纸空间
        (vl-cmdf "_.pspace")
        ;;; (setq layoutprefix (FormatName (getvalue 'NamePrefix) bd nil))
        ;;; (if (member layoutprefix (GetLayoutList)) 
        ;;;   (setq layoutprefix (GetNewAutoNumName layoutprefix 2 (GetLayoutList)))
        ;;; )
        (vl-cmdf "_.layout" "_rename" "$temp_No_Rotation_Ctab" layoutprefix)
      )
      (vl-cmdf "_.layout" "_delete" "$temp_No_Rotation")
    )
  )
  ;; 创建90度模板
  (if RotationList 
    (progn
      (vla-put-layoutcreateviewport prdisplay :vlax-false)
      (vla-put-layoutshowplotsetup prdisplay :vlax-false)
      (vl-cmdf "_.Layout" "_New" "$temp_90_Rotation")
      (vl-cmdf "_.Layout" "_Set" "$temp_90_Rotation")
      (vla-put-layoutcreateviewport prdisplay crtvp)
      (vla-put-layoutshowplotsetup prdisplay showpg)
      (if (= "1" ltscale) 
        (setvar "psltscale" 0)
      )
      (setq layout (vla-item layouts "$temp_90_Rotation"))
      (vla-put-ConfigName 
        layout
        (vla-get-ConfigName (vla-item layouts "Model"))
      )
      (vla-put-CanonicalMediaName 
        layout
        (vla-get-CanonicalMediaName (vla-item layouts "Model"))
      )
      (vla-put-stylesheet 
        layout
        (vla-get-stylesheet (vla-item layouts "Model"))
      )
      (vla-put-showplotstyles layout :vlax-true)
      (vla-put-paperunits layout acMilliMeters)
      (vla-put-standardscale layout acVp1_1)
      (vla-getpapermargins layout 'MarginLL 'MarginUR)
      (setq marginll (vlax-safearray->list marginll))
      (setq marginur (vlax-safearray->list marginur))
      (setq ll (mapcar '- (list (cadr marginUR) (car marginLL))))
      (setq ur (mapcar '+ ll (list pHeight pwidth)))
      (cond 
        ((= plotscale "ScaleToFit")
         (vl-cmdf "_.MView" 
                  '(0 0)
                  (mapcar '- 
                          (list pHeight pwidth)
                          (list (cadr marginur) (car marginur))
                          (list (cadr marginll) (car marginll))
                  )
         )
        )
        ((= plotscale "Auto") (vl-cmdf "_.Mview" ll ur))
        ('T (vl-cmdf "_.Mview" ll ur))
      )
      (vla-put-paperunits layout acMilliMeters)
      (vla-put-standardscale layout acVpCustomScale)
      (vla-setcustomscale layout 1 1)
      (vla-put-plotrotation layout (if upsidedown ac270degrees ac90degrees))
      ;(vla-regen acaddoc acAllViewports)
      (vl-cmdf "_.zoom" "_all")
      (foreach bd RotationList 
        (setq bd2 (expandbounding bd 1e2))
        (copylayout "$temp_90_Rotation_Ctab")
        (vl-cmdf "_.MSPACE")
        (vl-cmdf "_.zoom" "_window" "_non" (car bd2) "_non" (cadr bd2))
        (vl-cmdf "_.Zoom" "_Window" "_non" (car bd) "_non" (cadr bd))
        (if (numberp plotscale) 
          (vl-cmdf "_.zoom" 
                   "_Scale"
                   (strcat (rtos (/ 1.0 plotscale) 2 100) "xp")
          )
        )
        ;; 恢复到图纸空间
        (vl-cmdf "_.pspace")
        ;;; (setq layoutprefix (getstring "\n请输入名称前缀:"))
        ;;; (if (member layoutprefix (GetLayoutList)) 
        ;;;   (setq layoutprefix (GetNewAutoNumName layoutprefix 2 (GetLayoutList)))
        ;;; )
        (vl-cmdf "_.layout" "_rename" "$temp_90_Rotation_Ctab" layoutprefix)
      )
      (vl-cmdf "_.layout" "_delete" "$temp_90_Rotation")
    )
  )
  ;; 回到模型
  (setvar "ctab" "Model")
  (princ "\n布局成功生成。")
)

;; 批量打印布局
  (defun doPlotLayoutsInTabOrder (/ LayoutNameList plot-to-file-p key indexlist sellst)
    (setq LayoutNameList
	   (vl-remove-if
	     (function (lambda (name)
			 (= (strcase name) "MODEL")
		       )
	     )
	     (layout-tab-list acaddoc)
	   )
    )
    (setq layoutNameList
	   (vl-sort
	     layoutNameList
	     '(lambda (a b)
          (< (vla-get-taborder (vla-item (vla-get-layouts acaddoc) a))
             (vla-get-taborder (vla-item (vla-get-layouts acaddoc) b))
          )
	      )
	   )
    )
    (initget "All Select _All Select")
    (setq key
	   (getkword
	     "\n打印布局[全部(A)/选择(S)]/<全部>:"
	   )
    )
    (if	(eq key "Select")
      (progn
        (if (setq sellst (Odcl:MultList "请选择要打印的布局" "布局列表（可多选）" LayoutNameList))
          (progn
            (setq LayoutNameList sellst)
            (setq indexlist T)
          )
          (setq indexlist nil)
        )
        (if (null indexlist)
          (exit)
        )
      )
    )
    (initget "Yes No _Yes No")
    (setq plot-to-file-p
	   (getkword "打印成PLT文件? [是(Y)/否(N)] <否>: ")
    )
    (setq Plot-to-file-p (= plot-to-file-p "Yes"))
    (PlotLayoutsInTabOrder LayoutNameList plot-to-file-p)
    (setq n (length LayoutNameList))
    (princ)
  )

;;; 打印布局相关函数
;;;--------------------------------------------------------
;; 批量打印布局引擎
(defun PlotLayoutsInTabOrder
	 (LayoutNameList Plot-To-File-P / LayoutName ctab)
    (setq ctab (getvar "ctab"))
    ;;; (if (= 1 (atoi (getvalue 'PlotStamp)))
    ;;;   (vl-cmdf "-plotstamp" "o" "_non")
    ;;;   (vl-cmdf "-plotstamp" "off" "_non")
    ;;; )
    (foreach LayoutName	LayoutNameList
      (vl-cmdf "_.layout" "_set" layoutname)
      (vl-cmdf "_.pspace")
      (vl-cmdf "_.zoom" "_e")
      (if plot-to-file-p
        (vl-cmdf ".-plot" "No"
           LayoutName
           ""
           ""
           "Yes"
           ""
           "No"
           "Yes"
        )
        (vl-cmdf ".-plot" "No"
           LayoutName
           ""
           ""
           "No"
           "No"
           "Yes"
        )
      )
    )
    (vl-cmdf "_.layout" "_set" ctab)
  )
;; 布局顺序列表
  (defun layout-tab-list (doc / layouts)
    (mapcar 'vla-get-name
      (vl-sort
        (vlax-for layout (vla-get-layouts doc)
          (setq layouts (cons layout layouts))
        )
        '(lambda (a b)
           (< (vla-get-taborder a)
        (vla-get-taborder b)
           )
         )
      )
    )
  )
  
;; 得到自动编号名
;;; (defun GetNewAutoNumName (prefix FixNum NameList / i istr Name)
;;;     (setq i 0)
;;;     (setq istr (itoa i))
;;;     (while (< (strlen istr) FixNum)
;;;         (setq istr (strcat "0" istr))
;;;     )
;;;     (setq Name (strcat prefix istr))
;;;     (while (member Name Namelist)
;;;         (setq i (1+ i))
;;;         (setq istr (itoa i))
;;;         (while (< (strlen istr) FixNum)
;;;             (setq istr (strcat "0" istr))
;;;         )
;;;         (setq Name (strcat prefix istr))
;;;     )
;;;     Name
;;; )

;; 布局列表
(defun GetLayoutList ( / layouts i lst lay)
    (setq layouts (vla-get-layouts acaddoc) i -1)
    (repeat (vla-get-count layouts)
        (setq lst (cons (setq i (1+ i)) lst))
    )
    (vlax-for lay layouts
        (setq lst (subst (vla-get-name lay) (vla-get-taborder lay) lst))
    )
    (reverse lst)
)

;; 给图框设置增量
(defun expandbounding (bd fuzz / pt1 pt2 offset w h)
    (setq w (- (caadr bd) (caar bd)))
    (setq h (- (cadadr bd) (cadar bd)))
    (setq offset (/ w fuzz))    ;增量，用于防止打印时打不出边线
    (setq pt1 (mapcar '- (car bd) (list offset offset)))
    (setq pt2 (mapcar '+ (cadr bd) (list offset offset)))
    (list pt1 pt2)
)

;; 判断图框是否横向
(defun islandscape (bounding / x1 y1 x2 y2)
    (setq x1 (caar bounding)
        y1 (cadar bounding)
        x2 (caadr bounding)
        y2 (cadadr bounding)
    )
    (if	(< (abs (- x1 x2)) (abs (- y1 y2)))
        nil
        'T
    )
)

;; 复制布局
(defun CopyLayout (nLayoutName / clayoutName tmpName) 
  (setq clayoutName (getvar "ctab"))
  (setq tmpName (strcat "$_QF_TMP_LAYOUT_NAME_OF_" clayoutName))
  (vl-cmdf "_.layout" "_copy" clayoutName tmpName)
  (vl-cmdf "_.layout" "_rename" clayoutName NLayoutName)
  (vl-cmdf "_.layout" "_rename" tmpName clayoutName)
)
;;;--------------------------------------------------------

;; 高亮显示引擎
(defun HiLiShow (bdlist / x) 
  (vl-cmdf "_.redraw")
  (foreach x bdlist 
    (grdraw (car x) (list (caar x) (cadadr x)) acRed 1)
    (grdraw (list (caar x) (cadadr x)) (list (caadr x) (cadar x)) acRed 1)
    (grdraw (list (caadr x) (cadar x)) (last x) acRed 1)
    (grdraw (last x) (car x) acRed 1)
  )
  (getstring "\n图中红色交叉框区即为选中的图框<返回>")
  (princ)
)

;; 预览引擎
(defun Priew (bdlist / plot clayout bounding tart key) 
  (setq plot (vla-get-plot acaddoc))
  (setq clayout (vla-get-activelayout acaddoc))
  (setq bdlist (mapcar (function (lambda (v) (expandbounding v 1e6))) bdlist))
  (foreach bounding bdlist 
    ;; 设置打印范围
    (setq tart (getvar "target"))
    (vla-SetWindowToPlot clayout 
                         (ax:2dpoint (mapcar '- (car bounding) tart))
                         (ax:2dpoint (mapcar '- (cadr bounding) tart))
    )

    (if (= :vlax-false (vla-displayplotpreview plot acfullpreview)) 
      (exit)
    )

    (if (/= bounding (last bdlist)) 
      (progn (initget "Yes No") 
             (setq key (getkword "是否继续预览下一张? [Yes/No]<Yes>"))
             (if (= key "No") 
               (exit)
             )
      )
    )
  )
  (princ)
)

 ;;2维点列程
  (defun ax:2DPoint (pt) 
  (vlax-make-variant 
    (vlax-safearray-fill 
      (vlax-make-safearray vlax-vbdouble '(0 . 1))
      (list (car pt) (cadr pt))
    )
  )
)
;; 批量打印对话框初始化
(defun N003 (color / dnw dcl_id x y D5 dx dy wcm dd Plo PLa Lau PPd Pdw ban odn bdn aun res reh ler tob reo wri cle ret rsh rep baa n bdlist a b)
  (if (findfile "0_Set.ini") 
    (setq D5  (fn-lst "0_Set.ini")
          Plo (nth 0 D5)
          PLa (nth 1 D5)
          Lau (nth 2 D5)
          PPd (nth 3 D5)
          Pdw (nth 4 D5)
          ban (nth 5 D5)
          odn (nth 6 D5)
          bdn (nth 7 D5)
          aun (nth 8 D5)
          baa (nth 9 D5)
          res (nth 10 D5)
          reh (nth 11 D5)
          ler (nth 12 D5)
          tob (nth 13 D5)
          reo (nth 14 D5)
          wri (nth 15 D5)
          cle (nth 16 D5)
    )
    (setq Plo "1"
          PLa "0"
          Lau "0"
          PPd "0"
          Pdw "0"
          ban ""
          odn "0"
          bdn "0"
          aun "1"
          baa "<Att:a3:零件图号>"
          res "*图框,*TITLE"
          reh "*图框,*TITLE"
          ler "1"
          tob "0"
          reo "0"
          wri "1"
          cle "0"
    )
  )
  
  (setq dnw    (vl-string-right-trim "\\" (getvar "DWGPREFIX"))
        dcl_id (load_dialog "N003.dcl")
        dd     1
  )
  (while (< dd 10)  ;while循环开始
    (if (not (new_dialog "qa_p" dcl_id "")) 
      (prompt "Unable to open the dialog box!")
    )

    (start_image "Display")
    (slide_image 0 0 (dimx_tile "Display") (dimy_tile "Display") "Q3_D.sld")
    (end_image)
    (start_image "a1")
    (setq x (dimx_tile "a1"))
    (setq y (dimy_tile "a1"))
    (setq dx (/ x 3)
          dy (/ y 1)
    )
    (if wcm 
      (progn (setq sel 3) (dlg_recf (+ dx dx) 1 dx (- dy 1) color))
      (dlg_recf 1 1 dx (- dy 2) color)
    )
    (slide_image 0 -1 (- x 1) (- y 1) "N003.sld")
    (end_image)

    ;; 对话框状态设置
    (set_tile "PlotRa" Plo)
    (set_tile "PLautRa" PLa)
    (set_tile "LautRa" Lau)
    (set_tile "PPdfRa" PPd)
    (set_tile "PDwgRa" Pdw)
    (set_tile "banb" ban)
    (set_tile "Odn" odn)
    (set_tile "Bdn" bdn)
    (set_tile "Aun" aun)
    (set_tile "bana" baa)
    (set_tile "tub" res)
    (set_tile "tut" reh)
    (set_tile "leri" ler)
    (set_tile "tobo" tob)
    (set_tile "reor" reo)
    (set_tile "writb" wri)
    (set_tile "clesa" cle)
    
    (cond 
    ((or (= "1" Plo) (= "1" PLa)) (disableCtrls '("bana" "banb" "navar" "salo" "Browse" "Odn" "Bdn" "Aun" "Rnm" "writb" "clesa")))
    ((or (= "1" PPd) (= "1" Pdw)) (EnableCtrls '("bana" "banb" "navar" "salo" "Browse" "Odn" "Bdn" "Aun" "Rnm")))
    ((or (= "1" Lau)) (EnableCtrls '("bana" "banb" "navar" "Browse" "Odn" "Bdn" "Aun" "Rnm"))(disableCtrls '("salo"))))

    ;; 互锁交通枢纽设置
    (if fold 
      (set_tile "salo" fold)
      (set_tile "salo" dnw)
    )
    (if (and rsh ret) 
      (set_tile "tub" ret)
    )
    (if (and (not rsh) ret) 
      (set_tile "tut" ret)
    )
    (if (/= (GET_TILE "tub") "*图框,*TITLE") 
      (disableCtrls '("tut" "layer"))
    )
    (if (/= (GET_TILE "tut") "*图框,*TITLE") 
      (disableCtrls '("tub" "block"))
    )
    (if n 
      (set_tile "v1" (itoa n))
    )

    (action_tile "PlotRa" 
                 "(disableCtrls '(\"bana\" \"banb\" \"navar\" \"salo\" \"Browse\" \"Odn\" \"Bdn\" \"Aun\" \"Rnm\" \"writb\" \"clesa\"))"
    )
    (action_tile "PLautRa" 
                 "(disableCtrls '(\"bana\" \"banb\" \"navar\" \"salo\" \"Browse\" \"Odn\" \"Bdn\" \"Aun\" \"Rnm\" \"writb\" \"clesa\"))"
    )
    (action_tile "LautRa" 
                 "(EnableCtrls '(\"bana\" \"banb\" \"navar\" \"Browse\" \"Odn\" \"Bdn\" \"Aun\" \"Rnm\"))(disableCtrls '(\"salo\" \"writb\" \"clesa\"))"
    )
    (action_tile "PPdfRa" 
                 "(EnableCtrls '(\"bana\" \"banb\" \"navar\" \"salo\" \"Browse\" \"Odn\" \"Bdn\" \"Aun\" \"Rnm\"))(set_tile \"banb\" \"pdf\")"
    )
    (action_tile "PDwgRa" 
                 "(EnableCtrls '(\"bana\" \"banb\" \"navar\" \"salo\" \"Browse\" \"Odn\" \"Bdn\" \"Aun\" \"Rnm\" \"writb\" \"clesa\"))(set_tile \"banb\" \"dwg\")"
    )
    ;; 互锁设置结束

    (action_tile "Pick" "(Dcl_b)(DONE_DIALOG 1)")
    (action_tile "block" "(Dcl_b)(setq ret nil)(DONE_DIALOG 2)")
    (action_tile "layer" "(Dcl_b)(setq ret nil)(DONE_DIALOG 3)")
    (action_tile "navar" "(Dcl_b)(DONE_DIALOG 4)")
    (action_tile "Browse" "(Dcl_b)(DONE_DIALOG 5)")
    (action_tile "a1" "(Dcl_b)(N003A $key colorall 1)(DONE_DIALOG 6)")
    (action_tile "sedrw" "(Dcl_b)(setq res (GET_TILE \"tub\") reh (GET_TILE \"tut\"))(DONE_DIALOG 7)")
    (action_tile "hight" "(Dcl_b)(DONE_DIALOG 8)")
    (action_tile "Preview" "(Dcl_b)(DONE_DIALOG 9)")
    (action_tile "accept" "(Dcl_b)(Dcl_M)(setq fold (GET_TILE \"salo\") rn_b (GET_TILE \"bana\"))(DONE_DIALOG 10)")
    (action_tile "cancel" "(DONE_DIALOG 11)")
    (action_tile "help" "(help \"lsp2008\" \"cad2021\")") ;ALR.default.001,实用Common_Lisp函数.,functions,vlz,cad2021
    (setq dd (start_dialog))
    (cond 
      ((= dd 1) (setq ret (Selena))) ;;;指定图块或图层(1)
      ((= dd 2) (setq res (GetBlock))) ;;;指定图块(2)
      ((= dd 3) (setq reh (GetLayer))) ;;;指定图层(3)
      ((= dd 4) (Isvt)) ;;;文件名变量(4)
      ((= dd 5) (setq fold (G_Folder))) ;;;文件保存位置(5)
      ((= dd 6) (Dcl_c)) ;;;图像按钮(6)
      ((= dd 7) (Dcl_c)) ;;;选择要处理的图纸(7)
      ((= dd 8) (HiLiShow bdlist))
      ((= dd 9) (Priew (OrderFrames bdlist)))
      ((= dd 10) (Dcl_r)(Dcl_d))
      ((= dd 11) (if (findfile "0_Set.ini") (vl-file-delete (findfile "0_Set.ini")))(princ "\n亲！天天好心情哦~^o^~"))
    )
  ) ;while循环结束
  (unload_dialog dcl_id)
  (princ)
)

(defun dlg_recf (x0 y0 w0 h0 colorf)
 (fill_image x0 y0 w0 h0 colorf)
)

(defun Dcl_r (/ cad D5) 
  (setq cad (vl-filename-directory (findfile "acad.exe"))
        D5  (list Plo PLa Lau PPd Pdw ban odn bdn aun rn_b res reh ler tob reo wri cle)
  )
  (lst-fn (vl-string-translate "\\""/" (strcat cad "\\CAD二次开发\\data\\0_Set.ini")) D5 "w")
)

(defun N003A (key color color1 / w h dx dy) 
  (setq w (dimx_tile key))
  (setq h (dimy_tile key))
  (setq dx (/ w 3)
        dy (/ h 1)
  )
  (start_image key)
  (dlg_recf 1 1 dx (- dy 1) -2)
  (dlg_recf dx 1 dx (- dy 1) -2)
  (dlg_recf (+ dx dx) 1 dx (- dy 1) -2)
  (if 
    (and 
      (> $x 0)
      (< $x dx)
    )
    (if (< $y dy) 
      (progn 
        (setq sel 1)
        (dlg_recf 1 1 dx (- dy 1) color)
      )
    )
  )
  (if 
    (and 
      (> $x dx)
      (< $x (* dx 2))
    )
    (if (< $y dy) 
      (progn 
        (setq sel 2)
        (dlg_recf dx 1 dx (- dy 1) color)
      )
    )
  )
  (if 
    (and 
      (> $x (+ dx dx))
      (< $x (* dx 3))
    )
    (if (< $y dy) 
      (progn 
        (setq sel 3)
        (dlg_recf (+ dx dx) 1 dx (- dy 1) color)
      )
    )
  )
  (slide_image 0 -1 (- x 1) (- y 1) "N003.sld")
  (end_image)
)
;; 批打图框备选
(defun one (s / wx lname obj p1 p2 px1 py1 px2 py2 mx my path bd m) 
  (if 
    (not 
      (findfile "读我qa.ini") ;  (findfile (strcat (lt:sys-deskTopDir) "\\读我qa.txt"))
    )
    (progn 
      (IF (NULL rem-info) 
        (LOAD "rem-info.LSP")
      )
      (rem-info "读我qa")
    )
  )
  (load "bc.lsp")
  (setq wx (getreal "\n输入排序图框容许误差值\n<空格默认当前...>:"))
  (if (not wx) 
    (setq wx 0.1)
  )
  (setq s (HH:ssPts:Sort s "xY" wx))
  ;;;--------------------------------------------------------
  (setq bdlist nil)
  (setq n (sslength s))
  (setq m 0)
  (while (< m n) 
    (setq lname (ssname s m))
    (setq obj (vlax-ename->vla-object lname))
    (vla-GetBoundingBox obj 'minpt 'maxpt)
    (setq p1 (trans (vlax-safearray->list minPt) 0 1)) ;左下角
    (setq p2 (trans (vlax-safearray->list maxPt) 0 1)) ;右上角
    ;;;mx┏━━━━a━━━━┓p2
    ;;;  ┃         ┃
    ;;;  b         b
    ;;;  ┃         ┃
    ;;;p1┗━━━━a━━━━┛my
    (setq px1 (car p1)) ; 取得p1坐标的x值
    (setq py1 (cadr p1)) ; 取得p1坐标的y值
    (setq px2 (car p2)) ; 取得p2坐标的x值
    (setq py2 (cadr p2)) ; 取得p2坐标的y值
    (setq mx (list px1 py2)) ; 求得mx坐标值
    (setq my (list px2 py1)) ; 求得my坐标值
    (setq a (distance mx p2)) ;mx点 ,p2点之间的距离：横向的距离值
    (setq b (distance p1 mx)) ;p1点 ,mx点之间的距离：纵向的距离值
    (setq bd (list p1 p2))
    (setq bdlist (cons bd bdlist))
    (setq m (1+ m))
  )
  (setq rsh T
        ret (vla-get-effectivename obj)
  )
)
;; 单打图框备选
(defun two (ges / s sort shulist p p1 p2 px1 py1 px2 py2 mx my path bd) 
  (prompt "\n请选择图形!")
  (if 
    (and 
      (setq s (SSGET ges))
      (= (cdr (assoc 0 (setq sort (entget (ssname s 0))))) "LINE")
      (= (sslength s) 1)
    )
    (progn 
      (setq p (cdr (assoc 10 sort)))
      (setq p1 (polar p (angtof "45") 1))
      (setq p2 (polar p (angtof "225") 1))
      (command "_.select" "box" p1 p2 "")
      (setq s (SSGET "P"))
    )
  )
  (setq wcm T
        ret (vla-get-Layer (vlax-ename->vla-object (ssname s 0)))
  )
  (setq bdlist nil)
  (setq n 0)
  (while s 
    (setq shulist (minmm_ssbox s))
    (setq p1 (car shulist)) ;左下角
    (setq p2 (cadr shulist)) ;右上角
    ;;;mx┏━━━━a━━━━┓p2
    ;;;  ┃         ┃
    ;;;  b         b
    ;;;  ┃         ┃
    ;;;p1┗━━━━a━━━━┛my
    (setq px1 (car p1)) ; 取得p1坐标的x值
    (setq py1 (cadr p1)) ; 取得p1坐标的y值
    (setq px2 (car p2)) ; 取得p2坐标的x值
    (setq py2 (cadr p2)) ; 取得p2坐标的y值

    (setq mx (list px1 py2)) ; 求得mx坐标值
    (setq my (list px2 py1)) ; 求得my坐标值
    (setq a (distance mx p2)) ;mx点 ,p2值点之间的距离：横向的距离
    (setq b (distance p1 mx)) ;p1点 ,mx点之间的距离：纵向的距离值
    (setq bd (list p1 p2))
    (setq bdlist (cons bd bdlist))
    (setq n (1+ n))
    (prompt "\n★请选择下一个图形！")
    (if 
      (and 
        (setq s (SSGET ges))
        (= (cdr (assoc 0 (setq sort (entget (ssname s 0))))) "LINE")
        (= (sslength s) 1)
      )
      (progn 
        (setq p (cdr (assoc 10 sort)))
        (setq p1 (polar p (angtof "45") 1))
        (setq p2 (polar p (angtof "225") 1))
        (command "_.select" "box" p1 p2 "")
        (setq s (SSGET "P"))
      )
    )
  )
)

;; 对图框排序
(defun OrderFrames (bdlist / vscoor)
    (defun vscoor (n)   ;屏幕视觉坐标，允许误差啦～大致相同就相同了。
        (fix (/ n (/ (getvar "viewsize") 100.0)))
    )
    ;;主要排序
    (if	(= tob "1")
        (setq bdlist (vl-sort bdlist
            '(lambda (f1 f2 / rt x1 y1 x2 y2)
                ;;x1, y1对应于第一组的图框中心点的坐标
                (setq y1 (vscoor (cadar f1)))
                (setq y2 (vscoor (cadar f2)))
                (setq x1 (vscoor (caar f1)))
                (setq x2 (vscoor (caar f2)))
                (setq rt (> y1 y2))
                ;;优先Y坐标比较，大的话在前
                (if (and (null rt) (= y1 y2))
                    ;;Y坐标相同时，比较X坐标，小的话在前
                    (setq rt (< x1 x2))
                )
                rt
            )
            )
        )
    )
    (if	(= ler "1")
        (setq bdlist (vl-sort bdlist
            '(lambda (f1 f2 / rt x1 y1 x2 y2)
                (setq y1 (vscoor (cadar f1)))
                (setq y2 (vscoor (cadar f2)))
                (setq x1 (vscoor (caar f1)))
                (setq x2 (vscoor (caar f2)))
                (setq rt (< x1 x2))
                ;;优先X坐标比较，小的话在前
                (if (and (null rt) (= x1 x2))
                    ;;当X坐标相同时，比较Y坐标，大的话在前
                    (setq rt (> y1 y2))
                )
                rt
                )
            )
        )
    )
    (if	(= reo "1")
        (setq bdlist (reverse bdlist))
    )
    bdlist
)
;;; 主程式开始~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 (setq acaddoc (vla-get-activedocument (vlax-get-acad-object)))
 (setq Shel (Vlax-Get-Or-Create-Object "Shell.Application"))
 (setq ocho (getvar "cmdecho"))
 (setvar "cmdecho" 0)
 (setq kk (getvar "osmode"))
 (setvar "osmode" 0)
 (command "ucs" "")
 (setq prna (defultprint))
 (prompt "\n此程序用来批量或单张打印框选图形图纸!")
 (prompt "\n由ZYD设计工作室开发，限内部使用!")
  
 (setq colorall 105)
 (N003 colorall)
 (setvar "osmode" kk)
 (setvar "cmdecho" ocho)
 (if (not (vlax-object-released-p Shel)) 
    (vlax-release-object Shel)
  )
 (prompt "\n版权所有！仅ZYD设计工作室内部使用!!!")
 (princ)
)

;|
命令: -plot
是否需要详细打印配置？[是(Y)/否(N)] <否>: y

输入布局名或 [?] <模型>:
输入输出设备的名称或 [?] <FX DocuPrint P158 b>:
输入图纸尺寸或 [?] <A3(297x420 mm)>:
输入图纸单位 [英寸(I)/毫米(M)] <毫米>: m
输入图形方向 [纵向(P)/横向(L)] <横向>: l
是否反向打印？[是(Y)/否(N)] <否>:
输入打印区域 [显示(D)/范围(E)/图形界限(L)/视图(V)/窗口(W)] <窗口>: w
输入窗口的左下角 <37746.160752,2799.425042>: 输入窗口的右上角 <39333.760752,3922.085042>: 
输入打印比例 (打印的 毫米=图形单位) 或 [布满(F)] <布满>:
输入打印偏移 (x,y) 或 [居中打印(C)] <-13.65,11.55>: c
是否按样式打印？[是(Y)/否(N)] <是>: y
输入打印样式表名称或 [?] (输入 . 表示无) <acad.ctb>:
是否打印线宽？[是(Y)/否(N)] <是>:
输入着色打印设置 [按显示(A)/线框(W)/隐藏(H)/视觉样式(V)/渲染(R)] <线框>:
是否打印到文件 [是(Y)/否(N)] <N>:
是否保存对页面设置的修改 [是(Y)/否(N)]? <N>
是否继续打印？[是(Y)/否(N)] <Y>:
有效打印区域:  287.34 宽 X 402.50 高
|;


(defun c:000000 (/ s a n m lname li p0 p1 p2 x y xx yy) 
  (setq a (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (setq kk (getvar "osmode"))
  (setvar "osmode" 0)
  (prompt "\n此程序用来打印图面上所有a3和a4图框里的图形!")
  (prompt "\n请在命令行里输入00即可!")

  (setq s (ssget "x"))
  (setq n (sslength s))
  (setq m 0)
  (setq l 0)
  (setq j 0)
  (while (< m n) 
    (setq lname (ssname s m))
    (setq li (entget lname))
    (if (= (cdr (assoc '2 li)) "a3") 
      (progn 
        (setq pa (assoc '10 li))
        (setq p0 (list (nth 1 pa) (nth 2 pa)))
        (setq xx (cdr (assoc '41 li)))
        (setq yy (cdr (assoc '42 li)))
        (setq x (* xx 410))
        (setq y (* yy 289))
        (setq pp (polar p0 0 (/ x 2)))
        (setq p1 (polar pp (/ pi 2) (/ y 2)))
        (setq ppp (polar p0 pi (/ x 2)))
        (setq p2 (polar ppp (* pi 1.5) (/ y 2)))
        (command "-plot" "yes" "" "\\\\192.168.0.4\\Canon LBP2900" "a3" "m" "l" "yes" 
                 "w" p2 p1 "" "" "yes" "acad.ctb" "yes" "no" "no" "yes" "yes"
        )
        (setq l (+ l 1))
      )
    )

    (if (= (cdr (assoc '2 li)) "a4") 
      (progn 
        (setq pa (assoc '10 li))
        (setq p0 (list (nth 1 pa) (nth 2 pa)))
        (setq xx (cdr (assoc '41 li)))
        (setq yy (cdr (assoc '42 li)))
        (setq x (* xx 200))
        (setq y (* yy 289))
        (setq pp (polar p0 0 (/ x 2)))
        (setq p1 (polar pp (/ pi 2) (/ y 2)))
        (setq ppp (polar p0 pi (/ x 2)))
        (setq p2 (polar ppp (* pi 1.5) (/ y 2)))
        (command "-plot" "yes" "" "\\\\192.168.0.4\\Canon LBP2900" "a4" "m" "p" "yes" 
                 "w" p2 p1 "" "" "yes" "acad.ctb" "yes" "no" "no" "yes" "yes"
        )
        (setq j (+ j 1))
      )
    )



    (setq m (+ m 1))
  )
  (setvar "osmode" kk)
  (setvar "cmdecho" a)
  (prompt "\打印请在郑友东电脑上取图!!!")
  (princ)
)
