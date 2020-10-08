;;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;������ӡ��ʼ


;;;--------------------------------------------------------------------------
;;;   0.LSP
;;;    ��Ȩ(C) 2020-2025  Lang_ge
;;;
;;; ģ�Ϳռ��������ӡ���� ��һ��
;;; Lang_ge, 2020��8��3��
;;; ���¸��£�2020��8��3��12ʱ39��

;;;   ��������ѿɹ������κ���;����Ŀ������޸ļ�����, ������ѭ����ԭ��:
;;;
;;;   1)  ���еİ�Ȩͨ����������ÿһ�ݿ�����.
;;;   2)  ��ص�˵���ĵ�Ҳ�������а�Ȩͨ�漰�������ͨ��.

;;;   ��������ṩ��ΪӦ���ϵĲο�, ��δ�����������κα�֤; �����κ�����
;;;   ��;֮��Ӧ��, �Լ���ҵ���������������ı�֤, �ڴ�һ�����Է���.

;;; ��Ҫ���ܴ����￪ʼ
(defun c:QA (/ *Dwg* *Dwn* *error* *Pdf* ax:2DPoint CopyLayout Dcl_b Dcl_c Dcl_d 
             Dcl_M Dcl_r defultprint dlg_recf doBatchLayout doPlotLayoutsInTabOrder 
             expandbounding G_Folder G_tag GetBlock GetBlockList Getblpe GetLayer 
             GetLayerList HiLiShow Is_dcl islandscape Isvt layout-tab-list Mwp N003 
             N003A Nava one OrderFrames Play PlotLayoutsInTabOrder Preng Priew Selena 
             Spblpe Specla two acaddoc ocho kk prna colorall prt_id s word sel sen 
             N_list rn_b
            )
 (vl-load-com)
 
;;; ͨ�ú���:��ע����ȡĬ�ϴ�ӡ����ʼ
(defun defultprint (/ device)
 (vl-load-com)
 (substr (setq device (vl-registry-read "HKEY_CURRENT_USER\\Software\\Microsoft\\Windows NT\\CurrentVersion\\Windows" "Device"))
         1 (vl-string-search "," device)
 )
)
;;; ͨ�ú���:��ע����ȡĬ�ϴ�ӡ������

;;;����������ָ������Ƶ�ļ�
(defun Play (file /)
 ;;;(Vlax-Put-Property (vlax-Create-Object "WMPlayer.OCX") 'URL file)
 (findfile file)
)

;;; ������
(defun *error* (ERROR)
 (princ "\n>>>ZYD��ƹ�����:")
 (princ "��ӡ���ܱ�ȡ��!!")
 (setvar "osmode" 703)
)

;;; ѡ���ļ��жԻ�����
(defun G_Folder (/ get) 
  (if (setq get (Vlax-Invoke-Method Shel 'BrowseForFolder 0 "ѡ�񱣴�Ŀ¼" 16 0)) 
    (setq get (vlax-get-property (vlax-get-property get 'self) 'path))
  )
  (if (vl-file-directory-p get) get)
  get
)

;; ѡ��ͼ����ͼ��
(defun Selena (/ ename ena) 
  (setq ret nil)
  (setq ename (car (entsel "\nָ��ͼ�����ڵ���������:")))
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
;; �������ܱ������
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
    (alert "�������ܱ��")
  )
  (if (= sek 2) 
    (alert "AutoCADϵͳ����")
  )
  (if (= sek 3) 
    (alert "��ָ�������Զ��������")
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

;; ���ܱ���DCL�ӳ���
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
;; ����ͼ���б�
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

;; ��ȡ����ͼ���б�
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

;; ͼ���б�
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

;; ��ȡͼ���б�
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

;; ͼ���е�����
(defun Getblpe (/ tmp_s t_a3 t_ag TMP_l TMP_M dcl_id repn) 
  (setq tmp_s (car (entsel "\n���ͼ��ʰȡ����ͼ��������:")))
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

;; ָ��ͼ���ض�������
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

;; ��ȡͼ���ض���ǩ������ֵ
(defun G_tag ( blk tag )
    (setq tag (strcase tag))
    (vl-some '(lambda (x) (if (= tag (strcase (vla-get-tagstring x))) (vla-get-textstring x)))
        (vlax-invoke (vlax-ename->vla-object blk) 'getattributes)
    )
)

;; ͼ���ϵ��ض����� [��ʽ��:vla-get-StyleName (cons 7 tyNa)]
(defun Specla (/ lent ena lame ltxt p0 ss m enn dcl_id) 
  (setq lent (car (entsel "�������Ҳ�ѡ��һ�����������ȷ���ض���ͼ����:")))
  (setq ena (vlax-ename->vla-object lent))
  (if (null lent) (exit))
  (setq lame (vla-get-Layer ena)) ;ͼ����
  (setq ltxt (vla-get-textstring ena)) ;ͼ������
  (setvar "osmode" 703)
  (setq p0 (getpoint "���ѡһ�����ͽϴ��ͼ��Χ��Ӧ�����ղ�ѡ����������:"))
  (setq ss (ssget "_w" p0 (getcorner p0 "�Խǵ�:") (list '(-4 . "<OR") '(0 . "TEXT") '(0 . "MTEXT") '(-4 . "OR>") (cons 8 lame))))
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

;; �������ִ���
(defun Mwp (ment /) 
  (if (= (cdr (assoc 0 (entget ment))) "MTEXT") 
    (progn 
      (command "_explode" ment "")
      (vl-string-trim " " (cdr (assoc 1 (entget (entlast)))))
    )
    (cdr (assoc 1 (entget ment)))
  )
)

;; DCL�Ի���ѡ����Ϣ
(DEFUN Dcl_M (/) 
  (cond 
    ((= (GET_TILE "PlotRa") "1") (progn (princ "\n�ʼ������ӡ") (setq sen 1)))
    ((= (GET_TILE "LautRa") "1") (progn (princ "\n�ʼ�������ɲ���") (setq sen 2)))
    ((= (GET_TILE "PPdfRa") "1") (progn (princ "\n�ʼ������ӡPDF�ĵ�") (setq sen 3)))
    ((= (GET_TILE "PDwgRa") "1") (progn (princ "\n�ʼ�����з�Dwg~�ĵ�") (setq sen 4)))
    ((= (GET_TILE "PLautRa") "1") (progn (princ "\n�ʼ������ӡ���в���") (setq sen 5)))
  )
)

;; ���ѡ���״ֵ̬
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
;; ͼ��ѡ���л�
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
         (setq word (strcase (getstring "\n������ƥ���ַ�:")))
         (setq s (ssget (list (cons 0 "INSERT") (cons 2 (strcat "*" word "*")))))
         (one s)
       )
     )
     (if (= sel 3) 
       (two '((0 . "line,arc,lwpolyline,circle,3dsolid,DIMENSION")))
     )
    )
    ((and res (/= res "*ͼ��,*TITLE"))
     (setq s (ssget (list (cons 0 "INSERT") (cons 2 res))))
     (one s)
    )
    ((and reh (/= reh "*ͼ��,*TITLE"))
     (two (list (cons 0 "line") (cons 8 reh)))
    )
  )
)

;; ��ӡ��ʼ��
(defun Dcl_d (/ x m) 
  (setq m 0)
  (if bdlist 
    (foreach x (OrderFrames bdlist) 
      (setq p1 (car x) p2 (last x))
      (princ (strcat "\n���ڽ���: ��" (itoa (setq m (1+ m))) "/" (itoa n) "ҳ"))
      (Preng)
    )
    (Preng)
  )
  (if n (princ (strcat "\nOK!�˴ι����:" (itoa n) " ��ͼֽ")))
  (if path (vlax-invoke-method Shel 'Open path))
)

;; ��ӡ�������
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

;; ���ĵ�������!
(DEFUN *Dwn* (path fold dwg sufix / x) 
  (if (not N_list) (setq N_list '()))
  (foreach x N_list 
    (if (wcmatch x (strcat dwg "*")) 
      (setq dwg (strcat dwg (rtos (1- (atof (vl-string-subst "" dwg x))) 2 0)))
    )
  )
  (setq N_list (cons dwg N_list))
  (vl-mkdir (strcat path fold)) ;vl-file-directory-p(�ж��ļ����Ƿ����)
  (strcat path fold "\\" dwg sufix)
)

;; �ļ�������!
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
             (alert "���ݴ���,�������Ʊ���!")
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

;; ��ӡPDF�ĵ�!
(DEFUN *Pdf* (/ dwg dwna) 
  (setvar "FILEDIA" 0)
  (setq dwg (Nava rn_b))
  (setq dwna (*Dwn* fold "\\pdf" dwg ""))
  (setq path (vl-filename-directory dwna))
  ; (alert dwna) ;;������
  (command "-EXPORT" "P" "w" p1 p2 "y" "" "" "l" "f" "y" "monochrome.ctb" "y" dwna) 
  (princ)
)

;; �з�DWG�ĵ�!
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

;; �������ɲ���
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
  
   ;; ȡ�ò�����
  (setvar "FILEDIA" 0)
  (setq layoutprefix (vl-string-left-trim "\\" (*Dwn* "" "" (Nava rn_b) "")))

  ;;���������ڷ�ֹ��ӡʱ�򲻳�����
  (setq portraitList (vl-remove-if 'islandscape bdlist))
  (setq landscapelist (vl-remove-if-not 'islandscape bdlist))
  (setq clayout (vla-get-activelayout acaddoc))
  (vla-getpapersize clayout 'pWidth 'pHeight)

  ;;ҳ�����ô�����
  (setq catch (vl-catch-all-apply 'vla-put-paperunits (list clayout acMillimeters)))
  (if (vl-catch-all-error-p catch) 
    (progn 
      (vla-put-configname clayout "��")
      (vla-put-paperunits clayout acMillimeters)
    )
  )
  ;; ȡ�õ�ǰֽ�ŵĳ��߳���
  (if (< pwidth pheight) 
    (setq PaperWidth pHeight)
    (setq PaperWidth pWidth)
  )
  ;; ������תʲô
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

  ;; ����0��ģ��
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
        ;; �ָ���ͼֽ�ռ�
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
  ;; ����90��ģ��
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
        ;; �ָ���ͼֽ�ռ�
        (vl-cmdf "_.pspace")
        ;;; (setq layoutprefix (getstring "\n����������ǰ׺:"))
        ;;; (if (member layoutprefix (GetLayoutList)) 
        ;;;   (setq layoutprefix (GetNewAutoNumName layoutprefix 2 (GetLayoutList)))
        ;;; )
        (vl-cmdf "_.layout" "_rename" "$temp_90_Rotation_Ctab" layoutprefix)
      )
      (vl-cmdf "_.layout" "_delete" "$temp_90_Rotation")
    )
  )
  ;; �ص�ģ��
  (setvar "ctab" "Model")
  (princ "\n���ֳɹ����ɡ�")
)

;; ������ӡ����
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
	     "\n��ӡ����[ȫ��(A)/ѡ��(S)]/<ȫ��>:"
	   )
    )
    (if	(eq key "Select")
      (progn
        (if (setq sellst (Odcl:MultList "��ѡ��Ҫ��ӡ�Ĳ���" "�����б��ɶ�ѡ��" LayoutNameList))
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
	   (getkword "��ӡ��PLT�ļ�? [��(Y)/��(N)] <��>: ")
    )
    (setq Plot-to-file-p (= plot-to-file-p "Yes"))
    (PlotLayoutsInTabOrder LayoutNameList plot-to-file-p)
    (setq n (length LayoutNameList))
    (princ)
  )

;;; ��ӡ������غ���
;;;--------------------------------------------------------
;; ������ӡ��������
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
;; ����˳���б�
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
  
;; �õ��Զ������
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

;; �����б�
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

;; ��ͼ����������
(defun expandbounding (bd fuzz / pt1 pt2 offset w h)
    (setq w (- (caadr bd) (caar bd)))
    (setq h (- (cadadr bd) (cadar bd)))
    (setq offset (/ w fuzz))    ;���������ڷ�ֹ��ӡʱ�򲻳�����
    (setq pt1 (mapcar '- (car bd) (list offset offset)))
    (setq pt2 (mapcar '+ (cadr bd) (list offset offset)))
    (list pt1 pt2)
)

;; �ж�ͼ���Ƿ����
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

;; ���Ʋ���
(defun CopyLayout (nLayoutName / clayoutName tmpName) 
  (setq clayoutName (getvar "ctab"))
  (setq tmpName (strcat "$_QF_TMP_LAYOUT_NAME_OF_" clayoutName))
  (vl-cmdf "_.layout" "_copy" clayoutName tmpName)
  (vl-cmdf "_.layout" "_rename" clayoutName NLayoutName)
  (vl-cmdf "_.layout" "_rename" tmpName clayoutName)
)
;;;--------------------------------------------------------

;; ������ʾ����
(defun HiLiShow (bdlist / x) 
  (vl-cmdf "_.redraw")
  (foreach x bdlist 
    (grdraw (car x) (list (caar x) (cadadr x)) acRed 1)
    (grdraw (list (caar x) (cadadr x)) (list (caadr x) (cadar x)) acRed 1)
    (grdraw (list (caadr x) (cadar x)) (last x) acRed 1)
    (grdraw (last x) (car x) acRed 1)
  )
  (getstring "\nͼ�к�ɫ���������Ϊѡ�е�ͼ��<����>")
  (princ)
)

;; Ԥ������
(defun Priew (bdlist / plot clayout bounding tart key) 
  (setq plot (vla-get-plot acaddoc))
  (setq clayout (vla-get-activelayout acaddoc))
  (setq bdlist (mapcar (function (lambda (v) (expandbounding v 1e6))) bdlist))
  (foreach bounding bdlist 
    ;; ���ô�ӡ��Χ
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
             (setq key (getkword "�Ƿ����Ԥ����һ��? [Yes/No]<Yes>"))
             (if (= key "No") 
               (exit)
             )
      )
    )
  )
  (princ)
)

 ;;2ά���г�
  (defun ax:2DPoint (pt) 
  (vlax-make-variant 
    (vlax-safearray-fill 
      (vlax-make-safearray vlax-vbdouble '(0 . 1))
      (list (car pt) (cadr pt))
    )
  )
)
;; ������ӡ�Ի����ʼ��
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
          baa "<Att:a3:���ͼ��>"
          res "*ͼ��,*TITLE"
          reh "*ͼ��,*TITLE"
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
  (while (< dd 10)  ;whileѭ����ʼ
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

    ;; �Ի���״̬����
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

    ;; ������ͨ��Ŧ����
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
    (if (/= (GET_TILE "tub") "*ͼ��,*TITLE") 
      (disableCtrls '("tut" "layer"))
    )
    (if (/= (GET_TILE "tut") "*ͼ��,*TITLE") 
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
    ;; �������ý���

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
    (action_tile "help" "(help \"lsp2008\" \"cad2021\")") ;ALR.default.001,ʵ��Common_Lisp����.,functions,vlz,cad2021
    (setq dd (start_dialog))
    (cond 
      ((= dd 1) (setq ret (Selena))) ;;;ָ��ͼ���ͼ��(1)
      ((= dd 2) (setq res (GetBlock))) ;;;ָ��ͼ��(2)
      ((= dd 3) (setq reh (GetLayer))) ;;;ָ��ͼ��(3)
      ((= dd 4) (Isvt)) ;;;�ļ�������(4)
      ((= dd 5) (setq fold (G_Folder))) ;;;�ļ�����λ��(5)
      ((= dd 6) (Dcl_c)) ;;;ͼ��ť(6)
      ((= dd 7) (Dcl_c)) ;;;ѡ��Ҫ�����ͼֽ(7)
      ((= dd 8) (HiLiShow bdlist))
      ((= dd 9) (Priew (OrderFrames bdlist)))
      ((= dd 10) (Dcl_r)(Dcl_d))
      ((= dd 11) (if (findfile "0_Set.ini") (vl-file-delete (findfile "0_Set.ini")))(princ "\n�ף����������Ŷ~^o^~"))
    )
  ) ;whileѭ������
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
  (lst-fn (vl-string-translate "\\""/" (strcat cad "\\CAD���ο���\\data\\0_Set.ini")) D5 "w")
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
;; ����ͼ��ѡ
(defun one (s / wx lname obj p1 p2 px1 py1 px2 py2 mx my path bd m) 
  (if 
    (not 
      (findfile "����qa.ini") ;  (findfile (strcat (lt:sys-deskTopDir) "\\����qa.txt"))
    )
    (progn 
      (IF (NULL rem-info) 
        (LOAD "rem-info.LSP")
      )
      (rem-info "����qa")
    )
  )
  (load "bc.lsp")
  (setq wx (getreal "\n��������ͼ���������ֵ\n<�ո�Ĭ�ϵ�ǰ...>:"))
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
    (setq p1 (trans (vlax-safearray->list minPt) 0 1)) ;���½�
    (setq p2 (trans (vlax-safearray->list maxPt) 0 1)) ;���Ͻ�
    ;;;mx����������a����������p2
    ;;;  ��         ��
    ;;;  b         b
    ;;;  ��         ��
    ;;;p1����������a����������my
    (setq px1 (car p1)) ; ȡ��p1�����xֵ
    (setq py1 (cadr p1)) ; ȡ��p1�����yֵ
    (setq px2 (car p2)) ; ȡ��p2�����xֵ
    (setq py2 (cadr p2)) ; ȡ��p2�����yֵ
    (setq mx (list px1 py2)) ; ���mx����ֵ
    (setq my (list px2 py1)) ; ���my����ֵ
    (setq a (distance mx p2)) ;mx�� ,p2��֮��ľ��룺����ľ���ֵ
    (setq b (distance p1 mx)) ;p1�� ,mx��֮��ľ��룺����ľ���ֵ
    (setq bd (list p1 p2))
    (setq bdlist (cons bd bdlist))
    (setq m (1+ m))
  )
  (setq rsh T
        ret (vla-get-effectivename obj)
  )
)
;; ����ͼ��ѡ
(defun two (ges / s sort shulist p p1 p2 px1 py1 px2 py2 mx my path bd) 
  (prompt "\n��ѡ��ͼ��!")
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
    (setq p1 (car shulist)) ;���½�
    (setq p2 (cadr shulist)) ;���Ͻ�
    ;;;mx����������a����������p2
    ;;;  ��         ��
    ;;;  b         b
    ;;;  ��         ��
    ;;;p1����������a����������my
    (setq px1 (car p1)) ; ȡ��p1�����xֵ
    (setq py1 (cadr p1)) ; ȡ��p1�����yֵ
    (setq px2 (car p2)) ; ȡ��p2�����xֵ
    (setq py2 (cadr p2)) ; ȡ��p2�����yֵ

    (setq mx (list px1 py2)) ; ���mx����ֵ
    (setq my (list px2 py1)) ; ���my����ֵ
    (setq a (distance mx p2)) ;mx�� ,p2ֵ��֮��ľ��룺����ľ���
    (setq b (distance p1 mx)) ;p1�� ,mx��֮��ľ��룺����ľ���ֵ
    (setq bd (list p1 p2))
    (setq bdlist (cons bd bdlist))
    (setq n (1+ n))
    (prompt "\n����ѡ����һ��ͼ�Σ�")
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

;; ��ͼ������
(defun OrderFrames (bdlist / vscoor)
    (defun vscoor (n)   ;��Ļ�Ӿ����꣬�����������������ͬ����ͬ�ˡ�
        (fix (/ n (/ (getvar "viewsize") 100.0)))
    )
    ;;��Ҫ����
    (if	(= tob "1")
        (setq bdlist (vl-sort bdlist
            '(lambda (f1 f2 / rt x1 y1 x2 y2)
                ;;x1, y1��Ӧ�ڵ�һ���ͼ�����ĵ������
                (setq y1 (vscoor (cadar f1)))
                (setq y2 (vscoor (cadar f2)))
                (setq x1 (vscoor (caar f1)))
                (setq x2 (vscoor (caar f2)))
                (setq rt (> y1 y2))
                ;;����Y����Ƚϣ���Ļ���ǰ
                (if (and (null rt) (= y1 y2))
                    ;;Y������ͬʱ���Ƚ�X���꣬С�Ļ���ǰ
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
                ;;����X����Ƚϣ�С�Ļ���ǰ
                (if (and (null rt) (= x1 x2))
                    ;;��X������ͬʱ���Ƚ�Y���꣬��Ļ���ǰ
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
;;; ����ʽ��ʼ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 (setq acaddoc (vla-get-activedocument (vlax-get-acad-object)))
 (setq Shel (Vlax-Get-Or-Create-Object "Shell.Application"))
 (setq ocho (getvar "cmdecho"))
 (setvar "cmdecho" 0)
 (setq kk (getvar "osmode"))
 (setvar "osmode" 0)
 (command "ucs" "")
 (setq prna (defultprint))
 (prompt "\n�˳��������������Ŵ�ӡ��ѡͼ��ͼֽ!")
 (prompt "\n��ZYD��ƹ����ҿ��������ڲ�ʹ��!")
  
 (setq colorall 105)
 (N003 colorall)
 (setvar "osmode" kk)
 (setvar "cmdecho" ocho)
 (if (not (vlax-object-released-p Shel)) 
    (vlax-release-object Shel)
  )
 (prompt "\n��Ȩ���У���ZYD��ƹ������ڲ�ʹ��!!!")
 (princ)
)

;|
����: -plot
�Ƿ���Ҫ��ϸ��ӡ���ã�[��(Y)/��(N)] <��>: y

���벼������ [?] <ģ��>:
��������豸�����ƻ� [?] <FX DocuPrint P158 b>:
����ͼֽ�ߴ�� [?] <A3(297x420 mm)>:
����ͼֽ��λ [Ӣ��(I)/����(M)] <����>: m
����ͼ�η��� [����(P)/����(L)] <����>: l
�Ƿ����ӡ��[��(Y)/��(N)] <��>:
�����ӡ���� [��ʾ(D)/��Χ(E)/ͼ�ν���(L)/��ͼ(V)/����(W)] <����>: w
���봰�ڵ����½� <37746.160752,2799.425042>: ���봰�ڵ����Ͻ� <39333.760752,3922.085042>: 
�����ӡ���� (��ӡ�� ����=ͼ�ε�λ) �� [����(F)] <����>:
�����ӡƫ�� (x,y) �� [���д�ӡ(C)] <-13.65,11.55>: c
�Ƿ���ʽ��ӡ��[��(Y)/��(N)] <��>: y
�����ӡ��ʽ�����ƻ� [?] (���� . ��ʾ��) <acad.ctb>:
�Ƿ��ӡ�߿�[��(Y)/��(N)] <��>:
������ɫ��ӡ���� [����ʾ(A)/�߿�(W)/����(H)/�Ӿ���ʽ(V)/��Ⱦ(R)] <�߿�>:
�Ƿ��ӡ���ļ� [��(Y)/��(N)] <N>:
�Ƿ񱣴��ҳ�����õ��޸� [��(Y)/��(N)]? <N>
�Ƿ������ӡ��[��(Y)/��(N)] <Y>:
��Ч��ӡ����:  287.34 �� X 402.50 ��
|;


(defun c:000000 (/ s a n m lname li p0 p1 p2 x y xx yy) 
  (setq a (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (setq kk (getvar "osmode"))
  (setvar "osmode" 0)
  (prompt "\n�˳���������ӡͼ��������a3��a4ͼ�����ͼ��!")
  (prompt "\n����������������00����!")

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
  (prompt "\��ӡ����֣�Ѷ�������ȡͼ!!!")
  (princ)
)
