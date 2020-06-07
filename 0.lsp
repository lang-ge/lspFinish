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

(defun *error* (ERROR)
 (princ "\n>>>ZYD��ƹ�����:")
 (princ "��ӡ���ܱ�ȡ��!!")
 (setvar "osmode" 703)
)
(defun c:QA (/ a kk prna colorall prt_id s word sel sen N_list)
 (setq a (getvar "cmdecho"))
 (setvar "cmdecho" 0)
 (setq kk (getvar "osmode"))
 (setvar "osmode" 0)
 (command "ucs" "")
 (setq prna (defultprint))
 (prompt "\n�˳��������������Ŵ�ӡ��ѡͼ��ͼֽ!")
 (prompt "\n��ZYD��ƹ����ҿ��������ڲ�ʹ��!")
  
 (setq colorall 105)
 (N003 colorall)
 (if (not sel)
  (exit))
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
  (two)
 )
 (setvar "osmode" kk)
 (setvar "cmdecho" a)
 (prompt "\n��Ȩ���У���ZYD��ƹ������ڲ�ʹ��!!!")
 (princ)
)
;;;DCL�Ի���ѡ����Ϣ
(DEFUN Dcl_M (/) 
  (cond 
    ((= (GET_TILE "PlotRa") "1") (progn (princ "\n�ʼ������ӡ") (setq sen 1)))
    ((= (GET_TILE "LautRa") "1") (progn (princ "\n�ʼ�������ɲ���") (setq sen 2)))
    ((= (GET_TILE "PPdfRa") "1") (progn (princ "\n�ʼ������ӡPDF�ĵ�") (setq sen 3)))
    ((= (GET_TILE "PDwgRa") "1") (progn (princ "\n�ʼ�����з�Dwg~�ĵ�") (setq sen 4)))
    ((= (GET_TILE "PLautRa") "1") (progn (princ "\n�ʼ������ӡ���в���") (setq sen 5)))
  )
)
;;;��ӡ�������
(defun Preng (/ dwg) 
  (cond 
    ((= sen 1)
     (if (> a b) 
       (command "-plot" "yes" "" prna "A4" "m" "l" "" "w" p1 p2 "f" "c" "yes" 
                "acad.ctb" "yes" "" "no" "yes" "yes"
       )
       (command "-plot" "yes" "" prna "A4" "m" "p" "" "w" p1 p2 "f" "c" "yes" 
                "acad.ctb" "yes" "" "no" "yes" "yes"
       )
     )
    )
    ((= sen 2)
     (progn 
       (princ "xxxx")
       (setq sen 2)
     )
    )
    ((= sen 3) (*Pdf*))
    ((= sen 4) (*Dwg* (ssget "w" p1 p2)))
    ((= sen 5)
     (progn 
       (princ "xxxx")
       (setq sen 5)
     )
    )
  )
)

;;; �������ɲ���
  (defun doBatchLayout (bdlist plotscale ltscale autoRotate UpsideDown / landscapeList 
                      layoutprefix portraitList NoRotationList RotationList bd bd2 
                      layouts layout ll ur MarginLL MarginUR prdisplay crtvp showpg 
                      pwidth pHeight PaperWidth item
                     ) 
  (setq prdisplay (vla-get-display 
                    (vla-get-preferences (vlax-get-acad-object))
                  )
  )
  (setq layouts (vla-get-layouts acaddoc))
  (setq bdlist (mapcar (function (lambda (v) (expandbounding v 1e6))) bdlist))
  ;;���������ڷ�ֹ��ӡʱ�򲻳�����
  (setq portraitList (vl-remove-if 'islandscape bdlist))
  (setq landscapelist (vl-remove-if-not 'islandscape bdlist))
  (vla-getpapersize clayout 'pWidth 'pHeight)
  ;; ȡ�õ�ǰֽ�ŵĳ��߳���
  (if (< pwidth pheight) 
    (setq PaperWidth pHeight)
    (setq PaperWidth pWidth)
  )
  ;; ������תʲô
  (setq NoRotationList (if (> pwidth pHeight) 
                         landscapeList
                         portraitList
                       )
  )
  (setq RotationList (if (> pwidth pHeight) 
                       portraitList
                       landscapeList
                     )
  )

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
        (setq layoutprefix (FormatName (getvalue 'NamePrefix) bd nil))
        (vl-cmdf "_.Zoom" "_Window" "_non" (car bd) "_non" (cadr bd))
        (if (numberp plotscale) 
          (vl-cmdf "_.zoom" 
                   "_Scale"
                   (strcat (rtos (/ 1.0 plotscale) 2 100) "xp")
          )
        )
        ;; �ָ���ͼֽ�ռ�
        (vl-cmdf "_.pspace")
        (if (member layoutprefix (GetLayoutList)) 
          (setq layoutprefix (GetNewAutoNumName layoutprefix 2 (GetLayoutList)))
        )
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
        (setq layoutprefix (FormatName (getvalue 'NamePrefix) bd nil))
        (vl-cmdf "_.Zoom" "_Window" "_non" (car bd) "_non" (cadr bd))
        (if (numberp plotscale) 
          (vl-cmdf "_.zoom" 
                   "_Scale"
                   (strcat (rtos (/ 1.0 plotscale) 2 100) "xp")
          )
        )
        ;; �ָ���ͼֽ�ռ�
        (vl-cmdf "_.pspace")
        (if (member layoutprefix (GetLayoutList)) 
          (setq layoutprefix (GetNewAutoNumName layoutprefix 2 (GetLayoutList)))
        )
        (vl-cmdf "_.layout" "_rename" "$temp_90_Rotation_Ctab" layoutprefix)
      )
      (vl-cmdf "_.layout" "_delete" "$temp_90_Rotation")
    )
  )
  ;; back to model
  (setvar "ctab" "Model")
  (princ "\n����������ϡ�")
)

;;; ���ĵ�������!
(DEFUN *Dwn* (dwg sufix fold / x) 
  (if (not N_list) (setq N_list '()))
  (foreach x N_list 
    (if (wcmatch x (strcat dwg "*")) 
      (setq dwg (strcat dwg 
                        (RTOS (1- (atof (vl-string-left-trim dwg x))) 2 0)
                )
      )
    )
  )
  (setq N_list (append (list dwg) N_list))
  (SETQ path (strcat (GETVAR "DWGPREFIX") fold "\\"))
  (vl-mkdir path) ;vl-file-directory-p(�ж��ļ����Ƿ����)
  (strcat path dwg sufix)
)

;;; ��ӡPDF�ĵ�!
(DEFUN *Pdf* (/ GRP zd shujlb x dwna) 
  (setvar "filedia" 0)
  (setq GRP (ssget "w" p1 p2))
  (if (setq zd (SSGET "P" '((2 . "*a[345]*")))) 
    (progn 
      (setq shujlb (MJ:GetAttributes (cdar (entget (ssname zd 0)))))
      (setq dwg (strcat (nth 1 (nth 3 shujlb)) (nth 1 (nth 2 shujlb))))
    )
    (setq dwg (getstring "�������ļ�����:"))
  )
  (setq dwna (*Dwn* dwg "" "pdf"))
  ;;; (alert dwg) ;;������
  (command "-EXPORT" "P" "w" p1 p2 "y" "" "" "l" "f" "y" "monochrome.ctb" "y" dwna)
  ;;; (command "-EXPORT" "P" "w" p1 p2 "y" "a3" "m" "l" "f" "y" "acad.ctb" "y" dwg)
)

;;; �з�DWG�ĵ�!
(DEFUN *Dwg* (GRP / zd shujlb dwg dwna) 
  (command "_copy" GRP "" "0,0,0" "0,0,0")
  (setq GRP (SSGET "P"))
  (if (not (setq zd (SSGET "P" '((2 . "*a[345]*"))))) 
    (if (setq ent (car (entsel "�ļ���ȱʧ!��ѡ��ͼ��!"))) 
      (setq zd (ssadd ent))
    )
  )
  (if zd 
    (progn 
      (setq shujlb (MJ:GetAttributes (cdar (entget (ssname zd 0)))))
      (setq DWG (strcat (nth 1 (nth 3 shujlb)) (nth 1 (nth 2 shujlb))))
    )
    (setq dwg (getstring "�������ļ�����:"))
  )


  ;;; (if (not (setq zd (SSGET "P" '((2 . "*a[345]*")))))
  ;;;   (setq zd (ssadd (car (entsel "�ļ���ȱʧ!��ѡ��ͼ��!"))))
  ;;; )
  ;;; (setq shujlb (MJ:GetAttributes (cdar (entget (ssname zd 0)))))
  ;;; (setq dwg (strcat (nth 1 (nth 3 shujlb)) (nth 1 (nth 2 shujlb))))
  (setq dwna (*Dwn* dwg ".dwg" "dwg"))
  (SETVAR "FILEDIA" 0)
  (command "_EXPORT" dwna "" "0,0,0" GRP "")
  (princ)
)

(defun N003 (color / dcl_id x y dx dy)
 (setq dcl_id (load_dialog "N003"))
 (if (> dcl_id 0)
  (new_dialog "N003" dcl_id)
  (prompt "Unable to open the dialog box!")
 )
 (start_image "a1")
 (setq x (dimx_tile "a1"))
 (setq y (dimy_tile "a1"))
 (setq dx (/ x 3)
       dy (/ y 1)
 )
 (dlg_recf 1 1 dx (- dy 2) color)
 (slide_image 0 -1 (- x 1) (- y 1) "N003.sld")
 (end_image)
 (set_tile "PlotRa" "1")
 (action_tile "a1" "(Play (findfile \"menu back.wav\"))(setq fname (N003A $key colorall 1))(Dcl_M)(done_dialog dcl_id)")
 (action_tile "accept" "(Dcl_M)(if(= sel nil)(progn(done_dialog dcl_id)(setq sel 1))(done_dialog dcl_id))")
 (action_tile "help" "(help \"lsp2008\" \"cad2021\")") ;ALR.default.001,ʵ��Common_Lisp����.,functions,vlz,cad2021
 (action_tile "cancel" "(setq sel nil)")
 (if (/= sel nil)
  (done_dialog dcl_id)
 )
 (start_dialog)
)
(defun dlg_recf (x0 y0 w0 h0 colorf)
 (fill_image x0 y0 w0 h0 colorf)
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
 (if (and
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
 (if (and
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
 (if (and
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
;;;�����ʽ
(defun one (s / wx n m lname obj p1 p2 px1 py1 px2 py2 mx my a b path)
 ;;;-------------------------------------------------------- 
 (if (not
       (findfile "����qa.ini")
;       (findfile (strcat (lt:sys-deskTopDir) "\\����qa.txt"))
     )
  (progn
    (IF	(NULL rem-info)
      (LOAD "rem-info.LSP")
    ) (rem-info "����qa")))
 (load "bc.lsp")
 (setq wx (getreal "\n��������ͼ���������ֵ\n<�ո�Ĭ�ϵ�ǰ...>:"))
 (if (not wx) (setq wx 0.1))
 (setq s (HH:ssPts:Sort s "xY" wx))
 ;;;--------------------------------------------------------
 (setq n (sslength s))
 (setq m 0)
 (while (< m n)
  (setq lname (ssname s m))
  (setq obj (vlax-ename->vla-object lname))
  (vla-GetBoundingBox obj 'minpt 'maxpt)
  (setq p1 (trans (vlax-safearray->list minPt) 0 1));;;���½�
  (setq p2 (trans (vlax-safearray->list maxPt) 0 1));;;���Ͻ�
;;;mx����a��������p2
;;;  ��         ��
;;;   b          b
;;;  ��         ��
;;;  ��         ��
;;;p1����a��������my
  (setq px1 (car p1))			; ȡ��p1�����xֵ
  (setq py1 (cadr p1))			; ȡ��p1�����yֵ
  (setq px2 (car p2))			; ȡ��p2�����xֵ
  (setq py2 (cadr p2))			; ȡ��p2�����yֵ
  (setq mx (list px1 py2))		; ���mx����ֵ
  (setq my (list px2 py1))		; ���my����ֵ
  (setq a (distance mx p2));;;mx�� ,p2��֮��ľ��룺����ľ���ֵ
  (setq b (distance p1 mx));;;p1�� ,mx��֮��ľ��룺����ľ���ֵ
  (Preng)
 ;(command "modemacro" (strcat "���ڴ�ӡ: ��" (itoa (+ m 1)) "/" (itoa n) "ҳ"))
  (princ (strcat "\n���ڽ���: ��" (itoa (+ m 1)) "/" (itoa n) "ҳ"))
  (setq m (+ m 1))
 )
  (princ (strcat "\nOK!�˴ι����:" (itoa n) " ��ͼֽ"))
  (if path (startapp "explorer" (strcat "/open," path)))
 )
;;;�����ʽ
(defun two (/ s sort m shulist p p1 p2 px1 py1 px2 py2 mx my a b path) 
  (prompt "\n��ѡ��ͼ��!")
  (if 
    (and 
      (setq s (SSGET '((0 . "line,arc,lwpolyline,circle,3dsolid,DIMENSION"))))
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

  (setq m 0)
  (while s 
    (setq shulist (minmm_ssbox s))
    (setq p1 (car shulist)) ;;;���½�
    (setq p2 (cadr shulist)) ;;;���Ͻ�
    ;;;mx����a��������p2
    ;;;  ��         ��
    ;;;   b          b
    ;;;  ��         ��
    ;;;  ��         ��
    ;;;p1����a��������my
    (setq px1 (car p1)) ; ȡ��p1�����xֵ
    (setq py1 (cadr p1)) ; ȡ��p1�����yֵ
    (setq px2 (car p2)) ; ȡ��p2�����xֵ
    (setq py2 (cadr p2)) ; ȡ��p2�����yֵ

    (setq mx (list px1 py2)) ; ���mx����ֵ
    (setq my (list px2 py1)) ; ���my����ֵ
    (setq a (distance mx p2)) ;;;mx�� ,p2ֵ��֮��ľ��룺����ľ���
    (setq b (distance p1 mx)) ;;;p1�� ,mx��֮��ľ��룺����ľ���ֵ

    (Preng)
    (princ (strcat "\n���ڽ���: ��" (itoa (+ m 1)) "ҳ"))
    (setq m (1+ m))
    (prompt "\n����ѡ����һ��ͼ�Σ�")
    (if 
      (and 
        (setq s (SSGET '((0 . "line,arc,lwpolyline,circle,3dsolid,DIMENSION"))))
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
  (princ (strcat "\nOK!�˴ι����:" (itoa m) " ��ͼֽ"))
  (if path (startapp "explorer" (strcat "/open," path)))
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

(setq s (ssget "x" ))   
(setq n (sslength s)) 
(setq m 0)  
(setq l 0)
(setq j 0)
(while (< m n) 
    (setq lname (ssname s m))
    (setq li (entget lname))
    (if  (=(cdr  (assoc '2  li)) "a3")
          (progn     
            (setq pa  (assoc '10 li))   
            (setq p0 ( list (nth 1 pa) (nth 2 pa)))  
            (setq xx (cdr (assoc '41 li))) 
            (setq yy (cdr (assoc '42 li)))
            (setq x (* xx 410))   
            (setq y (* yy 289))
	    (setq pp (polar p0 0 (/ x 2)))
	    (setq p1 (polar pp (/ pi 2) (/ y 2)))	  
            (setq ppp (polar p0 pi (/ x 2)))
            (setq p2 (polar ppp (* pi 1.5) (/ y 2) ))             
            (command "-plot" "yes" "" "\\\\192.168.0.4\\Canon LBP2900" "a3" "m" "l" "yes" "w" p2 p1 "" "" "yes" "acad.ctb" "yes" "no" "no" "yes" "yes" )
            (setq l (+ l 1))
             
          )
       )
     
      (if  (=(cdr  (assoc '2  li)) "a4")
          (progn     
            (setq pa  (assoc '10 li))   
            (setq p0 ( list (nth 1 pa) (nth 2 pa)))  
            (setq xx (cdr (assoc '41 li))) 
            (setq yy (cdr (assoc '42 li)))
            (setq x (* xx 200))   
            (setq y (* yy 289))  
            (setq pp (polar p0 0 (/ x 2)))
	    (setq p1 (polar pp (/ pi 2) (/ y 2)))	  
            (setq ppp (polar p0 pi (/ x 2)))
            (setq p2 (polar ppp (* pi 1.5) (/ y 2) ))    
            (command "-plot" "yes" "" "\\\\192.168.0.4\\Canon LBP2900" "a4" "m" "p" "yes" "w" p2 p1 "" "" "yes" "acad.ctb" "yes" "no" "no" "yes" "yes" )
            (setq j (+ j 1))
           
          )
       )
     
    
      
    (setq m (+ m 1 ))
)
(setvar "osmode" kk)
(setvar "cmdecho" a)
(prompt "\��ӡ����֣�Ѷ�������ȡͼ!!!")
(princ)
)
