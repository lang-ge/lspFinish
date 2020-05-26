
;;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;������ӡ��ʼ


;;;--------------------------------------------------------------------------
;;;   Batchplot.LSP
;;;    ��Ȩ(C) 1999-2003  ���
;;;
;;; ģ�Ϳռ��������ӡ���� �ڶ���
;;; ���, 2003��11��1��
;;; ���¸��£�2005��4��8��10ʱ2��

;;;   ��������ѿɹ������κ���;����Ŀ������޸ļ�����, ������ѭ����ԭ��:
;;;
;;;   1)  ���еİ�Ȩͨ����������ÿһ�ݿ�����.
;;;   2)  ��ص�˵���ĵ�Ҳ�������а�Ȩͨ�漰�������ͨ��.

;;;   ��������ṩ��ΪӦ���ϵĲο�, ��δ�����������κα�֤; �����κ�����
;;;   ��;֮��Ӧ��, �Լ���ҵ���������������ı�֤, �ڴ�һ�����Է���.

(defun BATCHPLOT (/ acaddoc plot clayout catch BlockList LayerList pathName default_dclvalues dclvalues
					intResult PrinterList plotstyles cpagesetups CfgCellText G:LASTFOLDER
                    ConfigRead ConfigWrite ConfigGetKey ConfigPutKey getalldocs
                    GetLayoutList GetBlockList GetLayerList AddBS SetValue GetValue
                    carList FindNum ChangePageSetup DisableCtrls EnableCtrls SetCtrlStatus
                    RefreshPlotSettings
                    c:Ltools/BatchPlot#OnInitialize
                    c:Ltools/BatchPlot/TextButton1#OnClicked
                    c:Ltools/BatchPlot/TextButton2#OnClicked
                    c:Ltools/BatchPlot/TextButton3#OnClicked
                    c:Ltools/BatchPlot/TextButton4#OnClicked
                    c:Ltools/BatchPlot/TextButton5#OnClicked
					c:Ltools/BatchPlot/TextButton6#OnClicked
                    c:Ltools/BatchPlot/TextButton7#OnClicked
                    c:Ltools/BatchPlot/TextButton8#OnClicked
                    c:Ltools/BatchPlot/TextButton9#OnClicked
                    c:Ltools/BatchPlot/TextButton10#OnClicked
                    c:Ltools/BatchPlot/TextButton11#OnClicked
                    c:Ltools/BatchPlot/TextButton12#OnClicked
                    c:Ltools/BatchPlot/TextButton13#OnClicked
                    c:Ltools/BatchPlot/TextButton14#OnClicked
                    c:Ltools/BatchPlot/TextButton15#OnClicked
					c:Ltools/BatchPlot/TextButton16#OnClicked
					c:Ltools/BatchPlot/TextButton17#OnClicked
                    c:Ltools/BatchPlot/OptionList1#OnSelChanged
                    c:Ltools/BatchPlot/OptionList2#OnSelChanged
                    c:Ltools/BatchPlot/OptionList3#OnSelChanged
                    c:Ltools/BatchPlot/OptionList4#OnSelChanged
                    c:Ltools/BatchPlot/OptionList5#OnSelChanged
                    c:Ltools/BatchPlot/TextBox1#OnUpdate
                    c:Ltools/BatchPlot/TextBox2#OnUpdate
                    c:Ltools/BatchPlot/TextBox3#OnUpdate
					c:Ltools/BatchPlot/TextBox3#OnEditChanged
                    c:Ltools/BatchPlot/TextBox4#OnUpdate
                    c:Ltools/BatchPlot/TextBox5#OnUpdate
                    c:Ltools/BatchPlot/TextBox6#OnUpdate
                    c:Ltools/BatchPlot/TextBox7#OnUpdate
                    c:Ltools/BatchPlot/TextBox8#OnUpdate
					c:Ltools/BatchPlot/TextBox9#OnUpdate
                    c:Ltools/BatchPlot/OptionButton1#OnClicked
                    c:Ltools/BatchPlot/OptionButton2#OnClicked
                    c:Ltools/BatchPlot/CheckBox1#OnClicked
                    c:Ltools/BatchPlot/CheckBox2#OnClicked
                    c:Ltools/BatchPlot/CheckBox3#OnClicked
                    c:Ltools/BatchPlot/CheckBox4#OnClicked
                    c:Ltools/BatchPlot/CheckBox5#OnClicked
                    c:Ltools/BatchPlot/CheckBox6#OnClicked
                    c:Ltools/BatchPlot/CheckBox7#OnClicked
                    c:Ltools/BatchPlot/CheckBox8#OnClicked
					c:Ltools/BatchPlot/CheckBox9#OnClicked
					c:Ltools/BatchPlot/CheckBox10#OnClicked
					c:Ltools/BatchPlot/CheckBox11#OnClicked
                    c:Ltools/BatchPlot/ComboBox1#OnSelChanged
                    c:Ltools/BatchPlot/ComboBox2#OnSelChanged
                    c:Ltools/BatchPlot/ComboBox3#OnSelChanged
                    c:Ltools/BatchPlot/ComboBox4#OnSelChanged
					c:Ltools/BatchPlot/SpinButton1#OnChanged
					c:Ltools/BatchPlot/ListBox1#OnDblClicked

                    c:Ltools/PlotConfig#OnInitialize
                    c:Ltools/PlotConfig/TextButton1#OnClicked

					c:Ltools/PlotConfig/Grid31#OnBeginLabelEdit
                    c:Ltools/PlotConfig/Grid31#OnEndLabelEdit
                    c:Ltools/PlotConfig/Grid31#OnDblClicked
                    c:Ltools/PlotConfig/TextButton31#OnClicked
                    c:Ltools/PlotConfig/TextButton32#OnClicked
                    c:Ltools/PlotConfig/TextButton33#OnClicked
                    c:Ltools/PlotConfig/TextButton34#OnClicked

                    c:Ltools/PlotConfig/Grid21#OnBeginLabelEdit
                    c:Ltools/PlotConfig/Grid21#OnDblClicked
                    c:Ltools/PlotConfig/TextButton21#OnClicked
                    c:Ltools/PlotConfig/TextButton22#OnClicked
                    c:Ltools/PlotConfig/TextButton23#OnClicked
                    c:Ltools/PlotConfig/TextButton24#OnClicked



                    IsRectang BoxFilter
                    GetBoundingBox refgeom mxv trp mxm 2DPoint GetWidthHeight
                    islandscape fixscale CopyLayout GetFileList GetNewAutoNumName
                    doBatchLayout doBatchPlot
                    PlotLayoutsInTabOrder doPlotLayoutsInTabOrder layout-tab-list
                    doPlotLayouts doBatchWblock FormatName
                    BlockFilter SelectBlock SelectLayer GetBlockAtt GetTextNum expandbounding
                    GetFrames HighLightShow OrderFrames doPlots
					AutoLoadODclArx Load-odcl-project
		 )

;;��ȡINI�ļ�
(defun ConfigRead (pathName flag / fsINI sLine lstINI lstKey lstClass sKey sClass)
    (if (setq fsINI (open pathName "r"))
        (progn
        (while (setq sLine (read-line fsINI))
            (setq sLine (LM:str-trimblank sLine))
            (cond
                ((wcmatch sLine "`[*`]")    ;Ϊ����
                    (if sClass  ;��������
                        (progn
                        (setq lstClass (list sClass (reverse lstKey)))
                        (setq lstINI (cons lstClass lstINI))
                        (setq lstKey nil)
                        )
                    )
                    (setq sClass (vl-string-left-trim "[" (vl-string-right-trim "]" sLine)))    ;����������
                )
                ((wcmatch sLine "*`=*") ;Ϊ�¼�
                    (setq sKey (LM:str->lst sLine "="))
                    (if flag
                        (setq lstKey (cons (list (LM:str-trimblank (car sKey)) (LM:str-trimblank (cadr sKey))) lstKey) )	;�����¼�ֵ
                        (setq lstKey (cons (cons (read (LM:str-trimblank (car sKey))) (LM:str-trimblank (cadr sKey))) lstKey) )	;�����¼�ֵ
                    )
                )
                ((and flag (or (wcmatch sLine ";*") (= sLine "")))	;ע���кͿ���
                    (if sClass
                        (setq lstKey (cons (list nil nil sLine) lstKey))
                        (setq lstINI (cons (list nil nil sLine) lstINI))
                    )
                )
            )
        )
        ;;����ǰ�������Ϣ����
        (if sClass	;��������
            (progn
            (setq lstClass (list sClass (reverse lstKey)))
            (setq lstINI (cons lstClass lstINI))
            (setq lstKey nil)
            )
        )
        )
    )
    (close fsINI)
    (reverse lstINI)
)

;;�������б�洢���ļ�
(defun ConfigWrite (lstINI pathName / lstLine lstKey sKey sValue sComment sClass fsINI)
    (if (and lstINI (setq fsINI (open pathName "W")))
        (progn
        (while lstINI
            (setq lstLine (car lstINI) lstINI (cdr lstINI))
            (cond
                ((equal (type lstLine) 'LIST)
                    (if (car lstLine) (setq sClass (strcat "[" (car lstLine) "]"))  (setq sClass ""))
                    (if (caddr lstLine) (setq sComment (caddr lstLine)) (setq sComment ""))
                    (setq lstLine (cadr lstLine))
                    (write-line (strcat sClass sComment) fsINI)
                    (while lstLine
                        (setq lstKey (car lstLine) lstLine (cdr lstLIne))
                        (if (car lstKey) (setq sKey (car lstKey)) (setq sKey ""))
                        (if (cadr lstKey) (setq sValue (strcat " = " (cadr lstKey))) (setq sValue ""))
                        (if (caddr lstKey) (setq sComment (caddr lstKey)) (setq sComment ""))
                        (write-line (strcat sKey sValue sComment) fsINI)
                    )
                )
                (T
                    (if (null lstLine)
                        (write-line "" fsINI)
                        (write-line lstLine fsINI)
                    )
                )
            )
        )
        (close fsINI)
        )
    )
)


(defun getalldocs (/ docs)
  (vlax-map-collection (vla-get-Documents (vlax-get-acad-object))
    (function
      (lambda ( doc )
        (setq docs
          (cons (vla-get-fullname doc) docs
          )
        )
      )
    )
  )
  (reverse docs)
)

;;��ȡ ��ֵ
(defun ConfigGetKey (lstINI sClass sKey / )
    (cadr (assoc sKey (cadr (assoc sClass lstINI))))
)

;;���� ��ֵ
(defun ConfigPutKey (lstINI sClass sKey sValue / lstClass lstKeys lstKey)
    (if (setq lstClass (assoc (strcat sClass) lstINI))
        (if (setq lstKey (assoc (strcat sKey) (cadr lstClass)))
            (progn
            (setq lstKey (list sKey sValue))
            (setq lstKeys (cadr lstClass))
            (setq lstKeys (subst lstKey (assoc (strcat sKey) lstKeys) lstKeys))
            (setq lstClass (list sClass lstKeys (caddr lstClass)))
            (setq lstINI (subst lstClass (assoc (strcat sClass) lstINI) lstINI))
            )
        )
    )
    lstINI
)

;;�����б�
(defun GetLayoutList ( / layouts i lst)
    (setq layouts (vla-get-layouts acaddoc) i -1)
    (repeat (vla-get-count layouts)
        (setq lst (cons (setq i (1+ i)) lst))
    )
    (vlax-for lay layouts
        (setq lst (subst (vla-get-name lay) (vla-get-taborder lay) lst))
    )
    (reverse lst)
)

;;����ͼ���б�
(defun GetBlockList (/ blklst blk)
    (setq blklst nil)
    (if	(tblnext "BLOCK" T)
        (progn
        (setq blklst (cons (cdr (assoc 2 (tblnext "BLOCK" T))) blklst))
        (while (setq blk (tblNext "BLOCK"))
            (setq blklst (cons (cdr (assoc 2 blk)) blklst))
        )
        )
    )
    (vl-remove-if '(lambda (x) (= "*" (substr x 1 1))) blklst)
)

;;ͼ���б�
(defun GetLayerList (/ laylst lay)
    (setq laylst nil)
    (if	(tblnext "LAYER" T)
        (progn (setq laylst (cons (cdr (assoc 2 (tblnext "LAYER" T))) laylst))
        (while (setq lay (tblNext "LAYER"))
            (setq laylst (cons (cdr (assoc 2 lay)) laylst))
        )
        )
    )
    (vl-sort laylst '<)
)

;;Ŀ¼����油�䷴б��
(defun AddBS (cPath)
    (if	(not (= (substr cPath (strlen cPath)) "\\"))
        (strcat cPath "\\")
        cPath
    )
)

;;���ñ���ֵ
(defun SetValue (key value / )
    (if	(assoc key DCLValues)
        (setq DCLValues (subst (cons key value) (assoc key DCLValues) DCLValues))
        (setq DCLValues (cons (cons key value) DCLValues))
    )
)

;;��ȡ����ֵ
(defun GetValue (key / )
    (if	(assoc key DCLValues)
        (cdr (assoc key DCLValues))
        (cdr (assoc key default_dclvalues))
    )
)

;;��Ƕ�ױ��е����ݽ��з���
(defun carList (lst / sublst)
    (foreach x lst
        (setq sublst (cons (nth 0 x) sublst))
    )
    (reverse sublst)
)

;;��һ�������б����ҵ����ڵ��ڸ���ֵ�������ֵ
(defun FindNum (e lst / len i flg item)
    (setq
        lst (vl-sort lst '<)
        len (length lst)
        i 0
        flg nil
    )
    (while (and (< i len) (not flg))
        (setq item (nth i lst))
        (if (<= e item)
            (setq flg T)
            (setq i (1+ i))
        )
    )
    (if flg item nil)
)

;;�����Զ���ҳ���ӡ����
(defun ChangePageSetup (lst / PrinterName PaperSize PlotStyleTable PaperList LocalPaperList i)
    (setq PrinterName (nth 1 lst)
        PaperSize (nth 2 lst)
        PlotStyleTable (nth 3 lst)
    )
    (if (member PrinterName PrinterList)
        (progn
        (if (/= PrinterName (vla-get-configname clayout))
            (vla-put-configname clayout PrinterName)    ;���ô�ӡ��
        )
        ;;(vla-RefreshPlotDeviceInfo clayout)
        (setq PaperList (vlax-safearray->list (vlax-variant-value (vla-GetCanonicalMediaNames clayout)))
            LocalPaperList (mapcar (function (lambda (x) (vla-GetLocaleMediaName clayout x))) PaperList)
        )
        (if (and (member PaperSize LocalPaperList) (member PlotStyleTable plotstyles))
            (progn
            (if (/= PlotStyleTable (vla-get-stylesheet clayout))
                (vla-put-stylesheet clayout PlotStyleTable) ;���ô�ӡ��ʽ
            )
            (setq i (vl-position (strcase PaperSize) (mapcar (function strcase) LocalPaperList)))
            (vla-put-CanonicalMediaName clayout (nth i PaperList))  ;���ô�ӡֽ��
            ;;(vla-put-paperunits clayout acMillimeters)
            (if (= (vla-get-paperunits clayout) acInches) (vla-put-paperunits clayout acMillimeters))
            )
            (progn
            (if (not (member PaperSize LocalPaperList))
                (princ (strcat "\n�����ֽ�š�" PaperSize "������ȷ��\n"))
            )
            (if (not (member PlotStyleTable plotstyles))
                (princ (strcat "\n����Ĵ�ӡ��ʽ����ļ�����" PlotStyleTable "������ȷ��\n"))
            )
            )
        )
        )
        (princ (strcat "\n����Ĵ�ӡ�����ơ�" PrinterName "������ȷ��\n"))
    )
    (princ)
)

;;���Կؼ�
(defun DisableCtrls (keylist / str)
    (foreach key keylist
        (setq str (strcat "(dcl-Control-SetEnabled Ltools/BatchPlot/" key " nil)"))
        (eval (read str))
    )
)

;;����ؼ�
(defun EnableCtrls (keylist / str)
    (foreach key keylist
        (setq str (strcat "(dcl-Control-SetEnabled Ltools/BatchPlot/" key " T)"))
        (eval (read str))
    )
)

;;���ÿؼ�״̬
(defun SetCtrlStatus ()
    (EnableCtrls
        '("OptionList1" "OptionList2" "OptionList4" "OptionList5"
        "TextButton1" "TextButton2"	"TextButton3" "TextButton4"	"TextButton5"	"TextButton6"
        "TextButton7" "TextButton8"	"TextButton9" "TextButton10" "TextButton11" "TextButton12"
        "TextButton13" "TextButton14" "TextButton15"
        "TextBox1" "TextBox2" "TextBox3" "TextBox4" "TextBox5" "TextBox6" "TextBox7" "TextBox8" "TextBox9"
        "ComboBox1" "ComboBox2" "ComboBox3" "ComboBox4"
        "CheckBox1" "CheckBox2" "CheckBox3" "CheckBox4" "CheckBox5" "CheckBox6" "CheckBox7" "CheckBox8" "CheckBox9" "CheckBox11"
        "OptionButton1" "OptionButton2"
		"SpinButton1")
    )
    (if	(null (getvalue 'SelectedFrames))
        (DisableCtrls '("TextButton5" "TextButton9" "TextButton10"))
    )
    (cond
        ((= 0 (dcl-Control-GetCurrentSelection Ltools/BatchPlot/OptionList1))
            (dcl-Control-SetValue Ltools/BatchPlot/CheckBox5 0)
            (DisableCtrls
            '("TextButton1" "TextButton2" "TextButton3" "TextBox1" "TextBox2" "CheckBox5" "CheckBox7")
            )
        )
        ((= 1 (dcl-Control-GetCurrentSelection Ltools/BatchPlot/OptionList1))
            (DisableCtrls '("TextButton3" "TextBox2"))
        )
        ((= 2 (dcl-Control-GetCurrentSelection Ltools/BatchPlot/OptionList1))
            (dcl-Control-SetValue Ltools/BatchPlot/CheckBox5 0)
            (DisableCtrls '("TextButton2" "TextBox1"  "CheckBox5"))
        )
    )
    (cond
        ((= 0 (dcl-Control-GetCurrentSelection Ltools/BatchPlot/OptionList2))
            (dcl-Control-SetValue Ltools/BatchPlot/CheckBox8 0)
            (dcl-Control-SetValue Ltools/BatchPlot/CheckBox9 0)
            (DisableCtrls
            '("TextBox7" "TextBox8" "TextBox9" "TextButton8" "CheckBox6" "CheckBox8" "CheckBox9" "TextButton14" "TextButton15")
            )
            (EnableCtrls  '("TextBox3" "SpinButton1"))
        )
        ((= 1 (dcl-Control-GetCurrentSelection Ltools/BatchPlot/OptionList2))
            (dcl-Control-SetValue Ltools/BatchPlot/CheckBox8 0)
            (dcl-Control-SetValue Ltools/BatchPlot/CheckBox9 0)
            (DisableCtrls
            '("TextBox3" "OptionButton1" "OptionButton2" "TextBox5" "TextBox6" "TextBox8" "TextBox9" "TextButton7" "TextButton8" "CheckBox1" "CheckBox8" "CheckBox9" "SpinButton1")
            )
        )
        ((= 2 (dcl-Control-GetCurrentSelection Ltools/BatchPlot/OptionList2))
            (DisableCtrls '("TextBox3" "CheckBox6" "CheckBox8" "SpinButton1"))
        )
        ((= 3 (dcl-Control-GetCurrentSelection Ltools/BatchPlot/OptionList2))
            (dcl-Control-SetValue Ltools/BatchPlot/CheckBox8 0)
            (dcl-Control-SetValue Ltools/BatchPlot/CheckBox9 0)
            (DisableCtrls
            '("OptionList1" "OptionList4" "OptionList5"
            "TextButton1"	"TextButton2"	"TextButton3"	"TextButton4"	"TextButton5"
            "TextButton8"	"TextButton9" "TextButton12" "TextButton13" "TextButton14" "TextButton15"
            "TextBox1"	"TextBox2"	"TextBox3"	"TextBox4"	"TextBox5"	"TextBox6"	"TextBox7"	"TextBox8" "TextBox9"
            "ComboBox1" "ComboBox2" "ComboBox3" "ComboBox4"
            "CheckBox2" "CheckBox3" "CheckBox4" "CheckBox5" "CheckBox6" "CheckBox7" "CheckBox8" "CheckBox9"
            "OptionButton1" "OptionButton2" "SpinButton1")
            )
            (EnableCtrls '("TextButton10"))
        )
        ((= 4 (dcl-Control-GetCurrentSelection Ltools/BatchPlot/OptionList2))
            (dcl-Control-SetValue Ltools/BatchPlot/CheckBox8 0)
            (dcl-Control-SetValue Ltools/BatchPlot/CheckBox9 0)
            (DisableCtrls
            '("OptionList4"
            "TextButton5" "TextButton7" "TextButton9" "TextButton12" "TextButton13"
            "TextBox3"	"TextBox4"	"TextBox5"	"TextBox6" "TextBox9"
            "ComboBox1" "ComboBox2" "ComboBox3" "ComboBox4"
            "CheckBox1" "CheckBox3" "CheckBox4" "CheckBox5" "CheckBox6" "CheckBox8" "CheckBox9"
            "OptionButton1" "OptionButton2" "SpinButton1")
            )
        )
    )
    (if	(/= 2 (dcl-Control-GetCurrentSelection Ltools/BatchPlot/OptionList4))
        (DisableCtrls '("TextBox4"))
    )
    (if	(= 0 (dcl-Control-GetValue Ltools/BatchPlot/CheckBox1))
        (DisableCtrls '("TextButton7"))
    )
    (if	(= 1 (dcl-Control-GetValue Ltools/BatchPlot/CheckBox5))
        (DisableCtrls '("ComboBox3"))
    )

    (if	(= 1 (dcl-Control-GetValue Ltools/BatchPlot/CheckBox9))
        (progn
        (DisableCtrls '("TextBox3" "TextBox9" "CheckBox5" "CheckBox6" "ComboBox1" "ComboBox2" "ComboBox3" "TextButton12" "SpinButton1"))
        (dcl-Control-SetVisible Ltools/BatchPlot/OptionList3 T)
        (EnableCtrls  '("CheckBox8"))
        )
        (dcl-Control-SetVisible Ltools/BatchPlot/OptionList3 nil)
    )

    (if	(= 1 (dcl-Control-GetValue Ltools/BatchPlot/CheckBox8))
        (EnableCtrls '("TextButton10"))
    )

    (if	(= 0 (dcl-Control-GetValue Ltools/BatchPlot/OptionButton2))
        (DisableCtrls '("TextBox5" "TextBox6"))
    )
    (if (= "��" (vla-get-configname clayout))
        (DisableCtrls '("TextButton12"))
        (if (not (vl-file-systime (strcat (getenv "PrinterConfigDir") "\\" (vla-get-configname clayout))))
            (DisableCtrls '("TextButton12"))
        )
    )
    (if (= 0 (dcl-ComboBox-GetCurSel Ltools/BatchPlot/ComboBox4))
        (DisableCtrls '("TextButton13"))
    )
)

;;ˢ�´�ӡ����״̬
(defun RefreshPlotSettings ( / printer PrinterIndex papers paper PaperIndex plotstyle plotstyleIndex xPath)
    ;;(vla-RefreshPlotDeviceInfo clayout)
    ;;���ô�ӡ���б�
    (setq PrinterList (dcl-Control-GetList Ltools/BatchPlot/ComboBox2))
    (if (and (setq printer (vla-get-configname clayout)) (/= "��" printer) (setq PrinterIndex (vl-position  printer PrinterList)))
        (progn
        (dcl-ComboBox-SetCurSel Ltools/BatchPlot/ComboBox2 PrinterIndex)
		(cond
			((wcmatch (strcase printer) "*PDF*")
				(dcl-Control-SetText Ltools/BatchPlot/TextBox9 "pdf")
				(setvalue 'NameExt "pdf")
			)
			((wcmatch (strcase printer) "*PNG*")
				(dcl-Control-SetText Ltools/BatchPlot/TextBox9 "png")
				(setvalue 'NameExt "png")
			)
			((wcmatch (strcase printer) "*JPG*")
				(dcl-Control-SetText Ltools/BatchPlot/TextBox9 "jpg")
				(setvalue 'NameExt "jpg")
			)
			(T
				(dcl-Control-SetText Ltools/BatchPlot/TextBox9 "plt")
				(setvalue 'NameExt "plt")
			)
		)
        (setq xPath (strcat (getenv "PrinterConfigDir") "\\" printer))
        (if (vl-file-systime xPath)
            (EnableCtrls '("TextButton12"))
            (DisableCtrls '("TextButton12"))
        )
        )
        (progn
        (dcl-ComboBox-SetCurSel Ltools/BatchPlot/ComboBox2 0)
        (DisableCtrls '("TextButton12"))
        )
    )
    ;;���ô�ӡֽ���б�
    (setq papers (mapcar (function (lambda (x) (vla-GetLocaleMediaName clayout x))) (vlax-safearray->list (vlax-variant-value (vla-GetCanonicalMediaNames clayout)))))
    (dcl-ComboBox-Clear Ltools/BatchPlot/ComboBox3)
    (dcl-ComboBox-AddList Ltools/BatchPlot/ComboBox3 papers)
    (if (and (setq paper (vla-GetLocaleMediaName clayout (vla-get-CanonicalMediaName clayout))) (setq PaperIndex (vl-position paper papers)))
        (dcl-ComboBox-SetCurSel Ltools/BatchPlot/ComboBox3 PaperIndex)
    )
    ;;���ô�ӡ��ʽ�б�
    (setq plotstyles (dcl-Control-GetList Ltools/BatchPlot/ComboBox4))
    (if (and (setq plotstyle (vla-get-stylesheet clayout)) (setq plotstyleIndex (vl-position plotstyle plotstyles)))
        (progn
        (dcl-ComboBox-SetCurSel Ltools/BatchPlot/ComboBox4 plotstyleIndex)
        (EnableCtrls '("TextButton13"))
        )
        (progn
        (dcl-ComboBox-SetCurSel Ltools/BatchPlot/ComboBox4 0)
        (DisableCtrls '("TextButton13"))
        )
    )
)

;;������ӡ�Ի����ʼ��
(defun c:Ltools/BatchPlot#OnInitialize (/)
    ;;��ʼ���ؼ�
    (setq cpagesetups (mapcar (function (lambda (x) (LM:str->lst x ","))) (LM:str->lst (getvalue 'PageSetup) "|")))
    (dcl-ComboBox-Clear Ltools/BatchPlot/ComboBox1)
    (dcl-ComboBox-AddList Ltools/BatchPlot/ComboBox1 (carList cpagesetups))
    (RefreshPlotSettings)
    (if (and (getvalue 'BlockName) (mapcar (function (lambda (x) (and x (vl-position x Blocklist)))) (LM:str->lst (getvalue 'BlockName) ",")))
        (dcl-Control-SetText Ltools/BatchPlot/TextBox1 (getvalue 'BlockName))
    )
    (if (and (getvalue 'LayerName) (mapcar (function (lambda (x) (and x (vl-position x Layerlist)))) (LM:str->lst (getvalue 'LayerName) ",")))
        (dcl-Control-SetText Ltools/BatchPlot/TextBox2 (getvalue 'LayerName))
    )
    (dcl-Control-SetText Ltools/BatchPlot/TextBox3 (getvalue 'Copies))
	(dcl-Control-SetValue Ltools/BatchPlot/SpinButton1 (atoi (getvalue 'Copies)))
    (dcl-OptionList-SetCurSel Ltools/BatchPlot/OptionList1 (atoi (getvalue 'Frame)))
    (dcl-OptionList-SetCurSel Ltools/BatchPlot/OptionList2 (atoi (getvalue 'Output)))
    (if (= (getvalue 'PlotScale) "Auto")
        (dcl-OptionList-SetCurSel Ltools/BatchPlot/OptionList4 0)
        (if (= (getvalue 'PlotScale) "ScaleToFit")
            (dcl-OptionList-SetCurSel Ltools/BatchPlot/OptionList4 1)
            (progn
            (dcl-OptionList-SetCurSel Ltools/BatchPlot/OptionList4 2)
            (dcl-Control-SetText Ltools/BatchPlot/TextBox4 (getvalue 'PlotScale))
            )
        )
    )
    (dcl-OptionList-SetCurSel Ltools/BatchPlot/OptionList5 (atoi (getvalue 'PlotOrder)))
    (dcl-OptionList-SetCurSel Ltools/BatchPlot/OptionList3 (atoi (getvalue 'LongPaperSize)))
    (if (= (getvalue 'Offset) "Center")
        (progn
		(dcl-Control-SetValue Ltools/BatchPlot/OptionButton1 1)
		(dcl-Control-SetValue Ltools/BatchPlot/OptionButton2 0)
		)
        (progn
		(dcl-Control-SetValue Ltools/BatchPlot/OptionButton1 0)
        (dcl-Control-SetValue Ltools/BatchPlot/OptionButton2 1)
        (dcl-Control-SetText Ltools/BatchPlot/TextBox5 (car (getvalue 'Offset)))
        (dcl-Control-SetText Ltools/BatchPlot/TextBox6 (cadr (getvalue 'Offset)))
        )
    )
    (if (= 1 (atoi (getvalue 'PlotStamp)))
        (progn
        (dcl-Control-SetVisible Ltools/BatchPlot/TextButton7 T)
        (dcl-Control-SetValue Ltools/BatchPlot/CheckBox1 1)
        )
        (progn
        (dcl-Control-SetVisible Ltools/BatchPlot/TextButton7 nil)
        (dcl-Control-SetValue Ltools/BatchPlot/CheckBox1 0)
        )
    )
    (dcl-Control-SetValue Ltools/BatchPlot/CheckBox2 (getvalue 'ReverseOrder))
    (dcl-Control-SetValue Ltools/BatchPlot/CheckBox3 (getvalue 'AutoRotate))
    (dcl-Control-SetValue Ltools/BatchPlot/CheckBox4 (getvalue 'UpsideDown))
    (dcl-Control-SetValue Ltools/BatchPlot/CheckBox5 (getvalue 'AutoPaper))
    (dcl-Control-SetValue Ltools/BatchPlot/CheckBox6 (getvalue 'MSLineScale))
    (dcl-Control-SetValue Ltools/BatchPlot/CheckBox7 (getvalue 'AutoSelect))
    (dcl-Control-SetValue Ltools/BatchPlot/CheckBox8 (getvalue 'PlotLayouts))
    (dcl-Control-SetValue Ltools/BatchPlot/CheckBox9 (getvalue 'LongPaper))
	(dcl-Control-SetValue Ltools/BatchPlot/CheckBox11 (getvalue 'DocPlotStyle))
	(dcl-TextBox-SetFilter Ltools/BatchPlot/TextBox4 "0123456789.")
	(dcl-TextBox-SetFilter Ltools/BatchPlot/TextBox5 "0123456789.")
	(dcl-TextBox-SetFilter Ltools/BatchPlot/TextBox6 "0123456789.")
    (dcl-Control-SetText Ltools/BatchPlot/TextBox7 (getvalue 'NamePrefix))
    (dcl-Control-SetText Ltools/BatchPlot/TextBox8 (getvalue 'PlotFileFolder))
	(dcl-Control-SetText Ltools/BatchPlot/TextBox9 (getvalue 'NameExt))
    (dcl-Control-SetCaption Ltools/BatchPlot/Label1 (strcat	"ѡ��ͼֽ:" (itoa (length (getvalue 'SelectedFrames)))))
    (SetCtrlStatus)
)

;;TextButtonEvents
(defun c:Ltools/BatchPlot/TextButton1#OnClicked (/)
    (dcl-form-close Ltools/BatchPlot 11)
)
(defun c:Ltools/BatchPlot/TextButton2#OnClicked (/ sellst)
    (if (setq sellst (Odcl:MultList "��ѡ��ͼ����" "ͼ�����б��ɶ�ѡ��" Blocklist))
        (progn
        (dcl-Control-SetText Ltools/BatchPlot/TextBox1 (LM:lst->str sellst ","))
        (setvalue 'BlockName (LM:lst->str sellst ","))
        )
    )
)
(defun c:Ltools/BatchPlot/TextButton3#OnClicked (/ sellst)
    (if (setq sellst (Odcl:MultList "��ѡ��ͼ����" "ͼ�����б��ɶ�ѡ��" Layerlist))
        (progn
        (dcl-Control-SetText Ltools/BatchPlot/TextBox2 (LM:lst->str sellst ","))
        (setvalue 'LayerName (LM:lst->str sellst ","))
        )
    )
)
(defun c:Ltools/BatchPlot/TextButton4#OnClicked (/)
    (dcl-form-close Ltools/BatchPlot 14)
)
(defun c:Ltools/BatchPlot/TextButton5#OnClicked (/)
    (dcl-form-close Ltools/BatchPlot 15)
)
(defun c:Ltools/BatchPlot/TextButton6#OnClicked (/)
	(dcl-form-show Ltools/PlotConfig)
)
(defun c:Ltools/BatchPlot/TextButton7#OnClicked (/)
    (dcl-form-close Ltools/BatchPlot 17)
)
(defun c:Ltools/BatchPlot/TextButton8#OnClicked (/ path)
    (if (setq path (dcl-selectfolder "ѡ��PLT�ļ�����λ��" nil nil 81))
        (progn
        (setq path (AddBS path))
        (dcl-Control-SetText Ltools/BatchPlot/TextBox8 path)
        (setvalue 'PlotFileFolder path)
        )
    )
)
(defun c:Ltools/BatchPlot/TextButton9#OnClicked (/)
    (dcl-form-close Ltools/BatchPlot 19)
)
(defun c:Ltools/BatchPlot/TextButton10#OnClicked (/)
    (dcl-form-close Ltools/BatchPlot 1)
)
(defun c:Ltools/BatchPlot/TextButton11#OnClicked (/)
    (dcl-form-close Ltools/BatchPlot 2)
)
(defun c:Ltools/BatchPlot/TextButton12#OnClicked (/ ItemData xPath)
    (setq ItemData (dcl-Control-GetText Ltools/BatchPlot/ComboBox2))
    (setq xPath (strcat (getenv "PrinterConfigDir") "\\" ItemData))
    (if (vl-file-systime xPath)
        (startapp (strcat "pc3exe " xPath))
    )
)
(defun c:Ltools/BatchPlot/TextButton13#OnClicked (/ ItemData xPath)
    (setq ItemData (dcl-Control-GetText Ltools/BatchPlot/ComboBox4))
    (if (/= "��" ItemData)
        (progn
        (setq xPath (strcat (getenv "PrinterStyleSheetDir") "\\" ItemData))
        (startapp (strcat "styexe " xPath))
        )
    )
)
(defun c:Ltools/BatchPlot/TextButton14#OnClicked (/)
    (dcl-form-close Ltools/BatchPlot 114)
)
(defun c:Ltools/BatchPlot/TextButton15#OnClicked (/)
    (dcl-form-close Ltools/BatchPlot 115)
)


;;OptionListEvents
(defun c:Ltools/BatchPlot/OptionList1#OnSelChanged (ItemIndexOrCount Value /)
    (SetCtrlStatus)
    (setvalue 'Frame (itoa ItemIndexOrCount))
    (if (/= 1 ItemIndexOrCount)
		(setvalue 'AutoPaper "0")
    )
)
(defun c:Ltools/BatchPlot/OptionList2#OnSelChanged (ItemIndexOrCount Value /)
    (SetCtrlStatus)
    (setvalue 'Output (itoa ItemIndexOrCount))
    (if (/= 2 ItemIndexOrCount)
        (progn
		(setvalue 'LongPaper "0")
		(setvalue 'PlotLayouts "0")
        )
    )
    (if (= 4 ItemIndexOrCount)
        (progn
		(dcl-Control-SetText Ltools/BatchPlot/TextBox9 "dwg")
		(setvalue 'NameExt "dwg")
        )
    )
)
(defun c:Ltools/BatchPlot/OptionList3#OnSelChanged (ItemIndexOrCount Value /)
    (setvalue 'LongPaperSize (itoa ItemIndexOrCount))
)
(defun c:Ltools/BatchPlot/OptionList4#OnSelChanged (ItemIndexOrCount Value /)
    (if (= 0 ItemIndexOrCount)
        (progn
        (SetCtrlStatus)
        (setvalue 'PlotScale "Auto")
        )
    )
    (if (= 1 ItemIndexOrCount)
        (progn
        (SetCtrlStatus)
        (setvalue 'PlotScale "ScaleToFit")
        )
    )
    (if (= 2 ItemIndexOrCount)
        (progn
        (SetCtrlStatus)
        (setvalue 'PlotScale (dcl-Control-GetText Ltools/BatchPlot/TextBox4))
        (dcl-Control-SetFocus Ltools/BatchPlot/TextBox4)
        )
    )
)
(defun c:Ltools/BatchPlot/OptionList5#OnSelChanged (ItemIndexOrCount Value /)
    (setvalue 'PlotOrder (itoa ItemIndexOrCount))
)


;;TextBoxEvents
(defun c:Ltools/BatchPlot/TextBox1#OnUpdate (NewValue /)
    (setvalue 'BlockName NewValue)
)
(defun c:Ltools/BatchPlot/TextBox2#OnUpdate (NewValue /)
    (setvalue 'LayerName NewValue)
)
(defun c:Ltools/BatchPlot/TextBox3#OnUpdate (NewValue /)
	(setvalue 'Copies NewValue)
)
(defun c:Ltools/BatchPlot/TextBox3#OnEditChanged (NewValue /)
	(dcl-Control-SetValue Ltools/BatchPlot/SpinButton1 (atoi NewValue))
	(setvalue 'Copies NewValue)
)

(defun c:Ltools/BatchPlot/TextBox4#OnUpdate (NewValue /)
    (setvalue 'PlotScale NewValue)
)
(defun c:Ltools/BatchPlot/TextBox5#OnUpdate (NewValue /)
    (setvalue 'Offset (list NewValue (dcl-Control-GetText Ltools/BatchPlot/TextBox6)))
)
(defun c:Ltools/BatchPlot/TextBox6#OnUpdate (NewValue /)
    (setvalue 'Offset (list (dcl-Control-GetText Ltools/BatchPlot/TextBox5) NewValue))
)
(defun c:Ltools/BatchPlot/TextBox7#OnUpdate (NewValue /)
    (setvalue 'NamePrefix NewValue)
)
(defun c:Ltools/BatchPlot/TextBox8#OnUpdate (NewValue /)
    (setvalue 'PlotFileFolder NewValue)
)
(defun c:Ltools/BatchPlot/TextBox9#OnUpdate (NewValue /)
    (setvalue 'NameExt NewValue)
)

;;OptionButtonEvents
(defun c:Ltools/BatchPlot/OptionButton1#OnClicked (Value /)
    (SetCtrlStatus)
    (setvalue 'Offset "Center")
)
(defun c:Ltools/BatchPlot/OptionButton2#OnClicked (Value /)
    (SetCtrlStatus)
    (setvalue 'Offset (list (dcl-Control-GetText Ltools/BatchPlot/TextBox5) (dcl-Control-GetText Ltools/BatchPlot/TextBox6)))
    (dcl-Control-SetFocus Ltools/BatchPlot/TextBox5)
)

;;CheckBoxEvents
(defun c:Ltools/BatchPlot/CheckBox1#OnClicked (Value /)
    (setvalue 'PlotStamp (itoa Value))
    (if (= 1 Value)
        (progn
        (dcl-Control-SetVisible Ltools/BatchPlot/TextButton7 T)
        (EnableCtrls '("TextButton7"))
        )
        (progn
        (dcl-Control-SetVisible Ltools/BatchPlot/TextButton7 nil)
        (DisableCtrls '("TextButton7"))
        )
    )
)
(defun c:Ltools/BatchPlot/CheckBox2#OnClicked (Value /)
    (setvalue 'ReverseOrder (itoa Value))
)
(defun c:Ltools/BatchPlot/CheckBox3#OnClicked (Value /)
    (setvalue 'AutoRotate (itoa Value))
)
(defun c:Ltools/BatchPlot/CheckBox4#OnClicked (Value /)
    (setvalue 'UpsideDown (itoa Value))
)
(defun c:Ltools/BatchPlot/CheckBox5#OnClicked (Value /)
    (SetCtrlStatus)
    (setvalue 'AutoPaper (itoa Value))
)
(defun c:Ltools/BatchPlot/CheckBox6#OnClicked (Value /)
    (setvalue 'MSLineScale (itoa Value))
)
(defun c:Ltools/BatchPlot/CheckBox7#OnClicked (Value /)
    (setvalue 'AutoSelect (itoa Value))
)
(defun c:Ltools/BatchPlot/CheckBox8#OnClicked (Value /)
    (setvalue 'PlotLayouts (itoa Value))
    (if (= 1 Value)
    (EnableCtrls '("TextButton10"))
    (disableCtrls '("TextButton10"))
    )
)
(defun c:Ltools/BatchPlot/CheckBox9#OnClicked (Value /)
    (if (= 1 Value)
        (progn
		(dcl-Control-SetText Ltools/BatchPlot/TextBox9 "pdf")
		(setvalue 'NameExt "pdf")
        )
    )
    (SetCtrlStatus)
    (setvalue 'LongPaper (itoa Value))
)
(defun c:Ltools/BatchPlot/CheckBox11#OnClicked (Value /)
    (setvalue 'DocPlotStyle (itoa Value))
)

;;ComboBoxEvents
(defun c:Ltools/BatchPlot/ComboBox1#OnSelChanged (ItemIndexOrCount Value /)
    (ChangePageSetup (nth ItemIndexOrCount cpagesetups))
    (RefreshPlotSettings)
)
(defun c:Ltools/BatchPlot/ComboBox2#OnSelChanged (ItemIndexOrCount Value /)
    (vla-put-configname clayout Value)
    ;;(vla-RefreshPlotDeviceInfo clayout)
    (RefreshPlotSettings)
)
(defun c:Ltools/BatchPlot/ComboBox3#OnSelChanged (ItemIndexOrCount Value /)
    (vla-put-CanonicalMediaName clayout (nth ItemIndexOrCount (vlax-safearray->list (vlax-variant-value (vla-GetCanonicalMediaNames clayout)))))
)
(defun c:Ltools/BatchPlot/ComboBox4#OnSelChanged (ItemIndexOrCount Value /)
    (if (/= Value "��")
        (progn
        (vla-put-stylesheet clayout Value)
        (EnableCtrls '("TextButton13"))
        )
        (progn
        (vla-put-stylesheet clayout "")
        (DisableCtrls '("TextButton13"))
        )
    )
)

(defun c:Ltools/BatchPlot/SpinButton1#OnChanged (Value Delta /)
	(dcl-Control-SetText Ltools/BatchPlot/TextBox3 (itoa Value))
	(setvalue 'Copies (itoa Value))
)

(defun c:Ltools/BatchPlot/cmdExpand#OnClicked (/)
	(if (= (car (dcl-Form-GetControlArea Ltools/BatchPlot)) 630)
		(progn
			(dcl-Form-Resize Ltools/BatchPlot 880 480)
			(dcl-Control-SetPicture Ltools/BatchPlot/cmdExpand 143)
		)
		(progn
			(dcl-Form-Resize Ltools/BatchPlot 630 480)
			(dcl-Control-SetPicture Ltools/BatchPlot/cmdExpand 142)
		)
	)
)

;;���ĵ���ӡ
(defun c:Ltools/BatchPlot/TextButton16#OnClicked (/ bs_filelist fn)
	(setq bs_filelist (dcl-SelectFiles "ͼ�� (*.dwg)|*.dwg||" ; Filters
										   "ѡ���ļ�" ; Title
										   (if g:lastfolder
											   g:lastfolder
											   (getvar "DWGPREFIX")
										   ) ; Default Folder
					  )
	)
	(if bs_filelist
		(progn
			(setq g:lastfolder (vl-filename-directory (car bs_filelist)))
			(foreach fn bs_filelist
				(if (< (dcl-ListBox-Findstringexact Ltools/BatchPlot/ListBox1 fn) 0)
					(dcl-ListBox-Addstring Ltools/BatchPlot/ListBox1 fn)
				)
			)
		)
	)
)
(defun c:Ltools/BatchPlot/TextButton17#OnClicked (/ alldocs file file_list len m lin)
    (dcl-form-close Ltools/BatchPlot 2)
    (setvar "cmdecho" 0)

    (setvalue 'printer (vla-get-configname clayout))
    (setvalue 'printpaper (vla-get-CanonicalMediaName clayout))
    (setvalue 'printstyle (vla-get-stylesheet clayout))
    (vl-bb-set 'bbdcl DCLValues)

    (setq alldocs (getalldocs))
    (setq file (open "c:\\WINDOWS\\temp\\tempscriptfile.txt" "w"))
    (setq file_list (dcl-Control-GetList Ltools/BatchPlot/ListBox1))
    (setq len (length file_list))
    (setq m 0)
    (repeat (- len 1)
    (if (vl-position (nth m file_list) alldocs)
    (setq lin (strcat "_open " "\"" (nth m file_list) "\"" " Y" " BatchPlot" " close" " Y"))
    (setq lin (strcat "_open " "\"" (nth m file_list) "\"" " BatchPlot" " close" " Y"))
    )
    (write-line lin file)
    (setq m (+ m 1))
    )
    (if (vl-position (nth m file_list) alldocs)
    (setq lin (strcat "_open " "\"" (nth m file_list) "\"" " Y" " BatchPlot" " BpReset" " close" " Y"))
    (setq lin (strcat "_open " "\"" (nth m file_list) "\"" " BatchPlot" " BpReset" " close" " Y"))
    )
    (write-line lin file)

    (close file)
    (if (findfile "C:/WINDOWS/temp/tempscriptfile.scr")
    (vl-file-delete "C:/WINDOWS/temp/tempscriptfile.scr")
    )
    (vl-file-rename "C:/WINDOWS/temp/tempscriptfile.txt" "C:/WINDOWS/temp/tempscriptfile.scr")
    (command "script" "C:/WINDOWS/temp/tempscriptfile.scr")

    (setvar "cmdecho" 0)
    (princ)

)
(defun c:Ltools/BatchPlot/ListBox1#OnDblClicked (/)
  (dcl-ListBox-DeleteItem Ltools/BatchPlot/ListBox1 (dcl-ListBox-GetCurSel Ltools/BatchPlot/ListBox1))
)
(defun c:Ltools/BatchPlot/CheckBox10#OnClicked (Value / alldocs)
    (setq alldocs (getalldocs))
    (if (= 1 Value)
			(foreach fn alldocs
				(if (< (dcl-ListBox-FINDSTRINGEXACT Ltools/BatchPlot/ListBox1 fn) 0)
					(dcl-ListBox-ADDSTRING Ltools/BatchPlot/ListBox1 fn)
				)
			)
			(foreach fn alldocs
				(if (dcl-ListBox-FINDSTRINGEXACT Ltools/BatchPlot/ListBox1 fn)
					(dcl-ListBox-DeleteItem Ltools/BatchPlot/ListBox1 (dcl-ListBox-FINDSTRINGEXACT Ltools/BatchPlot/ListBox1 fn))
				)
			)
    )
)

;;��ӡ���öԻ����ʼ��
(defun c:Ltools/PlotConfig#OnInitialize (/ FrameDclvalue OutputDclvalue PlotOrderDclvalue LongPaperSizeDclvalue ColumnNum)

	;;tab1
    (setq FrameDclvalue '("���ã�*_TITLE��" "ͼ�飺ͼ��Ϊ�ض�ͼ��" "ͼ�㣺ָ��ͼ���վ���"))
    (dcl-ComboBox-Clear Ltools/PlotConfig/ComboBox1)
    (dcl-ComboBox-AddList Ltools/PlotConfig/ComboBox1 FrameDclvalue)
    (dcl-ComboBox-SetCurSel Ltools/PlotConfig/ComboBox1 (atoi (getvalue 'Frame)))
    (setq OutputDclvalue '("ֱ��������ӡ" "�������ɲ���" "��ӡ���ļ�" "��ӡ���в���" "ͼֽ�鵵����֣�"))
    (dcl-ComboBox-Clear Ltools/PlotConfig/ComboBox2)
    (dcl-ComboBox-AddList Ltools/PlotConfig/ComboBox2 OutputDclvalue)
    (dcl-ComboBox-SetCurSel Ltools/PlotConfig/ComboBox2 (atoi (getvalue 'Output)))
    (setq PlotOrderDclvalue '("ѡ��˳��" "���ң�����" "���£�����"))
    (dcl-ComboBox-Clear Ltools/PlotConfig/ComboBox3)
    (dcl-ComboBox-AddList Ltools/PlotConfig/ComboBox3 PlotOrderDclvalue)
    (dcl-ComboBox-SetCurSel Ltools/PlotConfig/ComboBox3 (atoi (getvalue 'PlotOrder)))
    (setq LongPaperSizeDclvalue '("A0+" "A1+" "A2+" "A3+" "A4+" "A0��A1��A2+"))
    (dcl-ComboBox-Clear Ltools/PlotConfig/ComboBox4)
    (dcl-ComboBox-AddList Ltools/PlotConfig/ComboBox4 LongPaperSizeDclvalue)
    (dcl-ComboBox-SetCurSel Ltools/PlotConfig/ComboBox4 (atoi (getvalue 'LongPaperSize)))
	(dcl-ComboBox-SelectString  Ltools/PlotConfig/ComboBox5 (getvalue 'Offset))
	(dcl-ComboBox-SelectString  Ltools/PlotConfig/ComboBox6 (getvalue 'PlotScale))

    (dcl-Control-SetText Ltools/PlotConfig/TextBox1 (getvalue 'BlockName))
	(dcl-Control-SetText Ltools/PlotConfig/TextBox2 (getvalue 'LayerName))
	(dcl-Control-SetText Ltools/PlotConfig/TextBox3 (getvalue 'Copies))
	(dcl-Control-SetText Ltools/PlotConfig/TextBox4 (getvalue 'BlockFrameList))
    (dcl-Control-SetValue Ltools/PlotConfig/CheckBox1 (getvalue 'AutoSelect))
    (dcl-Control-SetValue Ltools/PlotConfig/CheckBox2 (getvalue 'AutoPaper))
    (dcl-Control-SetValue Ltools/PlotConfig/CheckBox3 (getvalue 'PlotStamp))
	(dcl-Control-SetValue Ltools/PlotConfig/CheckBox4 (getvalue 'ReverseOrder))
    (dcl-Control-SetValue Ltools/PlotConfig/CheckBox5 (getvalue 'AutoRotate))
    (dcl-Control-SetValue Ltools/PlotConfig/CheckBox6 (getvalue 'UpsideDown))
    (dcl-Control-SetValue Ltools/PlotConfig/CheckBox7 (getvalue 'MSLineScale))
    (dcl-Control-SetValue Ltools/PlotConfig/CheckBox8 (getvalue 'LongPaper))
    (dcl-Control-SetValue Ltools/PlotConfig/CheckBox9 (getvalue 'PlotLayouts))
	(dcl-Control-SetValue Ltools/PlotConfig/CheckBox11 (getvalue 'DocPlotStyle))

	;;tab2
    (dcl_Grid_Clear Ltools/PlotConfig/Grid31)
    (foreach n cpagesetups
        (dcl-Grid-AddRow Ltools/PlotConfig/Grid31 (cons "" n))
    )
    ;;tab3
    (setq ColumnNum (dcl-Grid-GetColumnCount Ltools/PlotConfig/Grid21))
    (if (< 2 ColumnNum)
        (repeat (1- ColumnNum )
        (dcl-Grid-DeleteColumn Ltools/PlotConfig/Grid21 ColumnNum)
        (setq ColumnNum (1- ColumnNum))
        )
    )
    (dcl-Grid-AddColumns Ltools/PlotConfig/Grid21 (mapcar (function (lambda (x) (list x 0 100 0))) (LM:str->lst (getvalue 'BlockFrameList) ",")))
    (dcl_Grid_Clear Ltools/PlotConfig/Grid21)
    (foreach n (mapcar (function (lambda (x) (LM:str->lst x ","))) (LM:str->lst (getvalue 'PrintPaperList) "|"))
        (dcl-Grid-AddRow Ltools/PlotConfig/Grid21 (cons "" n))
    )
)


;;tab1
(defun c:Ltools/PlotConfig/TextButton1#OnClicked (/ pathName ConfigStr)

    (if (setq pathName (findfile "Batchplot.ini"))
        (setq ConfigStr (ConfigRead pathName T))
    )
	(setq ConfigStr (ConfigPutKey ConfigStr "Batchplot" "Frame" (itoa (dcl-ComboBox-GetCurSel Ltools/PlotConfig/ComboBox1))))
	(setq ConfigStr (ConfigPutKey ConfigStr "Batchplot" "Output" (itoa (dcl-ComboBox-GetCurSel Ltools/PlotConfig/ComboBox2))))
	(setq ConfigStr (ConfigPutKey ConfigStr "Batchplot" "PlotOrder" (itoa (dcl-ComboBox-GetCurSel Ltools/PlotConfig/ComboBox3))))
	(setq ConfigStr (ConfigPutKey ConfigStr "Batchplot" "LongPaperSize" (itoa (dcl-ComboBox-GetCurSel Ltools/PlotConfig/ComboBox4))))
	(setq ConfigStr (ConfigPutKey ConfigStr "Batchplot" "Offset" (dcl-ComboBox-GetEBText Ltools/PlotConfig/ComboBox5)))
	(setq ConfigStr (ConfigPutKey ConfigStr "Batchplot" "PlotScale" (dcl-ComboBox-GetEBText Ltools/PlotConfig/ComboBox6)))

	(setq ConfigStr (ConfigPutKey ConfigStr "Batchplot" "BlockName" (dcl-Control-GetText Ltools/PlotConfig/TextBox1)))
	(setq ConfigStr (ConfigPutKey ConfigStr "Batchplot" "LayerName" (dcl-Control-GetText Ltools/PlotConfig/TextBox2)))
	(setq ConfigStr (ConfigPutKey ConfigStr "Batchplot" "Copies" (dcl-Control-GetText Ltools/PlotConfig/TextBox3)))
	(setq ConfigStr (ConfigPutKey ConfigStr "Batchplot" "BlockFrameList" (dcl-Control-GetText Ltools/PlotConfig/TextBox4)))

	(setq ConfigStr (ConfigPutKey ConfigStr "Batchplot" "AutoSelect" (itoa (dcl-Control-GetValue Ltools/PlotConfig/CheckBox1))))
	(setq ConfigStr (ConfigPutKey ConfigStr "Batchplot" "AutoPaper" (itoa (dcl-Control-GetValue Ltools/PlotConfig/CheckBox2))))
	(setq ConfigStr (ConfigPutKey ConfigStr "Batchplot" "PlotStamp" (itoa (dcl-Control-GetValue Ltools/PlotConfig/CheckBox3))))
	(setq ConfigStr (ConfigPutKey ConfigStr "Batchplot" "ReverseOrder" (itoa (dcl-Control-GetValue Ltools/PlotConfig/CheckBox4))))
	(setq ConfigStr (ConfigPutKey ConfigStr "Batchplot" "AutoRotate" (itoa (dcl-Control-GetValue Ltools/PlotConfig/CheckBox5))))
	(setq ConfigStr (ConfigPutKey ConfigStr "Batchplot" "UpsideDown" (itoa (dcl-Control-GetValue Ltools/PlotConfig/CheckBox6))))
	(setq ConfigStr (ConfigPutKey ConfigStr "Batchplot" "MSLineScale" (itoa (dcl-Control-GetValue Ltools/PlotConfig/CheckBox7))))
	(setq ConfigStr (ConfigPutKey ConfigStr "Batchplot" "LongPaper" (itoa (dcl-Control-GetValue Ltools/PlotConfig/CheckBox8))))
	(setq ConfigStr (ConfigPutKey ConfigStr "Batchplot" "PlotLayouts" (itoa (dcl-Control-GetValue Ltools/PlotConfig/CheckBox9))))
	(setq ConfigStr (ConfigPutKey ConfigStr "Batchplot" "DocPlotStyle" (itoa (dcl-Control-GetValue Ltools/PlotConfig/CheckBox11))))

    (ConfigWrite ConfigStr pathName)
)



;;tab2
(defun c:Ltools/PlotConfig/Grid31#OnBeginLabelEdit (Row Column / PaperList LocalPaperList)
    (setq CfgCellText (dcl-Grid-GetCellText Ltools/PlotConfig/Grid31 Row Column))
    (if (and (= 3 Column) (/= "" (dcl-Grid-GetCellText Ltools/PlotConfig/Grid31 Row 2)))
        (progn
        (vla-put-configname clayout (dcl-Grid-GetCellText Ltools/PlotConfig/Grid31 Row 2))
        ;;(vla-RefreshPlotDeviceInfo clayout)
        (setq PaperList (vlax-safearray->list (vlax-variant-value (vla-GetCanonicalMediaNames clayout)))
        LocalPaperList (mapcar (function (lambda (x) (vla-GetLocaleMediaName clayout x))) PaperList)
        )
        (dcl-Grid-SetCellStyle Ltools/PlotConfig/Grid31 Row 3 18)
        (dcl-Grid-SetCellDropList Ltools/PlotConfig/Grid31 Row 3 LocalPaperList)
        )
    )
)
(defun c:Ltools/PlotConfig/Grid31#OnEndLabelEdit (Row Column /)
    (if (and (/= CfgCellText (dcl-Grid-GetCellText Ltools/PlotConfig/Grid31 Row Column)) (= 2 Column))
        (dcl-Grid-SetCellText Ltools/PlotConfig/Grid31 Row 3 "")
    )
)
(defun c:Ltools/PlotConfig/Grid31#OnDblClicked (Row Column /)
    (dcl-Grid-DeleteRow Ltools/PlotConfig/Grid31 Row)
)
(defun c:Ltools/PlotConfig/TextButton31#OnClicked (/ CurCell CurRowData)
    (if (and (setq CurCell (dcl-Grid-GetCurCell Ltools/PlotConfig/Grid31)) (/= -1 (car CurCell)))
        (progn
        (setq CurRowData (dcl-Grid-GetRowCells Ltools/PlotConfig/Grid31 (car CurCell)))
        (dcl-Grid-InsertRow Ltools/PlotConfig/Grid31 (car CurCell) CurRowData)
        )
        (dcl-Grid-AddRow Ltools/PlotConfig/Grid31 "" "��Ԥ��" "��" "" "��")
    )
)
(defun c:Ltools/PlotConfig/TextButton32#OnClicked (/ CurCell CurRowData UpRowData ColumnNum)
    (if (and (setq CurCell (dcl-Grid-GetCurCell Ltools/PlotConfig/Grid31)) (/= -1 (car CurCell)) (/= 0 (car CurCell)))
        (progn
        (setq CurRowData (dcl-Grid-GetRowCells Ltools/PlotConfig/Grid31 (car CurCell)))
        (setq UpRowData (dcl-Grid-GetRowCells Ltools/PlotConfig/Grid31 (1- (car CurCell))))
        (setq ColumnNum (dcl-Grid-GetColumnCount Ltools/PlotConfig/Grid31))
        (repeat ColumnNum
            (dcl-Grid-SetCellText Ltools/PlotConfig/Grid31 (car CurCell) ColumnNum (nth ColumnNum UpRowData))
            (dcl-Grid-SetCellText Ltools/PlotConfig/Grid31 (1- (car CurCell)) ColumnNum (nth ColumnNum CurRowData))
            (setq ColumnNum (1- ColumnNum))
        )
        (dcl-Grid-SetCurCell Ltools/PlotConfig/Grid31 (1- (car CurCell)))
        )
    )
)
(defun c:Ltools/PlotConfig/TextButton33#OnClicked (/ CurCell CurRowData DownRowData ColumnNum)
    (if (and (setq CurCell (dcl-Grid-GetCurCell Ltools/PlotConfig/Grid31)) (/= -1 (car CurCell)) (/= (1- (dcl-Grid-GetRowCount Ltools/PlotConfig/Grid31)) (car CurCell)))
        (progn
        (setq CurRowData (dcl-Grid-GetRowCells Ltools/PlotConfig/Grid31 (car CurCell)))
        (setq DownRowData (dcl-Grid-GetRowCells Ltools/PlotConfig/Grid31 (1+ (car CurCell))))
        (setq ColumnNum (dcl-Grid-GetColumnCount Ltools/PlotConfig/Grid31))
        (repeat ColumnNum
            (dcl-Grid-SetCellText Ltools/PlotConfig/Grid31 (car CurCell) ColumnNum (nth ColumnNum DownRowData))
            (dcl-Grid-SetCellText Ltools/PlotConfig/Grid31 (1+ (car CurCell)) ColumnNum (nth ColumnNum CurRowData))
            (setq ColumnNum (1- ColumnNum))
        )
        (dcl-Grid-SetCurCell Ltools/PlotConfig/Grid31 (1+ (car CurCell)))
        )
    )
)
(defun c:Ltools/PlotConfig/TextButton34#OnClicked (/ i PsStr pathName ConfigStr)
    (setq i 0 PsStr '())
    (repeat (dcl-Grid-GetRowCount Ltools/PlotConfig/Grid31)
        (setq PsStr (cons (LM:lst->str (LM:RemoveNth 0 (dcl-Grid-GetRowCells Ltools/PlotConfig/Grid31 i)) ",") PsStr))
        (setq i (1+ i))
    )
    (setq PsStr (LM:lst->str (reverse PsStr) "|"))
    (if (setq pathName (findfile "Batchplot.ini"))
        (setq ConfigStr (ConfigRead pathName T))
    )
    (setq ConfigStr (ConfigPutKey ConfigStr "Batchplot" "PageSetup" PsStr))
    (ConfigWrite ConfigStr pathName)
    (setq cpagesetups (mapcar (function (lambda (x) (LM:str->lst x ","))) (LM:str->lst PsStr "|")))
    (dcl-ComboBox-Clear Ltools/BatchPlot/ComboBox1)
    (dcl-ComboBox-AddList Ltools/BatchPlot/ComboBox1 (carList cpagesetups))
)

;;tab3
(defun c:Ltools/PlotConfig/Grid21#OnBeginLabelEdit (Row Column / PaperList LocalPaperList)
    (if (and (>= Column 2) (/= "" (dcl-Grid-GetCellText Ltools/PlotConfig/Grid21 Row 1)))
        (progn
        (vla-put-configname clayout (dcl-Grid-GetCellText Ltools/PlotConfig/Grid21 Row 1))
        (setq PaperList (vlax-safearray->list (vlax-variant-value (vla-GetCanonicalMediaNames clayout)))
        LocalPaperList (mapcar (function (lambda (x) (vla-GetLocaleMediaName clayout x))) PaperList)
        )
        (dcl-Grid-SetCellStyle Ltools/PlotConfig/Grid21 Row Column 18)
        (dcl-Grid-SetCellDropList Ltools/PlotConfig/Grid21 Row Column LocalPaperList)
        )
    )
)
(defun c:Ltools/PlotConfig/Grid21#OnDblClicked (Row Column /)
    (dcl-Grid-DeleteRow Ltools/PlotConfig/Grid21 Row)
)
(defun c:Ltools/PlotConfig/TextButton21#OnClicked (/ CurCell CurRowData)
    (if (and (setq CurCell (dcl-Grid-GetCurCell Ltools/PlotConfig/Grid21)) (/= -1 (car CurCell)))
        (progn
        (setq CurRowData (dcl-Grid-GetRowCells Ltools/PlotConfig/Grid21 (car CurCell)))
        (dcl-Grid-InsertRow Ltools/PlotConfig/Grid21 (car CurCell) CurRowData)
        )
        (dcl-Grid-AddRow Ltools/PlotConfig/Grid21 "" "��")
    )
)
(defun c:Ltools/PlotConfig/TextButton22#OnClicked (/ CurCell CurRowData UpRowData ColumnNum)
    (if (and (setq CurCell (dcl-Grid-GetCurCell Ltools/PlotConfig/Grid21)) (/= -1 (car CurCell)) (/= 0 (car CurCell)))
        (progn
        (setq CurRowData (dcl-Grid-GetRowCells Ltools/PlotConfig/Grid21 (car CurCell)))
        (setq UpRowData (dcl-Grid-GetRowCells Ltools/PlotConfig/Grid21 (1- (car CurCell))))
        (setq ColumnNum (dcl-Grid-GetColumnCount Ltools/PlotConfig/Grid21))
        (repeat ColumnNum
            (dcl-Grid-SetCellText Ltools/PlotConfig/Grid21 (car CurCell) ColumnNum (nth ColumnNum UpRowData))
            (dcl-Grid-SetCellText Ltools/PlotConfig/Grid21 (1- (car CurCell)) ColumnNum (nth ColumnNum CurRowData))
            (setq ColumnNum (1- ColumnNum))
        )
        (dcl-Grid-SetCurCell Ltools/PlotConfig/Grid21 (1- (car CurCell)))
        )
    )
)
(defun c:Ltools/PlotConfig/TextButton23#OnClicked (/ CurCell CurRowData DownRowData ColumnNum)
    (if (and (setq CurCell (dcl-Grid-GetCurCell Ltools/PlotConfig/Grid21)) (/= -1 (car CurCell)) (/= (1- (dcl-Grid-GetRowCount Ltools/PlotConfig/Grid21)) (car CurCell)))
        (progn
        (setq CurRowData (dcl-Grid-GetRowCells Ltools/PlotConfig/Grid21 (car CurCell)))
        (setq DownRowData (dcl-Grid-GetRowCells Ltools/PlotConfig/Grid21 (1+ (car CurCell))))
        (setq ColumnNum (dcl-Grid-GetColumnCount Ltools/PlotConfig/Grid21))
        (repeat ColumnNum
            (dcl-Grid-SetCellText Ltools/PlotConfig/Grid21 (car CurCell) ColumnNum (nth ColumnNum DownRowData))
            (dcl-Grid-SetCellText Ltools/PlotConfig/Grid21 (1+ (car CurCell)) ColumnNum (nth ColumnNum CurRowData))
            (setq ColumnNum (1- ColumnNum))
        )
        (dcl-Grid-SetCurCell Ltools/PlotConfig/Grid21 (1+ (car CurCell)))
        )
    )
)
(defun c:Ltools/PlotConfig/TextButton24#OnClicked (/ i PsStr pathName ConfigStr BlkStr)
    (setq i 0 PsStr '())
    (repeat (dcl-Grid-GetRowCount Ltools/PlotConfig/Grid21)
        (setq PsStr (cons (LM:lst->str (LM:RemoveNth 0 (dcl-Grid-GetRowCells Ltools/PlotConfig/Grid21 i)) ",") PsStr))
        (setq i (1+ i))
    )
    (setq PsStr (LM:lst->str (reverse PsStr) "|"))
    (if (setq pathName (findfile "Batchplot.ini"))
        (setq ConfigStr (ConfigRead pathName T))
    )
    (setq ConfigStr (ConfigPutKey ConfigStr "Batchplot" "PrintPaperList" PsStr))
    (ConfigWrite ConfigStr pathName)
    (setvalue 'PrintPaperList PsStr)
)



;;�ж϶�����Ƿ�Ϊ����ͼ��
(defun IsRectang (ename / pts p1 p2 p3 p4 x y)
  (and
       (equal (vlax-curve-getstartpoint (vlax-ename->vla-object ename)) (vlax-curve-getendpoint (vlax-ename->vla-object ename)) 1e-8)
	   (setq pts (mapcar (function (lambda (v) (trans v ename 1))) (LM:massoc 10 (entget ename))))
       (mapcar 'set '(p1 p2 p3 p4) pts)
       (equal (distance p1 p3) (distance p2 p4) 1e-8)
       (equal (mapcar '(lambda (x y) (/ (+ x y) 2.0)) p1 p3) (mapcar '(lambda (x y) (/ (+ x y) 2.0)) p2 p4) 1e-8)
       (or
       (equal (angle p1 p2) 0.0 1e-8)
       (equal (angle p1 p2) pi 1e-8)
       (equal (angle p1 p2) (* 0.5 pi) 1e-8)
       (equal (angle p1 p2) (* 1.5 pi) 1e-8)
       )
  )
)


;;ɾ��Ƕ��ͼ��
(defun BoxFilter (ss / i ename minpoint maxpoint pmin pmax lst item pt1x pt1y pt2x pt2y ename1 j item1 pt3x pt3y pt4x pt4y ename2)
    (repeat (setq i (sslength ss))
        (setq ename (ssname ss (setq i (1- i))))
        (vla-getboundingbox (vlax-ename->vla-object ename) 'minpoint 'maxpoint)
        (setq pmin (vlax-safearray->list minpoint)
            pmax (vlax-safearray->list maxpoint)
        )
        (setq lst (cons (list pmin pmax ename) lst))
    )
    (repeat (setq i (length lst))
        (setq item (nth (setq i (1- i)) lst))
        (setq pt1x (car (car item))
            pt1y (cadr (car item))
            pt2x (car (cadr item))
            pt2y (cadr (cadr item))
            ename1 (caddr item)
        )
        (repeat (setq j (length lst))
            (setq item1 (nth (setq j (1- j)) lst))
            (setq pt3x (car (car item1))
                pt3y (cadr (car item1))
                pt4x (car (cadr item1))
                pt4y (cadr (cadr item1))
                ename2 (caddr item1)
            )
            (if (and (>= pt3x pt1x) (>= pt3y pt1y) (<= pt4x pt2x) (<= pt4y pt2y) (/= ename1 ename2))
                (if (member item1 lst)
                    (setq lst (vl-remove item1 lst))
                )
            )
        )
    )
    (setq ss (ssadd))
    (foreach x lst
        (if (= (type (caddr x)) 'ename)
            (ssadd (caddr x) ss)
        )
    )
    ss
)

;;ͼ���Χ��
(defun GetBoundingBox ( blk / bnm llp lst urp )
    (setq bnm (strcase (vla-get-name blk)))
    (cond
        (   (setq lst (cdr (assoc bnm LM:blockboundingbox:cache))))
        (   (progn
                (vlax-for obj (vla-item (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object))) bnm)
                    (cond
                        (   (= "AcDbBlockReference" (vla-get-objectname obj))
                            (setq lst (append lst (GetBoundingBox obj)))
                        )
                        (   (and
                                (= :vlax-true (vla-get-visible obj))
                                (not (wcmatch (vla-get-objectname obj) "AcDbAttributeDefinition,AcDb*Text"))
                                (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'llp 'urp))))
                            )
                            (setq lst (vl-list* (vlax-safearray->list llp) (vlax-safearray->list urp) lst))
                        )
                    )
                )
                lst
            )
            (setq lst (mapcar '(lambda ( fun ) (apply 'mapcar (cons fun lst))) '(min max)))
            (setq LM:blockboundingbox:cache (cons (cons bnm lst) LM:blockboundingbox:cache))
        )
    )
    (apply
        (function
            (lambda ( m v )
                (mapcar (function (lambda ( p ) (mapcar '+ (mxv m p) v))) lst)
            )
        )
        (refgeom (vlax-vla-object->ename blk))
    )
)

(defun refgeom ( ent / ang ang mat ocs )
    (setq enx (entget ent)
          ang (cdr (assoc 050 enx))
          ocs (cdr (assoc 210 enx))
    )
    (list
        (setq mat
            (mxm
                (mapcar '(lambda ( v ) (trans v 0 ocs t))
                   '(
                        (1.0 0.0 0.0)
                        (0.0 1.0 0.0)
                        (0.0 0.0 1.0)
                    )
                )
                (mxm
                    (list
                        (list (cos ang) (- (sin ang)) 0.0)
                        (list (sin ang) (cos ang)     0.0)
                       '(0.0 0.0 1.0)
                    )
                    (list
                        (list (cdr (assoc 41 enx)) 0.0 0.0)
                        (list 0.0 (cdr (assoc 42 enx)) 0.0)
                        (list 0.0 0.0 (cdr (assoc 43 enx)))
                    )
                )
            )
        )
        (mapcar '- (trans (cdr (assoc 10 enx)) ocs 0)
            (mxv mat (cdr (assoc 10 (tblsearch "block" (cdr (assoc 2 enx))))))
        )
    )
)

(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

(defun trp ( m )
    (apply 'mapcar (cons 'list m))
)

(defun mxm ( m n )
    ((lambda ( a ) (mapcar '(lambda ( r ) (mxv a r)) m)) (trp n))
)



;;2ά���г�
(defun 2DPoint (pt / )
    (vlax-make-variant (vlax-safearray-fill (vlax-make-safearray vlax-vbdouble '(0 . 1)) (list (car pt) (cadr pt))))
)

;;ȡ��ͼ��ĳ��ߺͶ̱߳���
(defun GetWidthHeight (bounding / x1 y1 x2 y2)
    (setq x1 (caar bounding)
        y1 (cadar bounding)
        x2 (caadr bounding)
        y2 (cadadr bounding)
    )
    (if	(< (abs (- x1 x2)) (abs (- y1 y2)))
        (list (abs (- y1 y2)) (abs (- x1 x2)))
        (list (abs (- x1 x2)) (abs (- y1 y2)))
    )
)

;;�ж�ͼ���Ƿ����
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

;;�Զ�����
(defun fixscale (n / i large small)
    (setq i 0
        large (> n 100)
        small (< n 10)
    )
    (while (or (> n 100) (< n 10))
        (if large
            (setq n (/ n 10.0))
            (setq n (* n 10.0))
        )
        (setq i (1+ i))
    )
    (setq n (fix (+ 0.5 n)))
    (repeat i
        (if large
            (setq n (* n 10.0))
            (setq n (/ n 10.0))
        )
    )
    n
)

;;���Ʋ���
(defun CopyLayout (nLayoutName / clayoutName tmpName)
    (setq clayoutName (getvar "ctab"))
    (setq tmpName (strcat "$_QF_TMP_LAYOUT_NAME_OF_" clayoutName))
    (vl-cmdf "_.layout" "_copy" clayoutName tmpName)
    (vl-cmdf "_.layout" "_rename" clayoutName NLayoutName)
    (vl-cmdf "_.layout" "_rename" tmpName clayoutName)
)

;;�õ�ͬ���ļ��б�
(defun GetFileList ( FileName )
    (vl-directory-files (getvalue 'PlotFileFolder) FileName 1)
)

;;�õ��Զ������
(defun GetNewAutoNumName (prefix FixNum NameList / i istr Name)
    (setq i 0)
    (setq istr (itoa i))
    (while (< (strlen istr) FixNum)
        (setq istr (strcat "0" istr))
    )
    (setq Name (strcat prefix istr))
    (while (member Name Namelist)
        (setq i (1+ i))
        (setq istr (itoa i))
        (while (< (strlen istr) FixNum)
            (setq istr (strcat "0" istr))
        )
        (setq Name (strcat prefix istr))
    )
    Name
)

  ;; �������ɲ���
  (defun doBatchLayout (bdlist plotscale
			ltscale autoRotate UpsideDown
			/	     landscapeList layoutprefix
			portraitList NoRotationList
			RotationList bd bd2 layouts
			layout	     ll		  ur
			MarginLL     MarginUR	  prdisplay
			crtvp	     showpg	  pwidth
			pHeight	     PaperWidth	  item
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
    (if	(< pwidth pheight)
      (setq PaperWidth pHeight)
      (setq PaperWidth pWidth)
    )
    ;; decide what to rotated
    (setq NoRotationList
	   (if (> pwidth pHeight)
	     landscapeList
	     portraitList
	   )
    )
    (setq RotationList
	   (if (> pwidth pHeight)
	     portraitList
	     landscapeList
	   )
    )

    (if	(null AutoRotate)
      (progn
        (setq NoRotationList bdlist)
        (setq RotationList nil)
      )
    )

    (setq crtvp	 (vla-get-LayoutCreateViewport prdisplay)
	  showpg (vla-get-LayoutShowPlotSetup prdisplay)
    )
    (vla-put-layoutcreateviewport prdisplay :vlax-false)
    (vla-put-layoutshowplotsetup prdisplay :vlax-false)

    ;; create 0 degree template
    (if	NoRotationList
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
        (cond ((= plotscale "ScaleToFit")
               (vl-cmdf	"_.MView"
            '(0 0)
            (mapcar	'-
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
          ;; restore to paperspace
          (vl-cmdf "_.pspace")
          (if (member layoutprefix (GetLayoutList))
          (setq layoutprefix (GetNewAutoNumName layoutprefix 2 (GetLayoutList)))
          )
          (vl-cmdf "_.layout" "_rename" "$temp_No_Rotation_Ctab" layoutprefix)
        )
        (vl-cmdf "_.layout" "_delete" "$temp_No_Rotation")
      )
    )
    ;; create 90 degree template
    (if	RotationList
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
        (cond ((= plotscale "ScaleToFit")
               (vl-cmdf	"_.MView"
            '(0 0)
            (mapcar	'-
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
          ;; restore to paperspace
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

  ;; ��Ҫ����������ʵ��:
  (defun doBatchPlot (bdlist	   mode		plotscale
		      AutoRotate  upsidedown / catch exbounding
		      scale pWidth
		      pHeight PaperWidth key
		      plotfile
		      count	   i            target
          PdfPaperList PdfLocalPaperList index
          PrintPaperLst PrinterLst PaperLst Papers ss ssent blockname Paper
          bWidthHeight bWidth fWidth bHeight LtPaper PaperInfo PaperOn A0lst A1lst A2lst A3lst A4lst bd2)

    (if (= 1 (atoi (getvalue 'PlotStamp)))
      (vl-cmdf "-plotstamp" "o" "_non")
      (vl-cmdf "-plotstamp" "off" "_non")
    )

    (if	(and (= mode "FILE") (= 1 (atoi (getvalue 'LongPaper))))
      (progn
         (setq	catch (vl-catch-all-apply
          'vla-put-configname
          (list clayout "DWG To PDF-LT.pc3")
              )
         )
         (if (vl-catch-all-error-p catch)
           (progn
           (alert (strcat "\n��ӡ��DWG To PDF-LT.pc3δ��װ��"))
           (exit)
           )
         )

        (setq PdfPaperList (vlax-safearray->list (vlax-variant-value (vla-GetCanonicalMediaNames clayout)))
              PdfLocalPaperList (mapcar (function (lambda (x) (vla-GetLocaleMediaName clayout x))) PdfPaperList)
        )
      )
      (progn
        (vla-getpapersize clayout 'pWidth 'pHeight)
        ;; ȡ�õ�ǰֽ�ŵĳ��߳���
        (if	(< pwidth pheight)
          (setq PaperWidth pHeight)
          (setq PaperWidth pWidth)
        )
      )
    )

    ;; ȡ�õ�ǰ��ӡ��������ֽ����Ϣ
    (if	(= 1 (atoi (getvalue 'AutoPaper)))
        (setq PdfPaperList (vlax-safearray->list (vlax-variant-value (vla-GetCanonicalMediaNames clayout)))
              PdfLocalPaperList (mapcar (function (lambda (x) (vla-GetLocaleMediaName clayout x))) PdfPaperList)
        )
    )

    ;; ��ÿ��ͼ��ѭ��
    (setq count	(length bdlist)
	  i	0
    )
    (foreach bounding bdlist
      (setq i (1+ i))


    ;;�ӳ�ͼֽ�Զ�ƥ���ӡֽ��
    (if	(and (= mode "FILE") (= 1 (atoi (getvalue 'LongPaper))))
      (progn
      (setq bWidthHeight (GetWidthHeight bounding))
      (setq bWidth (car bWidthHeight))
      (setq bHeight (cadr bWidthHeight))
      (setq PaperOn T)
        (cond
          ((= 0 (atoi (getvalue 'LongPaperSize)))
          (setq bWidth (/ (* 841 bWidth) bHeight))
          (setq A0lst '(891 1189 1338 1486 1635 1783 1932 2080 2230 2378 1200 1220 1240 1260 1280 1300 1320 1340 1360 1380 1400 1420 1440 1460 1480 1500 1520 1540 1560 1580 1600 1620 1640 1660 1680 1700 1720 1740 1760 1780 1800 1820 1840 1860 1880 1900 1920 1940 1960 1980 2000 2020 2040 2060 2100 2120 2140 2160 2180 2200 2220 2240 2260 2280 2300 2320 2340 2360 2380 2400 2420 2440 2460 2480 2500 2550 2600 2650 2700 2750 2800 2850 2900 2950 3000 3050 3100 3150 3200 3250 3300 3350 3400 3450 3500 3550 3600 3650 3700 3750 3800 2850 3900 3950 4000 4050 4100 4150 4200 4250 4300 5350 4400 4450 4500 4550 4600 4650 4700 4750 4800 4850 4900 4950 5000))
          (if (setq fWidth (FindNum bWidth A0lst))
          (setq LtPaper (strcat "A0-841X" (rtos fWidth 2 0)))
          (setq LtPaper (strcat "A0-841X" (rtos bWidth 2 0))))
          )
          ((= 1 (atoi (getvalue 'LongPaperSize)))
          (setq bWidth (/ (* 594 bWidth) bHeight))
          (setq A1lst '(841 1051 1261 1471 1682 1892 2102 860 880 900 920 940 960 980 1000 1020 1040 1060 1080 1100 1120 1140 1160 1180 1200 1220 1240 1260 1280 1300 1320 1340 1360 1380 1400 1420 1440 1460 1480 1500 1520 1540 1560 1580 1600 1620 1640 1660 1680 1700 1720 1740 1760 1780 1800 1820 1840 1860 1880 1900 1920 1940 1960 1980 2000 2020 2040 2060 2080 2100 2120 2140 2160 2180 2200 2220 2240 2260 2280 2300 2320 2340 2360 2380 2400 2420 2440 2460 2480 2500 2550 2600 2650 2700 2750 2800 2850 2900 2950 3000 3050 3100 3150 3200 3250 3300 3350 3400 3450 3500 3550 3600 3650 3700 3750 3800 2850 3900 3950 4000 4050 4100 4150 4200 4250 4300 5350 4400 4450 4500 4550 4600 4650 4700 4750 4800 4850 4900 4950 5000))
          (if (setq fWidth (FindNum bWidth A1lst))
          (setq LtPaper (strcat "A1-594X" (rtos fWidth 2 0)))
          (setq LtPaper (strcat "A1-594X" (rtos bWidth 2 0))))
          )
          ((= 2 (atoi (getvalue 'LongPaperSize)))
          (setq bWidth (/ (* 420 bWidth) bHeight))
          (setq A2lst '(594 743 841 891 1041 1189 1338 1486 1635 1783 1932 2080 600 620 640 660 680 700 720 740 760 780 800 820 840 860 880 900 920 940 960 980 1000 1020 1040 1060 1080 1100 1120 1140 1160 1180 1200 1220 1240 1260 1280 1300 1320 1340 1360 1380 1400 1420 1440 1460 1480 1500 1520 1540 1560 1580 1600 1620 1640 1660 1680 1700 1720 1740 1760 1780 1800 1820 1840 1860 1880 1900 1920 1940 1960 1980 2000 2020 2040 2060 2100 2120 2140 2160 2180 2200 2220 2240 2260 2280 2300 2320 2340 2360 2380 2400 2420 2440 2460 2480 2500 2550 2600 2650 2700 2750 2800 2850 2900 2950 3000 3050 3100 3150 3200 3250 3300 3350 3400 3450 3500 3550 3600 3650 3700 3750 3800 2850 3900 3950 4000 4050 4100 4150 4200 4250 4300 5350 4400 4450 4500 4550 4600 4650 4700 4750 4800 4850 4900 4950 5000))
          (if (setq fWidth (FindNum bWidth A2lst))
          (setq LtPaper (strcat "A2-420X" (rtos fWidth 2 0)))
          (setq LtPaper (strcat "A2-420X" (rtos bWidth 2 0))))
          )
          ((= 3 (atoi (getvalue 'LongPaperSize)))
          (setq bWidth (/ (* 297 bWidth) bHeight))
          (setq A3lst '(420 630 841 1051 1261 1471 1682 1892 440 460 480 500 520 540 560 580 600 620 640 660 680 700 720 740 760 780 800 820 840 860 880 900 920 940 960 980 1000 1020 1040 1060 1080 1100 1120 1140 1160 1180 1200 1220 1240 1260 1280 1300 1320 1340 1360 1380 1400 1420 1440 1460 1480 1500 1520 1540 1560 1580 1600 1620 1640 1660 1680 1700 1720 1740 1760 1780 1800 1820 1840 1860 1880 1900 1920 1940 1960 1980 2000 2020 2040 2060 2080 2100 2120 2140 2160 2180 2200 2220 2240 2260 2280 2300 2320 2340 2360 2380 2400 2420 2440 2460 2480 2500 2550 2600 2650 2700 2750 2800 2850 2900 2950 3000 3050 3100 3150 3200 3250 3300 3350 3400 3450 3500 3550 3600 3650 3700 3750 3800 2850 3900 3950 4000 4050 4100 4150 4200 4250 4300 5350 4400 4450 4500 4550 4600 4650 4700 4750 4800 4850 4900 4950 5000))
          (if (setq fWidth (FindNum bWidth A3lst))
          (setq LtPaper (strcat "A3-297X" (rtos fWidth 2 0)))
          (setq LtPaper (strcat "A3-297X" (rtos bWidth 2 0))))
          )
          ((= 4 (atoi (getvalue 'LongPaperSize)))
          (setq bWidth (/ (* 210 bWidth) bHeight))
          (setq A4lst '(297 445 594 300 320 340 360 380 400 420 440 460 480 500 520 540 560 580 600 620 640 660 680 700 720 740 760 780 800 820 840 860 880 900 920 940 960 980 1000 1020 1040 1060 1080 1100 1120 1140 1160 1180 1200 1220 1240 1260 1280 1300 1320 1340 1360 1380 1400 1420 1440 1460 1480 1500 1520 1540 1560 1580 1600 1620 1640 1660 1680 1700 1720 1740 1760 1780 1800 1820 1840 1860 1880 1900 1920 1940 1960 1980 2000 2020 2040 2060 2080 2100 2120 2140 2160 2180 2200 2220 2240 2260 2280 2300 2320 2340 2360 2380 2400 2420 2440 2460 2480 2500 2550 2600 2650 2700 2750 2800 2850 2900 2950 3000 3050 3100 3150 3200 3250 3300 3350 3400 3450 3500 3550 3600 3650 3700 3750 3800 2850 3900 3950 4000 4050 4100 4150 4200 4250 4300 5350 4400 4450 4500 4550 4600 4650 4700 4750 4800 4850 4900 4950 5000))
          (if (setq fWidth (FindNum bWidth A4lst))
          (setq LtPaper (strcat "A4-210X" (rtos fWidth 2 0)))
          (setq LtPaper (strcat "A4-210X" (rtos bWidth 2 0))))
          )
          ((= 5 (atoi (getvalue 'LongPaperSize)))
          (setq PaperInfo (/ bWidth bHeight))
            (cond
              ((or (equal PaperInfo (/ 1189 841.0) 1e-6) (equal PaperInfo (/ 1338 841.0) 1e-6)
              (equal PaperInfo (/ 1486 841.0) 1e-6) (equal PaperInfo (/ 1635 841.0) 1e-6)
              (equal PaperInfo (/ 1783 841.0) 1e-6) (equal PaperInfo (/ 1932 841.0) 1e-6)
              (equal PaperInfo (/ 2080 841.0) 1e-6) (equal PaperInfo (/ 2230 841.0) 1e-6)
              (equal PaperInfo (/ 2378 841.0) 1e-6))
              (setq bWidth (/ (* 841 bWidth) bHeight))
              (setq LtPaper (strcat "A0-841X" (rtos bWidth 2 0)))
              )
              ((or (equal PaperInfo (/ 841 594.0) 1e-6) (equal PaperInfo (/ 1051 594.0) 1e-6)
              (equal PaperInfo (/ 1261 594.0) 1e-6) (equal PaperInfo (/ 1471 594.0) 1e-6)
              (equal PaperInfo (/ 1682 594.0) 1e-6) (equal PaperInfo (/ 1892 594.0) 1e-6)
              (equal PaperInfo (/ 2102 594.0) 1e-6))
              (setq bWidth (/ (* 594 bWidth) bHeight))
              (setq LtPaper (strcat "A1-594X" (rtos bWidth 2 0)))
              )
              ((or (equal PaperInfo (/ 594 420.0) 1e-6) (equal PaperInfo (/ 743 420.0) 1e-6)
              (equal PaperInfo (/ 841 420.0) 1e-6) (equal PaperInfo (/ 891 420.0) 1e-6)
              (equal PaperInfo (/ 1041 420.0) 1e-6) (equal PaperInfo (/ 1189 420.0) 1e-6)
              (equal PaperInfo (/ 1338 420.0) 1e-6) (equal PaperInfo (/ 1486 420.0) 1e-6)
              (equal PaperInfo (/ 1635 420.0) 1e-6) (equal PaperInfo (/ 1783 420.0) 1e-6)
              (equal PaperInfo (/ 1932 420.0) 1e-6) (equal PaperInfo (/ 2080 420.0) 1e-6))
              (setq bWidth (/ (* 420 bWidth) bHeight))
              (setq LtPaper (strcat "A2-420X" (rtos bWidth 2 0)))
              )
              (T
              (setq LtPaper (strcat "AX-" (rtos bHeight 2 0) "X" (rtos bWidth 2 0)))
              )
            )
          )
        )
        (if (setq index (vl-position (strcase LtPaper) (mapcar (function strcase) PdfLocalPaperList)))
          (vla-put-CanonicalMediaName clayout (nth index PdfPaperList))
          (progn
          (vla-put-CanonicalMediaName clayout (nth 0 PdfPaperList))
          (apply 'vl-cmdf (cons "rectang" bounding))
          (vl-cmdf "_.-BHATCH" "p" "ansi31" "100" "0" "s" (entlast) "" "")
          (setq PaperOn nil)
          ;(alert (strcat "\nֽ�š�" LtPaper "��δ���壡"))
          ;(exit)
          )
        )
        (vla-getpapersize clayout 'pWidth 'pHeight)
        ;; ȡ�õ�ǰֽ�ŵĳ��߳���
        (if	(< pwidth pheight)
          (setq PaperWidth pHeight)
          (setq PaperWidth pWidth)
        )
      )
    )

    ;;����ͼ�����Զ�ƥ���ӡֽ��
    (if	(= 1 (atoi (getvalue 'AutoPaper)))
      (progn
	  (setq PrintPaperLst (mapcar (function (lambda (x) (LM:str->lst x ","))) (LM:str->lst (getvalue 'PrintPaperList) "|")))
	  (setq PrinterLst (carList PrintPaperLst))
	  (setq PaperLst (mapcar (function (lambda (x) (LM:RemoveNth 0 x))) PrintPaperLst))
        (if (setq index (vl-position (vla-get-configname clayout) PrinterLst))
            (setq Papers (nth index PaperLst))
            (progn
            (alert (strcat "\n��ӡ����" (vla-get-configname clayout) "����Ԥ��ֽ����Ϣ�����ڣ�"))
            (exit)
            )
        )
        (setq ss (ssget "_w" (car bounding) (cadr bounding) (list '(0 . "INSERT") (cons 2 (BlockFilter (getvalue 'BlockFrameList))))))
        (if ss
            (progn
            (setq ssent (ssname ss 0))
			(setq blockname (vla-get-effectivename (vlax-ename->vla-object ssent)))
			(setq index (vl-position blockname (LM:str->lst (getvalue 'BlockFrameList) ",")))
			(setq Paper (nth index Papers))
            (if (setq index (vl-position (strcase Paper) (mapcar (function strcase) PdfLocalPaperList)))
                (vla-put-CanonicalMediaName clayout (nth index PdfPaperList))  ;���ô�ӡֽ��
                (progn
                (alert (strcat "\nֽ�š�" Paper "�������ڣ�"))
                (exit)
                )
            )
            )
            (progn
                (alert (strcat "\nͼ�顾" (getvalue 'BlockName) "����Ԥ�����Ϣ�в����ڣ�"))
                (exit)
            )
        )
        (vla-getpapersize clayout 'pWidth 'pHeight)
        ;; ȡ�õ�ǰֽ�ŵĳ��߳���
        (if	(< pwidth pheight)
          (setq PaperWidth pHeight)
          (setq PaperWidth pWidth)
        )
      )
    )


      ;;(vla-put-paperunits clayout acMilliMeters)
      (if (= (vla-get-paperunits clayout) acInches) (vla-put-paperunits clayout acMillimeters))

      ;; ����Ϊ����ʽ��ӡ
	  (vla-put-plotwithplotstyles clayout :vlax-true)

      ;;(vla-put-plotorigin clayout (2DPoint '(0 0)))
      ;; ���ô�ӡ����
      (if AutoRotate
        (if (= (islandscape bounding) (> pWidth pHeight))
          (vla-put-plotrotation clayout (if upsidedown ac180degrees ac0degrees))
          (vla-put-plotrotation clayout (if upsidedown ac270degrees ac90degrees))
        )
      )
      ;; ���ô�ӡ��Χ
      (setq target (getvar "target"))
      (setq exbounding (expandbounding bounding 1e6))
      ;;���������ڷ�ֹ��ӡʱ�򲻳�����
      (vla-SetWindowToPlot
        clayout
        (2DPoint (mapcar '- (car exbounding) target))
        (2DPoint (mapcar '- (cadr exbounding) target))
      )
      ;; (apply 'vl-cmdf (cons "rectang" exbounding))
      ;; ���ô�ӡ��ʽΪwindow
      (vla-put-plottype clayout acWindow)
      ;; ���ô�ӡ����
      (cond
        ((= plotscale "ScaleToFit")
         (progn	(vla-put-standardscale clayout acScaleToFit)
          (princ "\n��ǰ��ӡ����: �ʺϿɴ�ӡ����")
          (grtext	-2
            (strcat	"\n���ڴ�ӡ: ��"
              (itoa i)
              "/"
              (itoa count)
              "ҳ, ����: �ʺϿɴ�ӡ����"
            )
          )
         )
        )
        ((= plotscale "Auto")
         (progn
           (setq scale (fixscale (/ (car (GetWidthHeight exbounding)) PaperWidth)))
           (vla-put-standardscale clayout acVpCustomScale)
           (vla-setcustomscale clayout 1 scale)
           (princ (strcat "\n��"
              (itoa i)
              "/"
              (itoa count)
              "ҳ, ���� 1:"
              (rtos scale)
            )
           )
           (grtext -2
             (strcat "���ڴ�ӡ: ��"
               (itoa i)
               "/"
               (itoa count)
               "ҳ, ���� 1:"
               (rtos scale)
             )
           )

         )
        )
        ('T
         (progn	(vla-put-standardscale clayout acVpCustomScale)
          (vla-setcustomscale clayout 1 plotscale)
          (princ (strcat "\n��"
                   (itoa i)
                   "/"
                   (itoa count)
                   "ҳ, ���� 1:"
                   plotscale
                 )
          )
          (grtext	-2
            (strcat	"���ڴ�ӡ: ��"
              (itoa i)
              "/"
              (itoa count)
              "ҳ, ���� 1:"
              plotscale
            )
          )
         )
        )
      )
      ;; ��AutoCAD����״̬����Ϣ����������AutoCAD����������Ϣ�� AutoCAD2000��2002���ܼ�ʱ����״̬����ʾ��
      ;; ������ʾAutoCAD 2004��2005����ִ�д˾䡣
      (if (< (atof (getvar "acadver")) 16.0)
	      (vla-eval (vlax-get-acad-object) "doEvents")
      )
      ;; �����Զ����д�ӡ
      (if (= "Center" (getvalue 'Offset))
        (vla-put-centerplot clayout :vlax-true)
        ;; else
        (vla-put-plotorigin clayout (2DPoint (getvalue 'Offset)))
      )
      ;; ��ӡ��Ԥ��
      (cond
        ((= mode "PLOT")
         (progn
           (princ "\n��ӡ����: ")
           (princ (getvalue 'Copies))
           (princ "\n")
           (vla-put-NumberofCopies plot (read (getvalue 'Copies)))
           (if (= :vlax-false (vla-plotToDevice plot))
             (exit)
           )
         )
        )
        ((= mode "PREVIEW")
         (if
           (= :vlax-false (vla-displayplotpreview plot acfullpreview))
            (exit)
         )
        )
        ((and (= mode "FILE") (= 0 (atoi (getvalue 'LongPaper))))
         (progn
          (setq bd2 (expandbounding bounding 1e2))
          (vl-cmdf "_.zoom" "_window" "_non" (car bd2) "_non" (cadr bd2))
          (setq plotfile (FormatName (getvalue 'NamePrefix) exbounding  (strcat "*." (getvalue 'NameExt))))
          (setq plotfile (strcat (GetValue 'PlotFileFolder) plotfile "." (getvalue 'NameExt)))
          (princ "\n���ɴ�ӡ�ļ�: ")
          (princ plotfile)
          (princ "\n")
          (vla-plottofile plot plotfile)
          (vl-cmdf "_.zoom" "_p")
         )
        )
        ((and PaperOn (= mode "FILE") (= 1 (atoi (getvalue 'LongPaper))))
         (progn
          (setq bd2 (expandbounding bounding 1e2))
          (vl-cmdf "_.zoom" "_window" "_non" (car bd2) "_non" (cadr bd2))
          (setq plotfile (FormatName (getvalue 'NamePrefix) exbounding "*.pdf"))
          (setq plotfile (strcat (GetValue 'PlotFileFolder) plotfile ".pdf"))
          (princ "\n���ɴ�ӡ�ļ�: ")
          (princ plotfile)
          (princ "\n")
          (vla-plottofile plot plotfile)
          (vl-cmdf "_.zoom" "_p")
         )
        )
      )
      (if (and (= mode "PREVIEW") (/= bounding (last bdlist)))
        (progn (initget "Yes No")
               (setq key (getkword "�Ƿ����Ԥ����һ��? [Yes/No]<Yes>"))
               (if (= key "No")
           (exit)
               )
        )
      )
    )
  )

  ;; ������ӡ����
  (defun PlotLayoutsInTabOrder
	 (LayoutNameList Plot-To-File-P / LayoutName ctab)
    (setq ctab (getvar "ctab"))
    (if (= 1 (atoi (getvalue 'PlotStamp)))
      (vl-cmdf "-plotstamp" "o" "_non")
      (vl-cmdf "-plotstamp" "off" "_non")
    )
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

  (defun doPlotLayoutsInTabOrder
	 (/ LayoutNameList plot-to-file-p key indexlist sellst)
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
    (princ)
  )


  ;;���յ�ǰ���ñ�����ӡ������ͼֽ
  (defun doPlotLayouts ( / LayoutNameList ctab PlotStyleTable filterlist ss)
    (setq LayoutNameList (layout-tab-list acaddoc))
    (setq ctab (getvar "ctab"))
    (setq PlotStyleTable (vla-get-stylesheet clayout))

    (foreach LayoutName	LayoutNameList
      (vl-cmdf "_.layout" "_set" layoutname)
      (if (/= "MODEL" (strcase layoutname))
      (vl-cmdf "_.pspace")
      )
      (vl-cmdf "_.zoom" "_e")
      (setq clayout (vla-get-activelayout acaddoc))
      (vla-put-stylesheet clayout PlotStyleTable)

      ;; ͼ��ͼ��
      (if (= "1" (getvalue 'Frame))
	      (setq filterlist
           (list '(0 . "INSERT")
           (cons 2 (BlockFilter (getvalue 'BlockName)))
           (cons 410 LayoutName)
           )
	      )
      )
      ;; ����ͼ��(PLINE)
      (if (= "0" (getvalue 'Frame))
        (setq filterlist
           (list '(0 . "LWPOLYLINE")
           '(8 . "*_TITLE")
           (cons 410 LayoutName)
            '(-4 . "<OR")
                '(-4 . "<AND") '(90 . 5) '(70 . 0) '(-4 . "AND>")
                '(-4 . "<AND") '(90 . 4) '(70 . 1) '(-4 . "AND>")
            '(-4 . "OR>")
           '(42 . 0.0)
          )
        )
      )
      ;; ָ��ͼ�����PLINE
      (if (= "2" (getvalue 'Frame))
        (setq filterlist
           (list '(0 . "LWPOLYLINE")
           (cons 8 (getvalue 'LayerName))
           (cons 410 LayoutName)
            '(-4 . "<OR")
                '(-4 . "<AND") '(90 . 5) '(70 . 0) '(-4 . "AND>")
                '(-4 . "<AND") '(90 . 4) '(70 . 1) '(-4 . "AND>")
            '(-4 . "OR>")
           '(42 . 0.0)
           )
        )
      )

     (setq ss (ssget "_x" filterlist))
     (if ss
     (progn
     ;(GetFrames ss)
     (vl-catch-all-apply 'GetFrames (list ss))
        (if (= "2" (getvalue 'Output))
        (doBatchPlot
        (orderFrames (getvalue 'SelectedFrames))
        "FILE"
        (getvalue 'plotscale)
        (= "1" (getvalue 'AutoRotate))
        (= "1" (getvalue 'UpsideDown))
        )
        )
     )
     )
    )
    (vl-cmdf "_.layout" "_set" ctab)
  )



;;�����鵵
(defun doBatchWblock (bdlist / dwgfile bd2)
    (foreach bd bdlist
        (setq bd2 (expandbounding bd 1e2))
        (vl-cmdf "_.zoom" "_window" "_non" (car bd2) "_non" (cadr bd2))
        (setq dwgfile (FormatName (getvalue 'NamePrefix) bd "*.dwg"))
        (setq dwgfile (strcat (GetValue 'PlotFileFolder) dwgfile ".dwg"))
        (ssget "_w" (car bd) (cadr bd))
        (princ "\n���ɹ鵵�ļ���")
        (princ dwgfile)
        (princ "\n")
        (vla-wblock acaddoc dwgfile (vla-get-activeselectionset acaddoc))
        (vl-cmdf "_.zoom" "_p")
    )
)

;;�����ļ���
(defun FormatName (str bd ext / Attlist Attstr str1 i Bname Tag ss ssent AttstrN
                            Txtstr str2 n Lname Num ss2 ssent2 TxtstrN filebaselist)
    (cond
    ;;�����滻���Կ�
    ((setq Attlist (LM:StrRegExpE "<Att:.+?:.+?>" str ""))
        (foreach Attstr Attlist
        (setq str1 (substr Attstr 6))
        (setq i (vl-string-search ":" str1))
        (setq Bname (substr str1 1 i))
        (setq Tag (substr str1 (1+ i)))
        (setq Tag (vl-string-left-trim ":" (vl-string-right-trim ">" Tag)))
        (setq ss (ssget "_w" (car bd) (cadr bd) (list '(0 . "INSERT") (cons 2 (BlockFilter Bname)))))
        (if ss
            (progn
            (setq ssent (ssname ss 0))
            (setq AttstrN (LM:vl-getattributevalue (vlax-ename->vla-object ssent) Tag))
            (setq str (LM:StringSubst AttstrN Attstr str))
            )
            (setq str (LM:StringSubst "" Attstr str))
        )
        )
    )
    ;;�����滻�ض�ͼ���ϵ�����
    ((setq Txtstr (car (LM:StrRegExpE "<Txt:.+:.+>" str "")))
        (progn
        (setq str2 (substr Txtstr 6))
        (setq n (vl-string-search ":" str2))
        (setq Lname (substr str2 1 n))
        (setq Num (substr str2 (1+ n)))
        (setq Num (vl-string-left-trim ":" (vl-string-right-trim ">" Num)))
        (setq ss2 (ssget "_w" (car bd) (cadr bd) (list '(0 . "TEXT") (cons 8 Lname))))
        (if ss2
            (progn
            (setq ssent2 (ssname ss2 (1- (atoi Num))))
            (setq TxtstrN (vlax-get (vlax-ename->vla-object ssent2) 'TextString))
            (setq str (LM:StringSubst TxtstrN Txtstr str))
            )
            (setq str (LM:StringSubst "" Txtstr str))
        )
        )
    )
    (T
        (progn
        (if ext
            (setq filebaselist (mapcar 'vl-filename-base (GetFileList (strcat str ext))))
            (setq filebaselist (GetLayoutList))
        )
        (setq str (GetNewAutoNumName str 2 filebaselist))
        )
    )
    )
    str
)


;;��̬ͼ����ʵ��������
(defun BlockFilter (blknamelst / ss n count ent str blkname)
  (setq ss (ssget "X" '((0 . "INSERT"))))
  (setq        n     0
        count (sslength ss)
        str   ""
  )
  (repeat count
    (setq ent (ssname ss n))
    (setq blkname (vla-get-effectivename
                     (vlax-ename->vla-object ent)
                   )
    )
    (if        (wcmatch blkname blknamelst)
      (setq str (strcat str ",`" (cdr (assoc 2 (entget ent)))))
    )
    (setq n (1+ n))
  )
  str
)




;;ѡ��ͼ��
(defun SelectBlock (/ ss)
    (vl-catch-all-apply
        '(lambda ( / ename bname)
            (if (setq ename (car (LM:entsel  "\nָ��ͼ��ͼ�飺"  ":S"   '((0 . "INSERT"))  "\n��ѡ���񲻷���Ҫ��������ѡ��")))
				(setq bname (vla-get-effectivename (vlax-ename->vla-object ename)))
            )
            (setvalue 'BlockName bname)
            (if (= "0" (getvalue 'AutoSelect))
                (progn
                (setq ss (ssadd))
                (ssadd ename ss)
                )
                (setq ss (ssget "_x" (list '(0 . "INSERT")
                    (cons 2 (BlockFilter bname))
                    (cons 410 (getvar "ctab"))
                    ))
                )
            )
            ;(GetFrames ss)
            (vl-catch-all-apply 'GetFrames (list ss))
        )
        nil
    )
)

;;ѡ��ͼ��
(defun SelectLayer ( / ss)
    (vl-catch-all-apply
        '(lambda (/ ename lname)
            (setq ename (car (entsel "\nָ��ͼ�����ڵ�ͼ�����������:")))
            (if (null ename)
                (exit)
            )
            (setq lname (vla-get-Layer (vlax-ename->vla-object ename)))
            (setvalue 'LayerName lname)
            (if (= "0" (getvalue 'AutoSelect))
                (progn
                (setq ss (ssadd))
                (ssadd ename ss)
                )
                (setq ss (ssget "_x" (list '(0 . "LWPOLYLINE")
                    (cons 8 lname)
                    (cons 410 (getvar "ctab"))
                    '(-4 . "<OR")
                        '(-4 . "<AND") '(90 . 5) '(70 . 0) '(-4 . "AND>")
                        '(-4 . "<AND") '(90 . 4) '(70 . 1) '(-4 . "AND>")
                    '(-4 . "OR>")
                    '(42 . 0.0)
                    ))
                )
            )
            ;(GetFrames ss)
            (vl-catch-all-apply 'GetFrames (list ss))
        )
        nil
    )
)

;;����ͼ������ͼ��
(defun GetBlockAtt ( / ent taglst selstr blockname str)
    (if (setq ent (car (LM:entsel "\n��ѡ�����Կ�ͼ��" "_:L" '((0 . "INSERT") (66 . 1)) "\n��ѡ���񲻷���Ҫ����ѡ�����Կ�ͼ��")))
        (progn
        (setq taglst (mapcar (function (lambda (att) (vla-get-tagstring att))) (vlax-invoke (vlax-ename->vla-object ent) 'getattributes)))
        (setq selstr (Odcl:SingleList "��ѡ��������" "�������б�" taglst))
        (setq blockname (vla-get-effectivename (vlax-ename->vla-object ent)))
        (setq str (strcat "<Att:" blockname ":" selstr ">"))
        (setvalue 'NamePrefix (strcat (getvalue 'NamePrefix) str))
        )
    )
)

;;��������ͼ��
(defun GetTextNum ( / ent layername pt1 pt2 ss i str)
    (if (setq ent (car (LM:entsel "\n��ѡ������" "_:L" '((0 . "TEXT")) "\n��ѡ���񲻷���Ҫ����ѡ�����֣�")))
        (progn
        (setq layername (vlax-get (vlax-ename->vla-object ent) 'Layer))
            (if (and
                (setq pt1 (getpoint "\n��ָ��ͼ��Χ��һ���ǵ㣺"))
                (setq pt2 (getcorner pt1 "\n��ָ��ͼ��Χ��һ���ǵ㣺"))
                )
                (progn
                (setq ss (ssget "_w" pt1 pt2 (list '(0 . "TEXT") (cons 8 layername))))
                (setq i (1+ (vl-position  ent (LM:ss->ent ss))))
                (setq str (strcat "<Txt:" layername ":" (itoa i) ">"))
                (setvalue 'NamePrefix (strcat (getvalue 'NamePrefix) str))
                )
            )
        )
    )
)

;;��ͼ����������
(defun expandbounding (bd fuzz / pt1 pt2 offset w h)
    (setq w (- (caadr bd) (caar bd)))
    (setq h (- (cadadr bd) (cadar bd)))
    (setq offset (/ w fuzz))    ;���������ڷ�ֹ��ӡʱ�򲻳�����
    (setq pt1 (mapcar '- (car bd) (list offset offset)))
    (setq pt2 (mapcar '+ (cadr bd) (list offset offset)))
    (list pt1 pt2)
)

;;ѡ��ͼ��
(defun GetFrames (ss / filterlist i ename elist bdlist pts ll ur bd e)
    (setvar "highlight" 1)
    (if (= nil ss)
	(progn
		;;ͼ��ͼ��
		(if (= "1" (getvalue 'Frame))
			(setq filterlist
				(list '(0 . "INSERT")
					(cons 2 (BlockFilter (getvalue 'BlockName)))
				)
			)
		)
		;;����ͼ��(PLINE)
		(if (= "0" (getvalue 'Frame))
			(setq filterlist
				(list '(0 . "LWPOLYLINE")
					'(8 . "*_TITLE")
					'(-4 . "<OR")
						'(-4 . "<AND") '(90 . 5) '(70 . 0) '(-4 . "AND>")
						'(-4 . "<AND") '(90 . 4) '(70 . 1) '(-4 . "AND>")
					'(-4 . "OR>")
					'(42 . 0.0)
				)
			)
		)
		;;ָ��ͼ�����PLINE
		(if (= "2" (getvalue 'Frame))
			(setq filterlist
				(list '(0 . "LWPOLYLINE")
					(cons 8 (getvalue 'LayerName))
					'(-4 . "<OR")
						'(-4 . "<AND") '(90 . 5) '(70 . 0) '(-4 . "AND>")
						'(-4 . "<AND") '(90 . 4) '(70 . 1) '(-4 . "AND>")
					'(-4 . "OR>")
					'(42 . 0.0)
				)
			)
		)
        (setq ss (ssget filterlist))
    )
	)

    (if (/= "1" (getvalue 'Frame))
        (repeat (setq i (sslength ss))
            (setq ename (ssname ss (setq i (1- i))))
            (if (not (IsRectang ename))
            (ssdel ename ss)
            )
        )
    )

    (setq ss (BoxFilter ss))

    (if ss
        (progn
        (setq elist (LM:ss->ent ss))
		(setq bdlist nil)
        (if (= "1" (getvalue 'Frame))
            (foreach e elist
                (setq bd (getboundingbox (vlax-ename->vla-object e)))
				(setq bd (list (trans (car bd) 0 1) (trans (cadr bd) 0 1)))
                (setq bdlist (cons bd bdlist))
            )
            (foreach e elist
                (setq pts (mapcar (function (lambda (v) (trans v e 1))) (LM:massoc 10 (entget e))))
                (setq ll (list (apply 'min (mapcar 'car pts)) (apply 'min (mapcar 'cadr pts))))
                (setq ur (list (apply 'max (mapcar 'car pts)) (apply 'max (mapcar 'cadr pts))))
                (setq bdlist (cons (list ll ur) bdlist))
            )
        )
		(setq bdlist (reverse bdlist))
        (setvalue 'SelectedFrames bdlist)
        (HighLightShow bdlist)
        )
    )
)

;;������ʾ
(defun HighLightShow (bdlist / )
    (vl-cmdf "_.redraw")
    (foreach bd	bdlist
        (grdraw (car bd) (list (caar bd) (cadadr bd)) acRed 1)
        (grdraw (car bd) (list (caadr bd) (cadar bd)) acRed 1)
        (grdraw (cadr bd) (list (caar bd) (cadadr bd)) acRed 1)
        (grdraw (cadr bd) (list (caadr bd) (cadar bd)) acRed 1)
        (grdraw (cadr bd) (car bd) acRed 1)
        (grdraw (list (caar bd) (cadadr bd)) (list (caadr bd) (cadar bd)) acRed 1)
    )
    (princ)
)

;;��ͼ������
(defun OrderFrames (bdlist / vscoor)
    (defun vscoor (n)   ;��Ļ�Ӿ����꣬�����������������ͬ����ͬ�ˡ�
        (fix (/ n (/ (getvar "viewsize") 100.0)))
    )
    ;;main orderframes
    (if	(= (getvalue 'PlotOrder) "1")
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
    (if	(= (getvalue 'PlotOrder) "2")
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
    (if	(= (getvalue 'ReverseOrder) "1")
        (setq bdlist (reverse bdlist))
    )
    bdlist
)

;;���յ�ǰ���ö��ĵ���ӡ
(defun doPlots ( / filterlist ss)
    (setq dclvalues (vl-bb-ref 'bbdcl))

    (if	(not (and (= "2" (getvalue 'Output)) (= "1" (getvalue 'LongPaper))))
		(progn
		(vla-put-configname clayout (getvalue 'printer))
		(vla-put-CanonicalMediaName clayout (getvalue 'printpaper))
		)
    )

    (if	(= "0" (getvalue 'DocPlotStyle))
		(vla-put-stylesheet clayout (getvalue 'printstyle))
    )

    ;;����ͼ��(PLINE)
    (if (= "0" (getvalue 'Frame))
        (setq filterlist
            (list '(0 . "LWPOLYLINE")
                '(8 . "*_TITLE")
				(cons 410 (getvar "ctab"))
                '(-4 . "<OR")
                    '(-4 . "<AND") '(90 . 5) '(70 . 0) '(-4 . "AND>")
                    '(-4 . "<AND") '(90 . 4) '(70 . 1) '(-4 . "AND>")
                '(-4 . "OR>")
                '(42 . 0.0)
            )
        )
    )
    ;; ͼ��ͼ��
    (if (= "1" (getvalue 'Frame))
      (setq filterlist
       (list '(0 . "INSERT")
       (cons 2 (BlockFilter (getvalue 'BlockName)))
	   (cons 410 (getvar "ctab"))
       )
      )
    )
    ;;ָ��ͼ�����PLINE
    (if (= "2" (getvalue 'Frame))
        (setq filterlist
            (list '(0 . "LWPOLYLINE")
                (cons 8 (getvalue 'LayerName))
				(cons 410 (getvar "ctab"))
                '(-4 . "<OR")
                    '(-4 . "<AND") '(90 . 5) '(70 . 0) '(-4 . "AND>")
                    '(-4 . "<AND") '(90 . 4) '(70 . 1) '(-4 . "AND>")
                '(-4 . "OR>")
                '(42 . 0.0)
            )
        )
    )
    (setq ss (ssget "_x" filterlist))
    (if ss
        (progn
			(vl-catch-all-apply 'GetFrames (list ss))
			(if (= "0" (getvalue 'Output))
                (doBatchPlot
                (orderFrames (getvalue 'SelectedFrames))
                "PLOT"
                (getvalue 'plotscale)
                (= "1" (getvalue 'AutoRotate))
                (= "1" (getvalue 'UpsideDown))
                )
			)
			(if (= "1" (getvalue 'Output))
                (doBatchLayout
                (orderFrames (getvalue 'SelectedFrames))
                (getvalue 'plotscale)
                (getvalue 'mslinescale)
                (= "1" (getvalue 'AutoRotate))
                (= "1" (getvalue 'UpsideDown))
                )
			)
			(if (= "2" (getvalue 'Output))
                (if (= "1" (getvalue 'PlotLayouts))
                    (doPlotLayouts)
                    (doBatchPlot
                    (orderFrames (getvalue 'SelectedFrames))
                    "FILE"
                    (getvalue 'plotscale)
                    (= "1" (getvalue 'AutoRotate))
                    (= "1" (getvalue 'UpsideDown))
                    )
                )
			)
			(if (= "3" (getvalue 'Output))
			    (doPlotLayoutsInTabOrder)
			)
			(if (= "4" (getvalue 'Output))
                (doBatchWblock
                (orderFrames (getvalue 'SelectedFrames))
                )
			)
        )
    )
)

(defun AutoLoadODclArx  (/ loaded fn v fnn)
    (if (not dcl-getversionex)
    (cond
      ((= "16" (setq v (itoa (atoi (getvar 'acadver)))))
       (if
      (setq fnn (findfile (setq fn (strcat "opendcl." v ".arx"))))
       (setq loaded (arxload fnn "1"))
       (setq loaded "2")
       )
       )
      ((or (= "17" v) (= "18" v) (= "19" v) (= "20" v) (= "21" v) (= "22" v) (= "23" v))
       (if (= "x86" (getenv "PROCESSOR_ARCHITECTURE"))
         (if (setq fnn
           (findfile (setq fn (strcat "opendcl." v ".arx"))))
        (setq loaded (arxload fnn "1"))
        (setq loaded "2")
        )
         (if (setq fnn
           (findfile (setq fn (strcat "opendcl.x64." v ".arx"))))
        (setq loaded
         (arxload fnn
            "1"))
        (setq loaded "2")
        )
         )
       )
      (t (Setq loaded "2"))
      )
      (setq loaded "3") ;_ �Ѽ���
      )
    (if (= "1" loaded)
      (progn
        (princ (strcat fn "����ʧ�ܣ������˳���"))
        (exit)
        )
      (if	(= "2" loaded)
        (progn
    (princ
      (strcat "δ�ҵ���Ӧ��\"" fn "\"�ļ��������˳���"))
    (exit)
    )
        )
      )
    loaded
)

(defun Load-odcl-project ( projname reload password alias / bytes rtype )
      (cond
          (	(null dcl-project-import)
              (princ "\n��OpenDCL 5.0 ���ϰ汾֧�֡�")
              nil
          )
          ( (and
  (setq bytes (vl-get-resource projname))
  (eq 'str (setq rtype (type bytes)))
  (not (eq "" bytes))
      )
            (dcl-project-import bytes password alias)
          )
    ((dcl-project-load (findfile projname) reload alias )  )
      )
)



  ;; ============================================
  ;; MAIN
  ;; ============================================

  ;(setvar "ctab" "Model")

  (setq acaddoc (vla-get-activedocument (vlax-get-acad-object)))
  (setq plot (vla-get-plot acaddoc))
  (setq clayout (vla-get-activelayout acaddoc))


(if (vl-bb-ref 'bbdcl)
(doplots)
(progn

  ;;(vla-put-paperunits clayout acMillimeters)

  (setq	catch (vl-catch-all-apply
		'vla-put-paperunits
		(list clayout acMillimeters)
	      )
  )

  (if (vl-catch-all-error-p catch)
    (progn
;      (alert
;	"���ô�ӡ���뵥λʱ���������޷�ʹ�õ�ǰ�Ĵ��ӡ���ã����ܴ�ӡ�������򲻴��ڣ����ߴ�ӡ��û�����ӣ����ߴ�ӡ��������������⡣\n\n������������ȷ��ҳ�����á�"
;      )
;      (vl-cmdf "_.pagesetup")
;      (while (/= "" (getvar "cmdnames")) (vl-cmdf pause))
      (vla-put-configname clayout "��")
      ;(vla-RefreshPlotDeviceInfo clayout)
      (vla-put-paperunits clayout acMillimeters)
    )
  )

  (setq BlockList (getblocklist))
  (setq LayerList (getLayerlist))


  (if (setq pathName (findfile "Batchplot.ini"))
    (setq default_dclvalues (cadr (assoc "Batchplot" (ConfigRead pathName nil))))
    (progn
      (alert
	    "ȱ��Batchplot.ini�����ļ���"
      )
      (exit)
    )
  )

  (setq	default_dclvalues (cons (cons 'NamePrefix (strcat (vl-filename-base (getvar "dwgname")) "_")) default_dclvalues))
  (setq	default_dclvalues (cons (cons 'NameExt "plt") default_dclvalues))
  (setq	default_dclvalues (cons (cons 'PlotFileFolder (getvar "dwgprefix")) default_dclvalues))
  (setq	default_dclvalues (cons (cons 'SelectedFrames nil) default_dclvalues))

  (setq	dclvalues default_dclvalues)
  (if (= "" (getvalue 'BlockName))
  (setvalue 'BlockName (car BlockList))
  )
  (if (= "" (getvalue 'LayerName))
  (setvalue 'LayerName (car LayerList))
  )
	;(dcl-project-load "Ltools.odcl" T)

  (AutoLoadODclArx)
  (Load-odcl-project "Ltools.odcl" nil nil nil)

  (setq intResult nil)
  (while (Not (or (= 2 intResult) (= 1 intResult)))
	(setq intResult (dcl-form-show Ltools/BatchPlot))
	(cond
		((= intResult 17)
			(vl-cmdf "_.DDPlotStamp")
			(while (/= "" (getvar "cmdnames")) (vl-cmdf pause))
		)
		((= intResult 11)
			(if (= "1" (getvalue 'Frame))
			(selectblock)
			(selectLayer)
			)
		)
		((= intResult 14)
            (vl-catch-all-apply 'GetFrames (list nil))
		)
		((= intResult 114)
            (vl-catch-all-apply 'GetBlockAtt nil)
		)
		((= intResult 115)
            (vl-catch-all-apply 'GetTextNum nil)
		)
		((= intResult 19)
			(vl-catch-all-apply
			'(lambda ()
			(doBatchPlot
			(orderFrames (getvalue 'SelectedFrames))
			"PREVIEW"
			(getvalue 'plotscale)
			(= "1" (getvalue 'AutoRotate))
			(= "1" (getvalue 'UpsideDown))
			)
			)
			nil
			)
		)
		((= intResult 15)
			(vl-catch-all-apply
			'(lambda ()
			(HighLightShow (getvalue 'SelectedFrames))
			(getstring "\nͼ�к�ɫ���������Ϊѡ�е�ͼ��<����>")
			)
			nil
			)
		)
		((= intResult 1)
			(if (= "0" (getvalue 'Output))
                (doBatchPlot
                (orderFrames (getvalue 'SelectedFrames))
                "PLOT"
                (getvalue 'plotscale)
                (= "1" (getvalue 'AutoRotate))
                (= "1" (getvalue 'UpsideDown))
                )
			)
			(if (= "1" (getvalue 'Output))
                (doBatchLayout
                (orderFrames (getvalue 'SelectedFrames))
                (getvalue 'plotscale)
                (getvalue 'mslinescale)
                (= "1" (getvalue 'AutoRotate))
                (= "1" (getvalue 'UpsideDown))
                )
			)
			(if (= "2" (getvalue 'Output))
                (if (= "1" (getvalue 'PlotLayouts))
                    (doPlotLayouts)
                    (doBatchPlot
                    (orderFrames (getvalue 'SelectedFrames))
                    "FILE"
                    (getvalue 'plotscale)
                    (= "1" (getvalue 'AutoRotate))
                    (= "1" (getvalue 'UpsideDown))
                    )
                )
			)
			(if (= "3" (getvalue 'Output))
			    (doPlotLayoutsInTabOrder)
			)
			(if (= "4" (getvalue 'Output))
                (doBatchWblock
                (orderFrames (getvalue 'SelectedFrames))
                )
			)
		)
	)
	)
)
)
  (vl-cmdf "_.redraw")
  (princ)
)



;;;=========================
;;;|    ���BatchPlot    |
;;;=========================
(defun c:BatchPlot (/ cmdecho sysvar)

	(defun *error* (msg)
		(princ (strcat "\n����" msg))
		(princ)
	)

  (if (< (atof (getvar "acadver")) 15.0)
    (progn
      (alert
	"�˳���ΪAutoCAD2000���ϵİ汾��ơ���֧��AutoCAD R14�����°汾��"
      )
      (exit)
    )
  )

  (setq cmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (vl-cmdf "_.undo" "_begin")
  ;; save and set system variables
  (setq	sysvar (LM:ChangeVars
		 '(("backgroundplot" . 0)
		   ("LayoutRegenCtl" . 0)
		   ("osmode" . 0)
		   ("dimzin" . 8)
		   ("highlight" . 1)
		   ("ucsicon" . 0)
		  )
	       )
  )

  ;(vl-cmdf "_.ucs" "_world")
  ;(vl-cmdf "_.ucs" "_view")
  ;; for debug version:
  ;;(batchplot)
  (vl-catch-all-apply 'batchplot nil)
  ;(vl-cmdf "_.ucs" "_p")
  ;(vl-cmdf "_.ucs" "_p")
  (LM:ChangeVars sysvar)
  (grtext)
  (vl-cmdf "_.undo" "_end")
  (setvar "cmdecho" cmdecho)
  (princ)
)

(defun c:BPlot () (c:batchplot))

;;����ȫ����Ϣ
(defun c:BpReset ()
	(vl-bb-set 'bbdcl nil)
	(princ)
)

(defun c:RemoveBplot (/ ReturnValue strpath path prefix fl l_str fr item att
                             removeSupportPath GetFileAtt SetFileAtt)
  (defun removeSupportPath (dir / tmp)
    (setq tmp "")
    (mapcar '(lambda (x)
      (if (/= (strcase x) (strcase dir))
        (setq tmp (strcat tmp x ";"))
      )
      )
      (LM:str->lst (getenv "ACAD") ";")
    )
    (setenv "ACAD" (substr tmp 1 (1- (strlen tmp))))
    (princ)
  )
  (defun GetFileAtt (fp / fso fl attr)
    (if  (and fp (/= "" fp))
      (progn
      (setq fso (vlax-create-object "Scripting.FileSystemObject"))
      (setq fl (vlax-invoke-method fso 'GetFile fp))
      (setq attr (vlax-get-property fl 'Attributes))
      (vlax-release-object fl)
      (vlax-release-object fso)
      )
    )
    attr
  )
  (defun SetFileAtt (fp flag / fso fl)
    (if  (and fp (/= "" fp) flag)
      (progn
      (setq fso (vlax-create-object "Scripting.FileSystemObject"))
      (setq fl (vlax-invoke-method fso 'GetFile fp))
      (vlax-put-property fl 'Attributes flag)
      (vlax-release-object fl)
      (vlax-release-object fso)
      )
    )
  )

  (setq ReturnValue (dcl-MessageBox "��ȷ��Ҫж����" "��ʾ" 3 1))
  (cond
  ((= ReturnValue 1)
    (if (and
      (setq strpath (findfile "Batchplot.vlx"))
      (setq path (vl-filename-directory strpath))
      (setq prefix (getenv "ACAD"))
      (vl-string-search path prefix))
      (removeSupportPath path)
    )

    (if (setq fl (findfile "acaddoc.lsp"))
      (progn
        (setq l_str nil)
        (setq fr (open fl "r"))
        (while (setq item (read-line fr))
        (setq l_str (append l_str (list item)))
        )
        (close fr)
        (setq l_str (vl-remove "(load \"Batchplot\" nil)" l_str))

        (if (setq fr (open fl "w"))
          (progn
            (foreach itm l_str (write-line itm fr))
            (close fr)
          )
          (progn
            (setq att (GetFileAtt fl))
            (SetFileAtt fl 0)
            (setq fr (open fl "w"))
            (foreach itm l_str (write-line itm fr))
            (close fr)
            (SetFileAtt fl att)
          )
        )
      )
      (princ "\nûacaddoc.lsp��")
    )
    (dcl-Project-Unload "Ltools")
    (setq c:Batchplot nil c:bplot nil c:RemoveBplot nil)
    (princ "\n��ж�أ�")
    (princ)
  )
  ((= ReturnValue 2)
  (princ)
  )
  )
)



(princ "\n������ӡ���� ����: BatchPlot �� BPlot ��ж������RemoveBplot")
(princ)
;;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;������ӡ����


;;;++++++++++++++++++++++++++++++++++++++++++++++++
;;;����������ʼ

;��������¼��ʼ
(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)
;��������¼����
(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)
;��ǰ��ĵ�
(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)
;�޸�ָ��ϵͳ��������������ǰֵ
(defun LM:ChangeVars (lst)
  (mapcar
    '(lambda (x / tmp var)
       (setq tmp
         (cons (car x)
           (if (= (type (setq var (getvar (car x)))) 'list)
             (list var)
             var
           )
         )
       )
       (setvar (car x)
	       (if (= (type (cdr x)) 'list)
           (cadr x)
           (cdr x)
	       )
       )
       tmp
     )
    lst
  )
)
;;�ַ���ת�б�
(defun LM:str->lst ( str del / len lst pos )
    (setq len (1+ (strlen del)))
    (while (setq pos (vl-string-search del str))
        (setq lst (cons (substr str 1 pos) lst)
              str (substr str (+ pos len))
        )
    )
    (reverse (cons str lst))
)
;;�б�ת�ַ���
(defun LM:lst->str ( lst del / str )
    (setq str (car lst))
    (foreach itm (cdr lst) (setq str (strcat str del itm)))
    str
)
;;ȥ���б�ָ��λ��
(defun LM:RemoveNth ( n l / i )
    (setq i -1)
    (vl-remove-if '(lambda ( x ) (= (setq i (1+ i)) n)) l)
)
;;ȥ���ַ�����λ�հ�
(defun LM:str-trimblank (sTrim / )
  (if (/= sTrim (setq sTrim (vl-string-trim "\t" (vl-string-trim " " sTrim))))
    (LM:str-trimblank sTrim)
    sTrim
    )
)
;ɾ���б��е�ָ����
(defun LM:RemoveItem (i l /)
  (vl-remove-if '(lambda (e) (equal e i)) l)
)
;;����б��������ظ�Ԫ��
(defun LM:Unique ( l / x r )
    (while l
        (setq x (car l)
              l (vl-remove x (cdr l))
              r (cons x r)
        )
    )
    (reverse r)
)

;;���ذ���ÿһ�������б��е�ָ������cdr(��Եĺ󲿷�)���б�
(defun LM:massoc (i l)
    (mapcar 'cdr
        (vl-remove-if-not '(lambda (x) (equal i (car x))) l)
    )
)

;;ѡ��ת��ΪͼԪ���б�
(defun LM:ss->ent ( ss / i l )
    (if ss
        (repeat (setq i (sslength ss))
            (setq l (cons (ssname ss (setq i (1- i))) l))
        )
    )
)

;;��ȡ�ض�ͼ����ض�����ֵ
(defun LM:vl-getattributevalue ( blk tag )
    (setq tag (strcase tag))
    (vl-some '(lambda ( att ) (if (= tag (strcase (vla-get-tagstring att))) (vla-get-textstring att)))
        (vlax-invoke blk 'getattributes)
    )
)

;;�ַ����滻������ƥ����ַ��������滻��
(defun LM:StringSubst ( new old str / inc len )
    (setq len (strlen new)
          inc 0
    )
    (while (setq inc (vl-string-search old str inc))
        (setq str (vl-string-subst new old str inc)
              inc (+ inc len)
        )
    )
    str
)

;;��������ʽƥ���ַ�
(defun LM:StrRegExpE (pat str key / *xvbsexp keys matches l)
  (if (not *xvbsexp)
    (setq *xvbsexp (vlax-get-or-create-object "VBScript.RegExp"))
  )
  (vlax-put *xvbsexp 'Pattern pat)
  (if (not key)
    (setq key "")
  )
  (setq key (strcase key))
  (setq        keys '(("I" "IgnoreCase")
               ("G" "Global")
               ("M" "Multiline")
              )
  )
  (mapcar
    '(lambda (x)
       (if (wcmatch key (strcat "*" (car x) "*"))
         (vlax-put *xvbsexp (read (cadr x)) 0)
         (vlax-put *xvbsexp (read (cadr x)) -1)
       )
     )
    keys
  )
  (setq matches (vlax-invoke *xvbsexp 'Execute str))
  (vlax-for x matches (setq l (cons (vla-get-value x) l)))
  (reverse l)
)

;;��������ʽ�滻�ַ�
(defun LM:StrRegExpR (pat str nstr key / *xvbsexp keys)
  (if (not *xvbsexp)
    (setq *xvbsexp (vlax-get-or-create-object "VBScript.RegExp"))
  )
  (vlax-put *xvbsexp 'Pattern pat)
  (if (not key)(setq key ""))
  (setq key (strcase key))
  (setq keys '(("I"  "IgnoreCase")("G"  "Global")("M"  "Multiline")))
  (mapcar '(lambda(x)
             (if (wcmatch key (strcat "*" (car x) "*"))
               (vlax-put *xvbsexp (read(cadr x)) 0)
               (vlax-put *xvbsexp (read(cadr x)) -1)
               ))
          keys)
  (vlax-invoke *xvbsexp 'replace str nstr)
)


;;���ܣ�����ʾ���ؼ��֡����˱�ѡ�����ʱ����ʾ���һ�������ѡ�����entsel
;;�÷���( LM:entsel  ��ʾ��Ϣ  �ؼ���  ���˱�  ѡ�����ʱ��ʾ)
;;������(LM:entsel  "\n��ѡ��һ��Բ��"  "A B C"   '((0 . "circle"))  "\n��ѡ���񲻷���Ҫ��������ѡ��")
;;˵�������˱���ssget�Ĺ��˱���ͬ
(defun LM:entsel (msg key fil ermsg / el ss)
	(while (and (setvar "errno" 0)
		(not (and (setq el (apply '(lambda (msg key) (initget key) (entsel msg)) (list msg key)))
			(if (= (type el) 'str)
			el
				(if (setq ss (ssget (cadr el) fil))
				ss
				(progn (princ ermsg) (setq ss nil))
				);if
			);if
		);and
		);not
		(/= (getvar "errno") 52)
		);and
	);while
	el
)

;��ѡ�б�
(defun Odcl:MultList (Title Caption lst / sellst
                      c:Ltools/MultList#OnInitialize
                      c:Ltools/MultList/TextButton1#OnClicked
                      c:Ltools/MultList/TextButton2#OnClicked)
  (defun c:Ltools/MultList#OnInitialize (/)
    (dcl-Control-SetTitleBarText Ltools/MultList Title)
    (dcl-Control-SetCaption Ltools/MultList/Label1 Caption)
    (dcl-ListBox-Clear Ltools/MultList/ListBox1)
    (dcl-ListBox-AddList Ltools/MultList/ListBox1 lst)
  )
  (defun c:Ltools/MultList/TextButton1#OnClicked (/)
    (if (> (dcl-ListBox-GetSelCount Ltools/MultList/ListBox1) 0)
      (progn
      (dcl-form-close Ltools/MultList)
      (setq sellst (dcl-ListBox-GetSelectedItems Ltools/MultList/ListBox1))
      )
      (dcl-MessageBox "������ѡ��һ����" "L��������ʾ" 2 3)
    )
  )
  (defun c:Ltools/MultList/TextButton2#OnClicked (/)
    (dcl-form-close Ltools/MultList)
  )
  (dcl-form-show Ltools/MultList)
  sellst
)

;��ѡ�б�
(defun Odcl:SingleList (Title Caption lst / sel
                      c:Ltools/SingleList#OnInitialize
                      c:Ltools/SingleList/TextButton1#OnClicked
                      c:Ltools/SingleList/TextButton2#OnClicked)
  (defun c:Ltools/SingleList#OnInitialize (/)
    (dcl-Control-SetTitleBarText Ltools/SingleList Title)
    (dcl-Control-SetCaption Ltools/SingleList/Label1 Caption)
    (dcl-ListBox-Clear Ltools/SingleList/ListBox1)
    (dcl-ListBox-AddList Ltools/SingleList/ListBox1 lst)
  )
  (defun c:Ltools/SingleList/TextButton1#OnClicked (/)
    (if (/= (dcl-ListBox-GetCurSel Ltools/SingleList/ListBox1) nil)
      (progn
      (dcl-form-close Ltools/SingleList)
      (setq sel (dcl-ListBox-GetItemText Ltools/SingleList/ListBox1 (dcl-ListBox-GetCurSel Ltools/SingleList/ListBox1)))
      )
      (dcl-MessageBox "��ѡ��" "L��������ʾ" 2 3)
    )
  )
  (defun c:Ltools/SingleList/TextButton2#OnClicked (/)
    (dcl-form-close Ltools/SingleList)
  )
  (dcl-form-show Ltools/SingleList)
  sel
)


;;;++++++++++++++++++++++++++++++++++++++++++++++++
;;;������������