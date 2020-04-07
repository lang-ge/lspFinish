;; -----------------------== {增量数组} == ----------------------;;
;; ;;
;;该程序将在增加;;的同时排列对象的选择。
;; ;;中的注释对象中找到的任何数字内容
;;选择。 ;;
;; ;;
;;该程序具有两种操作模式：标准和动态。 ;;
;;标准命令：'incarray'将不显示动态预览。
;;但又会比动态版本运行得更快，更流畅；
;; -尝试进行以下操作时，这种差异尤其明显；
;;排列大量对象。 ;;
;; ;;
;;动态模式：“ incarrayd”将显示;;的预览。
;;鼠标在屏幕上拖动时排列的对象。但是；
;;由于用于生成此预览的方法，该模式仅为;;
;;适用于使用该程序排列少量对象的情况。 ;;
;; ;;
;;启动程序后，提示用户指定;;。
;;增加值，然后提示您选择对象；
;;排列。此选择可以包括带有;;的任何图形对象。
;;视口除外。 ;;
;; ;;
;;在进行有效选择之后，用户应指定一个基点；
;;和相对于基点的数组向量。角度和长度
;;向量的方向将确定;;的方向和密度。
;;分别排列向量越短，数组越密集。 ;;
;; ;;
;;现在可以通过在;;上拖动鼠标来生成数组。
;;屏幕，直到阵列达到所需的大小。如果对象；
;;选择包括文本，多行文字，属性定义，尺寸，;;
;;或Multileader对象，在文本中找到的任何数字数据；
;;这些对象的内容将由;;自动增加。
;;给定增量值，将对象排列在一起。 ;;
;; ----------------------------------------------------------------------;;
;;作者：Lee Mac，版权所有?2014-www.lee-mac.com ;;
;; ----------------------------------------------------------------------;;
;;版本1.0-2011-07-27 ;;
;; ;;
;; -首次发布。 ;;
;; ----------------------------------------------------------------------;;
;;版本1.1-2011-07-29 ;;
;; ;;
;; -通过向置换表达式添加位移标记来修复UCS错误。 ;;
;;感谢Swamp用户HighflyingBird找到了这个。 ;;
;; ----------------------------------------------------------------------;;
;;版本1.2-2011-07-29 ;;
;; ;;
;; -添加了程序的非动态版本。 ;;
;; -增加了增加属性定义标签，提示和文本的功能。 ;;
;; -改进的增量功能可保留所有前导和;;
;;尾随零。 ;;
;; ----------------------------------------------------------------------;;
;;版本1.3-2011-08-05 ;;
;; ;;
;; -增加了排列所有对象的功能，而不仅仅是注释对象。 ;;
;;选择中包含数字;;的任何注释对象。
;;数据仍将增加。 ;;
;; -数组化属性块中的属性现在增加。 ;;
;; -MLeader文字和尺寸替代文字会增加。 ;;
;; ----------------------------------------------------------------------;;
;;版本1.4-2011-09-30 ;;
;; ;;
;; -修复了在锁定图层上排列属性时的错误。 ;;
;; ----------------------------------------------------------------------;;
;;版本1.5-2014-04-13 ;;
;; ;;
;; -程序完全重写。 ;;
;; -增加了提示输入增量值。 ;;
;; ----------------------------------------------------------------------;;
;;版本1.6-2014-04-13 ;;
;; ;;
;; -修复了AutoCAD 2006中存在的变量名冲突和grvecs错误；
;;如Swamp用户CAB所报道-非常感谢。 ;;
;; ----------------------------------------------------------------------;;
;;版本1.7-2014-06-07 ;;
;; ;;
;; -更正了累积的舍入误差在;;之后出现在44；
;;将值1递增1。
;; ----------------------------------------------------------------------;;

(defun c:t1  nil (LM:incarray nil)) ;; Standard version
(defun c:t2 nil (LM:incarray  t )) ;; Dynamic  version

;;----------------------------------------------------------------------;;

(defun LM:incarray ( dyn / *error* bpt dim dis ept inc lst obl qty tmp vxu vxw )

    (defun *error* ( msg )
        (if (= 'int (type dim))
            (setvar 'dimzin dim)
        )
        (foreach obj obl
            (if (and (= 'vla-object (type obj)) (not (vlax-erased-p obj)) (vlax-write-enabled-p obj))
                (vla-delete obj)
            )
        )
        (incarray:endundo (incarray:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (redraw) (princ)
    )

    (if (not (and (setq inc (getenv "LMac\\incarray")) (setq inc (distof inc))))
        (setq inc 1)
    )
    (if (setq tmp (getreal (strcat "\n指定增量 <" (incarray:num->str inc) ">: ")))
        (setenv "LMac\\incarray" (incarray:num->str (setq inc tmp)))
    )
    (incarray:startundo (incarray:acdoc))
    (setq dim (getvar 'dimzin))
    (setvar 'dimzin 0)
    (cond
        (   (not
                (and
                    (setq lst (incarray:selection->list (ssget "_:L" '((0 . "~VIEWPORT")))))
                    (setq bpt (getpoint "\n指定基点: "))
                    (progn
                        (while
                            (and
                                (setq vxu (getpoint "\n指定数组向量: " bpt))
                                (equal bpt vxu 1e-8)
                            )
                            (princ "\n无效的向量数组.")
                        )
                        vxu
                    )
                    (setq vxu (mapcar '- vxu bpt)
                          vxw (trans vxu 1 0 t)
                          dis (distance '(0.0 0.0 0.0) vxw)
                    )
                )
            )
        )
        (   dyn
            (princ "\n指定数组终点: ")
            (while (= 5 (car (setq ept (grread t 13 0))))
                (redraw)
                (foreach obj obl (vla-delete obj))
                (setq qty (/ (caddr (trans (mapcar '- (cadr ept) bpt) 1 vxw t)) dis)
                      obl (incarray:copyvector lst (mapcar (if (minusp qty) '- '+) vxw) (abs (fix qty)) inc)
                )
                (grvecs (list -3 bpt (mapcar '(lambda ( a b ) (+ (* a qty) b)) vxu bpt)))
            )
        )
        (   (setq ept (getpoint bpt "\n指定数组终点: "))
            (setq qty (fix (/ (caddr (trans (mapcar '- ept bpt) 1 vxw t)) dis)))
            (incarray:copyvector lst (mapcar (if (minusp qty) '- '+) vxw) (abs (fix qty)) inc)
        )
    )
    (setvar 'dimzin dim)
    (incarray:endundo (incarray:acdoc))
    (redraw) (princ)
)

;;----------------------------------------------------------------------;;

(defun incarray:num->str ( x / dim rtn )
    (if (equal x (atof (rtos x 2 0)) 1e-8)
        (rtos x 2 0)
        (progn
            (setq dim (getvar 'dimzin))
            (setvar 'dimzin 8)
            (setq rtn (vl-catch-all-apply 'rtos (list x 2 15)))
            (setvar 'dimzin dim)
            (if (not (vl-catch-all-error-p rtn)) rtn)
        )
    )
)

;;----------------------------------------------------------------------;;

(defun incarray:copyvector ( lst vec qty inc / cnt obj obl org )
    (setq org (vlax-3D-point 0 0)
          cnt 1
    )
    (repeat qty
        (foreach itm lst
            (setq obj (vla-copy (car itm))
                  obl (cons obj obl)
            )
            (vla-move obj org (vlax-3D-point (mapcar '* vec (list cnt cnt cnt))))
            (if (= "AcDbBlockReference" (vla-get-objectname obj))
                (mapcar
                    (function
                        (lambda ( att prp )
                            (vl-catch-all-apply 'vlax-put-property
                                (list att (car prp)
                                    (apply 'strcat
                                        (mapcar '(lambda ( x ) (incarray:increment x (* cnt inc)))
                                            (cdr prp)
                                        )
                                    )
                                )
                            )
                        )
                    )
                    (vlax-invoke obj 'getattributes)
                    (cdr itm)
                )
                (foreach prp (cdr itm)
                    (vlax-put-property obj (car prp)
                        (apply 'strcat
                            (mapcar '(lambda ( x ) (incarray:increment x (* cnt inc)))
                                (cdr prp)
                            )
                        )
                    )
                )
            )
        )
        (setq cnt (1+ cnt))
    )
    obl
)

;;----------------------------------------------------------------------;;

(defun incarray:selection->list ( sel / idx lst obj obn )
    (if sel
        (repeat (setq idx (sslength sel))
            (setq obj (vlax-ename->vla-object (ssname sel (setq idx (1- idx))))
                  obn (vla-get-objectname obj)
            )
            (if (and (= "AcDbBlockReference" obn) (= :vlax-true (vla-get-hasattributes obj)))
                (setq lst
                    (cons
                        (cons obj
                            (mapcar '(lambda ( a ) (vl-list* 'textstring (incarray:splitstring (vla-get-textstring a))))
                                (vlax-invoke obj 'getattributes)
                            )
                        )
                        lst
                    )
                )
                (setq lst
                    (cons
                        (cons obj
                            (mapcar '(lambda ( p ) (vl-list* p (incarray:splitstring (vlax-get-property obj p))))
                                (cond
                                    (   (wcmatch obn "AcDb*Text,AcDbMLeader") '(textstring))
                                    (   (wcmatch obn "AcDb*Dimension")        '(textoverride))
                                    (   (= "AcDbAttributeDefinition" obn)     '(tagstring promptstring textstring))
                                )
                            )
                        )
                        lst
                    )
                )
            )
        )
    )
)

;;----------------------------------------------------------------------;;

(defun incarray:splitstring ( str / lst )
    (setq lst (vl-string->list str))
    (read (vl-list->string (vl-list* 40 34 (incarray:split lst (< 47 (car lst) 58)))))
)

;;----------------------------------------------------------------------;;

(defun incarray:split ( lst flg )
    (cond
        (   (null lst) '(34 41))
        (   (= 92 (car lst))
            (if flg
                (vl-list* 34 32 34 92 92 (incarray:split (cdr lst) nil))
                (vl-list* 92 92 (incarray:split (cdr lst) flg))
            )
        )
        (   (or (< 47 (car lst) 58) (and (= 46 (car lst)) flg (< 47 (cadr lst) 58)))
            (if flg
                (vl-list* (car lst) (incarray:split (cdr lst) flg))
                (vl-list* 34 32 34 (car lst) (incarray:split (cdr lst) t))
            )
        )
        (   flg (vl-list* 34 32 34 (car lst) (incarray:split (cdr lst) nil)))
        (   (vl-list* (car lst) (incarray:split (cdr lst) nil)))
    )
)

;;----------------------------------------------------------------------;;

(defun incarray:increment ( str inc / dci dcs len num )
    (if (numberp (read str))
        (progn
            (setq num (+ (distof str) inc)
                  inc (incarray:num->str inc)
                  str (vl-string-left-trim "-" str)
                  inc (vl-string-left-trim "-" inc)
                  dci (incarray:decimalplaces inc)
                  dcs (incarray:decimalplaces str)
                  len (strlen str)
                  str (vl-string-left-trim "-" (rtos num 2 (max dci dcs)))
            )
            (cond
                (   (< 0 dcs) (setq len (+ (- len dcs) (max dci dcs))))
                (   (< 0 dci) (setq len (+ dci len 1)))
            )
            (repeat (- len (strlen str))
                (setq str (strcat "0" str))
            )
            (if (minusp num)
                (strcat "-" str)
                str
            )
        )
        str
    )
)

;;----------------------------------------------------------------------;;

(defun incarray:decimalplaces ( str / pos )
    (if (setq pos (vl-string-position 46 str))
        (- (strlen str) pos 1)
        0
    )
)

;;----------------------------------------------------------------------;;

(defun incarray:startundo ( doc )
    (incarray:endundo doc)
    (vla-startundomark doc)
)

;;----------------------------------------------------------------------;;

(defun incarray:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;;----------------------------------------------------------------------;;

(defun incarray:acdoc nil
    (eval (list 'defun 'incarray:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (incarray:acdoc)
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: IncArray.lsp | Version 1.7 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: \"incarray\" - Standard | \"incarrayd\" - Dynamic ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;