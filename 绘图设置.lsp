(DEFUN C:VVV (/ *DIMLFAC*) 
  ;;; 设置测量单位比例因子
  (defun *DIMLFAC* (/ custy) 
    (vl-load-com)
    (setq custy (vla-get-name 
                  (vla-get-ActiveDimStyle 
                    (vla-get-activedocument (vlax-get-acad-object))
                  )
                )
    )
    (if (GETVAR "DIMLFAC") (SETVAR "DIMLFAC" 1))
    (vl-cmdf "._DIMSTYLE" "S" custy "y")
    (princ)
  ) 
  ;;; (command "style" "standard" "PMingLiU-ExtB" "0" "0.8" "0" "" "" "")
  (PRINC "\n系统变量设定!!!")
  (*DIMLFAC*)
  (c:eea)
  (C:V3)
  (setvar "ATTDIA" 0)
  (SETVAR "FILEDIA" 1)
  (SETVAR "polarmode" 6)
  (SETVAR "polaraddang" "30.0;150.0;210.0;330.0")
  (SETVAR "MIRRTEXT" 0)
  (SETVAR "CMDDIA" 1)
  (SETVAR "DIMTOH" 0)
  (SETVAR "OSMODE" 4799)
  (SETVAR "AUTOSNAP" 63)
  (PRINC "\nOK!!")
  (PRINC)
)

;;; 文字界外对齐切换
(defun C:V1 (/ i ss sn dim1) 
  ;;; (setq *DOC* (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setvar "DIMTOH" (if (= 0 (getvar "DIMTOH")) 1 0))
  (setq i  0
        ss (ssget)
        sn (sslength ss)
  )
  (while (> sn i) 
    (setq dim1 (vlax-ename->vla-object (ssname ss i)))
    (vla-put-TextOutsideAlign 
      dim1
      (if (= (vla-get-TextOutsideAlign dim1) :vlax-true) 
        :vlax-false
        :vlax-true
      )
    )
    (vla-Update dim1)
    (setq i (1+ i))
  )
    ;;; (vla-Regen *DOC* acActiveViewport)
)

;;; 设置标注全局比例因子
(defun C:V2 (/ custy stc date) 
  (vl-load-com)
  (setq custy (vla-get-name 
                (vla-get-ActiveDimStyle 
                  (vla-get-activedocument (vlax-get-acad-object))
                )
              )
  )
  (setq stc (getvar "DIMSCALE"))
  (setq date (getreal (strcat "\n输入 DIMSCALE 的新值 <" (rtos stc 2 1) ">:")))
  (if (not date) 
    (setq date stc)
  )
  (SETVAR "DIMSCALE" date)
  (vl-cmdf "._DIMSTYLE" "S" custy "y")
  (princ)
)

;;; 设置文字样式
(defun C:V3 (/ *Acment* mkstyle)
  (vl-load-com)
  (setq *Acment* (vla-get-activedocument (vlax-get-acad-object)))
  (defun mkstyle (owner ys zt Height Width / Styles style a)
    (or	(= (type (setq Styles (vlax-get-property owner 'TextStyles)
		       style  (VL-CATCH-ALL-APPLY
				'vlax-invoke-method
				(List Styles 'item ys)
			      )
		 )
	   )

	   'vla-object
	)
	(setq style (vlax-invoke-method Styles "add" ys))
    )
    (vla-SetFont style zt :vlax-false :vlax-false 0 0)
    (vla-put-Height style Height)
    (vla-put-Width style Width)
  )
  (mkstyle (vlax-get-property (vlax-get-acad-object) 'activedocument)
	   "SLDTEXTSTYLE0"
	   "宋体"
	   0
	   0.75
  )
  (mkstyle (vlax-get-property (vlax-get-acad-object) 'activedocument)
	   "CHINESE"
	   "宋体"
	   0
	   0.75
  )
  (mkstyle (vlax-get-property (vlax-get-acad-object) 'activedocument)
	   "standard"
	   "宋体"
	   0
	   0.75
  )
  (vla-Regen *Acment* :vlax-true) ;再生文档
  (princ)
)

;;; 图纸空间切换模型空间
(DEFUN c:V4 (/ OBJ shulist pt1 pt2) 
  (vl-load-com)
  (if (/= (setq OBJ (ssget "_x")) nil) 
    (setq shulist (minmm_ssbox OBJ)
          pt1     (car shulist)
          pt2     (cadr shulist)
    )
  )
  (vl-cmdf "_.Mview" pt1 pt2)
  (vl-cmdf "_.chspace" OBJ "")
  (princ)
)

;;; ***********************************************
;;; 时间播报
(defun C:V5 (/ datea date0 date1) 
  (SETVAR "ATTREQ" 1)
  (setq datea (rtos (getvar "cdate") 2 0))
  (setq date0 (menucmd "M=$(edtime,$(getvar,date),hh:mm)"))
  (cond 
    ((< "00:00" date0 "09:00") (setq date1 "早上"))
    ((< "09:00" date0 "12:00") (setq date1 "上午"))
    ((< "12:00" date0 "13:30") (setq date1 "中午"))
    ((< "13:30" date0 "18:30") (setq date1 "下午"))
    ((< "18:30" date0 "23:30") (setq date1 "晚上"))
    (t nil)
  )
  (SayIt (strcat "现在是" date1 date0))
  (princ)
)
;;; 155 [功能] 文本朗读
(defun SayIt (Phrase$ / Sapi) 
  (setq Sapi (vlax-create-object "Sapi.SpVoice"))
  (vlax-invoke Sapi "Speak" Phrase$ 0)
  (vlax-release-object Sapi)
  (princ)
)

;;; ***********************************************
;;; 打印机切换dos_clipboard
(defun C:V6 (/ olddynmode prna *WBEM* SVR str key)
  (vl-load-com)
  (setvar "cmdecho" 0)
  (setq olddynmode (getvar "dynmode"))
  (if (/= 1 olddynmode)
    (setvar "dynmode" 1)
  )
  (setq prna (defultprint))
  (setq *WBEM* (vlax-create-object "WbemScripting.SWbemLocator"))
  (setq SVR (vlax-invoke *WBEM* 'ConnectServer))
  (vlax-for obj (vlax-invoke SVR 'InstancesOF "Win32_Printer") 
    ;;(vlax-dump-object obj T)
    (if (= (vlax-get obj 'Default) -1) 
      (setq str (vlax-get obj 'Status))
    )
  )
  (if (= str "OK")
    (progn
      (initget "1 2 3")
      (setq key (getkword (acet-str-format "\n检测到打印机为暂停状态![恢复打印(1)/放弃^-^(2)/查看队列(3)]/<恢复打印>:")))
      (cond 
        ((eq key "1")
         (startapp (strcat "rundll32 printui.dll,PrintUIEntry /Xs /n \"" prna "\" status Resume"))
        )
        ((eq key "2")
         (princ "程序退出!")
        )
        ((eq key "3")
         (startapp (strcat "rundll32 printui.dll,PrintUIEntry /o /n \"" prna "\""))
        )
      )
    )
    (progn
      (initget "1 2 3")
      (setq key (getkword (acet-str-format "\n检测到打印机为恢复状态![暂停打印(1)/放弃^-^(2)/查看队列(3)]/<暂停打印>:")))
      (cond 
        ((eq key "1")
         (startapp (strcat "rundll32 printui.dll,PrintUIEntry /Xs /n \"" prna "\" status Pause"))
        )
        ((eq key "2")
         (princ "程序退出!")
        )
        ((eq key "3")
         (startapp (strcat "rundll32 printui.dll,PrintUIEntry /o /n \"" prna "\""))
        )
      )
    )
  )
  (vlax-release-object SVR)
  (vlax-release-object *WBEM*)
  (setvar "dynmode" olddynmode)
  (princ)
)
;;;生成透明命令
;;; (vlax-add-cmd "V1" 'V1 "V1" 1)
;;; (princ)

;;; 透明命令撤销
;;; (defun c:V4 () 
;;;   (vlax-remove-cmd "V1")
;;; )
