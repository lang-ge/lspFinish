;;; Function library

;;;VsCode辅助函数A
;;; ***************************************************************************
;; ▓ (Sendcommand)
;; [功能] 辅助联接VScode与CAD通信
;; [参数] 绝对路径
;; [返回] 无
;; [测试]1.(Sendcommand FileName)
;;; (defun Sendcommand (FileName / F str nm nu en wcm myacad document) 
;;;   (vl-load-com)
;;;   (if (setq F (open FileName "r")) 
;;;     (progn 
;;;       (while (and (setq str (read-line F)) (not wcm)) 
;;;         (if (wcmatch (strcase str T) "*c:*") 
;;;           (setq nm  (vl-string-search "c:" str)
;;;                 nu  (- (vl-string-search " (" str) nm)
;;;                 en  (substr str (+ nm 3) (- nu 2))
;;;                 wcm T
;;;           )
;;;         )
;;;       )
;;;       (close F)
;;;     )
;;;   )
;;;   (if (not en) 
;;;     (setq en "No command found.")
;;;   )
;;;   (setq myacad   (Vlax-get-acad-object) ;;获取AutoCAD应用程序本身
;;;         document (Vla-get-activedocument myacad) ;;;获取活动文档
;;;   )
;;;   (vla-sendcommand document (strcat "(load " "\"" FileName "\"" ")"))
;;;   (princ (princ "\n★命令: ^" en "^ 输入继续-^o^-^o^"))
;;;   ;;; (vla-sendcommand document en)
;;;   (princ)
;;; )

;;;VsCode辅助函数B
;;; ***************************************************************************
;; ▓ (Sendcommand)
;; [功能] 辅助联接VScode与CAD通信
;; [参数] 绝对路径
;; [返回] 无
;; [测试]1.(Sendcommand FileName)
;;; (defun Sendcommand (FileName /)  ;;VsCode辅助函数
;;;   (vla-sendcommand (vlax-get-property (vlax-get-acad-object) 'activedocument) 
;;;                    (strcat "(load " "\"" FileName "\"" ")")
;;;   )
;;;   (princ)
;;; )

;;;通用函数A
;;; ***************************************************************************
;; ▓ (minmm_ss->enlist)
;; [功能] 把变体数据转化为表
;; [参数] 选择集
;; [返回] 表
;; [测试]1.(minmm_ss->enlist ss)
(defun minmm_ss->enlist (ss / lst n en) 
  (setq n -1)
  (while (setq en (ssname ss (setq n (1+ n)))) 
    (setq lst (cons en lst))
  )
)
;;; (defun minmm_objBox (obj / minpoint maxpoint) 
;;;   (vla-GetBoundingBox obj 'minpoint 'maxpoint)
;;;   ;取得包容图元的最大点和最小点
;;;   (setq minpoint (vlax-safearray->list minpoint)) ;把变体数据转化为表
;;;   (setq maxpoint (vlax-safearray->list maxpoint)) ;把变体数据转化为表
;;;   (setq minpoint (trans minpoint 0 1))
;;;   (setq maxpoint (trans maxpoint 0 1))
;;;   (setq obj (list minpoint maxpoint)) ;;;组合成点表
;;; )
;;; 取得包容图元的最大点和最小点
(defun minmm_objBox (obj / Minp Maxp) 
  (vla-GetBoundingBox obj 'Minp 'Maxp)
  (mapcar 'vlax-safearray->list (list Minp Maxp))
)
(defun minmm_ssbox (ss / boxlst maxlst minlst objlst) 
  (vl-load-com)
  (setq objlst (mapcar 'vlax-ename->vla-object (minmm_ss->enlist ss)))
  (setq boxlst (mapcar 'minmm_objBox objlst))
  (setq minlst (mapcar 'car boxlst))
  (setq maxlst (mapcar 'cadr boxlst))
  (list 
    (apply 'mapcar (cons 'min minlst))
    (apply 'mapcar (cons 'max maxlst))
  )
)
;;;通用函数A结束

;;;通用函数B
;;; ***************************************************************************
;; ▓ (lt:ss-entnext)
;; [功能] 获取在图元 en 之后产生的图元的选择集
;; [参数] en----图元名
;; [返回] 选择集
;; [测试]1.(setq en (entlast))
;;         执行创建图元的命令，如 LINE,BOUNDARY
;;         (setq ss (lt:ss-entnext en))
;;       2.(setq ss (lt:ss-entnext (car(entsel))))
(defun lt:ss-entnext (en / ss) 
  (if en 
    (progn 
      (setq ss (ssadd))
      (while (setq en (entnext en)) 
        (if 
          (not 
            (member (cdr (assoc 0 (entget en))) 
                    '("ATTRIB" "VERTEX" "SEQEND")
            )
          )
          (ssadd en ss)
        )
      )
      (if (zerop (sslength ss)) (setq ss nil))
      ss
    )
    (ssget "_x")
  )
)
;;;通用函数B结束

;;;通用函数C
;;; ***************************************************************************
;; ▓ (MJ:GetAttributes)
;; [功能] 获取图块 ent 之后产生的属性
;; [参数] ent----图块名
;; [返回] 属性
;; [测试]1.(setq shujlb (MJ:GetAttributes (car (entsel))))
;;       2.(setq sheji  (nth 1 (nth 0 shujlb) );;;0为块第一个属性
(defun MJ:GetAttributes (ent / lst) 
  (setq *Obj2En* vlax-vla-object->ename)
  (if 
    (safearray-value 
      (setq lst (vlax-variant-value 
                  (vla-getattributes 
                    (vlax-ename->vla-object ent)
                  )
                )
      )
    )
    (mapcar 
      '(lambda (x) 
         (list 
           (vla-get-tagstring x)
           (vla-get-textstring x)
           (*Obj2En* x)
         )
       )
      (vlax-safearray->list lst)
    )
  )
)

;;;通用函数
;;; ***************************************************************************
;; ▓ (ZL-TXTFILE-READTOLIST)
;; [功能] 读取文件并转换为表
;; [参数] 文件名路径
;; [返回] 表
;; [测试](ZL-TXTFILE-READTOLIST "D:\\TEST.TXT")
;;; (defun ZL-TXTFILE-READTOLIST (TXTFILE / LST_JG F TMP) 
;;;   (setq LST_JG '())
;;;   (if (setq F (open TXTFILE "r")) 
;;;     (progn 
;;;       (while (setq TMP (read-line F)) 
;;;         (setq LST_JG (cons TMP LST_JG))
;;;       )
;;;       (close F)
;;;     )
;;;   )
;;;   ;;返回
;;;   (reverse LST_JG)
;;; )

;;;通用函数(文本类)
;;; ***************************************************************************
;; ▓ (fn-lst)
;; [功能] 将文本文件转换为字符串数据表
;; [参数] file----文件名(带路径)
;; [返回] 字符表
;; [测试](fn-lst "note_bak.ini")
(defun fn-lst (file / fn ftext lst) 
  (if (setq fn (open (findfile file) "r")) 
    (progn 
      (while (setq ftext (read-line fn)) 
        (setq lst (append lst (list ftext)))
      )
      (close fn)
    )
    (alert "文件写入失败~~ 请检查目标文件夹的管理权限!")
  )
  lst
)

;; [功能] 将字符串数据表转换为文本文件
(defun lst-fn (file lst mod / fn) 
  (setq fn (open file mod))
  (foreach x lst 
    (if (/= 'str (type x)) 
      (setq x (vl-prin1-to-string x)) ;表转化为字符串函数
    )
    (write-line x fn)
  )
  (close fn)
)

;; [功能] 将数据表加入DCL列表框
(defun lst-dcl (key lst Mod No /) 
  (start_list key Mod No)
  (mapcar 'add_list lst)
  (end_list)
)

;; [功能] 删除任意指定行的文本内容
(defun DelLine (file NumberOfLine / cnt line lst f) 
  (setq lst '()
        cnt 0
  )
  (setq f (open file "r"))
  (while (setq line (read-line f)) 
    (setq cnt (1+ cnt))
    (if (/= cnt NumberOfLine) 
      (setq lst (append lst (list line)))
    )
  )
  (close f)
  (setq f (open file "w"))
  (foreach atm lst (write-line atm f))
  (close f)
)

;; [功能] 替换任意指定行的文本内容
(defun RelLine (file NumberOfLine new_line / cnt line lst f) 
  (setq lst '()
        cnt 0
  )
  (setq f (open file "r"))
  (while (setq line (read-line f)) 
    (setq cnt (1+ cnt))
    (if (/= cnt NumberOfLine) 
      (setq lst (append lst (list line)))
      (setq lst (append lst (list new_line)))
    )
  )
  (close f)
  (setq f (open file "w"))
  (foreach atm lst (write-line atm f))
  (close f)
)

;;;通用函数
;;; ***************************************************************************
;; ▓ (vs-f-path)
;; [功能] 转换文件路径字符格式
;; [参数] 字符串
;; [返回] 格式后字符串
;; [测试]1.(vs-f-path string)
(Defun vs-f-path (string /) 
  (while (vl-string-search "\\" string) 
    (setq string (vl-string-subst "/" "\\" string))
  )
  string
)

;;;通用函数
;;; ***************************************************************************
;; ▓ (SET-CLIP-STRING)
;; [功能] 将文字写入系统剪贴板
;; [参数] 字符串
;; [返回] 无
;; [测试]1.(SET-CLIP-STRING STR)
(defun SET-CLIP-STRING (STR / HTML RESULT) 
  (and 
    (= (type STR) 'STR)
    (setq HTML (vlax-create-object "htmlfile"))
    (setq RESULT (vlax-invoke 
                   (vlax-get (vlax-get HTML 'PARENTWINDOW) 'CLIPBOARDDATA)
                   'SETDATA
                   "Text"
                   STR
                 )
    )
    (vlax-release-object HTML)
  )
)

;;;通用函数
;;; ***************************************************************************
;; ▓ (GET-CLIP-STRING)
;; [功能] 从系统剪贴板读取字符串
;; [参数] 无
;; [返回] 无
;; [测试]1.(GET-CLIP-STRING)
(defun GET-CLIP-STRING (/ HTML RESULT) 
  (and (setq HTML (vlax-create-object "htmlfile")) 
       (setq RESULT (vlax-invoke 
                      (vlax-get (vlax-get HTML 'PARENTWINDOW) 
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
;;;通用函数
;;; ***************************************************************************
;; ▓ (cs_pross)
;; [功能] 进度条
;; [参数] 总数/单数
;; [返回] 进度显示
;; [测试](cs_pross to I)
(defun cs_pross (to I / cs_txt myi) 
  ;;; (setq cs_txt "██████████████")
  (setq cs_txt ">>>>>>>>>>>>>>>>>>>>>>>>>>>>")
  (setq myi    (fix (/ (* (strlen cs_txt) I) to))
        cs_txt (substr cs_txt 1 myi)
  )
  (grtext -2 cs_txt)
)
;;; cs_pross函数"添加耗时功能"
;;; (setq stime (getvar "date"))
;;; (cs_pross to I);函数置中
;;; (setq etime (getvar "date"))
;;; (setq Htime (* 86400.0 (- (- etime stime) (fix (- etime stime)))))
;;; (grtext -2 (strcat "任务完成，耗时" (rtos Htime 2 2) "秒..."))

;;;通用函数
;;; ***************************************************************************
;; ▓ (Scr_pross)
;; [功能] 滚动进度条
;; [参数] 单数
;; [返回] 滚动进度显示
;; [测试](Scr_pross I)
(defun Scr_pross (I / cs_txt) 
  (setq cs_txt '("█" "██" "███" "████" "█████" "██████" "███████" "████████" 
                 "█████████" "██████████" "███████████" "████████████" "█████████████" 
                 "██████████████"
                )
  )
  (grtext -2 (nth (rem (1+ I) 14) cs_txt)) ;滚动进度条核心代码
)

;;;通用函数
;;; ***************************************************************************
;; ▓ (disableCtrls)
;; [功能] 控件函数
;; [参数] keylst
;; [返回] 无
;; [测试](disableCtrls '("T3" "T4"))

  ;;灰显控件
(defun disableCtrls (keylst / key) 
  (foreach key keylst 
    (mode_tile key 1)
  )
)
  ;;激活控件
(defun EnableCtrls (keylst / key) 
  (foreach key keylst 
    (mode_tile key 0)
  )
)

;;;通用函数
;;; ***************************************************************************
;; ▓ (setq t0 (get-utime))
;; [功能] 时间函数
;; [参数] 无
;; [返回] 当前时间,秒
;; [测试](setq t0 (get-utime))
;;; (defun get-utime ()
;;;   (* 86400 (getvar "tdusrtimer"))
;;; )
;;; (getvar "date")


;;;通用函数
;;; ***************************************************************************
;;;本程序是南充郑友东整理 2015年6月13日
;;ssPts: 1 选择集，返回排序后选择集
;;Key: "xyzXYZ"任意组合,例如"yX",y在前表示y坐标优先，小y表示从小到大(注:二维点时，不能有z)
;;FUZZ: 允许误差
;;示例 (HH:ssPts:Sort zd "XYZ" 3.5) ;返回排序后的选择集<Selection set: fb3>
(defun HH:ssPts:Sort (ssPts KEY FUZZ / E EN FUN LST N SORTPTS SORTSS) 
  ;;1 选择集图元排序
  (defun sortSS (PTS FUN F FUZZ) 
    (vl-sort pts 
             '(lambda (a b) 
                (if (not (equal (F (car a)) (F (car b)) fuzz)) 
                  (fun (F (car a)) (F (car b)))
                )
              )
    )
  )
  ;;2 排序
  (defun sortSS1 (myfun PTS KEY FUZZ) 
    (setq Key (vl-string->list Key))
    (foreach xyz (reverse Key) 
      (cond 
        ((< xyz 100)
         (setq fun >)
         (setq xyz (nth (- xyz 88) (list car cadr caddr)))
        )
        (T
         (setq fun <)
         (setq xyz (nth (- xyz 120) (list car cadr caddr)))
        )
      )
      (setq Pts (myfun Pts fun xyz fuzz))
    )
  )
  ;;3 本程序主程序
  (cond 
    ((= (type ssPts) 'PICKSET)
     (repeat (setq n (sslength ssPts)) 
       (if 
         (and (setq e (ssname ssPts (setq n (1- n)))) 
              (setq en (entget e))
         )
         (setq lst (cons (cons (cdr (assoc 10 en)) e) lst))
       )
     )
     (setq SS (ssadd))
     (foreach CN (mapcar 'cdr (sortSS1 sortSS lst KEY FUZZ)) 
       (if (= (type CN) 'ENAME) 
         (ssadd CN SS)
       )
     )
     (if (= (sslength SS) 0) 
       NIL
       SS
     )
    )
  )
)

;;;通用函数
;;; ***************************************************************************
;; ▓ (defultprint)
;; [功能] 从注册表获取默认打印机
;; [参数] 无
;; [返回] 默认打印机
;; [测试](defultprint)
(defun defultprint (/ device)
 (vl-load-com)
 (substr (setq device (vl-registry-read "HKEY_CURRENT_USER\\Software\\Microsoft\\Windows NT\\CurrentVersion\\Windows" "Device"))
         1 (vl-string-search "," device)
 )
)

(load "wubi.lsp") ;;; 加载输入法切换程式