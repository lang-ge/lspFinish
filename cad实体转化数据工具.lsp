;;;功能：数据表格式转化 
(defun _fixdxfdata (elst) 
  (vl-remove-if 
    '(lambda (pair) (member (car pair) '(5 6 8 102 330)))
    elst
  )
)
;;; cad实体数据转化
(defun c:11 (/ ss i lis ent str) 
  (setq ss (ssget))
  (setq i   0
        lis '()
  )
  (repeat (sslength ss) 
    (setq ent (entget (ssname ss i)))
    (setq str (vl-prin1-to-string (cdr (_fixdxfdata ent))))
    (setq lis (cons str lis))
    (setq i (1+ i))
  )
  (setq dwg  (vl-filename-base (getvar "DWGNAME")) ; 取当前文档名
        path (getvar "dwgprefix") ; 取当前文档路径
  )
  (setq filename (strcat path dwg ".txt"))
  (setq fn (open filename "w"))
  (print lis fn)
  (close fn)
  (princ)
)

;;; ==================================================*====================
;;; 选择集的中点
(defun P_mpt (ss / shulist pt1 pt2 px1 py1 px2 py2 mx my mpt) 
  (setq shulist (minmm_ssbox ss))
  (setq pt1 (car shulist))
  (setq pt2 (cadr shulist))
  (setq px1 (car pt1)) ; 取得pt1坐标的x值
  (setq py1 (cadr pt1)) ; 取得pt1坐标的y值
  (setq px2 (car pt2)) ; 取得pt2坐标的x值
  (setq py2 (cadr pt2)) ; 取得pt2坐标的y值
  (setq mx (/ (+ px1 px2) 2)) ; 求得mx值
  (setq my (/ (+ py1 py2) 2)) ; 求得my值
  (setq mpt (list mx my)) ; 结合mx & my成mpt点
)
;;; ==================================================*
;;; 实体生成块返回块名
(defun P_block (ss /) 
  (PRINC "\n将实体做成图块:")
  (command "_UCS" "W")
  (setq mpt (P_mpt ss)) ;取得待操作实体
  (setq bname (ITOA (FIX (* (GETVAR "TDINDWG") 100000000))))
  (command "_.BLOCK" bname mpt ss "") ;生成块bb
  (command "_UCS" "P")
)

;;; ==================================================*
;;; 实体数据生成小马cad实体块
(defun c:22 (/ ents SS) 
  (setq ents (MAPCAR 
               'entmakex
               (MAPCAR 
                 'READ
                 '("cad数据")
               )
             )
  )
  (setq SS (ssadd))
  (foreach N ents 
    (ssadd N SS)
  )

  (P_block SS) ;生成块
  (vl-cmdf "_.INSERT" bname mpt "" "" "") ;插入块
  (princ bname)
)