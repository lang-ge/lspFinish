;;;���ܣ����ݱ��ʽת�� 
(defun _fixdxfdata (elst) 
  (vl-remove-if 
    '(lambda (pair) (member (car pair) '(5 6 8 102 330)))
    elst
  )
)
;;; cadʵ������ת��
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
  (setq dwg  (vl-filename-base (getvar "DWGNAME")) ; ȡ��ǰ�ĵ���
        path (getvar "dwgprefix") ; ȡ��ǰ�ĵ�·��
  )
  (setq filename (strcat path dwg ".txt"))
  (setq fn (open filename "w"))
  (print lis fn)
  (close fn)
  (princ)
)

;;; ==================================================*====================
;;; ѡ�񼯵��е�
(defun P_mpt (ss / shulist pt1 pt2 px1 py1 px2 py2 mx my mpt) 
  (setq shulist (minmm_ssbox ss))
  (setq pt1 (car shulist))
  (setq pt2 (cadr shulist))
  (setq px1 (car pt1)) ; ȡ��pt1�����xֵ
  (setq py1 (cadr pt1)) ; ȡ��pt1�����yֵ
  (setq px2 (car pt2)) ; ȡ��pt2�����xֵ
  (setq py2 (cadr pt2)) ; ȡ��pt2�����yֵ
  (setq mx (/ (+ px1 px2) 2)) ; ���mxֵ
  (setq my (/ (+ py1 py2) 2)) ; ���myֵ
  (setq mpt (list mx my)) ; ���mx & my��mpt��
)
;;; ==================================================*
;;; ʵ�����ɿ鷵�ؿ���
(defun P_block (ss /) 
  (PRINC "\n��ʵ������ͼ��:")
  (command "_UCS" "W")
  (setq mpt (P_mpt ss)) ;ȡ�ô�����ʵ��
  (setq bname (ITOA (FIX (* (GETVAR "TDINDWG") 100000000))))
  (command "_.BLOCK" bname mpt ss "") ;���ɿ�bb
  (command "_UCS" "P")
)

;;; ==================================================*
;;; ʵ����������С��cadʵ���
(defun c:22 (/ ents SS) 
  (setq ents (MAPCAR 
               'entmakex
               (MAPCAR 
                 'READ
                 '("cad����")
               )
             )
  )
  (setq SS (ssadd))
  (foreach N ents 
    (ssadd N SS)
  )

  (P_block SS) ;���ɿ�
  (vl-cmdf "_.INSERT" bname mpt "" "" "") ;�����
  (princ bname)
)