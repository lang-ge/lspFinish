(vl-load-com)
(defun c:268 (/ osmode_old zyd ang #def kw) 
  (setvar "cmdecho" 0) ;�رջ���
  (setq osmode_old (getvar "OSMODE")) ;��׽����
  (setvar "osmode" 1) ;�رն���׽
  (prompt "\n�˳����������ͼ��ע!")
  (prompt "\nָ����һ���ߴ����ԭ��!")
  (command "_.DIMALIGNED" pause pause pause "")
  (while (setq zyd (entlast)) 
    (setq ang (angle (cdr (assoc 14 (entget zyd))) (cdr (assoc 10 (entget zyd)))))
    (if (not #def) (setq #def "30"))
    (initget "30 -30")
    (setq kw (getkword (acet-str-format "ѡ����б�Ƕ� [30/-30] <%1>: " #def)))
    (if (or (not kw) (equal kw "")) 
      (setq kw #def)
      (setq #def kw)
    )
    (cond 
      ((= kw "30")
       (command "_.dimedit" "O" zyd "" (angtos (+ ang 0.523599) 0 2) "")
      )
      ((= kw "-30")
       (command "_.dimedit" "O" zyd "" (angtos (- ang 0.523599) 0 2) "")
      )
    )
    (setq zyd nil)
    (prompt "\n�������ѡ����˳���")
    (command "_.DIMALIGNED" pause pause pause "")
  )
  (setvar "OSMODE" osmode_old)
  (princ) ;��Ĭ�˳�
)
;;--------------------------------------------------------