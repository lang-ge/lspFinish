(vl-load-com)
(defun c:268 (/ osmode_old zyd ang #def kw) 
  (setvar "cmdecho" 0) ;关闭回显
  (setq osmode_old (getvar "OSMODE")) ;捕捉设置
  (setvar "osmode" 1) ;关闭对象捕捉
  (prompt "\n此程序用于轴测图标注!")
  (prompt "\n指定第一条尺寸界线原点!")
  (command "_.DIMALIGNED" pause pause pause "")
  (while (setq zyd (entlast)) 
    (setq ang (angle (cdr (assoc 14 (entget zyd))) (cdr (assoc 10 (entget zyd)))))
    (if (not #def) (setq #def "30"))
    (initget "30 -30")
    (setq kw (getkword (acet-str-format "选择倾斜角度 [30/-30] <%1>: " #def)))
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
    (prompt "\n★请继续选择或退出！")
    (command "_.DIMALIGNED" pause pause pause "")
  )
  (setvar "OSMODE" osmode_old)
  (princ) ;静默退出
)
;;--------------------------------------------------------