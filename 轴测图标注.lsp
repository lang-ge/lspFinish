(setq zyd (ssget))
(if (< (car (cdr (assoc 13 (entget (ssname zyd 0)))))
       (car (cdr (assoc 14 (entget (ssname zyd 0)))))
    )
  (command "_.dimedit" "O" zyd "" "30" "")
  (command "_.dimedit" "O" zyd "" "150" "")
)



(command "_.DIMALIGNED" pause pause pause "")
(while (setq zyd (entlast))
  (if (< (car (cdr (assoc 13 (entget zyd))))
	 (car (cdr (assoc 14 (entget zyd))))
      )
    (command "_.dimedit" "O" zyd "" "30" "")
    (command "_.dimedit" "O" zyd "" "150" "")
  )
  (command "_.DIMALIGNED" pause pause pause "")
)


(vl-load-com)
(defun c:dall (/ osmode_old zyd)
  (setvar "cmdecho" 0)			;关闭回显
  (setq osmode_old (getvar "OSMODE"))	;捕捉设置
  (setvar "osmode" 1)			;关闭对象捕捉
  (prompt "\n此程序用于轴测图标注!")
  (prompt "\n指定第一条尺寸界线原点!")
  (command "_.DIMALIGNED" pause pause pause "")
  (while (setq zyd (entlast))
    (if	(< (car (cdr (assoc 13 (entget zyd))))
	   (car (cdr (assoc 14 (entget zyd))))
	)
      (command "_.dimedit" "O" zyd "" "30" "")
      (command "_.dimedit" "O" zyd "" "150" "")
    )
    (setq zyd nil)
    (prompt "\n★请继续选择或退出！")
    (command "_.DIMALIGNED" pause pause pause "")
  )
  (setvar "OSMODE" osmode_old)
  (princ)				;静默退出
)
;;--------------------------------------------------------