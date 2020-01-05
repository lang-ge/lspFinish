;;;计算11~999中的数满足m,m2,m3均为回文数
;;;测试：(rev)
(defun rev (/ a dd)
  (setq	a  "" dd 11 )
  (while (<= dd 1000) ;while循环开始    
    (if	(and (= (reve (ITOA dd)) (ITOA dd))
	     (= (reve (ITOA (expt dd 2))) (ITOA (expt dd 2)))
	     (= (reve (ITOA (expt dd 3))) (ITOA (expt dd 3)))
	)
      (setq a (strcat a (ITOA dd) ","))
    )
    (setq dd (1+ dd))
  ) ;while循环结束
  (PRINC a)
)


;;;==================================================*
;;;[功能] 字符串倒序
;;;测试：(reve (getstring "输入数字 "))
(defun reve (s / n a d1)
  (setq n (strlen s))
  (setq a "")
  (repeat n
    (setq d1 (chr (vl-string-elt s (1- n))))
    (setq a (strcat a d1))
    (setq n (1- n))
  )
  a
)






































































































































































































































































































































































































































































































































































































































































































































































































