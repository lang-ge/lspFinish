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
  (setvar "cmdecho" 0)			;�رջ���
  (setq osmode_old (getvar "OSMODE"))	;��׽����
  (setvar "osmode" 1)			;�رն���׽
  (prompt "\n�˳����������ͼ��ע!")
  (prompt "\nָ����һ���ߴ����ԭ��!")
  (command "_.DIMALIGNED" pause pause pause "")
  (while (setq zyd (entlast))
    (if	(< (car (cdr (assoc 13 (entget zyd))))
	   (car (cdr (assoc 14 (entget zyd))))
	)
      (command "_.dimedit" "O" zyd "" "30" "")
      (command "_.dimedit" "O" zyd "" "150" "")
    )
    (setq zyd nil)
    (prompt "\n�������ѡ����˳���")
    (command "_.DIMALIGNED" pause pause pause "")
  )
  (setvar "OSMODE" osmode_old)
  (princ)				;��Ĭ�˳�
)
;;--------------------------------------------------------