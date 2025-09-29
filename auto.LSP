
;;;部分代码转自mjtd
;;; 〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓
;;; 判断是否加载本文件
(if (car (atoms-family 1 '("vl-load-com")))
  (vl-load-com)
  (progn
    (Alert
      "这个程序集是为AutoCAD 2000以及更高的版本设计的，有些功能有可能在没有Visual Lisp for R14支持的AutoCAD R14上不能正确地运行。"
    )
    (exit)				; 版本不符，退出加载。
  )
)

;;; 〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓
;;; 取得本程序的路径
(defun GetPath (AppID)
  (vl-registry-read
    (strcat
      "HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\"
      AppID
      "_is1"
    )
    "Inno Setup: App Path"
  )
)

(defun GetmylyPath () (GetPath "图层管理中心"))

;;; 〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓
;;; 解析字符串为表
(defun strParse	(Str Delimiter / SearchStr StringLen return n char)
  (setq SearchStr Str)
  (setq StringLen (strlen SearchStr))
  (setq return '())
  (while (> StringLen 0)
    (setq n 1)
    (setq char (substr SearchStr 1 1))
    (while (and (/= char Delimiter) (/= char ""))
      (setq n (1+ n))
      (setq char (substr SearchStr n 1))
    )
    (setq return (cons (substr SearchStr 1 (1- n)) return))
    (setq SearchStr (substr SearchStr (1+ n) StringLen))
    (setq StringLen (strlen SearchStr))
  )
  (reverse return)
)

;;; 〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓
;;; 反解析表为字符串
(defun StrUnParse (Lst Delimiter / return)
  (setq return "")
  (foreach str Lst
    (setq return (strcat return Delimiter str))
  )
  (substr return 2)
)

;;; 〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓
;;; 移除支持文件搜索路径
(defun QF_RemoveSupportPath (PathToRemove / supportlist)
  (setq supportlist (strparse (getenv "ACAD") ";"))
  (setq supportlist (vl-remove "" supportlist))
  (setq	supportlist
	 (vl-remove-if
	   '(lambda (x) (= (strcase x) (strcase PathToRemove)))
	   supportlist
	 )
  )
  (setenv "ACAD" (strUnParse supportlist ";"))
)

;;; 〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓
;;; 添加支持文件搜索路径
;;; 第二个参数如果为真, 插最前，否则插最后

(defun AddSupportPath (PathToAdd isFirst / supportlist)
  (QF_RemoveSupportPath PathToAdd)
  (setq supportlist (strparse (getenv "ACAD") ";"))
  (setq supportlist (vl-remove "" supportlist))
  (if isFirst
    (setq supportlist (cons PathToAdd supportlist))
    (setq supportlist (append supportlist (list PathToAdd)))
  )
  (setenv "ACAD" (strUnParse supportlist ";"))
)

;;; 〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓
;;; 加载菜单
(defun load-mymenu (/ n)
  (if (not (menugroup "mlayer"))
    (progn (setq n 1)
	   (while (< n 24)
	     (if (menucmd (strcat "P" (itoa n) ".1=?"))
	       (setq n (+ n 1))
	       (progn (if (> n 3)
			(setq n (- n 2))
			(setq n 3)
		      )
		      (command "menuload" "tuceng")
		      (menucmd (strcat "p" (itoa n) "=+mlayer.pop111"))
		      (setq n 25)
	       )
	     )
	   )
    )
  )
  (princ)
)

;;; 〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓
;;; 初始化主函数
(defun Init_mylayer ()
  (if (not (menugroup "mlayer"))	; 如果菜单组还没有被加载，则加载之
    (progn (AddSupportPath (GetmylyPath) nil) ; 添加支持路径
	   (AddSupportPath (strcat (GetmylyPath) "\\LISP") nil)
	   (AddSupportPath (strcat (GetmylyPath) "\\LIB") nil)
	   (AddSupportPath (strcat (GetmylyPath) "\\BIN") nil)
	   (load-mymenu)
    )
  )
  (princ)
)

;;; 〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓〓
;;; 主程序：
(princ "\n加载图层管理中心菜单……")

(setq cmdecho_save (getvar "cmdecho"))
(setvar "cmdecho" 0)

;;; 执行初始化
(Init_mylayer)

(setvar "cmdecho" cmdecho_save)
(setq cmdecho_save nil)

(princ "\n图层管理中心菜单 α版 加载完毕。")
(princ)
