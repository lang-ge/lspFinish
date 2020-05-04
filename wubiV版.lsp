;cad输入法自动切换程序
(vl-load-reactors)
(vl-load-com) 
;加密注册程序
(setq DWX (vlax-get-or-create-object "DynamicWrapperX"))

;;; (vlax-invoke DWX 'Register "user32" "LoadKeyboardLayout" "i=sl" "r=q")
;;; (vlax-invoke DWX 'LoadKeyboardLayout "00000409" 1) ;;切换到英文
;;; (vlax-invoke DWX 'LoadKeyboardLayout "00000804" 1) ;;切换到中文
;;; 以上为通用功能
;;; 注意：上面代码的第二行和第三行分别将返回输入法布局标识的"指针"为下面的代码服务

;;; 也可以直接用下面的 API 函数来测试键盘布局的指针
;;; (vlax-invoke DWX 'Register "user32" "GetKeyboardLayout" "i=q" "r=q")
;;; (vlax-invoke DWX 'GetKeyboardLayout 0) ;;手动切换一次输入法，再运行一次

(vlax-invoke DWX 'Register "user32" "ActivateKeyboardLayout" "i=ql" "r=q")
(vlax-invoke DWX 'ActivateKeyboardLayout 67699721 1)  ;;切换到英文(67699721为指针)
(if (not mouse_reactor) (setq mouse_reactor (vlr-mouse-reactor nil '((:vlr-beginDoubleClick . test11)))))
(defun test11(a b)
	(vlax-invoke DWX 'ActivateKeyboardLayout 134481924 1) 
)
;;;设计的输入切换命令
;;;x参数命令集合
(defun mlh(c)
	(setq mljh '("_EDD" "FIND" "MTEDIT" "DDEDIT" "TEXT" "TEXTEDIT" "MTEXT" "_saveas"))
	(setq mljh1 (mapcar '(lambda (x) (= (car c) x)) mljh))
	(apply 'or mljh1)
	)
;cad命令输入反应器
(if (not command_reactor)(setq command_reactor
	(vlr-command-reactor NIL '((:vlr-commandWillStart . Test_12)))
))
(defun test_12(a b)
	(if (mlh b)	
			(progn (vlax-invoke DWX 'ActivateKeyboardLayout 134481924 1) 
				)
	(princ)    
	))
;;取消CAD命令反应器
(if (not command_reactor3)(setq command_reactor3
	(vlr-command-reactor nil '((:vlr-commandCancelled . Test2)))
)) 

(defun Test2(a b)
	(if (mlh b)			
			(progn (vlax-invoke DWX 'ActivateKeyboardLayout 67699721 1) 
				 )
	(princ)
    
	))
;;;无法完成CAD命令反应器
(if (not command_reactor2)(setq command_reactor2
	(vlr-command-reactor nil '((:vlr-commandFailed . Test5)))
)) 

(defun Test5(a b)
	(if (mlh b)		
			(progn (vlax-invoke DWX 'ActivateKeyboardLayout 67699721 1) 
				 )
	(princ)
    
	))
;;;cad命令完成反应器
(if (not command_reactor1)(setq command_reactor1
	(vlr-command-reactor NIL '((:vlr-commandEnded . Test3)))
))
(defun test3(a b)
	(if (not (= (car b) "QSAVE"))
		(progn (vlax-invoke DWX 'ActivateKeyboardLayout 67699721 1);ctrl+shift+1为中文，ctrl+shift+0为英文
		)
	)
	;(vlax-invoke shell "SendKeys" "+^0")
	(princ)
)  
                           
;;; (defun c:bangzhu()
;;; 	(alert "输入法设置ctrl+shift+1为中文，ctrl+shift+0为英文")
;;; )
