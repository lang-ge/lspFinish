;cad���뷨�Զ��л�����
(vl-load-reactors)
(vl-load-com) 
;����ע�����
(setq DWX (vlax-get-or-create-object "DynamicWrapperX"))

;;; (vlax-invoke DWX 'Register "user32" "LoadKeyboardLayout" "i=sl" "r=q")
;;; (vlax-invoke DWX 'LoadKeyboardLayout "00000409" 1) ;;�л���Ӣ��
;;; (vlax-invoke DWX 'LoadKeyboardLayout "00000804" 1) ;;�л�������
;;; ����Ϊͨ�ù���
;;; ע�⣺�������ĵڶ��к͵����зֱ𽫷������뷨���ֱ�ʶ��"ָ��"Ϊ����Ĵ������

;;; Ҳ����ֱ��������� API ���������Լ��̲��ֵ�ָ��
;;; (vlax-invoke DWX 'Register "user32" "GetKeyboardLayout" "i=q" "r=q")
;;; (vlax-invoke DWX 'GetKeyboardLayout 0) ;;�ֶ��л�һ�����뷨��������һ��

(vlax-invoke DWX 'Register "user32" "ActivateKeyboardLayout" "i=ql" "r=q")
(vlax-invoke DWX 'ActivateKeyboardLayout 67699721 1)  ;;�л���Ӣ��(67699721Ϊָ��)
(if (not mouse_reactor) (setq mouse_reactor (vlr-mouse-reactor nil '((:vlr-beginDoubleClick . test11)))))
(defun test11(a b)
	(vlax-invoke DWX 'ActivateKeyboardLayout 134481924 1) 
)
;;;��Ƶ������л�����
;;;x���������
(defun mlh(c)
	(setq mljh '("_EDD" "FIND" "MTEDIT" "DDEDIT" "TEXT" "TEXTEDIT" "MTEXT" "_saveas"))
	(setq mljh1 (mapcar '(lambda (x) (= (car c) x)) mljh))
	(apply 'or mljh1)
	)
;cad�������뷴Ӧ��
(if (not command_reactor)(setq command_reactor
	(vlr-command-reactor NIL '((:vlr-commandWillStart . Test_12)))
))
(defun test_12(a b)
	(if (mlh b)	
			(progn (vlax-invoke DWX 'ActivateKeyboardLayout 134481924 1) 
				)
	(princ)    
	))
;;ȡ��CAD���Ӧ��
(if (not command_reactor3)(setq command_reactor3
	(vlr-command-reactor nil '((:vlr-commandCancelled . Test2)))
)) 

(defun Test2(a b)
	(if (mlh b)			
			(progn (vlax-invoke DWX 'ActivateKeyboardLayout 67699721 1) 
				 )
	(princ)
    
	))
;;;�޷����CAD���Ӧ��
(if (not command_reactor2)(setq command_reactor2
	(vlr-command-reactor nil '((:vlr-commandFailed . Test5)))
)) 

(defun Test5(a b)
	(if (mlh b)		
			(progn (vlax-invoke DWX 'ActivateKeyboardLayout 67699721 1) 
				 )
	(princ)
    
	))
;;;cad������ɷ�Ӧ��
(if (not command_reactor1)(setq command_reactor1
	(vlr-command-reactor NIL '((:vlr-commandEnded . Test3)))
))
(defun test3(a b)
	(if (not (= (car b) "QSAVE"))
		(progn (vlax-invoke DWX 'ActivateKeyboardLayout 67699721 1);ctrl+shift+1Ϊ���ģ�ctrl+shift+0ΪӢ��
		)
	)
	;(vlax-invoke shell "SendKeys" "+^0")
	(princ)
)  
                           
;;; (defun c:bangzhu()
;;; 	(alert "���뷨����ctrl+shift+1Ϊ���ģ�ctrl+shift+0ΪӢ��")
;;; )
