; �����Ѿ�ʵ�����������ܣ�����������������һ�¾ͺ��ˣ�
;���ļ�Ŀ¼����λ�ļ�
(startapp "explorer" "/select,C:\\Windows\\notepad.exe")

;�򿪱��ļ���·����ʱ������Ѿ��򿪹��ˣ��͵��������û�У��ʹ򿪣�
(command "shell" "start d:\\")

; ��Ҫ��Ч���������������������ٴ��ļ��У�ʼ��ֻ��һ��Ŀ¼������λ���ļ�



;;;�����������ʵ��
(defun test ( ext / shell)
  (vl-load-com)
  (setq shell (vlax-create-object "shell.application"))
  (vlax-invoke shell 'open ext)
  (vlax-release-object shell)
)

;;(test "C:\\Windows")

(SETQ path (strcat (GETVAR "DWGPREFIX") "pdf" "\\"))
(startapp "explorer" "/select,E:\\֣��\\���ɿƼ�\\DEV\\20200529\\CQJ001-004-01A ������ͼ.dwg")

(if path (startapp "explorer" (strcat "/open," path)))