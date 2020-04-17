;;; World���Ϳ��ʼ��!
(defun jinn-get-word-Lib () 
  (setq office:dir "C:\\Program Files (x86)\\Microsoft Office\\")
  (cond 
    ((setq mwlib (findfile (strcat office:dir "office11\\" "MSWORD.olb"))))
    ((setq mwlib (findfile (strcat office:dir "office12\\" "MSWORD.olb"))))
    (t (setq mwlib nil))
  )
  (if mwlib 
    (vlax-import-type-library :tlb-filename mwlib :methods-prefix "wd-" 
                              :properties-prefix "wp-" :constants-prefix "wm-"
    )
    (alert "Word typelib �ļ�������")
  )
)
;;; excel���Ϳ��ʼ��!
(if (equal exc-xlScalelinear nil)
  (vlax-import-type-library
    :tlb-filename
    "C:/Program Files (x86)/Microsoft Office/Office12/Excel.exe"
    :methods-prefix
    "exm-"
    :properties-prefix
    "exp-"
    :constants-prefix
    "exe-"
   )
)

;;; ��word��������
(setq myword (vlax-get-object "word.application.12"))

;;; ��excel��������
(setq myexcel (vlax-get-object "excel.application.12"))

;;; �ȼ�����word��ʵ������
(setq myword (vlax-get-or-create-object "word.application.12"))
(vla-put-Visible myword :vlax-true)

;;; ֱ�ӽ���word��ʵ��
(setq myword (vlax-create-object "word.application.12"))

vlax-read-enabled-p ;�ж϶����Ƿ���Ա���ȡ
vlax-write-enabled-p ;�ж϶����Ƿ���Ա�����
vlax-erased-p ;�ж϶����Ƿ��Ѿ���ɾ��
(vla-put-color vla-circle acred);���Ͳ�������"acred"