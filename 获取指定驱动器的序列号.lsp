;;;��ȡָ�������������к�
(setq FSO (vlax-create-object "scripting.FileSystemObject"))
(vlax-get (vlax-invoke FSO 'GetDrive "c:") 'SerialNumber)
(vlax-invoke FSO 'BuildPath "d:\\aa" "abc")
(vlax-invoke FSO 'CopyFolder "E:\\snippets" "d:\\")
;;��ȡ�������б�
(defun GetDrive	(n / d)
  (vlax-for x (vlax-get FSO 'Drives) (setq d (cons x d)))
  (nth n (reverse d))
)

;;��ȡ�����������к���Ϊ������
(vlax-get (GetDrive 1) 'SerialNumber)

;;��ȡ������·���б�
(setq dr '())
(vlax-for x (vlax-get FSO 'Drives)(setq dr (cons(vlax-get x 'Path) dr)))

;;�鿴 folder ��������Ժͷ���
(vlax-dump-object (vlax-invoke FSO 'GetFile "e:\\snippets\\autolisp.json") t)

;;ʾ��17
;;���� (fnSum "d:\\dc")
;;���� (11446 2266)
(defun fnSum(p / F Filesum Foldsum)
    (setq F (vlax-create-object "scripting.FileSystemObject")
        Filesum 0 Foldsum 0)
    (defun getsum(FD / FDs)
        (setq FDs (vlax-get FD 'SubFolders)
            Filesum (+ Filesum (vlax-get (vlax-get FD 'Files) 'count))
            Foldsum (+ Foldsum (vlax-get FDs 'count)))
        (vlax-for x FDs (getsum x))
    )
(if (= -1 (vlax-invoke F 'FolderExists p))(getsum (vlax-invoke F 'GetFolder p)))
(vlax-release-object F)
(list Filesum Foldsum)
)
