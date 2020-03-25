;;;获取指定驱动器的序列号
(setq FSO (vlax-create-object "scripting.FileSystemObject"))
(vlax-get (vlax-invoke FSO 'GetDrive "c:") 'SerialNumber)
(vlax-invoke FSO 'BuildPath "d:\\aa" "abc")
(vlax-invoke FSO 'CopyFolder "E:\\snippets" "d:\\")
;;获取驱动器列表
(defun GetDrive	(n / d)
  (vlax-for x (vlax-get FSO 'Drives) (setq d (cons x d)))
  (nth n (reverse d))
)

;;获取驱动器的序列号作为机器码
(vlax-get (GetDrive 1) 'SerialNumber)

;;获取驱动器路径列表
(setq dr '())
(vlax-for x (vlax-get FSO 'Drives)(setq dr (cons(vlax-get x 'Path) dr)))

;;查看 folder 对象的属性和方法
(vlax-dump-object (vlax-invoke FSO 'GetFile "e:\\snippets\\autolisp.json") t)

;;示例17
;;调用 (fnSum "d:\\dc")
;;返回 (11446 2266)
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
