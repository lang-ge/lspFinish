;;; ѡ���ļ��Ի���
(defun c:257 (/ fna fh op) 

  ;��ʼ��һ�����Դӵ�һ�γ����лָ���������������
  ; load_testʧ�ܲ����·���������.

  (if (not Acet:Acadinfo-Olderr) 
    (setq Acet:Acadinfo-Olderr *error*)
  ) ;if

  (defun *error* (msg /) 
    (if Acet:Acadinfo-Error-On-Load-Test 
      (progn 
        (if fh 
          (close fh)
        ) ;if
        (setq *error*              Acet:Acadinfo-Olderr
              Acet:Acadinfo-Olderr nil
        ) ;setq
        (c:acadinfo)
        (setq Acet:Acadinfo-Error-On-Load-Test nil)
      ) ;progn
      (princ msg)
    ) ;if
  ) ;defun *error*


  ;; ��ʾ��һ���ı��ļ���д����Ϣ
  (if (not Acet:Acadinfo-Error-On-Load-Test) 
    (progn 
      (setq docDir (getvar "MYDOCUMENTSPREFIX"))
      (setq initFile (strcat docDir "\\acadinfo"))
      (setq fna (getfiled "Select File Name" initFile "txt" 33))
    )
  )


  (if Acet:Acadinfo-Error-On-Load-Test 
    (setq op "a")
    (progn 
      (textscr)
      (princ 
        (strcat "\n" "\nACADINFO is a utility for gathering information about" 
                "\nyour AutoCAD installation and current setup. The routine" 
                "\nwill examine your system and write a text file called" "\n\'" fna "\'" 
                "\nto your hard drive."
        ) ;strcat
      ) ;princ
      (getstring "\nPress ENTER to continue or ESC to cancel... ")

      (princ "\n\nExamining your AutoCAD setup. Please wait...\n")
      (setq op "w")
    ) ;progn then
  ) ;if

  (if (setq fh (open fna op)) 
    (progn 
      (close fh)
      (setq fh (open fna op)) ;�رգ�Ȼ���ٴδ�
      ;����ָ��е��������ա�

      (if (not Acet:Acadinfo-Error-On-Load-Test) 
        (progn 

          (princ "\Performing load tests...")

          (acet-acadinfo-do-header fh)

          (acet-acadinfo-do-general fh)

          (acet-acadinfo-do-express fh)

          (acet-acadinfo-do-fileloads fh)

          (write-line "Tests for successful load of LISP initialization files." fh)
          (write-line (acet-acadinfo-test-load "acad2008.lsp") fh)
          (write-line (acet-acadinfo-test-load "acad2008doc.lsp") fh)
          (write-line (acet-acadinfo-test-load "acettest.fas") fh)
          (write-line (acet-acadinfo-test-load "acetutil.fas") fh)
          (write-line (acet-acadinfo-test-load "acetmain.mnl") fh)
        ) ;progn then
        (progn 
          (write-line "" fh)
          (write-line "*****FAILURE during lisp file load tests.**** " fh)
          (write-line "One of the following files causes an error on load: " fh)
          (write-line "  acad2008.lsp" fh)
          (write-line "  acad2008doc.lsp" fh)
          (write-line "  acettest.fas" fh)
          (write-line "  acetutil.fas" fh)
          (write-line "  acetmain.mnl" fh)
        ) ;progn else
      ) ;if

      (write-line "" fh)
      (write-line (strcat "(arx) -> " (acet-acadinfo-item-to-string (arx))) fh)
      (write-line "" fh)

      (write-line " ------------------------- TYPELIB TEST -------------------------" 
                  fh
      )
      (write-line "" fh)
      (acet-acadinfo-check-typelib fh)
      (write-line "" fh)


      (write-line " ------------------- SYSTEM VARIABLE SETTINGS -------------------" 
                  fh
      )
      (write-line "|;" fh)

      (close fh)

      (acet-acadinfo-vars-to-scr fna -1) ;append

      (acet-acadinfo-lisp-dump fna)
      (princ "\nDone.")
    ) ;progn then
    (princ "\nCannot open file for write.")
  ) ;if

  (setq *error* olderr)

  (princ)
)

;; �÷���(qf_getFolder msg)
;; ���ӣ�(qf_getFolder "ѡ���ļ���:")
;; ����ֵ���ַ������ļ���·�����������cancel, ����nil
(defun qf_getFolder (msg / WinShell shFolder path catchit) 
  (vl-load-com)
  (setq winshell (vlax-create-object "Shell.Application"))
  (setq shFolder (vlax-invoke-method WinShell 'BrowseForFolder 0 msg 1))
  (setq catchit (vl-catch-all-apply 
                  '(lambda () 
                     (setq shFolder (vlax-get-property shFolder 'self))
                     (setq path (vlax-get-property shFolder 'path))
                   )
                )
  )
  (if (vl-catch-all-error-p catchit) 
    nil
    path
  )
)

;;���ṩ"ѡ���ļ���"��������,���Expresstool
(arxload "acetutil.arx" NIL)
(setq path (strcat 
             (strcase 
               (acet-ui-pickdir 
                 "ѡ��Ŀ¼"
                 (vl-string-right-trim "\\" (getvar "dwgprefix"))
                 "����ļ���"
               )
             )
           )
)


;;�_�n�ؚw�D�nĿ�
(defun C:QO (/ dwg) 
  (setq dwg (getfiled "Select drawing" (getvar "DWGPREFIX") "dwg" 0))
  (if dwg 
    (vla-activate 
      (vla-open (vla-get-documents (vlax-get-acad-object)) dwg)
    )
  )
  (princ)
  (princ "\nQOPEN QO1 ")
)

;;�_���Y�ϊA
(defun c:QF (/) 
  (setvar "cmdecho" 0)
  (startapp "explorer.exe" (getvar "dwgprefix"))
  (setvar "cmdecho" 1)
  (princ "\nQOPEN QF ")
  (princ)
)
;;;==================================================*
;by����è2015
(vl-load-com)
;32λcad�ļ���ѡ
(defun non-GetFiles (/) 
  (if 
    (/= (vl-registry-read "HKEY_CLASSES_ROOT\\Licenses\\4D553650-6ABE-11cf-8ADB-00AA00C00905" ) 
        "gfjmrfkfifkmkfffrlmmgmhmnlulkmfmqkqj"
    )
    (vl-registry-write "HKEY_CLASSES_ROOT\\Licenses\\4D553650-6ABE-11cf-8ADB-00AA00C00905" 
                       ""
                       "gfjmrfkfifkmkfffrlmmgmhmnlulkmfmqkqj"
    )
  )
  (if (setq x (Vlax-Get-Or-Create-Object "MSComDlg.CommonDialog")) 
    (progn (vlax-put-property x "DialogTitle" "nonsmall -- Lisp��ѡDWG�ļ�") 
           (vlax-put-property x "Filter" "DWG Files|*.dwg|All Files|*.*")
           (vlax-put-property x "MaxFileSize" 10000)
           ;        (vlax-put-property x "Flags" 512)
           (vlax-put-property x "Flags" 1574404)
           (vlax-put-property x "Action" 1)
           (vlax-get x "Filename")
    )
  )
)


;�ļ���·��ѡ��-֧���½��ļ���
(defun non_GetFolder (/ get shell) 
  (vl-load-com)
  (setq Shell (Vlax-Get-Or-Create-Object "Shell.Application"))
  (if (setq get (Vlax-Invoke-Method Shell 'BrowseForFolder 0 "ѡ��Ŀ¼" 1)) 
    (setq get (vlax-get-property (vlax-get-property get 'self) 'path))
  )
  (if (vl-file-directory-p get) get)
)


;32λ+64λcad�ļ���ѡ���Ƽ�ʹ�����
(defun getfiles () 
  (setq word (Vlax-Create-Object "Word.Application"))
  (setq dialog (Vlax-Get-Property word 'FileDialog 3))
  (Vlax-Put-Property dialog 'InitialFileName "d:\\")
  (Vlax-Invoke-Method (Vlax-Get dialog 'Filters) 'Add "DWG Files" "*.dwg" 1)
  (Vlax-Invoke-Method dialog 'Show)
  (setq items (Vlax-Get dialog 'SelectedItems))
  (vlax-for item 
            items
            (print item)
  )
  (while (= (Vlax-Get word 'Visible) 0) 
    (Vlax-Put-Property word 'Visible -1)
  )
  (Vlax-Invoke-Method word 'Quit)
)
(getfiles)


;32λ+64λcad�ļ���ѡ��excel����ʱ�����ڴ治�˳������Բ���
(defun getfiles () 
  (setq Excel (Vlax-Create-Object "Excel.Application"))
  (setq dialog (Vlax-Get-Property Excel 'FileDialog 3))
  (Vlax-Put-Property dialog 'InitialFileName "d:\\")
  (Vlax-Invoke-Method (Vlax-Get dialog 'Filters) 'Add "DWG Files" "*.dwg" 1)
  (Vlax-Invoke-Method dialog 'Show)
  (setq items (Vlax-Get dialog 'SelectedItems))
  (vlax-for item 
            items
            (print item)
  )
  (while (= (Vlax-Get Excel 'Visible) 0) 
    (Vlax-Put-Property Excel 'Visible -1)
  )
  (Vlax-Invoke-Method Excel 'Quit)
)


;ie���ϴ������ܵ�������Ϳؼ���Ӱ��ֻ�ܴ�һ��������·���������Ի���Ҫͨ������IE�����������⡣���� �C Internetѡ�� �C ��ȫ �C �Զ��弶�� �C �ҵ����������еġ��������ļ�������������ʱ��������Ŀ¼·������ѡ�С����á����ɡ�
(defun getfiles () 
  (setq ie (Vlax-Create-Object "InternetExplorer.Application"))
  (Vlax-Invoke-Method ie 'Navigate "about:blank")
  (setq ele (Vlax-Invoke-Method (Vlax-Get-Property ie 'document) 
                                'getElementsByTagName
                                "body"
            )
  )
  (setq body (Vlax-Invoke-Method ele 'item 0))
  (Vlax-Put-Property body 
                     'innerHTML
                     "<input type='file' id='fileDialog' multiple accept='.dwg'>"
  )
  (while (or (= (Vlax-Get ie 'Busy) -1) (/= (Vlax-Get ie 'ReadyState) 4)) 
    (command "delay" 200)
  )
  (setq dialog (Vlax-Invoke-Method (Vlax-Get (Vlax-Get-Property ie 'document) 'all) 
                                   'item
                                   "fileDialog"
               )
  )
  (Vlax-Invoke-Method dialog 'click)
  (print (vlax-get dialog 'value))
  (Vlax-Invoke-Method ie 'Quit)
)