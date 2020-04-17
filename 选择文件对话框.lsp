(defun c:257 ( / fna fh op)

;initialize a bare bones error handler that will recover from a first try
;load_test failure and re-issue this command.

  (if (not Acet:Acadinfo-Olderr)
      (setq Acet:Acadinfo-Olderr *error*)
  );if

  (defun *error* ( msg / )
    (if Acet:Acadinfo-Error-On-Load-Test
      (progn
        (if fh
            (close fh)
        );if
        (setq *error* Acet:Acadinfo-Olderr
              Acet:Acadinfo-Olderr nil
        );setq
        (c:acadinfo)
        (setq Acet:Acadinfo-Error-On-Load-Test nil)
      );progn
      (princ msg)
    );if
  );defun *error*


  ;; prompt to open a text file to write info
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
       (princ (strcat "\n"
                      "\nACADINFO is a utility for gathering information about"
                      "\nyour AutoCAD installation and current setup. The routine"
                      "\nwill examine your system and write a text file called"
                      "\n\'" fna "\'"
		              "\nto your hard drive."
              );strcat
       );princ
       (getstring "\nPress ENTER to continue or ESC to cancel... ")

       (princ "\n\nExamining your AutoCAD setup. Please wait...\n")
       (setq op "w")
    );progn then
  );if

  (if (setq fh (open fna op))
    (progn
      (close fh) (setq fh (open fna op)) ;close and re-open again in case of
                                         ;garbage echo from error recovery.

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
        );progn then
        (progn
          (write-line "" fh)
          (write-line "*****FAILURE during lisp file load tests.**** " fh)
          (write-line "One of the following files causes an error on load: " fh)
          (write-line "  acad2008.lsp"  fh)
          (write-line "  acad2008doc.lsp"  fh)
          (write-line "  acettest.fas" fh)
          (write-line "  acetutil.fas" fh)
          (write-line "  acetmain.mnl" fh)
        );progn else
      );if

      (write-line "" fh)
      (write-line (strcat "(arx) -> " (acet-acadinfo-item-to-string (arx))) fh)
      (write-line "" fh)

      (write-line " ------------------------- TYPELIB TEST -------------------------" fh)
      (write-line "" fh)
      (acet-acadinfo-check-typelib fh)
      (write-line "" fh)


      (write-line " ------------------- SYSTEM VARIABLE SETTINGS -------------------" fh)
      (write-line "|;" fh)

      (close fh)

      (acet-acadinfo-vars-to-scr fna -1);append

      (acet-acadinfo-lisp-dump fna)
      (princ "\nDone.")
    );progn then
    (princ "\nCannot open file for write.")
  );if

  (setq *error* olderr)

  (princ)
)