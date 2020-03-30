(defun C:RTEXT (/ fetch ans def tStyle tSize tRot done)
 
  (defun fetch (mode / cont lay vc)
    ;;  从查看中心开始
    (setq vc (getvar "VIEWCTR"))
    ;; 选择文字或文件
    (if (setq cont (if (= mode 0)
                 (acet-ui-getfile "选择文字档"
                              (acet-filename-ext-remove (getvar "DWGNAME"))
                              "txt;*" "Acet:RText" 1664)
                 (AcetRText:pickText "") ) )
      (progn
        ;;创建对象
        (entmake (list '(0 . "RTEXT")
                       '(100 . "AcDbEntity")
                       '(100 . "RText")
                        (cons 10 (trans vc 1 0))
                        (cons 40 tSize)
                        (cons 7 tStyle)
                        (cons 50 tRot)
                        (cons 1 cont)
                        (cons 70 mode)
                        (cons 210 (acet-geom-cross-product (getvar "UCSXDIR")
                                                           (getvar "UCSYDIR") ) )
                 )
        )
        ;; 移至要求的位置
        (setq lay (AcetRText:unlockLayer nil))
        (princ "\nSpecify start point of RText: ")
        (command "_.MOVE" (entlast) "" vc pause)
        (while (wcmatch (getvar "CMDNAMES") "*MOVE*")
          (command pause)
        )
        (AcetRText:modify (entlast) T)
        (AcetRText:lockLayer lay)
      )
    )
  )
 
  (if (AcetRText:appload)
    (progn
      (acet-error-init '(("CMDECHO" 0  "LIMCHECK" 0  "ORTHOMODE" 0) T))
 
      (setq tStyle (getvar "TEXTSTYLE")
            tSize (getvar "TEXTSIZE")
            tRot (AcetRText:defRotation nil)
            def (getenv "AcetRText:type") )
      (if (or (not def)
              (and (/= "Diesel" def)
                   (/= "File" def) ) )
        (setenv "AcetRText:type" (setq def "File"))
      )
 
      (while (not done)
       (princ (acet-str-format "\nCurrent settings: Style=%1  Height=%2  Rotation=%3\n"
                               tStyle
                               (ai_rtos tSize)
                               (angtos tRot) ) )
        (initget "Style Height Rotation File Diesel _Style Height Rotation File Diesel")
        (setq ans (getkword (acet-str-format "Enter an option [Style/Height/Rotation/File/Diesel] <%1>: " def))
              ans (if ans ans def) )
        (cond
          ((= ans "Style")
            (setq tStyle (AcetRText:pickStyle tStyle))
            (setvar "TEXTSTYLE" tStyle)
          )
          ((= ans "Height")
            (setq tSize (AcetRText:pickHeight tSize))
            (setvar "TEXTSIZE" tSize)
          )
          ((= ans "Rotation")
            (setq tRot (AcetRText:pickRotation tRot))
            (AcetRText:defRotation tRot)
          )
          ((= ans "File")
            (fetch 0)
            (setq done T)
            (setq def "File")
          )
          ((= ans "Diesel")
            (fetch 1)
            (setq done T)
            (setq def "Diesel")
          )
        )
      )
      (setenv "AcetRText:type" def)
 
      (acet-error-restore)
    )
  )
)
 
 
;;
;;  RTEDIT - edit RText object
;;
(defun C:RTEDIT (/ ename)
  (if (AcetRText:appload)
    (progn
      (acet-error-init '(("CMDECHO" 0) T (if ename (redraw ename 4))))
 
      ;;  select one RText object, not on locked layer
      (if (setq ename (acet-ui-single-select '((0 . "RTEXT")) nil))
        (AcetRText:modify ename nil)
      )
 
      (acet-error-restore)
    )
  )
)
 
 
;;
;;  RTEXTAPP - set RText editing app
;;
(defun C:RTEXTAPP (/ app new)
  (if (AcetRText:appload)
    (progn
      (acet-error-init nil)
      ;;  locate current value
      (if (not (setq app (getenv "AcetRText:editor")))
        (setq app "") )
      ;;  pick new value
      (setq new (getstring T (acet-str-format
              "\nEnter RText editing application or . for system default <\"%1\">: "
              app ) ) )
      ;;  validate clear and default
      (cond
        ((= new ".")
          (setq new "") )
        ((= new "")
          (setq new app) )
      )
      ;;  set new value
      (setenv "AcetRText:editor" new)
      (acet-error-restore)
    )
  )
  (princ)
)
 
 
;;  ????????????
(defun AcetRText:appload ()
  (if (member nil (mapcar
                   '(lambda (x) (if (member x (arx)) T (arxload x nil)))
                   '("acetutil.arx" "rtext.arx") ) )
    (alert (acet-str-format "???????ObjectARX??."))
    T
  )
)
 
 
;;  modify RText object
(defun AcetRText:modify (ename setdef / ent lay tStyle tSize tRot ans done xv)
  (if (not ename)
    ;;  select one RText object, locked layer OK
    (setq ename (acet-ui-single-select '((0 . "RTEXT")) T))
  )
  (if (and ename
           (setq ent (entget ename)) )
    (progn
      (redraw ename 3)
      (setq lay (AcetRText:unlockLayer (cdr (assoc 8 ent)))
            xv (cdr (assoc 210 ent))
            tStyle (cdr (assoc 7 ent))
            tSize (cdr (assoc 40 ent))
            tRot (acet-geom-angle-trans (cdr (assoc 50 ent)) xv 1)
      )
      (while (not done)
        (princ (acet-str-format
                   "\nCurrent values: Style=%1  Height=%2  Rotation=%3\n"
                   tStyle (ai_rtos tSize) (angtos tRot) ) )
        (initget "Style Height Rotation Edit _Style Height Rotation Edit")
        (if (setq ans (getkword "Enter an option [Style/Height/Rotation/Edit]: "))
          (progn
            (cond
              ((= ans "Style")
                (setq tStyle (AcetRText:pickStyle tStyle))
                (setq ent (subst (cons 7 tStyle) (assoc 7 ent) ent))
                (redraw ename 4)
                (entmod ent)
                (entupd ename)
                (princ " ")
                (redraw ename 3)
                (if setdef
                  (setvar "TEXTSTYLE" tStyle)
                )
              )
              ((= ans "Height")
                (setq tSize (AcetRText:pickHeight tSize))
                (setq ent (subst (cons 40 tSize) (assoc 40 ent) ent))
                (redraw ename 4)
                (entmod ent)
                (entupd ename)
                (princ " ")
                (redraw ename 3)
                (if setdef
                  (setvar "TEXTSIZE" tSize)
                )
              )
              ((= ans "Rotation")
                (setq tRot (AcetRText:pickRotation tRot))
                (setq ent (subst (cons 50 (acet-geom-angle-trans tRot 1 xv))
                                 (assoc 50 ent)
                                 ent ) )
                (redraw ename 4)
                (entmod ent)
                (entupd ename)
                (princ " ")
                (redraw ename 3)
                (if setdef
                  (AcetRText:defRotation tRot)
                )
              )
              ((= ans "Edit")
                (AcetRText:edit ent)
                (entupd ename)
                (redraw ename 4)
                (entupd ename)
                (princ " ")
                (redraw ename 3)
                (setq ent (entget ename))
              )
            )
          )
          (setq done T)
        )
      )
 
      (AcetRText:lockLayer lay)
      (redraw ename 4)
    )
    (princ "\nNothing found")
  )
)
 
 
;;  edit given RText object
(defun AcetRText:edit (ent / cont dsl app mode)
  (setq cont (cdr (assoc 1 ent))
        mode (cdr (assoc 70 ent))
  )
 
  (if (and (/= 1 (logand 1 mode))
           (not (findfile cont)) )
    (progn
      (alert (acet-str-format "Cannot find RText file: %1" cont))
      (setq cont (acet-ui-getfile "Select text file" cont "txt;*" "Acet:RText" 1664) )
      (if (and cont
               (findfile cont) )
        (entmod (subst (cons 1 cont) (assoc 1 ent) ent))
      )
    )
    (progn
      (if (and (= 1 (logand 1 mode))
               (setq dsl (AcetRText:pickText cont)) )
        (entmod (subst (cons 1 dsl) (assoc 1 ent) ent))
        (progn
          (if (= 1 (logand 1 mode))
            (setq dsl "")
          )
        )
      )
    )
  )
 
  (if (not dsl)
    (progn
      ;;  pick editing app
      (if (or (not (setq app (getenv "AcetRText:editor")))
              (= app "") )
        (setq app (acet-filename-associated-app cont))
      )
      ;;  use Notepad if nothing else found
      (if (not app)
        (setq app "Notepad")
      )
      ;;  run editor
      (if (/= -1 (acet-sys-spawn 1 app cont))
        (entupd (cdr (assoc -1 ent)))
        (princ (acet-str-format "\nEditor failure."))
      )
    )
  )
 
  (princ "\nUpdates may not be apparent until next regen.")
)
 
 
;;  pick text height
(defun AcetRText:pickHeight (hgt / work a b)
  (setq hgt (if hgt hgt (getvar "TEXTSIZE"))
        work (ai_rtos hgt) )
 
  (initget 6)
  (setq work (getdist (acet-str-format "\nSpecify height <%1>: " work)))
  (if work work hgt)
)
 
 
;;  pick text style
(defun AcetRText:pickStyle (def / showStyles new done)
 
  (defun showStyles (/ pat sty)
    (setq pat (getstring T
                  (acet-str-format "\nEnter text style(s) to list <*>: ") )
          pat (if (= "" pat) "*" (xstrcase pat)) )
    (while (setq sty (tblnext "STYLE" (not sty)))
      (if (and (not (wcmatch (cdr (assoc 2 sty)) "*|*"))
               (wcmatch (cdr (assoc 2 sty)) pat) )
        (progn
          (princ (acet-str-format
                     "Style name: %1   Font files: %2\n"
                     (cdr (assoc 2 sty))
                     (cdr (assoc 3 sty)) ) )
          (princ (acet-str-format
                     "   Height: %1  Width factor: %2  Obliquing angle: %3\n"
                     (cdr (assoc 40 sty))
                     (cdr (assoc 41 sty))
                     (angtos (cdr (assoc 50 sty))) ) )
          (princ (acet-str-format "   Generation: %1\n\n"
                     (if (= 0 (cdr (assoc 70 sty)))
                       "Normal"
                       (acet-str-format "%1%2%3"
                         (if (/= 0 (logand 2 (cdr (assoc 71 sty))))
                             "Backwards " "")
                         (if (/= 0 (logand 4 (cdr (assoc 71 sty))))
                             "Upside-down " "")
                         (if (/= 0 (logand 4 (cdr (assoc 70 sty))))
                             "Vertical" "")
                       )
                     )
                  )
          )
        )
      )
    )
  )
 
  (if (and def
           (setq def (xstrcase def))
           (tblsearch "STYLE" def) )
    (setq new def)
    (setq new (getvar "tStyle")
          def new )
  )
 
  (while (not done)
    (setq new (getstring T (acet-str-format
                           "\nEnter name of text style or [?] <%1>: " new))
          new (if (= "" new) def (xstrcase new)) )
    (if (tblsearch "STYLE" new)
      (setq done T)
      (if (= new "?")
        (showStyles)
        (princ (acet-str-format "\nCannot find text style \"%1\"." new))
      )
    )
  )
 
  new
)
 
 
;;  pick text rotation
(defun AcetRText:pickRotation (ang / def)
  (setq def (if ang ang (AcetRText:defRotation nil))
        ang (getangle (acet-str-format
                "\nSpecify rotation angle of RText <%1>: " (angtos def)))
  )
  (if ang ang def)
)
 
 
;;  ??/????????
(defun AcetRText:defRotation (ang / lay ent)
  (setq ang (if ang (angtos ang (getvar "AUNITS") 8) "")
        ent (entlast) )
 
  ;;  ????????CLAYER
  (setq lay (AcetRText:unlockLayer nil))
 
  ;;  ????Text???????????
  (command "_.TEXT" "0,0")
  ;;  extra arg required if zero text height
  (if (= 0.0 (cdr (assoc 40 (entget (tblobjname "STYLE" (getvar "TEXTSTYLE"))))))
    (command "") )
  (command ang ".")
 
  ;;  ?????????
  (if (/= ent (entlast))
    (progn
      (setq ang (cdr (assoc 50 (entget (entlast)))))
      (entdel (entlast))
    )
    (setq ang 0.0)
  )
 
  ;;  ???????CLAYER
  (AcetRText:lockLayer lay)
 
  ang
)
 
 
;;  unlock layer if locked, returns layer or nil
(defun AcetRText:unlockLayer (lay / lck)
  ;;  use CLAYER if none given
  (setq lay (if lay lay (getvar "CLAYER"))
        lck (acet-layer-locked lay) )
  (if lck
    (progn
      (command "_.-LAYER" "_UNLOCK" lay "")
      lay
    )
    nil
  )
)
 
 
;;  lock layer if previously locked
(defun AcetRText:lockLayer (lay)
  (if lay
    (command "_.-LAYER" "_LOCK" lay "")
  )
)
 
 
;;  pick contents
(defun AcetRText:pickText (str / work)
  (if (or (= 0 (getvar "CMDDIA"))
          (= 4 (logand 4 (getvar "CMDACTIVE"))) )
    (progn
      (setq str "")
      (while (/= work "")
        (setq work (getstring T (acet-str-format "\nEnter RText: ")))
        (if (/= work "")
          (setq str (strcat str (if (< 0 (strlen str)) "\r\n" "") work))
        )
      )
      str
    )
    (acet-ui-txted str "Edit RText")
  )
)


(princ)
