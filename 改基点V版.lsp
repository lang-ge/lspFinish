;;;图块基点修改 ，但图块实际位置保持不变
;;;明经通道 编制 By Gu_xl 2011年7月
;;;(defun c:CBB () (c:BlockBase))
(defun c:259 (/ loop base) 
  (while 
    (and 
      (setq en (car (entsel "\n 选择一个图块：")))
      (= "INSERT" (cdr (assoc 0 (entget en))))
    )
    (setq base (cdr (assoc 10 (entget en))))
    (sssetfirst nil (ssadd en))
    (setq pt (getpoint base "\n 图块新基点"))
    (if pt (gxl-BlockBaseEdit en pt))
    ;(sssetfirst)
  )
)
;;; 块基点更改函数
(defun gxl-BlockBaseEdit (InsertEName newInsPt1 / BlockToInsertXform 
                          InsertToBlockXform BlockToInsertSetup VectorCrossProduct 
                          3DTransformAB 3DTransformBA blks LOOP sel BlockName blkdef 
                          oldInsPt1 oldInsPt2 newInsPt2 ss idx XformSpec atts att 
                          *ACDOCUMENT*
                         ) 
  (setq *ACDOCUMENT* (vla-get-ActiveDocument (vlax-get-acad-object)))
  ;;;子程序
  (defun BlockToInsertXform (P1 TransformSpec) 
    (3dTransformAB 
      (nth 0 TransformSpec)
      (nth 1 TransformSpec)
      (nth 2 TransformSpec)
      (nth 3 TransformSpec)
      (nth 4 TransformSpec)
      P1
    ) ;_ end 3dTransformAB
  ) ;_ end defun
  (defun InsertToBlockXform (P1 TransformSpec) 
    (3dTransformBA 
      (nth 0 TransformSpec)
      (nth 1 TransformSpec)
      (nth 2 TransformSpec)
      (nth 3 TransformSpec)
      (nth 4 TransformSpec)
      P1
    ) ;_ end 3dTransformBA
  ) ;_ end defun
  (defun BlockToInsertSetup (InsertEname / InsertEList ZAxis NCSXAxis InsertAngle) 
    (if (= 'str (type InsertEName)) 
      (progn 
        (setq InsertEName (vlax-vla-object->ename 
                            (vla-Item blks InsertEName)
                          ) ;_ vlax-vla-object->ename
        ) ;_ setq
        (list '(1 0 0) 
              '(0 1 0)
              '(0 0 1)
              (GXL-NUM-AX->LISPVALUE 
                (vla-get-Origin (vlax-ename->vla-object InsertEName))
              ) ;_ GXL-NUM-AX->LISPVALUE
              '(1 1 1)
        ) ;_ list
      ) ;_ progn
      (progn 
        (setq ZAxis       (GXL-NUM-AX->LISPVALUE (vla-get-Normal InsertEname))
              InsertAngle (vla-get-Rotation InsertEname)
              NCSXAxis    (trans (list (cos InsertAngle) (sin InsertAngle) 0.0) 
                                 ZAxis
                                 0
                          ) ;_ end trans
        ) ;_ end setq
        (list 
          NCSXAxis
          (VectorCrossProduct ZAxis NCSXAxis)
          ZAxis
          (trans 
            (GXL-NUM-AX->LISPVALUE (vla-get-InsertionPoint InsertEname))
            ZAxis
            0
          ) ;_ trans
          (list (vla-get-XScaleFactor InsertEname) 
                (vla-get-YScaleFactor InsertEname)
                (vla-get-ZScaleFactor InsertEname)
          ) ;_ end list
        ) ;_ end list
      ) ;_ progn
    ) ;_ if
  ) ;_ end defun
  (defun VectorCrossProduct (InputVector1 InputVector2) 
    (list 
      (- (* (cadr InputVector1) (caddr InputVector2)) 
         (* (cadr InputVector2) (caddr InputVector1))
      ) ;_ end -
      (- (* (caddr InputVector1) (car InputVector2)) 
         (* (caddr InputVector2) (car InputVector1))
      ) ;_ end -
      (- (* (car InputVector1) (cadr InputVector2)) 
         (* (car InputVector2) (cadr InputVector1))
      ) ;_ end -
    ) ;_ end list
  ) ;_ end defun
  (defun 3DTransformAB (XA YA ZA OA SA P1 /) 
    (setq P1 (mapcar '* P1 SA))
    (mapcar '+ 
            OA
            (list 
              (+ (* (car XA) (car P1)) 
                 (* (car YA) (cadr P1))
                 (* (car ZA) (caddr P1))
              ) ;_ end +
              (+ (* (cadr XA) (car P1)) 
                 (* (cadr YA) (cadr P1))
                 (* (cadr ZA) (caddr P1))
              ) ;_ end +
              (+ (* (caddr XA) (car P1)) 
                 (* (caddr YA) (cadr P1))
                 (* (caddr ZA) (caddr P1))
              ) ;_ end +
            ) ;_ end list
    ) ;_ end mapcar
  ) ;_ end defun
  (defun 3DTransformBA (XA YA ZA OA SA P1 /) 
    (setq P1 (mapcar '- P1 OA))
    (mapcar '/ 
            (list 
              (+ (* (car XA) (car P1)) 
                 (* (cadr XA) (cadr P1))
                 (* (caddr XA) (caddr P1))
              ) ;_ end +
              (+ (* (car YA) (car P1)) 
                 (* (cadr YA) (cadr P1))
                 (* (caddr YA) (caddr P1))
              ) ;_ end +
              (+ (* (car ZA) (car P1)) 
                 (* (cadr ZA) (cadr P1))
                 (* (caddr ZA) (caddr P1))
              ) ;_ end +
            ) ;_ end list
            SA
    ) ;_ end mapcar
  ) ;_ end defun
  ;主程序
  (setq blks (vla-get-blocks *ACDOCUMENT*))
  (if (= 'str (type InsertEName)) 
    (progn 
      (setq XformSpec (BlockToInsertSetup InsertEName)
            BlockName InsertEName
      ) ;_ setq
      (setq InsertEName (vla-Item blks InsertEName))
      (setq oldInsPt1 (GXL-NUM-AX->LISPVALUE (vla-get-Origin InsertEName))) ;_ setq
    ) ;_ progn
    (progn 
      (if (= 'ename (type InsertEName)) 
        (setq InsertEName (vlax-ename->vla-object InsertEName))
      )
      (setq oldInsPt1 (GXL-NUM-AX->LISPVALUE 
                        (vla-get-InsertionPoint InsertEName)
                      )
            BlockName (vla-get-name InsertEName)
            XformSpec (BlockToInsertSetup InsertEName)
      ) ;_ setq
    ) ;_ progn
  ) ;_ if
  (setq oldInsPt2 (InsertToBlockXform oldInsPt1 XformSpec)
        newInsPt2 (InsertToBlockXform newInsPt1 XformSpec)
  ) ;_ setq
  (setq blkdef (vla-item blks BlockName))
  (vlax-for obj 
            blkdef
            (vla-move obj 
                      (vlax-3d-point newInsPt2)
                      (vlax-3d-point oldInsPt2)
            ) ;_ vla-move
  ) ;_ vlax-for
  ;;;修改块定义基点
  (vlax-for blk 
            blks
            (vlax-for obj 
                      blk
                      (cond 
                        ((and (= "AcDbBlockReference" (vla-get-ObjectName obj)) 
                              (= (strcase BlockName) (strcase (vla-get-name obj)))
                         ) ;_ and
                         (setq XformSpec (BlockToInsertSetup obj))
                         (setq oldInsPt1 (BlockToInsertXform oldInsPt2 XformSpec)
                               newInsPt1 (BlockToInsertXform newInsPt2 XformSpec)
                         ) ;_ setq
                         (vla-move obj 
                                   (vlax-3d-point oldInsPt1)
                                   (vlax-3d-point newInsPt1)
                         ) ;_ vla-move
                         (if 
                           (setq atts (GXL-NUM-AX->LISPVALUE 
                                        (vla-GetAttributes obj)
                                      )
                           )
                           (foreach att atts 
                             (vla-move att 
                                       (vlax-3d-point newInsPt1)
                                       (vlax-3d-point oldInsPt1)
                             )
                           )
                         )
                        )
                        ((and (= "AcDbMInsertBlock" (vla-get-ObjectName obj)) 
                              (= (strcase BlockName) (strcase (vla-get-name obj)))
                         ) ;_ and
                         (setq XformSpec (BlockToInsertSetup obj))
                         (setq oldInsPt1 (BlockToInsertXform oldInsPt2 XformSpec)
                               newInsPt1 (BlockToInsertXform newInsPt2 XformSpec)
                         ) ;_ setq
                         (vla-move obj 
                                   (vlax-3d-point oldInsPt1)
                                   (vlax-3d-point newInsPt1)
                         ) ;_ vla-move
                         (if 
                           (setq atts (GXL-NUM-AX->LISPVALUE 
                                        (vla-GetAttributes obj)
                                      )
                           )
                           (foreach att atts 
                             (vla-move att 
                                       (vlax-3d-point newInsPt1)
                                       (vlax-3d-point oldInsPt1)
                             )
                           )
                         )
                        )
                      ) ;_ cond
            ) ;_ vlax-for
  ) ;_ vlax-for
  (vla-regen *ACDOCUMENT* acActiveViewport)
)
(defun gxl-Num-AX->LispValue (v) 
  (cond 
    ((= (type v) 'variant) (gxl-Num-AX->LispValue (vlax-variant-value v)))
    ((= (type v) 'safearray)
     (mapcar 'gxl-Num-AX->LispValue (safearray-value v))
    )
    ((= (type v) 'list)
     (mapcar 'gxl-Num-AX->LispValue v)
    )
    (T v)
  )
)!