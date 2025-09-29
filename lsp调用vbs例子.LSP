(defun $URLencode$ (str / code *SCR DATA) 
  ;($URLencode$  "http://192.168.0.107:8848/download?filename=中国.png")
  (if STR 
    (if 
      (or 
        *SCR
        (setq *SCR (vlax-create-object 
                     "Aec32BitAppServer.AecScriptControl.1"
                   )
        )
        (setq *SCR (vlax-create-object "ScriptControl"))
      )
      (progn 
        (vlax-put *SCR 'language "VBScript")
        (setq code "Function UTF8Encode(szString)
        Dim szChar,szTemp,szCode
        Dim szHex,szBin
        Dim iCount1,iCount2
        Dim iStrLen1,iStrLen2
        Dim lResult
        Dim lAscVal
        exclude=\"-_.!~*'();/?:@&=+$,#\"
        szString = Trim(szString)
        iStrLen1 = Len(szString)
        For iCount1 = 1 To iStrLen1
            szChar = Mid(szString, iCount1, 1)
            lAscVal = AscW(szChar)
            If lAscVal >= &H0 And lAscVal <= &HFF Then
                If (lAscVal >= &H30 And lAscVal <= &H39) Or (lAscVal >= &H41 And lAscVal <= &H5A) Or (lAscVal >= &H61 And lAscVal <= &H7A) Or InStr(exclude,szChar) >0 Then
                    szCode = szCode & szChar
                Else
                    szCode = szCode & \"%\" & Hex(AscW(szChar))
                End If
            Else
                szHex = Hex(AscW(szChar))
                iStrLen2 = Len(szHex)
                For iCount2 = 1 To iStrLen2
                    szChar = Mid(szHex, iCount2, 1)
                    szBin = szBin & Mid(\"0000;0001;0010;0011;0100;0101;0110;0111;1000;1001;1010;1011;1100;1101;1110;1111;\", CLng(\"&H\" & szChar) * 5 + 1, 4)
                Next
                szTemp = \"1110\" & Left(szBin, 4) & \"10\" & Mid(szBin, 5, 6) & \"10\" & Right(szBin, 6)
                For iCount2 = 1 To 24
                    If Mid(szTemp, iCount2, 1) = \"1\" Then
                        lResult = lResult + 1 * 2 ^ (24 - iCount2)
                    Else
                        lResult = lResult + 0 * 2 ^ (24 - iCount2)
                    End If
                Next
                szTemp = Hex(lResult)
                szCode = szCode & \"%\" & Left(szTemp, 2) & \"%\" & Mid(szTemp, 3, 2) & \"%\" & Right(szTemp, 2)
            End If
            szBin = vbNullString
            lResult = 0
        Next
        UTF8Encode = szCode
    End Function"
        )
        (if 
          (and 
            (not 
              (vl-catch-all-error-p 
                (vl-catch-all-apply 
                  'vlax-invoke
                  (list *SCR 
                        'addcode
                        code
                  )
                )
              )
            )
            (not 
              (vl-catch-all-error-p 
                (setq str (vl-catch-all-apply 
                            'vlax-invoke
                            (list *SCR 
                                  'run
                                  "UTF8Encode"
                                  str
                            )
                          )
                )
              )
            )
          )
          ()
          (setq str nil)
        )
        (if *SCR 
          (vlax-release-object *SCR)
        )
      )
      (progn (print "调用VBScript转码失败，组件未找到"))
    )
  )
  str
)
;;; 返回系统特殊目录
(defun GetSpecialPath (n / fso path)
  (setq fso  (vlax-create-object "Scripting.FileSystemObject"))
  (setq path (vlax-get (vlax-invoke fso 'GetSpecialFolder n) 'path))
  (vlax-release-object fso)
  path
)

(GetSpecialPath 2)