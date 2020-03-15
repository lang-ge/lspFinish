::'@cscript //nologo //e:vbscript "%~f0" %* & @goto :eof
::On Error Resume Next

'Option Explicit
If WScript.Arguments.Count = 0 Then
	Die "No files found."
Else
	Dim arg, i, FileName
	For i = 0 To WScript.Arguments.Count - 1
		arg = WScript.Arguments(i)
		'FileName = "{(}load """ & Replace(arg, "\", "/") & """{)}~" 
		FileName = "(load """ & Replace(arg, "\", "/") & """)"
		Clistr (FileName)
	Next   
End If
'CAD-VL¿Ø¼þ
Dim ws,App,VLApp
Set ws = CreateObject("wscript.shell")
Set App = GetObject(, "AutoCAD.Application")
Set VLApp = App.GetInterfaceObject("VL.Application.16")
VLApp.ActiveDocument.Functions.Item("princ").funcall(cstr(FileName))
REM If App Then
REM 	ws.appactivate "AutoCAD 2008":ws.SendKeys "% R":WScript.Sleep 50:ws.SendKeys "^v~" '"3z "
REM Else
REM 	ws.Run """D:\Program Files\AutoCAD 2008\acad.exe""", 3:WScript.Sleep 3000:ws.SendKeys " ":WScript.Sleep 50:ws.SendKeys "^v~" '"3z "
REM End If

'´íÎóÌáÊ¾
Sub Die(msg)
	WScript.Echo "AutoLISP: " & msg
	WScript.Quit(1)
End Sub