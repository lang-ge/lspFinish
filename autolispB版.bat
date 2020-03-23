::'@cscript //nologo //e:vbscript "%~f0" %* & @goto :eof
::On Error Resume Next

'Option Explicit
If WScript.Arguments.Count = 0 Then
	Die "No files found."
Else
	Dim arg, i, FileName
	For i = 0 To WScript.Arguments.Count - 1
		arg = WScript.Arguments(i)
		FileName = Replace(arg, "\", "/") 
		' FileName = "(load """ & Replace(arg, "\", "/") & """)"
	Next   
End If
'ActiveX控制
Dim ws,App,VLApp
Set ws = CreateObject("wscript.shell")
Set App = GetObject(, "AutoCAD.Application")
Set VLApp = App.GetInterfaceObject("VL.Application.16")
VLApp.ActiveDocument.Functions.Item("Sendcommand").funcall(cstr(FileName))
If App Then
	ws.appactivate "AutoCAD 2008":ws.SendKeys "% R":WScript.Sleep 50:ws.SendKeys "~" '"3z "
Else
	ws.Run """D:\Program Files\AutoCAD 2008\acad.exe""", 3:WScript.Sleep 3000:ws.SendKeys " ":WScript.Sleep 50:ws.SendKeys "~" '"3z "
End If

Set arg = Nothing : Set i = Nothing : Set FileName = Nothing : Set ws = Nothing : Set App = Nothing : Set VLApp = Nothing

'错误提示
Sub Die(msg)
	WScript.Echo "AutoLISP: " & msg
	WScript.Quit(1)
End Sub