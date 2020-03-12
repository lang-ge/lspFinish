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
Dim ws,App
Set ws = CreateObject("wscript.shell")
Set App = GetObject(, "AutoCAD.Application")
If App Then
	ws.appactivate "AutoCAD 2008":ws.SendKeys "% R":WScript.Sleep 50:ws.SendKeys "^v~" '"3z "
Else
	ws.Run """D:\Program Files\AutoCAD 2008\acad.exe""", 3:WScript.Sleep 3000:ws.SendKeys " ":WScript.Sleep 50:ws.SendKeys "^v~" '"3z "
End If

'设置剪切板的内容
Function Clistr (FileName)
	Dim Form, TextBox
	Set Form = CreateObject("Forms.Form.1")
	Set TextBox = Form.Controls.Add("Forms.TextBox.1").Object
	TextBox.MultiLine = True
	TextBox.Text = FileName
	TextBox.SelStart = 0
	TextBox.SelLength = TextBox.TextLength
	TextBox.Copy
End Function

'错误提示
Sub Die(msg)
	WScript.Echo "AutoLISP: " & msg
	WScript.Quit(1)
End Sub