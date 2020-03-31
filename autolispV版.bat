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

If App Then
    Init "检测到第一次运行，初始化成功."
	ws.appactivate "AutoCAD 2008":WScript.Sleep 500:ws.SendKeys "N ":WScript.Sleep 1000:VLApp.ActiveDocument.Functions.Item("Sendcommand").funcall(cstr(FileName)):WScript.Sleep 50:ws.SendKeys "~" 
Else
	ws.Run """D:\Program Files\AutoCAD 2008\acad.exe""", 3:WScript.Sleep 3000:ws.SendKeys " ":WScript.Sleep 50:ws.SendKeys "~" 
End If

Set arg = Nothing : Set i = Nothing : Set FileName = Nothing : Set ws = Nothing : Set App = Nothing : Set VLApp = Nothing

'Lisp函数文件初始化!
Sub Init(msg)
	Dim fso, VPath, filepath1, filepath2, objTextFile, openFile
	filepath2 = App.path & "\Support\Vscad.lsp"
	Set fso = CreateObject("Scripting.FileSystemObject")
	If Not fso.FileExists(filepath2) Then
		VPath = fso.GetParentFolderName(WScript.ScriptFullName)
		filepath1 = fso.BuildPath(Vpath, "Vscad.ini")	
		Set openFile=fso.OpenTextFile(filepath1,1,True)
		Set objTextFile = fso.OpenTextFile(filepath2, 2, True)
		objTextFile.WriteLine (openFile.ReadAll)
		objTextFile.Close
		WScript.Echo "Vscad: " & msg			
	End If
End Sub

'错误提示
Sub Die(msg)
	WScript.Echo "AutoLISP: " & msg
	WScript.Quit(1)
End Sub