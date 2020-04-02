::'@cscript //nologo //e:vbscript "%~f0" %* & @goto :eof
::On Error Resume Next

'Option Explicit
If WScript.Arguments.Count = 0 Then
	Die "No files found."
Else
	Dim arg, i, FileName, FileName1
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
	FileName1 = "★命令: ^" & FileName1 & "^ 输入继续-^o^-^o^"

	ws.appactivate "AutoCAD 2008":ws.SendKeys "% R":WScript.Sleep 100:ws.SendKeys "N ":WScript.Sleep 1000
	VLApp.ActiveDocument.Functions.Item("Sendcommand").funcall(cstr(FileName))
	WScript.Sleep 50:ws.SendKeys "~"
	VLApp.ActiveDocument.Functions.Item("print").funcall(cstr(FileName1))
	 
Else
	ws.Run """D:\Program Files\AutoCAD 2008\acad.exe""", 3:WScript.Sleep 3000:ws.SendKeys " ":WScript.Sleep 50:ws.SendKeys "~" 
End If

Set arg = Nothing : Set i = Nothing : Set FileName = Nothing : Set ws = Nothing : Set App = Nothing : Set VLApp = Nothing

'Lisp函数文件初始化!
Sub Init(msg)
	Dim fso, VsPath, filepath1, filepath2, objTextFile, openFile, openFile2, openFile3, MyArray, MyArray1 
	Set fso = CreateObject("Scripting.FileSystemObject")
	'目标文件
	filepath2 = App.path & "\Support\acad2008.lsp"
	Set openFile2 = fso.OpenTextFile(filepath2, 1, True)

	'提取Lisp命令
	Set openFile3 = fso.OpenTextFile(arg, 1, True)
	MyArray = Split(openFile3.ReadAll, "c:", -1, 1)
	MyArray1 = Split(MyArray(1), " ", -1, 1)
	FileName1 = MyArray1(0)
	
	If Not RegExpZ ("defun Sendcommand ", openFile2.ReadAll) Then
		VsPath = fso.GetParentFolderName(WScript.ScriptFullName)
		'cad支持文件
		filepath1 = fso.BuildPath(Vspath, "Vscad.ini")	
		Set openFile=fso.OpenTextFile(filepath1,1,True)
		'写入配套文件
		Set objTextFile = fso.OpenTextFile(filepath2, 8, False)
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

'正则表达式引擎
Function RegExpZ(patrn, strng)
	Dim re
	Set re = New RegExp
	re.Pattern = patrn
	re.Global = True
	re.IgnoreCase = True
	RegExpZ = re.Test(strng)
End Function