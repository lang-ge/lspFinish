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
'ActiveX����
Dim ws,App,VLApp
Set ws = CreateObject("wscript.shell")
Set App = GetObject(, "AutoCAD.Application")
Set VLApp = App.GetInterfaceObject("VL.Application.16")

If App Then
    Init "��⵽��һ�����У���ʼ���ɹ�."
	FileName1 = "������: ^" & FileName1 & "^ �������-^o^-^o^"

	ws.appactivate "AutoCAD 2008":ws.SendKeys "% R":WScript.Sleep 100:ws.SendKeys "N ":WScript.Sleep 1000
	VLApp.ActiveDocument.Functions.Item("Sendcommand").funcall(cstr(FileName))
	WScript.Sleep 50:ws.SendKeys "~"
	VLApp.ActiveDocument.Functions.Item("print").funcall(cstr(FileName1))
	 
Else
	ws.Run """D:\Program Files\AutoCAD 2008\acad.exe""", 3:WScript.Sleep 3000:ws.SendKeys " ":WScript.Sleep 50:ws.SendKeys "~" 
End If

Set arg = Nothing : Set i = Nothing : Set FileName = Nothing : Set ws = Nothing : Set App = Nothing : Set VLApp = Nothing

'Lisp�����ļ���ʼ��!
Sub Init(msg)
	Dim fso, VsPath, filepath1, filepath2, objTextFile, openFile, openFile2, openFile3, MyArray, MyArray1 
	Set fso = CreateObject("Scripting.FileSystemObject")
	'Ŀ���ļ�
	filepath2 = App.path & "\Support\acad2008.lsp"
	Set openFile2 = fso.OpenTextFile(filepath2, 1, True)

	'��ȡLisp����
	Set openFile3 = fso.OpenTextFile(arg, 1, True)
	MyArray = Split(openFile3.ReadAll, "c:", -1, 1)
	MyArray1 = Split(MyArray(1), " ", -1, 1)
	FileName1 = MyArray1(0)
	
	If Not RegExpZ ("defun Sendcommand ", openFile2.ReadAll) Then
		VsPath = fso.GetParentFolderName(WScript.ScriptFullName)
		'cad֧���ļ�
		filepath1 = fso.BuildPath(Vspath, "Vscad.ini")	
		Set openFile=fso.OpenTextFile(filepath1,1,True)
		'д�������ļ�
		Set objTextFile = fso.OpenTextFile(filepath2, 8, False)
		objTextFile.WriteLine (openFile.ReadAll)
		objTextFile.Close
		WScript.Echo "Vscad: " & msg			
	End If
End Sub

'������ʾ
Sub Die(msg)
	WScript.Echo "AutoLISP: " & msg
	WScript.Quit(1)
End Sub

'������ʽ����
Function RegExpZ(patrn, strng)
	Dim re
	Set re = New RegExp
	re.Pattern = patrn
	re.Global = True
	re.IgnoreCase = True
	RegExpZ = re.Test(strng)
End Function