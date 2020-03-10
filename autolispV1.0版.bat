::'@cscript //nologo //e:vbscript "%~f0" %* & @goto :eof
::On Error Resume Next

'Option Explicit
If WScript.Arguments.Count = 0 Then
	Die "No files found."
Else
	Dim arg, i, FileName
	For i = 0 To WScript.Arguments.Count - 1
		arg = WScript.Arguments(i)
		FileName = "{(}load """ & Replace(arg, "\", "/") & """{)}~" 
	Next   
End If
Dim ws
Set ws = CreateObject("wscript.shell")
If IsProcess("acad.exe") = True Then
	ws.appactivate "AutoCAD 2008":ws.SendKeys "% R":WScript.Sleep 50:ws.SendKeys FileName '"3z "
Else
	ws.Run """D:\Program Files\AutoCAD 2008\acad.exe""", 3:WScript.Sleep 3000:ws.SendKeys " ":WScript.Sleep 50:ws.SendKeys FileName '"3z "
End If

'检查当前进程列表中是否存在指定的进程2
Function IsProcess(strProcessName)
	
	Dim objWMIService
	Set objWMIService = GetObject("winmgmts:\\.\root\cimv2")
	Dim colProcessList
	Set colProcessList = objWMIService.ExecQuery("SELECT * FROM Win32_Process WHERE Name='" & strProcessName & "'")
	IsProcess = CBool(colProcessList.Count)
	
End Function

