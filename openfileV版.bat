::'@cscript //nologo //e:vbscript "%~f0" %* & @goto :eof
::On Error Resume Next

'Option Explicit
If WScript.Arguments.Count = 0 Then
	Die "No files found."
Else
	Dim arg, i
	For i = 0 To WScript.Arguments.Count - 1
		arg = """" & WScript.Arguments(i) & """"
	Next   
End If
'������
Dim ws
Set ws = CreateObject("Wscript.Shell")
ws.run "%systemroot%\explorer.exe / open, "+arg, 2
'WScript.Sleep 500:ws.SendKeys "~" 

Set arg = Nothing : Set i = Nothing : Set ws = Nothing

'������ʾ
Sub Die(msg)
	WScript.Echo "AutoLISP: " & msg
	WScript.Quit(1)
End Sub