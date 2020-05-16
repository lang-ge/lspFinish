Option Explicit
' 主体程序
Dim count, Switch, foldername, ws, fs, Suffix, fileName, Swstr
count = 0
Switch = MsgBox ("^.^是:文件合并，否:名称替换，取消:文件名统计^.^",vbYesNoCancel,"★文件处理工具★")
If Switch = 7 Then
	Swstr = InputBox("请输入需要替掉的字符")
End If
foldername = InputBox("请输入目标文件夹", "VBS文件工具1.35")
'foldername = SelectFolder(null)
If foldername = "" Then
	WScript.quit
End If
Set fs = CreateObject("scripting.filesystemobject")
digui (foldername)'调用递归函数进行查找
MsgBox "任务成功！文件数量合计: " & CStr(count), 65, "数量统计"

Set ws = CreateObject("Wscript.Shell")
If Switch = 7 Then
	ws.run "%systemroot%\explorer.exe /select, "+fileName
Else
	ws.run "%systemroot%\explorer.exe / open, "+fileName, 2
End If
Set count = Nothing : Set foldername = Nothing : Set ws = Nothing 
Set fs = Nothing : Set Suffix = Nothing : Set fileName = Nothing

'递归查找函数
Function digui(path)
	Dim folder, subfolders, Files, i, j
	Set folder = fs.getfolder(path)
	Set subfolders = folder.subfolders
	Set Files = folder.Files
	For Each i In Files
		Suffix = (fs.GetExtensionName(i))
		'FileName=FileName & i.path & vbNewLine '找到则追加到变量FileName中
		' merge(i.path)
		' Rname(i)
		' Adname(i)
		Select Case Switch
			Case 6
			merge(i)
			Case 7
			Rname(i)
			Case Else
			Adname(i)
		End Select
	Next
	For Each j In subfolders
		digui (j.path) '递归查找子目录
	Next
	Set folder = Nothing : Set subfolders = Nothing 
	Set Files = Nothing : Set i = Nothing : Set j = Nothing
End Function

' 选择文件夹对话框
Function SelectFolder(default)
	Dim Folder
	If IsNull(default) Then
		default = "::{20D04FE0-3AEA-1069-A2D8-08002B30309D}"
	End If
	Set Folder = CreateObject("Shell.Application").BrowseForFolder(0, "", 0, default)
	If Folder Is Nothing Then
		SelectFolder = ""
	Else
		SelectFolder = Folder.Self.Path
	End If
	Set Folder = Nothing
End Function

'合并引擎!
Sub merge(i)
	Dim openFile, objTextFile 
	Set openFile=fs.OpenTextFile(i.path,1,True)
	' VPath = fs.GetParentFolderName(WScript.ScriptFullName)
	fileName = fs.BuildPath(foldername, "Finish." + Suffix) 
	Set objTextFile = fs.OpenTextFile(fileName, 8,True)
	count = count + 1
	objTextFile.WriteLine (";;;==================================================*" & vbLf & ";;;例" & count & ": [功能] ")
	objTextFile.WriteLine (openFile.ReadAll)
	objTextFile.Close
	Set openFile = Nothing : Set objTextFile = Nothing
End Sub

'替换引擎!
Sub Rname(i)
	i.name = Replace(i.name, Swstr, "")
	fileName = i.path 
	count = count + 1
End Sub

'文件名统计引擎!
Sub Adname(i)
	Dim objTextFile 
	fileName = fs.BuildPath(foldername, "log.txt") 
	Set objTextFile = fs.OpenTextFile(fileName, 8,True)
	objTextFile.WriteLine (fs.GetBaseName(i.path))
	objTextFile.Close
	count = count + 1
	Set objTextFile = Nothing
End Sub