Option Explicit
' 主体程序
Dim count, foldername, ws, fs, Suffix, fileName
count = 0
foldername = InputBox("请输入想要在哪个文件夹查找", "VBS查找文件")
'foldername = SelectFolder(null)
If foldername = "" Then
	WScript.quit
End If
Set fs = CreateObject("scripting.filesystemobject")
digui (foldername)'调用递归函数进行查找
MsgBox "合并文件数量为: " & CStr(count), 65, "数量统计"

Set ws = CreateObject("Wscript.Shell")
ws.run "%systemroot%\explorer.exe /select, "+fileName
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
		merge(i.path)
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
Sub merge(Fpath)
    Dim openFile, objTextFile 
	Set openFile=fs.OpenTextFile(Fpath,1,True)
	' VPath = fs.GetParentFolderName(WScript.ScriptFullName)
	fileName = fs.BuildPath(foldername, "Finish." + Suffix) 
	Set objTextFile = fs.OpenTextFile(fileName, 8,True)
	count = count + 1
	objTextFile.WriteLine (";;;==================================================*" & vbLf & ";;;例" & count & ": [功能] ")
	objTextFile.WriteLine (openFile.ReadAll)
	objTextFile.Close
	Set openFile = Nothing : Set objTextFile = Nothing
End Sub