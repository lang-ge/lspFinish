Option Explicit
' �������
Dim count, foldername, ws, fs, Suffix, fileName
count = 0
foldername = InputBox("��������Ҫ���ĸ��ļ��в���", "VBS�����ļ�")
'foldername = SelectFolder(null)
If foldername = "" Then
	WScript.quit
End If
Set fs = CreateObject("scripting.filesystemobject")
digui (foldername)'���õݹ麯�����в���
MsgBox "�ϲ��ļ�����Ϊ: " & CStr(count), 65, "����ͳ��"

Set ws = CreateObject("Wscript.Shell")
ws.run "%systemroot%\explorer.exe /select, "+fileName
Set count = Nothing : Set foldername = Nothing : Set ws = Nothing 
Set fs = Nothing : Set Suffix = Nothing : Set fileName = Nothing


'�ݹ���Һ���
Function digui(path)
    Dim folder, subfolders, Files, i, j
	Set folder = fs.getfolder(path)
	Set subfolders = folder.subfolders
	Set Files = folder.Files
	For Each i In Files
	    Suffix = (fs.GetExtensionName(i))
		'FileName=FileName & i.path & vbNewLine '�ҵ���׷�ӵ�����FileName��
		merge(i.path)
	Next
	For Each j In subfolders
		digui (j.path) '�ݹ������Ŀ¼
	Next
	Set folder = Nothing : Set subfolders = Nothing 
    Set Files = Nothing : Set i = Nothing : Set j = Nothing
End Function

' ѡ���ļ��жԻ���
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

'�ϲ�����!
Sub merge(Fpath)
    Dim openFile, objTextFile 
	Set openFile=fs.OpenTextFile(Fpath,1,True)
	' VPath = fs.GetParentFolderName(WScript.ScriptFullName)
	fileName = fs.BuildPath(foldername, "Finish." + Suffix) 
	Set objTextFile = fs.OpenTextFile(fileName, 8,True)
	count = count + 1
	objTextFile.WriteLine (";;;==================================================*" & vbLf & ";;;��" & count & ": [����] ")
	objTextFile.WriteLine (openFile.ReadAll)
	objTextFile.Close
	Set openFile = Nothing : Set objTextFile = Nothing
End Sub