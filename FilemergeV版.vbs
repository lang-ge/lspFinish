Option Explicit
' �������
Dim count, Switch, foldername, ws, fs, Suffix, fileName, Swstr
count = 0
Switch = MsgBox ("^.^��:�ļ��ϲ�����:�����滻��ȡ��:�ļ���ͳ��^.^",vbYesNoCancel,"���ļ������ߡ�")
If Switch = 7 Then
	Swstr = InputBox("��������Ҫ������ַ�")
End If
foldername = InputBox("������Ŀ���ļ���", "VBS�ļ�����1.35")
'foldername = SelectFolder(null)
If foldername = "" Then
	WScript.quit
End If
Set fs = CreateObject("scripting.filesystemobject")
digui (foldername)'���õݹ麯�����в���
MsgBox "����ɹ����ļ������ϼ�: " & CStr(count), 65, "����ͳ��"

Set ws = CreateObject("Wscript.Shell")
If Switch = 7 Then
	ws.run "%systemroot%\explorer.exe /select, "+fileName
Else
	ws.run "%systemroot%\explorer.exe / open, "+fileName, 2
End If
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
Sub merge(i)
	Dim openFile, objTextFile 
	Set openFile=fs.OpenTextFile(i.path,1,True)
	' VPath = fs.GetParentFolderName(WScript.ScriptFullName)
	fileName = fs.BuildPath(foldername, "Finish." + Suffix) 
	Set objTextFile = fs.OpenTextFile(fileName, 8,True)
	count = count + 1
	objTextFile.WriteLine (";;;==================================================*" & vbLf & ";;;��" & count & ": [����] ")
	objTextFile.WriteLine (openFile.ReadAll)
	objTextFile.Close
	Set openFile = Nothing : Set objTextFile = Nothing
End Sub

'�滻����!
Sub Rname(i)
	i.name = Replace(i.name, Swstr, "")
	fileName = i.path 
	count = count + 1
End Sub

'�ļ���ͳ������!
Sub Adname(i)
	Dim objTextFile 
	fileName = fs.BuildPath(foldername, "log.txt") 
	Set objTextFile = fs.OpenTextFile(fileName, 8,True)
	objTextFile.WriteLine (fs.GetBaseName(i.path))
	objTextFile.Close
	count = count + 1
	Set objTextFile = Nothing
End Sub