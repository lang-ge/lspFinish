Option Explicit
' �������
Dim count, foldername, fs
count = 0
foldername = InputBox("��������Ҫ���ĸ��ļ��в���", "VBS�����ļ�")
'foldername = SelectFolder(null)
If foldername = "" Then
	WScript.quit
End If
Set fs = CreateObject("scripting.filesystemobject")
digui (foldername)'���õݹ麯�����в���
MsgBox "�ϲ��ļ�����Ϊ: " & CStr(count), 65, "����ͳ��"


'�ݹ���Һ���
Function digui(path)
    Dim folder, subfolders, Files, i, j
	Set folder = fs.getfolder(path)
	Set subfolders = folder.subfolders
	Set Files = folder.Files
	For Each i In Files
		'FileName=FileName & i.path & vbNewLine '�ҵ���׷�ӵ�����FileName��
		Init(i.path)
	Next
	For Each j In subfolders
		digui (j.path) '�ݹ������Ŀ¼
	Next
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
End Function

'�ϲ�����!
Sub Init(FileName)
    Dim openFile, objTextFile 
	Set openFile=fs.OpenTextFile(FileName,1,True)
	' VPath = fs.GetParentFolderName(WScript.ScriptFullName)
	fileName = fs.BuildPath(foldername, "finish.lsp") 
	Set objTextFile = fs.OpenTextFile(fileName, 8,True)
	count = count + 1
	objTextFile.WriteLine (";;;==================================================*" & vbLf & ";;;��" & count & ": [����] ")
	objTextFile.WriteLine (openFile.ReadAll)
	objTextFile.Close
End Sub