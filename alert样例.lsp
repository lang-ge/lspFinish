;;;作者石必强 安徽合肥 
;;;intButton = object.Popup(strText,[nSecondsToWait],[strTitle],[nType])
(defun c:bb1  ()( bbalert  "显示确定按钮" 2 "必强提示1" 0)(princ))
(defun c:bb2 ()( bbalert  "显示“确定”和“取消”按钮。" 2 "必强提示2" 1)(princ))
(defun c:bb3  ()( bbalert  "显示中止、重试和忽略按钮。" 2 "必强提示3" 2)(princ))
(defun c:bb4  ()( bbalert  "显示“是”、“否”和“取消”按钮。" 2 "必强提示4" 3)(princ))
(defun c:bb5  ()( bbalert  "显示“是”和“否”按钮。" 2 "必强提示5" 4)(princ))
(defun c:bb6  ()( bbalert  "显示“重试”和“取消”按钮。" 2 "必强提示6" 5)(princ))
(defun c:bb7  ()( bbalert  "显示“取消”，再试一次，然后继续按钮。" 2 "必强提示7" 6)(princ))
(defun c:bb8  ()( bbalert  "显示停止标记图标。" 2 "必强提示8" 16)(princ))
(defun c:bb9  ()( bbalert  "显示问题图标" 2 "必强提示9" 32)(princ))
(defun c:bb10  ()( bbalert  "显示感叹号图标。" 2 "必强提示10" 48)(princ))
(defun c:bb11  ()( bbalert  "标记的图标显示的信息。" 2 "必强提示11" 64)(princ))
(defun c:bb12  ()( bbalert  "第二个按钮是默认按钮。" 2 "必强提示12" 256)(princ))
(defun c:bb13  ()( bbalert  "第三个按钮是默认按钮。" 2 "必强提示13" 512)(princ))
(defun c:bb14  ()( bbalert  "消息框是系统模式消息框，显示在最顶部的窗口中。" 2 "必强提示14" 4096)(princ))
(defun c:bb15  ()( bbalert "文本右对齐。" 2 "Title15" 524288)(princ))
(defun c:bb16  ()( bbalert  "消息和标题文本按从右到左的阅读顺序显示。" 2 "必强提示16" 1048576)(princ))
(defun bbalert  (a b c d)(vlax-invoke (vla-GetInterfaceObject (vlax-get-acad-object) "WScript.Shell")"popup" a b c d)(princ))
;;;(defun bbalertrun (a b c d)(vlax-invoke (vla-GetInterfaceObject (vlax-get-acad-object) "WScript.Shell")"popup" a b c d))
;;;(setq bbalertnum( bbalertrun  "显示“是”、“否”和“取消”按钮。" 2 "Title4" 3))
;;;返回值
;;;-1 在NsecondStowait秒过去之前，用户没有单击按钮。
;;;1 确定按钮
;;;2 取消按钮
;;;3 中止按钮
;;;4 重试按钮
;;;5 忽略按钮
;;;6 “是“按钮
;;;7 “否“按钮
;;;10“重试”按钮
;;;11 确定继续按钮