;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;进度条在doslib中有专门的函数
;;一个进度条在图中，一个在状态。
;;(work1 "数据处理" 1000);;;1000~10000
;;; 例子1
(defun WORK1 (PROM Y)
  (setq X 0)
  (dos_getprogress PROM "进行中，请耐心等待..." Y)
  (repeat (fix (/ Y 3))
    (setq A 1)
  )
  (while (< X Y)
    (dos_getprogress -1)
    (setq X (1+ X))
  )
  (dos_getprogress t)
)

(defun WORK2 (PROM Y)
  (dos_progbar PROM 10)
  (repeat (fix (/ Y 3))
    (setq A 1)
  )
  (dos_progbar -1)
  (dos_progbar)
  (setq X 0)
  (dos_progbar PROM Y)
  (while (< X Y)
    (dos_progbar -1)
    (setq X (1+ X))
  )
  (dos_progbar)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Library: acetutil.arx (expresstools)
acet-ui-progress [label [max]])
(acet-ui-progress current)

Display progress meter.

Arguments
label If provided, a text string that will appear as a label for the progress meter.
max If provided, the maximum value in the range to be displayed (starting with 0).
current If provided, gives the current value, which should be less than max; positive values are absolute while negative values increment the current position.

If no parameters are provided, the progress meter is removed.

Return Values
The return value depends on the action performed:
Initialize: returns T if successful, otherwise NIL.
Update: returns current.
Restore: returns NIL.
Example
  ;;  init meter
  (acet-ui-progress "Working:" (length theList))
  ;;  process each item
  (foreach item theList
    ;;  perform action
    (doSomethingTo item)
    ;;  update meter by one item
    (acet-ui-progress -1)
  )
  ;;  kill meter
  (acet-ui-progress)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;|
xdrx-api

96. xdrx_pbarbegin
功能：设置ACAD状态行进度条初值
调用格式：(xdrx_pbarbegin <提示字符串> <终值>)
说明：1.参数<终值>:整数值
      2.显示进度条，必须先调用此函数，在LISP的循环体外

97. xdrx_pbarsetpos
功能：设置进度条的位置
调用格式：(xdrx_pbarsetpos <位置>)
说明：1.<位置>:在0和由函数xdrx_pbarbegin设置的终值之间的整数值
      2.该函数在lisp循环体内调用
返回值：NIL
98. xdrx_pbarend
功能：清除ACAD状态行的进度条
调用格式: (xdrx_pbarend)
返回值：NIL

举例：函数xdrx_pbarbegin,xdrx_pbarsetpos,xdrx_pbarend函数应用
     (defun c:test(/ i)
        (xdrx_pbarbegin "已经完成:" 100)
        (setq i 0)
        (repeat 100  ;;循环执行10秒左右
           (xdrx_pbarsetpos (setq i (1+ i)))
           (xdrx_sleep 100);延迟0.1秒
        )
        (xdrx_pbarend)
        (princ)
     )
	 (prompt (stract "\n"  (mutilStr num "|") "\n"))
(defun mutilstr(num sym / str)
   (setq str "")
  (repeat num
   (setq str (strcat sym str))
   )
   str
)
关键在于： (strcat "\n" str "\n")
|;


;;; 例子2
(defun act_slider(/ all counter)
  (setq all 10)
  (setq counter 1.0)
  (while (>= all 0)
    (setq counter 1.0)
    (while (< counter 100000)
      (setq counter (* counter 1.0001))
    )
    (set_tile "sl" (rtos (- 10 all)))
    (set_tile "text" (strcat (rtos (* (/ (- 10.0 all) 10.0) 100.0)) "%"))
    (setq all (1- all))
  )
  (princ "完成")
  (princ )
)
(defun c:Ctrl_slider( / dcl_id)
  (if (< (setq dcl_id (load_dialog "slider")) 0)
    (exit)
  )

  (if (not (new_dialog "sls" dcl_id))
    (exit)
  )
  (act_slider)
  
  (start_dialog)  
  (unload_dialog dcl_id)
  (princ)
)

;------------------------------
sls:dialog{
        label = "进度";
        :slider {label = "显示进度"; key = "sl"; fixed_width = true; width = 25; max_value = 10; min_value = 1;}
        :text {label = "进度"; key = "text";}
        ok_cancel;
}

;;; 一个趣味程序,很久以前的事情了
(defun c:setup ()
;;;_____________________________
  (defun done (/ n m)
    (mode_tile "accept" 1)
    (mode_tile "cancel" 1)
    (start_image "t2")
    (fill_image
      0
      0
      (dimx_tile "t2")
      (dimy_tile "t2")
      5
    )
    (end_image)
    (setq n 1)
    (repeat 100
      (setq m 0)
      (repeat 4500
        (setq m (1+ m))
      )
      (set_tile        "t1"
                (strcat "已经进行了% " (vl-princ-to-string n))
      )
      (start_image "t2")
      (fill_image
        0
        0
        (fix (* n (/ (dimx_tile "t2") 100.0)))
        (dimy_tile "t2")
        1
      )
      (end_image)
      (setq n (1+ n))
    )
    (mode_tile "accept" 1)
    (mode_tile "cancel" 0)
    (set_tile "t1" "OursCAd 注册程序已经完成!")
  )
;;;_____________________________
  (setq t1 " ☆☆☆☆☆ ")                ;serial由oursjg1-->oursjgjs后得到
  (setq dh nil)
  (setq seria1 "缺省值为  1234567890 ")
  (SETQ dh (LOAD_DIALOG "atcad.DCL"))
  (if (new_dialog "oursetup" dh)
    (progn
      (set_tile "t1" t1)
      (set_tile "pass" seria1)
      (action_tile "cancel" "(done_dialog 0)")
      (action_tile "accept" "(dogwrite)(done)")
      (action_tile "pass" "(setq seria1 $value)")
      (setq do_next (start_dialog))
      (cond
        ((= 1 do_next)
         (progn
           (done)
         )
        )

      )
    )
  )
  (unload_dialog dh)
  (PRINC)
)


;;;;;;;;;;dcl文件
oursetup:dialog{label="☆ ☆ ☆ Ours CAD  安 装 程 式 ☆ ☆ ☆";
            alignment=centered;
      spacer_1;
     :row{
     :text{label=" Ours CAD 疯狂设计工具软件环境初始化";}
     :text{label=" %%% ";key="t1";}
         }
     : image_button {color=5;key="t2";width=60;height=1;}
     :row{
      :edit_box{label="输入密码:";edit_width=55;key="pass";password_char="*";}
        }
      spacer_1;                   
      ok_cancel;   
              }