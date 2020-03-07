;【自动横向图框】

(defun c:HK(/ *error* e_lst os v1 v2 dimzinold attreqold pt0 pt1 pt2 ptmin ptmax l1 l2 ll ur disx disy acx acy acx_list text_h ss s1 e el dim_h i  j  n a b size f1 f3 list1 fc fcvla fclist)

   (vl-load-com)
   (if (not (and (findfile "D:\\自动图框\\FRAME_HA3.dwg") (findfile "D:\\自动图框\\FRAME_HA2.dwg") (findfile "D:\\自动图框\\FRAME_HA1.dwg")));图框文件是否存在
      (progn
         (PRINC "\nD:\\自动图框目录下没有发现图框模板。\n")(PRINC)
         ( quit )
      )
   )
   (setq pt1 (getpoint "请选择图形左上角:\n"))
   (setq pt2 (getcorner pt1 "请选择图形右下角:\n"))
   (setq size 0);图号
   (setq size(getdist "\n自动确定图号或 [A3(3)/A2(2)/A1(1)]:"))
   (PRINC "\n自动横向图框，反馈请联系:李劲松（18919611085） \n")(PRINC)
   (setq e_lst (mapcar (function (lambda (n) (list 'setvar n (getvar n)))) '( "osmode" "cmdecho" "blipmode" "attreq"  "dimzin" ))) ;环境变量初始值
   (setq acx_list '(1 1.5 2 2.5 3 4 5 6 10 15 20 25 30 40 50 60 100 150 200 250 300 400 500 600 1000 1500 2000 2500 3000 4000 5000 6000 10000 20000 50000 100000)) ;比例列表
   ;|(setq os (getvar "osmode"))
   (setq v1(getvar "cmdecho"))
   (setq v2(getvar "blipmode"))
   (setq attreqold(getvar "attreq"))
   |;
   (setvar "osmode" 0)
   (setvar "blipmode" 0)
   (setvar "cmdecho" 0)
   (setvar "attreq" 0)
   

   (defun *error* (msg)
      (mapcar 'eval e_lst)
      (if (not (member msg '(nil "函数被取消" ";错误:quit / exit abort") ))
           (progn 
              (PRINC "\n属性文件出错，恢复恢复默认值。 \n")(PRINC)
	      (setq f3(open "D:\\自动图框\\ProjectInfo.dat" "w"));打开数据文件
              (write-line '"\(\(\"条形码\" \"\"\)\(\"比例\" \"1:2\"\)\(\"日期\" \"2019年5月\"\)\(\"图号\" \"HYS-B0207C-A-08\"\)\(\"图纸级别\" \"1\"\)\(\"设计阶段\" \"施工图\"\)\(\"工程名称\" \"安徽合肥小庙220kV变电站新建工程\"\)\(\"图名\" \"电气总平面布置图\"\)\)" f3);写入默认内容
              (close f3)
	      (princ (strcat ";错误:" msg))

	   )
      )
   )
   (if FrameEattEdit(vlr-remove FrameEattEdit)) ;感应器清理
  
   ;边界盒begin
   (setq ss (ssget "_W" pt1 pt2  ))
   (if (/= ss nil) 
     (progn
        (repeat (setq j (sslength ss))
           (vla-getboundingbox  (vlax-ename->vla-object (ssname ss (setq j (1- j))))  'll 'ur )
           (setq l1 (cons (vlax-safearray->list ll) l1) l2 (cons (vlax-safearray->list ur) l2))
        )
        (mapcar 'set
          (list 'll 'ur)
          (mapcar '(lambda (a b) (apply 'mapcar (cons a b)))
                  '(min max)
                  (list l1 l2)
          )
       )
       (setq ptmin (list (max (min (car pt1) (car pt2)) (min (car ll) (car ur))) (min (max (cadr pt1) (cadr pt2)) (max (cadr ll) (cadr ur)))))  ;图框左上角
       (setq ptmax (list (min (max (car pt1) (car pt2)) (max (car ll) (car ur))) (max (min (cadr pt1) (cadr pt2)) (min (cadr ll) (cadr ur)))))  ;图框右下角
       (setq  ll ptmin ur ptmax)
     )
     (setq  ll pt1 ur pt2)
   )
   (setq pt0 (list (min (car ll) (car ur)) (max (cadr ll) (cadr ur))))  ;图框插入点
   ;边界盒end  
   (setq disx (abs (- (car ll) (car ur))))
   (setq disy (abs (- (cadr ll) (cadr ur))))
   (setq acx 1.00)
   (setq s1(ssget "_W" pt1 pt2  '((0 . "TEXT,MTEXT"))));以交互方式得到一个选择集
   (setq text_h 0) ;文本的高度初值为0
   (setq dim_h (getvar "dimtxt")) ;标注文本的高度
   (if (= s1 nil) (setq text_h 1)
     (progn
       (setq n 0) ;序号的初值为0
       (repeat (sslength s1);重复执行，执行的次数等于所选对象的个数  
         (setq e(ssname  s1  n));得到选择集内第n个对象的图元名
         (setq el(entget e));得到这个对象的图元表
         (if (< text_h (cdr (assoc 40 el))) (setq text_h (cdr (assoc 40 el))));取最大字号
         (setq n(1+ n)) ;序号n的数量加1
       )
     )
   )
   (if (> dim_h text_h) (setq text_h dim_h))

   (if (and (/= size 3) (/= size 2) (/= size 1))
     (progn
            (setq i (/ disx text_h))
            (cond ((<=  i  170) (setq size 3))
                  ((<=  i  240) (setq size 2))
                  (t  (setq size 1))
	    )	     
     )
   )

   (if  (findfile "D:\\自动图框\\ProjectInfo.dat");属性数据文件是否存在
     (if (setq f1( open "D:\\自动图框\\ProjectInfo.dat" "r"));打开属性数据文件
       (progn
         (setq list1( read ( read-line f1)));读取第一行数据
	 (if  (not (and (= 8 (length list1)) (cadr(assoc "条形码" list1)) (cadr(assoc "比例" list1)) (cadr(assoc "日期" list1))
	                (cadr(assoc "图号" list1)) (cadr(assoc "图纸级别" list1)) (cadr(assoc "设计阶段" list1))
	                (cadr(assoc "工程名称" list1))  (cadr(assoc "图名" list1)) 
		   )
	      )
	   (setq list1 '(("条形码" "") ("比例" "1:2") ("日期" "2019年5月") ("图号" "HYS-B0207C-A-08")
			 ("图纸级别" "1")("设计阶段" "施工图")("工程名称" "安徽合肥小庙220kV变电站新建工程")
			 ("图名" "电气总平面布置图")));设为默认值
	 )
	 (close f1);关闭数据文件
       )
     )
     (setq list1 '(("条形码" "") ("比例" "1:2") ("日期" "2019年5月") ("图号" "HYS-B0207C-A-08")
		("图纸级别" "1")("设计阶段" "施工图")("工程名称" "安徽合肥小庙220kV变电站新建工程")
		("图名" "电气总平面布置图")));设为默认值
   )
   (cond ((=  size  3);A3
	   (progn
	       (setq acx  (/ disx 360.00))
               (setq acy  (/ disy 227.00))
               (if (> acy acx) (setq acx acy))
	       (setq i 0)
               (setq acy 1)
               (while (and (< i 34) (> acx (nth i acx_list)) )
		  (progn
                     (setq acy (nth (+ 1 i) acx_list))
                     (setq i (+ 1 i))
		  )
               )
	       (setq acx acy)


	     
	       ;(setq acx (*  (fix (+ (* acx 2.0) 0.9999)) 0.5))
	       ;(command "insert"  "FRAME_HA3" pt0 "x" acx "" "" "" )
	       (command "insert"  "D:\\自动图框\\FRAME_HA3.dwg" pt0 acx "" "" )
	       (setq dimzinold(getvar "DIMZIN"))
	       (setvar "DIMZIN" 0)
	       (PRINC (strcat "\n  图号:A3;比例:" (rtos acx 2 0) "\n"))(PRINC)
	       (setvar "DIMZIN" dimzinold)
	    )
	  )
	((=  size  2);A2
           (progn
	       (setq acx  (/ disx 530.00))
               (setq acy  (/ disy 340.00))
               (if (> acy acx) (setq acx acy))
	       (setq i 0)
               (setq acy 1)
               (while (and (< i 34) (> acx (nth i acx_list)) )
		  (progn
                     (setq acy (nth (+ 1 i) acx_list))
                     (setq i (+ 1 i))
		  )
               )
	       (setq acx acy)
	       ;(setq acx (*  (fix (+ (* acx 2.0) 0.9999)) 0.5))
	       ;(command "insert"  "FRAME_HA2" pt0 "x" acx "" "" "" )
	       (command "insert"  "D:\\自动图框\\FRAME_HA2.dwg" pt0 acx "" "" )
	       (setq dimzinold(getvar "DIMZIN"))
	       (setvar "DIMZIN" 0)
	       (PRINC (strcat "\n  图号:A2;比例:" (rtos acx 2 0) "\n"))(PRINC)
	       (setvar "DIMZIN" dimzinold)
	   )
	)
 	(t  
           (progn;A1
	       (setq acx  (/ disx 776.00))
               (setq acy  (/ disy 514.00))
               (if (> acy acx) (setq acx acy))
	       (setq i 0)
               (setq acy 1)
               (while (and (< i 34) (> acx (nth i acx_list)) )
		  (progn
                     (setq acy (nth (+ 1 i) acx_list))
                     (setq i (+ 1 i))
		  )
               )
	       (setq acx acy)
	       ;(setq acx (*  (fix (+ (* acx 2.0) 0.9999)) 0.5))
	       ;(command "insert"  "FRAME_HA1" pt0 "x" acx "" "" "" ) 
               (command "insert"  "D:\\自动图框\\FRAME_HA1.dwg" pt0 acx "" "" ) 
	       (setq dimzinold(getvar "DIMZIN"))
	       (setvar "DIMZIN" 0)
	       (PRINC (strcat "\n  图号:A1;比例:" (rtos acx 2 0) "\n"))(PRINC)
	       (setvar "DIMZIN" dimzinold)
	   )
	 )
         )

   (setq fc(entlast));图框块句柄
   (if fcvla (setq fcvla nil))
   (if (entget fc );图框块句柄
       (progn
          (setq fcvla(vlax-ename->vla-object fc));图框块转换VLA对象
          (setq fclist (vlax-safearray->list (vlax-variant-value (vla-getattributes fcvla))));图框块属性表

          (if (= 8 (length fclist))
             (progn
                (vlax-put-property (nth 0 fclist) "TextString" (cadr(assoc "条形码" list1)))
                (vlax-put-property (nth 1 fclist) "TextString" (strcat "1:" (rtos acx 2 0)))
	        ;(vlax-put-property (nth 1 fclist) "TextString" (cadr(assoc "比例" list1)))
                (vlax-put-property (nth 2 fclist) "TextString" (cadr(assoc "日期" list1)))
                (vlax-put-property (nth 3 fclist) "TextString" (cadr(assoc "图号" list1)))
                (vlax-put-property (nth 4 fclist) "TextString" (cadr(assoc "图纸级别" list1)))
                (vlax-put-property (nth 5 fclist) "TextString" (cadr(assoc "设计阶段" list1)))
                (vlax-put-property (nth 6 fclist) "TextString" (vl-string-translate '"\P" '"\n" (vl-string-trim '"\W0.8000;"  (cadr(assoc "工程名称" list1)))))
                (vlax-put-property (nth 7 fclist) "TextString" (vl-string-translate '"\P" '"\n" (vl-string-trim '"\W0.8000;"  (cadr(assoc "图名" list1)))))
                (setq FrameEattEdit (vlr-object-reactor fclist nil '((:vlr-modified . AtEattEditCommand)))) ;反应器链接到图框块属性上，改动调用提取属性保存 		      
	     )
	  )
       )          
     )
   ;|(setvar "cmdecho" v1);恢复普通命令提示信息原状态
   (setvar "blipmode" v2);恢复光标标记原状态
   (setvar "osmode" os)
   (setvar "attreq" attreqold)
   |;
   (*error* nil);恢复环境变量初始值
)


(defun AtEattEditCommand(notifier-object reactor-object parameter-list / f2 ec ecvla att  attlist att_txt attxtlist newlist )
   (vl-load-com)
   (setq ec (entlast));图框块句柄
   ;(if FrameEattEdit(vlr-remove FrameEattEdit)) ;感应器清理
   (if  (and (vlax-read-enabled-p notifier-object) (entget ec ));图框块句柄,判断VLA可读
       (progn
          (setq ecvla(vlax-ename->vla-object ec));图框块转换VLA对象
          (setq attlist (vlax-safearray->list (vlax-variant-value (vla-getattributes ecvla))))
	  (if (= 8 (length attlist))
             (progn
                (foreach att attlist
                   (setq att_txt (vla-get-textstring att))
                   (setq attxtlist (cons att_txt attxtlist))
                   (reverse attxtlist)
                )
                (setq newlist(strcat '"\(\(\"条形码\" \"" (nth 7 attxtlist) '"\"\)"
                      "\(\"比例\" \"" (nth 6 attxtlist) '"\"\)"
                      "\(\"日期\" \"" (nth 5 attxtlist) '"\"\)"
                      "\(\"图号\" \"" (nth 4 attxtlist) '"\"\)"
		      "\(\"图纸级别\" \"" (nth 3 attxtlist) '"\"\)"
		      "\(\"设计阶段\" \"" (nth 2 attxtlist) '"\"\)"
		      "\(\"工程名称\" \"" (nth 1 attxtlist) "\"\)"
                      "\(\"图名\" \"" (nth 0 attxtlist) "\"\)\)")	 
                )
                (setq f2(open "D:\\自动图框\\ProjectInfo.dat" "w"));打开数据文件
                (write-line newlist f2);写入内容
                (close f2)		      
	     )
	  )
       )          
     )
)