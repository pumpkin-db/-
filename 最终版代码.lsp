(setq points_info nil)
(defun c:Read (/ filepath file line_count point_data point_list line line-pos feature_code point_type connect_type)
  
  ;变量filepath存文件路径
  (setq filepath (getfiled "选择文件" "" "txt" 0));把路径赋值给filepath
  (if (not filepath)
    (progn (alert "未选择") (exit)));没选择文件就关掉 progn可以让if执行多条语句
  (setq file (open filepath "r"));open打开文件，r表示
  (if (not file)
    (progn (alert "打开失败") (exit)))

  (setq line_count 0);给行计数，初始值为0
  (setq point_data (quote()));point_data用于临时存储读到的点的信息，quote()表示把空列表赋给point_data。也可以使用'()代替(quote())
  (setq point_list (quote()));point_list用来存储所有已经读过的点的信息
  
  (while (setq line (read-line file));从file中read-line,只要不为空，line就有值，整个表达式不为空，则循环执行
    (setq line_count(+ line_count 1));行数+1
    (if (> line_count 1);跳过第一标题行
    (progn 
    (setq line-pos (rem(- line_count 1) 4));line_count-1再模4,就可以得到当前是在数据的哪一部分
    (cond ;类似swich语句，自动加了break,同样默认只执行一个表达式，如果需要多个要使用progn
      ((= line-pos 1)
       (setq point_data (list (atoi line))));atoi把字符串变为整数,开始新的列表
      ((= line-pos 2)
       (setq point_data (append point_data (list (atof line)))));添加x坐标，其中atof把字符串变为浮点数
      ((= line-pos 3)
       (setq point_data (append point_data (list (atof line) 0.0))));添加y坐标，把z设定为0
      ((= line-pos 0)
       (progn 
       (setq point_data (append point_data (list line)));把属性行直接添加进去
      
      ;每读取完一个点就执行下面的语句，进行存储
       (if (>= (strlen line) 6);判断已经到这个点的最后一行并且长度为6位
        (progn 
          (setq feature_code (substr line 1 4));提取前四位地物编码
          (setq point_type (substr line 5 1));提取第五位，只提取一个
          (setq connect_type (atoi (substr line 6 1)));提取第六位，并用atoi变为整数
          (setq point_data (append point_data (list feature_code point_type connect_type)));把刚才提取的东西都放到point_data里面
          (setq point_list (append point_list (list point_data)));把point_data打包放到point_list中
        ))
       
      (setq point_data nil);放在if外，不管条件是否成立都把point_data清除掉，防止干扰下一个点)
)
      );cond的0分支结束
)
)
    )
  )
  (close file)
  (setq points_info point_list);把point_list赋给全局变量points_info
  (princ "\n读取完毕")
  (princ (length points_info))
  (princ " 个点")
  (princ)
)

;下面是第二个部分，画图
(defun c:Paint (/ current_feature current_type prev_pt num x y z feature_code point_type connect_type current_pt feature_color mid_pt nochange_start_pt spline_points num_points degree num_knots knot_list i point_list)
  (if (not points_info)
    (progn 
      (princ "\n先输入Read，再输入Paint")
      (exit)
    )
  )
    ; 为每个地物图层设置对应的颜色
  (command "_.-layer" "_m" "B644" "_c" "150" "B644" "");教学建筑
  (command "_.-layer" "_m" "F555" "_c" "213" "F555" "");废墟与宿舍
  (command "_.-layer" "_m" "S233" "_c" "42" "S233" "");科原食堂浴池
  (command "_.-layer" "_m" "L000" "_c" "102" "L000" "");足球网球篮球场
  (command "_.-layer" "_m" "T777" "_c" "67" "T777" "");停车场
  (command "_.-layer" "_m" "P000" "_c" "22" "P000" "");跑道

  (defun get_color (code);定义一个颜色函数
    (cond
      ((= code "B644") 150)
      ((= code "F555") 213)
      ((= code "S233") 42)
      ((= code "L000") 102)
      ((= code "T777") 67)
      ((= code "P000") 22)
      (t 7)
    )
  )

  (defun paint_spline (pt_list)
      ; 计算控制点数量
  (setq num_points (length pt_list))
  (setq degree 3) ; 3次样条
  
  ; 构建节点向量 (对于n个控制点的k次样条，需要n+k+1个节点)
  (setq knot_list '())
  (setq num_knots (+ num_points degree 1))
  
  ; 创建均匀节点向量
  (setq i 0)
  (while (< i num_knots)
    (if (< i degree)
      (setq knot_list (cons '(40 . 0.0) knot_list))
      (if (>= i num_points)
        (setq knot_list (cons '(40 . 1.0) knot_list)) ; 归一化到[0,1]
        (setq knot_list (cons (cons 40 (/ (- i degree) (- num_points degree) 1.0)) knot_list))
      )
    )
    (setq i (1+ i))
  )
  (setq knot_list (reverse knot_list))
  
  ; 构建控制点数据
  (setq point_list '())
  (foreach pt pt_list
    (setq point_list (cons (cons 10 pt) point_list))
  )
  (setq point_list (reverse point_list))
  
  ; 创建样条曲线
  (entmake
    (append
      (list
        '(0 . "SPLINE")
        '(100 . "AcDbEntity")
        '(100 . "AcDbSpline")
        '(70 . 8)     ; 平面样条 + 控制点模式
        (cons 8 current_feature) ; 图层
        (cons 71 degree)    ; 样条阶数
        (cons 72 num_points) ; 控制点数量
        '(73 . 0)     ; 拟合点数量为0
        (cons 74 (length knot_list)) ; 节点数量
        '(42 . 0.0000001)  ; 拟合公差
        '(43 . 0.0000001)  ; 控制点公差
      )
      knot_list    ; 节点向量
      point_list   ; 控制点
    )
  )
  (princ)
  );样条曲线绘制函数，AI写的，神奇的是，竟然能运行

  (setq spline_points nil); 初始化样条曲线点表
  (setq current_feature nil);当前的地物
  ;(setq start_pt nil);起始点
  (setq prev_pt nil);上一点，先初始化这3个变量
  (setq nochange_start_pt nil);定义一个在同一个地物中不会变的起点
  (setq current_type nil) ; 初始化连接类型变量，用于记录当前地物整体的连接方式（1-直线，2-圆弧，3-样条）

  (foreach point points_info ;类似Py,每提取出的一个点都命名为point,用于后续调用
    ;这段把point里面的要用的东西都提取出来
    (setq num (nth 0 point))     ; 点序号
    (setq x (nth 1 point))       ; X坐标
    (setq y (nth 2 point))       ; Y坐标  
    (setq z (nth 3 point))       ; Z坐标
    (setq feature_code (nth 5 point)) ; 地物编码
    (setq point_type (nth 6 point)) ; 点类型（B/M/E/C）
    (setq connect_type (nth 7 point)) ; 连接类型（1/2/3）

    (setq current_pt (list x y z));定义当前点的坐标

    ;由于有很多不同的地物类型，在开始绘图之前需要判断当前的地物是否改变了，便于改变颜色
    (if (or (not current_feature) (not (equal current_feature feature_code)));如果current_feature为空，或者current_feature与提取的feature_code不相等，就是地物类型发生了变化
      (progn
        (setq current_feature feature_code);更改当前地物的编码,可以一样，但都检查一下
        (setq feature_color (get_color feature_code));更改颜色
        ;(setq start_pt current_pt)
        (setq current_type connect_type) ;当地物改变时，记录当前地物的连接类型
        (if (= point_type "B")
          (setq nochange_start_pt current_pt));只在新的起点更改nochange_start_pt

        (setq prev_pt current_pt);把两个点都变成当前的点

        ;(command "_.-layer" "_m" feature_code "_c" feature_color feature_code "");_m创建新图层，名字为feature_code，_c来创建颜色，颜色为feature_color，附着在feature_code图层上,最后“”表示运行此命令，相当于回车
        ;(command "_.-layer" "_s" feature_code "");_s设置图层,设置为feature_code
      )
    )
    (if (and prev_pt (not (= point_type "B")));上一点不为空，且不为起点，就开始画图
      (progn
        (cond
          ;直线
          ((= connect_type 1)
            ;(command "_.line" prev_pt current_pt "");调用pline画图，从prev_pt到current_pt
            (progn
            (entmake (list 
           (cons 0 "LINE")          ; 图元类型
           (cons 8 current_feature) ; 图层
           (cons 10 prev_pt)        ; 起点
           (cons 11 current_pt)       ; 终点
           ;(cons 62 feature_color)  ; 颜色
           (cons 6 "Continuous")       ; 线型
           ))
  )
          )
          ;样条曲线
          ((= connect_type 3)
            ;(command "_.-layer" "_s" feature_code "" "");切换到当前图层
            ;(command "_.spline" prev_pt current_pt "");spline画样条曲线
            ;(command "") ; 结束点输入
            ;(command "") ; 确认起点切向（默认）
            ;(command "") ; 确认端点切向（默认）
            (if (= point_type "B") 
    (progn
      ; 起点：
      (setq spline_points nil)
      (setq spline_points (list prev_pt));先把上一个点加进来
    )
  )
  
  (if (= point_type "M")
    (progn
      ; 中间点：添加到样条点表
      (setq spline_points (append spline_points (list prev_pt)))
    )
  )
  
(if (= point_type "E")
    (progn
      ; 终点：完成样条点表并创建样条曲线
      (setq spline_points (append spline_points (list prev_pt current_pt)))
      (paint_spline spline_points)
      (setq spline_points nil)
    )
  )
          )
          ;圆弧
          ((= connect_type 2)
            (command "_.-layer" "_s" feature_code "" "");切换到当前图层
            (command "_.pline" prev_pt "a" current_pt "");pline的子模式画圆弧
          )
        )
      )
    )
    (setq prev_pt current_pt);更新上一点的坐标
    ;下面处理一下终点
    (cond 
      ;E
      ((= point_type "E")
       (setq current_feature nil);把current_feature清空，可以把start_pt更新一下
       ;(setq start_pt nil)
       (setq prev_pt nil);把坐标都重新初始一下，设为空，表示结束
       (setq current_type nil)
      )
      ;C
      ((= point_type "C")
        (if nochange_start_pt
          (progn
            (cond
              ((= current_type 1)
                ;(command "_.line" start_pt nochange_current_pt"")
               (progn   (entmake (list 
           (cons 0 "LINE")          ; 图元类型
           (cons 8 current_feature) ; 图层
           (cons 10 current_pt)        ; 起点
           (cons 11 nochange_start_pt); 终点
           ;(cons 62 feature_color)  ; 颜色
           (cons 6 "Continuous")       ; 线型
           ))
  )
              )
              ((= current_type 3)
                (command "_.-layer" "_s" feature_code "" "");切换到当前图层
                (command "_.spline" current_pt nochange_start_pt "")
                (command "") ; 结束点输入
                (command "") ; 确认起点切向（默认）
                (command "") ; 确认端点切向（默认）
              )
              ((= current_type 2)
                (command "_.-layer" "_s" feature_code "" "");切换到当前图层
                (command "_.pline" current_pt "a" nochange_start_pt "")
              )
            )
          ))
       (setq current_feature nil)
       ;(setq start_pt nil)
       (setq prev_pt nil)
       (setq current_type nil)
      )
    )
  )
  (princ (strcat "\n共 " (itoa (length points_info)) " 个点。"))
  (princ)
)