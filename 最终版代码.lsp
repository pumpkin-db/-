(setq points_info nil)
(defun c:Read (/ filepath file line_count point_data point_list line line-pos feature_code point_type connect_type)
  
  ;����filepath���ļ�·��
  (setq filepath (getfiled "ѡ���ļ�" "" "txt" 0));��·����ֵ��filepath
  (if (not filepath)
    (progn (alert "δѡ��") (exit)));ûѡ���ļ��͹ص� progn������ifִ�ж������
  (setq file (open filepath "r"));open���ļ���r��ʾ
  (if (not file)
    (progn (alert "��ʧ��") (exit)))

  (setq line_count 0);���м�������ʼֵΪ0
  (setq point_data (quote()));point_data������ʱ�洢�����ĵ����Ϣ��quote()��ʾ�ѿ��б���point_data��Ҳ����ʹ��'()����(quote())
  (setq point_list (quote()));point_list�����洢�����Ѿ������ĵ����Ϣ
  
  (while (setq line (read-line file));��file��read-line,ֻҪ��Ϊ�գ�line����ֵ���������ʽ��Ϊ�գ���ѭ��ִ��
    (setq line_count(+ line_count 1));����+1
    (if (> line_count 1);������һ������
    (progn 
    (setq line-pos (rem(- line_count 1) 4));line_count-1��ģ4,�Ϳ��Եõ���ǰ�������ݵ���һ����
    (cond ;����swich��䣬�Զ�����break,ͬ��Ĭ��ִֻ��һ�����ʽ�������Ҫ���Ҫʹ��progn
      ((= line-pos 1)
       (setq point_data (list (atoi line))));atoi���ַ�����Ϊ����,��ʼ�µ��б�
      ((= line-pos 2)
       (setq point_data (append point_data (list (atof line)))));���x���꣬����atof���ַ�����Ϊ������
      ((= line-pos 3)
       (setq point_data (append point_data (list (atof line) 0.0))));���y���꣬��z�趨Ϊ0
      ((= line-pos 0)
       (progn 
       (setq point_data (append point_data (list line)));��������ֱ����ӽ�ȥ
      
      ;ÿ��ȡ��һ�����ִ���������䣬���д洢
       (if (>= (strlen line) 6);�ж��Ѿ������������һ�в��ҳ���Ϊ6λ
        (progn 
          (setq feature_code (substr line 1 4));��ȡǰ��λ�������
          (setq point_type (substr line 5 1));��ȡ����λ��ֻ��ȡһ��
          (setq connect_type (atoi (substr line 6 1)));��ȡ����λ������atoi��Ϊ����
          (setq point_data (append point_data (list feature_code point_type connect_type)));�Ѹղ���ȡ�Ķ������ŵ�point_data����
          (setq point_list (append point_list (list point_data)));��point_data����ŵ�point_list��
        ))
       
      (setq point_data nil);����if�⣬���������Ƿ��������point_data���������ֹ������һ����)
)
      );cond��0��֧����
)
)
    )
  )
  (close file)
  (setq points_info point_list);��point_list����ȫ�ֱ���points_info
  (princ "\n��ȡ���")
  (princ (length points_info))
  (princ " ����")
  (princ)
)

;�����ǵڶ������֣���ͼ
(defun c:Paint (/ current_feature current_type prev_pt num x y z feature_code point_type connect_type current_pt feature_color mid_pt nochange_start_pt spline_points num_points degree num_knots knot_list i point_list)
  (if (not points_info)
    (progn 
      (princ "\n������Read��������Paint")
      (exit)
    )
  )
    ; Ϊÿ������ͼ�����ö�Ӧ����ɫ
  (command "_.-layer" "_m" "B644" "_c" "150" "B644" "");��ѧ����
  (command "_.-layer" "_m" "F555" "_c" "213" "F555" "");����������
  (command "_.-layer" "_m" "S233" "_c" "42" "S233" "");��ԭʳ��ԡ��
  (command "_.-layer" "_m" "L000" "_c" "102" "L000" "");������������
  (command "_.-layer" "_m" "T777" "_c" "67" "T777" "");ͣ����
  (command "_.-layer" "_m" "P000" "_c" "22" "P000" "");�ܵ�

  (defun get_color (code);����һ����ɫ����
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
      ; ������Ƶ�����
  (setq num_points (length pt_list))
  (setq degree 3) ; 3������
  
  ; �����ڵ����� (����n�����Ƶ��k����������Ҫn+k+1���ڵ�)
  (setq knot_list '())
  (setq num_knots (+ num_points degree 1))
  
  ; �������Ƚڵ�����
  (setq i 0)
  (while (< i num_knots)
    (if (< i degree)
      (setq knot_list (cons '(40 . 0.0) knot_list))
      (if (>= i num_points)
        (setq knot_list (cons '(40 . 1.0) knot_list)) ; ��һ����[0,1]
        (setq knot_list (cons (cons 40 (/ (- i degree) (- num_points degree) 1.0)) knot_list))
      )
    )
    (setq i (1+ i))
  )
  (setq knot_list (reverse knot_list))
  
  ; �������Ƶ�����
  (setq point_list '())
  (foreach pt pt_list
    (setq point_list (cons (cons 10 pt) point_list))
  )
  (setq point_list (reverse point_list))
  
  ; ������������
  (entmake
    (append
      (list
        '(0 . "SPLINE")
        '(100 . "AcDbEntity")
        '(100 . "AcDbSpline")
        '(70 . 8)     ; ƽ������ + ���Ƶ�ģʽ
        (cons 8 current_feature) ; ͼ��
        (cons 71 degree)    ; ��������
        (cons 72 num_points) ; ���Ƶ�����
        '(73 . 0)     ; ��ϵ�����Ϊ0
        (cons 74 (length knot_list)) ; �ڵ�����
        '(42 . 0.0000001)  ; ��Ϲ���
        '(43 . 0.0000001)  ; ���Ƶ㹫��
      )
      knot_list    ; �ڵ�����
      point_list   ; ���Ƶ�
    )
  )
  (princ)
  );�������߻��ƺ�����AIд�ģ�������ǣ���Ȼ������

  (setq spline_points nil); ��ʼ���������ߵ��
  (setq current_feature nil);��ǰ�ĵ���
  ;(setq start_pt nil);��ʼ��
  (setq prev_pt nil);��һ�㣬�ȳ�ʼ����3������
  (setq nochange_start_pt nil);����һ����ͬһ�������в��������
  (setq current_type nil) ; ��ʼ���������ͱ��������ڼ�¼��ǰ������������ӷ�ʽ��1-ֱ�ߣ�2-Բ����3-������

  (foreach point points_info ;����Py,ÿ��ȡ����һ���㶼����Ϊpoint,���ں�������
    ;��ΰ�point�����Ҫ�õĶ�������ȡ����
    (setq num (nth 0 point))     ; �����
    (setq x (nth 1 point))       ; X����
    (setq y (nth 2 point))       ; Y����  
    (setq z (nth 3 point))       ; Z����
    (setq feature_code (nth 5 point)) ; �������
    (setq point_type (nth 6 point)) ; �����ͣ�B/M/E/C��
    (setq connect_type (nth 7 point)) ; �������ͣ�1/2/3��

    (setq current_pt (list x y z));���嵱ǰ�������

    ;�����кܶ಻ͬ�ĵ������ͣ��ڿ�ʼ��ͼ֮ǰ��Ҫ�жϵ�ǰ�ĵ����Ƿ�ı��ˣ����ڸı���ɫ
    (if (or (not current_feature) (not (equal current_feature feature_code)));���current_featureΪ�գ�����current_feature����ȡ��feature_code����ȣ����ǵ������ͷ����˱仯
      (progn
        (setq current_feature feature_code);���ĵ�ǰ����ı���,����һ�����������һ��
        (setq feature_color (get_color feature_code));������ɫ
        ;(setq start_pt current_pt)
        (setq current_type connect_type) ;������ı�ʱ����¼��ǰ�������������
        (if (= point_type "B")
          (setq nochange_start_pt current_pt));ֻ���µ�������nochange_start_pt

        (setq prev_pt current_pt);�������㶼��ɵ�ǰ�ĵ�

        ;(command "_.-layer" "_m" feature_code "_c" feature_color feature_code "");_m������ͼ�㣬����Ϊfeature_code��_c��������ɫ����ɫΪfeature_color��������feature_codeͼ����,��󡰡���ʾ���д�����൱�ڻس�
        ;(command "_.-layer" "_s" feature_code "");_s����ͼ��,����Ϊfeature_code
      )
    )
    (if (and prev_pt (not (= point_type "B")));��һ�㲻Ϊ�գ��Ҳ�Ϊ��㣬�Ϳ�ʼ��ͼ
      (progn
        (cond
          ;ֱ��
          ((= connect_type 1)
            ;(command "_.line" prev_pt current_pt "");����pline��ͼ����prev_pt��current_pt
            (progn
            (entmake (list 
           (cons 0 "LINE")          ; ͼԪ����
           (cons 8 current_feature) ; ͼ��
           (cons 10 prev_pt)        ; ���
           (cons 11 current_pt)       ; �յ�
           ;(cons 62 feature_color)  ; ��ɫ
           (cons 6 "Continuous")       ; ����
           ))
  )
          )
          ;��������
          ((= connect_type 3)
            ;(command "_.-layer" "_s" feature_code "" "");�л�����ǰͼ��
            ;(command "_.spline" prev_pt current_pt "");spline����������
            ;(command "") ; ����������
            ;(command "") ; ȷ���������Ĭ�ϣ�
            ;(command "") ; ȷ�϶˵�����Ĭ�ϣ�
            (if (= point_type "B") 
    (progn
      ; ��㣺
      (setq spline_points nil)
      (setq spline_points (list prev_pt));�Ȱ���һ����ӽ���
    )
  )
  
  (if (= point_type "M")
    (progn
      ; �м�㣺��ӵ��������
      (setq spline_points (append spline_points (list prev_pt)))
    )
  )
  
(if (= point_type "E")
    (progn
      ; �յ㣺����������������������
      (setq spline_points (append spline_points (list prev_pt current_pt)))
      (paint_spline spline_points)
      (setq spline_points nil)
    )
  )
          )
          ;Բ��
          ((= connect_type 2)
            (command "_.-layer" "_s" feature_code "" "");�л�����ǰͼ��
            (command "_.pline" prev_pt "a" current_pt "");pline����ģʽ��Բ��
          )
        )
      )
    )
    (setq prev_pt current_pt);������һ�������
    ;���洦��һ���յ�
    (cond 
      ;E
      ((= point_type "E")
       (setq current_feature nil);��current_feature��գ����԰�start_pt����һ��
       ;(setq start_pt nil)
       (setq prev_pt nil);�����궼���³�ʼһ�£���Ϊ�գ���ʾ����
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
           (cons 0 "LINE")          ; ͼԪ����
           (cons 8 current_feature) ; ͼ��
           (cons 10 current_pt)        ; ���
           (cons 11 nochange_start_pt); �յ�
           ;(cons 62 feature_color)  ; ��ɫ
           (cons 6 "Continuous")       ; ����
           ))
  )
              )
              ((= current_type 3)
                (command "_.-layer" "_s" feature_code "" "");�л�����ǰͼ��
                (command "_.spline" current_pt nochange_start_pt "")
                (command "") ; ����������
                (command "") ; ȷ���������Ĭ�ϣ�
                (command "") ; ȷ�϶˵�����Ĭ�ϣ�
              )
              ((= current_type 2)
                (command "_.-layer" "_s" feature_code "" "");�л�����ǰͼ��
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
  (princ (strcat "\n�� " (itoa (length points_info)) " ���㡣"))
  (princ)
)