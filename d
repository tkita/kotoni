diff --git a/kotoni-fsm.el b/kotoni-fsm.el
index 0afca7a..09a7315 100644
--- a/kotoni-fsm.el
+++ b/kotoni-fsm.el
@@ -223,6 +223,7 @@
                   (t 0)))
          (is-upper  (and (<= ?A ch-int) (<= ch-int ?Z)))
          (lower-ch  (if is-upper (+ ch-int 32) ch-int))
+         (is-alpha  (and (<= ?a lower-ch) (<= lower-ch ?z)))
          (is-number (and (<= ?1 ch-int) (<= ch-int ?9))))
 
     (cond
@@ -245,6 +246,17 @@
         (ktn-select-nth-candidate index))
       (ktn-kakutei))
 
+     ;; 4. 【修正】変換中に小文字アルファベットが入力された場合：
+     ;;    現在の変換を確定し、打たれた文字をそのまま通常入力（normal）として処理する
+     (is-alpha
+      (let ((char-str (ktn--safe-char-to-string lower-ch)))
+        (ktn-kakutei)
+        ;; 確定後の通常モード（normal）でその文字を改めて処理・挿入する
+        (let* ((res (ktn--process-roma-input char-str)))
+          (unless (string-empty-p (car res))
+            (insert (car res)))
+          (ktn--state-set :roma-buffer (cdr res)))))
+
      ;; 4. その他の変換中操作（必要に応じてスペースや次候補など）
      (t
       ;; 必要に応じたデフォルトの変換中キー処理
diff --git a/test/ktn-test.el b/test/ktn-test.el
index 4a76c1d..bb9613b 100644
--- a/test/ktn-test.el
+++ b/test/ktn-test.el
@@ -141,6 +141,12 @@
           "K a n j i SPC x C-j")))
     (should (string-match-p "幹事" result))))
 
+(ert-deftest ktn-continuous/test1 ()
+  (let ((result
+         (ktn-e2e-test
+          "K a n j i SPC s a i C-j")))
+    (should (string-match-p "漢字さい" result))))
+
 (ert-deftest ktn-continuous-conversion/test1 ()
   (let ((result
          (ktn-e2e-test
