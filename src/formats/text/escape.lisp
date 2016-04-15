(in-package #:prometheus.formats.text)

;; http://cl-cookbook.sourceforge.net/strings.html#manip
(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
          while pos)))

(defun escape-metric-help (help)
  (replace-all (replace-all help "\\" "\\\\")
               #.(string #\Newline) "\\n"))

(defun escape-label-value (help)
  (replace-all
   (replace-all
    (replace-all
     help
     "\\" "\\\\")
    #.(string #\Newline) "\\n")
   "\"" "\\\""))
