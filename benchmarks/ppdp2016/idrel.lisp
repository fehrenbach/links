#!/usr/bin/sbcl --script
(defconstant N (read-from-string (cadr *posix-argv*)))
(defconstant M (read-from-string (caddr *posix-argv*)))

(loop for j from 1 to M do
      (format t "DROP TABLE IF EXISTS i_s_c_o_~a_~a;~%" N j)
      (format t "CREATE TABLE i_s_c_o_~a_~a (i INTEGER PRIMARY KEY, s TEXT, cardinal TEXT, ordinal TEXT) WITH OIDS;~%" N j)
      (format t "INSERT INTO i_s_c_o_~a_~a (i, s, cardinal, ordinal) VALUES~%" N j)
      (loop for i from 1 to (1- N) do
            (format t "(~d,'~d','~R','~:R'),~%" i i i i)
            finally
            (format t "(~d,'~d','~R','~:R');~%" i i i i)
      ))
