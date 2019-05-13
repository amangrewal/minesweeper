;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TODO:
;;;;
;;;;    -Better printing (ncurses?)
;;;;    -Allow for more than 26 columns
;;;;            -Only after better printing and/or mouse support. It's really hard to see which
;;;;            square is which, even on moderately sized boards.
;;;;    -Better error messages
;;;;    -Include instructions
;;;;
;;;;    -Tested with ccl, cmucl, ecl, sbcl
;;;;            -clisp works, but with subpar printing in a shell.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#-clisp (require 'asdf)
#-clisp (require 'uiop)

(defun trim-whitespace (str)
  (let ((whitespace #(#\Space #\Tab)))
    (string-trim whitespace str)))

(defun clear-screen ()
  ;;using swank here is probably too liberal. I just want to prevent any shenanigans when 
  ;;using slime or similar.
  #-(or swank clisp os-windows) (uiop:run-program "clear" :output *standard-output*))

(defmacro with-game-result (expr &rest end-forms)
  `(case (catch 'game-result
            ,expr)
     ,@end-forms
     (t (error "We shouldn't be here."))))

(defun win ()
  (throw 'game-result 'win))

(defun lose ()
  (throw 'game-result 'lose))

(defmacro with-cell ((var board x y) &body body)
  `(let ((,var (pos ,board ,x ,y)))
     ,@body))

(defclass cell ()
  ((visible :reader visibility :type keyword :initform :invisible)
   (value :reader value :type fixnum :initform 0)))

(defmethod make-visible ((cell cell))
  (setf (slot-value cell 'visible) :visible))

(defmethod minep ((cell cell))
  "Is the cell a mine?"
  (= -1 (value cell)))

(defmethod make-mine ((cell cell))
  (setf (slot-value cell 'value) -1))

(defmethod toggle-mark ((cell cell))
  (case (visibility cell)
    (:marked
     (setf (slot-value cell 'visible) :invisible)
     -1)
    (:invisible
     (setf (slot-value cell 'visible) :marked)
     1)
    (:visible
      0)))

(defun print-value (cell)
  "Returns a string representing what the cell should be pretty-printed as."
  (case (visibility cell)
      (:visible (cond
                  ((minep cell) "*")
                  ((zerop (value cell)) " ")
                  (t (value cell))))
      (:marked "F")
      (:invisible ".")))

(defclass board ()
  ((board :accessor board)
   (num-mines :reader num-mines :type (integer 0 *))
   (num-visible :accessor num-visible :type (integer 0 *) :initform 0)
   (num-marked :accessor num-marked :type (integer 0 *) :initform 0)))

(defmethod width ((board board))
  (array-dimension (board board) 1))

(defmethod height ((board board))
  (array-dimension (board board) 0))

(defmethod size ((board board))
  (* (width board) (height board)))

(defmacro loop-around-pos (func board x y default-val)
  (let ((i (gensym "i"))
        (j (gensym "j")))
    `(loop for ,j from -1 to 1
           do (loop for ,i from -1 to 1
                    do (call-ignoring-out-of-bounds ,func
                                                    ,board (+ ,x ,i) (+ ,y ,j)
                                                    ,default-val)))))

;;This macro is used to simplify the code around the edges of the board.
(defmacro call-ignoring-out-of-bounds (fun board x y default-val)
  "Calls (fun board x y), ignoring any out-of-bound errors"
  `(if (and (>= ,x 0)
            (>= ,y 0)
            (< ,x (width ,board))
            (< ,y (height ,board)))
       (funcall ,fun ,board ,x ,y)
       ,default-val))

(defmethod pos ((board board) x y)
  (aref (board board) y x))

(defmethod insert-mine ((board board) x y)
  (make-mine (pos board x y)))

(defmethod mark ((board board) x y)
  (incf (num-marked board) (toggle-mark (pos board x y))))

(defmethod click ((board board) x y)
  (with-cell (c board x y)
    (unless (member (visibility c) '(:visible :marked))
      (make-visible c)
      (incf (num-visible board))
      (cond
        ((minep c)
         (lose))
        ((zerop (value c))
         (loop for (i j) in '((0 -1) (1 0) (0 1) (-1 0))
               do (call-ignoring-out-of-bounds #'click board (+ x i) (+ y j) nil)))
        ;;otherwise do nothing else
        ))))

(defmethod middle-click ((board board) x y)
  "If the cell has the correct number of flags adjacent to it,
   this will click on all the other cells adjacent to it.

   In a graphical situation this is usually done by middle-clicking
   or clicking both mouse buttons simultaneously."
  (with-cell (c board x y)
    (when (eq (visibility c) :visible)
      (let ((num-flags 0))
        (loop-around-pos (lambda (board x y)
                           (with-cell (c board x y)
                             (when (eq (visibility c) :marked)
                               (incf num-flags 1))))
                         board x y
                         nil)
        (when (= num-flags (value c))
          (loop-around-pos #'click board x y nil))))))


(defmethod initialize-instance :after ((board board) &key size)
  "Places mines and sets numbers accordingly."
  (declare (optimize (debug 3) (speed 0) (safety 3)))
  (setf (board board) (make-array size))
  (loop for i from 0 below (size board)
        do (setf (row-major-aref (board board) i) (make-instance 'cell)))
  (setf (slot-value board 'num-mines) (ceiling (* (width board) (height board))
                                               10))
  (let ((mine-locations (do ((i 0)
                             (mines '())
                             (location (cons (random (width board)) (random (height board)))
                                       (cons (random (width board)) (random (height board)))))
                            ((>= i (num-mines board)) mines)
                            (unless (member location mines :test #'equal)
                              (push location mines)
                              (incf i)))))
    (loop for (x . y) in mine-locations
          do (insert-mine board x y)))
  (loop for y from 0 below (height board)
        do (loop for x from 0 below (width board)
                 do (with-cell (c board x y)
                      (unless (minep c)
                        (let ((total 0))
                          (loop-around-pos (lambda (board x y)
                                             (with-cell (c board x y)
                                               (when (minep c)
                                                 (incf total))))
                                           board x y
                                           nil)
                          (setf (slot-value c 'value) total)))))))

(defun print-status-line (board starting-time)
  (format t "Mines remaining: ~d Time: ~d~%" (- (num-mines board) (num-marked board))
                                                (- (get-universal-time) starting-time)))
(defun pprint-board (board starting-time)
  "Prints the board all pretty like"
  (print-status-line board starting-time)
  (let ((first-column-width (floor (1+ (log (height board) 10)))))
    (format t "~va" first-column-width " ")
    (loop for x from 0 below (width board)
          do (format t " ~c " (code-char (+ x (char-code #\A)))))
    (terpri)
    (loop for y from 0 below (height board)
          do (format t "~vd" first-column-width y)
          (loop for x from 0 below (width board)
                do (format t " ~a " (print-value (pos board x y))))
          (terpri))))

(defun prompt (str prompt &rest args)
  (apply #'format str prompt args)
  (finish-output)
  (read-line))

(defun get-user-input ()
  (read-line))

(defun parse-input (input max-x max-y)
  "Validates and parses input. The return value is either a list of 2 non-negative integers (x y) or nil.

   Only returns nil when the input is invalid."
  (let ((input (string-upcase (trim-whitespace input)))
        (action :click)
        (x nil)
        (y nil)
        (pos 0))
    (setf x (cond ((and (member (char input pos) '(#\F #\D))
                        (char>= (char input (1+ pos)) #\A)
                        (char<= (char input (1+ pos)) #\Z))
                   (progn
                     (case (char input pos)
                       (#\F (setf action :mark))
                       (#\D (setf action :middle-click)))
                     (incf pos)
                     (- (char-code (char input pos)) (char-code #\A))))
                  ((and (char>= (char input pos) #\A) (char<= (char input pos) #\Z))
                   (- (char-code (char input pos)) (char-code #\A)))
                  (t nil)))
    (handler-case (setf y (parse-integer input :start (1+ pos) :junk-allowed t))
      (error nil))
    (if (or (null x)
            (null y)
            (not (typep x (list 'integer 0 max-x)))
            (not (typep y (list 'integer 0 max-y))))
        nil
        (list action x y))))

(defun print-welcome-message ()
  (format t "Welcome to minesweeper.~%"))

(defun validate-size (line)
  "Returns either a list containing 2 positive integers or nil."
  (let ((height 0))
    (handler-case (multiple-value-bind (width pos) (parse-integer line :junk-allowed t)
                    (setf height (parse-integer line :start (1+ pos)))
                    (if (and width
                             height
                             (>= width 3)
                             (>= height 3)
                             (<= width 26))
                        (list height width)
                        (format t "Invalid size, using default (5x5).~%")))
      (error nil))))

(defun game-loop ()
  (clear-screen)
  (print-welcome-message)
  (setf *random-state* (make-random-state t))

  (let* ((size (validate-size (prompt t "Please choose a board size: ")))
         (game-board (make-instance 'board :size (or size '(5 5))))
         (starting-time (get-universal-time)))
    (pprint-board game-board starting-time)
    (with-game-result
      (loop
        do (let ((parsed-input (parse-input (prompt t "Pick a square: ")
                                            (1- (width game-board))
                                            (1- (height game-board)))))
             (if parsed-input
                 (destructuring-bind (action x y) parsed-input
                   (case action
                     (:click (click game-board x y))
                     (:mark (mark game-board x y))
                     (:middle-click (middle-click game-board x y))) 
                   (when (= (num-mines game-board) (- (size game-board)
                                                      (num-visible game-board)))
                     (win))
                   (clear-screen)
                   (pprint-board game-board starting-time))
                 (format t "Invalid selection. "))))
      (win (clear-screen)
           (pprint-board game-board starting-time)
           (format t "Congratulations! You won!~%"))
      (lose (clear-screen)
            (pprint-board game-board starting-time)
            (format t "You lost.~%"))))
  (values))

(defun debug-print (b)
  (loop for y from 0 below (height b)
        do (format t "~d" y)
           (loop for x from 0 below (width b)
                 do (format t " ~a " (value (pos b x y))))
           (terpri)))

(game-loop)
