;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TODO:
;;;;
;;;;    -Better printing (ncurses?)
;;;;    -Allow for more than 26 columns
;;;;            -Only after better printing and/or mouse support. It's really hard to see which
;;;;            square is which, even on moderately sized boards.
;;;;    -Better error messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun trim-whitespace (str)
  (let ((whitespace #(#\Space #\Tab)))
    (string-trim whitespace str)))

(defmacro with-game-result (expr &rest end-forms)
  `(case (catch 'game-result
            ,expr)
     ,@end-forms
     (t (error "We shouldn't be here."))))

(defun win ()
  (throw 'game-result 'win))

(defun lose ()
  (throw 'game-result 'lose))

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

(defmethod make-marked ((cell cell))
  (cond
    ((eq (visibility cell) :marked)
     (setf (slot-value cell 'visible) :invisible)
     -1)  
    (t
     (setf (slot-value cell 'visible) :marked)
     1)))

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

(defmacro call-ignoring-out-of-bounds (fun board x y default-val &rest more-args)
  "Calls (fun board x y), ignoring any out-of-bound errors"
  `(if (and (>= ,x 0)
            (>= ,y 0)
            (< ,x (width ,board))
            (< ,y (height ,board)))
       (apply ,fun ,board ,x ,y ,more-args)
       ,default-val))
  
(defmethod pos ((board board) x y)
  (aref (board board) y x))

(defmethod insert-mine ((board board) x y)
  (make-mine (pos board x y)))

(defmethod mark ((board board) x y)
  (incf (num-marked board) (make-marked (pos board x y))))

(defmethod click ((board board) x y)
  (let ((c (pos board x y)))
    (unless (member (visibility c) '(:visible :marked))
      (make-visible c)
      (incf (num-visible board))
      (cond
        ((minep c)
         (lose))
        ((zerop (value c))
         (loop for (i j) in '((0 -1) (1 0) (0 1) (-1 0))
               ;;ignore out-of-bounds errors to simplify code around edges of board
               do (call-ignoring-out-of-bounds #'click board (+ x i) (+ y j) nil)))
        ;;otherwise do nothing else
        ))))

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
                 do (unless (minep (pos board x y)) 
                      (loop for k from -1 to 1
                            with total = 0
                            do (loop for l from -1 to 1
                                     ;;ignore out-of-bounds errors to simplify code around edges of board
                                     when (call-ignoring-out-of-bounds
                                            (lambda (&rest args) (minep (apply #'pos args)))
                                            board (+ x k) (+ y l)
                                            nil)
                                     do (incf total))
                            finally (setf (slot-value (pos board x y) 'value) total))))))

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
    (format t "~a ~a ~a ~a ~a~%" input action x y pos)
    (setf x (cond ((and (char= (char input pos) #\F)
                        (char>= (char input (1+ pos)) #\A)
                        (char<= (char input (1+ pos)) #\Z))
                   (progn
                     (setf action :mark)  
                     (incf pos)
                     (format t "~a ~a ~a ~a ~a~%" input action x y pos)
                     (- (char-code (char input pos)) (char-code #\A))))
                  ((and (char>= (char input pos) #\A) (char<= (char input pos) #\Z))
                   (- (char-code (char input pos)) (char-code #\A)))
                  (t nil)))
    (handler-case (setf y (parse-integer input :start (1+ pos) :junk-allowed t))
      (error nil))
    (format t "~a ~a ~a ~a ~a~%" input action x y pos)
    (if (or (null x)
            (null y)
            (not (typep x (list 'integer 0 max-x)))
            (not (typep y (list 'integer 0 max-y))))
        nil
        (list action x y))))

(defun print-welcome-message ()
  (format t "Welcome to minesweeper.~%"))

(defun print-status-line (board starting-time)
  (format t "~2%Mines remaining: ~d Time: ~d~%" (- (num-mines board) (num-marked board))
                                                (- (get-universal-time) starting-time)))

(defun validate-size (line)
  "Returns either a list containing 2 positive integers or nil."
  (let ((width 0)
        (height 0))
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
                     (:click (format t "~a asdf~%" action) (click game-board x y))
                     (:mark (format t "here~%") (mark game-board x y)))
                   (when (= (num-mines game-board) (- (size game-board)
                                                      (num-visible game-board)))
                     (win))
                   (pprint-board game-board starting-time))
                 (format t "Invalid selection. "))))
      (win (pprint-board game-board starting-time) (format t "Congratulations! You won!~%"))
      (lose (pprint-board game-board starting-time) (format t "You lost.~%"))))
  (values))

(defun debug-print (b)
  (loop for y from 0 below (height b)
        do (format t "~d" y)
           (loop for x from 0 below (width b)
                 do (format t " ~a " (value (pos b x y))))
           (terpri)))

(game-loop)
