;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TODO:
;;;;
;;;;    -Allow for more than 26 columns
;;;;    -Ask for board size on startup
;;;;    -Print status bar
;;;;    -Better printing (ncurses?)
;;;;    -Allow mines to be marked
;;;;    -Make this Work on other implementations
;;;;            -Currently, only sbcl is supported
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cell ()
  ((visible :reader visiblep :type boolean :initform nil)
   (value :reader value :type fixnum :initform 0)))

(defmethod make-visible ((cell cell))
  (setf (slot-value cell 'visible) t))

(defmethod minep ((cell cell))
  "Is the cell a mine?"
  (= -1 (value cell)))

(defmethod make-mine ((cell cell))
  (setf (slot-value cell 'value) -1))

(defun print-value (cell)
  "Returns a string representing what the cell should be pretty-printed as."
  (if (visiblep cell)
      (cond 
        ((minep cell) "*")
        ((zerop (value cell)) " ")
        (t (value cell)))
      "."))

(defclass board ()
  ((board :accessor board :initform (make-array '(5 5)))
   (num-mines :reader num-mines :type (integer 0 *))
   (num-visible :accessor num-visible :type (integer 0 *) :initform 0)))

(defmethod width ((board board))
  (array-dimension (board board) 1))

(defmethod height ((board board))
  (array-dimension (board board) 0))

(defmethod pos ((board board) x y)
  (aref (board board) y x))

(defmethod insert-mine ((board board) x y)
  (make-mine (pos board x y)))

(defmethod click ((board board) x y)
  (let ((c (pos board x y)))
    (unless (visiblep c)
      (make-visible c)
      (incf (num-visible board))
      (cond
        ((minep c)
         (throw 'game-over :lost))
        ((zerop (value c))
         (loop for i from -1 to 1
               do (loop for j from -1 to 1
                        ;;ignore out-of-bounds errors to simplify code around edges of board
                        do (handler-case (click board (+ x i) (+ y j))
                             #+sbcl (sb-int:invalid-array-index-error (var) (declare (ignore var)))))))
        ;;otherwise do nothing else
        ))))

(defmethod initialize-instance :after ((board board) &key)
  "Places mines and sets numbers accordingly."
  (declare (optimize (debug 3) (speed 0) (safety 3)))
  (loop for i from 0 below (* (height board) (width board))
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
                                     when (handler-case (minep (pos board (+ x k) (+ y l)))
                                            #+sbcl (sb-int:invalid-array-index-error (var) (declare (ignore var)) nil))
                                     do (incf total))
                            finally (setf (slot-value (pos board x y) 'value) total))))))

(defun pprint-board (board)
  "Prints the board all pretty like"
  (format t " ")
  (loop for x from 0 below (width board)
        do (format t " ~c " (code-char (+ x (char-code #\A)))))
  (terpri)
  (loop for y from 0 below (height board)
        do (format t "~d" y)
           (loop for x from 0 below (width board)
                 do (format t " ~a " (print-value (pos board x y))))
           (terpri)))

(defun prompt (str prompt &rest args)
  (apply #'format str prompt args)
  (finish-output)
  (read-line))

(defun get-user-input ()
  (read-line))

(defun parse-input (input max-x max-y)
  "Validates and parses input. The return value is either a list of 2 non-negative integers (x y) or nil.
   
   Only returns nil when the input is invalid."
  (let ((start-of-string (position-if-not (lambda (x) (member x '(#\Space #\Tab))) input))
        (x nil)
        (y nil))
    (setf x (when start-of-string
              (let ((col (char-upcase (char input start-of-string))))
                (when (and (char>= col #\A) (char<= col #\Z))
                  (- (char-code col) (char-code #\A))))))
    (handler-case (setf y (parse-integer input :start (1+ start-of-string) :junk-allowed t))
      (error nil))
    (if (or (null x)
            (null y)
            (not (typep x (list 'integer 0 max-x)))
            (not (typep y (list 'integer 0 max-y))))
        nil
        (list x y))))

(defun print-welcome-message ()
  (format t "Welcome to minesweeper.~%"))

(defun game-loop ()
  (print-welcome-message)
  (setf *random-state* (make-random-state t))
  (let ((game-board (make-instance 'board)))
    (pprint-board game-board)
    (case (catch 'game-over
             (loop
               do (let ((parsed-input (parse-input (prompt t "Pick a square: ")
                                                   (1- (width game-board))
                                                   (1- (height game-board)))))
                    (if parsed-input
                        (destructuring-bind (x y) parsed-input
                          (click game-board x y)
                          (when (= (num-mines game-board) (- (* (width game-board) (height game-board))
                                                             (num-visible game-board)))
                            (throw 'game-over :won))
                          (pprint-board game-board))
                        (format t "Invalid selection. ")))))
      (:lost (pprint-board game-board) (format t "You lost."))
      (:won (pprint-board game-board) (format t "Congratulations! You won!"))
      (t (error "We shouldn't be here"))))
  (values))

(defun debug-print (b)
  (loop for y from 0 below (height b)
        do (format t "~d" y)
           (loop for x from 0 below (width b)
                 do (format t " ~a " (value (pos b x y))))
           (terpri)))

(game-loop)
