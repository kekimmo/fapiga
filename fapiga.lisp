
(ql:quickload "lispbuilder-sdl")

(defpackage :fapiga
  (:use :cl :sdl))

(in-package :fapiga)

(defparameter *window-width* 800)
(defparameter *window-height* 600)
(defparameter *brick-width* 24)
(defparameter *brick-height* 24)
(defparameter *field-width* 10)
(defparameter *field-height* 20)
(defparameter *kill-row* 2)
(defparameter *field* nil)
(defparameter *field-x* 3)
(defparameter *field-y* 2)
(defparameter *next-x* (+ *field-x*
                          *field-width*
                          2
                          1))
(defparameter *next-y* *field-y*)
(defparameter *piece* nil)
(defparameter *field-top-brick* *field-height*)


(defstruct form
  width
  height
  left
  right
  top
  bottom
  bricks)


(defun all-in-column-p (p arr height x)
  (loop for y from 0 below height
        always (funcall p (aref arr y x))))

(defun all-in-row-p (p arr width y)
  (loop for x from 0 below width
        always (funcall p (aref arr y x))))

(defun column-not-empty-p (arr height x)
  (not (all-in-column-p #'zerop arr height x)))

(defun row-not-empty-p (arr width y)
  (not (all-in-row-p #'zerop arr width y)))


(defun list->form (contents)
  (let* ((height (length contents))
         (width (length (first contents)))
         (bricks (make-array (list height width) :initial-contents contents))
         left right top bottom)
    (loop for x from 0 below width
          if (column-not-empty-p bricks height x)
          minimize x into min
          and maximize x into max
          finally (setf left min
                        right max))
    (loop for y from 0 below height
          if (row-not-empty-p bricks width y)
          minimize y into min
          and maximize y into max
          finally (setf top min
                        bottom max))
    (make-form :width width
               :height height
               :left left
               :right right
               :top top
               :bottom bottom
               :bricks bricks)
    ))


(defun form-brick-at-p (form x y)
  (not (zerop (aref (form-bricks form) y x))))


(defclass piece-type ()
  ((name :initarg :name
         :reader piece-type-name)
   (brick-type :initarg :brick-type
               :reader piece-type-brick-type)
   (forms :initarg :forms
          :reader piece-type-forms)))

(defparameter *piece-types* (list))
(defparameter *brick-types* (list))

(defun define-piece-type (name brick-type &rest form-lists)
  (setf *piece-types* (acons name
                             (make-instance 'piece-type
                                            :name name
                                            :brick-type brick-type
                                            :forms (make-array (length form-lists)
                                                               :initial-contents
                                                               (mapcar #'list->form form-lists)))
                             *piece-types*))
  (pushnew brick-type *brick-types*)
  )

(defun random-element (seq)
  (elt seq (random (length seq))))

(defun random-piece-type ()
  (cdr (random-element *piece-types*)))

(defmacro rgb (r g b)
  `(sdl:color :r ,r :g ,g :b ,b))

(define-piece-type :I (rgb 0 255 255)
                   '((0 0 0 0)
                     (1 1 1 1)
                     (0 0 0 0)
                     (0 0 0 0))

                   '((0 0 1 0)
                     (0 0 1 0)
                     (0 0 1 0)
                     (0 0 1 0))

                   '((0 0 0 0)
                     (0 0 0 0)
                     (1 1 1 1)
                     (0 0 0 0))

                   '((0 1 0 0)
                     (0 1 0 0)
                     (0 1 0 0)
                     (0 1 0 0))
                   )

(define-piece-type :J (rgb 0 0 255)
                   '((1 0 0)
                     (1 1 1)
                     (0 0 0))

                   '((0 1 1)
                     (0 1 0)
                     (0 1 0))

                   '((0 0 0)
                     (1 1 1)
                     (0 0 1))

                   '((0 1 0)
                     (0 1 0)
                     (1 1 0))
                   )

(define-piece-type :L (rgb 255 170 0)
                   '((0 0 1)
                     (1 1 1)
                     (0 0 0))

                   '((0 1 0)
                     (0 1 0)
                     (0 1 1))

                   '((0 0 0)
                     (1 1 1)
                     (1 0 0))

                   '((1 1 0)
                     (0 1 0)
                     (0 1 0))
                   )

(define-piece-type :O (rgb 255 255 0)
                   '((1 1)
                     (1 1)))

(define-piece-type :S (rgb 0 255 0)
                   '((0 1 1)
                     (1 1 0)
                     (0 0 0))

                   '((0 1 0)
                     (0 1 1)
                     (0 0 1))

                   '((0 0 0)
                     (0 1 1)
                     (1 1 0))

                   '((1 0 0)
                     (1 1 0)
                     (0 1 0))
                   )

(define-piece-type :Z (rgb 255 0 0)
                   '((1 1 0)
                     (0 1 1)
                     (0 0 0))

                   '((0 0 1)
                     (0 1 1)
                     (0 1 0))

                   '((0 0 0)
                     (1 1 0)
                     (0 1 1))

                   '((0 1 0)
                     (1 1 0)
                     (1 0 0))
                   )

(define-piece-type :T (rgb 153 0 255)
                   '((0 1 0)
                     (1 1 1)
                     (0 0 0))

                   '((0 1 0)
                     (0 1 1)
                     (0 1 0))

                   '((0 0 0)
                     (1 1 1)
                     (0 1 0))

                   '((0 1 0)
                     (1 1 0)
                     (0 1 0))
                   )



(defclass piece ()
  ((x :initarg :x
      :reader piece-x)
   (y :initarg :y
      :reader piece-y)
   (form-index :initarg :form-index
               :reader piece-form-index)
   (type :initarg :type
         :reader piece-type))
  (:default-initargs :x 0 :y 0 :form-index 0)
  )


(defun piece-current-form (piece)
  (with-slots (form-index type) piece
    (aref (piece-type-forms type) form-index)))

(defun piece-brick-type (piece)
  (piece-type-brick-type (piece-type piece)))

(defun piece-top (piece)
  (+ (piece-y piece)
     (form-top (piece-current-form piece))))

(defun piece-right (piece)
  (+ (slot-value piece 'x)
     (form-right (piece-current-form piece))))

(defun piece-bottom (piece)
  (+ (slot-value piece 'y)
     (form-bottom (piece-current-form piece))))

(defun piece-width (piece)
  (form-width (piece-current-form piece)))

(defun piece-bounding-edges (piece)
  (with-slots (x y) piece
    (with-slots (width height) (piece-current-form piece)
      (values x y (+ x width -1) (+ y height -1)))))

(defun piece-edges (piece)
  (with-slots (left right top bottom) (piece-current-form piece)
    (values left top right bottom)
    )
  )

(defun piece-copy-altered (piece &key
                                 (x (piece-x piece))
                                 (y (piece-y piece))
                                 (type (piece-type piece))
                                 (form-index (piece-form-index piece))
                                 )
  (make-instance 'piece
                 :x x
                 :y y
                 :type type
                 :form-index form-index
                 ))

(defun piece-fall (piece)
  (piece-copy-altered piece :y (1+ (piece-y piece))))

(defun piece-move (piece dir)
  (piece-copy-altered piece :x (+ (piece-x piece) (ecase dir (:left -1) (:right 1)))))

(defun piece-rotate (piece dir)
  (piece-copy-altered piece :form-index (mod (+ (ecase dir (:left -1) (:right 1))
                                                (piece-form-index piece))
                                             (length (piece-type-forms (piece-type piece))))))

(defmacro field-top-brick (field)
  '*field-top-brick*)

(defmacro field-row (field y)
  `(aref ,field ,y))

(defun field-brick-at (field x y)
  (aref (field-row field y) x))

(defun field-set-brick-at (field x y brick-type)
  (when (< y (field-top-brick field))
    (setf (field-top-brick field) y))
  (setf (aref (aref field y) x) brick-type)
  brick-type)

(defsetf field-brick-at field-set-brick-at)

(defparameter *textures* (list))
(defparameter *textures-colorize-color* (rgb 255 255 0))

(define-condition textures-not-found (error)
  ((kind :initarg :kind
         :reader kind))
  (:report (lambda (c stream) (format stream "Textures not found: ~a." (kind c))))
  )

(defun get-textures (kind)
  (or (first (getf *textures* kind))
      (error 'textures-not-found :kind kind)))

(defun colorize-pixel (pixel-color color)
  (if (sdl:color= pixel-color *textures-colorize-color*)
    color
    pixel-color))

(defun colorize-image (image colorize-color)
  (let ((new-image (sdl:copy-surface :surface image :fill nil :color-key-fill nil)))
    (dotimes (x (sdl:width image))
      (dotimes (y (sdl:height image))
        (sdl:draw-pixel-* x y :surface new-image
                          :color (colorize-pixel (sdl:read-pixel-* x y :surface image)
                                                 colorize-color))))
    new-image))

(defun load-texture (source)
  (format t "Loading texture ~a... " source)
  (prog1
    (sdl:convert-to-display-format :surface
                                   (sdl:load-image source :color-key (rgb 255 255 255)))
    (format t "OK.~%")))

(defun load-and-colorize (source colors)
  (let ((img (load-texture source)))
    (format t "Colorizing texture ~a...~%" source)
    (prog1
      (loop with textures = (make-hash-table)
            for color in colors
            do (setf (gethash color textures) (colorize-image img color))
            finally (return textures))
      (format t "OK.~%"))))

(defun load-textures-in (dir)
  (loop for file in (directory (make-pathname :directory (list :relative dir)
                                              :name :wild
                                              :type "bmp"))
        collect (load-texture file))
  )

(defun load-textures ()
  (format t "* Textures *~%")
  (setf (getf *textures* :brick)
        (loop for file in (directory (make-pathname :directory '(:relative "images/bricks")
                                                    :name :wild
                                                    :type "bmp"))
              collect (load-and-colorize file *brick-types*)))
  (setf (getf *textures* :field) (load-textures-in "images/field"))
  (setf (getf *textures* :next) (load-textures-in "images/next"))
  (setf (getf *textures* :game-over) (load-textures-in "images/game-over"))
  (format t "* /Textures *~%")
  )


(defun transform-field-coords (x y)
  (values (+ *field-x* (* *brick-width* x))
          (+ *field-y* (* *brick-height* y)))
  )


(defun draw-brick (x y brick-type)
  (multiple-value-bind (screen-x screen-y) (transform-field-coords x y)
    ;(sdl:draw-box-* screen-x screen-y *brick-width* *brick-height*
    ;                :color brick-type :stroke-color (rgb 50 50 50))
    (sdl:draw-surface-at-* (gethash brick-type (get-textures :brick))
                           screen-x screen-y)
    ))


(defun draw-field (x y field)
  ;(sdl:draw-box-* x y (* *field-width* *brick-width*) (* *field-height* *brick-height*)
  ;                :color (rgb 255 255 255))
  ;(sdl:draw-box-* x y (* *field-width* *brick-width*) (* (1+ *kill-row*) *brick-height*)
  ;                :color (rgb 255 200 200))
  ;
  (multiple-value-bind (screen-x screen-y) (transform-field-coords x y)
    (sdl:draw-surface-at-* (get-textures :field) screen-x screen-y)
    (loop for brick-x from 0 below *field-width*
          do (loop for brick-y from 0 below *field-height*
                   for brick = (field-brick-at field brick-x brick-y)
                   if brick do (draw-brick (+ 1 x brick-x) (+ 1 y brick-y) brick)))

    )
  )

(defun draw-piece (piece &key
                         (offset-x 0)
                         (offset-y 0)
                         (x (+ offset-x (piece-x piece)))
                         (y (+ offset-y (piece-y piece))))
  (with-accessors ((brick-type piece-brick-type) (form piece-current-form)) piece
    (loop for brick-y from 0 below (form-height form) do
          (loop for brick-x from 0 below (form-width form)
                if (form-brick-at-p form brick-x brick-y)
                do (draw-brick (+ x brick-x) (+ y brick-y) brick-type)))))


(defun draw-next (x y next-piece)
  (multiple-value-bind (screen-x screen-y) (transform-field-coords x y)
    (sdl:draw-surface-at-* (get-textures :next) screen-x screen-y))
  (draw-piece next-piece
              :x (+ x 1 (truncate (- 6 (piece-width next-piece)) 2))
              :y (+ y 5)))


(defun draw-game-over (x y)
  (multiple-value-bind (screen-x screen-y) (transform-field-coords x y)
    (sdl:draw-surface-at-* (get-textures :game-over) screen-x screen-y))
  )


(defun spawn-piece ()
  (make-instance 'piece :x (truncate *field-width* 3)
                 :type (random-piece-type))
  )


(defun collision-p (field piece)
  (with-accessors ((piece-x piece-x) (piece-y piece-y)) piece
    (multiple-value-bind (form-left
                           form-top 
                           form-right 
                           form-bottom) (piece-edges piece)
      (or (or (minusp (+ piece-x form-left))
              (minusp (+ piece-y form-top))
              (>= (+ piece-x form-right) *field-width*)
              (>= (+ piece-y form-bottom) *field-height*))

          (loop with form = (piece-current-form piece)
                for form-x from form-left upto form-right
                for x = (+ piece-x form-x)
                thereis (loop for form-y from form-top upto form-bottom
                              for y = (+ piece-y form-y)
                              thereis (and (form-brick-at-p form form-x form-y)
                                           (field-brick-at field x y)))))) 


    ))

(defun landed-p (field piece)
  (collision-p field (piece-fall piece)))


(defun lock (field piece)
  (multiple-value-bind (left top right bottom) (piece-edges piece)
    (loop with form = (piece-current-form piece)
          with brick-type = (piece-brick-type piece)
          with piece-x = (piece-x piece)
          with piece-y = (piece-y piece)
          for form-x from left upto right
          for x = (+ piece-x form-x)
          do (loop for form-y from top upto bottom
                   for y = (+ piece-y form-y)
                   if (form-brick-at-p form form-x form-y)
                   do (setf (field-brick-at field x y) brick-type)))))


(defparameter *key-map* '(
                          (:sdl-key-space . :drop)
                          (:sdl-key-up . :rotate-right)
                          (:sdl-key-down . :fall)
                          (:sdl-key-right . :right)
                          (:sdl-key-left . :left)
                          ))


(defun command-get (keys)
  (loop for (key . command) in *key-map*
        if (find key keys) return command))


(define-condition command-attempted ()
  ((command :initarg :command
            :reader command)))

(define-condition command-failed (command-attempted)
  ((reason :initarg :reason
           :reader reason)))


(defun command-perform (state command)
  (with-slots (field piece) state
    (let ((altered-piece (ecase command
                           ((:left :right) (piece-move piece command))
                           (:rotate-right (piece-rotate piece :right))
                           (:drop (loop for p = piece then (piece-fall p)
                                        until (landed-p field p)
                                        finally (return p)))
                           (:fall (piece-fall piece)))))
      (cond ((collision-p field altered-piece)
             (signal 'command-failed :command command :reason :no-room))
            (t
             (setf piece altered-piece)
             (signal 'command-attempted :command command))))))



(defun field-replace-row (field y row)
  (setf (field-row field y) row))

(defun field-move-row (field from to)
  (format t "Move row: ~d -> ~d.~%" from to)
  (field-replace-row field to (field-row field from)))

(defun field-make-row ()
  (make-array *field-width* :initial-element nil))

(defun field-clear-row (field y)
  (format t "Clear row: ~d.~%" y)
  (field-replace-row field y (field-make-row)))

(define-condition rows-removed ()
  ((rows :initarg :rows
         :reader rows)))

(defun remove-full-rows (field &optional (range-from 0) (range-upto (- *field-height* 1)))
  (format t "Check rows ~d-~d.~%" range-from range-upto)
  (let ((full-rows (loop for y from range-upto downto range-from
                         for row = (field-row field y)
                         if (every #'identity row) collect y
                         )))
    (if full-rows
      (loop with rows-removed = 0
            for y from range-upto downto (field-top-brick field)
            if (find y full-rows) do (incf rows-removed)
            else do (field-move-row field y (+ y rows-removed))
            finally (progn (loop for y2 from (+ y 1)
                                 repeat rows-removed
                                 do (field-clear-row field y2))
                           (incf (field-top-brick field) rows-removed)
                           (signal 'rows-removed :rows full-rows))))
    (length full-rows)
    )
  )


(defun make-field ()
  (let ((rows (make-array *field-height*)))
    (map-into rows #'field-make-row)))



(define-condition game-over ()
  ())

(defstruct state
  field
  (piece nil)
  (next-piece nil)
  (fall-counter 0)
  (points 0)
  (over nil))

(defparameter *fall-every-nth* 10)
(defparameter *points-for-row* 100)

(defun points-for-removals (row-count)
  (if (zerop row-count)
    0
    (* *points-for-row* row-count row-count)))

(define-condition points-earned ()
  ((points :initarg :points :reader points)
   (total :initarg :total :reader total)))

(defun add-points (state points)
  (with-slots ((total points)) state
    (incf total points)
    (signal 'points-earned :points points :total total)))

(define-condition game-already-over (error) ())

(defun state-advance (state &optional command)
  (with-slots (field piece next-piece fall-counter over) state
    (when over
      (error 'game-already-over))
    (loop until piece
          do (shiftf piece next-piece (spawn-piece)))
    (when command
      (command-perform state command))
    (let ((landed-p (landed-p field piece)))
      (when landed-p
        (lock field piece)
        (let* ((row-count (remove-full-rows field (piece-top piece) (piece-bottom piece)))
               (row-points (points-for-removals row-count)))
          (when (plusp row-points)
            (add-points state row-points)))
        (setf piece nil))
      (incf fall-counter)
      (when (= fall-counter *fall-every-nth*)
        (setf fall-counter 0)
        (unless landed-p
          (setf piece (piece-fall piece)))))
    (when (<= (field-top-brick field) *kill-row*)
      (setf over t)
      (signal 'game-over))
    ))


(defun sdl-main ()
  (sdl:window *window-width* *window-height* :double-buffer t)
  (sdl:enable-key-repeat nil nil)
  (setf (sdl:frame-rate) 10)
  (load-textures)
  (let* ((*field* (make-field))
         (keys-pressed (list))
         (state (make-state :field *field*))
         )
    (sdl:with-events ()
      (:idle
        (let ((command (command-get keys-pressed)))
          (unless (state-over state)
            (handler-bind
              ((game-over (lambda (c)
                            (format t "Game over!~%")))
               (points-earned (lambda (c)
                                (format t "Points: ~d (+~d)~%" (total c) (points c))))
               ;(condition (lambda (c)
               ;             (print c)))
               )

              (state-advance state (command-get keys-pressed)))))
        (setf keys-pressed (list))

        (sdl:clear-display (rgb 255 255 255))
        (with-slots (field piece next-piece over) state
          (draw-field *field-x* *field-y* field)
          (draw-next *next-x* *next-y* next-piece)
          (when piece
            (draw-piece piece :offset-x (1+ *field-x*) :offset-y (1+ *field-y*)))
          (when over
            (draw-game-over *field-x* *field-y*))
          )
        (sdl:update-display)
        (
         )
        )
      (:key-down-event (:key key)
       (pushnew key keys-pressed))
      (:quit-event () t)
      ) 

    )
  )


(defun main ()
  (sdl:with-init (sdl:sdl-init-video) (sdl-main)))

(main)

