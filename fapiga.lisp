
(ql:quickload "lispbuilder-sdl")


(defparameter *window-width* 640)
(defparameter *window-height* 480)
(defparameter *brick-width* 16)
(defparameter *brick-height* 16)
(defparameter *field-width* 10)
(defparameter *field-height* 20)
(defparameter *kill-row* 2)
(defparameter *field* nil)
(defparameter *field-x* 0)
(defparameter *field-y* 0)
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
  ((brick-type :initarg :brick-type
               :reader piece-type-brick-type)
   (forms :initarg :forms
          :reader piece-type-forms)))

(defparameter *piece-types* (list))

(defun define-piece-type (name brick-type &rest form-lists)
  (setf *piece-types* (acons name
                             (make-instance 'piece-type :brick-type brick-type
                                            :forms (make-array (length form-lists)
                                                               :initial-contents
                                                               (mapcar #'list->form form-lists)))
                             *piece-types*)))

(defun random-piece-type ()
  (cdr (nth (random (length *piece-types*)) *piece-types*)))

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
  ((x :initform 1
      :initarg :x
      :reader piece-x)
   (y :initform 0
      :initarg :y
      :reader piece-y)
   (form-index :initform 0
               :initarg :form-index
               :reader piece-form-index)
   (type :initarg :type
         :reader piece-type))
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

(defmacro field-row (field y)
  `(aref ,field ,y))

(defmacro field-brick-at (field x y)
  `(aref (field-row ,field ,y) ,x))

(defun field-put-brick (field x y brick-type)
  (when (< y *field-top-brick*)
    (setf *field-top-brick* y))
  (setf (field-brick-at field x y) brick-type))

(defmacro field-top-brick (field)
  '*field-top-brick*)


(defun transform-field-coords (x y)
  (values (+ *field-x* (* *brick-width* x))
          (+ *field-y* (* *brick-height* y)))
  )


(defun draw-brick (x y brick-type)
  (multiple-value-bind (screen-x screen-y) (transform-field-coords x y)
    (sdl:draw-box-* screen-x screen-y *brick-width* *brick-height*
                    :color brick-type :stroke-color (rgb 50 50 50))))


(defun draw-field (x y field)
  (sdl:draw-box-* x y (* *field-width* *brick-width*) (* *field-height* *brick-height*)
                  :color (rgb 0 0 50))
  (sdl:draw-box-* x y (* *field-width* *brick-width*) (* (1+ *kill-row*) *brick-height*)
                  :color (rgb 50 0 0))
  (loop for brick-x from 0 below *field-width*
        do (loop for brick-y from 0 below *field-height*
                 for brick = (field-brick-at field brick-x brick-y)
                 if brick do (draw-brick brick-x brick-y brick)))
  )

(defun draw-piece (piece)
  (with-accessors ((piece-x piece-x) (piece-y piece-y) (brick-type piece-brick-type)) piece
    (let ((form (piece-current-form piece)))
      (loop for y from 0 below (form-height form) do
            (loop for x from 0 below (form-width form)
                  if (form-brick-at-p form x y)
                  do (draw-brick (+ piece-x x) (+ piece-y y) brick-type))))))


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
                   do (field-put-brick field x y brick-type)))))


(defparameter *key-map* '(
                          (:sdl-key-space . :drop)
                          (:sdl-key-up . :rotate-right)
                          (:sdl-key-right . :right)
                          (:sdl-key-left . :left)
                          ))


(defun command-get (keys)
  (loop for (key . command) in *key-map*
        if (find key keys) return command))


(defun command-perform (command)
  (let ((altered-piece (ecase command
                         ((:left :right) (piece-move *piece* command))
                         (:rotate-right (piece-rotate *piece* :right))
                         (:drop (loop for p = *piece* then (piece-fall p)
                                      until (landed-p *field* p)
                                      finally (return p))))))
    (unless (collision-p *field* altered-piece)
      (setf *piece* altered-piece))))



(defun field-replace-row (field y row)
  (setf (field-row field y) row))

(defun field-move-row (field from to)
  (format t "Move row: ~d -> ~d.~%" from to)
  (field-replace-row field to (field-row field from)))

(defun field-clear-row (field y)
  (format t "Clear row: ~d.~%" y)
  (field-replace-row field y (make-array *field-width* :initial-element nil)))

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
                           (incf (field-top-brick field) rows-removed))
            )
      0
      )
    )
  )


(defun make-field ()
  (let ((rows (make-array *field-height*)))
    (dotimes (y *field-height*)
      (setf (aref rows y) (make-array *field-width* :initial-element nil)))
    rows))


(define-condition game-over (condition) ())

(defun sdl-main ()
  (sdl:window *window-width* *window-height* :double-buffer t :hw t)
  (sdl:enable-key-repeat nil nil)
  (setf (sdl:frame-rate) 10)
  (sdl:clear-display sdl:*black*)

  (let ((*field* (make-field))
        (counter 0)
        (keys-pressed (list))
        )
    (sdl:with-events ()
      (:idle
        (when (not *piece*)
          (setf *piece* (spawn-piece)))

        (let ((command (command-get keys-pressed)))
          (when (and *piece* command)
            (command-perform command)
            ))
        (setf keys-pressed (list))

        (let ((landed-p (landed-p *field* *piece*)))
          (when landed-p
            (lock *field* *piece*)
            (remove-full-rows *field* (piece-top *piece*) (piece-bottom *piece*))
            (setf *piece* nil))
          (incf counter)
          (when (= counter 10)
            (setf counter 0)
            (unless landed-p
              (setf *piece* (piece-fall *piece*)))
            )
          )

        (sdl:clear-display sdl:*black*)
        (draw-field *field-x* *field-y* *field*)
        (when *piece*
          (draw-piece *piece*))
        (sdl:update-display)
        (when (<= (field-top-brick *field*) *kill-row*)
          (sdl:push-quit-event)
          ;(error 'game-over)
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

