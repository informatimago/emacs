
;; https://en.wikipedia.org/wiki/CIE_1931_color_space#CIE_xy_chromaticity_diagram_and_the_CIE_xyY_color_space

(in-package :cl-user)

(defun |xyz|-from-|XYZ| (x* y* z*)
  (list (/ x* (+ x* y* z*))
          (/ y* (+ x* y* z*))
          (/ z* (+ x* y* z*))))

(defun |xyY|-from-|XYZ| (x* y* z*)
  (list (/ x* (+ x* y* z*))
          (/ y* (+ x* y* z*))
          y*))

(defun |XYZ|-from-|xyY| (x y y*)
  (list (/ (* x y*) y)
        y*
        (/ (* (- 1 x y) y*) y)))

(|XYZ|-from-|xyY| 0.31271 0.32902 1.0)
;; --> (0.95042855 1.0 1.0889004)

(|XYZ|-from-|xyY|  0.31271  0.32902 1.0)
;; --> (0.95042855 1.0 1.0889004)

(apply (function |XYZ|-from-|xyY|)
       (|xyY|-from-|XYZ|  0.3 0.3 0.2))
;; --> (0.3 0.3 0.2)

(defun |xyY|-mix (a b)
  (destructuring-bind (x1 y1 L1) a
    (destructuring-bind (x2 y2 L2) b
      (list (/ (+ (* (/ x1 y1) l1)
                  (* (/ x2 y2) l2))
               (+ (/ l1 y1)
                  (/ l2 y2)))
            (/ (+ l1 l2)
               (+ (/ l1 y1)
                  (/ l2 y2)))))))

(|xyY|-mix 11)
