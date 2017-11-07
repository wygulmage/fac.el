;;; fac.el --- face stuff -*- lexical-binding: t -*-
(eval-when-compile
  (mapc #'require [cl-macs pcase let+]))
(mapc #'require
      [color hook-up primary-pane miscellaneous seq])

(let+ (
;;; Private Functions:

;;;; Colors (should be somewhere else, like `color.el'...)
       blend-colors
        ((C1 C2)
         "(R G B) -> (R G B) -> (R G B)
Evenly blend C1 and C2, two emacs RGB triplets."
         (declare (pure t) (side-effect-free t))
         (seq-mapn (lambda (X Y) (* 0.5 (+ X Y)))
                   C1 C2))

       intensify-color
        ((COLOR REFERENCE)
         "(R G B) -> (R G B) -> (R G B)
Shift COLOR away from REFERENCE."
         (declare (pure t) (side-effect-free t))
         (seq-mapn (lambda (C R)
                     (* 0.5 (+ C (if (> C R) 1 0))))
                   COLOR REFERENCE)))

;;; Public Functions:

;;;; Basic face stuff

  (defun fac-select (&rest FONTS)
    "Return the first available font in FONTS, or the default font if none are available."
    (cond ((null FONTS) (face-attribute 'default :family))
          ((member (car FONTS) (fac-family-list)) (car FONTS))
          (t (apply #'fac-select (cdr FONTS)))))

  (defun fac-def-faces (GROUP &rest FACES)
    "Create FACES (name docstring . properties) in GROUP. No fancy business here; the display is always t."
    (declare (indent 1))
    (cl-loop
     for (name docstring . properties) in FACES
     do (custom-declare-face name `((t . ,properties)) docstring :group GROUP)))

  (defun fac-set-attributes (&rest FACES)
    "From FACES of (face :attr-1 a1 :attr-2 a2 ...) lists, give each face its attributes. Create undefined faces."
    (cl-loop
     for (face . attributes) in FACES
     do
     ;; (unless (facep face) (make-face face))
     (apply #'set-face-attribute face nil attributes)))

  (defun fac-set-faces-attributes (FACES &rest ATTRIBUTES)
    "Give all FACES ATTRIBUTES."
    (seq-doseq (face FACES)
      (apply #'set-face-attribute face nil ATTRIBUTES)))

  (defun fac-shift-foreground (FUNCTION FACE REFERENCE)
    "Set FACE's foreground to the result of applying FUNCTION to REFERENCE's foreground and background. If neither REFERENCE nor the default face has defined colors, do nothing."
    (let+ (color-of
           ((KEY)
            (color-name-to-rgb
             (face-attribute REFERENCE KEY nil 'default)))
           foreground (color-of :foreground)
           background (color-of :background))
      (when (and background foreground)
        (set-face-attribute
         FACE
         nil
         :foreground (apply #'color-rgb-to-hex
                            (funcall FUNCTION
                                     foreground
                                     background))))))

  (defun fac-fade-foreground (FACE REFERENCE)
    "Make FACE's foreground a less intense version of REFERENCE's.
REFERENCE is used to avoid fading FACE into oblivion with repreated applications."
    (fac-shift-foreground #'blend-colors FACE REFERENCE))

  (defun fac-intensify-foreground (FACE REFERENCE)
    (fac-shift-foreground #'intensify-color FACE REFERENCE))

;;; Adaptive faces

  (defvar fac--adaptive-faces-table (make-hash-table)
    "a table of adaptive face setup functions")
  (defun fac-reset-adaptive-faces ()
    "Rerun each adaptive face setup function."
    (maphash (lambda (_key procedure) (funcall procedure))
             fac--adaptive-faces-table))

  (defun fac-def-adaptive-faces (GROUP &rest ADAPTIVE-FACES)
    "Create ersatz faces in customization group GROUP.
Each ADAPTIVE-FACE takes the form (NAME DOCSTRING ACTIVE-ATTRIBUTES INACTIVE-ATTRIBUTES &optional FACE-SETUP).

Use a face by calling (GROUP-NAME).

Modify the active or inactive face by setting the attributes of GROUP-NAME-active or GROUP-NAME-inactive.

FACE-SETUP should be a procedure of 2 arguments (faces) that sets attributes of the first relative to the second; the :inherit of the first face will be used as the second argument."
    (declare (indent 1))
    (let+
     (def-adaptive-face
      ((name doc active inactive &optional face-setup)
       "Create one adaptive face."
       (let+
           (getter-name (misc--symb GROUP "-" name)
            active-name (misc--symb getter-name "-active")
            inactive-name (misc--symb getter-name "-inactive"))
        (fac-def-faces GROUP
          `(,active-name ,doc ,@active)
          `(,inactive-name ,doc ,@inactive))
        (fset getter-name
              `(lambda ()
                 (if (primary-pane-active?)
                     ',active-name
                   ',inactive-name)))
        (when face-setup
          (puthash getter-name
           `(lambda ()
              (,face-setup ',active-name
                           ',(face-attribute active-name :inherit))
              (,face-setup ',inactive-name
                           ',(face-attribute inactive-name :inherit)))
           fac--adaptive-faces-table)))))
     ;; Create the faces.
     (seq-doseq (f ADAPTIVE-FACES)
       (apply #'def-adaptive-face f)))
    (fac-reset-adaptive-faces))

  (unless (boundp 'after-load-theme-hook)
    (hook-up-def-hook :after #'load-theme))

  (hook-up [after-load-theme-hook]
           [fac-reset-adaptive-faces])

  (defun fac-normalize-box (FACE)
    "The :box property of FACE as a list."
    (pcase (face-attribute FACE :box)
      ('nil nil)
      ('t `(:color ,(face-attribute FACE :foreground nil 'default)
                   :line-width 1))
      ((and (pred consp) x) x)
      (x `(:color ,x :line-width 1))))

  (defun fac-box->lines (FACE)
    "Turn a box into under- and over-lines."
   (let+ (color (plist-get (fac-normalize-box FACE) :color))
     (when color (set-face-attribute
                  FACE nil :box nil :underline color :overline color))))

  (provide 'fac))
