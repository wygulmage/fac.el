;;; fac.el --- face stuff -*- lexical-binding: t -*-
(eval-when-compile
  (require 'umr))
(mapc #'require
      [hooker primary-pane miscellaneous])

;;; Colors (should be somewhere else, like `colors.el'...)

(defun fac--blend-colors (C1 C2)
  "(R G B) -> (R G B) -> (R G B)
    Evenly blend C1 and C2, two emacs RGB triplets."
  (declare (pure t) (side-effect-free t))
  (seq-mapn (lambda (X Y) (* 0.5 (+ X Y)))
            C1 C2))

(defun fac--intensify-color (COLOR REFERENCE)
  "(R G B) -> (R G B) -> (R G B)
    Shift COLOR away from REFERENCE."
  (declare (pure t) (side-effect-free t))
  (seq-mapn (lambda (C R)
              (* 0.5 (+ C (if (> C R) 1 0))))
            COLOR REFERENCE))

;;; Basic face stuff

(defun fac-select (&rest FONTS)
  "Return the first available font in FONTS, or the default font if none are available."
  (umr-if (null FONTS) (face-attribute 'default :family)
          (member (car FONTS) (fac-family-list)) (car FONTS)
          (apply #'fac-select (cdr FONTS))))

(defun fac-def-faces (GROUP &rest FACES)
  "Create FACES (name docstring properties) in GROUP. No fancy business here; the display is always t."
  (declare (indent 1))
  (cl-loop
   for (name docstring . properties) in FACES
   do (custom-declare-face name `((t . ,properties)) docstring :group GROUP)))

(defun fac-set-face-attributes (&rest FACES)
  "From FACES of (face :attr-1 a1 :attr-2 a2 ...) lists, give each face its attributes. Create undefined faces."
  (cl-loop
   for (face . attributes) in FACES
   do
   ;; (unless (facep face) (make-face face))
   (apply #'set-face-attribute face nil attributes)))

(defun fac-shift-face-foreground (FUNCTION FACE REFERENCE)
  "Set FACE's foreground to the result of applying FUNCTION to REFERENCE's foreground and background."
  (umr-let
   color-of ((KEY)
             (color-name-to-rgb (face-attribute REFERENCE KEY nil 'default)))
   (set-face-attribute
    FACE
    nil
    :foreground (apply #'color-rgb-to-hex
                       (funcall FUNCTION
                                (color-of :foreground)
                                (color-of :background))))))

(defun fac-fade-face-foreground (FACE REFERENCE)
  "Make FACE's foreground a less intense version of REFERENCE's.
REFERENCE is used to avoid fading FACE into oblivion with repreated applications."
  (fac-shift-face-foreground #'fac--blend-colors FACE REFERENCE))

(defun fac-intensify-face-foreground (FACE REFERENCE)
  (fac-shift-face-foreground #'fac--intensify-color FACE REFERENCE))

;;; Adaptive faces

(defvar fac-adaptive-faces-setup ()
    "a list of adaptive face setup functions")

  (defun fac-def-adaptive-faces (GROUP &rest ADAPTIVE-FACES)
    "Create ersatz faces in customization group GROUP.
Each ADAPTIVE-FACE take the form (NAME DOCSTRING ACTIVE-ATTRIBUTES INACTIVE-ATTRIBUTES &optional ACTIVE-SETUP INACTIVE-SETUP).

Each face should be used by calling (GROUP-NAME-face).

The active or inactive version can be modified by setting the attributes of GROUP-NAME-active-face or GROUP-NAME-inactive-face.

FACE-SETUP should a procedure of 2 arguments (faces) that sets attributes of the first argument relative to the second; the :inherit of the active faces will be used for the second."
    (declare (indent 1))
    (umr-let
     def-adaptive-face
     ((name doc active inactive &optional face-setup)
      (umr-let
       face-symbol ((s) (misc--symb GROUP "-" name s "-face"))
       active-name (face-symbol "-active")
       inactive-name (face-symbol "-inactive")
       getter-name (face-symbol "")
       (progn
         (fac-def-faces GROUP
           `(,active-name ,doc ,@active)
           `(,inactive-name ,doc ,@inactive))
         (fset getter-name
               `(lambda ()
                  (if (primary-pane-active?)
                      ',active-name
                    ',inactive-name)))
         (when face-setup
           (add-hook 'adaptive-faces-setup
                     `(lambda ()
                        (,face-setup ',active-name
                                     ',(face-attribute active-name :inherit))
                        (,face-setup ',inactive-name
                                     ',(face-attribute inactive-name :inherit))))))))
     (seq-doseq (f ADAPTIVE-FACES)
       (apply #'def-adaptive-face f))))

(defun fac-reset-adaptive-faces ()
  (run-hooks 'fac-adaptive-faces-setup))

(hooker-make-hook :after load-theme
  fac-reset-adaptive-faces)

(provide 'fac)