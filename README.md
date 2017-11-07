# fac.el
Emacs Face Manipulation

## Basic face stuff:
`(fac-select FONT1 FONT2 ...)`: Return the first available font.

`(fac-def-faces GROUP '(face1 docstring1 . properties1) '(face2 docstring2 . properties2) ...)`: Create each `FACE` under `GROUP`.

`(fac-set-attributes '(face1 :attribute1 a1 attribute2 a2 ...) '(face2 ...) ...)`: Give each `FACE` its attributes.

`(fac-set-faces-attributes '(face1 face2 ...) :attribute a1 :attribute a2 ...)`: Give each attribute to the `FACE`s.

`(fac-shift-foreground FUNCTION FACE REFERENCE)`: Set `FACE`'s foreground to the result of applying `FUNCTION` to `REFERENCE`'s foreground and background.

`(fac-fade-foreground FACE REFERENCE)`: Set `FACE`'s foreground to a less intense version of `REFERENCE`'s.

`(fac-intensify-foreground FACE REFERENCES)`: Set `FACE`'s foreground to a more intense version of `REFERENCE`'s.

`(fac-def-adaptive-faces GROUP '(NAME1 DOCSTRING1 ACTIVE-ATTRIBUTES1 INACTIVE-ATTRIBUTES1 &optional SETUP-PROCEDURE1) ...)`: Create ersatz faces, which can look different depending on whether the pane is active, under GROUP. Use an ersatz face by calling `(GROUP-NAME)`. If included, `SETUP-PROCEDURE` should be a procedure of 2 arguments (faces) that sets the first relative to the second.

`(fac-normalize-box FACE)`: Return the `:box` property of `FACE` as a list.

`(fac-box->lines FACE)`: Replace `FACE`'s `:box` property with over- and under- lines.
