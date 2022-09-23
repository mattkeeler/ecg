;; ecg.lisp --- Emacs Configuration Generator

;; URL: https://git.sr.ht/~pkal/ecg
;; Author: Philip Kaludercic <philipk@posteo.net>
;; Version: 0.1

;;; Commentary:

;; This script implements a simple web server, that provides a form to
;; generate a custom Emacs configuration.

;;; License:

;; Copyright (C) 2022  Philip Kaludercic

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(declaim (optimize (speed 3) (space 3) (safety 0) (debug 1)))

(require "asdf")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :cl-who)
  (asdf:load-system :hunchentoot)
  (asdf:load-system :alexandria)
  (load "elisp-indent.lisp"))

(defpackage :emacs-configuration-generator
  (:use :common-lisp :alexandria :cl-who)
  (:nicknames :ecg)
  (:export #:start))
(in-package :ecg)



(declaim (ftype (function () string) next-name))

(defvar *name-counter* 0)
(defun next-name ()
  (prog1 (format nil "id-~d" *name-counter*)
    (incf *name-counter*)))

;; Based on a post by Peter Norvig in response to
;; https://groups.google.com/g/comp.lang.lisp/c/-W1FeuHq0DI
(defun parse-float (str)
  "Return a fraction read from string containing a float."
  (multiple-value-bind (integer i) (parse-integer str :junk-allowed t)
    (multiple-value-bind (frac j) (parse-integer str :start (+ i 1))
      (+ integer (/ frac (expt 10 (- j i 1)))))))

(defparameter *default-version* 27
  "Without any further information, what version do we assume?")

(defun emacs-version ()
  (let ((query (hunchentoot:parameter "emacs-version")))
    (if query *default-version* (parse-float query))))

(defstruct (section (:constructor make-section (title))) title)
(defstruct (subsection (:constructor make-subsection (title))) title)
(defstruct query question (name (next-name)) type)
(defstruct single-choice question (name (next-name)) alternatives default radiop)
(defstruct details summary text)
(defstruct conditional question (name (next-name)) code comment inverted)
(defstruct choice question (name (next-name)) options comment)
(defstruct choice-option
  text value code image version)
(defstruct (elpa-package
            (:constructor make-elpa-package
                (pkg-name generic-name archive
                 &optional prelude &rest options
                 &aux (url (ecase archive
                             ((gnu-elpa)
                              (format nil "https://elpa.gnu.org/packages/~a.html" pkg-name))
                             ((nongnu-elpa)
                              (format nil "https://elpa.nongnu.org/nongnu/~a.html" pkg-name)))))))
  pkg-name generic-name (name (next-name)) url prelude options)
(defstruct (builtin-package
            (:constructor make-builtin-package
                (pkg-name generic-name
                 &optional prelude &rest options)))
  pkg-name generic-name (name (next-name)) prelude options)

(defgeneric generate-form (entry)
  (:method ((sec section))
    (with-html-output (*standard-output*)
      (:h2 (str (section-title sec)))))
  (:method ((sec subsection))
    (with-html-output (*standard-output*)
      (:h3 (str (subsection-title sec)))))
  (:method ((text string))
    (with-html-output (*standard-output*)
      (:p (str text))))
  (:method ((query query))
    (with-slots (name question type) query
      (with-html-output (*standard-output*)
        (:div
         (:label :for name (esc question) ": ")
         (:input :type type :name name)))))
  (:method ((query single-choice))
    (with-slots (name question default alternatives radiop) query
      (assert (not (null alternatives)))
      (if radiop
          (with-html-output (*standard-output*)
            (:div
             (:p (esc question))
             (dolist (alt alternatives)
               (htm
                (:div
                 (:input :value (if (consp alt) (cdr alt) alt)
                         :checked (equal (if (consp alt) (cdr alt) alt) default)
                         :type "radio" :name name)
                 " "
                 (:label (str (if (consp alt) (car alt) alt))))))))
          (with-html-output (*standard-output*)
            (:div
             (:label :for name (esc question) ": ")
             (:select
              :name name
              (dolist (alt alternatives)
                (htm
                 (:option
                  :selected (equal alt default)
                  :value alt
                  (esc alt))))))))))
  (:method ((details details))
    (with-slots (summary text) details
      (with-html-output (*standard-output*)
        (:details
         (:summary (esc summary))
         (str text)))))
  (:method ((conditional conditional))
    (with-slots (name question inverted) conditional
      (with-html-output (*standard-output*)
        (:input :type "checkbox" :name name
                :checked inverted)
        " " (:label :for name (esc question))
        :br)))
  (:method ((choice choice))
    (with-slots (question name options) choice
      (when question
        (with-html-output (*standard-output*) (:p (esc question))))
      (with-html-output (*standard-output*)
        (:ul
         :class "choice"
         (assert (not (null options)))
         (dolist (option options)
           (with-slots (text value image version) option
             (htm
              (:li
               (:input :type "radio"
                       :name name
                       :value value)
               " "
               (:label :for name :title (format nil "Requires Emacs ~A" version)
                       (esc text))
               (when image
                 (with-html-output (*standard-output*)
                   (:img :src image :loading "lazy")))))))))))
  (:method ((fn function))
    (declare (ignore fn)))
  (:method ((package elpa-package))
    (with-slots (pkg-name generic-name name url prelude options) package
      (with-html-output (*standard-output*)
        ((:div :class "package")
         (:span :class "package-name"
                "Package "
                (:q (:a :href url  (:tt (esc pkg-name)))))
         (:h4 (esc generic-name))
         (dolist (option (ensure-list prelude))
           (generate-form option))
         (:input :type "checkbox" :name name :class "cond") " "
         (:label :for name "Add this package?")

         (:blockquote :class "dependent"
                      (dolist (option options)
                        (generate-form option)))))))
  (:method ((package builtin-package))
    (with-slots (pkg-name generic-name name prelude options) package
      (with-html-output (*standard-output*)
        (:div :class "package"
              (:span :class "package-name" "Built-In Package "
                     (:q (:tt (esc pkg-name))))
              (:h4 (esc generic-name))
              (dolist (option (ensure-list prelude))
                (generate-form option))
              (:input :type "checkbox" :name name :class "cond") " "
              (:label :for name "Add this package?")
              (:blockquote :class "dependent"
                           (dolist (option options)
                             (generate-form option))))))))

(defgeneric generate-conf (entry)
  (:method ((conditional conditional))
    (when (hunchentoot:parameter (conditional-name conditional))
      (format t "~%~:[~;~:*~%;; ~A~]~&~A"
              (conditional-comment conditional)
              (conditional-code conditional))))
  (:method ((entry t))
    (declare (ignore entry)))
  (:method ((fn function))
    (funcall fn))
  (:method ((choice choice))
    (with-slots (name options comment) choice
      (let ((option (find (hunchentoot:parameter name)
                          options
                          :key #'choice-option-value
                          :test #'string=)))
        (when (and option (choice-option-code option))
          (with-slots (code version) option
            (format t "~2%~:[~;~:*;; ~A~]~&~:[;; Requires Emacs ~D~%;; ~;~*~]~A"
                    comment
                    (<= version (emacs-version))
                    version
                    code))))))
  (:method ((package elpa-package))
    (with-slots (pkg-name generic-name name prelude options) package
      (dolist (option (ensure-list prelude))
        (generate-conf option))
      (when (hunchentoot:parameter name)
        (format t "~2%;;; ~A~%(unless (package-installed-p '~A)
(package-install '~:*~A))" generic-name pkg-name)
        (dolist (option options)
          (generate-conf option)))))
  (:method ((package builtin-package))
    (with-slots (pkg-name generic-name name prelude options) package
      (dolist (option (ensure-list prelude))
        (generate-conf option))
      (when (hunchentoot:parameter name)
        (format t "~2%;;; ~A" generic-name)
        (dolist (option options)
          (generate-conf option))))))


;; List of options

(defmacro par (&rest body)
  (let ((arg (gensym)))
    `(with-output-to-string (*standard-output*)
       (with-html-output (*standard-output*) (:p ,@body)))))

(defmacro ul (&rest items)
  (let ((arg (gensym)))
    `(with-output-to-string (*standard-output*)
       (with-html-output (*standard-output*)
         (:ul ,@(mapcar (curry #'list :li) items))))))

(defvar *options*
  (list
   (make-section "General")
   "Some options might depend on the version of Emacs you have installed.
If you know what version you will be using, set it here.  Otherwise it
will be assumed that you have Emacs 27.1  installed (currently most widely available
version)."
   (make-single-choice
    :question "What Emacs version do you have installed"
    :name "emacs-version"
    :alternatives '("29.0"
                    "28.1"
                    "27.2" "27.1"
                    "26.3" "26.2" "26.1"
                    "26.3" "26.2" "26.1"
                    "25.3" "25.2" "25.1"
                    "24.5" "24.4" "24.3")
    :default "27.1")
   (lambda ()
     (format t ";;; Personal configuration -*- lexical-binding: t -*-")
     (format t "~2%;; Save the contents of this file under ~A"
             (if (< (emacs-version) 27)
                 "~/.config/emacs/init.el"
                 "~/.emacs.d/init.el"))
     (format t "~%;; Do not forget to use Emacs' built-in help system:
;; Use C-h C-h to get an overview of all help commands.  All you
;; need to know about Emacs (what commands exist, what functions do,
;; what variables specify), the help system can provide."))

   (lambda ()
     (when (< (emacs-version) 28)
       (format t "~2%;; Add the NonGNU ELPA package archive")
       (format t "~%(require 'package)")
       (format t "~%(add-to-list 'package-archives")
       (format t "  '(\"nongnu\" . \"https://elpa.nongnu.org/nongnu/\"))"))
     (when (< (emacs-version) 26.3)
       (format t "~2%;; Fix TLS issues when contacting ELPA")
       (format t "~%;; See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341")
       (format t "~%(setq gnutls-algorithm-priority \"NORMAL:-VERS-TLS1.3\")")))

   (make-subsection "Appearance")
   "You might use a custom theme for aesthetic reasons or because you have
a visual impairment.  Here follows a list of themes that are bundled
with Emacs, that you might be interested in."
   (make-choice
    :comment "Load a custom theme"
    :options
    (cons
     (make-choice-option
      :text "Default"
      :version 24
      :image "./static/images/default.svg")
     ;; Use this Elisp script to generate theme images:
     ;;
     ;; (dolist (theme (custom-available-themes))
     ;;   (load-theme theme t)
     ;;   (write-region (x-export-frames nil 'svg) nil
     ;;                              (format "%s.svg" theme))
     ;;   (disable-theme theme))
     (loop :for (theme . version) :in
           '(("adwaita" . 24)
             ("deeper-blue" . 24)
             ("dichromacy" . 24)
             ("leuven" . 24)
             ("leuven-dark" . 29)
             ("light-blue" . 24)
             ("manoj-dark" . 24)
             ("misterioso" . 24)
             ("modus-operandi" . 28)
             ("modus-vivendi" . 28)
             ("tango" . 24)
             ("tango-dark" . 24)
             ("tsdh-dark" . 24)
             ("tsdh-light" . 24)
             ("wheatgrass" . 24)
             ("whiteboard" . 24)
             ("wombat" . 24))
           :collect
           (make-choice-option
            :text (string-capitalize (substitute #\Space #\- theme))
            :value theme
            :code (format nil "(load-theme '~a t)" theme)
            :version version
            :image (format nil "./static/images/~a.svg" theme)))))

   ;; FIXME: There should be a way to extend this query to make use of
   ;; `document.fonts' [0] if Javascript is available.
   ;;
   ;; [0] https://developer.mozilla.org/en-US/docs/Web/API/Document/fonts
   (make-query :question "What Font do you want to use by default?"
               :name "default-font-face"
               :type "text")
   (lambda ()
     (let ((font (hunchentoot:parameter "default-font-face")))
       (when (string/= font "")
         (format t "~2%;; Set default font face
(set-face-attribute 'default nil :font \"~A\")"
                 font))))

   "By default Emacs has a few GUI elements enabled.  Some prefer to disable these,
as they do everything using the keyboard.  Beginners should think twice about doing
this, as the GUI (especially the menu bar) provide useful pointers."
   (make-conditional
    :question "Disable menu bar?"
    :comment "Disable the menu bar"
    :code "(menu-bar-mode -1)")
   (make-conditional
    :question "Disable tool bar?"
    :comment "Disable the tool bar"
    :code "(tool-bar-mode -1)")
   (make-conditional
    :question "Disable scroll bars?"
    :comment "Disable the scroll bars"
    :code "(scroll-bar-mode -1)")

   "By default Emacs comes with a splash screen, including a list of
useful pointers.  If you are sure you don't need it anymore, it can be
disabled."
   (make-conditional
    :question "Disable splash screen?"
    :comment "Disable splash screen"
    :code "(setq inhibit-startup-screen t)")

   (make-subsection "User Interface")

   (make-elpa-package
    "vertico" "Completion framework" 'gnu-elpa
    (list
     (par "Emacs default completion behaves similar to "
          (:a :href "https://www.gnu.org/s/bash/" "Bash")
          ", in that it first attempts to expand a string
up until an unambiguous point, then pops up a list of possible
completions.  A popular alternative to this " (:q "expanding")
          "approach is " (:q "interactive narrowing") ", that is to say
the list of candidates (files, buffers, etc.) are immediately
presented and the user restricts these until they have found
what they are looking for.")
     (par "If interested, you can add " (:q "Vertico") " a
popular package that implements this kind of interaction."))
    (make-conditional
     :question "Enabled by default"
     :comment "Enable completion by narrowing"
     :code "(vertico-mode t)"
     :inverted t)
    (make-conditional
     :question "Display completions horizontally (which saves space),
instead of vertically (which might be easier to read)?"
     :comment "Enable horizontal completion"
     :code "(vertico-flat-mode t)")
    (make-conditional
     :question "Improve directory navigation?"
     :comment "Improve directory navigation"
     :code "(with-eval-after-load 'vertico
(define-key vertico-map (kbd \"RET\") #'vertico-directory-enter)
(define-key vertico-map (kbd \"DEL\") #'vertico-directory-delete-word)
(define-key vertico-map (kbd \"M-d\") #'vertico-directory-delete-char))")
    (make-elpa-package
     "consult" "Extended completion utilities" 'gnu-elpa
     (par "If you decide to use Vertico, " (:q "Consult") " might also
be of interest.  It defines a number of convenient commands that
make use of narrowing completion.")
     (make-conditional
      :question "Use for switching buffers"
      :code "(global-set-key [rebind switch-to-buffer] #'consult-buffer)")
     (make-conditional
      :question "Add command to search all lines in a buffer."
      :code "(global-set-key (kbd \"C-c j\") #'consult-line)")
     (make-conditional
      :question "Add command to jump to definitions."
      :code "(global-set-key (kbd \"C-c i\") #'consult-imenu)")))

   (make-conditional
    :question "Ignore case when completing?"
    :code "(setq read-buffer-completion-ignore-case t
read-file-name-completion-ignore-case t
completion-ignore-case t)")

   "If you prefer to see the line numbers at a quick glance,
you might be interested want to enable line numbering.  It is
disabled by default, because you don't really need it most of
the time.  Also note that you might not want to have it enabled
everywhere, as often the content of a text buffer isn't just a
text file you might address line by line, but documentation or
interactive applications such as a shell/REPL.  It is therefore
perhaps better to only enable line numbering when interacting
with program code."

   (make-single-choice
    :question "Enable inline line numbering?"
    :name "line-numbering"
    :alternatives '(("No, keep it disabled" . "no")
                    ("Yes, but only in programming modes" . "prog")
                    ("Yes, enable it everywhere" . "yes"))
    :default "no"
    :radiop t)
   (lambda ()
     (let ((dln (hunchentoot:parameter "line-numbering")))
       (cond
         ((string= dln "yes")
          (format t "~2%;; Enable line numbering by default~%")
          (if (< (emacs-version) 26)
              (format t "(global-linum-mode t)")
              (format t "(global-display-line-numbers-mode t)")))
         ((string= dln "prog")
          (format t "~2%;; Enable line numbering in `prog-mode'")
          (format t "~%(add-hook 'prog-mode-hook #'")
          (if (< (emacs-version) 26)
              (format t "linum-mode")
              (format t "display-line-numbers-mode"))
          (format t ")")))))

   "Do you want Emacs to automatically add the closing parentheses,
when you insert the opening ones?  This would also apply to brackets
of all sorts, quotes and in some cases additional constructs depending
on the programming language.  Note that if you are used to typing out
the closing parenthesis, this shouldn't be an issue, as Emacs will just
move over it if it were to be inserted."

   (make-conditional
    :question "Automatically pair parentheses?"
    :comment "Automatically pair parentheses"
    :code "(electric-pair-mode t)")

   (make-section "Programming")
   "Most people use Emacs for programming.  This section has a few popular
programming languages and tools you might be interested in."

   (make-subsection "Programming-language agnostic tools")

   (make-elpa-package
    "eglot" "LSP Support" 'gnu-elpa
    (list
     (par "The " (:q (:a :href "https://microsoft.github.io/language-server-protocol/"
                         "Language Server Protocol"))
          " has become a popular method to provide language introspection
(error checking, completion, ...) independently of an editor.  To make
use of this in Emacs, a package has to be installed.")
     (par "Note that this still requires an LSP " (:em "Server") " to be provided, for whatever language you intend to use."))
    (make-conditional
     :question "Enabled by default when programming?"
     :comment "Enable LSP support by default in programming buffers"
     :code "(add-hook 'prog-mode-hook #'eglot-ensure)"))

   (make-builtin-package
    "flymake" "Inline static analysis"
    (list
     "To indicates static analysis messages on the fly, use this."
     "Note that this package is built-in but can be updated to add new
features.")
    (make-conditional
     :question "Enabled by default when programming?"
     :comment "Enabled inline static analysis"
     :code "(add-hook 'prog-mode-hook #'flymake-mode)"
     :inverted t)
    (make-conditional
     :question "Display messages without prompting?"
     :comment "Display messages when idle, without prompting"
     :code "(setq help-at-pt-display-when-idle t)")
    (make-conditional
     :question "Bind commands to navigate messages?"
     :comment "Message navigation bindings"
     :code "(with-eval-after-load 'flymake
(define-key flymake-mode-map (kbd \"C-c n\") #'flymake-goto-next-error)
(define-key flymake-mode-map (kbd \"C-c p\") #'flymake-goto-prev-error))"))

   (make-elpa-package
    "company" "Pop-up auto-completion" 'gnu-elpa
    (list
     "Enable this if you like unprompted completion to be displayed
in a little popup dialogue, while typing."
     "Do not confuse this with a completion framework.  They are used
for querying the user for information, this completes text in a buffer.")
    (make-conditional
     :question "Enabled by default when programming?"
     :comment "Enable Company by default in programming buffers"
     :code "(add-hook 'prog-mode-hook #'company-mode)"
     :inverted t)
    ;; TODO: Add more configuration options
    )

   (make-subsection "Version control")

   (make-elpa-package
    "magit" "Git client" 'nongnu-elpa
    (list
     (par "The well known Git client "
          (:q (:a :href "https://magit.vc/""Magit"))
          " is one of the
most popular packages, and is said to make using "
          (:a :href "https://git-scm.com/" "Git") " easier."))
    (make-conditional
     :question "Bind to a convenient key?"
     :comment "Bind the `magit-status' command to a convenient key."
     :code "(global-set-key (kbd \"C-c g\") #'magit-status)"
     :inverted t)
    (make-conditional
     :question "Show word-granularity differences within diff hunks?"
     :comment "Show word-granularity differences within diff hunks"
     :code "(setq magit-diff-refine-hunk t)"))

   (make-elpa-package
    "diff-hl" "Indication of local VCS changes" 'gnu-elpa
    (par "If you wish to see at a quick glance what section of a file
have been added, modified or removed, you might be interested in
highlighting these changes next to the content of the buffer.")
    (make-conditional
     :question "Enabled by default when programming?"
     :comment "Enable `diff-hl' support by default in programming buffers"
     :code "(add-hook 'prog-mode-hook #'diff-hl-mode)"
     :inverted t)
    (make-conditional
     :question "Update the highlighting without saving?"
     :comment "Update the highlighting without saving"
     :code "(diff-hl-flydiff-mode t)"))

   (make-subsection "Programming languages and Programming-adjacent languages")
   (par "While Emacs has support for a number of languages out of the box
(C, C++, Python, Perl, Lisp, Fortran, XML, HTML, Pascal, Ruby, TCL,
TeX, ...), some can make use of some additional configuring, while
others need external packages.")

   (make-elpa-package "ada-mode" "Ada Support" 'gnu-elpa)
   (make-elpa-package "clojure-mode" "Clojure Support" 'nongnu-elpa)
   (make-elpa-package "csharp-mode" "C# Support" 'gnu-elpa)
   (make-elpa-package "d-mode" "D Support" 'nongnu-elpa)
   (make-elpa-package "elixir-mode" "Elixir Support" 'nongnu-elpa)
   (make-elpa-package "go-mode" "Go Support" 'nongnu-elpa)
   (make-elpa-package "haskell-mode" "Haskell Support" 'nongnu-elpa)
   (make-elpa-package "j-mode" "J Support" 'nongnu-elpa)
   (make-elpa-package "json-mode" "JSON Support" 'gnu-elpa)
   (make-elpa-package "julia-mode" "Julia Support" 'nongnu-elpa)
   (make-elpa-package "kotlin-mode" "Kotlin Support" 'nongnu-elpa)
   (make-elpa-package "lua-mode" "Lua Support" 'nongnu-elpa)
   (make-elpa-package "nasm-mode" "NASM Support" 'nongnu-elpa)
   (make-elpa-package "php-mode" "PHP Support" 'nongnu-elpa)
   (make-elpa-package "raku-mode" "Raku Support" 'nongnu-elpa)
   (make-elpa-package "rust-mode" "Rust Support" 'nongnu-elpa)
   (make-elpa-package "scala-mode" "Scala Support" 'nongnu-elpa)
   (make-elpa-package "sml-mode" "Standard ML Support" 'gnu-elpa)
   (make-elpa-package "swift-mode" "Swift Support" 'nongnu-elpa)
   (make-elpa-package "typescript-mode" "Typescript Support" 'nongnu-elpa)
   (make-elpa-package "yaml-mode" "YAML Support" 'nongnu-elpa)

   (make-section "Writing")

   (make-subsection "LaTeX")
   (make-elpa-package
    "auctex" "LaTeX support" 'gnu-elpa
    (par "Via " (:a :href "https://orgmode.org/" "AucTeX")
         ", Emacs has good support for working with LaTeX,
including help when inserting macros, quick math-mode input, automated
building and viewing of documents and inline preview.")
    (lambda ()
      (declare (ignore))
      (format t "~%(setq TeX-auto-save t)")
      (format t "~%(setq TeX-parse-self t)")
      (format t "~%(setq-default TeX-master nil)"))
    (make-conditional
     :question "Enabled LaTeX math support?"
     :comment "Enable LaTeX math support"
     :code "(add-hook 'LaTeX-mode-map #'LaTeX-math-mode)")
    (make-conditional
     :question "Enabled reference managment?"
     :comment "Enable reference mangment"
     :code "(add-hook 'LaTeX-mode-map #'reftex-mode)"))

   (make-subsection "Markdown")
   (make-elpa-package
    "markdown-mode" "Markdown support" 'nongnu-elpa
    (par "The " (:a :href "https://en.wikipedia.org/wiki/Markdown" "Markdown")
         " markup language is commonly used for documentation.  If you use it,
adding this package might be convenient."))

   (make-subsection "Org Mode")
   (make-builtin-package
    "org" "Outline-based notes management and organizer"
    (list
     (par "The well known markup format for Emacs, "
          (:a :href "https://orgmode.org/" "Org Mode")
          " can be used for
anything from managing appointments, writing manuals, literate programs
or executing code like a programming notebook.")
     (par "While Org Mode is bundled in by default, you can decide
to fetch a new newer version that what is provided by default by
your Emacs installation."))
    (make-conditional
     :question "Add binding to store links?"
     :code "(global-set-key (kbd \"C-c l\") #'org-store-link)")
    (make-conditional
     :question "Add binding to view your agenda?"
     :code "(global-set-key (kbd \"C-c a\") #'org-agenda)")
    (make-elpa-package
     "org-contrib" "Additional Org-mode related functionality" 'nongnu-elpa
     (par "If interested in Org Mode, you might also like these
extensions that are not distributed with Org by default.")))

   (make-section "Utilities")

   (make-subsection "Applications")

   (make-elpa-package
    "crdt" "Collaborative Editing" 'gnu-elpa
    (par "If you have friends using Emacs, this package might be of
use if you need to work on the same files at the same time, so that
everyone can see what everyone else is doing.  The package uses "
         (:a :href "https://en.wikipedia.org/wiki/Conflict-free_replicated_data_type"
             "Conflict-free replicated data types")
         " to synchronise the buffer state, without the need for a
central server or service."))

   (make-builtin-package
    "rcirc" "IRC Client"
    (par (:a :href "https://en.wikipedia.org/wiki/Internet_Relay_Chat" "IRC")
         " remains popular, especially among Emacs users. If
you want to hang out in a chat room or need to contact a project
you are having issues with, having a basic IRC configuration can
be of use.")
    (make-query :question "What IRC nick do you want to use?"
                :name "rcirc-nick"
                :type "text")
    (lambda ()
      (when (< (emacs-version) 28)
        (format t "~2%;; Connect to Librea
(setq rcirc-server-alist
'((\"irc.libera.chat\" :channels (\"#emacs\")
:port 6697 :encryption tls)))"))
      (let ((nick (hunchentoot:parameter "rcirc-nick")))
        (when (string/= nick "")
          (format t "~2%;; Set your IRC nick~%(setq rcirc-default-nick ~S)" nick))))
    (make-conditional
     :question "Indicate channel activity in the mode line?"
     :code "(add-hook 'rcirc-mode-hook #'rcirc-track-minor-mode)"
     :inverted t)
    (make-conditional
     :question "Hide less important messages like users joining or leaving?"
     :code "(add-hook 'rcirc-mode-hook #'rcirc-omit-mode)"
     :inverted t))

   (make-elpa-package
    "editorconfig" "EditorConfig support" 'nongnu-elpa
    (par "A popular method to ensure consistent formatting and
indentation between editors is "
         (:a :href "https://editorconfig.org/" "EditorConfig")
         ".  If you need to use this, add this package.")
    (make-conditional
     :question "Enabled by default?"
     :comment "Enable EditorConfig "
     :code "(editorconfig-mode t)"
     :inverted t))

   (make-subsection "Text manipulation and navigation")
   "Since everything is just a text buffer in Emacs, general purpose
utilities and commands have wide applicability.  This section suggests
a few popular examples."

   (make-elpa-package
    "avy" "Jump to arbitrary positions" 'gnu-elpa
    (par "This package provides functionality to jump to to any position
(and manipulate it) using very few keystrokes.  For this, you look at
the position where you want point to be, invoke Avy, and then enter
the sequence of characters displayed at that position.")
    (make-conditional
     :question "Add a binding to jump to a word?"
     :code "(global-set-key (kbd \"C-c z\") #'avy-goto-word-1)"
     :inverted t)
    (make-conditional
     :question "Jump to any open window?"
     :comment "Jump to any open window or frame"
     :code "(setq avy-all-windows 'all-frames)"))

   (make-subsection "Editor emulation")
   "Emacs is programmable and the default behaviour can be modified,
improved or disabled.  As such it follows that it is possible to emulate
the behaviour and user experience of other editors."

   (par (:em "An example:") " CUA (" (:q (:a :href "https://en.wikipedia.org/wiki/IBM_Common_User_Access"
                                             "Common User Access"))
        ") are the conventions popularised by IBM in the 1980's, that
are used by most other programmes.  Think of copying using " (:kbd "Control+C") ", pasting using " (:kbd "Control+V") ".")
   (par "Emacs not only predates these conventions, but stands in
conflict with the bindings by default.  An attempt at resolving this
can be done using" (:tt "cua-mode")". If you find yourself struggling
with Emacs bindings, enabling this might help overcome your initial difficulties.")
   (make-conditional
    :question "Enable CUA key bindings?"
    :comment "Enable CUA key bindings"
    :code "(cua-mode t)")

   (make-elpa-package
    "evil" "Vim Emulation" 'nongnu-elpa
    (par "The child of the beast, "
         (:a :href "https://www.vim.org/" "Vim")
         ", another popular editor is often
mistakenly used instead of Emacs.  Some have sadly gotten used to the
sinful ways, and prefer the " (:q "modal")
         " approach to Emacs default bindings.  If you too are affected by
this curse, this package might help.")
    (make-conditional
     :question "Enabled by default?"
     :comment "Enable Vim emulation"
     :code "(evil-mode t)"
     :inverted t))

   (make-elpa-package
    "brief" "Brief Emulation" 'gnu-elpa
    (par (:a :href "https://en.wikipedia.org/wiki/Brief_(text_editor)"
             "An editor")
         " more popular during the time of MS DOS, can also be emulated
by Emacs."))

   (make-section "Miscellaneous")
   "Finally a few useful options, tricks and hacks that are suggested."

   (lambda ()
     (format t "~2%;; Miscellaneous options"))
   (make-conditional
    :question "Guess the major mode from the file name?"
    :code "(setq-default major-mode
(lambda () ; guess major mode from file name
(unless buffer-file-name
(let ((buffer-file-name (buffer-name)))
(set-auto-mode)))))"
    :inverted t)
   (make-conditional
    :question "Require a confirmation before closing Emacs?"
    :code "(setq confirm-kill-emacs #'yes-or-no-p)"
    :inverted t)
   (make-conditional
    :question "Resize frame and window pixel-wise (instead of character-wise)?"
    :code "(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)"

    :inverted t)
   (make-conditional
    :question "Remember the last position in a file?"
    :code "(save-place-mode t)"
    :inverted t)
   (make-conditional
    :question "Remember previous user input (file selection, etc.)?"
    :code "(savehist-mode t)"
    :inverted t)
   (make-conditional
    :question "Remember what files were last opened?"
    :code "(recentf-mode t)"
    :inverted t)
   (make-conditional
    :question "Abbreviate all yes-or-no queries?"
    ;; TODO: use `use-short-answers' for Emacs 28 or newer
    :code "(defalias 'yes-or-no #'y-or-n-p)"
    :inverted t)

   (lambda ()
     (format t "~2%;; Store automatic customisation options elsewhere")
     (format t "~%(setq custom-file (locate-user-emacs-file \"custom.el\"))")
     (format t "~%(when (file-exists-p custom-file)
(load custom-file))"))))


;; Prepare the web server

(defun generate-form-page ()
  (with-html-output (*standard-output* nil :prologue t)
    (:html
     (:head
      (:title "Emacs Configuration Generator")
      (:meta :charset "utf-8")
      (:meta :name "viewport"
             :content "width=device-width")
      (:link :rel "icon"
             :type "image/x-icon"
             :href "./static/images/favicon.ico")
      (:link :rel "stylesheet"
             :href "./static/style.css"))
     (:body
      (:header
       (:img :src "./static/images/emacs.gif" :class "right")
       (:h1 "Emacs Configuration Generator")
       (:p "Some people claim that "
           (:a :href "https://www.gnu.org/software/emacs/"
               "Emacs")
           " is difficult to start with."
           " The main problem is probably the "
           (:q "chicken-and-egg")
           " situation:  To make the most use of Emacs, you"
           " probably need to understand Emacs (Lisp), but"
           " to grok Emacs Lisp you pretty much neeed to"
           " understand the fundamentals of Emacs.")
       (:p "A common suggestion is to use " (:q "frameworks")
           " or ready-made configurations that provide a layer of abstraction to
help set up common functionality.  This site is an attempt to approach
the issue from a different standpoint, by having an interested user
pick-and-choose what they would like to start with and provide a
template to build on.  Note that you will be suggested a few packages
that are downloaded over the internet, from the ELPA ("
           (:q "Emacs Lisp package archive") ") repositories.")
       (:blockquote
        (:strong "Note:") " This site is still experimental, and
there is a lot more that can be done.  See "
        (:a :href "https://amodernist.com/texts/ecg.html" "this article")
        " for more details.")
       (:p "So if interested, fill out the form below and"
           " have a configuration file generated.")
       (:hr))
      (:main
       (:form :action "./generate" :method "POST"
              (mapc #'generate-form *options*)
              (:br)
              (:input :type "submit" :value "Make an init.el")))
      (:aside
       (:hr)
       (:h2 "Further links")
       (:p "If you have no previous experience with Emacs,
take your time to try out to built-in tutorial ("
           (:kbd "C-h t" ) ").  To view the documentation
for a package use " (:kbd "C-h P" ) ". Here are a few more relevant
links, that might be of use")
       (:ul :class "multicol"
            (:li (:a :href "https://www.gnu.org/software/emacs/"
                     "GNU Emacs homepage"))
            (:li (:a :href "https://www.gnu.org/software/emacs/tour/index.html"
                     "The Emacs tour"))
            (:li (:a :href "https://elpa.gnu.org/"
                     "GNU ELPA Package archive"))
            (:li (:a :href "https://elpa.nongnu.org/"
                     "NonGNU ELPA Package archive"))
            (:li (:a :href "https://www.gnu.org/software/emacs/manual/html_node/emacs/index.html"
                     "The GNU Emacs manual"))
            (:li (:a :href "https://sachachua.com/blog/category/emacs-news/"
                     "Emacs News"))
            (:li (:a :href "https://www.emacswiki.org/"
                     "EmacsWiki"))
            (:li (:a :href "https://mail.gnu.org/mailman/listinfo/help-gnu-emacs"
                     (:tt "help-gnu-emacs") " mailing list")))
       (:p "Also consider joining the "
           (:a :href "https://www.emacswiki.org/emacs/EmacsChannel"
               (:tt "#emacs"))
           " channel on "
           (:a :href "https://libera.chat/" "Libera Chat") "."))
      (:footer
       (:abbr :title "Emacs Configuration Generator" "ECG")
       " is "
       (:a :href "https://www.fsf.org/about/what-is-free-software" "Free Software")
       " and is available on "
       (:a :href "https://git.sr.ht/~pkal/ecg" "Sourcehut")
       " under the Terms and Conditions of the "
       (:a :href "https://www.gnu.org/licenses/agpl-3.0.en.html"
           "AGPL 3.0 License")
       ".")))))

(defvar *ecg-acceptor* (make-instance 'hunchentoot:easy-acceptor :port 9095))
(setf (hunchentoot:acceptor-document-root *ecg-acceptor*) "./static")

(hunchentoot:define-easy-handler (front-page :uri "/") ()
  (with-output-to-string (*standard-output*)
    (generate-form-page)))

(hunchentoot:define-easy-handler (generate :uri "/generate") ()
  (setf (hunchentoot:content-type*) "text/plain")
  (with-output-to-string (*standard-output*)
    (elisp-indent:with-indenting-output
      (dolist (opt *options*)
        (generate-conf opt)))))

(defun start ()
  (hunchentoot:start *ecg-acceptor*))
