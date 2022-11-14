;;; flymake-proselint.el --- Flymake backend for proselint -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022  Free Software Foundation, Inc.
;;
;; Author: Manuel Uberti <manuel.uberti@inventati.org>
;; Maintainer: Manuel Uberti <~manuel-uberti/flymake-proselint@lists.sr.ht>
;; Version: 0.3.0
;; Keywords: convenience
;; Package-Requires: ((emacs "27.1"))
;; URL: https://git.sr.ht/~manuel-uberti/flymake-proselint

;; flymake-proselint is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later version.
;;
;; flymake-proselint is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License
;; along with flymake-proselint.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; This package adds support for proselint (http://proselint.com/) in Flymake.

;; Once installed, the backend can be enabled with:
;; (add-hook 'markdown-mode-hook #'flymake-proselint-setup)

;;; Code:

(eval-when-compile
  (require 'subr-x)
  (require 'pcase))
(require 'flymake)
(require 'xdg)

(defgroup flymake-proselint ()
  "Flymake backend for proselint."
  :prefix "flymake-proselint-"
  :group 'flymake)

(defcustom flymake-proselint-executable "proselint"
  "Name of the Proselint executable."
  :type 'string)

(defcustom flymake-proselint-message-format
  "%m%r"
  "A format string to generate diagnostic messages.
The following %-sequences are replaced:

  %m - the message text
  %r - replacement suggestions
  %c - the error code"
  :type 'string)

(defconst flymake-proselint--options
  ;; Regenerate using:
  ;;
  ;; (with-temp-buffer
  ;;   (call-process flymake-proselint-executable nil t nil "--dump-config")
  ;;   (goto-char (point-min))
  ;;   (let ((config (json-parse-buffer :object-type 'alist)))
  ;;     (mapcar #'car (alist-get 'checks config))))
  '(airlinese.misc
    annotations.misc
    archaism.misc
    cliches.hell
    cliches.misc
    consistency.spacing
    consistency.spelling
    corporate_speak.misc
    cursing.filth
    cursing.nfl
    cursing.nword
    dates_times.am_pm
    dates_times.dates
    hedging.misc
    hyperbole.misc
    jargon.misc
    lexical_illusions.misc
    lgbtq.offensive_terms
    lgbtq.terms
    links.broken
    malapropisms.misc
    misc.apologizing
    misc.back_formations
    misc.bureaucratese
    misc.but
    misc.capitalization
    misc.chatspeak
    misc.commercialese
    misc.composition
    misc.currency
    misc.debased
    misc.false_plurals
    misc.illogic
    misc.inferior_superior
    misc.institution_name
    misc.latin
    misc.many_a
    misc.metaconcepts
    misc.metadiscourse
    misc.narcissism
    misc.not_guilty
    misc.phrasal_adjectives
    misc.preferred_forms
    misc.pretension
    misc.professions
    misc.punctuation
    misc.scare_quotes
    misc.suddenly
    misc.tense_present
    misc.waxed
    misc.whence
    mixed_metaphors.misc
    mondegreens.misc
    needless_variants.misc
    nonwords.misc
    oxymorons.misc
    psychology.misc
    redundancy.misc
    redundancy.ras_syndrome
    security.credit_card
    security.password
    sexism.misc
    skunked_terms.misc
    spelling.able_atable
    spelling.able_ible
    spelling.athletes
    spelling.em_im_en_in
    spelling.er_or
    spelling.in_un
    spelling.misc
    terms.animal_adjectives
    terms.denizen_labels
    terms.eponymous_adjectives
    terms.venery
    typography.diacritical_marks
    typography.exclamation
    typography.symbols
    uncomparables.misc
    weasel_words.misc
    weasel_words.very)
  "List of Proselint options.")

(defconst flymake-proselint--custom-type
  `(set :greedy t
	,@(mapcar
	   (lambda (opt)
	     ;; TODO: Add a :tag
	     `(const ,opt))
	   flymake-proselint--options))
  "Custom option type for proselint configurations.")

(defun flymake-proselint-safe-option-p (val)
  "Check if VAL is a safe (and valid) local value."
  (and (listp val)
       (catch 'fail
	 (dolist (elem val)
	   (unless (memq elem flymake-proselint--options)
	     (throw 'fail nil)))
	 t)))

(defcustom flymake-proselint-max-errors 1000
  "After how many errors should Proselint give up?
For this option to work, the user option
`flymake-proselint-config-directory' must be non-nil."
  :safe #'natnump
  :type 'natnum)

(defcustom flymake-proselint-enable '()
  "List of Proselint options to enable.
See `flymake-proselint--options' for a list of possible options.
For this option to work, the user option
`flymake-proselint-config-directory' must be non-nil."
  :safe #'flymake-proselint-safe-option-p
  :type flymake-proselint--custom-type)

(defcustom flymake-proselint-disable '()
  "List of Proselint options to disable.
See `flymake-proselint--options' for a list of possible options.
For this option to work, the user option
`flymake-proselint-config-directory' must be non-nil."
  :safe #'flymake-proselint-safe-option-p
  :type flymake-proselint--custom-type)

(defcustom flymake-proselint-config-directory
  (when-let* ((parent (or (xdg-cache-home) (temporary-file-directory)))
              (dir (expand-file-name "flymake-proselint" parent)))
    (condition-case nil
        (progn
          (unless (file-exists-p dir)
            (make-directory dir))
          dir)
      ;; In case there was an issue while creating the directory, this
      ;; option will just be disabled.
      (permission-denied nil)))
  "Directory to use for temporary configuration files.
If nil, `flymake-proselint' will not generate temporary
configuration files.  This means that the user options
`flymake-proselint-max-errors', `flymake-proselint-enable' and
`flymake-proselint-disable' will have no effect.."
  :type 'boolean)

(defun flymake-proselint-generate-configuration ()
  "Generate a configuration and return path.
This function uses the buffer local values of
`flymake-proselint-max-errors', `flymake-proselint-enable' and
`flymake-proselint-disable'.  If no configuration was
generated (either because it wasn't necessary or because disabled
by `flymake-proselint-inhibit-configuration'), the return value
will be nil."
  (unless (and flymake-proselint-config-directory
               (= flymake-proselint-max-errors 1000) ;default
               (null flymake-proselint-enable)
               (null flymake-proselint-disable))
    (let ((output (expand-file-name
                   ;; As done by `make-backup-file-name-1'
                   (secure-hash
                    'sha256
                    (prin1-to-string
                     (list (file-truename (buffer-file-name))
                           flymake-proselint-max-errors
                           flymake-proselint-enable
                           flymake-proselint-disable)))
                   flymake-proselint-config-directory)))
      (unless (file-exists-p output)
        (let* ((config (with-temp-buffer
                         (call-process flymake-proselint-executable nil t nil "--dump-config")
                         (goto-char (point-min))
                         (json-parse-buffer :object-type 'alist)))
               (checks (alist-get 'checks config)))
          (setf (alist-get 'checks config)
                (mapcar
                 (lambda (opt)
                   (cons opt
                         (cond
                          ((memq opt flymake-proselint-disable) :false)
                          ((memq opt flymake-proselint-enable)  t)
                          ((alist-get opt checks)))))
                 flymake-proselint--options)
                (alist-get 'max_errors config)
                flymake-proselint-max-errors)
          (with-temp-file output (json-insert config))))
      output)))

(defun flymake-proselint-sentinel-1 (source data)
  "Handle a successfully parsed DATA from SOURCE.
DATA is a list of error diagnostics that are converted into
Flymake diagnostic objects."
  (let (diags)
    (dolist (err (plist-get data :errors))
      (push (flymake-make-diagnostic
             source
             (plist-get err :start)
             (plist-get err :end)
             (pcase (plist-get err :severity)
               ("warning"	:warning)
               ("suggestion"	:note)
               (_		:error))
             (format-spec
              flymake-proselint-message-format
              `((?m . ,(plist-get err :message))
                (?c . ,(plist-get err :check))
                (?r . ,(let ((replacements (plist-get err :replacements)))
                         (cond
                          ((or (eq replacements :null) (null replacements))
                           ;; There are no replacements.
                           "")
                          ((stringp replacements)
                           (concat " (Replacement: " replacements ")"))
                          ((listp replacements)
                           (concat " (Replacements: "
                                   (mapconcat
                                    (lambda (r)
                                      (plist-get r :unique))
                                    replacements ", ")
                                   ")"))))))))
            diags))
    diags))

(defvar-local flymake-proselint--flymake-proc nil)

(defun flymake-proselint-sentinel (proc _event)
  "Sentinel on PROC for handling Proselint response.
A successfully parsed message is passed onto the function
`flymake-proselint-sentinel-1' for further handling."
  (let ((source (process-get proc 'source)))
    (when (buffer-live-p source)
      (pcase (process-status proc)
        ('exit
         (let ((report-fn (process-get proc 'report-fn)))
           (unwind-protect
               (with-current-buffer (process-buffer proc)
                 (goto-char (point-min))
                 (cond
                  ((with-current-buffer source
                     (not (eq proc flymake-proselint--flymake-proc)))
                   (flymake-log :warning "Canceling obsolete check %s" proc))
                  ((= (point-max) (point-min))
                   (flymake-log :debug "Empty response"))
                  ((condition-case err
                       (let ((response (json-parse-buffer :object-type 'plist
                                                          :array-type 'list)))
                         (if (string= (plist-get response :status) "success")
                             (thread-last
                               (plist-get response :data)
                               (flymake-proselint-sentinel-1 source)
                               (funcall report-fn))
                           (flymake-log :error "Check failed")))
                     (json-parse-error
                      (flymake-log :error "Invalid response: %S" err))))))
             (with-current-buffer source
               (setq flymake-proselint--flymake-proc nil))
             (kill-buffer (process-buffer proc)))))
        ('signal (kill-buffer (process-buffer proc)))))))

(defun flymake-proselint-backend (report-fn &rest _args)
  "Flymake backend for Proselint.
REPORT-FN is the flymake reporter function.  See the Info
node (flymake) Backend functions for more details."
  (unless (executable-find flymake-proselint-executable)
    (user-error "Executable proselint not found on PATH"))

  (when (process-live-p flymake-proselint--flymake-proc)
    (kill-process flymake-proselint--flymake-proc))

  (let ((proc (make-process
               :name "proselint-flymake" :noquery t :connection-type 'pipe
               :buffer (generate-new-buffer " *proselint-flymake*")
               :command
               (if-let* ((conf (flymake-proselint-generate-configuration)))
                   (list flymake-proselint-executable "--config" conf "--json" "-")
                 (list flymake-proselint-executable "--json" "-"))
               :sentinel #'flymake-proselint-sentinel)))
    (process-put proc 'source (current-buffer))
    (process-put proc 'report-fn report-fn)
    (setq flymake-proselint--flymake-proc proc)
    (save-restriction
      (widen)
      (process-send-region proc (point-min) (point-max))
      (process-send-eof proc))))

;;;###autoload
(defun flymake-proselint-setup ()
  "Enable Flymake backend proselint."
  (add-hook 'flymake-diagnostic-functions #'flymake-proselint-backend nil t))

(provide 'flymake-proselint)

;;; flymake-proselint.el ends here
