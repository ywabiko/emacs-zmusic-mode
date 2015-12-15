;;;
;;; zmusic-mode.el -- major mode for zmusic
;;;

;; Author: Yasuhiro Wabiko
;; Created: 17 Dec 1997
;; Version: Revision: 0.3
;; Keywords: zmusic, MML, MIDI
;; Copyright (C) Yasuhiro Wabiko

;; History
;;   v0.1 Jan 06, 1998 First release
;;   v0.2 Jun 06, 2006 wabee's TMIDI DDE support, Meadow-2.10 support
;;   v0.3 Jul 06, 2006 ".exclusive" support
;;   v0.4 Dec 15, 2015 cosmetic changes only
;;; Code:

(require 'shell)
(require 'compile)
(require 'timer)

(defvar zmusic-mode-debug-flag nil)
(defvar zmusic-send-command-modified-tick 0)
(defvar zmusic-playmidi-command "eplaymidi"
  "*Command used to run SMF player (ex. playmidi).")
(defvar zmusic-xeplaymidi-command "xeplaymidi"
  "*Command used to run visual SMF player (ex. xeplaymidi).")
(defvar zmusic-xwrdplay-command "xwrdplay"
  "*Command used to run WRD player (ex. xwrdplay).")
(defvar zmusic-playmidi-option "-e"
  "*Argument string for shell/external SMF player for foreground playback. eg) '-e' for [e]playmidi.")
(defvar zmusic-playmidi-background-option "&"
  "*Argument string for shell/external SMF player for background playback. eg) '&' for [e]playmidi.")
(defvar zmusic-shell-filename "/bin/bash"
  "*Command used as shell.")

(defvar zmusic-playmidi-device "/dev/sequencer"
  "*Device name that playmidi uses.")
(defvar zmusic-playmidi-term-command "fuser -k -TERM"
  "*Command used to terminate playmidi.")
(defvar zmusic-playmidi-kill-command "fuser -k -KILL"
  "*Command used to kill playmidi.")

(defvar zmusic-template-file nil
  "*Template file name used when inserting a new ZMS code.
Set to nil when template is not used.")

;;; 色
(defvar zmusic-color-comment 'firebrick
  "Color of comments" )
(defvar zmusic-color-dot-common-command 'DarkGreen
  "Color of common commands (that begins with '.')" )
(defvar zmusic-color-common-command 'RoyalBlue
  "Color of common commands in the form of (...), excluding track numbers." )
(defvar zmusic-color-repeat 'RoyalBlue 
  "Color of repeat command." )
(defvar zmusic-color-flow-control 'RoyalBlue 
  "Color of flow control commands, eg. [d.s.][d.c.]" )
(defvar zmusic-color-debug 'DeepPink
  "Color of debug command, eg. [!][@]" )
(defvar zmusic-color-track-number 'red-underline
  "Color of track number" )
(defvar zmusic-color-waon 'ForestGreen
  "Color of chords" )
(defvar zmusic-color-renpu 'ForestGreen
  "Color of tuplets" )
(defvar zmusic-color-portament 'ForestGreen
  "Color of portaments" )
(defvar zmusic-color-tie 'grey40
  "Color of ties (&)" )
(defvar zmusic-color-step-time 'Maroon
  "Color of step time commands (l)" )
(defvar zmusic-color-gate-time 'brown
  "Color of gate time commands (q)" )
(defvar zmusic-color-velocity 'tomato
  "Color of verocity commands (@u,z)" )
(defvar zmusic-color-hold 'NavyBlue
  "Color of hold pedals (@d)" )
(defvar zmusic-color-general-command 'DarkGoldenrod
  "Color of general commands" )

(defvar zmusic-blink-matching-repeat-delay 1
  "Time period in seconds to blink matching repeat command."
  )
(defvar zmusic-mode-hook nil
  "*Hook run when zmusic mode is started.")

(defvar zmusic-mode-syntax-table nil
  "Syntax table used while in zmusic mode.")

;;;
;;; Utilities from wabi.el
;;;

(defun list-to-string (l)
  "
Convert list into string. In Perl: 
 join(\",\",@list);
"
  (interactive)
  (let ((x))
    (if (eq l nil) "Nil"
      (if (not (eq (cdr l) nil))
	  (setq x (concat (car l) "," (list-to-string (cdr l))))
	(setq x (car l))
	)
      )))

(defun get-current-frame-config (key)
  "
Get current frame config from current-frame-configuration.

eg) To retrieve current cursor color,
     (get-current-frame-config 'cursor-color) 
"
  (interactive)
  (cdr 
   (assq 
    key
    (car (cdr (car (cdr (current-frame-configuration))))) ; maybe incorrect:-)
    ))
  )


;;;;;
;;;;; Cursor movement
;;;;;

(defun zmusic-jump-to-track (track)
  "
Jump to the specified track.
The current implementation searches for the first track command
from the beginning of the buffer and jumps to it.  If you split a
track into multiple pieces, you should consider other commands.

"
  (interactive "sJump to Track: ")
  (let* (
	 (oldpnt (point))
	 )
    (goto-char (point-min))
    (if (eq
	 (search-forward-regexp (concat "^\\((t[\t ]*" track "[\t ]*)\\)") (point-max) t)
	 nil)
	(progn
	  (goto-char oldpnt)
	  (error (concat "No such track: " track))
	  )
      ))
  )

(defun zmusic-jump-to-track-forward (track)
  "
Jump forward to the specified track.
"
  (interactive "sJump Forward to Track: ")
  (let* (
	 (oldpnt (point))
	 )
    (if (eq
	 (search-forward-regexp (concat "^\\((t[\t ]*" track "[\t ]*)\\)") (point-max) t)
	 nil)
	(progn
	  (goto-char oldpnt)
	  (error (concat "No such track: " track))
	  )
      ))
  )
(defun zmusic-jump-to-track-backward (track)
  "
Jump backward to the specified track.
"
  (interactive "sJump Backward to Track: ")
  (let* (
	 (oldpnt (point))
	 )
    (if (eq
	 (search-backward-regexp (concat "^\\((t[\t ]*" track "[\t ]*)\\)") (point-min) t)
	 nil)
	(progn
	  (goto-char oldpnt)
	  (error (concat "No such track: " track))
	  )
      ))
  )



;;;
;;; Repeat-related
;;;

(defvar zmusic-localvar-timer1 nil
  "DO NOT USE! (internal use only)"
  )

(defun zmusic-blink-matching-repeat (open close)
  )

(defun zmusic-blink-matching-repeat-original (open close)
  "
Highlit the maching repeat command with the one at the just
before the cursor position for 1 second.
If the matching repeat command is located outside of the current
visible buffer, show the command in the mini buffer.
"
  (interactive)
  (save-excursion
    (let ((zface 'zmusic-matching-repeat) p l x bol eol)
      (setq p (zmusic-match-repeat open close))
      (setq l (length open))
      (if (>= p 0)
	  (progn
	    ;; process all remaining requests
	    ;; for avoiding the racing
	    (while (not (eq zmusic-localvar-timer1 nil))
	      (setq x (car (reverse zmusic-localvar-timer1)))
	      (cancel-timer (car x))
	      (setq zmusic-localvar-timer1
		    (reverse (cdr (reverse zmusic-localvar-timer1))))
	      (add-text-properties (car (cdr x)) (+ (car (cdr x)) (car (cdr (cdr x))))  '(face default))
	      )

	    (add-text-properties p (+ p l)  '(face zmusic-matching-repeat))
	    (if (<= p (window-start)) ; when p is out of sight
		(progn
		;;; prepare message to be printed in minibuffer.
		  (goto-char p)
		  (beginning-of-line)
		  (setq bol (point))
		  (end-of-line)
		  (setq eol (point))
		  (message (concat "Matches " (buffer-substring bol eol)))
		  ))
	    (setq zmusic-localvar-timer1 
		  (cons
		   (list
		    (run-at-time (concat
				  zmusic-blink-matching-repeat-delay
				  " sec")
				 nil 'zmusic-blink-matching-repeat-2)
		    p l)
		   zmusic-localvar-timer1
		   )
		  )
	    )
	)
      )
    ))


(defun zmusic-blink-matching-repeat-2 ()
  "
sub function for timer release and face release.
(to be used with zmusic-blink-matching-repeat)

"
;; pop from zmusic-localvar-timer1 and use it.
(let* ((x ))
  (if (not (eq zmusic-localvar-timer1 nil))
      (progn
	(setq x (car (reverse zmusic-localvar-timer1)))
	
	(setq zmusic-localvar-timer1
	      (reverse (cdr (reverse zmusic-localvar-timer1))))
	(cancel-timer (car x))
	(add-text-properties (car (cdr x))
			     (+ (car (cdr x)) (car (cdr (cdr x))))  '(face default))
	))
  ))



(defun zmusic-match-repeat (open close)
  "
Return the point of the matching repeat command.
This is called immediately after you close the repeat command.
  (interactive)
; Algorithm:
;   Search backward for |: or :|
;   If :| is found, increment the counter.
;   If |: is found, decrement the counter.
;   If the counter is 0, found the peer. If >0, keep searching.
  (save-excursion
    (let* ((orig (point))
	   (cnt 0)
	   (cont 1)
	   (str "")
	   (limit (zmusic-beginning-of-this-track))
	   )
      (while
	  (and 
	   (eq cont 1)
	   (re-search-backward (concat "\\(" open "\\)\\|\\(" close "\\)") limit t)
	   )
	(cond
	 ((looking-at close)
	  (setq cnt (+ cnt 1))
	  (setq str (concat str "[CLOSE " cnt "]"))
					;	  (setq cont 0)
					;	  (add-text-properties (point) (+ (point) 1)  '(face highlight))
	  )
	 ((looking-at open)
	  (progn
	    (setq cnt (- cnt 1))
	    (setq str (concat str "[OPEN " cnt "]"))
	    (if (eq cnt 0)
		(setq cont 0)
	      (backward-char 1)
	      )
	    ))
	 
	 (t 
	  (setq str (concat str "failed?"))
	  )
	 )
	)
      (if (or
	   (not (eq cnt 0))  ; does not match anymore
	   (eq cont 1)  ; never matches from the beginning
	   )
	  (progn
	    (goto-char orig)  ; return to original point
					;		(message (concat "No matching repeat found. " str))
					;		(point)
	    -1 ; error code
	    )
	(point)  ; return value of this function
	)
      )))

(defun zmusic-bar-self-insert-wrapper ()
  "
This is called when you inserted \"|\".

If the previous character is \":\",
we think a repeat close command \":|\" was inserted,
and therefore call a function to highlight the matching one.

The current algorithm is less than optimal, but in most cases it just works fine:)
"
(interactive)
(let ((rep 0))
  (if (string= (char-to-string (preceding-char)) ":")
      (setq rep 1)
    )
  (insert "|")
  (if (eq rep 1)
      (zmusic-blink-matching-repeat "|:" ":|")
    (if zmusic-mode-debug-flag
	(message "not repeat")
      )
    )
  );let
)

(defun zmusic-close-paren-self-insert-wrapper ()
  "
This is called whtn you inserted a \"}\".

If the previosu character is \"}\",
we think a tick prefix MML \"{{...}}\" was inserted,
and therefore we call a function to highlight the matching \"{{\".

Othereise, we think a tuplets command \"{...}\" was inserted,
we call a function to highlight the maching \"{\".

The current algorithm is less than optimal, but in most cases it just works fine:)
"
(interactive)
(let ((rep 0))
  (if (string= (char-to-string (preceding-char)) "}")
      (setq rep 1)
    )
  (insert "}")
  (if (eq rep 1)
      (zmusic-blink-matching-repeat "{{" "}}")
    (zmusic-blink-matching-repeat "{" "}")
    )
  );let
)


;;;
;;; Track number-related
;;;

(defun zmusic-is-in-track (trk)
  "
Retun T or Nil according to whether the current line belongs to the given track number.
"
  (interactive "NTrack : ")
  (let* ((eot)(res nil)(x))
    (save-excursion
      (re-search-backward "\\((t\\)" nil t)
      (goto-char (match-end 0))
      (save-excursion ; get end-of-term position to limit search
	(re-search-forward "\\()\\)" nil t)
	(setq eot (point))
	)
      (while
	  (re-search-forward "\\([0-9]+\\)" eot t)
	(goto-char (match-end 0))
	(if (string= trk (buffer-substring (match-beginning 0) (match-end 0)))
	    (setq res t)
	  )
	)
      res
      )
    )
  )

(defun zmusic-this-track-number ()
  "
Return the current track number.

The implementation is that it searches backward for a track start command (t...),
then it joins all track numbers by a comma and return the result.
eg)
(t1) --> \"1\"
(t3,4) --> \"3,4\"
(t 3, 4 ) --> \"3,4\"
"
(interactive)
(let* ((eot)(res nil)(x))
  (save-excursion
    (end-of-line)
    (re-search-backward "\\((t\\)" nil t)
    (goto-char (match-end 0))
    (save-excursion ; get end-of-term position to limit search
      (re-search-forward "\\()\\)" nil t)
      (setq eot (point))
      )
    (while
	(re-search-forward "\\([0-9]+\\)" eot t)
      (goto-char (match-end 0))
      (setq res
	    (cons (buffer-substring (match-beginning 0) (match-end 0))
		  res
		  )
	    )
      )
    )
					;  (message (concat "track=" (list-to-string res))) ; return value
  (list-to-string (reverse res)) ; return value
  )
)


(defun zmusic-beginning-of-this-track ()
  "
Return the position that starts the current track code.
eg)
(t1)
(t1)
returns the first line.

(t3,4)
(t 3, 4)
returns the first line.

(t1)
(t1,3)
returns them seperately (i.e. treated as different track).
"
(interactive)
(save-excursion
  (let ((trk))
    (end-of-line)
    (setq trk (zmusic-this-track-number)) ; get this line's track number
    (beginning-of-line)
    (while
	(string= trk (zmusic-this-track-number))
      (forward-line -1)
      )
    )
  (forward-line 1)
					;    (message (concat "Track = " trk))
  (point)
  ))

(defun zmusic-goto-beginning-of-this-track ()
  "
Move to the position where the current track begins.
"
  (interactive)
  (goto-char (zmusic-beginning-of-this-track))
  )


;;;;
;;;; unclosed repeat commands stuff
;;;;

(defun zmusic-close-unclosed-paren ()
  "
Close an uncloded parentheses. (not implemeted yet)
"
  (interactive)
  (save-excursion
    (re-search-backward (concat "\\(|:\\)\\|\\(:|\\)") limit t)
    (if (> (zmusic-unclosed-repeat "|:" ":|") 0)
	(progn
	  (insert ":|")
	  (zmusic-blink-matching-repeat "|:" ":|")
	  )
      )
    )
  )

(defun zmusic-goto-unclosed-repeat ()
  "
Move to an unclosed repeat start command \"|:\" if any.
"
  (interactive)
  (let* ((x (zmusic-unclosed-repeat "|:" ":|")))
    (if (> x 0)
	(goto-char x))
    )
  )


(defun zmusic-close-unclosed-repeat ()
  "
Insert a repeat end command if there is an unclosed repeat start
command \"|:\" if any.
"
  (interactive)
  (if (> (zmusic-unclosed-repeat "|:" ":|") 0)
      (progn
	(insert ":|")
	(zmusic-blink-matching-repeat "|:" ":|")
	)
    )
  )

(defun zmusic-goto-unclosed-tick-reserve ()
  " 
Move to a position where there is an unclosed tick reserve
start command \"{{\".
"
  (interactive)
  (let* ((x (zmusic-unclosed-repeat "{{" "}}")))
    (if (> x 0)
	(goto-char x))
    )
  )


(defun zmusic-close-unclosed-tick-reserve ()
  "
Insert a tick reserve end command if there is an unclosed tick
 reserve start command \"{{\".
"
  (interactive)
  (if (> (zmusic-unclosed-repeat "{{" "}}") 0)
      (progn
	(insert "}}")
	(zmusic-blink-matching-repeat "{{" "}}")
	)
    )
  )


(defun zmusic-unclosed-repeat (open close)
  "
Return a position of an unclosed open parentheses (eg. repeat
start command) nearby backward.
"
  (interactive)
  (save-excursion
    (let* ((orig (point))
	   (x 0)
	   (cont 1)
	   (str "")
	   (limit (zmusic-beginning-of-this-track))
	   (res (point))
	   )
      (if (< limit 0)
	  (setq limit (point-min)))
					;(goto-char limit)))) ; debug

      (while (> cont 0)  ;; skip pairs of repeats
	(re-search-backward (concat "\\(" open "\\)\\|\\(" close "\\)") limit t)
	(if (looking-at close)
	    (progn
	      (forward-char (length close))
	      (goto-char (zmusic-match-repeat open close))
	      (backward-char 1)
	      )
	  (if (looking-at open)
	      (progn
		(setq res (point)) ; FOUND
		(setq cont 0)  ; break
		)
	    (progn
	      (setq res -1) ; ERROR
	      (setq cont 0) ; break
	      )
	    )
	  )
	)
      res
      )))



;;;;
;;;; IDE-related functions
;;;;

(defun zmusic-run-compile ()
  (interactive)
  (compile compile-command)
  )

(defun zmusic-shell-running ()
  "Return whether zmusic-shell is running or not."
  (interactive)
  (and (get-process "zmusic-shell")
       (eq (process-status (get-process "zmusic-shell")) 'run)))

(defun zmusic-kill-shell ()
  "Kill the currently running zmusic job."
  (interactive)

  (let ((zmusic-shell (get-buffer "*zmusic-shell*"))
	(old-buffer (current-buffer)))
    (if (null zmusic-shell)
	(message "No zmusic output buffer")
      (pop-to-buffer zmusic-shell)
      (bury-buffer zmusic-shell)
      (comint-kill-subjob)
      (pop-to-buffer old-buffer))))


(defun zmusic-start-shell ()
  "*Start zmusic sub shell."
  (interactive)
  (make-comint
   "zmusic-shell"
   zmusic-shell-filename
   nil
   )
  )


(defun zmusic-check-clock ()
  (interactive)
  (message "not implemented yet.")
  )


(defun zmusic-append-suffix (file-name suffix)
  "Append to FILENAME the suffix SUFFIX, using same algorithm zmusic uses;
i.e. delete the zms suffix \".zms\" from FILENAME, and append SUFFIX to it.
"
  (concat 
   (substring file-name 0
	      (string-match "\\.zms$" file-name))
   suffix))

(defun zmusic-term-playmidi ()
  "
Terminate playback by zmusic-playmidi by seding SIGTERM.
(A temporally solution until we use an SMF playback tool that can
be controlled via inter process communication.)
"
	  (interactive)
	  (if (not (zmusic-shell-running))
		  (zmusic-start-shell))
		 (zmusic-send-command 
           (concat zmusic-playmidi-term-command
                   " "
                   zmusic-playmidi-device))
;         (zmusic-kill-shell)

)


(defun zmusic-kill-playmidi ()
  "
Terminate playback by zmusic-playmidi by seding SIGKILL.
(A temporally solution until we use an SMF playback tool that can
be controlled via inter process communication.)
"
	  (interactive)
	  (if (not (zmusic-shell-running))
		  (zmusic-start-shell))
		 (zmusic-send-command 
           (concat zmusic-playmidi-kill-command
                   " "
                   zmusic-playmidi-device))
;		 (zmusic-recenter-output-buffer nil)
)

(defun zmusic-playmidi ()
  "
Playback SMF file that corresponds to the current buffer.

Due to a time lag to release a MIDI device, play after stop does not always work.
If it does not play, please try C-c s as well.
"
  (interactive)
  (if (not (zmusic-shell-running))
      (zmusic-start-shell))
					;	 (display-buffer (process-buffer (get-process "zmusic-playmidi")))
  (zmusic-send-command (concat
			zmusic-playmidi-command
					;" -e "
			" "
			zmusic-playmidi-option
			" "
			(zmusic-append-suffix
			 (concat
			  (file-name-directory (buffer-file-name))
			  (file-name-nondirectory (buffer-file-name))
			  )
			 ".mid"
			 )
			zmusic-playmidi-background-option
			))
					;         (zmusic-kill-shell)
					;		 (zmusic-recenter-output-buffer nil)
  )


(defun zmusic-eplaymidi-clear-lockfile ()
  "
Delete the lock file left by previous eplaymidi.
(/tmp/LCK..eplaymidi).
"
	  (interactive)
         (zmusic-kill-playmidi)
	  (if (not (zmusic-shell-running))
		  (zmusic-start-shell))
	  
		  (zmusic-send-command 
		   (concat
			"rm /tmp/LCK..eplaymidi"
			)
		   )
		 (zmusic-recenter-output-buffer nil)
)

(defun zmusic-run-xeplaymidi ()
  (interactive)
  (if (not (zmusic-shell-running))
      (zmusic-start-shell))
  
  (zmusic-send-command 
   (concat
    zmusic-xeplaymidi-command
    "&"
    )
   )
  (zmusic-recenter-output-buffer nil)
  )

(defun zmusic-run-xwrdplay ()
  (interactive)
  (if (not (zmusic-shell-running))
      (zmusic-start-shell))
  
  (zmusic-send-command 
   (concat
    zmusic-xwrdplay-command
    " "
    (zmusic-append-suffix
     (concat
      (file-name-directory (buffer-file-name))
      (file-name-nondirectory (buffer-file-name))
      )
     ".wrd"
     )
    "&"
    )
   )
  (zmusic-recenter-output-buffer nil)
  )


(defun zmusic-send-command (command &optional file background)
  "Send COMMAND to zmusic shell process, substituting optional FILE for *.
Do this in background if optional BACKGROUND is t.  If COMMAND has no *,
FILE will be appended, preceded by a blank, to COMMAND.  If FILE is nil, no
substitution will be made in COMMAND.  COMMAND can be any expression that
evaluates to a command string."
  (save-excursion
    (let* ((cmd (eval command))
	   (proc (get-process "zmusic-shell"))
	   (buf (process-buffer proc))
           (star (string-match "\\*" cmd))
	   (string
	    (concat
	     (if file
		 (if star (concat (substring cmd 0 star)
				  file (substring cmd (1+ star)))
		   (concat cmd " " file))
	       cmd)
	     (if background "&" ""))))
      ;; Switch to buffer before checking for subproc output in it.
      (set-buffer buf)
      ;; If text is unchanged since previous zmusic-send-command,
      ;; we haven't got any output.  So wait for output now.
      (if (= (buffer-modified-tick buf) zmusic-send-command-modified-tick)
	  (accept-process-output proc))
      (goto-char (process-mark proc))
      (insert string)
      (comint-send-input)
      (setq zmusic-send-command-modified-tick (buffer-modified-tick buf))
      )
    )
  )

(defun zmusic-recenter-output-buffer (linenum)
  "Redisplay buffer of zmusic job output so that most recent output can be seen.
The last line of the buffer is displayed on
line LINE of the window, or centered if LINE is nil."
  (interactive "P")
  (let ((zmusic-shell (get-buffer "*zmusic-shell*"))
	(old-buffer (current-buffer)))
    (if (null zmusic-shell)
	(message "No zmusic output buffer")
      (pop-to-buffer zmusic-shell)
      (bury-buffer zmusic-shell)
      (goto-char (point-max))
      (recenter (if linenum
		    (prefix-numeric-value linenum)
		  (/ (window-height) 2)))
      (pop-to-buffer old-buffer))))


(defun zmusic-insert-template-file ()
  "

Insert the template file.

"
  (interactive)
  (let* ((success 1))
    (condition-case nil
	(insert-file-contents zmusic-template-file)
      (file-error
       (setq success 0)
       )
      )
    (if (eq success 0)
	(with-output-to-temp-buffer "*zmusic-mode-error*"
	  (princ 
	   (concat
	    "Cannot open template ("
	    zmusic-template-file
	    ")."
	    ))
	  (terpri)
	  (print-help-return-message))
      )))

;;;;
;;;; zmusic-mode.el - major mode common stuff
;;;;

(defun zmusic-define-common-keys (keymap)
  "Define the keys that we want defined both in zmusic mode and in the zmusic shell."
  (define-key keymap "|" 'zmusic-bar-self-insert-wrapper)
  (define-key keymap "}" 'zmusic-close-paren-self-insert-wrapper)
  (define-key keymap "\C-c\C-c" 'zmusic-run-compile)
					;  (define-key keymap "\C-c\C-v" 'zmusic-check-clock)
  (define-key keymap "\C-cp" 'zmusic-playmidi)
  (define-key keymap "\C-cs" 'zmusic-term-playmidi)
  (define-key keymap "\C-cv" 'zmusic-run-xeplaymidi)
  (define-key keymap "\C-cw" 'zmusic-run-xwrdplay)
  (define-key keymap "\C-ck" 'zmusic-eplaymidi-clear-lockfile)
  (define-key keymap "\C-c\C-l" 'zmusic-recenter-output-buffer)
  (define-key keymap "\C-c\C-j" 'zmusic-jump-to-track)
  (define-key keymap "\C-c\C-f" 'zmusic-jump-to-track-forward)
  (define-key keymap "\C-c\C-b" 'zmusic-jump-to-track-backward)
  (define-key keymap "\C-c:" 'zmusic-insert-repeats)
  (define-key keymap "\C-c\C-e" 'zmusic-close-unclosed-repeat)
  (define-key keymap "\C-c\C-u" 'zmusic-goto-unclosed-repeat)
					;  (define-key keymap "\C-ci" 'zmusic-view-include-file)
  (define-key keymap [S-return] 'zmusic-newline)

  (define-key keymap [menu-bar zmusic] (cons "Zmusic" (make-sparse-keymap "zmusic")))

  (define-key keymap [menu-bar zmusic zmusic-goto-unclosed-repeat] '("Jump to Last Unended Repeat" . zmusic-goto-unclosed-repeat))
  (define-key keymap [menu-bar zmusic zmusic-close-unclosed-repeat] '("Close Unended Repeat" . zmusic-close-unclosed-repeat))
  (define-key keymap [menu-bar zmusic zmusic-jump-to-track-backward] '("Jump Backward to Track" . zmusic-jump-to-track-backward))
  (define-key keymap [menu-bar zmusic zmusic-jump-to-track-forward] '("Jump Forward to Track" . zmusic-jump-to-track-forward))
  (define-key keymap [menu-bar zmusic zmusic-jump-to-track] '("Jump to Track" . zmusic-jump-to-track))
  (define-key keymap [menu-bar zmusic zmusic-recenter-output-buffer]
    '("Recenter Shell Buffer" . zmusic-recenter-output-buffer))
  (define-key keymap [menu-bar zmusic zmusic-run-xwrdplay] '("WRD Player" . zmusic-run-xwrdplay))
  (define-key keymap [menu-bar zmusic zmusic-run-xeplaymidi] '("Status Viewer" . zmusic-run-xeplaymidi))
  (define-key keymap [menu-bar zmusic zmusic-kill-playmidi] '("Stop" . zmusic-term-playmidi))
  (define-key keymap [menu-bar zmusic zmusic-playmidi] '("Play" . zmusic-playmidi))
					;  (define-key keymap [menu-bar zmusic zmusic-check-clock] '("Check Total Clock by ZMC" . zmusic-check-clock))
  (define-key keymap [menu-bar zmusic next-error] '("Jump to Next Error" . next-error))
  (define-key keymap [menu-bar zmusic zmusic-run-compile] '("Compile" . zmusic-run-compile))
  )

(defvar zmusic-mode-map nil "Keymap for zmusic mode.")

(if zmusic-mode-map 
    nil
  (setq zmusic-mode-map (make-sparse-keymap))
  (zmusic-define-common-keys zmusic-mode-map)
  )

(defun zmusic-common-initialization ()
  (kill-all-local-variables)
  (use-local-map zmusic-mode-map)

  ;; compilation-related
  (make-local-variable 'compile-command)
  (make-local-variable 'compilation-error-regexp-alist)
  (setq compile-command
	(concat "zms2mid "
		(zmusic-append-suffix 
		 (file-name-nondirectory (buffer-file-name))
		 "")))
					;				buffer-file-name))
  (setq compilation-error-regexp-alist
	(list
	 (list "\\([^ \t\n]+\\)[ \t]*\\([0-9]+\\):" 1 2)
	 (list "\\([^ \t\n]+\\)[ \t]*\\([0-9]+\\):[ \t]*\\([0-9]+\\):" 1 2 3)
	 ))

  ;; Syntax Table 
  (if zmusic-mode-syntax-table
      ()
    (setq zmusic-mode-syntax-table (make-syntax-table text-mode-syntax-table))
    (modify-syntax-entry ?<  ".  " zmusic-mode-syntax-table)
    (modify-syntax-entry ?>  ".  " zmusic-mode-syntax-table)
    (modify-syntax-entry ?@   "w   " zmusic-mode-syntax-table)
    (modify-syntax-entry ?{   "(}  " zmusic-mode-syntax-table)
    (modify-syntax-entry ?}   "){  " zmusic-mode-syntax-table)
    (modify-syntax-entry ?\"   "\"   " zmusic-mode-syntax-table)
					;	(modify-syntax-entry ?'   ")'   " zmusic-mode-syntax-table) ; no good
					;	(modify-syntax-entry ?'   "\"   " zmusic-mode-syntax-table) ; no good
					;	(modify-syntax-entry ?'   "$   " zmusic-mode-syntax-table) ; not very good
    (modify-syntax-entry ?'   "_   " zmusic-mode-syntax-table) ; best for now
    (modify-syntax-entry ?/   "\\\;   " zmusic-mode-syntax-table)
    )
  (set-syntax-table zmusic-mode-syntax-table)

;;; Face for blinking repeat commands
  (make-face 'zmusic-matching-repeat) ; [Note] nothing done if already exists
  (copy-face 'highlight 'zmusic-matching-repeat)
  (set-face-foreground
   'zmusic-matching-repeat
   (get-current-frame-config 'cursor-color)
   )

  )

(defun zmusic-mode ()
  "Major mode for zmusic.

	C-c C-c		Compile ZMS
    M-x compile Same as above.
    C-x `       Go to next error.
	C-c p		Playback the SMF file that corresponds to the current buffer.
	C-c s		Stop playback.
	C-c v		Launch status viewer (default:xeplaymidi).
	C-c w		Launch WRD player (default:xwrdplay).
	C-c k		Delete lock file for eplaymidi (rm /tmp/LCK..eplaymidi)
	C-c C-l		Refresh *zmusic-shell* buffer.

	C-c C-j		Jump to the given track (search from the top).
	C-c C-f		Jump to the given track (search forward).
	C-c C-b		Jump to the given track (search backward).

	C-c :		Insert repeat commands `|:  :|'
	C-c C-e		Close unclosed repeat command by inserting `:|'
	C-c C-u		Jump to unclosed repeat command nearby backwards.

"
  (interactive)
  (zmusic-common-initialization)
  (setq mode-name "zmusic")
  (setq major-mode 'zmusic-mode)

  (if (and (stringp zmusic-template-file)
	   (zerop (buffer-size)))
      (zmusic-insert-template-file)
    )
  
  ;;
  ;; hilit19 relatd stuff
  ;; 

  (if (fboundp 'hilit-set-mode-patterns)
      (progn
	(hilit-lookup-face-create 'zmusic-comment)
	(hilit-translate zmusic-comment zmusic-color-comment)
	(hilit-lookup-face-create 'zmusic-dot-common-command)
	(hilit-translate zmusic-dot-common-command zmusic-color-dot-common-command)
	(hilit-lookup-face-create 'zmusic-common-command)
	(hilit-translate zmusic-common-command zmusic-color-common-command)
	(hilit-lookup-face-create 'zmusic-repeat)
	(hilit-translate zmusic-repeat zmusic-color-repeat)
	(hilit-lookup-face-create 'zmusic-flow-control)
	(hilit-translate zmusic-flow-control zmusic-color-flow-control)
	(hilit-lookup-face-create 'zmusic-debug)
	(hilit-translate zmusic-debug zmusic-color-debug)
	(hilit-lookup-face-create 'zmusic-track-number)
	(hilit-translate zmusic-track-number zmusic-color-track-number)
	(hilit-lookup-face-create 'zmusic-waon)
	(hilit-translate zmusic-waon zmusic-color-waon)
	(hilit-lookup-face-create 'zmusic-renpu)
	(hilit-translate zmusic-renpu zmusic-color-renpu)
	(hilit-lookup-face-create 'zmusic-portament)
	(hilit-translate zmusic-portament zmusic-color-portament)
	(hilit-lookup-face-create 'zmusic-tie)
	(hilit-translate zmusic-tie zmusic-color-tie)
	(hilit-lookup-face-create 'zmusic-step-time)
	(hilit-translate zmusic-step-time zmusic-color-step-time)
	(hilit-lookup-face-create 'zmusic-gate-time)
	(hilit-translate zmusic-gate-time zmusic-color-gate-time)
	(hilit-lookup-face-create 'zmusic-velocity)
	(hilit-translate zmusic-velocity zmusic-color-velocity)
	(hilit-lookup-face-create 'zmusic-hold)
	(hilit-translate zmusic-hold zmusic-color-hold)
	(hilit-lookup-face-create 'zmusic-general-command)
	(hilit-translate zmusic-general-command zmusic-color-general-command)
	(hilit-set-mode-patterns
	 'zmusic-mode
	 '(
	   ("^\\.comment" "$" zmusic-dot-common-command) ; .comment
	   ("^\\.define" "$" zmusic-dot-common-command) ; .comment
	   ("^\\.\\(sc55\\|mt32\\|u220\\|m1\\)_print[ \t\n]*[$a-f0-9]*[ \t\n]*\".*\"" nil zmusic-dot-common-command) ; .sc55_init
	   ("^\\.sc55_init" nil zmusic-dot-common-command) ; .sc55_init
	   ("^\\.sc55_\\(v_reserve\\|reverb\\|chorus\\|part_setup\\|drum_setup\\|display\\)" "}" zmusic-dot-common-command) ; .sc55_reverb
	   ("^\\.mt32_\\(p_reserve\\|reverb\\|part_setup\\|drum_setup\\|common\\|partial\\|patch\\)" "}" zmusic-dot-common-command) ; .mt32_reverb
	   ("^\\.u220_\\(setup\\|common\\|part_setup\\|drum_setup\\|timbre\\|drum_inst\\)" "}" zmusic-dot-common-command) ; .u220_setup
	   ("^\\.m1_\\(midi_ch\\|part_setup\\|effect_setup\\)" "}" zmusic-dot-common-command) ; .m1_part_setup
	   ("^\\.send_to_m1[ \t\n]*[$a-f0-9]*" nil zmusic-dot-common-command) ; .send_to_m1
	   ("^\\.roland" "}" zmusic-dot-common-command) ; .sc55_init
	   ("^\\.exclusive" "}" zmusic-dot-common-command) ; .sc55_init
	   ("/" "$" zmusic-comment)  ; comment
	   ("(t[\t ]*[0-9]+\\([\t ]*,[\t ]*[0-9]+\\)*[\t ]*)" nil zmusic-track-number) ;  (t1)
	   ("(p)" nil zmusic-common-command) ;  (p1)
					;	   ("(p[\t ]*[0-9]+\\([\t ]*,[\t ]*[0-9]+\\)*[\t ]*)" nil label) ;  (p1)
	   ("'[a-g<>][-+#a-g<>0-9\\.^]*'\\([0-9]+\\)?\\(,[-+0-9]+\\)?" nil zmusic-waon) ; chord
	   ("{[a-g<>][-+#a-gr<>0-9\\.^]}" nil zmusic-renpu) ; tuplets

	   ("(i)" nil zmusic-common-command) ; (i) (any command whose prefix is not a tone)
	   ("([bdoz][0-9]+)" nil zmusic-common-command)  ; (d0) (any command whose prefix could be a tone)
	   ("(amidi[0-9]+,[0-9]+)" nil zmusic-common-command) ; (aMIDI1,10) 
	   ("(m[0-9]+,[0-9]+)" nil zmusic-common-command) ; (m1,1000) 
	   ("\\[[!@]\\]" nil zmusic-debug) ; [!]
	   ("\\[" "\\]" zmusic-flow-control) ; [d.s.]
	   ("\\\\[-+0-9]+" nil zmusic-flow-control) ; \5
	   ("|:[0-9]*" nil zmusic-repeat) ; repeat
	   ("|[0-9]*" nil zmusic-repeat) ; repeat
	   (":|" nil zmusic-repeat) ; repeat

	   ("([<>]*[a-g][-#+]?\\([0-9^.]+\\)?\\(,\\)?[<>]*\\(o[0-9]\\)?[a-g][-#+]?[<>]*)\\([0-9^.]+\\)?\\(,[0-9]+\\)?" nil zmusic-portament) ; portament

;;;;;; Warning: Followings are too slow on Pentium133+RAM48M and 50-KB ZMS.
	   ("&" nil zmusic-tie) ; & 
	   ("@[0-9]+" nil zmusic-general-command) ; @11
	   ("z\\([-+0-9]*\\|$[0-9a-f]*\\)\\(,\\([-+0-9]*\\|$[0-9a-f]*\\)\\)*" nil zmusic-velocity) ; z90,100,120
	   ("@u\\([-+0-9]*\\|$[0-9a-f]*\\)" nil zmusic-velocity) ; @u90
	   ("q\\([-+0-9]*\\|$[0-9a-f]*\\)\\(,\\([-+0-9]*\\|$[0-9a-f]*\\)\\)*" nil zmusic-gate-time) ; q90,100,120
	   ("@d\\([-+0-9]*\\|$[0-9a-f]*\\)" nil zmusic-hold) ; @d100
	   ("l\\([-+0-9]*\\|$[0-9a-f]*\\)" nil zmusic-step-time) ; l32
	   ("[himsx_~`]\\([-+0-9]*\\|$[0-9a-f]*\\)\\(,\\([-+0-9]*\\|$[0-9a-f]*\\)\\)*" nil zmusic-general-command) ; z90,100,120
	   ("@[abceghijklmpqrsvxyz][-+\\$a-f0-9]*\\(,[-+\\$a-f0-9]*\\)*" nil zmusic-general-command) ; @z90,100,120
	   
	   ("[hjknoptuvwy][-+0-9]+" nil zmusic-general-command) ; j1
	   )
	 nil 'case-insensitive)
	nil)
    )
  (run-hooks 'zmusic-mode-hook))

(provide 'zmusic-mode)

;;; zmusic-mode.el ends here
