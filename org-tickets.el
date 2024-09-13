;; (define-key emacs-lisp-mode-map (kbd "C-c C-m") 'emacs-lisp-byte-compile-and-load)
(eval-when-compile (require 'org-install))
(eval-when-compile (require 'cl))
(eval-when-compile (require 'calendar))
(eval-when-compile ;; (require 'org-babel-init)
  ;; (require 'org-babel-emacs-lisp)
  ())

(defcustom susie-email "susie@sapphire.phys.unm.edu" "The address to use to send mail to Susie." :type '(string) :group 'org-tickets)
(defcustom director-email "director@carc.unm.edu" "The address to use to send mail to the Director." :type '(string) :group 'org-tickets)
(defcustom director-short-name "Director" "The short name to use to address the Director." :type '(string) :group 'org-tickets)

(defmacro with-tickets-dot-org (&rest body)
  `(progn
     (let ((pwd default-directory))
       (save-window-excursion
       (save-excursion
         (or (and (switch-to-buffer 
                   (find-buffer-visiting "tickets.org")))
             (and
              (and 
               (or (find-file (format "tickets.org"))
                   (and (find-file (format "%s/work/HPCERC/org-tickets/tickets.org" (getenv "HOME")))
                        (setf default-directory (format "%s/work/HPCERC/org-tickets/tickets.org" (getenv "HOME"))))))
              (if (find-buffer-visiting "tickets.org")
                  (switch-to-buffer (find-buffer-visiting "tickets.org"))
                  (error "couldn't switch to tickets.org!")))
             (error "couldn't find tickets.org!"))
         ,@body)))))

(defmacro my-replace-regexp (regexp to-string)
  `(while (re-search-forward ,regexp nil t)
     (replace-match ,to-string nil nil)))

(defmacro my-gensym (&optional prefix)
  "fake gensym"
  `(make-symbol (format "%s%d"
                        (or ,prefix "G") 
                        (setf *my-gensym-counter* (1+ *my-gensym-counter*)))))


(eval-when-compile (defvar *my-gensym-counter* 0))

;; name-id-list: ((id name (var-name var-docstring) (accessor-name accessor-docstring)) ... )

(defmacro named-fields (name-id-list)
  `(progn
     ,@ ;; for some strange reason, emacs indents this wrong if you don't 
        ;; separate the ,@ and the (let ... ) :P !
     (let ((ret nil))
       (dolist (thing name-id-list ret)
         (let ((var-name (or (and (nth 2 thing)
                                  (if (listp (nth 2 thing))
                                      (car (nth 2 thing))
                                      (nth 2 thing)))
                             (nth 0 thing)))
               (accessor-name (or (and (nth 3 thing)
                                       (if (listp (nth 3 thing))
                                           (car (nth 3 thing))
                                           (nth 3 thing)))
                                  (nth 0 thing))))
           (setf ret
                 (append 
                  ret
                  `((defvar ,var-name ,(nth 1 thing))
                    (setf ,var-name ,(nth 1 thing))
                    (defmacro ,accessor-name (ticket)
                      `(nth ,',var-name ,ticket)))))))
       ret
       )
     ))

(named-fields ((ticket-id 0)
               (ticket-subject 1)
               (ticket-opened 2)
               (ticket-state 3)
               (ticket-user 4)
               (ticket-user-email 5)
               (ticket-owner 6)
               (ticket-last-entry 7)
               (ticket-category 8)
               (ticket-who 9)
               ))

;; is this right?
(named-fields ((transition-state 0)
               (transition-days 1)
               (transition-next 2)
               (transition-breakdown-p 3)
               (transition-report-order 4)))

(defmacro ticket-new-who (ticket)
  `(let ((owner (owner-short-name (ticket-owner ,ticket)))
         (who-column (nth 9 ,ticket)))
     (cond 
       ;; owner and who-col don't match
       ((and owner (not (equal "" owner)) 
             who-column (not (equal "" who-column))
             (not (equal owner who-column)))
        who-column)
       ;; owner and who-col do match
       ((and owner (not (equal "" owner))
             who-column (not (equal "" who-column)))
        owner)
       ;; nothing in the who-col
       ((or (not who-column)
            (equal "" who-column))
        owner)
       ;; nothing in the owner column
       ((or (not owner)
            (equal "" owner))
        who-column))))

(defmacro ticket-link (ticket)
  `(if (posix-string-match ".*?\\[\\([0-9]+\\).*" ,ticket)
       (replace-regexp-in-string 
        ".*?\\[\\([0-9]+\\).*" "[[\\1]]" ,ticket)
       (warn (format "no match %s\n" ,ticket))))

(defun email-link (ticket)
  (if (posix-string-match ".*?\\(\\w+@\\w+\\).*" ticket)
      (replace-regexp-in-string 
       "\\(.*\\)" "[[https://www.carc.unm.edu/irs/find_person_by_email.html?email_address=\\1][\\1]]" ticket)
       (warn (format "no match %s\n" ticket))))

;; (defmacro email-link (ticket)
;;   `(if (posix-string-match ".*?\\[\\(\\w+@\\w+\\).*" ,ticket)
;;        (replace-regexp-in-string 
;;         ".*?\\(\\w+@\\w+\\).*" "[[https://www.carc.unm.edu/irs/find_person_by_email.html?email_address=\\1]]" ,ticket)
;;        (warn (format "no match %s\n" ,ticket))))

(defvar *default-state* "NEW")

;; macro-writing macro for handling various table changes
(defmacro changing-entry (field name &optional macro-name)
  `(defmacro ,(or macro-name (intern (format "changing-%s" name))) (&rest body)
     (let ((point (my-gensym))
           (result (my-gensym)))
       `(let ((,point (point))
              (,result ()))
          (org-table-goto-column (1+ ,',field))
          (setf ,result (progn ,@body))
          (goto-char ,point)
          (org-ctrl-c-ctrl-c)
          (let ((new (org-table-get nil (1+ ,',field))))
            (set-text-properties 0 (length new) () new)
            (print (format ,,(format "New %s: %%s" name) new)))
          ,result))))

(changing-entry ticket-category state)
(changing-entry ticket-owner owner)

(defun my-copy-list (list)
  (let ((ret nil))
    (nreverse
     (dolist (var list ret)
       (push var ret)))))

;; compare two time strings
(defun time>-p (t1 t2)
         (> (org-time-string-to-absolute t1)
            (org-time-string-to-absolute t2)))

(defun time<-p (t1 t2)
         (> (org-time-string-to-absolute t1)
            (org-time-string-to-absolute t2)))

(defun time=-p (t1 t2)
         (= (org-time-string-to-absolute t1)
            (org-time-string-to-absolute t2)))

;; decode url-encoded string
(defun url-decode-string (str)
  (let ((start 0)
	(buf)
	(case-fold-search t))
    (while (string-match "%\\(?:\\([0-9a-f][0-9a-f]\\)\\|0d%0a\\)" str start)
      (push (substring str start (match-beginning 0)) buf)
      (push (if (match-beginning 1)
		(vector (string-to-number (match-string 1 str) 16))
	      "\n")
	    buf)
      (setq start (match-end 0)))
    (setq str (apply 'concat (nreverse (cons (substring str start) buf))))))

;; find link description in a link

(defun link-description (link)
  (replace-regexp-in-string "\\[\\[.+?\\]\\[\\(.*?\\)\\]\\]" "\\1" link))

(defun link-link (link)
  (replace-regexp-in-string "\\[\\[\\(.+?\\)\\]\\[\\(.*?\\)\\]\\]" "\\1" link))

(defvar *orgtbl-hline* nil)

(defun ticket-categories (ticket-lst)
  (let ((categories (make-hash-table :test #'equal))
        (transitions (slurp-table "state-transitions"))
        (ret ()))
    (mapc
     (lambda (ticket)
       (unless (or (equal *orgtbl-hline* ticket)
                   (equal 'hline ticket)
                   (equal "AIRS ticket #" (ticket-id ticket)))
         (unless (gethash (ticket-category ticket) categories)
           (setf (gethash (ticket-category ticket) categories)
                 (make-hash-table :test #'equal)))
         ;; ensure count
         (unless 
             (gethash 'count (gethash (ticket-category ticket) categories))
           (setf (gethash 'count (gethash (ticket-category ticket) categories)) 0))
         ;; increment category count
         (incf (gethash 'count (gethash (ticket-category ticket) categories)))
         ;; save the ticket to the ticket-category
         (push (ticket-link (ticket-id ticket)) (gethash 'tickets (gethash (ticket-category ticket) categories)))
         ;; ensure category by user count
         (unless (gethash (ticket-new-who ticket) (gethash (ticket-category ticket) categories))
           (setf (gethash (ticket-new-who ticket) 
                          (gethash (ticket-category ticket) categories)) (list 0 nil)))
         ;; increment count
         (incf (car (gethash (ticket-new-who ticket)
                             (gethash (ticket-category ticket) categories))))
         ;; save the ticket to the cat / user list
         (push (ticket-link  (ticket-id ticket)) (cadr (gethash (ticket-new-who ticket)
                                                        (gethash (ticket-category ticket) categories)))))
       )
     ticket-lst)
    ;; map categories table 
    (let ((total 0))
      (maphash
       (lambda (category cat-table)
         (when (eql 0 (length category))
           (setf category *default-state*))
         (push (list category "Total:"  (gethash 'count cat-table)
                     (replace-regexp-in-string "\\((\\|)\\)" ""
                                               (format "%s" (gethash 'tickets cat-table)))) ret)
         (when (or (equal (nth 3 (assoc category transitions)) "YES")
                   (equal (nth 3 (assoc category transitions)) "yes")
                   (equal (nth 3 (assoc category transitions)) "t"))
           (maphash 
            (lambda (k v)
              (unless (or (equal 'count k)
                          (equal 'tickets k)
                          (equal "" k))
              (push (list category k  (car v) (replace-regexp-in-string "\\((\\|)\\)" "" (format "%s" (cadr v)))) ret)))
            cat-table))
         (incf total (gethash 'count cat-table))
         (push 'hline ret))
       categories)
      (push (list "Still Open Tickets" "Total:" 
                  (- total 
                     (or
                      (ignore-errors
                        (gethash 'count (gethash "closed" categories)))
                      0))) ret)
      (push (list "All Tickets" "Total:" total) ret))
    (remove 'hline (reverse ret))))

(defvar *latex-header* 
"#+LATEX_HEADER: \\usepackage{fullpage}

#+BEGIN_LaTeX
\\setcounter{secnumdepth}{-1}

\\makeatletter
\\renewcommand\\section{\\@startsection{section}{1}{\\z@}%
{-3.5ex \\@plus -1ex \\@minus -.2ex}%
{1.3ex \\@plus.2ex}%
{\\normalfont\\medium\\bfseries}}

\\makeatother

\\makeatletter
\\newenvironment{itemize*}%
  {%
    %from parlist package, reduces indent before bulletpoints
    % \\setdefaultleftmargin{1em}{1em}{}{}{}{} 
    \\compress %places itemize into minipage, removing whitespace before
    \\begin{itemize}%
    \\\\setlength{\\itemsep}{0pt}%
    \\setlength{\\topsep}{0pt} 
    \\setlength{\\partopsep}{0pt}
    \\setlength{\\parsep}{0pt}
    \\setlength{\\parskip}{0pt}}%
  {\\end{itemize}}
\\makeatother
#+END_LaTeX")

(defvar *ticket-report-cutoff* 1000)

(defvar *weekly-ticket-report-orgmode-header* "#+TITLE: Weekly Ticket Report")

;; new version June 10 2014
(defun* weekly-ticket-report (&optional (ticket-table "")) 
  (interactive "*sTicket Table: \n")
  (let ((ticket-table (if (not (equal "" ticket-table))
                          ticket-table
                          "tickets"))
        (ticket-lst (slurp-table ticket-table))
        (ret ())
        (buffer (get-buffer-create "weekly-ticket-report.org")))
    (mapc
     (lambda (ticket)
       (unless (or (equal *orgtbl-hline* ticket)
                   (equal 'hline ticket)
                   (equal "AIRS ticket #" (ticket-id ticket)))
         (push ticket ret)))
     ticket-lst)
    (with-temp-buffer 
      (erase-buffer)
      ;; XXX insert org-mode header here!
      (insert *weekly-ticket-report-orgmode-header*)
      (insert "\n* Open Tickets\n")
      (insert "| AIRS ticket # | Subject line of original request | User Name | Ticket Czar state |\n")
      (insert "|-+-+-+-|\n")
      (mapc
       (lambda (ticket)
         (insert (format "| %s | %s | %s | %s |\n" 
                         (ticket-id ticket) 
                         (ticket-subject ticket)
                         (ticket-user ticket)
                         (ticket-category ticket))))
       ret)
      
      (org-html-export-as-html)
      ))
  (with-buffer "*Org HTML Export*"
    (save-buffer))
  nil)

(defun* old-weekly-ticket-report (&optional (ticket-table "") (cutoff ""))
  (interactive "*sTicket Table: \nsCutoff(1000): \n")

  (let ((ticket-table (if (not (equal "" ticket-table))
                          ticket-table
                          "tickets"))
        (cutoff (if (not (equal "" cutoff)) cutoff 1000))
        (ticket-lst (slurp-table ticket-table))
        (categories (make-hash-table :test #'equal))
        (transitions (slurp-table "state-transitions"))
        (ret ())
        (buffer (get-buffer-create "weekly-ticket-report.org")))
    (mapc
     (lambda (ticket)
       (unless (or (equal *orgtbl-hline* ticket)
                   (equal 'hline ticket)
                   (equal "AIRS ticket #" (ticket-id ticket)))
         (let* ((state (ticket-category ticket))
                (state-order (nth 4 (assoc state transitions))))
           (when (and state-order (< state-order cutoff))
             (push (cons state-order ticket) ret)))))
     ticket-lst)
    (with-buffer buffer
      (erase-buffer)
      (let ((attention-tickets (mapcar #'cdr (sort (reverse ret) (lambda (x y) (< (car x) (car y)))))))
        ;; (insert "\n* Ticket Table\n")
        ;; (mapcar
        ;;  (lambda (ticket)
        ;;    (mapcar
        ;;     (lambda (item)
        ;;       (insert (format "| %s " item)))
        ;;     ticket)
        ;;    (insert (format "|\n")))
        ;;  attention-tickets)
        (insert "\n#+LATEX: \\eject\n")
        (insert "\n* Ticket Summary\n")
        (mapc
         (lambda (ticket)
           (insert-ticket-summary ticket))
         attention-tickets)
        (insert "\n#+LATEX: \\eject\n")
        (insert "\n* Full Threads\n")
        (mapc
         (lambda (ticket)
           ;; (insert (format "XXX ticket-info %s\n" (ticket-id ticket)))
           (insert-ticket-info ticket)
           (insert "\n\\eject\n"))
         attention-tickets)
        (save-excursion
          (goto-char 1)
          (replace-string "_" "\\_")))
      (save-excursion
        (goto-char 1)
        (insert *latex-header*))
      (save-buffer buffer)))

  nil)

(defun* ticket-report (match-func &optional (ticket-table "") (cutoff "") (output ""))
  (interactive "*aMatch Function: \nsTicket Table: \nsCuttoff: \nFOutput File: \n")
  (let* ((ticket-table (if (not (equal "" ticket-table))
                           ticket-table
                           "tickets"))
         (cutoff (if (not (equal "" cutoff)) cutoff 1000))
         (ticket-lst (slurp-table ticket-table))
         (categories (make-hash-table :test #'equal))
         (transitions (slurp-table "state-transitions"))
         (output-buffer-name (if (not (equal "" output)) output "ticket-report.org"))
         (ret ())
         (buffer (find-file output-buffer-name)))
    (mapc
     (lambda (ticket)
       (unless (or (equal *orgtbl-hline* ticket)
                   (equal 'hline ticket)
                   (equal "AIRS ticket #" (ticket-id ticket)))
         (let* ((*transitions* transitions)
                (matchedp (funcall match-func ticket)))
           (when matchedp
             (push (cons matchedp ticket) ret)))))
     ticket-lst)
    (with-buffer buffer
      (erase-buffer)
      (let ((attention-tickets (mapcar #'cdr (sort (reverse ret) (lambda (x y) (< (car x) (car y)))))))
        (insert "\n#+LATEX: \\eject\n")
        (insert "\n* Ticket Summary\n")
        (mapc
         (lambda (ticket)
           (insert-ticket-summary ticket))
         attention-tickets)
        (insert "\n#+LATEX: \\eject\n")
        (insert "\n* Full Threads\n")
        (mapc
         (lambda (ticket)
           ;; (insert (format "XXX ticket-info %s\n" (ticket-id ticket)))
           (insert-ticket-info ticket)
           (insert "\n\\eject\n"))
         attention-tickets)
        (save-excursion
          (goto-char 1)
          (replace-string "_" "\\_")))
      (save-excursion
        (goto-char 1)
        (insert *latex-header*))
      (save-buffer buffer)))
  nil)

(defmacro if-not-blank (var then &optional else)
  `(if (and ,var
        (not (equal "" ,var))
        (not (equal "nil" ,var)))
       ,then
       ,else
       ))

(defun insert-ticket-summary (ticket)
  "Insert the ticket summary for this ticket as per SRA:
   Ticket title
   Ticket # (AIRS)
   Initial date of submission
   Name of user
   Email of user
   Ticket owner
   Ticket status (as in your short report)"
  (insert (format "** Ticket ID: %s\n" (ticket-id ticket)))
  (insert (format "   - Subject: %s\n" (ticket-subject ticket)))
  (insert (format "   - Submission Date: %s\n" (ticket-opened ticket)))
  (insert (format "   - User Name: %s\n" (ticket-user ticket)))
  (insert (format "   - User Email: %s\n" (ticket-user-email ticket)))
  (insert (format "   - Ticket Owner: %s\n" (ticket-owner ticket)))
  (insert (format "   - Ticket Status: %s\n" (ticket-category ticket))))

(defun insert-ticket-info (ticket)
  (insert (format "** Ticket ID: %s\n" (ticket-id ticket)))
  (insert (format "*** Subject: %s\n" (ticket-subject ticket)))
  (let* ((tid (replace-regexp-in-string "\\[\\[.*\\]\\[\\(.*\\)\\]\\]" "\\1" (ticket-id ticket)))
         (info (all-ticket-information tid))
        )
    (destructuring-bind (&key owner user-info originator &allow-other-keys)
        info
      (destructuring-bind (&key name title institution employer department nsf-status contact-info projects)
          user-info
        (insert (format "*** Ticket Owner: %s\n" owner))
        (if-not-blank name
           (insert (format "*** User: %s\n" name))
           (insert (format "*** User: %s\n" originator)))
        (if-not-blank institution
             (insert (format "**** Institution: %s\n" institution)))
        (if-not-blank employer
             (insert (format "**** Employer: %s\n" employer)))
        (if-not-blank department
             (insert (format "**** Department: %s\n" department)))
        (if-not-blank nsf-status
             (insert (format "**** NSF-Status: %s\n" nsf-status)))
        (insert (format "**** Contact information for %s\n" (or name originator)))
        (mapc (lambda (contact)
                  (case (car contact)
                    (:email (insert (format  "***** Email: %s\n" (second contact))))
                    (:phone (insert (format  "***** Phone: %s\n" (second contact))))
                    (:mobile (insert (format "***** Cell: %s\n" (second contact))))
                    (:fax   (insert (format  "***** Fax: %s\n" (second contact))))))
                (or contact-info `((:email (format "%s" ,originator)))))
        (insert (format "*** All Ticket Items for ticket %s\n" tid))
        (insert-ticket-items-info-into-org-buffer tid 4)
        ))
    ))

(defun find-table (table-name-regex)
  (let ((point (point))
        (table-loc ())
        (table ()))
    (goto-char 0)
    (when (search-forward-regexp 
           (format "#\\+tblname: %s" table-name-regex) (point-max) t)
      (search-forward-regexp "^")
      (setf table-loc (point))
      (goto-char point))
    table-loc))

(defun find-results-table (table-name-regex)
  (warn "looking for results table: %s" table-name-regex)
  (let ((point (point))
        (table-loc ())
        (table ()))
    (goto-char 0)
    (when (and
           (or
            (search-forward-regexp 
             (format "#\\+call: ticket-categories(a=%s)" table-name-regex)
             ;; (format "#\\+call: %s" table-name-regex)
           (point-max) t)
            (error "could not find results table matching '#+call: ticket=categories(a=%s)" table-name-regex))
           (search-forward-regexp 
           ;; (format "#\\+RESULTS: %s" table-name-regex)
           (format "#\\+RESULTS:")
           (point-max) t))
    (search-forward-regexp "^")
    (setf table-loc (point))
    (goto-char point))
    table-loc))

(defun find-some-table (table-name-regex)
  (let ((point (point))
        (table-loc ())
        (table ()))
    (goto-char 0)
    (when (search-forward-regexp table-name-regex (point-max) t)
      ;; (search-forward table-name-regex)
      (search-forward-regexp "^")
      (setf table-loc (point))
      (goto-char point))
    table-loc))

(defun org-babel-read (cell &rest rest)  ;;; org-babel-read (cell)
  "Convert the string value of CELL to a number if appropriate.
Otherwise return it unmodified as a string.

This is taken almost directly from `org-babel-read' which is in turn
taken almoast directly from `org-read-prop'."
  (if (and (stringp cell) (not (equal cell "")))
      (or (org-babel-number-p cell)
          (progn (set-text-properties 0 (length cell) nil cell) cell))
    cell))

(defun slurp-table (table-name-regex &optional type)
  (unless type (setf type :standard))
  (unless (member type '(:standard :none :results))
    (error "unknown type for table: %s" type))
  (let ((point (point))
        (table ())
        (table-loc 
         (cond ((eql type :none) (find-some-table table-name-regex))
           ((or (eql type :standard) (not type)) (find-table table-name-regex))
               ((eql type :results) (find-results-table table-name-regex))
               (t (error "unknown table type")))))
    (if (not table-loc)
        (warn (format "table %s not found in buffer: %s!" table-name-regex (current-buffer)))
        (progn
          (goto-char table-loc)
          (search-forward-regexp "^|")
          (setf table (org-babel-read-table))
          (goto-char point)
          ))
    ;; (remove 'hline table)
    table))

(defmacro slurp-owners-table ()
  `(slurp-table "ticket-owners"))

(defun first-match (test list)
  (when list
    (if (funcall test (car list))
        (car list)
        (first-match test (cdr list)))))


(defun next-state (current)
  (let ((transitions (slurp-table "state-transitions")))
    (nth 2 (first-match
            (lambda (transition) 
              (and (listp transition)
                   (equal current (car transition))))
            transitions))))

(defun previous-state (current)
  (let ((transitions (slurp-table "state-transitions")))
    (nth 0 (first-match
            (lambda (transition) 
              (and (listp transition)
                   (equal current (nth 2 transition))))
            transitions))))

(defun org-ticket-next-state ()
  (interactive)
  (changing-state
   (let* ((current (replace-regexp-in-string "\\(^\s+\\|\s+$\\)" "" 
                                             (org-table-get-field)))
          (next (next-state current)))
     (when next
       (org-table-blank-field)
       (insert
        (format "%s" (next-state current)))))))

(defun org-ticket-previous-state ()
  (interactive)
  (changing-state
   (let* ((current (replace-regexp-in-string "\\(^\s+\\|\s+$\\)" "" 
                                            (org-table-get-field)))
         (prev (previous-state current)))
    (when prev
      (org-table-blank-field)
      (insert
       (format "%s" (previous-state current)))))))

(defun org-ticket-closed ()
  (interactive)
  (changing-state
   (org-table-blank-field)
   (insert "closed")))

(defun org-ticket-responded ()
  (interactive)
  (changing-state
   (org-table-blank-field)
   (insert "responded")))

(defun find-nth (item list &optional count)
  (when list
    (unless count (setf count 0))
    (if (eql item (car list))
        count
        (find-nth item (cdr list) (incf count)))))

(defvar *org-ticket-default-next-state* nil "nil for no default - 
current for current state - next for next state (in the transitions table)")

(defun org-ticket-change-state ()
  (interactive)
  (changing-state
   (let* ((current 
           (replace-regexp-in-string "\\(^\s+\\|\s+$\\)" "" 
                                     (org-table-get-field)))
          (all-states 
           (remove () 
                   (mapcar (lambda (x) (if (listp x) (nth 0 x)))
                           (cddr (slurp-table
                                  "state-transitions")))))
          (new-state 
           ;; pick the next state, use all the states in the state table for 
           ;; completions and "history"
           (org-completing-read
            "Next State?: " all-states () () 
            (cond 
              ((eql 'current *org-ticket-default-next-state*) 
               current)
              ((eql 'next *org-ticket-default-next-state*) 
               (next-state current))
              (t nil))
            (cons 'all-states 
                  (or (find-nth current all-states) 0))
            current)))
     (when new-state
       (org-table-blank-field)
       (insert
        (format "%s" new-state))))))

(defun org-ticket-change-owner ()
  (interactive)
  (changing-owner
   (let* ((current (org-table-get-field))
          (all-owners
           (mapcar (lambda (x) (if (listp x) (nth 0 x)))
                   (cddr (slurp-table "ticket-owners"))))
          (new-owner
           ;; pick the next state, use all the owners in the owners table for 
           ;; completions and "history"
           (org-completing-read
            "New Owner?: " all-owners () ()
            (cond 
              ((eql 'current *org-ticket-default-next-state*) 
               current)
              ((eql 'next *org-ticket-default-next-state*) 
               (next-state current))
              (t nil))
            (cons 'all-owners
                  (or (find-nth current all-owners) 0))
            current)))
     (when new-owner
       (org-table-blank-field)
       (insert
        (format "%s" new-owner))))))

(defun ticket-update (old new)
  (let ((info (make-hash-table :test #'equal))
        (owners (slurp-owners-table))
        (transitions (slurp-table "state-transitions"))
        (ret ()))
    (mapc
     (lambda (ticket)
       (if (listp ticket)
           (setf (gethash (ticket-id ticket) info) ticket)))
     old)
    (let ((res (mapc
                (lambda (ticket)
                  (when (listp ticket)
                    (when (gethash (ticket-id ticket) info)
                      (setf (ticket-category ticket) 
                            (ticket-category 
                             (gethash (ticket-id ticket) info)))
                      (setf (ticket-who ticket) 
                            (ticket-who (gethash (ticket-id ticket) info))))
                    (when (not (ticket-category ticket))
                      (setf (ticket-category ticket) *default-state*))
                    (let ((next 
                           (car (find-matches 
                                 (lambda (transition) 
                                   (and (listp transition)
                                        (equal (ticket-category ticket)
                                               (car transition))))
                                 transitions))))
                      (when (and next 
                                 (string-match "<.*>" 
                                               (ticket-last-entry ticket))
                                 (not (or (equal (nth 1 next) 1.0e+INF)
                                          (equal (nth 1 next) "1.0e+INF")))
                                 (> (* -1 (nth 1 next))
                                    (work-days (ticket-last-entry ticket))))
                        (setf (ticket-category ticket) (nth 2 next)))))
                  (if (listp ticket)
                      (mapc (lambda (y) (format "%s" y)) ticket)
                      ticket))
                new)))
      res)))

(defun new-tickets (tickets)
  (let ((list ()))
    (mapc (lambda (ticket)
            (when (and (listp ticket)
                       (string-match "<.*>" (ticket-opened ticket))
                       (< -1 (work-days (ticket-opened ticket))))
              (warn (format "days: %s time: %s" (work-days (ticket-opened ticket)) (ticket-opened ticket)))
              (push (ticket-id ticket) list)))
          tickets)
    (list (list "Count" "Tickets")
          (list (length list)           
                (replace-regexp-in-string "\\((\\|)\\)" ""
                                          (format "%s" (nreverse list)))))))

(defun list-from-csv (file)
  (let ((beg (point))
        (pm (point-max))
        new)
    ;; read in the new tickets
    (insert-file-contents file)
    (goto-char beg)
    ;; convert to a table
    (org-ticket-convert-region beg (+ (point) (- (point-max) pm)) pm)
    (goto-char beg)
    ;; read in the table
    (setf new (org-babel-read-table))
    ;; remove it
    (kill-region (point) (+ (point) (- (point-max) pm)))
    new))

(defun datestring-calendar-date (date-string)
  (string-match "\\([0-9][0-9][0-9][0-9]\\)-?\\([0-9][0-9]\\)-?\\([0-9][0-9]\\)" date-string)
  (list (car (read-from-string (match-string 2 date-string)))
        (car (read-from-string (match-string 3 date-string)))
        (car (read-from-string (match-string 1 date-string)))))

(defvar *org-ticket-already-imported* (make-hash-table :test #'equal))

(defun org-ticket-import (file &optional today separator)
  "Import ticket FILE as a table.
The file is assumed to be tab-separated.  Such files can be produced by most
spreadsheet and database applications.  If no tabs (at least one per line)
are found, lines will be split on whitespace into fields."
  (interactive "*f\nP")
  (let* ((base-name (file-name-sans-extension (file-name-nondirectory file)))
         (date (or today (datestring-calendar-date base-name))))
    (save-excursion
      (or (switch-to-buffer (find-buffer-visiting "tickets.org"))
          (switch-to-buffer (find-file (format "%s/work/HPCERC/org-tickets/tickets.org" (getenv "HOME")))))
      (unless 
          (save-excursion
            (goto-char 0)
            (search-forward-regexp (format " %s" (calendar-date-string date)) (point-max) t)
            )
        (let* (
               (tblname (format "#+tblname: tickets%s\n" base-name))
               old new result 
               (start (point))
               (daily-loc ()))
          (or (and (find-buffer-visiting "tickets.org")
                   (switch-to-buffer (find-buffer-visiting "tickets.org")))
              (find-file (format "%s/work/HPCERC/org-tickets/tickets.org" (getenv "HOME")))
              (error "couldn't find tickets.org buffer!"))

          (with-current-buffer "tickets.org"
            (goto-char 0)
            (search-forward-regexp "^\\* Daily")
            (show-children 1)
            (or (bolp) (newline))
            (setf daily-loc (point))
            ;; add a new line for today's date
            (insert (format "** %s\n" (calendar-date-string date)))
            (let ((beg (point))
                  (pm (point-max)))
              ;; read in new tickets file
              (setf new (list-from-csv file))
              ;; read in the new tickets
              (insert-file-contents file)

              (goto-char beg)
              ;; convert to a table
              (org-ticket-convert-region beg (+ (point) (- (point-max) pm)) separator)
              (goto-char beg)
              ;; read in the table
              (setf new (org-babel-read-table))
              ;; remove it
              (kill-region (point) (+ (point) (- (point-max) pm)))
              
              ;; read in old tickets table
              (setf old (slurp-table "tickets[0-9]+"))
              ;; merge the old and new tickets
              (setf result (ticket-update old new))
              (let ((beg (point))
                    (pm (point-max)))
                ;; add the tickets table
                (insert "*** tickets\n")
                (insert tblname)
                (insert
                 (orgtbl-to-orgtbl  
                  result
                  '((:lfmt "%s") (:fmt "%s"))))
                (org-table-align)
                (goto-char (+ beg (- (point-max) pm)))
                (newline)
                (let ((cat-beg (point))
                      (cat-pm (point-max)))
                  ;; add the categories table
                  (insert "*** categories\n")
                  (insert (format "#+call: ticket-categories(a=tickets%s)" base-name))
                  (org-ctrl-c-ctrl-c)
                  (goto-char cat-beg)
                  (goto-char (+ 1 cat-beg (- (point-max) cat-pm)))
                  (newline 2))
                (goto-char daily-loc)
                (org-cycle)
                (goto-char daily-loc)))))))))


(defun org-ticket-convert-region (beg0 end0 &optional separator)
  "Convert region to a table.
The region goes from BEG0 to END0, but these borders will be moved
slightly, to make sure a beginning of line in the first line is included.

SEPARATOR specifies the field separator in the lines.  It can have the
following values:

'(4)     Use the comma as a field separator
'(16)    Use a TAB as field separator
integer  When a number, use that many spaces as field separator
nil      When nil, the command tries to be smart and figure out the
         separator in the following way:
         - when each line contains a TAB, assume TAB-separated material
         - when each line contains a comma, assume CSV material
         - else, assume one or more SPACE characters as separator."
  (interactive "rP")
  (let* ((beg (min beg0 end0))
	 (end (max beg0 end0))
	 re)
    (goto-char beg)
    (beginning-of-line 1)
    (setq beg (move-marker (make-marker) (point)))
    (goto-char end)
    (if (bolp) (backward-char 1) (end-of-line 1))
    (setq end (move-marker (make-marker) (point)))
    ;; Get the right field separator
    (unless separator
      (goto-char beg)
      (setq separator
	    (cond
              ((not (re-search-forward "^[^\n\t]+$" end t)) '(16))
              ((not (re-search-forward "^[^\n,]+$" end t)) '(4))
              (t 1))))
    (goto-char beg)
    (if (equal separator '(4))
	(while (<= (point) end)
	  ;; parse the csv stuff
	  (cond
            ((looking-at "^") (insert "| "))
            ((looking-at "[ \t]*$") (replace-match " |") (beginning-of-line 2))
            ((looking-at "[ \t]*\"\\([^\"\n]*\\)\"")
	    (replace-match "\\1")
	    (if (looking-at "\"") (insert "\"")))
	   ((looking-at "[^,\n]+") (goto-char (match-end 0)))
	   ((looking-at "[ \t]*,") (replace-match " | "))
	   (t (beginning-of-line 2))))
        (setq re (cond
                   ((equal separator '(4)) "^\\|\"?[ \t]*,[ \t]*\"?")
                   ((equal separator '(16)) "^\\|\t")
                   ((integerp separator)
                    (progn
                      ; (format "^ *\\| *\t *\\| \\{%d,\\}" separator)
                      (format "^ *\\| *\t *")))
                   (t (error "This should not happen")))))
    (while (re-search-forward re end t)
      (replace-match "| " t t))
    (goto-char beg)
    (while (re-search-forward "^| *\\([0-9]+\\)" end t)
      (replace-match "| [[https://www.carc.unm.edu/irs/view_ticket.html?ticket_id=\\1][\\1]]"))
    (goto-char beg)
    (while (re-search-forward "| *\\([0-9]*-[0-9]*-[0-9]* [0-9]*:[0-9]*:[0-9]*\\)" end t)
      (replace-match "| <\\1>"))
    (goto-char beg)
    (while (re-search-forward "'" end t)
      (replace-match "\\\\'"))
    (goto-char beg)
    (org-table-align)
    (re-search-forward "| *$")
    ;; add two extra columns for state and who
    (insert "State")
    (org-table-insert-column)
    (insert "Who")
    (org-table-insert-hline)
    (goto-char beg)
    (org-table-align)))

(defun dow-test (day-of-week)
  (cond ((> 3 day-of-week)
         'new)
        (t 'old)))

(defun work-days (timestamp &optional today)
  (let* ((today (or today (calendar-current-date)))
         (day-of-week (calendar-day-of-week today))
         (days-passed (org-time-stamp-to-now timestamp)))
    (when (> 3 day-of-week)
      (incf days-passed (- 3 day-of-week)))
    days-passed))

(defun find-matches (test list &optional result)
  (if list
        (find-matches test (cdr list) 
                (if (funcall test (car list))
                    (cons (car list) result)
                    result))
        (nreverse result)))

(defun owner-short-name (full-name)
  "Convert the owner's full name to their short name.  
eg. 'James Prewett' -> 'Jim'"
  (let* ((owners (slurp-owners-table))
         (owner-info (assoc full-name owners)))
    (nth 1 owner-info)))

(defun update-ticket-after-mail (buffer point)
  (switch-to-buffer buffer)
  (goto-char point)
  (org-ticket-next-state))

(defun org-ticket-please-update ()    
  (interactive)
  (org-beginning-of-line)
  (let ((point (point))
        (buffer (current-buffer))
        (tickets (slurp-table "tickets[0-9]+"))
        (id (progn (search-forward-regexp "\\[\\([0-9]+\\)\\]") (match-string 1)))
        ret)
    
    (dolist (item tickets)
        (when (and (listp item) (posix-string-match (format "\\[%s\\]" id) (ticket-id item)))
          (setf ret item)))
    (if ret
        (let* ((ticket ret)
               (owners (slurp-owners-table))
               (owner (ticket-owner ticket))
               (owner-info (assoc owner owners))
               (email (nth 2 owner-info))
               (name (nth 1 owner-info)))
          (compose-mail email
                        (format "%s, Please update ticket %s - RE: %s" 
                                       name
                                       id  
                                       (ticket-mail-subject id))
                        
                         '(("CC" "help@carc.unm.edu"))
                         nil
                        (lambda (x) (switch-to-buffer x))
                        nil
                        `((update-ticket-after-mail . (,buffer ,point))))
          (insert (format "%s,\nCan I please get you to update ticket %s?\n\nLink: %s\n\nThanks,\n%s\n" name id (link-link (ticket-id ticket)) (user-full-name))))
        (error "no ticket found! in buffer %s" buffer))))

(defun org-ticket-redflag-warning ()    
  (interactive)
  (org-beginning-of-line)
  (let ((tickets (slurp-table "tickets[0-9]+"))
        (id (progn (search-forward-regexp "\\[\\([0-9]+\\)\\]") (match-string 1)))
        ret)
    (dolist (item tickets)
        (when (and (listp item) (posix-string-match (format "\\[%s\\]" id) (ticket-id item)))
          (setf ret item)))
    (if ret
        (let* ((ticket ret)
               (owners (slurp-owners-table))
               (owner (ticket-owner ticket))
               (owner-info (assoc owner owners))
               (email (nth 2 owner-info))
               (name (nth 1 owner-info)))
          (compose-mail email (format "%s, RF WARNING Please update ticket %s - RE: %s" 
                                      name
                                      id  
                                      (ticket-mail-subject id)) 
                        '(("CC" "help@carc.unm.edu"))
                        nil
                        (lambda (x) (switch-to-buffer x)))
          (insert (format "%s,\nCan I please get you to update ticket %s?  I'm supposed to RedFlag it today!!!\n\nLink: %s\n\nThanks,\n%s\n" name id (link-link (ticket-id ticket)) (user-full-name)))
          )
        (error "no ticket found!"))))

(defun ticket-list (id)
  (block ()
    (let ((tickets (slurp-table "tickets[0-9]+"))
        ret)
    (dolist (item tickets)
      (when (and (listp item) (posix-string-match (format "\\[%s\\]" id) (ticket-id item)))
        (setf ret item)
        (return item)))
    ret)))

(defun ticket-mail-subject (id)
  (let* ((ticket (ticket-list id))
         (opened (replace-regexp-in-string 
                  "[<>]" ""
                  (ticket-opened ticket)))
         (ticket-md5 (substring (md5 (format "%s%s" id opened))
                             0 6)))
    (format "%s (AIRS-Ticket-Id: %s/%s)" 
            (ticket-subject ticket) id ticket-md5 )))


(defun* org-tickets-current-date (&key date)
  "Return a string representing today's date"
  (let ((today (or date (calendar-current-date))))
    (format "%d%02d%02d" (nth 2 today) (nth 0 today) (nth 1 today))))

(defun* org-ticket-report-PDF (&key list-ticket-ids keep-links keep-state-column latex-only open date)
  "create a PDF daily report of the ticket categories"
  (interactive)
  (with-tickets-dot-org
  (let* ((today (or date (org-tickets-current-date)))
         (name (format "ticket-categories(a=tickets%s)" (org-tickets-current-date)))
         (org-filename (format "daily-report.org"))
         (LaTeX-filename (format "daily-report.tex"))
         (PDF-filename (format "daily-report.tex"))
         ;; no titles for this report
         (org-export-latex-title-command "")
         table)
    (switch-to-buffer (find-buffer-visiting "tickets.org"))
    (if (setf table (slurp-table name))
        (progn
          (warn "couldn't find table!  Attempting to import... ")
          (org-ticket-import 
           (cond ((file-exists-p 
                   (format "/home/irs/TICKET_SUMMARIES/%s.csv" today))
                  (format "/home/irs/TICKET_SUMMARIES/%s.csv" today))
                 ((file-exists-p 
                   (format "/Users/dl/work/HPCERC/org-tickets/%s.csv" today))
                  (format "/Users/dl/work/HPCERC/org-tickets/%s.csv" today))))
          (goto-char 0)
          (switch-to-buffer (find-buffer-visiting "tickets.org"))
          (or (setf table (slurp-table name))
              (error "still couldn't make the darn table... bailing"))))
    (switch-to-buffer org-filename)
    (goto-char 1)
    (save-excursion 
      (insert "Previous contents:\n\n")
      (org-mode)
      (comment-region 1 (point-max))
      )
    (mapc (lambda (category-entry)
              (if (eql category-entry 'hline)
                  (insert (format "|-|-|-|\n"))
                  (when (listp category-entry)
                    (insert "| ")
                    (mapc (lambda (x) 
                            (insert (format "%s |" x))) 
                          category-entry)
                    (insert "\n"))))
          (mapcar (lambda (entry)
                    (let ((e2 (my-copy-list entry)))
                      (unless keep-state-column
                        (unless (equal "Total:" (nth 1 e2))
                        (setf (nth 0 e2) "")))
                      (if list-ticket-ids
                          e2
                          (progn
                            (setf (cdddr e2) nil)
                            e2))))
                   table))
    (goto-char 2)
    (org-ctrl-c-ctrl-c)
    (goto-char 0)
    (insert "#+LaTeX_CLASS: report\n#+LATEX_HEADER: \\pagestyle{empty}\n")
    (insert "#+LATEX_HEADER: \\renewcommand\\maketitle{}\n#+LATEX_HEADER: \\renewcommand\\tableofcontents{}\n")
    (text-mode)
    ;; kill off the links
    (if keep-links
        (my-replace-regexp "\\[\\[\\([0-9]+\\)\\]\\]" "[[https://www.carc.unm.edu/irs/view_ticket.html?ticket_id=\\1][\\1]]")
        (my-replace-regexp "\\(\\[\\[\\|\\]\\]\\)" ""))
    (org-mode)
    (write-file org-filename)
    (cond ((and (not latex-only) open)
           (org-export-as-pdf-and-open 0))
          ((not latex-only)
           (org-export-as-pdf 0))
          (open
           (org-export-as-latex 0))
          (t
           (org-export-as-latex 0)
           ))
    (kill-buffer org-filename)
    (if (and open latex-only)
        (switch-to-buffer LaTeX-filename)
        (kill-buffer LaTeX-filename)))))

(defun* org-ticket-report-ascii (&key list-ticket-ids keep-links keep-state-column date open)
  "create an ASCII daily report of the ticket categories"
  (interactive)
  (warn (format "making ascii report for date: %s" (or date (org-tickets-current-date))))
  (let* ((date (org-tickets-current-date :date date))
         (today (or date (org-tickets-current-date)))
         ;; (name-foo (format "ticket-categories(a=tickets%s)" today))
         (name-foo (format "ticket-categories"))
         (name (format "tickets%s" today))
         (org-filename (format "daily-report.org"))
         (ascii-filename (format "daily-report.txt"))
         table)
    (with-tickets-dot-org
        (if (setf table (slurp-table name :results))
            nil
            (progn
              (warn "couldn't find table!  Attempting to import... ")
              (org-ticket-import 
               (cond ((file-exists-p 
                       (format "/home/irs/TICKET_SUMMARIES/%s.csv" today))
                      (format "/home/irs/TICKET_SUMMARIES/%s.csv" today))
                     ((file-exists-p 
                       (format "/Users/dl/work/HPCERC/org-tickets/%s.csv" today))
                      (format "/Users/dl/work/HPCERC/org-tickets/%s.csv" today))))
              (goto-char 0)
              (or (setf table (slurp-table name))
                  (error "still couldn't make the darn table... bailing"))))
      (switch-to-buffer org-filename)
      (kill-region 1 (point-max))
      (goto-char 1)
      (org-mode)
      (mapc (lambda (category-entry)
              (if (eql category-entry 'hline)
                  (insert (format "|-|-|-|\n"))
                  (when (listp category-entry)
                    (insert "| ")
                    (mapc (lambda (x) 
                            (insert (format "%s |" x))) 
                          category-entry)
                    (insert "\n"))))
            (mapcar (lambda (entry)
                      (if (eql entry 'hline)
                          entry
                          (let ((e2 (my-copy-list entry)))
                            (unless keep-state-column
                              (unless (equal "Total:" (nth 1 e2))
                                (setf (nth 0 e2) "")))
                            (if list-ticket-ids
                                e2
                            (progn
                              (setf (cdddr e2) nil)
                              e2))))
                      )
                    table))
      (goto-char 2)
      (org-ctrl-c-ctrl-c)
      (goto-char 0)
      (text-mode)
      ;; kill off the links
      (if keep-links
          (my-replace-regexp "\\[\\[\\([0-9]+\\)\\]\\]" "[[https://www.carc.unm.edu/irs/view_ticket.html?ticket_id=\\1][\\1]]")
          (my-replace-regexp "\\(\\[\\[\\|\\]\\]\\)" ""))
      (org-mode)
      (write-file org-filename)
      (write-file ascii-filename))
    (if open
        (switch-to-buffer ascii-filename))))

;; probably could be implemented a little better :P
(defun* org-ticket-email-report (&key date)
  (interactive)
  (org-ticket-report-ascii :date date)
  (compose-mail (format "%s" director-email) 
                (format "Daily Ticket Report %s" 
                        (calendar-date-string 
                         (or date (calendar-current-date)))) 
                '(("CC" . "download@carc.unm.edu,systems@carc.unm.edu")) nil (lambda (x) (switch-to-buffer x)))
  (insert
   (format "Here is the daily ticket report for %s: \n\n"
           (calendar-date-string (or date (calendar-current-date)))))
  (insert-file-contents "daily-report.txt"))


(defun read-time (&optional time)
  (datestring-calendar-date (org-read-date () () () () (current-time))))

(defun* org-ticket-daily-report-export (&key date)
  "Dispatcher for the daily ticket report"
  (interactive)
  (let ((date (or date (calendar-current-date))))
    (or (switch-to-buffer (find-buffer-visiting "tickets.org"))
        (and
         (or (find-file (format "tickets.org"))
             (find-file (format "%s/work/HPCERC/org-tickets/tickets.org" (getenv "HOME"))))
         (switch-to-buffer (find-buffer-visiting "tickets.org")))
        (error "couldn't find tickets.org!"))

    (let* (subtree-p
           (help-buffer "*Org Tickets Daily Export Help*")
           (help 
            (format "Org-Tickets Daily Export Help
\[d]   set date of export (currently: %s)
\[e]   email table to the Director

\[a]   produce an ASCII file \[b]  keeping all state info
\[c]   listing ticket IDs    \[d]  preserving link information (more interesting with PDF!)

\[l]   produce a LaTeX file  \[m]  keeping all state info
\[n]   listing ticket IDs    \[o]  preserving link information
\[L,M,N,O] ...and produce a PDF file"
                    (calendar-date-string (or date (calendar-current-date)))))
           ;; list-ticket-ids keep-links keep-state-column open date
           (cmds
            '((?d (lambda () (interactive) (org-ticket-daily-report-export :date (read-time))))
              (?e (lambda () (interactive) (org-ticket-email-report :date date)) t)
              (?a (lambda () (interactive) (org-ticket-report-ascii :date date)) t)
              (?b (lambda () (interactive) (org-ticket-report-ascii :keep-state-column t :open t :date date)) t)
              (?c (lambda () (interactive) (org-ticket-report-ascii :list-ticket-ids t :keep-state-column t :open t :date date)) t)
              (?d (lambda () (interactive) (org-ticket-report-ascii :list-ticket-ids t :keep-links t :keep-state-column t :open t :date date)) t)
              (?l org-ticket-report-PDF nil)
              (?m (lambda () (interactive) (org-ticket-report-PDF :list-ticket-ids nil :keep-links nil :keep-state-column t :latex-only nil :open t :date date)) t)
              (?n (lambda () (interactive) (org-ticket-report-PDF :list-ticket-ids t :keep-links nil :keep-state-column t :latex-only nil :open t :date date)) t)
              (?o (lambda () (interactive) (org-ticket-report-PDF :list-ticket-ids t :keep-links t :keep-state-column t :latex-only nil :open t :date date)) t)
              (?L (lambda () (interactive) (org-ticket-report-PDF :list-ticket-ids nil :keep-links nil :keep-state-column nil :latex-only t :open t :date date)) t)
              (?M (lambda () (interactive) (org-ticket-report-PDF :list-ticket-ids nil :keep-links nil :keep-state-column t :latex-only t :open t :date date)) t)
              (?N (lambda () (interactive) (org-ticket-report-PDF :list-ticket-ids t :keep-links nil :keep-state-column t :latex-only t :open t :date date)) t)
              (?O (lambda () (interactive) (org-ticket-report-PDF :list-ticket-ids t :keep-links t :keep-state-column t :latex-only t :open t :date date)) t)))
           r1 r2 ass
           (cpos (point)) (cbuf (current-buffer)) bpos)
      (save-excursion
        (save-window-excursion
          (delete-other-windows)
          (with-output-to-temp-buffer help-buffer
            (princ help))
          (org-fit-window-to-buffer (get-buffer-window help-buffer))
          (message "Select command: ")
          (setq r1 (read-char-exclusive))))
      (and bpos (goto-char bpos))
      (setq r2 (if (< r1 27) (+ r1 96) r1))
      (unless (setq ass (assq r2 cmds))
        (error "No command associated with key %c" r1))
      (if subtree-p (progn (outline-mark-subtree) (activate-mark)))
      (call-interactively (nth 1 ass))
      (when (and bpos (get-buffer-window cbuf))
	(let ((cw (selected-window)))
	  (select-window (get-buffer-window cbuf))
	  (goto-char cpos)
	  (deactivate-mark)
	  (select-window cw))))))

;; the column that holds the czar state
(defvar *czar-state* 9)

(defun push-state-string (string state)
  (let ((state-list (cdar (read-from-string string))))
    (format "%S" (append (list :Previous: state) state-list))))

(defun pop-state-string (string)
  (let ((state-list (cdar (read-from-string string))))
    (if (cdr state-list)
        (format "%S" (append (list :Previous:) (cdr state-list)))
        "")))

(defun org-ticket-push-state (new-state)
  (let ((current-column (org-table-current-column)))
    (save-excursion 
    (org-table-goto-column *czar-state*)
    (let* ((p (point))
           (start (search-backward "|"))
           (end (search-forward "|" nil nil 2))
           (current (progn
                      (goto-char p)
                      (org-trim (org-table-get-field))))
           (old (get-text-property p 'state-stack))
           (new (cons current old)))
      (set-text-properties 0 (length current) nil current)
      (warn (format "p: %s s: %s e: %s c: %s o: %s n: %s"
                     p start end current old new))
      (save-excursion
        (org-table-blank-field)
        (insert new-state)
        )
      (add-text-properties 
       start end `(state-stack ,new))))
    (org-table-align)
    (org-table-goto-column current-column)))

(defvar org-ticket-previous-states-regexp
  "|.* \\((:PREVIOUS: .*)\\)"
  "A regular expression to match previous states entries in ticket tables")

;; not quite what I want :P
(defun org-tickets-hide-previous-states (&optional limit)
  "Run through the buffer and add overlays to previous states."
  (interactive)
  (unless limit (setf limit (point-max)))
  (save-excursion
    (goto-char 1)
    (while (re-search-forward org-ticket-previous-states-regexp limit t)
        (let* ((help (concat "Previous: "
                             (org-match-string-no-properties 1)))
               ;; FIXME: above we should remove the escapes.
               ;; but that requires another match, protecting match data,
               ;; a lot of overhead for font-lock.
               (ip (org-maybe-intangible
                    (list 'invisible 'org-ticket-previous))))
          (add-text-properties (match-beginning 1) (match-end 1)  
                               '(invisible t
                                 org-dwidth t org-dwidth-n 0
                                 org-cwidth t org-cwidth-n 0 
                                 foo baz))
          (org-rear-nonsticky-at (match-beginning 1))
          (org-rear-nonsticky-at (match-end 1))
          t))))

(setq org-font-lock-hook 'org-tickets-hide-previous-states)
(setq org-font-lock-hook nil)


(setf org-confirm-babel-evaluate nil)


(require 'html-parse)


(eval-when-compile (defvar *my-gensym-counter* 0))

(defmacro with-buffer (buffer &rest body)
  "evaluate body in the buffer named, then return to the original buffer"
  `(save-window-excursion 
     (switch-to-buffer ,buffer)
     (progn ,@body)))

(defun map-leaves (function tree &optional ret)
  "Apply function to all leaves of the tree"
  (cond ((and (listp tree)
              (null tree))
         (append ret tree))
        ((listp tree)
         (cons
          (map-leaves function (car tree) ret)
          (map-leaves function (cdr tree))))
        ((atom tree)
         (append (nreverse ret)
                 (funcall function tree)))))

(defun strip-formatting (tree)
  (map-leaves 
   (lambda (leaf)
     (remove-text-properties 0 (length leaf) '(face nil fontified nil) leaf)
     leaf)
   tree))

(defun extract-ticket-items (buffer)
  "Pure Voodoo!"
  (let* ((rows (with-buffer
                    buffer
                  (goto-char (point-min))
                  (fix-html (current-buffer))
                  (goto-char (point-min))
                  (search-forward "<table>" (point-max) nil 3)
                  (beginning-of-line)
                  (cddr (read-xml)))))
    (mapcar (lambda (row)
              (let* ((iid (cadadr (third  row)))
                     (from (cadr (fourth row)))
                     (date (cadr (fifth row)))
                     (type (cadr (sixth row)))
                     (subject (cadr (seventh row))))
                (list iid from date type subject)))
            rows)))

(defcustom *AIRS-username* "" "AIRS Username"  :group 'org-tickets   :type '(string))
(defcustom *AIRS-password* "" "AIRS Password" :group 'org-tickets   :type '(string))

(defun get-ticket-items (ticket-id)
  "Return a list of ticket items for a ticket"
  (let ((url (format "https://www.carc.unm.edu/irs/view_ticket.html?ticket_id=%s" ticket-id)))
    (with-temp-buffer
      (w3m-retrieve url nil nil 
                    (format "auth_user=%s&auth_password=%s&wcommand=LOGIN"
                            *AIRS-username*
                            *AIRS-password*)
                    nil)
      (extract-ticket-items (current-buffer)))))

(defun get-ticket-item-ids (ticket-id)
  (mapcar #'car (get-ticket-items ticket-id)))

(defun extract-ticket-info (buffer)
  "More Voodoo!"
  (let* ((rows (with-buffer
                   buffer
                 (goto-char (point-min))
                 (fix-html (current-buffer))
                 (goto-char (point-min))
                 (search-forward "<table border=0>")
                 (beginning-of-line)
                 (cdr (fourth (read-xml)))))
         (ticket-id (cadr (first rows)))
         (created (cadr (second rows)))
         (closed (cadr (third rows)))
         (last-changed (cadr (fourth rows)))
         (last-changed-by (cadr (fifth rows)))
         (assigned (replace-regexp-in-string "\\\n +" "" (cadr (sixth rows))))
         (originator nil))
    (goto-char (point-min))
    (search-forward "iid")
    (end-of-line)
    (let* ((stuff (read-xml)))
      (list :ticket-id-and-checksum ticket-id 
            :created created 
            :closed closed 
            :last-changed last-changed 
            :last-changed-by last-changed-by 
            :assigned assigned
            :originator (cadr (fourth stuff))))))

(defun get-ticket-info (ticket-id)
  (with-temp-buffer
    (insert-ticket ticket-id)
    (extract-ticket-info (current-buffer))))

(defun insert-ticket (ticket-id)
  "Insert page for ticket into the current buffer"
  (interactive "*nTicket ID: ")
  (let ((url (format 
              "https://www.carc.unm.edu/irs/view_ticket.html?ticket_id=%s" 
              ticket-id)))
    (w3m-retrieve url  nil nil 
                  (format "auth_user=%s&auth_password=%s&wcommand=LOGIN"
                          *AIRS-username*
                          *AIRS-password*)
                  nil)))

(defun insert-ticket-item (ticket-id item-id)
  "Insert page for ticket item into the current buffer"
  (interactive "*nTicket ID: \nnItem ID: \n")
  (let ((url (format 
              "https://www.carc.unm.edu/irs/view_ticket_plain.html?ticket_id=%s&item_id=%s"
              ticket-id item-id)))
    (w3m-retrieve url  nil nil 
                  (format "auth_user=%s&auth_password=%s&wcommand=LOGIN"
                          *AIRS-username*
                          *AIRS-password*)
                  nil)
    (goto-char 1)
    ))

(defun fix-html (buffer)
  (my-replace-regexp "<\\(BR\\|P\\|p\\|HR\\|LI\\|DD\\)>" "<\\1/>")
  (my-replace-regexp "</p>" ""))

(defun find-message-type (&optional buffer)
  (interactive)
  (save-excursion
    (with-buffer (or buffer (current-buffer))
      (goto-char (point-min))
      (or (re-search-forward "<P/>\\(.*?\\) Message" nil t)
          (re-search-forward "<P/>\\(Log Entry\\)" nil nil))
      (match-string 1))))

(defun extract-item-info (buffer &optional type)
  "Extract information about a ticket item from a buffer determining the type of the message automagically unless type is specified"
  (fix-html buffer)
  (let* ((message-type 
          (or (cond ((or (equal type "")
                     (equal type "nil"))
                 nil)
                (t type))
              (find-message-type buffer)))
         (message-data 
          (let ((raw (save-excursion
                       (with-buffer
                           buffer
                         (goto-char (point-min))
                         (fix-html buffer)
                         (goto-char (point-min))
                         (search-forward "<table border=1>")
                         (beginning-of-line)
                         (read-xml)))))
            (setf *raw* raw)
            (cddr raw))))
    (values (cond ((equal "Email" message-type)
                   (extract-email-item-info (cdr *raw*)))
                  ((equal "Web" message-type)
                   (extract-web-item-info message-data))
                  ((equal "Log Entry" message-type)
                   (extract-log-item-info message-data))
                  (t 
                   (error "unknown message type: %s" message-type)))
            message-type)))

(defun extract-log-item-info (message-data)
  (ignore-errors
    (destructuring-bind ((a (b from) (c date)) d (e (f subject &optional g &rest message))) message-data
      `((:subject ,subject)
        (:from ,from)
        (:date ,date)
        (:message ,message)
        ))))

(defun extract-web-item-info (message-data)
  (destructuring-bind ((a (b (c subject))) d (e (f (g from)) (h first) (i last)) j (k (l entry-date)) m (n (o &rest message))) message-data
      `((:subject ,subject)
        (:from ,(substring from 2))
        (:first ,first)
        (:last ,last)
        (:entry-date ,entry-date)
        (:message ,message))))

;; (defun extract-email-item-info (message-data)
;;   (setf *message-data* message-data)
;;   (destructuring-bind ((a (b (c subject))) d (e e1 (f (g from)) (f1 (g1 to))) j  (k (l sent-date) (m &optional recvd-date)) m (n  (o &rest message) &rest foo) &rest rest)
;;       message-data
;;     `((:subject ,subject)
;;       (:from ,(substring from 2))
;;       (:to ,(substring to 2))
;;       (:sent-date ,sent-date)
;;       (:recvd-date ,(or sent-date recvd-date))
;;       (:message ,message)
;;       )))

(defun extract-email-item-info (message-data)
  (setf *message-data* message-data)
  (or
   (ignore-errors
     (destructuring-bind ((a &rest) (b (c (d subject))) e (f g (g1 (g2 from)) (h (i to))) j (k (l sent-date) (m recvd-date)) n (o (p (q &rest message)) &rest foo) &rest) *message-data*
         `(
          (:subject ,subject)
          (:from ,from)
          (:to ,to)
          (:sent-date ,sent-date)
          (:recvd-date ,recvd-date)
          (:message ,message))))
   (destructuring-bind ((a &rest) (b (c (d subject))) e (f g (g1 (g2 from)) (h (i to))) j (k (l sent-date) (m recvd-date)) n (o (p  &rest message) &rest foo) &rest) *message-data*
         `(
          (:subject ,subject)
          (:from ,from)
          (:to ,to)
          (:sent-date ,sent-date)
          (:recvd-date ,recvd-date)
          (:message ,message)))))

(defun decode-stuff-in-buffer (buffer)
  "replace &lt; and &gt; with < and > respectively in the buffer; remove ctrl-m"
  (interactive "BBuffer: \n")
  (with-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (my-replace-regexp "&lt;" "<")
      (goto-char (point-min))
      (my-replace-regexp "&gt;" ">")
      (goto-char (point-min))
      (my-replace-regexp "&amp;" "&")
      (goto-char (point-min))
      (my-replace-regexp "" "")
      (goto-char (point-min))
      (my-replace-regexp "\240\240" "* ")
      (goto-char (point-min))
      (my-replace-regexp "\240" "    "))))

;; (defun insert-item-info-into-buffer (ticket-id item-id &optional buffer)
;;   (interactive "*nTicket ID: \nnItem ID: \nBBuffer: \n")
;;   (save-window-excursion
;;     (with-temp-buffer
;;       (insert-ticket-item ticket-id item-id)
;;       (setf item-info (extract-item-info (current-buffer))))
;;     (with-buffer (or buffer (current-buffer))
;;       (insert 
;;        (format "Ticket: %s\nItem: %s\nSubject: %s\nType: %s\nFrom: %s\nTo: XXX\nMessage Body:\n"
;;                ticket-id item-id
;;                (cadr (assoc :subject item-info))
;;                (cadr (assoc :type item-info))
;;                (cadr (assoc :from item-info))))
;;       (mapc
;;        (lambda (x) 
;;          (insert 
;;           (cond ((and (listp x)
;;                       (or (equal x '("P"))
;;                           (equal x '("BR"))))
;;                  "\n")
;;                 ((listp x)
;;                  (or (cadr x) ""))
;;                 (t x))))
;;        (cadr (assoc :message item-info)))
;;       (decode-stuff-in-buffer (current-buffer)))))

(defun insert-item-info-into-buffer (ticket-id item-id &optional type buffer)
  (interactive "*nTicket ID: \nnItem ID: \nsType: \nbBuffer: \n")
  (let ((item-info nil)
        (item-type nil))
    (save-window-excursion
      (with-temp-buffer
        (insert-ticket-item ticket-id item-id)
        (multiple-value-bind (info type)
            (extract-item-info (current-buffer) type)
          (setf item-info info
                *item-info2* info
                item-type type)))
      (with-buffer (or buffer (current-buffer))
        (insert (format "Item ID: %s\n" item-id))
        (cond ((equal item-type "Web")
             (insert-web-item-info-into-buffer item-info buffer))
            ((equal item-type "Email")
             (insert-email-item-info-into-buffer item-info nil buffer))
            ((equal item-type "Log Entry")
             (insert-logentry-item-info-into-buffer item-info buffer)))
        ))))

(defun* insert-item-info-into-org-buffer (ticket-id item-id &optional (level 1) type buffer)
  (interactive "*nTicket ID: \nnItem ID: \nsType: \nbBuffer: \n")
  (let ((item-info nil)
        (item-type nil)
        (str (make-string level 42)))
    (save-window-excursion
      (with-temp-buffer
        (insert-ticket-item ticket-id item-id)
        (multiple-value-bind (info type)
            (extract-item-info (current-buffer) type)
          (setf item-info info
                item-type type)))
      (with-buffer (or buffer (current-buffer))
        (insert (format "%s Item ID: %s\n" str item-id))
        (insert "\n\\begin{verbatim}\n")
        (cond ((equal item-type "Web")
             (insert-web-item-info-into-buffer item-info buffer))
            ((equal item-type "Email")
             (insert-email-item-info-into-buffer item-info nil buffer))
            ((equal item-type "Log Entry")
             (insert-logentry-item-info-into-buffer item-info buffer)))
        (insert "\n\\end{verbatim}\n")))))

(defun insert-email-item-info-into-buffer (item-info &optional include-czar-entries buffer)
  (setf *item-info* item-info)
  (with-buffer (or buffer (current-buffer))
    (let ((subject (cadr (assoc :subject item-info)))
          (message (cadr (assoc :message item-info))))
      (cond 
        ((and (eql nil include-czar-entries)
              (or
               ;; current ticket czar updates
               (string-match "^\\w+, Please update ticket [0-9]+" subject)
               ;; old-style ticket czar updates
               (string-match "^Please update ticket [0-9]+" subject))
              ;;(string-match "\\w+,\\nCan I please get you to update ticket [0-9]+\\?")
              )
         (insert 
          (format "Ticket Czar \"please update\" message sent to %s\n" 
                  (cadr (assoc :to item-info))))
         (insert 
          (format "Sent Date: %s\n"                
                  (cadr (assoc :sent-date item-info)))))
        ((and (eql nil include-czar-entries)
              ;; czar RF warning
              (string-match 
               "^\\w+, RF WARNING Please update ticket [0-9]+" subject))
         (insert 
          (format "Ticket Czar \"REDFLAG WARNING\" message sent to %s\n" 
                  (cadr (assoc :to item-info))))
         (insert 
          (format "Sent Date: %s\n"                
                  (cadr (assoc :sent-date item-info)))))
        (t
         (insert
          (format 
           "AIRS Email Message:\nSent Date: %s\nReceived Date: %s\nFrom: %s\nTo: %s\nSubject: %s\n"
           (cadr (assoc :sent-date item-info))
           (cadr (assoc :recvd-date item-info))
           (cadr (assoc :from item-info))
           (cadr (assoc :to item-info))
           subject))
         (insert
          "Message: \n\n")
         (insert-item-message message)
         (insert "\n")
         (decode-stuff-in-buffer (current-buffer)))
        ))))

(defvar *newlines* '("P" "P/" "BR" "BR/"  "DT" "DD" "HR" "HR/"))
(defvar *ignores* '("PRE" "DL" "A"  "STRONG" "LI" "H1" "TR" "TH" "COLSPAN" "HREF" "ALIGN"))

(defun insert-item-message (message)
  (cond ((null message)
         (insert ""))
        ((and (listp message)
              (stringp (car message))
              (member (upcase (car message)) *ignores*))
         (insert-item-message (cdr message)))
        ((and (listp message)
              (stringp (car message))
              (member (upcase (car message)) *newlines*))
         (insert "\n")
         (insert-item-message (cdr message)))
        ((and (stringp message)
              (member (upcase message) *newlines*))
         (insert "\n"))
        ((and (stringp message)
              (member (upcase message) *ignores*))
         (insert ""))
        ((stringp message)
         (insert message))
        (t 
         (mapc
          (lambda (x)
            (unless (and (listp x) (member (car x) '("A")))
              (insert-item-message x)))
          message))))

(defun insert-logentry-item-info-into-buffer (item-info &optional buffer)
  (with-buffer (or buffer (current-buffer))
    (insert
     (format "AIRS Log Entry:\nDate: %s\nFrom: %s\nSubject: %s\n"
             (cadr (assoc :date item-info))
             (cadr (assoc :from item-info))
             (cadr (assoc :subject item-info))))
    (insert "Message:\n\n")
    (insert-item-message
       (cadr (assoc :message item-info)))
    (insert "\n")
    (decode-stuff-in-buffer (current-buffer))))

(defun insert-web-item-info-into-buffer (item-info &optional buffer)
  (with-buffer (or buffer (current-buffer))
    (insert
     (format "AIRS Web Entry:\nDate: %s\nFrom: %s %s (%s)\nSubject: %s\n"
             (cadr (assoc :entry-date item-info))
             (or (cadr (assoc :first item-info)) "")
             (or (cadr (assoc :last item-info)) "")
             (or (cadr (assoc :from item-info)) "")
             (or (cadr (assoc :subject item-info)) "")))
    (insert "Message:\n\n")
    (insert-item-message (cadr (assoc :message item-info)))
    (decode-stuff-in-buffer (current-buffer))))

(defun insert-ticket-items-info-into-buffer (ticket-id &optional buffer reverse)
  (interactive "*nTicket ID: \nbBuffer: \n")
  (save-window-excursion
    (with-buffer (or buffer (current-buffer))
      ;; (goto-char (point-max))
      (insert (format "Ticket ID: %s\n" ticket-id))
      (let ((item-ids
             (if reverse
                 (reverse (get-ticket-item-ids ticket-id))
                 (get-ticket-item-ids ticket-id))))
        (mapc (lambda (item-id)
                (ignore-errors
                  (insert-item-info-into-buffer ticket-id item-id nil (or buffer (current-buffer))))
                ;;(goto-char (point-max))
                  (insert "\n\n"))
                item-ids)))))

(defun* insert-ticket-items-info-into-org-buffer (ticket-id &optional (level 1)  buffer reverse)
  (interactive "*nTicket ID: \nbBuffer: \n")
  (let ((str (make-string level 42)))
    (save-window-excursion
    (with-buffer (or buffer (current-buffer))
      ;; (goto-char (point-max))
      (insert (format "%s Ticket ID: %s\n" str ticket-id))
      (let ((item-ids
             (if reverse
                 (reverse (get-ticket-item-ids ticket-id))
                 (get-ticket-item-ids ticket-id))))
        (mapc (lambda (item-id) 
                (ignore-errors
                  (insert-item-info-into-org-buffer ticket-id item-id level nil (or buffer (current-buffer))))
                ;;(goto-char (point-max))
                  (insert "\n\n"))
                item-ids))))))

(defun insert-lookup-user-by-email (email)
  "Insert page for user by email into the current buffer"
  (interactive "sEmail Address: ")
  (let ((url (format 
              "https://www.carc.unm.edu/irs/find_person_by_email.html?email_address=%s"
              (w3m-url-encode-string email))))
    (w3m-retrieve url  nil nil 
                  (format "auth_user=%s&auth_password=%s&wcommand=LOGIN"
                          *AIRS-username*
                          *AIRS-password*)
                  nil)))

(defun find-person-id-by-email (email)
  (interactive "sEmail Address: ")
  (ignore-errors
    (with-temp-buffer
    (insert-lookup-user-by-email email)
    (goto-char (point-min))
    (search-forward "<H1> Find Person by Email </H1>")
    (let* ((data (ignore-errors (cdr (cadddr (cadadr (read-xml))))))
           (person-info 
            (destructuring-bind (td ((a (href . url))(b name)))
                (car data)
              (list :url url :name name)))
           (institution
            (cadadr data)))
      (append person-info (list :institution institution)))
    )))

(defvar *base-url* "https://www.carc.unm.edu/irs/")

(defun lookup-person-info-by-email (email)
  (interactive "sEmail Address: ")
  (ignore-errors
    (progn
    (let ((person-info (find-person-id-by-email email)))
      (with-temp-buffer
        (let ((url (format "%s%s" *base-url* (cadr (member :url person-info))))
              name
              affiliation
              title
              institution
              employer
              department
              nsf-status
              contact-info
              contact ;; distilled contact-info
              projects
              )
          (w3m-retrieve url  nil nil 
                        (format "auth_user=%s&auth_password=%s&wcommand=LOGIN"
                                *AIRS-username*
                                *AIRS-password*)
                        nil)
          (goto-char (point-min))
          (search-forward "<TD><H1>")
          (beginning-of-line)
          (setf name (cadadr (read-xml)))
          (search-forward "<!--- BEGIN Affiliation TEMPLATE --->")
          (setf affiliation (read-xml))
          (setf title (cadr (third (second affiliation))))
          (setf institution (cadr (third (third affiliation))))
          (setf employer (cadr (third (fourth affiliation))))
          (setf department (cadr (third (fifth affiliation))))
          (setf nsf-status (cadr (third (sixth affiliation))))
          (search-forward "<!--- BEGIN Contact TEMPLATE --->")
          (setf contact-info (read-xml))
          (setf contact
                (mapcar 
                 (lambda (x) 
                   (cond ((equal (cadadr x) "Office") 
                          (list :phone (cadadr (cadadr (third x)))))
                         ((equal (cadadr x) "Fax") 
                          (list :fax (cadadr (cadadr (third x)))))
                         ((equal (cadadr x) "Home") 
                          (list :fax (cadadr (cadadr (third x)))))
                         ((equal (cadadr x) "Mobile")
                          (list :mobile (cadadr (cadadr (third x)))))
                         ((equal (cadadr x) "Email")
                          (list :email (cadr (cadadr (cadadr (third x))))))
                         (t
                          (list :email (and (listp (cadadr (third x))) (cadr (cadadr (cadadr (third x)))))))))
                 (cdddr contact-info)))
          (search-forward "<TITLE>Choose Project</TITLE>")
          (search-forward "<table>")
          (beginning-of-line)
          (setf projects
                (remove nil
                        (mapcar 
                         (lambda (x) 
                           (ignore-errors
                             (destructuring-bind (tr (td (a (b project-id &rest))) (td2 title) (td3 project-pi) &rest rest) x 
                               `(:project-id ,project-id 
                                             :title ,(substring title 6 (- (length title) 3))
                                             :project-pi ,(substring project-pi 0 (- (length project-pi) 5))))))
                         (read-xml))))
          (list :name name
                :title (or title "") :institution (or institution "")
                :employer (or employer "")
                :department (or department "")
                :nsf-status (or (and nsf-status
                                     (substring nsf-status 3 (- (length nsf-status) 3))) "")
                :contact-info contact
                :projects projects)))))))

(defun insert-person-info (person-info)
  (destructuring-bind (&key name title institution employer department nsf-status contact-info projects)
      person-info
      (insert (format "Name: %s\nInstitution: %s\nDepartment: %s\nNSF Status: %s\n"
                      name
                      institution
                      department
                      nsf-status))
      (let* ((emails (remove-if-not 
                      (lambda (x) (equal :email (car x))) contact-info))
             (phones (remove-if-not 
                      (lambda (x) (equal :phone (car x))) contact-info))
             (faxen (remove-if-not 
                     (lambda (x) (equal :fax (car x))) contact-info)))
        (mapc (lambda (email)
                  (insert (format "Email: %s\n" (cadr email))))
                emails)
        (mapc (lambda (phone)
                  (insert (format "Phone: %s\n" (cadr phone))))
                phones)
        (mapc (lambda (fax)
                  (insert (format "Fax: %s\n" (cadr fax))))
                faxen))
      (insert (format "This user is a member of the following projects:\n"))
      (if (> (length projects) 0)
          (mapc 
           (lambda (project)
             (destructuring-bind (&key project-id title project-pi) project
               (insert 
                (format "AIRS Project ID: %s PI: %s\n" project-id project-pi))
               (insert (format "Project Title: %s\n" title))))
           projects)
          (insert "No Projects Found.")
          )))

(defun insert-person-info-by-email (email)
  (interactive "sEmail Address: ")
  (let ((person-info (lookup-person-info-by-email email)))
    (insert-person-info person-info)))

(defun insert-lookup-user-by-name (last first)
  "Insert page for user by name into the current buffer"
  (interactive "sLast Name: \nsFirst Name: ")
  (let ((url (format 
              "https://www.carc.unm.edu/irs/list_persons.html?next=find_person.html&next_args=&target=&acct_req=&last_name=%s&first_name=*%s*"
              (w3m-url-encode-string last)
              (w3m-url-encode-string first))))
    (w3m-retrieve url  nil nil 
                  (format "auth_user=%s&auth_password=%s&wcommand=LOGIN"
                          *AIRS-username*
                          *AIRS-password*)
                  nil)))

(defun find-person-id-by-name (last first) ;; XXX
  (interactive "sLast Name: \nsFirst Name: ")
  (with-temp-buffer
    (insert-lookup-user-by-name last first)
    (goto-char (point-min))
    (search-forward "<H1> Choose Person </H1>")
    (let* ((data (cdr (cadddr (cadadr (read-xml)))))
           (person-info 
            (destructuring-bind (td ((a (href . url))(b name)))
                (car data)
              (list :url url :name name)))
           (institution
            (cadadr data)))
      (append person-info (list :institution institution)))
    ))

(defun lookup-person-info-by-name (last &optional first)
  (interactive "sLast Name: \nsFirst Name: \n")
  (let ((person-info (ignore-errors (find-person-id-by-name last first))))
    (when person-info
      (with-temp-buffer
      (let ((url (format "%s%s" *base-url* (cadr (member :url person-info))))
              name
              affiliation
              title
              institution
              employer
              department
              nsf-status
              contact-info
              contact ;; distilled contact-info
              projects
              )
          (w3m-retrieve url  nil nil 
                        (format "auth_user=%s&auth_password=%s&wcommand=LOGIN"
                                *AIRS-username*
                                *AIRS-password*)
                        nil)
        (goto-char (point-min))
        (search-forward "<TD><H1>")
        (beginning-of-line)
        (setf name (cadadr (read-xml)))
        (search-forward "<!--- BEGIN Affiliation TEMPLATE --->")
        (setf affiliation (read-xml))
        (setf title (cadr (third (second affiliation))))
        (setf institution (cadr (third (third affiliation))))
        (setf employer (cadr (third (fourth affiliation))))
        (setf department (cadr (third (fifth affiliation))))
        (setf nsf-status (cadr (third (sixth affiliation))))
        (search-forward "<!--- BEGIN Contact TEMPLATE --->")
        (setf contact-info (read-xml))
        (setf contact
              (mapcar 
               (lambda (x) 
                 (cond ((equal (cadadr x) "Office") 
                        (list :phone (cadadr (cadadr (third x)))))
                       ((equal (cadadr x) "Fax") 
                        (list :fax (cadadr (cadadr (third x)))))
                       ((equal (cadadr x) "Email")
                        (list :email (cadr (cadadr (cadadr (third x))))))
                       ((equal (cadadr x) "Mobile")
                        (list :mobile (cadadr (cadadr (third x)))))
                       ((equal (cadadr x) "Home")
                        (list :home (cadadr (cadadr (third x)))))
                       (t
                        (list :email (and (listp (cadadr (third x)))
                                          (cadr (cadadr (cadadr (third x)))))))))
               (cdddr contact-info)))
        (search-forward "<TITLE>Choose Project</TITLE>")
        (search-forward "<table>")
        (beginning-of-line)
        (setf projects
              (remove nil
                      (mapcar 
                       (lambda (x) 
                         (ignore-errors
                           (destructuring-bind (tr (td (a (b project-id &rest))) (td2 title) (td3 project-pi) &rest rest) x 
                             `(:project-id ,project-id 
                                           :title ,(substring title 6 (- (length title) 3))
                                           :project-pi ,(substring project-pi 0 (- (length project-pi) 5))))))
                       (read-xml))))
        (list :name name
              :title (or title "") :institution (or institution "")
              :employer (or employer "")
              :department (or department "")
              :nsf-status (or (and nsf-status
                                   (substring nsf-status 3 (- (length nsf-status) 3))) "")
              :contact-info contact
              :projects projects))))))

(defun insert-person-info-by-name (last first)
  (interactive "sLast Name: \nsFirst Name: ")
  (let ((person-info (lookup-person-info-by-name last first)))
    (insert-person-info person-info)))

(defun s-trim-left (s)
  "Remove whitespace at the beginning of S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))

(defun s-trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun s-trim (s)
  "Remove whitespace at the beginning and end of S."
  (s-trim-left (s-trim-right s)))

(defun owner-to-name (owner)
  ;; doesn't work for short emails
  (let* ((owners (slurp-owners-table))
         (xowner (s-trim owner))
         (results nil))
    ;;; find the email in the owners table
    (mapcar (lambda (x) (if (and (listp x)
                                 (or (equal xowner (car x))
                                     (and
                                      (> (length (caddr x)) (length xowner))
                                      (equal xowner (substring  (caddr x) 0 (length xowner))))))

                            (push x results))) owners)
    (cadar results) ; the cadar is the owner's first name
    ))

;; this is gross!
;; (defun owner-to-name (owner)
;;   (let ((xowner (s-trim owner)))
;;     (cond ((equal xowner "download")
;;            "Jim Prewett")
;;           ((equal xowner "barchu02")
;;            "Ben Archuleta")
;;           ((equal xowner "azzawi")
;;            "Hussein Al-Azzawi")
;;           ((equal xowner "rjohns03")
;;            "Ryan Johnson")
;;           ((equal xowner "josesanc")
;;            "Jose Sanchez")
;;           ((equal xowner "mfricke")
;;            "Matthew Fricke")
;;           ((equal xowner "manish")
;;            "Manish Bhattarai")
;;           ((equal xowner "Savin")
;;            "Savin Shynu Varghese")
;;           ((equal xowner "SAVIN")
;;            "Savin Shynu Varghese")
;;           (t
;;            (error (format "Unknown Owner: %s" xowner))))))

(defun foo (ticket-id)
  (interactive "*nTicket ID: ")
  (destructuring-bind (&key ticket-id-and-checksum created closed last-changed last-changed-by assigned originator)
      (get-ticket-info ticket-id)
    (let* ((owner (owner-to-name assigned))
           (user-info
            (if (posix-string-match "@" originator)
                (lookup-person-info-by-email originator)
                (let* ((names (split-string originator " "))
                       (first (first names))
                       (last (second names)))
                  (lookup-person-info-by-name last first))))
           (user-name 
            (destructuring-bind (&key name title institution employer department nsf-status contact-info projects)
                user-info
              name)))
      (list
       :ticket-id ticket-id
       :user-name user-name
       :owner owner))
    ))

(defun insert-redflag-email-text (ticket-id &optional buffer)
  (interactive "*nTicket ID: \nbBuffer: \n")
  (destructuring-bind (&key ticket-id-and-checksum created closed last-changed last-changed-by assigned originator)
      (get-ticket-info ticket-id)
    (let* ((buffer (or buffer (current-buffer)))
           (owner (owner-to-name assigned))
           (user-info
            (if originator
                (if (posix-string-match "@" originator)
                    (ignore-errors (lookup-person-info-by-email originator))
                    (let* ((names (split-string originator " "))
                           (first (first names))
                           (last (second names)))
                      (ignore-errors (lookup-person-info-by-name last first))))))
           (user-name 
            (destructuring-bind (&key name title institution employer department nsf-status contact-info projects)
                user-info
              name))
           (names (nreverse (and user-name (split-string user-name ", "))))
           (user-first-name (first names))
           (user-last-name
            (let ((split (and (second names) (split-string (second names) "Prof. "))))
              (or (cadr split) (car split)))))
      (with-buffer buffer
        (if (or user-first-name user-last-name)
            (insert 
         (format "%s,

This ticket, ticket ID %s, has been REDFLAGGED.  The owner of
this ticket is %s.  The user that submitted this ticket \nis %s %s.
Below is the complete history of the ticket followed by more
information about the user.\n\n" 
              director-short-name ticket-id owner user-first-name user-last-name))
            (insert 
         (format "%s,

This ticket, ticket ID %s, has been REDFLAGGED.  The owner of
this ticket is %s.  The user that submitted this ticket \nis not known to AIRS.
Below is the complete history of the ticket\n\n" 
              director-short-name ticket-id owner)))
        (insert (format "Complete List of AIRS Ticket Items for ticket ID %s:\n" ticket-id))
        (insert-ticket-items-info-into-buffer ticket-id)
        ;; (when (or user-first-name user-last-name)
        ;;   (insert (format "\nInformation about %s %s:\n" user-first-name user-last-name))
        ;;   (insert-person-info user-info))
        (insert (format "\n\nThanks,\n%s\n" (user-full-name)))
        ))))

(defun insert-punt-email-text (ticket-id &optional buffer)
  (interactive "*nTicket ID: \nbBuffer: \n")
      (destructuring-bind (&key ticket-id-and-checksum created closed last-changed last-changed-by assigned originator)
      (get-ticket-info ticket-id)
    (let* ((buffer (or buffer (current-buffer)))
           (owner (owner-to-name assigned))
        ;; (owner (or (ignore-errors
                 ;;             (owner-to-name assigned))
                 ;;           assigned)
                 ;; )
             (user-info
              (or (if originator
                      (if (posix-string-match "@" originator)
                          (lookup-person-info-by-email originator)
                          (let* ((names (split-string originator " "))
                                 (first (first names))
                                 (last (second names)))
                            (let ((ret   (lookup-person-info-by-name last first)))
                              ret))))
                  '(:name "unknown")))
             (user-name 
              (destructuring-bind (&key name title institution employer department nsf-status contact-info projects)
                  user-info
                ;; (warn (format "got name: %s" name))
                ;; (warn (format "user info: %s" user-info))
                name))
             (names (nreverse (and user-name (split-string user-name ", "))))
             (user-first-name (first names))
             (user-last-name
              (let ((split (and (second names) (split-string (second names) "Prof. "))))
                (or (cadr split) (car split)))))
           (with-buffer buffer
             (if (or user-first-name user-last-name)
                 (insert 
                  (format "%s,

This ticket, ticket ID %s, is being \"punted\" to you.  The user that
submitted this ticket is %s %s.  Below is more information about the
user followed by the complete history of the ticket.\n" 
                          director-short-name ticket-id user-first-name user-last-name))
                 (insert 
                  (format "%s,

This ticket, ticket ID %s, is being \"punted\" to you.  The user that
submitted this ticket is not known to AIRS.\nThe user's email address is: %s.\n  Below is the complete history of the ticket.\n" 
                          director-short-name ticket-id originator)))
             (when (or user-first-name user-last-name)
               (insert (format "\nInformation about %s %s:\n" user-first-name user-last-name))
               (insert-person-info user-info))
             (insert "\nTicket Items:\n")
             (insert-ticket-items-info-into-buffer ticket-id)
             (insert (format "\n\nThanks,\n%s\n" (user-full-name)))
             ))))

(defun insert-attention-email-text (ticket-id &optional buffer)
  (interactive "*nTicket ID: \nbBuffer: \n")
      (destructuring-bind (&key ticket-id-and-checksum created closed last-changed last-changed-by assigned originator)
      (get-ticket-info ticket-id)
    (let* ((buffer (or buffer (current-buffer)))
           (owner (owner-to-name assigned))
           (user-info
            (or (if originator
                     (if (posix-string-match "@" originator)
                         (lookup-person-info-by-email originator)
                         (let* ((names (split-string originator " "))
                                (first (first names))
                                (last (second names)))
                           (let ((ret   (lookup-person-info-by-name last first)))
                             ret))))
                 '(:name "unknown")))
           (user-name 
            (destructuring-bind (&key name title institution employer department nsf-status contact-info projects)
                user-info
              (warn (format "got name: %s" name))
              (warn (format "user info: %s" user-info))
              name))
           (names (nreverse (and user-name (split-string user-name ", "))))
           (user-first-name (first names))
           (user-last-name
            (let ((split (and (second names) (split-string (second names) "Prof. "))))
                (or (cadr split) (car split)))))
      (with-buffer buffer
        (if (or user-first-name user-last-name)
            (insert 
             (format "%s,

This ticket, ticket ID %s, needs your attention.  The user that
submitted this ticket is %s %s.  The owner of this ticket is %s.
Below is more information about the user followed by the complete history of the ticket.\n"  
                     director-short-name ticket-id user-first-name user-last-name owner)))
        (when (or user-first-name user-last-name)
          (insert (format "\nInformation about %s %s:\n" user-first-name user-last-name))
          (insert-person-info user-info))
        (insert "\nTicket Items:\n")
        (insert-ticket-items-info-into-buffer ticket-id)
        (insert (format "\n\nThanks,\n%s\n" (user-full-name)))
        ))))

(defun insert-ticket-summary-email-text (ticket-id &optional buffer)
  (interactive "*nTicket ID: \nbBuffer: \n")
  (destructuring-bind (&key ticket-id-and-checksum created closed last-changed last-changed-by assigned originator)
      (get-ticket-info ticket-id)
    (let* ((buffer (or buffer (current-buffer)))
           (owner (owner-to-name assigned))
           (user-info
            (if originator
                (if (posix-string-match "@" originator)
                (lookup-person-info-by-email originator)
                (let* ((names (split-string originator " "))
                       (first (first names))
                       (last (second names)))
                  (lookup-person-info-by-name last first)))))
           (user-name 
            (destructuring-bind (&key name title institution employer department nsf-status contact-info projects)
                user-info
              name))
           (names (nreverse (and user-name (split-string user-name ", "))))
           (user-first-name (first names))
           (user-last-name
            (let ((split (and (second names) (split-string (second names) "Prof. "))))
                (or (cadr split) (car split)))))
      (with-buffer buffer
        (if (or user-first-name user-last-name)
            (insert 
             (format "%s,

Here is the complete history for ticket ID %s.  The user that submitted this ticket is %s %s.  Below is the complete history of the ticket\n" 
                     director-short-name ticket-id user-first-name user-last-name))
            (insert 
             (format "%s,

Here is the complete history for ticket ID %s.  The user that submitted this ticket is not known to AIRS.  The user's email address is: %s.\nBelow is the complete history of the ticket.\n"
                     director-short-name ticket-id originator)))
        (insert "\nTicket Items:\n")
        (insert-ticket-items-info-into-buffer ticket-id)
        (insert (format "\n\nThanks,\n%s\n" (user-full-name)))
        ))))

(defun org-ticket-compose-full-history ()    
  (interactive)
  (org-beginning-of-line)
  (let ((point (point))
        (buffer (current-buffer))
        (id (progn (search-forward-regexp "\\[\\([0-9]+\\)\\]") 
                   (match-string 1))))
    (if id
        (progn
          (compose-mail director-email
                        (format "Full Thread for Ticket %s -- %s" id (ticket-mail-subject id))
                      '(("CC" "help@carc.unm.edu"))
                      nil
                      (lambda (x) (switch-to-buffer x))
                      nil
                      )
          (insert-ticket-summary-email-text id (current-buffer)))

        (error "no ticket found! in buffer %s" buffer))))

(defun org-ticket-compose-redflag ()    
  (interactive)
  (org-beginning-of-line)
  (let ((point (point))
        (buffer (current-buffer))
        (id (progn (search-forward-regexp "\\[\\([0-9]+\\)\\]") 
                   (match-string 1))))
    (if id
        (progn
          (compose-mail director-email
                      (format "Ticket %s has been REDFLAGGED -- RE: %s" 
                              id  
                              (ticket-mail-subject id))
                      
                      '(("CC" "help@carc.unm.edu"))
                      nil
                      (lambda (x) (switch-to-buffer x))
                      nil
                      `((update-ticket-after-mail . (,buffer ,point))))
          (insert-redflag-email-text id (current-buffer)))

        (error "no ticket found! in buffer %s" buffer))))

(defun org-ticket-compose-punt ()    
  (interactive)
  (org-beginning-of-line)
  (let ((point (point))
        (buffer (current-buffer))
        (id (progn (search-forward-regexp "\\[\\([0-9]+\\)\\]") 
                   (match-string 1))))
    (if id
        (progn
          (compose-mail director-email
                      (format "Ticket %s is being punted to you -- RE: %s" 
                              id  
                              (ticket-mail-subject id))
                      
                      '(("CC" "help@carc.unm.edu"))
                      nil
                      (lambda (x) (switch-to-buffer x))
                      nil
                      `((update-ticket-after-mail . (,buffer ,point))))
          (insert-punt-email-text id (current-buffer)))

        (error "no ticket found! in buffer %s" buffer))))

(defun org-ticket-compose-attention ()    
  (interactive)
  (org-beginning-of-line)
  (let ((point (point))
        (buffer (current-buffer))
        (id (progn (search-forward-regexp "\\[\\([0-9]+\\)\\]") 
                   (match-string 1))))
    (if id
        (progn
          (compose-mail director-email
                      (format "Ticket %s needs your attention -- RE: %s" 
                              id  
                              (ticket-mail-subject id))
                      
                      nil
                      ;; '(("CC" "help@carc.unm.edu"))
                      nil
                      (lambda (x) (switch-to-buffer x))
                      nil
                      nil)
          (insert-attention-email-text id (current-buffer)))

        (error "no ticket found! in buffer %s" buffer))))

(defun all-ticket-information (ticket-id)
  (interactive "*nTicket ID: \nbBuffer: \n")
  (destructuring-bind (&key ticket-id-and-checksum created closed last-changed last-changed-by assigned originator)
      (get-ticket-info ticket-id)
    (let* ((owner (owner-to-name assigned))
           (user-info
            (if originator
                (if (posix-string-match "@" originator)
                    (progn
                      (lookup-person-info-by-email originator))
                    (let* ((names (split-string originator " "))
                           (first (first names))
                           (last (second names)))
                      (lookup-person-info-by-name last (format "%s*" first))))))
           (user-name 
            (destructuring-bind (&key name title institution employer department nsf-status contact-info projects)
                user-info
              name))
           (names (nreverse (and user-name (split-string user-name ", "))))
           (user-first-name (first names))
           (user-last-name
            (let ((split (and (second names) (split-string (second names) "Prof. "))))
                (or (cadr split) (car split)))))
      `(:owner ,owner :user-info ,user-info :user-name ,user-name :user-first-name ,user-first-name :user-last-name ,user-last-name
               :ticket-id-and-checksum ,ticket-id-and-checksum 
               :created ,created
               :closed ,closed 
               :last-changed ,last-changed
               :last-changed-by ,last-changed-by 
               :assigned ,assigned 
               :originator ,originator))))

(defun filter-tickets (table function)
  (loop for ticket in table
       with func = (intern function)
     when (funcall func ticket)
     collect ticket))

(defun org-ticket-current-ticket ()
  (let (start end)
    (save-excursion
      (setf end (search-forward "]"))
      (setf start (search-backward "["))
      (buffer-substring-no-properties (1+ start) (1- end)))))

(defun org-ticket-goto-ticket (ticket-id)
  (interactive
   (list
    (read-string (format "Ticket ID (%s): " (org-ticket-current-ticket))
                 nil nil (org-ticket-current-ticket)))
   )
  (let ((location
         (save-excursion
           (goto-line 1)
           (search-forward-regexp "^*** tickets")
           (search-forward (format "[%s]" ticket-id))
           (point))))
    (push-mark)
    (goto-char location)))

;;;###autoload
(define-derived-mode org-tickets-mode org-mode "Org-Tickets"
  "Ticket Tracking system based on Carsten Dominik's Org-Mode
   \\{org-tickets-mode-map}"
  ;; key bindings
  (define-key org-tickets-mode-map (kbd "C-c t") 'org-ticket-import)
  (define-key org-tickets-mode-map (kbd "C-c n") 'org-ticket-next-state)
  (define-key org-tickets-mode-map (kbd "C-c p") 'org-ticket-previous-state)
  (define-key org-tickets-mode-map (kbd "C-c c") 'org-ticket-closed)
  (define-key org-tickets-mode-map (kbd "C-c r") 'org-ticket-responded)
  (define-key org-tickets-mode-map (kbd "C-c s") 'org-ticket-change-state)
  (define-key org-tickets-mode-map (kbd "C-c m") 'org-ticket-please-update)
  (define-key org-tickets-mode-map (kbd "C-c w") 'org-ticket-redflag-warning)
  (define-key org-tickets-mode-map (kbd "C-c e") 'org-ticket-daily-report-export)
  (define-key org-tickets-mode-map (kbd "C-c o") 'org-ticket-change-owner)
  (define-key org-tickets-mode-map (kbd "C-c f") 'org-ticket-compose-redflag)
  (define-key org-tickets-mode-map (kbd "C-c x") 'org-ticket-compose-full-history)
  (define-key org-tickets-mode-map (kbd "C-c u") 'org-ticket-compose-punt)
  (define-key org-tickets-mode-map (kbd "C-c y") 'org-ticket-compose-attention)
  (define-key org-tickets-mode-map (kbd "C-c h") 'org-ticket-goto-ticket)
  (define-key org-tickets-mode-map (kbd "C-c g") 'org-ticket-goto-ticket))



(defvar czar-menu (make-sparse-keymap "Czar (SPC for more options)"))
(define-key czar-menu (kbd "s")
       '(menu-item "Change State of Ticket" org-ticket-change-state
         :help "Change the state of a ticket"))
(define-key czar-menu (kbd "t") 
  '(menu-item "Import daily CSV file" org-ticket-import
    :help "Import the daily CSV file into this buffer"))
(define-key czar-menu (kbd "n") 
  '(menu-item "Set ticket to next state" org-ticket-next-state
    :help "Set the state of the ticket on the current line to the next state defined in the state transitions table"))
(define-key czar-menu (kbd "p") 
  '(menu-item "Set ticket to previous state" org-ticket-previous-state
    :help "Set the state of the ticket on the current line to the previous state defined in the state transitions table"))
(define-key czar-menu (kbd "c") 
  '(menu-item "Set ticket state to closed" org-ticket-closed
    :help "Set the state of the ticket on the current line to closed"))
(define-key czar-menu (kbd "r") 
  '(menu-item "Set ticket state to responded" org-ticket-responded
    :help "Set the state of the ticket on the current line to responded"))
(define-key czar-menu (kbd "s") 
  '(menu-item "Change the state of a ticket" org-ticket-change-state
    :help "Change the state of the ticket on the current line"))
(define-key czar-menu (kbd "m") 
  '(menu-item "Compose a 'please update' message to the owner of the ticket" org-ticket-please-update
    :help "Compose a message to the owner of the ticket asking them to please update the ticket on the current line"))
(define-key czar-menu (kbd "w") 
  '(menu-item "Send a 'redflag warning' message to the owner of the ticket" org-ticket-redflag-warning :help "Compose a message to the owner of the ticket asking them to please update the ticket on the current line as it is about to be REDFLAGGED"))
(define-key czar-menu (kbd "e") 
  '(menu-item "Send the daily report" org-ticket-daily-report-export
    :help "Compose a daily report"))
(define-key czar-menu (kbd "o") 
  '(menu-item "Change the owner of a ticket" org-ticket-change-owner
    :help "Change the owner of the ticket on the current line"))
(define-key czar-menu (kbd "f") 
  '(menu-item "Send a 'redflag' message for a ticket" org-ticket-compose-redflag
    :help "Compose a message indicating that the ticket on the current line has been REDFLAGGED (The ticket owner has not responded after several attempts)"))
(define-key czar-menu (kbd "u") 
  '(menu-item "Send a 'punt' message for a ticket" org-ticket-compose-punt
    :help "Compose a message indicating that the ticket on the current line has been PUNTED (The user has not responded after several attempts)"))
(define-key czar-menu (kbd "x") 
  '(menu-item "Send the full history for a ticket to the Director" org-ticket-compose-full-history
    :help "Compose a message to the Director with the full history of the ticket."))

(defvar czar-keybd-menu (copy-keymap czar-menu))

(eval-when (:load-toplevel :execute)
  (define-key org-tickets-mode-map (kbd "C-c z")  czar-menu)
  (define-key org-tickets-mode-map [menu-bar czar]
    (cons "Czar" czar-menu)))

(provide 'org-tickets)

