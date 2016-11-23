(defvar org-toodledo-sim-id 10000)
(defvar org-toodledo-sim-lastedit-task 0)
(defvar org-toodledo-sim-lastdelete-task 0)
(defvar org-toodledo-sim-curtime 0)
(defvar org-toodledo-sim-pro "1")
(defvar org-toodledo-sim-db-tasks nil)
(defvar org-toodledo-sim-db-deleted nil)
(defvar org-toodledo-sim-invalid-key nil)
(defvar org-toodledo-sim-db-folders nil)

(defun org-toodledo-sim-init (pro)
  (setq org-toodledo-sim-curtime 1
        org-toodledo-sim-lastedit-task 0
        org-toodledo-sim-lastdelete-task 0
        org-toodledo-sim-db-tasks nil
        org-toodledo-sim-db-deleted nil
        org-toodledo-sim-pro pro
        org-toodledo-sim-mode t))

(defun org-toodledo-sim-http-post (method &optional params)
  (org-toodledo-debug "Simulated http-post for '%s' with '%S'" method params)
  (with-temp-buffer
    (org-toodledo-sim-xml-init)
    (cond
     (org-toodledo-sim-invalid-key (org-toodledo-sim-invalid-key))
     ((string= method "account/token") (org-toodledo-sim-account-token params))
     ((string= method "account/get") (org-toodledo-sim-account-get params))
     ((string= method "tasks/get") (org-toodledo-sim-tasks-get params))
     ((string= method "tasks/add") (org-toodledo-sim-tasks-add params))
     ((string= method "tasks/edit") (org-toodledo-sim-tasks-edit params))
     ((string= method "tasks/delete") (org-toodledo-sim-tasks-delete params))
     ((string= method "tasks/deleted") (org-toodledo-sim-tasks-deleted params))
     ((string= method "folders/get") (org-toodledo-sim-folders-get params))
     ((string= method "folders/add") (org-toodledo-sim-folders-add params))

     (t (error "No sim support for method: %S" method)))
    (list (buffer-substring (point-min) (point-max))
          (format "HTTP/1.1 200 OK
Date: Sat, 10 Mar 2012 01:21:28 GMT
Server: Apache/2.2.3 (Red Hat)
Content-Length: %d
Keep-Alive: timeout=2, max=100
Connection: Keep-Alive
Content-Type: application/xml
" (point-max)) 200)))

;;
;; invalid key
;;
(defun org-toodledo-sim-invalid-key ()
  (insert "<error id='2'>Invalid key</error>")
  (setq org-toodledo-sim-invalid-key nil)
  )

;;
;; account/token
;;
(defun org-toodledo-sim-account-token (params)
  (insert (format "<token>%d</token>\n" (time-to-seconds)))
)

;;
;; account/get
;;
(defun org-toodledo-sim-account-get (params)
  (insert "<account>\n")
  (insert (format "  <lastedit_task>%s</lastedit_task>\n"
                  org-toodledo-sim-lastedit-task))
  (insert (format "  <lastdelete_task>%s</lastdelete_task>\n"
                  org-toodledo-sim-lastdelete-task))
  (insert (format "  <pro>%s</pro>\n" org-toodledo-sim-pro))
  (insert "</account>\n")
  )

;;
;; tasks/get
;;
(defun org-toodledo-sim-tasks-get (params)
  (let ((modafter (string-to-number (cdr (assoc "modafter" params))))
        (comp (string-to-number (cdr (assoc "comp" params)))))
    (org-toodledo-sim-xml-get-tasks
     (delq nil
           (mapcar
            (lambda (task)
              (if (and (> (string-to-number (org-toodledo-task-modified task))
                          modafter)
                       (or (eq comp -1)
                           (not (org-toodledo-task-is-completed task))))
                  task nil))
            (mapcar 'cdr org-toodledo-sim-db-tasks))))))

(defun org-toodledo-sim-xml-get-tasks (tasks)
  (let ((l (length tasks)))
    (insert (format "<tasks num='%d' total='%d'>\n" l l)))
  (mapc 'org-toodledo-sim-xml-get-task tasks)
  (insert "</tasks>"))

(defun org-toodledo-sim-xml-get-task (task)
  (insert "  <task>\n")
  (mapc (lambda (pair)
          (let ((f (car pair))
                (v (cdr pair)))
            (insert (format "    <%s>%s</%s>\n" f v f))))
        task)
  (insert "  </task>\n")
)

;;
;; tasks/deleted
;;
(defun org-toodledo-sim-tasks-deleted (params)
  (let*
      ((after (string-to-number (cdr (assoc "after" params))))
       (del-list
        (delq nil
              (mapcar (lambda (elem)
                        (let ((id (car elem))
                              (deltime (cdr elem)))
                          (if (> deltime after) elem nil)))
                      org-toodledo-sim-db-deleted))))

    (insert (format "<deleted num=\"%d\">\n" (length del-list)))
    (mapc (lambda (elem)
            (insert
             (format "  <task><id>%s</id><stamp>%s</stamp></task>\n"
                     (car elem) (cdr elem))))
          del-list)
    (insert "</deleted>\n")))

;;
;; tasks/add
;;
(defun org-toodledo-sim-tasks-add (params)
  (insert "<tasks>\n")
  (mapc
   (lambda (json-task)
     (setq task
           (mapcar (lambda (pair)
                     (cons (symbol-name (car pair)) (cdr pair))) json-task))
     (let ((ref (cdr (assoc "ref" task)))
           (id (org-toodledo-sim-db-new-task task)))
       (cond
        ((integerp id)
         (insert
          (format "  <error id='%d'>%s</error>"
                  id (org-toodledo-error-num-to-str (int-to-string id)))))

        (t
         (insert "  <task>\n")
         (insert (format "    <id>%s</id>\n" id))
         (insert (format "    <ref>%s</ref>\n" (or ref "")))
         (insert "  </task>\n")))))
   (json-read-from-string (cdr (assoc "tasks" params))))
  (insert "</tasks>\n"))

;;
;; tasks/edit
;;
(defun org-toodledo-sim-tasks-edit (params)
  (insert "<tasks>\n")
  (mapc
   (lambda (json-task)
     (setq task
           (mapcar (lambda (pair)
                     (cons (symbol-name (car pair)) (cdr pair))) json-task))
     (let ((id (org-toodledo-sim-db-edit-task task)))
       (cond
        ((> id 0)
         (insert
          (format "  <error id='%d'>%s</error>"
                  id (org-toodledo-error-num-to-str (int-to-string id)))))

        (t
         (insert "  <task>\n")
         (insert
          (format "    <id>%s</id>\n" (org-toodledo-task-id task)))
         (insert "  </task>\n")))))
   (json-read-from-string (cdr (assoc "tasks" params))))
  (insert "</tasks>\n"))

;;
;; tasks/delete
;;
(defun org-toodledo-sim-tasks-delete (params)
  (insert "<deleted>\n")
  (mapc
   (lambda (id)
     (if (org-toodledo-sim-db-delete-task id)
         (insert (format "  <id>%s</id>\n" id))
       (insert "  <error id='7'>Invalid ID number</error>")))
   (json-read-from-string (cdr (assoc "tasks" params))))
  (insert "</deleted>\n"))

;;
;; Managing the simulated DB of tasks and deleted tasks
;;
(defun org-toodledo-sim-db-new-task (task)
  (org-toodledo-sim-advance-time)
  (let ((id (org-toodledo-task-id task))
        (title (org-toodledo-task-title task)))

    (cond
     ((string-match "simerror=\\([0-9]+\\)" title)
      (string-to-number (match-string 1 title)))

     (t
      (if (or (not id) (equal id "0"))
          (setq id (int-to-string
                    (setq org-toodledo-sim-id (1+ org-toodledo-sim-id)))))
      (alist-put task "id" id)
      (alist-put task "modified" (int-to-string org-toodledo-sim-curtime))
      (setq org-toodledo-sim-lastedit-task org-toodledo-sim-curtime)
      (alist-put org-toodledo-sim-db-tasks id task)
      id))))

(defun org-toodledo-sim-db-edit-task (task)
  (org-toodledo-sim-advance-time)
  (let ((id (org-toodledo-task-id task))
        (title (org-toodledo-task-title task)))
    (cond
     ((string-match "simerror=\\([0-9]+\\)" title)
      (string-to-number (match-string 1 title)))

     ((null (assoc id org-toodledo-sim-db-tasks))
      ;; invalid task-id
      7)

     (t
      (alist-put task "modified" (int-to-string org-toodledo-sim-curtime))
      (setq org-toodledo-sim-lastedit-task org-toodledo-sim-curtime)
      (alist-put org-toodledo-sim-db-tasks id task)
      0))))

(defun org-toodledo-sim-db-delete-task (id &optional dont-save)
  (org-toodledo-sim-advance-time)
  (if (null (assoc id org-toodledo-sim-db-tasks))
      (progn
        (org-toodledo-debug
         "org-toodledo-sim-db-delete-task: task not found %S" id)
        nil)
    (org-toodledo-debug "org-toodledo-sim-db-delete-task: deleting task %S" id)
    (alist-delete org-toodledo-sim-db-tasks id)
    (unless dont-save
      (alist-put org-toodledo-sim-db-deleted id org-toodledo-sim-curtime)
      (setq org-toodledo-sim-lastdelete-task org-toodledo-sim-curtime))
    t))

;;
;; Folders
;;

(defun org-toodledo-sim-folders-get (params)
  (insert "<folders>")
  (mapc (lambda (f)
          (insert
           (format "<folder><id>%s</id><private>0</private>\
<archived>0</archived><order>%s</order><name>%s</name></folder>"
                   (cdr (assoc "id" f))
                   (cdr (assoc "order" f))
                   (cdr (assoc "name" f)))))
        org-toodledo-sim-db-folders)
  (insert "</folders>")
  )

(defun org-toodledo-sim-folders-add (params)
  (let ((name (cdr (assoc "name" params)))
        (id (int-to-string
             (1+ (apply 'max (mapcar
                              (lambda (folder)
                                (string-to-number (cdr (assoc "id" folder))))
                              org-toodledo-sim-db-folders)))))
        (order (int-to-string
                (1+ (apply 'max (mapcar
                                 (lambda (folder)
                                   (string-to-number (cdr (assoc "order" folder))))
                                 org-toodledo-sim-db-folders)))))
        )
    (if (member-ignore-case name
                            (mapcar (lambda (folder)
                                      (cdr (assoc "name" folder)))
                                    org-toodledo-sim-db-folders))
        (insert
         "<error id='5'>A folder with that name that already exists</error>")
      (org-toodledo-sim-db-new-folder id name order)
      (insert "<folders>\n")
      (format "<folder><id>%s</id><private>0</private>\
<archived>0</archived><order>%s</order><name>%s</name></folder>"
              id name order)
      (insert "</folders>\n")
      )
    )
  )


(defun org-toodledo-sim-db-new-folder (id name order)
  (let ((f `(("id" . ,id)
             ("name" . ,name)
             ("order" . ,order))))

  (setq org-toodledo-sim-db-folders
        (append org-toodledo-sim-db-folders (list f)))))

;;
;; Misc
;;
(defun org-toodledo-sim-advance-time ()
  (setq org-toodledo-sim-curtime (+ org-toodledo-sim-curtime 1)))

(defun org-toodledo-sim-xml-init ()
  (insert "<?xml version=\\\"1.0\\\" encoding=\\\"UTF-8\\\"?>\n")
  )

(defun org-toodledo-sim-make-task (&rest params)
  (let* ((task '(("id" . "0")
                 ("title" . "Task title")
                 ("modified" . "0")
                 ("completed" . "0")
                 ("folder" . "0")
                 ("context" . "0")
                 ("goal" . "0")
                 ("priority" . "0")
                 ("repeat" . "")
                 ("repeatfrom" . "0")
                 ("duedate" . "0")
                 ("startdate" . "0")
                 ("duetime" . "0")
                 ("starttime" . "0")
                 ("length" . "0")
                 ("status" . "0")
                 ("note" . "")
                 ("parent" . "0")
                 )))
    (mapcar (lambda (pair)
              (alist-put task (car pair) (cdr pair))) params)
    task))

(provide 'org-toodledo-sim)
