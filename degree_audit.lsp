; Code by Noah Gegner, Jada Bonnett, and Jack Miller

(defun get-course-credits (course catalog)
    (if (null catalog)
        nil
        (if (eql course (caar catalog))
            (cadar catalog)
            (get-course-credits course (cdr catalog))
        )
    )
)

(defun count-hours (courses catalog hours required electives)
    (if (null courses)
        hours
        (if (typep (car courses) 'list)
            (if (and (check-grade (car courses)) (or (check-valid-course (caar courses) required) (check-valid-course (caar courses) electives)))
                (count-hours (cdr courses) catalog (+ hours (get-course-credits (caar courses) catalog)) required electives)
                (count-hours (cdr courses) catalog hours required electives)
            )
            (if (or (check-valid-course (car courses) required) (check-valid-course (car courses) electives))
                (count-hours (cdr courses) catalog (+ hours (get-course-credits (car courses) catalog)) required electives)
                (count-hours (cdr courses) catalog hours required electives)
            )
        )
    )
)

(defun check-valid-course (course required)
    (if (null required)
        nil
        (if (typep (car required) 'list)
            (if (member course (car required))
                t
                (check-valid-course course (cdr required))
            )
            (if (eql course (car required))
                t
                (check-valid-course course (cdr required))
            )
        )
    )
)

(defun check-grade (course)
    (setf grade (cadr course))
    (if (and (not (eql grade 'D+)) (not (eql grade 'D)) (not (eql grade 'D-)) (not (eql grade 'F)))
        t
        nil
    )
)

(defun check-completed (completed current-class)
    (if (null completed)
        nil
        (if (eql (caar completed) current-class)
            (if (check-grade (car completed))
                t
                nil
            )
            (check-completed (cdr completed) current-class)
        )
    )
)

(defun check-planned (plan current-class)
    (if (null plan)
        nil
        (if (eql (car plan) current-class)
            t
            (check-planned (cdr plan) current-class)
        )
    )
)

(defun check-or (completed plan options)
    (if (null options)
        nil
        (if (or (check-completed completed (car options)) (check-planned plan (car options)))
            t
            (check-or completed plan (cdr options))
        )
    )
)

(defun check-required (completed plan required needed)
    (if (null required)
        needed
        (if (typep (car required) 'list)
            (if (check-or completed plan (cdar required))
                (check-required completed plan (cdr required) needed)
                (check-required completed plan (cdr required) (append needed (list (car required))))
            )
            (if (or (check-completed completed (car required)) (check-planned plan (car required)))
                (check-required completed plan (cdr required) needed)
                (check-required completed plan (cdr required) (append needed (list (car required)))) 
            )
        )
    )
)

(defun grad-check (person degree catalog)
    (setf required-hours (cadar degree)
          required (cdadr degree)
          electives (cdaddr degree)
          completed (cdar person)
          plan (cdadr person)
    )
    (setf courses (append completed plan)
          hours-taken (count-hours courses catalog 0 required electives)
          needed (check-required completed plan required '())
    )
    (if (or (>= required-hours hours-taken) (not (null needed)))
        (format t "FAIL ;~% ~A EXTRA-HOURS ~A~%" needed (- hours-taken required-hours))
        (format t "SUCCESS ;~% ~A EXTRA-HOURS ~A~%" nil (- hours-taken required-hours))
    )
)

