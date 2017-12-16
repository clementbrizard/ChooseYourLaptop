
(defun verifier (but)
  (let (EC (BR *BR*) (BF *BF*) regleCourante)
    (loop
      (if (present but BF)
        (return "Vérifié !")
        (dolist (regle BR)
          (when (declenchable regle BF)
            (push regle EC)
            (setq BR (remove regle BR)))))
      (if EC
        (progn
          (setq regleCourante (pop EC))
          (pushnew (getConclusion regleCourante) BF))
        (return "Non vérifié")))))


(defun present (fait BF)
  (if (member fait BF :test 'equal)
      T
    NIL))

(defun declenchable (regle BF)
  (let ((OK T))
    (dolist (fait (cadr regle) OK)
      (if (not (present fait BF))
          (setq OK NIL)))))

(defun getConclusion (regle)
  (caddr regle))

(defun viderBF ()
  (while (not (equal *BF* ()))
    (pop *BF*)))



  




