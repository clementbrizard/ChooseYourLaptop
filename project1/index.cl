
(defun chercher ()
  (let (EC (BR *BR*) (BF *BF*) regleCourante results)
    (loop
      (afficherListe BF)
      (format t "~%")
      (if (not (equal nil (ordi_trouve BF)))
          (pushnew (ordi_trouve BF) results :test 'equal))
      
      (dolist (regle BR)
        (when (declenchable regle BF)
          (push regle EC)
          (setq BR (remove regle BR))))
      
      (if EC
          (progn
            (setq regleCourante (pop EC))
            (push (caddr regleCourante) BF))
        (progn
          (if (not (equal nil results))
              (afficherListe results)
            (if (conseilADonner)
                (donnerConseil)
              (format t "Aucun ordi ne correspond à vos critères")))
          (return "Fin de la recherche"))))))

(defun ordi_trouve (BF)
  (dolist (fait BF)
    (if (equal 'ordi (car fait))
        (return (car(cddr fait))))))

(defun declenchable (regle BF)
  (let ((OK T))
    (dolist (fait (cadr regle) OK)
      (if (not (present fait BF))
          (setq OK NIL)))))

(defun present (fait BF)
  (if (member fait BF :test 'equal)
      T
NIL))

(defun viderBF ()
  (while (not (equal *BF* ()))
    (pop *BF*)))

(defun conseilADonner ()
  (dolist (fait *BF*)
    (if (equal (car fait) 'conseil)
        T)
    )
  NIL)

(defun donnerConseil ()
  (dolist (fait *BF*)
    (if (equal (car fait) 'conseil)
        (cddr fait))))

(defun afficherListe (liste)
  (dolist (elem liste)
    (print elem)))
     



  



