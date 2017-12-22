(defun chainageAvant ()
  (let (EC (BR *BR*) (BF *BF*) regleCourante results)
    (loop
      (format t "~%")
      (if (not (equal nil (ordiTrouve BF)))
          (pushnew (ordiTrouve BF) results :test 'equal))
      
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
              (progn
                (format t "Ordi(s) correspondant à vos critères : ~%")
                (afficherListe results))
            (progn
              (format t "Aucun ordi ne correspond à vos critères ~% ~%")
              (afficherOrdisManques)))
          (return "Fin de la recherche"))))))

(defun ordiTrouve (BF)
  (dolist (fait BF)
    (if (equal 'ordi (car fait))
        (return (car(cddr fait))))))

(defun afficherOrdisManques ()
  (format t "-------------------- ~% ~%")
  (while *ordisManques*
    (format t "Ordi qui aurait été possible : ~a ~%" (pop *ordisManques*))
    (format t "Critères nécessaires : ~a ~%" (pop *faitsNecessaires*))
    (format t "Fait manquant : ~a ~% ~%" (pop *faitsManquants*))
    (format t "-------------------- ~% ~%")))
    

(defun declenchable (regle BF)
  (let ((OK T) (ordi (car (last (car (last regle))))) (ccl (car (car (cddr regle)))) faitsManquants)
    (dolist (fait (cadr regle))
      (if (not (present fait BF))
          (progn
            (setq OK NIL)
            (push fait faitsManquants))))
    (if (and (not (present ordi *ordisManques*)) (memeUsage regle BF) (equal 'ordi ccl))
        (if (equal (length faitsManquants) 1)
            (regleManquee regle ordi (car faitsManquants))))      
    OK))

(defun regleManquee (regle ordi faitManquant)
  (if (not (present ordi *ordisManques*))
      (progn
        (push (cadr regle) *faitsNecessaires*)
        (push ordi *ordisManques*)
        (push faitManquant *faitsManquants*))))
  

(defun present (fait BF)
  (if (member fait BF :test 'equal)
      T
    NIL))

(defun vider ()
  (while (not (equal *BF* ()))
    (pop *BF*))
  (while (not (equal *ordisManques* ()))
    (pop *ordisManques*))
  (while (not (equal *faitsNecessaires* ()))
    (pop *faitsNecessaires*))
  (while (not (equal *faitsManquants* ()))
    (pop *faitsManquants*)))
  

(defun afficherListe (liste)
  (dolist (elem liste)
    (print elem))
  (format t "~%"))

(defun memeUsage (regle base)
  (if (equal (getUsage (cadr regle)) (getUsage base))
      T))

(defun getUsage (base)
  (let (usage)
    (dolist (fait base usage)
      (if (equal (car fait) 'usage)
          (setq usage fait)))))

     


  



