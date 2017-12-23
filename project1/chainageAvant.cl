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
  (let (faitManquant budget)
    (format t "-------------------- ~% ~%")
    (while *ordisManques*
      (format t "Ordi qui aurait été possible : ~a ~%" (car (last (pop *ordisManques*))))
      (format t "Critères nécessaires : ~a ~%" (pop *faitsNecessaires*))
      (setq faitManquant (pop *faitsManquants*))
      (if (equal 'prix (car faitManquant))
          (progn
            (setq budget (budgetFromPrix faitManquant))
            (format t "Fait manquant : ~%")
            (afficherListe budget)
            (format t "~%"))
        (format t "Fait manquant : ~a ~% ~%" faitManquant))
      (format t "-------------------- ~% ~%"))))

(defun budgetFromPrix (faitPrix)
  (let (type usage (prix (car (last faitPrix))) res)
    (if (present '(type = Mac) *BF*)
        (setq type 'Mac)
      (if (present '(usage = bureautique) *BF*)
          (setq usage 'bureautique)
        (setq usage 'gaming)))
    (cond
     ((equal usage 'bureautique)
      (cond
       ((equal prix '1)
        (push '(budget > 500) res))
       ((equal prix '2)
        (progn
          (push '(budget > 350) res)
          (push '(budget <= 500) res)))
       ((equal prix '3)
        (push '(budget <= 350) res))))
     ((equal usage 'gaming)
      (cond
       ((equal prix '1)
        (push '(budget > 1100) res))
       ((equal prix '2)
        (progn
          (push '(budget > 900) res)
          (push '(budget <= 1100) res)))
       ((equal prix '3)
        (push '(budget <= 9000) res))))
     ((equal type 'Mac)
      (cond
       ((equal prix '1)
        (push '(budget > 2500) res))
       ((equal prix '2)
        (progn
          (push '(budget > 1500) res)
          (push '(budget <= 2500) res)))
       ((equal prix '3)
        (push '(budget <= 1500) res)))))
    res))
       
(defun declenchable (regle BF)
  (let ((OK T) (ccl (car (last regle))) faitsManquants)
    (dolist (fait (cadr regle))
      (if (not (present fait BF))
          (progn
            (setq OK NIL)
            (push fait faitsManquants))))
    (if (regleManqueeValide regle faitsManquants BF)
        (regleManquee regle (car faitsManquants)))  
    OK))

(defun regleManqueeValide (regle faitsManquants base)
  (let ((ccl (car (last regle))))
    (if (and (equal 'ordi (car ccl))
             (equal (length faitsManquants) 1)
             (not (present ccl *ordisManques*)))
        (if (present '(type = PC) (cadr regle))
            (if (memeUsage (cadr regle) base)
                T)
          T))))
  

(defun regleManquee (regle faitManquant)
  (if (not (present (car (last regle)) *ordisManques*))
      (progn
        (push (cadr regle) *faitsNecessaires*)
        (push (car (last regle)) *ordisManques*)
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
    (pop *faitsManquants*))
  (while (not (equal *reglesCandidates* ()))
    (pop *reglesCandidates*)))
  
(defun afficherListe (liste)
  (dolist (elem liste)
    (print elem))
  (format t "~%"))

(defun memeUsage (faits base)
  (if (and (equal (getUsage faits) (getUsage base))
           (not (equal nil (getUsage faits))))
      T))

(defun getUsage (faits)
  (let (usage)
    (dolist (fait faits usage)
      (if (equal (car fait) 'usage)
(setq usage fait)))))

     


  



