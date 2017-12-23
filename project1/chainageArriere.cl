(defun chainageArriere (ordiChoisi)
  (let ((but (list 'ordi '= ordiChoisi)))
    (if (verifierFait but)
        (progn
          (format t "~%-------------------- ~% ~%")
          (format t "L'ordi choisi correspond à vos critères.~%~%")
          "Fin de la recherche")
      (progn
        (afficherCauses)
        "Fin de la recherche"))))
        
(defun verifierFait (fait)
  (let ((OK NIL) EC regle)
    (if (member fait *BF* :test 'equal)
        (setq OK T)
      (progn
        (setq EC (reglesCandidates fait))
        (while (and (not OK) EC)
          (setq regle (pop EC))
          (setq OK (verifierRegle regle)))))
    OK))

(defun reglesCandidates (but)
  (let (res)
    (dolist (regle *BR* (reverse res)) 
      (if (equal but (car (last regle)))
          (if (equal 'ordi (car (car (last regle))))
              (progn
                (push regle *reglesCandidates*)
                (push (car regle) res))
            (if (regleValide regle) 
                (push (car regle) res)))))))

(defun verifierRegle (regle)
  (let ((OK T) (premisses (getPremisses regle)) premisse)
    (while (and OK premisses)
      (setq premisse (pop premisses))
      (setq OK (verifierFait premisse))
      (if (not OK)
          (pushnew premisse *faitsManquants* :test 'equal)))
    OK))

(defun getPremisses (regle)
  (cadr (assoc regle *BR*)))

(defun regleValide (regle)
  (if (not (equal nil *reglesCandidates*))
      (let ((regleCandidate (pop *reglesCandidates*)) OK)
        (cond
         ((and (present '(type = PC) (cadr regleCandidate))
               (present '(type = Mac) (cadr regle)))
          (setq OK NIL))
         ((and (present '(type = Mac) (cadr regleCandidate))
               (present '(type = PC) (cadr regle)))          
          (setq OK NIL))
         ((and (or (present '(usage = bureautique) (cadr regleCandidate))
                   (present '(usage = gaming) (cadr regleCandidate)))
               (or (present '(usage = bureautique) (cadr regle))
                   (present '(usage = gaming) (cadr regle)))               
               (not (memeUsage (cadr regleCandidate) (cadr regle))))
          (setq OK NIL))
         ((equal nil nil)
           (setq OK T)))
        (push regleCandidate *reglesCandidates*)
        OK)))

(defun afficherCauses ()
  (format t "~%-------------------- ~% ~%")
  (let (fait)
    (format t "L'ordi choisi ne correspond pas à vos critères. Réessayez avec ces nouveaux critères : ~%~%")
    (while *faitsManquants*
      (setq fait (pop *faitsManquants*))
      (if (and (not (equal (car fait) 'prix))
              (not (equal (car fait) 'usage)))
          (format t "~a ~%" fait)))
    (format t "~%")))
          
         
         
                                      



      
          
              
                
                
                
                      
    
