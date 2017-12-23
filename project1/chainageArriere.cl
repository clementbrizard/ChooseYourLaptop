(defun chainageArriere (ordiChoisi)
  (let ((but (list 'ordi '= ordiChoisi)))
    (if (verifierFait but)
        "L'ordi choisi correspond à vos critères"
      "L'ordi choisi ne correspond pas à vos critères")))
        
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
          (push (car regle) res)))))

(defun verifierRegle (regle)
  (let ((OK T) (premisses (getPremisses regle)) premisse)
    (while (and OK premisses)
      (setq premisse (pop premisses))
      (setq OK (verifierFait premisse)))
    OK))

(defun getPremisses (regle)
  (cadr (assoc regle *BR*)))



      
          
              
                
                
                
                      
    
