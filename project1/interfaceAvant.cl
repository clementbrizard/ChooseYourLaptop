;;;
; Interface utilisateur pour le chaînage avant.
; Constitue la base de faits à partir des réponses
; de l'utilisateur. Appelle ensuite la fonction de 
; chaînage avant
;;

(defun beginAvant()
  (let (type)
    (format t "Type PC ou Mac : ")
    (setq type (read))
    (push (list 'type '= type) *BF*)
    
    (format t "Exigences sur une échelle de 1 à 3 :~%")
    
    (if (equal type 'PC)
        (progn
          (format t "Traitement de texte : ")
          (push (list 'traitement_texte '= (read)) *BF*)
          (format t "Utilisation d'Internet : ")
          (push (list 'internet '= (read)) *BF*)
          (format t "Pouvoir jouer à des jeux vidéo : ")
          (push (list 'gaming '= (read)) *BF*)))
            
    (format t "Budget : ")
    (configurerBudget (read))
    
    (format t "Vous pouvez maintenant répondre 0 si vous ne savez pas : ~%")

    (format t "Autonomie : ")
    (push (list 'autonomie '= (read)) *BF*)
    
    (format t "Puissance : ")
    (push (list 'puissance '= (read)) *BF*)
    
    (format t "Mémoire : ")
    (push (list 'mémoire '= (read)) *BF*)
  
    (format t "Écran : ")
    (push (list 'écran '= (read)) *BF*)
    
    (format t "Qualité graphique : ")
    (push (list 'carte_graphique '= (read)) *BF*)
    
    (chainageAvant)))

;;;
; Remplis la base de faits à partir du
; du budget
;;

(defun configurerBudget (budget)
  
  ; positionnemement par rapport aux seuils
  ; du marché bureautique
   
  (cond 
   ((<= budget 350)
    (push '(budget <= 350) *BF*))
   ((<= budget 500)
    (progn
      (push '(budget <= 500) *BF*)
      (push '(budget > 350) *BF*)))
   ((> budget 500)
    (push '(budget > 500) *BF*)))
  
  ; gaming
  
  (cond
   ((<= budget 900)
    (push '(budget <= 900) *BF*))
   ((<= budget 1100)
    (progn
      (push '(budget <= 1100) *BF*)
      (push '(budget > 900) *BF*)))
   ((> budget 1100)
    (push '(budget > 1100) *BF*)))
  
  ; Macs
  
  (cond
   ((<= budget 1500)
    (push '(budget <= 1500) *BF*))
   ((<= budget 2500)
    (progn
      (push '(budget <= 2500) *BF*)
      (push '(budget > 1500) *BF*)))
   ((> budget 2500)
    (push '(budget > 2500) *BF*))))


  
  



   
   




  
