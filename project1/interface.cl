(defun begin()
  (let (type usage budget but)
    (format t "Type PC ou Mac : ")
    (setq type (read))
    (push (list 'type '= type) *BF*)
    
    (if (equal type 'PC)
        (progn
          (format t "Bureautique ou gaming : ")
          (setq usage (read))
          (push (list 'usage '= usage) *BF*)))
    
    (format t "Budget : ")
    (setq budget (read))
    (push (list 'budget '= budget) *BF*)
    (setBudget budget type usage)
  
    (format t "Exigences sur une échelle de 1 à 3 :~%")
  
    (format t "Autonomie : ")
    (push (list 'autonomie '= (read)) *BF*)
    
    (format t "Puissance : ")
    (push (list 'puissance '= (read)) *BF*)
    
    (format t "Mémoire : ")
    (push (list 'mémoire '= (read)) *BF*)
  
    (format t "Écran : ")
    (push (list 'écran '= (read)) *BF*)
    
    (if (equal usage 'gaming)
        (progn
          (format t "Carte graphique : ")
          (push (list 'Carte 'graphique '= (read)) *BF*)))
    
    (cond
     ((equal usage 'bureautique)
      (setq but (list 'ordi '= (getChoixBureautique))))
     
     ((equal usage 'gaming)
      (setq but (list 'ordi '= (getChoixGaming))))
     
     ((equal type 'Mac)
      (setq but (list 'ordi '= (getChoixApple)))))
    
    (verifier but)))
  

(defun getChoixBureautique ()
  (let (choix)
    (format t "Quel ordinateur pensez-vous choisir : ~%")
    (format t "1 : ASUS EeeBook E402NA-FA045T Bleu ~%")
    (format t "2 : Acer Aspire ES1-523-24HN ~%")
    (format t "3 : ASUS R702UA-BX169T ~%")
    (format t "4 : ASUS R556QA-XX061T ~%")
    (format t "5 : ASUS P1510UA-GQ281R ~%")
    (format t "6 : Lenovo IdeaPad 320-17IKB 80XM00BUFR ~%")
    
    (format t "Votre choix : ")
    
    (setq choix (read))
    
    (cond
     ((equal choix '1)
      (setq choix '(ASUS EeeBook E402NA-FA045T Bleu)))
     ((equal choix '2)
      (setq choix '(Acer Aspire ES1-523-24HN)))
     ((equal choix '3)
      (setq choix '(ASUS R702UA-BX169T)))
     ((equal choix '4)
      (setq choix '(ASUS R556QA-XX061T)))
     ((equal choix '5)
      (setq choix '(ASUS P1510UA-GQ281R)))
     ((equal choix '6)
      (setq choix '(Lenovo IdeaPad 320-17IKB 80XM00BUFR))))
    
    choix))
     

(defun getChoixGaming ()
  (let (choix)
    (format t "Quel ordinateur pensez-vous choisir : ~%")
    (format t "1 : MSI GP62 7RE-415XFR Leopard Pro ~%")
    (format t "2 : MSI GL62VR 7RFX-867FR ~%")
    (format t "3 : MSI GP72M 7RDX-871XFR Leopard ~%")
    (format t "4 : Gigabyte Sabre 15 - P45G v7 C32W10-FR ~%")
    (format t "5 : Acer Predator Helios 300 G3-572-72MT ~%")
    (format t "6 : MSI GE72MVR 7RG-055FR Apache Pro ~%")
  
    (format t "Votre choix : ")
    
    (setq choix (read))
    
    (cond
     ((equal choix '1)
      (setq choix '(MSI GP62 7RE-415XFR Leopard Pro)))
     ((equal choix '2)
      (setq choix '(MSI GL62VR 7RFX-867FR)))
     ((equal choix '3)
      (setq choix '(MSI GP72M 7RDX-871XFR Leopard)))
     ((equal choix '4)
      (setq choix '(Gigabyte Sabre 15 - P45G v7 C32W10-FR)))
     ((equal choix '5)
      (setq choix '(Acer Predator Helios 300 G3-572-72MT)))
     ((equal choix '6)
      (setq choix '(MSI GE72MVR 7RG-055FR Apache Pro))))
    
    choix))

(defun getChoixApple ()
  (let (choix)
    (format t "Quel ordinateur pensez-vous choisir : ~%")
    (format t "1 : MacBook Air 128 Go ~%")
    (format t "2 : MacBook Air 256 Go ~%")
    (format t "3 : MacBook 256 Go ~%")
    (format t "4 : MacBook Pro 13p 128 Go ~%")
    (format t "5 : MacBook Pro 13p 256 Go ~%")
    (format t "6 : MacBook 512 Go ~%")
    (format t "7 : MacBook Pro 13p Touch Bar 256 Go ~%")
    (format t "8 : MacBook Pro 13p Touch Bar 512 Go ~%")
    (format t "9 : MacBook Pro 15p ~%")
    (format t "10 : MacBook Pro 15p Touch Bar ~%")
  
    (format t "Votre choix : ")
    
    (setq choix (read))
    
    (cond
     ((equal choix '1)
      (setq choix '(MacBook Air 128 Go)))
     ((equal choix '2)
      (setq choix '(MacBook Air 256 Go)))
     ((equal choix '3)
      (setq choix '(MacBook 256 Go)))
     ((equal choix '4)
      (setq choix '(MacBook Pro 13p 128 Go)))
     ((equal choix '5)
      (setq choix '(MacBook Pro 13p 256 Go)))
     ((equal choix '6)
      (setq choix '(MacBook 512 Go)))
     ((equal choix '7)
      (setq choix '(MacBook Pro 13p Touch Bar 256 Go)))
     ((equal choix '8)
      (setq choix '(MacBook Pro 13p Touch Bar 512 Go)))
     ((equal choix '9)
      (setq choix '(MacBook Pro 15p)))
     ((equal choix '10)
      (setq choix '(MacBook Pro 15p))))
    
    choix))


(defun setBudget (budget type usage)
  (cond
   ((equal usage 'bureautique)
    (cond
     ((<= budget 350)
      (push '(budget <= 350) *BF*))
     ((<= budget 500)
      (progn
        (push '(budget <= 500) *BF*)
        (push '(budget > 350) *BF*)))
     ((> budget 500)
      (push '(budget > 500) *BF*))))
   ((equal usage 'gaming)
    (cond
     ((<= budget 900)
      (push '(budget <= 900) *BF*))
     ((<= budget 1100)
      (progn
        (push '(budget <= 1100) *BF*)
        (push '(budget > 900) *BF*)))
     ((> budget 1100)
      (push '(budget > 1100) *BF*))))
   ((equal type 'Mac)
    (cond
     ((<= budget 1500)
      (push '(budget <= 1500) *BF*))
     ((<= budget 2500)
      (progn
        (push '(budget <= 2500) *BF*)
        (push '(budget > 1500) *BF*)))
     ((> budget 2500)
      (push '(budget > 2500) *BF*))))))
   
   




  
