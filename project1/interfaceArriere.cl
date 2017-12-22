(defun beginArriere()
  (let (type budget ordiChoisi)
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
    
    (setq ordiChoisi (choisirOrdi))
   
    (chainageArriere ordiChoisi)))

(defun choisirOrdi ()
  (let (choix)
    (format t "Quel ordinateur pensez-vous choisir : ~%~%")
    
    (format t "Ordis plutôt orientés bureautique : ~%~%")
   
    (format t "1 : ASUS EeeBook E402NA-FA045T Bleu ~%")
    (format t "2 : Acer Aspire ES1-523-24HN ~%")
    (format t "3 : ASUS R702UA-BX169T ~%")
    (format t "4 : ASUS R556QA-XX061T ~%")
    (format t "5 : ASUS P1510UA-GQ281R ~%")
    (format t "6 : Lenovo IdeaPad 320-17IKB 80XM00BUFR ~%~%")
    
    (format t "Ordis plutôt orientés gaming : ~%~%")
    
    (format t "7 : MSI GP62 7RE-415XFR Leopard Pro ~%")
    (format t "8 : MSI GL62VR 7RFX-867FR ~%")
    (format t "9 : MSI GP72M 7RDX-871XFR Leopard ~%")
    (format t "10 : Gigabyte Sabre 15 - P45G v7 C32W10-FR ~%")
    (format t "11 : Acer Predator Helios 300 G3-572-72MT ~%")
    (format t "12 : MSI GE72MVR 7RG-055FR Apache Pro ~%~%")
    
    (format t "Macs : ~%~%")
    
    (format t "13 : MacBook Air 128 Go ~%")
    (format t "14 : MacBook Air 256 Go ~%")
    (format t "15 : MacBook 256 Go ~%")
    (format t "16 : MacBook Pro 13p 128 Go ~%")
    (format t "17 : MacBook Pro 13p 256 Go ~%")
    (format t "18 : MacBook 512 Go ~%")
    (format t "19 : MacBook Pro 13p Touch Bar 256 Go ~%")
    (format t "20 : MacBook Pro 13p Touch Bar 512 Go ~%")
    (format t "21 : MacBook Pro 15p ~%")
    (format t "22 : MacBook Pro 15p Touch Bar ~%~%")
    
    (format t "Votre choix : ")
    
    (setq choix (read))
    
    (cond
     ; bureautique
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
      (setq choix '(Lenovo IdeaPad 320-17IKB 80XM00BUFR)))
     ; gaming
     ((equal choix '7)
      (setq choix '(MSI GP62 7RE-415XFR Leopard Pro)))
     ((equal choix '8)
      (setq choix '(MSI GL62VR 7RFX-867FR)))
     ((equal choix '9)
      (setq choix '(MSI GP72M 7RDX-871XFR Leopard)))
     ((equal choix '10)
      (setq choix '(Gigabyte Sabre 15 - P45G v7 C32W10-FR)))
     ((equal choix '11)
      (setq choix '(Acer Predator Helios 300 G3-572-72MT)))
     ((equal choix '12)
      (setq choix '(MSI GE72MVR 7RG-055FR Apache Pro)))
     ; Macs
     ((equal choix '13)
      (setq choix '(MacBook Air 128 Go)))
     ((equal choix '14)
      (setq choix '(MacBook Air 256 Go)))
     ((equal choix '15)
      (setq choix '(MacBook 256 Go)))
     ((equal choix '16)
      (setq choix '(MacBook Pro 13p 128 Go)))
     ((equal choix '17)
      (setq choix '(MacBook Pro 13p 256 Go)))
     ((equal choix '18)
      (setq choix '(MacBook 512 Go)))
     ((equal choix '19)
      (setq choix '(MacBook Pro 13p Touch Bar 256 Go)))
     ((equal choix '20)
      (setq choix '(MacBook Pro 13p Touch Bar 512 Go)))
     ((equal choix '21)
      (setq choix '(MacBook Pro 15p)))
     ((equal choix '22)
      (setq choix '(MacBook Pro 15p Touch Bar))))
     
    choix))
  
  
  







