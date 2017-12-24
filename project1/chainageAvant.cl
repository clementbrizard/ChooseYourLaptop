;;;
; Fonction de chaînage avant. Cherche les 
; ordis possibles à partir de la base de
; faits. Si aucun ordi ne convient, appelle 
; une fonction de conseil
;;

(defun chainageAvant ()
  (let (EC (BR *BR*) regleCourante results)
    (loop
      (format t "~%")
      ; Un ordi a-t-il été trouvé ?
      (if (not (equal nil (ordiTrouve)))
          (pushnew (ordiTrouve) results :test 'equal))
      ; pushnew car plusieurs règles mènent aux mêmes 
      ; ordis et pourraient être satisfaites en même
      ; temps
      
      ; Si non chercher les règles déclenchables
      (dolist (regle BR)
        (when (declenchable regle)
          (push regle EC)
          (setq BR (remove regle BR))))
      
      ; S'il reste des règles déclenchables, appliquer
      ; celle en tête de liste
      (if EC
          (progn
            (setq regleCourante (pop EC))
            (push (caddr regleCourante) *BF*))
        
        ; Si non 
        (progn
          ; Si des ordis ont été trouvés, les afficher
          (if (not (equal nil results))
              (progn
                (format t "~%-------------------- ~% ~%")
                (format t "Ordi(s) correspondant à vos critères : ~%")
                (afficherListe results)
                (format t "~%"))
            ; Si non appel d'une fonction de conseil
              (afficherOrdisManques))
          (return "Fin de la recherche"))))))


;;;
; Recherche dans la base de faits
; si un ordi a été trouvé. Retourne 
; le nom de l'ordi le cas échéant
;;

(defun ordiTrouve ()
  (dolist (fait *BF*)
    (if (equal 'ordi (car fait))
        (return (car(cddr fait))))))


;;;
; Recherche les règles déclenchables à partir de la 
; base de faits. Si une règle n'est pas déclenchable,
; appelle une fonction qui teste si cette règle concluait
; sur un ordi conforme au type (PC, Mac) et à l'usage 
; (bureautique, gaming) en cas de PC. S oui, appelle une
; fonction qui sauvegarde l'ordi manqué et les faits manquants
; responsables de l'échec.
;;

(defun declenchable (regle)
  (let ((OK T) faitsManquants)
    (dolist (fait (cadr regle))
      (if (not (present fait *BF*))
          (progn
            (setq OK NIL)
            (push fait faitsManquants))))
    ; La règle courante est non déclenchable, conclue-
    ; elle sur un ordi conforme à la base de faits ?
    (if (regleManqueeValide regle faitsManquants)
        ; Si oui on enregistre les raisons de l'échec
        ; pour les restituer à la fin si aucun ordi 
        ; n'a été trouvé
        (saveRegleManquee regle (car faitsManquants)))  
    OK))


;;;
; Teste si la règle manquée conclue sur un ordi 
; cohérent avec une base de faits donnée
;;

(defun regleManqueeValide (regle faitsManquants)
  (let ((ccl (car (last regle))))
    ; Si la règle conclue bien sur un ordi
    (if (and (equal 'ordi (car ccl))
             ; Et qu'il y a seulement un fait manquant 
             ; (on n'affiche pas les ordis manqués à 
             ; cause de plus d'un fait manquant)
             (equal (length faitsManquants) 1)
             ; Si l'ordi n'est pas déjà dans la 
             ; liste des ordis manqués
             (not (present ccl *ordisManques*)))
        ; Si la règle conclue sur un PC
        (if (present '(type = PC) (cadr regle))
            ; Si l'usage du PC est le même que celui
            ; obtenu à partir de la base passée en
            ; paramètre
            (if (memeUsage (cadr regle) *BF*)
                ; La règle est valide
                T)
          ; Si non la règle conclue sur un Mac, pas de
          ; vérification supplémentaire à effectuer, la
          ; règle est valide
          T))))
  

;;;
; Enregistre les infos nécessaires pour,
; si aucun ordi n'a été trouvé, montrer à 
; l'utilisateur les ordis qu'il aurait pu
; avoir si ses critères avaient été 
; légèrement différents
;;

(defun saveRegleManquee (regle faitManquant)
  (if (not (present (car (last regle)) *ordisManques*))
      (progn
        (push (cadr regle) *faitsNecessaires*)
        (push (car (last regle)) *ordisManques*)
        (push faitManquant *faitsManquants*))))


;;;
; Fonction d'affichage à l'utilisateur
; des ordis qu'il aurait pu avoir, des
; critères qui étaient nécessaires, et
; ceux qu'il lui a manqués
;;

(defun afficherOrdisManques ()
  (let (faitManquant budget)
    (format t "-------------------- ~% ~%")
    (format t "Aucun ordi ne correspond à vos critères :~% ~%")
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


;;;
; À partir du niveau de prix (1, 2 ou 3)
; du type de l'ordi (PC ou Mac) et de l'usage
; si PC, renvoie le positionnement sur le
; budget. Nécessaire au moment de l'affichage
; des faits manquants
;;

(defun budgetFromPrix (faitPrix)
  (let (type usage (prix (car (last faitPrix))) res)
    
    (if (present '(type = Mac) *BF*)
        (setq type 'Mac)
      (if (present '(usage = bureautique) *BF*)
          (setq usage 'bureautique)
        (setq usage 'gaming)))
    (cond
     
     ; Si l'utilisateur souhaitait un PC bureautique
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
     
     ; Si l'utilisateur souhaitait un PC gaming
     ((equal usage 'gaming)
      (cond
       ((equal prix '1)
        (push '(budget > 1100) res))
       ((equal prix '2)
        (progn
          (push '(budget > 900) res)
          (push '(budget <= 1100) res)))
       ((equal prix '3)
        (push '(budget <= 900) res))))
     
     ; Si l'utilisateur souhaitait un Mac
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
       



  
  




     


  



