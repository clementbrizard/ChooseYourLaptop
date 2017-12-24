;;;
; Fonction de chaînage arrière. Vérifie
; que l'ordi choisi par l'utilisateur
; convient à ses critères. Si non appelle
; une fonction de conseil
;;

(defun chainageArriere (ordiChoisi)
  (let ((but (list 'ordi '= ordiChoisi)))
    
    ; Si l'ordi convient
    (if (verifierFait but)
        (progn
          (format t "~%-------------------- ~% ~%")
          (format t "L'ordi choisi correspond à vos critères.~%~%")
          "Fin de la recherche")
      
      ; Si non afficher les causes
      (progn
        (afficherCauses)
        "Fin de la recherche"))))


;;;
; Vérifie si un fait existe dans la base
; de faits ou peut être obtenu à partir de 
; cette base
;;
        
(defun verifierFait (fait)
  (let ((OK NIL) EC regle)
    
    ; Si le fait est déjà dans la base
    (if (member fait *BF* :test 'equal)
        (setq OK T)
      
      ; Si non
      (progn
        
        ; Récupérer les règles concluant
        ; sur ce fait
        (setq EC (reglesCandidates fait))
        
        ; Chercher parmi les candidates une
        ; règle dont tous les faits sont vérifiés
        (while (and (not OK) EC)
          (setq regle (pop EC))
          (setq OK (verifierRegle regle)))))
    OK))


;;;
; Récupérer les règles concluant un fait 
; Si ce fait est du type (ordi = ...),
; enregistrer les règles candidates. 
; Utile pour la fonction de conseil si 
; aucune règle concluant sur l'ordi visé
; n'a été trouvée
;;

(defun reglesCandidates (but)
  (let (res)
    (dolist (regle *BR* (reverse res))
      
      ; Si la règle conclue sur le fait à vérifier
      (if (equal but (car (last regle)))
          
          ; Si la règle conclue sur un ordi
          (if (equal 'ordi (car (car (last regle))))
              (progn
                
                ; Enregistrer la règle et l'ajouter
                ; à la liste résultat
                (push regle *reglesCandidates*)
                (push (car regle) res))
            
            ; Si non si la règle est cohérente par
            ; rapport à la base de faits, simplement 
            ; l'ajouter à la liste résultat
            (if (regleValide regle) 
                (push (car regle) res)))))))


;;;
; Vérifier si tous les faits d'une règle
; sont présents dans la base de faits. Si
; non enregistre les faits manquants. Utile
; pour la fonction de conseil
;;

(defun verifierRegle (regle)
  (let ((OK T) (premisses (getPremisses regle)) premisse)
    (while (and OK premisses)
      (setq premisse (pop premisses))
      (setq OK (verifierFait premisse))
      (if (not OK)
          (pushnew premisse *faitsManquants* :test 'equal)))
    OK))


;;;
; Vérifie qu'une règle est valide par 
; rapport à la base de faits. Exemple :
; si on vérifie que (prix = 1) on ne veut
; pas enregistrer la règle concluant qu'un 
; PC bureautique a (prix = 1) alors que 
; l'utilisateur a choisi de tester un Mac

(defun regleValide (regle)
  (if (not (equal nil *reglesCandidates*))
      
      ; on récupére les caractéristiques de l'ordi à
      ; tester à partir d'une règle candidate
      (let ((regleCandidate (pop *reglesCandidates*)) OK)
        (cond
         
         ; Si l'utilisateur teste un PC et que la règle 
         ; vaaut pour un Mac, la règle n'est pas valide
         ((and (present '(type = PC) (cadr regleCandidate))
               (present '(type = Mac) (cadr regle)))
          (setq OK NIL))
         
         ; Si l'utilisateur teste un Mac et que la règle 
         ; vaut pour un PC, la règle n'est pas valide
         ((and (present '(type = Mac) (cadr regleCandidate))
               (present '(type = PC) (cadr regle)))          
          (setq OK NIL))
         
         ; Si l'utilisateur teste un PC et que l'usage présent
         ; dans la règle candidate est différent de celui du 
         ; PC à tester, la règle n'est pas valide
         ((and (or (present '(usage = bureautique) (cadr regleCandidate))
                   (present '(usage = gaming) (cadr regleCandidate)))
               (or (present '(usage = bureautique) (cadr regle))
                   (present '(usage = gaming) (cadr regle)))               
               (not (memeUsage (cadr regleCandidate) (cadr regle))))
          (setq OK NIL))
         
         ; Si non la règle est valide
         ((equal nil nil)
           (setq OK T)))
        (push regleCandidate *reglesCandidates*)
        OK)))


;;; 
; Récupère les prémisses d'une règle
;;

(defun getPremisses (regle)
  (cadr (assoc regle *BR*)))


;;;
; Affiche les critères de l'ordi choisi
; par l'utilisateur qui diffèrent des
; critères qu'il a renseignés
;;

(defun afficherCauses ()
  (format t "~%-------------------- ~% ~%")
  (let (fait)
    (format t "L'ordi choisi ne correspond pas à vos critères. Vous pouvez ré-essayer aves les critères suivants : ~%~%")
    (while *faitsManquants*
      (setq fait (pop *faitsManquants*))
      
      ; On n'affiche pas les faits relatifs à l'usage 
      ; et au prix car ils n'ont pas de sens pour 
      ; l'utilisateur étant donné qu'ils ont été déterminés
      ; à partir d'infos plus primaires qu'il a indiquées
      (if (and (not (equal (car fait) 'prix))
              (not (equal (car fait) 'usage)))
          (format t "~a ~%" fait)))
    (format t "~%")))
          
         
         
                                      



      
          
              
                
                
                
                      
    
