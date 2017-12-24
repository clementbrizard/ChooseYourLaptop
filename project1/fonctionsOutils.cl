;;;
; Teste si un fait est présent 
; dans une base de faits
;;

(defun present (fait BF)
  (if (member fait BF :test 'equal)
      T
    NIL))


;;;
; Vide toutes les variables globales
;;

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
    (pop *reglesCandidates*))
  "Vous pouvez commencer une recherche")


;;;
; Détermine si l'usage renseigné par
; une liste de faits est le même que 
; celui indiqué par une autre base
; de faits. Retourne NIL en cas de 
; différence et si les deux listes 
; ne renseignent pas l'usage.
;;

(defun memeUsage (faits base)
  (if (and (equal (getUsage faits) (getUsage base))
           (not (equal nil (getUsage faits))))
      T
    NIL))


;;;
; Récupère l'usage présent dans
; une liste de faits. Retourne NIL
; si l'usage n'est pas renseigné
;;

(defun getUsage (faits)
  (let (usage)
    (dolist (fait faits usage)
      (if (equal (car fait) 'usage)
          (setq usage fait)))))


;;;
; Affiche une liste d'éléments
; en sautant une ligne entre 
; chaque élément
;;

(defun afficherListe (liste)
  (dolist (elem liste)
    (print elem))
  (format t "~%"))

