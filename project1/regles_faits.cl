(setq *BR* '(
             (R1 ((type = PC) (gaming = 2)) (usage = gaming))
             (R2 ((type = PC) (gaming = 3)) (usage = gaming))
             (R3 ((type = PC) (gaming = 1)) (usage = bureautique))
             (R4 ((type = PC) (usage = bureautique) (budget <= 350)) (prix = 3))
             (R5 ((type = PC) (usage = bureautique) (budget > 350) (budget <= 500)) (prix = 2))
             (R6 ((type = PC) (usage = bureautique) (budget > 500)) (prix = 1))
             (R7 ((type = PC) (usage = bureautique) (prix = 3) (autonomie = 3 )) (ordi = (ASUS EeeBook E402NA-FA045T Bleu)))
             (R8 ((type = PC) (usage = bureautique) (prix = 3) (mémoire = 1 )) (ordi = (ASUS EeeBook E402NA-FA045T Bleu)))
             (R9 ((type = PC) (usage = bureautique) (prix = 3) (autonomie = 2 )) (ordi = (Acer Aspire ES1-523-24HN)))
             (R10 ((type = PC) (usage = bureautique) (prix = 3) (mémoire = 2 )) (ordi = (Acer Aspire ES1-523-24HN)))
             (R11 ((type = PC) (usage = bureautique) (prix = 2) (écran = 3 )) (ordi = (ASUS R702UA-BX169T)))
             (R12 ((type = PC) (usage = bureautique) (prix = 2) (autonomie = 2 )) (ordi = (ASUS R702UA-BX169T)))
             (R13 ((type = PC) (usage = bureautique) (prix = 2) (mémoire = 2 )) (ordi = (ASUS R702UA-BX169T)))
             (R14 ((type = PC) (usage = bureautique) (prix = 2) (mémoire = 3 )) (ordi = (ASUS R556QA-XX061T)))
             (R15 ((type = PC) (usage = bureautique) (prix = 2) (autonomie = 3 )) (ordi = (ASUS R556QA-XX061T)))
             (R16 ((type = PC) (usage = bureautique) (prix = 2) (écran = 2)) (ordi = (ASUS R556QA-XX061T)))
             (R17 ((type = PC) (usage = bureautique) (prix = 1) (autonomie = 2 )) (ordi = (ASUS P1510UA-GQ281R)))
             (R18 ((type = PC) (usage = bureautique) (prix = 1) (écran = 2 )) (ordi = (ASUS P1510UA-GQ281R)))
             (R19 ((type = PC) (usage = bureautique) (prix = 1) (mémoire = 1 )) (ordi = (ASUS P1510UA-GQ281R)))
             (R20 ((type = PC) (usage = bureautique) (prix = 1) (écran = 3 )) (ordi = (Lenovo IdeaPad 320-17IKB 80XM00BUFR)))
             (R21 ((type = PC) (usage = bureautique) (prix = 1) (mémoire = 2 )) (ordi = (Lenovo IdeaPad 320-17IKB 80XM00BUFR)))
             (R22 ((type = PC) (usage = bureautique) (prix = 1) (autonomie = 1)) (ordi = (Lenovo IdeaPad 320-17IKB 80XM00BUFR)))
             (R23 ((type = PC) (usage = gaming) (budget <= 900)) (prix = 3))
             (R24 ((type = PC) (usage = gaming) (budget > 900) (budget <= 1100)) (prix = 2))
             (R25 ((type = PC) (usage = gaming) (budget > 1100)) (prix = 1))
             (R26 ((type = PC) (usage = gaming) (prix = 3)) (ordi = (MSI GP62 7RE-415XFR Leopard Pro)))
             (R27 ((type = PC) (usage = gaming) (prix = 2) (carte_graphique = 2)) (ordi = (MSI GL62VR 7RFX-867FR)))
             (R28 ((type = PC) (usage = gaming) (prix = 2) (puissance = 1)) (ordi = (MSI GL62VR 7RFX-867FR)))
             (R29 ((type = PC) (usage = gaming) (prix = 2) (écran = 2)) (ordi = (MSI GL62VR 7RFX-867FR)))
             (R30 ((type = PC) (usage = gaming) (prix = 2) (autonomie = 3)) (ordi = (MSI GL62VR 7RFX-867FR)))
             (R31 ((type = PC) (usage = gaming) (prix = 2) (carte_graphique = 1)) (ordi = (MSI GP72M 7RDX-871XFR Leopard)))
             (R32 ((type = PC) (usage = gaming) (prix = 2) (puissance = 2)) (ordi = (MSI GP72M 7RDX-871XFR Leopard)))
             (R33 ((type = PC) (usage = gaming) (prix = 2) (écran = 3)) (ordi = (MSI GP72M 7RDX-871XFR Leopard)))
             (R34 ((type = PC) (usage = gaming) (prix = 2) (autonomie = 2)) (ordi = (MSI GP72M 7RDX-871XFR Leopard)))
             (R35 ((type = PC) (usage = gaming) (prix = 1) (carte_graphique = 1)) (ordi = (Gigabyte Sabre 15 - P45G v7 C32W10-FR)))
             (R36 ((type = PC) (usage = gaming) (prix = 1) (autonomie = 2)) (ordi = (Gigabyte Sabre 15 - P45G v7 C32W10-FR)))
             (R37 ((type = PC) (usage = gaming) (prix = 1) (carte_graphique = 2)) (ordi = (Acer Predator Helios 300 G3-572-72MT)))
             (R38 ((type = PC) (usage = gaming) (prix = 1) (carte_graphique = 3)) (ordi = (MSI GE72MVR 7RG-055FR Apache Pro)))
             (R39 ((type = PC) (usage = gaming) (prix = 1) (puissance = 2)) (ordi = (MSI GE72MVR 7RG-055FR Apache Pro)))
             (R40 ((type = PC) (usage = gaming) (prix = 1) (écran = 3)) (ordi = (MSI GE72MVR 7RG-055FR Apache Pro)))
             (R41 ((type = Mac) (budget <= 1500)) (prix = 3))
             (R42 ((type = Mac) (budget > 1500) (budget <= 2500)) (prix = 2))
             (R43 ((type = Mac) (budget > 2500)) (prix = 1))
             (R44 ((type = Mac) (prix = 3) (mémoire = 1) (puissance = 2)) (ordi = (MacBook Air 128 Go)))
             (R45 ((type = Mac) (prix = 3) (mémoire = 1) (écran = 1)) (ordi = (MacBook Air 128 Go)))
             (R46 ((type = Mac) (prix = 3) (mémoire = 1) (autonomie = 3)) (ordi = (MacBook Air 128 Go)))
             (R47 ((type = Mac) (prix = 3) (puissance = 2) (écran = 1) (autonomie = 3)) (ordi = (MacBook Air 256 Go)))
             (R48 ((type = Mac) (prix = 3) (écran = 1) (autonomie = 2)) (ordi = (MacBook 256 Go)))
             (R49 ((type = Mac) (prix = 3) (mémoire = 1) (puissance = 3)) (ordi = (MacBook Pro 13p 128 Go)))
             (R50 ((type = Mac) (prix = 3) (mémoire = 1) (écran = 2)) (ordi = (MacBook Pro 13p 128 Go)))
             (R51 ((type = Mac) (prix = 3) (mémoire = 1) (autonomie = 2)) (ordi = (MacBook Pro 13p 128 Go)))
             (R52 ((type = Mac) (prix = 3) (mémoire = 2) (puissance = 3)) (ordi = (MacBook Pro 13p 256 Go)))
             (R53 ((type = Mac) (prix = 3) (mémoire = 2) (écran = 2)) (ordi = (MacBook Pro 13p 256 Go)))
             (R54 ((type = Mac) (prix = 2) (écran = 1)) (ordi = (MacBook 512 Go)))
             (R55 ((type = Mac) (prix = 2) (puissance = 2)) (ordi = (MacBook 512 Go)))
             (R56 ((type = Mac) (prix = 2) (mémoire = 2)) (ordi = (MacBook Pro 13p Touch Bar 256 Go)))
             (R57 ((type = Mac) (prix = 2) (mémoire = 3) (puissance = 3)) (ordi = (MacBook Pro 13p Touch Bar 512 Go)))
             (R58 ((type = Mac) (prix = 1) (mémoire = 2)) (ordi = (MacBook Pro 15p)))
             (R59 ((type = Mac) (prix = 1) (mémoire = 3)) (ordi = (MacBook Pro 15p Touch Bar)))))
                        
(setq *BF* ())

(setq *ordisManques* ())

(setq *faitsNecessaires* ())

(setq *faitsManquants* ())



