# Minesweeper
 kilian.hett@labri.fr

 sharelatex : https://fr.sharelatex.com/project/58c2a9df183ae9ab43e438e2
 trello : https://trello.com/b/op8EFZSj/projet-minesweeper

circo -Tpng graph.gv -o graphcirco.png
dot -Tpng graph.gv -o graphdot.png


1. Description des fonctionnalités
==================================
Les fonctions implémentées durant ce projet permettent la création d'un plateau de jeu de démineur suivant un format de graphe donné via un fichier, et les fonctionnalités qui y sont associées, c'est-à-dire que l'on peut miner le plateau aléatoirement, découvrir des cases, leur associer des flags, accéder aux nombres de voisins minés d'une case découverte, et que l'on peut perdre ou gagner une partie selon les règles communes du jeu.


2. liste des fonctions importantes
==================================

   //fonction créant un plateau à partir d'un graphe
   //arguments: file_name fichier décrivant le graphe
   >board-create file_name

-----------------------------------------------------------------------------------------------------------------

   //fonction permettant de connaître le nombre de mines voisines d'une case, si cette dernière est découverte
   //arguments: b le plateau
                c_id la case
   >board-get-n-mines-neighbors b c_id

-----------------------------------------------------------------------------------------------------------------

   //fonction permettant de mettre un "flag" sur une case afin de montrer si le joueur pense qu'elle est ou non minée
   //arguments: b le plateau
                c_id la case
                flag une chaîne de caractère ("none", "mined" ou "safe")
   >cell-set-flag b c_id flag
     
-----------------------------------------------------------------------------------------------------------------

   //fonction générant aléatoirement un nombre donné de mines sur un plateau
   //arguments: b le plateau
                nb_mines le nombre de mines à générer
   >board-generate-mines b nb_mines

-----------------------------------------------------------------------------------------------------------------
     
   //fonction découvrant une case du plateau de jeu (et possiblement d'autres de manière récursive)
   //arguments: b le plateau
                c_id la case
   > board-uncover-cell b c_id

-----------------------------------------------------------------------------------------------------------------

   //fonction de fin de jeu
   //arguments: b le plateau
                won un booléen indiquant si la partie a été gagnée ou perdue
   >game-end b won

-----------------------------------------------------------------------------------------------------------------

   //fonction appliquant la stratégie associée aux coups non déterministes
   //arguments: b le plateau
                
   >limited_search_strategy b

-----------------------------------------------------------------------------------------------------------------
 