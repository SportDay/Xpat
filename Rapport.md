## Fonctionnalités
L'option `-check` est implémentée et fonctionne.

L'option `-search` a un début d'implémentation mais n'est pas encore fonctionnelle.

## Compilation et exécution
La compilation se fait avec `dune`.

Seule l'option `-check` est acceptée.

## Découpage modulaire
`Types` contient tous les types.

`Mouvement` gère le déplacement des cartes.

`Plateau` contient les informations du jeu : le nombre de colonnes, le nombre de registres, etc.

## Remarque
Nous avons utilisé des références pour le plateau (`type settings`) car cela aurait été trop lourd de recréer tous le plateau à chaque déplacement de cartes. 

Signification des nombres utilisés :
- **1024** indique un mouvement vers une colonne vide
- **2048** est un déplacement vers un registre
- **4096** est une erreur
- **-99** représente une case vide dans le registre