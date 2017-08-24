# Le package Barchen

Ce package a été construit spécifiquement pour les besoins de la société **Barchen - Compétences en finance**.  
Les fonctions qu'il contient sont adaptées aux données de l'entreprise, et donc pas forcément applicables à n'importe quel autre dataset.  
Pour l'utiliser c'est très simple, il vous suffit de suivre la procédure suivante.

## Etape 1 - Installation du package

N'étant pas disponible sur le CRAN, l'installation de ce package sur votre machine doit se faire en exécutant la commande suivante : `devtools::install_github("guillaumelf/BarchenPackage")`

## Etape 2 - Chargement du package

Le package est installé sur votre machine, il faut maintenant le charger dans votre environnement de travail à l'aide de la commande suivante : `library(Barchen)`

## Etape 3 - Utilisation des fonctions

Ce package contient principalement des fonctions permettant :

* d'extraire les données stockées sur le server de **Bärchen**
* de calculer des agrégats à partir des données brutes
* d'effectuer des fusions entre les tables
* de procéder à une ACP, puis un clustering sur la base de travail créée
* de tracer des graphes représentant les différents agrégats pour chaque cluster
* de faire de la modélisation

Pour avoir la liste de toutes les fonctions du package, c'est très simple, il vous suffit de cliquer sur l'onglet 'Packages'.   

![img1](https://github.com/guillaumelf/BarchenPackage/blob/master/pictures/rmd1.png)

Puis sur le package Barchen.  

![img2](https://github.com/guillaumelf/BarchenPackage/blob/master/pictures/rmd2.png)

Vous avez une liste de fonctions qui apparaît. Vous pouvez consulter l'aide de la fonction en cliquant directement dessus.  

![img3](https://github.com/guillaumelf/BarchenPackage/blob/master/pictures/rmd3.png)

Ou bien en tapant dans la console le nom de la fonction précédé d'un '?'  

![img4](https://github.com/guillaumelf/BarchenPackage/blob/master/pictures/rmd4.png)


