# Applications Shiny data-acess et dataset-archive (*en cours de développemnent*)

Les applications en ligne sont accessibles à cette [adresse](https://data-snot.cnrs.fr/data-access/) et [celle-ci](https://data-snot.cnrs.fr/dataset-archive/)

## Organisation de l'arborescence

```
.
├── app.R
├── confConnexion.R
├── datatype_couleur.csv
├── dependanciesCSS.R
├── dependanciesGraph.R
├── dependanciesQuery.R
├── dependancies.R
├── Fig
├── mod_amelioration.R
├── mod_carto.R
├── mod_extractiondataset.R
├── mod_extraction.R
├── sous_module
├── sql
├── translation
└── www
```

avec :

- app.R : Fichier principal contenant le lancement des modules et dépendances
- mod_amelioration.R : Module pour rajouter améliorer l'application sur la forge
- mod_carto.R : Module "Welcome" pour présenter la cartographie des sites, des stations et les données disponibles
- mod_extractiondataset.R : Module pour extraire les jeux de données selon le standard PIVOT/OZCAR
- mod_extraction.R : Module pour visualiser et extraire les données. *Ce module devra être éclaté en plusieurs sous-modules pour faciliter sa maintenance*
- confConnexion.R : Paramètres de connexion vers les bases de données *Fichier sensible, ne pas versionner*
- dependanciesCSS : Dépendances sur des aspects de styles pour l'appli (pour les figures, pour l'appli...)
- dependanciesGraph : Ensemble de fonctions associées à la création des graphiques dans l'application
- dependanciesQuery : Ensemble de fonctions pour lancer des requêtes vers la bdd
- dependanciesData : Ensemble des fonctions pour manipuler les données
- dependancies : Chargement de toutes les dépendances
- sous_module : Répertoire contenant les sous-modules du module mod_extraction.R (*en cours de développement, non utilisé*)
- sql : Répertoire contenant les requêtes sql pour construire les vues matérialisées utilisées 
- translation : Répertoire contenant le fichier csv pour internationaliser l'application avec [Shiny.i18n](https://github.com/Appsilon/Shiny.i18n)
- www : Répertoire contenant les figures, les fichiers markdown, les csv, les css...

## Pour déployer l'application

Modifier les paramètres de connexion vers la base de données dans `dependanciesQuery.R` et lancer :

```
shiny::runApp()
```

## Pour administrer et mettre à jour l'application

Consulter la [doc en ligne](https://sourcesup.renater.fr/www/si-snot/5_Deploiement_shiny.html)