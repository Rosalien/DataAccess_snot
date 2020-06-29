# Applications Shiny data-access et dataset-archive

* [data-access](https://data-snot.cnrs.fr/data-access/) pour visualiser et extraire les données
* [dataset-archive](https://data-snot.cnrs.fr/dataset-archive/) pour la création d'archives accompagnées de métadonnées

## Organisation de l'arborescence

```
.
├── app.R
├── confConnexion.R
├── dependanciesCSS.R
├── dependanciesData.R
├── dependanciesGraph.R
├── dependanciesQuery.R
├── dependancies.R
├── mod_carto.R
├── mod_extractiondataset.R
├── mod_extraction.R
├── mod_about.R
├── sql
├── translation
└── www
```

avec :

- app.R : Fichier principal pour construire les applications (lancement des modules et dépendances)
- mod_carto.R : Module "Welcome" de l'application data-access : pour présenter la cartographie des sites, des stations et les données disponibles
- mod_extraction.R : Module Access-data de l'application data-access : pour visualiser et extraire les données. *Ce module devra être éclaté en plusieurs sous-modules pour faciliter sa maintenance*
- mod_extractiondataset.R : Module principal de l'application dataset-archive : extrait les jeux de données selon le standard PIVOT/OZCAR ou Zenodo
- confConnexion.R : Paramètres de connexion vers les bases de données *Fichier sensible, ne pas versionner*
- dependanciesCSS.R : Dépendances sur des aspects de styles pour l'appli (pour les figures, pour l'appli...)
- dependanciesGraph.R : Ensemble de fonctions associées à la création des graphiques dans l'application
- dependanciesQuery.R : Ensemble de fonctions pour lancer des requêtes vers la bdd
- dependanciesData.R : Ensemble des fonctions pour manipuler les données
- dependancies.R : Chargement de toutes les dépendances
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

-----