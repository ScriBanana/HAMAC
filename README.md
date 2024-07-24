# HAMAC Routine
The Herds Activity Mapping and Analytical Classification (HAMAC) routine was designed to format, clean, concatenate and classify GPS and accelerometer data gathered from tracking livestock.

## Citation
To cite this routine, use the following:
À faire
Scriban, A., Nabeneza, S., …. Herds Activity Mapping and Analytical Classification.

## Usage
#### A. Nettoyage des données
1. Whole_Dir permet de scanner toutes les données extraites depuis le début de la manip. Il est optionnel pour GPS, nécessaire pour ACT.
2. Faire tourner GPS_Data_Prep et ACT_Data_Prep pour lire les CSV bruts, concaténer les données et faire un premier nettoyage grossier.
3. Summary_Graph permet faire le graph de couverture des données. Marche avec les données GPS brutes et les données nettoyées.
4. Animal_Segmentation permet d'associer à chaque point (GPS et ACT) son animal, en fonction des dates et du numéro de collier.
5. ACT2GPS_attribution attribue à chaque point GPS la moyenne des données accéléro dans une fenêtre de temps donnée.
6. Calcul des distances entre chaque point et des angles à chaque pas avec prepData de moveHMM.
7. Suppression des outliers sur la vitesse.
#### B. Détermination du meilleur modèle de Markov caché sans covariable
8. fitHMM : Fit du HMM sur la base de jeux de paramètres rentrés à la main.
9. Explo_noCovariate : boucle large sur des jeux de paramètres pour éviter les optima locaux (pas dans l'algo de base)

## Authors and acknowledgment
- Daniel
- Serge
- Arthur
- MoveHMM

## License
CCBY, licence à rajouter
_For open source projects, say how it is licensed._
