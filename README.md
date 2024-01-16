# HAMAC Routine
The Herds Activity Mapping and Analytical Classification (HAMAC) routine was designed to format, clean, concatenate and classify GPS and accelerometer data gathered from tracking livestock.

## Description
_Let people know what your project can do specifically. Provide context and add a link to any reference visitors might be unfamiliar with. A list of Features or a Background subsection can also be added here. If there are alternatives to your project, this is a good place to list differentiating factors._

## Visuals
_Depending on what you are making, it can be a good idea to include screenshots or even a video (you'll frequently see GIFs rather than actual videos). Tools like ttygif can help, but check out Asciinema for a more sophisticated method._

## Usage
1. Whole_Dir permet de scanner toutes les données extraites depuis le début de la manip. Il est optionnel pour GPS, nécessaire pour ACT.
2. Faire tourner GPS_Data_Prep et ACT_Data_Prep pour lire les CSV bruts, concaténer les données et faire un premier nettoyage grossier
3. Summary_Graph permet faire le graph de couverture des données
4. Animal_Segmentation permet d'associer à chaque point (GPS et ACT) son animal, en fonction des dates et du numéro de collier.
5. ACT2GPS_attribution attribue à chaque point GPS la moyenne des données accéléro dans une fenêtre de temps donnée
6. Calcul des distances entre chaque point et des angles à chaque pas avec prepData de moveHMM
7. Suppression des outliers sur la vitesse
8.

## Authors and acknowledgment
- Daniel
- Serge
- Arthur
- MoveHMM

## License
_For open source projects, say how it is licensed._
