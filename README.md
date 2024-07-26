# HAMAC Routine
The Herds Activity Mapping and Analytical Classification (HAMAC) routine was designed to format, clean, concatenate and classify GPS and accelerometer data gathered from tracking livestock to train behaviour models and exploit the resulting data in diverse ways.

## Citation
To cite this routine, use the following:
_À faire_
Scriban, A., Nabeneza, S., …. Herds Activity Mapping and Analytical Classification. https://gitlab.cirad.fr/selmet/hamac

## Usage
#### A. Data cleaning and formatting
A1. Scans a directory to load all GPS and Accelerometer data files from tracking
A2. Cleans and formats GPS data
A3. Adds additional environmental variables to the GPS data
A4. Cleans and formats accelero data
A5. Graphical representation of the temporal data coverage

#### B. Data association and preparation
B1. Associates GPS data to the animals
B2. Associates GPS and accelerometer data
B3. Computes trajectory metrics to fit HMMs on

#### C. Model training
C1. Preliminary script to load the fit function and the data and prime the log
C2. Runs a single fitting process on a given parameter set
C3. Parameter space exploration and sampling of fits to ensure the optimal likelihood

#### D. Exploiting model outputs
D1. Quality of fit and trajectory metrics
D2. Plots of the density of probability for trajectory metrics
D3. Simulating virtual mobility

#### E. Exploiting activity data
E1. Preliminary script to merge GPS data and activity from a model
E2. Graphical representations of the temporal repartition of activities
E3. Diverse behavioural metrics
E4. Associates position and activity data to land-use data
E5. Repartition of activities over land-use and seasons


## Authors and acknowledgment
- Daniel
- Serge
- Arthur
- MoveHMM

## License
CCBY, licence à rajouter en sus
_For open source projects, say how it is licensed._

## Relevant publication and datasets
Scriban, A., Nabeneza, S., Cornélis, D., Delay, É., Vayssières, J., Cesaro, J.-D., Salgado, P., 2024. GPS-based hidden Markov models to document pastoral mobility in the Sahel. Sensors. Submitted.
Scriban, Arthur; Nabeneza, Serge; Salgado, Paulo, 2024, "Mobility, behaviour and land-use data from GPS tracking cattle herds in Sahel agropastoral systems", https://doi.org/10.18167/DVN1/GHJKQO, CIRAD Dataverse
