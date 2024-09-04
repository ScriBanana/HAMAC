# HAMAC Routine
The Herds Activity Mapping and Analytical Classification (HAMAC) routine prepares, classifies into activities and produces variouss representations of GPS and accelerometer data gathered from tracking livestock to train behaviour models and exploit the resulting data in diverse ways. It makes use of parallel computing and leverages the Hidden Markov Model framework to classify activities. The later part relies heavily on the moveHMM R package

## Citation
To cite this routine, please use the following:
Scriban, A., Nabeneza, S., Cornelis, D., Salgado, P., 2024. Herds Activity Mapping and Analytical Classification. Cirad, Montpellier, France. https://gitlab.cirad.fr/selmet/hamac.

## Usage
The routine is divided into functional blocks, each containing individual script pages, each corresponding to a unitary operation. The order of the groups and script pages is sequential, yet all elements are optional. Intermediate data saves within each group allow modular execution, as required. Here is a breakdown of what each group and individual script does:

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

## License
This code is made open-source under the [CeCILL-B](https://cecill.info/licences.en.html) licence.

## Relevant publication and datasets
Scriban, A., Nabeneza, S., Cornélis, D., Delay, É., Vayssières, J., Cesaro, J.-D., Salgado, P., 2024. GPS-based hidden Markov models to document pastoral mobility in the Sahel. Sensors. Submitted.
Scriban, Arthur; Nabeneza, Serge; Salgado, Paulo, 2024, "Mobility, behaviour and land-use data from GPS tracking cattle herds in Sahel agropastoral systems", https://doi.org/10.18167/DVN1/GHJKQO, CIRAD Dataverse
