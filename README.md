# UVP lights quality control

UVP counts particles on an image taken in the water and we recently discovered that for some images, those numbers were lower than expected, by about half compared to be neighbouring images. This was diagnosed as being caused by a fault in the lights where only one light of the two illuminates the water volume, leading to ~half the number of particles.

The goal of this code it to automatically detect these cases, to flag them, and to correct the files output by zooprocess to import the correct data into EcoTaxa.

## Code structure

`0.get_uvpb_profiles.R` lists and read all profiles from the server in Villefranche.

`1.subset_profiles_for_tests.R` reads some of them, that represent various levels of faulty lights, and stores them in `test_data.csv.gz` so that they can be used as benchmark for automatic filtering of faulty lights.

Each `filter-***.R` file implements a filtering function, that take a profile of number of objects per UVP frame as input and attempts to detect the cases when this number suddenly drops.  
Some filters could also be implemented in python (and possibly MATLAB) and be used in the same way. Currently a version of a simple quantile based filter is implemented in standalone Jupyter notebooks.

`test_filter.R` reads the test data, loads a filtering function, applies the filter and plots the results in `test_plots`. The blue line is the decision threshold between images considered to be faulty (red points) and those considered to be correct (black points).

`app.R` is a dynamic web application that allows to browse all projects stored on the server in Villefranche, apply a filtering function with user-tweakable parameters, and view the result. The app is live at http://obs-vlfr.fr/data/view/zoo/uvp_qc/.

## TODOs

- [x] Select test data for various conditions
- [x] Implement some filtering functions
- [ ] Implement the `density_function`, `symmetric_anomaly`, `moving_k_means` filters in the same way as the others (i.e., as a function)
- [ ] Make it easier to prototype filters in python and use them in `test_filter.R`
- [ ] Improve the speed of the density-based filter
- [ ] Find other, better filters?
- [ ] Use an existing filtering function, rather than custom code, in the visualisation app
- [ ] Simplify the user interface (number and meaning of the filtering parameters)
- [ ] Allow the user to flag a filtering are correct or incorrect in the visualisation app
- [ ] Store the filtering parameters when a profile is flagged
- [ ] Write the modified `_dat.txt` file when the filtering of a profile is flagged as good.
- [ ] Match EcoTaxa images to be able to remove them
