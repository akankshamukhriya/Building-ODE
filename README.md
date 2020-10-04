# Details of this work are under construction and this work is under process.

We here submit the Data and Code files, necessary to interpret, replicate, and build on the findings reported in the paper (under review for publication) titled as "Building Outlier Detection Ensembels by Selective Parameterizations of Heterogeneous Methods".

Two zip files are in this repository, each of Data and Code.

Data File :
The Data.zip file contains eleven benchmark datasets, with the pre-processed versions, as used in the paper (under review for publication). All these files are in form of Comma Separated Values (.csv). 
The benchmark datasets used in the paper (under review for publication) are: ALOI, PenDigits, ANN-Thyroid, SatImage, PageBlocks, OpticalDigits, SpamBase, Waveform, InternetAds, Pima, and Arrhythmia. For some of these we use original versions given in UCI repository, while for others we use some versions given in DAMI repository: https://www.dbs.ifi.lmu.de/research/outlier-evaluation/DAMI/. For all the versions, we present details in the paper (under review for publication), and the same versions are attached here.


Following are the list of .R files required to run the ensemble selection approach:

1. Base Outlier Detection Methods: 
In our work, we use six base detectors: Average k-NN, LOF, KDEOS, COF, LDOF, and LDF detectors. Other than for Average k-NN, we use ELKI framework to execute rest five detectors on eleven benchmark datasets, used in the manuscript (under review for publication). ELKI is an open source software, and the link, as provided in the paper (under review for publication), is: https://elki-project.github.io/algorithms/

2. Existing Member Selection Approaches for Outlier Detection Ensembels:
Following are the state-of-art member selection methods for outlier detection ensembles, which we use in this work to validate performance of proposed selector, described as corresponding .R files

(1) Vselect.R: Vertical Selection Method for Ensembles for Outlier Detection [Rayana et. al. 2015]

(2) Boostselect.R:  Unsupervised Boosting-based Member Selection for Outlier Detection [Campos et. al. 2018]

3. 


How to run?
