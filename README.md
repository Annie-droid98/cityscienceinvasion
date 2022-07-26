# Bachelorthesis
You have to read in these files first : 1. 10kmGridIrelandandGB.R to download the EAA reference grids and prepare them for the models
                               2. Skript Sattelitedata.R, download the CLC 2018 data, recategories and compute the proportion of each landcover type for each grid

Than you have to download the csv EichhörnchenPublisheruntil100obs.csv and EichhörnchenPublisher(Mammalia,Amphibia,Reptilia,Aves).csv
Now you can execute the Skript_Counting_10kmcitizenscience_all_pub.R to count the different species for the model using citizen science and the model for all publishers or SkriptVertebrata.R for the models using "Vertebrata" observation as a reference taxon for normalisation

Use the Skript1.25Models.R to model the glmmms for the models using mammalia observations as a reference taxon for normalisation and the models using "vertebrata" as a reference taxon for normalisation
