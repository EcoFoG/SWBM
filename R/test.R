# test

DataClim <- read.csv("./data/DataClim.csv", sep = ";", dec = ".", header = T, stringsAsFactors = F)

SWB_model(DataClim)

# Not working because model generated interceptions > precipitations... Like, a lot of them.
