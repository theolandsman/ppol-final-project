library(readr)
library(readxl)
library(smacof)
library(ggplot2)
AlaskaCVR <- read_excel("~/Documents/AlaskaCVR.xlsx", na = "under")
AlaskaCVR$Sanders<-match(1,AlaskaCVR[,7:11],)
vector<-c(AlaskaCVR[1,7:11])

