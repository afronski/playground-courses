# Text file:
auta2012 <- read.table(file="http://biecek.pl/MOOC/dane/auta2012mini.csv",
                       sep=";", dec=",", header=TRUE)

# Excela file:
library(gdata)
auta2012 <- read.xls("http://biecek.pl/MOOC/dane/auta2012mini.xls", sheet=1)

# RDA file (it loads whole workspace, with variables):
load(url("http://biecek.pl/MOOC/dane/auta2012mini.rda"))
