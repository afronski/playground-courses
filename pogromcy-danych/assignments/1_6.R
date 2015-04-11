# Text file:
write.table(koty_ptaki, file="dane_o_kotach.csv", sep=",", dec=".")

# Excela file:
library(xlsx)
write.xls(koty_ptaki, file="koty_ptaki.xlsx", sheetName="New Sheet")

# RDA file:
save(koty_ptaki, file="koty_ptaki.rda")
load("koty_ptaki.rda")
