# All besides falcons:
koty_ptaki[-(11:12), ]
koty_ptaki[c(1:10, 13), ]

# Only cats:
koty_ptaki[1:7, ]
koty_ptaki[koty_ptaki$druzyna == "Kot", ]

# Only two columns:
koty_ptaki[, c("waga", "predkosc")]
koty_ptaki[, c(2, 4)]

# All besides last column:
koty_ptaki[, 1:6]
koty_ptaki[, -7]

# Lightweight animals:
koty_ptaki[koty_ptaki$waga < 100, 1:4]
