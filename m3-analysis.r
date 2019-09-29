#https://forecasters.org/data/m3comp/M3C.xls
M3 <- read_excel("(...)/M3C.xls",sheet = 3)

min(M3$N)
max(M3$N)
mean(M3$N)
