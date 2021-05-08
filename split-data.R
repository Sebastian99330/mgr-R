setwd("C:\\Users\\Seba\\OneDrive\\Dokumenty\\Projekty\\Rstudio\\Mgr")
my_data <- read.table("prostate_cancer.txt", sep = "" , header = T)

sample(1:4, size = 20, replace = TRUE)

liczba_czesci <- 4
liczba_wierszy <- nrow(my_data)
split(my_data, rep(1:liczba_czesci), 
                   each=liczba_wierszy/liczba_czesci, length.out=liczba_wierszy))

# sample

#split(my_data, 4, each=liczba_czesci, length.out=liczba_wierszy))