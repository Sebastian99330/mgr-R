args = commandArgs(trailingOnly=TRUE)

# ten skrypt przyjmuje jeden argument:
# sciezka do pliku z danymi wejsciowymi
# Jak przy wywolaniu skryptu nie podano pierwszego argumentu to rzucamy blad
if (length(args)==0) {
  stop("Sciezka do pliku wejsciowego jest wymagana.", call.=FALSE)
} 

# wczytanie danych
my_data <- read.table(args[1], sep = "" , header = T)

# zapis danych do pliku - nie jest potrzebny poki co
# write.table(data, file = args[2], sep = " ", row.names = TRUE)

library(survival)
library(ggfortify) #plot

# Kaplan Meier plot
# grupuje po treatment
mykm <- survfit(Surv(time, status) ~ treatment, data = my_data)

# otwarcie pliku do ktorego rysujemy wykres KM
jpeg("KM_plot.jpg", width = 1698, height = 754)

# Wykres zawiera dwie linie, po jednej dla kazdej wartosci zmiennej "treatment" czyli 1 i 2
# Sa takze zaznaczone przedzialy ufnosci wokol kazdej linii
autoplot(mykm)

# zamkniecie pliku do ktoego rysujemy wykres
dev.off()


# zapisze do pliku dane tekstowe - statystyki z log-rank oraz z regresji coxa
# ustawiam plik do ktoego bedziemy pisac
sink(args[2])


# Log-rank test
survdiff(Surv(my_data$time, my_data$status) ~ my_data$treatment)

# Cox Proportional Hazards Model
cox <- coxph(Surv(time, status) ~ treatment + age + sh+ size + index, data = my_data)

# wypisanie statystyk
summary(cox)

# zamkniecie pliku
sink()

# otwarcie pliku do ktoego rysujemy wykres z regresji coxa
jpeg("cph_plot.jpg", width = 1698, height = 754)

# funkcja autoplot() nie przyjmuje bezposrednio obiektu cox 
# czyli obiektu, ktory zwraca funkcja coxph,
# wiec trzeba go wpakowac po drodze w funkcje survfit()
autoplot(survfit(cox))

# zamykamy plik do ktorego rysujemy wykres
dev.off()

