
# Data algorithms ksi¹¿ka
time=c(5.880903491,11.07186858,10.97330596,1.347022587,8.246406571,
       6.209445585,1.80698152,6.899383984,1.281314168,5.650924025,
       10.25051335,2.036960986,6.800821355,6.932238193,6.800821355,
       2.595482546,8.344969199,5.848049281,4.238193019,5.815195072,
       6.340862423,2.529774127,2.628336756,0.689938398,1.347022587,
       2.694045175,6.078028747,1.21560575,8.410677618,8.509240246)
censor=c(1,1,0,1,1,1,1,0,1,1,0,1,0,1,1,1,0,0,1,1,1,1,1,1,1,0,1,1,0,0)

library("survival")
library("survminer")


survObj = Surv(time, censor)
data("lung")
head(lung)

coxph(Surv(time, censor) ~ logRatio)
coxph(formula = Surv(time, censor) ~ logRatio)

fit <- survfit(Surv(time, status) ~ sex, data = lung)
print(fit)
head(summary(fit))



d <- data.frame(time = fit$time,
                n.risk = fit$n.risk,
                n.event = fit$n.event,
                n.censor = fit$n.censor,
                surv = fit$surv,
                upper = fit$upper,
                lower = fit$lower
)
head(d)

library(SMPracticals)
head(motorette)
plot(motorette$x, motorette$y)
?motorette

# rozwi¹zanie koñcówki rozdzia³u 2
obiekt <- Surv(motorette$y, motorette$cens)
?Surv
mykm <- survfit(obiekt ~ motorette$x, data = motorette)
mykm2 <- survfit(Surv(y,cens) ~ 1, data = motorette)
plot(mykm2, mark.time = TRUE)

autoplot(obiekt)
survdiff(formula = obiekt ~ motorette$x,data = motorette)


?coxph
head(prost)


# DOBRE
my_data <- read.table("./Mgr/prostate_cancer.txt", sep = "" , header = T)
C:/Users/Seba/OneDrive/Dokumenty/Projekty/Rstudio/Mgr/prostate_cancer.txt
return head(my_data)
engine.eval("my_data <- read.table(\"C:/Users/Seba/OneDrive/Dokumenty/Projekty/Rstudio/Mgr/prostate_cancer.txt\", sep = \"\" , header = T)");



# oficjal skrypt
load_file <- function(file_path, separator, has_headers) {
  my_data <- read.table(file_path, sep = separator , header = has_headers)
  return (my_data)
}

my_data <- load_file("C:/Users/Seba/OneDrive/Dokumenty/Projekty/Rstudio/Mgr/prostate_cancer.txt", separator = "" , has_headers = T)
head(my_data)


## kolejny skrypt
# Providing column names
calculate_surv <- function(file_path, separator, has_headers) {
  my_data <- read.table(file_path, sep = separator , header = has_headers)
  return (head(my_data))
}	

my_data <- calculate_surv("./Mgr/prostate_cancer.txt", separator = "" , has_headers = T)
head(my_data)



text <- calculate_surv("./Mgr/prostate_cancer.txt", "" , T)
text

## koniec kolejnego skryptu

## Kozak wersja pliku
load_file <- function(file_path, separator, has_headers) {
  my_data <- read.table(file_path, sep = separator , header = has_headers)
  return (my_data)
}

my_data <- load_file("C:/Users/Seba/OneDrive/Dokumenty/Projekty/Rstudio/Mgr/prostate_cancer.txt", separator = "" , has_headers = T)
head(my_data)

write.table(my_data, file = "C:/Users/Seba/OneDrive/Dokumenty/Projekty/Rstudio/Mgr/output.txt", sep = " ", row.names = TRUE)
## Koniec kozak wersji pliku


Lokalizacja skryptu: "C:/Users/Seba/OneDrive/Dokumenty/Projekty/Rstudio/Mgr/script.r"
Lokalizacja pliku z inputem: "C:/Users/Seba/OneDrive/Dokumenty/Projekty/Rstudio/Mgr/prostate_cancer.txt"
Lokalizacja pliku do którego skrypt wpisuje output: "C:/Users/Seba/OneDrive/Dokumenty/Projekty/Rstudio/Mgr/output.txt"


W cmd wtedy się odpala:
rscript --vanilla "C:/Users/Seba/OneDrive/Dokumenty/Projekty/Rstudio/Mgr/script.r" "C:/Users/Seba/OneDrive/Dokumenty/Projekty/Rstudio/Mgr/prostate_cancer.txt" "C:/Users/Seba/OneDrive/Dokumenty/Projekty/Rstudio/Mgr/output.txt"

Albo trzeba być w odpowiednim folderze C:\Users\Seba\OneDrive\Dokumenty\Projekty\Rstudio\Mgr   i wtedy odpalić:
rscript --vanilla script.r "C:/Users/Seba/OneDrive/Dokumenty/Projekty/Rstudio/Mgr/prostate_cancer.txt" "C:/Users/Seba/OneDrive/Dokumenty/Projekty/Rstudio/Mgr/output.txt"
Albo będąc w którymkolwiek folderze, odpalić:
rscript --vanilla "C:/Users/Seba/OneDrive/Dokumenty/Projekty/Rstudio/Mgr/script.r" "C:/Users/Seba/OneDrive/Dokumenty/Projekty/Rstudio/Mgr/prostate_cancer.txt" "C:/Users/Seba/OneDrive/Dokumenty/Projekty/Rstudio/Mgr/output.txt"


# Dobra wersja skryptu, powyższy opis:
args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
  stop("Sciezka do pliku wejsciowego jest wymagana.", call.=FALSE)
} else if (length(args)==1) {
  # default output file
  args[2] = "C:/Users/Seba/OneDrive/Dokumenty/Projekty/Rstudio/Mgr/output.txt"
}


load_file <- function(file_path, separator, has_headers) {
  my_data <- read.table(file_path, sep = separator , header = has_headers)
  return (my_data)
}

my_data <- load_file(args[1], separator = "" , has_headers = T)
head(my_data)

write.table(my_data, file = args[2], sep = " ", row.names = TRUE)

#
# Open jpeg file
#pdf("KM_plot.pdf")
#jpeg("KM_plot.jpg", width = 849, height = 377)
jpeg("KM_plot.jpg", width = 1698, height = 754)
#jpeg("KM_plot.jpg", width = 1024, height = 768)