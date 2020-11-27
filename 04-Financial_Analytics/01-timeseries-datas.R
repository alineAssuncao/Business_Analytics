# Análise de Séries Temporais 

# A Linguagem R possui diversos pacotes para criar, manipular e visualizar dados de séries temporais,
# essenciais quando se trabalha com Financial Analytics.

# Objetos do tipo data.frame em R, são excelentes para armazenar dados, mas não são eficientes para lidar com 
# dados de séries temporais. Abaixo uma lista dos principais pacotes e objetos usados para análise de séries temporais em R.

# Objetos de Séries Temporais
# Objeto fts (Pacote fts) 
# Objeto its (Pacote its)
# Objeto irts (Pacote tseries)
# Objeto timeSeries (Pacote timeSeries) 
# Objeto ti (Pacote tis)
# Objetos ts e mts (Pacote stats)
# Objeto zoo (Pacote zoo)
# Objeto xts (Pacote xts)

# xts = eXtensible Time Series
# xts é um objeto estendido do pacote zoo
# Índice (dados do tipo data) + Matriz

# Dados de séries temporais, são basicamente matrizes de dados, cujo índice é uma data.
# Representamos esta data em R com diferentes objetos

# Objeto chron (Pacote chron)
# Objeto Date (Pacote base)
# Objeto yearmon (Pacote zoo)
# Objeto yearqtr (Pacote zoo)
# Objeto POSIXct (Pacote base)
# Objeto POSIXlt (Pacote base)
# Objeto timeDate (Pacote timeDate)

getwd()
#### Bonus - Trabalhando com Datas em R #####

# Datas em R são armazenados como número de dias a partir de 1/1/1970
# Objetos do tipo Data são usados para representar datas e não horários dentro de uma data (ao longo do dia)
my.date = as.Date("2017/1/1")
my.date
class(my.date)
as.numeric(my.date)
myDates = c("2016-12-20", "2017-01-20")
as.Date(myDates)

# Especificando o formato de entrada dos dados
as.Date("1/1/1970", format = "%m/%d/%Y")
as.Date("January 1, 1970", format = "%B %d, %Y")
as.Date("01JAN70", format = "%d%b%y") 

# Função format()
format(my.date, "%b %d, %Y") 
as.Date("Jan 1970", format = "%b %Y") 

# Data considerando o número de dias
as.Date(32500, origin = as.Date("1900-01-01")) 

# Extraindo um valor específico e transformando os dados
myYear = format(my.date, "%Y")
myYear
class(myYear)
as.numeric(myYear)
as.numeric(format(my.date, "%Y")) 
weekdays(my.date)
months(my.date)
quarters(my.date)

# Operações com datas
my.date
my.date + 1
my.date - 1
my.date + 31

# Comparações Lógicas
my.date
my.date1 = as.Date("1980-01-01")
my.date1 > my.date 

# Diferença entre datas
diff.date = my.date1 - my.date
diff.date 
class(diff.date)
as.numeric(diff.date)
my.date + diff.date 

# Criando sequências de datas
my.dates = seq(as.Date("2015/1/1"), as.Date("2016/12/31"), "3 months")
head(my.dates)
tail(my.dates) 
my.dates = seq(from = as.Date("2015/1/1"), by = "3 months", length.out = 61)
my.dates

# Plot de Objetos do Tipo Data
rint = round(runif(500)*365)
startDate = as.Date("2016-01-01")
myDates = startDate + rint
head(myDates)
class(myDates)

# Histograma
hist(myDates, breaks = "months", freq = TRUE, 
     main = "Distribuição de Datas por Mês", col = "slateblue1", xlab = "", format = "%b %Y", las = 2)

# Objeto Posix
# Classe POSIXct representa datas no formato de tempo em número de segundos desde 01-01-1970 (semelhante ao objeto Date, mas com suporte aos tempos ao longo do dia)
# Classe POSIXlt representa datas no formato de tempo com uma lista nomeada de segundos, minutos, horas, dias, meses e anos
myDateTimeStr = "2016-12-15 11:27:04"

# Criando objetos POSIXct
myPOSIXct = as.POSIXct(myDateTimeStr)
myPOSIXct
class(myPOSIXct)
as.numeric(myPOSIXct)

# Alterando o formato padrão
myDateTimeStr1 = "19-12-2006 10:17:07"
myPOSIXct1 = as.POSIXct(myDateTimeStr1, format = "%d-%m-%Y %H:%M:%S")
myPOSIXct1 

# Criando objetos POSIXct a partir de dados numéricos
myPOSIXct2 = as.POSIXct(0, origin = "1970-01-01")
myPOSIXct2
as.numeric(myPOSIXct2) 
myPOSIXct5 = ISOdatetime(year = 2016, month = 12, day = 21, hour = 10, min = 17, sec = 7, tz = "")
class(myPOSIXct5)
myPOSIXct5 

# Alterando o formato do output
myPOSIXct
format(myPOSIXct, format = "%b %d, %Y") 
format(myPOSIXct, format = "%B")
format(myPOSIXct, format = "%Z")
as.numeric(format(myPOSIXct, format = "%Y"))
as.numeric(format(myPOSIXct, format = "%S")) 

# Criando objetos POSIXlt
myDateTimeStr
myPOSIXlt = as.POSIXlt(myDateTimeStr)
myPOSIXlt
class(myPOSIXlt)

# Ajustando o formato de entrada
myDateTimeStr1 = "21-12-2016 11:19:02"
myPOSIXlt1 = as.POSIXlt(myDateTimeStr1, format = "%d-%m-%Y %H:%M:%S")
names(unclass(myPOSIXlt)) 

# Extraindo valores específicos da data
myPOSIXlt$sec
myPOSIXlt$hour
myPOSIXlt$mday
myPOSIXlt$mon
myPOSIXlt$year
myPOSIXlt$wday 
myPOSIXlt$yday
myPOSIXlt$isdst 

# Conversão
myPOSIXct
class(myPOSIXct)
myPOSIXlt = as.POSIXlt(myPOSIXct)
class(myPOSIXlt)
as.POSIXlt(myPOSIXct)$sec 

myPOSIXct
myDate = as.Date(myPOSIXct)
myDate
class(myDate) 

myPOSIXct = as.POSIXct(myDate, tz = "GMT")
myPOSIXct
myPOSIXlt = as.POSIXlt(myDate, tz = "GMT")
myPOSIXlt
myPOSIXct = as.POSIXct(myPOSIXlt)
myPOSIXct

HfDateTimeStr = "2016-12-15 18:13:04.125"
HfPOSIXct = as.POSIXct(HfDateTimeStr)
HfPOSIXct 
options(digits.secs = 3)
HfPOSIXct 

# Manipulando POSIXct
dt1 = as.POSIXct("2016-12-23 00:00:00")
dt2 = as.POSIXct("2016-12-23 05:00:00")
diffDateTime = dt2 - dt1
class(diffDateTime)
diffDateTime
units(diffDateTime)
as.numeric(diffDateTime) 

units(diffDateTime) = "secs"
diffDateTime
as.numeric(diffDateTime) 


# Pacote lubridate
install.packages("lubridate")
library(lubridate)

ymd("20161221")
ymd("2016 Dec 21") 

ymd_hms("20161219101707")
ymd_hms("2016 Dec 19 10:17:07")
mdy("Dec 19, 2016")
mdy_hms("December 19, 2016 10:17:07")
dmy_hms("19-Dec, 2016 10:17:07") 

ymd_hms("2016 Dec 19 10:17:07", tz="") 

ymd(20161219) 

myDateTime = ymd_hms("2016 Dec 19 10:17:07")
myDateTime 
year(myDateTime)
month(myDateTime)
week(myDateTime)
yday(myDateTime)
mday(myDateTime)
wday(myDateTime)
hour(myDateTime)
minute(myDateTime)
second(myDateTime)
tz(myDateTime)
wday(myDateTime, label=TRUE)
month(myDateTime, label=TRUE) 

# Atualizando uma data
mday(myDateTime) = 20
myDateTime 

update(myDateTime, year = 2016, month = 1, day = 1, hour = 5, min = 0, sec = 0) 







