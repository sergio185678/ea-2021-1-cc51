#abrir el archivo
booking_data <- read.csv("D:/Developer/hotel_bookings_miss.csv", header = TRUE,stringsAsFactors = FALSE, sep = ",")
summary(booking_data)
str(booking_data)
colnames(booking_data)

#valores NA
valorNA <- function(x){
  sum=0
  for(i in 1:ncol(x))
  {
    cat(colnames(x[i]), ", Valores NA:", colSums(is.na(x[i])),"\n")
  }
}
valorNA(booking_data)

valorVacio <- function(x){
  sum=0
  for(i in 1:ncol(x))
  {
    cat(colnames(x[i]), "valores NA:", colSums(x[i]==""),"\n")
  }
}
valorVacio(booking_data)



##funcion moda
moda <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

##creo otra data quitando los vacios
booking_limpio= na.omit(booking_data)

#Reemplazando valores NA
replace_na <- function(x, x_limpio){
  for(i in 1:ncol(x))
  {
    if (colSums(is.na(x[i])) > 0)
    {
      moda_col = moda(x_limpio[,i])
      x[,i][is.na(x[,i])] = moda_col
    }
  }
  return(x)
}
booking_data_na = replace_na(booking_data, booking_limpio)
valorNA(booking_data_na)

#detectando outliers(valores atípicos)
boxplot(booking_data$lead_time)
boxplot(booking_data$stays_in_weekend_nights)
boxplot(booking_data$adults)
boxplot(booking_data$children)
boxplot(booking_data$babies)
boxplot(booking_data$previous_cancellations)
boxplot(booking_data$previous_bookings_not_canceled)
boxplot(booking_data$booking_changes)
boxplot(booking_data$days_in_waiting_list)
boxplot(booking_data$required_car_parking_spaces)
boxplot(booking_data$total_of_special_requests)


#arreglando valores atípicos

fix_outliers <- function(df){
  for(i in 1:ncol(df))
  {
    if(typeof(df[,i]) != "character")
    {
      Q <- quantile(df[,i], probs=c(0.25, 0.75))
      iqr <- IQR(df[,i])
      up <-  Q[2]+1.5*iqr   
      low<- Q[1]-1.5*iqr 
      
      df[,i][df[,i] < (Q[1] - 1.5*iqr)] = round(mean(df[,i]),0)
      df[,i][df[,i] > (Q[2] + 1.5*iqr)] = round(mean(df[,i]),0)
    }
  }
  return(df)
}

booking_data_na = fix_outliers(booking_data_na)
boxplot(booking_data_na$total_of_special_requests)
boxplot(booking_data_na$previous_cancellations)



#pregunta a

library(plyr)
count(booking_data_na, "hotel")

#pregunta b

library(ggplot2)

n_bookings <- booking_data_na

n_bookings$reservation_status_date <- as.Date(n_bookings$reservation_status_date,"%m/%d/%Y")
n_bookings <- n_bookings[order(n_bookings$reservation_status_date),]

table_n <- table(n_bookings$reservation_status_date)

barplot(table_n,main="Distribución de reservas según las fechas",xlab="Fechas",ylab="Reservas")

#pregunta c y d

n_bookings$reservation_year = format(as.Date(n_bookings$reservation_status_date, format="%m/%d/%Y"),"%Y")

n_bookings_per_year <- subset(n_bookings, n_bookings$reservation_year != "2014")

table_year <- table(n_bookings_per_year$reservation_year)

barplot(table_year,main="Temporadas de reservas",
        xlab="Año",
        ylab="# de reservas",
        col= c("red", "green", "blue"))

legend("topright",
       c("Alta","Media","Baja"),
       fill= c("green","blue","red"))


#pregunta e

countBC <- function(df){
  sumb=0
  sumc=0
  sumbyc=0
  sumboc=0
  for(i in 1:nrow(df))
  {
    if (df$children[i] > 0 & df$babies[i] > 0)
    {
      sumbyc = sumbyc + 1
    }
    if (df$children[i] > 0 & df$babies[i] == 0)
    {
      sumc = sumc + 1
    }
    if (df$children[i] == 0 & df$babies[i] > 0)
    {
      sumb = sumb + 1
    }
    if (df$children[i] > 0 | df$babies[i] > 0)
    {
      sumboc = sumboc + 1
    }
  }
  cat("Cantidad de reservas solo con bebés:",sumb,"\n")
  cat("Cantidad de reservas solo con niños:",sumc,"\n")
  cat("Cantidad de reservas con bebés y niños:",sumbyc,"\n")
  cat("Cantidad de reservas con bebés o niños:",sumboc,"\n")
}
countBC(booking_data_na)

#pregunta f

table_vehiiculos <- table(n_bookings$required_car_parking_spaces)
barplot(table_vehiiculos,main="Distribución de reservas según la cantidad de espacios requeridos de aparcamiento de autos",xlab="Espacio requerido",ylab="Reservas")

#pregunta g

library(ggplot2)
mts <- c("January","February","March","April","May","June","July","August","September","October","November","December")
b2 <- booking_data_na
b2$reservation_status_month <- format(as.Date(b2$reservation_status_date, format="%m/%d/%Y"),"%m")
b2 <- b2[order(b2$reservation_status_month),]
b2$reservation_status_month <- as.integer(b2$reservation_status_month)
b2$reservation_status_month <- mts[b2$reservation_status_month]

cancelByMonth <- subset(b2, b2$is_canceled == 1, select = c(is_canceled, reservation_status_month))
qplot(data = b2, reservation_status_month, fill = is_canceled, bins = 30, main="")


