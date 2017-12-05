library(ggplot2)
data.stars <- read.table("HD174936in.dat")
data <- data.frame(dias=data.stars$V1, counts=data.stars$V2)
summary(data)

grafico <- ggplot()+ geom_line(data=data,aes(x=dias,y=counts, color="1", group=1))
grafico

mean(data$dias)
mean(data$counts)
acf(data$dias, type ="correlation", plot = TRUE)
acf(data$counts, type ="correlation", plot = TRUE)

#Empezar el tiempo desde 0
data$dias = data$dias-min(data$dias)

#Estandarizar couts
data$counts = (data$counts-mean(data$counts))/sd(data$counts)
mean(data$counts)
sd(data$counts)

#Pasar tiempo de días a segudos
data$sec = data$dias*24*3600

#Agrupar
groupby <- function(x, y, step){
  old_l <- length(x)
  new_l <- as.integer(old_l/step)
  print(old_l)
  print(new_l)
  flush.console()
  x_mean <- double(new_l)
  y_mean <- double(new_l)
  y_std <- double(new_l)
  y_min <- double(new_l)
  y_max <- double(new_l)

  for (i in 1:new_l){
    pos_old <- i*step
    x_mean[i] <- mean(x[(pos_old-step+1):pos_old])
    y_mean[i] <- mean(y[(pos_old-step+1):pos_old])
    y_std[i] <- sd(y[(pos_old-step+1):pos_old])
    y_min[i] <- min(y[(pos_old-step+1):pos_old])
    y_max[i] <- max(y[(pos_old-step+1):pos_old])
    print(i)
    print(new_l)
    #print(length(x))
    print(sum(is.na(x[(pos_old-step+1):pos_old])))
    flush.console() 
  }
  
  return(list(x_mean=x_mean, y_mean=y_mean, y_std=y_std, y_min=y_min, y_max=y_max))
}

#Datos tomados cada 32s, vamos a agrupar cada hora aproximadamente
time_scale = 3600
data_hour <- groupby(data$sec, data$counts, 113)
data_hour$x_mean = data_hour$x_mean/3600.0

grafico <- ggplot() + geom_line(aes(x=data_hour$x_mean, y=data_hour$y_mean), color='blue')
grafico <- grafico + geom_line(aes(x=data_hour$x_mean, y=data_hour$y_mean+data_hour$y_std), color='red')
grafico <- grafico + geom_line(aes(x=data_hour$x_mean, y=data_hour$y_mean-data_hour$y_std), color='red')
grafico <- grafico + geom_line(aes(x=data_hour$x_mean, y=data_hour$y_min), color='green')
grafico <- grafico + geom_line(aes(x=data_hour$x_mean, y=data_hour$y_max), color='green')
grafico

#Datos tomados cada 32s, vamos a agrupar cada día aproximadamente
data_day <- groupby(data$sec, data$counts, 2700)
data_day$x_mean = data_day$x_mean/(24.0*3600.0)

grafico <- ggplot() + geom_line(aes(x=data_day$x_mean, y=data_day$y_mean), color='blue')
grafico <- grafico + geom_line(aes(x=data_day$x_mean, y=data_day$y_mean+data_day$y_std), color='red')
grafico <- grafico + geom_line(aes(x=data_day$x_mean, y=data_day$y_mean-data_day$y_std), color='red')
grafico <- grafico + geom_line(aes(x=data_day$x_mean, y=data_day$y_min), color='green')
grafico <- grafico + geom_line(aes(x=data_day$x_mean, y=data_day$y_max), color='green')
grafico
