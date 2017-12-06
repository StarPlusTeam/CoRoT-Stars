library(ggplot2)

# Lectura de datos
data.medidas <- read.table("HD174936in.dat")
data <- data.frame(time=data.medidas$V1, counts=data.medidas$V2)

plot(data$time,data$counts)

# Calculo de la autocorrelacion
data.corr <- acf(data$counts, lag.max = 500)
xcross <- data.frame(lag=data.corr$lag, acf=data.corr$acf)

grafico1 <- ggplot()+ geom_line(data=xcross,aes(x=lag,y=acf,color="Autocross",group=1))
grafico1 <- grafico + theme(axis.text.x = element_text(angle = 90, hjust = 1))
grafico1

# Calculo de la DFT
data.fft <- fft(data$counts-mean(data$counts))
freqn = seq(0,2*pi,length=dim(data)[1])

tf_2k  <- data.frame(fn = freqn[1:2000], mod = abs(data.fft)[1:2000])

grafico2 <- ggplot()+ geom_line(data=tf_2k,aes(x=fn,y=mod,color="Modulo DFT",group=1))
grafico2 <- grafico2 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
grafico2