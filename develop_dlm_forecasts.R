library(dplyr)
library(lubridate)
library(arrow)
library(glue)
library(readr)
library(rjags)
library(daymetr)
library(ecoforecastR)
library(zoo)
library(padr)

o <- readr::read_csv("GRANGE_CREEK_TEMP.csv") |>
  dplyr::mutate(datetime = lubridate::ymd_hms(datetime)) |>
  dplyr::mutate(datetime = lubridate::floor_date(datetime, "hour")) |>
  rename(date = datetime)
m <- read_csv("historical_met_data.csv")
f <- read_csv("noaa_gefs_forecast.csv")

target <- left_join(o, m, by = "date")

DLM_function <- function(site) {
  
  paste(site, "Beginning...")
 
  y <- target$temp_c
  temp <- target$temperature_2m
  
  data <- list(y = y,n = length(y),
               x_ic=mean(y, na.rm = T),
               tau_ic= 1/sd(y),
               a_obs=1,
               r_obs=1,          
               a_add=1,
               r_add=1,           
               temp = temp
  )
  
  #model = list(obs="y",fixed="~ 1 + X + temp", n.iternumber = 20000)
  
  model = list(obs="y",fixed="~ 1 + X + temp", n.iternumber = 10000)
  
  ef.out <- ecoforecastR::fit_dlm(model=model,data)
  
  params <- window(ef.out$params,start=1000)
  #plot(params)
  #summary(params)
  #cor(as.matrix(params))
  #pairs(as.matrix(params))
  
  out <- as.matrix(ef.out$predict)
  ci <- apply(out,2,quantile,c(0.025,0.5,0.975))
  plot(time,ci[2,],
       type='n',
       ylim=range(y,na.rm=TRUE),
       ylab="Water Temperature",
  )
  ecoforecastR::ciEnvelope(time,ci[1,],ci[3,],col=ecoforecastR::col.alpha("lightBlue",0.75))
  points(time,y,pch="+",cex=0.5)
  
  
  
  #### Milestone 6 ####
  
  
  forecastN <- function(IC, betaIntercept, betaX, betaTemp, temp , Q = 0, n = Nmc){
    
    NT = 30
    N <- matrix(NA,n,NT)  ## storage
    Nprev <- IC           ## initialize
    for(t in 1:NT){
      mu = Nprev + betaX * Nprev + betaTemp * temp[t] + betaIntercept  ## mean
      N[,t] <- rnorm(n,mu,Q)                         ## predict next step
      Nprev <- N[,t]                                  ## update IC
    }
    return(N)
  }
  
  ## parameters
  
  reference_date <- Sys.Date() - 1
  
  df_future <- f |>
    dplyr::select(date, matches('temperature_2m')) |>
    tidyr::pivot_longer(cols = temperature_2m_member0:temperature_2m_member30,
                        names_to = "ensembles", values_to = "value") |>
    group_by(date) |>
    summarize(mean_temperature = mean(value))

  params <- as.matrix(ef.out$params)
  param.mean <- apply(params,2,mean)
  
  IC <- as.matrix(ef.out$predict)
  
  
  #forecast <- forecastN(mean(IC[,"x[1035]"]), param.mean[1], param.mean[2], param.mean[3], noaa_air_temp_future, Q = 0, n = 1)
  NT = 168
  time1 = 1:NT
  x = 1:length(ci[2,])
  
  time2 = length(x):(length(x)+NT-1)
  
  Nmc = 1000
  prow = sample.int(nrow(params),Nmc,replace=TRUE)
  forecast <- forecastN(IC[prow, ncol(IC)], params[prow,"betaIntercept"], params[prow,"betaX"], params[prow, "betatemp"], df_future$mean_temperature, Q = 0, n = Nmc)
  
  par(mfrow=c(1,1))
  
  plot(x,ci[2,],
       type='n',
       ylim=range(y,na.rm=TRUE),
       ylab="Temperature",
       # xlim = c(1500,length(ci[2,]) + NT),
       xlim = c(min(time2) - 30, max(time2)),
       main = "Forecast with IC and Parameter Uncertainty"
  )
  ecoforecastR::ciEnvelope(x,ci[1,],ci[3,],col=ecoforecastR::col.alpha("lightBlue",0.75))
  points(x,y,pch="+",cex=0.5)
  
  N.IP.ci = apply(forecast,2,quantile,c(0.025,0.5,0.975))
  ecoforecastR::ciEnvelope(time2,N.IP.ci[1,],N.IP.ci[3,],col=col.alpha("lightBlue",1))
  lines(time2,N.IP.ci[2,],lwd=0.5)
  legend("bottomright", legend = c("Data", "CI", "IC Uncertainty", "Parameter Uncertainty"), lty = c(NA,1,1,1), col = c("black", "lightblue", "black", "red"), pch = c("+", NA, NA, NA), cex = 0.7)
  
  
  # Format variables for return and submission
  prediction <- as.vector(N.IP.ci[2,])
  
  datetime <- seq(reference_date, by = "day", length.out = 30)
  site_id <- site
  forecast_df <- data.frame(site_id, datetime, prediction)
  
  return(forecast_df)
  ##########
  
  #Qmc <- 1/sqrt(params[prow,"Q"])
  
  
  ### Uncertainty Analysis
  ### calculation of variances
  #varI     <- apply(N.I,2,var)
  #varIP    <- apply(N.IP,2,var)
  #varMat   <- rbind(varI,varIP)
  
  ## in-sample stacked area plot
  #V.pred.rel.in <- apply(varMat[-5,],2,function(x) {x/max(x)})
  #plot(time2,V.pred.rel.in[1,],ylim=c(0,1),type='n',main="Relative Variance: In-Sample",ylab="Proportion of Variance",xlab="time")
  
  #N.cols <- c("red","blue")
  
  #ciEnvelope(time2,rep(0,ncol(V.pred.rel.in)),V.pred.rel.in[1,],col=N.cols[1])
  #ciEnvelope(time2,V.pred.rel.in[1,],V.pred.rel.in[2,],col=N.cols[2])
  #legend("topleft",legend=c("Process","InitCond"),col=rev(N.cols[-5]),lty=1,lwd=5)
}
