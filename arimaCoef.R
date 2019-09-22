library(astsa)
library(forecast)

#Load data
path = "mlo_data/"
daily_c13iso = read.csv(paste(path, "daily_flask_c13_mlo.csv", sep=""))
monthly_c13iso = read.csv(paste(path, "monthly_flask_c13_mlo.csv", sep=""))
daily_o18iso = read.csv(paste(path, "daily_flask_o18_mlo.csv", sep=""))
intermittent_c13iso = read.csv(paste(path, "intermittent_flask_c14_mlo.csv", sep=""))
monthly_o18iso = read.csv(paste(path, "monthly_flask_o18_mlo.csv", sep=""))
daily_flask = read.csv(paste(path, "daily_flask_co2_mlo.csv", sep=""))
monthly_flask = read.csv(paste(path, "monthly_flask_co2_mlo.csv", sep=""), na.strings=-99.99)
ten_min_insitu = read.csv(paste(path, "ten_minute_in_situ_co2_mlo.txt", sep=""))
daily_insitu = read.csv(paste(path, "daily_in_situ_co2_mlo.csv", sep=""), comment.char = '%')
weekly_insitu = read.csv(paste(path, "weekly_in_situ_co2_mlo.csv", sep=""))
monthly_insitu = read.csv(paste(path, "monthly_in_situ_co2_mlo.csv", sep=""))

ten_min_insitu
#Prep data
#fixes headers
names(monthly_flask) = c( "Yr", "Mn", "Date1", "Date2", "CO2","seasonally", "fit",  "seasonally2", "CO2_filled", "seasonally3")
monthly_flask

#Splitting into two dataframes so that we can delete the NA rows in the non-filled-in data
monthly_flask1 = monthly_flask
monthly_flask1[9:10] = list(NULL) 

monthly_flask2 = monthly_flask
monthly_flask2[5:8] = list(NULL) 
monthly_flask2
na.omit(monthly_flask1)

n.bootstrap <- 200
simulated <- bld.mbb.bootstrap(monthly_flask2$CO2_filled[-1], n.bootstrap)
coefs <- matrix(0, nrow = n.bootstrap, ncol = length(mod$fit$coef))
i <- 1
while(i <= n.bootstrap){
  print(i)
  ts.temp = ts(simulated[[i]],start = c(1960, 2), frequency = 12)
  mod.temp <- tryCatch(sarima(ts.temp, p=4, d=1, q=1, P=5, Q=1, D=1, S=12, tol = 1e-3), error = function(e) NA) # At time optim not too friendly, so select new boot-strap sample. 
  if(is.na(mod.temp)){
    print("ERROR")
    simulated[[i]] = bld.mbb.bootstrap(monthly_flask2$CO2_filled[-1], 2)[[1]]
    next
  }
  print("NO ERROR")
  coefs[i,] <- mod.temp$fit$coef
  i = i + 1
}
write.table(coefs,file="bootstrap_coef.txt") # keeps the rownames