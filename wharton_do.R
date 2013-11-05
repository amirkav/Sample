wd <- "C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton"
###############################################################################
### Read the data
all.zips <- db.getZips()[,1]  # all zips in the database
ids <- db.getSPs(all.zips[1])[,1]  # all user ids in a zipcode
zip.cons <- db.getAllData(all.zips[1]); dim(zip.cons)  # 15-min elec cons data for a given zip
user.cons <- db.getIntervalData(ids[1]); dim(user.cons)  # 15-min elec cons data for a given user id
zip.weather <- db.getWeatherData(all.zips[1]); dim(zip.weather)  # hourly data on outside temp, pressure, dewpoint, and precipitation
zip <- db.getZipForId(ids[1]); print(zip)  # zipcode of a given user

### Using weather and user data objects
zip.weather.data <- WeatherClass(all.zips[1]); names(zip.weather.data)
  # days: unique dates; set: sets the value of an existing column; add: adds a new data frame to the list
zip.weather.data$rh(zip.weather.data$tout, zip.weather.data$dp)

### Exploratory plots
user.data <- ResDataClass(ids[1]); names(user.data)
plot(user.data)
plot(r=user.data,type='temp')
plot(user.data,type='hourly')
plot(user.data$tout)
plot(user.data$tout,user.data$kw)

# First need to connect to PGE database (through FireFox) for this to work
zip.summary <- db.getZipData(all.zips[1]); dim(zip.summary)



##############################################################################################
### Effect of Socioeconomic variables ###
##############################################################################################
# zips.summary is a table that contains info on energy consumption at a zip-code level
# for example, average energy consumption of households in a zipcode.
names(acs.econ)[which(names(acs.econ)=="GEO.id2")] <- "ZIP"
econ.merge <- merge(zips.summary, acs.econ, by="ZIP")
names(acs.housing)[which(names(acs.housing)=="GEO.id2")] <- "ZIP"
housing.merge <- merge(zips.summary, acs.housing, by="ZIP")
names(acs.social)[which(names(acs.social)=="GEO.id2")] <- "ZIP"
social.merge <- merge(zips.summary, acs.social, by="ZIP")
names(acs.median.income)[which(names(acs.median.income)=="GEO.id2")] <- "ZIP"
median.income.merge <- merge(zips.summary, acs.median.income, by="ZIP")
names(acs.utility.payment)[which(names(acs.utility.payment)=="GEO.id2")] <- "ZIP"
utility.payment.merge <- merge(zips.summary, acs.utility.payment, by="ZIP")

# Economic characterisitcs
res <- "mean.mean"
exp <- "HC01_VC85"  # Estimate median income of household
exp <- "HC01_VC86"  # Estimate mean income of households
exp <- "HC01_VC113"  # Estimate mean income and benefits of families
exp <- "HC01_VC115"  # Estimate income and benefits per capita

plot(econ.merge[[res]], econ.merge[[exp]])
summary(lm(econ.merge[[res]] ~ econ.merge[[exp]]))

# Housing characteristics
res <- "mean.mean"
exp <- "HC03_VC13"  # Percent 1-unit detached houses
exp <- "HC03_VC16"  # Percent 3or4-unit houses
exp <- "HC03_VC17"  # Percent 5to9-unit houses
exp <- "HC03_VC18"  # Percent 10to19-unit houses
exp <- "HC03_VC26"  # Percent houses built 2005 or later
exp <- "HC03_VC28"  # Percent houses built 1990 to 1994
exp <- "HC03_VC31"  # Percent houses built 1960 to 1969
exp <- "HC03_VC33"  # Percent houses built 1940 to 1949
exp <- "HC03_VC40"  # Percent 2-room houses
exp <- "HC03_VC41"  # Percent 3-room houses
exp <- "HC03_VC42"  # Percent 4-room houses
exp <- "HC03_VC43"  # Percent 5-room houses
exp <- "HC03_VC44"  # Percent 6-room houses
exp <- "HC03_VC46"  # Percent 8-room houses
exp <- "HC03_VC47"  # Percent 9 or more rooms houses
exp <- "HC03_VC63"  # Percent owner-occupied houses
exp <- "HC03_VC64"  # Percent renter-occupied houses
exp <- "HC03_VC91"  # Percent bottled tank or LP gas for heating fuel
exp <- "HC03_VC92"  # Percent electricity for heating fuel

# Social characteristics
res <- "mean.mean"

# Median income
res <- "mean.mean"
exp <- "HD01_VD02"  # Estimate Pay for utilities
exp <- "HD01_VD03"  # Estimate Not pay for utilities

utility.payment.merge[[exp]] <- as.numeric(utility.payment.merge[[exp]])
plot(y=utility.payment.merge[[res]], x=utility.payment.merge[[exp]])
summary(lm(utility.payment.merge[[res]] ~ utility.payment.merge[[exp]]))
fit <- lm(utility.payment.merge[[res]] ~ utility.payment.merge[[exp]])
abline(a=fit$coefficients[1], b=fit$coefficients[2])


