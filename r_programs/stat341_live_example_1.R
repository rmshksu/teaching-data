########################
### DATA EXPLORATION ###
########################

url = "https://raw.githubusercontent.com/rmshksu/teaching-data/refs/heads/main/ambient_data_st341.csv"
ambient_wn = read.csv(url, stringsAsFactors = FALSE)
ambient_wn = ambient_wn[,-c(1,5)]

# check structure
str(ambient_wn)

# showing the first 6 rows of the data
head(ambient_wn)

# simple histogram
hist(ambient_wn$PHOSPHU)

# mean removing na values
mean(ambient_wn$PHOSPHU, na.rm = TRUE)

# mean of data with na values removes
mean(na.omit(ambient_wn$PHOSPHU))

# five number summary + na count
summary(ambient_wn$PHOSPHU)

# simple scatters
plot(ambient_wn$dt,ambient_wn$PHOSPHU)
plot(ambient_wn$year,ambient_wn$PHOSPHU)

# different time cutoff lengths
length(subset(ambient_wn$year, ambient_wn$year < 2006))
length(subset(ambient_wn$year, ambient_wn$year >= 2015 | ambient_wn$year < 2019))

# since tibble/df -> sapply -> output = dataframe
analyte_means = sapply(ambient_wn[,-c(1:11)], mean, na.rm = TRUE)

print(analyte_means)

# round to 3rd sig fig
round(analyte_means,3)

# grab just total suspended solids
tss = ambient_wn[,c(1:11, 21)]

# count of rows with NA values
sum(is.na(tss))

# remove rows with NA values
tss = na.omit(tss)

# you can do this in one step
nitrate = na.omit(ambient_wn[,c(1:11, 15)])

plot(tss$dt, tss$TSS, 
     xlab = "Days", ylab = "TSS (mg/L)",
     pch = 20, col = "#51288550")
abline(h = mean(tss$TSS), col = "#D1D1D1", lty = 2, lwd = 2)

plot(nitrate$dt, nitrate$NITRATE,
     xlab = "Days", ylab = "Nitrate (mg/L)",
     pch = 20, col = "#51288550")
abline(h = mean(nitrate$NITRATE), col = "#D1D1D1", lty = 2, lwd = 2)

# mean, standard deviation, and minimum for tss
tss_by_basin = aggregate(TSS ~ BASIN, # by basin
                         data = tss,
                         FUN = function(x) c(mean(x), sd(x), min(x)))

# str is 2 columns
str(tss_by_basin) # R is hell

# coerce to matrix then dataframe fixes the problem
tss_by_basin = as.data.frame(as.matrix(tss_by_basin)) # R is hell
colnames(tss_by_basin) = c("BASIN", "Mean", "Sd", "Min") # fix column names

tss_by_basin

# proportion of samples equal to minimum
sum(tss$TSS == min(tss$TSS)) / nrow(tss)

# can do the same thing with dplyr (but faster)
nitrate_by_basin = nitrate %>%
  group_by(BASIN) %>%
  summarise(mean_nitrate = mean(NITRATE),
            sd_nitrate = sd(NITRATE),
            min_nitrate = min(NITRATE))

nitrate_by_basin

# proportion equal to minimum nitrate
sum(nitrate$NITRATE == min(nitrate$NITRATE)) / nrow(nitrate)

# IQR to measure TSS outliers
quantile(tss$TSS, 0.25) - (1.5*IQR(tss$TSS)) # won't find any outside
quantile(tss$TSS, 0.75) + (1.5*IQR(tss$TSS)) # this is egregious

# proportion outside upper iqr
sum(tss$TSS > quantile(tss$TSS, 0.75) + (1.5*IQR(tss$TSS))) / nrow(tss)

# "screen" the outliers
tss_screened = subset(tss, tss$TSS < quantile(tss$TSS, 0.75) + (1.5*IQR(tss$TSS)))

# outliers are subjective, sd is a viable choice
# proportion within 2 sd
1 - sum(nitrate$NITRATE < mean(nitrate$NITRATE) - 2*sd(nitrate$NITRATE) | 
          nitrate$NITRATE > mean(nitrate$NITRATE) + 2*sd(nitrate$NITRATE)) / 
  nrow(nitrate) # why would the intuitive way suck?

# scale is a hilarious thing
hist(tss$TSS, xlab = "TSS (mg/L)", main = "Bad scaling", col = "white")

# two ways to approach scale
hist(tss$TSS, xlab = "TSS (mg/L)", main = "Scale by limit", col = "gold",
     xlim = c(0,quantile(tss$TSS, 0.75) + (1.5*IQR(tss$TSS))), breaks = 500)

hist(tss_screened$TSS, xlab = "TSS (mg/L)",
     main = "Scale by reduction", col = "#51288590")

# subset to values less than 2 sd above the mean
nitrate_screened = subset(nitrate, nitrate$NITRATE < 
                            (mean(nitrate$NITRATE) + 2*sd(nitrate$NITRATE)))

# screening by standard deviation is more aggressive
hist(nitrate_screened$NITRATE, xlab = "Nitrate (mg/L)",
     main = "2 Stdev. screening", col = "#51288590")


#######################
### MODEL PROPOSALS ###
#######################

