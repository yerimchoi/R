pkgs <- c('dplyr', 'stringr', 'lpSolveAPI', 'DJL', 'ggplot2', 'lubridate','RColorBrewer')
sapply(pkgs, require, character.only = TRUE)

###############################################################################################################
# data preprocessing
###############################################################################################################
# data 1
data <- read.csv('C:/Users/lvaid/skku/석사-1/생산성분석/covid/dataset/final_owid-covid-data.csv')

nation <- c('Australia', 'Austria', 'Czech Republic', 'Germany', 'Hungary', 'Italy', 'Japan', 'South Korea', 'Mexico', 'New Zealand', 'Norway', 'Poland', 'Portugal', 'Slovakia', 'Sweden', 'Turkey', 'United Kingdom', 'United States')

covid <- data.frame(matrix(ncol = ncol(data)))
names(covid) <- names(data)

for (i in 1:nrow(data)){
  name <- data[i,]$location
  
  for (n in nation){
    if (name == n){
      covid <- rbind(covid, data[i,])
      break
    } 
    else {
      next
    }
  }
}

covid <- covid[-c(1),]
for (i in 1:nrow(covid)){
  covid[i,]$date <- paste(strsplit(as.character(covid[i,]$date), split = "-")[[1]], collapse = "")
}
covid <- covid %>% select(location, date, total_cases, new_cases, total_deaths, new_deaths, total_tests, new_tests, new_tests_smoothed, population, population_density, hospital_beds_per_100k)

covid[is.na(covid)] <- 0
write.csv(covid, 'C:/Users/lvaid/skku/석사-1/생산성분석/covid/dataset/final_covid-data.csv')


# data 2, recovered data
recovered.data <- read.csv('C:/Users/lvaid/skku/석사-1/생산성분석/covid/dataset/final_time_series_covid_19_recovered.csv')
recovered.nation <- c('Australia', 'Austria', 'Czechia', 'Germany', 'Hungary', 'Italy', 'Japan', 'Korea, South', 'Mexico', 'New Zealand', 'Norway', 'Poland', 'Portugal', 'Slovakia', 'Sweden', 'Turkey', 'United Kingdom', 'US')

# data preprocessing
recovered <- data.frame(matrix(ncol = ncol(recovered.data)))
names(recovered) <- names(recovered.data)

for (i in 1:nrow(recovered.data)){
  name <- recovered.data[i,]$Country.Region
  
  for (n in recovered.nation){
    if (name == n){
      recovered <- rbind(recovered, recovered.data[i,])
      break
    } 
    else {
      next
    }
  }
}
recovered <- recovered[-c(1),]
recovered %>% select(-Province.State, -Lat, -Long) %>% group_by(Country.Region) %>% summarise(n = n())

re.1 <- recovered %>% select(-Province.State, -Lat, -Long) %>% filter(Country.Region != 'Australia' & Country.Region != 'United Kingdom')
re.1 %>% group_by(Country.Region) %>% summarise(n = n())

re.2 <- data.frame(matrix(nrow = 2, ncol = ncol(re.1)))
names(re.2) <- names(re.1)

aus <- recovered %>% filter(Country.Region == 'Australia') %>% select(-Province.State, -Lat, -Long)
re.2[1, 1] <- 'Australia'
re.2[1, 2:ncol(re.2)] <- colSums(aus[-1])

uk <- recovered %>% filter(Country.Region == 'United Kingdom') %>% select(-Province.State, -Lat, -Long)
re.2[2, 1] <- 'United Kingdom'
re.2[2, 2:ncol(re.2)] <- colSums(uk[-1])

recov <- rbind(re.1, re.2)
recov %>% group_by(Country.Region) %>% summarise(n = n())

r <- c()
for (i in 1:nrow(recov)){
  row <- recov[i,]
  temp <- data.frame(matrix(nrow = ncol(recov)-1, ncol = 3))
  temp[, 1] <- row[1]
  
  for (d in 1:nrow(temp)){
    dat <- names(row)[d+1]
    temp[d, 2] <- paste(strsplit(str_sub(dat, 2, -1), split = ".", fixed = TRUE)[[1]], collapse = "")
  }
  
  temp[, 3] <- t(row[-1])
  r <- rbind(r, temp)
}
names(r) <- c('location', 'date', 'total_recovered')

r[r=='Korea, South'] <- 'South Korea'
r[r=='US'] <- 'United States'
r[r=='Czechia'] <- 'Czech Republic'
merged.data <- merge(covid, r, key = c('location, date'))

icu <- read.csv('C:/Users/lvaid/skku/석사-1/생산성분석/covid/dataset/Negative pressure room.csv', stringsAsFactors = FALSE)
icu[icu == 'Korea'] <- 'South Korea'
icu[icu == 'Slovak Republic'] <- 'Slovakia'
merged.data <- merge(merged.data, icu, key = c('location'))

ihrccs <- read.csv('C:/Users/lvaid/skku/석사-1/생산성분석/covid/dataset/International Health Regulations.csv', stringsAsFactors = FALSE)
ihr <- ihrccs %>% select(location, X2019)
names(ihr) <- c('location', 'ihrccs')
merged.data <- merge(merged.data, ihr, key = c('location'))

gdp <- read.csv('C:/Users/lvaid/skku/석사-1/생산성분석/covid/dataset/gdp.csv', stringsAsFactors = FALSE)
names(gdp) <- c('location', 'gdp')
merged.data <- merge(merged.data, gdp, key = c('location'))


write.csv(merged.data, 'C:/Users/lvaid/skku/석사-1/생산성분석/covid/dataset/final_merged_data.csv')


a <- unique(covid %>% group_by(location) %>% select(aged_70_older))
for (i in 1:nrow(a)){
  if (a[i,]$location %in% nation){
    print(a[i,])
  }
}
###############################################################################################################
# First output-oriented VRS model
###############################################################################################################
merged.covid <- read.csv('C:/Users/lvaid/skku/석사-1/생산성분석/covid/dataset/final_merged_data.csv', stringsAsFactors = FALSE)
merged.covid <- merged.covid %>% filter(location != 'Sweden' & location != 'Norway')
nation <- c('Australia', 'Austria', 'Czech Republic', 'Germany', 'Hungary', 'Italy', 'Japan', 'South Korea', 'Mexico', 'New Zealand', 'Poland', 'Portugal', 'Slovakia', 'Turkey', 'United Kingdom', 'United States')

first <- merged.covid %>% select(location, ihrccs, population, population_density, gdp, hospital_beds_per_100k, ICU_beds_per_100k, ventilators)
first <- unique(first)

x <- data.frame(x1 = first$ihrccs, x2 = first$population_density, x3 = first$population, x4 = first$gdp) 
y <- data.frame(y1 = first$ICU_beds_per_100k, y2 = first$hospital_beds_per_100k, y3 = first$ventilators)

n_x <- nrow(x)
c_x <- ncol(x)
c_y <- ncol(y)

# Empty result & variables
first.result <- data.frame(matrix(nrow = n_x, ncol = 2))
names(first.result) <- c("location", "infra_eff")

for (d in 1:n_x){
  lp.vrs <- make.lp(0, c_y + c_x + 1) 
  
  # Declare an objective function
  set.objfn(lp.vrs, c(x[d, ], 1), indices = c((c_y+1):(c_y+c_x), c_y+c_x+1))
  
  # Constraint
  for (j in 1:n_x){
    add.constraint(lp.vrs, c(x[j, ], -y[j, ]),
                   indices = c((c_y+1):(c_y+c_x), 1:c_y), ">=", 0)
  }
  
  add.constraint(lp.vrs, c(y[d, ]), 
                 indices = c(1:c_y), "=", 1)
  
  # Solve & save results
  solve.lpExtPtr(lp.vrs)
  first.result[d, 1] <- first[d,]$location
  first.result[d, 2] <- get.objective(lp.vrs)
}

# View the results
first.result


###############################################################################################################
# Data pre-processing for Second step 
###############################################################################################################
second <- merged.covid %>% select(location, date, total_tests, total_cases, total_recovered, total_deaths, population)
second <- merge(second, first.result, key = c('location'))

# First
# refill the empty total_tests 
refilled.covid <- data.frame()

for (i in 1:length(nation)){
  temp <- second %>% filter(location == nation[i])
  empty <- temp %>% filter(total_tests < total_cases)
  filled <- temp %>% filter(total_tests >= total_cases)
  
  if (nrow(empty) != 0){
    print(nation[i])
    temp2 <- second %>% filter(location == nation[i])
    y = temp2$total_tests/temp2$total_cases
    y[is.nan(y)] <- 0
    y[is.infinite(y)] <- 0
    x = seq(1, length(y), 1)
    
    reg <- lm(y ~ x)
    pred <- round(predict(reg, newdata = data.frame(x = empty$total_cases)))
    
    empty$total_tests <- pred
    temp <- rbind(empty, filled)
    refilled.covid <- rbind(refilled.covid, temp)
  }
  
  else{
    refilled.covid <- rbind(refilled.covid, temp)
  }
}

refilled.covid <- refilled.covid[order(refilled.covid$location, refilled.covid$date),]

index <- c(refilled.covid$total_tests < refilled.covid$total_cases)
for (i in 1:length(index)){
  if (index[i] == TRUE){
    refilled.covid[i, ]$total_tests <- refilled.covid[i, ]$total_cases
  }
}

write.csv(refilled.covid, 'C:/Users/lvaid/skku/석사-1/생산성분석/covid/dataset/final_refilled_data.csv')


# Second
# find each calender
nation <- c('Australia', 'Austria', 'Czech Republic', 'Germany', 'Hungary', 'Italy', 'Japan', 'South Korea', 'Mexico', 'New Zealand', 'Poland', 'Portugal', 'Slovakia', 'Turkey', 'United Kingdom', 'United States')
refilled.covid <- read.csv('C:/Users/lvaid/skku/석사-1/생산성분석/covid/dataset/final_refilled_data.csv')
refilled.covid$total_tests <- round(refilled.covid$total_tests)
refilled.covid <- refilled.covid %>% filter(location != 'Norway' & location != 'Sweden')

calenders <- data.frame(matrix(nrow = length(nation) * 6, ncol = 2))
names(calenders) <- c('location', 'date')

for (i in 1:length(nation)){
  temp <- refilled.covid %>% filter(location == nation[i])
  calender <- c()
  
  for (n in 1:nrow(temp)){
    start.death <- temp[n, ]$total_deaths
    start.recovery <- temp[n, ]$total_recovered
    
    if (start.death >= 1 & start.recovery >= 1){
      start.day <- temp[n, ]$date
      calender <- c(calender, start.day)
      break
    } else{
      next
    }
  }
  
  day <- start.day
  print(nation[i])
  print(start.day)
  calenders[6*(i-1)+1, 1] <- nation[i]
  calenders[6*(i-1)+1, 2] <- day
  
  for (s in 1:5){
    next.day <- ymd(day) + days(14)
    next.day <- gsub("-", "", as.character(next.day), fixed=TRUE)
    
    if (next.day > 20200616){
      break
    } else {
      calender <- c(calender, next.day)
      day <- next.day
      calenders[6*(i-1)+s+1, 1] <- nation[i]
      calenders[6*(i-1)+s+1, 2] <- day
    }
  }
}


# Third
# select each data
final.covid <- data.frame()

for (i in 1:length(nation)){
  temp <- refilled.covid %>% filter(location == nation[i])
  calender <- calenders %>% filter(location == nation[i])
  calender <- calender$date

  for (d in 1:length(calender)){
    row <- temp %>% filter(date == calender[d])
    row <- cbind(row, d)
    final.covid <- rbind(final.covid, row)
  }
}

names(final.covid) <- c('index', 'location', 'date', 
                         'total_tests', 'total_cases', 'total_recovered', 'total_deaths', 
                         'population', 'infra_eff', 'period')
final.covid
###############################################################################################################
# Run VRS with Weak Disposability
###############################################################################################################
# Run DEA with Bad Output
result.dea <- data.frame()

for (c in 1:6){
  temp <- final.covid %>% filter(period == c)
  covid.nation <- c()
  
  for (t in 1:nrow(temp)){
    index <- which(nation == temp[t,]$location)
    covid.nation <- c(covid.nation, index)
  }

  X <- data.frame(x1 = temp$infra_eff, x2 = temp$total_tests/temp$population)
  V <- data.frame(v1 = temp$total_recovered/temp$population)
  W <- data.frame(w1 = temp$total_deaths/temp$population, w2 = temp$total_cases/temp$population)
  
  n_x <- nrow(X)
  n_v <- nrow(V)
  n_w <- nrow(W)
  
  c_x <- ncol(X)
  c_v <- ncol(V)
  c_w <- ncol(W)
  
  for (d in 1:n_x){
    # Declare LP object
    lp.dea <- make.lp(0, n_x * 2 + 1 + c_x + c_v) # lambda + efficiency + xslack + yslack
     
    # Declare an objective function
    set.objfn(lp.dea, c(1), indices = c(n_x*2 + 1))
    
    # Constraint for X
    for(i in 1:c_x){
      add.constraint(lp.dea, c(X[, i], X[, i], 1),
                     indices = c(1:(n_x*2), n_x*2 + 1 + i), "=", X[d, i])
    }
    
    # Constraint for V
    for(r in 1:c_v){
      add.constraint(lp.dea, c(V[, r], -1),
                     indices = c(1:n_v, n_v*2 + 1 + c_x + r), "=", V[d, r])
    }
    
    # Constraint for W
    for(r in 1:c_w){
      add.constraint(lp.dea, c(W[, r], -W[d, r]),
                     indices = c(1:n_w, n_w*2 + 1), "=", 0)
    }
    
    # VRS
    add.constraint(lp.dea, c(rep(1, n_x*2)), indices = c(1:(n_x*2)), "=", 1)
    
    # Set lower bounds
    set.bounds(lp.dea, lower = c(rep(0, n_x), rep(0, n_x), -Inf, rep(0, c_x + c_v)))
    
    # Solve
    solve.lpExtPtr(lp.dea)
    
    result.dea[covid.nation[d], c] <- get.objective(lp.dea)
  }
}

result.dea
result <- cbind(nation, result.dea)
names(result) <- c('nation', 'period1', 'period2', 'period3', 'period4', 'period5', 'period6')
result
###############################################################################################################
# Malmquist Index Analysis - My code
###############################################################################################################
malmquist <- function(temp, d) {
  for (t in 1:nrow(temp)){
    index <- which(nation == temp[t,]$location)
    covid.nation <- c(covid.nation, index)
  }
  
  X <- data.frame(x1 = temp$infra_eff, x2 = temp$total_tests/temp$population)
  V <- data.frame(v1 = temp$total_recovered/temp$population)
  W <- data.frame(w1 = temp$total_deaths/temp$population, w2 = temp$total_cases/temp$population)
  
  n_x <- nrow(X)
  n_v <- nrow(V)
  n_w <- nrow(W)
  
  c_x <- ncol(X)
  c_v <- ncol(V)
  c_w <- ncol(W)
  
  # Declare LP object
  lp.dea <- make.lp(0, n_x * 2 + 1 + c_x + c_v) # lambda + efficiency + xslack + yslack
  
  # Declare an objective function
  set.objfn(lp.dea, c(1), indices = c(n_x*2 + 1))
  
  # Constraint for X
  for(i in 1:c_x){
    add.constraint(lp.dea, c(X[, i], X[, i], 1),
                   indices = c(1:(n_x*2), n_x*2 + 1 + i), "=", X[d, i])
  }
  
  # Constraint for V
  for(r in 1:c_v){
    add.constraint(lp.dea, c(V[, r], -1),
                   indices = c(1:n_v, n_v*2 + 1 + c_x + r), "=", V[d, r])
  }
  
  # Constraint for W
  for(r in 1:c_w){
    add.constraint(lp.dea, c(W[, r], -W[d, r]),
                   indices = c(1:n_w, n_w*2 + 1), "=", 0)
  }
  
  # VRS
  add.constraint(lp.dea, c(rep(1, n_x*2)), indices = c(1:(n_x*2)), "=", 1)
  
  # Set lower bounds
  set.bounds(lp.dea, lower = c(rep(0, n_x), rep(0, n_x), -Inf, rep(0, c_x + c_v)))
  
  # Solve
  solve.lpExtPtr(lp.dea)
  return(get.objective(lp.dea))
}


# TEC
tec <- data.frame(matrix(nrow = length(nation), ncol = 6))
names(tec) <- c('nation', 'period1', 'period2', 'period3', 'period4', 'period5')
tec[, 1] <- nation

for (i in 1:5){
  value <- result.dea[, i+1] / result.dea[, i]
  tec[, i+1] <- value
}

tec.plot <- data.frame(matrix(nrow = length(nation) * 5, ncol = 3))
names(tec.plot) <- c('nation', 'period', 'tec')
tec.plot[, 1] <- rep(nation, each = 5)
tec.plot[, 2] <- rep(c(1:5), length(nation))

for (i in 1:length(nation)){
  row <- as.numeric(tec[i, c(2:6)])
  tec.plot[(5*(i-1)+1):(5*(i-1)+5), 3] <- row
}

# FS
fs <- data.frame(matrix(nrow = length(nation), ncol = 6))
names(fs) <- c('nation', 'period1', 'period2', 'period3', 'period4', 'period5')
fs[, 1] <- nation

for (n in 1:length(nation)){
  for (i in 1:5){
    t1 <- result.dea[n, i+1]
    row1 <- final.covid %>% filter(location == nation[n] & period == i+1)
    temp1 <- final.covid %>% filter(location != nation[n] & period == i)
    temp1 <- rbind(temp1, row)
    eff1 <- malmquist(temp1, 17)/t1
    
    t2 <- result.dea[n, i]
    row2 <- final.covid %>% filter(location == nation[n] & period == i)
    temp2 <- final.covid %>% filter(location != nation[n] & period == i+1)
    temp2 <- rbind(temp2, row)
    eff2 <- t2/malmquist(temp2, 17)
    
    eff <- sqrt(eff1 * eff2)
    fs[n, i+1] <- eff
  }
}

fs.plot <- data.frame(matrix(nrow = length(nation) * 5, ncol = 3))
names(fs.plot) <- c('nation', 'period', 'fs')
fs.plot[, 1] <- rep(nation, each = 5)
fs.plot[, 2] <- rep(c(1:5), length(nation))

for (i in 1:length(nation)){
  row <- as.numeric(fs[i, c(2:6)])
  fs.plot[(5*(i-1)+1):(5*(i-1)+5), 3] <- row
}

# MI
mi <- data.frame(matrix(nrow = length(nation), ncol = 6))
names(mi) <- c('nation', 'period1', 'period2', 'period3', 'period4', 'period5')
mi[, 1] <- nation

for (n in 1:length(nation)){
  for (i in 1:5){
    tec.val <- tec[n, i+1]
    fs.val <- fs[n, i+1]
    
    t1 <- result.dea[n, i]
    row1 <- final.covid %>% filter(location == nation[n] & period == i+1)
    temp1 <- final.covid %>% filter(location != nation[n] & period == i)
    temp1 <- rbind(temp1, row)
    eff1 <- malmquist(temp1, 17)/t1
    
    t2 <- result.dea[n, i+1]
    row2 <- final.covid %>% filter(location == nation[n] & period == i)
    temp2 <- final.covid %>% filter(location != nation[n] & period == i+1)
    temp2 <- rbind(temp2, row)
    eff2 <- t2/malmquist(temp2, 17)
    
    eff <- sqrt(eff1 * eff2)
    mi[n, i+1] <- eff
  }
}

mi.plot <- data.frame(matrix(nrow = length(nation) * 5, ncol = 3))
names(mi.plot) <- c('nation', 'period', 'MI')
mi.plot[, 1] <- rep(nation, each = 5)
mi.plot[, 2] <- rep(c(1:5), length(nation))

for (i in 1:length(nation)){
  row <- as.numeric(mi[i, c(2:6)])
  mi.plot[(5*(i-1)+1):(5*(i-1)+5), 3] <- row
}


# Show plots
# TEC
ggplot(tec.plot, aes(x = period, y = tec, colour = nation, group = nation)) + 
  geom_point(size = 2) + geom_line(size = 1) + labs(y = "TEC") +
  geom_hline(yintercept=1, linetype='dashed', color='black', size=0.3) +
  scale_shape_discrete(labels=nation) + scale_color_manual(values = c(brewer.pal(7, "Dark2"), brewer.pal(6, "Set2"), brewer.pal(3, "Accent")))

# FS
ggplot(fs.plot, aes(x = period, y = fs, colour = nation, group = nation)) + 
  geom_point(size = 2) + geom_line(size = 1) + labs(y = "FS") +
  geom_hline(yintercept=1, linetype='dashed', color='black', size=0.3) +
  scale_shape_discrete(labels=nation) + scale_color_manual(values = c(brewer.pal(7, "Dark2"), brewer.pal(6, "Set2"), brewer.pal(3, "Accent")))

# MI
ggplot(mi.plot, aes(x = period, y = MI, colour = nation, group = nation)) + 
  geom_point(size = 2) + geom_line(size = 1) + labs(y = "MI") +
  geom_hline(yintercept=1, linetype='dashed', color='black', size=0.3) +
  scale_shape_discrete(labels=nation) + scale_color_manual(values = c(brewer.pal(7, "Dark2"), brewer.pal(6, "Set2"), brewer.pal(3, "Accent")))
