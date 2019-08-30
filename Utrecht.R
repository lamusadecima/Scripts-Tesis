setwd("Desktop")

#Modularity 3 communities
my.data = read.csv("ADSO table3.csv")
my.data[,20]
Birth = my.data[,15]
Modularity = my.data[,20]
boxplot(Modularity, Birth)
boxplot(Modularity, data =  Birth)
boxplot(Modularity)
boxplot(Birth~Modularity, main = "Modularity class", ylab = "Birth date", col = c("green", "blue", "brown"))

#Modularity highest value
my.data[,12]
Mod2 = my.data[,5]
boxplot(Birth~Mod2, main = "Modularity class", ylab = "Birth date", col = c("green", "brown", "light blue", "dark green", "pink", "purple", "orange"))

my.data[4,]
#Historiogram of Birth

hist(Birth)

#Linear regression
Birth[5,]
difference_to_mean_year = Birth - mean(Birth)
difference_to_mean_year

abs_difference_to_mean_year = abs(difference_to_mean_year)
Closeness = my.data[,12]
Harmonic_closeness_centrality = my.data[,13]
Eccentricity = my.data[,11]

x = abs_difference_to_mean_year
y = Eccentricity
regression <- lm(x~y)
summary(regression)
confint(regression)

# Excluding Joseph de Litala as outlier
Birth2 = my.data[-(4),15]
Birth2
my.data2 = my.data[-(4),]
difference_to_mean_year2 = Birth2 - mean(Birth2)
difference_to_mean_year2

abs_difference_to_mean_year2 = abs(difference_to_mean_year2)
Closeness2 = my.data2[,12]
Harmonic_closeness_centrality2 = my.data2[,13]
Eccentricity2 = my.data2[,11]

x = abs_difference_to_mean_year2
y = Harmonic_closeness_centrality2
regression <- lm(x~y)
summary(regression)
confint(regression)