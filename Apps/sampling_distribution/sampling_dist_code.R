library(tidyverse)

grades.all <- read_csv("/Users/home/Documents/Teaching/Previous/Volz summer 19/lab_stuff/lab_stuff_directory/data/grades.csv") %>% select(1:2)
grades.all <- reshape2::melt(grades.all)
colnames(grades.all) <- c("Homework", "Grade")
### Homeworks: Hist. All HWs so far ----

y <- matrix(c(round(mean(grades.all[, "Grade"]), 0),
              round(sd(grades.all[, "Grade"]), 0)),
            nrow= 2, ncol = 1)
x <- matrix(c("Mean =", "Standard Deviation ="), 
            nrow = 2, ncol = 1)
stats.sofar <- matrix(paste(x, y))
hw.sofar.plot <- ggplot(grades.all, aes(x= Grade)) + 
  geom_histogram(aes(y = ..count..), binwidth = 5, colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666", aes(y = (..scaled.. * max(table(grades.all$Grade))))) +
  labs(title = "Histogram: HW Grades So Far") +
  ylab("Frequency") + xlab("Grade") + 
  theme_classic()  

hw.sofar.plot
cat("\n", stats.sofar[1, ], "\n", stats.sofar[2, ])


## Random Sample of 30 grades.all # 1 (no really, it's random...) ----
sim <- as.data.frame(sample(grades.all[, 2], replace = FALSE, size = 10))
colnames(sim) <- "Simulatedgrades.all"
y <- matrix(c(round(mean(sim$Simulatedgrades.all), 0), 
              round(sd(sim$Simulatedgrades.all), 0)),
            nrow= 2, ncol = 1)
x <- matrix(c("Mean =", "Standard Deviation ="), 
            nrow = 2, ncol = 1)
stats <- matrix(paste(x, y))

ggplot(sim, aes(x= Simulatedgrades.all)) + 
  geom_histogram(aes(y= (..ncount..) * max(sim)), binwidth = 5, colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666", aes(y = (..scaled..) * max(sim))) +
  labs(title = "Rando Sample of Grades (n = 10)") +
  ylab("Frequency") + xlab("Grade") +
  theme_classic() +
  theme(text = element_text(size=10, 
                            family = "Times New Roman"),
        axis.text.x = element_text(angle=90, 
                                   hjust=1),
        plot.title = element_text(hjust = 0.5)) 

cat("\n", stats[1, ], "\n", stats[2, ])

## Means from 5000 samples, 100 grades.all in each sample ----
sample.means <- rep(NA, 100) # 100 = number of means
for(i in 1:100){
  samp <- sample(grades.all[, 2], replace = FALSE, size = 30)
  sample.means[i] <- mean(samp)
}
sample.means <- as.data.frame(sample.means)
colnames(sample.means) <- "Sample.Means"
y <- matrix(c(round(mean(sample.means$Sample.Means), 1),
              round(sd(sample.means$Sample.Means), 1)),
            nrow= 2, ncol = 1)
x <- matrix(c("Mean =", "Standard Error ="), 
            nrow = 2, ncol = 1)
stats <- matrix(paste(x, y))

ggplot(sample.means, aes(x= sample.means$Sample.Means)) + 
  geom_histogram(aes(y= (..ncount..) * max(sample.means$Sample.Means)), binwidth = 0.5, colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666", aes(y = (..scaled..) * max(sample.means$Sample.Means))) +
  labs(title = "HW Sampling Distribution (100 means, n = 30)") +
  ylab("Frequency") + xlab("Sample Mean") +
  theme_classic() +
  theme(text = element_text(size=10, 
                            family = "Times New Roman"),
        axis.text.x = element_text(angle=90, 
                                   hjust=1),
        plot.title = element_text(hjust = 0.5)) 

cat("\n", stats[1, ], "\n", stats[2, ])


## HW 3 Only ----
HW3 <- subset(grades.all, Homework == "HW3")
# Plot
y <- matrix(c(round(mean(HW3$Grade), 1),
              round(sd(HW3$Grade), 1)),
            nrow= 2, ncol = 1)
x <- matrix(c("Mean =", "Standard Error ="), 
            nrow = 2, ncol = 1)
stats <- matrix(paste(x, y))
ggplot(HW4, aes(x= HW3$Grade)) + 
  geom_histogram(aes(y= (..ncount..) * max(HW3$Grade)), binwidth = 0.5, colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666", aes(y = (..scaled..) * max(HW3$Grade))) +
  labs(title = "HW grades.all Sampling Distribution") +
  ylab("Frequency") + xlab("Sample Mean") +
  theme_classic() +
  theme(text = element_text(size=30, 
                            family = "Times New Roman"),
        axis.text.x = element_text(angle=90, 
                                   hjust=1),
        plot.title = element_text(hjust = 0.5)) 

cat("\n", stats[1, ], "\n", stats[2, ])


## Means from 100 samples, 50 grades in each sample ----
sample.means <- rep(NA, 100) # 100 = number of means
for(i in 1:100){
  samp <- sample(grades.all[, 2], replace = FALSE, size = 50)
  sample.means[i] <- mean(samp)
}
sample.means <- as.data.frame(sample.means)
colnames(sample.means) <- "Sample.Means"
y <- matrix(c(round(mean(sample.means$Sample.Means), 1),
              round(sd(sample.means$Sample.Means), 1)),
            nrow= 2, ncol = 1)
x <- matrix(c("Mean =", "Standard Error ="), 
            nrow = 2, ncol = 1)
stats <- matrix(paste(x, y))

ggplot(sample.means, aes(x= sample.means$Sample.Means)) + 
  geom_histogram(aes(y= (..ncount..) * max(sample.means$Sample.Means)), binwidth = 0.5, colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666", aes(y = (..scaled..) * max(sample.means$Sample.Means))) +
  labs(title = "HW Sampling Distribution (100 means, n = 50)") +
  ylab("Frequency") + xlab("Sample Mean") +
  theme_classic() +
  theme(text = element_text(size=10, 
                            family = "Times New Roman"),
        axis.text.x = element_text(angle=90, 
                                   hjust=1),
        plot.title = element_text(hjust = 0.5)) 

cat("\n", stats[1, ], "\n", stats[2, ])




## Means from 100 samples, 21 grades in each sample ----
sample.means <- rep(NA, 100) # 100 = number of means
for(i in 1:100){
  samp <- sample(grades.all[, 2], replace = FALSE, size = 21)
  sample.means[i] <- mean(samp)
}
sample.means <- as.data.frame(sample.means)
colnames(sample.means) <- "Sample.Means"
y <- matrix(c(round(mean(sample.means$Sample.Means), 1),
              round(sd(sample.means$Sample.Means), 1)),
            nrow= 2, ncol = 1)
x <- matrix(c("Mean =", "Standard Error ="), 
            nrow = 2, ncol = 1)
stats <- matrix(paste(x, y))

ggplot(sample.means, aes(x= sample.means$Sample.Means)) + 
  geom_histogram(aes(y= (..ncount..) * max(sample.means$Sample.Means)), binwidth = 0.5, colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666", aes(y = (..scaled..) * max(sample.means$Sample.Means))) +
  labs(title = "HW Sampling Distribution (100 means, n = 21)") +
  ylab("Frequency") + xlab("Sample Mean") +
  theme_classic() +
  theme(text = element_text(size=10, 
                            family = "Times New Roman"),
        axis.text.x = element_text(angle=90, 
                                   hjust=1),
        plot.title = element_text(hjust = 0.5)) 

cat("\n", stats[1, ], "\n", stats[2, ])





## Means from 100 samples, 5 grades in each sample ----
sample.means <- rep(NA, 100) # 100 = number of means
for(i in 1:100){
  samp <- sample(grades.all[, 2], replace = FALSE, size = 6)
  sample.means[i] <- mean(samp)
}
sample.means <- as.data.frame(sample.means)
colnames(sample.means) <- "Sample.Means"
y <- matrix(c(round(mean(sample.means$Sample.Means), 1),
              round(sd(sample.means$Sample.Means), 1)),
            nrow= 2, ncol = 1)
x <- matrix(c("Mean =", "Standard Error ="), 
            nrow = 2, ncol = 1)
stats <- matrix(paste(x, y))

ggplot(sample.means, aes(x= sample.means$Sample.Means)) + 
  geom_histogram(aes(y= (..ncount..) * max(sample.means$Sample.Means)), binwidth = 0.5, colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666", aes(y = (..scaled..) * max(sample.means$Sample.Means))) +
  labs(title = "HW Sampling Distribution (100 means, n = 5)") +
  ylab("Frequency") + xlab("Sample Mean") +
  theme_classic() +
  theme(text = element_text(size=10, 
                            family = "Times New Roman"),
        axis.text.x = element_text(angle=90, 
                                   hjust=1),
        plot.title = element_text(hjust = 0.5)) 

cat("\n", stats[1, ], "\n", stats[2, ])





### One-sample T.test vs z-test ----
HW2 <- subset(grades.all, Homework == "HW2")
mean(grades.all$Grade)
sd(grades.all$Grade)

mean(HW2)
sd(HW2$Grade)

ZTest(HW2$Grade, mu = 78.94898, sd = 27.56243, alternative = "two.sided")
t.test(HW2$Grade, mu = 78.94898, alternative = "two.sided")

nrow(HW2)


### Samp Disof Differences ----
sample.means1 <- rep(NA, 100) # 100 = number of means
for(i in 1:100){
  samp1 <- sample(grades[, 2], replace = TRUE, size = 100)
  sample.means1[i] <- mean(samp1)
}

sample.means2 <- rep(NA, 100) # 100 = number of means
for(i in 1:100){
  samp2 <- sample(grades[, 2], replace = TRUE, size = 100)
  sample.means2[i] <- mean(samp2)
}
sample.means.diff <- sample.means1 - sample.means2

y <- matrix(c(round(mean(sample.means.diff), 1),
              round(sd(sample.means.diff), 1)),
            nrow= 2, ncol = 1)
x <- matrix(c("Mean =", "Standard Error ="), 
            nrow = 2, ncol = 1)
stats <- matrix(paste(x, y))

sample.means.diff <- as.data.frame(sample.means.diff)


ggplot(sample.means.diff, aes(x= sample.means.diff)) + 
  geom_histogram(aes(y= (..ncount..) * max(sample.means.diff[,1])), binwidth = 0.5, colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666", aes(y = (..scaled..) * max(sample.means.diff[,1]))) +
  labs(title = "HW Sampling Distribution of Differences") +
  ylab("Frequency") + xlab("Sample Mean") +
  theme_classic() +
  theme(text = element_text(size=10, 
                            family = "Times New Roman"),
        axis.text.x = element_text(angle=90, 
                                   hjust=1),
        plot.title = element_text(hjust = 0.5)) 

cat("\n", stats[1, ], "\n", stats[2, ])

### Independent-samples T-test ----
t.test(sample.means1, sample.means2, var.equal = FALSE)
