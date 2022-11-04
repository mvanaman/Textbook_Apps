# simulate data for class examples (guessing GPA)
packages <- c("tidyverse", "GGally")
lapply(packages, require, character.only = TRUE)

# n = 120
# DV = GPA
# nominal IGPA = 4 majors (english, phil, history, psych)
# binary IGPA = soc sci vs humanities (determined by majors)
# binary IV2 = employed vs not
# continuous IGPA = number hours studying per week
# continuous IV2 = SAT
cor2cov <- function(corrs, SDs) {
  diag(SDs) %*% corrs %*% diag(SDs)
}
set.seed(352)
corrs <- rbind(c(1, 0.35, 0.65), c(0.35, 1, 0.43), c(0.65, 0.43, 1))
colnames(corrs) <- c("SAT", "GPA", "Hours_Studying")
corrs
SDs <- c(SAT = 300, GPA = 1.1, Hours_Studying = 2.33)
covs <- cor2cov(corrs, SDs)
covs
mu <- c(SAT = 1400, GPA = 3.47, Hours_Studying = 5.88)
dataset <- MASS::mvrnorm(n = 120, mu = mu, Sigma = covs) %>% as_tibble() 
# ## check progress
# dataset %>% summarise(across(where(is.numeric), list(mean = mean, sd = sd)))
# ggpairs(dataset, lower = list(continuous = "smooth", method = "lm"))

# add employment
dataset <- dataset %>%
  mutate(
    Employed = ifelse(
      GPA > median(GPA) |
        SAT > median(SAT) | 
        Hours_Studying > median(Hours_Studying),
      sample(c("No", "Yes"), n(), replace = TRUE, p = c(0.75, 0.25)),
      sample(c("No", "Yes"), n(), replace = TRUE, p = c(0.25, 0.75))
    )
  ) 
# ## check progress
# dataset %>% 
#   group_by(Employed) %>% 
#   summarise(across(where(is.numeric), list(mean = mean, sd = sd)))
# ggpairs(dataset, mapping = aes(color = Employed), lower = list(continuous = "smooth", method = "lm"))

# add major
dataset <- dataset %>%
  mutate(
    Major = case_when(
      GPA < quantile(GPA, 0.25) ~ sample(
        c("G1", "G2", "G3", "G4"),
        n(),
        replace = TRUE,
        p = c(0.70, 0.1, 0.1, 0.1)
      ),
      GPA < quantile(GPA, 0.5) ~ sample(
        c("G1", "G2", "G3", "G4"),
        n(),
        replace = TRUE,
        p = c(0.1, 0.7, 0.1, 0.1)
      ),
      GPA < quantile(GPA, 0.75) ~ sample(
        c("G1", "G2", "G3", "G4"),
        n(),
        replace = TRUE,
        p = c(0.1, 0.1, 0.7, 0.1)
      ),
      TRUE ~ sample(
        c("G1", "G2", "G3", "G4"),
        n(),
        replace = TRUE,
        p = c(0.1, 0.1, 0.1, 0.7)
      )
    )
  ) 
dataset <- dataset %>% mutate(across(where(is.character), as.factor))

# prove it worked
dataset %>% 
  dplyr::select(-Employed, -Major) %>% 
  ggpairs(lower = list(continuous = "smooth", method = "lm"))
dataset %>% 
  dplyr::select(-Major) %>% 
  ggpairs(mapping = aes(color = Employed), lower = list(continuous = "smooth", method = "lm"))
dataset %>% 
  dplyr::select(-Employed) %>% 
  ggpairs(mapping = aes(color = Major), lower = list(continuous = "smooth", method = "lm"))

write.csv(dataset, "dataset.csv")
