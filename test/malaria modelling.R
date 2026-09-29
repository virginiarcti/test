# Install some packages
install.packages('malariasimulation')
library('remotes')
install_github('mrc-ide/malariasimulation')

getwd()
library(readxl)
malaria <- read_excel("Ndhiwa mos data_baseline.xlsx")
View(malaria)
# descriptive

summary(malaria$No.of.LLIN)
summary(malaria$No.of.non_LLIN)
summary(malaria$No.of.female_anopheles)
summary(malaria$num_people)
summary(malaria$Area)

# data visualization
#histogram,box plot jitters.
#bargraph
library(ggplot2)
View(malaria)
ggplot(malaria, aes(x= No.of.LLIN ))+
  geom_bar(position = "dodge", colour= "blue", fill="red")+
  labs(title = "Graph", x= "data", y="Count")+
  theme_minimal()

ggplot(malaria,aes(x= No.of.LLIN))+
   geom_bar(position="dodge",color ="blue", fill="red") +
   labs(title = "Graph",x = "LLN",y = "count") +
   theme_minimal()
#Histogram
library(ggplot2)
ggplot(malaria, aes(x =No.of.female_anopheles)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.3) +
  labs(title = "Histogram of Mosquito Count",
       x = "Mosquito Count",
       y = "Frequency") +
  theme_minimal()
#box plot baseline
#control and intervention
library(ggplot2)
ggplot(malaria, aes(x =arm, y =No.of.female_anopheles )) +
  geom_boxplot(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Boxplot of Mosquito Count",
       x = "",
       y = "Mosquito Count") +
  theme_minimal()
#box plot after intervention
#control and intervention
file.exists("./data/Dec 2024 Mos Data_celingnet.xlsx")
malaria2 <- read_xlsx("./data/Dec 2024 Mos Data_celingnet.xlsx")
View(malaria2)
library(ggplot2)
ggplot(malaria2, aes(x = arm, y =No.of.female_anopheles )) +
  geom_boxplot(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Boxplot of Mosquito Count",
       x = "arm",
       y = "Mosquito Count") +
  theme_minimal()
library(dplyr)
#add time variable(pre-baseline&post-after intervention)
malariam <- malaria%>%
  mutate(time='pre')
View(malariam)
malariam2 <- malaria2%>%
  mutate(time="post")
View(malariam2)
#merge data by rows
merged_data <- bind_rows(malariam, malariam2)
View(merged_data)
#
library(ggplot2)
ggplot(merged_data, aes(x = time, y = No.of.female_anopheles)) +
  geom_boxplot(fill = "blue", color = "black", alpha = 0.7) +
  facet_wrap(~arm)
  labs(title = "Boxplot of MtimeMosquito Count (Merged Data)",
       x = "arm",
       y = "Mosquito Count") +
  theme_minimal()
