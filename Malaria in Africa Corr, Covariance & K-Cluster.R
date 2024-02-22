library(tidyverse)
library(cluster)
install.packages("factoextra")
library(factoextra)
install.packages("Hmisc")
library(Hmisc)
library(reshape2)

Malaria_in_Africa <- read.csv("C:/Users/GBEMUDU .A. ANTHONY/Downloads/DatasetAfricaMalaria.csv")

#Exploratory Analysis
str(Malaria_in_Africa)

#column Names
dim(Malaria_in_Africa)
names(Malaria_in_Africa)
glimpse(Malaria_in_Africa)
head(Malaria_in_Africa)
tail(Malaria_in_Africa)
summary(Malaria_in_Africa)

#Check for null Values
which(is.na(Malaria_in_Africa))
is.na(Malaria_in_Africa)

#Drop irrelevant columns
Malaria_in_Africa <- Malaria_in_Africa [-c(1,2,3,25,26,27)]

names(Malaria_in_Africa)[1] <- "Incidence_per_1000"
names(Malaria_in_Africa)[2] <- "Cases_Reported"
names(Malaria_in_Africa)[3] <- "Use_of_insecticide_nets"
names(Malaria_in_Africa)[4] <- "Children_with_fever_rcv_drugs"
names(Malaria_in_Africa)[5] <- "IPT_of_Preg_women"
names(Malaria_in_Africa)[6] <- "PUSMDWS(% of popul)"
names(Malaria_in_Africa)[7] <- "PUSMDWS(Rural)"
names(Malaria_in_Africa)[8] <- "PUSMDWS(Urban)"
names(Malaria_in_Africa)[9] <- "PUSMSS(% of popul)"
names(Malaria_in_Africa)[10] <- "PUSMSS(Rural)"
names(Malaria_in_Africa)[11] <- "PUSMSS(Urban)"
names(Malaria_in_Africa)[12] <- "Rural Popul(% of ttl popul)"
names(Malaria_in_Africa)[13] <- "Rural popul grwth"
names(Malaria_in_Africa)[14] <- "Urban popul(% of ttl popul)"
names(Malaria_in_Africa)[15] <- "Urban popul grwth"
names(Malaria_in_Africa)[16] <- "PU@LBDWS(% of popul)"
names(Malaria_in_Africa)[17] <- "PU@LBDWS(Rural)"
names(Malaria_in_Africa)[18] <- "PU@LBDWS(Urban)"
names(Malaria_in_Africa)[19] <- "PU@LBSS(% of Popul)"
names(Malaria_in_Africa)[20] <- "PU@LBSS(Rural)"
names(Malaria_in_Africa)[21] <- "PU@LBSS(Urban)"

#Verify column name change
colnames(Malaria_in_Africa)

#Replace NA values with 0
Malaria_in_Africa[is.na(Malaria_in_Africa)] <- 0

#Check for duplicate values
duplicated(Malaria_in_Africa)

#Mean of each Column
colMeans(Malaria_in_Africa) %>%view()


#Correlation Matrix
Mal_corr_Var <- Malaria_in_Africa [,c("Incidence_per_1000","Cases_Reported","Use_of_insecticide_nets",
                                      "Children_with_fever_rcv_drugs","IPT_of_Preg_women", "PUSMDWS(% of popul)",
                                      "PUSMDWS(Rural)","PUSMDWS(Urban)","PUSMSS(% of popul)","PUSMSS(Rural)","PUSMSS(Urban)",
                                      "Rural Popul(% of ttl popul)","Rural popul grwth","Urban popul(% of ttl popul)","Urban popul grwth",
                                      "PU@LBDWS(% of popul)","PU@LBDWS(Rural)","PU@LBDWS(Urban)","PU@LBSS(% of Popul)",
                                      "PU@LBSS(Rural)","PU@LBSS(Urban)")]


cor(Mal_corr_Var) %>% view()

#Correlation Heat Map
corr_Matrix <- cor(Mal_corr_Var)
corr_HeatMap <- melt(corr_Matrix)

#Plot Correlation Heat Map
ggplot(data = corr_HeatMap, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Covariance Matrix
Mal_Cov_Var <- Malaria_in_Africa [,c("Incidence_per_1000","Cases_Reported","Use_of_insecticide_nets",
                                     "Children_with_fever_rcv_drugs","IPT_of_Preg_women", "PUSMDWS(% of popul)",
                                     "PUSMDWS(Rural)","PUSMDWS(Urban)","PUSMSS(% of popul)","PUSMSS(Rural)","PUSMSS(Urban)",
                                     "Rural Popul(% of ttl popul)","Rural popul grwth","Urban popul(% of ttl popul)","Urban popul grwth",
                                     "PU@LBDWS(% of popul)","PU@LBDWS(Rural)","PU@LBDWS(Urban)","PU@LBSS(% of Popul)",
                                     "PU@LBSS(Rural)","PU@LBSS(Urban)")]

cov(Mal_Cov_Var) %>% view()


#Normalize/Scale our data
mal_n_Afri_norm <- as.data.frame(scale(Malaria_in_Africa))

#View Scaled Data frame
view(mal_n_Afri_norm)

#Determining optimal value for k 
set.seed(1234)

##Elbow method
fviz_nbclust(mal_n_Afri_norm,kmeans, method = "wss")

##Silhouette method
fviz_nbclust(mal_n_Afri_norm,kmeans, method = "silhouette")

fviz_nbclust(mal_n_Afri_norm,kmeans, method = "gap_stat")

#Using K=8 for K-Cluster
KCluster <- kmeans(mal_n_Afri_norm, centers = 2, nstart = 25)
str(KCluster)

#View K-cluster
KCluster

#Plot K-Cluster
fviz_cluster(KCluster, data = mal_n_Afri_norm)
