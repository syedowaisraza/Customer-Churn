# Required libraries
library(ggplot2)
library(lattice)
library(caret)
library(e1071)
library(Hmisc)
library(dplyr)
library(stringr)
library(patchwork)
library(cowplot)
library(corrplot)
library(tidyverse)
library(reshape2)
library(scales)
library(writexl)
library(plotly)
library(DMwR)
library(ggcorrplot)
library(gtsummary)

max.print = 1000000

# Read the data
D<- read.csv(file = 'C:/Users/marwa/OneDrive/Desktop/machinelearning/Telco_customer_churn.csv')
#Cleaning the data by removing unique columns
Data<- select(D,-c(1,2,3,4,29))
atr <- attributes(Data)

#write_xlsx(Data, "datanumeric.xlsx")
#percentage of churn
df <- data.frame(
  "Churn.Label" = sample(c("Yes", "No"), 7043, replace=TRUE),
  "CustomerID" = 1:7043
)

# Count number of customers with each label
df_count <- df %>%
  count("Churn.Label") %>%
  mutate(percent = n / sum(n))


churn_pct <- prop.table(table(df$Churn.Label)) * 100
fig <- plot_ly(labels = names(churn_pct), values = churn_pct, type = "pie")
fig
#pdf("myplot.pdf")

churn_counts <- table(D$`Churn.Label`)

finalfigure<- plot_ly(labels= names(churn_counts), values= churn_counts, type = "pie")
finalfigure



#Pre-processing the dataset to convert all the features to numeric value
for (i in 1:(ncol(Data)-1)) {
  if (is.character(Data[, i])==TRUE){
    for(j in 1:nrow(Data)){
      ascis <- as.numeric(charToRaw(Data[j, i]))
      Data[ j, i] <- sum(ascis)
    }
  }
  Data[,i] <- as.numeric(Data[,i])
}

# select numeric features for correlation matrix
features <- c('Tenure.Months', 'Monthly.Charges','Gender','Dependents','Churn.Value')


# create correlation matrix and convert to data frame
corr_matrix <- cor(Data[, features])
corr_matrix_df <- melt(as.matrix(corr_matrix))

# set color scale for correlation values
color <- colorRampPalette(c("#9966FF", "#CCCCFF","white", "#663366", "#984ea3"))(n = 100)

# plot correlation matrix with heatmap
ggplot(data = corr_matrix_df, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + 
  scale_fill_gradientn(colors = color) + 
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#number of churn
grouped_datanumber <- aggregate(CustomerID ~ Churn.Label, D, function(x) length(unique(x)))

# Create pie chart
figurechurn <- plot_ly(grouped_datanumber, labels = ~Churn.Label, values = ~CustomerID, type = 'pie',marker = list(colors = c("#9966FF", "#CCCCFF")))

# Set the title of the chart
figurechurn <- figurechurn %>% layout(title = "Customer Churn %")

# Show the plot
figurechurn






#converting target column to factor
Data[,ncol(Data)] <-(as.factor(Data[,ncol(Data)]))
#omitting empty rows only 0.15% of data is missing values
newData<-na.omit(Data)
#plots

tbl_summary(Data)



correlation_matrix <- cor(newData[, 1:ncol(newData) - 1])
correlation_matrix
corrplot(correlation_matrix, method = "number", type = "full", order = "hclust", addCoef.col = "black", tl.cex = 0.8, number.cex = 0.6)

# whisker plot for the input variables 
par(mfrow=c(1,5))
boxplot(newData[,10], main=names(newData)[10])
boxplot(newData[,12], main=names(newData)[12])
boxplot(newData[,22], main=names(newData)[22])
boxplot(newData[,4], main=names(newData)[4])
boxplot(newData[,5], main=names(newData)[5])

#histogram to observe distribution of variables of the dataset
par(mfrow=c(1,5))
hist(Data[,7])
hist(Data[,21])
hist(Data[,10])
hist(Data[,18])
hist(Data[,20])

#genderplot
genderdist<-as.data.frame(prop.table(table(D$Gender))*100)
colnames(genderdist)<- c("Gender", "Percentage")
genderplot1<- ggplot(genderdist, aes(x = Gender, y = Percentage, fill = Gender)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#9966FF", "#CCCCFF")) +
  theme_minimal() +
  labs(title = "Distribution of Gender %", x = "Gender", y = "Percentage") +
  ylim(0, 100) +
  theme(legend.position = "none")


genderchurn <- D %>%
  group_by(`Gender`, `Churn.Label`) %>%
  summarise(count = n())

# create bar plot with facet_wrap
genderplot2<- ggplot(genderchurn, aes(x = `Gender`, y = count, fill = `Churn.Label`, text = count)) +
  geom_col(position = "dodge") +
  facet_wrap(~ `Gender`, nrow = 1) +
  labs(title = "Number of churn customers by Gender",
       x = "Gender",
       y = "Count",
       fill = "Churn.Label") +
  theme_bw() +
  scale_fill_manual(values = c("#8B00FF", "#BF3EFF"))
combined_plot <- genderplot1 + genderplot2 + plot_layout(ncol = 2)

combined_plot

#tenureplot
Tenurdist<-as.data.frame(prop.table(table(D$Tenure.Months))*100)
colnames(Tenurdist)<- c("Tenure.Months", "Percentage")
ggplot(Tenurdist, aes(x = Tenure.Months, y = Percentage, fill = Tenure.Months,  cex.axis = 0.8)) +
  geom_bar(stat = "identity") + 
  theme_minimal() +
  labs(title = "Distribution of Tenure.Months %", x = "Tenure.Months", y = "Percentage") + 
  ylim(0, 10) +
  theme(legend.position = "none")
#tenurebychurn

fig0 <- plot_ly(D, x = ~Tenure.Months, color = ~Churn.Label, type = "histogram")
fig0 <- fig0 %>% add_histogram(bingroup = 1, name = "No")
fig0 <- fig0 %>% add_histogram(bingroup = 1, name = "Yes")
fig0 <- fig0 %>% layout(title = "Tenure Months Distribution by Churn Label",
                        xaxis = list(title = "Tenure Months"),
                        yaxis = list(title = "Count"),
                        bargap = 0.2,
                        barmode = "overlay",
                        boxmode = "group",
                        legend = list(title = "Churn Label", x = 0.85, y = 0.95))

fig0


#dependents plot
dependents<-D$Dependents
dependents1<- prop.table(table(dependents))*100
axes1<- ggplot() + 
  geom_bar(stat =  "count", aes(x= dependents,fill = dependents)) +
  scale_fill_manual(values = c("#9966FF", "#CCCCFF", "#663366", "#984ea3")) + 
  ggtitle("Distribution of Dependents data %") +
  ylab("Percentage") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
axes1

dependentschurn <- D %>%
  group_by(`Dependents`, `Churn.Label`) %>%
  summarise(count = n())

# create bar plot with facet_wrap
dependentsplot2<- ggplot(dependentschurn, aes(x = `Dependents`, y = count, fill = `Churn.Label`, text = count)) +
  geom_col(position = "dodge") +
  facet_wrap(~ `Dependents`, nrow = 1) +
  labs(title = "Number of churn customers by Dependents",
       x = "Dependents",
       y = "Count",
       fill = "Churn.Label") +
  theme_bw() +
  scale_fill_manual(values = c("#8B00FF", "#BF3EFF"))
combined_plotdependents <- axes1 + dependentsplot2 + plot_layout(ncol = 2)
combined_plotdependents

#churnreasonplot
reasongrouped_data <- D %>%
  count(`Churn.Reason`, name = "Count") %>%
  arrange(desc(Count))

reasonfig <- plot_ly(data = reasongrouped_data,
                     x = ~`Churn.Reason`,
                     y = ~Count,
                     type = 'bar',
                     marker = list(color = ~Count, colorscale = "Viridis"),
                     text = ~Count,
                     textposition = 'auto') %>%
  layout(title = "Churn Reasons",
         xaxis = list(title = "Churn Reason"),
         yaxis = list(title = "Number of Customers"))


reasonfig
#correlation
corr_df <- D %>%
  mutate(`Churn.Label` = ifelse(`Churn.Label` == "Yes", 1, 0))
selected_columns <- corr_df %>%
  select(`Churn.Label`, `Phone.Service`, `Multiple.Lines`, `Internet.Service`, `Online.Security`,
         `Online.Backup`, `Device.Protection`, `Tech.Support`, `Streaming.TV`, `Streaming.Movies`)

# Convert the categorical columns into dummy variables
df_dummies <- model.matrix(~ . - 1, data = selected_columns)

# Convert the resulting matrix back to a data frame
df_dummies <- as.data.frame(df_dummies)

# Calculate correlation and sort values
correlations <- cor(df_dummies)
sorted_correlations <- sort(correlations[,'Churn.Label'], decreasing = TRUE)

# Convert the sorted correlations to a data frame
correlation_data <- data.frame(Feature = names(sorted_correlations),
                               Correlation = as.numeric(sorted_correlations))

# Create the bar chart using ggplot2
dummyplot <- ggplot(correlation_data, aes(x = reorder(Feature, Correlation), y = Correlation, fill = Correlation)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Feature Correlations with Churn Label",
       x = "Feature",
       y = "Correlation",
       fill = "Correlation") +
  coord_flip() +
  scale_fill_gradient(low = "#9966FF", high = "#CCCCFF", 
                      limits = c(-1, 1), 
                      breaks = c(-1, -0.5, 0, 0.5, 1), 
                      labels = c("-1", "-0.5", "0", "0.5", "1"), 
                      name = "Correlation") +
  theme_minimal()

dummyplot

#internetsecurityplot

figinternet <- plot_ly(D %>% 
                         group_by(`Internet.Service`, `Churn.Label`) %>% 
                         summarize(CustomerID_count = n()), 
                       values = ~CustomerID_count, 
                       facet_col = ~`Churn.Label`, 
                       labels = ~`Internet.Service`, 
                       type = 'pie', 
                       title = 'What type of internet was connected to the clients who left the service?', 
                       marker = list(colors = c( '#BEAED4',"#9966FF","#984ea3")))

figinternet <- figinternet %>% layout(title = "What type of internet was connected to the clients who left the service?")
figinternet


# group and count data by Internet Service, Online Security and Churn Label
secandinternet <- D %>%
  group_by(`Internet.Service`, `Online.Security`, `Churn.Label`) %>%
  summarise(count = n())

# create bar plot with facet_wrap
ggplot(secandinternet, aes(x = `Internet.Service`, y = count, fill = `Churn.Label`, text = count)) +
  geom_col(position = "dodge") +
  facet_wrap(~ `Online.Security`, nrow = 1) +
  labs(title = "Number of customers by Internet Service and Online Security",
       x = "Internet.Service",
       y = "Count",
       fill = "Churn.Label") +
  theme_bw() +
  scale_fill_manual(values = c("#8B00FF", "#BF3EFF"))

#feature selection

FeatureSelectedData<-subset(newData, select = c(Churn.Reason,Tenure.Months,Contract, Online.Security, Tech.Support, Internet.Service, Online.Backup,Device.Protection,Total.Charges,Streaming.Movies,Monthly.Charges,Streaming.TV,Dependents,Paperless.Billing,Partner,CLTV,Payment.Method,Senior.Citizen,Churn.Value))

#normalised
FeatureSelectedData[,1:(ncol(FeatureSelectedData)-1)] <- scale(FeatureSelectedData[,1:(ncol(FeatureSelectedData)-1)]) # Exercise - 8

# header line of on-screen performance metrics
message("\t TR-Data% \t TS-Data% \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t Precision% \t Recall%")
# header line of TXT file's performance metrics
cat("---------------------------------------------------------------------------------------------------------------------------------", file = "C:/Users/marwa/OneDrive/Desktop/machinelearning/nb/nb_result.txt", sep = "\n")
cat("\t TR-Data% \t TS-Data% \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity%\t Precision% \t Recall%", file = "C:/Users/marwa/OneDrive/Desktop/machinelearning/nb/nb_result.txt", sep = "\n", append = TRUE) # Apply cat & append
cat("---------------------------------------------------------------------------------------------------------------------------------", file = "C:/Users/marwa/OneDrive/Desktop/machinelearning/nb/nb_result.txt", sep = "\n", append = TRUE)
# store performance metrics scores
pf = data.frame(matrix(
  vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50",
                                  "TR-60", "TR-70", "TR-80", "TR-90"),
                                c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),
  stringsAsFactors=F)
pfc <- 0 # pfc - performance frame counter
training_data_percentages <- seq(from = 0.1, to = 0.9, length.out = 9) # creating sequence to represent training data ratio
# varying training data amount to check the Naivebayes performance
for (t in training_data_percentages){
  pfc <- pfc+1
  partition <- createDataPartition(FeatureSelectedData$Churn.Value, p=t, list=FALSE) # index of training data
  training_data <- FeatureSelectedData[partition,] # training dataset
  testing_data <- FeatureSelectedData[-partition,] # testing dataset
  #balancing the data
  Databalanced <- SMOTE(Churn.Value~., training_data, perc.under = 200)
  
  #Seed value
  set.seed(120)
  #  Naivebayes model
  NB_model <- naiveBayes(Churn.Value ~ ., data = Databalanced, laplace=3)
  #Predicting on test data
  testingPredicted_outcomes <- predict(NB_model, newdata =
                                  testing_data[,1:ncol(testing_data)-1])
 
  #creating rules in the loop for customer profiling
  
   cat("Naive Bayes Rules (Conditional Probability Tables) for training/testing data ratio:", t, "\n")
  feature_names <- colnames(Databalanced)[!colnames(Databalanced) %in% "Churn.Value"]
  for (feature_name in feature_names) {
    cat("\nFeature:", feature_name, "\n")
    print(NB_model$tables[[feature_name]])
  }
  
  
  # Confusion Matrix
  cm <- confusionMatrix(testing_data$Churn.Value, testingPredicted_outcomes)
  print(cm)
  # below message() function shows the performance metrics on-screen
  message("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
          format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
          format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2))
  # write the performance metrics in TXT file
  cat("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
      format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
      format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n", file
      = "C:/Users/marwa/OneDrive/Desktop/machinelearning/nb/nb_result.txt", sep = " ", append = TRUE)
  # --------- assigning the performance metrics ----------------
  pf[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =
                                 2)
  pf[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
  pf[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2),
                                  nsmall = 2)
  pf[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2),
                                  nsmall = 2)
  pf[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall
                                = 2)
  pf[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)
}



#result visualisation
Accuracy<- 0.98
Sensitivity <- 0.98
specificity <- 0.99
Precision<- 0.99
eval_metrics <- tibble(
  Metric = c("Accuracy", "Sensitivity", "Specificity", "Precision"),
  Value = c(Accuracy, Sensitivity, specificity,Precision)
)
figriskgroup<- ggplot(eval_metrics, aes(x = Metric, y = Value, fill = Metric)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.2f", Value)), vjust = -0.5, size = 4, color = "white") +
  labs(title = "Naive Bayes Model Evaluation Metrics",
       x = "",
       y = "") +
  scale_fill_manual(values = c('#BEAED4',"#9966FF","#984ea3","#CCCCFF")) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 14, color = "black", hjust = 0.5),
        axis.text.y = element_text(size = 14, color = "black"),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5))

figriskgroup

#plot of profile feature
dataprofile <- list(
  Churn.Reason=list(Y=c(0,1), mean=c(-0.5562449, 1.5412514), sd=c(0.0000000, 0.6767731)),
  Tenure.Months=list(Y=c(0,1), mean=c(0.2251019, -0.6234034), sd=c(0.9930378, 0.7697948)),
  Contract=list(Y=c(0,1), mean=c(-0.2254401, 0.6816319), sd=c(0.9970850, 0.6308081)),
  Paperless.Billing = list(Y=c(0, 1), mean=c(-0.09981825, 0.28210603), sd=c(1.0138018, 0.8996757))

)


df <- data.frame(Feature=character(), Y=factor(), Mean=numeric(), SD=numeric())
for (feature_name in names(dataprofile)) {
  feature_data <- dataprofile[[feature_name]]
  temp_df <- data.frame(Feature=feature_name, Y=as.factor(feature_data$Y), Mean=feature_data$mean, SD=feature_data$sd)
  df <- rbind(df, temp_df)
}

#  lower and upper bounds for the boxplot
df$Lower <- df$Mean - df$SD
df$Upper <- df$Mean + df$SD

# Convert the data frame to a melted format for ggplot
df_melted <- melt(df, id.vars=c("Feature", "Y"), variable.name="Statistic", value.name="Value")

#  grouped boxplot
ggplot(df, aes(x=factor(Y), y=Mean, fill=Y)) +
  geom_tile(aes(width = 0.5, height = Upper - Lower)) +
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=0.1) +
  geom_point() +
  facet_wrap(~ Feature, scales="free_y", ncol=2) + # Adjust the number of columns for the number of features
  labs(title="Custom Boxplot-like of Features", x="Y", y="Value") +
  theme_minimal() +
  theme(legend.position="none")
