setwd("C:/Users/casteven/OneDrive - Imperial College London/PHD/PHD Work/MAIN_ANALYSIS/FEATURE_IMPORTANCE")## Feature importance 

rm(list = ls())
data_selected <- readRDS("DataPartition/feature_importance_imputed_scaled_dataset.RDS")
columns <- colnames(data_selected)

set.seed(255)
library(performanceEstimation)
# start formula
fi.columns    <- columns[! columns %in% c("APOB","PCSK9","LDLR", 
                                          "diet_oily_fish","diet_raw_veg", 
                                          "diet_cooked_veg","diet_meat","education","country")]
data_selected <- data_selected[, fi.columns]
pred.columns  <- colnames(data_selected)[ colnames(data_selected)!="ANY"]
fi.fml        <- formula(paste("ANY~", paste(pred.columns, collapse="+")))
# end formula
# start sampling
fct.data.train <- data_selected
fct.data.train$ANY <- as.factor(fct.data.train$ANY)
fct.data.train.SMOTE     <- smote(fi.fml, data = as.data.frame(fct.data.train)
                                  , k=2
                                  , perc.over  = 1
                                  , perc.under = 2)
fct.data.train <- fct.data.train.SMOTE[sample(nrow(fct.data.train.SMOTE)),]
fct.data.train$ANY <- as.numeric(ifelse( fct.data.train$ANY == "1", 1, 0 ))
# end sampling
options("na.action" =  "na.pass")
fi.fml.mat        <- formula(paste("~", paste(pred.columns, collapse="+")))
design.matrix     <- model.matrix(fi.fml.mat , fct.data.train )
options("na.action"  = "na.omit")
require(xgboost)

xgbcv <- xgb.cv(data = design.matrix, label = fct.data.train$ANY, nrounds = 100, nfold = 6, max_depth = 3, objective = "binary:logistic",
                eta = 0.50, nthread = 1, showsd = T,stratified = T, print_every_n = 1, early_stopping_rounds = 2, eval_metric = "auc")
xgb_cat <- xgboost(data = design.matrix, max_depth = 3, label = fct.data.train$ANY, objective = "binary:logistic",
                   eta = 0.50,  nthread = 1, nrounds = xgbcv$best_iteration, print_every_n = 10, 
                   early_stopping_rounds = 2, eval_metric = "auc")

# view variable importance plot
mat <- xgb.importance(feature_names = colnames(design.matrix), model = xgb_cat)[1:10, ]
mat$Feature <-   c("1: LDL-C (calculated)", "2: Statin", "3: LDL-C (measured)", "4: FamHx CAD",
                   "5: ATC code C","6: Apolipoprotein B","7: Tryglicerides","8: Waist","9: Age","10: Gamma-glutamyltransferase")

#require(Ckmeans.1d.dp) # install.packages("Ckmeans.1d.dp")
a <- cbind(mat[,c("Feature","Gain")], metric="Gain")
b <- cbind(mat[,c("Feature","Frequency")], metric="Frequency")
c <- cbind(mat[,c("Feature","Cover")], metric="Cover")

colnames(a) <- colnames(b) <- colnames(c)  <- c("Feature","Value","Metrics")

library(dplyr)
data <- do.call(rbind, list(a,b,c))
data$Feature <- factor(data$Feature, (data %>% filter(Metrics=="Gain") %>% arrange(Value) %>% select(Feature))$Feature)


library(ggplot2)
tiff(filename="featureImportance.tiff", units="in", width=8, height=5,  res=300)
data %>% arrange(-Value) %>% ggplot(aes(x=Feature, y=Value, fill = Metrics)) +
  geom_bar(stat="identity",position=position_dodge()) +   coord_flip()  + theme_minimal()
dev.off()

