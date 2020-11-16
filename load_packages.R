load_packages <- function(){
  list.of.packages <- c("ggpubr","plyr", "lubridate", "zoo", "dplyr", "odbc", "ggplot2", "crayon", "rmarkdown", 
                        "esquisse", "shiny", "grid", "gridExtra", "scales", "caret", "randomForest", "openxlsx", "RColorBrewer", 
                        "data.table", "tidyr", "FactoMineR", "corrplot", "Hmisc", "corrplot", "rfUtilities", "colortools", 
                        "stringr", "tm", "stringi", "dbplyr", "DBI", "tidytext", "keras", "gtools", "intervals", "ngram",
                        "topicmodels", "wordcloud2", "wordcloud", "gbm", "xgboost", "pROC", "plotROC", "ROCR", 
                        "mvmeta", "ggrepel"
  )
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  library(ggpubr)
  library(plyr)
  library(lubridate)
  library(zoo)
  library(dplyr)
  library(odbc)
  library(ggplot2)
  library(crayon)
  library(rmarkdown)
  library(esquisse)
  library(shiny)
  library(grid)
  library(gridExtra)
  library(scales)
  library(caret)
  library(randomForest)
  library(openxlsx)
  library(RColorBrewer)
  library(data.table)
  library(tidyr)
  library(FactoMineR)
  library(corrplot)
  library(Hmisc)
  library(corrplot)
  library(rfUtilities)
  library(colortools)
  library(stringr)
  library(tm)
  library(stringi)
  library(dbplyr)
  library(DBI)
  library(tidytext)
  library(keras)
  library(gtools)
  library(intervals)
  library(ngram)
  library(topicmodels)
  library(wordcloud2)
  library(wordcloud)
  library(gbm)
  library(xgboost)
  library(pROC)
  library(plotROC)
  library(ROCR)
  library(mvmeta)
  library(ggrepel)
  
}

load_packages()