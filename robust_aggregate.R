robust_aggregate <- function(typeag, bygroup, change_names, d, ag_col, primary_grp, fill_grp){
  #typeag = the typeag of aggregation we want to do 
  #bygroup = T or F, either we are aggregating by multipel groups or just 1
  #change_names = T or F for whether or not we want to change the column names to be more descriptive
  #d = data frame
  #ag_col = column that is to be aggregated
  #primary_grp is the primary group that we are aggregating on
  #fill_grp is the additional layer that we are aggregating on if choose to do so
  
  #statements to indicate sincertiy to initial arguments 
  if(typeag != "avg" & typeag != "percent" & typeag != "count" & typeag != "sum"){
    statement <- "Please specify typeag as 'avg', 'count', 'sum' or 'percent'"
    return(statement) 
  } 
  if(change_names != T & change_names != F){
    statement <- "Please indicate the 'change_names' argument as T or F"
    return(statement)
  }
  if(bygroup != T & bygroup != F){
    statement <- "Please indicate the 'bygroup' argument as T or F"
    return(statement)
  } else{ #if we have none of these issues we continue
    
    if(bygroup == T){ #if we are doing by additional group (fill_grp)
      if(typeag == "avg"){
        ag <- aggregate(d[[ag_col]], list(d[[primary_grp]], d[[fill_grp]]), mean, na.rm = T) #avg
      } else if(typeag == "sum"){
        ag <- aggregate(d[[ag_col]], list(d[[primary_grp]], d[[fill_grp]]), sum, na.rm = T) #sum
      } else if(typeag == "percent"){
        fullag <- aggregate(d[[ag_col]], list(d[[primary_grp]]), length)
        colnames(fullag)[colnames(fullag) == "x"] <- "tot"
        ag <- aggregate(d[[ag_col]], list(d[[primary_grp]], d[[fill_grp]]), length)
        ag <- merge(fullag, ag, by = "Group.1")
        ag$Percent <- ag$x/ag$tot #counts w/percent 
      } else{
        ag <- aggregate(d[[ag_col]], list(d[[primary_grp]], d[[fill_grp]]), length) #there are instances where counts without percents can make merging further data sets easier
      }
    } else{ #if we are not doing by additional group (fill_grp)
      if(typeag == "avg"){
        ag <- aggregate(d[[ag_col]], list(d[[primary_grp]]), mean, na.rm = T) #avg
      } else if(typeag == "sum"){
        ag <- aggregate(d[[ag_col]], list(d[[primary_grp]]), sum, na.rm = T) #sum
      } else{
        ag <- aggregate(d[[ag_col]], list(d[[primary_grp]]), length)
        ag$Percent <- ag$x/nrow(d) #unlike when doing it by group, for the most part percents here are just useful and not cumbersome 
      }
    }
  }
  if(change_names == T){ #if we want to change the names, will do so appropriately. sometimes better not too if planning to merge wtih other data sets. when not doing this, changing names is ideal
    colnames(ag)[colnames(ag) == "Group.1"] <- primary_grp
    if("Group.2" %in% colnames(ag)){
      colnames(ag)[colnames(ag) == "Group.2"] <- fill_grp
    }
    if(typeag == "Percent"){
      colnames(ag)[colnames(ag) == "x"] <- paste0("Count_",ag_col)
    } else{
      colnames(ag)[colnames(ag) == "x"] <- paste0(typeag,"_",ag_col)
    }
    ag
  }
}

#example 
data("mtcars")
avg_mpg_by_cyl <- robust_aggregate("avg", F, T, mtcars, "mpg", "cyl")
avg_mpg_by_cyl_by_gear <- robust_aggregate("avg", T, T, mtcars, "mpg", "cyl", "gear")
