#function to iterate through robust aggregate to get multiple ags in 1 df 
#need to edit the column names/output and need to ensure values appear in order they are written
iterate_robust_aggregate <- function(typeag, bygroup, d, ag_col, primary_grp, fill_grp){
  
  #pretty much the same arguments as the robust aggregate
  #change column names is predifned in function
  #can add a vector c("col_name1", "col_name2) for any ag_col, primary_grp, or fill_grp
    #funciton will iterate over each group 
  #fill_grp must be present but can be NA if don't want to use
  
  aglist <- list()
  aglist2 <- list()
  
  # if(length(ag_col) > 1){
  #   ag_col <- factor(ag_col, ordered = T)
  # } #need to figure out how to get it so that values will appear in order they are written

  #if all groups are set to 1, we don't have anything to iterate 
  if(length(ag_col) == 1 & length(primary_grp) == 1 & length(fill_grp) == 1){
    statement <- "No need to iterate if there is only 1 of all aggregation metrics"
    return(statement)
  }
  
  #if we need all different items, output likely to be confusing, don't allow for now
  if(length(ag_col) > 1 & length(primary_grp) > 1 & length(fill_grp) > 1){
    statement <- "Unadvisable to have all changing groups, output will be difficult to follow. Will fix"
    return(statement)
  }
  
  #if we want different ag_col for same groups (do not need a fill group)
  if(length(ag_col) > 1 & length(primary_grp) == 1 & length(fill_grp) == 1){ 
    for(i in levels(unique(as.factor(ag_col)))){ #for each ag_col
      ag <- robust_aggregate(typeag = typeag, bygroup = bygroup, change_names = T, 
                             d = d, ag_col = i, primary_grp = primary_grp, fill_grp = fill_grp)
      aglist[[i]] <- ag #take aggregates and combine to list 
    }
    if(bygroup == T){ #if this is by group must merge by both groups 
      ag <- Reduce(function(x,y) merge(x,y, by = c(primary_grp, fill_grp)), aglist)
    } else{
      ag <- Reduce(function(x,y) merge(x,y, by = primary_grp), aglist)
    }
  } 
  
  #if we want different ag col with same primary group and different fill group 
  #(or same fill group and different primary group - at that point results are equiv)
  if(length(ag_col) > 1 & 
     ((length(primary_grp) == 1 & length(fill_grp) > 1) | (length(primary_grp) > 1 & length(fill_grp) == 1))){
    if(length(primary_grp) == 1){ #if the fill group has multiple values 
      for(j in levels(unique(as.factor(fill_grp)))){ #for each fill group
        for(i in levels(unique(as.factor(ag_col)))){ #for each ag val
          ag <- robust_aggregate(typeag = typeag, bygroup = bygroup, change_names = T, 
                                 d = d, ag_col = i, primary_grp = primary_grp, fill_grp = j) #take ags
          ag$Group.2_Type <- j #define the type that will appear in the fill group
          aglist[[i]] <- ag
        }
        if(bygroup != T){
          statement <- "If bygroup is False, cannot take changing fill group, because fill group DNE"
          return(statement)
        } else{
          ag <- Reduce(function(x,y) merge(x,y, by = c(j, primary_grp, "Group.2_Type")), aglist) #merge 
        }
        colnames(ag)[colnames(ag) == j] <- "Group.2" #rename column so can combine with rbindlist
        aglist2[[j]] <- ag #append to J list
      }
      ag <- rbindlist(aglist2) #required dplyr 
    } else{ #if the primary grp has multiple values 
      for(j in levels(unique(as.factor(primary_grp)))){ 
        for(i in levels(unique(as.factor(ag_col)))){
          ag <- robust_aggregate(typeag = typeag, bygroup = bygroup, change_names = T, 
                                 d = d, ag_col = i, primary_grp = j, fill_grp = fill_grp)
          ag$Group.2_Type <- j
          aglist[[i]] <- ag
        }
        if(bygroup == T){
          ag <- Reduce(function(x,y) merge(x,y, by = c(j, fill_grp, "Group.2_Type")), aglist)
        } else{
          ag <- Reduce(function(x,y) merge(x,y, by = c(j, "Group.2_Type")), aglist)
        }
        colnames(ag)[colnames(ag) == j] <- "Group.2"
        aglist2[[j]] <- ag
      }
      ag <- rbindlist(aglist2) #required dplyr 
    }
    
    #ag[ ,c(1,3,2,4:ncol(ag))] #need to re-order columns this isn't working
  }
  
  #if we want same ag_col for same primary group for different fill grp
  #(or if we want same ag_col for dif primary grp and same fill - same results)
  if(length(ag_col) == 1 & 
     ((length(primary_grp) == 1 & length(fill_grp) > 1) | (length(primary_grp) > 1 & length(fill_grp) == 1))){
    if(length(primary_grp) == 1){ #if flll_grp has multiple values 
      for(i in levels(unique(as.factor(fill_grp)))){
        ag <- robust_aggregate(typeag = typeag, bygroup = bygroup, change_names = T, 
                               d = d, ag_col = ag_col, primary_grp = primary_grp, fill_grp = i)
        colnames(ag)[colnames(ag) == i] <- "Group.2"
        ag$Group.2_Type <- i
        aglist[[i]] <- ag
      }
      if(bygroup != T){
        statement <- "If bygroup is False, cannot take changing fill group, because fill group DNE"
        return(statement)
      } else{
        ag <- rbindlist(aglist) #requires dplyr
      }
    } else{ #if primary_grp has multiple values 
      for(i in levels(unique(as.factor(primary_grp)))){
          ag <- robust_aggregate(typeag = typeag, bygroup = bygroup, change_names = T, 
                                 d = d, ag_col = ag_col, primary_grp = i, fill_grp = fill_grp)
          colnames(ag)[colnames(ag) == i] <- "Group.2"
          ag$Group.2_Type <- i
          aglist[[i]] <- ag
      }
      ag <- rbindlist(aglist)
    }
    #ag[ ,c(1,3,2,4:ncol(ag))] #need to re-order columns this isn't working
  }
  ag
}
# #Need to figure this part out - and then all difs 
# #if we want same ag_col for different primary group for different fill grp 
# if(length(ag_col) == 1 & length(primary_grp) > 1 & length(fill_grp) > 1){
#   for(i in levels(unique(as.factor(primary_grp)))){
#     for(j in levels(unique(as.factor(fill_grp)))){
#       ag <- robust_aggregate(typeag = typeag, bygroup = bygroup, change_names = T, 
#                              d = d, ag_col = ag_col, primary_grp = i, fill_grp = j)
#       # ag$Group.1_Type <- i
#       # ag$Group.2_Type <- j
#       aglist[[j]] <- ag
#     }
#     # if(bygroup != T){
#     #   statement <- "If bygroup is False, cannot take changing fill group, because fill group DNE"
#     #   return(statement)
#     # } else{
#     #   ag <- Reduce(function(x,y) merge(x,y, by = c(i)), aglist)
#     # }
#     # #colnames(ag)[colnames(ag) == i] <- "Group.1"
#     # aglist2[[i]] <- ag
#   }
#   ag2 <- rbindlist(aglist2)
# }
  
  
#examples
data("mtcars")

#if we want different ag_col for same groups (do not need a fill group)
ag <- iterate_robust_aggregate("avg", F, mtcars, c("mpg","hp"), "cyl", NA) #wihtout fill
ag <- iterate_robust_aggregate("avg", T, mtcars, c("mpg","hp"), "cyl", "vs") #with fill

#if we want different ag col with 1 different group
ag <- iterate_robust_aggregate("avg", F, mtcars, c("mpg","hp"), c("cyl","vs"), NA) #dif prim without fill
ag1 <- iterate_robust_aggregate("avg", T, mtcars, c("mpg","hp"), c("cyl","vs"), "gear") #dif prim with fill
ag2 <- iterate_robust_aggregate("avg", T, mtcars, c("mpg","hp"), "cyl", c("vs", "gear")) #same prim with fill 
ag3 <- iterate_robust_aggregate("avg", T, mtcars, c("mpg","hp"), "gear", c("cyl","vs")) #dif prim with fill
#ag3 = ag1

#if we want same ag_col with 1 different group
ag1 <- iterate_robust_aggregate("avg", T, mtcars, "mpg", "cyl", c("vs", "gear")) #single prim dif fill
ag2 <- iterate_robust_aggregate("avg", T, mtcars, "mpg", c("vs", "gear"), "cyl") #dif prim sing fill
#ag2 = ag1
ag <- iterate_robust_aggregate("avg", F, mtcars, "mpg", c("cyl","vs"), NA) #without fill


