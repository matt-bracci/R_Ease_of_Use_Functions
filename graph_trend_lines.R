#create line graphs of numeric variable 
graph_trend_line <- function(d, type_ag, agvar, tp, grp, grps_to_use, sz_line, sz_point, sz_lbl){
  
  #d = data frame
  #type_ag = the type of aggregation ('avg', 'count', 'percent', 'sum')
  #agvar = variable we want to aggregate
  #tp = time period variable (line graphs occur over time)
  #grp = NA or column we want to group on
  #grps_to_use = vector of names of the groups we want to graph if showing multiiple lines. can be 'all' if we want to retain all groups and can add "Overall" if we want to show certain groups and the overall avg
  #sz_line = the size of the line
  #sz_point = the size of the point
  #sz_lbl = the size of the label

  #add error handling / variable sincerity 
  
  #aggregate data - calls from robust aggregate function
  if(is.na(grp)){ #if we are not taking by group, simply need the overall avg
    ag <- robust_aggregate(type_ag, F, F, d, agvar, tp) 
    ag$Group.2 <- "Overall" #we add this for later when we make the labels and text groups for graph
  } else{ #else if we are going by group
    ag <- robust_aggregate(type_ag, T, F, d, agvar, tp, grp) #take avgs by group
    if(!"all" %in% grps_to_use){ #if 'all' is not in our grps_to_use
      if(!"Overall" %in% grps_to_use){ #if Overall is also not in our groups to use
        aglist <- list()
        for(i in levels(unique(as.factor(grps_to_use)))){ #take each group in groups to use
          agi <- ag[ag$Group.2 == i, ]
          aglist[[i]] <- agi
        }
        ag <- rbindlist(aglist) #reduce data set to only groups we want 
      } else{ #else if 'all' is not in our grps_to_us but 'Overall' is 
        aglist <- list()
        for(i in levels(unique(as.factor(grps_to_use)))){
          if(i == "Overall"){ #if its overall skip
            next
          }
          agi <- ag[ag$Group.2 == i, ]
          aglist[[i]] <- agi
        }
        ag <- rbindlist(aglist) #get avgs for grops we want
        ag2 <- robust_aggregate(type_ag, F, F, d, agvar, tp) #take overall avgs 
        ag2$Group.2 = "Overall"
        ag <- rbind(ag2, ag) #combine overall avgs with reduced group avgs 
      }
    }
  }
  
  #create labels and geom_text areas for graphs
  lbl_list <- list()
  geom_text_list <- list()
  minval <- as.numeric(min(ag$x))
  for(j in levels(droplevels(unique(as.factor(ag$Group.2))))){
    d <- ag[ag$Group.2 == j, ]
    lbl <- j
    lbl_list[[j]] <- lbl
    if(minval %in% d$x & !is.na(grp)){
      gt <- geom_text(data = d, aes(label = round(x, 0)), vjust = 2, size = sz_lbl, fontface = 'bold')
    } else{
      gt <- geom_text(data = d, aes(label = round(x, 0)), vjust = -1, size = sz_lbl, fontface = 'bold')
    }
    geom_text_list[[j]] <- gt
  } #need to fix this such that the labels are either above or below as appropriate - right now only works for 2 lines really
  
  #create title 
  ttl <- ifelse(is.na(grp), paste(agvar, "by", tp), paste(agvar, "by", tp, "by", grp))
  
  #create graph 
  ggplot(data = ag, aes(x = Group.1, y = x, group = Group.2, color = Group.2)) + 
    geom_line(size = sz_line) +
    scale_color_manual(labels = lbl_list) +
    scale_color_brewer(palette = "Dark2") +
    geom_point(size = sz_point) +
    geom_text_list +
    labs(title = ttl, y = agvar, x = "", fill = "") +
    ylim(min(ag$x) - 10, max(ag$x) + 10) +
    #scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    guides(size = guide_legend(override.aes = list(shape = 1))) +
    dark_blue_theme1
}


#examples
data("airquality")
graph_trend_line(airquality, "avg", "Temp", "Month", NA, NA, 1, 2, 3) #just the avg temp per month
airquality$Heavy_Wind <- ifelse(airquality$Wind > 10, "Strong Wind", "Weak Wind")
graph_trend_line(airquality, "avg", "Temp", "Month", "Heavy_Wind", "all", 1, 2, 3) #avg temp per month (strong wind and weak wind)
graph_trend_line(airquality, "avg", "Temp", "Month", "Heavy_Wind", c("Overall", "Strong Wind"), 1, 2, 3) #avg temp per month (overall and strong wind only)
graph_trend_line(airquality, "avg", "Temp", "Month", "Heavy_Wind", c("Overall", "Strong Wind", "Weak Wind"), 1, 2, 3) #avg temp per month (overall and all subgroups)