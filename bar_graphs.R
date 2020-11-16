bar_graph <- function(dat, to_ag, type_of_ag, to_dodge, sz, ylav, dgdwid, ag_col, xgp, fgp){
  #dat = data frame to use
  #to_ag = T or F (do we need to aggregate the data or is it already prepared for graph?)
  #type_of_ag = 'sum', 'avg', 'percent', 'count'
  #to_dodge = T or F (are we dodging or not i.e graphing across more than 1 grp or not
  #sz = size of the label
  #ylav = y limit value
  #dgdwid == dodge width 
  #ag_col <- column we are aggregating on (or that is already aggregated)
  #xgp = the x group, 1st column we are graphing on /aggregating if needed
  #fgp = the fill group, 2nd column we are graphing on /aggregating if needed
  
  
  #need to ensure arguments are proper 
  
  
  #obtaining the data we want to graph 
  if(to_ag == T & to_dodge == T){  #when we need to aggregate and dodge 
    if(type_of_ag == "avg"){
      ag <- aggregate(dat[[ag_col]], list(dat[[xgp]], dat[[fgp]]), mean, na.rm = TRUE)
    } else if(type_of_ag == "sum"){
      ag <- aggregate(dat[[ag_col]], list(dat[[xgp]], dat[[fgp]]), sum, na.rm = TRUE)
    } else{
      fullag <- aggregate(dat[[ag_col]], list(dat[[xgp]]), length)
      colnames(fullag)[colnames(fullag) == "x"] <- "tot"
      ag <- aggregate(dat[[ag_col]], list(dat[[xgp]], dat[[fgp]]), length)
      ag <- merge(fullag, ag, by = "Group.1")
      ag$Percent <- ag$x/ag$tot 
    }
  } else if(to_ag == T & to_dodge == F){ #when we need to aggregate and not dodge 
    if(type_of_ag == "avg"){
      ag <- aggregate(dat[[ag_col]], list(dat[[xgp]]), mean, na.rm = TRUE)
    } else if(type_of_ag == "sum"){
      ag <- aggregate(dat[[ag_col]], list(dat[[xgp]]), sum, na.rm = TRUE)
    } else{
      ag <- aggregate(dat[[ag_col]], list(dat[[xgp]]), length)
      ag$Percent <- ag$x/nrow(dat)
    }
  } else{ #when we don't need to aggregate 
    ag <- dat #need to work it out so that if the column names are not proper we adjust them
  }
  
  #load the graphing theme 
  theme_to_use <- theme(axis.text.x = element_text(angle = 45, size = 12, vjust = 1, hjust = 1),
                            axis.text.y = element_text(size = 12),
                            panel.background = element_rect(fill = "white"),
                            panel.grid.major = element_line(color = "grey90"),
                            panel.grid.minor = element_line(color = "grey90"),
                            legend.position = "bottom")
  
  #create the graphs
  if(to_dodge == T){ #if we're dodging 
    if(is.numeric(ag$Group.2)){
      ag$Group.2 <- as.factor(ag$Group.2)
    } #if the second group is numeric, needs to be converted to factor to graph
    
    if(type_of_ag == "avg"){
      g <- ggplot(data = ag, aes(x = Group.1, y = x, label = round(x, digits = 1), fill = Group.2, width = dgdwid)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(position = position_dodge(width = dgdwid), fontface = 'bold', vjust = -0.6, size = sz) + ylim(0, max(ag$x) + ylav) +
        labs(x = ag_col, title = paste("Average", ag_col, "by", xgp, "by", fgp), y = xgp, fill = fgp) +
        scale_fill_brewer(palette = "Dark2") +
        theme_to_use
    } else if(type_of_ag == "sum"){
      g <- ggplot(data = ag, aes(x = Group.1, y = x, label = round(x, digits = 0), fill = Group.2, width = dgdwid)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(position = position_dodge(width = dgdwid), fontface = 'bold', vjust = -0.6, size = sz) + ylim(0, max(ag$x) + ylav) +
        labs(x = ag_col, title = paste("Total", ag_col, "by", xgp, "by", fgp), y = xgp, fill = fgp) +
        scale_fill_brewer(palette = "Dark2") +
        theme_to_use
    } else if(type_of_ag == "count"){
      g <- ggplot(data = ag, aes(x = Group.1, y = x, label = round(x, digits = 0), fill = Group.2, width = dgdwid)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(position = position_dodge(width = dgdwid), fontface = 'bold', vjust = -0.6, size = sz) + ylim(0, max(ag$x) + ylav) +
        labs(x = ag_col, title = paste("Count", ag_col, "by", xgp, "by", fgp), y = xgp, fill = fgp) +
        scale_fill_brewer(palette = "Dark2") +
        theme_to_use
    } else{
      g <- ggplot(data = ag, aes(x = Group.1, y = Percent, 
                                 label = paste0(round(100 * Percent, digits = 1),"%"), fill = Group.2, width = dgdwid)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(position = position_dodge(width = dgdwid), fontface = 'bold', vjust = -0.6, size = sz) +
        labs(x = ag_col, title = paste("Percent of", ag_col, "by", xgp, "by", fgp), y = xgp, fill = fgp) +
        scale_fill_brewer(palette = "Dark2") +
        scale_y_continuous(labels = scales::percent, limits = c(0, max(ag$Percent) + ylav)) +
        theme_to_use
    }
  } else{ #if we're not dodging 
    if(type_of_ag == "avg"){
      g <- ggplot(data = ag, aes(x = Group.1, y = x)) + ylim(0, max(ag$x) + ylav) +
        geom_bar(stat = "identity", fill = rgb(32,55,100, maxColorValue = 255)) + 
        labs(x = xgp, title = paste("Average", ag_col, "by", xgp), y = ag_col) +
        geom_text(aes(label = round(x, digits = 1), fontface = 'bold', vjust = -0.6), size = sz) +
        theme_to_use
    } else if(type_of_ag == "sum"){
      g <- ggplot(data = ag, aes(x = Group.1, y = x)) + ylim(0, max(ag$x) + ylav) +
        geom_bar(stat = "identity", fill = rgb(32,55,100, maxColorValue = 255)) + 
        labs(x = xgp, title = paste("Total", ag_col, "by", xgp), y = ag_col) +
        geom_text(aes(label = round(x, digits = 0), fontface = 'bold', vjust = -0.6), size = sz) +
        theme_to_use
    } else if(type_of_ag == "count"){
      g <- ggplot(data = ag, aes(x = Group.1, y = x)) + ylim(0, max(ag$x) + ylav) +
        geom_bar(stat = "identity", fill = rgb(32,55,100, maxColorValue = 255)) + 
        labs(x = xgp, title = paste("Count", ag_col, "by", xgp), y = ag_col) +
        geom_text(aes(label = round(x, digits = 0), fontface = 'bold', vjust = -0.6), size = sz) +
        theme_to_use
    } else{
      g <- ggplot(data = ag, aes(x = Group.1, y = Percent)) +
        geom_bar(stat = "identity", fill = rgb(32,55,100, maxColorValue = 255)) + 
        labs(x = xgp, title = paste("Percent of", ag_col, "by", xgp), y = ag_col) +
        geom_text(aes(label = paste0(round(100 * Percent, digits = 1),"%"), fontface = 'bold', vjust = -0.6), size = sz) +
        scale_y_continuous(labels = scales::percent, limits = c(0, max(ag$Percent) + ylav)) +
        theme_to_use
    }
  }
  g
}


#examples
data("mtcars")
bar_graph(dat = mtcars, to_ag = T, type_of_ag = "avg", to_dodge = F,
          sz = 4, ylav = 3, dgdwid = 0.75, 
          ag_col = "mpg", xgp = "cyl") #single bar graph of average non-aggregated

bar_graph(dat = mtcars, to_ag = T, type_of_ag = "avg", to_dodge = T,
          sz = 4, ylav = 3, dgdwid = 0.75, 
          ag_col = "mpg", xgp = "cyl", fgp = "gear") #dodged bar graph of average non-aggregated


mtcars$cat <- ifelse(mtcars$mpg < 20, "Under 20 mpg", "20+ Mpg")
mtcars$ID <- 1:nrow(mtcars)

bar_graph(dat = mtcars, to_ag = T, type_of_ag = "percent", to_dodge = F,
          sz = 4, ylav = 0.03, dgdwid = 0.75, 
          ag_col = "ID", xgp = "cat") #single percent non-aggregated

bar_graph(dat = mtcars, to_ag = T, type_of_ag = "percent", to_dodge = T,
          sz = 4, ylav = 0.1, dgdwid = 0.75, 
          ag_col = "ID", xgp = "cat", fgp = "cyl") #dodged percent non-aggregated


#pre-aggregated avg 
avg <- aggregate(mtcars$mpg, list(mtcars$cyl), mean, na.rm = T)
bar_graph(dat = avg, to_ag = F, type_of_ag = "avg", to_dodge = F,
          sz = 4, ylav = 3, dgdwid = 0.75, 
          ag_col = "mpg", xgp = "cyl") 
#need to work this out so that if it is aggregated before w/ different names will still work,
#and so that we don't need to type in the ag_col, xgp or fgp arguments when they're already there
#maybe just make it so that you always aggregate in function, but could get annoying if 
#need aggregations for other things and not just graph and you already have them loaded, why type if out again?




# # #test
# dat = mtcars
# to_ag = T
# type_of_ag = "percent"
# to_dodge = T
# sz = 4
# ylav = 0.1
# dgdwid = 0.75
# ag_col = "ID"
# xgp = "cat"
# fgp = "cyl"
# ag_col = "mpg"
# xgp = "cyl"
# fgp = "gear"
