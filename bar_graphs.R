
bar_graph <- function(dat, to_ag, type_of_ag, type_of_bar, sz, ylav, dgdwid, ag_col, xgp, fgp){
  #dat = data frame to use
  #to_ag = T or F (do we need to aggregate the data or is it already prepared for graph?)
  #type_of_ag = 'sum', 'avg', 'percent', 'count'
  #type_of_bar = 'single' or 'dodge' or 'stack' 
  #sz = size of the label
  #ylav = y limit value
  #dgdwid == dodge width 
  #ag_col <- column we are aggregating on (or that is already aggregated)
  #xgp = the x group, 1st column we are graphing on /aggregating if needed
  #fgp = the fill group, 2nd column we are graphing on /aggregating if needed
  
  
  #need to ensure arguments are proper 
  if(to_ag != T & to_ag != F){
    statement <- print("Please enter the to_ag argument as T or F")
    return(statement)
  }
  if(type_of_ag != "avg" & type_of_ag != "count" & type_of_ag != "sum" & type_of_ag != "percent"){
    statement <- print("Please enter the type_of_ag argument as 'avg', 'count', 'sum' or 'percent'")
    return(statement)
  }
  if(type_of_bar != "single" & type_of_bar != "dodge" & type_of_bar != "stack"){
    statement <- print("Please enter the type_of_bar argument as 'single', 'dodge', or 'stack'")
    return(statement)
  }
  if(!is.numeric(sz) | !is.numeric(ylav)){
    statement <- print("sz and ylav arguments must be numeric. One or both of these is currently not")
    return(statement)
  }
  if(type_of_bar != "single" & (is.na(dgdwid) | !is.numeric(dgdwid))){
    statement <- print("When type_of_bar argument is 'dodge' or 'stack', then must provide dgdwid argument as numeric")
    return(statement)
  }
  if(type_of_bar == "stack" & type_of_ag == "avg"){
    statement <- warning("Are you sure you want to show averages in a stacked format?")
  }
  
  
  
  #obtaining the data we want to graph 
  if(to_ag == T & (type_of_bar == "dodge" | type_of_bar == "stack")){  #when we need to aggregate on groups 
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
  } else if(to_ag == T){ #when we need to aggregate on single group 
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
  if(type_of_ag == "percent" & type_of_bar == "stack"){ #if this is the graph, we don't need any y axis 
    theme_to_use <- theme(axis.text.x = element_text(angle = 45, size = 12, vjust = 1, hjust = 1),
                          axis.text.y = element_blank(),
                          panel.background = element_rect(fill = "white"),
                          panel.grid.major = element_line(color = "grey90"),
                          panel.grid.minor = element_line(color = "grey90"),
                          legend.position = "bottom")
  } else{
    theme_to_use <- theme(axis.text.x = element_text(angle = 45, size = 12, vjust = 1, hjust = 1),
                          axis.text.y = element_text(size = 12),
                          panel.background = element_rect(fill = "white"),
                          panel.grid.major = element_line(color = "grey90"),
                          panel.grid.minor = element_line(color = "grey90"),
                          legend.position = "bottom")
  }
  
  
  #create the graphs
  if(type_of_bar == "dodge"){ #if we're dodging 
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
  } else if(type_of_bar == "stack"){
      if(type_of_ag == "avg"){ #since this is an uncommon graph (stacked avgs) - create prompt/error handle this w/warning message
        ag$Group.2 <- reorder(ag$Group.2, ag$x, mean) #must re order factors baed on its highest values
        ag$Group.2 <- factor(ag$Group.2, levels=rev(levels(ag$Group.2))) #to get largest on bottom must reverse these levels
        g <- ggplot(data = ag, aes(x = Group.1, y = x, label = round(x, digits = 1), fill = Group.2)) +
          geom_bar(stat = "identity") +
          geom_text(fontface = 'bold', colour = "white", size = sz, position = position_stack(vjust = 0.7)) + 
          #ylim(0, max(ag$x) + ylav) +
          labs(x = ag_col, title = paste("Average", ag_col, "by", xgp, "by", fgp), y = xgp, fill = fgp) +
          scale_fill_brewer(palette = "Dark2") +
          theme_to_use
      } else if(type_of_ag == "sum"){
        ag$Group.2 <- reorder(ag$Group.2, ag$x, sum) #must re order factors baed on its highest values
        #ag$Group.2 <- factor(ag$Group.2, levels=rev(levels(ag$Group.2))) #to get largest on bottom must reverse these levels
        g <- ggplot(data = ag, aes(x = Group.1, y = x, label = round(x, digits = 0), fill = Group.2)) +
          geom_bar(stat = "identity") +
          geom_text(fontface = 'bold', colour = "white", size = sz, position = position_stack(vjust = 0.7)) + 
          #ylim(0, max(ag$x) + ylav) +
          labs(x = ag_col, title = paste("Total", ag_col, "by", xgp, "by", fgp), y = xgp, fill = fgp) +
          scale_fill_brewer(palette = "Dark2") +
          theme_to_use
      } else if(type_of_ag == "count"){
        ag$Group.2 <- reorder(ag$Group.2, ag$x, length) #must re order factors baed on its highest values
        ag$Group.2 <- factor(ag$Group.2, levels=rev(levels(ag$Group.2))) #to get largest on bottom must reverse these levels
        g <- ggplot(data = ag, aes(x = Group.1, y = x, label = round(x, digits = 0), fill = Group.2)) +
          geom_bar(stat = "identity") +
          geom_text(fontface = 'bold', colour = "white", size = sz, position = position_stack(vjust = 0.7)) + 
          #ylim(0, max(ag$x) + ylav) +
          labs(x = ag_col, title = paste("Count", ag_col, "by", xgp, "by", fgp), y = xgp, fill = fgp) +
          scale_fill_brewer(palette = "Dark2") +
          theme_to_use
      } else{
        ag$Group.2 <- reorder(ag$Group.2, ag$x, length) #must re order factors baed on its highest values
        ag$Group.2 <- factor(ag$Group.2, levels=rev(levels(ag$Group.2))) #to get largest on bottom must reverse these levels
        g <- ggplot(data = ag, aes(x = Group.1, y = x, label = paste0(round(100 * Percent, digits = 0),"%"), fill = Group.2)) +
          geom_bar(stat = "identity") +
          geom_text(fontface = 'bold', colour = "white", size = sz, position = position_stack(vjust = 0.7)) + 
          #scale_y_continuous(labels = scales::percent) +
          labs(x = ag_col, title = paste("Percent of", ag_col, "by", xgp, "by", fgp), y = xgp, fill = fgp) +
          scale_fill_brewer(palette = "Dark2") +
          theme_to_use
      }
  } else{ #if we just have a single group 
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



#single bar pre aggregated
avg <- aggregate(mtcars$mpg, list(mtcars$cyl), mean)
bar_graph(dat = avg, to_ag = F, type_of_ag = "avg", type_of_bar = "single",
          sz = 4, ylav = 3, dgdwid = NA,
          ag_col = "mpg", xgp = "cyl") #need to adjust column names. still need to type names in. prob can be fixed together

#single bar graph non-aggregated
bar_graph(dat = mtcars, to_ag = T, type_of_ag = "Percent", type_of_bar = "single",
          sz = 4, ylav = 0.05, dgdwid = NA,
          ag_col = "mpg", xgp = "cyl")

#dodged bar graph non-aggregated
bar_graph(dat = mtcars, to_ag = T, type_of_ag = "sum", type_of_bar = "dodge",
          sz = 4, ylav = 3, dgdwid = 0.75, 
          ag_col = "mpg", xgp = "cyl", fgp = "gear")


#stacked bar graph non-aggregated
mtcars$cat <- ifelse(mtcars$mpg < 20, "Under 20 mpg", "20+ Mpg")
mtcars$ID <- 1:nrow(mtcars)
bar_graph(dat = mtcars, to_ag = T, type_of_ag = "count", type_of_bar = "stack",
          sz = 4, ylav = 0.1, dgdwid = 0.75, 
          ag_col = "ID", xgp = "cat", fgp = "cyl") 


