graph_distribution_fill <- function(d, dist_var, fl, cutoff, cutoff_val){
  #d = data frmae
  #dist_var = column to show distribution for
  #fl = the fill variable (variable we want to split distributins on)
  #cutoff = NA, 'gt' or 'lt'. If NA we don't take cutoff, if 'gt' we take values above, lt below
  #cutoff_val = the val that we want to cutoff 
  
  #control statements 
  if(!dist_var %in% colnames(d)){
    statement <- print("You provided a dist_var that is not present in data frame d")
    return(statement)
  }
  if(!fl %in% colnames(d) & !is.na(fl)){
    statement <- print("You provided a fl variable that is not present in data frame d")
    return(statement)
  }
  if(!is.na(cutoff) & cutoff != "gt" & cutoff != "lt"){
    statement <- print("Please make cutoff be NA, 'gt' or 'lt'")
    return(statement)
  }
  if(!is.na(cutoff) & !is.numeric(cutoff_val)){
    statement <- print("The cutoff_val must be numeric or NA")
    return(statement)
  }
  
  #theme 
  disto_theme <- theme(axis.text.x = element_text(angle = 45, size = 12, vjust = 1, hjust = 1),
                       panel.background = element_rect(fill = "white"),
                       panel.grid.major = element_line(color = "grey90"),
                       panel.grid.minor = element_line(color = "grey90"),
                       legend.position = "bottom",
                       axis.title.y = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.y = element_blank())
  

  #if we are cuttting off intervlas must do that and create descriptor
  if(cutoff == "gt" & !is.na(cutoff)){
    d <- d[d[[dist_var]] > cutoff_val, ]
    cutoff_descript <- paste("Over", cutoff_val, "days")
  } else if(cutoff == "lt" & !is.na(cutoff)){
    d <- d[d[[dist_var]] < cutoff_val, ]
    cutoff_descript <- paste("Under", cutoff_val, "days")
  } else{
    cutoff_descript <- ""
  }
  
  #final label 
  flbl <- paste(dist_var, "Distribution", cutoff_descript)
  
  #make the graphs 
  if(is.na(fl)){
    ggplot(d, aes(x = d[[dist_var]])) +
      geom_density(color = "#08306B", fill = "#08306B") +
      labs(fill = "", x = dist_var, y = "", title = flbl) +
      disto_theme
  } else{
    d[[fl]] <- as.factor(d[[fl]]) #must convert fill to factor to work 
    ggplot(d, aes(x = d[[dist_var]], fill = d[[fl]])) +
      geom_density(alpha = 0.5) +
      scale_fill_brewer(palette = "Dark2") +
      labs(fill = fl, x = dist_var, y = "", title = flbl) +
      disto_theme
  }
}


#function that will iterate through and graph distribution on various cutoffs
iterate_graph_distribution_fill <- function(d, st, fn, stp, dist_var, fl){
  #set cutoff to less than as we are trying to show the disribution at various cutoffs 
  cutoff <- "lt"
  
  #iterate graphs 
  graphlist <- list()
  for(i in seq(st, fn, stp)){
    ix <- i - 1
    if(is.na(fl)){
      indiv <- graph_distribution_fill(d, dist_var, NA, cutoff, ix)
      graphlist[[i]] <- indiv
    } else{
      indiv <- graph_distribution_fill(d, dist_var, NA, cutoff, ix)
      filled <- graph_distribution_fill(d, dist_var, fl, cutoff, ix)
      graphs <- list(indiv, filled)
      graphlist[[i]] <- graphs
    }
  }
  graphlist
}



#examples
data("mtcars")
#graph the mpg
graph_distribution_fill(d = mtcars, dist_var = "mpg", fl = NA, cutoff = NA, cutoff_val = NA)
#graph the mpg against cyl
graph_distribution_fill(d = mtcars, dist_var = "mpg", fl = "cyl", cutoff = NA, cutoff_val = NA)
#graph the mpg against cyl for cars with mpg under 20
graph_distribution_fill(d = mtcars, dist_var = "mpg", fl = "cyl", cutoff = "lt", cutoff_val = 20)
#iterate over various cutoffs of mpg 
iterate_graph_distribution_fill(d = mtcars, st = 1, fn = max(mtcars$mpg), stp = 5, dist_var = "mpg", fl = NA)
#iterate over periods of mpg against cyl
iterate_graph_distribution_fill(d = mtcars, st = 1, fn = max(mtcars$mpg), stp = 5, dist_var = "mpg", fl = "cyl")