### analysis

set.seed(1230) 

ltitle="Results_CO2Drivers_Analysis.txt"


# Prepare Data

data <- read.csv(here("code/Replication Code/CO2DriversEU_dataset.csv"))
data$lgdp_sq <- data$lgdp^2

data <- as.data.table(data)

data$transport.emissions_pc <- data$transport.emissions/data$pop
data$ltransport.emissions_pc <- log(data$transport.emissions_pc)
data[, L1.ltransport.emissions_pc:=c(NA, ltransport.emissions_pc[-.N]), by="country"]

# Group specification
EU15 <- c("Austria", "Belgium", "Germany", "Denmark", "Spain", "Finland",
          "France", "United Kingdom", "Ireland", "Italy", "Luxembourg", 
          "Netherlands", "Greece", "Portugal", "Sweden")
EU16 <- c("Croatia", "Bulgaria", "Cyprus","Czech Republic", "Estonia", 
          "Hungary", "Lithuania", "Latvia", "Malta", "Poland", "Romania", 
          "Slovak Republic", "Slovenia", "Switzerland", "Iceland", 
          "Norway")
EU31 <- c(EU15, EU16)

# Heterogenous effects preparation
group.interactions <- c("lgdp_EU15", "lgdp_EU16", "lgdpsq_EU15", "lgdpsq_EU16",
                        "lpop_EU15", "lpop_EU16")
data$lgdp_EU15 <- data$lgdp * (data$country %in% EU15)
data$lgdp_EU16 <- data$lgdp * (data$country %in% EU16)
data$lgdpsq_EU15 <- data$lgdp_sq * (data$country %in% EU15)
data$lgdpsq_EU16 <- data$lgdp_sq * (data$country %in% EU16)
data$lpop_EU15 <- data$lpop * (data$country %in% EU15)
data$lpop_EU16 <- data$lpop * (data$country %in% EU16)


###### Analysis:

cat(
  paste0(
    "#################################################################### \n",
    "#                                                                  # \n",
    "#                 CO2 DRIVERS EU - ANALYSIS                        # \n",
    "#                                                                  # \n",
    "#################################################################### \n",
    "\n \n \n"),
  file = ltitle
)



# Analysis

for(group in 1:2){
  
  # Prepare sample and data
  sample <- list(EU15, EU31)[[group]]
  dat <- filter(data, country %in% sample, year>=1995)
  
  # Print Sample Header
  cat(
    paste0(
      "############################## \n",
      "#  SAMPLE = EU", length(sample), " \n",
      "############################## \n",
      "\n \n "),
    file = ltitle,
    append = T
  )
  
  for(p.value in c(.05, .01, .001)){
    
    # Break analysis:
    is <- isatpanel(
      data = dat,
      formula = ifelse(
        group == 1, "ltransport.emissions_pc ~ lgdp + lgdp_sq + lpop",
        paste0(
          "ltransport.emissions_pc ~ ", 
          paste(group.interactions, collapse = " + ")
        )
      ) %>% as.formula,
      index = c("country", "year"),
      effect = "twoways",
      iis = T,
      fesis = T, 
      t.pval=p.value
    )
    
    # Print analysis results
    cat(
      paste0(
        " \n ###########################", 
        " \n # p-value: ", p.value,
        " \n \n "), 
      file = ltitle, 
      append = T)
    
    sink(ltitle, append=T)
    print(is)
    sink()
    
    cat(" \n \n \n \n \n", 
        file = ltitle, 
        append = T)
  }
}
