EU15 <- c("Austria", "Belgium", "Germany", "Denmark", "Spain", "Finland",
          "France", "United Kingdom", "Ireland", "Italy", "Luxembourg", 
          "Netherlands", "Greece", "Portugal", "Sweden")

EU31 <- c("Austria", "Croatia", "Belgium", "Bulgaria", "Cyprus", 
          "Czech Republic", "Germany", "Denmark", "Spain", "Estonia", "Finland",
          "France", "United Kingdom", "Greece", "Hungary", "Ireland", "Italy",
          "Lithuania", "Luxembourg", "Latvia", "Malta", "Netherlands", "Poland",
          "Portugal", "Romania", "Slovak Republic", "Slovenia", "Sweden", 
          "Switzerland", "Iceland", "Norway")

OECD <- readRDS(here("data/out/oecd_countries.rds"))

country_dict <- readRDS(here("data/out/countrydict.RDS"))
#saveRDS(country_dict, "data/out/countrydict.RDS")

getcountry <- country_dict$country.name.en
names(getcountry) <- country_dict$country.name.alt

# gen_hms <- function(vars, ds, lab){
#   # Check that all EU31 and OECD countries are present/named correctly
#   test <- c(EU31, OECD)[!(c(EU31,OECD) %in% ds$country)]
#   if(length(test) == 0){
#     print("All OECD/EU31 countries present/correctly labelled.")
#   }else{print(test)}
#   
#   ds_eu_oecd <- ds %>% filter(country %in% c(EU31, OECD))
#   ds_else <- ds %>% filter(!(country %in% c(EU31, OECD)))
#   
#   heatmaps <- c()
#   
#   for(v in vars){
#     for(j in 1:2){
#       if(j == 1){dat = ds_eu_oecd}else{dat = ds_else}
#       hm <- dat %>% 
#         filter(get(lab) == v) %>% 
#         select(country, year, emissions) %>%
#         pivot_wider(id_cols = country, names_from = year, values_from = emissions) %>%
#         as.data.table %>%
#         as.matrix(rownames = 1) %>%
#         superheat(heat.na.col = "red",
#                   heat.pal = c(low = "skyblue", high = "darkblue"),
#                   bottom.label.text.angle = 90,
#                   force.left.label = TRUE,
#                   title = paste(v, ifelse(identical(dat, ds_eu_oecd), "EU_OECD", "ELSE")))
#       heatmaps <- c(heatmaps, hm)
#     }
#   }
# }

test_diff <- function(df1 = data.frame, df2 = data.frame, vars = list){
  for(i in 1:length(vars)){
    a <- df1 %>% pull(vars[i])
    b <- df2 %>% pull(vars[i])
    if(length(setdiff(a,b) != 0)) {
      print(paste(deparse(substitute(df1)),"has extra:",setdiff(a,b)))
    }else{print(paste(vars[i],"match"))}
    if(length(setdiff(b,a) != 0)) {
      print(paste(deparse(substitute(df2)),"has extra:",setdiff(b,a)))
    }else{print(paste(vars[i],"match"))}
  }
}
