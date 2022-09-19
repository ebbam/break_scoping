### data assembly
 
set.seed(123)

EU31 <- c("Austria", "Croatia", "Belgium", "Bulgaria", "Cyprus", 
          "Czech Republic", "Germany", "Denmark", "Spain", "Estonia", "Finland",
          "France", "United Kingdom", "Greece", "Hungary", "Ireland", "Italy",
          "Lithuania", "Luxembourg", "Latvia", "Malta", "Netherlands", "Poland",
          "Portugal", "Romania", "Slovak Republic", "Slovenia", "Sweden", 
          "Switzerland", "Iceland", "Norway")

#### Import and merge Data
gdp <- read.csv(here("code/Replication Code/Data/WB_gdpconst.csv"), skip = 3) %>%
  filter(Country.Name %in% EU31) %>%
  select(c(1,14:63)) %>%
  pivot_longer(2:51, names_to = "year") %>%
  transmute(country=Country.Name, year=as.numeric(str_remove(year, "X")),
            gdp=value)

pop <- read.csv(here("code/Replication Code/Data/WB_totpop.csv"), skip=3) %>%
  filter(Country.Name %in% EU31) %>%
  select(c(1,13:63)) %>%
  pivot_longer(5:52, names_to = "year") %>%
  transmute(country=Country.Name, year=as.numeric(str_remove(year, "X")),
            pop=value)

transp.em <- read_excel(here("code/Replication Code/Data/v50_CO2_excl_short-cycle_org_C_1970_2018.xls"), 
                       sheet = 1, range = cell_rows(10:2284)) %>%
  mutate(Name = ifelse(Name=="Slovakia", "Slovak Republic", Name)) %>%
  filter(Name %in% EU31, IPCC=="1.A.3.b") %>%
  pivot_longer(7:55, "year") %>%
  transmute(country=Name, year=as.numeric(str_remove(year, "X")),
                                          transport.emissions=as.numeric(value))

data <- left_join(gdp, pop, c("country", "year")) %>%
  left_join(transp.em, c("country", "year"))


#### Transform Variables
data$lgdp <- log(data$gdp)
data$lpop <- log(data$pop)
data$ltransport.emissions <- log(data$transport.emissions)
data$const <- 1


#### Level Variables
data <- as.data.table(data)
data[, L1.ltransport.emissions:=c(NA, ltransport.emissions[-.N]), by="country"]
data[, L1.lgdp:=c(NA, lgdp[-.N]), by="country"]
data[, L1.lpop:=c(NA, lpop[-.N]), by="country"]


#### Output
write.csv(data, here("code/Replication Code/CO2DriversEU_dataset.csv"), row.names = F)


### clean working directory
rm(gdp, pop, transp.em)





