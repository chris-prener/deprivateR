# look at various adi values and correlation with booster uptake ----

# pull vaccine data by; county ----
tokenz<-'chCxsk4zel6QXbaemotF65C9L'

read.socrata(
  urlz,
  app_token = tokenz,
  email     = "tim.wiemken@gmail.com",
  password  =  "ThisIsNotAGoodP@ssw0rd!!!"
) %>%
  tibble() %>%
  mutate(
    fips =  stringr::str_pad(fips, side="left", pad="0", width=5)
  )  -> vax

# vax %>%
#   group_by(date) %>%
#   summarise(
#     num = sum(as.numeric(booster_doses_18plus), na.rm=T),
#     denom = sum(as.numeric(census2019_18pluspop), na.rm=T),
#     rate = num/denom *100
#   ) %>%
#   select(date, rate) %>%
#   filter(
#     rate !=0
#   )-> vax2

# get sociome data ----
adi_sociome <- sociome::get_adi(geography = "county", dataset = "acs5", year = 2019)
# get broadstreet data, physical file but can download from web ----
adi_broadstreet <- vroom::vroom("/Users/timothywiemken/Library/CloudStorage/OneDrive-Pfizer/Documents/Research/github/COVID disparities/extra data/adi.csv")
adi_broadstreet %>% filter(year == 2019) -> adi_broadstreet
# wisconsin data
#adi_wisc <- vroom::vroom("/Users/timothywiemken/Downloads/adi-download/US_2019_ADI_Census Block Group_v3.1.txt")

# clean a bit for plottin g----
vax %>%
  filter(
    date == max(date)
  ) -> vax2

# merge all of these ----
df <- merge(vax2, adi_sociome, by.x = "fips", by.y='GEOID', all.x=T)
df <- merge(df, adi_broadstreet, by.x = "fips", by.y='county_fips_code', all.x=T)
# dump unknown counties ----
df %>%
  filter(
    fips != "00UNK"
  ) -> df

# make quantiles if needed ----
df$adi_broad_quant <- factor(Hmisc::cut2(df$area_deprivation_index_percent, g=4), levels = levels(Hmisc::cut2(df$area_deprivation_index_percent, g=4)), labels = c(1:4))
df$adi_sociome_main <- factor(Hmisc::cut2(df$ADI, g=4), levels = levels(Hmisc::cut2(df$ADI, g=4)), labels = c(1:4))
df$adi_sociome_fin <- factor(Hmisc::cut2(df$Financial_Strength, g=4), levels = levels(Hmisc::cut2(df$Financial_Strength, g=4)), labels = c(1:4))
df$adi_sociome_econ <- factor(Hmisc::cut2(df$Economic_Hardship_and_Inequality, g=4), levels = levels(Hmisc::cut2(df$Economic_Hardship_and_Inequality, g=4)), labels = c(1:4))
df$adi_sociome_edu <- factor(Hmisc::cut2(df$Educational_Attainment, g=4), levels = levels(Hmisc::cut2(df$Educational_Attainment, g=4)), labels = c(1:4))


# slim down df if needed ----
df2 <- df[,c("fips", "booster_doses_vax_pct", "adi_broad_quant", "adi_sociome_main", "adi_sociome_fin", "adi_sociome_econ", "adi_sociome_edu")]

# pivot for plotting if needed ----
df2 %>%
  tidyr::pivot_longer(
    cols = 3:7,
    names_to = "score"
  ) -> df3



# get correlation coefficients between ADIs and booste ruptake ----
ac <- round(cor.test(x=df$area_deprivation_index_percent, as.numeric(df$booster_doses_vax_pct))$estimate,2)
bc <- round( cor.test(x=df$ADI, as.numeric(df$booster_doses_vax_pct))$estimate,2)
cc <- round(cor.test(x=df$Financial_Strength, as.numeric(df$booster_doses_vax_pct))$estimate,2)
dc <- round(cor.test(x=df$Economic_Hardship_and_Inequality, as.numeric(df$booster_doses_vax_pct))$estimate,2)
ec <- round(cor.test(x=df$Educational_Attainment, as.numeric(df$booster_doses_vax_pct))$estimate,2)

# plot ----
library(ggplot2)
a <- ggplot(data=df) + 
  geom_smooth(aes(x = area_deprivation_index_percent, y = as.numeric(booster_doses_vax_pct)), method = lm, se=F)
b <- ggplot(data=df) +   
  geom_smooth(aes(x = ADI, y = as.numeric(booster_doses_vax_pct)), method = lm, se=F)
c <- ggplot(data=df) + 
  geom_smooth(aes(x = Financial_Strength, y = as.numeric(booster_doses_vax_pct)), method = lm, se=F)
d <- ggplot(data=df) + 
  geom_smooth(aes(x = Economic_Hardship_and_Inequality, y = as.numeric(booster_doses_vax_pct)), method = lm, se=F)
e <- ggplot(data=df) + 
  geom_smooth(aes(x = Educational_Attainment, y = as.numeric(booster_doses_vax_pct)), method = lm, se=F) 

# small multiples and add correlation to title ----
library(cowplot)
plot_grid(a,b,c,d,e, 
          labels = c(paste0('Broadstreet; rho = ', ac) , 
                     paste0('Sociome; rho = ', bc), 
                     paste0("Financial; rho = ", cc), 
                     paste0("Economic; rho = ", dc), 
                     paste0("Educational; rho = ", ec)))

