#clear all----------
# Clear console messages
cat( "\014" )
# Clear plots
if( dev.cur() > 1 ) dev.off()
# Clear global workspace
rm( list = ls( envir = globalenv() ), envir = globalenv() )

#ライブラリ-----------
library(tidyverse)
library(stargazer)
library(plm)
library(estimatr)
library(huxtable)
library(texreg)
library(MatchIt) 
library(fixest)
library(modelsummary)
library(ggfixest)
library(gridExtra)
library(grid)
library(did)
library(magrittr)
library(HonestDiD)
library(parallel)
library(ssynthdid)
library(synthdid)
library(lubridate, warn.conflicts = FALSE)
library(ggplot2)
library(patchwork)
library(cobalt)
library(WeightIt)
library(openxlsx)
library(BMisc)
library(gt)
library(webshot2)
library(psych)
library(NipponMap)
library(sf)


# 設定-------------
## WDの設定
setwd("D:/R/260407Industrial-Policy-Analysis-Tool")

## 政策名の設定
target <-"●●政策" #政策名を入れる

## パネルデータの設定
df0_0 <- read.csv("01_prepare_output/merged_key.csv", encoding = "UTF-8")


##policy-------
dfpolicy2.1	<- read.xlsx("treated_list/企業リスト.xlsx") %>%　select(法人番号, 処置年度) %>%
  rename(id = 法人番号, year = 処置年度) %>%
  mutate(id = as.character(id), year = as.integer(year))


## industrial category-----
industry_name <- read.csv("data/industry_name.csv", encoding = "UTF-8")%>% 
  rename(industry="符号") %>%
  mutate(industry= as.factor(industry),
         industry_code = as.factor(industry_code))

##Companies-----------
df0 <- df0_0%>%
  select(-1)%>%
  mutate(id = as.character(id),
         year = as.integer(year),
         age = year - foundation_year,
         age2 =age^2,
         
         net_worth = sum_debt - current_debt - fixed_debt,
         net_profit_workers = net_profit/workers,
         
         net_worth = ifelse(net_worth<0, NA, net_worth), # 負になるものを除外
         ROE = net_profit/net_worth, # networth = 資産-負債
         ROE = ifelse(is.finite(ROE), ROE, NA),
         ROA = net_profit/sum_asset,
         ROA = ifelse(is.finite(ROA), ROA, NA),
         
         salary = sum_salary/workers,
         
         RD_jisha = ifelse(is.na(RD_jisha),0,RD_jisha), 
         RD_itaku = ifelse(is.na(RD_itaku),0,RD_itaku), 
         RD = RD_jisha + RD_itaku,
         RD = ifelse(RD<0, NA, RD), # 負になるものを除外
         
         benefit = ifelse(benefit<0, NA, benefit), # 負になるものを除外
         fixed_asset = ifelse(fixed_asset<0, NA, fixed_asset), # 負になるものを除外
         intangible_asset = ifelse(intangible_asset<0, NA, intangible_asset), # 負になるものを除外
         current_debt = ifelse(current_debt<0, NA, current_debt), # 負になるものを除外
         fixed_debt = ifelse(fixed_debt<0, NA, fixed_debt), # 負になるものを除外
         tangible_asset_acquisition = ifelse(tangible_asset_acquisition<0, NA, tangible_asset_acquisition), # 負になるものを除外
         dividend = ifelse(dividend<0, NA, dividend), # 負になるものを除外
         sum_salary = ifelse(sum_salary<0, NA, sum_salary), # 負になるものを除外
         tax = ifelse(tax<0, NA, tax), # 負になるものを除外
         export = ifelse(export<0, NA, export), # 負になるものを除外
         RD_jisha = ifelse(RD_jisha<0, NA, RD_jisha), # 負になるものを除外
         foundation_year = ifelse(foundation_year < 578, NA, foundation_year),
         
         log_sales = log(sales),
         log_office= log(office),
         log_workers = log(workers),
         log_capital = log(capital) ,
         log_sum_asset = log(sum_asset),
         log_salary = log(sum_salary/workers),
         
         #generate logs
         log_tangible_asset =  ifelse(tangible_asset==0, NA, log(tangible_asset)),
         log_intangible_asset =  ifelse(intangible_asset==0, NA, log(intangible_asset)),
         log_benefit =  ifelse(benefit==0, NA, log(benefit/workers)), #一人あたりに変換
         log_indefinite_workers =  ifelse(indefinite_workers==0, NA, log(indefinite_workers)),
         log_fixedterm_workers =  ifelse(fixedterm_workers==0, NA, log(fixedterm_workers)),
         log_fixedterm_workers_equivalent =  ifelse(fixedterm_workers_equivalent==0, NA, log(fixedterm_workers_equivalent)),
         log_tax =  ifelse(tax==0, NA, log(tax)),
         
         # flags for many zeros
         flag_export = ifelse(export > 0, 1, 0),
         flag_import = ifelse(import > 0, 1, 0),
         flag_training = ifelse(training > 0, 1, 0),
         flag_RD = ifelse(RD > 0, 1, 0),
         flag_patent = ifelse(patent > 0, 1, 0),
         flag_jitsuyo = ifelse(jitsuyo > 0, 1, 0),
         flag_isho = ifelse(isho > 0, 1, 0),
         flag_training = ifelse(training > 0, 1, 0),
         flag_investment_affiliate_domestic = ifelse(investment_affiliate_domestic > 0, 1, 0),
         flag_investment_affiliate_overseas = ifelse(investment_affiliate_overseas > 0, 1, 0),
         flag_dividend = ifelse(dividend > 0, 1, 0),
         
         
         log_export = ifelse(export<=0, NA, log(export)),
         log_import = ifelse(import<=0, NA, log(import)) ,
         log_training = ifelse(training<=0, NA, log(training)), 
         log_RD = ifelse(RD<=0, NA, log(RD)),
         log_patent = ifelse(patent<=0, NA, log(patent)),
         log_jitsuyo = ifelse(jitsuyo<=0, NA, log(jitsuyo)),
         log_isho =ifelse(isho<=0, NA, log(isho)),
         log_training = ifelse(training<=0, NA, log(training)),
         log_investment_affiliate_domestic = ifelse(investment_affiliate_domestic<=0, NA, log(investment_affiliate_domestic)),
         log_investment_affiliate_overseas = ifelse(investment_affiliate_overseas<=0, NA, log(investment_affiliate_overseas)),
         log_dividend = ifelse(dividend<=0, NA, log(dividend)),
         
         # 0をNAに変換
         export = ifelse(export<=0, NA, export),
         import = ifelse(import<=0, NA, import),
         training = ifelse(training<=0, NA, training), 
         RD = ifelse(RD<=0, NA, RD),
         patent = ifelse(patent<=0, NA, patent),
         jitsuyo = ifelse(jitsuyo<=0, NA, jitsuyo),
         isho =ifelse(isho<=0, NA, isho),
         training = ifelse(training<=0, NA, training),
         investment_affiliate_domestic = ifelse(investment_affiliate_domestic<=0, NA, investment_affiliate_domestic),
         investment_affiliate_overseas = ifelse(investment_affiliate_overseas<=0, NA, investment_affiliate_overseas),
         dividend = ifelse(dividend<=0, NA, dividend),
         
         # factor化
         pref = as.factor(pref),
         industry = as.factor(industry),
         subsidiary = as.factor(subsidiary),
         parent = as.factor(parent))

# prepare-----
dfm_kikatsu_policy <- left_join(df0,dfpolicy2.1,by=c("id","year"))

# create data of the policy
dfpolicy3 <- dfpolicy2.1 %>% # create list of ids to make intervention year
  group_by(id)%>%
  summarise(intervention_year = min(year),
            final_intervention = max(year),
            multiple_interventions = ifelse(intervention_year==final_intervention, 0, 1))%>% ## to note there are multiple interventions
  drop_na()

ignored_interventions <- sum(dfpolicy3$multiple_interventions)
count_intervention <- nrow(dfpolicy3)
start_year <- min(dfpolicy3$intervention_year)

# merge everything
dfm_kikatsu0 <- left_join(df0,dfpolicy2.1,by=c("id","year"))
dfm_kikatsu0<- left_join(dfm_kikatsu0, dfpolicy3, by=c("id"))%>%
  left_join(industry_name, by = c("industry"))

dfm_kikatsu <<- dfm_kikatsu0 %>%
  mutate(year_to_treat = ifelse(is.na(intervention_year), -1000, year - intervention_year),
         intervention_year = ifelse(is.na(intervention_year),0, intervention_year),  # keep non_intervention,
         treat = ifelse(year_to_treat==-1000,0,1) #いずれトリートされるか否か
  )%>%
  distinct(id, year,.keep_all = TRUE)%>% 
  drop_na(id#,year,log_sales,intervention_year,log_office, age, log_capital, log_workers,ROA
  )%>%
  mutate(id = as.factor(id))

dfpolicy4 <- dfm_kikatsu %>% 
  group_by(id)%>%
  summarise(year0 = min(intervention_year),year1 = max(intervention_year),multiple_intervention=ifelse(year0==year1, 0, 1)
  ) %>% 
  select(!c(year1))%>%
  group_by(year0)%>%
  summarise(count = n(),
            multiple_intervention = sum(multiple_intervention))

## propensity score-----------
dfm_kikatsu_propensity0 <- dfm_kikatsu %>%
  pdata.frame(index = c("id","year")) 

# make lags
dfm_kikatsu_propensity0$log_sales1 = plm::lag(dfm_kikatsu_propensity0$log_sales, 1)
dfm_kikatsu_propensity0$log_sales2 = plm::lag(dfm_kikatsu_propensity0$log_sales, 2)
dfm_kikatsu_propensity0$log_sales5 = plm::lag(dfm_kikatsu_propensity0$log_sales, 5)
dfm_kikatsu_propensity0$log_sales7 = plm::lag(dfm_kikatsu_propensity0$log_sales, 7)
dfm_kikatsu_propensity0$ROA1 = plm::lag(dfm_kikatsu_propensity0$ROA, 1)
dfm_kikatsu_propensity0$ROA2 = plm::lag(dfm_kikatsu_propensity0$ROA, 2)
dfm_kikatsu_propensity0$ROA5 = plm::lag(dfm_kikatsu_propensity0$ROA, 5)
dfm_kikatsu_propensity0$ROA7 = plm::lag(dfm_kikatsu_propensity0$ROA, 7)
dfm_kikatsu_propensity0$net_profit_workers1 = plm::lag(dfm_kikatsu_propensity0$net_profit_workers, 1)
dfm_kikatsu_propensity0$net_profit_workers2 = plm::lag(dfm_kikatsu_propensity0$net_profit_workers, 2)
dfm_kikatsu_propensity0$net_profit_workers5 = plm::lag(dfm_kikatsu_propensity0$net_profit_workers, 5)
dfm_kikatsu_propensity0$net_profit_workers7 = plm::lag(dfm_kikatsu_propensity0$net_profit_workers, 7)
dfm_kikatsu_propensity0$log_salary1 = plm::lag(dfm_kikatsu_propensity0$log_salary, 1)
dfm_kikatsu_propensity0$log_salary2 = plm::lag(dfm_kikatsu_propensity0$log_salary, 2)
dfm_kikatsu_propensity0$log_salary5 = plm::lag(dfm_kikatsu_propensity0$log_salary, 5)
dfm_kikatsu_propensity0$log_salary7 = plm::lag(dfm_kikatsu_propensity0$log_salary, 7)
dfm_kikatsu_propensity0$log_sum_asset1 = plm::lag(dfm_kikatsu_propensity0$log_sum_asset, 1)
dfm_kikatsu_propensity0$log_sum_asset2 = plm::lag(dfm_kikatsu_propensity0$log_sum_asset, 2)
dfm_kikatsu_propensity0$log_sum_asset5 = plm::lag(dfm_kikatsu_propensity0$log_sum_asset, 5)
dfm_kikatsu_propensity0$log_sum_asset7 = plm::lag(dfm_kikatsu_propensity0$log_sum_asset, 7)
dfm_kikatsu_propensity0$log_workers1 = plm::lag(dfm_kikatsu_propensity0$log_workers, 1)
dfm_kikatsu_propensity0$log_workers2 = plm::lag(dfm_kikatsu_propensity0$log_workers, 2)
dfm_kikatsu_propensity0$log_workers5 = plm::lag(dfm_kikatsu_propensity0$log_workers, 5)
dfm_kikatsu_propensity0$log_workers7 = plm::lag(dfm_kikatsu_propensity0$log_workers, 7)
dfm_kikatsu_propensity0$log_office1 = plm::lag(dfm_kikatsu_propensity0$log_office, 1)
dfm_kikatsu_propensity0$log_office2 = plm::lag(dfm_kikatsu_propensity0$log_office, 2)
dfm_kikatsu_propensity0$log_office5 = plm::lag(dfm_kikatsu_propensity0$log_office, 5)
dfm_kikatsu_propensity0$log_office7 = plm::lag(dfm_kikatsu_propensity0$log_office, 7)

dfm_kikatsu_propensity0$age_lag2 = plm::lag(dfm_kikatsu_propensity0$age, 2)
dfm_kikatsu_propensity0$age2_lag2 = plm::lag(dfm_kikatsu_propensity0$age2, 2)
dfm_kikatsu_propensity0$industry_code_lag2 = plm::lag(dfm_kikatsu_propensity0$industry_code, 2)
dfm_kikatsu_propensity0$log_capital_lag2 = plm::lag(dfm_kikatsu_propensity0$log_capital, 2)
dfm_kikatsu_propensity0$subsidiary2 = plm::lag(dfm_kikatsu_propensity0$subsidiary, 2)
dfm_kikatsu_propensity0$parent2 = plm::lag(dfm_kikatsu_propensity0$parent, 2)

# make diff
dfm_kikatsu_propensity0 <- dfm_kikatsu_propensity0 %>%
  mutate(log_sales_diff=(log_sales2 - log_sales7)/(log_sales7+1),
         ROA_diff=(ROA2 - ROA7), #AMDが異常に大きくなるので、割らない
         net_profit_workers_diff=(net_profit_workers2 - net_profit_workers7),#AMDが異常に大きくなるので、割らない
         log_salary_diff=(log_salary2 - log_salary7)/(log_salary7+1),
         log_sum_asset_diff=(log_sum_asset2 - log_sum_asset7)/(log_sum_asset7+1),
         log_workers_diff=(log_workers2 - log_workers7)/(log_workers7+1),
         log_office_diff=(log_office2 - log_office7)/(log_office7+1),
         
  )

# NA削除
if (start_year >= 2009){
  dfm_kikatsu_propensity1_0 <- dfm_kikatsu_propensity0 %>%   filter(year ==start_year) %>%
    drop_na(age_lag2, age2_lag2,industry_code_lag2,subsidiary2,parent2, 
            log_sales2, log_sales_diff, 
            ROA2, ROA_diff, 
            net_profit_workers2, net_profit_workers_diff, 
            log_salary2, log_salary_diff, 
            log_sum_asset2, log_sum_asset_diff, 
            log_workers2, log_workers_diff,
            log_office2, log_office_diff,
            pref, industry
    )%>%
    
    group_by(intervention_year) %>%
    filter(n()>4) %>% ungroup() %>% select(-intervention_year) # delete small subgroups
  
  #  ps_formula <<- treat ~  pref + industry_code + age_lag2+age2_lag2+industry_code_lag2+subsidiary2+parent2+log_sales2+log_sales_diff+ROA2+ROA_diff+net_profit_workers2+net_profit_workers_diff+log_salary2+log_salary_diff+log_sum_asset2+log_sum_asset_diff+  log_workers2+log_workers_diff+log_office2+log_office_diff
  formula_cs0 <<- treat ~  pref_m + industry_code_m +subsidiary2+parent2+ age_lag2+age2_lag2 +log_sales2+log_sales_diff+ROA2+ROA_diff+net_profit_workers2+net_profit_workers_diff+log_salary2+log_salary_diff+log_sum_asset2+log_sum_asset_diff+  log_workers2+log_workers_diff+log_office2+log_office_diff
  formula_cs <<-　　　　 ~ pref_m + industry_code_m +subsidiary2+parent2+ age_lag2+age2_lag2 +log_sales2+log_sales_diff+ROA2+ROA_diff+net_profit_workers2+net_profit_workers_diff+log_salary2+log_salary_diff+log_sum_asset2+log_sum_asset_diff+  log_workers2+log_workers_diff+log_office2+log_office_diff
  
  
} else {
  dfm_kikatsu_propensity1_0 <- dfm_kikatsu_propensity0 %>%   filter(year ==start_year) %>%
    drop_na(age_lag2, age2_lag2,industry_code_lag2,#subsidiary2,parent2, #昔の調査では取っていなかった項目のため、年によって変更する
            log_sales2, log_sales_diff, 
            ROA2, ROA_diff, 
            net_profit_workers2, net_profit_workers_diff, 
            log_salary2, log_salary_diff, 
            log_sum_asset2, log_sum_asset_diff, 
            log_workers2, log_workers_diff,
            log_office2, log_office_diff,
            pref, industry
    )%>%
    group_by(intervention_year) %>%
    filter(n()>4) %>% ungroup() %>% select(-intervention_year) # delete small subgroups
  
  formula_cs0 <<- treat ~  pref_m + industry_code_m + age_lag2+age2_lag2+log_sales2+log_sales_diff+ROA2+ROA_diff+net_profit_workers2+net_profit_workers_diff+log_salary2+log_salary_diff+log_sum_asset2+log_sum_asset_diff+  log_workers2+log_workers_diff+log_office2+log_office_diff
  formula_cs <<-　　　　 ~ pref_m + industry_code_m + age_lag2+age2_lag2+log_sales2+log_sales_diff+ROA2+ROA_diff+net_profit_workers2+net_profit_workers_diff+log_salary2+log_salary_diff+log_sum_asset2+log_sum_asset_diff+  log_workers2+log_workers_diff+log_office2+log_office_diff
}
#　統合
pref_modified <- dfm_kikatsu_propensity1_0 %>%filter(treat==1) %>% group_by(pref) %>% summarize(n=n()) %>%
  mutate(pref_m = as.factor(ifelse(n>4, as.character(pref),"X"))) %>% select(-c("n"))
industry_modified <- dfm_kikatsu_propensity1_0 %>%filter(treat==1) %>% group_by(industry_code) %>% summarize(n=n()) %>%
  mutate(industry_code_m = as.factor(ifelse(n>4, as.character(industry_code),"X"))) %>% select(-c("n"))

dfm_kikatsu_propensity1 <- dfm_kikatsu_propensity1_0 %>%
  left_join(pref_modified, by = c("pref")) %>%
  left_join(industry_modified, by = c("industry_code")) %>%
  drop_na(pref_m, industry_code_m,industry_code, log_salary) %>%
  as.data.frame()   #通常の data.frame に変換

## IPW(CS)----
ipw_data_cs <- weightit(formula = formula_cs0, 
                        data = dfm_kikatsu_propensity1, method = "glm", estimand = "ATT") %>%
  trim(at=0.999, lower = FALSE, drop = TRUE) # trim extreme weights

filename_text <- paste("02_analysis_output/", target,"_00balance_ipw_cs_", count_intervention, ".text", sep="")
sink(file = filename_text)
print(summary(ipw_data_cs))
sink(file = NULL)


ipw_summary_cs <- wrap_table(
  data.frame(ESS = c(""),Control = c(ESS(ipw_data_cs$weights[ipw_data_cs$treat == 0])),
             Treated=c(ESS(ipw_data_cs$weights[ipw_data_cs$treat == 1])))
) 

## make plot
bal_att_cs <- love.plot(ipw_data_cs, abs = TRUE,
                        stars = "raw",     # Raw MDに★をつける
) +theme_light()
plot(bal_att_cs)


balance_prop_cs <- bal.plot(ipw_data_cs,which = "both", var.name = "prop.score")

bal_cs <- ipw_summary_cs/balance_prop_cs / bal_att_cs +
  plot_annotation(title = "IPW results", subtitle = target) +
  plot_layout(heights = c(1.5, 3, 14)) &
  theme(text = element_text(family = "Meiryo UI"))

## output of the balance test
filename_pdf <- paste("02_analysis_output/", target,"_00balance_ipw_cs_", count_intervention, ".pdf", sep="")
ggsave(filename_pdf, bal_cs, 
       width=20, height =35, units = "cm", dpi=200, device = cairo_pdf) 
filename_png <- paste("02_analysis_output/", target,"_00balance_ipw_cs_", count_intervention, ".png", sep="")
ggsave(filename_png, bal_cs, 
       width=20, height =35, units = "cm", dpi=200) 

## make a panel

dfm_kikatsu_propensity_ipw <- dfm_kikatsu_propensity1%>%
  select(id,age_lag2, age2_lag2,industry_code_lag2,subsidiary2,parent2,
         pref_m, industry_code_m,
         log_sales2, log_sales_diff, 
         ROA2, ROA_diff, 
         net_profit_workers2, net_profit_workers_diff, 
         log_salary2, log_salary_diff, 
         log_sum_asset2, log_sum_asset_diff, 
         log_workers2, log_workers_diff,
         log_office2, log_office_diff)%>%
  mutate(weights = ipw_data_cs$weights)

dfm_kikatsu2_ipw <- left_join(dfm_kikatsu,dfm_kikatsu_propensity_ipw,by=c("id"))%>%
  drop_na(weights, industry_code) %>%
  drop_na(log_sales, 
          ROA, 
          net_profit_workers,
          log_salary,  
          log_sum_asset, 
          log_workers, 
          log_office,
          pref, industry
  )%>% # 主要変素の欠落を削除
  filter(weights>0)%>%
  filter(year >= start_year -11) %>%
  mutate(id = as.numeric(id))

# 20 define functions  for robustness check- TWFE----------
robustness_TWFE_asset <- function(target){
  ################## log_sum_asset------------
  # PSM TWFE
  es_PSM_log_sum_asset_twfe = feols(log_sum_asset ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)

  # PSM sa20
  es_PSM_log_sum_asset_sa20 = feols(log_sum_asset ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)

  p_PSM_log_sum_asset_fe <- ggiplot(list('S&A(2020)' = es_PSM_log_sum_asset_sa20,'TWFE' = es_PSM_log_sum_asset_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_sum_asset",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
 
  ## create grid arrange
  pa_log_sum_asset <- p_PSM_log_sum_asset_fe
  
  ################## log_tangible_asset------------
  # PSM TWFE
  es_PSM_log_tangible_asset_twfe = feols(log_tangible_asset ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_log_tangible_asset_twfe <- feols(log_tangible_asset ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_tangible_asset_twfe<- c(es2_PSM_log_tangible_asset_twfe$coefficients[1], es2_PSM_log_tangible_asset_twfe$se[1])
  
  # PSM sa20
  es_PSM_log_tangible_asset_sa20 = feols(log_tangible_asset ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_tangible_asset_sa20 <- aggregate(es_PSM_log_tangible_asset_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_log_tangible_asset_fe <- ggiplot(list('S&A(2020)' = es_PSM_log_tangible_asset_sa20,'TWFE' = es_PSM_log_tangible_asset_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_tangible_asset",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # log_tangible_asset_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_log_tangible_asset_twfe[1], att_PSM_log_tangible_asset_sa20[1]),
  #                                     up = c(att_PSM_log_tangible_asset_twfe[1] + 1.645*att_PSM_log_tangible_asset_twfe[2],att_PSM_log_tangible_asset_sa20[1] + 1.645*att_PSM_log_tangible_asset_sa20[2]),
  #                                     down = c(att_PSM_log_tangible_asset_twfe[1] - 1.645*att_PSM_log_tangible_asset_twfe[2],att_PSM_log_tangible_asset_sa20[1] - 1.645*att_PSM_log_tangible_asset_sa20[2]),
  #                                     up95 = c(att_PSM_log_tangible_asset_twfe[1] + 1.960*att_PSM_log_tangible_asset_twfe[2], att_PSM_log_tangible_asset_sa20[1] + 1.960*att_PSM_log_tangible_asset_sa20[2]),
  #                                     down95 = c(att_PSM_log_tangible_asset_twfe[1] - 1.960*att_PSM_log_tangible_asset_twfe[2],att_PSM_log_tangible_asset_sa20[1] - 1.960*att_PSM_log_tangible_asset_sa20[2])
  # )
  # 
  # p_log_tangible_asset_summary <- ggplot(log_tangible_asset_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(log_tangible_asset_summary$down, na.rm=T),max(log_tangible_asset_summary$up, na.rm=T)), max(-min(log_tangible_asset_summary$down, na.rm=T),max(log_tangible_asset_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "log_tangible_asset", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_log_tangible_asset <- p_PSM_log_tangible_asset_fe#+p_log_tangible_asset_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ################## log_intangible_asset------------
  # PSM TWFE
  es_PSM_log_intangible_asset_twfe = feols(log_intangible_asset ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_log_intangible_asset_twfe <- feols(log_intangible_asset ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_intangible_asset_twfe<- c(es2_PSM_log_intangible_asset_twfe$coefficients[1], es2_PSM_log_intangible_asset_twfe$se[1])
  
  # PSM sa20
  es_PSM_log_intangible_asset_sa20 = feols(log_intangible_asset ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_intangible_asset_sa20 <- aggregate(es_PSM_log_intangible_asset_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_log_intangible_asset_fe <- ggiplot(list('S&A(2020)' = es_PSM_log_intangible_asset_sa20,'TWFE' = es_PSM_log_intangible_asset_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_intangible_asset",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # log_intangible_asset_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_log_intangible_asset_twfe[1], att_PSM_log_intangible_asset_sa20[1]),
  #                                     up = c(att_PSM_log_intangible_asset_twfe[1] + 1.645*att_PSM_log_intangible_asset_twfe[2],att_PSM_log_intangible_asset_sa20[1] + 1.645*att_PSM_log_intangible_asset_sa20[2]),
  #                                     down = c(att_PSM_log_intangible_asset_twfe[1] - 1.645*att_PSM_log_intangible_asset_twfe[2],att_PSM_log_intangible_asset_sa20[1] - 1.645*att_PSM_log_intangible_asset_sa20[2]),
  #                                     up95 = c(att_PSM_log_intangible_asset_twfe[1] + 1.960*att_PSM_log_intangible_asset_twfe[2], att_PSM_log_intangible_asset_sa20[1] + 1.960*att_PSM_log_intangible_asset_sa20[2]),
  #                                     down95 = c(att_PSM_log_intangible_asset_twfe[1] - 1.960*att_PSM_log_intangible_asset_twfe[2],att_PSM_log_intangible_asset_sa20[1] - 1.960*att_PSM_log_intangible_asset_sa20[2])
  # )
  # 
  # p_log_intangible_asset_summary <- ggplot(log_intangible_asset_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(log_intangible_asset_summary$down, na.rm=T),max(log_intangible_asset_summary$up, na.rm=T)), max(-min(log_intangible_asset_summary$down, na.rm=T),max(log_intangible_asset_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "log_intangible_asset", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_log_intangible_asset <- p_PSM_log_intangible_asset_fe#+p_log_intangible_asset_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ## output_asset-----------
  pa_asset_sum <- pa_log_sum_asset / pa_log_tangible_asset / pa_log_intangible_asset + plot_annotation(
    title = "Effect on the Asset",
    subtitle = target,
    #caption = caption_main
  ) &
    theme(text = element_text(family="Meiryo UI"))
  
  filename_pdf <- paste("03_robustness_output/", target,"_21_asset_TWFE_", count_intervention, ".pdf", sep="")
  ggsave(filename_pdf,pa_asset_sum, 
         width=12, height =25.5, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("03_robustness_output/", target,"_21_asset_TWFE_", count_intervention, ".png", sep="")
  ggsave(filename_png,pa_asset_sum, 
         width=12, height =25.5, units = "cm", dpi=200) 
  
  # write.xlsx(list("log_sum_asset"=log_sum_asset_summary, "log_tangible_asset"= log_tangible_asset_summary, "log_intangible_asset"= log_intangible_asset_summary), paste("03_robustness_output/",target,"_21asset_TWFE_table.xlsx", sep=""))
  # write.xlsx(list("log_sum_asset"=log_sum_asset_summary_appendix, "log_tangible_asset"= log_tangible_asset_summary_appendix, "log_intangible_asset"= log_intangible_asset_summary_appendix), paste("03_robustness_output/",target,"_21asset_TWFE_table_appendix.xlsx"))
}
robustness_TWFE_employment <- function(target){
  ################## log_workers------------
  # PSM TWFE
  es_PSM_log_workers_twfe = feols(log_workers ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_log_workers_twfe <- feols(log_workers ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_workers_twfe<- c(es2_PSM_log_workers_twfe$coefficients[1], es2_PSM_log_workers_twfe$se[1])
  
  # PSM sa20
  es_PSM_log_workers_sa20 = feols(log_workers ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_workers_sa20 <- aggregate(es_PSM_log_workers_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_log_workers_fe <- ggiplot(list('S&A(2020)' = es_PSM_log_workers_sa20,'TWFE' = es_PSM_log_workers_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_workers",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # log_workers_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_log_workers_twfe[1], att_PSM_log_workers_sa20[1]),
  #                                     up = c(att_PSM_log_workers_twfe[1] + 1.645*att_PSM_log_workers_twfe[2],att_PSM_log_workers_sa20[1] + 1.645*att_PSM_log_workers_sa20[2]),
  #                                     down = c(att_PSM_log_workers_twfe[1] - 1.645*att_PSM_log_workers_twfe[2],att_PSM_log_workers_sa20[1] - 1.645*att_PSM_log_workers_sa20[2]),
  #                                     up95 = c(att_PSM_log_workers_twfe[1] + 1.960*att_PSM_log_workers_twfe[2], att_PSM_log_workers_sa20[1] + 1.960*att_PSM_log_workers_sa20[2]),
  #                                     down95 = c(att_PSM_log_workers_twfe[1] - 1.960*att_PSM_log_workers_twfe[2],att_PSM_log_workers_sa20[1] - 1.960*att_PSM_log_workers_sa20[2])
  # )
  # 
  # p_log_workers_summary <- ggplot(log_workers_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(log_workers_summary$down, na.rm=T),max(log_workers_summary$up, na.rm=T)), max(-min(log_workers_summary$down, na.rm=T),max(log_workers_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "log_workers", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_log_workers <- p_PSM_log_workers_fe#+p_log_workers_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ################## log_indefinite_workers------------
  # PSM TWFE
  es_PSM_log_indefinite_workers_twfe = feols(log_indefinite_workers ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_log_indefinite_workers_twfe <- feols(log_indefinite_workers ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_indefinite_workers_twfe<- c(es2_PSM_log_indefinite_workers_twfe$coefficients[1], es2_PSM_log_indefinite_workers_twfe$se[1])
  
  # PSM sa20
  es_PSM_log_indefinite_workers_sa20 = feols(log_indefinite_workers ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_indefinite_workers_sa20 <- aggregate(es_PSM_log_indefinite_workers_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_log_indefinite_workers_fe <- ggiplot(list('S&A(2020)' = es_PSM_log_indefinite_workers_sa20,'TWFE' = es_PSM_log_indefinite_workers_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_indefinite_workers",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # log_indefinite_workers_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_log_indefinite_workers_twfe[1], att_PSM_log_indefinite_workers_sa20[1]),
  #                                     up = c(att_PSM_log_indefinite_workers_twfe[1] + 1.645*att_PSM_log_indefinite_workers_twfe[2],att_PSM_log_indefinite_workers_sa20[1] + 1.645*att_PSM_log_indefinite_workers_sa20[2]),
  #                                     down = c(att_PSM_log_indefinite_workers_twfe[1] - 1.645*att_PSM_log_indefinite_workers_twfe[2],att_PSM_log_indefinite_workers_sa20[1] - 1.645*att_PSM_log_indefinite_workers_sa20[2]),
  #                                     up95 = c(att_PSM_log_indefinite_workers_twfe[1] + 1.960*att_PSM_log_indefinite_workers_twfe[2], att_PSM_log_indefinite_workers_sa20[1] + 1.960*att_PSM_log_indefinite_workers_sa20[2]),
  #                                     down95 = c(att_PSM_log_indefinite_workers_twfe[1] - 1.960*att_PSM_log_indefinite_workers_twfe[2],att_PSM_log_indefinite_workers_sa20[1] - 1.960*att_PSM_log_indefinite_workers_sa20[2])
  # )
  # 
  # p_log_indefinite_workers_summary <- ggplot(log_indefinite_workers_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(log_indefinite_workers_summary$down, na.rm=T),max(log_indefinite_workers_summary$up, na.rm=T)), max(-min(log_indefinite_workers_summary$down, na.rm=T),max(log_indefinite_workers_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "log_indefinite_workers", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_log_indefinite_workers <- p_PSM_log_indefinite_workers_fe#+p_log_indefinite_workers_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ################## log_fixedterm_workers------------
  # PSM TWFE
  es_PSM_log_fixedterm_workers_twfe = feols(log_fixedterm_workers ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_log_fixedterm_workers_twfe <- feols(log_fixedterm_workers ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_fixedterm_workers_twfe<- c(es2_PSM_log_fixedterm_workers_twfe$coefficients[1], es2_PSM_log_fixedterm_workers_twfe$se[1])
  
  # PSM sa20
  es_PSM_log_fixedterm_workers_sa20 = feols(log_fixedterm_workers ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_fixedterm_workers_sa20 <- aggregate(es_PSM_log_fixedterm_workers_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_log_fixedterm_workers_fe <- ggiplot(list('S&A(2020)' = es_PSM_log_fixedterm_workers_sa20,'TWFE' = es_PSM_log_fixedterm_workers_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_fixedterm_workers",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # log_fixedterm_workers_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_log_fixedterm_workers_twfe[1], att_PSM_log_fixedterm_workers_sa20[1]),
  #                                     up = c(att_PSM_log_fixedterm_workers_twfe[1] + 1.645*att_PSM_log_fixedterm_workers_twfe[2],att_PSM_log_fixedterm_workers_sa20[1] + 1.645*att_PSM_log_fixedterm_workers_sa20[2]),
  #                                     down = c(att_PSM_log_fixedterm_workers_twfe[1] - 1.645*att_PSM_log_fixedterm_workers_twfe[2],att_PSM_log_fixedterm_workers_sa20[1] - 1.645*att_PSM_log_fixedterm_workers_sa20[2]),
  #                                     up95 = c(att_PSM_log_fixedterm_workers_twfe[1] + 1.960*att_PSM_log_fixedterm_workers_twfe[2], att_PSM_log_fixedterm_workers_sa20[1] + 1.960*att_PSM_log_fixedterm_workers_sa20[2]),
  #                                     down95 = c(att_PSM_log_fixedterm_workers_twfe[1] - 1.960*att_PSM_log_fixedterm_workers_twfe[2],att_PSM_log_fixedterm_workers_sa20[1] - 1.960*att_PSM_log_fixedterm_workers_sa20[2])
  # )
  # 
  # p_log_fixedterm_workers_summary <- ggplot(log_fixedterm_workers_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(log_fixedterm_workers_summary$down, na.rm=T),max(log_fixedterm_workers_summary$up, na.rm=T)), max(-min(log_fixedterm_workers_summary$down, na.rm=T),max(log_fixedterm_workers_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "log_fixedterm_workers", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_log_fixedterm_workers <- p_PSM_log_fixedterm_workers_fe#+p_log_fixedterm_workers_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ################## log_fixedterm_workers_equivalent------------
  # PSM TWFE
  es_PSM_log_fixedterm_workers_equivalent_twfe = feols(log_fixedterm_workers_equivalent ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_log_fixedterm_workers_equivalent_twfe <- feols(log_fixedterm_workers_equivalent ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_fixedterm_workers_equivalent_twfe<- c(es2_PSM_log_fixedterm_workers_equivalent_twfe$coefficients[1], es2_PSM_log_fixedterm_workers_equivalent_twfe$se[1])
  
  # PSM sa20
  es_PSM_log_fixedterm_workers_equivalent_sa20 = feols(log_fixedterm_workers_equivalent ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_fixedterm_workers_equivalent_sa20 <- aggregate(es_PSM_log_fixedterm_workers_equivalent_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_log_fixedterm_workers_equivalent_fe <- ggiplot(list('S&A(2020)' = es_PSM_log_fixedterm_workers_equivalent_sa20,'TWFE' = es_PSM_log_fixedterm_workers_equivalent_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_fixedterm_workers_equivalent",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # log_fixedterm_workers_equivalent_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_log_fixedterm_workers_equivalent_twfe[1], att_PSM_log_fixedterm_workers_equivalent_sa20[1]),
  #                                     up = c(att_PSM_log_fixedterm_workers_equivalent_twfe[1] + 1.645*att_PSM_log_fixedterm_workers_equivalent_twfe[2],att_PSM_log_fixedterm_workers_equivalent_sa20[1] + 1.645*att_PSM_log_fixedterm_workers_equivalent_sa20[2]),
  #                                     down = c(att_PSM_log_fixedterm_workers_equivalent_twfe[1] - 1.645*att_PSM_log_fixedterm_workers_equivalent_twfe[2],att_PSM_log_fixedterm_workers_equivalent_sa20[1] - 1.645*att_PSM_log_fixedterm_workers_equivalent_sa20[2]),
  #                                     up95 = c(att_PSM_log_fixedterm_workers_equivalent_twfe[1] + 1.960*att_PSM_log_fixedterm_workers_equivalent_twfe[2], att_PSM_log_fixedterm_workers_equivalent_sa20[1] + 1.960*att_PSM_log_fixedterm_workers_equivalent_sa20[2]),
  #                                     down95 = c(att_PSM_log_fixedterm_workers_equivalent_twfe[1] - 1.960*att_PSM_log_fixedterm_workers_equivalent_twfe[2],att_PSM_log_fixedterm_workers_equivalent_sa20[1] - 1.960*att_PSM_log_fixedterm_workers_equivalent_sa20[2])
  # )
  # 
  # p_log_fixedterm_workers_equivalent_summary <- ggplot(log_fixedterm_workers_equivalent_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(log_fixedterm_workers_equivalent_summary$down, na.rm=T),max(log_fixedterm_workers_equivalent_summary$up, na.rm=T)), max(-min(log_fixedterm_workers_equivalent_summary$down, na.rm=T),max(log_fixedterm_workers_equivalent_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "log_fixedterm_workers_equivalent", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_log_fixedterm_workers_equivalent <- p_PSM_log_fixedterm_workers_equivalent_fe#+p_log_fixedterm_workers_equivalent_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ################## log_salary------------
  # PSM TWFE
  es_PSM_log_salary_twfe = feols(log_salary ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_log_salary_twfe <- feols(log_salary ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_salary_twfe<- c(es2_PSM_log_salary_twfe$coefficients[1], es2_PSM_log_salary_twfe$se[1])
  
  # PSM sa20
  es_PSM_log_salary_sa20 = feols(log_salary ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_salary_sa20 <- aggregate(es_PSM_log_salary_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_log_salary_fe <- ggiplot(list('S&A(2020)' = es_PSM_log_salary_sa20,'TWFE' = es_PSM_log_salary_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_salary",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # log_salary_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_log_salary_twfe[1], att_PSM_log_salary_sa20[1]),
  #                                     up = c(att_PSM_log_salary_twfe[1] + 1.645*att_PSM_log_salary_twfe[2],att_PSM_log_salary_sa20[1] + 1.645*att_PSM_log_salary_sa20[2]),
  #                                     down = c(att_PSM_log_salary_twfe[1] - 1.645*att_PSM_log_salary_twfe[2],att_PSM_log_salary_sa20[1] - 1.645*att_PSM_log_salary_sa20[2]),
  #                                     up95 = c(att_PSM_log_salary_twfe[1] + 1.960*att_PSM_log_salary_twfe[2], att_PSM_log_salary_sa20[1] + 1.960*att_PSM_log_salary_sa20[2]),
  #                                     down95 = c(att_PSM_log_salary_twfe[1] - 1.960*att_PSM_log_salary_twfe[2],att_PSM_log_salary_sa20[1] - 1.960*att_PSM_log_salary_sa20[2])
  # )
  # 
  # p_log_salary_summary <- ggplot(log_salary_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(log_salary_summary$down, na.rm=T),max(log_salary_summary$up, na.rm=T)), max(-min(log_salary_summary$down, na.rm=T),max(log_salary_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "log_salary", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_log_salary <- p_PSM_log_salary_fe#+p_log_salary_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ################## log_benefit------------
  # PSM TWFE
  es_PSM_log_benefit_twfe = feols(log_benefit ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_log_benefit_twfe <- feols(log_benefit ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_benefit_twfe<- c(es2_PSM_log_benefit_twfe$coefficients[1], es2_PSM_log_benefit_twfe$se[1])
  
  # PSM sa20
  es_PSM_log_benefit_sa20 = feols(log_benefit ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_benefit_sa20 <- aggregate(es_PSM_log_benefit_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_log_benefit_fe <- ggiplot(list('S&A(2020)' = es_PSM_log_benefit_sa20,'TWFE' = es_PSM_log_benefit_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_benefit",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # log_benefit_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_log_benefit_twfe[1], att_PSM_log_benefit_sa20[1]),
  #                                     up = c(att_PSM_log_benefit_twfe[1] + 1.645*att_PSM_log_benefit_twfe[2],att_PSM_log_benefit_sa20[1] + 1.645*att_PSM_log_benefit_sa20[2]),
  #                                     down = c(att_PSM_log_benefit_twfe[1] - 1.645*att_PSM_log_benefit_twfe[2],att_PSM_log_benefit_sa20[1] - 1.645*att_PSM_log_benefit_sa20[2]),
  #                                     up95 = c(att_PSM_log_benefit_twfe[1] + 1.960*att_PSM_log_benefit_twfe[2], att_PSM_log_benefit_sa20[1] + 1.960*att_PSM_log_benefit_sa20[2]),
  #                                     down95 = c(att_PSM_log_benefit_twfe[1] - 1.960*att_PSM_log_benefit_twfe[2],att_PSM_log_benefit_sa20[1] - 1.960*att_PSM_log_benefit_sa20[2])
  # )
  # 
  # p_log_benefit_summary <- ggplot(log_benefit_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(log_benefit_summary$down, na.rm=T),max(log_benefit_summary$up, na.rm=T)), max(-min(log_benefit_summary$down, na.rm=T),max(log_benefit_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "log_benefit", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_log_benefit <- p_PSM_log_benefit_fe#+p_log_benefit_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ## output_employment-----------
  pa_employment_sum <- pa_log_workers/pa_log_indefinite_workers/pa_log_fixedterm_workers/pa_log_fixedterm_workers_equivalent/pa_log_salary/pa_log_benefit + plot_annotation(
    title = "Effect on the Employment",
    subtitle = target,
    #caption = caption_main
  ) &
    theme(text = element_text(family="Meiryo UI"))
  
  filename_pdf <- paste("03_robustness_output/", target,"_22_employment_TWFE_", count_intervention, ".pdf", sep="")
  ggsave(filename_pdf,pa_employment_sum, 
         width=12, height =48, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("03_robustness_output/", target,"_22_employment_TWFE_", count_intervention, ".png", sep="")
  ggsave(filename_png,pa_employment_sum, 
         width=12, height =48, units = "cm", dpi=200) 
  
  # write.xlsx(list("log_workers"=log_workers_summary,"log_indefinite_workers"=log_indefinite_workers_summary,"log_fixedterm_workers"=log_fixedterm_workers_summary,"log_fixedterm_workers_eq"=log_fixedterm_workers_equivalent_summary,"log_salary"=log_salary_summary,"log_benefit"=log_benefit_summary), paste("03_robustness_output/",target,"_22employment_TWFE_table.xlsx", sep=""))
  # write.xlsx(list("log_workers"=log_workers_summary_appendix,"log_indefinite_workers"=log_indefinite_workers_summary_appendix,"log_fixedterm_workers"=log_fixedterm_workers_summary_appendix,"log_fixedterm_workers_eq"=log_fixedterm_workers_equivalent_summary_appendix,"log_salary"=log_salary_summary_appendix,"log_benefit"=log_benefit_summary_appendix), paste("03_robustness_output/",target,"_22employment_TWFE_table_appendix.xlsx"))
}
robustness_TWFE_business <- function(target){
  ################## log_sales------------
  # PSM TWFE
  es_PSM_log_sales_twfe = feols(log_sales ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_log_sales_twfe <- feols(log_sales ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_sales_twfe<- c(es2_PSM_log_sales_twfe$coefficients[1], es2_PSM_log_sales_twfe$se[1])
  
  # PSM sa20
  es_PSM_log_sales_sa20 = feols(log_sales ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_sales_sa20 <- aggregate(es_PSM_log_sales_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_log_sales_fe <- ggiplot(list('S&A(2020)' = es_PSM_log_sales_sa20,'TWFE' = es_PSM_log_sales_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_sales",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # log_sales_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_log_sales_twfe[1], att_PSM_log_sales_sa20[1]),
  #                                     up = c(att_PSM_log_sales_twfe[1] + 1.645*att_PSM_log_sales_twfe[2],att_PSM_log_sales_sa20[1] + 1.645*att_PSM_log_sales_sa20[2]),
  #                                     down = c(att_PSM_log_sales_twfe[1] - 1.645*att_PSM_log_sales_twfe[2],att_PSM_log_sales_sa20[1] - 1.645*att_PSM_log_sales_sa20[2]),
  #                                     up95 = c(att_PSM_log_sales_twfe[1] + 1.960*att_PSM_log_sales_twfe[2], att_PSM_log_sales_sa20[1] + 1.960*att_PSM_log_sales_sa20[2]),
  #                                     down95 = c(att_PSM_log_sales_twfe[1] - 1.960*att_PSM_log_sales_twfe[2],att_PSM_log_sales_sa20[1] - 1.960*att_PSM_log_sales_sa20[2])
  # )
  # 
  # p_log_sales_summary <- ggplot(log_sales_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(log_sales_summary$down, na.rm=T),max(log_sales_summary$up, na.rm=T)), max(-min(log_sales_summary$down, na.rm=T),max(log_sales_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "log_sales", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_log_sales <- p_PSM_log_sales_fe#+p_log_sales_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ################## log_tax------------
  # PSM TWFE
  es_PSM_log_tax_twfe = feols(log_tax ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_log_tax_twfe <- feols(log_tax ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_tax_twfe<- c(es2_PSM_log_tax_twfe$coefficients[1], es2_PSM_log_tax_twfe$se[1])
  
  # PSM sa20
  es_PSM_log_tax_sa20 = feols(log_tax ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_tax_sa20 <- aggregate(es_PSM_log_tax_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_log_tax_fe <- ggiplot(list('S&A(2020)' = es_PSM_log_tax_sa20,'TWFE' = es_PSM_log_tax_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_tax",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # log_tax_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_log_tax_twfe[1], att_PSM_log_tax_sa20[1]),
  #                                     up = c(att_PSM_log_tax_twfe[1] + 1.645*att_PSM_log_tax_twfe[2],att_PSM_log_tax_sa20[1] + 1.645*att_PSM_log_tax_sa20[2]),
  #                                     down = c(att_PSM_log_tax_twfe[1] - 1.645*att_PSM_log_tax_twfe[2],att_PSM_log_tax_sa20[1] - 1.645*att_PSM_log_tax_sa20[2]),
  #                                     up95 = c(att_PSM_log_tax_twfe[1] + 1.960*att_PSM_log_tax_twfe[2], att_PSM_log_tax_sa20[1] + 1.960*att_PSM_log_tax_sa20[2]),
  #                                     down95 = c(att_PSM_log_tax_twfe[1] - 1.960*att_PSM_log_tax_twfe[2],att_PSM_log_tax_sa20[1] - 1.960*att_PSM_log_tax_sa20[2])
  # )
  # 
  # p_log_tax_summary <- ggplot(log_tax_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(log_tax_summary$down, na.rm=T),max(log_tax_summary$up, na.rm=T)), max(-min(log_tax_summary$down, na.rm=T),max(log_tax_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "log_tax", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_log_tax <- p_PSM_log_tax_fe#+p_log_tax_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ################## log_office------------
  # PSM TWFE
  es_PSM_log_office_twfe = feols(log_office ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_log_office_twfe <- feols(log_office ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_office_twfe<- c(es2_PSM_log_office_twfe$coefficients[1], es2_PSM_log_office_twfe$se[1])
  
  # PSM sa20
  es_PSM_log_office_sa20 = feols(log_office ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_office_sa20 <- aggregate(es_PSM_log_office_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_log_office_fe <- ggiplot(list('S&A(2020)' = es_PSM_log_office_sa20,'TWFE' = es_PSM_log_office_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_office",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # log_office_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_log_office_twfe[1], att_PSM_log_office_sa20[1]),
  #                                     up = c(att_PSM_log_office_twfe[1] + 1.645*att_PSM_log_office_twfe[2],att_PSM_log_office_sa20[1] + 1.645*att_PSM_log_office_sa20[2]),
  #                                     down = c(att_PSM_log_office_twfe[1] - 1.645*att_PSM_log_office_twfe[2],att_PSM_log_office_sa20[1] - 1.645*att_PSM_log_office_sa20[2]),
  #                                     up95 = c(att_PSM_log_office_twfe[1] + 1.960*att_PSM_log_office_twfe[2], att_PSM_log_office_sa20[1] + 1.960*att_PSM_log_office_sa20[2]),
  #                                     down95 = c(att_PSM_log_office_twfe[1] - 1.960*att_PSM_log_office_twfe[2],att_PSM_log_office_sa20[1] - 1.960*att_PSM_log_office_sa20[2])
  # )
  # 
  # p_log_office_summary <- ggplot(log_office_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(log_office_summary$down, na.rm=T),max(log_office_summary$up, na.rm=T)), max(-min(log_office_summary$down, na.rm=T),max(log_office_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "log_office", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_log_office <- p_PSM_log_office_fe#+p_log_office_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ################## ROA------------
  # PSM TWFE
  es_PSM_ROA_twfe = feols(ROA ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_ROA_twfe <- feols(ROA ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_ROA_twfe<- c(es2_PSM_ROA_twfe$coefficients[1], es2_PSM_ROA_twfe$se[1])
  
  # PSM sa20
  es_PSM_ROA_sa20 = feols(ROA ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_ROA_sa20 <- aggregate(es_PSM_ROA_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_ROA_fe <- ggiplot(list('S&A(2020)' = es_PSM_ROA_sa20,'TWFE' = es_PSM_ROA_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14),ylim = c(-0.1, 0.1)) + 
    labs(title = "ROA",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # ROA_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_ROA_twfe[1], att_PSM_ROA_sa20[1]),
  #                                     up = c(att_PSM_ROA_twfe[1] + 1.645*att_PSM_ROA_twfe[2],att_PSM_ROA_sa20[1] + 1.645*att_PSM_ROA_sa20[2]),
  #                                     down = c(att_PSM_ROA_twfe[1] - 1.645*att_PSM_ROA_twfe[2],att_PSM_ROA_sa20[1] - 1.645*att_PSM_ROA_sa20[2]),
  #                                     up95 = c(att_PSM_ROA_twfe[1] + 1.960*att_PSM_ROA_twfe[2], att_PSM_ROA_sa20[1] + 1.960*att_PSM_ROA_sa20[2]),
  #                                     down95 = c(att_PSM_ROA_twfe[1] - 1.960*att_PSM_ROA_twfe[2],att_PSM_ROA_sa20[1] - 1.960*att_PSM_ROA_sa20[2])
  # )
  # 
  # p_ROA_summary <- ggplot(ROA_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(ROA_summary$down, na.rm=T),max(ROA_summary$up, na.rm=T)), max(-min(ROA_summary$down, na.rm=T),max(ROA_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "ROA", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_ROA <- p_PSM_ROA_fe#+p_ROA_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ################## net_profit_workers------------
  # PSM TWFE
  es_PSM_net_profit_workers_twfe = feols(net_profit_workers ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_net_profit_workers_twfe <- feols(net_profit_workers ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_net_profit_workers_twfe<- c(es2_PSM_net_profit_workers_twfe$coefficients[1], es2_PSM_net_profit_workers_twfe$se[1])
  
  # PSM sa20
  es_PSM_net_profit_workers_sa20 = feols(net_profit_workers ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_net_profit_workers_sa20 <- aggregate(es_PSM_net_profit_workers_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_net_profit_workers_fe <- ggiplot(list('S&A(2020)' = es_PSM_net_profit_workers_sa20,'TWFE' = es_PSM_net_profit_workers_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14),ylim = c(-5000000, 5000000)) + 
    labs(title = "net_profit_workers",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # net_profit_workers_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_net_profit_workers_twfe[1], att_PSM_net_profit_workers_sa20[1]),
  #                                     up = c(att_PSM_net_profit_workers_twfe[1] + 1.645*att_PSM_net_profit_workers_twfe[2],att_PSM_net_profit_workers_sa20[1] + 1.645*att_PSM_net_profit_workers_sa20[2]),
  #                                     down = c(att_PSM_net_profit_workers_twfe[1] - 1.645*att_PSM_net_profit_workers_twfe[2],att_PSM_net_profit_workers_sa20[1] - 1.645*att_PSM_net_profit_workers_sa20[2]),
  #                                     up95 = c(att_PSM_net_profit_workers_twfe[1] + 1.960*att_PSM_net_profit_workers_twfe[2], att_PSM_net_profit_workers_sa20[1] + 1.960*att_PSM_net_profit_workers_sa20[2]),
  #                                     down95 = c(att_PSM_net_profit_workers_twfe[1] - 1.960*att_PSM_net_profit_workers_twfe[2],att_PSM_net_profit_workers_sa20[1] - 1.960*att_PSM_net_profit_workers_sa20[2])
  # )
  # 
  # p_net_profit_workers_summary <- ggplot(net_profit_workers_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(net_profit_workers_summary$down, na.rm=T),max(net_profit_workers_summary$up, na.rm=T)), max(-min(net_profit_workers_summary$down, na.rm=T),max(net_profit_workers_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "net_profit_workers", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_net_profit_workers <- p_PSM_net_profit_workers_fe#+p_net_profit_workers_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ## output_business-----------
  pa_business_sum <- pa_log_sales/pa_log_tax/pa_log_office/pa_ROA/pa_net_profit_workers + plot_annotation(
    title = "Effect on the Business",
    subtitle = target,
    #caption = caption_main
  ) &
    theme(text = element_text(family="Meiryo UI"))
  
  filename_pdf <- paste("03_robustness_output/", target,"_23_business_TWFE_", count_intervention, ".pdf", sep="")
  ggsave(filename_pdf,pa_business_sum, 
         width=12, height =40.5, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("03_robustness_output/", target,"_23_business_TWFE_", count_intervention, ".png", sep="")
  ggsave(filename_png,pa_business_sum, 
         width=12, height =40.5, units = "cm", dpi=200) 
  
  # write.xlsx(list("log_sales"=log_sales_summary,"log_tax"=log_tax_summary,"log_office"=log_office_summary,"ROA"=ROA_summary,"net_profit_workers"=net_profit_workers_summary), paste("03_robustness_output/",target,"_23business_TWFE_table.xlsx", sep=""))
  # write.xlsx(list("log_sales"=log_sales_summary_appendix,"log_tax"=log_tax_summary_appendix,"log_office"=log_office_summary_appendix,"ROA"=ROA_summary_appendix,"net_profit_workers"=net_profit_workers_summary_appendix), paste("03_robustness_output/",target,"_23business_TWFE_table_appendix.xlsx"))
}
robustness_TWFE_trade <- function(target){
  ################## flag_export------------
  # PSM TWFE
  es_PSM_flag_export_twfe = feols(flag_export ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_flag_export_twfe <- feols(flag_export ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_flag_export_twfe<- c(es2_PSM_flag_export_twfe$coefficients[1], es2_PSM_flag_export_twfe$se[1])
  
  # PSM sa20
  es_PSM_flag_export_sa20 = feols(flag_export ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_flag_export_sa20 <- aggregate(es_PSM_flag_export_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_flag_export_fe <- ggiplot(list('S&A(2020)' = es_PSM_flag_export_sa20,'TWFE' = es_PSM_flag_export_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + 
    labs(title = "flag_export",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # flag_export_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_flag_export_twfe[1], att_PSM_flag_export_sa20[1]),
  #                                     up = c(att_PSM_flag_export_twfe[1] + 1.645*att_PSM_flag_export_twfe[2],att_PSM_flag_export_sa20[1] + 1.645*att_PSM_flag_export_sa20[2]),
  #                                     down = c(att_PSM_flag_export_twfe[1] - 1.645*att_PSM_flag_export_twfe[2],att_PSM_flag_export_sa20[1] - 1.645*att_PSM_flag_export_sa20[2]),
  #                                     up95 = c(att_PSM_flag_export_twfe[1] + 1.960*att_PSM_flag_export_twfe[2], att_PSM_flag_export_sa20[1] + 1.960*att_PSM_flag_export_sa20[2]),
  #                                     down95 = c(att_PSM_flag_export_twfe[1] - 1.960*att_PSM_flag_export_twfe[2],att_PSM_flag_export_sa20[1] - 1.960*att_PSM_flag_export_sa20[2])
  # )
  # 
  # p_flag_export_summary <- ggplot(flag_export_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(flag_export_summary$down, na.rm=T),max(flag_export_summary$up, na.rm=T)), max(-min(flag_export_summary$down, na.rm=T),max(flag_export_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "flag_export", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_flag_export <- p_PSM_flag_export_fe#+p_flag_export_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ################## flag_import------------
  # PSM TWFE
  es_PSM_flag_import_twfe = feols(flag_import ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_flag_import_twfe <- feols(flag_import ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_flag_import_twfe<- c(es2_PSM_flag_import_twfe$coefficients[1], es2_PSM_flag_import_twfe$se[1])
  
  # PSM sa20
  es_PSM_flag_import_sa20 = feols(flag_import ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_flag_import_sa20 <- aggregate(es_PSM_flag_import_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_flag_import_fe <- ggiplot(list('S&A(2020)' = es_PSM_flag_import_sa20,'TWFE' = es_PSM_flag_import_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + 
    labs(title = "flag_import",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # flag_import_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_flag_import_twfe[1], att_PSM_flag_import_sa20[1]),
  #                                     up = c(att_PSM_flag_import_twfe[1] + 1.645*att_PSM_flag_import_twfe[2],att_PSM_flag_import_sa20[1] + 1.645*att_PSM_flag_import_sa20[2]),
  #                                     down = c(att_PSM_flag_import_twfe[1] - 1.645*att_PSM_flag_import_twfe[2],att_PSM_flag_import_sa20[1] - 1.645*att_PSM_flag_import_sa20[2]),
  #                                     up95 = c(att_PSM_flag_import_twfe[1] + 1.960*att_PSM_flag_import_twfe[2], att_PSM_flag_import_sa20[1] + 1.960*att_PSM_flag_import_sa20[2]),
  #                                     down95 = c(att_PSM_flag_import_twfe[1] - 1.960*att_PSM_flag_import_twfe[2],att_PSM_flag_import_sa20[1] - 1.960*att_PSM_flag_import_sa20[2])
  # )
  # 
  # p_flag_import_summary <- ggplot(flag_import_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(flag_import_summary$down, na.rm=T),max(flag_import_summary$up, na.rm=T)), max(-min(flag_import_summary$down, na.rm=T),max(flag_import_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "flag_import", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_flag_import <- p_PSM_flag_import_fe#+p_flag_import_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ################## log_export------------
  # PSM TWFE
  es_PSM_log_export_twfe = feols(log_export ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_log_export_twfe <- feols(log_export ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_export_twfe<- c(es2_PSM_log_export_twfe$coefficients[1], es2_PSM_log_export_twfe$se[1])
  
  # PSM sa20
  es_PSM_log_export_sa20 = feols(log_export ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_export_sa20 <- aggregate(es_PSM_log_export_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_log_export_fe <- ggiplot(list('S&A(2020)' = es_PSM_log_export_sa20,'TWFE' = es_PSM_log_export_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_export",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # log_export_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_log_export_twfe[1], att_PSM_log_export_sa20[1]),
  #                                     up = c(att_PSM_log_export_twfe[1] + 1.645*att_PSM_log_export_twfe[2],att_PSM_log_export_sa20[1] + 1.645*att_PSM_log_export_sa20[2]),
  #                                     down = c(att_PSM_log_export_twfe[1] - 1.645*att_PSM_log_export_twfe[2],att_PSM_log_export_sa20[1] - 1.645*att_PSM_log_export_sa20[2]),
  #                                     up95 = c(att_PSM_log_export_twfe[1] + 1.960*att_PSM_log_export_twfe[2], att_PSM_log_export_sa20[1] + 1.960*att_PSM_log_export_sa20[2]),
  #                                     down95 = c(att_PSM_log_export_twfe[1] - 1.960*att_PSM_log_export_twfe[2],att_PSM_log_export_sa20[1] - 1.960*att_PSM_log_export_sa20[2])
  # )
  # 
  # p_log_export_summary <- ggplot(log_export_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(log_export_summary$down, na.rm=T),max(log_export_summary$up, na.rm=T)), max(-min(log_export_summary$down, na.rm=T),max(log_export_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "log_export", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_log_export <- p_PSM_log_export_fe#+p_log_export_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ################## log_import------------
  # PSM TWFE
  es_PSM_log_import_twfe = feols(log_import ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_log_import_twfe <- feols(log_import ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_import_twfe<- c(es2_PSM_log_import_twfe$coefficients[1], es2_PSM_log_import_twfe$se[1])
  
  # PSM sa20
  es_PSM_log_import_sa20 = feols(log_import ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_import_sa20 <- aggregate(es_PSM_log_import_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_log_import_fe <- ggiplot(list('S&A(2020)' = es_PSM_log_import_sa20,'TWFE' = es_PSM_log_import_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_import",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # log_import_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_log_import_twfe[1], att_PSM_log_import_sa20[1]),
  #                                     up = c(att_PSM_log_import_twfe[1] + 1.645*att_PSM_log_import_twfe[2],att_PSM_log_import_sa20[1] + 1.645*att_PSM_log_import_sa20[2]),
  #                                     down = c(att_PSM_log_import_twfe[1] - 1.645*att_PSM_log_import_twfe[2],att_PSM_log_import_sa20[1] - 1.645*att_PSM_log_import_sa20[2]),
  #                                     up95 = c(att_PSM_log_import_twfe[1] + 1.960*att_PSM_log_import_twfe[2], att_PSM_log_import_sa20[1] + 1.960*att_PSM_log_import_sa20[2]),
  #                                     down95 = c(att_PSM_log_import_twfe[1] - 1.960*att_PSM_log_import_twfe[2],att_PSM_log_import_sa20[1] - 1.960*att_PSM_log_import_sa20[2])
  # )
  # 
  # p_log_import_summary <- ggplot(log_import_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(log_import_summary$down, na.rm=T),max(log_import_summary$up, na.rm=T)), max(-min(log_import_summary$down, na.rm=T),max(log_import_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "log_import", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_log_import <- p_PSM_log_import_fe#+p_log_import_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ## output_trade-----------
  pa_trade_sum <- pa_flag_export/pa_flag_import/pa_log_export/pa_log_import + plot_annotation(
    title = "Effect on the International Trade",
    subtitle = target,
    #caption = caption_main
  ) &
    theme(text = element_text(family="Meiryo UI"))
  
  filename_pdf <- paste("03_robustness_output/", target,"_24_trade_TWFE_", count_intervention, ".pdf", sep="")
  ggsave(filename_pdf,pa_trade_sum, 
         width=12, height =33, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("03_robustness_output/", target,"_24_trade_TWFE_", count_intervention, ".png", sep="")
  ggsave(filename_png,pa_trade_sum, 
         width=12, height =33, units = "cm", dpi=200) 
  
  # write.xlsx(list("flag_export"=flag_export_summary,"flag_import"=flag_import_summary,"log_export"=log_export_summary,"log_import"=log_import_summary), paste("03_robustness_output/",target,"_24trade_TWFE_table.xlsx", sep=""))
  # write.xlsx(list("flag_export"=flag_export_summary_appendix,"flag_import"=flag_import_summary_appendix,"log_export"=log_export_summary_appendix,"log_import"=log_import_summary_appendix), paste("03_robustness_output/",target,"_24trade_TWFE_table_appendix.xlsx"))
}
robustness_TWFE_trainingRD <- function(target){
  ################## flag_training------------
  # PSM TWFE
  es_PSM_flag_training_twfe = feols(flag_training ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_flag_training_twfe <- feols(flag_training ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_flag_training_twfe<- c(es2_PSM_flag_training_twfe$coefficients[1], es2_PSM_flag_training_twfe$se[1])
  
  # PSM sa20
  es_PSM_flag_training_sa20 = feols(flag_training ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_flag_training_sa20 <- aggregate(es_PSM_flag_training_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_flag_training_fe <- ggiplot(list('S&A(2020)' = es_PSM_flag_training_sa20,'TWFE' = es_PSM_flag_training_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + 
    labs(title = "flag_training",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # flag_training_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_flag_training_twfe[1], att_PSM_flag_training_sa20[1]),
  #                                     up = c(att_PSM_flag_training_twfe[1] + 1.645*att_PSM_flag_training_twfe[2],att_PSM_flag_training_sa20[1] + 1.645*att_PSM_flag_training_sa20[2]),
  #                                     down = c(att_PSM_flag_training_twfe[1] - 1.645*att_PSM_flag_training_twfe[2],att_PSM_flag_training_sa20[1] - 1.645*att_PSM_flag_training_sa20[2]),
  #                                     up95 = c(att_PSM_flag_training_twfe[1] + 1.960*att_PSM_flag_training_twfe[2], att_PSM_flag_training_sa20[1] + 1.960*att_PSM_flag_training_sa20[2]),
  #                                     down95 = c(att_PSM_flag_training_twfe[1] - 1.960*att_PSM_flag_training_twfe[2],att_PSM_flag_training_sa20[1] - 1.960*att_PSM_flag_training_sa20[2])
  # )
  # 
  # p_flag_training_summary <- ggplot(flag_training_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(flag_training_summary$down, na.rm=T),max(flag_training_summary$up, na.rm=T)), max(-min(flag_training_summary$down, na.rm=T),max(flag_training_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "flag_training", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_flag_training <- p_PSM_flag_training_fe#+p_flag_training_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ################## flag_RD------------
  # PSM TWFE
  es_PSM_flag_RD_twfe = feols(flag_RD ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_flag_RD_twfe <- feols(flag_RD ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_flag_RD_twfe<- c(es2_PSM_flag_RD_twfe$coefficients[1], es2_PSM_flag_RD_twfe$se[1])
  
  # PSM sa20
  es_PSM_flag_RD_sa20 = feols(flag_RD ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_flag_RD_sa20 <- aggregate(es_PSM_flag_RD_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_flag_RD_fe <- ggiplot(list('S&A(2020)' = es_PSM_flag_RD_sa20,'TWFE' = es_PSM_flag_RD_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + 
    labs(title = "flag_RD",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # flag_RD_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_flag_RD_twfe[1], att_PSM_flag_RD_sa20[1]),
  #                                     up = c(att_PSM_flag_RD_twfe[1] + 1.645*att_PSM_flag_RD_twfe[2],att_PSM_flag_RD_sa20[1] + 1.645*att_PSM_flag_RD_sa20[2]),
  #                                     down = c(att_PSM_flag_RD_twfe[1] - 1.645*att_PSM_flag_RD_twfe[2],att_PSM_flag_RD_sa20[1] - 1.645*att_PSM_flag_RD_sa20[2]),
  #                                     up95 = c(att_PSM_flag_RD_twfe[1] + 1.960*att_PSM_flag_RD_twfe[2], att_PSM_flag_RD_sa20[1] + 1.960*att_PSM_flag_RD_sa20[2]),
  #                                     down95 = c(att_PSM_flag_RD_twfe[1] - 1.960*att_PSM_flag_RD_twfe[2],att_PSM_flag_RD_sa20[1] - 1.960*att_PSM_flag_RD_sa20[2])
  # )
  # 
  # p_flag_RD_summary <- ggplot(flag_RD_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(flag_RD_summary$down, na.rm=T),max(flag_RD_summary$up, na.rm=T)), max(-min(flag_RD_summary$down, na.rm=T),max(flag_RD_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "flag_RD", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_flag_RD <- p_PSM_flag_RD_fe#+p_flag_RD_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ################## log_training------------
  # PSM TWFE
  es_PSM_log_training_twfe = feols(log_training ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_log_training_twfe <- feols(log_training ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_training_twfe<- c(es2_PSM_log_training_twfe$coefficients[1], es2_PSM_log_training_twfe$se[1])
  
  # PSM sa20
  es_PSM_log_training_sa20 = feols(log_training ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_training_sa20 <- aggregate(es_PSM_log_training_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_log_training_fe <- ggiplot(list('S&A(2020)' = es_PSM_log_training_sa20,'TWFE' = es_PSM_log_training_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_training",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # log_training_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_log_training_twfe[1], att_PSM_log_training_sa20[1]),
  #                                     up = c(att_PSM_log_training_twfe[1] + 1.645*att_PSM_log_training_twfe[2],att_PSM_log_training_sa20[1] + 1.645*att_PSM_log_training_sa20[2]),
  #                                     down = c(att_PSM_log_training_twfe[1] - 1.645*att_PSM_log_training_twfe[2],att_PSM_log_training_sa20[1] - 1.645*att_PSM_log_training_sa20[2]),
  #                                     up95 = c(att_PSM_log_training_twfe[1] + 1.960*att_PSM_log_training_twfe[2], att_PSM_log_training_sa20[1] + 1.960*att_PSM_log_training_sa20[2]),
  #                                     down95 = c(att_PSM_log_training_twfe[1] - 1.960*att_PSM_log_training_twfe[2],att_PSM_log_training_sa20[1] - 1.960*att_PSM_log_training_sa20[2])
  # )
  # 
  # p_log_training_summary <- ggplot(log_training_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(log_training_summary$down, na.rm=T),max(log_training_summary$up, na.rm=T)), max(-min(log_training_summary$down, na.rm=T),max(log_training_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "log_training", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_log_training <- p_PSM_log_training_fe#+p_log_training_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ################## log_RD------------
  # PSM TWFE
  es_PSM_log_RD_twfe = feols(log_RD ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_log_RD_twfe <- feols(log_RD ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_RD_twfe<- c(es2_PSM_log_RD_twfe$coefficients[1], es2_PSM_log_RD_twfe$se[1])
  
  # PSM sa20
  es_PSM_log_RD_sa20 = feols(log_RD ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_RD_sa20 <- aggregate(es_PSM_log_RD_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_log_RD_fe <- ggiplot(list('S&A(2020)' = es_PSM_log_RD_sa20,'TWFE' = es_PSM_log_RD_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_RD",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # log_RD_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_log_RD_twfe[1], att_PSM_log_RD_sa20[1]),
  #                                     up = c(att_PSM_log_RD_twfe[1] + 1.645*att_PSM_log_RD_twfe[2],att_PSM_log_RD_sa20[1] + 1.645*att_PSM_log_RD_sa20[2]),
  #                                     down = c(att_PSM_log_RD_twfe[1] - 1.645*att_PSM_log_RD_twfe[2],att_PSM_log_RD_sa20[1] - 1.645*att_PSM_log_RD_sa20[2]),
  #                                     up95 = c(att_PSM_log_RD_twfe[1] + 1.960*att_PSM_log_RD_twfe[2], att_PSM_log_RD_sa20[1] + 1.960*att_PSM_log_RD_sa20[2]),
  #                                     down95 = c(att_PSM_log_RD_twfe[1] - 1.960*att_PSM_log_RD_twfe[2],att_PSM_log_RD_sa20[1] - 1.960*att_PSM_log_RD_sa20[2])
  # )
  # 
  # p_log_RD_summary <- ggplot(log_RD_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(log_RD_summary$down, na.rm=T),max(log_RD_summary$up, na.rm=T)), max(-min(log_RD_summary$down, na.rm=T),max(log_RD_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "log_RD", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_log_RD <- p_PSM_log_RD_fe#+p_log_RD_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ## output_trainingRD-----------
  pa_trainingRD_sum <- pa_flag_training/pa_flag_RD/pa_log_training/pa_log_RD + plot_annotation(
    title = "Effect on the Training and RD",
    subtitle = target,
    #caption = caption_main
  ) &
    theme(text = element_text(family="Meiryo UI"))
  
  filename_pdf <- paste("03_robustness_output/", target,"_25_trainingRD_TWFE_", count_intervention, ".pdf", sep="")
  ggsave(filename_pdf,pa_trainingRD_sum, 
         width=12, height =33, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("03_robustness_output/", target,"_25_trainingRD_TWFE_", count_intervention, ".png", sep="")
  ggsave(filename_png,pa_trainingRD_sum, 
         width=12, height =33, units = "cm", dpi=200) 
  
  # write.xlsx(list("flag_training"=flag_training_summary,"flag_RD"=flag_RD_summary,"log_training"=log_training_summary,"log_RD"=log_RD_summary), paste("03_robustness_output/",target,"_25trainingRD_TWFE_table.xlsx", sep=""))
  # write.xlsx(list("flag_training"=flag_training_summary_appendix,"flag_RD"=flag_RD_summary_appendix,"log_training"=log_training_summary_appendix,"log_RD"=log_RD_summary_appendix), paste("03_robustness_output/",target,"_25trainingRD_TWFE_table_appendix.xlsx"))
}
robustness_TWFE_IP <- function(target){
  ################## flag_patent------------
  # PSM TWFE
  es_PSM_flag_patent_twfe = feols(flag_patent ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_flag_patent_twfe <- feols(flag_patent ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_flag_patent_twfe<- c(es2_PSM_flag_patent_twfe$coefficients[1], es2_PSM_flag_patent_twfe$se[1])
  
  # PSM sa20
  es_PSM_flag_patent_sa20 = feols(flag_patent ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_flag_patent_sa20 <- aggregate(es_PSM_flag_patent_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_flag_patent_fe <- ggiplot(list('S&A(2020)' = es_PSM_flag_patent_sa20,'TWFE' = es_PSM_flag_patent_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + 
    labs(title = "flag_patent",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # flag_patent_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_flag_patent_twfe[1], att_PSM_flag_patent_sa20[1]),
  #                                     up = c(att_PSM_flag_patent_twfe[1] + 1.645*att_PSM_flag_patent_twfe[2],att_PSM_flag_patent_sa20[1] + 1.645*att_PSM_flag_patent_sa20[2]),
  #                                     down = c(att_PSM_flag_patent_twfe[1] - 1.645*att_PSM_flag_patent_twfe[2],att_PSM_flag_patent_sa20[1] - 1.645*att_PSM_flag_patent_sa20[2]),
  #                                     up95 = c(att_PSM_flag_patent_twfe[1] + 1.960*att_PSM_flag_patent_twfe[2], att_PSM_flag_patent_sa20[1] + 1.960*att_PSM_flag_patent_sa20[2]),
  #                                     down95 = c(att_PSM_flag_patent_twfe[1] - 1.960*att_PSM_flag_patent_twfe[2],att_PSM_flag_patent_sa20[1] - 1.960*att_PSM_flag_patent_sa20[2])
  # )
  # 
  # p_flag_patent_summary <- ggplot(flag_patent_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(flag_patent_summary$down, na.rm=T),max(flag_patent_summary$up, na.rm=T)), max(-min(flag_patent_summary$down, na.rm=T),max(flag_patent_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "flag_patent", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_flag_patent <- p_PSM_flag_patent_fe#+p_flag_patent_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ################## flag_jitsuyo------------
  # PSM TWFE
  es_PSM_flag_jitsuyo_twfe = feols(flag_jitsuyo ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_flag_jitsuyo_twfe <- feols(flag_jitsuyo ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_flag_jitsuyo_twfe<- c(es2_PSM_flag_jitsuyo_twfe$coefficients[1], es2_PSM_flag_jitsuyo_twfe$se[1])
  
  # PSM sa20
  es_PSM_flag_jitsuyo_sa20 = feols(flag_jitsuyo ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_flag_jitsuyo_sa20 <- aggregate(es_PSM_flag_jitsuyo_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_flag_jitsuyo_fe <- ggiplot(list('S&A(2020)' = es_PSM_flag_jitsuyo_sa20,'TWFE' = es_PSM_flag_jitsuyo_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + 
    labs(title = "flag_jitsuyo",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # flag_jitsuyo_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_flag_jitsuyo_twfe[1], att_PSM_flag_jitsuyo_sa20[1]),
  #                                     up = c(att_PSM_flag_jitsuyo_twfe[1] + 1.645*att_PSM_flag_jitsuyo_twfe[2],att_PSM_flag_jitsuyo_sa20[1] + 1.645*att_PSM_flag_jitsuyo_sa20[2]),
  #                                     down = c(att_PSM_flag_jitsuyo_twfe[1] - 1.645*att_PSM_flag_jitsuyo_twfe[2],att_PSM_flag_jitsuyo_sa20[1] - 1.645*att_PSM_flag_jitsuyo_sa20[2]),
  #                                     up95 = c(att_PSM_flag_jitsuyo_twfe[1] + 1.960*att_PSM_flag_jitsuyo_twfe[2], att_PSM_flag_jitsuyo_sa20[1] + 1.960*att_PSM_flag_jitsuyo_sa20[2]),
  #                                     down95 = c(att_PSM_flag_jitsuyo_twfe[1] - 1.960*att_PSM_flag_jitsuyo_twfe[2],att_PSM_flag_jitsuyo_sa20[1] - 1.960*att_PSM_flag_jitsuyo_sa20[2])
  # )
  # 
  # p_flag_jitsuyo_summary <- ggplot(flag_jitsuyo_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(flag_jitsuyo_summary$down, na.rm=T),max(flag_jitsuyo_summary$up, na.rm=T)), max(-min(flag_jitsuyo_summary$down, na.rm=T),max(flag_jitsuyo_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "flag_jitsuyo", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_flag_jitsuyo <- p_PSM_flag_jitsuyo_fe#+p_flag_jitsuyo_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ################## flag_isho------------
  # PSM TWFE
  es_PSM_flag_isho_twfe = feols(flag_isho ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_flag_isho_twfe <- feols(flag_isho ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_flag_isho_twfe<- c(es2_PSM_flag_isho_twfe$coefficients[1], es2_PSM_flag_isho_twfe$se[1])
  
  # PSM sa20
  es_PSM_flag_isho_sa20 = feols(flag_isho ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_flag_isho_sa20 <- aggregate(es_PSM_flag_isho_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_flag_isho_fe <- ggiplot(list('S&A(2020)' = es_PSM_flag_isho_sa20,'TWFE' = es_PSM_flag_isho_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + 
    labs(title = "flag_isho",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # flag_isho_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_flag_isho_twfe[1], att_PSM_flag_isho_sa20[1]),
  #                                     up = c(att_PSM_flag_isho_twfe[1] + 1.645*att_PSM_flag_isho_twfe[2],att_PSM_flag_isho_sa20[1] + 1.645*att_PSM_flag_isho_sa20[2]),
  #                                     down = c(att_PSM_flag_isho_twfe[1] - 1.645*att_PSM_flag_isho_twfe[2],att_PSM_flag_isho_sa20[1] - 1.645*att_PSM_flag_isho_sa20[2]),
  #                                     up95 = c(att_PSM_flag_isho_twfe[1] + 1.960*att_PSM_flag_isho_twfe[2], att_PSM_flag_isho_sa20[1] + 1.960*att_PSM_flag_isho_sa20[2]),
  #                                     down95 = c(att_PSM_flag_isho_twfe[1] - 1.960*att_PSM_flag_isho_twfe[2],att_PSM_flag_isho_sa20[1] - 1.960*att_PSM_flag_isho_sa20[2])
  # )
  # 
  # p_flag_isho_summary <- ggplot(flag_isho_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(flag_isho_summary$down, na.rm=T),max(flag_isho_summary$up, na.rm=T)), max(-min(flag_isho_summary$down, na.rm=T),max(flag_isho_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "flag_isho", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_flag_isho <- p_PSM_flag_isho_fe#+p_flag_isho_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ################## log_patent------------
  # PSM TWFE
  es_PSM_log_patent_twfe = feols(log_patent ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_log_patent_twfe <- feols(log_patent ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_patent_twfe<- c(es2_PSM_log_patent_twfe$coefficients[1], es2_PSM_log_patent_twfe$se[1])
  
  # PSM sa20
  es_PSM_log_patent_sa20 = feols(log_patent ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_patent_sa20 <- aggregate(es_PSM_log_patent_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_log_patent_fe <- ggiplot(list('S&A(2020)' = es_PSM_log_patent_sa20,'TWFE' = es_PSM_log_patent_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_patent",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # log_patent_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_log_patent_twfe[1], att_PSM_log_patent_sa20[1]),
  #                                     up = c(att_PSM_log_patent_twfe[1] + 1.645*att_PSM_log_patent_twfe[2],att_PSM_log_patent_sa20[1] + 1.645*att_PSM_log_patent_sa20[2]),
  #                                     down = c(att_PSM_log_patent_twfe[1] - 1.645*att_PSM_log_patent_twfe[2],att_PSM_log_patent_sa20[1] - 1.645*att_PSM_log_patent_sa20[2]),
  #                                     up95 = c(att_PSM_log_patent_twfe[1] + 1.960*att_PSM_log_patent_twfe[2], att_PSM_log_patent_sa20[1] + 1.960*att_PSM_log_patent_sa20[2]),
  #                                     down95 = c(att_PSM_log_patent_twfe[1] - 1.960*att_PSM_log_patent_twfe[2],att_PSM_log_patent_sa20[1] - 1.960*att_PSM_log_patent_sa20[2])
  # )
  # 
  # p_log_patent_summary <- ggplot(log_patent_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(log_patent_summary$down, na.rm=T),max(log_patent_summary$up, na.rm=T)), max(-min(log_patent_summary$down, na.rm=T),max(log_patent_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "log_patent", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_log_patent <- p_PSM_log_patent_fe#+p_log_patent_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ################## log_jitsuyo------------
  # PSM TWFE
  es_PSM_log_jitsuyo_twfe = feols(log_jitsuyo ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_log_jitsuyo_twfe <- feols(log_jitsuyo ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_jitsuyo_twfe<- c(es2_PSM_log_jitsuyo_twfe$coefficients[1], es2_PSM_log_jitsuyo_twfe$se[1])
  
  # PSM sa20
  es_PSM_log_jitsuyo_sa20 = feols(log_jitsuyo ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_jitsuyo_sa20 <- aggregate(es_PSM_log_jitsuyo_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_log_jitsuyo_fe <- ggiplot(list('S&A(2020)' = es_PSM_log_jitsuyo_sa20,'TWFE' = es_PSM_log_jitsuyo_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_jitsuyo",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # log_jitsuyo_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_log_jitsuyo_twfe[1], att_PSM_log_jitsuyo_sa20[1]),
  #                                     up = c(att_PSM_log_jitsuyo_twfe[1] + 1.645*att_PSM_log_jitsuyo_twfe[2],att_PSM_log_jitsuyo_sa20[1] + 1.645*att_PSM_log_jitsuyo_sa20[2]),
  #                                     down = c(att_PSM_log_jitsuyo_twfe[1] - 1.645*att_PSM_log_jitsuyo_twfe[2],att_PSM_log_jitsuyo_sa20[1] - 1.645*att_PSM_log_jitsuyo_sa20[2]),
  #                                     up95 = c(att_PSM_log_jitsuyo_twfe[1] + 1.960*att_PSM_log_jitsuyo_twfe[2], att_PSM_log_jitsuyo_sa20[1] + 1.960*att_PSM_log_jitsuyo_sa20[2]),
  #                                     down95 = c(att_PSM_log_jitsuyo_twfe[1] - 1.960*att_PSM_log_jitsuyo_twfe[2],att_PSM_log_jitsuyo_sa20[1] - 1.960*att_PSM_log_jitsuyo_sa20[2])
  # )
  # 
  # p_log_jitsuyo_summary <- ggplot(log_jitsuyo_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(log_jitsuyo_summary$down, na.rm=T),max(log_jitsuyo_summary$up, na.rm=T)), max(-min(log_jitsuyo_summary$down, na.rm=T),max(log_jitsuyo_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "log_jitsuyo", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_log_jitsuyo <- p_PSM_log_jitsuyo_fe#+p_log_jitsuyo_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ################## log_isho------------
  # PSM TWFE
  es_PSM_log_isho_twfe = feols(log_isho ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_log_isho_twfe <- feols(log_isho ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_isho_twfe<- c(es2_PSM_log_isho_twfe$coefficients[1], es2_PSM_log_isho_twfe$se[1])
  
  # PSM sa20
  es_PSM_log_isho_sa20 = feols(log_isho ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_isho_sa20 <- aggregate(es_PSM_log_isho_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_log_isho_fe <- ggiplot(list('S&A(2020)' = es_PSM_log_isho_sa20,'TWFE' = es_PSM_log_isho_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_isho",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # log_isho_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_log_isho_twfe[1], att_PSM_log_isho_sa20[1]),
  #                                     up = c(att_PSM_log_isho_twfe[1] + 1.645*att_PSM_log_isho_twfe[2],att_PSM_log_isho_sa20[1] + 1.645*att_PSM_log_isho_sa20[2]),
  #                                     down = c(att_PSM_log_isho_twfe[1] - 1.645*att_PSM_log_isho_twfe[2],att_PSM_log_isho_sa20[1] - 1.645*att_PSM_log_isho_sa20[2]),
  #                                     up95 = c(att_PSM_log_isho_twfe[1] + 1.960*att_PSM_log_isho_twfe[2], att_PSM_log_isho_sa20[1] + 1.960*att_PSM_log_isho_sa20[2]),
  #                                     down95 = c(att_PSM_log_isho_twfe[1] - 1.960*att_PSM_log_isho_twfe[2],att_PSM_log_isho_sa20[1] - 1.960*att_PSM_log_isho_sa20[2])
  # )
  # 
  # p_log_isho_summary <- ggplot(log_isho_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(log_isho_summary$down, na.rm=T),max(log_isho_summary$up, na.rm=T)), max(-min(log_isho_summary$down, na.rm=T),max(log_isho_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "log_isho", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_log_isho <- p_PSM_log_isho_fe#+p_log_isho_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ## output_IP-----------
  pa_IP_sum <- pa_flag_patent/pa_flag_jitsuyo/pa_flag_isho/pa_log_patent/pa_log_jitsuyo/pa_log_isho + plot_annotation(
    title = "Effect on the Intellectual Property",
    subtitle = target,
    #caption = caption_main
  ) &
    theme(text = element_text(family="Meiryo UI"))
  
  filename_pdf <- paste("03_robustness_output/", target,"_26_IP_TWFE_", count_intervention, ".pdf", sep="")
  ggsave(filename_pdf,pa_IP_sum, 
         width=12, height =48, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("03_robustness_output/", target,"_26_IP_TWFE_", count_intervention, ".png", sep="")
  ggsave(filename_png,pa_IP_sum, 
         width=12, height =48, units = "cm", dpi=200) 
  
  # write.xlsx(list("flag_patent"=flag_patent_summary,"flag_jitsuyo"=flag_jitsuyo_summary,"flag_isho"=flag_isho_summary,"log_patent"=log_patent_summary,"log_jitsuyo"=log_jitsuyo_summary,"log_isho"=log_isho_summary), paste("03_robustness_output/",target,"_26IP_TWFE_table.xlsx", sep=""))
  # write.xlsx(list("flag_patent"=flag_patent_summary_appendix,"flag_jitsuyo"=flag_jitsuyo_summary_appendix,"flag_isho"=flag_isho_summary_appendix,"log_patent"=log_patent_summary_appendix,"log_jitsuyo"=log_jitsuyo_summary_appendix,"log_isho"=log_isho_summary_appendix), paste("03_robustness_output/",target,"_26IP_TWFE_table_appendix.xlsx"))
}
robustness_TWFE_investment <- function(target){
  ################## flag_investment_affiliate_domestic------------
  # PSM TWFE
  es_PSM_flag_investment_affiliate_domestic_twfe = feols(flag_investment_affiliate_domestic ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_flag_investment_affiliate_domestic_twfe <- feols(flag_investment_affiliate_domestic ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_flag_investment_affiliate_domestic_twfe<- c(es2_PSM_flag_investment_affiliate_domestic_twfe$coefficients[1], es2_PSM_flag_investment_affiliate_domestic_twfe$se[1])
  
  # PSM sa20
  es_PSM_flag_investment_affiliate_domestic_sa20 = feols(flag_investment_affiliate_domestic ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_flag_investment_affiliate_domestic_sa20 <- aggregate(es_PSM_flag_investment_affiliate_domestic_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_flag_investment_affiliate_domestic_fe <- ggiplot(list('S&A(2020)' = es_PSM_flag_investment_affiliate_domestic_sa20,'TWFE' = es_PSM_flag_investment_affiliate_domestic_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + 
    labs(title = "flag_investment_affiliate_domestic",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # flag_investment_affiliate_domestic_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_flag_investment_affiliate_domestic_twfe[1], att_PSM_flag_investment_affiliate_domestic_sa20[1]),
  #                                     up = c(att_PSM_flag_investment_affiliate_domestic_twfe[1] + 1.645*att_PSM_flag_investment_affiliate_domestic_twfe[2],att_PSM_flag_investment_affiliate_domestic_sa20[1] + 1.645*att_PSM_flag_investment_affiliate_domestic_sa20[2]),
  #                                     down = c(att_PSM_flag_investment_affiliate_domestic_twfe[1] - 1.645*att_PSM_flag_investment_affiliate_domestic_twfe[2],att_PSM_flag_investment_affiliate_domestic_sa20[1] - 1.645*att_PSM_flag_investment_affiliate_domestic_sa20[2]),
  #                                     up95 = c(att_PSM_flag_investment_affiliate_domestic_twfe[1] + 1.960*att_PSM_flag_investment_affiliate_domestic_twfe[2], att_PSM_flag_investment_affiliate_domestic_sa20[1] + 1.960*att_PSM_flag_investment_affiliate_domestic_sa20[2]),
  #                                     down95 = c(att_PSM_flag_investment_affiliate_domestic_twfe[1] - 1.960*att_PSM_flag_investment_affiliate_domestic_twfe[2],att_PSM_flag_investment_affiliate_domestic_sa20[1] - 1.960*att_PSM_flag_investment_affiliate_domestic_sa20[2])
  # )
  # 
  # p_flag_investment_affiliate_domestic_summary <- ggplot(flag_investment_affiliate_domestic_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(flag_investment_affiliate_domestic_summary$down, na.rm=T),max(flag_investment_affiliate_domestic_summary$up, na.rm=T)), max(-min(flag_investment_affiliate_domestic_summary$down, na.rm=T),max(flag_investment_affiliate_domestic_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "flag_investment_affiliate_domestic", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_flag_investment_affiliate_domestic <- p_PSM_flag_investment_affiliate_domestic_fe#+p_flag_investment_affiliate_domestic_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ################## flag_investment_affiliate_overseas------------
  # PSM TWFE
  es_PSM_flag_investment_affiliate_overseas_twfe = feols(flag_investment_affiliate_overseas ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_flag_investment_affiliate_overseas_twfe <- feols(flag_investment_affiliate_overseas ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_flag_investment_affiliate_overseas_twfe<- c(es2_PSM_flag_investment_affiliate_overseas_twfe$coefficients[1], es2_PSM_flag_investment_affiliate_overseas_twfe$se[1])
  
  # PSM sa20
  es_PSM_flag_investment_affiliate_overseas_sa20 = feols(flag_investment_affiliate_overseas ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_flag_investment_affiliate_overseas_sa20 <- aggregate(es_PSM_flag_investment_affiliate_overseas_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_flag_investment_affiliate_overseas_fe <- ggiplot(list('S&A(2020)' = es_PSM_flag_investment_affiliate_overseas_sa20,'TWFE' = es_PSM_flag_investment_affiliate_overseas_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + 
    labs(title = "flag_investment_affiliate_overseas",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # flag_investment_affiliate_overseas_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_flag_investment_affiliate_overseas_twfe[1], att_PSM_flag_investment_affiliate_overseas_sa20[1]),
  #                                     up = c(att_PSM_flag_investment_affiliate_overseas_twfe[1] + 1.645*att_PSM_flag_investment_affiliate_overseas_twfe[2],att_PSM_flag_investment_affiliate_overseas_sa20[1] + 1.645*att_PSM_flag_investment_affiliate_overseas_sa20[2]),
  #                                     down = c(att_PSM_flag_investment_affiliate_overseas_twfe[1] - 1.645*att_PSM_flag_investment_affiliate_overseas_twfe[2],att_PSM_flag_investment_affiliate_overseas_sa20[1] - 1.645*att_PSM_flag_investment_affiliate_overseas_sa20[2]),
  #                                     up95 = c(att_PSM_flag_investment_affiliate_overseas_twfe[1] + 1.960*att_PSM_flag_investment_affiliate_overseas_twfe[2], att_PSM_flag_investment_affiliate_overseas_sa20[1] + 1.960*att_PSM_flag_investment_affiliate_overseas_sa20[2]),
  #                                     down95 = c(att_PSM_flag_investment_affiliate_overseas_twfe[1] - 1.960*att_PSM_flag_investment_affiliate_overseas_twfe[2],att_PSM_flag_investment_affiliate_overseas_sa20[1] - 1.960*att_PSM_flag_investment_affiliate_overseas_sa20[2])
  # )
  # 
  # p_flag_investment_affiliate_overseas_summary <- ggplot(flag_investment_affiliate_overseas_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(flag_investment_affiliate_overseas_summary$down, na.rm=T),max(flag_investment_affiliate_overseas_summary$up, na.rm=T)), max(-min(flag_investment_affiliate_overseas_summary$down, na.rm=T),max(flag_investment_affiliate_overseas_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "flag_investment_affiliate_overseas", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_flag_investment_affiliate_overseas <- p_PSM_flag_investment_affiliate_overseas_fe#+p_flag_investment_affiliate_overseas_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ################## flag_dividend------------
  # PSM TWFE
  es_PSM_flag_dividend_twfe = feols(flag_dividend ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_flag_dividend_twfe <- feols(flag_dividend ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_flag_dividend_twfe<- c(es2_PSM_flag_dividend_twfe$coefficients[1], es2_PSM_flag_dividend_twfe$se[1])
  
  # PSM sa20
  es_PSM_flag_dividend_sa20 = feols(flag_dividend ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_flag_dividend_sa20 <- aggregate(es_PSM_flag_dividend_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_flag_dividend_fe <- ggiplot(list('S&A(2020)' = es_PSM_flag_dividend_sa20,'TWFE' = es_PSM_flag_dividend_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + 
    labs(title = "flag_dividend",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # flag_dividend_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_flag_dividend_twfe[1], att_PSM_flag_dividend_sa20[1]),
  #                                     up = c(att_PSM_flag_dividend_twfe[1] + 1.645*att_PSM_flag_dividend_twfe[2],att_PSM_flag_dividend_sa20[1] + 1.645*att_PSM_flag_dividend_sa20[2]),
  #                                     down = c(att_PSM_flag_dividend_twfe[1] - 1.645*att_PSM_flag_dividend_twfe[2],att_PSM_flag_dividend_sa20[1] - 1.645*att_PSM_flag_dividend_sa20[2]),
  #                                     up95 = c(att_PSM_flag_dividend_twfe[1] + 1.960*att_PSM_flag_dividend_twfe[2], att_PSM_flag_dividend_sa20[1] + 1.960*att_PSM_flag_dividend_sa20[2]),
  #                                     down95 = c(att_PSM_flag_dividend_twfe[1] - 1.960*att_PSM_flag_dividend_twfe[2],att_PSM_flag_dividend_sa20[1] - 1.960*att_PSM_flag_dividend_sa20[2])
  # )
  # 
  # p_flag_dividend_summary <- ggplot(flag_dividend_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(flag_dividend_summary$down, na.rm=T),max(flag_dividend_summary$up, na.rm=T)), max(-min(flag_dividend_summary$down, na.rm=T),max(flag_dividend_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "flag_dividend", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_flag_dividend <- p_PSM_flag_dividend_fe#+p_flag_dividend_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ################## log_investment_affiliate_domestic------------
  # PSM TWFE
  es_PSM_log_investment_affiliate_domestic_twfe = feols(log_investment_affiliate_domestic ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_log_investment_affiliate_domestic_twfe <- feols(log_investment_affiliate_domestic ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_investment_affiliate_domestic_twfe<- c(es2_PSM_log_investment_affiliate_domestic_twfe$coefficients[1], es2_PSM_log_investment_affiliate_domestic_twfe$se[1])
  
  # PSM sa20
  es_PSM_log_investment_affiliate_domestic_sa20 = feols(log_investment_affiliate_domestic ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_investment_affiliate_domestic_sa20 <- aggregate(es_PSM_log_investment_affiliate_domestic_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_log_investment_affiliate_domestic_fe <- ggiplot(list('S&A(2020)' = es_PSM_log_investment_affiliate_domestic_sa20,'TWFE' = es_PSM_log_investment_affiliate_domestic_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_investment_affiliate_domestic",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # log_investment_affiliate_domestic_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_log_investment_affiliate_domestic_twfe[1], att_PSM_log_investment_affiliate_domestic_sa20[1]),
  #                                     up = c(att_PSM_log_investment_affiliate_domestic_twfe[1] + 1.645*att_PSM_log_investment_affiliate_domestic_twfe[2],att_PSM_log_investment_affiliate_domestic_sa20[1] + 1.645*att_PSM_log_investment_affiliate_domestic_sa20[2]),
  #                                     down = c(att_PSM_log_investment_affiliate_domestic_twfe[1] - 1.645*att_PSM_log_investment_affiliate_domestic_twfe[2],att_PSM_log_investment_affiliate_domestic_sa20[1] - 1.645*att_PSM_log_investment_affiliate_domestic_sa20[2]),
  #                                     up95 = c(att_PSM_log_investment_affiliate_domestic_twfe[1] + 1.960*att_PSM_log_investment_affiliate_domestic_twfe[2], att_PSM_log_investment_affiliate_domestic_sa20[1] + 1.960*att_PSM_log_investment_affiliate_domestic_sa20[2]),
  #                                     down95 = c(att_PSM_log_investment_affiliate_domestic_twfe[1] - 1.960*att_PSM_log_investment_affiliate_domestic_twfe[2],att_PSM_log_investment_affiliate_domestic_sa20[1] - 1.960*att_PSM_log_investment_affiliate_domestic_sa20[2])
  # )
  # 
  # p_log_investment_affiliate_domestic_summary <- ggplot(log_investment_affiliate_domestic_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(log_investment_affiliate_domestic_summary$down, na.rm=T),max(log_investment_affiliate_domestic_summary$up, na.rm=T)), max(-min(log_investment_affiliate_domestic_summary$down, na.rm=T),max(log_investment_affiliate_domestic_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "log_investment_affiliate_domestic", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_log_investment_affiliate_domestic <- p_PSM_log_investment_affiliate_domestic_fe#+p_log_investment_affiliate_domestic_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ################## log_investment_affiliate_overseas------------
  # PSM TWFE
  es_PSM_log_investment_affiliate_overseas_twfe = feols(log_investment_affiliate_overseas ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_log_investment_affiliate_overseas_twfe <- feols(log_investment_affiliate_overseas ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_investment_affiliate_overseas_twfe<- c(es2_PSM_log_investment_affiliate_overseas_twfe$coefficients[1], es2_PSM_log_investment_affiliate_overseas_twfe$se[1])
  
  # PSM sa20
  es_PSM_log_investment_affiliate_overseas_sa20 = feols(log_investment_affiliate_overseas ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_investment_affiliate_overseas_sa20 <- aggregate(es_PSM_log_investment_affiliate_overseas_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_log_investment_affiliate_overseas_fe <- ggiplot(list('S&A(2020)' = es_PSM_log_investment_affiliate_overseas_sa20,'TWFE' = es_PSM_log_investment_affiliate_overseas_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_investment_affiliate_overseas",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # log_investment_affiliate_overseas_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_log_investment_affiliate_overseas_twfe[1], att_PSM_log_investment_affiliate_overseas_sa20[1]),
  #                                     up = c(att_PSM_log_investment_affiliate_overseas_twfe[1] + 1.645*att_PSM_log_investment_affiliate_overseas_twfe[2],att_PSM_log_investment_affiliate_overseas_sa20[1] + 1.645*att_PSM_log_investment_affiliate_overseas_sa20[2]),
  #                                     down = c(att_PSM_log_investment_affiliate_overseas_twfe[1] - 1.645*att_PSM_log_investment_affiliate_overseas_twfe[2],att_PSM_log_investment_affiliate_overseas_sa20[1] - 1.645*att_PSM_log_investment_affiliate_overseas_sa20[2]),
  #                                     up95 = c(att_PSM_log_investment_affiliate_overseas_twfe[1] + 1.960*att_PSM_log_investment_affiliate_overseas_twfe[2], att_PSM_log_investment_affiliate_overseas_sa20[1] + 1.960*att_PSM_log_investment_affiliate_overseas_sa20[2]),
  #                                     down95 = c(att_PSM_log_investment_affiliate_overseas_twfe[1] - 1.960*att_PSM_log_investment_affiliate_overseas_twfe[2],att_PSM_log_investment_affiliate_overseas_sa20[1] - 1.960*att_PSM_log_investment_affiliate_overseas_sa20[2])
  # )
  # 
  # p_log_investment_affiliate_overseas_summary <- ggplot(log_investment_affiliate_overseas_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(log_investment_affiliate_overseas_summary$down, na.rm=T),max(log_investment_affiliate_overseas_summary$up, na.rm=T)), max(-min(log_investment_affiliate_overseas_summary$down, na.rm=T),max(log_investment_affiliate_overseas_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "log_investment_affiliate_overseas", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_log_investment_affiliate_overseas <- p_PSM_log_investment_affiliate_overseas_fe#+p_log_investment_affiliate_overseas_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ################## log_dividend------------
  # PSM TWFE
  es_PSM_log_dividend_twfe = feols(log_dividend ~  i(year_to_treat,treat, ref = c(-2, -1000)) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # es2_PSM_log_dividend_twfe <- feols(log_dividend ~  treated | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_dividend_twfe<- c(es2_PSM_log_dividend_twfe$coefficients[1], es2_PSM_log_dividend_twfe$se[1])
  
  # PSM sa20
  es_PSM_log_dividend_sa20 = feols(log_dividend ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  # att_PSM_log_dividend_sa20 <- aggregate(es_PSM_log_dividend_sa20, "att", full = FALSE, use_weights = TRUE)
  
  
  p_PSM_log_dividend_fe <- ggiplot(list('S&A(2020)' = es_PSM_log_dividend_sa20,'TWFE' = es_PSM_log_dividend_twfe),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_dividend",subtitle = "TWFE + S&A(2020) - PSM, 95% CI") +
    theme_light()
  
  ## create summary graph
  # method <- c("TWFE","S&A(2020)")
  # log_dividend_summary <- data.frame(method = method,
  #                                     estimate = c(att_PSM_log_dividend_twfe[1], att_PSM_log_dividend_sa20[1]),
  #                                     up = c(att_PSM_log_dividend_twfe[1] + 1.645*att_PSM_log_dividend_twfe[2],att_PSM_log_dividend_sa20[1] + 1.645*att_PSM_log_dividend_sa20[2]),
  #                                     down = c(att_PSM_log_dividend_twfe[1] - 1.645*att_PSM_log_dividend_twfe[2],att_PSM_log_dividend_sa20[1] - 1.645*att_PSM_log_dividend_sa20[2]),
  #                                     up95 = c(att_PSM_log_dividend_twfe[1] + 1.960*att_PSM_log_dividend_twfe[2], att_PSM_log_dividend_sa20[1] + 1.960*att_PSM_log_dividend_sa20[2]),
  #                                     down95 = c(att_PSM_log_dividend_twfe[1] - 1.960*att_PSM_log_dividend_twfe[2],att_PSM_log_dividend_sa20[1] - 1.960*att_PSM_log_dividend_sa20[2])
  # )
  # 
  # p_log_dividend_summary <- ggplot(log_dividend_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
  #   geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
  #   geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
  #   geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white") +
  #   coord_cartesian(ylim = c(-max(-min(log_dividend_summary$down, na.rm=T),max(log_dividend_summary$up, na.rm=T)), max(-min(log_dividend_summary$down, na.rm=T),max(log_dividend_summary$up, na.rm=T))))+
  #   xlab("Method") +
  #   labs(title = "log_dividend", subtitle ="Aggregated ATT, 90 & 95% CI")+
  #   theme_light() +
  #   scale_color_manual(values = c("TWFE" = "#F8766D",
  #                                 "S&A(2020)"="#00BA38")) +
  #   geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') + 
  #   geom_hline(yintercept=0)  + theme(legend.position="none")
  
  
  ## create grid arrange
  pa_log_dividend <- p_PSM_log_dividend_fe#+p_log_dividend_summary# + plot_layout(ncol = 4, widths = c(1,1,1,1))
  
  ## output_investment-----------
  pa_investment_sum <- pa_flag_investment_affiliate_domestic/pa_flag_investment_affiliate_overseas/pa_flag_dividend/pa_log_investment_affiliate_domestic/pa_log_investment_affiliate_overseas/pa_log_dividend + plot_annotation(
    title = "Effect on the Investment",
    subtitle = target,
    #caption = caption_main
  ) &
    theme(text = element_text(family="Meiryo UI"))
  
  filename_pdf <- paste("03_robustness_output/", target,"_27_investment_TWFE_", count_intervention, ".pdf", sep="")
  ggsave(filename_pdf,pa_investment_sum, 
         width=12, height =48, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("03_robustness_output/", target,"_27_investment_TWFE_", count_intervention, ".png", sep="")
  ggsave(filename_png,pa_investment_sum, 
         width=12, height =48, units = "cm", dpi=200) 
  
  # write.xlsx(list("flag_investment_affiliate_dm"=flag_investment_affiliate_domestic_summary,"flag_investment_affiliate_ov"=flag_investment_affiliate_overseas_summary,"flag_dividend"=flag_dividend_summary,"log_investment_affiliate_dm"=log_investment_affiliate_domestic_summary,"log_investment_affiliate_ov"=log_investment_affiliate_overseas_summary,"log_dividend"=log_dividend_summary), paste("03_robustness_output/",target,"_27investment_TWFE_table.xlsx", sep=""))
  # write.xlsx(list("flag_investment_affiliate_dm"=flag_investment_affiliate_domestic_summary_appendix,"flag_investment_affiliate_ov"=flag_investment_affiliate_overseas_summary_appendix,"flag_dividend"=flag_dividend_summary_appendix,"log_investment_affiliate_dm"=log_investment_affiliate_domestic_summary_appendix,"log_investment_affiliate_ov"=log_investment_affiliate_overseas_summary_appendix,"log_dividend"=log_dividend_summary_appendix), paste("03_robustness_output/",target,"_27investment_TWFE_table_appendix.xlsx"))
}




## conduct TWFE------------

print(target)
try(robustness_TWFE_asset(target))
try(robustness_TWFE_employment(target))
try(robustness_TWFE_business(target))
try(robustness_TWFE_trade(target))
try(robustness_TWFE_trainingRD(target))
try(robustness_TWFE_IP(target))
try(robustness_TWFE_investment(target))

# 30 define functions  for robustness check- Data trimmed ----------

dfm_kikatsu2_ipw_trimmed <- dfm_kikatsu2_ipw %>%
  filter(year >= 2012 & year <=2019) ## 年を限定するために設定　補遺用

robustness_trimmed_asset <- function(target){
  ################## log_sum_asset------------
  # ipw
  es_PSM_log_sum_asset_sa20 = feols(log_sum_asset ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_log_sum_asset_sa20 <- aggregate(es_PSM_log_sum_asset_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_sum_asset_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_sum_asset_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_sum_asset", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_sum_asset <- att_gt(yname = "log_sum_asset",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                             xformla = formula_cs, 
                             est_method = "ipw",base_period="universal",alp=0.05,
                             data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                             #print_details=FALSE,
                             anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_sum_asset_smry <- aggte(cs_log_sum_asset, type = "dynamic",na.rm = TRUE)
  
  p_log_sum_asset_cs <- ggdid(cs_log_sum_asset_smry, title = "log_sum_asset")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_log_sum_asset_cs <- aggte(cs_log_sum_asset, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  log_sum_asset_summary <- data.frame(method = method,
                                      estimate = c(att_PSM_log_sum_asset_sa20[1], a_log_sum_asset_cs$overall.att#,a_log_sum_asset_Ssynth$att_estimate
                                      ),
                                      se = c(att_PSM_log_sum_asset_sa20[2],a_log_sum_asset_cs$overall.se#,se_ssynth
                                      ),
                                      up = c(att_PSM_log_sum_asset_sa20[1] + 1.645*att_PSM_log_sum_asset_sa20[2],a_log_sum_asset_cs$overall.att + 1.645*a_log_sum_asset_cs$overall.se#, a_log_sum_asset_Ssynth$att_estimate + 1.645 * se_ssynth
                                      ),
                                      down = c(att_PSM_log_sum_asset_sa20[1] - 1.645*att_PSM_log_sum_asset_sa20[2],a_log_sum_asset_cs$overall.att - 1.645*a_log_sum_asset_cs$overall.se#, a_log_sum_asset_Ssynth$att_estimate - 1.645 * se_ssynth
                                      ),
                                      up95 = c(att_PSM_log_sum_asset_sa20[1] + 1.960*att_PSM_log_sum_asset_sa20[2],a_log_sum_asset_cs$overall.att + 1.960*a_log_sum_asset_cs$overall.se#,  a_log_sum_asset_Ssynth$att_estimate + 1.960 * se_ssynth
                                      ),
                                      down95 = c(att_PSM_log_sum_asset_sa20[1] - 1.960*att_PSM_log_sum_asset_sa20[2],a_log_sum_asset_cs$overall.att - 1.960*a_log_sum_asset_cs$overall.se#,  a_log_sum_asset_Ssynth$att_estimate - 1.960 * se_ssynth
                                      ),
                                      up99 = c(att_PSM_log_sum_asset_sa20[1] + 2.5758*att_PSM_log_sum_asset_sa20[2],a_log_sum_asset_cs$overall.att + 2.5758*a_log_sum_asset_cs$overall.se#,  a_log_sum_asset_Ssynth$att_estimate + 2.5758 * se_ssynth
                                      ),
                                      down99 = c(att_PSM_log_sum_asset_sa20[1] - 2.5758*att_PSM_log_sum_asset_sa20[2],a_log_sum_asset_cs$overall.att - 2.5758*a_log_sum_asset_cs$overall.se#,  a_log_sum_asset_Ssynth$att_estimate - 2.5758 * se_ssynth
                                      )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  log_sum_asset_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_log_sum_asset_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_log_sum_asset_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_log_sum_asset_sa20$coeftable[,1],
           sa_se =es_PSM_log_sum_asset_sa20$coeftable[,2],
           sa_t =es_PSM_log_sum_asset_sa20$coeftable[,3],
           sa_p =es_PSM_log_sum_asset_sa20$coeftable[,4])
  log_sum_asset_summary_appendix_cs <- data.frame(year = cs_log_sum_asset_smry$egt,
                                                  cs =  cs_log_sum_asset_smry$att.egt,
                                                  cs_se = cs_log_sum_asset_smry$se.egt,
                                                  cs_cband_lower = cs_log_sum_asset_smry$att.egt - cs_log_sum_asset_smry$crit.val.egt*cs_log_sum_asset_smry$se.egt,
                                                  cs_cband_upper = cs_log_sum_asset_smry$att.egt + cs_log_sum_asset_smry$crit.val.egt*cs_log_sum_asset_smry$se.egt)
  log_sum_asset_summary_appendix <- right_join(log_sum_asset_summary_appendix_sa,log_sum_asset_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_log_sum_asset_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- log_sum_asset_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(log_sum_asset_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- log_sum_asset_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(log_sum_asset_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  log_sum_asset_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  log_sum_asset_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(log_sum_asset_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(log_sum_asset_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  log_sum_asset_summary <- cbind(log_sum_asset_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(log_sum_asset_summary$rob)
  
  p_log_sum_asset_summary <- ggplot(log_sum_asset_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_sum_asset_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_sum_asset_summary$pretrend[1]) +
    geom_rect(data=log_sum_asset_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_sum_asset_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_sum_asset_summary$down, na.rm=T),max(log_sum_asset_summary$up, na.rm=T)), max(-min(log_sum_asset_summary$down, na.rm=T),max(log_sum_asset_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_sum_asset <- p_PSM_log_sum_asset_sa20+p_log_sum_asset_cs+p_log_sum_asset_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_log_sum_asset)
  ################## log_tangible_asset------------
  # ipw
  es_PSM_log_tangible_asset_sa20 = feols(log_tangible_asset ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_log_tangible_asset_sa20 <- aggregate(es_PSM_log_tangible_asset_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_tangible_asset_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_tangible_asset_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_tangible_asset", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_tangible_asset <- att_gt(yname = "log_tangible_asset",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                                  xformla = formula_cs, 
                                  est_method = "ipw",base_period="universal",alp=0.05,
                                  data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                                  #print_details=FALSE,
                                  anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_tangible_asset_smry <- aggte(cs_log_tangible_asset, type = "dynamic",na.rm = TRUE)
  
  p_log_tangible_asset_cs <- ggdid(cs_log_tangible_asset_smry, title = "log_tangible_asset")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_log_tangible_asset_cs <- aggte(cs_log_tangible_asset, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  log_tangible_asset_summary <- data.frame(method = method,
                                           estimate = c(att_PSM_log_tangible_asset_sa20[1], a_log_tangible_asset_cs$overall.att#,a_log_tangible_asset_Ssynth$att_estimate
                                           ),
                                           se = c(att_PSM_log_tangible_asset_sa20[2],a_log_tangible_asset_cs$overall.se#,se_ssynth
                                           ),
                                           up = c(att_PSM_log_tangible_asset_sa20[1] + 1.645*att_PSM_log_tangible_asset_sa20[2],a_log_tangible_asset_cs$overall.att + 1.645*a_log_tangible_asset_cs$overall.se#, a_log_tangible_asset_Ssynth$att_estimate + 1.645 * se_ssynth
                                           ),
                                           down = c(att_PSM_log_tangible_asset_sa20[1] - 1.645*att_PSM_log_tangible_asset_sa20[2],a_log_tangible_asset_cs$overall.att - 1.645*a_log_tangible_asset_cs$overall.se#, a_log_tangible_asset_Ssynth$att_estimate - 1.645 * se_ssynth
                                           ),
                                           up95 = c(att_PSM_log_tangible_asset_sa20[1] + 1.960*att_PSM_log_tangible_asset_sa20[2],a_log_tangible_asset_cs$overall.att + 1.960*a_log_tangible_asset_cs$overall.se#,  a_log_tangible_asset_Ssynth$att_estimate + 1.960 * se_ssynth
                                           ),
                                           down95 = c(att_PSM_log_tangible_asset_sa20[1] - 1.960*att_PSM_log_tangible_asset_sa20[2],a_log_tangible_asset_cs$overall.att - 1.960*a_log_tangible_asset_cs$overall.se#,  a_log_tangible_asset_Ssynth$att_estimate - 1.960 * se_ssynth
                                           ),
                                           up99 = c(att_PSM_log_tangible_asset_sa20[1] + 2.5758*att_PSM_log_tangible_asset_sa20[2],a_log_tangible_asset_cs$overall.att + 2.5758*a_log_tangible_asset_cs$overall.se#,  a_log_tangible_asset_Ssynth$att_estimate + 2.5758 * se_ssynth
                                           ),
                                           down99 = c(att_PSM_log_tangible_asset_sa20[1] - 2.5758*att_PSM_log_tangible_asset_sa20[2],a_log_tangible_asset_cs$overall.att - 2.5758*a_log_tangible_asset_cs$overall.se#,  a_log_tangible_asset_Ssynth$att_estimate - 2.5758 * se_ssynth
                                           )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  log_tangible_asset_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_log_tangible_asset_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_log_tangible_asset_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_log_tangible_asset_sa20$coeftable[,1],
           sa_se =es_PSM_log_tangible_asset_sa20$coeftable[,2],
           sa_t =es_PSM_log_tangible_asset_sa20$coeftable[,3],
           sa_p =es_PSM_log_tangible_asset_sa20$coeftable[,4])
  log_tangible_asset_summary_appendix_cs <- data.frame(year = cs_log_tangible_asset_smry$egt,
                                                       cs =  cs_log_tangible_asset_smry$att.egt,
                                                       cs_se = cs_log_tangible_asset_smry$se.egt,
                                                       cs_cband_lower = cs_log_tangible_asset_smry$att.egt - cs_log_tangible_asset_smry$crit.val.egt*cs_log_tangible_asset_smry$se.egt,
                                                       cs_cband_upper = cs_log_tangible_asset_smry$att.egt + cs_log_tangible_asset_smry$crit.val.egt*cs_log_tangible_asset_smry$se.egt)
  log_tangible_asset_summary_appendix <- right_join(log_tangible_asset_summary_appendix_sa,log_tangible_asset_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_log_tangible_asset_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- log_tangible_asset_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(log_tangible_asset_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- log_tangible_asset_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(log_tangible_asset_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  log_tangible_asset_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  log_tangible_asset_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(log_tangible_asset_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(log_tangible_asset_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  log_tangible_asset_summary <- cbind(log_tangible_asset_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(log_tangible_asset_summary$rob)
  
  p_log_tangible_asset_summary <- ggplot(log_tangible_asset_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_tangible_asset_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_tangible_asset_summary$pretrend[1]) +
    geom_rect(data=log_tangible_asset_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_tangible_asset_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_tangible_asset_summary$down, na.rm=T),max(log_tangible_asset_summary$up, na.rm=T)), max(-min(log_tangible_asset_summary$down, na.rm=T),max(log_tangible_asset_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_tangible_asset <- p_PSM_log_tangible_asset_sa20+p_log_tangible_asset_cs+p_log_tangible_asset_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_log_tangible_asset)
  ################## log_intangible_asset------------
  # ipw
  es_PSM_log_intangible_asset_sa20 = feols(log_intangible_asset ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_log_intangible_asset_sa20 <- aggregate(es_PSM_log_intangible_asset_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_intangible_asset_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_intangible_asset_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_intangible_asset", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_intangible_asset <- att_gt(yname = "log_intangible_asset",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                                    xformla = formula_cs, 
                                    est_method = "ipw",base_period="universal",alp=0.05,
                                    data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                                    #print_details=FALSE,
                                    anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_intangible_asset_smry <- aggte(cs_log_intangible_asset, type = "dynamic",na.rm = TRUE)
  
  p_log_intangible_asset_cs <- ggdid(cs_log_intangible_asset_smry, title = "log_intangible_asset")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_log_intangible_asset_cs <- aggte(cs_log_intangible_asset, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  log_intangible_asset_summary <- data.frame(method = method,
                                             estimate = c(att_PSM_log_intangible_asset_sa20[1], a_log_intangible_asset_cs$overall.att#,a_log_intangible_asset_Ssynth$att_estimate
                                             ),
                                             se = c(att_PSM_log_intangible_asset_sa20[2],a_log_intangible_asset_cs$overall.se#,se_ssynth
                                             ),
                                             up = c(att_PSM_log_intangible_asset_sa20[1] + 1.645*att_PSM_log_intangible_asset_sa20[2],a_log_intangible_asset_cs$overall.att + 1.645*a_log_intangible_asset_cs$overall.se#, a_log_intangible_asset_Ssynth$att_estimate + 1.645 * se_ssynth
                                             ),
                                             down = c(att_PSM_log_intangible_asset_sa20[1] - 1.645*att_PSM_log_intangible_asset_sa20[2],a_log_intangible_asset_cs$overall.att - 1.645*a_log_intangible_asset_cs$overall.se#, a_log_intangible_asset_Ssynth$att_estimate - 1.645 * se_ssynth
                                             ),
                                             up95 = c(att_PSM_log_intangible_asset_sa20[1] + 1.960*att_PSM_log_intangible_asset_sa20[2],a_log_intangible_asset_cs$overall.att + 1.960*a_log_intangible_asset_cs$overall.se#,  a_log_intangible_asset_Ssynth$att_estimate + 1.960 * se_ssynth
                                             ),
                                             down95 = c(att_PSM_log_intangible_asset_sa20[1] - 1.960*att_PSM_log_intangible_asset_sa20[2],a_log_intangible_asset_cs$overall.att - 1.960*a_log_intangible_asset_cs$overall.se#,  a_log_intangible_asset_Ssynth$att_estimate - 1.960 * se_ssynth
                                             ),
                                             up99 = c(att_PSM_log_intangible_asset_sa20[1] + 2.5758*att_PSM_log_intangible_asset_sa20[2],a_log_intangible_asset_cs$overall.att + 2.5758*a_log_intangible_asset_cs$overall.se#,  a_log_intangible_asset_Ssynth$att_estimate + 2.5758 * se_ssynth
                                             ),
                                             down99 = c(att_PSM_log_intangible_asset_sa20[1] - 2.5758*att_PSM_log_intangible_asset_sa20[2],a_log_intangible_asset_cs$overall.att - 2.5758*a_log_intangible_asset_cs$overall.se#,  a_log_intangible_asset_Ssynth$att_estimate - 2.5758 * se_ssynth
                                             )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  log_intangible_asset_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_log_intangible_asset_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_log_intangible_asset_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_log_intangible_asset_sa20$coeftable[,1],
           sa_se =es_PSM_log_intangible_asset_sa20$coeftable[,2],
           sa_t =es_PSM_log_intangible_asset_sa20$coeftable[,3],
           sa_p =es_PSM_log_intangible_asset_sa20$coeftable[,4])
  log_intangible_asset_summary_appendix_cs <- data.frame(year = cs_log_intangible_asset_smry$egt,
                                                         cs =  cs_log_intangible_asset_smry$att.egt,
                                                         cs_se = cs_log_intangible_asset_smry$se.egt,
                                                         cs_cband_lower = cs_log_intangible_asset_smry$att.egt - cs_log_intangible_asset_smry$crit.val.egt*cs_log_intangible_asset_smry$se.egt,
                                                         cs_cband_upper = cs_log_intangible_asset_smry$att.egt + cs_log_intangible_asset_smry$crit.val.egt*cs_log_intangible_asset_smry$se.egt)
  log_intangible_asset_summary_appendix <- right_join(log_intangible_asset_summary_appendix_sa,log_intangible_asset_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_log_intangible_asset_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- log_intangible_asset_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(log_intangible_asset_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- log_intangible_asset_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(log_intangible_asset_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  log_intangible_asset_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  log_intangible_asset_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(log_intangible_asset_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(log_intangible_asset_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  log_intangible_asset_summary <- cbind(log_intangible_asset_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(log_intangible_asset_summary$rob)
  
  p_log_intangible_asset_summary <- ggplot(log_intangible_asset_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_intangible_asset_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_intangible_asset_summary$pretrend[1]) +
    geom_rect(data=log_intangible_asset_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_intangible_asset_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_intangible_asset_summary$down, na.rm=T),max(log_intangible_asset_summary$up, na.rm=T)), max(-min(log_intangible_asset_summary$down, na.rm=T),max(log_intangible_asset_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_intangible_asset <- p_PSM_log_intangible_asset_sa20+p_log_intangible_asset_cs+p_log_intangible_asset_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_log_intangible_asset)
  ## output_asset-----------
  pa_asset_sum <- pa_log_sum_asset / pa_log_tangible_asset / pa_log_intangible_asset + plot_annotation(
    title = "Effect on the Asset",
    subtitle = target,
    ##caption = caption_main
  ) &
    theme(text = element_text(family="Meiryo UI"))
  
  # filename <- paste("03_robustness_output/", target,"_01asset_", count_intervention, ".png", sep="")
  # ggsave(filename,pa_log_asset_sum, 
  #        width=20, height =22.5, units = "cm", dpi=200) 
  
  filename_pdf <- paste("03_robustness_output/", target,"_31_trimmed_asset_", count_intervention, ".pdf", sep="")
  ggsave(filename_pdf,pa_asset_sum, 
         width=15, height =18, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("03_robustness_output/", target,"_31_trimmed_asset_", count_intervention, ".png", sep="")
  ggsave(filename_png,pa_asset_sum, 
         width=15, height =18, units = "cm", dpi=200) 
  
  
  write.xlsx(list("log_sum_asset"=log_sum_asset_summary, "log_tangible_asset"= log_tangible_asset_summary, "log_intangible_asset"= log_intangible_asset_summary), paste("03_robustness_output/",target,"_31asset_trimmed_table.xlsx", sep=""))
  write.xlsx(list("log_sum_asset"=log_sum_asset_summary_appendix, "log_tangible_asset"= log_tangible_asset_summary_appendix, "log_intangible_asset"= log_intangible_asset_summary_appendix), paste("03_robustness_output/",target,"_31asset_trimmed_table_appendix.xlsx"))
}
robustness_trimmed_employment <- function(target){
  ################## log_workers------------
  # ipw
  es_PSM_log_workers_sa20 = feols(log_workers ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_log_workers_sa20 <- aggregate(es_PSM_log_workers_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_workers_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_workers_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_workers", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_workers <- att_gt(yname = "log_workers",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                           xformla = formula_cs, 
                           est_method = "ipw",base_period="universal",alp=0.05,
                           data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                           #print_details=FALSE,
                           anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_workers_smry <- aggte(cs_log_workers, type = "dynamic",na.rm = TRUE)
  
  p_log_workers_cs <- ggdid(cs_log_workers_smry, title = "log_workers")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_log_workers_cs <- aggte(cs_log_workers, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  log_workers_summary <- data.frame(method = method,
                                    estimate = c(att_PSM_log_workers_sa20[1], a_log_workers_cs$overall.att#,a_log_workers_Ssynth$att_estimate
                                    ),
                                    se = c(att_PSM_log_workers_sa20[2],a_log_workers_cs$overall.se#,se_ssynth
                                    ),
                                    up = c(att_PSM_log_workers_sa20[1] + 1.645*att_PSM_log_workers_sa20[2],a_log_workers_cs$overall.att + 1.645*a_log_workers_cs$overall.se#, a_log_workers_Ssynth$att_estimate + 1.645 * se_ssynth
                                    ),
                                    down = c(att_PSM_log_workers_sa20[1] - 1.645*att_PSM_log_workers_sa20[2],a_log_workers_cs$overall.att - 1.645*a_log_workers_cs$overall.se#, a_log_workers_Ssynth$att_estimate - 1.645 * se_ssynth
                                    ),
                                    up95 = c(att_PSM_log_workers_sa20[1] + 1.960*att_PSM_log_workers_sa20[2],a_log_workers_cs$overall.att + 1.960*a_log_workers_cs$overall.se#,  a_log_workers_Ssynth$att_estimate + 1.960 * se_ssynth
                                    ),
                                    down95 = c(att_PSM_log_workers_sa20[1] - 1.960*att_PSM_log_workers_sa20[2],a_log_workers_cs$overall.att - 1.960*a_log_workers_cs$overall.se#,  a_log_workers_Ssynth$att_estimate - 1.960 * se_ssynth
                                    ),
                                    up99 = c(att_PSM_log_workers_sa20[1] + 2.5758*att_PSM_log_workers_sa20[2],a_log_workers_cs$overall.att + 2.5758*a_log_workers_cs$overall.se#,  a_log_workers_Ssynth$att_estimate + 2.5758 * se_ssynth
                                    ),
                                    down99 = c(att_PSM_log_workers_sa20[1] - 2.5758*att_PSM_log_workers_sa20[2],a_log_workers_cs$overall.att - 2.5758*a_log_workers_cs$overall.se#,  a_log_workers_Ssynth$att_estimate - 2.5758 * se_ssynth
                                    )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  log_workers_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_log_workers_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_log_workers_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_log_workers_sa20$coeftable[,1],
           sa_se =es_PSM_log_workers_sa20$coeftable[,2],
           sa_t =es_PSM_log_workers_sa20$coeftable[,3],
           sa_p =es_PSM_log_workers_sa20$coeftable[,4])
  log_workers_summary_appendix_cs <- data.frame(year = cs_log_workers_smry$egt,
                                                cs =  cs_log_workers_smry$att.egt,
                                                cs_se = cs_log_workers_smry$se.egt,
                                                cs_cband_lower = cs_log_workers_smry$att.egt - cs_log_workers_smry$crit.val.egt*cs_log_workers_smry$se.egt,
                                                cs_cband_upper = cs_log_workers_smry$att.egt + cs_log_workers_smry$crit.val.egt*cs_log_workers_smry$se.egt)
  log_workers_summary_appendix <- right_join(log_workers_summary_appendix_sa,log_workers_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_log_workers_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- log_workers_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(log_workers_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- log_workers_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(log_workers_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  log_workers_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  log_workers_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(log_workers_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(log_workers_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  log_workers_summary <- cbind(log_workers_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(log_workers_summary$rob)
  
  p_log_workers_summary <- ggplot(log_workers_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_workers_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_workers_summary$pretrend[1]) +
    geom_rect(data=log_workers_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_workers_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_workers_summary$down, na.rm=T),max(log_workers_summary$up, na.rm=T)), max(-min(log_workers_summary$down, na.rm=T),max(log_workers_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_workers <- p_PSM_log_workers_sa20+p_log_workers_cs+p_log_workers_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_log_workers)
  ################## log_indefinite_workers------------
  # ipw
  es_PSM_log_indefinite_workers_sa20 = feols(log_indefinite_workers ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_log_indefinite_workers_sa20 <- aggregate(es_PSM_log_indefinite_workers_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_indefinite_workers_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_indefinite_workers_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_indefinite_workers", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_indefinite_workers <- att_gt(yname = "log_indefinite_workers",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                                      xformla = formula_cs, 
                                      est_method = "ipw",base_period="universal",alp=0.05,
                                      data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                                      #print_details=FALSE,
                                      anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_indefinite_workers_smry <- aggte(cs_log_indefinite_workers, type = "dynamic",na.rm = TRUE)
  
  p_log_indefinite_workers_cs <- ggdid(cs_log_indefinite_workers_smry, title = "log_indefinite_workers")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_log_indefinite_workers_cs <- aggte(cs_log_indefinite_workers, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  log_indefinite_workers_summary <- data.frame(method = method,
                                               estimate = c(att_PSM_log_indefinite_workers_sa20[1], a_log_indefinite_workers_cs$overall.att#,a_log_indefinite_workers_Ssynth$att_estimate
                                               ),
                                               se = c(att_PSM_log_indefinite_workers_sa20[2],a_log_indefinite_workers_cs$overall.se#,se_ssynth
                                               ),
                                               up = c(att_PSM_log_indefinite_workers_sa20[1] + 1.645*att_PSM_log_indefinite_workers_sa20[2],a_log_indefinite_workers_cs$overall.att + 1.645*a_log_indefinite_workers_cs$overall.se#, a_log_indefinite_workers_Ssynth$att_estimate + 1.645 * se_ssynth
                                               ),
                                               down = c(att_PSM_log_indefinite_workers_sa20[1] - 1.645*att_PSM_log_indefinite_workers_sa20[2],a_log_indefinite_workers_cs$overall.att - 1.645*a_log_indefinite_workers_cs$overall.se#, a_log_indefinite_workers_Ssynth$att_estimate - 1.645 * se_ssynth
                                               ),
                                               up95 = c(att_PSM_log_indefinite_workers_sa20[1] + 1.960*att_PSM_log_indefinite_workers_sa20[2],a_log_indefinite_workers_cs$overall.att + 1.960*a_log_indefinite_workers_cs$overall.se#,  a_log_indefinite_workers_Ssynth$att_estimate + 1.960 * se_ssynth
                                               ),
                                               down95 = c(att_PSM_log_indefinite_workers_sa20[1] - 1.960*att_PSM_log_indefinite_workers_sa20[2],a_log_indefinite_workers_cs$overall.att - 1.960*a_log_indefinite_workers_cs$overall.se#,  a_log_indefinite_workers_Ssynth$att_estimate - 1.960 * se_ssynth
                                               ),
                                               up99 = c(att_PSM_log_indefinite_workers_sa20[1] + 2.5758*att_PSM_log_indefinite_workers_sa20[2],a_log_indefinite_workers_cs$overall.att + 2.5758*a_log_indefinite_workers_cs$overall.se#,  a_log_indefinite_workers_Ssynth$att_estimate + 2.5758 * se_ssynth
                                               ),
                                               down99 = c(att_PSM_log_indefinite_workers_sa20[1] - 2.5758*att_PSM_log_indefinite_workers_sa20[2],a_log_indefinite_workers_cs$overall.att - 2.5758*a_log_indefinite_workers_cs$overall.se#,  a_log_indefinite_workers_Ssynth$att_estimate - 2.5758 * se_ssynth
                                               )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  log_indefinite_workers_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_log_indefinite_workers_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_log_indefinite_workers_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_log_indefinite_workers_sa20$coeftable[,1],
           sa_se =es_PSM_log_indefinite_workers_sa20$coeftable[,2],
           sa_t =es_PSM_log_indefinite_workers_sa20$coeftable[,3],
           sa_p =es_PSM_log_indefinite_workers_sa20$coeftable[,4])
  log_indefinite_workers_summary_appendix_cs <- data.frame(year = cs_log_indefinite_workers_smry$egt,
                                                           cs =  cs_log_indefinite_workers_smry$att.egt,
                                                           cs_se = cs_log_indefinite_workers_smry$se.egt,
                                                           cs_cband_lower = cs_log_indefinite_workers_smry$att.egt - cs_log_indefinite_workers_smry$crit.val.egt*cs_log_indefinite_workers_smry$se.egt,
                                                           cs_cband_upper = cs_log_indefinite_workers_smry$att.egt + cs_log_indefinite_workers_smry$crit.val.egt*cs_log_indefinite_workers_smry$se.egt)
  log_indefinite_workers_summary_appendix <- right_join(log_indefinite_workers_summary_appendix_sa,log_indefinite_workers_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_log_indefinite_workers_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- log_indefinite_workers_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(log_indefinite_workers_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- log_indefinite_workers_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(log_indefinite_workers_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  log_indefinite_workers_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  log_indefinite_workers_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(log_indefinite_workers_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(log_indefinite_workers_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  log_indefinite_workers_summary <- cbind(log_indefinite_workers_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(log_indefinite_workers_summary$rob)
  
  p_log_indefinite_workers_summary <- ggplot(log_indefinite_workers_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_indefinite_workers_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_indefinite_workers_summary$pretrend[1]) +
    geom_rect(data=log_indefinite_workers_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_indefinite_workers_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_indefinite_workers_summary$down, na.rm=T),max(log_indefinite_workers_summary$up, na.rm=T)), max(-min(log_indefinite_workers_summary$down, na.rm=T),max(log_indefinite_workers_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_indefinite_workers <- p_PSM_log_indefinite_workers_sa20+p_log_indefinite_workers_cs+p_log_indefinite_workers_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_log_indefinite_workers)
  ################## log_fixedterm_workers------------
  # ipw
  es_PSM_log_fixedterm_workers_sa20 = feols(log_fixedterm_workers ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_log_fixedterm_workers_sa20 <- aggregate(es_PSM_log_fixedterm_workers_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_fixedterm_workers_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_fixedterm_workers_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_fixedterm_workers", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_fixedterm_workers <- att_gt(yname = "log_fixedterm_workers",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                                     xformla = formula_cs, 
                                     est_method = "ipw",base_period="universal",alp=0.05,
                                     data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                                     #print_details=FALSE,
                                     anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_fixedterm_workers_smry <- aggte(cs_log_fixedterm_workers, type = "dynamic",na.rm = TRUE)
  
  p_log_fixedterm_workers_cs <- ggdid(cs_log_fixedterm_workers_smry, title = "log_fixedterm_workers")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_log_fixedterm_workers_cs <- aggte(cs_log_fixedterm_workers, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  log_fixedterm_workers_summary <- data.frame(method = method,
                                              estimate = c(att_PSM_log_fixedterm_workers_sa20[1], a_log_fixedterm_workers_cs$overall.att#,a_log_fixedterm_workers_Ssynth$att_estimate
                                              ),
                                              se = c(att_PSM_log_fixedterm_workers_sa20[2],a_log_fixedterm_workers_cs$overall.se#,se_ssynth
                                              ),
                                              up = c(att_PSM_log_fixedterm_workers_sa20[1] + 1.645*att_PSM_log_fixedterm_workers_sa20[2],a_log_fixedterm_workers_cs$overall.att + 1.645*a_log_fixedterm_workers_cs$overall.se#, a_log_fixedterm_workers_Ssynth$att_estimate + 1.645 * se_ssynth
                                              ),
                                              down = c(att_PSM_log_fixedterm_workers_sa20[1] - 1.645*att_PSM_log_fixedterm_workers_sa20[2],a_log_fixedterm_workers_cs$overall.att - 1.645*a_log_fixedterm_workers_cs$overall.se#, a_log_fixedterm_workers_Ssynth$att_estimate - 1.645 * se_ssynth
                                              ),
                                              up95 = c(att_PSM_log_fixedterm_workers_sa20[1] + 1.960*att_PSM_log_fixedterm_workers_sa20[2],a_log_fixedterm_workers_cs$overall.att + 1.960*a_log_fixedterm_workers_cs$overall.se#,  a_log_fixedterm_workers_Ssynth$att_estimate + 1.960 * se_ssynth
                                              ),
                                              down95 = c(att_PSM_log_fixedterm_workers_sa20[1] - 1.960*att_PSM_log_fixedterm_workers_sa20[2],a_log_fixedterm_workers_cs$overall.att - 1.960*a_log_fixedterm_workers_cs$overall.se#,  a_log_fixedterm_workers_Ssynth$att_estimate - 1.960 * se_ssynth
                                              ),
                                              up99 = c(att_PSM_log_fixedterm_workers_sa20[1] + 2.5758*att_PSM_log_fixedterm_workers_sa20[2],a_log_fixedterm_workers_cs$overall.att + 2.5758*a_log_fixedterm_workers_cs$overall.se#,  a_log_fixedterm_workers_Ssynth$att_estimate + 2.5758 * se_ssynth
                                              ),
                                              down99 = c(att_PSM_log_fixedterm_workers_sa20[1] - 2.5758*att_PSM_log_fixedterm_workers_sa20[2],a_log_fixedterm_workers_cs$overall.att - 2.5758*a_log_fixedterm_workers_cs$overall.se#,  a_log_fixedterm_workers_Ssynth$att_estimate - 2.5758 * se_ssynth
                                              )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  log_fixedterm_workers_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_log_fixedterm_workers_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_log_fixedterm_workers_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_log_fixedterm_workers_sa20$coeftable[,1],
           sa_se =es_PSM_log_fixedterm_workers_sa20$coeftable[,2],
           sa_t =es_PSM_log_fixedterm_workers_sa20$coeftable[,3],
           sa_p =es_PSM_log_fixedterm_workers_sa20$coeftable[,4])
  log_fixedterm_workers_summary_appendix_cs <- data.frame(year = cs_log_fixedterm_workers_smry$egt,
                                                          cs =  cs_log_fixedterm_workers_smry$att.egt,
                                                          cs_se = cs_log_fixedterm_workers_smry$se.egt,
                                                          cs_cband_lower = cs_log_fixedterm_workers_smry$att.egt - cs_log_fixedterm_workers_smry$crit.val.egt*cs_log_fixedterm_workers_smry$se.egt,
                                                          cs_cband_upper = cs_log_fixedterm_workers_smry$att.egt + cs_log_fixedterm_workers_smry$crit.val.egt*cs_log_fixedterm_workers_smry$se.egt)
  log_fixedterm_workers_summary_appendix <- right_join(log_fixedterm_workers_summary_appendix_sa,log_fixedterm_workers_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_log_fixedterm_workers_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- log_fixedterm_workers_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(log_fixedterm_workers_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- log_fixedterm_workers_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(log_fixedterm_workers_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  log_fixedterm_workers_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  log_fixedterm_workers_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(log_fixedterm_workers_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(log_fixedterm_workers_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  log_fixedterm_workers_summary <- cbind(log_fixedterm_workers_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(log_fixedterm_workers_summary$rob)
  
  p_log_fixedterm_workers_summary <- ggplot(log_fixedterm_workers_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_fixedterm_workers_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_fixedterm_workers_summary$pretrend[1]) +
    geom_rect(data=log_fixedterm_workers_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_fixedterm_workers_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_fixedterm_workers_summary$down, na.rm=T),max(log_fixedterm_workers_summary$up, na.rm=T)), max(-min(log_fixedterm_workers_summary$down, na.rm=T),max(log_fixedterm_workers_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_fixedterm_workers <- p_PSM_log_fixedterm_workers_sa20+p_log_fixedterm_workers_cs+p_log_fixedterm_workers_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_log_fixedterm_workers)
  ################## log_fixedterm_workers_equivalent------------
  # ipw
  es_PSM_log_fixedterm_workers_equivalent_sa20 = feols(log_fixedterm_workers_equivalent ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_log_fixedterm_workers_equivalent_sa20 <- aggregate(es_PSM_log_fixedterm_workers_equivalent_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_fixedterm_workers_equivalent_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_fixedterm_workers_equivalent_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_fixedterm_workers_equivalent", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_fixedterm_workers_equivalent <- att_gt(yname = "log_fixedterm_workers_equivalent",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                                                xformla = formula_cs, 
                                                est_method = "ipw",base_period="universal",alp=0.05,
                                                data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                                                #print_details=FALSE,
                                                anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_fixedterm_workers_equivalent_smry <- aggte(cs_log_fixedterm_workers_equivalent, type = "dynamic",na.rm = TRUE)
  
  p_log_fixedterm_workers_equivalent_cs <- ggdid(cs_log_fixedterm_workers_equivalent_smry, title = "log_fixedterm_workers_equivalent")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_log_fixedterm_workers_equivalent_cs <- aggte(cs_log_fixedterm_workers_equivalent, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  log_fixedterm_workers_equivalent_summary <- data.frame(method = method,
                                                         estimate = c(att_PSM_log_fixedterm_workers_equivalent_sa20[1], a_log_fixedterm_workers_equivalent_cs$overall.att#,a_log_fixedterm_workers_equivalent_Ssynth$att_estimate
                                                         ),
                                                         se = c(att_PSM_log_fixedterm_workers_equivalent_sa20[2],a_log_fixedterm_workers_equivalent_cs$overall.se#,se_ssynth
                                                         ),
                                                         up = c(att_PSM_log_fixedterm_workers_equivalent_sa20[1] + 1.645*att_PSM_log_fixedterm_workers_equivalent_sa20[2],a_log_fixedterm_workers_equivalent_cs$overall.att + 1.645*a_log_fixedterm_workers_equivalent_cs$overall.se#, a_log_fixedterm_workers_equivalent_Ssynth$att_estimate + 1.645 * se_ssynth
                                                         ),
                                                         down = c(att_PSM_log_fixedterm_workers_equivalent_sa20[1] - 1.645*att_PSM_log_fixedterm_workers_equivalent_sa20[2],a_log_fixedterm_workers_equivalent_cs$overall.att - 1.645*a_log_fixedterm_workers_equivalent_cs$overall.se#, a_log_fixedterm_workers_equivalent_Ssynth$att_estimate - 1.645 * se_ssynth
                                                         ),
                                                         up95 = c(att_PSM_log_fixedterm_workers_equivalent_sa20[1] + 1.960*att_PSM_log_fixedterm_workers_equivalent_sa20[2],a_log_fixedterm_workers_equivalent_cs$overall.att + 1.960*a_log_fixedterm_workers_equivalent_cs$overall.se#,  a_log_fixedterm_workers_equivalent_Ssynth$att_estimate + 1.960 * se_ssynth
                                                         ),
                                                         down95 = c(att_PSM_log_fixedterm_workers_equivalent_sa20[1] - 1.960*att_PSM_log_fixedterm_workers_equivalent_sa20[2],a_log_fixedterm_workers_equivalent_cs$overall.att - 1.960*a_log_fixedterm_workers_equivalent_cs$overall.se#,  a_log_fixedterm_workers_equivalent_Ssynth$att_estimate - 1.960 * se_ssynth
                                                         ),
                                                         up99 = c(att_PSM_log_fixedterm_workers_equivalent_sa20[1] + 2.5758*att_PSM_log_fixedterm_workers_equivalent_sa20[2],a_log_fixedterm_workers_equivalent_cs$overall.att + 2.5758*a_log_fixedterm_workers_equivalent_cs$overall.se#,  a_log_fixedterm_workers_equivalent_Ssynth$att_estimate + 2.5758 * se_ssynth
                                                         ),
                                                         down99 = c(att_PSM_log_fixedterm_workers_equivalent_sa20[1] - 2.5758*att_PSM_log_fixedterm_workers_equivalent_sa20[2],a_log_fixedterm_workers_equivalent_cs$overall.att - 2.5758*a_log_fixedterm_workers_equivalent_cs$overall.se#,  a_log_fixedterm_workers_equivalent_Ssynth$att_estimate - 2.5758 * se_ssynth
                                                         )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  log_fixedterm_workers_equivalent_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_log_fixedterm_workers_equivalent_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_log_fixedterm_workers_equivalent_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_log_fixedterm_workers_equivalent_sa20$coeftable[,1],
           sa_se =es_PSM_log_fixedterm_workers_equivalent_sa20$coeftable[,2],
           sa_t =es_PSM_log_fixedterm_workers_equivalent_sa20$coeftable[,3],
           sa_p =es_PSM_log_fixedterm_workers_equivalent_sa20$coeftable[,4])
  log_fixedterm_workers_equivalent_summary_appendix_cs <- data.frame(year = cs_log_fixedterm_workers_equivalent_smry$egt,
                                                                     cs =  cs_log_fixedterm_workers_equivalent_smry$att.egt,
                                                                     cs_se = cs_log_fixedterm_workers_equivalent_smry$se.egt,
                                                                     cs_cband_lower = cs_log_fixedterm_workers_equivalent_smry$att.egt - cs_log_fixedterm_workers_equivalent_smry$crit.val.egt*cs_log_fixedterm_workers_equivalent_smry$se.egt,
                                                                     cs_cband_upper = cs_log_fixedterm_workers_equivalent_smry$att.egt + cs_log_fixedterm_workers_equivalent_smry$crit.val.egt*cs_log_fixedterm_workers_equivalent_smry$se.egt)
  log_fixedterm_workers_equivalent_summary_appendix <- right_join(log_fixedterm_workers_equivalent_summary_appendix_sa,log_fixedterm_workers_equivalent_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_log_fixedterm_workers_equivalent_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- log_fixedterm_workers_equivalent_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(log_fixedterm_workers_equivalent_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- log_fixedterm_workers_equivalent_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(log_fixedterm_workers_equivalent_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  log_fixedterm_workers_equivalent_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  log_fixedterm_workers_equivalent_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(log_fixedterm_workers_equivalent_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(log_fixedterm_workers_equivalent_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  log_fixedterm_workers_equivalent_summary <- cbind(log_fixedterm_workers_equivalent_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(log_fixedterm_workers_equivalent_summary$rob)
  
  p_log_fixedterm_workers_equivalent_summary <- ggplot(log_fixedterm_workers_equivalent_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_fixedterm_workers_equivalent_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_fixedterm_workers_equivalent_summary$pretrend[1]) +
    geom_rect(data=log_fixedterm_workers_equivalent_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_fixedterm_workers_equivalent_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_fixedterm_workers_equivalent_summary$down, na.rm=T),max(log_fixedterm_workers_equivalent_summary$up, na.rm=T)), max(-min(log_fixedterm_workers_equivalent_summary$down, na.rm=T),max(log_fixedterm_workers_equivalent_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_fixedterm_workers_equivalent <- p_PSM_log_fixedterm_workers_equivalent_sa20+p_log_fixedterm_workers_equivalent_cs+p_log_fixedterm_workers_equivalent_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_log_fixedterm_workers_equivalent)
  ################## log_salary------------
  # ipw
  es_PSM_log_salary_sa20 = feols(log_salary ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_log_salary_sa20 <- aggregate(es_PSM_log_salary_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_salary_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_salary_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_salary", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_salary <- att_gt(yname = "log_salary",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                          xformla = formula_cs, 
                          est_method = "ipw",base_period="universal",alp=0.05,
                          data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                          #print_details=FALSE,
                          anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_salary_smry <- aggte(cs_log_salary, type = "dynamic",na.rm = TRUE)
  
  p_log_salary_cs <- ggdid(cs_log_salary_smry, title = "log_salary")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_log_salary_cs <- aggte(cs_log_salary, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  log_salary_summary <- data.frame(method = method,
                                   estimate = c(att_PSM_log_salary_sa20[1], a_log_salary_cs$overall.att#,a_log_salary_Ssynth$att_estimate
                                   ),
                                   se = c(att_PSM_log_salary_sa20[2],a_log_salary_cs$overall.se#,se_ssynth
                                   ),
                                   up = c(att_PSM_log_salary_sa20[1] + 1.645*att_PSM_log_salary_sa20[2],a_log_salary_cs$overall.att + 1.645*a_log_salary_cs$overall.se#, a_log_salary_Ssynth$att_estimate + 1.645 * se_ssynth
                                   ),
                                   down = c(att_PSM_log_salary_sa20[1] - 1.645*att_PSM_log_salary_sa20[2],a_log_salary_cs$overall.att - 1.645*a_log_salary_cs$overall.se#, a_log_salary_Ssynth$att_estimate - 1.645 * se_ssynth
                                   ),
                                   up95 = c(att_PSM_log_salary_sa20[1] + 1.960*att_PSM_log_salary_sa20[2],a_log_salary_cs$overall.att + 1.960*a_log_salary_cs$overall.se#,  a_log_salary_Ssynth$att_estimate + 1.960 * se_ssynth
                                   ),
                                   down95 = c(att_PSM_log_salary_sa20[1] - 1.960*att_PSM_log_salary_sa20[2],a_log_salary_cs$overall.att - 1.960*a_log_salary_cs$overall.se#,  a_log_salary_Ssynth$att_estimate - 1.960 * se_ssynth
                                   ),
                                   up99 = c(att_PSM_log_salary_sa20[1] + 2.5758*att_PSM_log_salary_sa20[2],a_log_salary_cs$overall.att + 2.5758*a_log_salary_cs$overall.se#,  a_log_salary_Ssynth$att_estimate + 2.5758 * se_ssynth
                                   ),
                                   down99 = c(att_PSM_log_salary_sa20[1] - 2.5758*att_PSM_log_salary_sa20[2],a_log_salary_cs$overall.att - 2.5758*a_log_salary_cs$overall.se#,  a_log_salary_Ssynth$att_estimate - 2.5758 * se_ssynth
                                   )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  log_salary_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_log_salary_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_log_salary_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_log_salary_sa20$coeftable[,1],
           sa_se =es_PSM_log_salary_sa20$coeftable[,2],
           sa_t =es_PSM_log_salary_sa20$coeftable[,3],
           sa_p =es_PSM_log_salary_sa20$coeftable[,4])
  log_salary_summary_appendix_cs <- data.frame(year = cs_log_salary_smry$egt,
                                               cs =  cs_log_salary_smry$att.egt,
                                               cs_se = cs_log_salary_smry$se.egt,
                                               cs_cband_lower = cs_log_salary_smry$att.egt - cs_log_salary_smry$crit.val.egt*cs_log_salary_smry$se.egt,
                                               cs_cband_upper = cs_log_salary_smry$att.egt + cs_log_salary_smry$crit.val.egt*cs_log_salary_smry$se.egt)
  log_salary_summary_appendix <- right_join(log_salary_summary_appendix_sa,log_salary_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_log_salary_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- log_salary_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(log_salary_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- log_salary_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(log_salary_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  log_salary_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  log_salary_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(log_salary_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(log_salary_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  log_salary_summary <- cbind(log_salary_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(log_salary_summary$rob)
  
  p_log_salary_summary <- ggplot(log_salary_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_salary_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_salary_summary$pretrend[1]) +
    geom_rect(data=log_salary_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_salary_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_salary_summary$down, na.rm=T),max(log_salary_summary$up, na.rm=T)), max(-min(log_salary_summary$down, na.rm=T),max(log_salary_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_salary <- p_PSM_log_salary_sa20+p_log_salary_cs+p_log_salary_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_log_salary)
  ################## log_benefit------------
  # ipw
  es_PSM_log_benefit_sa20 = feols(log_benefit ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_log_benefit_sa20 <- aggregate(es_PSM_log_benefit_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_benefit_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_benefit_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_benefit", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_benefit <- att_gt(yname = "log_benefit",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                           xformla = formula_cs, 
                           est_method = "ipw",base_period="universal",alp=0.05,
                           data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                           #print_details=FALSE,
                           anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_benefit_smry <- aggte(cs_log_benefit, type = "dynamic",na.rm = TRUE)
  
  p_log_benefit_cs <- ggdid(cs_log_benefit_smry, title = "log_benefit")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_log_benefit_cs <- aggte(cs_log_benefit, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  log_benefit_summary <- data.frame(method = method,
                                    estimate = c(att_PSM_log_benefit_sa20[1], a_log_benefit_cs$overall.att#,a_log_benefit_Ssynth$att_estimate
                                    ),
                                    se = c(att_PSM_log_benefit_sa20[2],a_log_benefit_cs$overall.se#,se_ssynth
                                    ),
                                    up = c(att_PSM_log_benefit_sa20[1] + 1.645*att_PSM_log_benefit_sa20[2],a_log_benefit_cs$overall.att + 1.645*a_log_benefit_cs$overall.se#, a_log_benefit_Ssynth$att_estimate + 1.645 * se_ssynth
                                    ),
                                    down = c(att_PSM_log_benefit_sa20[1] - 1.645*att_PSM_log_benefit_sa20[2],a_log_benefit_cs$overall.att - 1.645*a_log_benefit_cs$overall.se#, a_log_benefit_Ssynth$att_estimate - 1.645 * se_ssynth
                                    ),
                                    up95 = c(att_PSM_log_benefit_sa20[1] + 1.960*att_PSM_log_benefit_sa20[2],a_log_benefit_cs$overall.att + 1.960*a_log_benefit_cs$overall.se#,  a_log_benefit_Ssynth$att_estimate + 1.960 * se_ssynth
                                    ),
                                    down95 = c(att_PSM_log_benefit_sa20[1] - 1.960*att_PSM_log_benefit_sa20[2],a_log_benefit_cs$overall.att - 1.960*a_log_benefit_cs$overall.se#,  a_log_benefit_Ssynth$att_estimate - 1.960 * se_ssynth
                                    ),
                                    up99 = c(att_PSM_log_benefit_sa20[1] + 2.5758*att_PSM_log_benefit_sa20[2],a_log_benefit_cs$overall.att + 2.5758*a_log_benefit_cs$overall.se#,  a_log_benefit_Ssynth$att_estimate + 2.5758 * se_ssynth
                                    ),
                                    down99 = c(att_PSM_log_benefit_sa20[1] - 2.5758*att_PSM_log_benefit_sa20[2],a_log_benefit_cs$overall.att - 2.5758*a_log_benefit_cs$overall.se#,  a_log_benefit_Ssynth$att_estimate - 2.5758 * se_ssynth
                                    )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  log_benefit_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_log_benefit_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_log_benefit_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_log_benefit_sa20$coeftable[,1],
           sa_se =es_PSM_log_benefit_sa20$coeftable[,2],
           sa_t =es_PSM_log_benefit_sa20$coeftable[,3],
           sa_p =es_PSM_log_benefit_sa20$coeftable[,4])
  log_benefit_summary_appendix_cs <- data.frame(year = cs_log_benefit_smry$egt,
                                                cs =  cs_log_benefit_smry$att.egt,
                                                cs_se = cs_log_benefit_smry$se.egt,
                                                cs_cband_lower = cs_log_benefit_smry$att.egt - cs_log_benefit_smry$crit.val.egt*cs_log_benefit_smry$se.egt,
                                                cs_cband_upper = cs_log_benefit_smry$att.egt + cs_log_benefit_smry$crit.val.egt*cs_log_benefit_smry$se.egt)
  log_benefit_summary_appendix <- right_join(log_benefit_summary_appendix_sa,log_benefit_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_log_benefit_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- log_benefit_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(log_benefit_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- log_benefit_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(log_benefit_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  log_benefit_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  log_benefit_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(log_benefit_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(log_benefit_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  log_benefit_summary <- cbind(log_benefit_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(log_benefit_summary$rob)
  
  p_log_benefit_summary <- ggplot(log_benefit_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_benefit_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_benefit_summary$pretrend[1]) +
    geom_rect(data=log_benefit_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_benefit_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_benefit_summary$down, na.rm=T),max(log_benefit_summary$up, na.rm=T)), max(-min(log_benefit_summary$down, na.rm=T),max(log_benefit_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_benefit <- p_PSM_log_benefit_sa20+p_log_benefit_cs+p_log_benefit_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_log_benefit)
  ## output_employment-----------
  pa_employment_sum <- pa_log_workers/pa_log_indefinite_workers/pa_log_fixedterm_workers/pa_log_fixedterm_workers_equivalent/pa_log_salary/pa_log_benefit + plot_annotation(
    title = "Effect on the Employment",
    subtitle = target,
    ##caption = caption_main
  ) &
    theme(text = element_text(family="Meiryo UI"))
  
  # filename <- paste("03_robustness_output/", target,"_01employment_", count_intervention, ".png", sep="")
  # ggsave(filename,pa_log_employment_sum, 
  #        width=20, height =22.5, units = "cm", dpi=200) 
  
  filename_pdf <- paste("03_robustness_output/", target,"_32_trimmed_employment_", count_intervention, ".pdf", sep="")
  ggsave(filename_pdf,pa_employment_sum, 
         width=15, height =36, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("03_robustness_output/", target,"_32_trimmed_employment_", count_intervention, ".png", sep="")
  ggsave(filename_png,pa_employment_sum, 
         width=15, height =36, units = "cm", dpi=200) 
  
  
  write.xlsx(list("log_workers"=log_workers_summary,"log_indefinite_workers"=log_indefinite_workers_summary,"log_fixedterm_workers"=log_fixedterm_workers_summary,"log_fixedterm_workers_eq"=log_fixedterm_workers_equivalent_summary,"log_salary"=log_salary_summary,"log_benefit"=log_benefit_summary), paste("03_robustness_output/",target,"_32employment_trimmed_table.xlsx", sep=""))
  write.xlsx(list("log_workers"=log_workers_summary_appendix,"log_indefinite_workers"=log_indefinite_workers_summary_appendix,"log_fixedterm_workers"=log_fixedterm_workers_summary_appendix,"log_fixedterm_workers_eq"=log_fixedterm_workers_equivalent_summary_appendix,"log_salary"=log_salary_summary_appendix,"log_benefit"=log_benefit_summary_appendix), paste("03_robustness_output/",target,"_32employment_trimmed_table_appendix.xlsx"))
}
robustness_trimmed_business <- function(target){
  ################## log_sales------------
  # ipw
  es_PSM_log_sales_sa20 = feols(log_sales ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_log_sales_sa20 <- aggregate(es_PSM_log_sales_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_sales_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_sales_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_sales", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_sales <- att_gt(yname = "log_sales",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                         xformla = formula_cs, 
                         est_method = "ipw",base_period="universal",alp=0.05,
                         data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                         #print_details=FALSE,
                         anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_sales_smry <- aggte(cs_log_sales, type = "dynamic",na.rm = TRUE)
  
  p_log_sales_cs <- ggdid(cs_log_sales_smry, title = "log_sales")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_log_sales_cs <- aggte(cs_log_sales, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  log_sales_summary <- data.frame(method = method,
                                  estimate = c(att_PSM_log_sales_sa20[1], a_log_sales_cs$overall.att#,a_log_sales_Ssynth$att_estimate
                                  ),
                                  se = c(att_PSM_log_sales_sa20[2],a_log_sales_cs$overall.se#,se_ssynth
                                  ),
                                  up = c(att_PSM_log_sales_sa20[1] + 1.645*att_PSM_log_sales_sa20[2],a_log_sales_cs$overall.att + 1.645*a_log_sales_cs$overall.se#, a_log_sales_Ssynth$att_estimate + 1.645 * se_ssynth
                                  ),
                                  down = c(att_PSM_log_sales_sa20[1] - 1.645*att_PSM_log_sales_sa20[2],a_log_sales_cs$overall.att - 1.645*a_log_sales_cs$overall.se#, a_log_sales_Ssynth$att_estimate - 1.645 * se_ssynth
                                  ),
                                  up95 = c(att_PSM_log_sales_sa20[1] + 1.960*att_PSM_log_sales_sa20[2],a_log_sales_cs$overall.att + 1.960*a_log_sales_cs$overall.se#,  a_log_sales_Ssynth$att_estimate + 1.960 * se_ssynth
                                  ),
                                  down95 = c(att_PSM_log_sales_sa20[1] - 1.960*att_PSM_log_sales_sa20[2],a_log_sales_cs$overall.att - 1.960*a_log_sales_cs$overall.se#,  a_log_sales_Ssynth$att_estimate - 1.960 * se_ssynth
                                  ),
                                  up99 = c(att_PSM_log_sales_sa20[1] + 2.5758*att_PSM_log_sales_sa20[2],a_log_sales_cs$overall.att + 2.5758*a_log_sales_cs$overall.se#,  a_log_sales_Ssynth$att_estimate + 2.5758 * se_ssynth
                                  ),
                                  down99 = c(att_PSM_log_sales_sa20[1] - 2.5758*att_PSM_log_sales_sa20[2],a_log_sales_cs$overall.att - 2.5758*a_log_sales_cs$overall.se#,  a_log_sales_Ssynth$att_estimate - 2.5758 * se_ssynth
                                  )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  log_sales_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_log_sales_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_log_sales_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_log_sales_sa20$coeftable[,1],
           sa_se =es_PSM_log_sales_sa20$coeftable[,2],
           sa_t =es_PSM_log_sales_sa20$coeftable[,3],
           sa_p =es_PSM_log_sales_sa20$coeftable[,4])
  log_sales_summary_appendix_cs <- data.frame(year = cs_log_sales_smry$egt,
                                              cs =  cs_log_sales_smry$att.egt,
                                              cs_se = cs_log_sales_smry$se.egt,
                                              cs_cband_lower = cs_log_sales_smry$att.egt - cs_log_sales_smry$crit.val.egt*cs_log_sales_smry$se.egt,
                                              cs_cband_upper = cs_log_sales_smry$att.egt + cs_log_sales_smry$crit.val.egt*cs_log_sales_smry$se.egt)
  log_sales_summary_appendix <- right_join(log_sales_summary_appendix_sa,log_sales_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_log_sales_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- log_sales_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(log_sales_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- log_sales_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(log_sales_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  log_sales_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  log_sales_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(log_sales_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(log_sales_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  log_sales_summary <- cbind(log_sales_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(log_sales_summary$rob)
  
  p_log_sales_summary <- ggplot(log_sales_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_sales_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_sales_summary$pretrend[1]) +
    geom_rect(data=log_sales_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_sales_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_sales_summary$down, na.rm=T),max(log_sales_summary$up, na.rm=T)), max(-min(log_sales_summary$down, na.rm=T),max(log_sales_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_sales <- p_PSM_log_sales_sa20+p_log_sales_cs+p_log_sales_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_log_sales)
  ################## log_tax------------
  # ipw
  es_PSM_log_tax_sa20 = feols(log_tax ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_log_tax_sa20 <- aggregate(es_PSM_log_tax_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_tax_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_tax_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_tax", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_tax <- att_gt(yname = "log_tax",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                       xformla = formula_cs, 
                       est_method = "ipw",base_period="universal",alp=0.05,
                       data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                       #print_details=FALSE,
                       anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_tax_smry <- aggte(cs_log_tax, type = "dynamic",na.rm = TRUE)
  
  p_log_tax_cs <- ggdid(cs_log_tax_smry, title = "log_tax")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_log_tax_cs <- aggte(cs_log_tax, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  log_tax_summary <- data.frame(method = method,
                                estimate = c(att_PSM_log_tax_sa20[1], a_log_tax_cs$overall.att#,a_log_tax_Ssynth$att_estimate
                                ),
                                se = c(att_PSM_log_tax_sa20[2],a_log_tax_cs$overall.se#,se_ssynth
                                ),
                                up = c(att_PSM_log_tax_sa20[1] + 1.645*att_PSM_log_tax_sa20[2],a_log_tax_cs$overall.att + 1.645*a_log_tax_cs$overall.se#, a_log_tax_Ssynth$att_estimate + 1.645 * se_ssynth
                                ),
                                down = c(att_PSM_log_tax_sa20[1] - 1.645*att_PSM_log_tax_sa20[2],a_log_tax_cs$overall.att - 1.645*a_log_tax_cs$overall.se#, a_log_tax_Ssynth$att_estimate - 1.645 * se_ssynth
                                ),
                                up95 = c(att_PSM_log_tax_sa20[1] + 1.960*att_PSM_log_tax_sa20[2],a_log_tax_cs$overall.att + 1.960*a_log_tax_cs$overall.se#,  a_log_tax_Ssynth$att_estimate + 1.960 * se_ssynth
                                ),
                                down95 = c(att_PSM_log_tax_sa20[1] - 1.960*att_PSM_log_tax_sa20[2],a_log_tax_cs$overall.att - 1.960*a_log_tax_cs$overall.se#,  a_log_tax_Ssynth$att_estimate - 1.960 * se_ssynth
                                ),
                                up99 = c(att_PSM_log_tax_sa20[1] + 2.5758*att_PSM_log_tax_sa20[2],a_log_tax_cs$overall.att + 2.5758*a_log_tax_cs$overall.se#,  a_log_tax_Ssynth$att_estimate + 2.5758 * se_ssynth
                                ),
                                down99 = c(att_PSM_log_tax_sa20[1] - 2.5758*att_PSM_log_tax_sa20[2],a_log_tax_cs$overall.att - 2.5758*a_log_tax_cs$overall.se#,  a_log_tax_Ssynth$att_estimate - 2.5758 * se_ssynth
                                )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  log_tax_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_log_tax_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_log_tax_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_log_tax_sa20$coeftable[,1],
           sa_se =es_PSM_log_tax_sa20$coeftable[,2],
           sa_t =es_PSM_log_tax_sa20$coeftable[,3],
           sa_p =es_PSM_log_tax_sa20$coeftable[,4])
  log_tax_summary_appendix_cs <- data.frame(year = cs_log_tax_smry$egt,
                                            cs =  cs_log_tax_smry$att.egt,
                                            cs_se = cs_log_tax_smry$se.egt,
                                            cs_cband_lower = cs_log_tax_smry$att.egt - cs_log_tax_smry$crit.val.egt*cs_log_tax_smry$se.egt,
                                            cs_cband_upper = cs_log_tax_smry$att.egt + cs_log_tax_smry$crit.val.egt*cs_log_tax_smry$se.egt)
  log_tax_summary_appendix <- right_join(log_tax_summary_appendix_sa,log_tax_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_log_tax_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- log_tax_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(log_tax_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- log_tax_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(log_tax_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  log_tax_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  log_tax_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(log_tax_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(log_tax_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  log_tax_summary <- cbind(log_tax_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(log_tax_summary$rob)
  
  p_log_tax_summary <- ggplot(log_tax_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_tax_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_tax_summary$pretrend[1]) +
    geom_rect(data=log_tax_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_tax_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_tax_summary$down, na.rm=T),max(log_tax_summary$up, na.rm=T)), max(-min(log_tax_summary$down, na.rm=T),max(log_tax_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_tax <- p_PSM_log_tax_sa20+p_log_tax_cs+p_log_tax_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_log_tax)
  ################## log_office------------
  # ipw
  es_PSM_log_office_sa20 = feols(log_office ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_log_office_sa20 <- aggregate(es_PSM_log_office_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_office_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_office_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_office", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_office <- att_gt(yname = "log_office",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                          xformla = formula_cs, 
                          est_method = "ipw",base_period="universal",alp=0.05,
                          data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                          #print_details=FALSE,
                          anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_office_smry <- aggte(cs_log_office, type = "dynamic",na.rm = TRUE)
  
  p_log_office_cs <- ggdid(cs_log_office_smry, title = "log_office")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_log_office_cs <- aggte(cs_log_office, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  log_office_summary <- data.frame(method = method,
                                   estimate = c(att_PSM_log_office_sa20[1], a_log_office_cs$overall.att#,a_log_office_Ssynth$att_estimate
                                   ),
                                   se = c(att_PSM_log_office_sa20[2],a_log_office_cs$overall.se#,se_ssynth
                                   ),
                                   up = c(att_PSM_log_office_sa20[1] + 1.645*att_PSM_log_office_sa20[2],a_log_office_cs$overall.att + 1.645*a_log_office_cs$overall.se#, a_log_office_Ssynth$att_estimate + 1.645 * se_ssynth
                                   ),
                                   down = c(att_PSM_log_office_sa20[1] - 1.645*att_PSM_log_office_sa20[2],a_log_office_cs$overall.att - 1.645*a_log_office_cs$overall.se#, a_log_office_Ssynth$att_estimate - 1.645 * se_ssynth
                                   ),
                                   up95 = c(att_PSM_log_office_sa20[1] + 1.960*att_PSM_log_office_sa20[2],a_log_office_cs$overall.att + 1.960*a_log_office_cs$overall.se#,  a_log_office_Ssynth$att_estimate + 1.960 * se_ssynth
                                   ),
                                   down95 = c(att_PSM_log_office_sa20[1] - 1.960*att_PSM_log_office_sa20[2],a_log_office_cs$overall.att - 1.960*a_log_office_cs$overall.se#,  a_log_office_Ssynth$att_estimate - 1.960 * se_ssynth
                                   ),
                                   up99 = c(att_PSM_log_office_sa20[1] + 2.5758*att_PSM_log_office_sa20[2],a_log_office_cs$overall.att + 2.5758*a_log_office_cs$overall.se#,  a_log_office_Ssynth$att_estimate + 2.5758 * se_ssynth
                                   ),
                                   down99 = c(att_PSM_log_office_sa20[1] - 2.5758*att_PSM_log_office_sa20[2],a_log_office_cs$overall.att - 2.5758*a_log_office_cs$overall.se#,  a_log_office_Ssynth$att_estimate - 2.5758 * se_ssynth
                                   )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  log_office_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_log_office_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_log_office_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_log_office_sa20$coeftable[,1],
           sa_se =es_PSM_log_office_sa20$coeftable[,2],
           sa_t =es_PSM_log_office_sa20$coeftable[,3],
           sa_p =es_PSM_log_office_sa20$coeftable[,4])
  log_office_summary_appendix_cs <- data.frame(year = cs_log_office_smry$egt,
                                               cs =  cs_log_office_smry$att.egt,
                                               cs_se = cs_log_office_smry$se.egt,
                                               cs_cband_lower = cs_log_office_smry$att.egt - cs_log_office_smry$crit.val.egt*cs_log_office_smry$se.egt,
                                               cs_cband_upper = cs_log_office_smry$att.egt + cs_log_office_smry$crit.val.egt*cs_log_office_smry$se.egt)
  log_office_summary_appendix <- right_join(log_office_summary_appendix_sa,log_office_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_log_office_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- log_office_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(log_office_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- log_office_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(log_office_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  log_office_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  log_office_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(log_office_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(log_office_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  log_office_summary <- cbind(log_office_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(log_office_summary$rob)
  
  p_log_office_summary <- ggplot(log_office_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_office_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_office_summary$pretrend[1]) +
    geom_rect(data=log_office_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_office_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_office_summary$down, na.rm=T),max(log_office_summary$up, na.rm=T)), max(-min(log_office_summary$down, na.rm=T),max(log_office_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_office <- p_PSM_log_office_sa20+p_log_office_cs+p_log_office_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_log_office)
  ################## ROA------------
  # ipw
  es_PSM_ROA_sa20 = feols(ROA ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_ROA_sa20 <- aggregate(es_PSM_ROA_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_ROA_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_ROA_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14),ylim = c(-0.1, 0.1)) + 
    labs(title = "ROA", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_ROA <- att_gt(yname = "ROA",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                   xformla = formula_cs, 
                   est_method = "ipw",base_period="universal",alp=0.05,
                   data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                   #print_details=FALSE,
                   anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_ROA_smry <- aggte(cs_ROA, type = "dynamic",na.rm = TRUE)
  
  p_ROA_cs <- ggdid(cs_ROA_smry, title = "ROA")+coord_cartesian(xlim = c(-10,14),ylim = c(-0.1, 0.1))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_ROA_cs <- aggte(cs_ROA, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  ROA_summary <- data.frame(method = method,
                            estimate = c(att_PSM_ROA_sa20[1], a_ROA_cs$overall.att#,a_ROA_Ssynth$att_estimate
                            ),
                            se = c(att_PSM_ROA_sa20[2],a_ROA_cs$overall.se#,se_ssynth
                            ),
                            up = c(att_PSM_ROA_sa20[1] + 1.645*att_PSM_ROA_sa20[2],a_ROA_cs$overall.att + 1.645*a_ROA_cs$overall.se#, a_ROA_Ssynth$att_estimate + 1.645 * se_ssynth
                            ),
                            down = c(att_PSM_ROA_sa20[1] - 1.645*att_PSM_ROA_sa20[2],a_ROA_cs$overall.att - 1.645*a_ROA_cs$overall.se#, a_ROA_Ssynth$att_estimate - 1.645 * se_ssynth
                            ),
                            up95 = c(att_PSM_ROA_sa20[1] + 1.960*att_PSM_ROA_sa20[2],a_ROA_cs$overall.att + 1.960*a_ROA_cs$overall.se#,  a_ROA_Ssynth$att_estimate + 1.960 * se_ssynth
                            ),
                            down95 = c(att_PSM_ROA_sa20[1] - 1.960*att_PSM_ROA_sa20[2],a_ROA_cs$overall.att - 1.960*a_ROA_cs$overall.se#,  a_ROA_Ssynth$att_estimate - 1.960 * se_ssynth
                            ),
                            up99 = c(att_PSM_ROA_sa20[1] + 2.5758*att_PSM_ROA_sa20[2],a_ROA_cs$overall.att + 2.5758*a_ROA_cs$overall.se#,  a_ROA_Ssynth$att_estimate + 2.5758 * se_ssynth
                            ),
                            down99 = c(att_PSM_ROA_sa20[1] - 2.5758*att_PSM_ROA_sa20[2],a_ROA_cs$overall.att - 2.5758*a_ROA_cs$overall.se#,  a_ROA_Ssynth$att_estimate - 2.5758 * se_ssynth
                            )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  ROA_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_ROA_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_ROA_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_ROA_sa20$coeftable[,1],
           sa_se =es_PSM_ROA_sa20$coeftable[,2],
           sa_t =es_PSM_ROA_sa20$coeftable[,3],
           sa_p =es_PSM_ROA_sa20$coeftable[,4])
  ROA_summary_appendix_cs <- data.frame(year = cs_ROA_smry$egt,
                                        cs =  cs_ROA_smry$att.egt,
                                        cs_se = cs_ROA_smry$se.egt,
                                        cs_cband_lower = cs_ROA_smry$att.egt - cs_ROA_smry$crit.val.egt*cs_ROA_smry$se.egt,
                                        cs_cband_upper = cs_ROA_smry$att.egt + cs_ROA_smry$crit.val.egt*cs_ROA_smry$se.egt)
  ROA_summary_appendix <- right_join(ROA_summary_appendix_sa,ROA_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_ROA_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- ROA_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(ROA_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- ROA_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(ROA_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  ROA_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  ROA_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(ROA_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(ROA_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  ROA_summary <- cbind(ROA_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(ROA_summary$rob)
  
  p_ROA_summary <- ggplot(ROA_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=ROA_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*ROA_summary$pretrend[1]) +
    geom_rect(data=ROA_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*ROA_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(ROA_summary$down, na.rm=T),max(ROA_summary$up, na.rm=T)), max(-min(ROA_summary$down, na.rm=T),max(ROA_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_ROA <- p_PSM_ROA_sa20+p_ROA_cs+p_ROA_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_ROA)
  ################## net_profit_workers------------
  # ipw
  es_PSM_net_profit_workers_sa20 = feols(net_profit_workers ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_net_profit_workers_sa20 <- aggregate(es_PSM_net_profit_workers_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_net_profit_workers_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_net_profit_workers_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14),ylim = c(-5000000, 5000000)) + 
    labs(title = "net_profit_workers", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_net_profit_workers <- att_gt(yname = "net_profit_workers",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                                  xformla = formula_cs, 
                                  est_method = "ipw",base_period="universal",alp=0.05,
                                  data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                                  #print_details=FALSE,
                                  anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_net_profit_workers_smry <- aggte(cs_net_profit_workers, type = "dynamic",na.rm = TRUE)
  
  p_net_profit_workers_cs <- ggdid(cs_net_profit_workers_smry, title = "net_profit_workers")+coord_cartesian(xlim = c(-10,14),ylim = c(-5000000, 5000000))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_net_profit_workers_cs <- aggte(cs_net_profit_workers, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  net_profit_workers_summary <- data.frame(method = method,
                                           estimate = c(att_PSM_net_profit_workers_sa20[1], a_net_profit_workers_cs$overall.att#,a_net_profit_workers_Ssynth$att_estimate
                                           ),
                                           se = c(att_PSM_net_profit_workers_sa20[2],a_net_profit_workers_cs$overall.se#,se_ssynth
                                           ),
                                           up = c(att_PSM_net_profit_workers_sa20[1] + 1.645*att_PSM_net_profit_workers_sa20[2],a_net_profit_workers_cs$overall.att + 1.645*a_net_profit_workers_cs$overall.se#, a_net_profit_workers_Ssynth$att_estimate + 1.645 * se_ssynth
                                           ),
                                           down = c(att_PSM_net_profit_workers_sa20[1] - 1.645*att_PSM_net_profit_workers_sa20[2],a_net_profit_workers_cs$overall.att - 1.645*a_net_profit_workers_cs$overall.se#, a_net_profit_workers_Ssynth$att_estimate - 1.645 * se_ssynth
                                           ),
                                           up95 = c(att_PSM_net_profit_workers_sa20[1] + 1.960*att_PSM_net_profit_workers_sa20[2],a_net_profit_workers_cs$overall.att + 1.960*a_net_profit_workers_cs$overall.se#,  a_net_profit_workers_Ssynth$att_estimate + 1.960 * se_ssynth
                                           ),
                                           down95 = c(att_PSM_net_profit_workers_sa20[1] - 1.960*att_PSM_net_profit_workers_sa20[2],a_net_profit_workers_cs$overall.att - 1.960*a_net_profit_workers_cs$overall.se#,  a_net_profit_workers_Ssynth$att_estimate - 1.960 * se_ssynth
                                           ),
                                           up99 = c(att_PSM_net_profit_workers_sa20[1] + 2.5758*att_PSM_net_profit_workers_sa20[2],a_net_profit_workers_cs$overall.att + 2.5758*a_net_profit_workers_cs$overall.se#,  a_net_profit_workers_Ssynth$att_estimate + 2.5758 * se_ssynth
                                           ),
                                           down99 = c(att_PSM_net_profit_workers_sa20[1] - 2.5758*att_PSM_net_profit_workers_sa20[2],a_net_profit_workers_cs$overall.att - 2.5758*a_net_profit_workers_cs$overall.se#,  a_net_profit_workers_Ssynth$att_estimate - 2.5758 * se_ssynth
                                           )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  net_profit_workers_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_net_profit_workers_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_net_profit_workers_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_net_profit_workers_sa20$coeftable[,1],
           sa_se =es_PSM_net_profit_workers_sa20$coeftable[,2],
           sa_t =es_PSM_net_profit_workers_sa20$coeftable[,3],
           sa_p =es_PSM_net_profit_workers_sa20$coeftable[,4])
  net_profit_workers_summary_appendix_cs <- data.frame(year = cs_net_profit_workers_smry$egt,
                                                       cs =  cs_net_profit_workers_smry$att.egt,
                                                       cs_se = cs_net_profit_workers_smry$se.egt,
                                                       cs_cband_lower = cs_net_profit_workers_smry$att.egt - cs_net_profit_workers_smry$crit.val.egt*cs_net_profit_workers_smry$se.egt,
                                                       cs_cband_upper = cs_net_profit_workers_smry$att.egt + cs_net_profit_workers_smry$crit.val.egt*cs_net_profit_workers_smry$se.egt)
  net_profit_workers_summary_appendix <- right_join(net_profit_workers_summary_appendix_sa,net_profit_workers_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_net_profit_workers_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- net_profit_workers_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(net_profit_workers_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- net_profit_workers_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(net_profit_workers_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  net_profit_workers_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  net_profit_workers_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(net_profit_workers_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(net_profit_workers_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  net_profit_workers_summary <- cbind(net_profit_workers_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(net_profit_workers_summary$rob)
  
  p_net_profit_workers_summary <- ggplot(net_profit_workers_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=net_profit_workers_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*net_profit_workers_summary$pretrend[1]) +
    geom_rect(data=net_profit_workers_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*net_profit_workers_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(net_profit_workers_summary$down, na.rm=T),max(net_profit_workers_summary$up, na.rm=T)), max(-min(net_profit_workers_summary$down, na.rm=T),max(net_profit_workers_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_net_profit_workers <- p_PSM_net_profit_workers_sa20+p_net_profit_workers_cs+p_net_profit_workers_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_net_profit_workers)
  ## output_business-----------
  pa_business_sum <- pa_log_sales/pa_log_tax/pa_log_office/pa_ROA/pa_net_profit_workers + plot_annotation(
    title = "Effect on the Business",
    subtitle = target,
    ##caption = caption_main
  ) &
    theme(text = element_text(family="Meiryo UI"))
  
  # filename <- paste("03_robustness_output/", target,"_01business_", count_intervention, ".png", sep="")
  # ggsave(filename,pa_log_business_sum, 
  #        width=20, height =22.5, units = "cm", dpi=200) 
  
  filename_pdf <- paste("03_robustness_output/", target,"_33_trimmed_business_", count_intervention, ".pdf", sep="")
  ggsave(filename_pdf,pa_business_sum, 
         width=15, height =30, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("03_robustness_output/", target,"_33_trimmed_business_", count_intervention, ".png", sep="")
  ggsave(filename_png,pa_business_sum, 
         width=15, height =30, units = "cm", dpi=200) 
  
  
  write.xlsx(list("log_sales"=log_sales_summary,"log_tax"=log_tax_summary,"log_office"=log_office_summary,"ROA"=ROA_summary,"net_profit_workers"=net_profit_workers_summary), paste("03_robustness_output/",target,"_33business_trimmed_table.xlsx", sep=""))
  write.xlsx(list("log_sales"=log_sales_summary_appendix,"log_tax"=log_tax_summary_appendix,"log_office"=log_office_summary_appendix,"ROA"=ROA_summary_appendix,"net_profit_workers"=net_profit_workers_summary_appendix), paste("03_robustness_output/",target,"_33business_trimmed_table_appendix.xlsx"))
}
robustness_trimmed_trade <- function(target){
  ################## flag_export------------
  # ipw
  es_PSM_flag_export_sa20 = feols(flag_export ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_flag_export_sa20 <- aggregate(es_PSM_flag_export_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_flag_export_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_flag_export_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + 
    labs(title = "flag_export", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_flag_export <- att_gt(yname = "flag_export",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                           xformla = formula_cs, 
                           est_method = "ipw",base_period="universal",alp=0.05,
                           data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                           #print_details=FALSE,
                           anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_flag_export_smry <- aggte(cs_flag_export, type = "dynamic",na.rm = TRUE)
  
  p_flag_export_cs <- ggdid(cs_flag_export_smry, title = "flag_export")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_flag_export_cs <- aggte(cs_flag_export, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  flag_export_summary <- data.frame(method = method,
                                    estimate = c(att_PSM_flag_export_sa20[1], a_flag_export_cs$overall.att#,a_flag_export_Ssynth$att_estimate
                                    ),
                                    se = c(att_PSM_flag_export_sa20[2],a_flag_export_cs$overall.se#,se_ssynth
                                    ),
                                    up = c(att_PSM_flag_export_sa20[1] + 1.645*att_PSM_flag_export_sa20[2],a_flag_export_cs$overall.att + 1.645*a_flag_export_cs$overall.se#, a_flag_export_Ssynth$att_estimate + 1.645 * se_ssynth
                                    ),
                                    down = c(att_PSM_flag_export_sa20[1] - 1.645*att_PSM_flag_export_sa20[2],a_flag_export_cs$overall.att - 1.645*a_flag_export_cs$overall.se#, a_flag_export_Ssynth$att_estimate - 1.645 * se_ssynth
                                    ),
                                    up95 = c(att_PSM_flag_export_sa20[1] + 1.960*att_PSM_flag_export_sa20[2],a_flag_export_cs$overall.att + 1.960*a_flag_export_cs$overall.se#,  a_flag_export_Ssynth$att_estimate + 1.960 * se_ssynth
                                    ),
                                    down95 = c(att_PSM_flag_export_sa20[1] - 1.960*att_PSM_flag_export_sa20[2],a_flag_export_cs$overall.att - 1.960*a_flag_export_cs$overall.se#,  a_flag_export_Ssynth$att_estimate - 1.960 * se_ssynth
                                    ),
                                    up99 = c(att_PSM_flag_export_sa20[1] + 2.5758*att_PSM_flag_export_sa20[2],a_flag_export_cs$overall.att + 2.5758*a_flag_export_cs$overall.se#,  a_flag_export_Ssynth$att_estimate + 2.5758 * se_ssynth
                                    ),
                                    down99 = c(att_PSM_flag_export_sa20[1] - 2.5758*att_PSM_flag_export_sa20[2],a_flag_export_cs$overall.att - 2.5758*a_flag_export_cs$overall.se#,  a_flag_export_Ssynth$att_estimate - 2.5758 * se_ssynth
                                    )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  flag_export_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_flag_export_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_flag_export_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_flag_export_sa20$coeftable[,1],
           sa_se =es_PSM_flag_export_sa20$coeftable[,2],
           sa_t =es_PSM_flag_export_sa20$coeftable[,3],
           sa_p =es_PSM_flag_export_sa20$coeftable[,4])
  flag_export_summary_appendix_cs <- data.frame(year = cs_flag_export_smry$egt,
                                                cs =  cs_flag_export_smry$att.egt,
                                                cs_se = cs_flag_export_smry$se.egt,
                                                cs_cband_lower = cs_flag_export_smry$att.egt - cs_flag_export_smry$crit.val.egt*cs_flag_export_smry$se.egt,
                                                cs_cband_upper = cs_flag_export_smry$att.egt + cs_flag_export_smry$crit.val.egt*cs_flag_export_smry$se.egt)
  flag_export_summary_appendix <- right_join(flag_export_summary_appendix_sa,flag_export_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_flag_export_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- flag_export_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(flag_export_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- flag_export_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(flag_export_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  flag_export_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  flag_export_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(flag_export_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(flag_export_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  flag_export_summary <- cbind(flag_export_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(flag_export_summary$rob)
  
  p_flag_export_summary <- ggplot(flag_export_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=flag_export_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_export_summary$pretrend[1]) +
    geom_rect(data=flag_export_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_export_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(flag_export_summary$down, na.rm=T),max(flag_export_summary$up, na.rm=T)), max(-min(flag_export_summary$down, na.rm=T),max(flag_export_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_flag_export <- p_PSM_flag_export_sa20+p_flag_export_cs+p_flag_export_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_flag_export)
  ################## flag_import------------
  # ipw
  es_PSM_flag_import_sa20 = feols(flag_import ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_flag_import_sa20 <- aggregate(es_PSM_flag_import_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_flag_import_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_flag_import_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + 
    labs(title = "flag_import", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_flag_import <- att_gt(yname = "flag_import",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                           xformla = formula_cs, 
                           est_method = "ipw",base_period="universal",alp=0.05,
                           data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                           #print_details=FALSE,
                           anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_flag_import_smry <- aggte(cs_flag_import, type = "dynamic",na.rm = TRUE)
  
  p_flag_import_cs <- ggdid(cs_flag_import_smry, title = "flag_import")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_flag_import_cs <- aggte(cs_flag_import, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  flag_import_summary <- data.frame(method = method,
                                    estimate = c(att_PSM_flag_import_sa20[1], a_flag_import_cs$overall.att#,a_flag_import_Ssynth$att_estimate
                                    ),
                                    se = c(att_PSM_flag_import_sa20[2],a_flag_import_cs$overall.se#,se_ssynth
                                    ),
                                    up = c(att_PSM_flag_import_sa20[1] + 1.645*att_PSM_flag_import_sa20[2],a_flag_import_cs$overall.att + 1.645*a_flag_import_cs$overall.se#, a_flag_import_Ssynth$att_estimate + 1.645 * se_ssynth
                                    ),
                                    down = c(att_PSM_flag_import_sa20[1] - 1.645*att_PSM_flag_import_sa20[2],a_flag_import_cs$overall.att - 1.645*a_flag_import_cs$overall.se#, a_flag_import_Ssynth$att_estimate - 1.645 * se_ssynth
                                    ),
                                    up95 = c(att_PSM_flag_import_sa20[1] + 1.960*att_PSM_flag_import_sa20[2],a_flag_import_cs$overall.att + 1.960*a_flag_import_cs$overall.se#,  a_flag_import_Ssynth$att_estimate + 1.960 * se_ssynth
                                    ),
                                    down95 = c(att_PSM_flag_import_sa20[1] - 1.960*att_PSM_flag_import_sa20[2],a_flag_import_cs$overall.att - 1.960*a_flag_import_cs$overall.se#,  a_flag_import_Ssynth$att_estimate - 1.960 * se_ssynth
                                    ),
                                    up99 = c(att_PSM_flag_import_sa20[1] + 2.5758*att_PSM_flag_import_sa20[2],a_flag_import_cs$overall.att + 2.5758*a_flag_import_cs$overall.se#,  a_flag_import_Ssynth$att_estimate + 2.5758 * se_ssynth
                                    ),
                                    down99 = c(att_PSM_flag_import_sa20[1] - 2.5758*att_PSM_flag_import_sa20[2],a_flag_import_cs$overall.att - 2.5758*a_flag_import_cs$overall.se#,  a_flag_import_Ssynth$att_estimate - 2.5758 * se_ssynth
                                    )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  flag_import_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_flag_import_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_flag_import_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_flag_import_sa20$coeftable[,1],
           sa_se =es_PSM_flag_import_sa20$coeftable[,2],
           sa_t =es_PSM_flag_import_sa20$coeftable[,3],
           sa_p =es_PSM_flag_import_sa20$coeftable[,4])
  flag_import_summary_appendix_cs <- data.frame(year = cs_flag_import_smry$egt,
                                                cs =  cs_flag_import_smry$att.egt,
                                                cs_se = cs_flag_import_smry$se.egt,
                                                cs_cband_lower = cs_flag_import_smry$att.egt - cs_flag_import_smry$crit.val.egt*cs_flag_import_smry$se.egt,
                                                cs_cband_upper = cs_flag_import_smry$att.egt + cs_flag_import_smry$crit.val.egt*cs_flag_import_smry$se.egt)
  flag_import_summary_appendix <- right_join(flag_import_summary_appendix_sa,flag_import_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_flag_import_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- flag_import_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(flag_import_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- flag_import_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(flag_import_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  flag_import_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  flag_import_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(flag_import_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(flag_import_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  flag_import_summary <- cbind(flag_import_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(flag_import_summary$rob)
  
  p_flag_import_summary <- ggplot(flag_import_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=flag_import_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_import_summary$pretrend[1]) +
    geom_rect(data=flag_import_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_import_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(flag_import_summary$down, na.rm=T),max(flag_import_summary$up, na.rm=T)), max(-min(flag_import_summary$down, na.rm=T),max(flag_import_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_flag_import <- p_PSM_flag_import_sa20+p_flag_import_cs+p_flag_import_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_flag_import)
  ################## log_export------------
  # ipw
  es_PSM_log_export_sa20 = feols(log_export ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_log_export_sa20 <- aggregate(es_PSM_log_export_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_export_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_export_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_export", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_export <- att_gt(yname = "log_export",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                          xformla = formula_cs, 
                          est_method = "ipw",base_period="universal",alp=0.05,
                          data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                          #print_details=FALSE,
                          anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_export_smry <- aggte(cs_log_export, type = "dynamic",na.rm = TRUE)
  
  p_log_export_cs <- ggdid(cs_log_export_smry, title = "log_export")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_log_export_cs <- aggte(cs_log_export, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  log_export_summary <- data.frame(method = method,
                                   estimate = c(att_PSM_log_export_sa20[1], a_log_export_cs$overall.att#,a_log_export_Ssynth$att_estimate
                                   ),
                                   se = c(att_PSM_log_export_sa20[2],a_log_export_cs$overall.se#,se_ssynth
                                   ),
                                   up = c(att_PSM_log_export_sa20[1] + 1.645*att_PSM_log_export_sa20[2],a_log_export_cs$overall.att + 1.645*a_log_export_cs$overall.se#, a_log_export_Ssynth$att_estimate + 1.645 * se_ssynth
                                   ),
                                   down = c(att_PSM_log_export_sa20[1] - 1.645*att_PSM_log_export_sa20[2],a_log_export_cs$overall.att - 1.645*a_log_export_cs$overall.se#, a_log_export_Ssynth$att_estimate - 1.645 * se_ssynth
                                   ),
                                   up95 = c(att_PSM_log_export_sa20[1] + 1.960*att_PSM_log_export_sa20[2],a_log_export_cs$overall.att + 1.960*a_log_export_cs$overall.se#,  a_log_export_Ssynth$att_estimate + 1.960 * se_ssynth
                                   ),
                                   down95 = c(att_PSM_log_export_sa20[1] - 1.960*att_PSM_log_export_sa20[2],a_log_export_cs$overall.att - 1.960*a_log_export_cs$overall.se#,  a_log_export_Ssynth$att_estimate - 1.960 * se_ssynth
                                   ),
                                   up99 = c(att_PSM_log_export_sa20[1] + 2.5758*att_PSM_log_export_sa20[2],a_log_export_cs$overall.att + 2.5758*a_log_export_cs$overall.se#,  a_log_export_Ssynth$att_estimate + 2.5758 * se_ssynth
                                   ),
                                   down99 = c(att_PSM_log_export_sa20[1] - 2.5758*att_PSM_log_export_sa20[2],a_log_export_cs$overall.att - 2.5758*a_log_export_cs$overall.se#,  a_log_export_Ssynth$att_estimate - 2.5758 * se_ssynth
                                   )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  log_export_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_log_export_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_log_export_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_log_export_sa20$coeftable[,1],
           sa_se =es_PSM_log_export_sa20$coeftable[,2],
           sa_t =es_PSM_log_export_sa20$coeftable[,3],
           sa_p =es_PSM_log_export_sa20$coeftable[,4])
  log_export_summary_appendix_cs <- data.frame(year = cs_log_export_smry$egt,
                                               cs =  cs_log_export_smry$att.egt,
                                               cs_se = cs_log_export_smry$se.egt,
                                               cs_cband_lower = cs_log_export_smry$att.egt - cs_log_export_smry$crit.val.egt*cs_log_export_smry$se.egt,
                                               cs_cband_upper = cs_log_export_smry$att.egt + cs_log_export_smry$crit.val.egt*cs_log_export_smry$se.egt)
  log_export_summary_appendix <- right_join(log_export_summary_appendix_sa,log_export_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_log_export_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- log_export_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(log_export_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- log_export_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(log_export_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  log_export_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  log_export_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(log_export_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(log_export_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  log_export_summary <- cbind(log_export_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(log_export_summary$rob)
  
  p_log_export_summary <- ggplot(log_export_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_export_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_export_summary$pretrend[1]) +
    geom_rect(data=log_export_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_export_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_export_summary$down, na.rm=T),max(log_export_summary$up, na.rm=T)), max(-min(log_export_summary$down, na.rm=T),max(log_export_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_export <- p_PSM_log_export_sa20+p_log_export_cs+p_log_export_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_log_export)
  ################## log_import------------
  # ipw
  es_PSM_log_import_sa20 = feols(log_import ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_log_import_sa20 <- aggregate(es_PSM_log_import_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_import_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_import_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_import", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_import <- att_gt(yname = "log_import",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                          xformla = formula_cs, 
                          est_method = "ipw",base_period="universal",alp=0.05,
                          data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                          #print_details=FALSE,
                          anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_import_smry <- aggte(cs_log_import, type = "dynamic",na.rm = TRUE)
  
  p_log_import_cs <- ggdid(cs_log_import_smry, title = "log_import")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_log_import_cs <- aggte(cs_log_import, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  log_import_summary <- data.frame(method = method,
                                   estimate = c(att_PSM_log_import_sa20[1], a_log_import_cs$overall.att#,a_log_import_Ssynth$att_estimate
                                   ),
                                   se = c(att_PSM_log_import_sa20[2],a_log_import_cs$overall.se#,se_ssynth
                                   ),
                                   up = c(att_PSM_log_import_sa20[1] + 1.645*att_PSM_log_import_sa20[2],a_log_import_cs$overall.att + 1.645*a_log_import_cs$overall.se#, a_log_import_Ssynth$att_estimate + 1.645 * se_ssynth
                                   ),
                                   down = c(att_PSM_log_import_sa20[1] - 1.645*att_PSM_log_import_sa20[2],a_log_import_cs$overall.att - 1.645*a_log_import_cs$overall.se#, a_log_import_Ssynth$att_estimate - 1.645 * se_ssynth
                                   ),
                                   up95 = c(att_PSM_log_import_sa20[1] + 1.960*att_PSM_log_import_sa20[2],a_log_import_cs$overall.att + 1.960*a_log_import_cs$overall.se#,  a_log_import_Ssynth$att_estimate + 1.960 * se_ssynth
                                   ),
                                   down95 = c(att_PSM_log_import_sa20[1] - 1.960*att_PSM_log_import_sa20[2],a_log_import_cs$overall.att - 1.960*a_log_import_cs$overall.se#,  a_log_import_Ssynth$att_estimate - 1.960 * se_ssynth
                                   ),
                                   up99 = c(att_PSM_log_import_sa20[1] + 2.5758*att_PSM_log_import_sa20[2],a_log_import_cs$overall.att + 2.5758*a_log_import_cs$overall.se#,  a_log_import_Ssynth$att_estimate + 2.5758 * se_ssynth
                                   ),
                                   down99 = c(att_PSM_log_import_sa20[1] - 2.5758*att_PSM_log_import_sa20[2],a_log_import_cs$overall.att - 2.5758*a_log_import_cs$overall.se#,  a_log_import_Ssynth$att_estimate - 2.5758 * se_ssynth
                                   )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  log_import_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_log_import_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_log_import_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_log_import_sa20$coeftable[,1],
           sa_se =es_PSM_log_import_sa20$coeftable[,2],
           sa_t =es_PSM_log_import_sa20$coeftable[,3],
           sa_p =es_PSM_log_import_sa20$coeftable[,4])
  log_import_summary_appendix_cs <- data.frame(year = cs_log_import_smry$egt,
                                               cs =  cs_log_import_smry$att.egt,
                                               cs_se = cs_log_import_smry$se.egt,
                                               cs_cband_lower = cs_log_import_smry$att.egt - cs_log_import_smry$crit.val.egt*cs_log_import_smry$se.egt,
                                               cs_cband_upper = cs_log_import_smry$att.egt + cs_log_import_smry$crit.val.egt*cs_log_import_smry$se.egt)
  log_import_summary_appendix <- right_join(log_import_summary_appendix_sa,log_import_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_log_import_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- log_import_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(log_import_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- log_import_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(log_import_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  log_import_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  log_import_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(log_import_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(log_import_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  log_import_summary <- cbind(log_import_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(log_import_summary$rob)
  
  p_log_import_summary <- ggplot(log_import_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_import_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_import_summary$pretrend[1]) +
    geom_rect(data=log_import_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_import_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_import_summary$down, na.rm=T),max(log_import_summary$up, na.rm=T)), max(-min(log_import_summary$down, na.rm=T),max(log_import_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_import <- p_PSM_log_import_sa20+p_log_import_cs+p_log_import_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_log_import)
  ## output_trade-----------
  pa_trade_sum <- pa_flag_export/pa_flag_import/pa_log_export/pa_log_import + plot_annotation(
    title = "Effect on the International Trade",
    subtitle = target,
    ##caption = caption_main
  ) &
    theme(text = element_text(family="Meiryo UI"))
  
  # filename <- paste("03_robustness_output/", target,"_01trade_", count_intervention, ".png", sep="")
  # ggsave(filename,pa_log_trade_sum, 
  #        width=20, height =22.5, units = "cm", dpi=200) 
  
  filename_pdf <- paste("03_robustness_output/", target,"_34_trimmed_trade_", count_intervention, ".pdf", sep="")
  ggsave(filename_pdf,pa_trade_sum, 
         width=15, height =24, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("03_robustness_output/", target,"_34_trimmed_trade_", count_intervention, ".png", sep="")
  ggsave(filename_png,pa_trade_sum, 
         width=15, height =24, units = "cm", dpi=200) 
  
  
  write.xlsx(list("flag_export"=flag_export_summary,"flag_import"=flag_import_summary,"log_export"=log_export_summary,"log_import"=log_import_summary), paste("03_robustness_output/",target,"_34trade_trimmed_table.xlsx", sep=""))
  write.xlsx(list("flag_export"=flag_export_summary_appendix,"flag_import"=flag_import_summary_appendix,"log_export"=log_export_summary_appendix,"log_import"=log_import_summary_appendix), paste("03_robustness_output/",target,"_34trade_trimmed_table_appendix.xlsx"))
}
robustness_trimmed_trainingRD <- function(target){
  ################## flag_training------------
  # ipw
  es_PSM_flag_training_sa20 = feols(flag_training ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_flag_training_sa20 <- aggregate(es_PSM_flag_training_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_flag_training_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_flag_training_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + 
    labs(title = "flag_training", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_flag_training <- att_gt(yname = "flag_training",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                             xformla = formula_cs, 
                             est_method = "ipw",base_period="universal",alp=0.05,
                             data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                             #print_details=FALSE,
                             anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_flag_training_smry <- aggte(cs_flag_training, type = "dynamic",na.rm = TRUE)
  
  p_flag_training_cs <- ggdid(cs_flag_training_smry, title = "flag_training")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_flag_training_cs <- aggte(cs_flag_training, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  flag_training_summary <- data.frame(method = method,
                                      estimate = c(att_PSM_flag_training_sa20[1], a_flag_training_cs$overall.att#,a_flag_training_Ssynth$att_estimate
                                      ),
                                      se = c(att_PSM_flag_training_sa20[2],a_flag_training_cs$overall.se#,se_ssynth
                                      ),
                                      up = c(att_PSM_flag_training_sa20[1] + 1.645*att_PSM_flag_training_sa20[2],a_flag_training_cs$overall.att + 1.645*a_flag_training_cs$overall.se#, a_flag_training_Ssynth$att_estimate + 1.645 * se_ssynth
                                      ),
                                      down = c(att_PSM_flag_training_sa20[1] - 1.645*att_PSM_flag_training_sa20[2],a_flag_training_cs$overall.att - 1.645*a_flag_training_cs$overall.se#, a_flag_training_Ssynth$att_estimate - 1.645 * se_ssynth
                                      ),
                                      up95 = c(att_PSM_flag_training_sa20[1] + 1.960*att_PSM_flag_training_sa20[2],a_flag_training_cs$overall.att + 1.960*a_flag_training_cs$overall.se#,  a_flag_training_Ssynth$att_estimate + 1.960 * se_ssynth
                                      ),
                                      down95 = c(att_PSM_flag_training_sa20[1] - 1.960*att_PSM_flag_training_sa20[2],a_flag_training_cs$overall.att - 1.960*a_flag_training_cs$overall.se#,  a_flag_training_Ssynth$att_estimate - 1.960 * se_ssynth
                                      ),
                                      up99 = c(att_PSM_flag_training_sa20[1] + 2.5758*att_PSM_flag_training_sa20[2],a_flag_training_cs$overall.att + 2.5758*a_flag_training_cs$overall.se#,  a_flag_training_Ssynth$att_estimate + 2.5758 * se_ssynth
                                      ),
                                      down99 = c(att_PSM_flag_training_sa20[1] - 2.5758*att_PSM_flag_training_sa20[2],a_flag_training_cs$overall.att - 2.5758*a_flag_training_cs$overall.se#,  a_flag_training_Ssynth$att_estimate - 2.5758 * se_ssynth
                                      )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  flag_training_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_flag_training_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_flag_training_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_flag_training_sa20$coeftable[,1],
           sa_se =es_PSM_flag_training_sa20$coeftable[,2],
           sa_t =es_PSM_flag_training_sa20$coeftable[,3],
           sa_p =es_PSM_flag_training_sa20$coeftable[,4])
  flag_training_summary_appendix_cs <- data.frame(year = cs_flag_training_smry$egt,
                                                  cs =  cs_flag_training_smry$att.egt,
                                                  cs_se = cs_flag_training_smry$se.egt,
                                                  cs_cband_lower = cs_flag_training_smry$att.egt - cs_flag_training_smry$crit.val.egt*cs_flag_training_smry$se.egt,
                                                  cs_cband_upper = cs_flag_training_smry$att.egt + cs_flag_training_smry$crit.val.egt*cs_flag_training_smry$se.egt)
  flag_training_summary_appendix <- right_join(flag_training_summary_appendix_sa,flag_training_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_flag_training_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- flag_training_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(flag_training_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- flag_training_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(flag_training_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  flag_training_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  flag_training_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(flag_training_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(flag_training_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  flag_training_summary <- cbind(flag_training_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(flag_training_summary$rob)
  
  p_flag_training_summary <- ggplot(flag_training_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=flag_training_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_training_summary$pretrend[1]) +
    geom_rect(data=flag_training_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_training_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(flag_training_summary$down, na.rm=T),max(flag_training_summary$up, na.rm=T)), max(-min(flag_training_summary$down, na.rm=T),max(flag_training_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_flag_training <- p_PSM_flag_training_sa20+p_flag_training_cs+p_flag_training_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_flag_training)
  ################## flag_RD------------
  # ipw
  es_PSM_flag_RD_sa20 = feols(flag_RD ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_flag_RD_sa20 <- aggregate(es_PSM_flag_RD_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_flag_RD_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_flag_RD_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + 
    labs(title = "flag_RD", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_flag_RD <- att_gt(yname = "flag_RD",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                       xformla = formula_cs, 
                       est_method = "ipw",base_period="universal",alp=0.05,
                       data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                       #print_details=FALSE,
                       anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_flag_RD_smry <- aggte(cs_flag_RD, type = "dynamic",na.rm = TRUE)
  
  p_flag_RD_cs <- ggdid(cs_flag_RD_smry, title = "flag_RD")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_flag_RD_cs <- aggte(cs_flag_RD, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  flag_RD_summary <- data.frame(method = method,
                                estimate = c(att_PSM_flag_RD_sa20[1], a_flag_RD_cs$overall.att#,a_flag_RD_Ssynth$att_estimate
                                ),
                                se = c(att_PSM_flag_RD_sa20[2],a_flag_RD_cs$overall.se#,se_ssynth
                                ),
                                up = c(att_PSM_flag_RD_sa20[1] + 1.645*att_PSM_flag_RD_sa20[2],a_flag_RD_cs$overall.att + 1.645*a_flag_RD_cs$overall.se#, a_flag_RD_Ssynth$att_estimate + 1.645 * se_ssynth
                                ),
                                down = c(att_PSM_flag_RD_sa20[1] - 1.645*att_PSM_flag_RD_sa20[2],a_flag_RD_cs$overall.att - 1.645*a_flag_RD_cs$overall.se#, a_flag_RD_Ssynth$att_estimate - 1.645 * se_ssynth
                                ),
                                up95 = c(att_PSM_flag_RD_sa20[1] + 1.960*att_PSM_flag_RD_sa20[2],a_flag_RD_cs$overall.att + 1.960*a_flag_RD_cs$overall.se#,  a_flag_RD_Ssynth$att_estimate + 1.960 * se_ssynth
                                ),
                                down95 = c(att_PSM_flag_RD_sa20[1] - 1.960*att_PSM_flag_RD_sa20[2],a_flag_RD_cs$overall.att - 1.960*a_flag_RD_cs$overall.se#,  a_flag_RD_Ssynth$att_estimate - 1.960 * se_ssynth
                                ),
                                up99 = c(att_PSM_flag_RD_sa20[1] + 2.5758*att_PSM_flag_RD_sa20[2],a_flag_RD_cs$overall.att + 2.5758*a_flag_RD_cs$overall.se#,  a_flag_RD_Ssynth$att_estimate + 2.5758 * se_ssynth
                                ),
                                down99 = c(att_PSM_flag_RD_sa20[1] - 2.5758*att_PSM_flag_RD_sa20[2],a_flag_RD_cs$overall.att - 2.5758*a_flag_RD_cs$overall.se#,  a_flag_RD_Ssynth$att_estimate - 2.5758 * se_ssynth
                                )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  flag_RD_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_flag_RD_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_flag_RD_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_flag_RD_sa20$coeftable[,1],
           sa_se =es_PSM_flag_RD_sa20$coeftable[,2],
           sa_t =es_PSM_flag_RD_sa20$coeftable[,3],
           sa_p =es_PSM_flag_RD_sa20$coeftable[,4])
  flag_RD_summary_appendix_cs <- data.frame(year = cs_flag_RD_smry$egt,
                                            cs =  cs_flag_RD_smry$att.egt,
                                            cs_se = cs_flag_RD_smry$se.egt,
                                            cs_cband_lower = cs_flag_RD_smry$att.egt - cs_flag_RD_smry$crit.val.egt*cs_flag_RD_smry$se.egt,
                                            cs_cband_upper = cs_flag_RD_smry$att.egt + cs_flag_RD_smry$crit.val.egt*cs_flag_RD_smry$se.egt)
  flag_RD_summary_appendix <- right_join(flag_RD_summary_appendix_sa,flag_RD_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_flag_RD_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- flag_RD_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(flag_RD_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- flag_RD_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(flag_RD_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  flag_RD_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  flag_RD_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(flag_RD_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(flag_RD_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  flag_RD_summary <- cbind(flag_RD_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(flag_RD_summary$rob)
  
  p_flag_RD_summary <- ggplot(flag_RD_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=flag_RD_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_RD_summary$pretrend[1]) +
    geom_rect(data=flag_RD_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_RD_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(flag_RD_summary$down, na.rm=T),max(flag_RD_summary$up, na.rm=T)), max(-min(flag_RD_summary$down, na.rm=T),max(flag_RD_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_flag_RD <- p_PSM_flag_RD_sa20+p_flag_RD_cs+p_flag_RD_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_flag_RD)
  ################## log_training------------
  # ipw
  es_PSM_log_training_sa20 = feols(log_training ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_log_training_sa20 <- aggregate(es_PSM_log_training_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_training_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_training_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_training", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_training <- att_gt(yname = "log_training",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                            xformla = formula_cs, 
                            est_method = "ipw",base_period="universal",alp=0.05,
                            data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                            #print_details=FALSE,
                            anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_training_smry <- aggte(cs_log_training, type = "dynamic",na.rm = TRUE)
  
  p_log_training_cs <- ggdid(cs_log_training_smry, title = "log_training")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_log_training_cs <- aggte(cs_log_training, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  log_training_summary <- data.frame(method = method,
                                     estimate = c(att_PSM_log_training_sa20[1], a_log_training_cs$overall.att#,a_log_training_Ssynth$att_estimate
                                     ),
                                     se = c(att_PSM_log_training_sa20[2],a_log_training_cs$overall.se#,se_ssynth
                                     ),
                                     up = c(att_PSM_log_training_sa20[1] + 1.645*att_PSM_log_training_sa20[2],a_log_training_cs$overall.att + 1.645*a_log_training_cs$overall.se#, a_log_training_Ssynth$att_estimate + 1.645 * se_ssynth
                                     ),
                                     down = c(att_PSM_log_training_sa20[1] - 1.645*att_PSM_log_training_sa20[2],a_log_training_cs$overall.att - 1.645*a_log_training_cs$overall.se#, a_log_training_Ssynth$att_estimate - 1.645 * se_ssynth
                                     ),
                                     up95 = c(att_PSM_log_training_sa20[1] + 1.960*att_PSM_log_training_sa20[2],a_log_training_cs$overall.att + 1.960*a_log_training_cs$overall.se#,  a_log_training_Ssynth$att_estimate + 1.960 * se_ssynth
                                     ),
                                     down95 = c(att_PSM_log_training_sa20[1] - 1.960*att_PSM_log_training_sa20[2],a_log_training_cs$overall.att - 1.960*a_log_training_cs$overall.se#,  a_log_training_Ssynth$att_estimate - 1.960 * se_ssynth
                                     ),
                                     up99 = c(att_PSM_log_training_sa20[1] + 2.5758*att_PSM_log_training_sa20[2],a_log_training_cs$overall.att + 2.5758*a_log_training_cs$overall.se#,  a_log_training_Ssynth$att_estimate + 2.5758 * se_ssynth
                                     ),
                                     down99 = c(att_PSM_log_training_sa20[1] - 2.5758*att_PSM_log_training_sa20[2],a_log_training_cs$overall.att - 2.5758*a_log_training_cs$overall.se#,  a_log_training_Ssynth$att_estimate - 2.5758 * se_ssynth
                                     )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  log_training_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_log_training_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_log_training_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_log_training_sa20$coeftable[,1],
           sa_se =es_PSM_log_training_sa20$coeftable[,2],
           sa_t =es_PSM_log_training_sa20$coeftable[,3],
           sa_p =es_PSM_log_training_sa20$coeftable[,4])
  log_training_summary_appendix_cs <- data.frame(year = cs_log_training_smry$egt,
                                                 cs =  cs_log_training_smry$att.egt,
                                                 cs_se = cs_log_training_smry$se.egt,
                                                 cs_cband_lower = cs_log_training_smry$att.egt - cs_log_training_smry$crit.val.egt*cs_log_training_smry$se.egt,
                                                 cs_cband_upper = cs_log_training_smry$att.egt + cs_log_training_smry$crit.val.egt*cs_log_training_smry$se.egt)
  log_training_summary_appendix <- right_join(log_training_summary_appendix_sa,log_training_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_log_training_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- log_training_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(log_training_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- log_training_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(log_training_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  log_training_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  log_training_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(log_training_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(log_training_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  log_training_summary <- cbind(log_training_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(log_training_summary$rob)
  
  p_log_training_summary <- ggplot(log_training_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_training_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_training_summary$pretrend[1]) +
    geom_rect(data=log_training_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_training_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_training_summary$down, na.rm=T),max(log_training_summary$up, na.rm=T)), max(-min(log_training_summary$down, na.rm=T),max(log_training_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_training <- p_PSM_log_training_sa20+p_log_training_cs+p_log_training_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_log_training)
  ################## log_RD------------
  # ipw
  es_PSM_log_RD_sa20 = feols(log_RD ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_log_RD_sa20 <- aggregate(es_PSM_log_RD_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_RD_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_RD_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_RD", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_RD <- att_gt(yname = "log_RD",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                      xformla = formula_cs, 
                      est_method = "ipw",base_period="universal",alp=0.05,
                      data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                      #print_details=FALSE,
                      anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_RD_smry <- aggte(cs_log_RD, type = "dynamic",na.rm = TRUE)
  
  p_log_RD_cs <- ggdid(cs_log_RD_smry, title = "log_RD")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_log_RD_cs <- aggte(cs_log_RD, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  log_RD_summary <- data.frame(method = method,
                               estimate = c(att_PSM_log_RD_sa20[1], a_log_RD_cs$overall.att#,a_log_RD_Ssynth$att_estimate
                               ),
                               se = c(att_PSM_log_RD_sa20[2],a_log_RD_cs$overall.se#,se_ssynth
                               ),
                               up = c(att_PSM_log_RD_sa20[1] + 1.645*att_PSM_log_RD_sa20[2],a_log_RD_cs$overall.att + 1.645*a_log_RD_cs$overall.se#, a_log_RD_Ssynth$att_estimate + 1.645 * se_ssynth
                               ),
                               down = c(att_PSM_log_RD_sa20[1] - 1.645*att_PSM_log_RD_sa20[2],a_log_RD_cs$overall.att - 1.645*a_log_RD_cs$overall.se#, a_log_RD_Ssynth$att_estimate - 1.645 * se_ssynth
                               ),
                               up95 = c(att_PSM_log_RD_sa20[1] + 1.960*att_PSM_log_RD_sa20[2],a_log_RD_cs$overall.att + 1.960*a_log_RD_cs$overall.se#,  a_log_RD_Ssynth$att_estimate + 1.960 * se_ssynth
                               ),
                               down95 = c(att_PSM_log_RD_sa20[1] - 1.960*att_PSM_log_RD_sa20[2],a_log_RD_cs$overall.att - 1.960*a_log_RD_cs$overall.se#,  a_log_RD_Ssynth$att_estimate - 1.960 * se_ssynth
                               ),
                               up99 = c(att_PSM_log_RD_sa20[1] + 2.5758*att_PSM_log_RD_sa20[2],a_log_RD_cs$overall.att + 2.5758*a_log_RD_cs$overall.se#,  a_log_RD_Ssynth$att_estimate + 2.5758 * se_ssynth
                               ),
                               down99 = c(att_PSM_log_RD_sa20[1] - 2.5758*att_PSM_log_RD_sa20[2],a_log_RD_cs$overall.att - 2.5758*a_log_RD_cs$overall.se#,  a_log_RD_Ssynth$att_estimate - 2.5758 * se_ssynth
                               )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  log_RD_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_log_RD_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_log_RD_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_log_RD_sa20$coeftable[,1],
           sa_se =es_PSM_log_RD_sa20$coeftable[,2],
           sa_t =es_PSM_log_RD_sa20$coeftable[,3],
           sa_p =es_PSM_log_RD_sa20$coeftable[,4])
  log_RD_summary_appendix_cs <- data.frame(year = cs_log_RD_smry$egt,
                                           cs =  cs_log_RD_smry$att.egt,
                                           cs_se = cs_log_RD_smry$se.egt,
                                           cs_cband_lower = cs_log_RD_smry$att.egt - cs_log_RD_smry$crit.val.egt*cs_log_RD_smry$se.egt,
                                           cs_cband_upper = cs_log_RD_smry$att.egt + cs_log_RD_smry$crit.val.egt*cs_log_RD_smry$se.egt)
  log_RD_summary_appendix <- right_join(log_RD_summary_appendix_sa,log_RD_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_log_RD_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- log_RD_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(log_RD_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- log_RD_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(log_RD_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  log_RD_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  log_RD_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(log_RD_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(log_RD_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  log_RD_summary <- cbind(log_RD_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(log_RD_summary$rob)
  
  p_log_RD_summary <- ggplot(log_RD_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_RD_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_RD_summary$pretrend[1]) +
    geom_rect(data=log_RD_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_RD_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_RD_summary$down, na.rm=T),max(log_RD_summary$up, na.rm=T)), max(-min(log_RD_summary$down, na.rm=T),max(log_RD_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_RD <- p_PSM_log_RD_sa20+p_log_RD_cs+p_log_RD_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_log_RD)
  ## output_trainingRD-----------
  pa_trainingRD_sum <- pa_flag_training/pa_flag_RD/pa_log_training/pa_log_RD + plot_annotation(
    title = "Effect on the Training and RD",
    subtitle = target,
    ##caption = caption_main
  ) &
    theme(text = element_text(family="Meiryo UI"))
  
  # filename <- paste("03_robustness_output/", target,"_01trainingRD_", count_intervention, ".png", sep="")
  # ggsave(filename,pa_log_trainingRD_sum, 
  #        width=20, height =22.5, units = "cm", dpi=200) 
  
  filename_pdf <- paste("03_robustness_output/", target,"_35_trimmed_trainingRD_", count_intervention, ".pdf", sep="")
  ggsave(filename_pdf,pa_trainingRD_sum, 
         width=15, height =24, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("03_robustness_output/", target,"_35_trimmed_trainingRD_", count_intervention, ".png", sep="")
  ggsave(filename_png,pa_trainingRD_sum, 
         width=15, height =24, units = "cm", dpi=200) 
  
  
  write.xlsx(list("flag_training"=flag_training_summary,"flag_RD"=flag_RD_summary,"log_training"=log_training_summary,"log_RD"=log_RD_summary), paste("03_robustness_output/",target,"_35trainingRD_trimmed_table.xlsx", sep=""))
  write.xlsx(list("flag_training"=flag_training_summary_appendix,"flag_RD"=flag_RD_summary_appendix,"log_training"=log_training_summary_appendix,"log_RD"=log_RD_summary_appendix), paste("03_robustness_output/",target,"_35trainingRD_trimmed_table_appendix.xlsx"))
}
robustness_trimmed_IP <- function(target){
  ################## flag_patent------------
  # ipw
  es_PSM_flag_patent_sa20 = feols(flag_patent ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_flag_patent_sa20 <- aggregate(es_PSM_flag_patent_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_flag_patent_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_flag_patent_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + 
    labs(title = "flag_patent", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_flag_patent <- att_gt(yname = "flag_patent",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                           xformla = formula_cs, 
                           est_method = "ipw",base_period="universal",alp=0.05,
                           data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                           #print_details=FALSE,
                           anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_flag_patent_smry <- aggte(cs_flag_patent, type = "dynamic",na.rm = TRUE)
  
  p_flag_patent_cs <- ggdid(cs_flag_patent_smry, title = "flag_patent")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_flag_patent_cs <- aggte(cs_flag_patent, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  flag_patent_summary <- data.frame(method = method,
                                    estimate = c(att_PSM_flag_patent_sa20[1], a_flag_patent_cs$overall.att#,a_flag_patent_Ssynth$att_estimate
                                    ),
                                    se = c(att_PSM_flag_patent_sa20[2],a_flag_patent_cs$overall.se#,se_ssynth
                                    ),
                                    up = c(att_PSM_flag_patent_sa20[1] + 1.645*att_PSM_flag_patent_sa20[2],a_flag_patent_cs$overall.att + 1.645*a_flag_patent_cs$overall.se#, a_flag_patent_Ssynth$att_estimate + 1.645 * se_ssynth
                                    ),
                                    down = c(att_PSM_flag_patent_sa20[1] - 1.645*att_PSM_flag_patent_sa20[2],a_flag_patent_cs$overall.att - 1.645*a_flag_patent_cs$overall.se#, a_flag_patent_Ssynth$att_estimate - 1.645 * se_ssynth
                                    ),
                                    up95 = c(att_PSM_flag_patent_sa20[1] + 1.960*att_PSM_flag_patent_sa20[2],a_flag_patent_cs$overall.att + 1.960*a_flag_patent_cs$overall.se#,  a_flag_patent_Ssynth$att_estimate + 1.960 * se_ssynth
                                    ),
                                    down95 = c(att_PSM_flag_patent_sa20[1] - 1.960*att_PSM_flag_patent_sa20[2],a_flag_patent_cs$overall.att - 1.960*a_flag_patent_cs$overall.se#,  a_flag_patent_Ssynth$att_estimate - 1.960 * se_ssynth
                                    ),
                                    up99 = c(att_PSM_flag_patent_sa20[1] + 2.5758*att_PSM_flag_patent_sa20[2],a_flag_patent_cs$overall.att + 2.5758*a_flag_patent_cs$overall.se#,  a_flag_patent_Ssynth$att_estimate + 2.5758 * se_ssynth
                                    ),
                                    down99 = c(att_PSM_flag_patent_sa20[1] - 2.5758*att_PSM_flag_patent_sa20[2],a_flag_patent_cs$overall.att - 2.5758*a_flag_patent_cs$overall.se#,  a_flag_patent_Ssynth$att_estimate - 2.5758 * se_ssynth
                                    )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  flag_patent_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_flag_patent_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_flag_patent_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_flag_patent_sa20$coeftable[,1],
           sa_se =es_PSM_flag_patent_sa20$coeftable[,2],
           sa_t =es_PSM_flag_patent_sa20$coeftable[,3],
           sa_p =es_PSM_flag_patent_sa20$coeftable[,4])
  flag_patent_summary_appendix_cs <- data.frame(year = cs_flag_patent_smry$egt,
                                                cs =  cs_flag_patent_smry$att.egt,
                                                cs_se = cs_flag_patent_smry$se.egt,
                                                cs_cband_lower = cs_flag_patent_smry$att.egt - cs_flag_patent_smry$crit.val.egt*cs_flag_patent_smry$se.egt,
                                                cs_cband_upper = cs_flag_patent_smry$att.egt + cs_flag_patent_smry$crit.val.egt*cs_flag_patent_smry$se.egt)
  flag_patent_summary_appendix <- right_join(flag_patent_summary_appendix_sa,flag_patent_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_flag_patent_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- flag_patent_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(flag_patent_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- flag_patent_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(flag_patent_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  flag_patent_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  flag_patent_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(flag_patent_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(flag_patent_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  flag_patent_summary <- cbind(flag_patent_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(flag_patent_summary$rob)
  
  p_flag_patent_summary <- ggplot(flag_patent_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=flag_patent_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_patent_summary$pretrend[1]) +
    geom_rect(data=flag_patent_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_patent_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(flag_patent_summary$down, na.rm=T),max(flag_patent_summary$up, na.rm=T)), max(-min(flag_patent_summary$down, na.rm=T),max(flag_patent_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_flag_patent <- p_PSM_flag_patent_sa20+p_flag_patent_cs+p_flag_patent_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_flag_patent)
  ################## flag_jitsuyo------------
  # ipw
  es_PSM_flag_jitsuyo_sa20 = feols(flag_jitsuyo ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_flag_jitsuyo_sa20 <- aggregate(es_PSM_flag_jitsuyo_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_flag_jitsuyo_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_flag_jitsuyo_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + 
    labs(title = "flag_jitsuyo", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_flag_jitsuyo <- att_gt(yname = "flag_jitsuyo",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                            xformla = formula_cs, 
                            est_method = "ipw",base_period="universal",alp=0.05,
                            data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                            #print_details=FALSE,
                            anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_flag_jitsuyo_smry <- aggte(cs_flag_jitsuyo, type = "dynamic",na.rm = TRUE)
  
  p_flag_jitsuyo_cs <- ggdid(cs_flag_jitsuyo_smry, title = "flag_jitsuyo")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_flag_jitsuyo_cs <- aggte(cs_flag_jitsuyo, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  flag_jitsuyo_summary <- data.frame(method = method,
                                     estimate = c(att_PSM_flag_jitsuyo_sa20[1], a_flag_jitsuyo_cs$overall.att#,a_flag_jitsuyo_Ssynth$att_estimate
                                     ),
                                     se = c(att_PSM_flag_jitsuyo_sa20[2],a_flag_jitsuyo_cs$overall.se#,se_ssynth
                                     ),
                                     up = c(att_PSM_flag_jitsuyo_sa20[1] + 1.645*att_PSM_flag_jitsuyo_sa20[2],a_flag_jitsuyo_cs$overall.att + 1.645*a_flag_jitsuyo_cs$overall.se#, a_flag_jitsuyo_Ssynth$att_estimate + 1.645 * se_ssynth
                                     ),
                                     down = c(att_PSM_flag_jitsuyo_sa20[1] - 1.645*att_PSM_flag_jitsuyo_sa20[2],a_flag_jitsuyo_cs$overall.att - 1.645*a_flag_jitsuyo_cs$overall.se#, a_flag_jitsuyo_Ssynth$att_estimate - 1.645 * se_ssynth
                                     ),
                                     up95 = c(att_PSM_flag_jitsuyo_sa20[1] + 1.960*att_PSM_flag_jitsuyo_sa20[2],a_flag_jitsuyo_cs$overall.att + 1.960*a_flag_jitsuyo_cs$overall.se#,  a_flag_jitsuyo_Ssynth$att_estimate + 1.960 * se_ssynth
                                     ),
                                     down95 = c(att_PSM_flag_jitsuyo_sa20[1] - 1.960*att_PSM_flag_jitsuyo_sa20[2],a_flag_jitsuyo_cs$overall.att - 1.960*a_flag_jitsuyo_cs$overall.se#,  a_flag_jitsuyo_Ssynth$att_estimate - 1.960 * se_ssynth
                                     ),
                                     up99 = c(att_PSM_flag_jitsuyo_sa20[1] + 2.5758*att_PSM_flag_jitsuyo_sa20[2],a_flag_jitsuyo_cs$overall.att + 2.5758*a_flag_jitsuyo_cs$overall.se#,  a_flag_jitsuyo_Ssynth$att_estimate + 2.5758 * se_ssynth
                                     ),
                                     down99 = c(att_PSM_flag_jitsuyo_sa20[1] - 2.5758*att_PSM_flag_jitsuyo_sa20[2],a_flag_jitsuyo_cs$overall.att - 2.5758*a_flag_jitsuyo_cs$overall.se#,  a_flag_jitsuyo_Ssynth$att_estimate - 2.5758 * se_ssynth
                                     )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  flag_jitsuyo_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_flag_jitsuyo_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_flag_jitsuyo_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_flag_jitsuyo_sa20$coeftable[,1],
           sa_se =es_PSM_flag_jitsuyo_sa20$coeftable[,2],
           sa_t =es_PSM_flag_jitsuyo_sa20$coeftable[,3],
           sa_p =es_PSM_flag_jitsuyo_sa20$coeftable[,4])
  flag_jitsuyo_summary_appendix_cs <- data.frame(year = cs_flag_jitsuyo_smry$egt,
                                                 cs =  cs_flag_jitsuyo_smry$att.egt,
                                                 cs_se = cs_flag_jitsuyo_smry$se.egt,
                                                 cs_cband_lower = cs_flag_jitsuyo_smry$att.egt - cs_flag_jitsuyo_smry$crit.val.egt*cs_flag_jitsuyo_smry$se.egt,
                                                 cs_cband_upper = cs_flag_jitsuyo_smry$att.egt + cs_flag_jitsuyo_smry$crit.val.egt*cs_flag_jitsuyo_smry$se.egt)
  flag_jitsuyo_summary_appendix <- right_join(flag_jitsuyo_summary_appendix_sa,flag_jitsuyo_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_flag_jitsuyo_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- flag_jitsuyo_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(flag_jitsuyo_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- flag_jitsuyo_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(flag_jitsuyo_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  flag_jitsuyo_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  flag_jitsuyo_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(flag_jitsuyo_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(flag_jitsuyo_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  flag_jitsuyo_summary <- cbind(flag_jitsuyo_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(flag_jitsuyo_summary$rob)
  
  p_flag_jitsuyo_summary <- ggplot(flag_jitsuyo_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=flag_jitsuyo_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_jitsuyo_summary$pretrend[1]) +
    geom_rect(data=flag_jitsuyo_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_jitsuyo_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(flag_jitsuyo_summary$down, na.rm=T),max(flag_jitsuyo_summary$up, na.rm=T)), max(-min(flag_jitsuyo_summary$down, na.rm=T),max(flag_jitsuyo_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_flag_jitsuyo <- p_PSM_flag_jitsuyo_sa20+p_flag_jitsuyo_cs+p_flag_jitsuyo_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_flag_jitsuyo)
  ################## flag_isho------------
  # ipw
  es_PSM_flag_isho_sa20 = feols(flag_isho ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_flag_isho_sa20 <- aggregate(es_PSM_flag_isho_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_flag_isho_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_flag_isho_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + 
    labs(title = "flag_isho", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_flag_isho <- att_gt(yname = "flag_isho",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                         xformla = formula_cs, 
                         est_method = "ipw",base_period="universal",alp=0.05,
                         data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                         #print_details=FALSE,
                         anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_flag_isho_smry <- aggte(cs_flag_isho, type = "dynamic",na.rm = TRUE)
  
  p_flag_isho_cs <- ggdid(cs_flag_isho_smry, title = "flag_isho")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_flag_isho_cs <- aggte(cs_flag_isho, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  flag_isho_summary <- data.frame(method = method,
                                  estimate = c(att_PSM_flag_isho_sa20[1], a_flag_isho_cs$overall.att#,a_flag_isho_Ssynth$att_estimate
                                  ),
                                  se = c(att_PSM_flag_isho_sa20[2],a_flag_isho_cs$overall.se#,se_ssynth
                                  ),
                                  up = c(att_PSM_flag_isho_sa20[1] + 1.645*att_PSM_flag_isho_sa20[2],a_flag_isho_cs$overall.att + 1.645*a_flag_isho_cs$overall.se#, a_flag_isho_Ssynth$att_estimate + 1.645 * se_ssynth
                                  ),
                                  down = c(att_PSM_flag_isho_sa20[1] - 1.645*att_PSM_flag_isho_sa20[2],a_flag_isho_cs$overall.att - 1.645*a_flag_isho_cs$overall.se#, a_flag_isho_Ssynth$att_estimate - 1.645 * se_ssynth
                                  ),
                                  up95 = c(att_PSM_flag_isho_sa20[1] + 1.960*att_PSM_flag_isho_sa20[2],a_flag_isho_cs$overall.att + 1.960*a_flag_isho_cs$overall.se#,  a_flag_isho_Ssynth$att_estimate + 1.960 * se_ssynth
                                  ),
                                  down95 = c(att_PSM_flag_isho_sa20[1] - 1.960*att_PSM_flag_isho_sa20[2],a_flag_isho_cs$overall.att - 1.960*a_flag_isho_cs$overall.se#,  a_flag_isho_Ssynth$att_estimate - 1.960 * se_ssynth
                                  ),
                                  up99 = c(att_PSM_flag_isho_sa20[1] + 2.5758*att_PSM_flag_isho_sa20[2],a_flag_isho_cs$overall.att + 2.5758*a_flag_isho_cs$overall.se#,  a_flag_isho_Ssynth$att_estimate + 2.5758 * se_ssynth
                                  ),
                                  down99 = c(att_PSM_flag_isho_sa20[1] - 2.5758*att_PSM_flag_isho_sa20[2],a_flag_isho_cs$overall.att - 2.5758*a_flag_isho_cs$overall.se#,  a_flag_isho_Ssynth$att_estimate - 2.5758 * se_ssynth
                                  )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  flag_isho_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_flag_isho_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_flag_isho_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_flag_isho_sa20$coeftable[,1],
           sa_se =es_PSM_flag_isho_sa20$coeftable[,2],
           sa_t =es_PSM_flag_isho_sa20$coeftable[,3],
           sa_p =es_PSM_flag_isho_sa20$coeftable[,4])
  flag_isho_summary_appendix_cs <- data.frame(year = cs_flag_isho_smry$egt,
                                              cs =  cs_flag_isho_smry$att.egt,
                                              cs_se = cs_flag_isho_smry$se.egt,
                                              cs_cband_lower = cs_flag_isho_smry$att.egt - cs_flag_isho_smry$crit.val.egt*cs_flag_isho_smry$se.egt,
                                              cs_cband_upper = cs_flag_isho_smry$att.egt + cs_flag_isho_smry$crit.val.egt*cs_flag_isho_smry$se.egt)
  flag_isho_summary_appendix <- right_join(flag_isho_summary_appendix_sa,flag_isho_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_flag_isho_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- flag_isho_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(flag_isho_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- flag_isho_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(flag_isho_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  flag_isho_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  flag_isho_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(flag_isho_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(flag_isho_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  flag_isho_summary <- cbind(flag_isho_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(flag_isho_summary$rob)
  
  p_flag_isho_summary <- ggplot(flag_isho_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=flag_isho_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_isho_summary$pretrend[1]) +
    geom_rect(data=flag_isho_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_isho_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(flag_isho_summary$down, na.rm=T),max(flag_isho_summary$up, na.rm=T)), max(-min(flag_isho_summary$down, na.rm=T),max(flag_isho_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_flag_isho <- p_PSM_flag_isho_sa20+p_flag_isho_cs+p_flag_isho_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_flag_isho)
  ################## log_patent------------
  # ipw
  es_PSM_log_patent_sa20 = feols(log_patent ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_log_patent_sa20 <- aggregate(es_PSM_log_patent_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_patent_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_patent_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_patent", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_patent <- att_gt(yname = "log_patent",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                          xformla = formula_cs, 
                          est_method = "ipw",base_period="universal",alp=0.05,
                          data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                          #print_details=FALSE,
                          anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_patent_smry <- aggte(cs_log_patent, type = "dynamic",na.rm = TRUE)
  
  p_log_patent_cs <- ggdid(cs_log_patent_smry, title = "log_patent")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_log_patent_cs <- aggte(cs_log_patent, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  log_patent_summary <- data.frame(method = method,
                                   estimate = c(att_PSM_log_patent_sa20[1], a_log_patent_cs$overall.att#,a_log_patent_Ssynth$att_estimate
                                   ),
                                   se = c(att_PSM_log_patent_sa20[2],a_log_patent_cs$overall.se#,se_ssynth
                                   ),
                                   up = c(att_PSM_log_patent_sa20[1] + 1.645*att_PSM_log_patent_sa20[2],a_log_patent_cs$overall.att + 1.645*a_log_patent_cs$overall.se#, a_log_patent_Ssynth$att_estimate + 1.645 * se_ssynth
                                   ),
                                   down = c(att_PSM_log_patent_sa20[1] - 1.645*att_PSM_log_patent_sa20[2],a_log_patent_cs$overall.att - 1.645*a_log_patent_cs$overall.se#, a_log_patent_Ssynth$att_estimate - 1.645 * se_ssynth
                                   ),
                                   up95 = c(att_PSM_log_patent_sa20[1] + 1.960*att_PSM_log_patent_sa20[2],a_log_patent_cs$overall.att + 1.960*a_log_patent_cs$overall.se#,  a_log_patent_Ssynth$att_estimate + 1.960 * se_ssynth
                                   ),
                                   down95 = c(att_PSM_log_patent_sa20[1] - 1.960*att_PSM_log_patent_sa20[2],a_log_patent_cs$overall.att - 1.960*a_log_patent_cs$overall.se#,  a_log_patent_Ssynth$att_estimate - 1.960 * se_ssynth
                                   ),
                                   up99 = c(att_PSM_log_patent_sa20[1] + 2.5758*att_PSM_log_patent_sa20[2],a_log_patent_cs$overall.att + 2.5758*a_log_patent_cs$overall.se#,  a_log_patent_Ssynth$att_estimate + 2.5758 * se_ssynth
                                   ),
                                   down99 = c(att_PSM_log_patent_sa20[1] - 2.5758*att_PSM_log_patent_sa20[2],a_log_patent_cs$overall.att - 2.5758*a_log_patent_cs$overall.se#,  a_log_patent_Ssynth$att_estimate - 2.5758 * se_ssynth
                                   )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  log_patent_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_log_patent_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_log_patent_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_log_patent_sa20$coeftable[,1],
           sa_se =es_PSM_log_patent_sa20$coeftable[,2],
           sa_t =es_PSM_log_patent_sa20$coeftable[,3],
           sa_p =es_PSM_log_patent_sa20$coeftable[,4])
  log_patent_summary_appendix_cs <- data.frame(year = cs_log_patent_smry$egt,
                                               cs =  cs_log_patent_smry$att.egt,
                                               cs_se = cs_log_patent_smry$se.egt,
                                               cs_cband_lower = cs_log_patent_smry$att.egt - cs_log_patent_smry$crit.val.egt*cs_log_patent_smry$se.egt,
                                               cs_cband_upper = cs_log_patent_smry$att.egt + cs_log_patent_smry$crit.val.egt*cs_log_patent_smry$se.egt)
  log_patent_summary_appendix <- right_join(log_patent_summary_appendix_sa,log_patent_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_log_patent_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- log_patent_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(log_patent_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- log_patent_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(log_patent_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  log_patent_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  log_patent_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(log_patent_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(log_patent_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  log_patent_summary <- cbind(log_patent_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(log_patent_summary$rob)
  
  p_log_patent_summary <- ggplot(log_patent_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_patent_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_patent_summary$pretrend[1]) +
    geom_rect(data=log_patent_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_patent_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_patent_summary$down, na.rm=T),max(log_patent_summary$up, na.rm=T)), max(-min(log_patent_summary$down, na.rm=T),max(log_patent_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_patent <- p_PSM_log_patent_sa20+p_log_patent_cs+p_log_patent_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_log_patent)
  ################## log_jitsuyo------------
  # ipw
  es_PSM_log_jitsuyo_sa20 = feols(log_jitsuyo ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_log_jitsuyo_sa20 <- aggregate(es_PSM_log_jitsuyo_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_jitsuyo_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_jitsuyo_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_jitsuyo", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_jitsuyo <- att_gt(yname = "log_jitsuyo",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                           xformla = formula_cs, 
                           est_method = "ipw",base_period="universal",alp=0.05,
                           data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                           #print_details=FALSE,
                           anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_jitsuyo_smry <- aggte(cs_log_jitsuyo, type = "dynamic",na.rm = TRUE)
  
  p_log_jitsuyo_cs <- ggdid(cs_log_jitsuyo_smry, title = "log_jitsuyo")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_log_jitsuyo_cs <- aggte(cs_log_jitsuyo, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  log_jitsuyo_summary <- data.frame(method = method,
                                    estimate = c(att_PSM_log_jitsuyo_sa20[1], a_log_jitsuyo_cs$overall.att#,a_log_jitsuyo_Ssynth$att_estimate
                                    ),
                                    se = c(att_PSM_log_jitsuyo_sa20[2],a_log_jitsuyo_cs$overall.se#,se_ssynth
                                    ),
                                    up = c(att_PSM_log_jitsuyo_sa20[1] + 1.645*att_PSM_log_jitsuyo_sa20[2],a_log_jitsuyo_cs$overall.att + 1.645*a_log_jitsuyo_cs$overall.se#, a_log_jitsuyo_Ssynth$att_estimate + 1.645 * se_ssynth
                                    ),
                                    down = c(att_PSM_log_jitsuyo_sa20[1] - 1.645*att_PSM_log_jitsuyo_sa20[2],a_log_jitsuyo_cs$overall.att - 1.645*a_log_jitsuyo_cs$overall.se#, a_log_jitsuyo_Ssynth$att_estimate - 1.645 * se_ssynth
                                    ),
                                    up95 = c(att_PSM_log_jitsuyo_sa20[1] + 1.960*att_PSM_log_jitsuyo_sa20[2],a_log_jitsuyo_cs$overall.att + 1.960*a_log_jitsuyo_cs$overall.se#,  a_log_jitsuyo_Ssynth$att_estimate + 1.960 * se_ssynth
                                    ),
                                    down95 = c(att_PSM_log_jitsuyo_sa20[1] - 1.960*att_PSM_log_jitsuyo_sa20[2],a_log_jitsuyo_cs$overall.att - 1.960*a_log_jitsuyo_cs$overall.se#,  a_log_jitsuyo_Ssynth$att_estimate - 1.960 * se_ssynth
                                    ),
                                    up99 = c(att_PSM_log_jitsuyo_sa20[1] + 2.5758*att_PSM_log_jitsuyo_sa20[2],a_log_jitsuyo_cs$overall.att + 2.5758*a_log_jitsuyo_cs$overall.se#,  a_log_jitsuyo_Ssynth$att_estimate + 2.5758 * se_ssynth
                                    ),
                                    down99 = c(att_PSM_log_jitsuyo_sa20[1] - 2.5758*att_PSM_log_jitsuyo_sa20[2],a_log_jitsuyo_cs$overall.att - 2.5758*a_log_jitsuyo_cs$overall.se#,  a_log_jitsuyo_Ssynth$att_estimate - 2.5758 * se_ssynth
                                    )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  log_jitsuyo_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_log_jitsuyo_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_log_jitsuyo_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_log_jitsuyo_sa20$coeftable[,1],
           sa_se =es_PSM_log_jitsuyo_sa20$coeftable[,2],
           sa_t =es_PSM_log_jitsuyo_sa20$coeftable[,3],
           sa_p =es_PSM_log_jitsuyo_sa20$coeftable[,4])
  log_jitsuyo_summary_appendix_cs <- data.frame(year = cs_log_jitsuyo_smry$egt,
                                                cs =  cs_log_jitsuyo_smry$att.egt,
                                                cs_se = cs_log_jitsuyo_smry$se.egt,
                                                cs_cband_lower = cs_log_jitsuyo_smry$att.egt - cs_log_jitsuyo_smry$crit.val.egt*cs_log_jitsuyo_smry$se.egt,
                                                cs_cband_upper = cs_log_jitsuyo_smry$att.egt + cs_log_jitsuyo_smry$crit.val.egt*cs_log_jitsuyo_smry$se.egt)
  log_jitsuyo_summary_appendix <- right_join(log_jitsuyo_summary_appendix_sa,log_jitsuyo_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_log_jitsuyo_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- log_jitsuyo_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(log_jitsuyo_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- log_jitsuyo_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(log_jitsuyo_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  log_jitsuyo_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  log_jitsuyo_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(log_jitsuyo_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(log_jitsuyo_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  log_jitsuyo_summary <- cbind(log_jitsuyo_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(log_jitsuyo_summary$rob)
  
  p_log_jitsuyo_summary <- ggplot(log_jitsuyo_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_jitsuyo_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_jitsuyo_summary$pretrend[1]) +
    geom_rect(data=log_jitsuyo_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_jitsuyo_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_jitsuyo_summary$down, na.rm=T),max(log_jitsuyo_summary$up, na.rm=T)), max(-min(log_jitsuyo_summary$down, na.rm=T),max(log_jitsuyo_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_jitsuyo <- p_PSM_log_jitsuyo_sa20+p_log_jitsuyo_cs+p_log_jitsuyo_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_log_jitsuyo)
  ################## log_isho------------
  # ipw
  es_PSM_log_isho_sa20 = feols(log_isho ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_log_isho_sa20 <- aggregate(es_PSM_log_isho_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_isho_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_isho_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_isho", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_isho <- att_gt(yname = "log_isho",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                        xformla = formula_cs, 
                        est_method = "ipw",base_period="universal",alp=0.05,
                        data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                        #print_details=FALSE,
                        anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_isho_smry <- aggte(cs_log_isho, type = "dynamic",na.rm = TRUE)
  
  p_log_isho_cs <- ggdid(cs_log_isho_smry, title = "log_isho")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_log_isho_cs <- aggte(cs_log_isho, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  log_isho_summary <- data.frame(method = method,
                                 estimate = c(att_PSM_log_isho_sa20[1], a_log_isho_cs$overall.att#,a_log_isho_Ssynth$att_estimate
                                 ),
                                 se = c(att_PSM_log_isho_sa20[2],a_log_isho_cs$overall.se#,se_ssynth
                                 ),
                                 up = c(att_PSM_log_isho_sa20[1] + 1.645*att_PSM_log_isho_sa20[2],a_log_isho_cs$overall.att + 1.645*a_log_isho_cs$overall.se#, a_log_isho_Ssynth$att_estimate + 1.645 * se_ssynth
                                 ),
                                 down = c(att_PSM_log_isho_sa20[1] - 1.645*att_PSM_log_isho_sa20[2],a_log_isho_cs$overall.att - 1.645*a_log_isho_cs$overall.se#, a_log_isho_Ssynth$att_estimate - 1.645 * se_ssynth
                                 ),
                                 up95 = c(att_PSM_log_isho_sa20[1] + 1.960*att_PSM_log_isho_sa20[2],a_log_isho_cs$overall.att + 1.960*a_log_isho_cs$overall.se#,  a_log_isho_Ssynth$att_estimate + 1.960 * se_ssynth
                                 ),
                                 down95 = c(att_PSM_log_isho_sa20[1] - 1.960*att_PSM_log_isho_sa20[2],a_log_isho_cs$overall.att - 1.960*a_log_isho_cs$overall.se#,  a_log_isho_Ssynth$att_estimate - 1.960 * se_ssynth
                                 ),
                                 up99 = c(att_PSM_log_isho_sa20[1] + 2.5758*att_PSM_log_isho_sa20[2],a_log_isho_cs$overall.att + 2.5758*a_log_isho_cs$overall.se#,  a_log_isho_Ssynth$att_estimate + 2.5758 * se_ssynth
                                 ),
                                 down99 = c(att_PSM_log_isho_sa20[1] - 2.5758*att_PSM_log_isho_sa20[2],a_log_isho_cs$overall.att - 2.5758*a_log_isho_cs$overall.se#,  a_log_isho_Ssynth$att_estimate - 2.5758 * se_ssynth
                                 )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  log_isho_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_log_isho_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_log_isho_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_log_isho_sa20$coeftable[,1],
           sa_se =es_PSM_log_isho_sa20$coeftable[,2],
           sa_t =es_PSM_log_isho_sa20$coeftable[,3],
           sa_p =es_PSM_log_isho_sa20$coeftable[,4])
  log_isho_summary_appendix_cs <- data.frame(year = cs_log_isho_smry$egt,
                                             cs =  cs_log_isho_smry$att.egt,
                                             cs_se = cs_log_isho_smry$se.egt,
                                             cs_cband_lower = cs_log_isho_smry$att.egt - cs_log_isho_smry$crit.val.egt*cs_log_isho_smry$se.egt,
                                             cs_cband_upper = cs_log_isho_smry$att.egt + cs_log_isho_smry$crit.val.egt*cs_log_isho_smry$se.egt)
  log_isho_summary_appendix <- right_join(log_isho_summary_appendix_sa,log_isho_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_log_isho_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- log_isho_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(log_isho_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- log_isho_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(log_isho_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  log_isho_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  log_isho_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(log_isho_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(log_isho_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  log_isho_summary <- cbind(log_isho_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(log_isho_summary$rob)
  
  p_log_isho_summary <- ggplot(log_isho_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_isho_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_isho_summary$pretrend[1]) +
    geom_rect(data=log_isho_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_isho_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_isho_summary$down, na.rm=T),max(log_isho_summary$up, na.rm=T)), max(-min(log_isho_summary$down, na.rm=T),max(log_isho_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_isho <- p_PSM_log_isho_sa20+p_log_isho_cs+p_log_isho_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_log_isho)
  ## output_IP-----------
  pa_IP_sum <- pa_flag_patent/pa_flag_jitsuyo/pa_flag_isho/pa_log_patent/pa_log_jitsuyo/pa_log_isho + plot_annotation(
    title = "Effect on the Intellectual Property",
    subtitle = target,
    ##caption = caption_main
  ) &
    theme(text = element_text(family="Meiryo UI"))
  
  # filename <- paste("03_robustness_output/", target,"_01IP_", count_intervention, ".png", sep="")
  # ggsave(filename,pa_log_IP_sum, 
  #        width=20, height =22.5, units = "cm", dpi=200) 
  
  filename_pdf <- paste("03_robustness_output/", target,"_36_trimmed_IP_", count_intervention, ".pdf", sep="")
  ggsave(filename_pdf,pa_IP_sum, 
         width=15, height =36, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("03_robustness_output/", target,"_36_trimmed_IP_", count_intervention, ".png", sep="")
  ggsave(filename_png,pa_IP_sum, 
         width=15, height =36, units = "cm", dpi=200) 
  
  
  write.xlsx(list("flag_patent"=flag_patent_summary,"flag_jitsuyo"=flag_jitsuyo_summary,"flag_isho"=flag_isho_summary,"log_patent"=log_patent_summary,"log_jitsuyo"=log_jitsuyo_summary,"log_isho"=log_isho_summary), paste("03_robustness_output/",target,"_36IP_trimmed_table.xlsx", sep=""))
  write.xlsx(list("flag_patent"=flag_patent_summary_appendix,"flag_jitsuyo"=flag_jitsuyo_summary_appendix,"flag_isho"=flag_isho_summary_appendix,"log_patent"=log_patent_summary_appendix,"log_jitsuyo"=log_jitsuyo_summary_appendix,"log_isho"=log_isho_summary_appendix), paste("03_robustness_output/",target,"_36IP_trimmed_table_appendix.xlsx"))
}
robustness_trimmed_investment <- function(target){
  ################## flag_investment_affiliate_domestic------------
  # ipw
  es_PSM_flag_investment_affiliate_domestic_sa20 = feols(flag_investment_affiliate_domestic ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_flag_investment_affiliate_domestic_sa20 <- aggregate(es_PSM_flag_investment_affiliate_domestic_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_flag_investment_affiliate_domestic_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_flag_investment_affiliate_domestic_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + 
    labs(title = "flag_investment_affiliate_domestic", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_flag_investment_affiliate_domestic <- att_gt(yname = "flag_investment_affiliate_domestic",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                                                  xformla = formula_cs, 
                                                  est_method = "ipw",base_period="universal",alp=0.05,
                                                  data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                                                  #print_details=FALSE,
                                                  anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_flag_investment_affiliate_domestic_smry <- aggte(cs_flag_investment_affiliate_domestic, type = "dynamic",na.rm = TRUE)
  
  p_flag_investment_affiliate_domestic_cs <- ggdid(cs_flag_investment_affiliate_domestic_smry, title = "flag_investment_affiliate_domestic")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_flag_investment_affiliate_domestic_cs <- aggte(cs_flag_investment_affiliate_domestic, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  flag_investment_affiliate_domestic_summary <- data.frame(method = method,
                                                           estimate = c(att_PSM_flag_investment_affiliate_domestic_sa20[1], a_flag_investment_affiliate_domestic_cs$overall.att#,a_flag_investment_affiliate_domestic_Ssynth$att_estimate
                                                           ),
                                                           se = c(att_PSM_flag_investment_affiliate_domestic_sa20[2],a_flag_investment_affiliate_domestic_cs$overall.se#,se_ssynth
                                                           ),
                                                           up = c(att_PSM_flag_investment_affiliate_domestic_sa20[1] + 1.645*att_PSM_flag_investment_affiliate_domestic_sa20[2],a_flag_investment_affiliate_domestic_cs$overall.att + 1.645*a_flag_investment_affiliate_domestic_cs$overall.se#, a_flag_investment_affiliate_domestic_Ssynth$att_estimate + 1.645 * se_ssynth
                                                           ),
                                                           down = c(att_PSM_flag_investment_affiliate_domestic_sa20[1] - 1.645*att_PSM_flag_investment_affiliate_domestic_sa20[2],a_flag_investment_affiliate_domestic_cs$overall.att - 1.645*a_flag_investment_affiliate_domestic_cs$overall.se#, a_flag_investment_affiliate_domestic_Ssynth$att_estimate - 1.645 * se_ssynth
                                                           ),
                                                           up95 = c(att_PSM_flag_investment_affiliate_domestic_sa20[1] + 1.960*att_PSM_flag_investment_affiliate_domestic_sa20[2],a_flag_investment_affiliate_domestic_cs$overall.att + 1.960*a_flag_investment_affiliate_domestic_cs$overall.se#,  a_flag_investment_affiliate_domestic_Ssynth$att_estimate + 1.960 * se_ssynth
                                                           ),
                                                           down95 = c(att_PSM_flag_investment_affiliate_domestic_sa20[1] - 1.960*att_PSM_flag_investment_affiliate_domestic_sa20[2],a_flag_investment_affiliate_domestic_cs$overall.att - 1.960*a_flag_investment_affiliate_domestic_cs$overall.se#,  a_flag_investment_affiliate_domestic_Ssynth$att_estimate - 1.960 * se_ssynth
                                                           ),
                                                           up99 = c(att_PSM_flag_investment_affiliate_domestic_sa20[1] + 2.5758*att_PSM_flag_investment_affiliate_domestic_sa20[2],a_flag_investment_affiliate_domestic_cs$overall.att + 2.5758*a_flag_investment_affiliate_domestic_cs$overall.se#,  a_flag_investment_affiliate_domestic_Ssynth$att_estimate + 2.5758 * se_ssynth
                                                           ),
                                                           down99 = c(att_PSM_flag_investment_affiliate_domestic_sa20[1] - 2.5758*att_PSM_flag_investment_affiliate_domestic_sa20[2],a_flag_investment_affiliate_domestic_cs$overall.att - 2.5758*a_flag_investment_affiliate_domestic_cs$overall.se#,  a_flag_investment_affiliate_domestic_Ssynth$att_estimate - 2.5758 * se_ssynth
                                                           )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  flag_investment_affiliate_domestic_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_flag_investment_affiliate_domestic_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_flag_investment_affiliate_domestic_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_flag_investment_affiliate_domestic_sa20$coeftable[,1],
           sa_se =es_PSM_flag_investment_affiliate_domestic_sa20$coeftable[,2],
           sa_t =es_PSM_flag_investment_affiliate_domestic_sa20$coeftable[,3],
           sa_p =es_PSM_flag_investment_affiliate_domestic_sa20$coeftable[,4])
  flag_investment_affiliate_domestic_summary_appendix_cs <- data.frame(year = cs_flag_investment_affiliate_domestic_smry$egt,
                                                                       cs =  cs_flag_investment_affiliate_domestic_smry$att.egt,
                                                                       cs_se = cs_flag_investment_affiliate_domestic_smry$se.egt,
                                                                       cs_cband_lower = cs_flag_investment_affiliate_domestic_smry$att.egt - cs_flag_investment_affiliate_domestic_smry$crit.val.egt*cs_flag_investment_affiliate_domestic_smry$se.egt,
                                                                       cs_cband_upper = cs_flag_investment_affiliate_domestic_smry$att.egt + cs_flag_investment_affiliate_domestic_smry$crit.val.egt*cs_flag_investment_affiliate_domestic_smry$se.egt)
  flag_investment_affiliate_domestic_summary_appendix <- right_join(flag_investment_affiliate_domestic_summary_appendix_sa,flag_investment_affiliate_domestic_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_flag_investment_affiliate_domestic_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- flag_investment_affiliate_domestic_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(flag_investment_affiliate_domestic_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- flag_investment_affiliate_domestic_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(flag_investment_affiliate_domestic_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  flag_investment_affiliate_domestic_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  flag_investment_affiliate_domestic_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(flag_investment_affiliate_domestic_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(flag_investment_affiliate_domestic_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  flag_investment_affiliate_domestic_summary <- cbind(flag_investment_affiliate_domestic_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(flag_investment_affiliate_domestic_summary$rob)
  
  p_flag_investment_affiliate_domestic_summary <- ggplot(flag_investment_affiliate_domestic_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=flag_investment_affiliate_domestic_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_investment_affiliate_domestic_summary$pretrend[1]) +
    geom_rect(data=flag_investment_affiliate_domestic_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_investment_affiliate_domestic_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(flag_investment_affiliate_domestic_summary$down, na.rm=T),max(flag_investment_affiliate_domestic_summary$up, na.rm=T)), max(-min(flag_investment_affiliate_domestic_summary$down, na.rm=T),max(flag_investment_affiliate_domestic_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_flag_investment_affiliate_domestic <- p_PSM_flag_investment_affiliate_domestic_sa20+p_flag_investment_affiliate_domestic_cs+p_flag_investment_affiliate_domestic_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_flag_investment_affiliate_domestic)
  ################## flag_investment_affiliate_overseas------------
  # ipw
  es_PSM_flag_investment_affiliate_overseas_sa20 = feols(flag_investment_affiliate_overseas ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_flag_investment_affiliate_overseas_sa20 <- aggregate(es_PSM_flag_investment_affiliate_overseas_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_flag_investment_affiliate_overseas_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_flag_investment_affiliate_overseas_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + 
    labs(title = "flag_investment_affiliate_overseas", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_flag_investment_affiliate_overseas <- att_gt(yname = "flag_investment_affiliate_overseas",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                                                  xformla = formula_cs, 
                                                  est_method = "ipw",base_period="universal",alp=0.05,
                                                  data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                                                  #print_details=FALSE,
                                                  anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_flag_investment_affiliate_overseas_smry <- aggte(cs_flag_investment_affiliate_overseas, type = "dynamic",na.rm = TRUE)
  
  p_flag_investment_affiliate_overseas_cs <- ggdid(cs_flag_investment_affiliate_overseas_smry, title = "flag_investment_affiliate_overseas")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_flag_investment_affiliate_overseas_cs <- aggte(cs_flag_investment_affiliate_overseas, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  flag_investment_affiliate_overseas_summary <- data.frame(method = method,
                                                           estimate = c(att_PSM_flag_investment_affiliate_overseas_sa20[1], a_flag_investment_affiliate_overseas_cs$overall.att#,a_flag_investment_affiliate_overseas_Ssynth$att_estimate
                                                           ),
                                                           se = c(att_PSM_flag_investment_affiliate_overseas_sa20[2],a_flag_investment_affiliate_overseas_cs$overall.se#,se_ssynth
                                                           ),
                                                           up = c(att_PSM_flag_investment_affiliate_overseas_sa20[1] + 1.645*att_PSM_flag_investment_affiliate_overseas_sa20[2],a_flag_investment_affiliate_overseas_cs$overall.att + 1.645*a_flag_investment_affiliate_overseas_cs$overall.se#, a_flag_investment_affiliate_overseas_Ssynth$att_estimate + 1.645 * se_ssynth
                                                           ),
                                                           down = c(att_PSM_flag_investment_affiliate_overseas_sa20[1] - 1.645*att_PSM_flag_investment_affiliate_overseas_sa20[2],a_flag_investment_affiliate_overseas_cs$overall.att - 1.645*a_flag_investment_affiliate_overseas_cs$overall.se#, a_flag_investment_affiliate_overseas_Ssynth$att_estimate - 1.645 * se_ssynth
                                                           ),
                                                           up95 = c(att_PSM_flag_investment_affiliate_overseas_sa20[1] + 1.960*att_PSM_flag_investment_affiliate_overseas_sa20[2],a_flag_investment_affiliate_overseas_cs$overall.att + 1.960*a_flag_investment_affiliate_overseas_cs$overall.se#,  a_flag_investment_affiliate_overseas_Ssynth$att_estimate + 1.960 * se_ssynth
                                                           ),
                                                           down95 = c(att_PSM_flag_investment_affiliate_overseas_sa20[1] - 1.960*att_PSM_flag_investment_affiliate_overseas_sa20[2],a_flag_investment_affiliate_overseas_cs$overall.att - 1.960*a_flag_investment_affiliate_overseas_cs$overall.se#,  a_flag_investment_affiliate_overseas_Ssynth$att_estimate - 1.960 * se_ssynth
                                                           ),
                                                           up99 = c(att_PSM_flag_investment_affiliate_overseas_sa20[1] + 2.5758*att_PSM_flag_investment_affiliate_overseas_sa20[2],a_flag_investment_affiliate_overseas_cs$overall.att + 2.5758*a_flag_investment_affiliate_overseas_cs$overall.se#,  a_flag_investment_affiliate_overseas_Ssynth$att_estimate + 2.5758 * se_ssynth
                                                           ),
                                                           down99 = c(att_PSM_flag_investment_affiliate_overseas_sa20[1] - 2.5758*att_PSM_flag_investment_affiliate_overseas_sa20[2],a_flag_investment_affiliate_overseas_cs$overall.att - 2.5758*a_flag_investment_affiliate_overseas_cs$overall.se#,  a_flag_investment_affiliate_overseas_Ssynth$att_estimate - 2.5758 * se_ssynth
                                                           )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  flag_investment_affiliate_overseas_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_flag_investment_affiliate_overseas_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_flag_investment_affiliate_overseas_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_flag_investment_affiliate_overseas_sa20$coeftable[,1],
           sa_se =es_PSM_flag_investment_affiliate_overseas_sa20$coeftable[,2],
           sa_t =es_PSM_flag_investment_affiliate_overseas_sa20$coeftable[,3],
           sa_p =es_PSM_flag_investment_affiliate_overseas_sa20$coeftable[,4])
  flag_investment_affiliate_overseas_summary_appendix_cs <- data.frame(year = cs_flag_investment_affiliate_overseas_smry$egt,
                                                                       cs =  cs_flag_investment_affiliate_overseas_smry$att.egt,
                                                                       cs_se = cs_flag_investment_affiliate_overseas_smry$se.egt,
                                                                       cs_cband_lower = cs_flag_investment_affiliate_overseas_smry$att.egt - cs_flag_investment_affiliate_overseas_smry$crit.val.egt*cs_flag_investment_affiliate_overseas_smry$se.egt,
                                                                       cs_cband_upper = cs_flag_investment_affiliate_overseas_smry$att.egt + cs_flag_investment_affiliate_overseas_smry$crit.val.egt*cs_flag_investment_affiliate_overseas_smry$se.egt)
  flag_investment_affiliate_overseas_summary_appendix <- right_join(flag_investment_affiliate_overseas_summary_appendix_sa,flag_investment_affiliate_overseas_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_flag_investment_affiliate_overseas_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- flag_investment_affiliate_overseas_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(flag_investment_affiliate_overseas_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- flag_investment_affiliate_overseas_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(flag_investment_affiliate_overseas_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  flag_investment_affiliate_overseas_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  flag_investment_affiliate_overseas_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(flag_investment_affiliate_overseas_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(flag_investment_affiliate_overseas_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  flag_investment_affiliate_overseas_summary <- cbind(flag_investment_affiliate_overseas_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(flag_investment_affiliate_overseas_summary$rob)
  
  p_flag_investment_affiliate_overseas_summary <- ggplot(flag_investment_affiliate_overseas_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=flag_investment_affiliate_overseas_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_investment_affiliate_overseas_summary$pretrend[1]) +
    geom_rect(data=flag_investment_affiliate_overseas_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_investment_affiliate_overseas_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(flag_investment_affiliate_overseas_summary$down, na.rm=T),max(flag_investment_affiliate_overseas_summary$up, na.rm=T)), max(-min(flag_investment_affiliate_overseas_summary$down, na.rm=T),max(flag_investment_affiliate_overseas_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_flag_investment_affiliate_overseas <- p_PSM_flag_investment_affiliate_overseas_sa20+p_flag_investment_affiliate_overseas_cs+p_flag_investment_affiliate_overseas_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_flag_investment_affiliate_overseas)
  ################## flag_dividend------------
  # ipw
  es_PSM_flag_dividend_sa20 = feols(flag_dividend ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_flag_dividend_sa20 <- aggregate(es_PSM_flag_dividend_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_flag_dividend_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_flag_dividend_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + 
    labs(title = "flag_dividend", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_flag_dividend <- att_gt(yname = "flag_dividend",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                             xformla = formula_cs, 
                             est_method = "ipw",base_period="universal",alp=0.05,
                             data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                             #print_details=FALSE,
                             anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_flag_dividend_smry <- aggte(cs_flag_dividend, type = "dynamic",na.rm = TRUE)
  
  p_flag_dividend_cs <- ggdid(cs_flag_dividend_smry, title = "flag_dividend")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_flag_dividend_cs <- aggte(cs_flag_dividend, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  flag_dividend_summary <- data.frame(method = method,
                                      estimate = c(att_PSM_flag_dividend_sa20[1], a_flag_dividend_cs$overall.att#,a_flag_dividend_Ssynth$att_estimate
                                      ),
                                      se = c(att_PSM_flag_dividend_sa20[2],a_flag_dividend_cs$overall.se#,se_ssynth
                                      ),
                                      up = c(att_PSM_flag_dividend_sa20[1] + 1.645*att_PSM_flag_dividend_sa20[2],a_flag_dividend_cs$overall.att + 1.645*a_flag_dividend_cs$overall.se#, a_flag_dividend_Ssynth$att_estimate + 1.645 * se_ssynth
                                      ),
                                      down = c(att_PSM_flag_dividend_sa20[1] - 1.645*att_PSM_flag_dividend_sa20[2],a_flag_dividend_cs$overall.att - 1.645*a_flag_dividend_cs$overall.se#, a_flag_dividend_Ssynth$att_estimate - 1.645 * se_ssynth
                                      ),
                                      up95 = c(att_PSM_flag_dividend_sa20[1] + 1.960*att_PSM_flag_dividend_sa20[2],a_flag_dividend_cs$overall.att + 1.960*a_flag_dividend_cs$overall.se#,  a_flag_dividend_Ssynth$att_estimate + 1.960 * se_ssynth
                                      ),
                                      down95 = c(att_PSM_flag_dividend_sa20[1] - 1.960*att_PSM_flag_dividend_sa20[2],a_flag_dividend_cs$overall.att - 1.960*a_flag_dividend_cs$overall.se#,  a_flag_dividend_Ssynth$att_estimate - 1.960 * se_ssynth
                                      ),
                                      up99 = c(att_PSM_flag_dividend_sa20[1] + 2.5758*att_PSM_flag_dividend_sa20[2],a_flag_dividend_cs$overall.att + 2.5758*a_flag_dividend_cs$overall.se#,  a_flag_dividend_Ssynth$att_estimate + 2.5758 * se_ssynth
                                      ),
                                      down99 = c(att_PSM_flag_dividend_sa20[1] - 2.5758*att_PSM_flag_dividend_sa20[2],a_flag_dividend_cs$overall.att - 2.5758*a_flag_dividend_cs$overall.se#,  a_flag_dividend_Ssynth$att_estimate - 2.5758 * se_ssynth
                                      )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  flag_dividend_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_flag_dividend_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_flag_dividend_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_flag_dividend_sa20$coeftable[,1],
           sa_se =es_PSM_flag_dividend_sa20$coeftable[,2],
           sa_t =es_PSM_flag_dividend_sa20$coeftable[,3],
           sa_p =es_PSM_flag_dividend_sa20$coeftable[,4])
  flag_dividend_summary_appendix_cs <- data.frame(year = cs_flag_dividend_smry$egt,
                                                  cs =  cs_flag_dividend_smry$att.egt,
                                                  cs_se = cs_flag_dividend_smry$se.egt,
                                                  cs_cband_lower = cs_flag_dividend_smry$att.egt - cs_flag_dividend_smry$crit.val.egt*cs_flag_dividend_smry$se.egt,
                                                  cs_cband_upper = cs_flag_dividend_smry$att.egt + cs_flag_dividend_smry$crit.val.egt*cs_flag_dividend_smry$se.egt)
  flag_dividend_summary_appendix <- right_join(flag_dividend_summary_appendix_sa,flag_dividend_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_flag_dividend_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- flag_dividend_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(flag_dividend_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- flag_dividend_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(flag_dividend_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  flag_dividend_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  flag_dividend_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(flag_dividend_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(flag_dividend_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  flag_dividend_summary <- cbind(flag_dividend_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(flag_dividend_summary$rob)
  
  p_flag_dividend_summary <- ggplot(flag_dividend_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=flag_dividend_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_dividend_summary$pretrend[1]) +
    geom_rect(data=flag_dividend_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_dividend_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(flag_dividend_summary$down, na.rm=T),max(flag_dividend_summary$up, na.rm=T)), max(-min(flag_dividend_summary$down, na.rm=T),max(flag_dividend_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_flag_dividend <- p_PSM_flag_dividend_sa20+p_flag_dividend_cs+p_flag_dividend_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_flag_dividend)
  ################## log_investment_affiliate_domestic------------
  # ipw
  es_PSM_log_investment_affiliate_domestic_sa20 = feols(log_investment_affiliate_domestic ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_log_investment_affiliate_domestic_sa20 <- aggregate(es_PSM_log_investment_affiliate_domestic_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_investment_affiliate_domestic_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_investment_affiliate_domestic_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_investment_affiliate_domestic", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_investment_affiliate_domestic <- att_gt(yname = "log_investment_affiliate_domestic",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                                                 xformla = formula_cs, 
                                                 est_method = "ipw",base_period="universal",alp=0.05,
                                                 data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                                                 #print_details=FALSE,
                                                 anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_investment_affiliate_domestic_smry <- aggte(cs_log_investment_affiliate_domestic, type = "dynamic",na.rm = TRUE)
  
  p_log_investment_affiliate_domestic_cs <- ggdid(cs_log_investment_affiliate_domestic_smry, title = "log_investment_affiliate_domestic")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_log_investment_affiliate_domestic_cs <- aggte(cs_log_investment_affiliate_domestic, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  log_investment_affiliate_domestic_summary <- data.frame(method = method,
                                                          estimate = c(att_PSM_log_investment_affiliate_domestic_sa20[1], a_log_investment_affiliate_domestic_cs$overall.att#,a_log_investment_affiliate_domestic_Ssynth$att_estimate
                                                          ),
                                                          se = c(att_PSM_log_investment_affiliate_domestic_sa20[2],a_log_investment_affiliate_domestic_cs$overall.se#,se_ssynth
                                                          ),
                                                          up = c(att_PSM_log_investment_affiliate_domestic_sa20[1] + 1.645*att_PSM_log_investment_affiliate_domestic_sa20[2],a_log_investment_affiliate_domestic_cs$overall.att + 1.645*a_log_investment_affiliate_domestic_cs$overall.se#, a_log_investment_affiliate_domestic_Ssynth$att_estimate + 1.645 * se_ssynth
                                                          ),
                                                          down = c(att_PSM_log_investment_affiliate_domestic_sa20[1] - 1.645*att_PSM_log_investment_affiliate_domestic_sa20[2],a_log_investment_affiliate_domestic_cs$overall.att - 1.645*a_log_investment_affiliate_domestic_cs$overall.se#, a_log_investment_affiliate_domestic_Ssynth$att_estimate - 1.645 * se_ssynth
                                                          ),
                                                          up95 = c(att_PSM_log_investment_affiliate_domestic_sa20[1] + 1.960*att_PSM_log_investment_affiliate_domestic_sa20[2],a_log_investment_affiliate_domestic_cs$overall.att + 1.960*a_log_investment_affiliate_domestic_cs$overall.se#,  a_log_investment_affiliate_domestic_Ssynth$att_estimate + 1.960 * se_ssynth
                                                          ),
                                                          down95 = c(att_PSM_log_investment_affiliate_domestic_sa20[1] - 1.960*att_PSM_log_investment_affiliate_domestic_sa20[2],a_log_investment_affiliate_domestic_cs$overall.att - 1.960*a_log_investment_affiliate_domestic_cs$overall.se#,  a_log_investment_affiliate_domestic_Ssynth$att_estimate - 1.960 * se_ssynth
                                                          ),
                                                          up99 = c(att_PSM_log_investment_affiliate_domestic_sa20[1] + 2.5758*att_PSM_log_investment_affiliate_domestic_sa20[2],a_log_investment_affiliate_domestic_cs$overall.att + 2.5758*a_log_investment_affiliate_domestic_cs$overall.se#,  a_log_investment_affiliate_domestic_Ssynth$att_estimate + 2.5758 * se_ssynth
                                                          ),
                                                          down99 = c(att_PSM_log_investment_affiliate_domestic_sa20[1] - 2.5758*att_PSM_log_investment_affiliate_domestic_sa20[2],a_log_investment_affiliate_domestic_cs$overall.att - 2.5758*a_log_investment_affiliate_domestic_cs$overall.se#,  a_log_investment_affiliate_domestic_Ssynth$att_estimate - 2.5758 * se_ssynth
                                                          )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  log_investment_affiliate_domestic_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_log_investment_affiliate_domestic_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_log_investment_affiliate_domestic_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_log_investment_affiliate_domestic_sa20$coeftable[,1],
           sa_se =es_PSM_log_investment_affiliate_domestic_sa20$coeftable[,2],
           sa_t =es_PSM_log_investment_affiliate_domestic_sa20$coeftable[,3],
           sa_p =es_PSM_log_investment_affiliate_domestic_sa20$coeftable[,4])
  log_investment_affiliate_domestic_summary_appendix_cs <- data.frame(year = cs_log_investment_affiliate_domestic_smry$egt,
                                                                      cs =  cs_log_investment_affiliate_domestic_smry$att.egt,
                                                                      cs_se = cs_log_investment_affiliate_domestic_smry$se.egt,
                                                                      cs_cband_lower = cs_log_investment_affiliate_domestic_smry$att.egt - cs_log_investment_affiliate_domestic_smry$crit.val.egt*cs_log_investment_affiliate_domestic_smry$se.egt,
                                                                      cs_cband_upper = cs_log_investment_affiliate_domestic_smry$att.egt + cs_log_investment_affiliate_domestic_smry$crit.val.egt*cs_log_investment_affiliate_domestic_smry$se.egt)
  log_investment_affiliate_domestic_summary_appendix <- right_join(log_investment_affiliate_domestic_summary_appendix_sa,log_investment_affiliate_domestic_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_log_investment_affiliate_domestic_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- log_investment_affiliate_domestic_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(log_investment_affiliate_domestic_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- log_investment_affiliate_domestic_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(log_investment_affiliate_domestic_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  log_investment_affiliate_domestic_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  log_investment_affiliate_domestic_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(log_investment_affiliate_domestic_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(log_investment_affiliate_domestic_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  log_investment_affiliate_domestic_summary <- cbind(log_investment_affiliate_domestic_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(log_investment_affiliate_domestic_summary$rob)
  
  p_log_investment_affiliate_domestic_summary <- ggplot(log_investment_affiliate_domestic_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_investment_affiliate_domestic_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_investment_affiliate_domestic_summary$pretrend[1]) +
    geom_rect(data=log_investment_affiliate_domestic_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_investment_affiliate_domestic_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_investment_affiliate_domestic_summary$down, na.rm=T),max(log_investment_affiliate_domestic_summary$up, na.rm=T)), max(-min(log_investment_affiliate_domestic_summary$down, na.rm=T),max(log_investment_affiliate_domestic_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_investment_affiliate_domestic <- p_PSM_log_investment_affiliate_domestic_sa20+p_log_investment_affiliate_domestic_cs+p_log_investment_affiliate_domestic_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_log_investment_affiliate_domestic)
  ################## log_investment_affiliate_overseas------------
  # ipw
  es_PSM_log_investment_affiliate_overseas_sa20 = feols(log_investment_affiliate_overseas ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_log_investment_affiliate_overseas_sa20 <- aggregate(es_PSM_log_investment_affiliate_overseas_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_investment_affiliate_overseas_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_investment_affiliate_overseas_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_investment_affiliate_overseas", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_investment_affiliate_overseas <- att_gt(yname = "log_investment_affiliate_overseas",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                                                 xformla = formula_cs, 
                                                 est_method = "ipw",base_period="universal",alp=0.05,
                                                 data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                                                 #print_details=FALSE,
                                                 anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_investment_affiliate_overseas_smry <- aggte(cs_log_investment_affiliate_overseas, type = "dynamic",na.rm = TRUE)
  
  p_log_investment_affiliate_overseas_cs <- ggdid(cs_log_investment_affiliate_overseas_smry, title = "log_investment_affiliate_overseas")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_log_investment_affiliate_overseas_cs <- aggte(cs_log_investment_affiliate_overseas, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  log_investment_affiliate_overseas_summary <- data.frame(method = method,
                                                          estimate = c(att_PSM_log_investment_affiliate_overseas_sa20[1], a_log_investment_affiliate_overseas_cs$overall.att#,a_log_investment_affiliate_overseas_Ssynth$att_estimate
                                                          ),
                                                          se = c(att_PSM_log_investment_affiliate_overseas_sa20[2],a_log_investment_affiliate_overseas_cs$overall.se#,se_ssynth
                                                          ),
                                                          up = c(att_PSM_log_investment_affiliate_overseas_sa20[1] + 1.645*att_PSM_log_investment_affiliate_overseas_sa20[2],a_log_investment_affiliate_overseas_cs$overall.att + 1.645*a_log_investment_affiliate_overseas_cs$overall.se#, a_log_investment_affiliate_overseas_Ssynth$att_estimate + 1.645 * se_ssynth
                                                          ),
                                                          down = c(att_PSM_log_investment_affiliate_overseas_sa20[1] - 1.645*att_PSM_log_investment_affiliate_overseas_sa20[2],a_log_investment_affiliate_overseas_cs$overall.att - 1.645*a_log_investment_affiliate_overseas_cs$overall.se#, a_log_investment_affiliate_overseas_Ssynth$att_estimate - 1.645 * se_ssynth
                                                          ),
                                                          up95 = c(att_PSM_log_investment_affiliate_overseas_sa20[1] + 1.960*att_PSM_log_investment_affiliate_overseas_sa20[2],a_log_investment_affiliate_overseas_cs$overall.att + 1.960*a_log_investment_affiliate_overseas_cs$overall.se#,  a_log_investment_affiliate_overseas_Ssynth$att_estimate + 1.960 * se_ssynth
                                                          ),
                                                          down95 = c(att_PSM_log_investment_affiliate_overseas_sa20[1] - 1.960*att_PSM_log_investment_affiliate_overseas_sa20[2],a_log_investment_affiliate_overseas_cs$overall.att - 1.960*a_log_investment_affiliate_overseas_cs$overall.se#,  a_log_investment_affiliate_overseas_Ssynth$att_estimate - 1.960 * se_ssynth
                                                          ),
                                                          up99 = c(att_PSM_log_investment_affiliate_overseas_sa20[1] + 2.5758*att_PSM_log_investment_affiliate_overseas_sa20[2],a_log_investment_affiliate_overseas_cs$overall.att + 2.5758*a_log_investment_affiliate_overseas_cs$overall.se#,  a_log_investment_affiliate_overseas_Ssynth$att_estimate + 2.5758 * se_ssynth
                                                          ),
                                                          down99 = c(att_PSM_log_investment_affiliate_overseas_sa20[1] - 2.5758*att_PSM_log_investment_affiliate_overseas_sa20[2],a_log_investment_affiliate_overseas_cs$overall.att - 2.5758*a_log_investment_affiliate_overseas_cs$overall.se#,  a_log_investment_affiliate_overseas_Ssynth$att_estimate - 2.5758 * se_ssynth
                                                          )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  log_investment_affiliate_overseas_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_log_investment_affiliate_overseas_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_log_investment_affiliate_overseas_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_log_investment_affiliate_overseas_sa20$coeftable[,1],
           sa_se =es_PSM_log_investment_affiliate_overseas_sa20$coeftable[,2],
           sa_t =es_PSM_log_investment_affiliate_overseas_sa20$coeftable[,3],
           sa_p =es_PSM_log_investment_affiliate_overseas_sa20$coeftable[,4])
  log_investment_affiliate_overseas_summary_appendix_cs <- data.frame(year = cs_log_investment_affiliate_overseas_smry$egt,
                                                                      cs =  cs_log_investment_affiliate_overseas_smry$att.egt,
                                                                      cs_se = cs_log_investment_affiliate_overseas_smry$se.egt,
                                                                      cs_cband_lower = cs_log_investment_affiliate_overseas_smry$att.egt - cs_log_investment_affiliate_overseas_smry$crit.val.egt*cs_log_investment_affiliate_overseas_smry$se.egt,
                                                                      cs_cband_upper = cs_log_investment_affiliate_overseas_smry$att.egt + cs_log_investment_affiliate_overseas_smry$crit.val.egt*cs_log_investment_affiliate_overseas_smry$se.egt)
  log_investment_affiliate_overseas_summary_appendix <- right_join(log_investment_affiliate_overseas_summary_appendix_sa,log_investment_affiliate_overseas_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_log_investment_affiliate_overseas_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- log_investment_affiliate_overseas_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(log_investment_affiliate_overseas_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- log_investment_affiliate_overseas_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(log_investment_affiliate_overseas_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  log_investment_affiliate_overseas_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  log_investment_affiliate_overseas_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(log_investment_affiliate_overseas_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(log_investment_affiliate_overseas_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  log_investment_affiliate_overseas_summary <- cbind(log_investment_affiliate_overseas_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(log_investment_affiliate_overseas_summary$rob)
  
  p_log_investment_affiliate_overseas_summary <- ggplot(log_investment_affiliate_overseas_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_investment_affiliate_overseas_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_investment_affiliate_overseas_summary$pretrend[1]) +
    geom_rect(data=log_investment_affiliate_overseas_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_investment_affiliate_overseas_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_investment_affiliate_overseas_summary$down, na.rm=T),max(log_investment_affiliate_overseas_summary$up, na.rm=T)), max(-min(log_investment_affiliate_overseas_summary$down, na.rm=T),max(log_investment_affiliate_overseas_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_investment_affiliate_overseas <- p_PSM_log_investment_affiliate_overseas_sa20+p_log_investment_affiliate_overseas_cs+p_log_investment_affiliate_overseas_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_log_investment_affiliate_overseas)
  ################## log_dividend------------
  # ipw
  es_PSM_log_dividend_sa20 = feols(log_dividend ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw_trimmed, cluster="id", weights=dfm_kikatsu2_ipw_trimmed$weights)
  att_PSM_log_dividend_sa20 <- aggregate(es_PSM_log_dividend_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_dividend_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_dividend_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_dividend", subtitle = "S&A - IPW, 90 & 95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_dividend <- att_gt(yname = "log_dividend",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                            xformla = formula_cs, 
                            est_method = "ipw",base_period="universal",alp=0.05,
                            data = dfm_kikatsu2_ipw_trimmed,#pl = TRUE,
                            #print_details=FALSE,
                            anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_dividend_smry <- aggte(cs_log_dividend, type = "dynamic",na.rm = TRUE)
  
  p_log_dividend_cs <- ggdid(cs_log_dividend_smry, title = "log_dividend")+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  a_log_dividend_cs <- aggte(cs_log_dividend, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A(2020)", "C&S(2021)")
  log_dividend_summary <- data.frame(method = method,
                                     estimate = c(att_PSM_log_dividend_sa20[1], a_log_dividend_cs$overall.att#,a_log_dividend_Ssynth$att_estimate
                                     ),
                                     se = c(att_PSM_log_dividend_sa20[2],a_log_dividend_cs$overall.se#,se_ssynth
                                     ),
                                     up = c(att_PSM_log_dividend_sa20[1] + 1.645*att_PSM_log_dividend_sa20[2],a_log_dividend_cs$overall.att + 1.645*a_log_dividend_cs$overall.se#, a_log_dividend_Ssynth$att_estimate + 1.645 * se_ssynth
                                     ),
                                     down = c(att_PSM_log_dividend_sa20[1] - 1.645*att_PSM_log_dividend_sa20[2],a_log_dividend_cs$overall.att - 1.645*a_log_dividend_cs$overall.se#, a_log_dividend_Ssynth$att_estimate - 1.645 * se_ssynth
                                     ),
                                     up95 = c(att_PSM_log_dividend_sa20[1] + 1.960*att_PSM_log_dividend_sa20[2],a_log_dividend_cs$overall.att + 1.960*a_log_dividend_cs$overall.se#,  a_log_dividend_Ssynth$att_estimate + 1.960 * se_ssynth
                                     ),
                                     down95 = c(att_PSM_log_dividend_sa20[1] - 1.960*att_PSM_log_dividend_sa20[2],a_log_dividend_cs$overall.att - 1.960*a_log_dividend_cs$overall.se#,  a_log_dividend_Ssynth$att_estimate - 1.960 * se_ssynth
                                     ),
                                     up99 = c(att_PSM_log_dividend_sa20[1] + 2.5758*att_PSM_log_dividend_sa20[2],a_log_dividend_cs$overall.att + 2.5758*a_log_dividend_cs$overall.se#,  a_log_dividend_Ssynth$att_estimate + 2.5758 * se_ssynth
                                     ),
                                     down99 = c(att_PSM_log_dividend_sa20[1] - 2.5758*att_PSM_log_dividend_sa20[2],a_log_dividend_cs$overall.att - 2.5758*a_log_dividend_cs$overall.se#,  a_log_dividend_Ssynth$att_estimate - 2.5758 * se_ssynth
                                     )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  # create period specific att table
  log_dividend_summary_appendix_sa <- data.frame(year = as.numeric(es_PSM_log_dividend_sa20$model_matrix_info[[1]]$items))%>%
    filter(year != as.numeric(es_PSM_log_dividend_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_log_dividend_sa20$coeftable[,1],
           sa_se =es_PSM_log_dividend_sa20$coeftable[,2],
           sa_t =es_PSM_log_dividend_sa20$coeftable[,3],
           sa_p =es_PSM_log_dividend_sa20$coeftable[,4])
  log_dividend_summary_appendix_cs <- data.frame(year = cs_log_dividend_smry$egt,
                                                 cs =  cs_log_dividend_smry$att.egt,
                                                 cs_se = cs_log_dividend_smry$se.egt,
                                                 cs_cband_lower = cs_log_dividend_smry$att.egt - cs_log_dividend_smry$crit.val.egt*cs_log_dividend_smry$se.egt,
                                                 cs_cband_upper = cs_log_dividend_smry$att.egt + cs_log_dividend_smry$crit.val.egt*cs_log_dividend_smry$se.egt)
  log_dividend_summary_appendix <- right_join(log_dividend_summary_appendix_sa,log_dividend_summary_appendix_cs,  by = c("year")) %>%
    arrange(year)%>%
    mutate(pretrend = ifelse(year < es_PSM_log_dividend_sa20$model_matrix_info[[1]]$ref,1,NA),
           sa_pretrend = ifelse(sa_p <= 0.05,1,0)*pretrend,
           cs_pretrend = ifelse(cs_cband_lower*cs_cband_upper > 0 ,1,0))
  
  ## pretrend test if its too large (checked based -7 to -3 years)
  sa_pretrend_mean0.0 <- log_dividend_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa) %>% abs()
  sa_pretrend_mean0 <- mean(sa_pretrend_mean0.0$sa)
  sa_pretrend_mean <- ifelse(sa_pretrend_mean0 > abs(log_dividend_summary$estimate[1]*1), 1, 0)
  cs_pretrend_mean0.0 <- log_dividend_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs) %>% abs()
  cs_pretrend_mean0 <- mean(cs_pretrend_mean0.0$cs)
  cs_pretrend_mean <- ifelse(cs_pretrend_mean0 > abs(log_dividend_summary$estimate[2]*1), 1, 0)
  
  ## pretrend test if positive observed more than once  (checked based -10 to -3 years)
  sa_pretrend_time <-  log_dividend_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(sa_pretrend) %>% sum()
  cs_pretrend_time <-  log_dividend_summary_appendix %>% filter(pretrend == 1, year >=  -7) %>% select(cs_pretrend) %>% sum()
  ##ignore pretrend if its too small
  sa_ignore_pretrend <- ifelse(sa_pretrend_mean0 < abs(log_dividend_summary$estimate[1]/2), 1, 0)
  cs_ignore_pretrend <- ifelse(cs_pretrend_mean0 < abs(log_dividend_summary$estimate[2]/2), 1, 0)
  #judge pretrend based on above
  sa_pretrend_judge <- (sa_pretrend_mean + ifelse(sa_pretrend_time>1, 1, 0)) * (1-sa_ignore_pretrend) # there is an issue of multiple testing problem
  cs_pretrend_judge <- (cs_pretrend_mean + ifelse(cs_pretrend_time>0, 1, 0)) * (1-cs_ignore_pretrend) # simultaneous confidence band is calculated in CS
  pretrend0 <- c(sa_pretrend_judge, cs_pretrend_judge)
  ## reflect the pretrend test to the summary
  log_dividend_summary <- cbind(log_dividend_summary, pretrend0) %>%
    mutate(pretrend = ifelse(pretrend0 > 0,1,0),
           rob = ifelse(is.na(result)==FALSE & pretrend == 0, 1,0)
    )
  #  result_robustness <- sum(log_dividend_summary$rob)
  
  p_log_dividend_summary <- ggplot(log_dividend_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_dividend_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_dividend_summary$pretrend[1]) +
    geom_rect(data=log_dividend_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_dividend_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_dividend_summary$down, na.rm=T),max(log_dividend_summary$up, na.rm=T)), max(-min(log_dividend_summary$down, na.rm=T),max(log_dividend_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90 & 95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_dividend <- p_PSM_log_dividend_sa20+p_log_dividend_cs+p_log_dividend_summary + plot_layout(ncol = 3, widths = c(1,1,0.7))
  print( pa_log_dividend)
  ## output_investment-----------
  pa_investment_sum <- pa_flag_investment_affiliate_domestic/pa_flag_investment_affiliate_overseas/pa_flag_dividend/pa_log_investment_affiliate_domestic/pa_log_investment_affiliate_overseas/pa_log_dividend + plot_annotation(
    title = "Effect on the Investment",
    subtitle = target,
    ##caption = caption_main
  ) &
    theme(text = element_text(family="Meiryo UI"))
  
  # filename <- paste("03_robustness_output/", target,"_01investment_", count_intervention, ".png", sep="")
  # ggsave(filename,pa_log_investment_sum, 
  #        width=20, height =22.5, units = "cm", dpi=200) 
  
  filename_pdf <- paste("03_robustness_output/", target,"_37_trimmed_investment_", count_intervention, ".pdf", sep="")
  ggsave(filename_pdf,pa_investment_sum, 
         width=15, height =36, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("03_robustness_output/", target,"_37_trimmed_investment_", count_intervention, ".png", sep="")
  ggsave(filename_png,pa_investment_sum, 
         width=15, height =36, units = "cm", dpi=200) 
  
  
  write.xlsx(list("flag_investment_affiliate_dm"=flag_investment_affiliate_domestic_summary,"flag_investment_affiliate_ov"=flag_investment_affiliate_overseas_summary,"flag_dividend"=flag_dividend_summary,"log_investment_affiliate_dm"=log_investment_affiliate_domestic_summary,"log_investment_affiliate_ov"=log_investment_affiliate_overseas_summary,"log_dividend"=log_dividend_summary), paste("03_robustness_output/",target,"_37investment_trimmed_table.xlsx", sep=""))
  write.xlsx(list("flag_investment_affiliate_dm"=flag_investment_affiliate_domestic_summary_appendix,"flag_investment_affiliate_ov"=flag_investment_affiliate_overseas_summary_appendix,"flag_dividend"=flag_dividend_summary_appendix,"log_investment_affiliate_dm"=log_investment_affiliate_domestic_summary_appendix,"log_investment_affiliate_ov"=log_investment_affiliate_overseas_summary_appendix,"log_dividend"=log_dividend_summary_appendix), paste("03_robustness_output/",target,"_37investment_trimmed_table_appendix.xlsx"))
}




## conduct evaluation------------
print(target)
try(robustness_trimmed_asset(target))
try(robustness_trimmed_employment(target))
try(robustness_trimmed_business(target))
try(robustness_trimmed_trade(target))
try(robustness_trimmed_trainingRD(target))
try(robustness_trimmed_IP(target))
try(robustness_trimmed_investment(target))

# 40 define functions for robustness check - group-------------
robustness_group_asset <- function(target){
  ################## log_sum_asset------------
  #CS
  cs_log_sum_asset <- att_gt(yname = "log_sum_asset",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                             xformla = formula_cs, 
                             est_method = "ipw",base_period="universal",alp=0.05,
                             data = dfm_kikatsu2_ipw,
                             anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_log_sum_asset_group <- aggte(cs_log_sum_asset, type = "group",na.rm = TRUE)
  p_log_sum_asset_cs_group <- ggdid(cs_log_sum_asset_group)
  
  ## create grid arrange
  pa_log_sum_asset <- p_log_sum_asset_cs_group+ 
    labs(title = "log_sum_asset", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_log_sum_asset)
  ################## log_tangible_asset------------
  #CS
  cs_log_tangible_asset <- att_gt(yname = "log_tangible_asset",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                                  xformla = formula_cs, 
                                  est_method = "ipw",base_period="universal",alp=0.05,
                                  data = dfm_kikatsu2_ipw,
                                  anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_log_tangible_asset_group <- aggte(cs_log_tangible_asset, type = "group",na.rm = TRUE)
  p_log_tangible_asset_cs_group <- ggdid(cs_log_tangible_asset_group)
  
  ## create grid arrange
  pa_log_tangible_asset <- p_log_tangible_asset_cs_group+ 
    labs(title = "log_tangible_asset", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_log_tangible_asset)
  ################## log_intangible_asset------------
  #CS
  cs_log_intangible_asset <- att_gt(yname = "log_intangible_asset",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                                    xformla = formula_cs, 
                                    est_method = "ipw",base_period="universal",alp=0.05,
                                    data = dfm_kikatsu2_ipw,#pl = TRUE,
                                    #print_details=FALSE,
                                    anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_log_intangible_asset_group <- aggte(cs_log_intangible_asset, type = "group",na.rm = TRUE)
  p_log_intangible_asset_cs_group <- ggdid(cs_log_intangible_asset_group)
  
  ## create grid arrange
  pa_log_intangible_asset <- p_log_intangible_asset_cs_group+ 
    labs(title = "log_intangible_asset", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_log_intangible_asset)
  ## output_asset-----------
  pa_asset_sum <- pa_log_sum_asset / pa_log_tangible_asset / pa_log_intangible_asset + plot_annotation(
    title = "Effect on the Asset",
    subtitle = target,
    #caption = caption_main
  ) &
    theme(text = element_text(family="Meiryo UI"))
  
  filename_pdf <- paste("03_robustness_output/", target,"_41asset_", count_intervention, ".pdf", sep="")
  ggsave(filename_pdf,pa_asset_sum, 
         width=10, height =25.5, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("03_robustness_output/", target,"_41asset_", count_intervention, ".png", sep="")
  ggsave(filename_png,pa_asset_sum, 
         width=10, height =25.5, units = "cm", dpi=200) 
}

robustness_group_employment <- function(target){
  ################## log_workers------------
  #CS
  cs_log_workers <- att_gt(yname = "log_workers",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                           xformla = formula_cs, 
                           est_method = "ipw",base_period="universal",alp=0.05,
                           data = dfm_kikatsu2_ipw,#pl = TRUE,
                           #print_details=FALSE,
                           anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_log_workers_group <- aggte(cs_log_workers, type = "group",na.rm = TRUE)
  p_log_workers_cs_group <- ggdid(cs_log_workers_group)
  
  ## create grid arrange
  pa_log_workers <- p_log_workers_cs_group+ 
    labs(title = "log_workers", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_log_workers)
  ################## log_indefinite_workers------------
  #CS
  cs_log_indefinite_workers <- att_gt(yname = "log_indefinite_workers",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                                      xformla = formula_cs, 
                                      est_method = "ipw",base_period="universal",alp=0.05,
                                      data = dfm_kikatsu2_ipw,#pl = TRUE,
                                      #print_details=FALSE,
                                      anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_log_indefinite_workers_group <- aggte(cs_log_indefinite_workers, type = "group",na.rm = TRUE)
  p_log_indefinite_workers_cs_group <- ggdid(cs_log_indefinite_workers_group)
  
  ## create grid arrange
  pa_log_indefinite_workers <- p_log_indefinite_workers_cs_group+ 
    labs(title = "log_indefinite_workers", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_log_indefinite_workers)
  ################## log_fixedterm_workers------------
  #CS
  cs_log_fixedterm_workers <- att_gt(yname = "log_fixedterm_workers",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                                     xformla = formula_cs, 
                                     est_method = "ipw",base_period="universal",alp=0.05,
                                     data = dfm_kikatsu2_ipw,#pl = TRUE,
                                     #print_details=FALSE,
                                     anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_log_fixedterm_workers_group <- aggte(cs_log_fixedterm_workers, type = "group",na.rm = TRUE)
  p_log_fixedterm_workers_cs_group <- ggdid(cs_log_fixedterm_workers_group)
  
  ## create grid arrange
  pa_log_fixedterm_workers <- p_log_fixedterm_workers_cs_group+ 
    labs(title = "log_fixedterm_workers", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_log_fixedterm_workers)
  ################## log_fixedterm_workers_equivalent------------
  #CS
  cs_log_fixedterm_workers_equivalent <- att_gt(yname = "log_fixedterm_workers_equivalent",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                                                xformla = formula_cs, 
                                                est_method = "ipw",base_period="universal",alp=0.05,
                                                data = dfm_kikatsu2_ipw,#pl = TRUE,
                                                #print_details=FALSE,
                                                anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_log_fixedterm_workers_equivalent_group <- aggte(cs_log_fixedterm_workers_equivalent, type = "group",na.rm = TRUE)
  p_log_fixedterm_workers_equivalent_cs_group <- ggdid(cs_log_fixedterm_workers_equivalent_group)
  
  ## create grid arrange
  pa_log_fixedterm_workers_equivalent <- p_log_fixedterm_workers_equivalent_cs_group+ 
    labs(title = "log_fixedterm_workers_equivalent", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_log_fixedterm_workers_equivalent)
  ################## log_salary------------
  #CS
  cs_log_salary <- att_gt(yname = "log_salary",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                          xformla = formula_cs, 
                          est_method = "ipw",base_period="universal",alp=0.05,
                          data = dfm_kikatsu2_ipw,#pl = TRUE,
                          #print_details=FALSE,
                          anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_log_salary_group <- aggte(cs_log_salary, type = "group",na.rm = TRUE)
  p_log_salary_cs_group <- ggdid(cs_log_salary_group)
  
  ## create grid arrange
  pa_log_salary <- p_log_salary_cs_group+ 
    labs(title = "log_salary", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_log_salary)
  ################## log_benefit------------
  #CS
  cs_log_benefit <- att_gt(yname = "log_benefit",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                           xformla = formula_cs, 
                           est_method = "ipw",base_period="universal",alp=0.05,
                           data = dfm_kikatsu2_ipw,#pl = TRUE,
                           #print_details=FALSE,
                           anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_log_benefit_group <- aggte(cs_log_benefit, type = "group",na.rm = TRUE)
  p_log_benefit_cs_group <- ggdid(cs_log_benefit_group)
  
  ## create grid arrange
  pa_log_benefit <- p_log_benefit_cs_group+ 
    labs(title = "log_benefit", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_log_benefit)
  ## output_employment-----------
  pa_employment_sum <- pa_log_workers/pa_log_indefinite_workers/pa_log_fixedterm_workers/pa_log_fixedterm_workers_equivalent/pa_log_salary/pa_log_benefit + plot_annotation(
    title = "Effect on the Employment",
    subtitle = target,
    #caption = caption_main
  ) &
    theme(text = element_text(family="Meiryo UI"))
  
  filename_pdf <- paste("03_robustness_output/", target,"_42employment_", count_intervention, ".pdf", sep="")
  ggsave(filename_pdf,pa_employment_sum, 
         width=10, height =48, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("03_robustness_output/", target,"_42employment_", count_intervention, ".png", sep="")
  ggsave(filename_png,pa_employment_sum, 
         width=10, height =48, units = "cm", dpi=200) 
}
robustness_group_business <- function(target){
  ################## log_sales------------
  #CS
  cs_log_sales <- att_gt(yname = "log_sales",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                         xformla = formula_cs, 
                         est_method = "ipw",base_period="universal",alp=0.05,
                         data = dfm_kikatsu2_ipw,#pl = TRUE,
                         #print_details=FALSE,
                         anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_log_sales_group <- aggte(cs_log_sales, type = "group",na.rm = TRUE)
  p_log_sales_cs_group <- ggdid(cs_log_sales_group)
  
  ## create grid arrange
  pa_log_sales <- p_log_sales_cs_group+ 
    labs(title = "log_sales", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_log_sales)
  ################## log_tax------------
  #CS
  cs_log_tax <- att_gt(yname = "log_tax",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                       xformla = formula_cs, 
                       est_method = "ipw",base_period="universal",alp=0.05,
                       data = dfm_kikatsu2_ipw,#pl = TRUE,
                       #print_details=FALSE,
                       anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_log_tax_group <- aggte(cs_log_tax, type = "group",na.rm = TRUE)
  p_log_tax_cs_group <- ggdid(cs_log_tax_group)
  
  ## create grid arrange
  pa_log_tax <- p_log_tax_cs_group+ 
    labs(title = "log_tax", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_log_tax)
  ################## log_office------------
  #CS
  cs_log_office <- att_gt(yname = "log_office",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                          xformla = formula_cs, 
                          est_method = "ipw",base_period="universal",alp=0.05,
                          data = dfm_kikatsu2_ipw,#pl = TRUE,
                          #print_details=FALSE,
                          anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_log_office_group <- aggte(cs_log_office, type = "group",na.rm = TRUE)
  p_log_office_cs_group <- ggdid(cs_log_office_group)
  
  ## create grid arrange
  pa_log_office <- p_log_office_cs_group+ 
    labs(title = "log_office", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_log_office)
  ################## ROA------------
  #CS
  cs_ROA <- att_gt(yname = "ROA",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                   xformla = formula_cs, 
                   est_method = "ipw",base_period="universal",alp=0.05,
                   data = dfm_kikatsu2_ipw,#pl = TRUE,
                   #print_details=FALSE,
                   anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_ROA_group <- aggte(cs_ROA, type = "group",na.rm = TRUE)
  p_ROA_cs_group <- ggdid(cs_ROA_group)
  
  ## create grid arrange
  pa_ROA <- p_ROA_cs_group+ 
    labs(title = "ROA", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_ROA)
  ################## net_profit_workers------------
  #CS
  cs_net_profit_workers <- att_gt(yname = "net_profit_workers",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                                  xformla = formula_cs, 
                                  est_method = "ipw",base_period="universal",alp=0.05,
                                  data = dfm_kikatsu2_ipw,#pl = TRUE,
                                  #print_details=FALSE,
                                  anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_net_profit_workers_group <- aggte(cs_net_profit_workers, type = "group",na.rm = TRUE)
  p_net_profit_workers_cs_group <- ggdid(cs_net_profit_workers_group)
  
  ## create grid arrange
  pa_net_profit_workers <- p_net_profit_workers_cs_group+ 
    labs(title = "net_profit_workers", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_net_profit_workers)
  ## output_business-----------
  pa_business_sum <- pa_log_sales/pa_log_tax/pa_log_office/pa_ROA/pa_net_profit_workers + plot_annotation(
    title = "Effect on the Business",
    subtitle = target,
    #caption = caption_main
  ) &
    theme(text = element_text(family="Meiryo UI"))
  
  filename_pdf <- paste("03_robustness_output/", target,"_43business_", count_intervention, ".pdf", sep="")
  ggsave(filename_pdf,pa_business_sum, 
         width=10, height =40.5, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("03_robustness_output/", target,"_43business_", count_intervention, ".png", sep="")
  ggsave(filename_png,pa_business_sum, 
         width=10, height =40.5, units = "cm", dpi=200) 
}
robustness_group_trade <- function(target){
  ################## flag_export------------
  #CS
  cs_flag_export <- att_gt(yname = "flag_export",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                           xformla = formula_cs, 
                           est_method = "ipw",base_period="universal",alp=0.05,
                           data = dfm_kikatsu2_ipw,#pl = TRUE,
                           #print_details=FALSE,
                           anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_flag_export_group <- aggte(cs_flag_export, type = "group",na.rm = TRUE)
  p_flag_export_cs_group <- ggdid(cs_flag_export_group)
  
  ## create grid arrange
  pa_flag_export <- p_flag_export_cs_group+ 
    labs(title = "flag_export", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_flag_export)
  ################## flag_import------------
  #CS
  cs_flag_import <- att_gt(yname = "flag_import",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                           xformla = formula_cs, 
                           est_method = "ipw",base_period="universal",alp=0.05,
                           data = dfm_kikatsu2_ipw,#pl = TRUE,
                           #print_details=FALSE,
                           anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_flag_import_group <- aggte(cs_flag_import, type = "group",na.rm = TRUE)
  p_flag_import_cs_group <- ggdid(cs_flag_import_group)
  
  ## create grid arrange
  pa_flag_import <- p_flag_import_cs_group+ 
    labs(title = "flag_import", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_flag_import)
  ################## log_export------------
  #CS
  cs_log_export <- att_gt(yname = "log_export",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                          xformla = formula_cs, 
                          est_method = "ipw",base_period="universal",alp=0.05,
                          data = dfm_kikatsu2_ipw,#pl = TRUE,
                          #print_details=FALSE,
                          anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_log_export_group <- aggte(cs_log_export, type = "group",na.rm = TRUE)
  p_log_export_cs_group <- ggdid(cs_log_export_group)
  
  ## create grid arrange
  pa_log_export <- p_log_export_cs_group+ 
    labs(title = "log_export", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_log_export)
  ################## log_import------------
  #CS
  cs_log_import <- att_gt(yname = "log_import",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                          xformla = formula_cs, 
                          est_method = "ipw",base_period="universal",alp=0.05,
                          data = dfm_kikatsu2_ipw,#pl = TRUE,
                          #print_details=FALSE,
                          anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_log_import_group <- aggte(cs_log_import, type = "group",na.rm = TRUE)
  p_log_import_cs_group <- ggdid(cs_log_import_group)
  
  ## create grid arrange
  pa_log_import <- p_log_import_cs_group+ 
    labs(title = "log_import", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_log_import)
  ## output_trade-----------
  pa_trade_sum <- pa_flag_export/pa_flag_import/pa_log_export/pa_log_import + plot_annotation(
    title = "Effect on the International Trade",
    subtitle = target,
    #caption = caption_main
  ) &
    theme(text = element_text(family="Meiryo UI"))
  
  filename_pdf <- paste("03_robustness_output/", target,"_44trade_", count_intervention, ".pdf", sep="")
  ggsave(filename_pdf,pa_trade_sum, 
         width=10, height =33, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("03_robustness_output/", target,"_44trade_", count_intervention, ".png", sep="")
  ggsave(filename_png,pa_trade_sum, 
         width=10, height =33, units = "cm", dpi=200) 
}
robustness_group_trainingRD <- function(target){
  ################## flag_training------------
  #CS
  cs_flag_training <- att_gt(yname = "flag_training",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                             xformla = formula_cs, 
                             est_method = "ipw",base_period="universal",alp=0.05,
                             data = dfm_kikatsu2_ipw,#pl = TRUE,
                             #print_details=FALSE,
                             anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_flag_training_group <- aggte(cs_flag_training, type = "group",na.rm = TRUE)
  p_flag_training_cs_group <- ggdid(cs_flag_training_group)
  
  ## create grid arrange
  pa_flag_training <- p_flag_training_cs_group+ 
    labs(title = "flag_training", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_flag_training)
  ################## flag_RD------------
  #CS
  cs_flag_RD <- att_gt(yname = "flag_RD",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                       xformla = formula_cs, 
                       est_method = "ipw",base_period="universal",alp=0.05,
                       data = dfm_kikatsu2_ipw,#pl = TRUE,
                       #print_details=FALSE,
                       anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_flag_RD_group <- aggte(cs_flag_RD, type = "group",na.rm = TRUE)
  p_flag_RD_cs_group <- ggdid(cs_flag_RD_group)
  
  ## create grid arrange
  pa_flag_RD <- p_flag_RD_cs_group+ 
    labs(title = "flag_RD", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_flag_RD)
  ################## log_training------------
  #CS
  cs_log_training <- att_gt(yname = "log_training",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                            xformla = formula_cs, 
                            est_method = "ipw",base_period="universal",alp=0.05,
                            data = dfm_kikatsu2_ipw,#pl = TRUE,
                            #print_details=FALSE,
                            anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_log_training_group <- aggte(cs_log_training, type = "group",na.rm = TRUE)
  p_log_training_cs_group <- ggdid(cs_log_training_group)
  
  ## create grid arrange
  pa_log_training <- p_log_training_cs_group+ 
    labs(title = "log_training", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_log_training)
  ################## log_RD------------
  #CS
  cs_log_RD <- att_gt(yname = "log_RD",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                      xformla = formula_cs, 
                      est_method = "ipw",base_period="universal",alp=0.05,
                      data = dfm_kikatsu2_ipw,#pl = TRUE,
                      #print_details=FALSE,
                      anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_log_RD_group <- aggte(cs_log_RD, type = "group",na.rm = TRUE)
  p_log_RD_cs_group <- ggdid(cs_log_RD_group)
  
  ## create grid arrange
  pa_log_RD <- p_log_RD_cs_group+ 
    labs(title = "log_RD", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_log_RD)
  ## output_trainingRD-----------
  pa_trainingRD_sum <- pa_flag_training/pa_flag_RD/pa_log_training/pa_log_RD + plot_annotation(
    title = "Effect on the Training and RD",
    subtitle = target,
    #caption = caption_main
  ) &
    theme(text = element_text(family="Meiryo UI"))
  
  filename_pdf <- paste("03_robustness_output/", target,"_45trainingRD_", count_intervention, ".pdf", sep="")
  ggsave(filename_pdf,pa_trainingRD_sum, 
         width=10, height =33, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("03_robustness_output/", target,"_45trainingRD_", count_intervention, ".png", sep="")
  ggsave(filename_png,pa_trainingRD_sum, 
         width=10, height =33, units = "cm", dpi=200) 
}
robustness_group_IP <- function(target){
  ################## flag_patent------------
  #CS
  cs_flag_patent <- att_gt(yname = "flag_patent",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                           xformla = formula_cs, 
                           est_method = "ipw",base_period="universal",alp=0.05,
                           data = dfm_kikatsu2_ipw,#pl = TRUE,
                           #print_details=FALSE,
                           anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_flag_patent_group <- aggte(cs_flag_patent, type = "group",na.rm = TRUE)
  p_flag_patent_cs_group <- ggdid(cs_flag_patent_group)
  
  ## create grid arrange
  pa_flag_patent <- p_flag_patent_cs_group+ 
    labs(title = "flag_patent", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_flag_patent)
  ################## flag_jitsuyo------------
  #CS
  cs_flag_jitsuyo <- att_gt(yname = "flag_jitsuyo",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                            xformla = formula_cs, 
                            est_method = "ipw",base_period="universal",alp=0.05,
                            data = dfm_kikatsu2_ipw,#pl = TRUE,
                            #print_details=FALSE,
                            anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_flag_jitsuyo_group <- aggte(cs_flag_jitsuyo, type = "group",na.rm = TRUE)
  p_flag_jitsuyo_cs_group <- ggdid(cs_flag_jitsuyo_group)
  
  ## create grid arrange
  pa_flag_jitsuyo <- p_flag_jitsuyo_cs_group+ 
    labs(title = "flag_jitsuyo", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_flag_jitsuyo)
  ################## flag_isho------------
  #CS
  cs_flag_isho <- att_gt(yname = "flag_isho",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                         xformla = formula_cs, 
                         est_method = "ipw",base_period="universal",alp=0.05,
                         data = dfm_kikatsu2_ipw,#pl = TRUE,
                         #print_details=FALSE,
                         anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_flag_isho_group <- aggte(cs_flag_isho, type = "group",na.rm = TRUE)
  p_flag_isho_cs_group <- ggdid(cs_flag_isho_group)
  
  ## create grid arrange
  pa_flag_isho <- p_flag_isho_cs_group+ 
    labs(title = "flag_isho", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_flag_isho)
  ################## log_patent------------
  #CS
  cs_log_patent <- att_gt(yname = "log_patent",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                          xformla = formula_cs, 
                          est_method = "ipw",base_period="universal",alp=0.05,
                          data = dfm_kikatsu2_ipw,#pl = TRUE,
                          #print_details=FALSE,
                          anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_log_patent_group <- aggte(cs_log_patent, type = "group",na.rm = TRUE)
  p_log_patent_cs_group <- ggdid(cs_log_patent_group)
  
  ## create grid arrange
  pa_log_patent <- p_log_patent_cs_group+ 
    labs(title = "log_patent", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_log_patent)
  ################## log_jitsuyo------------
  #CS
  cs_log_jitsuyo <- att_gt(yname = "log_jitsuyo",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                           xformla = formula_cs, 
                           est_method = "ipw",base_period="universal",alp=0.05,
                           data = dfm_kikatsu2_ipw,#pl = TRUE,
                           #print_details=FALSE,
                           anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_log_jitsuyo_group <- aggte(cs_log_jitsuyo, type = "group",na.rm = TRUE)
  p_log_jitsuyo_cs_group <- ggdid(cs_log_jitsuyo_group)
  
  ## create grid arrange
  pa_log_jitsuyo <- p_log_jitsuyo_cs_group+ 
    labs(title = "log_jitsuyo", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_log_jitsuyo)
  ################## log_isho------------
  #CS
  cs_log_isho <- att_gt(yname = "log_isho",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                        xformla = formula_cs, 
                        est_method = "ipw",base_period="universal",alp=0.05,
                        data = dfm_kikatsu2_ipw,#pl = TRUE,
                        #print_details=FALSE,
                        anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_log_isho_group <- aggte(cs_log_isho, type = "group",na.rm = TRUE)
  p_log_isho_cs_group <- ggdid(cs_log_isho_group)
  
  ## create grid arrange
  pa_log_isho <- p_log_isho_cs_group+ 
    labs(title = "log_isho", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_log_isho)
  ## output_IP-----------
  pa_IP_sum <- pa_flag_patent/pa_flag_jitsuyo/pa_flag_isho/pa_log_patent/pa_log_jitsuyo/pa_log_isho + plot_annotation(
    title = "Effect on the Intellectual Property",
    subtitle = target,
    #caption = caption_main
  ) &
    theme(text = element_text(family="Meiryo UI"))
  
  filename_pdf <- paste("03_robustness_output/", target,"_46IP_", count_intervention, ".pdf", sep="")
  ggsave(filename_pdf,pa_IP_sum, 
         width=10, height =48, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("03_robustness_output/", target,"_46IP_", count_intervention, ".png", sep="")
  ggsave(filename_png,pa_IP_sum, 
         width=10, height =48, units = "cm", dpi=200) 
}
robustness_group_investment <- function(target){
  ################## flag_investment_affiliate_domestic------------
  #CS
  cs_flag_investment_affiliate_domestic <- att_gt(yname = "flag_investment_affiliate_domestic",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                                                  xformla = formula_cs, 
                                                  est_method = "ipw",base_period="universal",alp=0.05,
                                                  data = dfm_kikatsu2_ipw,#pl = TRUE,
                                                  #print_details=FALSE,
                                                  anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_flag_investment_affiliate_domestic_group <- aggte(cs_flag_investment_affiliate_domestic, type = "group",na.rm = TRUE)
  p_flag_investment_affiliate_domestic_cs_group <- ggdid(cs_flag_investment_affiliate_domestic_group)
  
  ## create grid arrange
  pa_flag_investment_affiliate_domestic <- p_flag_investment_affiliate_domestic_cs_group+ 
    labs(title = "flag_investment_affiliate_domestic", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_flag_investment_affiliate_domestic)
  ################## flag_investment_affiliate_overseas------------
  #CS
  cs_flag_investment_affiliate_overseas <- att_gt(yname = "flag_investment_affiliate_overseas",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                                                  xformla = formula_cs, 
                                                  est_method = "ipw",base_period="universal",alp=0.05,
                                                  data = dfm_kikatsu2_ipw,#pl = TRUE,
                                                  #print_details=FALSE,
                                                  anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_flag_investment_affiliate_overseas_group <- aggte(cs_flag_investment_affiliate_overseas, type = "group",na.rm = TRUE)
  p_flag_investment_affiliate_overseas_cs_group <- ggdid(cs_flag_investment_affiliate_overseas_group)
  
  ## create grid arrange
  pa_flag_investment_affiliate_overseas <- p_flag_investment_affiliate_overseas_cs_group+ 
    labs(title = "flag_investment_affiliate_overseas", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_flag_investment_affiliate_overseas)
  ################## flag_dividend------------
  #CS
  cs_flag_dividend <- att_gt(yname = "flag_dividend",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                             xformla = formula_cs, 
                             est_method = "ipw",base_period="universal",alp=0.05,
                             data = dfm_kikatsu2_ipw,#pl = TRUE,
                             #print_details=FALSE,
                             anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_flag_dividend_group <- aggte(cs_flag_dividend, type = "group",na.rm = TRUE)
  p_flag_dividend_cs_group <- ggdid(cs_flag_dividend_group)
  
  ## create grid arrange
  pa_flag_dividend <- p_flag_dividend_cs_group+ 
    labs(title = "flag_dividend", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_flag_dividend)
  ################## log_investment_affiliate_domestic------------
  #CS
  cs_log_investment_affiliate_domestic <- att_gt(yname = "log_investment_affiliate_domestic",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                                                 xformla = formula_cs, 
                                                 est_method = "ipw",base_period="universal",alp=0.05,
                                                 data = dfm_kikatsu2_ipw,#pl = TRUE,
                                                 #print_details=FALSE,
                                                 anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_log_investment_affiliate_domestic_group <- aggte(cs_log_investment_affiliate_domestic, type = "group",na.rm = TRUE)
  p_log_investment_affiliate_domestic_cs_group <- ggdid(cs_log_investment_affiliate_domestic_group)
  
  ## create grid arrange
  pa_log_investment_affiliate_domestic <- p_log_investment_affiliate_domestic_cs_group+ 
    labs(title = "log_investment_affiliate_domestic", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_log_investment_affiliate_domestic)
  ################## log_investment_affiliate_overseas------------
  #CS
  cs_log_investment_affiliate_overseas <- att_gt(yname = "log_investment_affiliate_overseas",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                                                 xformla = formula_cs, 
                                                 est_method = "ipw",base_period="universal",alp=0.05,
                                                 data = dfm_kikatsu2_ipw,#pl = TRUE,
                                                 #print_details=FALSE,
                                                 anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_log_investment_affiliate_overseas_group <- aggte(cs_log_investment_affiliate_overseas, type = "group",na.rm = TRUE)
  p_log_investment_affiliate_overseas_cs_group <- ggdid(cs_log_investment_affiliate_overseas_group)
  
  ## create grid arrange
  pa_log_investment_affiliate_overseas <- p_log_investment_affiliate_overseas_cs_group+ 
    labs(title = "log_investment_affiliate_overseas", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_log_investment_affiliate_overseas)
  ################## log_dividend------------
  #CS
  cs_log_dividend <- att_gt(yname = "log_dividend",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                            xformla = formula_cs, 
                            est_method = "ipw",base_period="universal",alp=0.05,
                            data = dfm_kikatsu2_ipw,#pl = TRUE,
                            #print_details=FALSE,
                            anticipation = 1 #　１年前にanticipateすると設定
  )
  
  # by group
  cs_log_dividend_group <- aggte(cs_log_dividend, type = "group",na.rm = TRUE)
  p_log_dividend_cs_group <- ggdid(cs_log_dividend_group)
  
  ## create grid arrange
  pa_log_dividend <- p_log_dividend_cs_group+ 
    labs(title = "log_dividend", subtitle = "C&S - IPW, 95% CI") +theme_light() + theme(legend.position="none")
  print( pa_log_dividend)
  ## output_investment-----------
  pa_investment_sum <- pa_flag_investment_affiliate_domestic/pa_flag_investment_affiliate_overseas/pa_flag_dividend/pa_log_investment_affiliate_domestic/pa_log_investment_affiliate_overseas/pa_log_dividend + plot_annotation(
    title = "Effect on the Investment",
    subtitle = target,
    #caption = caption_main
  ) &
    theme(text = element_text(family="Meiryo UI"))
  
  filename_pdf <- paste("03_robustness_output/", target,"_47investment_", count_intervention, ".pdf", sep="")
  ggsave(filename_pdf,pa_investment_sum, 
         width=10, height =48, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("03_robustness_output/", target,"_47investment_", count_intervention, ".png", sep="")
  ggsave(filename_png,pa_investment_sum, 
         width=10, height =48, units = "cm", dpi=200) 
}



## conduct evaluation------------

print(target)
try(robustness_group_asset(target))
try(robustness_group_employment(target))
try(robustness_group_business(target))
try(robustness_group_trade(target))
try(robustness_group_trainingRD(target))
try(robustness_group_IP(target))
try(robustness_group_investment(target))


# 50  define functions for robustness to matching methods---------------- 
##create data for PSM------------
 # # PSM-nearest-----
 match_PSM_nearest <- matchit(formula_cs0,
                       method = "nearest", distance = "glm", link = "logit", data = dfm_kikatsu_propensity1, caliper = 0.1, ratio =2, 
                       #mahvars = ~  pref_m + industry_code_m + age_lag2+age2_lag2+subsidiary2+parent2+log_sales2+log_sales_diff+ROA2+ROA_diff+net_profit_workers2+net_profit_workers_diff+log_salary2+log_salary_diff+log_sum_asset2+log_sum_asset_diff+  log_workers2+log_workers_diff+log_office2+log_office_diff
                       )
 
 # balancing test 
 bal_att <- love.plot(match_PSM_nearest, 
                      threshold = 0.1, 
                      abs = TRUE, 
                      grid = TRUE, 
                      shapes = c(18, 20), 
                      color = c("tomato", "royalblue"), 
                      stars = "std",
                      title=paste("Covariate Balance", target)) +theme_light()
 plot(bal_att)
 filename <- paste("03_robustness_output/", target,"_50balance_match_PSM_nearest_", count_intervention, ".png", sep="")
 ggsave(filename,bal_att, 
        width=20, height =20, units = "cm", dpi=200) 
 m <- match.data(match_PSM_nearest)
 
 dfm_kikatsu_propensity_PSM_nearest <- match.data(match_PSM_nearest)%>%
   select(id,age_lag2, age2_lag2,industry_code_lag2,subsidiary2,parent2,
          pref_m, industry_code_m,
          log_sales2, log_sales_diff, 
          ROA2, ROA_diff, 
          net_profit_workers2, net_profit_workers_diff, 
          log_salary2, log_salary_diff, 
          log_sum_asset2, log_sum_asset_diff, 
          log_workers2, log_workers_diff,
          log_office2, log_office_diff,
          distance,weights,subclass
   )
 
 dfm_kikatsu2_PSM_nearest <<- left_join(dfm_kikatsu,dfm_kikatsu_propensity_PSM_nearest,by=c("id"))%>%
   drop_na(weights) %>%
   filter(year >= start_year -11) %>%
   mutate(id = as.numeric(id)) ## CSでnumericである必要
 
 ## mahalanobis-nearest------
 match_mahalanobis_nearest <- matchit(formula_cs0,
                                           method = "nearest", distance = "mahalanobis", data = dfm_kikatsu_propensity1,  ratio =2 )
# summary(match_mahalanobis_nearest, interactions = TRUE)
 
 # balancing test 
 bal_att <- love.plot(match_mahalanobis_nearest, 
                      threshold = 0.1, 
                      abs = TRUE, 
                      grid = TRUE, 
                      shapes = c(18, 20), 
                      color = c("tomato", "royalblue"), 
                      stars = "std",
                      title=paste("Covariate Balance", target)) +theme_light()
 plot(bal_att)
 filename <- paste("03_robustness_output/", target,"_50balance_match_mahalanobis_nearest_", count_intervention, ".png", sep="")
 ggsave(filename,bal_att, 
        width=20, height =20, units = "cm", dpi=200) 
 m <- match.data(match_mahalanobis_nearest)
 
 dfm_kikatsu_propensity_mahalanobis_nearest <- match.data(match_mahalanobis_nearest)%>%
   select(id,age_lag2, age2_lag2,industry_code_lag2,subsidiary2,parent2,
          pref_m, industry_code_m,
          log_sales2, log_sales_diff, 
          ROA2, ROA_diff, 
          net_profit_workers2, net_profit_workers_diff, 
          log_salary2, log_salary_diff, 
          log_sum_asset2, log_sum_asset_diff, 
          log_workers2, log_workers_diff,
          log_office2, log_office_diff,
          weights,subclass
   )
 
 dfm_kikatsu2_mahalanobis_nearest <<- left_join(dfm_kikatsu,dfm_kikatsu_propensity_mahalanobis_nearest,by=c("id"))%>%
   drop_na(weights) %>%
   filter(year >= start_year -11) %>%
   mutate(id = as.numeric(id)) ## CSでnumericである必要

 ## mahalanobis-full------
 match_mahalanobis_full <- matchit(formula_cs0,
                                        method = "full", distance = "mahalanobis", data = dfm_kikatsu_propensity1)

 # balancing test 
 bal_att <- love.plot(match_mahalanobis_full, 
                      threshold = 0.1, 
                      abs = TRUE, 
                      grid = TRUE, 
                      shapes = c(18, 20), 
                      color = c("tomato", "royalblue"), 
                      stars = "std",
                      title=paste("Covariate Balance", target)) +theme_light()
 plot(bal_att)
 filename <- paste("03_robustness_output/", target,"_50balance_match_mahalanobis_full_", count_intervention, ".png", sep="")
 ggsave(filename,bal_att, 
        width=20, height =20, units = "cm", dpi=200) 
 m <- match.data(match_mahalanobis_full)
 
 dfm_kikatsu_propensity_mahalanobis_full <- match.data(match_mahalanobis_full)%>%
   select(id,age_lag2, age2_lag2,industry_code_lag2,subsidiary2,parent2,
          pref_m, industry_code_m,
          log_sales2, log_sales_diff, 
          ROA2, ROA_diff, 
          net_profit_workers2, net_profit_workers_diff, 
          log_salary2, log_salary_diff, 
          log_sum_asset2, log_sum_asset_diff, 
          log_workers2, log_workers_diff,
          log_office2, log_office_diff,
          weights,subclass
   )
 
 dfm_kikatsu2_mahalanobis_full <<- left_join(dfm_kikatsu,dfm_kikatsu_propensity_mahalanobis_full,by=c("id"))%>%
   drop_na(weights) %>%
   filter(year >= start_year -11) %>%
   mutate(id = as.numeric(id)) 
 
 ## IPW---- 
 ipw_data <- weightit(formula=formula_cs0,data = dfm_kikatsu_propensity1, method = "glm", estimand = "ATT")
 bal_att <- love.plot(ipw_data, abs = TRUE) +theme_light()
 plot(bal_att)
 filename <- paste("03_robustness_output/", target,"_50balance_ipw_", count_intervention, ".png", sep="")
 ggsave(filename,bal_att, 
        width=20, height =20, units = "cm", dpi=200) 
 
 dfm_kikatsu_propensity_ipw <- dfm_kikatsu_propensity1%>%
   select(id)%>%
   mutate(weights = ipw_data$weights)
 
 dfm_kikatsu2_ipw <<- left_join(dfm_kikatsu,dfm_kikatsu_propensity_ipw,by=c("id"))%>%
   drop_na(weights) %>%
   filter(year >= start_year -11) %>%
   mutate(id = as.numeric(id)) 
 



# define functions for robustness check - PSM -------------
 robustness_PSM_asset <- function(target){
   ################## log_sum_asset------------
   # PSM-nearest
   es_PSM_log_sum_asset_sa20_PSM_nearest = feols(log_sum_asset ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_log_sum_asset_sa20_PSM_nearest <- aggregate(es_PSM_log_sum_asset_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_log_sum_asset_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_log_sum_asset_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_log_sum_asset_sa20_mahalanobis_nearest = feols(log_sum_asset ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_log_sum_asset_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_log_sum_asset_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_sum_asset_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_log_sum_asset_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_log_sum_asset_sa20_mahalanobis_full = feols(log_sum_asset ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_log_sum_asset_sa20_mahalanobis_full <- aggregate(es_mahalanobis_log_sum_asset_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_sum_asset_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_log_sum_asset_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_log_sum_asset_sa20_ipw = feols(log_sum_asset ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_log_sum_asset_sa20_ipw <- aggregate(es_mahalanobis_log_sum_asset_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_sum_asset_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_log_sum_asset_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_log_sum_asset <- p_PSM_log_sum_asset_sa20_PSM_nearest + p_mahalanobis_log_sum_asset_sa20_mahalanobis_nearest + p_mahalanobis_log_sum_asset_sa20_mahalanobis_full  + p_mahalanobis_log_sum_asset_sa20_ipw +
     plot_layout(ncol = 4)
   
   ################## log_tangible_asset------------
   # PSM-nearest
   es_PSM_log_tangible_asset_sa20_PSM_nearest = feols(log_tangible_asset ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_log_tangible_asset_sa20_PSM_nearest <- aggregate(es_PSM_log_tangible_asset_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_log_tangible_asset_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_log_tangible_asset_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_log_tangible_asset_sa20_mahalanobis_nearest = feols(log_tangible_asset ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_log_tangible_asset_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_log_tangible_asset_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_tangible_asset_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_log_tangible_asset_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_log_tangible_asset_sa20_mahalanobis_full = feols(log_tangible_asset ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_log_tangible_asset_sa20_mahalanobis_full <- aggregate(es_mahalanobis_log_tangible_asset_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_tangible_asset_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_log_tangible_asset_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_log_tangible_asset_sa20_ipw = feols(log_tangible_asset ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_log_tangible_asset_sa20_ipw <- aggregate(es_mahalanobis_log_tangible_asset_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_tangible_asset_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_log_tangible_asset_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_log_tangible_asset <- p_PSM_log_tangible_asset_sa20_PSM_nearest + p_mahalanobis_log_tangible_asset_sa20_mahalanobis_nearest + p_mahalanobis_log_tangible_asset_sa20_mahalanobis_full  + p_mahalanobis_log_tangible_asset_sa20_ipw +
     plot_layout(ncol = 4)
   
   ################## log_intangible_asset------------
   # PSM-nearest
   es_PSM_log_intangible_asset_sa20_PSM_nearest = feols(log_intangible_asset ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_log_intangible_asset_sa20_PSM_nearest <- aggregate(es_PSM_log_intangible_asset_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_log_intangible_asset_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_log_intangible_asset_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_log_intangible_asset_sa20_mahalanobis_nearest = feols(log_intangible_asset ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_log_intangible_asset_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_log_intangible_asset_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_intangible_asset_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_log_intangible_asset_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_log_intangible_asset_sa20_mahalanobis_full = feols(log_intangible_asset ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_log_intangible_asset_sa20_mahalanobis_full <- aggregate(es_mahalanobis_log_intangible_asset_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_intangible_asset_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_log_intangible_asset_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_log_intangible_asset_sa20_ipw = feols(log_intangible_asset ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_log_intangible_asset_sa20_ipw <- aggregate(es_mahalanobis_log_intangible_asset_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_intangible_asset_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_log_intangible_asset_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_log_intangible_asset <- p_PSM_log_intangible_asset_sa20_PSM_nearest + p_mahalanobis_log_intangible_asset_sa20_mahalanobis_nearest + p_mahalanobis_log_intangible_asset_sa20_mahalanobis_full  + p_mahalanobis_log_intangible_asset_sa20_ipw +
     plot_layout(ncol = 4)
   
   ## output_asset-----------
   pa_asset_sum <- pa_log_sum_asset / pa_log_tangible_asset / pa_log_intangible_asset + plot_annotation(
     title = "Effect on the Asset",
     subtitle = target,
     #caption = caption_main
   ) &
     theme(text = element_text(family="Meiryo UI"))
   
   filename_pdf <- paste("03_robustness_output/", target,"_51asset_", count_intervention, ".pdf", sep="")
   ggsave(filename_pdf,pa_asset_sum, 
          width=35, height =25.5, units = "cm", dpi=200, device = cairo_pdf) 
   filename_png <- paste("03_robustness_output/", target,"_51asset_", count_intervention, ".png", sep="")
   ggsave(filename_png,pa_asset_sum, 
          width=35, height =25.5, units = "cm", dpi=200) 
 }
 robustness_PSM_employment <- function(target){
   ################## log_workers------------
   # PSM-nearest
   es_PSM_log_workers_sa20_PSM_nearest = feols(log_workers ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_log_workers_sa20_PSM_nearest <- aggregate(es_PSM_log_workers_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_log_workers_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_log_workers_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_log_workers_sa20_mahalanobis_nearest = feols(log_workers ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_log_workers_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_log_workers_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_workers_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_log_workers_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_log_workers_sa20_mahalanobis_full = feols(log_workers ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_log_workers_sa20_mahalanobis_full <- aggregate(es_mahalanobis_log_workers_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_workers_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_log_workers_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_log_workers_sa20_ipw = feols(log_workers ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_log_workers_sa20_ipw <- aggregate(es_mahalanobis_log_workers_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_workers_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_log_workers_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_log_workers <- p_PSM_log_workers_sa20_PSM_nearest + p_mahalanobis_log_workers_sa20_mahalanobis_nearest + p_mahalanobis_log_workers_sa20_mahalanobis_full  + p_mahalanobis_log_workers_sa20_ipw +
     plot_layout(ncol = 4)
   
   ################## log_indefinite_workers------------
   # PSM-nearest
   es_PSM_log_indefinite_workers_sa20_PSM_nearest = feols(log_indefinite_workers ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_log_indefinite_workers_sa20_PSM_nearest <- aggregate(es_PSM_log_indefinite_workers_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_log_indefinite_workers_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_log_indefinite_workers_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_log_indefinite_workers_sa20_mahalanobis_nearest = feols(log_indefinite_workers ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_log_indefinite_workers_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_log_indefinite_workers_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_indefinite_workers_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_log_indefinite_workers_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_log_indefinite_workers_sa20_mahalanobis_full = feols(log_indefinite_workers ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_log_indefinite_workers_sa20_mahalanobis_full <- aggregate(es_mahalanobis_log_indefinite_workers_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_indefinite_workers_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_log_indefinite_workers_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_log_indefinite_workers_sa20_ipw = feols(log_indefinite_workers ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_log_indefinite_workers_sa20_ipw <- aggregate(es_mahalanobis_log_indefinite_workers_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_indefinite_workers_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_log_indefinite_workers_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_log_indefinite_workers <- p_PSM_log_indefinite_workers_sa20_PSM_nearest + p_mahalanobis_log_indefinite_workers_sa20_mahalanobis_nearest + p_mahalanobis_log_indefinite_workers_sa20_mahalanobis_full  + p_mahalanobis_log_indefinite_workers_sa20_ipw +
     plot_layout(ncol = 4)
   
   ################## log_fixedterm_workers------------
   # PSM-nearest
   es_PSM_log_fixedterm_workers_sa20_PSM_nearest = feols(log_fixedterm_workers ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_log_fixedterm_workers_sa20_PSM_nearest <- aggregate(es_PSM_log_fixedterm_workers_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_log_fixedterm_workers_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_log_fixedterm_workers_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_log_fixedterm_workers_sa20_mahalanobis_nearest = feols(log_fixedterm_workers ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_log_fixedterm_workers_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_log_fixedterm_workers_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_fixedterm_workers_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_log_fixedterm_workers_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_log_fixedterm_workers_sa20_mahalanobis_full = feols(log_fixedterm_workers ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_log_fixedterm_workers_sa20_mahalanobis_full <- aggregate(es_mahalanobis_log_fixedterm_workers_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_fixedterm_workers_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_log_fixedterm_workers_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_log_fixedterm_workers_sa20_ipw = feols(log_fixedterm_workers ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_log_fixedterm_workers_sa20_ipw <- aggregate(es_mahalanobis_log_fixedterm_workers_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_fixedterm_workers_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_log_fixedterm_workers_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_log_fixedterm_workers <- p_PSM_log_fixedterm_workers_sa20_PSM_nearest + p_mahalanobis_log_fixedterm_workers_sa20_mahalanobis_nearest + p_mahalanobis_log_fixedterm_workers_sa20_mahalanobis_full  + p_mahalanobis_log_fixedterm_workers_sa20_ipw +
     plot_layout(ncol = 4)
   
   ################## log_fixedterm_workers_equivalent------------
   # PSM-nearest
   es_PSM_log_fixedterm_workers_equivalent_sa20_PSM_nearest = feols(log_fixedterm_workers_equivalent ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_log_fixedterm_workers_equivalent_sa20_PSM_nearest <- aggregate(es_PSM_log_fixedterm_workers_equivalent_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_log_fixedterm_workers_equivalent_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_log_fixedterm_workers_equivalent_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_log_fixedterm_workers_equivalent_sa20_mahalanobis_nearest = feols(log_fixedterm_workers_equivalent ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_log_fixedterm_workers_equivalent_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_log_fixedterm_workers_equivalent_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_fixedterm_workers_equivalent_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_log_fixedterm_workers_equivalent_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_log_fixedterm_workers_equivalent_sa20_mahalanobis_full = feols(log_fixedterm_workers_equivalent ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_log_fixedterm_workers_equivalent_sa20_mahalanobis_full <- aggregate(es_mahalanobis_log_fixedterm_workers_equivalent_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_fixedterm_workers_equivalent_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_log_fixedterm_workers_equivalent_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_log_fixedterm_workers_equivalent_sa20_ipw = feols(log_fixedterm_workers_equivalent ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_log_fixedterm_workers_equivalent_sa20_ipw <- aggregate(es_mahalanobis_log_fixedterm_workers_equivalent_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_fixedterm_workers_equivalent_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_log_fixedterm_workers_equivalent_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_log_fixedterm_workers_equivalent <- p_PSM_log_fixedterm_workers_equivalent_sa20_PSM_nearest + p_mahalanobis_log_fixedterm_workers_equivalent_sa20_mahalanobis_nearest + p_mahalanobis_log_fixedterm_workers_equivalent_sa20_mahalanobis_full  + p_mahalanobis_log_fixedterm_workers_equivalent_sa20_ipw +
     plot_layout(ncol = 4)
   
   ################## log_salary------------
   # PSM-nearest
   es_PSM_log_salary_sa20_PSM_nearest = feols(log_salary ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_log_salary_sa20_PSM_nearest <- aggregate(es_PSM_log_salary_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_log_salary_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_log_salary_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_log_salary_sa20_mahalanobis_nearest = feols(log_salary ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_log_salary_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_log_salary_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_salary_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_log_salary_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_log_salary_sa20_mahalanobis_full = feols(log_salary ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_log_salary_sa20_mahalanobis_full <- aggregate(es_mahalanobis_log_salary_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_salary_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_log_salary_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_log_salary_sa20_ipw = feols(log_salary ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_log_salary_sa20_ipw <- aggregate(es_mahalanobis_log_salary_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_salary_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_log_salary_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_log_salary <- p_PSM_log_salary_sa20_PSM_nearest + p_mahalanobis_log_salary_sa20_mahalanobis_nearest + p_mahalanobis_log_salary_sa20_mahalanobis_full  + p_mahalanobis_log_salary_sa20_ipw +
     plot_layout(ncol = 4)
   
   ################## log_benefit------------
   # PSM-nearest
   es_PSM_log_benefit_sa20_PSM_nearest = feols(log_benefit ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_log_benefit_sa20_PSM_nearest <- aggregate(es_PSM_log_benefit_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_log_benefit_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_log_benefit_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_log_benefit_sa20_mahalanobis_nearest = feols(log_benefit ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_log_benefit_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_log_benefit_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_benefit_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_log_benefit_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_log_benefit_sa20_mahalanobis_full = feols(log_benefit ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_log_benefit_sa20_mahalanobis_full <- aggregate(es_mahalanobis_log_benefit_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_benefit_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_log_benefit_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_log_benefit_sa20_ipw = feols(log_benefit ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_log_benefit_sa20_ipw <- aggregate(es_mahalanobis_log_benefit_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_benefit_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_log_benefit_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_log_benefit <- p_PSM_log_benefit_sa20_PSM_nearest + p_mahalanobis_log_benefit_sa20_mahalanobis_nearest + p_mahalanobis_log_benefit_sa20_mahalanobis_full  + p_mahalanobis_log_benefit_sa20_ipw +
     plot_layout(ncol = 4)
   
   ## output_employment-----------
   pa_employment_sum <- pa_log_workers/pa_log_indefinite_workers/pa_log_fixedterm_workers/pa_log_fixedterm_workers_equivalent/pa_log_salary/pa_log_benefit + plot_annotation(
     title = "Effect on the Employment",
     subtitle = target,
     #caption = caption_main
   ) &
     theme(text = element_text(family="Meiryo UI"))
   
   filename_pdf <- paste("03_robustness_output/", target,"_52employment_", count_intervention, ".pdf", sep="")
   ggsave(filename_pdf,pa_employment_sum, 
          width=35, height =48, units = "cm", dpi=200, device = cairo_pdf) 
   filename_png <- paste("03_robustness_output/", target,"_52employment_", count_intervention, ".png", sep="")
   ggsave(filename_png,pa_employment_sum, 
          width=35, height =48, units = "cm", dpi=200) 
 }
 robustness_PSM_business <- function(target){
   ################## log_sales------------
   # PSM-nearest
   es_PSM_log_sales_sa20_PSM_nearest = feols(log_sales ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_log_sales_sa20_PSM_nearest <- aggregate(es_PSM_log_sales_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_log_sales_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_log_sales_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_log_sales_sa20_mahalanobis_nearest = feols(log_sales ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_log_sales_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_log_sales_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_sales_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_log_sales_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_log_sales_sa20_mahalanobis_full = feols(log_sales ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_log_sales_sa20_mahalanobis_full <- aggregate(es_mahalanobis_log_sales_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_sales_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_log_sales_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_log_sales_sa20_ipw = feols(log_sales ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_log_sales_sa20_ipw <- aggregate(es_mahalanobis_log_sales_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_sales_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_log_sales_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_log_sales <- p_PSM_log_sales_sa20_PSM_nearest + p_mahalanobis_log_sales_sa20_mahalanobis_nearest + p_mahalanobis_log_sales_sa20_mahalanobis_full  + p_mahalanobis_log_sales_sa20_ipw +
     plot_layout(ncol = 4)
   
   ################## log_tax------------
   # PSM-nearest
   es_PSM_log_tax_sa20_PSM_nearest = feols(log_tax ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_log_tax_sa20_PSM_nearest <- aggregate(es_PSM_log_tax_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_log_tax_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_log_tax_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_log_tax_sa20_mahalanobis_nearest = feols(log_tax ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_log_tax_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_log_tax_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_tax_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_log_tax_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_log_tax_sa20_mahalanobis_full = feols(log_tax ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_log_tax_sa20_mahalanobis_full <- aggregate(es_mahalanobis_log_tax_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_tax_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_log_tax_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_log_tax_sa20_ipw = feols(log_tax ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_log_tax_sa20_ipw <- aggregate(es_mahalanobis_log_tax_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_tax_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_log_tax_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_log_tax <- p_PSM_log_tax_sa20_PSM_nearest + p_mahalanobis_log_tax_sa20_mahalanobis_nearest + p_mahalanobis_log_tax_sa20_mahalanobis_full  + p_mahalanobis_log_tax_sa20_ipw +
     plot_layout(ncol = 4)
   
   ################## log_office------------
   # PSM-nearest
   es_PSM_log_office_sa20_PSM_nearest = feols(log_office ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_log_office_sa20_PSM_nearest <- aggregate(es_PSM_log_office_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_log_office_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_log_office_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_log_office_sa20_mahalanobis_nearest = feols(log_office ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_log_office_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_log_office_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_office_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_log_office_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_log_office_sa20_mahalanobis_full = feols(log_office ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_log_office_sa20_mahalanobis_full <- aggregate(es_mahalanobis_log_office_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_office_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_log_office_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_log_office_sa20_ipw = feols(log_office ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_log_office_sa20_ipw <- aggregate(es_mahalanobis_log_office_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_office_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_log_office_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_log_office <- p_PSM_log_office_sa20_PSM_nearest + p_mahalanobis_log_office_sa20_mahalanobis_nearest + p_mahalanobis_log_office_sa20_mahalanobis_full  + p_mahalanobis_log_office_sa20_ipw +
     plot_layout(ncol = 4)
   
   ################## ROA------------
   # PSM-nearest
   es_PSM_ROA_sa20_PSM_nearest = feols(ROA ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_ROA_sa20_PSM_nearest <- aggregate(es_PSM_ROA_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_ROA_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_ROA_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14),ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_ROA_sa20_mahalanobis_nearest = feols(ROA ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_ROA_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_ROA_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_ROA_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_ROA_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14),ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_ROA_sa20_mahalanobis_full = feols(ROA ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_ROA_sa20_mahalanobis_full <- aggregate(es_mahalanobis_ROA_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_ROA_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_ROA_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14),ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_ROA_sa20_ipw = feols(ROA ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_ROA_sa20_ipw <- aggregate(es_mahalanobis_ROA_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_ROA_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_ROA_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14),ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_ROA <- p_PSM_ROA_sa20_PSM_nearest + p_mahalanobis_ROA_sa20_mahalanobis_nearest + p_mahalanobis_ROA_sa20_mahalanobis_full  + p_mahalanobis_ROA_sa20_ipw +
     plot_layout(ncol = 4)
   
   ################## net_profit_workers------------
   # PSM-nearest
   es_PSM_net_profit_workers_sa20_PSM_nearest = feols(net_profit_workers ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_net_profit_workers_sa20_PSM_nearest <- aggregate(es_PSM_net_profit_workers_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_net_profit_workers_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_net_profit_workers_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14),ylim = c(-5000000, 5000000)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_net_profit_workers_sa20_mahalanobis_nearest = feols(net_profit_workers ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_net_profit_workers_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_net_profit_workers_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_net_profit_workers_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_net_profit_workers_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14),ylim = c(-5000000, 5000000)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_net_profit_workers_sa20_mahalanobis_full = feols(net_profit_workers ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_net_profit_workers_sa20_mahalanobis_full <- aggregate(es_mahalanobis_net_profit_workers_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_net_profit_workers_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_net_profit_workers_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14),ylim = c(-5000000, 5000000)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_net_profit_workers_sa20_ipw = feols(net_profit_workers ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_net_profit_workers_sa20_ipw <- aggregate(es_mahalanobis_net_profit_workers_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_net_profit_workers_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_net_profit_workers_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14),ylim = c(-5000000, 5000000)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_net_profit_workers <- p_PSM_net_profit_workers_sa20_PSM_nearest + p_mahalanobis_net_profit_workers_sa20_mahalanobis_nearest + p_mahalanobis_net_profit_workers_sa20_mahalanobis_full  + p_mahalanobis_net_profit_workers_sa20_ipw +
     plot_layout(ncol = 4)
   
   ## output_business-----------
   pa_business_sum <- pa_log_sales/pa_log_tax/pa_log_office/pa_ROA/pa_net_profit_workers + plot_annotation(
     title = "Effect on the Business",
     subtitle = target,
     #caption = caption_main
   ) &
     theme(text = element_text(family="Meiryo UI"))
   
   filename_pdf <- paste("03_robustness_output/", target,"_53business_", count_intervention, ".pdf", sep="")
   ggsave(filename_pdf,pa_business_sum, 
          width=35, height =40.5, units = "cm", dpi=200, device = cairo_pdf) 
   filename_png <- paste("03_robustness_output/", target,"_53business_", count_intervention, ".png", sep="")
   ggsave(filename_png,pa_business_sum, 
          width=35, height =40.5, units = "cm", dpi=200) 
 }
 robustness_PSM_trade <- function(target){
   ################## flag_export------------
   # PSM-nearest
   es_PSM_flag_export_sa20_PSM_nearest = feols(flag_export ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_flag_export_sa20_PSM_nearest <- aggregate(es_PSM_flag_export_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_flag_export_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_flag_export_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_flag_export_sa20_mahalanobis_nearest = feols(flag_export ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_flag_export_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_flag_export_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_flag_export_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_flag_export_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_flag_export_sa20_mahalanobis_full = feols(flag_export ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_flag_export_sa20_mahalanobis_full <- aggregate(es_mahalanobis_flag_export_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_flag_export_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_flag_export_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_flag_export_sa20_ipw = feols(flag_export ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_flag_export_sa20_ipw <- aggregate(es_mahalanobis_flag_export_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_flag_export_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_flag_export_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_flag_export <- p_PSM_flag_export_sa20_PSM_nearest + p_mahalanobis_flag_export_sa20_mahalanobis_nearest + p_mahalanobis_flag_export_sa20_mahalanobis_full  + p_mahalanobis_flag_export_sa20_ipw +
     plot_layout(ncol = 4)
   
   ################## flag_import------------
   # PSM-nearest
   es_PSM_flag_import_sa20_PSM_nearest = feols(flag_import ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_flag_import_sa20_PSM_nearest <- aggregate(es_PSM_flag_import_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_flag_import_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_flag_import_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_flag_import_sa20_mahalanobis_nearest = feols(flag_import ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_flag_import_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_flag_import_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_flag_import_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_flag_import_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_flag_import_sa20_mahalanobis_full = feols(flag_import ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_flag_import_sa20_mahalanobis_full <- aggregate(es_mahalanobis_flag_import_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_flag_import_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_flag_import_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_flag_import_sa20_ipw = feols(flag_import ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_flag_import_sa20_ipw <- aggregate(es_mahalanobis_flag_import_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_flag_import_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_flag_import_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_flag_import <- p_PSM_flag_import_sa20_PSM_nearest + p_mahalanobis_flag_import_sa20_mahalanobis_nearest + p_mahalanobis_flag_import_sa20_mahalanobis_full  + p_mahalanobis_flag_import_sa20_ipw +
     plot_layout(ncol = 4)
   
   ################## log_export------------
   # PSM-nearest
   es_PSM_log_export_sa20_PSM_nearest = feols(log_export ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_log_export_sa20_PSM_nearest <- aggregate(es_PSM_log_export_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_log_export_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_log_export_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_log_export_sa20_mahalanobis_nearest = feols(log_export ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_log_export_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_log_export_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_export_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_log_export_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_log_export_sa20_mahalanobis_full = feols(log_export ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_log_export_sa20_mahalanobis_full <- aggregate(es_mahalanobis_log_export_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_export_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_log_export_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_log_export_sa20_ipw = feols(log_export ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_log_export_sa20_ipw <- aggregate(es_mahalanobis_log_export_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_export_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_log_export_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_log_export <- p_PSM_log_export_sa20_PSM_nearest + p_mahalanobis_log_export_sa20_mahalanobis_nearest + p_mahalanobis_log_export_sa20_mahalanobis_full  + p_mahalanobis_log_export_sa20_ipw +
     plot_layout(ncol = 4)
   
   ################## log_import------------
   # PSM-nearest
   es_PSM_log_import_sa20_PSM_nearest = feols(log_import ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_log_import_sa20_PSM_nearest <- aggregate(es_PSM_log_import_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_log_import_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_log_import_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_log_import_sa20_mahalanobis_nearest = feols(log_import ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_log_import_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_log_import_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_import_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_log_import_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_log_import_sa20_mahalanobis_full = feols(log_import ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_log_import_sa20_mahalanobis_full <- aggregate(es_mahalanobis_log_import_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_import_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_log_import_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_log_import_sa20_ipw = feols(log_import ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_log_import_sa20_ipw <- aggregate(es_mahalanobis_log_import_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_import_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_log_import_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_log_import <- p_PSM_log_import_sa20_PSM_nearest + p_mahalanobis_log_import_sa20_mahalanobis_nearest + p_mahalanobis_log_import_sa20_mahalanobis_full  + p_mahalanobis_log_import_sa20_ipw +
     plot_layout(ncol = 4)
   
   ## output_trade-----------
   pa_trade_sum <- pa_flag_export/pa_flag_import/pa_log_export/pa_log_import + plot_annotation(
     title = "Effect on the International Trade",
     subtitle = target,
     #caption = caption_main
   ) &
     theme(text = element_text(family="Meiryo UI"))
   
   filename_pdf <- paste("03_robustness_output/", target,"_54trade_", count_intervention, ".pdf", sep="")
   ggsave(filename_pdf,pa_trade_sum, 
          width=35, height =33, units = "cm", dpi=200, device = cairo_pdf) 
   filename_png <- paste("03_robustness_output/", target,"_54trade_", count_intervention, ".png", sep="")
   ggsave(filename_png,pa_trade_sum, 
          width=35, height =33, units = "cm", dpi=200) 
 }
 robustness_PSM_trainingRD <- function(target){
   ################## flag_training------------
   # PSM-nearest
   es_PSM_flag_training_sa20_PSM_nearest = feols(flag_training ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_flag_training_sa20_PSM_nearest <- aggregate(es_PSM_flag_training_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_flag_training_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_flag_training_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_flag_training_sa20_mahalanobis_nearest = feols(flag_training ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_flag_training_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_flag_training_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_flag_training_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_flag_training_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_flag_training_sa20_mahalanobis_full = feols(flag_training ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_flag_training_sa20_mahalanobis_full <- aggregate(es_mahalanobis_flag_training_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_flag_training_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_flag_training_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_flag_training_sa20_ipw = feols(flag_training ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_flag_training_sa20_ipw <- aggregate(es_mahalanobis_flag_training_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_flag_training_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_flag_training_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_flag_training <- p_PSM_flag_training_sa20_PSM_nearest + p_mahalanobis_flag_training_sa20_mahalanobis_nearest + p_mahalanobis_flag_training_sa20_mahalanobis_full  + p_mahalanobis_flag_training_sa20_ipw +
     plot_layout(ncol = 4)
   
   ################## flag_RD------------
   # PSM-nearest
   es_PSM_flag_RD_sa20_PSM_nearest = feols(flag_RD ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_flag_RD_sa20_PSM_nearest <- aggregate(es_PSM_flag_RD_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_flag_RD_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_flag_RD_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_flag_RD_sa20_mahalanobis_nearest = feols(flag_RD ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_flag_RD_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_flag_RD_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_flag_RD_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_flag_RD_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_flag_RD_sa20_mahalanobis_full = feols(flag_RD ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_flag_RD_sa20_mahalanobis_full <- aggregate(es_mahalanobis_flag_RD_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_flag_RD_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_flag_RD_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_flag_RD_sa20_ipw = feols(flag_RD ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_flag_RD_sa20_ipw <- aggregate(es_mahalanobis_flag_RD_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_flag_RD_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_flag_RD_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_flag_RD <- p_PSM_flag_RD_sa20_PSM_nearest + p_mahalanobis_flag_RD_sa20_mahalanobis_nearest + p_mahalanobis_flag_RD_sa20_mahalanobis_full  + p_mahalanobis_flag_RD_sa20_ipw +
     plot_layout(ncol = 4)
   
   ################## log_training------------
   # PSM-nearest
   es_PSM_log_training_sa20_PSM_nearest = feols(log_training ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_log_training_sa20_PSM_nearest <- aggregate(es_PSM_log_training_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_log_training_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_log_training_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_log_training_sa20_mahalanobis_nearest = feols(log_training ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_log_training_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_log_training_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_training_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_log_training_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_log_training_sa20_mahalanobis_full = feols(log_training ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_log_training_sa20_mahalanobis_full <- aggregate(es_mahalanobis_log_training_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_training_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_log_training_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_log_training_sa20_ipw = feols(log_training ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_log_training_sa20_ipw <- aggregate(es_mahalanobis_log_training_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_training_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_log_training_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_log_training <- p_PSM_log_training_sa20_PSM_nearest + p_mahalanobis_log_training_sa20_mahalanobis_nearest + p_mahalanobis_log_training_sa20_mahalanobis_full  + p_mahalanobis_log_training_sa20_ipw +
     plot_layout(ncol = 4)
   
   ################## log_RD------------
   # PSM-nearest
   es_PSM_log_RD_sa20_PSM_nearest = feols(log_RD ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_log_RD_sa20_PSM_nearest <- aggregate(es_PSM_log_RD_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_log_RD_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_log_RD_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_log_RD_sa20_mahalanobis_nearest = feols(log_RD ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_log_RD_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_log_RD_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_RD_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_log_RD_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_log_RD_sa20_mahalanobis_full = feols(log_RD ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_log_RD_sa20_mahalanobis_full <- aggregate(es_mahalanobis_log_RD_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_RD_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_log_RD_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_log_RD_sa20_ipw = feols(log_RD ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_log_RD_sa20_ipw <- aggregate(es_mahalanobis_log_RD_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_RD_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_log_RD_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_log_RD <- p_PSM_log_RD_sa20_PSM_nearest + p_mahalanobis_log_RD_sa20_mahalanobis_nearest + p_mahalanobis_log_RD_sa20_mahalanobis_full  + p_mahalanobis_log_RD_sa20_ipw +
     plot_layout(ncol = 4)
   
   ## output_trainingRD-----------
   pa_trainingRD_sum <- pa_flag_training/pa_flag_RD/pa_log_training/pa_log_RD + plot_annotation(
     title = "Effect on the Training and RD",
     subtitle = target,
     #caption = caption_main
   ) &
     theme(text = element_text(family="Meiryo UI"))
   
   filename_pdf <- paste("03_robustness_output/", target,"_55trainingRD_", count_intervention, ".pdf", sep="")
   ggsave(filename_pdf,pa_trainingRD_sum, 
          width=35, height =33, units = "cm", dpi=200, device = cairo_pdf) 
   filename_png <- paste("03_robustness_output/", target,"_55trainingRD_", count_intervention, ".png", sep="")
   ggsave(filename_png,pa_trainingRD_sum, 
          width=35, height =33, units = "cm", dpi=200) 
 }
 robustness_PSM_IP <- function(target){
   ################## flag_patent------------
   # PSM-nearest
   es_PSM_flag_patent_sa20_PSM_nearest = feols(flag_patent ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_flag_patent_sa20_PSM_nearest <- aggregate(es_PSM_flag_patent_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_flag_patent_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_flag_patent_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_flag_patent_sa20_mahalanobis_nearest = feols(flag_patent ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_flag_patent_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_flag_patent_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_flag_patent_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_flag_patent_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_flag_patent_sa20_mahalanobis_full = feols(flag_patent ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_flag_patent_sa20_mahalanobis_full <- aggregate(es_mahalanobis_flag_patent_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_flag_patent_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_flag_patent_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_flag_patent_sa20_ipw = feols(flag_patent ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_flag_patent_sa20_ipw <- aggregate(es_mahalanobis_flag_patent_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_flag_patent_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_flag_patent_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_flag_patent <- p_PSM_flag_patent_sa20_PSM_nearest + p_mahalanobis_flag_patent_sa20_mahalanobis_nearest + p_mahalanobis_flag_patent_sa20_mahalanobis_full  + p_mahalanobis_flag_patent_sa20_ipw +
     plot_layout(ncol = 4)
   
   ################## flag_jitsuyo------------
   # PSM-nearest
   es_PSM_flag_jitsuyo_sa20_PSM_nearest = feols(flag_jitsuyo ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_flag_jitsuyo_sa20_PSM_nearest <- aggregate(es_PSM_flag_jitsuyo_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_flag_jitsuyo_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_flag_jitsuyo_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_flag_jitsuyo_sa20_mahalanobis_nearest = feols(flag_jitsuyo ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_flag_jitsuyo_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_flag_jitsuyo_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_flag_jitsuyo_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_flag_jitsuyo_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_flag_jitsuyo_sa20_mahalanobis_full = feols(flag_jitsuyo ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_flag_jitsuyo_sa20_mahalanobis_full <- aggregate(es_mahalanobis_flag_jitsuyo_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_flag_jitsuyo_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_flag_jitsuyo_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_flag_jitsuyo_sa20_ipw = feols(flag_jitsuyo ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_flag_jitsuyo_sa20_ipw <- aggregate(es_mahalanobis_flag_jitsuyo_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_flag_jitsuyo_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_flag_jitsuyo_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_flag_jitsuyo <- p_PSM_flag_jitsuyo_sa20_PSM_nearest + p_mahalanobis_flag_jitsuyo_sa20_mahalanobis_nearest + p_mahalanobis_flag_jitsuyo_sa20_mahalanobis_full  + p_mahalanobis_flag_jitsuyo_sa20_ipw +
     plot_layout(ncol = 4)
   
   ################## flag_isho------------
   # PSM-nearest
   es_PSM_flag_isho_sa20_PSM_nearest = feols(flag_isho ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_flag_isho_sa20_PSM_nearest <- aggregate(es_PSM_flag_isho_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_flag_isho_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_flag_isho_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_flag_isho_sa20_mahalanobis_nearest = feols(flag_isho ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_flag_isho_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_flag_isho_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_flag_isho_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_flag_isho_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_flag_isho_sa20_mahalanobis_full = feols(flag_isho ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_flag_isho_sa20_mahalanobis_full <- aggregate(es_mahalanobis_flag_isho_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_flag_isho_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_flag_isho_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_flag_isho_sa20_ipw = feols(flag_isho ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_flag_isho_sa20_ipw <- aggregate(es_mahalanobis_flag_isho_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_flag_isho_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_flag_isho_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_flag_isho <- p_PSM_flag_isho_sa20_PSM_nearest + p_mahalanobis_flag_isho_sa20_mahalanobis_nearest + p_mahalanobis_flag_isho_sa20_mahalanobis_full  + p_mahalanobis_flag_isho_sa20_ipw +
     plot_layout(ncol = 4)
   
   ################## log_patent------------
   # PSM-nearest
   es_PSM_log_patent_sa20_PSM_nearest = feols(log_patent ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_log_patent_sa20_PSM_nearest <- aggregate(es_PSM_log_patent_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_log_patent_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_log_patent_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_log_patent_sa20_mahalanobis_nearest = feols(log_patent ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_log_patent_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_log_patent_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_patent_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_log_patent_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_log_patent_sa20_mahalanobis_full = feols(log_patent ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_log_patent_sa20_mahalanobis_full <- aggregate(es_mahalanobis_log_patent_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_patent_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_log_patent_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_log_patent_sa20_ipw = feols(log_patent ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_log_patent_sa20_ipw <- aggregate(es_mahalanobis_log_patent_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_patent_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_log_patent_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_log_patent <- p_PSM_log_patent_sa20_PSM_nearest + p_mahalanobis_log_patent_sa20_mahalanobis_nearest + p_mahalanobis_log_patent_sa20_mahalanobis_full  + p_mahalanobis_log_patent_sa20_ipw +
     plot_layout(ncol = 4)
   
   ################## log_jitsuyo------------
   # PSM-nearest
   es_PSM_log_jitsuyo_sa20_PSM_nearest = feols(log_jitsuyo ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_log_jitsuyo_sa20_PSM_nearest <- aggregate(es_PSM_log_jitsuyo_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_log_jitsuyo_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_log_jitsuyo_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_log_jitsuyo_sa20_mahalanobis_nearest = feols(log_jitsuyo ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_log_jitsuyo_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_log_jitsuyo_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_jitsuyo_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_log_jitsuyo_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_log_jitsuyo_sa20_mahalanobis_full = feols(log_jitsuyo ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_log_jitsuyo_sa20_mahalanobis_full <- aggregate(es_mahalanobis_log_jitsuyo_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_jitsuyo_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_log_jitsuyo_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_log_jitsuyo_sa20_ipw = feols(log_jitsuyo ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_log_jitsuyo_sa20_ipw <- aggregate(es_mahalanobis_log_jitsuyo_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_jitsuyo_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_log_jitsuyo_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_log_jitsuyo <- p_PSM_log_jitsuyo_sa20_PSM_nearest + p_mahalanobis_log_jitsuyo_sa20_mahalanobis_nearest + p_mahalanobis_log_jitsuyo_sa20_mahalanobis_full  + p_mahalanobis_log_jitsuyo_sa20_ipw +
     plot_layout(ncol = 4)
   
   ################## log_isho------------
   # PSM-nearest
   es_PSM_log_isho_sa20_PSM_nearest = feols(log_isho ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_log_isho_sa20_PSM_nearest <- aggregate(es_PSM_log_isho_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_log_isho_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_log_isho_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_log_isho_sa20_mahalanobis_nearest = feols(log_isho ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_log_isho_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_log_isho_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_isho_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_log_isho_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_log_isho_sa20_mahalanobis_full = feols(log_isho ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_log_isho_sa20_mahalanobis_full <- aggregate(es_mahalanobis_log_isho_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_isho_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_log_isho_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_log_isho_sa20_ipw = feols(log_isho ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_log_isho_sa20_ipw <- aggregate(es_mahalanobis_log_isho_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_isho_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_log_isho_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_log_isho <- p_PSM_log_isho_sa20_PSM_nearest + p_mahalanobis_log_isho_sa20_mahalanobis_nearest + p_mahalanobis_log_isho_sa20_mahalanobis_full  + p_mahalanobis_log_isho_sa20_ipw +
     plot_layout(ncol = 4)
   
   ## output_IP-----------
   pa_IP_sum <- pa_flag_patent/pa_flag_jitsuyo/pa_flag_isho/pa_log_patent/pa_log_jitsuyo/pa_log_isho + plot_annotation(
     title = "Effect on the Intellectual Property",
     subtitle = target,
     #caption = caption_main
   ) &
     theme(text = element_text(family="Meiryo UI"))
   
   filename_pdf <- paste("03_robustness_output/", target,"_56IP_", count_intervention, ".pdf", sep="")
   ggsave(filename_pdf,pa_IP_sum, 
          width=35, height =48, units = "cm", dpi=200, device = cairo_pdf) 
   filename_png <- paste("03_robustness_output/", target,"_56IP_", count_intervention, ".png", sep="")
   ggsave(filename_png,pa_IP_sum, 
          width=35, height =48, units = "cm", dpi=200) 
 }
 robustness_PSM_investment <- function(target){
   ################## flag_investment_affiliate_domestic------------
   # PSM-nearest
   es_PSM_flag_investment_affiliate_domestic_sa20_PSM_nearest = feols(flag_investment_affiliate_domestic ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_flag_investment_affiliate_domestic_sa20_PSM_nearest <- aggregate(es_PSM_flag_investment_affiliate_domestic_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_flag_investment_affiliate_domestic_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_flag_investment_affiliate_domestic_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_flag_investment_affiliate_domestic_sa20_mahalanobis_nearest = feols(flag_investment_affiliate_domestic ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_flag_investment_affiliate_domestic_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_flag_investment_affiliate_domestic_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_flag_investment_affiliate_domestic_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_flag_investment_affiliate_domestic_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_flag_investment_affiliate_domestic_sa20_mahalanobis_full = feols(flag_investment_affiliate_domestic ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_flag_investment_affiliate_domestic_sa20_mahalanobis_full <- aggregate(es_mahalanobis_flag_investment_affiliate_domestic_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_flag_investment_affiliate_domestic_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_flag_investment_affiliate_domestic_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_flag_investment_affiliate_domestic_sa20_ipw = feols(flag_investment_affiliate_domestic ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_flag_investment_affiliate_domestic_sa20_ipw <- aggregate(es_mahalanobis_flag_investment_affiliate_domestic_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_flag_investment_affiliate_domestic_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_flag_investment_affiliate_domestic_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_flag_investment_affiliate_domestic <- p_PSM_flag_investment_affiliate_domestic_sa20_PSM_nearest + p_mahalanobis_flag_investment_affiliate_domestic_sa20_mahalanobis_nearest + p_mahalanobis_flag_investment_affiliate_domestic_sa20_mahalanobis_full  + p_mahalanobis_flag_investment_affiliate_domestic_sa20_ipw +
     plot_layout(ncol = 4)
   
   ################## flag_investment_affiliate_overseas------------
   # PSM-nearest
   es_PSM_flag_investment_affiliate_overseas_sa20_PSM_nearest = feols(flag_investment_affiliate_overseas ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_flag_investment_affiliate_overseas_sa20_PSM_nearest <- aggregate(es_PSM_flag_investment_affiliate_overseas_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_flag_investment_affiliate_overseas_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_flag_investment_affiliate_overseas_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_flag_investment_affiliate_overseas_sa20_mahalanobis_nearest = feols(flag_investment_affiliate_overseas ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_flag_investment_affiliate_overseas_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_flag_investment_affiliate_overseas_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_flag_investment_affiliate_overseas_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_flag_investment_affiliate_overseas_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_flag_investment_affiliate_overseas_sa20_mahalanobis_full = feols(flag_investment_affiliate_overseas ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_flag_investment_affiliate_overseas_sa20_mahalanobis_full <- aggregate(es_mahalanobis_flag_investment_affiliate_overseas_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_flag_investment_affiliate_overseas_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_flag_investment_affiliate_overseas_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_flag_investment_affiliate_overseas_sa20_ipw = feols(flag_investment_affiliate_overseas ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_flag_investment_affiliate_overseas_sa20_ipw <- aggregate(es_mahalanobis_flag_investment_affiliate_overseas_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_flag_investment_affiliate_overseas_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_flag_investment_affiliate_overseas_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_flag_investment_affiliate_overseas <- p_PSM_flag_investment_affiliate_overseas_sa20_PSM_nearest + p_mahalanobis_flag_investment_affiliate_overseas_sa20_mahalanobis_nearest + p_mahalanobis_flag_investment_affiliate_overseas_sa20_mahalanobis_full  + p_mahalanobis_flag_investment_affiliate_overseas_sa20_ipw +
     plot_layout(ncol = 4)
   
   ################## flag_dividend------------
   # PSM-nearest
   es_PSM_flag_dividend_sa20_PSM_nearest = feols(flag_dividend ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_flag_dividend_sa20_PSM_nearest <- aggregate(es_PSM_flag_dividend_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_flag_dividend_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_flag_dividend_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_flag_dividend_sa20_mahalanobis_nearest = feols(flag_dividend ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_flag_dividend_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_flag_dividend_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_flag_dividend_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_flag_dividend_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_flag_dividend_sa20_mahalanobis_full = feols(flag_dividend ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_flag_dividend_sa20_mahalanobis_full <- aggregate(es_mahalanobis_flag_dividend_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_flag_dividend_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_flag_dividend_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_flag_dividend_sa20_ipw = feols(flag_dividend ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_flag_dividend_sa20_ipw <- aggregate(es_mahalanobis_flag_dividend_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_flag_dividend_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_flag_dividend_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_flag_dividend <- p_PSM_flag_dividend_sa20_PSM_nearest + p_mahalanobis_flag_dividend_sa20_mahalanobis_nearest + p_mahalanobis_flag_dividend_sa20_mahalanobis_full  + p_mahalanobis_flag_dividend_sa20_ipw +
     plot_layout(ncol = 4)
   
   ################## log_investment_affiliate_domestic------------
   # PSM-nearest
   es_PSM_log_investment_affiliate_domestic_sa20_PSM_nearest = feols(log_investment_affiliate_domestic ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_log_investment_affiliate_domestic_sa20_PSM_nearest <- aggregate(es_PSM_log_investment_affiliate_domestic_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_log_investment_affiliate_domestic_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_log_investment_affiliate_domestic_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_log_investment_affiliate_domestic_sa20_mahalanobis_nearest = feols(log_investment_affiliate_domestic ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_log_investment_affiliate_domestic_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_log_investment_affiliate_domestic_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_investment_affiliate_domestic_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_log_investment_affiliate_domestic_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_log_investment_affiliate_domestic_sa20_mahalanobis_full = feols(log_investment_affiliate_domestic ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_log_investment_affiliate_domestic_sa20_mahalanobis_full <- aggregate(es_mahalanobis_log_investment_affiliate_domestic_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_investment_affiliate_domestic_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_log_investment_affiliate_domestic_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_log_investment_affiliate_domestic_sa20_ipw = feols(log_investment_affiliate_domestic ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_log_investment_affiliate_domestic_sa20_ipw <- aggregate(es_mahalanobis_log_investment_affiliate_domestic_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_investment_affiliate_domestic_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_log_investment_affiliate_domestic_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_log_investment_affiliate_domestic <- p_PSM_log_investment_affiliate_domestic_sa20_PSM_nearest + p_mahalanobis_log_investment_affiliate_domestic_sa20_mahalanobis_nearest + p_mahalanobis_log_investment_affiliate_domestic_sa20_mahalanobis_full  + p_mahalanobis_log_investment_affiliate_domestic_sa20_ipw +
     plot_layout(ncol = 4)
   
   ################## log_investment_affiliate_overseas------------
   # PSM-nearest
   es_PSM_log_investment_affiliate_overseas_sa20_PSM_nearest = feols(log_investment_affiliate_overseas ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_log_investment_affiliate_overseas_sa20_PSM_nearest <- aggregate(es_PSM_log_investment_affiliate_overseas_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_log_investment_affiliate_overseas_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_log_investment_affiliate_overseas_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_log_investment_affiliate_overseas_sa20_mahalanobis_nearest = feols(log_investment_affiliate_overseas ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_log_investment_affiliate_overseas_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_log_investment_affiliate_overseas_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_investment_affiliate_overseas_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_log_investment_affiliate_overseas_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_log_investment_affiliate_overseas_sa20_mahalanobis_full = feols(log_investment_affiliate_overseas ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_log_investment_affiliate_overseas_sa20_mahalanobis_full <- aggregate(es_mahalanobis_log_investment_affiliate_overseas_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_investment_affiliate_overseas_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_log_investment_affiliate_overseas_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_log_investment_affiliate_overseas_sa20_ipw = feols(log_investment_affiliate_overseas ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_log_investment_affiliate_overseas_sa20_ipw <- aggregate(es_mahalanobis_log_investment_affiliate_overseas_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_investment_affiliate_overseas_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_log_investment_affiliate_overseas_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_log_investment_affiliate_overseas <- p_PSM_log_investment_affiliate_overseas_sa20_PSM_nearest + p_mahalanobis_log_investment_affiliate_overseas_sa20_mahalanobis_nearest + p_mahalanobis_log_investment_affiliate_overseas_sa20_mahalanobis_full  + p_mahalanobis_log_investment_affiliate_overseas_sa20_ipw +
     plot_layout(ncol = 4)
   
   ################## log_dividend------------
   # PSM-nearest
   es_PSM_log_dividend_sa20_PSM_nearest = feols(log_dividend ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_PSM_nearest, cluster="id", weights=dfm_kikatsu2_PSM_nearest$weights
   )
   att_PSM_log_dividend_sa20_PSM_nearest <- aggregate(es_PSM_log_dividend_sa20_PSM_nearest, "att", full = FALSE, use_weights = TRUE)
   p_PSM_log_dividend_sa20_PSM_nearest <- ggiplot(list('S&A - PSM_nearest, 90 & 95% CI' = es_PSM_log_dividend_sa20_PSM_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - PSM_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
   
   # mahalanobis-nearest
   es_mahalanobis_log_dividend_sa20_mahalanobis_nearest = feols(log_dividend ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_nearest, cluster="id", weights=dfm_kikatsu2_mahalanobis_nearest$weights
   )
   att_mahalanobis_log_dividend_sa20_mahalanobis_nearest <- aggregate(es_mahalanobis_log_dividend_sa20_mahalanobis_nearest, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_dividend_sa20_mahalanobis_nearest <- ggiplot(list('S&A - mahalanobis_nearest, 90 & 95% CI' = es_mahalanobis_log_dividend_sa20_mahalanobis_nearest),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_nearest, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # mahalanobis-full
   es_mahalanobis_log_dividend_sa20_mahalanobis_full = feols(log_dividend ~ sunab(intervention_year,year,ref.p=-2)  | id + year, dfm_kikatsu2_mahalanobis_full, cluster="id", weights=dfm_kikatsu2_mahalanobis_full$weights
   )
   att_mahalanobis_log_dividend_sa20_mahalanobis_full <- aggregate(es_mahalanobis_log_dividend_sa20_mahalanobis_full, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_dividend_sa20_mahalanobis_full <- ggiplot(list('S&A -mahalanobis_full, 90 & 95% CI' = es_mahalanobis_log_dividend_sa20_mahalanobis_full),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - mahalanobis_full, 90 & 95% CI") +
     theme_light()+ theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   # ipw
   es_mahalanobis_log_dividend_sa20_ipw = feols(log_dividend ~ sunab(intervention_year,year,ref.p=-2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
   att_mahalanobis_log_dividend_sa20_ipw <- aggregate(es_mahalanobis_log_dividend_sa20_ipw, "att", full = FALSE, use_weights = TRUE)
   p_mahalanobis_log_dividend_sa20_ipw <- ggiplot(list('S&A -IPW, 90 & 95% CI' = es_mahalanobis_log_dividend_sa20_ipw),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + labs(subtitle = "S&A - IPW, 90 & 95% CI") +
     theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none", plot.title = element_blank())
   
   ## create grid arrange
   pa_log_dividend <- p_PSM_log_dividend_sa20_PSM_nearest + p_mahalanobis_log_dividend_sa20_mahalanobis_nearest + p_mahalanobis_log_dividend_sa20_mahalanobis_full  + p_mahalanobis_log_dividend_sa20_ipw +
     plot_layout(ncol = 4)
   
   ## output_investment-----------
   pa_investment_sum <- pa_flag_investment_affiliate_domestic/pa_flag_investment_affiliate_overseas/pa_flag_dividend/pa_log_investment_affiliate_domestic/pa_log_investment_affiliate_overseas/pa_log_dividend + plot_annotation(
     title = "Effect on the Investment",
     subtitle = target,
     #caption = caption_main
   ) &
     theme(text = element_text(family="Meiryo UI"))
   
   filename_pdf <- paste("03_robustness_output/", target,"_57investment_", count_intervention, ".pdf", sep="")
   ggsave(filename_pdf,pa_investment_sum, 
          width=35, height =48, units = "cm", dpi=200, device = cairo_pdf) 
   filename_png <- paste("03_robustness_output/", target,"_57investment_", count_intervention, ".png", sep="")
   ggsave(filename_png,pa_investment_sum, 
          width=35, height =48, units = "cm", dpi=200) 
 }
 
 
 
 
 ## conduct evaluation------------
 
 print(target)
 try(robustness_PSM_asset(target))
 try(robustness_PSM_employment(target))
 try(robustness_PSM_business(target))
 try(robustness_PSM_trade(target))
 try(robustness_PSM_trainingRD(target))
 try(robustness_PSM_IP(target))
 try(robustness_PSM_investment(target))
