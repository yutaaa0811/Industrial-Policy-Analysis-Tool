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

# define functions  for evaluation-----------
analysis_asset <- function(target){
  ################## log_sum_asset------------
  # ipw
  es_PSM_log_sum_asset_sa20 = feols(log_sum_asset ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_log_sum_asset_sa20 <- aggregate(es_PSM_log_sum_asset_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_sum_asset_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_sum_asset_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_sum_asset", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_sum_asset <- att_gt(yname = "log_sum_asset",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                             xformla = formula_cs, 
                             est_method = "ipw",base_period="universal",alp=0.05,
                             data = dfm_kikatsu2_ipw,
                             anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_sum_asset_smry <- aggte(cs_log_sum_asset, type = "dynamic",na.rm = TRUE)
  
  p_log_sum_asset_cs <- ggdid(cs_log_sum_asset_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_log_sum_asset_cs <- aggte(cs_log_sum_asset, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  log_sum_asset_summary <- data.frame(method = method,
                                      estimate = c(att_PSM_log_sum_asset_sa20[1], a_log_sum_asset_cs$overall.att
                                      ),
                                      se = c(att_PSM_log_sum_asset_sa20[2],a_log_sum_asset_cs$overall.se
                                      ),
                                      up = c(att_PSM_log_sum_asset_sa20[1] + 1.645*att_PSM_log_sum_asset_sa20[2],a_log_sum_asset_cs$overall.att + 1.645*a_log_sum_asset_cs$overall.se
                                      ),
                                      down = c(att_PSM_log_sum_asset_sa20[1] - 1.645*att_PSM_log_sum_asset_sa20[2],a_log_sum_asset_cs$overall.att - 1.645*a_log_sum_asset_cs$overall.se
                                      ),
                                      up95 = c(att_PSM_log_sum_asset_sa20[1] + 1.960*att_PSM_log_sum_asset_sa20[2],a_log_sum_asset_cs$overall.att + 1.960*a_log_sum_asset_cs$overall.se
                                      ),
                                      down95 = c(att_PSM_log_sum_asset_sa20[1] - 1.960*att_PSM_log_sum_asset_sa20[2],a_log_sum_asset_cs$overall.att - 1.960*a_log_sum_asset_cs$overall.se
                                      ),
                                      up99 = c(att_PSM_log_sum_asset_sa20[1] + 2.5758*att_PSM_log_sum_asset_sa20[2],a_log_sum_asset_cs$overall.att + 2.5758*a_log_sum_asset_cs$overall.se
                                      ),
                                      down99 = c(att_PSM_log_sum_asset_sa20[1] - 2.5758*att_PSM_log_sum_asset_sa20[2],a_log_sum_asset_cs$overall.att - 2.5758*a_log_sum_asset_cs$overall.se
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  log_sum_asset_summary_PDP <- data.frame(Category = "asset", Variable = "log_sum_asset", 
                                          ATT_SA = paste(round(log_sum_asset_summary[1,2],3), " (", round(log_sum_asset_summary[1,3],3), ") ",log_sum_asset_summary[1,"result"],
                                                         sep =""),
                                          PT_SA = ifelse(log_sum_asset_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                          ATT_CS = paste(round(log_sum_asset_summary[2,2],3), " (", round(log_sum_asset_summary[2,3],3), ") ",log_sum_asset_summary[2,"result"],
                                                         sep =""),
                                          PT_CS = ifelse(log_sum_asset_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_log_sum_asset_summary <- ggplot(log_sum_asset_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_sum_asset_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_sum_asset_summary$pretrend[1]) +
    geom_rect(data=log_sum_asset_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_sum_asset_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_sum_asset_summary$down, na.rm=T),max(log_sum_asset_summary$up, na.rm=T)), max(-min(log_sum_asset_summary$down, na.rm=T),max(log_sum_asset_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_sum_asset <- p_PSM_log_sum_asset_sa20+p_log_sum_asset_cs+p_log_sum_asset_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_log_sum_asset)
  
  ################## log_tangible_asset------------
  # ipw
  es_PSM_log_tangible_asset_sa20 = feols(log_tangible_asset ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_log_tangible_asset_sa20 <- aggregate(es_PSM_log_tangible_asset_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_tangible_asset_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_tangible_asset_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_tangible_asset", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_tangible_asset <- att_gt(yname = "log_tangible_asset",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                                  xformla = formula_cs, 
                                  est_method = "ipw",base_period="universal",alp=0.05,
                                  data = dfm_kikatsu2_ipw,
                                  anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_tangible_asset_smry <- aggte(cs_log_tangible_asset, type = "dynamic",na.rm = TRUE)
  
  p_log_tangible_asset_cs <- ggdid(cs_log_tangible_asset_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_log_tangible_asset_cs <- aggte(cs_log_tangible_asset, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  log_tangible_asset_summary <- data.frame(method = method,
                                           estimate = c(att_PSM_log_tangible_asset_sa20[1], a_log_tangible_asset_cs$overall.att
                                           ),
                                           se = c(att_PSM_log_tangible_asset_sa20[2],a_log_tangible_asset_cs$overall.se
                                           ),
                                           up = c(att_PSM_log_tangible_asset_sa20[1] + 1.645*att_PSM_log_tangible_asset_sa20[2],a_log_tangible_asset_cs$overall.att + 1.645*a_log_tangible_asset_cs$overall.se
                                           ),
                                           down = c(att_PSM_log_tangible_asset_sa20[1] - 1.645*att_PSM_log_tangible_asset_sa20[2],a_log_tangible_asset_cs$overall.att - 1.645*a_log_tangible_asset_cs$overall.se
                                           ),
                                           up95 = c(att_PSM_log_tangible_asset_sa20[1] + 1.960*att_PSM_log_tangible_asset_sa20[2],a_log_tangible_asset_cs$overall.att + 1.960*a_log_tangible_asset_cs$overall.se
                                           ),
                                           down95 = c(att_PSM_log_tangible_asset_sa20[1] - 1.960*att_PSM_log_tangible_asset_sa20[2],a_log_tangible_asset_cs$overall.att - 1.960*a_log_tangible_asset_cs$overall.se
                                           ),
                                           up99 = c(att_PSM_log_tangible_asset_sa20[1] + 2.5758*att_PSM_log_tangible_asset_sa20[2],a_log_tangible_asset_cs$overall.att + 2.5758*a_log_tangible_asset_cs$overall.se
                                           ),
                                           down99 = c(att_PSM_log_tangible_asset_sa20[1] - 2.5758*att_PSM_log_tangible_asset_sa20[2],a_log_tangible_asset_cs$overall.att - 2.5758*a_log_tangible_asset_cs$overall.se
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  log_tangible_asset_summary_PDP <- data.frame(Category = "asset", Variable = "log_tangible_asset", 
                                               ATT_SA = paste(round(log_tangible_asset_summary[1,2],3), " (", round(log_tangible_asset_summary[1,3],3), ") ",log_tangible_asset_summary[1,"result"],
                                                              sep =""),
                                               PT_SA = ifelse(log_tangible_asset_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                               ATT_CS = paste(round(log_tangible_asset_summary[2,2],3), " (", round(log_tangible_asset_summary[2,3],3), ") ",log_tangible_asset_summary[2,"result"],
                                                              sep =""),
                                               PT_CS = ifelse(log_tangible_asset_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_log_tangible_asset_summary <- ggplot(log_tangible_asset_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_tangible_asset_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_tangible_asset_summary$pretrend[1]) +
    geom_rect(data=log_tangible_asset_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_tangible_asset_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_tangible_asset_summary$down, na.rm=T),max(log_tangible_asset_summary$up, na.rm=T)), max(-min(log_tangible_asset_summary$down, na.rm=T),max(log_tangible_asset_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_tangible_asset <- p_PSM_log_tangible_asset_sa20+p_log_tangible_asset_cs+p_log_tangible_asset_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_log_tangible_asset)
  
  ################## log_intangible_asset------------
  # ipw
  es_PSM_log_intangible_asset_sa20 = feols(log_intangible_asset ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_log_intangible_asset_sa20 <- aggregate(es_PSM_log_intangible_asset_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_intangible_asset_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_intangible_asset_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_intangible_asset", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_intangible_asset <- att_gt(yname = "log_intangible_asset",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                                    xformla = formula_cs, 
                                    est_method = "ipw",base_period="universal",alp=0.05,
                                    data = dfm_kikatsu2_ipw,
                                    anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_intangible_asset_smry <- aggte(cs_log_intangible_asset, type = "dynamic",na.rm = TRUE)
  
  p_log_intangible_asset_cs <- ggdid(cs_log_intangible_asset_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_log_intangible_asset_cs <- aggte(cs_log_intangible_asset, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  log_intangible_asset_summary <- data.frame(method = method,
                                             estimate = c(att_PSM_log_intangible_asset_sa20[1], a_log_intangible_asset_cs$overall.att
                                             ),
                                             se = c(att_PSM_log_intangible_asset_sa20[2],a_log_intangible_asset_cs$overall.se
                                             ),
                                             up = c(att_PSM_log_intangible_asset_sa20[1] + 1.645*att_PSM_log_intangible_asset_sa20[2],a_log_intangible_asset_cs$overall.att + 1.645*a_log_intangible_asset_cs$overall.se
                                             ),
                                             down = c(att_PSM_log_intangible_asset_sa20[1] - 1.645*att_PSM_log_intangible_asset_sa20[2],a_log_intangible_asset_cs$overall.att - 1.645*a_log_intangible_asset_cs$overall.se
                                             ),
                                             up95 = c(att_PSM_log_intangible_asset_sa20[1] + 1.960*att_PSM_log_intangible_asset_sa20[2],a_log_intangible_asset_cs$overall.att + 1.960*a_log_intangible_asset_cs$overall.se
                                             ),
                                             down95 = c(att_PSM_log_intangible_asset_sa20[1] - 1.960*att_PSM_log_intangible_asset_sa20[2],a_log_intangible_asset_cs$overall.att - 1.960*a_log_intangible_asset_cs$overall.se
                                             ),
                                             up99 = c(att_PSM_log_intangible_asset_sa20[1] + 2.5758*att_PSM_log_intangible_asset_sa20[2],a_log_intangible_asset_cs$overall.att + 2.5758*a_log_intangible_asset_cs$overall.se
                                             ),
                                             down99 = c(att_PSM_log_intangible_asset_sa20[1] - 2.5758*att_PSM_log_intangible_asset_sa20[2],a_log_intangible_asset_cs$overall.att - 2.5758*a_log_intangible_asset_cs$overall.se
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  log_intangible_asset_summary_PDP <- data.frame(Category = "asset", Variable = "log_intangible_asset", 
                                                 ATT_SA = paste(round(log_intangible_asset_summary[1,2],3), " (", round(log_intangible_asset_summary[1,3],3), ") ",log_intangible_asset_summary[1,"result"],
                                                                sep =""),
                                                 PT_SA = ifelse(log_intangible_asset_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                                 ATT_CS = paste(round(log_intangible_asset_summary[2,2],3), " (", round(log_intangible_asset_summary[2,3],3), ") ",log_intangible_asset_summary[2,"result"],
                                                                sep =""),
                                                 PT_CS = ifelse(log_intangible_asset_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_log_intangible_asset_summary <- ggplot(log_intangible_asset_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_intangible_asset_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_intangible_asset_summary$pretrend[1]) +
    geom_rect(data=log_intangible_asset_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_intangible_asset_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_intangible_asset_summary$down, na.rm=T),max(log_intangible_asset_summary$up, na.rm=T)), max(-min(log_intangible_asset_summary$down, na.rm=T),max(log_intangible_asset_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_intangible_asset <- p_PSM_log_intangible_asset_sa20+p_log_intangible_asset_cs+p_log_intangible_asset_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_log_intangible_asset)
  
  ## output_asset-----------
  pa_asset_sum <- pa_log_sum_asset / pa_log_tangible_asset / pa_log_intangible_asset + plot_annotation(
    title = "Effect on the Asset",
    subtitle = target
  ) &
    theme(text = element_text(family="Meiryo UI"))
  
  filename_pdf <- paste("02_analysis_output/", target,"_01asset_", count_intervention, ".pdf", sep="")
  ggsave(filename_pdf,pa_asset_sum, 
         width=15, height =18, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("02_analysis_output/", target,"_01asset_", count_intervention, ".png", sep="")
  ggsave(filename_png,pa_asset_sum, 
         width=15, height =18, units = "cm", dpi=200) 
  
  write.xlsx(list("log_sum_asset"=log_sum_asset_summary, "log_tangible_asset"= log_tangible_asset_summary, "log_intangible_asset"= log_intangible_asset_summary), paste("02_analysis_output/",target,"_01asset_table.xlsx", sep=""))
  write.xlsx(list("log_sum_asset"=log_sum_asset_summary_appendix, "log_tangible_asset"= log_tangible_asset_summary_appendix, "log_intangible_asset"= log_intangible_asset_summary_appendix), paste("02_analysis_output/",target,"_01asset_table_appendix.xlsx", sep=""))
  
  
  # まとめ
  summary_PDP <- rbind(log_sum_asset_summary_PDP,log_tangible_asset_summary_PDP,log_intangible_asset_summary_PDP) %>%
    mutate(Category = "asset")
  write.xlsx(list("result"= summary_PDP), paste("02_analysis_output/",target,"_01asset_table_PDP.xlsx"))
  
}
analysis_employment <- function(target){
  ################## log_workers------------
  # ipw
  es_PSM_log_workers_sa20 = feols(log_workers ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_log_workers_sa20 <- aggregate(es_PSM_log_workers_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_workers_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_workers_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_workers", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_workers <- att_gt(yname = "log_workers",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                           xformla = formula_cs, 
                           est_method = "ipw",base_period="universal",alp=0.05,
                           data = dfm_kikatsu2_ipw,
                           anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_workers_smry <- aggte(cs_log_workers, type = "dynamic",na.rm = TRUE)
  
  p_log_workers_cs <- ggdid(cs_log_workers_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_log_workers_cs <- aggte(cs_log_workers, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  log_workers_summary <- data.frame(method = method,
                                    estimate = c(att_PSM_log_workers_sa20[1], a_log_workers_cs$overall.att
                                    ),
                                    se = c(att_PSM_log_workers_sa20[2],a_log_workers_cs$overall.se
                                    ),
                                    up = c(att_PSM_log_workers_sa20[1] + 1.645*att_PSM_log_workers_sa20[2],a_log_workers_cs$overall.att + 1.645*a_log_workers_cs$overall.se
                                    ),
                                    down = c(att_PSM_log_workers_sa20[1] - 1.645*att_PSM_log_workers_sa20[2],a_log_workers_cs$overall.att - 1.645*a_log_workers_cs$overall.se
                                    ),
                                    up95 = c(att_PSM_log_workers_sa20[1] + 1.960*att_PSM_log_workers_sa20[2],a_log_workers_cs$overall.att + 1.960*a_log_workers_cs$overall.se
                                    ),
                                    down95 = c(att_PSM_log_workers_sa20[1] - 1.960*att_PSM_log_workers_sa20[2],a_log_workers_cs$overall.att - 1.960*a_log_workers_cs$overall.se
                                    ),
                                    up99 = c(att_PSM_log_workers_sa20[1] + 2.5758*att_PSM_log_workers_sa20[2],a_log_workers_cs$overall.att + 2.5758*a_log_workers_cs$overall.se
                                    ),
                                    down99 = c(att_PSM_log_workers_sa20[1] - 2.5758*att_PSM_log_workers_sa20[2],a_log_workers_cs$overall.att - 2.5758*a_log_workers_cs$overall.se
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  log_workers_summary_PDP <- data.frame(Category = "asset", Variable = "log_workers", 
                                        ATT_SA = paste(round(log_workers_summary[1,2],3), " (", round(log_workers_summary[1,3],3), ") ",log_workers_summary[1,"result"],
                                                       sep =""),
                                        PT_SA = ifelse(log_workers_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                        ATT_CS = paste(round(log_workers_summary[2,2],3), " (", round(log_workers_summary[2,3],3), ") ",log_workers_summary[2,"result"],
                                                       sep =""),
                                        PT_CS = ifelse(log_workers_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_log_workers_summary <- ggplot(log_workers_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_workers_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_workers_summary$pretrend[1]) +
    geom_rect(data=log_workers_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_workers_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_workers_summary$down, na.rm=T),max(log_workers_summary$up, na.rm=T)), max(-min(log_workers_summary$down, na.rm=T),max(log_workers_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_workers <- p_PSM_log_workers_sa20+p_log_workers_cs+p_log_workers_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_log_workers)
  
  ################## log_indefinite_workers------------
  # ipw
  es_PSM_log_indefinite_workers_sa20 = feols(log_indefinite_workers ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_log_indefinite_workers_sa20 <- aggregate(es_PSM_log_indefinite_workers_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_indefinite_workers_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_indefinite_workers_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_indefinite_workers", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_indefinite_workers <- att_gt(yname = "log_indefinite_workers",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                                      xformla = formula_cs, 
                                      est_method = "ipw",base_period="universal",alp=0.05,
                                      data = dfm_kikatsu2_ipw,
                                      anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_indefinite_workers_smry <- aggte(cs_log_indefinite_workers, type = "dynamic",na.rm = TRUE)
  
  p_log_indefinite_workers_cs <- ggdid(cs_log_indefinite_workers_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_log_indefinite_workers_cs <- aggte(cs_log_indefinite_workers, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  log_indefinite_workers_summary <- data.frame(method = method,
                                               estimate = c(att_PSM_log_indefinite_workers_sa20[1], a_log_indefinite_workers_cs$overall.att
                                               ),
                                               se = c(att_PSM_log_indefinite_workers_sa20[2],a_log_indefinite_workers_cs$overall.se
                                               ),
                                               up = c(att_PSM_log_indefinite_workers_sa20[1] + 1.645*att_PSM_log_indefinite_workers_sa20[2],a_log_indefinite_workers_cs$overall.att + 1.645*a_log_indefinite_workers_cs$overall.se
                                               ),
                                               down = c(att_PSM_log_indefinite_workers_sa20[1] - 1.645*att_PSM_log_indefinite_workers_sa20[2],a_log_indefinite_workers_cs$overall.att - 1.645*a_log_indefinite_workers_cs$overall.se
                                               ),
                                               up95 = c(att_PSM_log_indefinite_workers_sa20[1] + 1.960*att_PSM_log_indefinite_workers_sa20[2],a_log_indefinite_workers_cs$overall.att + 1.960*a_log_indefinite_workers_cs$overall.se
                                               ),
                                               down95 = c(att_PSM_log_indefinite_workers_sa20[1] - 1.960*att_PSM_log_indefinite_workers_sa20[2],a_log_indefinite_workers_cs$overall.att - 1.960*a_log_indefinite_workers_cs$overall.se
                                               ),
                                               up99 = c(att_PSM_log_indefinite_workers_sa20[1] + 2.5758*att_PSM_log_indefinite_workers_sa20[2],a_log_indefinite_workers_cs$overall.att + 2.5758*a_log_indefinite_workers_cs$overall.se
                                               ),
                                               down99 = c(att_PSM_log_indefinite_workers_sa20[1] - 2.5758*att_PSM_log_indefinite_workers_sa20[2],a_log_indefinite_workers_cs$overall.att - 2.5758*a_log_indefinite_workers_cs$overall.se
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  log_indefinite_workers_summary_PDP <- data.frame(Category = "asset", Variable = "log_indefinite_workers", 
                                                   ATT_SA = paste(round(log_indefinite_workers_summary[1,2],3), " (", round(log_indefinite_workers_summary[1,3],3), ") ",log_indefinite_workers_summary[1,"result"],
                                                                  sep =""),
                                                   PT_SA = ifelse(log_indefinite_workers_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                                   ATT_CS = paste(round(log_indefinite_workers_summary[2,2],3), " (", round(log_indefinite_workers_summary[2,3],3), ") ",log_indefinite_workers_summary[2,"result"],
                                                                  sep =""),
                                                   PT_CS = ifelse(log_indefinite_workers_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_log_indefinite_workers_summary <- ggplot(log_indefinite_workers_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_indefinite_workers_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_indefinite_workers_summary$pretrend[1]) +
    geom_rect(data=log_indefinite_workers_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_indefinite_workers_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_indefinite_workers_summary$down, na.rm=T),max(log_indefinite_workers_summary$up, na.rm=T)), max(-min(log_indefinite_workers_summary$down, na.rm=T),max(log_indefinite_workers_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_indefinite_workers <- p_PSM_log_indefinite_workers_sa20+p_log_indefinite_workers_cs+p_log_indefinite_workers_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_log_indefinite_workers)
  
  ################## log_fixedterm_workers------------
  # ipw
  es_PSM_log_fixedterm_workers_sa20 = feols(log_fixedterm_workers ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_log_fixedterm_workers_sa20 <- aggregate(es_PSM_log_fixedterm_workers_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_fixedterm_workers_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_fixedterm_workers_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_fixedterm_workers", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_fixedterm_workers <- att_gt(yname = "log_fixedterm_workers",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                                     xformla = formula_cs, 
                                     est_method = "ipw",base_period="universal",alp=0.05,
                                     data = dfm_kikatsu2_ipw,
                                     anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_fixedterm_workers_smry <- aggte(cs_log_fixedterm_workers, type = "dynamic",na.rm = TRUE)
  
  p_log_fixedterm_workers_cs <- ggdid(cs_log_fixedterm_workers_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_log_fixedterm_workers_cs <- aggte(cs_log_fixedterm_workers, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  log_fixedterm_workers_summary <- data.frame(method = method,
                                              estimate = c(att_PSM_log_fixedterm_workers_sa20[1], a_log_fixedterm_workers_cs$overall.att
                                              ),
                                              se = c(att_PSM_log_fixedterm_workers_sa20[2],a_log_fixedterm_workers_cs$overall.se
                                              ),
                                              up = c(att_PSM_log_fixedterm_workers_sa20[1] + 1.645*att_PSM_log_fixedterm_workers_sa20[2],a_log_fixedterm_workers_cs$overall.att + 1.645*a_log_fixedterm_workers_cs$overall.se
                                              ),
                                              down = c(att_PSM_log_fixedterm_workers_sa20[1] - 1.645*att_PSM_log_fixedterm_workers_sa20[2],a_log_fixedterm_workers_cs$overall.att - 1.645*a_log_fixedterm_workers_cs$overall.se
                                              ),
                                              up95 = c(att_PSM_log_fixedterm_workers_sa20[1] + 1.960*att_PSM_log_fixedterm_workers_sa20[2],a_log_fixedterm_workers_cs$overall.att + 1.960*a_log_fixedterm_workers_cs$overall.se
                                              ),
                                              down95 = c(att_PSM_log_fixedterm_workers_sa20[1] - 1.960*att_PSM_log_fixedterm_workers_sa20[2],a_log_fixedterm_workers_cs$overall.att - 1.960*a_log_fixedterm_workers_cs$overall.se
                                              ),
                                              up99 = c(att_PSM_log_fixedterm_workers_sa20[1] + 2.5758*att_PSM_log_fixedterm_workers_sa20[2],a_log_fixedterm_workers_cs$overall.att + 2.5758*a_log_fixedterm_workers_cs$overall.se
                                              ),
                                              down99 = c(att_PSM_log_fixedterm_workers_sa20[1] - 2.5758*att_PSM_log_fixedterm_workers_sa20[2],a_log_fixedterm_workers_cs$overall.att - 2.5758*a_log_fixedterm_workers_cs$overall.se
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  log_fixedterm_workers_summary_PDP <- data.frame(Category = "asset", Variable = "log_fixedterm_workers", 
                                                  ATT_SA = paste(round(log_fixedterm_workers_summary[1,2],3), " (", round(log_fixedterm_workers_summary[1,3],3), ") ",log_fixedterm_workers_summary[1,"result"],
                                                                 sep =""),
                                                  PT_SA = ifelse(log_fixedterm_workers_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                                  ATT_CS = paste(round(log_fixedterm_workers_summary[2,2],3), " (", round(log_fixedterm_workers_summary[2,3],3), ") ",log_fixedterm_workers_summary[2,"result"],
                                                                 sep =""),
                                                  PT_CS = ifelse(log_fixedterm_workers_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_log_fixedterm_workers_summary <- ggplot(log_fixedterm_workers_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_fixedterm_workers_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_fixedterm_workers_summary$pretrend[1]) +
    geom_rect(data=log_fixedterm_workers_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_fixedterm_workers_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_fixedterm_workers_summary$down, na.rm=T),max(log_fixedterm_workers_summary$up, na.rm=T)), max(-min(log_fixedterm_workers_summary$down, na.rm=T),max(log_fixedterm_workers_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_fixedterm_workers <- p_PSM_log_fixedterm_workers_sa20+p_log_fixedterm_workers_cs+p_log_fixedterm_workers_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_log_fixedterm_workers)
  
  ################## log_fixedterm_workers_equivalent------------
  # ipw
  es_PSM_log_fixedterm_workers_equivalent_sa20 = feols(log_fixedterm_workers_equivalent ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_log_fixedterm_workers_equivalent_sa20 <- aggregate(es_PSM_log_fixedterm_workers_equivalent_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_fixedterm_workers_equivalent_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_fixedterm_workers_equivalent_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_fixedterm_workers_equivalent", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_fixedterm_workers_equivalent <- att_gt(yname = "log_fixedterm_workers_equivalent",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                                                xformla = formula_cs, 
                                                est_method = "ipw",base_period="universal",alp=0.05,
                                                data = dfm_kikatsu2_ipw,
                                                anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_fixedterm_workers_equivalent_smry <- aggte(cs_log_fixedterm_workers_equivalent, type = "dynamic",na.rm = TRUE)
  
  p_log_fixedterm_workers_equivalent_cs <- ggdid(cs_log_fixedterm_workers_equivalent_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_log_fixedterm_workers_equivalent_cs <- aggte(cs_log_fixedterm_workers_equivalent, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  log_fixedterm_workers_equivalent_summary <- data.frame(method = method,
                                                         estimate = c(att_PSM_log_fixedterm_workers_equivalent_sa20[1], a_log_fixedterm_workers_equivalent_cs$overall.att
                                                         ),
                                                         se = c(att_PSM_log_fixedterm_workers_equivalent_sa20[2],a_log_fixedterm_workers_equivalent_cs$overall.se
                                                         ),
                                                         up = c(att_PSM_log_fixedterm_workers_equivalent_sa20[1] + 1.645*att_PSM_log_fixedterm_workers_equivalent_sa20[2],a_log_fixedterm_workers_equivalent_cs$overall.att + 1.645*a_log_fixedterm_workers_equivalent_cs$overall.se
                                                         ),
                                                         down = c(att_PSM_log_fixedterm_workers_equivalent_sa20[1] - 1.645*att_PSM_log_fixedterm_workers_equivalent_sa20[2],a_log_fixedterm_workers_equivalent_cs$overall.att - 1.645*a_log_fixedterm_workers_equivalent_cs$overall.se
                                                         ),
                                                         up95 = c(att_PSM_log_fixedterm_workers_equivalent_sa20[1] + 1.960*att_PSM_log_fixedterm_workers_equivalent_sa20[2],a_log_fixedterm_workers_equivalent_cs$overall.att + 1.960*a_log_fixedterm_workers_equivalent_cs$overall.se
                                                         ),
                                                         down95 = c(att_PSM_log_fixedterm_workers_equivalent_sa20[1] - 1.960*att_PSM_log_fixedterm_workers_equivalent_sa20[2],a_log_fixedterm_workers_equivalent_cs$overall.att - 1.960*a_log_fixedterm_workers_equivalent_cs$overall.se
                                                         ),
                                                         up99 = c(att_PSM_log_fixedterm_workers_equivalent_sa20[1] + 2.5758*att_PSM_log_fixedterm_workers_equivalent_sa20[2],a_log_fixedterm_workers_equivalent_cs$overall.att + 2.5758*a_log_fixedterm_workers_equivalent_cs$overall.se
                                                         ),
                                                         down99 = c(att_PSM_log_fixedterm_workers_equivalent_sa20[1] - 2.5758*att_PSM_log_fixedterm_workers_equivalent_sa20[2],a_log_fixedterm_workers_equivalent_cs$overall.att - 2.5758*a_log_fixedterm_workers_equivalent_cs$overall.se
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  log_fixedterm_workers_equivalent_summary_PDP <- data.frame(Category = "asset", Variable = "log_fixedterm_workers_equivalent", 
                                                             ATT_SA = paste(round(log_fixedterm_workers_equivalent_summary[1,2],3), " (", round(log_fixedterm_workers_equivalent_summary[1,3],3), ") ",log_fixedterm_workers_equivalent_summary[1,"result"],
                                                                            sep =""),
                                                             PT_SA = ifelse(log_fixedterm_workers_equivalent_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                                             ATT_CS = paste(round(log_fixedterm_workers_equivalent_summary[2,2],3), " (", round(log_fixedterm_workers_equivalent_summary[2,3],3), ") ",log_fixedterm_workers_equivalent_summary[2,"result"],
                                                                            sep =""),
                                                             PT_CS = ifelse(log_fixedterm_workers_equivalent_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_log_fixedterm_workers_equivalent_summary <- ggplot(log_fixedterm_workers_equivalent_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_fixedterm_workers_equivalent_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_fixedterm_workers_equivalent_summary$pretrend[1]) +
    geom_rect(data=log_fixedterm_workers_equivalent_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_fixedterm_workers_equivalent_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_fixedterm_workers_equivalent_summary$down, na.rm=T),max(log_fixedterm_workers_equivalent_summary$up, na.rm=T)), max(-min(log_fixedterm_workers_equivalent_summary$down, na.rm=T),max(log_fixedterm_workers_equivalent_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_fixedterm_workers_equivalent <- p_PSM_log_fixedterm_workers_equivalent_sa20+p_log_fixedterm_workers_equivalent_cs+p_log_fixedterm_workers_equivalent_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_log_fixedterm_workers_equivalent)
  
  ################## log_salary------------
  # ipw
  es_PSM_log_salary_sa20 = feols(log_salary ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_log_salary_sa20 <- aggregate(es_PSM_log_salary_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_salary_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_salary_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_salary", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_salary <- att_gt(yname = "log_salary",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                          xformla = formula_cs, 
                          est_method = "ipw",base_period="universal",alp=0.05,
                          data = dfm_kikatsu2_ipw,
                          anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_salary_smry <- aggte(cs_log_salary, type = "dynamic",na.rm = TRUE)
  
  p_log_salary_cs <- ggdid(cs_log_salary_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_log_salary_cs <- aggte(cs_log_salary, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  log_salary_summary <- data.frame(method = method,
                                   estimate = c(att_PSM_log_salary_sa20[1], a_log_salary_cs$overall.att
                                   ),
                                   se = c(att_PSM_log_salary_sa20[2],a_log_salary_cs$overall.se
                                   ),
                                   up = c(att_PSM_log_salary_sa20[1] + 1.645*att_PSM_log_salary_sa20[2],a_log_salary_cs$overall.att + 1.645*a_log_salary_cs$overall.se
                                   ),
                                   down = c(att_PSM_log_salary_sa20[1] - 1.645*att_PSM_log_salary_sa20[2],a_log_salary_cs$overall.att - 1.645*a_log_salary_cs$overall.se
                                   ),
                                   up95 = c(att_PSM_log_salary_sa20[1] + 1.960*att_PSM_log_salary_sa20[2],a_log_salary_cs$overall.att + 1.960*a_log_salary_cs$overall.se
                                   ),
                                   down95 = c(att_PSM_log_salary_sa20[1] - 1.960*att_PSM_log_salary_sa20[2],a_log_salary_cs$overall.att - 1.960*a_log_salary_cs$overall.se
                                   ),
                                   up99 = c(att_PSM_log_salary_sa20[1] + 2.5758*att_PSM_log_salary_sa20[2],a_log_salary_cs$overall.att + 2.5758*a_log_salary_cs$overall.se
                                   ),
                                   down99 = c(att_PSM_log_salary_sa20[1] - 2.5758*att_PSM_log_salary_sa20[2],a_log_salary_cs$overall.att - 2.5758*a_log_salary_cs$overall.se
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  log_salary_summary_PDP <- data.frame(Category = "asset", Variable = "log_salary", 
                                       ATT_SA = paste(round(log_salary_summary[1,2],3), " (", round(log_salary_summary[1,3],3), ") ",log_salary_summary[1,"result"],
                                                      sep =""),
                                       PT_SA = ifelse(log_salary_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                       ATT_CS = paste(round(log_salary_summary[2,2],3), " (", round(log_salary_summary[2,3],3), ") ",log_salary_summary[2,"result"],
                                                      sep =""),
                                       PT_CS = ifelse(log_salary_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_log_salary_summary <- ggplot(log_salary_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_salary_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_salary_summary$pretrend[1]) +
    geom_rect(data=log_salary_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_salary_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_salary_summary$down, na.rm=T),max(log_salary_summary$up, na.rm=T)), max(-min(log_salary_summary$down, na.rm=T),max(log_salary_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_salary <- p_PSM_log_salary_sa20+p_log_salary_cs+p_log_salary_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_log_salary)
  
  ################## log_benefit------------
  # ipw
  es_PSM_log_benefit_sa20 = feols(log_benefit ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_log_benefit_sa20 <- aggregate(es_PSM_log_benefit_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_benefit_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_benefit_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_benefit", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_benefit <- att_gt(yname = "log_benefit",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                           xformla = formula_cs, 
                           est_method = "ipw",base_period="universal",alp=0.05,
                           data = dfm_kikatsu2_ipw,
                           anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_benefit_smry <- aggte(cs_log_benefit, type = "dynamic",na.rm = TRUE)
  
  p_log_benefit_cs <- ggdid(cs_log_benefit_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_log_benefit_cs <- aggte(cs_log_benefit, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  log_benefit_summary <- data.frame(method = method,
                                    estimate = c(att_PSM_log_benefit_sa20[1], a_log_benefit_cs$overall.att
                                    ),
                                    se = c(att_PSM_log_benefit_sa20[2],a_log_benefit_cs$overall.se
                                    ),
                                    up = c(att_PSM_log_benefit_sa20[1] + 1.645*att_PSM_log_benefit_sa20[2],a_log_benefit_cs$overall.att + 1.645*a_log_benefit_cs$overall.se
                                    ),
                                    down = c(att_PSM_log_benefit_sa20[1] - 1.645*att_PSM_log_benefit_sa20[2],a_log_benefit_cs$overall.att - 1.645*a_log_benefit_cs$overall.se
                                    ),
                                    up95 = c(att_PSM_log_benefit_sa20[1] + 1.960*att_PSM_log_benefit_sa20[2],a_log_benefit_cs$overall.att + 1.960*a_log_benefit_cs$overall.se
                                    ),
                                    down95 = c(att_PSM_log_benefit_sa20[1] - 1.960*att_PSM_log_benefit_sa20[2],a_log_benefit_cs$overall.att - 1.960*a_log_benefit_cs$overall.se
                                    ),
                                    up99 = c(att_PSM_log_benefit_sa20[1] + 2.5758*att_PSM_log_benefit_sa20[2],a_log_benefit_cs$overall.att + 2.5758*a_log_benefit_cs$overall.se
                                    ),
                                    down99 = c(att_PSM_log_benefit_sa20[1] - 2.5758*att_PSM_log_benefit_sa20[2],a_log_benefit_cs$overall.att - 2.5758*a_log_benefit_cs$overall.se
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  log_benefit_summary_PDP <- data.frame(Category = "asset", Variable = "log_benefit", 
                                        ATT_SA = paste(round(log_benefit_summary[1,2],3), " (", round(log_benefit_summary[1,3],3), ") ",log_benefit_summary[1,"result"],
                                                       sep =""),
                                        PT_SA = ifelse(log_benefit_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                        ATT_CS = paste(round(log_benefit_summary[2,2],3), " (", round(log_benefit_summary[2,3],3), ") ",log_benefit_summary[2,"result"],
                                                       sep =""),
                                        PT_CS = ifelse(log_benefit_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_log_benefit_summary <- ggplot(log_benefit_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_benefit_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_benefit_summary$pretrend[1]) +
    geom_rect(data=log_benefit_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_benefit_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_benefit_summary$down, na.rm=T),max(log_benefit_summary$up, na.rm=T)), max(-min(log_benefit_summary$down, na.rm=T),max(log_benefit_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_benefit <- p_PSM_log_benefit_sa20+p_log_benefit_cs+p_log_benefit_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_log_benefit)
  
  ## output_employment-----------
  pa_employment_sum <- pa_log_workers/pa_log_indefinite_workers/pa_log_fixedterm_workers/pa_log_fixedterm_workers_equivalent/pa_log_salary/pa_log_benefit + plot_annotation(
    title = "Effect on the Employment",
    subtitle = target
  ) &
    theme(text = element_text(family="Meiryo UI"))
  
  filename_pdf <- paste("02_analysis_output/", target,"_02employment_", count_intervention, ".pdf", sep="")
  ggsave(filename_pdf,pa_employment_sum, 
         width=15, height =36, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("02_analysis_output/", target,"_02employment_", count_intervention, ".png", sep="")
  ggsave(filename_png,pa_employment_sum, 
         width=15, height =36, units = "cm", dpi=200) 
  
  write.xlsx(list("log_workers"=log_workers_summary,"log_indefinite_workers"=log_indefinite_workers_summary,"log_fixedterm_workers"=log_fixedterm_workers_summary,"log_fixedterm_workers_eq"=log_fixedterm_workers_equivalent_summary,"log_salary"=log_salary_summary,"log_benefit"=log_benefit_summary), paste("02_analysis_output/",target,"_02employment_table.xlsx", sep=""))
  write.xlsx(list("log_workers"=log_workers_summary_appendix,"log_indefinite_workers"=log_indefinite_workers_summary_appendix,"log_fixedterm_workers"=log_fixedterm_workers_summary_appendix,"log_fixedterm_workers_eq"=log_fixedterm_workers_equivalent_summary_appendix,"log_salary"=log_salary_summary_appendix,"log_benefit"=log_benefit_summary_appendix), paste("02_analysis_output/",target,"_02employment_table_appendix.xlsx", sep=""))
  
  
  # まとめ
  summary_PDP <- rbind(log_workers_summary_PDP,log_indefinite_workers_summary_PDP,log_fixedterm_workers_summary_PDP,log_fixedterm_workers_equivalent_summary_PDP,log_salary_summary_PDP,log_benefit_summary_PDP) %>%
    mutate(Category = "employment")
  write.xlsx(list("result"= summary_PDP), paste("02_analysis_output/",target,"_02employment_table_PDP.xlsx"))
  
}
analysis_business <- function(target){
  ################## log_sales------------
  # ipw
  es_PSM_log_sales_sa20 = feols(log_sales ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_log_sales_sa20 <- aggregate(es_PSM_log_sales_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_sales_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_sales_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_sales", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_sales <- att_gt(yname = "log_sales",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                         xformla = formula_cs, 
                         est_method = "ipw",base_period="universal",alp=0.05,
                         data = dfm_kikatsu2_ipw,
                         anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_sales_smry <- aggte(cs_log_sales, type = "dynamic",na.rm = TRUE)
  
  p_log_sales_cs <- ggdid(cs_log_sales_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_log_sales_cs <- aggte(cs_log_sales, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  log_sales_summary <- data.frame(method = method,
                                  estimate = c(att_PSM_log_sales_sa20[1], a_log_sales_cs$overall.att
                                  ),
                                  se = c(att_PSM_log_sales_sa20[2],a_log_sales_cs$overall.se
                                  ),
                                  up = c(att_PSM_log_sales_sa20[1] + 1.645*att_PSM_log_sales_sa20[2],a_log_sales_cs$overall.att + 1.645*a_log_sales_cs$overall.se
                                  ),
                                  down = c(att_PSM_log_sales_sa20[1] - 1.645*att_PSM_log_sales_sa20[2],a_log_sales_cs$overall.att - 1.645*a_log_sales_cs$overall.se
                                  ),
                                  up95 = c(att_PSM_log_sales_sa20[1] + 1.960*att_PSM_log_sales_sa20[2],a_log_sales_cs$overall.att + 1.960*a_log_sales_cs$overall.se
                                  ),
                                  down95 = c(att_PSM_log_sales_sa20[1] - 1.960*att_PSM_log_sales_sa20[2],a_log_sales_cs$overall.att - 1.960*a_log_sales_cs$overall.se
                                  ),
                                  up99 = c(att_PSM_log_sales_sa20[1] + 2.5758*att_PSM_log_sales_sa20[2],a_log_sales_cs$overall.att + 2.5758*a_log_sales_cs$overall.se
                                  ),
                                  down99 = c(att_PSM_log_sales_sa20[1] - 2.5758*att_PSM_log_sales_sa20[2],a_log_sales_cs$overall.att - 2.5758*a_log_sales_cs$overall.se
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  log_sales_summary_PDP <- data.frame(Category = "asset", Variable = "log_sales", 
                                      ATT_SA = paste(round(log_sales_summary[1,2],3), " (", round(log_sales_summary[1,3],3), ") ",log_sales_summary[1,"result"],
                                                     sep =""),
                                      PT_SA = ifelse(log_sales_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                      ATT_CS = paste(round(log_sales_summary[2,2],3), " (", round(log_sales_summary[2,3],3), ") ",log_sales_summary[2,"result"],
                                                     sep =""),
                                      PT_CS = ifelse(log_sales_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_log_sales_summary <- ggplot(log_sales_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_sales_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_sales_summary$pretrend[1]) +
    geom_rect(data=log_sales_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_sales_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_sales_summary$down, na.rm=T),max(log_sales_summary$up, na.rm=T)), max(-min(log_sales_summary$down, na.rm=T),max(log_sales_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_sales <- p_PSM_log_sales_sa20+p_log_sales_cs+p_log_sales_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_log_sales)
  
  ################## log_tax------------
  # ipw
  es_PSM_log_tax_sa20 = feols(log_tax ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_log_tax_sa20 <- aggregate(es_PSM_log_tax_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_tax_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_tax_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_tax", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_tax <- att_gt(yname = "log_tax",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                       xformla = formula_cs, 
                       est_method = "ipw",base_period="universal",alp=0.05,
                       data = dfm_kikatsu2_ipw,
                       anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_tax_smry <- aggte(cs_log_tax, type = "dynamic",na.rm = TRUE)
  
  p_log_tax_cs <- ggdid(cs_log_tax_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_log_tax_cs <- aggte(cs_log_tax, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  log_tax_summary <- data.frame(method = method,
                                estimate = c(att_PSM_log_tax_sa20[1], a_log_tax_cs$overall.att
                                ),
                                se = c(att_PSM_log_tax_sa20[2],a_log_tax_cs$overall.se
                                ),
                                up = c(att_PSM_log_tax_sa20[1] + 1.645*att_PSM_log_tax_sa20[2],a_log_tax_cs$overall.att + 1.645*a_log_tax_cs$overall.se
                                ),
                                down = c(att_PSM_log_tax_sa20[1] - 1.645*att_PSM_log_tax_sa20[2],a_log_tax_cs$overall.att - 1.645*a_log_tax_cs$overall.se
                                ),
                                up95 = c(att_PSM_log_tax_sa20[1] + 1.960*att_PSM_log_tax_sa20[2],a_log_tax_cs$overall.att + 1.960*a_log_tax_cs$overall.se
                                ),
                                down95 = c(att_PSM_log_tax_sa20[1] - 1.960*att_PSM_log_tax_sa20[2],a_log_tax_cs$overall.att - 1.960*a_log_tax_cs$overall.se
                                ),
                                up99 = c(att_PSM_log_tax_sa20[1] + 2.5758*att_PSM_log_tax_sa20[2],a_log_tax_cs$overall.att + 2.5758*a_log_tax_cs$overall.se
                                ),
                                down99 = c(att_PSM_log_tax_sa20[1] - 2.5758*att_PSM_log_tax_sa20[2],a_log_tax_cs$overall.att - 2.5758*a_log_tax_cs$overall.se
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  log_tax_summary_PDP <- data.frame(Category = "asset", Variable = "log_tax", 
                                    ATT_SA = paste(round(log_tax_summary[1,2],3), " (", round(log_tax_summary[1,3],3), ") ",log_tax_summary[1,"result"],
                                                   sep =""),
                                    PT_SA = ifelse(log_tax_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                    ATT_CS = paste(round(log_tax_summary[2,2],3), " (", round(log_tax_summary[2,3],3), ") ",log_tax_summary[2,"result"],
                                                   sep =""),
                                    PT_CS = ifelse(log_tax_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_log_tax_summary <- ggplot(log_tax_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_tax_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_tax_summary$pretrend[1]) +
    geom_rect(data=log_tax_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_tax_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_tax_summary$down, na.rm=T),max(log_tax_summary$up, na.rm=T)), max(-min(log_tax_summary$down, na.rm=T),max(log_tax_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_tax <- p_PSM_log_tax_sa20+p_log_tax_cs+p_log_tax_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_log_tax)
  
  ################## log_office------------
  # ipw
  es_PSM_log_office_sa20 = feols(log_office ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_log_office_sa20 <- aggregate(es_PSM_log_office_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_office_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_office_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_office", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_office <- att_gt(yname = "log_office",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                          xformla = formula_cs, 
                          est_method = "ipw",base_period="universal",alp=0.05,
                          data = dfm_kikatsu2_ipw,
                          anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_office_smry <- aggte(cs_log_office, type = "dynamic",na.rm = TRUE)
  
  p_log_office_cs <- ggdid(cs_log_office_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_log_office_cs <- aggte(cs_log_office, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  log_office_summary <- data.frame(method = method,
                                   estimate = c(att_PSM_log_office_sa20[1], a_log_office_cs$overall.att
                                   ),
                                   se = c(att_PSM_log_office_sa20[2],a_log_office_cs$overall.se
                                   ),
                                   up = c(att_PSM_log_office_sa20[1] + 1.645*att_PSM_log_office_sa20[2],a_log_office_cs$overall.att + 1.645*a_log_office_cs$overall.se
                                   ),
                                   down = c(att_PSM_log_office_sa20[1] - 1.645*att_PSM_log_office_sa20[2],a_log_office_cs$overall.att - 1.645*a_log_office_cs$overall.se
                                   ),
                                   up95 = c(att_PSM_log_office_sa20[1] + 1.960*att_PSM_log_office_sa20[2],a_log_office_cs$overall.att + 1.960*a_log_office_cs$overall.se
                                   ),
                                   down95 = c(att_PSM_log_office_sa20[1] - 1.960*att_PSM_log_office_sa20[2],a_log_office_cs$overall.att - 1.960*a_log_office_cs$overall.se
                                   ),
                                   up99 = c(att_PSM_log_office_sa20[1] + 2.5758*att_PSM_log_office_sa20[2],a_log_office_cs$overall.att + 2.5758*a_log_office_cs$overall.se
                                   ),
                                   down99 = c(att_PSM_log_office_sa20[1] - 2.5758*att_PSM_log_office_sa20[2],a_log_office_cs$overall.att - 2.5758*a_log_office_cs$overall.se
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  log_office_summary_PDP <- data.frame(Category = "asset", Variable = "log_office", 
                                       ATT_SA = paste(round(log_office_summary[1,2],3), " (", round(log_office_summary[1,3],3), ") ",log_office_summary[1,"result"],
                                                      sep =""),
                                       PT_SA = ifelse(log_office_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                       ATT_CS = paste(round(log_office_summary[2,2],3), " (", round(log_office_summary[2,3],3), ") ",log_office_summary[2,"result"],
                                                      sep =""),
                                       PT_CS = ifelse(log_office_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_log_office_summary <- ggplot(log_office_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_office_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_office_summary$pretrend[1]) +
    geom_rect(data=log_office_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_office_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_office_summary$down, na.rm=T),max(log_office_summary$up, na.rm=T)), max(-min(log_office_summary$down, na.rm=T),max(log_office_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_office <- p_PSM_log_office_sa20+p_log_office_cs+p_log_office_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_log_office)
  
  ################## ROA------------
  # ipw
  es_PSM_ROA_sa20 = feols(ROA ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_ROA_sa20 <- aggregate(es_PSM_ROA_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_ROA_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_ROA_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14),ylim = c(-0.1, 0.1)) + 
    labs(title = "ROA", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_ROA <- att_gt(yname = "ROA",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                   xformla = formula_cs, 
                   est_method = "ipw",base_period="universal",alp=0.05,
                   data = dfm_kikatsu2_ipw,
                   anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_ROA_smry <- aggte(cs_ROA, type = "dynamic",na.rm = TRUE)
  
  p_ROA_cs <- ggdid(cs_ROA_smry)+coord_cartesian(xlim = c(-10,14),ylim = c(-0.1, 0.1))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_ROA_cs <- aggte(cs_ROA, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  ROA_summary <- data.frame(method = method,
                            estimate = c(att_PSM_ROA_sa20[1], a_ROA_cs$overall.att
                            ),
                            se = c(att_PSM_ROA_sa20[2],a_ROA_cs$overall.se
                            ),
                            up = c(att_PSM_ROA_sa20[1] + 1.645*att_PSM_ROA_sa20[2],a_ROA_cs$overall.att + 1.645*a_ROA_cs$overall.se
                            ),
                            down = c(att_PSM_ROA_sa20[1] - 1.645*att_PSM_ROA_sa20[2],a_ROA_cs$overall.att - 1.645*a_ROA_cs$overall.se
                            ),
                            up95 = c(att_PSM_ROA_sa20[1] + 1.960*att_PSM_ROA_sa20[2],a_ROA_cs$overall.att + 1.960*a_ROA_cs$overall.se
                            ),
                            down95 = c(att_PSM_ROA_sa20[1] - 1.960*att_PSM_ROA_sa20[2],a_ROA_cs$overall.att - 1.960*a_ROA_cs$overall.se
                            ),
                            up99 = c(att_PSM_ROA_sa20[1] + 2.5758*att_PSM_ROA_sa20[2],a_ROA_cs$overall.att + 2.5758*a_ROA_cs$overall.se
                            ),
                            down99 = c(att_PSM_ROA_sa20[1] - 2.5758*att_PSM_ROA_sa20[2],a_ROA_cs$overall.att - 2.5758*a_ROA_cs$overall.se
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  ROA_summary_PDP <- data.frame(Category = "asset", Variable = "ROA", 
                                ATT_SA = paste(round(ROA_summary[1,2],3), " (", round(ROA_summary[1,3],3), ") ",ROA_summary[1,"result"],
                                               sep =""),
                                PT_SA = ifelse(ROA_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                ATT_CS = paste(round(ROA_summary[2,2],3), " (", round(ROA_summary[2,3],3), ") ",ROA_summary[2,"result"],
                                               sep =""),
                                PT_CS = ifelse(ROA_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_ROA_summary <- ggplot(ROA_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=ROA_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*ROA_summary$pretrend[1]) +
    geom_rect(data=ROA_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*ROA_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(ROA_summary$down, na.rm=T),max(ROA_summary$up, na.rm=T)), max(-min(ROA_summary$down, na.rm=T),max(ROA_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_ROA <- p_PSM_ROA_sa20+p_ROA_cs+p_ROA_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_ROA)
  
  ################## net_profit_workers------------
  # ipw
  es_PSM_net_profit_workers_sa20 = feols(net_profit_workers ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_net_profit_workers_sa20 <- aggregate(es_PSM_net_profit_workers_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_net_profit_workers_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_net_profit_workers_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14),ylim = c(-5000000, 5000000)) + 
    labs(title = "net_profit_workers", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_net_profit_workers <- att_gt(yname = "net_profit_workers",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                                  xformla = formula_cs, 
                                  est_method = "ipw",base_period="universal",alp=0.05,
                                  data = dfm_kikatsu2_ipw,
                                  anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_net_profit_workers_smry <- aggte(cs_net_profit_workers, type = "dynamic",na.rm = TRUE)
  
  p_net_profit_workers_cs <- ggdid(cs_net_profit_workers_smry)+coord_cartesian(xlim = c(-10,14),ylim = c(-5000000, 5000000))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_net_profit_workers_cs <- aggte(cs_net_profit_workers, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  net_profit_workers_summary <- data.frame(method = method,
                                           estimate = c(att_PSM_net_profit_workers_sa20[1], a_net_profit_workers_cs$overall.att
                                           ),
                                           se = c(att_PSM_net_profit_workers_sa20[2],a_net_profit_workers_cs$overall.se
                                           ),
                                           up = c(att_PSM_net_profit_workers_sa20[1] + 1.645*att_PSM_net_profit_workers_sa20[2],a_net_profit_workers_cs$overall.att + 1.645*a_net_profit_workers_cs$overall.se
                                           ),
                                           down = c(att_PSM_net_profit_workers_sa20[1] - 1.645*att_PSM_net_profit_workers_sa20[2],a_net_profit_workers_cs$overall.att - 1.645*a_net_profit_workers_cs$overall.se
                                           ),
                                           up95 = c(att_PSM_net_profit_workers_sa20[1] + 1.960*att_PSM_net_profit_workers_sa20[2],a_net_profit_workers_cs$overall.att + 1.960*a_net_profit_workers_cs$overall.se
                                           ),
                                           down95 = c(att_PSM_net_profit_workers_sa20[1] - 1.960*att_PSM_net_profit_workers_sa20[2],a_net_profit_workers_cs$overall.att - 1.960*a_net_profit_workers_cs$overall.se
                                           ),
                                           up99 = c(att_PSM_net_profit_workers_sa20[1] + 2.5758*att_PSM_net_profit_workers_sa20[2],a_net_profit_workers_cs$overall.att + 2.5758*a_net_profit_workers_cs$overall.se
                                           ),
                                           down99 = c(att_PSM_net_profit_workers_sa20[1] - 2.5758*att_PSM_net_profit_workers_sa20[2],a_net_profit_workers_cs$overall.att - 2.5758*a_net_profit_workers_cs$overall.se
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  net_profit_workers_summary_PDP <- data.frame(Category = "asset", Variable = "net_profit_workers", 
                                               ATT_SA = paste(round(net_profit_workers_summary[1,2],3), " (", round(net_profit_workers_summary[1,3],3), ") ",net_profit_workers_summary[1,"result"],
                                                              sep =""),
                                               PT_SA = ifelse(net_profit_workers_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                               ATT_CS = paste(round(net_profit_workers_summary[2,2],3), " (", round(net_profit_workers_summary[2,3],3), ") ",net_profit_workers_summary[2,"result"],
                                                              sep =""),
                                               PT_CS = ifelse(net_profit_workers_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_net_profit_workers_summary <- ggplot(net_profit_workers_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=net_profit_workers_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*net_profit_workers_summary$pretrend[1]) +
    geom_rect(data=net_profit_workers_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*net_profit_workers_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(net_profit_workers_summary$down, na.rm=T),max(net_profit_workers_summary$up, na.rm=T)), max(-min(net_profit_workers_summary$down, na.rm=T),max(net_profit_workers_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_net_profit_workers <- p_PSM_net_profit_workers_sa20+p_net_profit_workers_cs+p_net_profit_workers_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_net_profit_workers)
  
  ## output_business-----------
  pa_business_sum <- pa_log_sales/pa_log_tax/pa_log_office/pa_ROA/pa_net_profit_workers + plot_annotation(
    title = "Effect on the Business",
    subtitle = target
  ) &
    theme(text = element_text(family="Meiryo UI"))
  
  filename_pdf <- paste("02_analysis_output/", target,"_03business_", count_intervention, ".pdf", sep="")
  ggsave(filename_pdf,pa_business_sum, 
         width=15, height =30, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("02_analysis_output/", target,"_03business_", count_intervention, ".png", sep="")
  ggsave(filename_png,pa_business_sum, 
         width=15, height =30, units = "cm", dpi=200) 
  
  write.xlsx(list("log_sales"=log_sales_summary,"log_tax"=log_tax_summary,"log_office"=log_office_summary,"ROA"=ROA_summary,"net_profit_workers"=net_profit_workers_summary), paste("02_analysis_output/",target,"_03business_table.xlsx", sep=""))
  write.xlsx(list("log_sales"=log_sales_summary_appendix,"log_tax"=log_tax_summary_appendix,"log_office"=log_office_summary_appendix,"ROA"=ROA_summary_appendix,"net_profit_workers"=net_profit_workers_summary_appendix), paste("02_analysis_output/",target,"_03business_table_appendix.xlsx", sep=""))
  
  
  # まとめ
  summary_PDP <- rbind(log_sales_summary_PDP,log_tax_summary_PDP,log_office_summary_PDP,ROA_summary_PDP,net_profit_workers_summary_PDP) %>%
    mutate(Category = "business")
  write.xlsx(list("result"= summary_PDP), paste("02_analysis_output/",target,"_03business_table_PDP.xlsx"))
  
}
analysis_trade <- function(target){
  ################## flag_export------------
  # ipw
  es_PSM_flag_export_sa20 = feols(flag_export ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_flag_export_sa20 <- aggregate(es_PSM_flag_export_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_flag_export_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_flag_export_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + 
    labs(title = "flag_export", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_flag_export <- att_gt(yname = "flag_export",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                           xformla = formula_cs, 
                           est_method = "ipw",base_period="universal",alp=0.05,
                           data = dfm_kikatsu2_ipw,
                           anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_flag_export_smry <- aggte(cs_flag_export, type = "dynamic",na.rm = TRUE)
  
  p_flag_export_cs <- ggdid(cs_flag_export_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_flag_export_cs <- aggte(cs_flag_export, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  flag_export_summary <- data.frame(method = method,
                                    estimate = c(att_PSM_flag_export_sa20[1], a_flag_export_cs$overall.att
                                    ),
                                    se = c(att_PSM_flag_export_sa20[2],a_flag_export_cs$overall.se
                                    ),
                                    up = c(att_PSM_flag_export_sa20[1] + 1.645*att_PSM_flag_export_sa20[2],a_flag_export_cs$overall.att + 1.645*a_flag_export_cs$overall.se
                                    ),
                                    down = c(att_PSM_flag_export_sa20[1] - 1.645*att_PSM_flag_export_sa20[2],a_flag_export_cs$overall.att - 1.645*a_flag_export_cs$overall.se
                                    ),
                                    up95 = c(att_PSM_flag_export_sa20[1] + 1.960*att_PSM_flag_export_sa20[2],a_flag_export_cs$overall.att + 1.960*a_flag_export_cs$overall.se
                                    ),
                                    down95 = c(att_PSM_flag_export_sa20[1] - 1.960*att_PSM_flag_export_sa20[2],a_flag_export_cs$overall.att - 1.960*a_flag_export_cs$overall.se
                                    ),
                                    up99 = c(att_PSM_flag_export_sa20[1] + 2.5758*att_PSM_flag_export_sa20[2],a_flag_export_cs$overall.att + 2.5758*a_flag_export_cs$overall.se
                                    ),
                                    down99 = c(att_PSM_flag_export_sa20[1] - 2.5758*att_PSM_flag_export_sa20[2],a_flag_export_cs$overall.att - 2.5758*a_flag_export_cs$overall.se
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  flag_export_summary_PDP <- data.frame(Category = "asset", Variable = "flag_export", 
                                        ATT_SA = paste(round(flag_export_summary[1,2],3), " (", round(flag_export_summary[1,3],3), ") ",flag_export_summary[1,"result"],
                                                       sep =""),
                                        PT_SA = ifelse(flag_export_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                        ATT_CS = paste(round(flag_export_summary[2,2],3), " (", round(flag_export_summary[2,3],3), ") ",flag_export_summary[2,"result"],
                                                       sep =""),
                                        PT_CS = ifelse(flag_export_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_flag_export_summary <- ggplot(flag_export_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=flag_export_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_export_summary$pretrend[1]) +
    geom_rect(data=flag_export_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_export_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(flag_export_summary$down, na.rm=T),max(flag_export_summary$up, na.rm=T)), max(-min(flag_export_summary$down, na.rm=T),max(flag_export_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_flag_export <- p_PSM_flag_export_sa20+p_flag_export_cs+p_flag_export_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_flag_export)
  
  ################## flag_import------------
  # ipw
  es_PSM_flag_import_sa20 = feols(flag_import ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_flag_import_sa20 <- aggregate(es_PSM_flag_import_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_flag_import_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_flag_import_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + 
    labs(title = "flag_import", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_flag_import <- att_gt(yname = "flag_import",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                           xformla = formula_cs, 
                           est_method = "ipw",base_period="universal",alp=0.05,
                           data = dfm_kikatsu2_ipw,
                           anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_flag_import_smry <- aggte(cs_flag_import, type = "dynamic",na.rm = TRUE)
  
  p_flag_import_cs <- ggdid(cs_flag_import_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_flag_import_cs <- aggte(cs_flag_import, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  flag_import_summary <- data.frame(method = method,
                                    estimate = c(att_PSM_flag_import_sa20[1], a_flag_import_cs$overall.att
                                    ),
                                    se = c(att_PSM_flag_import_sa20[2],a_flag_import_cs$overall.se
                                    ),
                                    up = c(att_PSM_flag_import_sa20[1] + 1.645*att_PSM_flag_import_sa20[2],a_flag_import_cs$overall.att + 1.645*a_flag_import_cs$overall.se
                                    ),
                                    down = c(att_PSM_flag_import_sa20[1] - 1.645*att_PSM_flag_import_sa20[2],a_flag_import_cs$overall.att - 1.645*a_flag_import_cs$overall.se
                                    ),
                                    up95 = c(att_PSM_flag_import_sa20[1] + 1.960*att_PSM_flag_import_sa20[2],a_flag_import_cs$overall.att + 1.960*a_flag_import_cs$overall.se
                                    ),
                                    down95 = c(att_PSM_flag_import_sa20[1] - 1.960*att_PSM_flag_import_sa20[2],a_flag_import_cs$overall.att - 1.960*a_flag_import_cs$overall.se
                                    ),
                                    up99 = c(att_PSM_flag_import_sa20[1] + 2.5758*att_PSM_flag_import_sa20[2],a_flag_import_cs$overall.att + 2.5758*a_flag_import_cs$overall.se
                                    ),
                                    down99 = c(att_PSM_flag_import_sa20[1] - 2.5758*att_PSM_flag_import_sa20[2],a_flag_import_cs$overall.att - 2.5758*a_flag_import_cs$overall.se
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  flag_import_summary_PDP <- data.frame(Category = "asset", Variable = "flag_import", 
                                        ATT_SA = paste(round(flag_import_summary[1,2],3), " (", round(flag_import_summary[1,3],3), ") ",flag_import_summary[1,"result"],
                                                       sep =""),
                                        PT_SA = ifelse(flag_import_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                        ATT_CS = paste(round(flag_import_summary[2,2],3), " (", round(flag_import_summary[2,3],3), ") ",flag_import_summary[2,"result"],
                                                       sep =""),
                                        PT_CS = ifelse(flag_import_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_flag_import_summary <- ggplot(flag_import_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=flag_import_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_import_summary$pretrend[1]) +
    geom_rect(data=flag_import_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_import_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(flag_import_summary$down, na.rm=T),max(flag_import_summary$up, na.rm=T)), max(-min(flag_import_summary$down, na.rm=T),max(flag_import_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_flag_import <- p_PSM_flag_import_sa20+p_flag_import_cs+p_flag_import_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_flag_import)
  
  ################## log_export------------
  # ipw
  es_PSM_log_export_sa20 = feols(log_export ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_log_export_sa20 <- aggregate(es_PSM_log_export_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_export_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_export_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_export", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_export <- att_gt(yname = "log_export",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                          xformla = formula_cs, 
                          est_method = "ipw",base_period="universal",alp=0.05,
                          data = dfm_kikatsu2_ipw,
                          anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_export_smry <- aggte(cs_log_export, type = "dynamic",na.rm = TRUE)
  
  p_log_export_cs <- ggdid(cs_log_export_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_log_export_cs <- aggte(cs_log_export, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  log_export_summary <- data.frame(method = method,
                                   estimate = c(att_PSM_log_export_sa20[1], a_log_export_cs$overall.att
                                   ),
                                   se = c(att_PSM_log_export_sa20[2],a_log_export_cs$overall.se
                                   ),
                                   up = c(att_PSM_log_export_sa20[1] + 1.645*att_PSM_log_export_sa20[2],a_log_export_cs$overall.att + 1.645*a_log_export_cs$overall.se
                                   ),
                                   down = c(att_PSM_log_export_sa20[1] - 1.645*att_PSM_log_export_sa20[2],a_log_export_cs$overall.att - 1.645*a_log_export_cs$overall.se
                                   ),
                                   up95 = c(att_PSM_log_export_sa20[1] + 1.960*att_PSM_log_export_sa20[2],a_log_export_cs$overall.att + 1.960*a_log_export_cs$overall.se
                                   ),
                                   down95 = c(att_PSM_log_export_sa20[1] - 1.960*att_PSM_log_export_sa20[2],a_log_export_cs$overall.att - 1.960*a_log_export_cs$overall.se
                                   ),
                                   up99 = c(att_PSM_log_export_sa20[1] + 2.5758*att_PSM_log_export_sa20[2],a_log_export_cs$overall.att + 2.5758*a_log_export_cs$overall.se
                                   ),
                                   down99 = c(att_PSM_log_export_sa20[1] - 2.5758*att_PSM_log_export_sa20[2],a_log_export_cs$overall.att - 2.5758*a_log_export_cs$overall.se
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  log_export_summary_PDP <- data.frame(Category = "asset", Variable = "log_export", 
                                       ATT_SA = paste(round(log_export_summary[1,2],3), " (", round(log_export_summary[1,3],3), ") ",log_export_summary[1,"result"],
                                                      sep =""),
                                       PT_SA = ifelse(log_export_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                       ATT_CS = paste(round(log_export_summary[2,2],3), " (", round(log_export_summary[2,3],3), ") ",log_export_summary[2,"result"],
                                                      sep =""),
                                       PT_CS = ifelse(log_export_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_log_export_summary <- ggplot(log_export_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_export_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_export_summary$pretrend[1]) +
    geom_rect(data=log_export_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_export_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_export_summary$down, na.rm=T),max(log_export_summary$up, na.rm=T)), max(-min(log_export_summary$down, na.rm=T),max(log_export_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_export <- p_PSM_log_export_sa20+p_log_export_cs+p_log_export_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_log_export)
  
  ################## log_import------------
  # ipw
  es_PSM_log_import_sa20 = feols(log_import ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_log_import_sa20 <- aggregate(es_PSM_log_import_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_import_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_import_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_import", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_import <- att_gt(yname = "log_import",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                          xformla = formula_cs, 
                          est_method = "ipw",base_period="universal",alp=0.05,
                          data = dfm_kikatsu2_ipw,
                          anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_import_smry <- aggte(cs_log_import, type = "dynamic",na.rm = TRUE)
  
  p_log_import_cs <- ggdid(cs_log_import_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_log_import_cs <- aggte(cs_log_import, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  log_import_summary <- data.frame(method = method,
                                   estimate = c(att_PSM_log_import_sa20[1], a_log_import_cs$overall.att
                                   ),
                                   se = c(att_PSM_log_import_sa20[2],a_log_import_cs$overall.se
                                   ),
                                   up = c(att_PSM_log_import_sa20[1] + 1.645*att_PSM_log_import_sa20[2],a_log_import_cs$overall.att + 1.645*a_log_import_cs$overall.se
                                   ),
                                   down = c(att_PSM_log_import_sa20[1] - 1.645*att_PSM_log_import_sa20[2],a_log_import_cs$overall.att - 1.645*a_log_import_cs$overall.se
                                   ),
                                   up95 = c(att_PSM_log_import_sa20[1] + 1.960*att_PSM_log_import_sa20[2],a_log_import_cs$overall.att + 1.960*a_log_import_cs$overall.se
                                   ),
                                   down95 = c(att_PSM_log_import_sa20[1] - 1.960*att_PSM_log_import_sa20[2],a_log_import_cs$overall.att - 1.960*a_log_import_cs$overall.se
                                   ),
                                   up99 = c(att_PSM_log_import_sa20[1] + 2.5758*att_PSM_log_import_sa20[2],a_log_import_cs$overall.att + 2.5758*a_log_import_cs$overall.se
                                   ),
                                   down99 = c(att_PSM_log_import_sa20[1] - 2.5758*att_PSM_log_import_sa20[2],a_log_import_cs$overall.att - 2.5758*a_log_import_cs$overall.se
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  log_import_summary_PDP <- data.frame(Category = "asset", Variable = "log_import", 
                                       ATT_SA = paste(round(log_import_summary[1,2],3), " (", round(log_import_summary[1,3],3), ") ",log_import_summary[1,"result"],
                                                      sep =""),
                                       PT_SA = ifelse(log_import_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                       ATT_CS = paste(round(log_import_summary[2,2],3), " (", round(log_import_summary[2,3],3), ") ",log_import_summary[2,"result"],
                                                      sep =""),
                                       PT_CS = ifelse(log_import_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_log_import_summary <- ggplot(log_import_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_import_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_import_summary$pretrend[1]) +
    geom_rect(data=log_import_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_import_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_import_summary$down, na.rm=T),max(log_import_summary$up, na.rm=T)), max(-min(log_import_summary$down, na.rm=T),max(log_import_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_import <- p_PSM_log_import_sa20+p_log_import_cs+p_log_import_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_log_import)
  
  ## output_trade-----------
  pa_trade_sum <- pa_flag_export/pa_flag_import/pa_log_export/pa_log_import + plot_annotation(
    title = "Effect on the International Trade",
    subtitle = target
  ) &
    theme(text = element_text(family="Meiryo UI"))
  
  filename_pdf <- paste("02_analysis_output/", target,"_04trade_", count_intervention, ".pdf", sep="")
  ggsave(filename_pdf,pa_trade_sum, 
         width=15, height =24, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("02_analysis_output/", target,"_04trade_", count_intervention, ".png", sep="")
  ggsave(filename_png,pa_trade_sum, 
         width=15, height =24, units = "cm", dpi=200) 
  
  write.xlsx(list("flag_export"=flag_export_summary,"flag_import"=flag_import_summary,"log_export"=log_export_summary,"log_import"=log_import_summary), paste("02_analysis_output/",target,"_04trade_table.xlsx", sep=""))
  write.xlsx(list("flag_export"=flag_export_summary_appendix,"flag_import"=flag_import_summary_appendix,"log_export"=log_export_summary_appendix,"log_import"=log_import_summary_appendix), paste("02_analysis_output/",target,"_04trade_table_appendix.xlsx", sep=""))
  
  
  # まとめ
  summary_PDP <- rbind(flag_export_summary_PDP,flag_import_summary_PDP,log_export_summary_PDP,log_import_summary_PDP) %>%
    mutate(Category = "trade")
  write.xlsx(list("result"= summary_PDP), paste("02_analysis_output/",target,"_04trade_table_PDP.xlsx"))
  
}
analysis_trainingRD <- function(target){
  ################## flag_training------------
  # ipw
  es_PSM_flag_training_sa20 = feols(flag_training ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_flag_training_sa20 <- aggregate(es_PSM_flag_training_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_flag_training_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_flag_training_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + 
    labs(title = "flag_training", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_flag_training <- att_gt(yname = "flag_training",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                             xformla = formula_cs, 
                             est_method = "ipw",base_period="universal",alp=0.05,
                             data = dfm_kikatsu2_ipw,
                             anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_flag_training_smry <- aggte(cs_flag_training, type = "dynamic",na.rm = TRUE)
  
  p_flag_training_cs <- ggdid(cs_flag_training_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_flag_training_cs <- aggte(cs_flag_training, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  flag_training_summary <- data.frame(method = method,
                                      estimate = c(att_PSM_flag_training_sa20[1], a_flag_training_cs$overall.att
                                      ),
                                      se = c(att_PSM_flag_training_sa20[2],a_flag_training_cs$overall.se
                                      ),
                                      up = c(att_PSM_flag_training_sa20[1] + 1.645*att_PSM_flag_training_sa20[2],a_flag_training_cs$overall.att + 1.645*a_flag_training_cs$overall.se
                                      ),
                                      down = c(att_PSM_flag_training_sa20[1] - 1.645*att_PSM_flag_training_sa20[2],a_flag_training_cs$overall.att - 1.645*a_flag_training_cs$overall.se
                                      ),
                                      up95 = c(att_PSM_flag_training_sa20[1] + 1.960*att_PSM_flag_training_sa20[2],a_flag_training_cs$overall.att + 1.960*a_flag_training_cs$overall.se
                                      ),
                                      down95 = c(att_PSM_flag_training_sa20[1] - 1.960*att_PSM_flag_training_sa20[2],a_flag_training_cs$overall.att - 1.960*a_flag_training_cs$overall.se
                                      ),
                                      up99 = c(att_PSM_flag_training_sa20[1] + 2.5758*att_PSM_flag_training_sa20[2],a_flag_training_cs$overall.att + 2.5758*a_flag_training_cs$overall.se
                                      ),
                                      down99 = c(att_PSM_flag_training_sa20[1] - 2.5758*att_PSM_flag_training_sa20[2],a_flag_training_cs$overall.att - 2.5758*a_flag_training_cs$overall.se
                                      )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  
  
  # create period specific att table
  flag_training_summary_appendix_sa <- data.frame(year = es_PSM_flag_training_sa20$coeftable[,0])%>%
    #    filter(year != as.numeric(es_PSM_flag_training_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_flag_training_sa20$coeftable[,1],
           sa_se =es_PSM_flag_training_sa20$coeftable[,2],
           sa_t =es_PSM_flag_training_sa20$coeftable[,3],
           sa_p =es_PSM_flag_training_sa20$coeftable[,4])
  flag_training_summary_appendix_cs <- data.frame(year = cs_flag_training_smry$egt,
                                                  cs =  cs_flag_training_smry$att.egt,
                                                  cs_se = cs_flag_training_smry$se.egt,
                                                  cs_cband_lower = cs_flag_training_smry$att.egt - cs_flag_training_smry$crit.val.egt*cs_flag_training_smry$se.egt,
                                                  cs_cband_upper = cs_flag_training_smry$att.egt + cs_flag_training_smry$crit.val.egt*cs_flag_training_smry$se.egt) %>%
    filter(cs != 0)
  flag_training_summary_appendix <- cbind(flag_training_summary_appendix_sa,flag_training_summary_appendix_cs#,  by = c("year")
  ) %>%
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  flag_training_summary_PDP <- data.frame(Category = "asset", Variable = "flag_training", 
                                          ATT_SA = paste(round(flag_training_summary[1,2],3), " (", round(flag_training_summary[1,3],3), ") ",flag_training_summary[1,"result"],
                                                         sep =""),
                                          PT_SA = ifelse(flag_training_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                          ATT_CS = paste(round(flag_training_summary[2,2],3), " (", round(flag_training_summary[2,3],3), ") ",flag_training_summary[2,"result"],
                                                         sep =""),
                                          PT_CS = ifelse(flag_training_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_flag_training_summary <- ggplot(flag_training_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=flag_training_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_training_summary$pretrend[1]) +
    geom_rect(data=flag_training_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_training_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(flag_training_summary$down, na.rm=T),max(flag_training_summary$up, na.rm=T)), max(-min(flag_training_summary$down, na.rm=T),max(flag_training_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_flag_training <- p_PSM_flag_training_sa20+p_flag_training_cs+p_flag_training_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_flag_training)
  
  ################## flag_RD------------
  # ipw
  es_PSM_flag_RD_sa20 = feols(flag_RD ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_flag_RD_sa20 <- aggregate(es_PSM_flag_RD_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_flag_RD_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_flag_RD_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + 
    labs(title = "flag_RD", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_flag_RD <- att_gt(yname = "flag_RD",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                       xformla = formula_cs, 
                       est_method = "ipw",base_period="universal",alp=0.05,
                       data = dfm_kikatsu2_ipw,
                       anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_flag_RD_smry <- aggte(cs_flag_RD, type = "dynamic",na.rm = TRUE)
  
  p_flag_RD_cs <- ggdid(cs_flag_RD_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_flag_RD_cs <- aggte(cs_flag_RD, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  flag_RD_summary <- data.frame(method = method,
                                estimate = c(att_PSM_flag_RD_sa20[1], a_flag_RD_cs$overall.att
                                ),
                                se = c(att_PSM_flag_RD_sa20[2],a_flag_RD_cs$overall.se
                                ),
                                up = c(att_PSM_flag_RD_sa20[1] + 1.645*att_PSM_flag_RD_sa20[2],a_flag_RD_cs$overall.att + 1.645*a_flag_RD_cs$overall.se
                                ),
                                down = c(att_PSM_flag_RD_sa20[1] - 1.645*att_PSM_flag_RD_sa20[2],a_flag_RD_cs$overall.att - 1.645*a_flag_RD_cs$overall.se
                                ),
                                up95 = c(att_PSM_flag_RD_sa20[1] + 1.960*att_PSM_flag_RD_sa20[2],a_flag_RD_cs$overall.att + 1.960*a_flag_RD_cs$overall.se
                                ),
                                down95 = c(att_PSM_flag_RD_sa20[1] - 1.960*att_PSM_flag_RD_sa20[2],a_flag_RD_cs$overall.att - 1.960*a_flag_RD_cs$overall.se
                                ),
                                up99 = c(att_PSM_flag_RD_sa20[1] + 2.5758*att_PSM_flag_RD_sa20[2],a_flag_RD_cs$overall.att + 2.5758*a_flag_RD_cs$overall.se
                                ),
                                down99 = c(att_PSM_flag_RD_sa20[1] - 2.5758*att_PSM_flag_RD_sa20[2],a_flag_RD_cs$overall.att - 2.5758*a_flag_RD_cs$overall.se
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  flag_RD_summary_PDP <- data.frame(Category = "asset", Variable = "flag_RD", 
                                    ATT_SA = paste(round(flag_RD_summary[1,2],3), " (", round(flag_RD_summary[1,3],3), ") ",flag_RD_summary[1,"result"],
                                                   sep =""),
                                    PT_SA = ifelse(flag_RD_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                    ATT_CS = paste(round(flag_RD_summary[2,2],3), " (", round(flag_RD_summary[2,3],3), ") ",flag_RD_summary[2,"result"],
                                                   sep =""),
                                    PT_CS = ifelse(flag_RD_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_flag_RD_summary <- ggplot(flag_RD_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=flag_RD_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_RD_summary$pretrend[1]) +
    geom_rect(data=flag_RD_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_RD_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(flag_RD_summary$down, na.rm=T),max(flag_RD_summary$up, na.rm=T)), max(-min(flag_RD_summary$down, na.rm=T),max(flag_RD_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_flag_RD <- p_PSM_flag_RD_sa20+p_flag_RD_cs+p_flag_RD_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_flag_RD)
  
  ################## log_training------------
  # ipw
  es_PSM_log_training_sa20 = feols(log_training ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_log_training_sa20 <- aggregate(es_PSM_log_training_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_training_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_training_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_training", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_training <- att_gt(yname = "log_training",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                            xformla = formula_cs, 
                            est_method = "ipw",base_period="universal",alp=0.05,
                            data = dfm_kikatsu2_ipw,
                            anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_training_smry <- aggte(cs_log_training, type = "dynamic",na.rm = TRUE)
  
  p_log_training_cs <- ggdid(cs_log_training_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_log_training_cs <- aggte(cs_log_training, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  log_training_summary <- data.frame(method = method,
                                     estimate = c(att_PSM_log_training_sa20[1], a_log_training_cs$overall.att
                                     ),
                                     se = c(att_PSM_log_training_sa20[2],a_log_training_cs$overall.se
                                     ),
                                     up = c(att_PSM_log_training_sa20[1] + 1.645*att_PSM_log_training_sa20[2],a_log_training_cs$overall.att + 1.645*a_log_training_cs$overall.se
                                     ),
                                     down = c(att_PSM_log_training_sa20[1] - 1.645*att_PSM_log_training_sa20[2],a_log_training_cs$overall.att - 1.645*a_log_training_cs$overall.se
                                     ),
                                     up95 = c(att_PSM_log_training_sa20[1] + 1.960*att_PSM_log_training_sa20[2],a_log_training_cs$overall.att + 1.960*a_log_training_cs$overall.se
                                     ),
                                     down95 = c(att_PSM_log_training_sa20[1] - 1.960*att_PSM_log_training_sa20[2],a_log_training_cs$overall.att - 1.960*a_log_training_cs$overall.se
                                     ),
                                     up99 = c(att_PSM_log_training_sa20[1] + 2.5758*att_PSM_log_training_sa20[2],a_log_training_cs$overall.att + 2.5758*a_log_training_cs$overall.se
                                     ),
                                     down99 = c(att_PSM_log_training_sa20[1] - 2.5758*att_PSM_log_training_sa20[2],a_log_training_cs$overall.att - 2.5758*a_log_training_cs$overall.se
                                     )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  
  
  # create period specific att table
  log_training_summary_appendix_sa <- data.frame(year = es_PSM_log_training_sa20$coeftable[,0])%>%
    #    filter(year != as.numeric(es_PSM_log_training_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_log_training_sa20$coeftable[,1],
           sa_se =es_PSM_log_training_sa20$coeftable[,2],
           sa_t =es_PSM_log_training_sa20$coeftable[,3],
           sa_p =es_PSM_log_training_sa20$coeftable[,4])
  log_training_summary_appendix_cs <- data.frame(year = cs_log_training_smry$egt,
                                                 cs =  cs_log_training_smry$att.egt,
                                                 cs_se = cs_log_training_smry$se.egt,
                                                 cs_cband_lower = cs_log_training_smry$att.egt - cs_log_training_smry$crit.val.egt*cs_log_training_smry$se.egt,
                                                 cs_cband_upper = cs_log_training_smry$att.egt + cs_log_training_smry$crit.val.egt*cs_log_training_smry$se.egt) %>%
    filter(cs != 0)
  log_training_summary_appendix <- cbind(log_training_summary_appendix_sa,log_training_summary_appendix_cs#,  by = c("year")
  ) %>%
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  log_training_summary_PDP <- data.frame(Category = "asset", Variable = "log_training", 
                                         ATT_SA = paste(round(log_training_summary[1,2],3), " (", round(log_training_summary[1,3],3), ") ",log_training_summary[1,"result"],
                                                        sep =""),
                                         PT_SA = ifelse(log_training_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                         ATT_CS = paste(round(log_training_summary[2,2],3), " (", round(log_training_summary[2,3],3), ") ",log_training_summary[2,"result"],
                                                        sep =""),
                                         PT_CS = ifelse(log_training_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_log_training_summary <- ggplot(log_training_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_training_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_training_summary$pretrend[1]) +
    geom_rect(data=log_training_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_training_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_training_summary$down, na.rm=T),max(log_training_summary$up, na.rm=T)), max(-min(log_training_summary$down, na.rm=T),max(log_training_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_training <- p_PSM_log_training_sa20+p_log_training_cs+p_log_training_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_log_training)
  
  ################## log_RD------------
  # ipw
  es_PSM_log_RD_sa20 = feols(log_RD ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_log_RD_sa20 <- aggregate(es_PSM_log_RD_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_RD_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_RD_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_RD", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_RD <- att_gt(yname = "log_RD",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                      xformla = formula_cs, 
                      est_method = "ipw",base_period="universal",alp=0.05,
                      data = dfm_kikatsu2_ipw,
                      anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_RD_smry <- aggte(cs_log_RD, type = "dynamic",na.rm = TRUE)
  
  p_log_RD_cs <- ggdid(cs_log_RD_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_log_RD_cs <- aggte(cs_log_RD, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  log_RD_summary <- data.frame(method = method,
                               estimate = c(att_PSM_log_RD_sa20[1], a_log_RD_cs$overall.att
                               ),
                               se = c(att_PSM_log_RD_sa20[2],a_log_RD_cs$overall.se
                               ),
                               up = c(att_PSM_log_RD_sa20[1] + 1.645*att_PSM_log_RD_sa20[2],a_log_RD_cs$overall.att + 1.645*a_log_RD_cs$overall.se
                               ),
                               down = c(att_PSM_log_RD_sa20[1] - 1.645*att_PSM_log_RD_sa20[2],a_log_RD_cs$overall.att - 1.645*a_log_RD_cs$overall.se
                               ),
                               up95 = c(att_PSM_log_RD_sa20[1] + 1.960*att_PSM_log_RD_sa20[2],a_log_RD_cs$overall.att + 1.960*a_log_RD_cs$overall.se
                               ),
                               down95 = c(att_PSM_log_RD_sa20[1] - 1.960*att_PSM_log_RD_sa20[2],a_log_RD_cs$overall.att - 1.960*a_log_RD_cs$overall.se
                               ),
                               up99 = c(att_PSM_log_RD_sa20[1] + 2.5758*att_PSM_log_RD_sa20[2],a_log_RD_cs$overall.att + 2.5758*a_log_RD_cs$overall.se
                               ),
                               down99 = c(att_PSM_log_RD_sa20[1] - 2.5758*att_PSM_log_RD_sa20[2],a_log_RD_cs$overall.att - 2.5758*a_log_RD_cs$overall.se
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  log_RD_summary_PDP <- data.frame(Category = "asset", Variable = "log_RD", 
                                   ATT_SA = paste(round(log_RD_summary[1,2],3), " (", round(log_RD_summary[1,3],3), ") ",log_RD_summary[1,"result"],
                                                  sep =""),
                                   PT_SA = ifelse(log_RD_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                   ATT_CS = paste(round(log_RD_summary[2,2],3), " (", round(log_RD_summary[2,3],3), ") ",log_RD_summary[2,"result"],
                                                  sep =""),
                                   PT_CS = ifelse(log_RD_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_log_RD_summary <- ggplot(log_RD_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_RD_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_RD_summary$pretrend[1]) +
    geom_rect(data=log_RD_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_RD_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_RD_summary$down, na.rm=T),max(log_RD_summary$up, na.rm=T)), max(-min(log_RD_summary$down, na.rm=T),max(log_RD_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_RD <- p_PSM_log_RD_sa20+p_log_RD_cs+p_log_RD_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_log_RD)
  
  ## output_trainingRD-----------
  pa_trainingRD_sum <- pa_flag_training/pa_flag_RD/pa_log_training/pa_log_RD + plot_annotation(
    title = "Effect on the Training and RD",
    subtitle = target
  ) &
    theme(text = element_text(family="Meiryo UI"))
  
  filename_pdf <- paste("02_analysis_output/", target,"_05trainingRD_", count_intervention, ".pdf", sep="")
  ggsave(filename_pdf,pa_trainingRD_sum, 
         width=15, height =24, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("02_analysis_output/", target,"_05trainingRD_", count_intervention, ".png", sep="")
  ggsave(filename_png,pa_trainingRD_sum, 
         width=15, height =24, units = "cm", dpi=200) 
  
  write.xlsx(list("flag_training"=flag_training_summary,"flag_RD"=flag_RD_summary,"log_training"=log_training_summary,"log_RD"=log_RD_summary), paste("02_analysis_output/",target,"_05trainingRD_table.xlsx", sep=""))
  write.xlsx(list("flag_training"=flag_training_summary_appendix,"flag_RD"=flag_RD_summary_appendix,"log_training"=log_training_summary_appendix,"log_RD"=log_RD_summary_appendix), paste("02_analysis_output/",target,"_05trainingRD_table_appendix.xlsx", sep=""))
  
  
  # まとめ
  summary_PDP <- rbind(flag_training_summary_PDP,flag_RD_summary_PDP,log_training_summary_PDP,log_RD_summary_PDP) %>%
    mutate(Category = "trainingRD")
  write.xlsx(list("result"= summary_PDP), paste("02_analysis_output/",target,"_05trainingRD_table_PDP.xlsx"))
  
}
analysis_IP <- function(target){
  ################## flag_patent------------
  # ipw
  es_PSM_flag_patent_sa20 = feols(flag_patent ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_flag_patent_sa20 <- aggregate(es_PSM_flag_patent_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_flag_patent_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_flag_patent_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + 
    labs(title = "flag_patent", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_flag_patent <- att_gt(yname = "flag_patent",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                           xformla = formula_cs, 
                           est_method = "ipw",base_period="universal",alp=0.05,
                           data = dfm_kikatsu2_ipw,
                           anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_flag_patent_smry <- aggte(cs_flag_patent, type = "dynamic",na.rm = TRUE)
  
  p_flag_patent_cs <- ggdid(cs_flag_patent_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_flag_patent_cs <- aggte(cs_flag_patent, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  flag_patent_summary <- data.frame(method = method,
                                    estimate = c(att_PSM_flag_patent_sa20[1], a_flag_patent_cs$overall.att
                                    ),
                                    se = c(att_PSM_flag_patent_sa20[2],a_flag_patent_cs$overall.se
                                    ),
                                    up = c(att_PSM_flag_patent_sa20[1] + 1.645*att_PSM_flag_patent_sa20[2],a_flag_patent_cs$overall.att + 1.645*a_flag_patent_cs$overall.se
                                    ),
                                    down = c(att_PSM_flag_patent_sa20[1] - 1.645*att_PSM_flag_patent_sa20[2],a_flag_patent_cs$overall.att - 1.645*a_flag_patent_cs$overall.se
                                    ),
                                    up95 = c(att_PSM_flag_patent_sa20[1] + 1.960*att_PSM_flag_patent_sa20[2],a_flag_patent_cs$overall.att + 1.960*a_flag_patent_cs$overall.se
                                    ),
                                    down95 = c(att_PSM_flag_patent_sa20[1] - 1.960*att_PSM_flag_patent_sa20[2],a_flag_patent_cs$overall.att - 1.960*a_flag_patent_cs$overall.se
                                    ),
                                    up99 = c(att_PSM_flag_patent_sa20[1] + 2.5758*att_PSM_flag_patent_sa20[2],a_flag_patent_cs$overall.att + 2.5758*a_flag_patent_cs$overall.se
                                    ),
                                    down99 = c(att_PSM_flag_patent_sa20[1] - 2.5758*att_PSM_flag_patent_sa20[2],a_flag_patent_cs$overall.att - 2.5758*a_flag_patent_cs$overall.se
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  flag_patent_summary_PDP <- data.frame(Category = "asset", Variable = "flag_patent", 
                                        ATT_SA = paste(round(flag_patent_summary[1,2],3), " (", round(flag_patent_summary[1,3],3), ") ",flag_patent_summary[1,"result"],
                                                       sep =""),
                                        PT_SA = ifelse(flag_patent_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                        ATT_CS = paste(round(flag_patent_summary[2,2],3), " (", round(flag_patent_summary[2,3],3), ") ",flag_patent_summary[2,"result"],
                                                       sep =""),
                                        PT_CS = ifelse(flag_patent_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_flag_patent_summary <- ggplot(flag_patent_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=flag_patent_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_patent_summary$pretrend[1]) +
    geom_rect(data=flag_patent_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_patent_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(flag_patent_summary$down, na.rm=T),max(flag_patent_summary$up, na.rm=T)), max(-min(flag_patent_summary$down, na.rm=T),max(flag_patent_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_flag_patent <- p_PSM_flag_patent_sa20+p_flag_patent_cs+p_flag_patent_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_flag_patent)
  
  ################## flag_jitsuyo------------
  # ipw
  es_PSM_flag_jitsuyo_sa20 = feols(flag_jitsuyo ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_flag_jitsuyo_sa20 <- aggregate(es_PSM_flag_jitsuyo_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_flag_jitsuyo_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_flag_jitsuyo_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + 
    labs(title = "flag_jitsuyo", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_flag_jitsuyo <- att_gt(yname = "flag_jitsuyo",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                            xformla = formula_cs, 
                            est_method = "ipw",base_period="universal",alp=0.05,
                            data = dfm_kikatsu2_ipw,
                            anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_flag_jitsuyo_smry <- aggte(cs_flag_jitsuyo, type = "dynamic",na.rm = TRUE)
  
  p_flag_jitsuyo_cs <- ggdid(cs_flag_jitsuyo_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_flag_jitsuyo_cs <- aggte(cs_flag_jitsuyo, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  flag_jitsuyo_summary <- data.frame(method = method,
                                     estimate = c(att_PSM_flag_jitsuyo_sa20[1], a_flag_jitsuyo_cs$overall.att
                                     ),
                                     se = c(att_PSM_flag_jitsuyo_sa20[2],a_flag_jitsuyo_cs$overall.se
                                     ),
                                     up = c(att_PSM_flag_jitsuyo_sa20[1] + 1.645*att_PSM_flag_jitsuyo_sa20[2],a_flag_jitsuyo_cs$overall.att + 1.645*a_flag_jitsuyo_cs$overall.se
                                     ),
                                     down = c(att_PSM_flag_jitsuyo_sa20[1] - 1.645*att_PSM_flag_jitsuyo_sa20[2],a_flag_jitsuyo_cs$overall.att - 1.645*a_flag_jitsuyo_cs$overall.se
                                     ),
                                     up95 = c(att_PSM_flag_jitsuyo_sa20[1] + 1.960*att_PSM_flag_jitsuyo_sa20[2],a_flag_jitsuyo_cs$overall.att + 1.960*a_flag_jitsuyo_cs$overall.se
                                     ),
                                     down95 = c(att_PSM_flag_jitsuyo_sa20[1] - 1.960*att_PSM_flag_jitsuyo_sa20[2],a_flag_jitsuyo_cs$overall.att - 1.960*a_flag_jitsuyo_cs$overall.se
                                     ),
                                     up99 = c(att_PSM_flag_jitsuyo_sa20[1] + 2.5758*att_PSM_flag_jitsuyo_sa20[2],a_flag_jitsuyo_cs$overall.att + 2.5758*a_flag_jitsuyo_cs$overall.se
                                     ),
                                     down99 = c(att_PSM_flag_jitsuyo_sa20[1] - 2.5758*att_PSM_flag_jitsuyo_sa20[2],a_flag_jitsuyo_cs$overall.att - 2.5758*a_flag_jitsuyo_cs$overall.se
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  flag_jitsuyo_summary_PDP <- data.frame(Category = "asset", Variable = "flag_jitsuyo", 
                                         ATT_SA = paste(round(flag_jitsuyo_summary[1,2],3), " (", round(flag_jitsuyo_summary[1,3],3), ") ",flag_jitsuyo_summary[1,"result"],
                                                        sep =""),
                                         PT_SA = ifelse(flag_jitsuyo_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                         ATT_CS = paste(round(flag_jitsuyo_summary[2,2],3), " (", round(flag_jitsuyo_summary[2,3],3), ") ",flag_jitsuyo_summary[2,"result"],
                                                        sep =""),
                                         PT_CS = ifelse(flag_jitsuyo_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_flag_jitsuyo_summary <- ggplot(flag_jitsuyo_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=flag_jitsuyo_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_jitsuyo_summary$pretrend[1]) +
    geom_rect(data=flag_jitsuyo_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_jitsuyo_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(flag_jitsuyo_summary$down, na.rm=T),max(flag_jitsuyo_summary$up, na.rm=T)), max(-min(flag_jitsuyo_summary$down, na.rm=T),max(flag_jitsuyo_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_flag_jitsuyo <- p_PSM_flag_jitsuyo_sa20+p_flag_jitsuyo_cs+p_flag_jitsuyo_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_flag_jitsuyo)
  
  ################## flag_isho------------
  # ipw
  es_PSM_flag_isho_sa20 = feols(flag_isho ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_flag_isho_sa20 <- aggregate(es_PSM_flag_isho_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_flag_isho_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_flag_isho_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + 
    labs(title = "flag_isho", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_flag_isho <- att_gt(yname = "flag_isho",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                         xformla = formula_cs, 
                         est_method = "ipw",base_period="universal",alp=0.05,
                         data = dfm_kikatsu2_ipw,
                         anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_flag_isho_smry <- aggte(cs_flag_isho, type = "dynamic",na.rm = TRUE)
  
  p_flag_isho_cs <- ggdid(cs_flag_isho_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_flag_isho_cs <- aggte(cs_flag_isho, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  flag_isho_summary <- data.frame(method = method,
                                  estimate = c(att_PSM_flag_isho_sa20[1], a_flag_isho_cs$overall.att
                                  ),
                                  se = c(att_PSM_flag_isho_sa20[2],a_flag_isho_cs$overall.se
                                  ),
                                  up = c(att_PSM_flag_isho_sa20[1] + 1.645*att_PSM_flag_isho_sa20[2],a_flag_isho_cs$overall.att + 1.645*a_flag_isho_cs$overall.se
                                  ),
                                  down = c(att_PSM_flag_isho_sa20[1] - 1.645*att_PSM_flag_isho_sa20[2],a_flag_isho_cs$overall.att - 1.645*a_flag_isho_cs$overall.se
                                  ),
                                  up95 = c(att_PSM_flag_isho_sa20[1] + 1.960*att_PSM_flag_isho_sa20[2],a_flag_isho_cs$overall.att + 1.960*a_flag_isho_cs$overall.se
                                  ),
                                  down95 = c(att_PSM_flag_isho_sa20[1] - 1.960*att_PSM_flag_isho_sa20[2],a_flag_isho_cs$overall.att - 1.960*a_flag_isho_cs$overall.se
                                  ),
                                  up99 = c(att_PSM_flag_isho_sa20[1] + 2.5758*att_PSM_flag_isho_sa20[2],a_flag_isho_cs$overall.att + 2.5758*a_flag_isho_cs$overall.se
                                  ),
                                  down99 = c(att_PSM_flag_isho_sa20[1] - 2.5758*att_PSM_flag_isho_sa20[2],a_flag_isho_cs$overall.att - 2.5758*a_flag_isho_cs$overall.se
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  flag_isho_summary_PDP <- data.frame(Category = "asset", Variable = "flag_isho", 
                                      ATT_SA = paste(round(flag_isho_summary[1,2],3), " (", round(flag_isho_summary[1,3],3), ") ",flag_isho_summary[1,"result"],
                                                     sep =""),
                                      PT_SA = ifelse(flag_isho_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                      ATT_CS = paste(round(flag_isho_summary[2,2],3), " (", round(flag_isho_summary[2,3],3), ") ",flag_isho_summary[2,"result"],
                                                     sep =""),
                                      PT_CS = ifelse(flag_isho_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_flag_isho_summary <- ggplot(flag_isho_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=flag_isho_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_isho_summary$pretrend[1]) +
    geom_rect(data=flag_isho_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_isho_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(flag_isho_summary$down, na.rm=T),max(flag_isho_summary$up, na.rm=T)), max(-min(flag_isho_summary$down, na.rm=T),max(flag_isho_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_flag_isho <- p_PSM_flag_isho_sa20+p_flag_isho_cs+p_flag_isho_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_flag_isho)
  
  ################## log_patent------------
  # ipw
  es_PSM_log_patent_sa20 = feols(log_patent ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_log_patent_sa20 <- aggregate(es_PSM_log_patent_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_patent_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_patent_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_patent", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_patent <- att_gt(yname = "log_patent",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                          xformla = formula_cs, 
                          est_method = "ipw",base_period="universal",alp=0.05,
                          data = dfm_kikatsu2_ipw,
                          anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_patent_smry <- aggte(cs_log_patent, type = "dynamic",na.rm = TRUE)
  
  p_log_patent_cs <- ggdid(cs_log_patent_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_log_patent_cs <- aggte(cs_log_patent, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  log_patent_summary <- data.frame(method = method,
                                   estimate = c(att_PSM_log_patent_sa20[1], a_log_patent_cs$overall.att
                                   ),
                                   se = c(att_PSM_log_patent_sa20[2],a_log_patent_cs$overall.se
                                   ),
                                   up = c(att_PSM_log_patent_sa20[1] + 1.645*att_PSM_log_patent_sa20[2],a_log_patent_cs$overall.att + 1.645*a_log_patent_cs$overall.se
                                   ),
                                   down = c(att_PSM_log_patent_sa20[1] - 1.645*att_PSM_log_patent_sa20[2],a_log_patent_cs$overall.att - 1.645*a_log_patent_cs$overall.se
                                   ),
                                   up95 = c(att_PSM_log_patent_sa20[1] + 1.960*att_PSM_log_patent_sa20[2],a_log_patent_cs$overall.att + 1.960*a_log_patent_cs$overall.se
                                   ),
                                   down95 = c(att_PSM_log_patent_sa20[1] - 1.960*att_PSM_log_patent_sa20[2],a_log_patent_cs$overall.att - 1.960*a_log_patent_cs$overall.se
                                   ),
                                   up99 = c(att_PSM_log_patent_sa20[1] + 2.5758*att_PSM_log_patent_sa20[2],a_log_patent_cs$overall.att + 2.5758*a_log_patent_cs$overall.se
                                   ),
                                   down99 = c(att_PSM_log_patent_sa20[1] - 2.5758*att_PSM_log_patent_sa20[2],a_log_patent_cs$overall.att - 2.5758*a_log_patent_cs$overall.se
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  log_patent_summary_PDP <- data.frame(Category = "asset", Variable = "log_patent", 
                                       ATT_SA = paste(round(log_patent_summary[1,2],3), " (", round(log_patent_summary[1,3],3), ") ",log_patent_summary[1,"result"],
                                                      sep =""),
                                       PT_SA = ifelse(log_patent_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                       ATT_CS = paste(round(log_patent_summary[2,2],3), " (", round(log_patent_summary[2,3],3), ") ",log_patent_summary[2,"result"],
                                                      sep =""),
                                       PT_CS = ifelse(log_patent_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_log_patent_summary <- ggplot(log_patent_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_patent_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_patent_summary$pretrend[1]) +
    geom_rect(data=log_patent_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_patent_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_patent_summary$down, na.rm=T),max(log_patent_summary$up, na.rm=T)), max(-min(log_patent_summary$down, na.rm=T),max(log_patent_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_patent <- p_PSM_log_patent_sa20+p_log_patent_cs+p_log_patent_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_log_patent)
  
  ################## log_jitsuyo------------
  # ipw
  es_PSM_log_jitsuyo_sa20 = feols(log_jitsuyo ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_log_jitsuyo_sa20 <- aggregate(es_PSM_log_jitsuyo_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_jitsuyo_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_jitsuyo_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_jitsuyo", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_jitsuyo <- att_gt(yname = "log_jitsuyo",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                           xformla = formula_cs, 
                           est_method = "ipw",base_period="universal",alp=0.05,
                           data = dfm_kikatsu2_ipw,
                           anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_jitsuyo_smry <- aggte(cs_log_jitsuyo, type = "dynamic",na.rm = TRUE)
  
  p_log_jitsuyo_cs <- ggdid(cs_log_jitsuyo_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_log_jitsuyo_cs <- aggte(cs_log_jitsuyo, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  log_jitsuyo_summary <- data.frame(method = method,
                                    estimate = c(att_PSM_log_jitsuyo_sa20[1], a_log_jitsuyo_cs$overall.att
                                    ),
                                    se = c(att_PSM_log_jitsuyo_sa20[2],a_log_jitsuyo_cs$overall.se
                                    ),
                                    up = c(att_PSM_log_jitsuyo_sa20[1] + 1.645*att_PSM_log_jitsuyo_sa20[2],a_log_jitsuyo_cs$overall.att + 1.645*a_log_jitsuyo_cs$overall.se
                                    ),
                                    down = c(att_PSM_log_jitsuyo_sa20[1] - 1.645*att_PSM_log_jitsuyo_sa20[2],a_log_jitsuyo_cs$overall.att - 1.645*a_log_jitsuyo_cs$overall.se
                                    ),
                                    up95 = c(att_PSM_log_jitsuyo_sa20[1] + 1.960*att_PSM_log_jitsuyo_sa20[2],a_log_jitsuyo_cs$overall.att + 1.960*a_log_jitsuyo_cs$overall.se
                                    ),
                                    down95 = c(att_PSM_log_jitsuyo_sa20[1] - 1.960*att_PSM_log_jitsuyo_sa20[2],a_log_jitsuyo_cs$overall.att - 1.960*a_log_jitsuyo_cs$overall.se
                                    ),
                                    up99 = c(att_PSM_log_jitsuyo_sa20[1] + 2.5758*att_PSM_log_jitsuyo_sa20[2],a_log_jitsuyo_cs$overall.att + 2.5758*a_log_jitsuyo_cs$overall.se
                                    ),
                                    down99 = c(att_PSM_log_jitsuyo_sa20[1] - 2.5758*att_PSM_log_jitsuyo_sa20[2],a_log_jitsuyo_cs$overall.att - 2.5758*a_log_jitsuyo_cs$overall.se
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  log_jitsuyo_summary_PDP <- data.frame(Category = "asset", Variable = "log_jitsuyo", 
                                        ATT_SA = paste(round(log_jitsuyo_summary[1,2],3), " (", round(log_jitsuyo_summary[1,3],3), ") ",log_jitsuyo_summary[1,"result"],
                                                       sep =""),
                                        PT_SA = ifelse(log_jitsuyo_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                        ATT_CS = paste(round(log_jitsuyo_summary[2,2],3), " (", round(log_jitsuyo_summary[2,3],3), ") ",log_jitsuyo_summary[2,"result"],
                                                       sep =""),
                                        PT_CS = ifelse(log_jitsuyo_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_log_jitsuyo_summary <- ggplot(log_jitsuyo_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_jitsuyo_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_jitsuyo_summary$pretrend[1]) +
    geom_rect(data=log_jitsuyo_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_jitsuyo_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_jitsuyo_summary$down, na.rm=T),max(log_jitsuyo_summary$up, na.rm=T)), max(-min(log_jitsuyo_summary$down, na.rm=T),max(log_jitsuyo_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_jitsuyo <- p_PSM_log_jitsuyo_sa20+p_log_jitsuyo_cs+p_log_jitsuyo_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_log_jitsuyo)
  
  ################## log_isho------------
  # ipw
  es_PSM_log_isho_sa20 = feols(log_isho ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_log_isho_sa20 <- aggregate(es_PSM_log_isho_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_isho_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_isho_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_isho", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_isho <- att_gt(yname = "log_isho",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                        xformla = formula_cs, 
                        est_method = "ipw",base_period="universal",alp=0.05,
                        data = dfm_kikatsu2_ipw,
                        anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_isho_smry <- aggte(cs_log_isho, type = "dynamic",na.rm = TRUE)
  
  p_log_isho_cs <- ggdid(cs_log_isho_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_log_isho_cs <- aggte(cs_log_isho, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  log_isho_summary <- data.frame(method = method,
                                 estimate = c(att_PSM_log_isho_sa20[1], a_log_isho_cs$overall.att
                                 ),
                                 se = c(att_PSM_log_isho_sa20[2],a_log_isho_cs$overall.se
                                 ),
                                 up = c(att_PSM_log_isho_sa20[1] + 1.645*att_PSM_log_isho_sa20[2],a_log_isho_cs$overall.att + 1.645*a_log_isho_cs$overall.se
                                 ),
                                 down = c(att_PSM_log_isho_sa20[1] - 1.645*att_PSM_log_isho_sa20[2],a_log_isho_cs$overall.att - 1.645*a_log_isho_cs$overall.se
                                 ),
                                 up95 = c(att_PSM_log_isho_sa20[1] + 1.960*att_PSM_log_isho_sa20[2],a_log_isho_cs$overall.att + 1.960*a_log_isho_cs$overall.se
                                 ),
                                 down95 = c(att_PSM_log_isho_sa20[1] - 1.960*att_PSM_log_isho_sa20[2],a_log_isho_cs$overall.att - 1.960*a_log_isho_cs$overall.se
                                 ),
                                 up99 = c(att_PSM_log_isho_sa20[1] + 2.5758*att_PSM_log_isho_sa20[2],a_log_isho_cs$overall.att + 2.5758*a_log_isho_cs$overall.se
                                 ),
                                 down99 = c(att_PSM_log_isho_sa20[1] - 2.5758*att_PSM_log_isho_sa20[2],a_log_isho_cs$overall.att - 2.5758*a_log_isho_cs$overall.se
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  log_isho_summary_PDP <- data.frame(Category = "asset", Variable = "log_isho", 
                                     ATT_SA = paste(round(log_isho_summary[1,2],3), " (", round(log_isho_summary[1,3],3), ") ",log_isho_summary[1,"result"],
                                                    sep =""),
                                     PT_SA = ifelse(log_isho_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                     ATT_CS = paste(round(log_isho_summary[2,2],3), " (", round(log_isho_summary[2,3],3), ") ",log_isho_summary[2,"result"],
                                                    sep =""),
                                     PT_CS = ifelse(log_isho_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_log_isho_summary <- ggplot(log_isho_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_isho_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_isho_summary$pretrend[1]) +
    geom_rect(data=log_isho_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_isho_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_isho_summary$down, na.rm=T),max(log_isho_summary$up, na.rm=T)), max(-min(log_isho_summary$down, na.rm=T),max(log_isho_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_isho <- p_PSM_log_isho_sa20+p_log_isho_cs+p_log_isho_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_log_isho)
  
  ## output_IP-----------
  pa_IP_sum <- pa_flag_patent/pa_flag_jitsuyo/pa_flag_isho/pa_log_patent/pa_log_jitsuyo/pa_log_isho + plot_annotation(
    title = "Effect on the Intellectual Property",
    subtitle = target
  ) &
    theme(text = element_text(family="Meiryo UI"))
  
  filename_pdf <- paste("02_analysis_output/", target,"_06IP_", count_intervention, ".pdf", sep="")
  ggsave(filename_pdf,pa_IP_sum, 
         width=15, height =36, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("02_analysis_output/", target,"_06IP_", count_intervention, ".png", sep="")
  ggsave(filename_png,pa_IP_sum, 
         width=15, height =36, units = "cm", dpi=200) 
  
  write.xlsx(list("flag_patent"=flag_patent_summary,"flag_jitsuyo"=flag_jitsuyo_summary,"flag_isho"=flag_isho_summary,"log_patent"=log_patent_summary,"log_jitsuyo"=log_jitsuyo_summary,"log_isho"=log_isho_summary), paste("02_analysis_output/",target,"_06IP_table.xlsx", sep=""))
  write.xlsx(list("flag_patent"=flag_patent_summary_appendix,"flag_jitsuyo"=flag_jitsuyo_summary_appendix,"flag_isho"=flag_isho_summary_appendix,"log_patent"=log_patent_summary_appendix,"log_jitsuyo"=log_jitsuyo_summary_appendix,"log_isho"=log_isho_summary_appendix), paste("02_analysis_output/",target,"_06IP_table_appendix.xlsx", sep=""))
  
  
  # まとめ
  summary_PDP <- rbind(flag_patent_summary_PDP,flag_jitsuyo_summary_PDP,flag_isho_summary_PDP,log_patent_summary_PDP,log_jitsuyo_summary_PDP,log_isho_summary_PDP) %>%
    mutate(Category = "IP")
  write.xlsx(list("result"= summary_PDP), paste("02_analysis_output/",target,"_06IP_table_PDP.xlsx"))
  
}
analysis_investment <- function(target){
  ################## flag_investment_affiliate_domestic------------
  # ipw
  es_PSM_flag_investment_affiliate_domestic_sa20 = feols(flag_investment_affiliate_domestic ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_flag_investment_affiliate_domestic_sa20 <- aggregate(es_PSM_flag_investment_affiliate_domestic_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_flag_investment_affiliate_domestic_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_flag_investment_affiliate_domestic_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + 
    labs(title = "flag_investment_affiliate_domestic", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_flag_investment_affiliate_domestic <- att_gt(yname = "flag_investment_affiliate_domestic",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                                                  xformla = formula_cs, 
                                                  est_method = "ipw",base_period="universal",alp=0.05,
                                                  data = dfm_kikatsu2_ipw,
                                                  anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_flag_investment_affiliate_domestic_smry <- aggte(cs_flag_investment_affiliate_domestic, type = "dynamic",na.rm = TRUE)
  
  p_flag_investment_affiliate_domestic_cs <- ggdid(cs_flag_investment_affiliate_domestic_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_flag_investment_affiliate_domestic_cs <- aggte(cs_flag_investment_affiliate_domestic, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  flag_investment_affiliate_domestic_summary <- data.frame(method = method,
                                                           estimate = c(att_PSM_flag_investment_affiliate_domestic_sa20[1], a_flag_investment_affiliate_domestic_cs$overall.att
                                                           ),
                                                           se = c(att_PSM_flag_investment_affiliate_domestic_sa20[2],a_flag_investment_affiliate_domestic_cs$overall.se
                                                           ),
                                                           up = c(att_PSM_flag_investment_affiliate_domestic_sa20[1] + 1.645*att_PSM_flag_investment_affiliate_domestic_sa20[2],a_flag_investment_affiliate_domestic_cs$overall.att + 1.645*a_flag_investment_affiliate_domestic_cs$overall.se
                                                           ),
                                                           down = c(att_PSM_flag_investment_affiliate_domestic_sa20[1] - 1.645*att_PSM_flag_investment_affiliate_domestic_sa20[2],a_flag_investment_affiliate_domestic_cs$overall.att - 1.645*a_flag_investment_affiliate_domestic_cs$overall.se
                                                           ),
                                                           up95 = c(att_PSM_flag_investment_affiliate_domestic_sa20[1] + 1.960*att_PSM_flag_investment_affiliate_domestic_sa20[2],a_flag_investment_affiliate_domestic_cs$overall.att + 1.960*a_flag_investment_affiliate_domestic_cs$overall.se
                                                           ),
                                                           down95 = c(att_PSM_flag_investment_affiliate_domestic_sa20[1] - 1.960*att_PSM_flag_investment_affiliate_domestic_sa20[2],a_flag_investment_affiliate_domestic_cs$overall.att - 1.960*a_flag_investment_affiliate_domestic_cs$overall.se
                                                           ),
                                                           up99 = c(att_PSM_flag_investment_affiliate_domestic_sa20[1] + 2.5758*att_PSM_flag_investment_affiliate_domestic_sa20[2],a_flag_investment_affiliate_domestic_cs$overall.att + 2.5758*a_flag_investment_affiliate_domestic_cs$overall.se
                                                           ),
                                                           down99 = c(att_PSM_flag_investment_affiliate_domestic_sa20[1] - 2.5758*att_PSM_flag_investment_affiliate_domestic_sa20[2],a_flag_investment_affiliate_domestic_cs$overall.att - 2.5758*a_flag_investment_affiliate_domestic_cs$overall.se
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  flag_investment_affiliate_domestic_summary_PDP <- data.frame(Category = "asset", Variable = "flag_investment_affiliate_domestic", 
                                                               ATT_SA = paste(round(flag_investment_affiliate_domestic_summary[1,2],3), " (", round(flag_investment_affiliate_domestic_summary[1,3],3), ") ",flag_investment_affiliate_domestic_summary[1,"result"],
                                                                              sep =""),
                                                               PT_SA = ifelse(flag_investment_affiliate_domestic_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                                               ATT_CS = paste(round(flag_investment_affiliate_domestic_summary[2,2],3), " (", round(flag_investment_affiliate_domestic_summary[2,3],3), ") ",flag_investment_affiliate_domestic_summary[2,"result"],
                                                                              sep =""),
                                                               PT_CS = ifelse(flag_investment_affiliate_domestic_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_flag_investment_affiliate_domestic_summary <- ggplot(flag_investment_affiliate_domestic_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=flag_investment_affiliate_domestic_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_investment_affiliate_domestic_summary$pretrend[1]) +
    geom_rect(data=flag_investment_affiliate_domestic_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_investment_affiliate_domestic_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(flag_investment_affiliate_domestic_summary$down, na.rm=T),max(flag_investment_affiliate_domestic_summary$up, na.rm=T)), max(-min(flag_investment_affiliate_domestic_summary$down, na.rm=T),max(flag_investment_affiliate_domestic_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_flag_investment_affiliate_domestic <- p_PSM_flag_investment_affiliate_domestic_sa20+p_flag_investment_affiliate_domestic_cs+p_flag_investment_affiliate_domestic_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_flag_investment_affiliate_domestic)
  
  ################## flag_investment_affiliate_overseas------------
  # ipw
  es_PSM_flag_investment_affiliate_overseas_sa20 = feols(flag_investment_affiliate_overseas ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_flag_investment_affiliate_overseas_sa20 <- aggregate(es_PSM_flag_investment_affiliate_overseas_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_flag_investment_affiliate_overseas_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_flag_investment_affiliate_overseas_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + 
    labs(title = "flag_investment_affiliate_overseas", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_flag_investment_affiliate_overseas <- att_gt(yname = "flag_investment_affiliate_overseas",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                                                  xformla = formula_cs, 
                                                  est_method = "ipw",base_period="universal",alp=0.05,
                                                  data = dfm_kikatsu2_ipw,
                                                  anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_flag_investment_affiliate_overseas_smry <- aggte(cs_flag_investment_affiliate_overseas, type = "dynamic",na.rm = TRUE)
  
  p_flag_investment_affiliate_overseas_cs <- ggdid(cs_flag_investment_affiliate_overseas_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_flag_investment_affiliate_overseas_cs <- aggte(cs_flag_investment_affiliate_overseas, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  flag_investment_affiliate_overseas_summary <- data.frame(method = method,
                                                           estimate = c(att_PSM_flag_investment_affiliate_overseas_sa20[1], a_flag_investment_affiliate_overseas_cs$overall.att
                                                           ),
                                                           se = c(att_PSM_flag_investment_affiliate_overseas_sa20[2],a_flag_investment_affiliate_overseas_cs$overall.se
                                                           ),
                                                           up = c(att_PSM_flag_investment_affiliate_overseas_sa20[1] + 1.645*att_PSM_flag_investment_affiliate_overseas_sa20[2],a_flag_investment_affiliate_overseas_cs$overall.att + 1.645*a_flag_investment_affiliate_overseas_cs$overall.se
                                                           ),
                                                           down = c(att_PSM_flag_investment_affiliate_overseas_sa20[1] - 1.645*att_PSM_flag_investment_affiliate_overseas_sa20[2],a_flag_investment_affiliate_overseas_cs$overall.att - 1.645*a_flag_investment_affiliate_overseas_cs$overall.se
                                                           ),
                                                           up95 = c(att_PSM_flag_investment_affiliate_overseas_sa20[1] + 1.960*att_PSM_flag_investment_affiliate_overseas_sa20[2],a_flag_investment_affiliate_overseas_cs$overall.att + 1.960*a_flag_investment_affiliate_overseas_cs$overall.se
                                                           ),
                                                           down95 = c(att_PSM_flag_investment_affiliate_overseas_sa20[1] - 1.960*att_PSM_flag_investment_affiliate_overseas_sa20[2],a_flag_investment_affiliate_overseas_cs$overall.att - 1.960*a_flag_investment_affiliate_overseas_cs$overall.se
                                                           ),
                                                           up99 = c(att_PSM_flag_investment_affiliate_overseas_sa20[1] + 2.5758*att_PSM_flag_investment_affiliate_overseas_sa20[2],a_flag_investment_affiliate_overseas_cs$overall.att + 2.5758*a_flag_investment_affiliate_overseas_cs$overall.se
                                                           ),
                                                           down99 = c(att_PSM_flag_investment_affiliate_overseas_sa20[1] - 2.5758*att_PSM_flag_investment_affiliate_overseas_sa20[2],a_flag_investment_affiliate_overseas_cs$overall.att - 2.5758*a_flag_investment_affiliate_overseas_cs$overall.se
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  flag_investment_affiliate_overseas_summary_PDP <- data.frame(Category = "asset", Variable = "flag_investment_affiliate_overseas", 
                                                               ATT_SA = paste(round(flag_investment_affiliate_overseas_summary[1,2],3), " (", round(flag_investment_affiliate_overseas_summary[1,3],3), ") ",flag_investment_affiliate_overseas_summary[1,"result"],
                                                                              sep =""),
                                                               PT_SA = ifelse(flag_investment_affiliate_overseas_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                                               ATT_CS = paste(round(flag_investment_affiliate_overseas_summary[2,2],3), " (", round(flag_investment_affiliate_overseas_summary[2,3],3), ") ",flag_investment_affiliate_overseas_summary[2,"result"],
                                                                              sep =""),
                                                               PT_CS = ifelse(flag_investment_affiliate_overseas_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_flag_investment_affiliate_overseas_summary <- ggplot(flag_investment_affiliate_overseas_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=flag_investment_affiliate_overseas_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_investment_affiliate_overseas_summary$pretrend[1]) +
    geom_rect(data=flag_investment_affiliate_overseas_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_investment_affiliate_overseas_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(flag_investment_affiliate_overseas_summary$down, na.rm=T),max(flag_investment_affiliate_overseas_summary$up, na.rm=T)), max(-min(flag_investment_affiliate_overseas_summary$down, na.rm=T),max(flag_investment_affiliate_overseas_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_flag_investment_affiliate_overseas <- p_PSM_flag_investment_affiliate_overseas_sa20+p_flag_investment_affiliate_overseas_cs+p_flag_investment_affiliate_overseas_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_flag_investment_affiliate_overseas)
  
  ################## flag_dividend------------
  # ipw
  es_PSM_flag_dividend_sa20 = feols(flag_dividend ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_flag_dividend_sa20 <- aggregate(es_PSM_flag_dividend_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_flag_dividend_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_flag_dividend_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1)) + 
    labs(title = "flag_dividend", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_flag_dividend <- att_gt(yname = "flag_dividend",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                             xformla = formula_cs, 
                             est_method = "ipw",base_period="universal",alp=0.05,
                             data = dfm_kikatsu2_ipw,
                             anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_flag_dividend_smry <- aggte(cs_flag_dividend, type = "dynamic",na.rm = TRUE)
  
  p_flag_dividend_cs <- ggdid(cs_flag_dividend_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.1, 0.1))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_flag_dividend_cs <- aggte(cs_flag_dividend, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  flag_dividend_summary <- data.frame(method = method,
                                      estimate = c(att_PSM_flag_dividend_sa20[1], a_flag_dividend_cs$overall.att
                                      ),
                                      se = c(att_PSM_flag_dividend_sa20[2],a_flag_dividend_cs$overall.se
                                      ),
                                      up = c(att_PSM_flag_dividend_sa20[1] + 1.645*att_PSM_flag_dividend_sa20[2],a_flag_dividend_cs$overall.att + 1.645*a_flag_dividend_cs$overall.se
                                      ),
                                      down = c(att_PSM_flag_dividend_sa20[1] - 1.645*att_PSM_flag_dividend_sa20[2],a_flag_dividend_cs$overall.att - 1.645*a_flag_dividend_cs$overall.se
                                      ),
                                      up95 = c(att_PSM_flag_dividend_sa20[1] + 1.960*att_PSM_flag_dividend_sa20[2],a_flag_dividend_cs$overall.att + 1.960*a_flag_dividend_cs$overall.se
                                      ),
                                      down95 = c(att_PSM_flag_dividend_sa20[1] - 1.960*att_PSM_flag_dividend_sa20[2],a_flag_dividend_cs$overall.att - 1.960*a_flag_dividend_cs$overall.se
                                      ),
                                      up99 = c(att_PSM_flag_dividend_sa20[1] + 2.5758*att_PSM_flag_dividend_sa20[2],a_flag_dividend_cs$overall.att + 2.5758*a_flag_dividend_cs$overall.se
                                      ),
                                      down99 = c(att_PSM_flag_dividend_sa20[1] - 2.5758*att_PSM_flag_dividend_sa20[2],a_flag_dividend_cs$overall.att - 2.5758*a_flag_dividend_cs$overall.se
                                      )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  
  
  # create period specific att table
  flag_dividend_summary_appendix_sa <- data.frame(year = es_PSM_flag_dividend_sa20$coeftable[,0])%>%
    #    filter(year != as.numeric(es_PSM_flag_dividend_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_flag_dividend_sa20$coeftable[,1],
           sa_se =es_PSM_flag_dividend_sa20$coeftable[,2],
           sa_t =es_PSM_flag_dividend_sa20$coeftable[,3],
           sa_p =es_PSM_flag_dividend_sa20$coeftable[,4])
  flag_dividend_summary_appendix_cs <- data.frame(year = cs_flag_dividend_smry$egt,
                                                  cs =  cs_flag_dividend_smry$att.egt,
                                                  cs_se = cs_flag_dividend_smry$se.egt,
                                                  cs_cband_lower = cs_flag_dividend_smry$att.egt - cs_flag_dividend_smry$crit.val.egt*cs_flag_dividend_smry$se.egt,
                                                  cs_cband_upper = cs_flag_dividend_smry$att.egt + cs_flag_dividend_smry$crit.val.egt*cs_flag_dividend_smry$se.egt) %>%
    filter(cs != 0)
  flag_dividend_summary_appendix <- cbind(flag_dividend_summary_appendix_sa,flag_dividend_summary_appendix_cs#,  by = c("year")
  ) %>%
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  flag_dividend_summary_PDP <- data.frame(Category = "asset", Variable = "flag_dividend", 
                                          ATT_SA = paste(round(flag_dividend_summary[1,2],3), " (", round(flag_dividend_summary[1,3],3), ") ",flag_dividend_summary[1,"result"],
                                                         sep =""),
                                          PT_SA = ifelse(flag_dividend_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                          ATT_CS = paste(round(flag_dividend_summary[2,2],3), " (", round(flag_dividend_summary[2,3],3), ") ",flag_dividend_summary[2,"result"],
                                                         sep =""),
                                          PT_CS = ifelse(flag_dividend_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_flag_dividend_summary <- ggplot(flag_dividend_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=flag_dividend_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_dividend_summary$pretrend[1]) +
    geom_rect(data=flag_dividend_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*flag_dividend_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(flag_dividend_summary$down, na.rm=T),max(flag_dividend_summary$up, na.rm=T)), max(-min(flag_dividend_summary$down, na.rm=T),max(flag_dividend_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_flag_dividend <- p_PSM_flag_dividend_sa20+p_flag_dividend_cs+p_flag_dividend_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_flag_dividend)
  
  ################## log_investment_affiliate_domestic------------
  # ipw
  es_PSM_log_investment_affiliate_domestic_sa20 = feols(log_investment_affiliate_domestic ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_log_investment_affiliate_domestic_sa20 <- aggregate(es_PSM_log_investment_affiliate_domestic_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_investment_affiliate_domestic_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_investment_affiliate_domestic_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_investment_affiliate_domestic", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_investment_affiliate_domestic <- att_gt(yname = "log_investment_affiliate_domestic",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                                                 xformla = formula_cs, 
                                                 est_method = "ipw",base_period="universal",alp=0.05,
                                                 data = dfm_kikatsu2_ipw,
                                                 anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_investment_affiliate_domestic_smry <- aggte(cs_log_investment_affiliate_domestic, type = "dynamic",na.rm = TRUE)
  
  p_log_investment_affiliate_domestic_cs <- ggdid(cs_log_investment_affiliate_domestic_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_log_investment_affiliate_domestic_cs <- aggte(cs_log_investment_affiliate_domestic, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  log_investment_affiliate_domestic_summary <- data.frame(method = method,
                                                          estimate = c(att_PSM_log_investment_affiliate_domestic_sa20[1], a_log_investment_affiliate_domestic_cs$overall.att
                                                          ),
                                                          se = c(att_PSM_log_investment_affiliate_domestic_sa20[2],a_log_investment_affiliate_domestic_cs$overall.se
                                                          ),
                                                          up = c(att_PSM_log_investment_affiliate_domestic_sa20[1] + 1.645*att_PSM_log_investment_affiliate_domestic_sa20[2],a_log_investment_affiliate_domestic_cs$overall.att + 1.645*a_log_investment_affiliate_domestic_cs$overall.se
                                                          ),
                                                          down = c(att_PSM_log_investment_affiliate_domestic_sa20[1] - 1.645*att_PSM_log_investment_affiliate_domestic_sa20[2],a_log_investment_affiliate_domestic_cs$overall.att - 1.645*a_log_investment_affiliate_domestic_cs$overall.se
                                                          ),
                                                          up95 = c(att_PSM_log_investment_affiliate_domestic_sa20[1] + 1.960*att_PSM_log_investment_affiliate_domestic_sa20[2],a_log_investment_affiliate_domestic_cs$overall.att + 1.960*a_log_investment_affiliate_domestic_cs$overall.se
                                                          ),
                                                          down95 = c(att_PSM_log_investment_affiliate_domestic_sa20[1] - 1.960*att_PSM_log_investment_affiliate_domestic_sa20[2],a_log_investment_affiliate_domestic_cs$overall.att - 1.960*a_log_investment_affiliate_domestic_cs$overall.se
                                                          ),
                                                          up99 = c(att_PSM_log_investment_affiliate_domestic_sa20[1] + 2.5758*att_PSM_log_investment_affiliate_domestic_sa20[2],a_log_investment_affiliate_domestic_cs$overall.att + 2.5758*a_log_investment_affiliate_domestic_cs$overall.se
                                                          ),
                                                          down99 = c(att_PSM_log_investment_affiliate_domestic_sa20[1] - 2.5758*att_PSM_log_investment_affiliate_domestic_sa20[2],a_log_investment_affiliate_domestic_cs$overall.att - 2.5758*a_log_investment_affiliate_domestic_cs$overall.se
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  log_investment_affiliate_domestic_summary_PDP <- data.frame(Category = "asset", Variable = "log_investment_affiliate_domestic", 
                                                              ATT_SA = paste(round(log_investment_affiliate_domestic_summary[1,2],3), " (", round(log_investment_affiliate_domestic_summary[1,3],3), ") ",log_investment_affiliate_domestic_summary[1,"result"],
                                                                             sep =""),
                                                              PT_SA = ifelse(log_investment_affiliate_domestic_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                                              ATT_CS = paste(round(log_investment_affiliate_domestic_summary[2,2],3), " (", round(log_investment_affiliate_domestic_summary[2,3],3), ") ",log_investment_affiliate_domestic_summary[2,"result"],
                                                                             sep =""),
                                                              PT_CS = ifelse(log_investment_affiliate_domestic_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_log_investment_affiliate_domestic_summary <- ggplot(log_investment_affiliate_domestic_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_investment_affiliate_domestic_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_investment_affiliate_domestic_summary$pretrend[1]) +
    geom_rect(data=log_investment_affiliate_domestic_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_investment_affiliate_domestic_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_investment_affiliate_domestic_summary$down, na.rm=T),max(log_investment_affiliate_domestic_summary$up, na.rm=T)), max(-min(log_investment_affiliate_domestic_summary$down, na.rm=T),max(log_investment_affiliate_domestic_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_investment_affiliate_domestic <- p_PSM_log_investment_affiliate_domestic_sa20+p_log_investment_affiliate_domestic_cs+p_log_investment_affiliate_domestic_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_log_investment_affiliate_domestic)
  
  ################## log_investment_affiliate_overseas------------
  # ipw
  es_PSM_log_investment_affiliate_overseas_sa20 = feols(log_investment_affiliate_overseas ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_log_investment_affiliate_overseas_sa20 <- aggregate(es_PSM_log_investment_affiliate_overseas_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_investment_affiliate_overseas_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_investment_affiliate_overseas_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_investment_affiliate_overseas", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_investment_affiliate_overseas <- att_gt(yname = "log_investment_affiliate_overseas",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                                                 xformla = formula_cs, 
                                                 est_method = "ipw",base_period="universal",alp=0.05,
                                                 data = dfm_kikatsu2_ipw,
                                                 anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_investment_affiliate_overseas_smry <- aggte(cs_log_investment_affiliate_overseas, type = "dynamic",na.rm = TRUE)
  
  p_log_investment_affiliate_overseas_cs <- ggdid(cs_log_investment_affiliate_overseas_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_log_investment_affiliate_overseas_cs <- aggte(cs_log_investment_affiliate_overseas, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  log_investment_affiliate_overseas_summary <- data.frame(method = method,
                                                          estimate = c(att_PSM_log_investment_affiliate_overseas_sa20[1], a_log_investment_affiliate_overseas_cs$overall.att
                                                          ),
                                                          se = c(att_PSM_log_investment_affiliate_overseas_sa20[2],a_log_investment_affiliate_overseas_cs$overall.se
                                                          ),
                                                          up = c(att_PSM_log_investment_affiliate_overseas_sa20[1] + 1.645*att_PSM_log_investment_affiliate_overseas_sa20[2],a_log_investment_affiliate_overseas_cs$overall.att + 1.645*a_log_investment_affiliate_overseas_cs$overall.se
                                                          ),
                                                          down = c(att_PSM_log_investment_affiliate_overseas_sa20[1] - 1.645*att_PSM_log_investment_affiliate_overseas_sa20[2],a_log_investment_affiliate_overseas_cs$overall.att - 1.645*a_log_investment_affiliate_overseas_cs$overall.se
                                                          ),
                                                          up95 = c(att_PSM_log_investment_affiliate_overseas_sa20[1] + 1.960*att_PSM_log_investment_affiliate_overseas_sa20[2],a_log_investment_affiliate_overseas_cs$overall.att + 1.960*a_log_investment_affiliate_overseas_cs$overall.se
                                                          ),
                                                          down95 = c(att_PSM_log_investment_affiliate_overseas_sa20[1] - 1.960*att_PSM_log_investment_affiliate_overseas_sa20[2],a_log_investment_affiliate_overseas_cs$overall.att - 1.960*a_log_investment_affiliate_overseas_cs$overall.se
                                                          ),
                                                          up99 = c(att_PSM_log_investment_affiliate_overseas_sa20[1] + 2.5758*att_PSM_log_investment_affiliate_overseas_sa20[2],a_log_investment_affiliate_overseas_cs$overall.att + 2.5758*a_log_investment_affiliate_overseas_cs$overall.se
                                                          ),
                                                          down99 = c(att_PSM_log_investment_affiliate_overseas_sa20[1] - 2.5758*att_PSM_log_investment_affiliate_overseas_sa20[2],a_log_investment_affiliate_overseas_cs$overall.att - 2.5758*a_log_investment_affiliate_overseas_cs$overall.se
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  log_investment_affiliate_overseas_summary_PDP <- data.frame(Category = "asset", Variable = "log_investment_affiliate_overseas", 
                                                              ATT_SA = paste(round(log_investment_affiliate_overseas_summary[1,2],3), " (", round(log_investment_affiliate_overseas_summary[1,3],3), ") ",log_investment_affiliate_overseas_summary[1,"result"],
                                                                             sep =""),
                                                              PT_SA = ifelse(log_investment_affiliate_overseas_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                                              ATT_CS = paste(round(log_investment_affiliate_overseas_summary[2,2],3), " (", round(log_investment_affiliate_overseas_summary[2,3],3), ") ",log_investment_affiliate_overseas_summary[2,"result"],
                                                                             sep =""),
                                                              PT_CS = ifelse(log_investment_affiliate_overseas_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_log_investment_affiliate_overseas_summary <- ggplot(log_investment_affiliate_overseas_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_investment_affiliate_overseas_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_investment_affiliate_overseas_summary$pretrend[1]) +
    geom_rect(data=log_investment_affiliate_overseas_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_investment_affiliate_overseas_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_investment_affiliate_overseas_summary$down, na.rm=T),max(log_investment_affiliate_overseas_summary$up, na.rm=T)), max(-min(log_investment_affiliate_overseas_summary$down, na.rm=T),max(log_investment_affiliate_overseas_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_investment_affiliate_overseas <- p_PSM_log_investment_affiliate_overseas_sa20+p_log_investment_affiliate_overseas_cs+p_log_investment_affiliate_overseas_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_log_investment_affiliate_overseas)
  
  ################## log_dividend------------
  # ipw
  es_PSM_log_dividend_sa20 = feols(log_dividend ~ sunab(intervention_year,year, ref.p = -2) | id + year, dfm_kikatsu2_ipw, cluster="id", weights=dfm_kikatsu2_ipw$weights)
  att_PSM_log_dividend_sa20 <- aggregate(es_PSM_log_dividend_sa20, "att", full = FALSE, use_weights = TRUE)
  p_PSM_log_dividend_sa20 <- ggiplot(list('Sun & Abraham (2020) IPW, 90 & 95% CI' = es_PSM_log_dividend_sa20),ref.line = 0, pt.join = TRUE, geom_style = 'ribbon', ci_level = c(.9, .95))+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5)) + 
    labs(title = "log_dividend", subtitle = "S&A-IPW, 90&95% CI") +
    theme_light() + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  #CS
  cs_log_dividend <- att_gt(yname = "log_dividend",tname = "year",idname = "id", gname = "intervention_year",allow_unbalanced_panel = TRUE,
                            xformla = formula_cs, 
                            est_method = "ipw",base_period="universal",alp=0.05,
                            data = dfm_kikatsu2_ipw,
                            anticipation = 1 #　１年前にanticipateすると設定
  )
  cs_log_dividend_smry <- aggte(cs_log_dividend, type = "dynamic",na.rm = TRUE)
  
  p_log_dividend_cs <- ggdid(cs_log_dividend_smry)+coord_cartesian(xlim = c(-10,14), ylim = c(-0.5, 0.5))+ 
    scale_x_continuous(breaks=seq(-10,15,5)) + 
    labs(subtitle = "C&S-IPW, 95% CI") +theme_light() + theme(legend.position="none", plot.title = element_blank())
  a_log_dividend_cs <- aggte(cs_log_dividend, type = "simple",na.rm = TRUE)
  
  # create summary graph
  method <- c("S&A", "C&S")
  log_dividend_summary <- data.frame(method = method,
                                     estimate = c(att_PSM_log_dividend_sa20[1], a_log_dividend_cs$overall.att
                                     ),
                                     se = c(att_PSM_log_dividend_sa20[2],a_log_dividend_cs$overall.se
                                     ),
                                     up = c(att_PSM_log_dividend_sa20[1] + 1.645*att_PSM_log_dividend_sa20[2],a_log_dividend_cs$overall.att + 1.645*a_log_dividend_cs$overall.se
                                     ),
                                     down = c(att_PSM_log_dividend_sa20[1] - 1.645*att_PSM_log_dividend_sa20[2],a_log_dividend_cs$overall.att - 1.645*a_log_dividend_cs$overall.se
                                     ),
                                     up95 = c(att_PSM_log_dividend_sa20[1] + 1.960*att_PSM_log_dividend_sa20[2],a_log_dividend_cs$overall.att + 1.960*a_log_dividend_cs$overall.se
                                     ),
                                     down95 = c(att_PSM_log_dividend_sa20[1] - 1.960*att_PSM_log_dividend_sa20[2],a_log_dividend_cs$overall.att - 1.960*a_log_dividend_cs$overall.se
                                     ),
                                     up99 = c(att_PSM_log_dividend_sa20[1] + 2.5758*att_PSM_log_dividend_sa20[2],a_log_dividend_cs$overall.att + 2.5758*a_log_dividend_cs$overall.se
                                     ),
                                     down99 = c(att_PSM_log_dividend_sa20[1] - 2.5758*att_PSM_log_dividend_sa20[2],a_log_dividend_cs$overall.att - 2.5758*a_log_dividend_cs$overall.se
                                     )) %>%
    mutate(result = ifelse(up99*down99>0,"***",ifelse(up95*down95>0,"**",ifelse(up*down>0,"*",NA)))) 
  
  
  
  # create period specific att table
  log_dividend_summary_appendix_sa <- data.frame(year = es_PSM_log_dividend_sa20$coeftable[,0])%>%
    #    filter(year != as.numeric(es_PSM_log_dividend_sa20$model_matrix_info[[1]]$ref))%>% # remove ref time period
    mutate(sa =es_PSM_log_dividend_sa20$coeftable[,1],
           sa_se =es_PSM_log_dividend_sa20$coeftable[,2],
           sa_t =es_PSM_log_dividend_sa20$coeftable[,3],
           sa_p =es_PSM_log_dividend_sa20$coeftable[,4])
  log_dividend_summary_appendix_cs <- data.frame(year = cs_log_dividend_smry$egt,
                                                 cs =  cs_log_dividend_smry$att.egt,
                                                 cs_se = cs_log_dividend_smry$se.egt,
                                                 cs_cband_lower = cs_log_dividend_smry$att.egt - cs_log_dividend_smry$crit.val.egt*cs_log_dividend_smry$se.egt,
                                                 cs_cband_upper = cs_log_dividend_smry$att.egt + cs_log_dividend_smry$crit.val.egt*cs_log_dividend_smry$se.egt) %>%
    filter(cs != 0)
  log_dividend_summary_appendix <- cbind(log_dividend_summary_appendix_sa,log_dividend_summary_appendix_cs#,  by = c("year")
  ) %>%
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
    mutate(pretrend = ifelse(pretrend0 > 0,1,0), # pretrend0>0なら、pretrendありという判定になる
           rob = ifelse(is.na(result)==FALSE & # 結果が統計的に有意
                          pretrend == 0, 1,0) # pretrendがないなら、robは１になる
    )
  
  # まとめ用の表作成
  log_dividend_summary_PDP <- data.frame(Category = "asset", Variable = "log_dividend", 
                                         ATT_SA = paste(round(log_dividend_summary[1,2],3), " (", round(log_dividend_summary[1,3],3), ") ",log_dividend_summary[1,"result"],
                                                        sep =""),
                                         PT_SA = ifelse(log_dividend_summary[1,"pretrend"]==0, "Satisfy", "Fail to Satisfy"),
                                         ATT_CS = paste(round(log_dividend_summary[2,2],3), " (", round(log_dividend_summary[2,3],3), ") ",log_dividend_summary[2,"result"],
                                                        sep =""),
                                         PT_CS = ifelse(log_dividend_summary[2,"pretrend"]==0, "Satisfy", "Fail to Satisfy")
  )
  
  
  p_log_dividend_summary <- ggplot(log_dividend_summary, aes(x = factor(method, level = method), y=estimate, color=factor(method))) +
    geom_rect(data=log_dividend_summary, mapping=aes(xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_dividend_summary$pretrend[1]) +
    geom_rect(data=log_dividend_summary, mapping=aes(xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf), color = NA, fill="gray", alpha=0.4*log_dividend_summary$pretrend[2])  +
    geom_errorbar(aes(ymin = down, ymax = up), width=.1, position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin = down95, ymax = up95), width=0, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1), size=3, shape=15, fill="white")+
    coord_cartesian(ylim = c(-max(-min(log_dividend_summary$down, na.rm=T),max(log_dividend_summary$up, na.rm=T)), max(-min(log_dividend_summary$down, na.rm=T),max(log_dividend_summary$up, na.rm=T))))+
    xlab("Method") +
    labs(title = "Aggregated ATT", subtitle ="90&95% CI")+
    theme_light() +
    geom_text(aes(x=method, y=estimate, label=sprintf("%2.3f", estimate),vjust=2),color='gray') +
    geom_hline(yintercept=0)  + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  ## create grid arrange
  pa_log_dividend <- p_PSM_log_dividend_sa20+p_log_dividend_cs+p_log_dividend_summary + plot_layout(ncol = 3, widths = c(1,1,1))
  print( pa_log_dividend)
  
  ## output_investment-----------
  pa_investment_sum <- pa_flag_investment_affiliate_domestic/pa_flag_investment_affiliate_overseas/pa_flag_dividend/pa_log_investment_affiliate_domestic/pa_log_investment_affiliate_overseas/pa_log_dividend + plot_annotation(
    title = "Effect on the Investment",
    subtitle = target
  ) &
    theme(text = element_text(family="Meiryo UI"))
  
  filename_pdf <- paste("02_analysis_output/", target,"_07investment_", count_intervention, ".pdf", sep="")
  ggsave(filename_pdf,pa_investment_sum, 
         width=15, height =36, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("02_analysis_output/", target,"_07investment_", count_intervention, ".png", sep="")
  ggsave(filename_png,pa_investment_sum, 
         width=15, height =36, units = "cm", dpi=200) 
  
  write.xlsx(list("flag_investment_affiliate_dm"=flag_investment_affiliate_domestic_summary,"flag_investment_affiliate_ov"=flag_investment_affiliate_overseas_summary,"flag_dividend"=flag_dividend_summary,"log_investment_affiliate_dm"=log_investment_affiliate_domestic_summary,"log_investment_affiliate_ov"=log_investment_affiliate_overseas_summary,"log_dividend"=log_dividend_summary), paste("02_analysis_output/",target,"_07investment_table.xlsx", sep=""))
  write.xlsx(list("flag_investment_affiliate_dm"=flag_investment_affiliate_domestic_summary_appendix,"flag_investment_affiliate_ov"=flag_investment_affiliate_overseas_summary_appendix,"flag_dividend"=flag_dividend_summary_appendix,"log_investment_affiliate_dm"=log_investment_affiliate_domestic_summary_appendix,"log_investment_affiliate_ov"=log_investment_affiliate_overseas_summary_appendix,"log_dividend"=log_dividend_summary_appendix), paste("02_analysis_output/",target,"_07investment_table_appendix.xlsx", sep=""))
  
  
  # まとめ
  summary_PDP <- rbind(flag_investment_affiliate_domestic_summary_PDP,flag_investment_affiliate_overseas_summary_PDP,flag_dividend_summary_PDP,log_investment_affiliate_domestic_summary_PDP,log_investment_affiliate_overseas_summary_PDP,log_dividend_summary_PDP) %>%
    mutate(Category = "investment")
  write.xlsx(list("result"= summary_PDP), paste("02_analysis_output/",target,"_07investment_table_PDP.xlsx"))
  
}



## conduct evaluation------------

print(target)
try(analysis_asset(target))
try(analysis_employment(target))
try(analysis_business(target))
try(analysis_trade(target))
try(analysis_trainingRD(target))
try(analysis_IP(target))
try(analysis_investment(target))

# descriptive statistics=----------------

#都道府県リスト読み込み
pref_list <-  read.csv("data/map/pref_list.csv", encoding = "UTF-8")

#地図読み込み
gm_jpn0 <- read_sf("data/map/gm-jpn-all_u_2_2/polbnda_jpn.shp") %>%
  mutate(pref_num = str_sub(adm_code,start=1,end=2)) %>%
  group_by(pref_num) %>% summarize()

gm_jpn <- gm_jpn0 %>%
  mutate(pref_num=as.numeric(pref_num)) %>%
  left_join(pref_list, by = c("pref_num")) %>%
  mutate(pref_short = str_sub(pref,start=1,end=2))

## 整理
gm_jpn_OnlyLgIs <- gm_jpn %>% st_cast("MULTIPOLYGON") %>% 
  st_cast("POLYGON")

# ポリゴン面積を計算し，10km2より大きなもののみを選択
gm_jpn_OnlyLgIs <- mutate(gm_jpn_OnlyLgIs, area=st_area(gm_jpn_OnlyLgIs)) %>%
  filter(area > units::set_units(1*10^7, m^2)) 

ggplot() + geom_sf(data=gm_jpn_OnlyLgIs) + 
  coord_sf(crs=sf::st_crs("EPSG:3857"),default_crs = sf::st_crs("EPSG:4326")) + 
  theme_bw()

## prepare data for descriptive-----------
dfm0.descriptive0 <- dfm_kikatsu2_ipw %>%
  mutate(treat_var = ifelse(treat==0,"ALL","Treated"), #いずれトリートされるか否か #記述統計用
         treat_var_year = ifelse(treat==0, treat_var, paste("T",year -year_to_treat ,sep="")))
dfm0.descriptive <- dfm0.descriptive0 %>%
  filter(year == start_year)

dfm0.descriptive.treat <- dfm0.descriptive %>% filter(treat == "Treated") %>% #treated のみを抜き出し
  mutate(treat = "ALL") ## allとして書き出せるように修正
dfm.descriptive <- rbind(dfm0.descriptive,dfm0.descriptive.treat)  

### time
dfm_kikatsu_time0 <- dfm_kikatsu2_ipw %>%
  mutate(treat_var = ifelse(treat==0,"ALL","Treated"))

dfm_kikatsu_time1 <-dfm_kikatsu_time0 %>% filter(treat_var == "Treated") %>% #treated のみを抜き出し
  mutate(treat_var = "ALL") ## allとして書き出せるように修正

dfm_kikatsu_time <- rbind(dfm_kikatsu_time0, dfm_kikatsu_time1) %>%
  group_by(year, treat_var) %>%
  summarize(
    mean_sales = mean(sales, na.rm = TRUE),
    median_sales = median(sales, na.rm = TRUE),
    quantile25_sales = quantile(sales, 0.25, na.rm = TRUE),
    quantile75_sales = quantile(sales, 0.75, na.rm = TRUE),
    
    mean_workers = mean(workers, na.rm = TRUE),
    median_workers = median(workers, na.rm = TRUE),
    quantile25_workers = quantile(workers, 0.25, na.rm = TRUE),
    quantile75_workers = quantile(workers, 0.75, na.rm = TRUE),
    
    mean_sum_asset = mean(sum_asset, na.rm = TRUE),
    median_sum_asset = median(sum_asset, na.rm = TRUE),
    quantile25_sum_asset = quantile(sum_asset, 0.25, na.rm = TRUE),
    quantile75_sum_asset = quantile(sum_asset, 0.75, na.rm = TRUE),
    
    .groups = "drop"
  )

## define function-----------
create_map <- function(target){
  pref_count <- dfm.descriptive %>% 
    group_by(pref
             , treat_var) %>%
    tally() %>%
    pivot_wider(names_from = "treat_var", values_from="n")
  
  gm_join <- left_join(gm_jpn_OnlyLgIs, pref_count, by = c("pref"
  )) %>%
    mutate(Treated = ifelse(is.na(Treated),0, Treated))%>%
    mutate(ratio = Treated / ALL)
  
  ## 割合
  map_ratio <- ggplot() + geom_sf(data=gm_join,aes(fill = ratio)) +
    scale_fill_gradient(low="white", high="#619CFF", trans="log10"
    )+
    coord_sf(crs=sf::st_crs("EPSG:3857"),default_crs = sf::st_crs("EPSG:4326")) + 
    theme_bw()
  
  map_ratio
  
  write.xlsx(list("pref_count"=pref_count), paste("02_analysis_output/",target,"_00_map_table.xlsx", sep=""))
  
  filename <- paste("02_analysis_output/", target,"_00map_ratio_", count_intervention, ".pdf", sep="")
  ggsave(filename,map_ratio, 
         width=12, height =10, units = "cm", dpi=200) 
  filename_png <- paste("02_analysis_output/", target,"_00map_ratio_", count_intervention, ".png", sep="")
  ggsave(filename_png,map_ratio, 
         width=12, height =10, units = "cm", dpi=200) 
}

desc_time <- function(target){
  p_time_sales <- ggplot(data = dfm_kikatsu_time, aes(year, group=treat_var)) +
    geom_line(aes(y=mean_sales, color =treat_var), linewidth = 1.5)+
    geom_ribbon(aes(ymin = quantile25_sales, ymax = quantile75_sales, fill = treat_var), alpha = 0.2)+
    geom_line(aes(y=median_sales, color =treat_var), linewidth = 0.5)+
    ggtitle("Transition of Sales")+
    labs(caption = "Thick line is the mean. 
         Thin line is the median. 
         Area bitween 1st and 3rd quartiles is filled.")+
    theme_light()
  p_time_workers <- ggplot(data = dfm_kikatsu_time, aes(year, group=treat_var)) +
    geom_line(aes(y=mean_workers, color =treat_var), linewidth = 1.5)+
    geom_ribbon(aes(ymin = quantile25_workers, ymax = quantile75_workers, fill = treat_var), alpha = 0.2)+
    geom_line(aes(y=median_workers, color =treat_var), linewidth = 0.5)+
    ggtitle("Transition of workers")+
    labs(caption = "Thick line is the mean. 
         Thin line is the median. 
         Area bitween 1st and 3rd quartiles is filled.")+
    theme_light()
  p_time_sum_asset <- ggplot(data = dfm_kikatsu_time, aes(year, group=treat_var)) +
    geom_line(aes(y=mean_sum_asset, color =treat_var), linewidth = 1.5)+
    geom_ribbon(aes(ymin = quantile25_sum_asset, ymax = quantile75_sum_asset, fill = treat_var), alpha = 0.2)+
    geom_line(aes(y=median_sum_asset, color =treat_var), linewidth = 0.5)+
    labs(caption = "Thick line is the mean. 
         Thin line is the median. 
         Area bitween 1st and 3rd quartiles is filled.")+
    ggtitle("Transition of sum_asset")+
    theme_light()
  
  write.xlsx(list("time_series"=dfm_kikatsu_time), paste("02_analysis_output/",target,"_00_transition.xlsx", sep=""))
  
  p_time <- p_time_sales + p_time_workers + p_time_sum_asset+ plot_layout(ncol = 3, widths = c(1,1,1)) + plot_annotation(
    subtitle = target)&
    theme(text = element_text(family="Meiryo UI"))
  
  filename <- paste("02_analysis_output/", target,"_00transition_", count_intervention, ".pdf", sep="")
  ggsave(filename,p_time, 
         width=30, height =10, units = "cm", dpi=200, device = cairo_pdf) 
  
  filename_png <- paste("02_analysis_output/", target,"_00transition_", count_intervention, ".png", sep="")
  ggsave(filename_png,p_time, 
         width=30, height =10, units = "cm", dpi=200) 
}

hist_industry <- function(target){
  dfm.descriptive2.0 <- dfm.descriptive%>%
    group_by(treat_var, industry_name)%>%
    tally()%>%
    pivot_wider(names_from = "treat_var", values_from="n") %>%
    mutate(ratio = Treated/ALL
    ) %>%
    drop_na()
  
  dfm.descriptive2 <- dfm.descriptive2.0%>%
    filter(Treated >9)
  
  p_industry <- ggplot(dfm.descriptive2, aes(x = reorder(industry_name, Treated), y = Treated, fill = ratio)) +
    scale_fill_gradient(low="#D1E2FF", high="#619CFF", trans="log10")+ 
    coord_flip() +
    geom_bar(position = position_dodge(), stat = "identity")+
    #ggtitle("Number of treated firms by industry") +
    labs(caption = "Deapth of color illustrates the ratio of treated firms in the sample within the industry.")+
    theme_light()+ plot_annotation(
      subtitle = target)&
    theme(text = element_text(family="Meiryo UI"))
  p_industry
  
  write.xlsx(list("industry"=dfm.descriptive2.0), paste("02_analysis_output/",target,"_00_industry.xlsx", sep=""))
  
  
  filename <- paste("02_analysis_output/", target,"_00industry_", count_intervention, ".pdf", sep="")
  ggsave(filename,p_industry, 
         width=15, height =length(dfm.descriptive2)*1.5, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("02_analysis_output/", target,"_00industry_", count_intervention, ".png", sep="")
  ggsave(filename_png,p_industry, 
         width=15, height =length(dfm.descriptive2)*1.5, units = "cm", dpi=200) 
}

make_stat <- function(target){
  ### stat tabledfm_kikatsu0 
  dfpolicy_stat0 <- dfm0.descriptive0%>%
    mutate(treat_var_year = ifelse(treat_var_year=="ALL", "Control",treat_var_year))%>% ## Allが実質的にcontrolで定義されているため、名称を戻す
    group_by(treat_var_year)%>%
    summarise("number of observation" = n())
  
  dfpolicy_stat1 <- dfm0.descriptive%>%
    group_by(id)%>%
    group_by(intervention_year)%>%
    summarise("number of id" = n(),
              "multiple intervention" = sum(multiple_interventions))%>%
    mutate(treat_var_year = ifelse(intervention_year ==0, "Control", paste("T", intervention_year, sep="" ))) %>%
    select(!c(intervention_year))
  
  
  dfpolicy_stat <- left_join(dfpolicy_stat0,dfpolicy_stat1,by=c("treat_var_year")) %>%
    rename(Subgroup = treat_var_year) %>%
    mutate("number of observation" = format(`number of observation`, big.mark=",", scientific=F),
           "number of id" = format(`number of id`, big.mark=",", scientific=F))%>%
    select(Subgroup,`number of observation`,`number of id`,`multiple intervention`
    )%>%
    mutate(`multiple intervention`=ifelse(Subgroup=="Control",NA,`multiple intervention`))
  
  filename0 <- paste("02_analysis_output/", target,"_00stat_", count_intervention, ".pdf", sep="")
  tg = gridExtra::tableGrob(dfpolicy_stat)
  h = grid::convertHeight(sum(tg$heights), "in", TRUE)
  w = grid::convertWidth(sum(tg$widths), "in", TRUE)
  ggplot2::ggsave(filename0, tg, width=w, height=h)
  
  ## match ratio -----
  ListLength <- dfpolicy2.1[,c("id")] %>%
    unique() %>%
    length()
  Matched <- dfm0.descriptive %>%
    filter(treat_var=="Treated")
  MatchLength <- Matched[,c("id")] %>%  unique() %>%
    as.list()%>%
    length()
  MatchRatio <- paste(round((MatchLength/ ListLength)*100,2) , "%")
  
  match_table <- 
    data.frame(ListLength, MatchLength,MatchRatio)
  
  filename0 <- paste("02_analysis_output/", target,"_00match_", count_intervention, ".pdf", sep="")
  tg = gridExtra::tableGrob(match_table)
  h = grid::convertHeight(sum(tg$heights), "in", TRUE)
  w = grid::convertWidth(sum(tg$widths), "in", TRUE)
  ggplot2::ggsave(filename0, tg, width=w, height=h)
  
  write.xlsx(list("sample_size"=dfpolicy_stat,"match_result"=match_table), paste("02_analysis_output/",target,"_00_stat_table.xlsx", sep=""))
  
}

## descriptive by category-----------------
descriptive_asset <- function(target){
  ##sum_asset desc----
  limit <- quantile(dfm.descriptive$sum_asset, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$sum_asset, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_sum_asset_all <- ggplot(dfm.descriptive, aes(treat_var, sum_asset, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "sum_asset") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$sum_asset, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_sum_asset_year <- ggplot(dfm.descriptive, aes(treat_var_year, sum_asset, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "sum_asset") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(sum_asset,na.rm = TRUE), median = median(sum_asset,na.rm = TRUE), quantile25 = quantile(sum_asset,0.25,na.rm = TRUE), quantile75 = quantile(sum_asset,0.75,na.rm = TRUE))
  p_time_sum_asset <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("sum_asset")+
    theme_light() + theme(legend.position = "none")
  
  p_time_qntl_sum_asset <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("sum_asset")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_sum_asset <- describeBy(dfm.descriptive$sum_asset,group = dfm.descriptive$treat_var,
                                     mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_sum_asset0 <- (p_sum_asset_all
  ) / wrap_table(desc_table_sum_asset, panel = "full", space = "free")
  p_sum_asset <-  p_sum_asset0 | (p_time_sum_asset + p_time_qntl_sum_asset
  ) + 
    theme_light()
  p_sum_asset
  
  
  ##tangible_asset desc----
  limit <- quantile(dfm.descriptive$tangible_asset, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$tangible_asset, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_tangible_asset_all <- ggplot(dfm.descriptive, aes(treat_var, tangible_asset, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "tangible_asset") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$tangible_asset, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_tangible_asset_year <- ggplot(dfm.descriptive, aes(treat_var_year, tangible_asset, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "tangible_asset") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(tangible_asset,na.rm = TRUE), median = median(tangible_asset,na.rm = TRUE), quantile25 = quantile(tangible_asset,0.25,na.rm = TRUE), quantile75 = quantile(tangible_asset,0.75,na.rm = TRUE))
  p_time_tangible_asset <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("tangible_asset")+
    theme_light() + theme(legend.position = "none")
  
  p_time_qntl_tangible_asset <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("tangible_asset")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_tangible_asset <- describeBy(dfm.descriptive$tangible_asset,group = dfm.descriptive$treat_var,
                                          mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_tangible_asset0 <- (p_tangible_asset_all
  ) / wrap_table(desc_table_tangible_asset, panel = "full", space = "free")
  p_tangible_asset <-  p_tangible_asset0 | (p_time_tangible_asset + p_time_qntl_tangible_asset
  ) + 
    theme_light()
  p_tangible_asset
  
  
  ##intangible_asset desc----
  limit <- quantile(dfm.descriptive$intangible_asset, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$intangible_asset, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_intangible_asset_all <- ggplot(dfm.descriptive, aes(treat_var, intangible_asset, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "intangible_asset") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$intangible_asset, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_intangible_asset_year <- ggplot(dfm.descriptive, aes(treat_var_year, intangible_asset, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "intangible_asset") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(intangible_asset,na.rm = TRUE), median = median(intangible_asset,na.rm = TRUE), quantile25 = quantile(intangible_asset,0.25,na.rm = TRUE), quantile75 = quantile(intangible_asset,0.75,na.rm = TRUE))
  p_time_intangible_asset <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("intangible_asset")+
    theme_light() + theme(legend.position = "none")
  
  p_time_qntl_intangible_asset <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("intangible_asset")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_intangible_asset <- describeBy(dfm.descriptive$intangible_asset,group = dfm.descriptive$treat_var,
                                            mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_intangible_asset0 <- (p_intangible_asset_all
  ) / wrap_table(desc_table_intangible_asset, panel = "full", space = "free")
  p_intangible_asset <-  p_intangible_asset0 | (p_time_intangible_asset + p_time_qntl_intangible_asset
  ) + 
    theme_light()
  p_intangible_asset
  
  
  ##  output_asset desc-----------
  p_asset <- p_sum_asset / p_tangible_asset / p_intangible_asset + plot_annotation(
    title = "Descriptive Statistics of Asset",
    subtitle = target,
    caption = "Thick line is the mean. Thin line is the median. Area bitween 1st and 3rd quartiles is filled."
  ) &
    theme(text = element_text(family="Meiryo UI")) 
  p_asset
  
  filename <- paste("02_analysis_output/", target,"_00descriptive_01_asset.pdf", sep="")
  ggsave(filename,p_asset, 
         width=40, height =24, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("02_analysis_output/", target,"_00descriptive_01_asset.png", sep="")
  ggsave(filename_png,p_asset, 
         width=40, height =24, units = "cm", dpi=200) 
}
descriptive_employment <- function(target){
  ##workers desc----
  limit <- quantile(dfm.descriptive$workers, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$workers, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_workers_all <- ggplot(dfm.descriptive, aes(treat_var, workers, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "workers") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$workers, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_workers_year <- ggplot(dfm.descriptive, aes(treat_var_year, workers, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "workers") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(workers,na.rm = TRUE), median = median(workers,na.rm = TRUE), quantile25 = quantile(workers,0.25,na.rm = TRUE), quantile75 = quantile(workers,0.75,na.rm = TRUE))
  p_time_workers <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("workers")+
    theme_light() + theme(legend.position = "none")
  
  p_time_qntl_workers <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("workers")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_workers <- describeBy(dfm.descriptive$workers,group = dfm.descriptive$treat_var,
                                   mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_workers0 <- (p_workers_all
  ) / wrap_table(desc_table_workers, panel = "full", space = "free")
  p_workers <-  p_workers0 | (p_time_workers + p_time_qntl_workers
  ) + 
    theme_light()
  p_workers
  
  
  ##indefinite_workers desc----
  limit <- quantile(dfm.descriptive$indefinite_workers, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$indefinite_workers, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_indefinite_workers_all <- ggplot(dfm.descriptive, aes(treat_var, indefinite_workers, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "indefinite_workers") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$indefinite_workers, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_indefinite_workers_year <- ggplot(dfm.descriptive, aes(treat_var_year, indefinite_workers, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "indefinite_workers") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(indefinite_workers,na.rm = TRUE), median = median(indefinite_workers,na.rm = TRUE), quantile25 = quantile(indefinite_workers,0.25,na.rm = TRUE), quantile75 = quantile(indefinite_workers,0.75,na.rm = TRUE))
  p_time_indefinite_workers <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("indefinite_workers")+
    theme_light() + theme(legend.position = "none")
  
  p_time_qntl_indefinite_workers <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("indefinite_workers")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_indefinite_workers <- describeBy(dfm.descriptive$indefinite_workers,group = dfm.descriptive$treat_var,
                                              mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_indefinite_workers0 <- (p_indefinite_workers_all
  ) / wrap_table(desc_table_indefinite_workers, panel = "full", space = "free")
  p_indefinite_workers <-  p_indefinite_workers0 | (p_time_indefinite_workers + p_time_qntl_indefinite_workers
  ) + 
    theme_light()
  p_indefinite_workers
  
  
  ##fixedterm_workers desc----
  limit <- quantile(dfm.descriptive$fixedterm_workers, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$fixedterm_workers, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_fixedterm_workers_all <- ggplot(dfm.descriptive, aes(treat_var, fixedterm_workers, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "fixedterm_workers") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$fixedterm_workers, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_fixedterm_workers_year <- ggplot(dfm.descriptive, aes(treat_var_year, fixedterm_workers, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "fixedterm_workers") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(fixedterm_workers,na.rm = TRUE), median = median(fixedterm_workers,na.rm = TRUE), quantile25 = quantile(fixedterm_workers,0.25,na.rm = TRUE), quantile75 = quantile(fixedterm_workers,0.75,na.rm = TRUE))
  p_time_fixedterm_workers <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("fixedterm_workers")+
    theme_light() + theme(legend.position = "none")
  
  p_time_qntl_fixedterm_workers <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("fixedterm_workers")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_fixedterm_workers <- describeBy(dfm.descriptive$fixedterm_workers,group = dfm.descriptive$treat_var,
                                             mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_fixedterm_workers0 <- (p_fixedterm_workers_all
  ) / wrap_table(desc_table_fixedterm_workers, panel = "full", space = "free")
  p_fixedterm_workers <-  p_fixedterm_workers0 | (p_time_fixedterm_workers + p_time_qntl_fixedterm_workers
  ) + 
    theme_light()
  p_fixedterm_workers
  
  
  ##fixedterm_workers_equivalent desc----
  limit <- quantile(dfm.descriptive$fixedterm_workers_equivalent, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$fixedterm_workers_equivalent, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_fixedterm_workers_equivalent_all <- ggplot(dfm.descriptive, aes(treat_var, fixedterm_workers_equivalent, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "fixedterm_workers_equivalent") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$fixedterm_workers_equivalent, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_fixedterm_workers_equivalent_year <- ggplot(dfm.descriptive, aes(treat_var_year, fixedterm_workers_equivalent, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "fixedterm_workers_equivalent") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(fixedterm_workers_equivalent,na.rm = TRUE), median = median(fixedterm_workers_equivalent,na.rm = TRUE), quantile25 = quantile(fixedterm_workers_equivalent,0.25,na.rm = TRUE), quantile75 = quantile(fixedterm_workers_equivalent,0.75,na.rm = TRUE))
  p_time_fixedterm_workers_equivalent <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("fixedterm_workers_equivalent")+
    theme_light() + theme(legend.position = "none")
  
  p_time_qntl_fixedterm_workers_equivalent <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("fixedterm_workers_equivalent")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_fixedterm_workers_equivalent <- describeBy(dfm.descriptive$fixedterm_workers_equivalent,group = dfm.descriptive$treat_var,
                                                        mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_fixedterm_workers_equivalent0 <- (p_fixedterm_workers_equivalent_all
  ) / wrap_table(desc_table_fixedterm_workers_equivalent, panel = "full", space = "free")
  p_fixedterm_workers_equivalent <-  p_fixedterm_workers_equivalent0 | (p_time_fixedterm_workers_equivalent + p_time_qntl_fixedterm_workers_equivalent
  ) + 
    theme_light()
  p_fixedterm_workers_equivalent
  
  
  ##salary desc----
  limit <- quantile(dfm.descriptive$salary, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$salary, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_salary_all <- ggplot(dfm.descriptive, aes(treat_var, salary, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "salary") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$salary, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_salary_year <- ggplot(dfm.descriptive, aes(treat_var_year, salary, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "salary") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(salary,na.rm = TRUE), median = median(salary,na.rm = TRUE), quantile25 = quantile(salary,0.25,na.rm = TRUE), quantile75 = quantile(salary,0.75,na.rm = TRUE))
  p_time_salary <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("salary")+
    theme_light() + theme(legend.position = "none")
  
  p_time_qntl_salary <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("salary")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_salary <- describeBy(dfm.descriptive$salary,group = dfm.descriptive$treat_var,
                                  mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_salary0 <- (p_salary_all
  ) / wrap_table(desc_table_salary, panel = "full", space = "free")
  p_salary <-  p_salary0 | (p_time_salary + p_time_qntl_salary
  ) + 
    theme_light()
  p_salary
  
  
  ##benefit desc----
  limit <- quantile(dfm.descriptive$benefit, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$benefit, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_benefit_all <- ggplot(dfm.descriptive, aes(treat_var, benefit, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "benefit") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$benefit, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_benefit_year <- ggplot(dfm.descriptive, aes(treat_var_year, benefit, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "benefit") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(benefit,na.rm = TRUE), median = median(benefit,na.rm = TRUE), quantile25 = quantile(benefit,0.25,na.rm = TRUE), quantile75 = quantile(benefit,0.75,na.rm = TRUE))
  p_time_benefit <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("benefit")+
    theme_light() + theme(legend.position = "none")
  
  p_time_qntl_benefit <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("benefit")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_benefit <- describeBy(dfm.descriptive$benefit,group = dfm.descriptive$treat_var,
                                   mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_benefit0 <- (p_benefit_all
  ) / wrap_table(desc_table_benefit, panel = "full", space = "free")
  p_benefit <-  p_benefit0 | (p_time_benefit + p_time_qntl_benefit
  ) + 
    theme_light()
  p_benefit
  
  
  ##  output_employment desc-----------
  p_employment <-p_workers/p_indefinite_workers/p_fixedterm_workers/p_fixedterm_workers_equivalent/p_salary/p_benefit + plot_annotation(
    title = "Descriptive Statistics of Employment",
    subtitle = target,
    caption = "Thick line is the mean. Thin line is the median. Area bitween 1st and 3rd quartiles is filled."
  ) &
    theme(text = element_text(family="Meiryo UI")) 
  p_employment
  
  filename <- paste("02_analysis_output/", target,"_00descriptive_02_employment.pdf", sep="")
  ggsave(filename,p_employment, 
         width=40, height =48, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("02_analysis_output/", target,"_00descriptive_02_employment.png", sep="")
  ggsave(filename_png,p_employment, 
         width=40, height =48, units = "cm", dpi=200) 
}
descriptive_business <- function(target){
  ##sales desc----
  limit <- quantile(dfm.descriptive$sales, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$sales, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_sales_all <- ggplot(dfm.descriptive, aes(treat_var, sales, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "sales") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$sales, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_sales_year <- ggplot(dfm.descriptive, aes(treat_var_year, sales, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "sales") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(sales,na.rm = TRUE), median = median(sales,na.rm = TRUE), quantile25 = quantile(sales,0.25,na.rm = TRUE), quantile75 = quantile(sales,0.75,na.rm = TRUE))
  p_time_sales <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("sales")+
    theme_light() + theme(legend.position = "none")
  
  p_time_qntl_sales <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("sales")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_sales <- describeBy(dfm.descriptive$sales,group = dfm.descriptive$treat_var,
                                 mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_sales0 <- (p_sales_all
  ) / wrap_table(desc_table_sales, panel = "full", space = "free")
  p_sales <-  p_sales0 | (p_time_sales + p_time_qntl_sales
  ) + 
    theme_light()
  p_sales
  
  
  ##tax desc----
  limit <- quantile(dfm.descriptive$tax, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$tax, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_tax_all <- ggplot(dfm.descriptive, aes(treat_var, tax, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "tax") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$tax, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_tax_year <- ggplot(dfm.descriptive, aes(treat_var_year, tax, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "tax") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(tax,na.rm = TRUE), median = median(tax,na.rm = TRUE), quantile25 = quantile(tax,0.25,na.rm = TRUE), quantile75 = quantile(tax,0.75,na.rm = TRUE))
  p_time_tax <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("tax")+
    theme_light() + theme(legend.position = "none")
  
  p_time_qntl_tax <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("tax")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_tax <- describeBy(dfm.descriptive$tax,group = dfm.descriptive$treat_var,
                               mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_tax0 <- (p_tax_all
  ) / wrap_table(desc_table_tax, panel = "full", space = "free")
  p_tax <-  p_tax0 | (p_time_tax + p_time_qntl_tax
  ) + 
    theme_light()
  p_tax
  
  
  ##office desc----
  limit <- quantile(dfm.descriptive$office, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$office, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_office_all <- ggplot(dfm.descriptive, aes(treat_var, office, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "office") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$office, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_office_year <- ggplot(dfm.descriptive, aes(treat_var_year, office, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "office") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(office,na.rm = TRUE), median = median(office,na.rm = TRUE), quantile25 = quantile(office,0.25,na.rm = TRUE), quantile75 = quantile(office,0.75,na.rm = TRUE))
  p_time_office <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("office")+
    theme_light() + theme(legend.position = "none")
  
  p_time_qntl_office <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("office")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_office <- describeBy(dfm.descriptive$office,group = dfm.descriptive$treat_var,
                                  mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_office0 <- (p_office_all
  ) / wrap_table(desc_table_office, panel = "full", space = "free")
  p_office <-  p_office0 | (p_time_office + p_time_qntl_office
  ) + 
    theme_light()
  p_office
  
  
  ##ROA desc----
  limit <- quantile(dfm.descriptive$ROA, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$ROA, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_ROA_all <- ggplot(dfm.descriptive, aes(treat_var, ROA, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "ROA") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$ROA, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_ROA_year <- ggplot(dfm.descriptive, aes(treat_var_year, ROA, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "ROA") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(ROA,na.rm = TRUE), median = median(ROA,na.rm = TRUE), quantile25 = quantile(ROA,0.25,na.rm = TRUE), quantile75 = quantile(ROA,0.75,na.rm = TRUE))
  p_time_ROA <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("ROA")+
    theme_light() + theme(legend.position = "none")
  
  p_time_qntl_ROA <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("ROA")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_ROA <- describeBy(dfm.descriptive$ROA,group = dfm.descriptive$treat_var,
                               mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_ROA0 <- (p_ROA_all
  ) / wrap_table(desc_table_ROA, panel = "full", space = "free")
  p_ROA <-  p_ROA0 | (p_time_ROA + p_time_qntl_ROA
  ) + 
    theme_light()
  p_ROA
  
  
  ##net_profit_workers desc----
  limit <- quantile(dfm.descriptive$net_profit_workers, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$net_profit_workers, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_net_profit_workers_all <- ggplot(dfm.descriptive, aes(treat_var, net_profit_workers, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "net_profit_workers") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$net_profit_workers, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_net_profit_workers_year <- ggplot(dfm.descriptive, aes(treat_var_year, net_profit_workers, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "net_profit_workers") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(net_profit_workers,na.rm = TRUE), median = median(net_profit_workers,na.rm = TRUE), quantile25 = quantile(net_profit_workers,0.25,na.rm = TRUE), quantile75 = quantile(net_profit_workers,0.75,na.rm = TRUE))
  p_time_net_profit_workers <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("net_profit_workers")+
    theme_light() + theme(legend.position = "none")
  
  p_time_qntl_net_profit_workers <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("net_profit_workers")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_net_profit_workers <- describeBy(dfm.descriptive$net_profit_workers,group = dfm.descriptive$treat_var,
                                              mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_net_profit_workers0 <- (p_net_profit_workers_all
  ) / wrap_table(desc_table_net_profit_workers, panel = "full", space = "free")
  p_net_profit_workers <-  p_net_profit_workers0 | (p_time_net_profit_workers + p_time_qntl_net_profit_workers
  ) + 
    theme_light()
  p_net_profit_workers
  
  
  ##  output_business desc-----------
  p_business <-p_sales/p_tax/p_office/p_ROA/p_net_profit_workers + plot_annotation(
    title = "Descriptive Statistics of Business",
    subtitle = target,
    caption = "Thick line is the mean. Thin line is the median. Area bitween 1st and 3rd quartiles is filled."
  ) &
    theme(text = element_text(family="Meiryo UI")) 
  p_business
  
  filename <- paste("02_analysis_output/", target,"_00descriptive_03_business.pdf", sep="")
  ggsave(filename,p_business, 
         width=40, height =40, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("02_analysis_output/", target,"_00descriptive_03_business.png", sep="")
  ggsave(filename_png,p_business, 
         width=40, height =40, units = "cm", dpi=200) 
}
descriptive_trade <- function(target){
  ##flag_export desc----
  limit <- quantile(dfm.descriptive$flag_export, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$flag_export, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_flag_export_all <- ggplot(dfm.descriptive, aes(treat_var, flag_export, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "flag_export") +
    #geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$flag_export, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_flag_export_year <- ggplot(dfm.descriptive, aes(treat_var_year, flag_export, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "flag_export") +
    #geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(flag_export,na.rm = TRUE), median = median(flag_export,na.rm = TRUE), quantile25 = quantile(flag_export,0.25,na.rm = TRUE), quantile75 = quantile(flag_export,0.75,na.rm = TRUE))
  p_time_flag_export <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("flag_export")+
    theme_light() + #theme(legend.position = "none")
    
    #+ p_time_qntlflag_export <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("flag_export")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_flag_export <- describeBy(dfm.descriptive$flag_export,group = dfm.descriptive$treat_var,
                                       mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_flag_export0 <- (p_flag_export_all
  ) / wrap_table(desc_table_flag_export, panel = "full", space = "free")
  p_flag_export <-  p_flag_export0 | (p_time_flag_export + plot_spacer() #+ #+ p_time_qntlflag_RDflag_export
  ) + 
    theme_light()
  p_flag_export
  
  
  ##flag_import desc----
  limit <- quantile(dfm.descriptive$flag_import, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$flag_import, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_flag_import_all <- ggplot(dfm.descriptive, aes(treat_var, flag_import, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "flag_import") +
    #geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$flag_import, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_flag_import_year <- ggplot(dfm.descriptive, aes(treat_var_year, flag_import, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "flag_import") +
    #geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(flag_import,na.rm = TRUE), median = median(flag_import,na.rm = TRUE), quantile25 = quantile(flag_import,0.25,na.rm = TRUE), quantile75 = quantile(flag_import,0.75,na.rm = TRUE))
  p_time_flag_import <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("flag_import")+
    theme_light() + #theme(legend.position = "none")
    
    #+ p_time_qntlflag_import <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("flag_import")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_flag_import <- describeBy(dfm.descriptive$flag_import,group = dfm.descriptive$treat_var,
                                       mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_flag_import0 <- (p_flag_import_all
  ) / wrap_table(desc_table_flag_import, panel = "full", space = "free")
  p_flag_import <-  p_flag_import0 | (p_time_flag_import + plot_spacer() #+ #+ p_time_qntlflag_RDflag_import
  ) + 
    theme_light()
  p_flag_import
  
  
  ##export desc----
  limit <- quantile(dfm.descriptive$export, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$export, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_export_all <- ggplot(dfm.descriptive, aes(treat_var, export, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "export") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$export, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_export_year <- ggplot(dfm.descriptive, aes(treat_var_year, export, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "export") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(export,na.rm = TRUE), median = median(export,na.rm = TRUE), quantile25 = quantile(export,0.25,na.rm = TRUE), quantile75 = quantile(export,0.75,na.rm = TRUE))
  p_time_export <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("export")+
    theme_light() + theme(legend.position = "none")
  
  p_time_qntl_export <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("export")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_export <- describeBy(dfm.descriptive$export,group = dfm.descriptive$treat_var,
                                  mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_export0 <- (p_export_all
  ) / wrap_table(desc_table_export, panel = "full", space = "free")
  p_export <-  p_export0 | (p_time_export + p_time_qntl_export
  ) + 
    theme_light()
  p_export
  
  
  ##import desc----
  limit <- quantile(dfm.descriptive$import, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$import, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_import_all <- ggplot(dfm.descriptive, aes(treat_var, import, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "import") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$import, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_import_year <- ggplot(dfm.descriptive, aes(treat_var_year, import, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "import") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(import,na.rm = TRUE), median = median(import,na.rm = TRUE), quantile25 = quantile(import,0.25,na.rm = TRUE), quantile75 = quantile(import,0.75,na.rm = TRUE))
  p_time_import <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("import")+
    theme_light() + theme(legend.position = "none")
  
  p_time_qntl_import <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("import")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_import <- describeBy(dfm.descriptive$import,group = dfm.descriptive$treat_var,
                                  mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_import0 <- (p_import_all
  ) / wrap_table(desc_table_import, panel = "full", space = "free")
  p_import <-  p_import0 | (p_time_import + p_time_qntl_import
  ) + 
    theme_light()
  p_import
  
  
  ##  output_trade desc-----------
  p_trade <-p_flag_export/p_flag_import/p_export/p_import + plot_annotation(
    title = "Descriptive Statistics of International Trade",
    subtitle = target,
    caption = "Thick line is the mean. Thin line is the median. Area bitween 1st and 3rd quartiles is filled."
  ) &
    theme(text = element_text(family="Meiryo UI")) 
  p_trade
  
  filename <- paste("02_analysis_output/", target,"_00descriptive_04_trade.pdf", sep="")
  ggsave(filename,p_trade, 
         width=40, height =32, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("02_analysis_output/", target,"_00descriptive_04_trade.png", sep="")
  ggsave(filename_png,p_trade, 
         width=40, height =32, units = "cm", dpi=200) 
}
descriptive_trainingRD <- function(target){
  ##flag_training desc----
  limit <- quantile(dfm.descriptive$flag_training, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$flag_training, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_flag_training_all <- ggplot(dfm.descriptive, aes(treat_var, flag_training, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "flag_training") +
    #geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$flag_training, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_flag_training_year <- ggplot(dfm.descriptive, aes(treat_var_year, flag_training, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "flag_training") +
    #geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(flag_training,na.rm = TRUE), median = median(flag_training,na.rm = TRUE), quantile25 = quantile(flag_training,0.25,na.rm = TRUE), quantile75 = quantile(flag_training,0.75,na.rm = TRUE))
  p_time_flag_training <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("flag_training")+
    theme_light() + #theme(legend.position = "none")
    
    #+ p_time_qntlflag_training <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("flag_training")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_flag_training <- describeBy(dfm.descriptive$flag_training,group = dfm.descriptive$treat_var,
                                         mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_flag_training0 <- (p_flag_training_all
  ) / wrap_table(desc_table_flag_training, panel = "full", space = "free")
  p_flag_training <-  p_flag_training0 | (p_time_flag_training + plot_spacer() #+ #+ p_time_qntlflag_RDflag_training
  ) + 
    theme_light()
  p_flag_training
  
  
  ##flag_RD desc----
  limit <- quantile(dfm.descriptive$flag_RD, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$flag_RD, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_flag_RD_all <- ggplot(dfm.descriptive, aes(treat_var, flag_RD, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "flag_RD") +
    #geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$flag_RD, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_flag_RD_year <- ggplot(dfm.descriptive, aes(treat_var_year, flag_RD, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "flag_RD") +
    #geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(flag_RD,na.rm = TRUE), median = median(flag_RD,na.rm = TRUE), quantile25 = quantile(flag_RD,0.25,na.rm = TRUE), quantile75 = quantile(flag_RD,0.75,na.rm = TRUE))
  p_time_flag_RD <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("flag_RD")+
    theme_light() + #theme(legend.position = "none")
    
    #+ p_time_qntlflag_RD <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("flag_RD")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_flag_RD <- describeBy(dfm.descriptive$flag_RD,group = dfm.descriptive$treat_var,
                                   mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_flag_RD0 <- (p_flag_RD_all
  ) / wrap_table(desc_table_flag_RD, panel = "full", space = "free")
  p_flag_RD <-  p_flag_RD0 | (p_time_flag_RD + plot_spacer() #+ #+ p_time_qntlflag_RDflag_RD
  ) + 
    theme_light()
  p_flag_RD
  
  
  ##training desc----
  limit <- quantile(dfm.descriptive$training, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$training, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_training_all <- ggplot(dfm.descriptive, aes(treat_var, training, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "training") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$training, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_training_year <- ggplot(dfm.descriptive, aes(treat_var_year, training, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "training") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(training,na.rm = TRUE), median = median(training,na.rm = TRUE), quantile25 = quantile(training,0.25,na.rm = TRUE), quantile75 = quantile(training,0.75,na.rm = TRUE))
  p_time_training <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("training")+
    theme_light() + theme(legend.position = "none")
  
  p_time_qntl_training <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("training")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_training <- describeBy(dfm.descriptive$training,group = dfm.descriptive$treat_var,
                                    mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_training0 <- (p_training_all
  ) / wrap_table(desc_table_training, panel = "full", space = "free")
  p_training <-  p_training0 | (p_time_training + p_time_qntl_training
  ) + 
    theme_light()
  p_training
  
  
  ##RD desc----
  limit <- quantile(dfm.descriptive$RD, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$RD, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_RD_all <- ggplot(dfm.descriptive, aes(treat_var, RD, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "RD") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$RD, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_RD_year <- ggplot(dfm.descriptive, aes(treat_var_year, RD, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "RD") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(RD,na.rm = TRUE), median = median(RD,na.rm = TRUE), quantile25 = quantile(RD,0.25,na.rm = TRUE), quantile75 = quantile(RD,0.75,na.rm = TRUE))
  p_time_RD <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("RD")+
    theme_light() + theme(legend.position = "none")
  
  p_time_qntl_RD <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("RD")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_RD <- describeBy(dfm.descriptive$RD,group = dfm.descriptive$treat_var,
                              mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_RD0 <- (p_RD_all
  ) / wrap_table(desc_table_RD, panel = "full", space = "free")
  p_RD <-  p_RD0 | (p_time_RD + p_time_qntl_RD
  ) + 
    theme_light()
  p_RD
  
  
  ##  output_trainingRD desc-----------
  p_trainingRD <-p_flag_training/p_flag_RD/p_training/p_RD + plot_annotation(
    title = "Descriptive Statistics of Training and RD",
    subtitle = target,
    caption = "Thick line is the mean. Thin line is the median. Area bitween 1st and 3rd quartiles is filled."
  ) &
    theme(text = element_text(family="Meiryo UI")) 
  p_trainingRD
  
  filename <- paste("02_analysis_output/", target,"_00descriptive_05_trainingRD.pdf", sep="")
  ggsave(filename,p_trainingRD, 
         width=40, height =32, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("02_analysis_output/", target,"_00descriptive_05_trainingRD.png", sep="")
  ggsave(filename_png,p_trainingRD, 
         width=40, height =32, units = "cm", dpi=200) 
}
descriptive_IP <- function(target){
  ##flag_patent desc----
  limit <- quantile(dfm.descriptive$flag_patent, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$flag_patent, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_flag_patent_all <- ggplot(dfm.descriptive, aes(treat_var, flag_patent, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "flag_patent") +
    #geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$flag_patent, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_flag_patent_year <- ggplot(dfm.descriptive, aes(treat_var_year, flag_patent, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "flag_patent") +
    #geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(flag_patent,na.rm = TRUE), median = median(flag_patent,na.rm = TRUE), quantile25 = quantile(flag_patent,0.25,na.rm = TRUE), quantile75 = quantile(flag_patent,0.75,na.rm = TRUE))
  p_time_flag_patent <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("flag_patent")+
    theme_light() + #theme(legend.position = "none")
    
    #+ p_time_qntlflag_patent <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("flag_patent")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_flag_patent <- describeBy(dfm.descriptive$flag_patent,group = dfm.descriptive$treat_var,
                                       mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_flag_patent0 <- (p_flag_patent_all
  ) / wrap_table(desc_table_flag_patent, panel = "full", space = "free")
  p_flag_patent <-  p_flag_patent0 | (p_time_flag_patent + plot_spacer() #+ #+ p_time_qntlflag_RDflag_patent
  ) + 
    theme_light()
  p_flag_patent
  
  
  ##flag_jitsuyo desc----
  limit <- quantile(dfm.descriptive$flag_jitsuyo, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$flag_jitsuyo, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_flag_jitsuyo_all <- ggplot(dfm.descriptive, aes(treat_var, flag_jitsuyo, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "flag_jitsuyo") +
    #geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$flag_jitsuyo, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_flag_jitsuyo_year <- ggplot(dfm.descriptive, aes(treat_var_year, flag_jitsuyo, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "flag_jitsuyo") +
    #geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(flag_jitsuyo,na.rm = TRUE), median = median(flag_jitsuyo,na.rm = TRUE), quantile25 = quantile(flag_jitsuyo,0.25,na.rm = TRUE), quantile75 = quantile(flag_jitsuyo,0.75,na.rm = TRUE))
  p_time_flag_jitsuyo <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("flag_jitsuyo")+
    theme_light() + #theme(legend.position = "none")
    
    #+ p_time_qntlflag_jitsuyo <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("flag_jitsuyo")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_flag_jitsuyo <- describeBy(dfm.descriptive$flag_jitsuyo,group = dfm.descriptive$treat_var,
                                        mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_flag_jitsuyo0 <- (p_flag_jitsuyo_all
  ) / wrap_table(desc_table_flag_jitsuyo, panel = "full", space = "free")
  p_flag_jitsuyo <-  p_flag_jitsuyo0 | (p_time_flag_jitsuyo + plot_spacer() #+ #+ p_time_qntlflag_RDflag_jitsuyo
  ) + 
    theme_light()
  p_flag_jitsuyo
  
  
  ##flag_isho desc----
  limit <- quantile(dfm.descriptive$flag_isho, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$flag_isho, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_flag_isho_all <- ggplot(dfm.descriptive, aes(treat_var, flag_isho, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "flag_isho") +
    #geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$flag_isho, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_flag_isho_year <- ggplot(dfm.descriptive, aes(treat_var_year, flag_isho, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "flag_isho") +
    #geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(flag_isho,na.rm = TRUE), median = median(flag_isho,na.rm = TRUE), quantile25 = quantile(flag_isho,0.25,na.rm = TRUE), quantile75 = quantile(flag_isho,0.75,na.rm = TRUE))
  p_time_flag_isho <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("flag_isho")+
    theme_light() + #theme(legend.position = "none")
    
    #+ p_time_qntlflag_isho <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("flag_isho")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_flag_isho <- describeBy(dfm.descriptive$flag_isho,group = dfm.descriptive$treat_var,
                                     mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_flag_isho0 <- (p_flag_isho_all
  ) / wrap_table(desc_table_flag_isho, panel = "full", space = "free")
  p_flag_isho <-  p_flag_isho0 | (p_time_flag_isho + plot_spacer() #+ #+ p_time_qntlflag_RDflag_isho
  ) + 
    theme_light()
  p_flag_isho
  
  
  ##patent desc----
  limit <- quantile(dfm.descriptive$patent, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$patent, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_patent_all <- ggplot(dfm.descriptive, aes(treat_var, patent, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "patent") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$patent, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_patent_year <- ggplot(dfm.descriptive, aes(treat_var_year, patent, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "patent") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(patent,na.rm = TRUE), median = median(patent,na.rm = TRUE), quantile25 = quantile(patent,0.25,na.rm = TRUE), quantile75 = quantile(patent,0.75,na.rm = TRUE))
  p_time_patent <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("patent")+
    theme_light() + theme(legend.position = "none")
  
  p_time_qntl_patent <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("patent")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_patent <- describeBy(dfm.descriptive$patent,group = dfm.descriptive$treat_var,
                                  mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_patent0 <- (p_patent_all
  ) / wrap_table(desc_table_patent, panel = "full", space = "free")
  p_patent <-  p_patent0 | (p_time_patent + p_time_qntl_patent
  ) + 
    theme_light()
  p_patent
  
  
  ##jitsuyo desc----
  limit <- quantile(dfm.descriptive$jitsuyo, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$jitsuyo, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_jitsuyo_all <- ggplot(dfm.descriptive, aes(treat_var, jitsuyo, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "jitsuyo") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$jitsuyo, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_jitsuyo_year <- ggplot(dfm.descriptive, aes(treat_var_year, jitsuyo, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "jitsuyo") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(jitsuyo,na.rm = TRUE), median = median(jitsuyo,na.rm = TRUE), quantile25 = quantile(jitsuyo,0.25,na.rm = TRUE), quantile75 = quantile(jitsuyo,0.75,na.rm = TRUE))
  p_time_jitsuyo <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("jitsuyo")+
    theme_light() + theme(legend.position = "none")
  
  p_time_qntl_jitsuyo <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("jitsuyo")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_jitsuyo <- describeBy(dfm.descriptive$jitsuyo,group = dfm.descriptive$treat_var,
                                   mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_jitsuyo0 <- (p_jitsuyo_all
  ) / wrap_table(desc_table_jitsuyo, panel = "full", space = "free")
  p_jitsuyo <-  p_jitsuyo0 | (p_time_jitsuyo + p_time_qntl_jitsuyo
  ) + 
    theme_light()
  p_jitsuyo
  
  
  ##isho desc----
  limit <- quantile(dfm.descriptive$isho, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$isho, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_isho_all <- ggplot(dfm.descriptive, aes(treat_var, isho, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "isho") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$isho, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_isho_year <- ggplot(dfm.descriptive, aes(treat_var_year, isho, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "isho") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(isho,na.rm = TRUE), median = median(isho,na.rm = TRUE), quantile25 = quantile(isho,0.25,na.rm = TRUE), quantile75 = quantile(isho,0.75,na.rm = TRUE))
  p_time_isho <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("isho")+
    theme_light() + theme(legend.position = "none")
  
  p_time_qntl_isho <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("isho")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_isho <- describeBy(dfm.descriptive$isho,group = dfm.descriptive$treat_var,
                                mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_isho0 <- (p_isho_all
  ) / wrap_table(desc_table_isho, panel = "full", space = "free")
  p_isho <-  p_isho0 | (p_time_isho + p_time_qntl_isho
  ) + 
    theme_light()
  p_isho
  
  
  ##  output_IP desc-----------
  p_IP <-p_flag_patent/p_flag_jitsuyo/p_flag_isho/p_patent/p_jitsuyo/p_isho + plot_annotation(
    title = "Descriptive Statistics of Intellectual Property",
    subtitle = target,
    caption = "Thick line is the mean. Thin line is the median. Area bitween 1st and 3rd quartiles is filled."
  ) &
    theme(text = element_text(family="Meiryo UI")) 
  p_IP
  
  filename <- paste("02_analysis_output/", target,"_00descriptive_06_IP.pdf", sep="")
  ggsave(filename,p_IP, 
         width=40, height =48, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("02_analysis_output/", target,"_00descriptive_06_IP.png", sep="")
  ggsave(filename_png,p_IP, 
         width=40, height =48, units = "cm", dpi=200) 
}
descriptive_investment <- function(target){
  ##flag_investment_affiliate_domestic desc----
  limit <- quantile(dfm.descriptive$flag_investment_affiliate_domestic, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$flag_investment_affiliate_domestic, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_flag_investment_affiliate_domestic_all <- ggplot(dfm.descriptive, aes(treat_var, flag_investment_affiliate_domestic, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "flag_investment_affiliate_domestic") +
    #geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$flag_investment_affiliate_domestic, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_flag_investment_affiliate_domestic_year <- ggplot(dfm.descriptive, aes(treat_var_year, flag_investment_affiliate_domestic, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "flag_investment_affiliate_domestic") +
    #geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(flag_investment_affiliate_domestic,na.rm = TRUE), median = median(flag_investment_affiliate_domestic,na.rm = TRUE), quantile25 = quantile(flag_investment_affiliate_domestic,0.25,na.rm = TRUE), quantile75 = quantile(flag_investment_affiliate_domestic,0.75,na.rm = TRUE))
  p_time_flag_investment_affiliate_domestic <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("flag_investment_affiliate_domestic")+
    theme_light() + #theme(legend.position = "none")
    
    #+ p_time_qntlflag_investment_affiliate_domestic <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("flag_investment_affiliate_domestic")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_flag_investment_affiliate_domestic <- describeBy(dfm.descriptive$flag_investment_affiliate_domestic,group = dfm.descriptive$treat_var,
                                                              mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_flag_investment_affiliate_domestic0 <- (p_flag_investment_affiliate_domestic_all
  ) / wrap_table(desc_table_flag_investment_affiliate_domestic, panel = "full", space = "free")
  p_flag_investment_affiliate_domestic <-  p_flag_investment_affiliate_domestic0 | (p_time_flag_investment_affiliate_domestic + plot_spacer() #+ #+ p_time_qntlflag_RDflag_investment_affiliate_domestic
  ) + 
    theme_light()
  p_flag_investment_affiliate_domestic
  
  
  ##flag_investment_affiliate_overseas desc----
  limit <- quantile(dfm.descriptive$flag_investment_affiliate_overseas, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$flag_investment_affiliate_overseas, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_flag_investment_affiliate_overseas_all <- ggplot(dfm.descriptive, aes(treat_var, flag_investment_affiliate_overseas, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "flag_investment_affiliate_overseas") +
    #geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$flag_investment_affiliate_overseas, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_flag_investment_affiliate_overseas_year <- ggplot(dfm.descriptive, aes(treat_var_year, flag_investment_affiliate_overseas, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "flag_investment_affiliate_overseas") +
    #geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(flag_investment_affiliate_overseas,na.rm = TRUE), median = median(flag_investment_affiliate_overseas,na.rm = TRUE), quantile25 = quantile(flag_investment_affiliate_overseas,0.25,na.rm = TRUE), quantile75 = quantile(flag_investment_affiliate_overseas,0.75,na.rm = TRUE))
  p_time_flag_investment_affiliate_overseas <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("flag_investment_affiliate_overseas")+
    theme_light() + #theme(legend.position = "none")
    
    #+ p_time_qntlflag_investment_affiliate_overseas <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("flag_investment_affiliate_overseas")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_flag_investment_affiliate_overseas <- describeBy(dfm.descriptive$flag_investment_affiliate_overseas,group = dfm.descriptive$treat_var,
                                                              mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_flag_investment_affiliate_overseas0 <- (p_flag_investment_affiliate_overseas_all
  ) / wrap_table(desc_table_flag_investment_affiliate_overseas, panel = "full", space = "free")
  p_flag_investment_affiliate_overseas <-  p_flag_investment_affiliate_overseas0 | (p_time_flag_investment_affiliate_overseas + plot_spacer() #+ #+ p_time_qntlflag_RDflag_investment_affiliate_overseas
  ) + 
    theme_light()
  p_flag_investment_affiliate_overseas
  
  
  ##flag_dividend desc----
  limit <- quantile(dfm.descriptive$flag_dividend, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$flag_dividend, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_flag_dividend_all <- ggplot(dfm.descriptive, aes(treat_var, flag_dividend, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "flag_dividend") +
    #geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$flag_dividend, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_flag_dividend_year <- ggplot(dfm.descriptive, aes(treat_var_year, flag_dividend, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "flag_dividend") +
    #geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(flag_dividend,na.rm = TRUE), median = median(flag_dividend,na.rm = TRUE), quantile25 = quantile(flag_dividend,0.25,na.rm = TRUE), quantile75 = quantile(flag_dividend,0.75,na.rm = TRUE))
  p_time_flag_dividend <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("flag_dividend")+
    theme_light() + #theme(legend.position = "none")
    
    #+ p_time_qntlflag_dividend <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("flag_dividend")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_flag_dividend <- describeBy(dfm.descriptive$flag_dividend,group = dfm.descriptive$treat_var,
                                         mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_flag_dividend0 <- (p_flag_dividend_all
  ) / wrap_table(desc_table_flag_dividend, panel = "full", space = "free")
  p_flag_dividend <-  p_flag_dividend0 | (p_time_flag_dividend + plot_spacer() #+ #+ p_time_qntlflag_RDflag_dividend
  ) + 
    theme_light()
  p_flag_dividend
  
  
  ##investment_affiliate_domestic desc----
  limit <- quantile(dfm.descriptive$investment_affiliate_domestic, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$investment_affiliate_domestic, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_investment_affiliate_domestic_all <- ggplot(dfm.descriptive, aes(treat_var, investment_affiliate_domestic, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "investment_affiliate_domestic") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$investment_affiliate_domestic, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_investment_affiliate_domestic_year <- ggplot(dfm.descriptive, aes(treat_var_year, investment_affiliate_domestic, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "investment_affiliate_domestic") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(investment_affiliate_domestic,na.rm = TRUE), median = median(investment_affiliate_domestic,na.rm = TRUE), quantile25 = quantile(investment_affiliate_domestic,0.25,na.rm = TRUE), quantile75 = quantile(investment_affiliate_domestic,0.75,na.rm = TRUE))
  p_time_investment_affiliate_domestic <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("investment_affiliate_domestic")+
    theme_light() + theme(legend.position = "none")
  
  p_time_qntl_investment_affiliate_domestic <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("investment_affiliate_domestic")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_investment_affiliate_domestic <- describeBy(dfm.descriptive$investment_affiliate_domestic,group = dfm.descriptive$treat_var,
                                                         mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_investment_affiliate_domestic0 <- (p_investment_affiliate_domestic_all
  ) / wrap_table(desc_table_investment_affiliate_domestic, panel = "full", space = "free")
  p_investment_affiliate_domestic <-  p_investment_affiliate_domestic0 | (p_time_investment_affiliate_domestic + p_time_qntl_investment_affiliate_domestic
  ) + 
    theme_light()
  p_investment_affiliate_domestic
  
  
  ##investment_affiliate_overseas desc----
  limit <- quantile(dfm.descriptive$investment_affiliate_overseas, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$investment_affiliate_overseas, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_investment_affiliate_overseas_all <- ggplot(dfm.descriptive, aes(treat_var, investment_affiliate_overseas, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "investment_affiliate_overseas") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$investment_affiliate_overseas, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_investment_affiliate_overseas_year <- ggplot(dfm.descriptive, aes(treat_var_year, investment_affiliate_overseas, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "investment_affiliate_overseas") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(investment_affiliate_overseas,na.rm = TRUE), median = median(investment_affiliate_overseas,na.rm = TRUE), quantile25 = quantile(investment_affiliate_overseas,0.25,na.rm = TRUE), quantile75 = quantile(investment_affiliate_overseas,0.75,na.rm = TRUE))
  p_time_investment_affiliate_overseas <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("investment_affiliate_overseas")+
    theme_light() + theme(legend.position = "none")
  
  p_time_qntl_investment_affiliate_overseas <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("investment_affiliate_overseas")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_investment_affiliate_overseas <- describeBy(dfm.descriptive$investment_affiliate_overseas,group = dfm.descriptive$treat_var,
                                                         mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_investment_affiliate_overseas0 <- (p_investment_affiliate_overseas_all
  ) / wrap_table(desc_table_investment_affiliate_overseas, panel = "full", space = "free")
  p_investment_affiliate_overseas <-  p_investment_affiliate_overseas0 | (p_time_investment_affiliate_overseas + p_time_qntl_investment_affiliate_overseas
  ) + 
    theme_light()
  p_investment_affiliate_overseas
  
  
  ##dividend desc----
  limit <- quantile(dfm.descriptive$dividend, c(0.1, 0.9), na.rm=TRUE)
  meanlab <- aggregate(dfm.descriptive$dividend, list(dfm.descriptive$treat_var), mean)
  xlabs <- paste(meanlab[,1],#"\n",format(round(meanlab[,2],3), big.mark=",", scientific=F),
                 "\n(N=",table(dfm.descriptive$treat),")",sep="")
  
  p_dividend_all <- ggplot(dfm.descriptive, aes(treat_var, dividend, fill = treat_var)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "dividend") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  meanlab_year <- aggregate(dfm.descriptive$dividend, list(dfm.descriptive$treat_var_year), mean)
  xlabs_year <- meanlab_year[,1]
  p_dividend_year <- ggplot(dfm.descriptive, aes(treat_var_year, dividend, fill = treat_var_year)) + # x軸にgroup、y軸にweight、fillで塗りつぶし
    labs(title = "dividend") +
    geom_violin(scale = "area",adjust =1 , bounds = c(min(limit[1],0), limit[2])) + # まずバイオリンプロット
    geom_boxplot(outlier.shape = NA, width = 0.2, fill = "white",alpha =0.2) + # その上に箱ひげ図
    stat_summary(geom = "point", fun.y = mean, shape = "diamond", color = "black", size = 3)+ # 平均値を追加
    coord_cartesian(ylim = c(min(limit[1],0), limit[2]+0.001))+
    scale_x_discrete(labels=xlabs_year)+
    theme_light()+   theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")
  
  dfm_kikatsu_time_temp <- rbind(dfm_kikatsu_time0,dfm_kikatsu_time1)%>%
    group_by(year, treat_var)%>%
    summarize(mean = mean(dividend,na.rm = TRUE), median = median(dividend,na.rm = TRUE), quantile25 = quantile(dividend,0.25,na.rm = TRUE), quantile75 = quantile(dividend,0.75,na.rm = TRUE))
  p_time_dividend <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +
    geom_line(aes(y=mean, color =treat_var), linewidth = 1.5)+expand_limits(y = 0) +
    ggtitle("dividend")+
    theme_light() + theme(legend.position = "none")
  
  p_time_qntl_dividend <- ggplot(data = dfm_kikatsu_time_temp, aes(year, group=treat_var)) +geom_ribbon(aes(ymin = quantile25, ymax = quantile75, fill = treat_var), alpha = 0.2)+geom_line(aes(y=median, color =treat_var), linewidth = 0.5)+
    expand_limits(y = 0) +ggtitle("dividend")+theme_light() #+ theme(legend.position = "none")
  
  desc_table_dividend <- describeBy(dfm.descriptive$dividend,group = dfm.descriptive$treat_var,
                                    mat = TRUE)  %>%
    mutate(
      mean   = formatC(mean,   format = "e", digits = 3),
      sd     = formatC(sd,     format = "e", digits = 3),
      median = formatC(median, format = "e", digits = 3) # 有効数字を設定
    )%>%
    select(c(group1, n, mean, sd, median#, min, max
    )) %>%
    rename(group = group1)
  
  p_dividend0 <- (p_dividend_all
  ) / wrap_table(desc_table_dividend, panel = "full", space = "free")
  p_dividend <-  p_dividend0 | (p_time_dividend + p_time_qntl_dividend
  ) + 
    theme_light()
  p_dividend
  
  
  ##  output_investment desc-----------
  p_investment <-p_flag_investment_affiliate_domestic/p_flag_investment_affiliate_overseas/p_flag_dividend/p_investment_affiliate_domestic/p_investment_affiliate_overseas/p_dividend + plot_annotation(
    title = "Descriptive Statistics of Investment",
    subtitle = target,
    caption = "Thick line is the mean. Thin line is the median. Area bitween 1st and 3rd quartiles is filled."
  ) &
    theme(text = element_text(family="Meiryo UI")) 
  p_investment
  
  filename <- paste("02_analysis_output/", target,"_00descriptive_07_investment.pdf", sep="")
  ggsave(filename,p_investment, 
         width=40, height =48, units = "cm", dpi=200, device = cairo_pdf) 
  filename_png <- paste("02_analysis_output/", target,"_00descriptive_07_investment.png", sep="")
  ggsave(filename_png,p_investment, 
         width=40, height =48, units = "cm", dpi=200) 
}




descriptive_covariate <- function(target){
  desc_table_covariate0 <- dfm.descriptive %>% select(treat_var, age, sales, log_sales2, log_sales_diff, 
                                                      ROA, ROA2, ROA_diff, 
                                                      net_profit_workers, net_profit_workers2, net_profit_workers_diff, 
                                                      salary, log_salary2, log_salary_diff, 
                                                      sum_asset, log_sum_asset2, log_sum_asset_diff,  
                                                      workers,  log_workers2, log_workers_diff, 
                                                      office,log_office2, log_office_diff
                                                      #industry_code,subsidiary,parent,pref
  ) %>%
    describeBy(group = dfm.descriptive$treat_var,  mat = TRUE, digits =2
    ) %>%
    select(c(group1, n, mean, sd, median#, min, max
             )) %>%
    rename(group = group1)
  desc_table_covariate <- desc_table_covariate0 %>%
    mutate(variable = rownames(desc_table_covariate0)) %>%
    select(variable, everything()) %>%
    mutate(variable = str_sub(variable, end = -2))
  
  desc_table_described0 <- dfm.descriptive %>% select(treat_var, 
                                                      log_sum_asset, log_tangible_asset, log_intangible_asset, log_workers, log_indefinite_workers
                                                      ,log_fixedterm_workers, log_fixedterm_workers_equivalent, log_salary
                                                      ,log_benefit, log_sales, log_tax, log_office, ROA, net_profit_workers
                                                      ,flag_export, flag_import, log_export, log_import, flag_training, flag_RD
                                                      ,log_training, log_RD, flag_patent, flag_jitsuyo, flag_isho
                                                      ,log_patent, log_jitsuyo, log_isho
                                                      ,flag_investment_affiliate_domestic, flag_investment_affiliate_overseas
                                                      ,flag_dividend, log_investment_affiliate_domestic, log_investment_affiliate_overseas
                                                      ,log_dividend) %>%
    describeBy(group = dfm.descriptive$treat_var,  mat = TRUE, digits =2
    ) %>%
    select(c(group1, n, mean, sd, median, min, max)) %>%
    rename(group = group1)
  desc_table_described <- desc_table_described0 %>% 
    mutate(variable = rownames(desc_table_described0)) %>%
    select(variable, everything())%>%
    mutate(variable = str_sub(variable, end = -2))
  
  write.xlsx(list("covariate_cont"=desc_table_covariate, "described_var"=desc_table_described), 
             paste("02_analysis_output/",target,"_00_covariate_table.xlsx", sep=""))
  
}


##conduct descriptive analysis -----------
try(create_map(target))
try(desc_time(target))
try(hist_industry(target))
try(make_stat(target))

try(descriptive_asset(target))
try(descriptive_employment(target))
try(descriptive_business(target))
try(descriptive_trade(target))
try(descriptive_trainingRD(target))
try(descriptive_IP(target))
try(descriptive_investment(target))


try(descriptive_covariate(target))
