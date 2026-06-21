###設定----------
# Clear console messages
cat( "\014" )
# Clear plots
if( dev.cur() > 1 ) dev.off()
# Clear global workspace
rm( list = ls( envir = globalenv() ), envir = globalenv() )

###ライブラリ-----------
library(tidyverse)
library(stargazer)
library(openxlsx)

# 設定-------------
setwd("D:/R/260407Industrial-Policy-Analysis-Tool") # 使用環境に合わせて修正する
is_key <- 1 #統計間マッチングキーを使用する場合は1、使用しない場合は0を設定

###readcsv-----------
df2023_0 <- read.csv("kikatsu_rawdata/2023_kohyo1.csv", header = TRUE, fileEncoding="Shift-JIS") %>% mutate(year = 2022)
colnames <- names(df2023_0)

df2022_0	<- read.csv("kikatsu_rawdata/2022_kohyo1.csv", header = TRUE,	 fileEncoding="Shift-JIS") %>% mutate(year = 2021) %>% setNames(colnames)
df2021_0	<- read.csv("kikatsu_rawdata/2021_kohyo1.csv", header = TRUE,	 fileEncoding="Shift-JIS") %>% mutate(year = 2020) %>% setNames(colnames)
df2020_0	<- read.csv("kikatsu_rawdata/2020_kohyo1.csv", header = TRUE,	 fileEncoding="Shift-JIS") %>% mutate(year = 2019) %>% setNames(colnames)
df2019_0	<- read.csv("kikatsu_rawdata/2019_kohyo1.csv", header = TRUE,	 fileEncoding="Shift-JIS") %>% mutate(year = 2018) %>% setNames(colnames)
df2018_0	<- read.csv("kikatsu_rawdata/H30年_kohyo1.csv", header = TRUE,	 fileEncoding="Shift-JIS") %>% mutate(year = 2017) %>% setNames(colnames)
df2017_0	<- read.csv("kikatsu_rawdata/H29年_kohyo1.csv", header = TRUE,	 fileEncoding="Shift-JIS") %>% mutate(year = 2016) %>% setNames(colnames)
df2016_0	<- read.csv("kikatsu_rawdata/H28年_kohyo1.csv", header = TRUE,	 fileEncoding="Shift-JIS") %>% mutate(year = 2015) %>% setNames(colnames)
df2015_0	<- read.csv("kikatsu_rawdata/H27年_kohyo1.csv", header = TRUE,	 fileEncoding="Shift-JIS") %>% mutate(year = 2014) %>% setNames(colnames)
df2014_0	<- read.csv("kikatsu_rawdata/H26年_kohyo1.csv", header = TRUE,	 fileEncoding="Shift-JIS") %>% mutate(year = 2013) %>% setNames(colnames)
df2013_0	<- read.csv("kikatsu_rawdata/H25年_kohyo1.csv", header = TRUE,	 fileEncoding="Shift-JIS") %>% mutate(year = 2012) %>% setNames(colnames)
df2012_0	<- read.csv("kikatsu_rawdata/H24年_kohyo1.csv", header = TRUE,	 fileEncoding="Shift-JIS") %>% mutate(year = 2011) %>% setNames(colnames)
df2011_0	<- read.csv("kikatsu_rawdata/H23年_kohyo1.csv", header = TRUE,	 fileEncoding="Shift-JIS") %>% mutate(year = 2010) %>% setNames(colnames)
df2010_0	<- read.csv("kikatsu_rawdata/H22年_kohyo1.csv", header = TRUE,	 fileEncoding="Shift-JIS") %>% mutate(year = 2009) %>% setNames(colnames)
df2009_0	<- read.csv("kikatsu_rawdata/H21年_kohyo1.csv", header = TRUE,	 fileEncoding="Shift-JIS") %>% mutate(year = 2008) %>% setNames(colnames)
df2008_0	<- read.csv("kikatsu_rawdata/H20年_kohyo1.csv", header = TRUE,	 fileEncoding="Shift-JIS") %>% mutate(year = 2007) %>% setNames(colnames)
df2007_0	<- read.csv("kikatsu_rawdata/H19年_kohyo1.csv", header = TRUE,	 fileEncoding="Shift-JIS") %>% mutate(year = 2006) %>% setNames(colnames)
df2006_0	<- read.csv("kikatsu_rawdata/H18年_kohyo1.csv", header = TRUE,	 fileEncoding="Shift-JIS") %>% mutate(year = 2005) %>% setNames(colnames)
df2005_0	<- read.csv("kikatsu_rawdata/H17年_kohyo1.csv", header = TRUE,	 fileEncoding="Shift-JIS") %>% mutate(year = 2004) %>% setNames(colnames)
df2004_0	<- read.csv("kikatsu_rawdata/H16年_kohyo1.csv", header = TRUE,	 fileEncoding="Shift-JIS") %>% mutate(year = 2003) %>% setNames(colnames)
df2003_0	<- read.csv("kikatsu_rawdata/H15年_kohyo1.csv", header = TRUE,	 fileEncoding="Shift-JIS") %>% mutate(year = 2002) %>% setNames(colnames)
df2002_0	<- read.csv("kikatsu_rawdata/H14年_kohyo1.csv", header = TRUE,	 fileEncoding="Shift-JIS") %>% mutate(year = 2001) %>% setNames(colnames)
df2001_0	<- read.csv("kikatsu_rawdata/H13年_kohyo1.csv", header = TRUE,	 fileEncoding="Shift-JIS") %>% mutate(year = 2000) %>% setNames(colnames)
df2000_0	<- read.csv("kikatsu_rawdata/H12年_kohyo1.csv", header = TRUE,	 fileEncoding="Shift-JIS") %>% mutate(year = 1999) %>% setNames(colnames)
df1999_0	<- read.csv("kikatsu_rawdata/H11年_kohyo1.csv", header = TRUE,	 fileEncoding="Shift-JIS") %>% mutate(year = 1998) %>% setNames(colnames)
df1998_0	<- read.csv("kikatsu_rawdata/H10年_kohyo1.csv", header = TRUE,	 fileEncoding="Shift-JIS") %>% mutate(year = 1997) %>% setNames(colnames)
df1997_0	<- read.csv("kikatsu_rawdata/H09年_kohyo1.csv", header = TRUE,	 fileEncoding="Shift-JIS") %>% mutate(year = 1996) %>% setNames(colnames)
df1996_0	<- read.csv("kikatsu_rawdata/H08年_kohyo1.csv", header = TRUE,	 fileEncoding="Shift-JIS") %>% mutate(year = 1995) %>% setNames(colnames)
df1995_0	<- read.csv("kikatsu_rawdata/H07年_kohyo1.csv", header = TRUE,	 fileEncoding="Shift-JIS") %>% mutate(year = 1994) %>% setNames(colnames)

df_merge1 <- dplyr::bind_rows(df2023_0,	df2022_0,	df2021_0,	df2020_0,	
                              df2019_0,	df2018_0,	df2017_0,	df2016_0,	df2015_0,	df2014_0,	df2013_0,	df2012_0,	df2011_0,	df2010_0,	
)%>%
  select(-c(郵便番号, 市外局番))
df_merge2 <- dplyr::bind_rows(df2009_0,	df2008_0,	df2007_0,	df2006_0,	df2005_0,	df2004_0,	df2003_0,	df2002_0,	df2001_0,	df2000_0,	
                              df1999_0,	df1998_0,	df1997_0,	df1996_0,	df1995_0,)%>%
  select(-c(郵便番号, 市外局番))

df_merge0 <- dplyr::bind_rows(df_merge1,df_merge2)

#変換-----

df_merged <- df_merge0 %>%  
  mutate(key_eikyu = 永久企業番号,
         key_kyoutsu = 共通企業番号,
         hojin_num = Ｋ0111法人番号,
         industry = 産業格付小分類,
         name =　企業名称,
         pref = 都道府県名,
         
         capital = Ｋ0101資本金額*1000000,
         foreignratio = Ｋ0102外資比率,
         foundation_year = Ｋ0103_1企業設立年,
         
         settlement_month1 =Ｋ0105_1企業決算期１.年1回.,
         settlement_month2 =Ｋ0105_2企業決算期２.年2回.,
         settlement_month3 =Ｋ0105_3企業決算期３.年3回.,
         # VAT_treatment =Ｋ0106消費税取扱い, 
         
         office = Ｋ0226_1事業所数合計, 
         
         workers = Ｋ0226_2従業者数合計,
         indefinite_workers = ifelse(year< 2006,NA,
                                     ifelse(is.na(Ｋ0227うち無期雇用者数),0, Ｋ0227うち無期雇用者数)),
         fixedterm_workers = ifelse(year< 1994,NA,
                                    ifelse(is.na(Ｋ0228うち1か月以上有期雇用者数),0, Ｋ0228うち1か月以上有期雇用者数)) ,
         fixedterm_workers_equivalent =ifelse(year<2006 ,NA,
                                              ifelse(is.na(Ｋ0229うち1か月以上有期雇用者就業時間換算),0, Ｋ0229うち1か月以上有期雇用者就業時間換算)),
         temp_workers = ifelse(year< 1994,NA,
                               ifelse(is.na(Ｋ0230臨時雇用者計),0, Ｋ0230臨時雇用者計) ),
         haken_workers = ifelse(year<2000 ,NA,
                                ifelse(is.na(Ｋ0231派遣従業者),0, Ｋ0231派遣従業者)) ,
         
         subsidiary = ifelse(year <2021,Ｋ0310子会社関連会社の所有と増減の有無,
                             ifelse(Ｋ0310子会社関連会社の所有と増減の有無 <3, 1, 2)), # 定義が年によって異なるため 1がある、２がないに変換
         parent = Ｋ0300_1親会社の有無, # 1がある、２がない
         
         current_asset = Ｋ0401流動資産*1000000,
         fixed_asset = Ｋ0403固定資産*1000000,
         tangible_asset = Ｋ0404内有形固定資産*1000000,
         intangible_asset = Ｋ0406無形固定資産*1000000,
         deferred_asset = ifelse(year<1994 ,NA,
                                 ifelse(is.na(Ｋ0409繰延資産),0, Ｋ0409繰延資産*1000000)),
         sum_asset = Ｋ0410資産合計*1000000,
         current_debt = Ｋ0411流動負債*1000000,
         fixed_debt = Ｋ0415固定負債*1000000,
         sum_debt = Ｋ0424負債純資産合計*1000000,
         
         investment_affiliate_domestic = ifelse(is.na(Ｋ0431_1国内関係投融資残高),0, Ｋ0431_1国内関係投融資残高*1000000),
         investment_affiliate_overseas = ifelse(is.na(Ｋ0431_2海外関係投融資残高),0, Ｋ0431_2海外関係投融資残高*1000000),
         tangible_asset_acquisition = ifelse(is.na(Ｋ0441有形固定資産当期取得額),0, Ｋ0441有形固定資産当期取得額*1000000) ,
         tangible_asset_decrease = ifelse(year<1995 ,NA,
                                          ifelse(is.na(Ｋ0444有形固定資産当期減少額),0, Ｋ0444有形固定資産当期減少額*1000000)) ,
         intangible_asset_acquisition = ifelse(year<2006 ,NA,
                                               ifelse(is.na(Ｋ0443無形固定資産当期取得額),0, Ｋ0443無形固定資産当期取得額*1000000) ),
         intangible_asset_decrease = ifelse(year<2006 ,NA,
                                            ifelse(is.na(Ｋ0445無形固定資産当期減少額),0, Ｋ0445無形固定資産当期減少額*1000000)) ,
         dividend = ifelse(year< 2009,NA,
                           ifelse(is.na(Ｋ0451配当金),0, Ｋ0451配当金*1000000) ),
         
         sales = Ｋ0501売上高*1000000,
         current_profit = Ｋ0507経常利益*1000000,
         net_profit = Ｋ0508当期純利益*1000000,
         sum_salary = Ｋ0514給与総額*1000000,
         benefit = Ｋ0515福利厚生費*1000000,
         tax = Ｋ0517租税公課*1000000,
         
         export =ifelse(year< 1997,NA,
                        ifelse(is.na(Ｋ0602_1取引売上海外輸出),0, Ｋ0602_1取引売上海外輸出*1000000)),
         import = ifelse(year< 1997,NA,
                         ifelse(is.na(Ｋ0610_1取引仕入海外輸入),0, Ｋ0610_1取引仕入海外輸入*1000000)),
         
         RD_jisha = ifelse(is.na(Ｋ0802研究費自社研究費),0, Ｋ0802研究費自社研究費*1000000),
         RD_itaku = ifelse(is.na(Ｋ0803_1研究費委託研究費),0, Ｋ0803_1研究費委託研究費*1000000),
         RD_jutaku = ifelse(is.na(Ｋ0804_1研究費受託研究費),0, Ｋ0804_1研究費受託研究費*1000000),
         RD_shutoku = ifelse(year<1997 ,NA,
                             ifelse(is.na(Ｋ0805_有形固定当期取得額研究),0, Ｋ0805_有形固定当期取得額研究*1000000)),
         training =ifelse(year<2009 ,NA,
                          ifelse(is.na(Ｋ0806能力開発費),0, Ｋ0806能力開発費*1000000)),
         patent = ifelse(year< 1997,NA,
                         ifelse(is.na(Ｋ0901_1特許特許所有),0, Ｋ0901_1特許特許所有)),
         jitsuyo = ifelse(year<1997 ,NA,
                          ifelse(is.na(Ｋ0902_1特許実用所有),0, Ｋ0902_1特許実用所有)),
         isho = ifelse(year<1997 ,NA,
                       ifelse(is.na(Ｋ0903_1特許意匠所有),0, Ｋ0903_1特許意匠所有)),
         inside_directors = ifelse(year<2009 ,NA,
                                  ifelse(is.na(Ｋ1001_1取締役人数社内),0, Ｋ1001_1取締役人数社内)),
         outside_directors = ifelse(year<2009 ,NA,
                                    ifelse(is.na(Ｋ1001_2取締役人数社外),0, Ｋ1001_2取締役人数社外)),
         #         institutional_design =Ｋ1002機関設計, 
  )%>%
  select(-c(1:(ncol(df_merge0)-1)))
#結合の書き出し---------
write.csv(df_merged, '01_prepare_output/temp/merged.csv', fileEncoding = "UTF-8")

#merged_keyの作成-----------
df0 <-#df_merged %>% 
  read.csv("01_prepare_output/temp/merged.csv", encoding = "UTF-8")%>%
  select(-c(1:1))%>% # 不要な列番号を削除
  mutate(id = as.character(hojin_num),
         key_eikyu = as.character(key_eikyu),
         key_kyoutsu = as.character(key_kyoutsu),
         year = as.character(year)) 


# key match-------------
dfkey <- df0%>%
  mutate(id_1 = as.character(id),
         key_eikyu = as.character(key_eikyu),
         key_kyoutsu2 = as.character(key_kyoutsu),
         year = as.character(year)) %>%
  select(-id) %>%
  arrange(key_eikyu, desc(year)) %>%
  drop_na(id_1, key_eikyu) %>%
  distinct(key_eikyu, .keep_all = TRUE) %>%  # key_eikyuの重複削除
  distinct(id_1, .keep_all = TRUE) %>%        # id_1の重複削除
  select(key_eikyu, id_1, key_kyoutsu2)


dfm0 <- left_join(df0,dfkey,by=c("key_eikyu"))
dfm0 <- dfm0%>%
  mutate(id = as.character(ifelse(is.na(id),id_1,id)))%>%
  select(-c(id_1,key_kyoutsu2))%>%
  select(id, everything())

if (is_key == 1) {
  # 統計間マッチングキーを用いる場合
  ## key の準備
  df2022key <- read.xlsx("key/A09_2022_企業活動基本調査.xlsx")
  df2021key <- read.xlsx("key/A09_2021_企業活動基本調査.xlsx")
  df2020key <- read.xlsx("key/A09_2020_企業活動基本調査.xlsx")
  df2019key <- read.xlsx("key/A09_2019_企業活動基本調査.xlsx")
  df2018key <- read.xlsx("key/A09_2018_企業活動基本調査.xlsx")
  df2017key <- read.xlsx("key/A09_2017_企業活動基本調査.xlsx")
  df2016key <- read.xlsx("key/A09_2016_企業活動基本調査.xlsx")
  df2015key <- read.xlsx("key/A09_2015_企業活動基本調査.xlsx")
  df2014key <- read.xlsx("key/A09_2014_企業活動基本調査.xlsx")
  df2013key <- read.xlsx("key/A09_2013_企業活動基本調査.xlsx")
  df2012key <- read.xlsx("key/A09_2012_企業活動基本調査.xlsx")
  df2011key <- read.xlsx("key/A09_2011_企業活動基本調査.xlsx")
  df2010key <- read.xlsx("key/A09_2010_企業活動基本調査.xlsx")
  
  
  df_m <- rbind(df2022key,df2021key,df2020key,df2019key,df2018key,df2017key,df2016key,df2015key,df2014key,df2013key,df2012key,df2011key,df2010key)
  
  
  colnames(df_m)[1] <- "year" 
  colnames(df_m)[2] <- "key_eikyu" #永久企業番号
  colnames(df_m)[3] <- "key_kyoutsu" #調査用事業所表記番号
  colnames(df_m)[4] <- "id_1" #法人番号
  
  df_m <- df_m %>% select(year,key_kyoutsu,id_1,key_eikyu)
  write.csv(df_m, '01_prepare_output/key.csv', fileEncoding = "UTF-8")
  
  ## make key
  dfkey <- read.csv("01_prepare_output/key.csv", encoding = "UTF-8")%>%
    mutate(id_1 = as.character(id_1),
           key_eikyu = as.character(key_eikyu),
           key_kyoutsu2 = as.character(key_kyoutsu),
           year = as.numeric(year)) %>%
    arrange(key_eikyu, desc(year)) %>%
    drop_na(id_1, key_eikyu) %>%
    distinct(key_eikyu, .keep_all = TRUE) %>%  # key_eikyuの重複削除
    distinct(id_1, .keep_all = TRUE) %>%        # id_1の重複削除
    select(key_eikyu, id_1, key_kyoutsu2)
  
  ## match by eikyu
  dfm0 <- left_join(dfm0,dfkey,by=c("key_eikyu"))
  dfm0 <<- dfm0%>%
    mutate(id = as.character(ifelse(is.na(id),id_1,id)))%>%
    select(-c(id_1,key_kyoutsu2))%>%
    select(id, everything())
  
}


# 使用データの書き出し------------
summary(as.numeric(dfm0$id))

write.csv(dfm0, '01_prepare_output/merged_key.csv', fileEncoding = "UTF-8")

