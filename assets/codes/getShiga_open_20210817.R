

#install.packages('pdftools')
library(pdftools)
library(dplyr)
library(tabulizer)
library(purrr)

path = "https://www.pref.shiga.lg.jp/file/attachment/5267145.pdf"
p_date = "8月15日"


tb <- extract_tables(path) # pdfデータを読み込み

patients_df <- update_patients_database_df(tb, published_date = p_date)



extract_real_data <- function(x){
  
  start = grep("例目", x[, 1]) + 1
  end = nrow(x)
  
  # 列一個多い問題の解決
  flag = FALSE
  
  # 最初になんかがくっついてる特殊な時
  if (x[1, 1] == ""){
    start = grep("例目", x[, 1]) + 2
    flag = TRUE
  }
  
  # なんか下に別のがくっついてる時
  if ( length(c(1:nrow(df))[df[, 1] == ""]) != 0 ){
    end = min(c(1:nrow(df))[df[, 1] == ""])
    flag = TRUE
  }
  
  if (flag == TRUE & ncol(x) > 11){
    x = x[, -(ncol(x)-1) ]
  }
  
  # 最後の列になんか変なのがくっついてる特殊な時
  if (x[1, ncol(x)] == "" & x[2, ncol(x)] == ""){
    x = x[, -(ncol(x)) ]
  }
  
  
  return( x[start:end, ] )
}


get_cum_positive_df <- function(x, published_date){
  
  for (i in (1:length(x))){
    
    df <- x[[i]]
    
    # 違う表じゃなかったら、ちゃんとあってる表だったら
    if (ncol(df) > 10 & nrow(df) > 3){
      
      df2 <- extract_real_data(df)
      
      df2 <- as.data.frame(df2, stringsAsFactors = FALSE)
      
      if ( length(colnames(df2)) == 11){
        df2$公表日 = published_date
        df2 <- df2[, c(1, ncol(df2) , c(2:(ncol(df2)-1)))]
      }
      
      if ( length(colnames(df2)) == 13){
        df2 <- df2[, -(ncol(df2)-1)]
      }
      
      colnames(df2) <- c("例目",
                         "公表日", 
                         "年代", 
                         "性別", 
                         "居住地", 
                         "職業", 
                         "発症日_推定",
                         "医療機関受診日", 
                         "陽性確定日", 
                         "推定される感染経路", 
                         "陽性患者との接触の有無", 
                         "調査対象"
      )
      
      if (i == 1){
        combined_df = df2
      }
      else{
        combined_df =　rbind(combined_df, df2)
      }
    }
    print(i)
  }
  
  combined_df = combined_df[order(combined_df$例目),]
  
  return(combined_df)
  
}


update_patients_database_df <- function(x, published_date ){
  
  database <- read.csv("patients_database.csv", stringsAsFactors = FALSE, fileEncoding="utf-8")
  
  # なんか最初の一列目が変
  database <- database[, -1]
  #database <- database[, -ncol(database)]
  
  # 最新版読み込み
  df_now <- get_cum_positive_df(x, published_date)
  
  # 最新版の最初のid(一番古いやつ)を取得
  start = df_now[1, 1]
  
  # matchでどこからを最新情報にするかをみる
  if (is.na(match(start, database[, 1])) == FALSE){
    end = match(start, database[, 1]) - 1
    
    if (end > 0){
      df <- rbind(database[1:end, ], df_now)
      
      write.csv(df, "patients_database.csv", fileEncoding = "utf-8")
    }
  }
  
  if (is.na(match(start, database[, 1])) == TRUE){
    
    df <- rbind(database, df_now)
    
    write.csv(df, "patients_database.csv", fileEncoding = "utf-8")
  }
  
  #時間合わせ
  time_convert <- read.csv("time_convert.csv", stringsAsFactors = FALSE); time_convert <- time_convert[,-1]
  
  MATCH <- match(df$公表日, time_convert$日付)
  
  df$公表日 = time_convert$Date[MATCH]
  
  return(df)
}


# 帳尻合わせ -------
patients_df = read.csv("patients_database.csv", stringsAsFactors = F)

patients_df = patients_df[patients_df$調査対象 != "", ]
patients_df = patients_df[, -1]

patients_df_tmp = patients_df
patients_df = distinct(patients_df, 例目,.keep_all=TRUE)


#時間合わせ
time_convert <- read.csv("time_convert.csv", stringsAsFactors = FALSE); time_convert <- time_convert[,-1]

MATCH <- match(patients_df$公表日, time_convert$日付)

patients_df$公表日 = time_convert$Date[MATCH]

write.csv(patients_df, "patients_database.csv", fileEncoding = "utf-8")





