library(ggplot)
library(tidyverse)

df <- data.frame(
    lang = c("SQL", "Python", "R", "HTML" ,"Javascript")
    , year = c(2018, 2018, 2018, 2018, 2018)
    # , prof = c()
)

df_all <- data.frame()
for (x in 1:nrow(df)){
    duration <- seq(df[[2]][x],format(Sys.Date(), "%Y"))
    lan <- df[[1]][x]
    df_l <- data.frame(
        years = duration
        , lang = rep(lan,length(duration)))
    df_all <- rbind(df_all,df_l)

}



ggplot(df_all,aes(x = years ,y =lang))+
geom_line()+
theme_minimal()
