library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)
library(data.table)

wd <- getwd()

file.path <- paste0(wd, '/File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx')
data <- read_excel(file.path, sheet = 2)
colnames(data) <- c('lsoa_code', 'lsao_name', 'lad_code', 'lad_name', 'imd_rank', 'imd_decile')

hist(data$imd_decile)

file2.path <- paste0(wd, '/2019_to_2020_free_school_meals_supplementary_grant_allocations.xlsx')
free.meals <- read_excel(file2.path, skip = 4)


hist(free.meals$Allocations)


ggplot(free.meals, aes(x=Allocations))+
  geom_histogram(fill='darkgoldenrod1', bins = 40)+
  theme_dark()+
  theme(axis.line = element_line(), axis.ticks = element_line(),
        text = element_text(family = 'Times'))


n_schools <- c()
codes <- unique(free.meals$`LA name`)
for (i in 1:length(codes)){
  n_schools[i] <- length(which(free.meals$`LA name` == codes[i]))
}

df <- data.frame(LA_name = codes, 
           n_schools = n_schools)

ggplot(df, aes(x=LA_name, y=n_schools))+
  geom_bar(stat = 'identity', fill='darkgoldenrod1')+
  theme_dark()+
  theme(axis.line = element_line(), axis.ticks = element_line(),
        text = element_text(family = 'Times'))

total_support <- c()
for (i in 1:length(codes)){
  total_support[i] <- sum(free.meals$Allocations[free.meals$`LA name` == codes[i]])
}
df$total_allocation <- total_support
df$mean_allocation <- df$total_allocation / df$n_schools

hist(df$mean_allocation)

new.data <- fread('https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/845345/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv')

new_df <- new.data[, c(4, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 41, 44, 47, 50)]

df2 <- df[order(-df$mean_allocation),]
new_df.2 <- new_df[new_df$`Local Authority District name (2019)` == df2[1,1],]
for (i in df2[c(2:5, 146:151), 1]){
  new_df.2 <- rbind(new_df.2, new_df[new_df$`Local Authority District name (2019)` == i,])
}

PCA <- prcomp(new_df.2[, -1], center = TRUE, scale. = TRUE)

data.frame(area = new_df.2$`Local Authority District name (2019)`,
           PC1 = PCA$x[,1],
           PC2 = PCA$x[,2]) %>%
  ggplot(., aes(x=PC1, y=PC2, color=area))+
  geom_point()+
  theme_dark()+
  theme(axis.line = element_line(), axis.ticks = element_line(),
        text = element_text(family = 'Times'), legend.position = 'none')


summary(PCA)

library(plotly)
library(colorspace)

final_df <- data.frame(area = new_df.2$`Local Authority District name (2019)`,
                       free_meals = rep(c('high', 'low'), c(849, 105)),
                       PC1 = PCA$x[,1],
                       PC2 = PCA$x[,2])

pal <- sequential_hcl(2, palette = 'OrYel')

fig <- plot_ly(data=final_df, x=final_df$PC1, y=final_df$PC2, name = final_df$area,color = final_df$free_meals, colors = pal) %>%
  layout(
    margin = 3,
    paper_bgcolor = "#1f1f1e",
    plot_bgcolor = "#1f1f1e",
    yaxis = list(color='#ffffff'),
    xaxis = list(color='#ffffff')
  )

Sys.setenv("username"="BenTenmann")
Sys.setenv("api_key"="cBQ9e6B0krbmU8RkL4pw")
api_create(fig, filename = "PCA_deprivation")


v <- c()
for ()


fig2 <- plot_ly()






