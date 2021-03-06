Covid BR
================
Mohammed Kaebi
06/03/2021

``` r
knitr::opts_chunk$set(echo = FALSE)

library(tidyverse)
library(lubridate)
library(zoo)
library(data.table)
library(ggthemes)
library(ggrepel)
library(gridExtra)
library(abjData)


## Casos ----

db <-
  read.csv(
    "https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv",
    stringsAsFactors = FALSE
  )

db$date <- as.Date(db$date)

db$newCases <- abs(db$newCases)
db$newDeaths <- abs(db$newDeaths)

total_df <- db %>% 
  group_by(state) %>% 
  mutate(cases_7dma = frollmean(newCases, 7),
         cases_14dma = frollmean(newCases, 14),
         ratio = newCases / cases_14dma,
         R = frollmean(ratio, 7),
         deaths_7dma = frollmean(newDeaths, 7),
         
         newCases_14daychg = newCases - lag(newCases, 14),
         newDeaths_14daychg = newDeaths - lag(newDeaths, 14),
         
         newCases_7dmachg = cases_7dma - lag(cases_7dma, 7),
         newDeaths_7dmachg = deaths_7dma - lag(deaths_7dma, 7),
         
         newCases_100k = totalCases_per_100k_inhabitants - lag(totalCases_per_100k_inhabitants),
         newCases_100k_7dma = frollmean(newCases_100k, 7),
         newCases_100k_14_daychg = newCases_100k - lag(newCases_100k, 14),
         
         newDeaths_100k = deaths_per_100k_inhabitants - lag(deaths_per_100k_inhabitants),
         newDeaths_100k_7dma = frollmean(newDeaths_100k, 7),
         newDeaths_100k_14_daychg = newDeaths_100k - lag(newDeaths_100k, 14))


graf_cases <- function(x, int, cor){
  filter(total_df, state == x) %>%
    ggplot() +
    geom_bar(aes(x = date, y = newCases), stat = "identity", color = "grey55", alpha = 0.6) +
    geom_line(aes(x = date, y = cases_7dma), color = cor, size = 1.3)+
    labs(
      title = paste0(x),
      subtitle = "Daily new cases & 7 day moving average",
      caption = "Source: Brazilian Health Ministry*\n*See disclaimer at the end",
      x = "",
      y = ""
    ) +
    scale_y_continuous(breaks = seq(0, max(total_df[total_df$state==x, 8], na.rm = T) + 10000, by = int))+
    scale_x_date(date_labels = "%d/%b", breaks = "3 weeks")+
    theme_classic() +
    theme(
      axis.title = element_text(size = 14),
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 12),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14),
      plot.caption = element_text(size = 10),
      legend.position = "none",
      panel.grid.major.y = element_line(linetype = "dotted", color = "gray70")
    )
}

graf_deaths <- function(x, int, cor){
  filter(total_df, state == x) %>%
    ggplot() +
    geom_bar(aes(x = date, y = newDeaths), stat = "identity", color = "grey55", alpha = 0.6) +
    geom_line(aes(x = date, y = deaths_7dma), color = cor, size = 1.3)+
    labs(
      title = paste0(x),
      subtitle = "Daily new deaths & 7 day moving average",
      caption = "Source: Brazilian Health Ministry*\n*See disclaimer at the end",
      x = "",
      y = ""
    ) +
    scale_y_continuous(breaks = seq(0, max(total_df[total_df$state==x, 6], na.rm = T) + 10000, by = int))+
    scale_x_date(date_labels = "%d/%b", breaks = "3 weeks")+
    theme_classic() +
    theme(
      axis.title = element_text(size = 14),
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 12),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14),
      plot.caption = element_text(size = 10),
      legend.position = "none",
      panel.grid.major.y = element_line(linetype = "dotted", color = "gray70")
    )
}
```

# Brasil

## Mapa - Novos casos e mortes

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

## Mapa - Casos e mortes acumulados

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

## Brasil (1)

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## Brasil (2)

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Brasil (3)

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Brasil (4)

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

# Sudeste

## Novos casos nas últimas semanas

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Novas mortes nas últimas semanas

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

## Sudeste - New cases

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

## Sudeste - New deaths

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

## Sudeste - Total cases

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## Sudeste - Total Deaths

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

## São Paulo

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

## Rio de Janeiro

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

## Minas Gerais

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

## Espírito Santo

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

# Sul

## Novos casos nas últimas semanas

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

## Novas mortes nas últimas semanas

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

## Sul - New cases

![](README_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

## Sul - New deaths

![](README_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

## Sul - Total cases

![](README_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

## Sul - Total deaths

![](README_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

## Santa Catarina

![](README_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

## Paraná

![](README_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

## Rio Grande do Sul

![](README_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

# Centro Oeste

## Novos casos nas últimas semanas

![](README_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

## Novas mortes nas últimas semanas

![](README_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

## Centro Oeste - New cases

![](README_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

## Centro Oeste - New deaths

![](README_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

## Centro Oeste - Total cases

![](README_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

## Centro Oeste - Total deaths

![](README_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

## Goiás

![](README_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

## Mato Grosso

![](README_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

## Mato Grosso do Sul

![](README_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

## Distrito Federal

![](README_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

# Nordeste

## Novos casos nas últimas semanas

![](README_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

## Novas mortes nas últimas semanas

![](README_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

## Nordeste - New cases I

![](README_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

## Nordeste - New cases II

![](README_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

## Nordeste - New deaths I

![](README_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

## Nordeste - New deaths II

![](README_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

## Nordeste - Total cases I

![](README_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

## Nordeste - Total cases II

![](README_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->

## Nordeste - Total deaths I

![](README_files/figure-gfm/unnamed-chunk-44-1.png)<!-- -->

## Nordeste - Total deaths II

![](README_files/figure-gfm/unnamed-chunk-45-1.png)<!-- -->

## Alagoas

![](README_files/figure-gfm/unnamed-chunk-46-1.png)<!-- -->

## Bahia

![](README_files/figure-gfm/unnamed-chunk-47-1.png)<!-- -->

## Ceará

![](README_files/figure-gfm/unnamed-chunk-48-1.png)<!-- -->

## Maranhão

![](README_files/figure-gfm/unnamed-chunk-49-1.png)<!-- -->

## Paraíba

![](README_files/figure-gfm/unnamed-chunk-50-1.png)<!-- -->

## Pernambuco

![](README_files/figure-gfm/unnamed-chunk-51-1.png)<!-- -->

## Piauí

![](README_files/figure-gfm/unnamed-chunk-52-1.png)<!-- -->

## Rio Grande do Norte

![](README_files/figure-gfm/unnamed-chunk-53-1.png)<!-- -->

## Sergipe

![](README_files/figure-gfm/unnamed-chunk-54-1.png)<!-- -->

# Norte

## Novos casos nas últimas semanas

![](README_files/figure-gfm/unnamed-chunk-55-1.png)<!-- -->

## Novas mortes nas últimas semanas

![](README_files/figure-gfm/unnamed-chunk-56-1.png)<!-- -->

## Norte - New cases I

![](README_files/figure-gfm/unnamed-chunk-57-1.png)<!-- -->

## Norte - New cases II

![](README_files/figure-gfm/unnamed-chunk-58-1.png)<!-- -->

## Norte - New deaths I

![](README_files/figure-gfm/unnamed-chunk-59-1.png)<!-- -->

## Norte - New deaths II

![](README_files/figure-gfm/unnamed-chunk-60-1.png)<!-- -->

## Norte - Total cases I

![](README_files/figure-gfm/unnamed-chunk-61-1.png)<!-- -->

## Norte - Total cases II

![](README_files/figure-gfm/unnamed-chunk-62-1.png)<!-- -->

## Norte - Total deaths I

![](README_files/figure-gfm/unnamed-chunk-63-1.png)<!-- -->

## Norte - Total deaths II

![](README_files/figure-gfm/unnamed-chunk-64-1.png)<!-- -->

## Amazonas

![](README_files/figure-gfm/unnamed-chunk-65-1.png)<!-- -->

## Roraima

![](README_files/figure-gfm/unnamed-chunk-66-1.png)<!-- -->

## Amapá

![](README_files/figure-gfm/unnamed-chunk-67-1.png)<!-- -->

## Pará

![](README_files/figure-gfm/unnamed-chunk-68-1.png)<!-- -->

## Tocantins

![](README_files/figure-gfm/unnamed-chunk-69-1.png)<!-- -->

## Rondônia

![](README_files/figure-gfm/unnamed-chunk-70-1.png)<!-- -->

## Acre

![](README_files/figure-gfm/unnamed-chunk-71-1.png)<!-- -->

# Data source

## Data source

Link: <https://github.com/wcota/covid19br>

Disclaimer: Dados do Ministério da Saúde até o dia em que estão
disponíveis. Para os mais recentes (e que ainda não foram divulgados
pelo Ministério da Saúde), dados das Secretarias Estaduais de Saúde.
