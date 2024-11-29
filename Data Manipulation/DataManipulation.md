# Read raw data

## Glucose

``` r
glucose = read_delim("DATA/glucose.csv", 
    delim = ";", escape_double = FALSE, col_types = cols(Merrie = col_factor(), 
        Replicate = col_factor (),
        Naald = col_logical())
    ) %>%
  dplyr::rename(Patient = Merrie,
                Treatment = Naald,
                "-1440" ="Tstal-1" ,
                "-10"="Tbefore",
                "0" = "T0",
                "10" = "T1",
                "20" = "T2",
                "30" = "T3",
                "40" = "T4",
                "1440" ="Tstal1",
                "4320" ="Tstal3" 
                )
```

## Cortisol

``` r
cortisol = read_delim("DATA/cortisol.csv", 
    delim = ";", escape_double = FALSE, col_types = cols(Merrie = col_factor(), 
        Replicate = col_factor (),
        Naald = col_logical()) ,
    trim_ws = TRUE, locale = locale(decimal_mark = ",")) %>%
  dplyr::rename(Patient = Merrie,
                Treatment = Naald,
                "-1440" ="Tstal-1" ,
                "-10"="Tbefore",
                "0" = "T0",
                "10" = "T1",
                "20" = "T2",
                "30" = "T3",
                "40" = "T4",
                "1440" ="Tstal1",
                "4320" ="Tstal3" 
                )
```

## Lactate

``` r
lactate = read_delim("DATA/lactaat.csv", 
    delim = ";", escape_double = FALSE, col_types = cols(Merrie = col_factor(), 
        Replicate = col_factor (), 
        Naald = col_logical()), 
    trim_ws = TRUE, locale = locale(decimal_mark = ",")) %>%
  dplyr::rename(Patient = Merrie,
                Treatment = Naald,
                "-1440" ="Tstal-1" ,
                "-10"="Tbefore",
                "0" = "T0",
                "10" = "T1",
                "20" = "T2",
                "30" = "T3",
                "40" = "T4",
                "1440" ="Tstal1",
                "4320" ="Tstal3" 
                )
```

##Punctures

``` r
punctures =  read_delim("DATA/punctures.csv", 
    delim = ";", escape_double = FALSE, col_types = cols(Merrie = col_factor(), 
        Replicate = col_factor (), 
        Naald = col_logical()), 
    trim_ws = TRUE, locale = locale(decimal_mark = ",")) %>%
  dplyr::rename(Patient = Merrie,
                Treatment = Naald,
                "-1440" ="Tstal-1" ,
                "-10"="Tbefore",
                "0" = "T0",
                "10" = "T1",
                "20" = "T2",
                "30" = "T3",
                "40" = "T4",
                "1440" ="Tstal1",
                "4320" ="Tstal3" 
                )
```

## Pain score

``` r
pain_score <- read_delim("DATA/pain score.csv", 
    delim = ";", escape_double = FALSE, col_types = cols(Merrie = col_factor(), 
        Replicate = col_factor (), 
        Naald = col_logical()), 
    trim_ws = TRUE, locale = locale(decimal_mark = ",")) %>%
  dplyr::rename(Patient = Merrie,
                Treatment = Naald,
                "-1440" ="Tstal-1" ,
                "-10"="Tbefore",
                "0" = "T0",
                "10" = "T1",
                "20" = "T2",
                "30" = "T3",
                "40" = "T4",
                "1440" ="Tstal1",
                "4320" ="Tstal3" 
                )
```

## Pain five

``` r
pain_score_5 = read_delim("DATA/pain score 5.csv", 
    delim = ";", escape_double = FALSE, col_types = cols(Merrie = col_factor(levels = c("A", 
        "B", "C", "D", "E", "F", "G", "H")), 
        Naald = col_logical(), Replicate = col_factor(levels = c("1", 
            "2", "3"))), locale = locale(decimal_mark = ","), 
    trim_ws = TRUE) %>%
  dplyr::rename(Patient = Merrie,
                Treatment = Naald,
                "-1440" ="Tstal-1" ,
                "-10"="Tbefore",
                "0" = "T0",
                "10" = "T1",
                "20" = "T2",
                "30" = "T3",
                "40" = "T4",
                "1440" ="Tstal1",
                "4320" ="Tstal3" 
                )
```

## Sedation

``` r
sedation = read_delim("DATA/sedatia.csv", 
    delim = ";", escape_double = FALSE, col_types = cols(Merrie = col_factor(levels = c("A", 
        "B", "C", "D", "E", "F", "G", "H")), 
        Naald = col_logical(), Replicate = col_factor(levels = c("1", 
            "2", "3"))), locale = locale(decimal_mark = ","), 
    trim_ws = TRUE) %>%
  dplyr::rename(Patient = Merrie,
                Treatment = Naald
                )
```

## Heart Parameters

``` r
Heart_parameters = read_delim("DATA/Heart_parameters.csv", 
    delim = ";", escape_double = FALSE, col_types = cols(Filename = col_skip(), 
        Session = col_factor(levels = c("OPU1", 
            "OPU2", "OPU3", "OPU4"))), locale = locale(decimal_mark = ","), 
    trim_ws = TRUE) %>% dplyr::mutate(Index = `...1`) %>% dplyr::select(-c(`...1`)) %>%
  group_by(Session,Horse) %>% mutate(SuposedTime = row_number(),
                                     Needle1= case_when(
                                       Horse == "Lucky" & Session == "OPU1"~12,
                                       TRUE ~ NA),
                                     Needle2 = case_when(
                                       Horse == "Lucky" & Session == "OPU1"~25,
                                       TRUE ~ NA),
                                     Needle3 = case_when(
                                       Horse == "Lucky" & Session == "OPU1"~35,
                                       TRUE ~ NA)
                                      )
```

    ## New names:
    ## • `` -> `...1`

## Cardiovascular

``` r
cardiovascular = read_delim("DATA/cardiovascular.csv", 
    delim = ";", escape_double = FALSE, col_types = cols(Patient = col_factor(levels = c("A", 
        "B", "C", "D", "E", "F", "G", "H")), 
        Replicate = col_factor(levels = c("1", 
            "2", "3")), HousingEnvironment = col_factor(levels = c("stocks", 
            "stable"))), locale = locale(decimal_mark = ","), 
    trim_ws = TRUE)
```

    ## New names:
    ## • `` -> `...12`
    ## • `` -> `...25`

    ## Warning: One or more parsing issues, call `problems()` on your data frame for details,
    ## e.g.:
    ##   dat <- vroom(...)
    ##   problems(dat)

# Pivot different Datasets

## Glucose

``` r
PivotGlucose = glucose %>% 
  tidyr::pivot_longer( cols = !c(Patient,Treatment, Replicate), names_to = "Time", values_to = "Glucose") %>%
  dplyr::filter(!is.na(Glucose))
```

## Cortisol

``` r
PivotCortisol = cortisol %>% 
  tidyr::pivot_longer( cols = !c(Patient, Treatment, Replicate), names_to = "Time", values_to = "Cortisol") %>%
  dplyr::filter(!is.na(Cortisol))
```

## Lactate

``` r
PivotLactate = lactate %>% 
  tidyr::pivot_longer( cols = !c(Patient, Treatment, Replicate), names_to = "Time", values_to = "Lactate") %>%
  dplyr::filter(!is.na(Lactate))
```

## Punctures

``` r
PivotPunctures = punctures %>% 
  tidyr::pivot_longer( cols = !c(Patient, Treatment, Replicate), names_to = "Time", values_to = "Punctures") %>%
  dplyr::filter(!is.na(Punctures))
```

## Pain score

``` r
PivotPainScore = pain_score %>% 
  tidyr::pivot_longer( cols = !c(Patient, Treatment, Replicate), names_to = "Time", values_to = "PainScore") %>%
  dplyr::filter(!is.na(PainScore))
```

##PainFive

``` r
PivotPainFive = pain_score_5 %>%
  tidyr::pivot_longer( cols = !c(Patient, Treatment, Replicate), names_to = "Time", values_to = "PainFive") %>%
  dplyr::filter(!is.na(PainFive))
```

# Joining data

``` r
JoinedData = left_join(x = PivotLactate, y = PivotGlucose, by = join_by(Patient, Treatment, Replicate, Time))
```

``` r
JoinedData = left_join(x = JoinedData, y = PivotCortisol, by = join_by(Patient, Treatment, Replicate, Time))
```

``` r
JoinedData = left_join(x = JoinedData, y = PivotPainScore, by = join_by(Patient, Treatment, Replicate, Time))
```

``` r
JoinedData = left_join(x = JoinedData, y = PivotPunctures, by = join_by(Patient, Treatment, Replicate, Time))
```

``` r
JoinedData = left_join(x = JoinedData, y = PivotPainFive, by = join_by(Patient, Treatment, Replicate, Time))
```

``` r
rm(PivotCortisol,PivotGlucose,PivotLactate)
```

# Variable engineering

``` r
Data = JoinedData %>%
  dplyr::mutate(HousingEnvironment = as.factor (case_when(
    Time == "-1440" ~ "Stable",
    Time == "-10" ~ "Stocks",
    Time == "0" ~ "Stocks",
    Time == "10" ~ "Stocks",
    Time == "20" ~ "Stocks",
    Time == "30" ~ "Stocks",
    Time == "40" ~ "Stocks",
    Time == "Tafter" ~ "Stocks",
    Time == "1440" ~ "Stable",
    Time == "4320" ~ "Stable",
    TRUE ~ "Other"
  )), 
  Manipulation = as.factor (case_when(
    Time == "-1440" ~ 0,
    Time == "-10" ~ 0,
    Time == "0" ~ 1,
    Time == "10" ~ 1,
    Time == "20" ~ 1,
    Time == "30" ~ 1,
    Time == "40" ~ 1,
    Time == "Tafter" ~ 0,    
    Time == "1440" ~ 0,
    Time == "4320" ~ 0,
    TRUE ~ 9999 
  )), # Tafter is converted back to a numeric value using the following code this was done because Tafter is depending on when the procedure ended 
  Temp= case_when(
    Time == "Tafter" ~ 10,    
    TRUE ~ 0),
  Time = as.numeric(na_if(Time, 'Tafter'))
  )%>%
  fill(Time, .direction = "down") %>%
  dplyr::mutate(Time = Time + Temp,
                Temp = NULL) %>%
  dplyr::filter(Time != "50") %>% group_by(Patient,Replicate) %>% arrange(Time) %>% mutate(cumulativepunctures = cumsum(Punctures)) %>% ungroup() %>% arrange(Patient,Replicate) #Removal of the one horse were the experiment lasted more than 50 min
```

# Temporary data exploration

## Lactate, Glucose, Cortisol and PainScore

``` r
Patient = list("A","B","C","D","E","F","G","H")

for (x in Patient){
  
PlotLactaat = ggplot(Data %>% dplyr::filter(Patient == x), aes(x= Time,y=Lactate,colour=Patient)) +
  geom_line(aes(linetype = Replicate)) 
PlotCortisol = ggplot(Data %>% dplyr::filter(Patient == x), aes(x= Time,y=Cortisol,colour=Patient)) +
  geom_line(aes(linetype = Replicate))  
PlotGlucose = ggplot(Data %>% dplyr::filter(Patient == x), aes(x= Time,y=Glucose,colour=Patient)) +
  geom_line(aes(linetype = Replicate))  
PlotPainScore = ggplot(Data %>% dplyr::filter(Patient == x), aes(x= Time,y=PainScore,colour=Patient)) +
  geom_line(aes(linetype = Replicate))

print(PlotGlucose)
print(PlotCortisol)
print(PlotLactaat)
print(PlotPainScore)
}
```

![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-1.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-2.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-3.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-4.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-5.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-6.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-7.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-8.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-9.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-10.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-11.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-12.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-13.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-14.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-15.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-16.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-17.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-18.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-19.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-20.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-21.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-22.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-23.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-24.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-25.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-26.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-27.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-28.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-29.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-30.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-31.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-23-32.png)

``` r
rm(Patient)
```

## Cardiovascular

``` r
Patient = list("A","B","C","D","E","F","G","H")

for (x in Patient){
PlotHR = ggplot(cardiovascular %>% dplyr::filter(Patient == x), aes(x = Time, y = HR, colour = Patient)) +
            geom_line(aes(linetype = Replicate), size = 1)  +
            facet_wrap(~Replicate)
  
PlotrrHRV = ggplot(cardiovascular %>% dplyr::filter(Patient == x), aes(x = Time, y = rrHRV,colour = Patient)) +
            geom_line(aes(linetype = Replicate), size = 1) +
            labs(title = Patient) +
            facet_wrap(~Replicate)

print(PlotHR)  
print(PlotrrHRV)
 
}
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: Removed 32 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

![](DataManipulation_files/figure-markdown_github/unnamed-chunk-24-1.png)

    ## Warning: Removed 32 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

![](DataManipulation_files/figure-markdown_github/unnamed-chunk-24-2.png)

    ## Warning: Removed 50 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

![](DataManipulation_files/figure-markdown_github/unnamed-chunk-24-3.png)

    ## Warning: Removed 50 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

![](DataManipulation_files/figure-markdown_github/unnamed-chunk-24-4.png)

    ## Warning: Removed 48 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

![](DataManipulation_files/figure-markdown_github/unnamed-chunk-24-5.png)

    ## Warning: Removed 48 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

![](DataManipulation_files/figure-markdown_github/unnamed-chunk-24-6.png)

    ## Warning: Removed 40 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

![](DataManipulation_files/figure-markdown_github/unnamed-chunk-24-7.png)

    ## Warning: Removed 43 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

![](DataManipulation_files/figure-markdown_github/unnamed-chunk-24-8.png)

    ## Warning: Removed 23 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

![](DataManipulation_files/figure-markdown_github/unnamed-chunk-24-9.png)

    ## Warning: Removed 25 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

![](DataManipulation_files/figure-markdown_github/unnamed-chunk-24-10.png)

    ## Warning: Removed 39 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

![](DataManipulation_files/figure-markdown_github/unnamed-chunk-24-11.png)

    ## Warning: Removed 39 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

![](DataManipulation_files/figure-markdown_github/unnamed-chunk-24-12.png)

    ## Warning: Removed 28 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

![](DataManipulation_files/figure-markdown_github/unnamed-chunk-24-13.png)

    ## Warning: Removed 28 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

![](DataManipulation_files/figure-markdown_github/unnamed-chunk-24-14.png)

    ## Warning: Removed 41 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

![](DataManipulation_files/figure-markdown_github/unnamed-chunk-24-15.png)

    ## Warning: Removed 41 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

![](DataManipulation_files/figure-markdown_github/unnamed-chunk-24-16.png)

``` r
rm(Patient)
```

## Heart Rate Plot

``` r
Patient = list("Ambras","Lucky","Roomer","Quickster","Vlekje","Venise","Tipsy","Ramona")

for (x in Patient){
name = as.character(x)
PlotHeartRate = ggplot(Heart_parameters %>% dplyr::filter(Horse == x), aes(x= SuposedTime,y=HR,colour=Horse)) +
  geom_line(aes(linetype = Session),size = 1) + labs(title = name) +
  facet_wrap(~Session)
PlotHeartRateVariablity = ggplot(Heart_parameters %>% dplyr::filter(Horse == x & Session != "OPU4"), aes(x= SuposedTime,y=rrHRV,colour=Horse)) +
  geom_line(aes(linetype = Session), size = 1)  + labs(title = name)+
  facet_wrap(~Session)

print(PlotHeartRate)
print(PlotHeartRateVariablity)

}
```

![](DataManipulation_files/figure-markdown_github/unnamed-chunk-25-1.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-25-2.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-25-3.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-25-4.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-25-5.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-25-6.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-25-7.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-25-8.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-25-9.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-25-10.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-25-11.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-25-12.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-25-13.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-25-14.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-25-15.png)![](DataManipulation_files/figure-markdown_github/unnamed-chunk-25-16.png)

``` r
rm(Patient)
```

``` r
x = "Lucky"
PlotHeartRate = ggplot(Heart_parameters %>% dplyr::filter(Horse == x), aes(x= SuposedTime,y=HR,colour=Horse)) +
  geom_line(aes(linetype = Session),size = 1) + 
  geom_vline(aes(xintercept=Needle1,fill="green"),size =1)+ 
  geom_vline(aes(xintercept=Needle2,fill="green"),size =1)+
  labs(title = x) +
  facet_wrap(~Session)
```

    ## Warning in geom_vline(aes(xintercept = Needle1, fill = "green"), size = 1):
    ## Ignoring unknown aesthetics: fill

    ## Warning in geom_vline(aes(xintercept = Needle2, fill = "green"), size = 1):
    ## Ignoring unknown aesthetics: fill

``` r
PlotHeartRateVariablity = ggplot(Heart_parameters %>% dplyr::filter(Horse == x & Session != "OPU4"), aes(x= SuposedTime,y=rrHRV,colour=Horse)) +
  geom_line(aes(linetype = Session), size = 1)  + 
  geom_vline(aes(xintercept=Needle1,fill="green"),size =1)+
  labs(title = x)+
  facet_wrap(~Session)
```

    ## Warning in geom_vline(aes(xintercept = Needle1, fill = "green"), size = 1):
    ## Ignoring unknown aesthetics: fill

``` r
print(PlotHeartRate)
```

    ## Warning: Removed 117 rows containing missing values or values outside the scale range
    ## (`geom_vline()`).

    ## Warning: Removed 117 rows containing missing values or values outside the scale range
    ## (`geom_vline()`).

![](DataManipulation_files/figure-markdown_github/unnamed-chunk-26-1.png)

``` r
print(PlotHeartRateVariablity)
```

    ## Warning: Removed 94 rows containing missing values or values outside the scale range
    ## (`geom_vline()`).

![](DataManipulation_files/figure-markdown_github/unnamed-chunk-26-2.png)

## Sedation

``` r
PlotSedation = ggplot(data=sedation, aes(x=Replicate, y=Detomidine, group=Patient, colour=Patient)) +                                                                                                                                                                                                 
  geom_line(size=1) +
  geom_point(size=2)

print(PlotSedation)
```

![](DataManipulation_files/figure-markdown_github/unnamed-chunk-27-1.png)

``` r
skim(Data)
```

|                                                  |      |
|:-------------------------------------------------|:-----|
| Name                                             | Data |
| Number of rows                                   | 186  |
| Number of columns                                | 13   |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |      |
| Column type frequency:                           |      |
| factor                                           | 4    |
| logical                                          | 1    |
| numeric                                          | 8    |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |      |
| Group variables                                  | None |

Data summary

**Variable type: factor**

| skim_variable      | n_missing | complete_rate | ordered | n_unique | top_counts                 |
|:---------------|--------:|-----------:|:------|-------:|:---------------------|
| Patient            |         0 |             1 | FALSE   |        8 | A: 26, D: 25, G: 24, E: 23 |
| Replicate          |         0 |             1 | FALSE   |        3 | 1: 68, 3: 64, 2: 54        |
| HousingEnvironment |         0 |             1 | FALSE   |        2 | Sto: 122, Sta: 64          |
| Manipulation       |         0 |             1 | FALSE   |        2 | 0: 111, 1: 75              |

**Variable type: logical**

| skim_variable | n_missing | complete_rate | mean | count             |
|:--------------|----------:|--------------:|-----:|:------------------|
| Treatment     |         0 |             1 | 0.66 | TRU: 122, FAL: 64 |

**Variable type: numeric**

| skim_variable       | n_missing | complete_rate |   mean |      sd |       p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:------------|------:|---------:|-----:|-----:|------:|----:|-----:|-----:|-----:|:----|
| Time                |         0 |          1.00 | 626.72 | 1573.14 | -1440.00 |  0.00 |  20.00 | 1440.00 | 4320.00 | ▁▇▂▁▂ |
| Lactate             |         0 |          1.00 |   1.05 |    0.37 |     0.24 |  0.76 |   1.04 |    1.31 |    2.11 | ▃▇▇▃▁ |
| Glucose             |         0 |          1.00 | 106.08 |   25.58 |    59.00 | 87.00 | 101.00 |  121.00 |  193.00 | ▃▇▃▁▁ |
| Cortisol            |         1 |          0.99 |   7.52 |    2.61 |     2.80 |  5.50 |   7.10 |    9.20 |   15.10 | ▅▇▆▃▁ |
| PainScore           |         0 |          1.00 |   5.12 |    3.17 |     0.00 |  2.00 |   6.00 |    7.00 |   12.00 | ▆▁▇▃▁ |
| Punctures           |         0 |          1.00 |   0.51 |    1.16 |     0.00 |  0.00 |   0.00 |    0.00 |    5.00 | ▇▁▁▁▁ |
| PainFive            |         0 |          1.00 |   2.53 |    1.56 |     0.00 |  1.00 |   2.00 |    3.00 |    7.00 | ▅▅▇▁▁ |
| cumulativepunctures |         0 |          1.00 |   2.18 |    2.98 |     0.00 |  0.00 |   0.00 |    5.00 |   10.00 | ▇▁▃▁▁ |

``` r
save(Data,file = "DATA/Data.RData")
save(sedation,file = "DATA/Sedation.RData")
save(cardiovascular,file = "DATA/Cardiovascular.RData")
```
