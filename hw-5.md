hw 5
================
Wurongyan Zhang
11/5/2019

## problem 1

``` r
library(tidyverse)
```

    ## ─ Attaching packages ────────────────────────── tidyverse 1.2.1 ─

    ## ✔ ggplot2 3.2.1     ✔ purrr   0.3.2
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
    ## ✔ tidyr   1.0.0     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ─ Conflicts ──────────────────────────── tidyverse_conflicts() ─
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(ggplot2)

set.seed(10)

iris_with_missing = iris %>% 
  map_df(~replace(.x, sample(1:150, 20), NA)) %>%
  mutate(Species = as.character(Species))
```

``` r
summary(is.na(iris_with_missing)) %>% 
  knitr::kable()
```

|  | Sepal.Length  | Sepal.Width   | Petal.Length  | Petal.Width   |    Species    |
|  | :------------ | :------------ | :------------ | :------------ | :-----------: |
|  | Mode :logical | Mode :logical | Mode :logical | Mode :logical | Mode :logical |
|  | FALSE:130     | FALSE:130     | FALSE:130     | FALSE:130     |   FALSE:130   |
|  | TRUE :20      | TRUE :20      | TRUE :20      | TRUE :20      |   TRUE :20    |

As we can see from the summary above, the data set iris\_with\_missing
has 20 missing values in each of the 5 variables.

``` r
na_func = function(x){
  if(is.character(x)){
    x=replace_na(x,"virginica")
  }
  else if(is.numeric(x)){
    x=replace_na(x, round(mean(x,na.rm=TRUE),1))
  }
  x
}

iris=map_dfr(iris_with_missing,na_func)

summary(is.na(iris))%>% 
  knitr::kable()
```

|  | Sepal.Length  | Sepal.Width   | Petal.Length  | Petal.Width   |    Species    |
|  | :------------ | :------------ | :------------ | :------------ | :-----------: |
|  | Mode :logical | Mode :logical | Mode :logical | Mode :logical | Mode :logical |
|  | FALSE:150     | FALSE:150     | FALSE:150     | FALSE:150     |   FALSE:150   |

As we can see from the second table, there are no missing values after
the function.

## problem 2

``` r
file = list.files("data")

file_func=function(x){
  path=str_c("./data/",x)
  read_csv(path)
 }

file_data = purrr::map_dfr(file, file_func) %>% 
 janitor::clean_names() %>% 
  mutate(file_name=file) %>% 
  mutate(file_name=str_remove(file_name,".csv")) %>% 
  separate(file_name, into = c("arm","subject_id"),sep="_") %>% arrange(arm,subject_id) %>% 
  select(subject_id, arm, everything())
```

``` r
file_data_week=file_data %>% 
  pivot_longer(week_1:week_8,
               names_to="weeks",
               values_to = "values") %>% 
  separate(weeks, into = c("week","weeks"),sep = "_")

file_data_week %>% 
  ggplot(aes(x=weeks, y=values, group=subject_id, color=subject_id))+
  geom_line()+facet_grid(~arm) +
  labs(title = "Observations on each subject over time")
```

![](hw-5_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

\#comments

## problem 3
