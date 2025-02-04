```{r setup, include=FALSE}

library(tidyverse)
library(readr)
library(tinytex)

```

## HUBZone DC college student analysis

Summary: Data obtained from https://www.fpds.gov and https://www.usaspending.gov was used to analyze HUBZone contracts awarded to firms in Washington, DC. Data shows that more than $1 billion in federal dollars was awarded since 2000. # However, many of those dollars were awarded to firms in areas considered economically stable. The point of the HUBZone program was to level the playing field between firms in economically stable communities and businesses in areas that are historically neglected. 

load data parsed for federal database

```{r load data}

#dc poverty rate
dc_student_poverty <- read_csv("~dc_enrollment_poverty.csv")

```

the first part of the analysis takes the poverty rate of each tract in DC and remove college students from the poverty universe, finding the poverty rate with students and without.  

```{r pressure, echo=FALSE}

dc_poverty_rate <- group_by(dc_student_poverty, Id2) %>% 
  summarise(total_poverty_rate = sum(poverty_level / total) * 100,
            total_wo_students = sum((poverty_level - (undergraduate +  graduate))) / total * 100,
            pct_point_change = sum(total_wo_students - total_poverty_rate))

```

to qualify for the HUBZone program, a tract must have a poverty rate of 25 percent or higher (no rounding)  

```{r result, echo=FALSE}

dc_below_25 <- filter(dc_poverty_rate, total_poverty_rate > 25, total_wo_students < 25)

dc_below_25

```
