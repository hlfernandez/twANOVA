twANOVA
========================

A simple web application implemented with R and Shiny to perform twi way ANOVA with didactical purposes, allowing students to see all the ANOVA required calculations.

Requirements
------------
To run this application you need:
  - R (http://www.r-project.org/)
  - Shiny (http://shiny.rstudio.com/, it can be installed with `install.packages("shiny")`)
  
Installation and usage
------------
### 1. Download
You can also download the twANOVA project directly from Github using the following command:
`git clone https://github.com/hlfernandez/twANOVA`

### 2. Launch the application

Go to the twANOVA's project base directory, run R and then run the following commands:
```R
library("shiny")
runApp("./")
```

The application is opened in the default web browser.

![Screenshot](https://raw.github.com/hlfernandez/twANOVA/master/screenshots/screenshot.png)

Tests
------------
### 1. Two-way ANOVA

To test the correctness of the implementation, a test script called `example.R` checks that the results obtained are the same.

```
> source("example.R")
AB Matrix:

        a1      a2      a3 
b1      165     150     100     415
b2      175     150     200     525
b3      190     185     260     635
        530     485     560     1575

Partial calculations:
A = 55315 
B = 56738.33 
AB = 58155 
T = 55125 
Y = 60265 

ANOVA summary table with the effect of the interaction:
S.V     Sum Sq  Df      Mean Sq F value Pr(>f)
A        190     2       95      1.6209          0.211828   
B        1613.33         2       806.67          13.763          3.6e-05  *** 
AxB      1226.67         4       306.67          5.2322          0.001992  ** 
S/AxB    2110    36      58.61 
Total    2110    44 

ANOVA summary table without the effect of the interaction:
S.V     Sum Sq  Df      Mean Sq F value Pr(>f)
A        190     2       95      1.1389          0.330345   
B        1613.33         2       806.67          9.6703          0.000375  *** 
Intra (S/AxB)    3336.67         40      83.42 
Total    5140    44 


Result without the effect of the interaction using aov(response ~  A+B , data=dataF):
            Df Sum Sq Mean Sq F value   Pr(>F)    
A            2    190    95.0   1.139 0.330345    
B            2   1613   806.7   9.670 0.000375 ***
Residuals   40   3337    83.4                     
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Result with the effect of the interaction using aov(response ~  A*B , data=dataF):
            Df Sum Sq Mean Sq F value   Pr(>F)    
A            2    190    95.0   1.621  0.21183    
B            2   1613   806.7  13.763 3.63e-05 ***
A:B          4   1227   306.7   5.232  0.00199 ** 
Residuals   36   2110    58.6                     
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

```