# Variable System (*VS*)

*VS* (Variable System) is a free open-source statistical package for the analysis of conditional path models based on 
structural equation modeling (SEM). It can be used to examine models with simple mediation, simple moderation, 
moderated mediation (moME), mediated moderation (meMO), and any combinations of them.

*VS* is easy to use. Users only need to specify the conceptual model using simple control languages in R and the 
package will automatically
1. transform the conceptual model into a testable SEM-based model,
2. estimate the basic model parameters, 
3. compute and test the conditional effects of interest, 
4. evaluate the model goodness-of-fit. 

## Install the *VS* package

*VS* is an R package. Before installing the *VS* package, you will need to have R installed on your computer.
While it is possible to use R without RStudio, which is an integrated development environment (IDE) for R,
you may also want to consider installing RStudio as it provides a user-friendly interface and many useful
features for working with R.

### 1. Install R
*VS* package requires R version 4.2.0 or above.  You can download R from the R-project website and select
a version of R that matches your computer operating system.

* [Download and install R](https://cran.rstudio.com/)

### 2. Install RStudio (optional)
While it is possible to use R without RStudio, R beginners are recommended to use R in RStudio.
You can download and install RStudio by following the installation instruction on the RStudio website.

* [Download and install RStudio](https://posit.co/download/rstudio-desktop/)

### 3. Install *VS* package
After you have installed R and RStudio, you can install the *VS* package by starting up R, and
typing and running the following R script:
```{r, eval = FALSE}
install.packages("devtools")
library(devtools)
install_github("VSquanpsy/VS")
```
\
To start using the *VS* package, you can load the *VS* package
```{r, eval = FALSE}
library(VS)
```
\
Try out the following example. If you can see the output, *VS* is successfully installed and you
can now use *VS* for conditional path analysis!
```{r, eval = FALSE}
example(VS_summary)
```
\
To view the *VS* documentation, type:
```{r, eval = FALSE}
help(VS)
```
