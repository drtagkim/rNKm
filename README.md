# rNKm
NK Landscape Modeling Toolkit for R

Complex adaptive systems can be characterized as consisting of a large number of components that can interact with each other. Kauffman's (1993) NK model is one of practical approaches that describe real world events closely relating to business events. In this R package, you can find useful functions and procedural codes to construct NK models with ease. Kauffman's (1993) NK model is originally developed in the context of evolutionary biology; however, we believe that numerous management scholars can use it for simulating a complex phenomena to develop theoretical ideas.

**Contributors:**
 - Taekyung Kim
 - Jungpil Hahn

**Maintainer:**

 - Taekyung Kim (kimtk@suwon.ac.kr)


## How to Install

In order to use **rNKm**, you must install R and RStudio, which means you probably want to visit [R CRAN](https://cran.r-project.org) and [RStudio Main Page](http://www.rstudio.com). Install them one by one. R is the first and RStudio is next.

After install both of them, run RStudio and find the console tab. Since rNKm is a work-in-progress package, you need to use **{devtools}** for installation. Let's install {devtools}.  

    > install.packages("devtools")
    > install.packages("stringi")

Make sure your computer is connected  to the Internet.

Let's install rNKm from the GitHub repository.

    > library(devtools)
    > install_github("drtagkim/rNKm")

