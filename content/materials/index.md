---
output: hugodown::md_document
title: "Materials"
type: "page"
summary: "Course Material"
author: ""
tags: []
date: 2021-01-30
rmd_hash: 00fdce269dc44758

---

Course Material
===============

Links to lecture slides, YouTube videos and other resources are all available on this page. You'll probably notice that some of the resources for the first few weeks refer to the class as ["robust tools"](https://robust-tools.djnavarro.net/), which was the name of class that I ran for my honours elective in 2020. The 2021 class partially overlaps with that class, so some material is reused.

Weeks 1-2
---------

The first section of the class provides an introduction to data visualisation in R using the ggplot2 package. It is also assumed to be the *first* contact a student has with R, so it covers a lot of the introductory topics like "what is an R commmand?", "how do I write a script?" and so on. It is designed so that you can do it without me: open up the slides and the RStudio Cloud project (both linked below) and then watch the YouTube videos; these will direct you to various hands on exercises you can do within the RStudio Cloud project.

-   [data visualisation slides](https://slides.djnavarro.net/starting-ggplot2)
-   [rstudio cloud project 901338](https://rstudio.cloud/project/901338)
-   [YouTube playlist](https://www.youtube.com/watch?v=6vdHGnD51F8&list=PLRPB0ZzEYegPa4uvvAVJnr6loSKbN4wLb&index=2)

<br>

Week 3
------

The next part of the class is a bridging section. It introduces a data set (the reasoning data) that I'll use later in the class, discusses reading data from and writing data to a CSV file, and introduces the pipe. Much of this section is devoted to a discussion of calculating "grouped summaries".

-   ["Hello data" slides](https://slides.djnavarro.net/starting-readr)
-   [RStudio cloud project 978818](https://rstudio.cloud/project/978818)
-   [YouTube playlist](https://www.youtube.com/watch?v=lpbqlu_Sk3M&list=PLRPB0ZzEYegPYKRLwJRh4AbWWSiIlzmC7)
-   [The reasoning experiment](https://robust-tools.djnavarro.net/reasoning/)

<br>

Weeks 4-5
---------

The third section in the class provides an introduction to data wrangling in R, mostly focusing on dplyr. Topics covered include: extracting a subset of the data, rearranging the data, computing new variables in a data set, merging multiple data sets, and pivoting data between "long" and "wide" forms.

-   [Data wrangling slides](https://slides.djnavarro.net/starting-dplyr)
-   [RStudio cloud project 1006868](https://rstudio.cloud/project/1006868)
-   [YouTube playlist](https://www.youtube.com/watch?v=_LiB4nUKi3o&list=PLRPB0ZzEYegNFsivhQWTFvKvpEc504EPH)

<br>

Weeks 6-8
---------

The plan for weeks 6-8 is for you to explore the world of data visualisation by heading along your own path. Some free resources you might find very useful. First, two books on data visualisation:

-   [Data Visualization: A Practical Introduction](https://socviz.co/) by Kieran Healy is a very good introduction. It covers similar territory to this class, with a little more detail on the R programming and the psychology of data visualisation.
-   [Fundamentals of Data Visualization](https://clauswilke.com/dataviz/) by Claus Wilke is a thorough text that covers a lot of ground, and has a lot of practical guidance.

Some general purpose resources:

-   All my slides and youtube videos are online at <a href="https://slides.djnavarro.net" class="uri">https://slides.djnavarro.net</a>. Most of them you will have already seen through this course, but a few will be new!
-   [R for Data Science](https://r4ds.had.co.nz/) by Hadley Wickham and Garrett Grolemund is a fantastic general introduction to data science in R. The visualisation content donsn't go much beyond what's in this class, but it teaches a lot of other skills that come in handy when trying to visualise real world data.

Some R packages that you might like to explore:

-   The [gganimate](https://gganimate.com/) package by Thomas Lin Pedersen extends ggplot2 to allow you to create animated gifs as graphics.
-   The [patchwork](https://patchwork.data-imaginist.com/) package, again by Thomas Lin Pedersen, makes it possible to compose multiple ggplot2 plots into a single image (very handy in real life!)
-   The [gghighlight](https://yutannihilation.github.io/gghighlight/index.html) package by Hiroaki Yutani allows you to easily "highlight" a subset of the data.  
-   The [leaflet](https://rstudio.github.io/leaflet/) package allows you to create interactive maps in the style of google maps etc. Note that it is not based on ggplot2, so it has a very different style.

That being said, there is a *huge* ecosystem of packages out there that are built on top of ggplot2, so if there's something you'd like to try doing, it's worth googling around to see what you can find, and if you can't find anything let me know and I'll see if I can find it!

A general strategy for doing this kind of exploration (which you don't have to follow) is to start out by finding a fun data set that you want to play around with, and then try to find the tools that would let you draw the awesomest graphic you can think of to make sense of the data!

Most importantly... please feel free to message me on slack with ideas, suggestions, or questions! It's what I'm here for 😄

Week 9
------

Details TBA

<br><br><br>

