# Flexible_harmonic_chorale_annotations

This page is a companion to the <a href="http://ismir2018.ircam.fr/doc/pdfs/283_Paper.pdf">paper</a> presented at the <a href="https://www.ismir.net">International Society for Music Information Retrieval</a> <a href="https://ismir2018.ircam.fr">2018 conference</a> in Paris France.

The kernData folder contains raw humdrum-format (\*\*kern) files of the 571 chorales used in the project: 371 by J.S. Bach and 200 by Michael Pr&aelig;torius.
The 371 Bach chorales originated from <a href="http://kern.ccarh.orh">Kern Scores</a>, but some corrections have been made (annotated in reference records, i.e., 184, 194, 246, 329) and one chorale (the five-part chorale, 150) was has been added to the set.
The 200 Pr&aelig;torius chorales were newly created for this project, and are not available anywhere else.


# FlexibleChoraleHarmonicAnalysis R package usage

The R package `FlexibleChoraleHarmonicAnalysis` contains the permutational analysis data for the 571 chorales, as well as several functions for filtering analyses.

To run this package locally, please refer to the following steps:

1. Download and install R at: https://www.r-project.org/
2. Download and install the "Rstudio Desktop Open Source Edition" at: https://rstudio.com/products/rstudio/
3. Clone the whole project to your local drive using: git clone https://github.com/DDMAL/Flexible_harmonic_chorale_annotations.git and switch into `dev` branch (this branch) using `git checkout dev` command.
4. Open Rstudio, and a few extra packages need to be downloaded and installed. Please type `options(timeout=10000)` in the terminal, since Rstudio only allows for 60 seconds to download the package, and we can specify the time as 10,000 seconds to give more time for download. 
5. Run `install.packages("data.table")`, `install.packages("stringr")`, `install.packages("rlang")`, `install.packages("readr")`, `install.packages("rstudioapi")`, `install.packages("plyr")`, `install.packages("purrr")`, and `install.packages("lazyeval")` in order.
6. After, install the `composeR_0.0.0.9000.tar.gz` package from the cloned repo. You can do so by clicking `Tools`-`Install Packages` in the GUI. Then install the `FlexibleChoraleHarmonicAnalysis_0.1.1.tar.gz` package from the cloned repo in the same manner.
7. Finally, load this rule-based algorithm package by running `library(FlexibleChoraleHarmonicAnalysis)`.

# Generating "maximally harmonic" chord labels 

This rule-based algorithm was used in the paper "An Interactive Workflow for Generating Chord Labels for Homorhythmic Music in Symbolic Formats" to generate "maximally harmonic" chord labels, which uses a combination of five ordered filters. The code of these filters can be found in Line 171 of the R script called `ChoraleAnalysis.R` under the `R` folder. To replicate these chord labels, select all the code and click `Run`, the resulting chord label annotations and the music will be automatically generated for the 571 chorales, saved in the home directory of your OS.


