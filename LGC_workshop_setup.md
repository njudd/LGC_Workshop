# Latent Growth Curve Workshop Setup

**Lifespan Cognitive Dynamics Lab** - [tutorial code](https://github.com/njudd/LGC_Workshop)

#### 0. (optional) Download a markdown editor

- This allows you to edit our text in `.md` files & take notes. There are many, [marktext](https://www.marktext.cc/) is a lightweight cross-platform option.

#### 1. Make sure you have the latest R downloaded

- Download the latest version for [Windows](https://cran.r-project.org/bin/windows/base/R-4.3.2-win.exe) or [Mac](https://cran.r-project.org/bin/macosx/)
- Type `R.version$version.string` in R it should be **4.4.2** (aka 'Pile of Leaves' with `R.version$nickname`)
- See the 'protip' at the bottom of this document if you want to keep the same packages you downloaded previously.

#### 2. Make sure you have the latest Rstudio downloaded

- You can check this by clicking on the menu `About Rstudio` in the Rstudio or Help interface dropdown.
- If not download the latest version of [Rstudio](https://posit.co/download/rstudio-desktop/)

#### 3. Make sure you have these packages installed

- You can do this by running the following code 

```
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load(lavaan, tidyverse, here, reshape2)
```

##### Protip: Save your current packages to make updating easier!

```
# do this before you update
mypks <- pacman::p_lib() # a list of packages
saveRDS(mypks, "~/mypks.rds") # saving them

# now update R & run the following after
mypks <- readRDS("~/mypks.rds") # load the list of packages
install.packages(mypks) #re-downlaod them
```
