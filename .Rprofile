if (file.exists("F:/Dropbox/dropbox")) { dropboxdir <- "F:/Dropbox/" 
} else if (file.exists("D:/Dropbox/dropbox")) { dropboxdir <- "D:/Dropbox/"
} else if (file.exists("D:/Mark/Dropbox/dropbox")) { dropboxdir <- "D:/Mark/Dropbox/"
} else { print('Dropbox directory not found.') }

basedir <- paste(dropboxdir,"Publications/201307 Huh w FT NJE/R/", sep="")
indir <- paste(basedir,"in", sep="")
outdir <- paste(basedir,"out", sep="")

require(gdata)
library(reshape)
library(plyr)
library(RColorBrewer)
require(directlabels)
library(zoo)
library(gmodels)
library(reshape2)
library(ggplot2)
library(scales)
library(lattice)
library(Unicode)
library(vcd)
library(languageR)
library(lme4)
library(devtools)
library(extrafont)
require(directlabels)
library(stringr)
#library(knitr)
#install_github("ggthemes", "jrnold")
#library(ggthemes)

# Build a ggplot theme and set it
windowsFonts(Calibri=windowsFont("TT Calibri"))
windowsFonts(GillSans=windowsFont("Gill Sans MT"))
windowsFonts(Arial=windowsFont("Arial"))

# Gill Sans theme
theme_md <- function (ticks=TRUE, base_family="GillSans", base_size=18) {
  ret <- theme_grey(base_family=base_family, base_size=base_size) +
    theme(
      axis.title.x = element_text(vjust=0),
      axis.title.y = element_text(vjust=0.2),
      plot.margin = unit(c(0, 0, 0.5, 0.5), "lines"),
      legend.title = element_text(colour="grey", face = "plain")
    )
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }
  ret
}
theme_set(theme_md(base_size=18))


# Set the outdir
setwd(outdir)
print(paste("Outdir: ", outdir, sep=""))