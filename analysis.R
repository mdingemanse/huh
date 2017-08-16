# Huh project analysis and plots
# Mark Dingemanse 2012-13
# ------------------------------


# Preliminaries -----------------------------------------------------------

# check for /in/ and /out/ directories (create them if needed)
add_working_dir <- function(x) { if(file.exists(x)) { cat(x,"dir:",paste0(getwd(),"/",x,"/")) } else { dir.create(paste0(getwd(),"/",x)) 
  cat("subdirectory",x,"created in",getwd()) } }
add_working_dir("in")
add_working_dir("out")

# Packages and useful functions
list.of.packages <- c("tidyverse","ggthemes","GGally","directlabels","reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)
rm(list.of.packages,new.packages)

# Load data ---------------------------------------------------------------

d = read.csv('in/data.txt', sep=" ")

# Select languages for which we have =<10 data points
numcases <- plyr::count(d,'language')
plus10 <- as.vector(numcases[which(numcases$freq > 9),]$language)
plus15 <- as.vector(numcases[which(numcases$freq > 14),]$language)
d.s <- droplevels(d[d$language %in% plus10,])
cases <- plyr::count(d.s,'language')

# Standard deviation of cases/language
sd(cases[,2])

# Visualisations ----------------------------------------------------------

# Build a ggplot theme and set it
windowsFonts(GillSans=windowsFont("Gill Sans MT"))

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


# Vowel quality -----------------------------------------------------------

# Create table of vowels by language
v = d.s[ , c("language", "height", "front")]
# Get rid of entries with NA values
v <- v[!is.na(v$height), ]


# draw every language at its mean front/height
v.langs <- v %>%
  group_by(language) %>%
  summarise(height=mean(height),front=mean(front)) %>%
  mutate(height = 3-height)

# v.langs$height = 3 - v.langs$height # invert value so that axis doesn't need to be reversed

languages <- levels(v.langs$language)
# abbreviate the languages to their first letters
languages <- substr(levels(v.langs$language),1,3)

# break height into Low/Mid/Heigh to match individual Lg plots
ggplot(v.langs, aes(height, front, label=languages)) +
  ylab("Height") +
  xlab("Backness") +
  coord_cartesian(ylim=c(-.5,6.5),xlim=c(-.5,6.5)) +
  scale_y_continuous(breaks=seq(0,6,3), labels=c("Low","Mid","High")) +
  scale_x_continuous(breaks=seq(0,6,3), labels=c("Front","Central","Back")) +
  scale_colour_manual(values=brewer.pal(10,"Paired")) +
  geom_dl(aes(label=languages),method=list("smart.grid",fontface="bold",cex=1))
ggsave(file="out/Figure_2_Languages_in_vowel_space.png", width=6.5,height=6)

# front/height plots by language with some jitter and alpha
cases <- sum(unlist(table(v$height)))
ggplot(v, aes(height, front)) +
  geom_point(alpha=5/10,size=5,position = position_jitter(width = .2, height = .2)) +
  ylab("Height") +
  xlab("Backness") +
  coord_cartesian(ylim=c(-.5,3.5),xlim=c(-.5,3.5)) +
  scale_y_discrete(breaks=seq(0,3,1), labels=c("Low","","","Mid")) +
  scale_x_reverse(breaks=seq(0,3,1), labels=c("Central","","","Front")) +
  theme(panel.grid.minor=element_blank(),legend.position = "none") +
  facet_wrap(~language)
ggsave(file=paste("out/Figure_3_Vowel_quality_n",cases,".png", sep=""), width=9,height=6)



# Formants ----------------------------------------------------------------

# Formant data for Spanish and Cha'palaa
formants = read.delim('in/formantData.txt')

formants$lg <- substr(formants$language, 1,1)

ggplot(formants, aes(f2, f1)) +
  geom_point(size=4,alpha=0) +
  coord_cartesian(xlim=c(2500,900),ylim=c(1000,250)) +
  scale_x_reverse() +
  scale_y_reverse() +
  ylab("F1 (Hz)") +
  xlab("F2 (Hz)") +
  geom_dl(aes(label=formants$lg),method=list(fontface="bold",cex=1))
ggsave(file="out/Figure_4_Formants.png", width=6.5,height=6)

# Intonation --------------------------------------------------------------

# Create table of intonation by language
i = d.s[ , c("language", "int")]

# order data by relative frequency of a level of a factor
i.o <- transform(i, language = ordered(language, levels = names( sort(prop.table(table(i$int, i$language),2)[1,]))))

# remake product plot using ggplot / ggmosaic
i.f <- i %>%
  drop_na(int) %>%
  group_by(language,int) %>%
  mutate(freq=n()) %>%
  group_by(language) %>%
  mutate(prop=freq/n()) %>%
  mutate(intonation = int) %>%
  distinct()

ggplot(i.f,aes(language,intonation,size=prop)) +
  ylab("Intonation") +
  xlab("") +
  scale_size(range=c(1,12)) +
  geom_point(shape=15) + guides(size=F) +
  scale_y_continuous(minor_breaks=NULL,breaks=seq(-3,3,1), labels=c("Falling","","","Level","","","Rising")) +
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave(file=paste("out/Figure_5_Intonation_n",cases,".png", sep=""), width=6,height=6)

# Intonation: pitch tracks ------------------------------------------------

# load files into list, melt list
files <- list.files(path="in/pitch_data",pattern=".pitch")
filenames <- paste0("in/pitch_data/",files)
pitch <- lapply(filenames, read.delim, header = TRUE, comment = "A")
names(pitch) <- gsub(".pitch","",files)
pitch <- melt(pitch, id=c('time','st'))
pitch$L1 <- as.factor(pitch$L1)

# Add language column
pitch$language <- NA
pitch[grep("Sp",pitch$L1),]$language <- "Spanish"
pitch[grep("Ch",pitch$L1),]$language <- "Chapalaa"
pitch$language <- as.factor(pitch$language)

# Compute means and normalised semitones
d.m <- pitch %>%
  group_by(L1) %>%
  mutate(mean_st = mean(st)) %>%
  mutate(norm_st = st - mean_st)

# Plot
ggplot(d.m, aes(time,norm_st,fill=L1)) + 
  geom_line(size = 0.6) + 
  xlab("Normalised time") +
  ylab("Centered pitch (semitones)") +
  coord_cartesian(ylim=c(-10,10)) +
  scale_x_continuous(breaks=c(0,.2,.4,.6,.8,1.0)) +
  facet_grid(. ~ language)
ggsave(file="out/Figure_6_Pitch_tracks.png", width=6,height=3.5)


# Onset -------------------------------------------------------------------

# Combined onset data, from glottal stop to zero to aspiration
onset = d.s[ , c("language", "onset")]
cases <- sum(unlist(table(d.s$onset)))

o.f <- onset %>%
  drop_na(onset) %>%
  group_by(language,onset) %>%
  mutate(freq=n()) %>%
  group_by(language) %>%
  mutate(prop=freq/n()) %>%
  distinct()

ggplot(o.f,aes(language,onset,size=prop)) +
  scale_size(range=c(1,12)) +
  geom_point(shape=15) + guides(size=F) +
  ylab("Onset") +
  xlab("") +
  scale_y_continuous(minor_breaks=NULL,breaks=seq(-3,3,1), labels=c("Glottal stop","","","Ø","","","Aspiration")) +
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave(file=paste("out/Figure_7_Onset_n",cases,".png", sep=""), width=9,height=6)


# Nasality ----------------------------------------------------------------

nasal = d.s[ , c("language","nasal")]
cases <- sum(unlist(table(d.s$nasal)))

n.f <- nasal %>%
  drop_na(nasal) %>%
  group_by(language,nasal) %>%
  mutate(freq=n()) %>%
  group_by(language) %>%
  mutate(prop=freq/n()) %>%
  distinct()

ggplot(n.f,aes(language,nasal,size=prop)) +
  scale_size(range=c(1,12)) +
  geom_point(shape=15) + guides(size=F) +
  ylab("Nasalisation") +
  xlab("") +
  scale_y_continuous(minor_breaks=NULL,breaks=seq(1,3,1), labels=c("Oral","","Nasal")) +
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave(file=paste("out/Figure_S1_Nasality_n",cases,".png", sep=""), width=9,height=6)


# Mouth closure (aperture) ------------------------------------------------

mouth = d.s[ , c("language", "mouth")]
cases <- sum(unlist(table(d.s$mouth)))

m.f <- mouth %>%
  drop_na(mouth) %>%
  group_by(language,mouth) %>%
  mutate(freq=n()) %>%
  group_by(language) %>%
  mutate(prop=freq/n()) %>%
  distinct()

ggplot(m.f,aes(language,mouth,size=prop)) +
  scale_size(range=c(1,12)) +
  geom_point(shape=15) + guides(size=F) +
  ylab("Mouth closure") +
  xlab("") +
  scale_y_continuous(minor_breaks=NULL,breaks=seq(-4,2), labels=c("Closed","","","Intermediate", "","", "Open")) +
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave(file=paste("out/Figure_S2_Closure_n",cases,".png", sep=""), width=9,height=6)



# Onset aspiration --------------------------------------------------------

# N.B. zero in this plot means coded as zero, not coded as "g" (which is also "not asp").

asp = d.s[ , c("language", "asp")]
cases <- sum(unlist(table(d.s$asp)))

a.f <- asp %>%
  drop_na(asp) %>%
  group_by(language,asp) %>%
  mutate(freq=n()) %>%
  group_by(language) %>%
  mutate(prop=freq/n()) %>%
  distinct()

ggplot(a.f,aes(language,asp,size=prop)) +
  scale_size(range=c(1,12)) +
  geom_point(shape=15) + guides(size=F) +
  ylab("Aspiration at onset") +
  xlab("") +
  scale_y_continuous(minor_breaks=NULL,breaks=c(0:3), labels=c("Ø","","","h")) +
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave(file=paste("out/Onset aspiration - n",cases,".png", sep=""), width=9,height=6)


# Onset: glottis action ---------------------------------------------------

# N.B. zero in this plot means coded as zero, not coded as "h" (which is also "not glot").

glot = d.s[ , c("language", "glot")]
cases <- sum(unlist(table(d.s$glot)))

g.f <- glot %>%
  drop_na(glot) %>%
  group_by(language,glot) %>%
  mutate(freq=n()) %>%
  group_by(language) %>%
  mutate(prop=freq/n()) %>%
  distinct()

ggplot(g.f,aes(language,glot,size=prop)) +
  scale_size(range=c(1,12)) +
  geom_point(shape=15) + guides(size=F) +  ylab("Glottal stop onset") +
  xlab("") +
  theme(axis.text.y = element_text(family="Times New Roman")) +
  scale_y_continuous(minor_breaks=NULL,breaks=c(0:3), labels=c("Ø", "","", "ʔ")) +
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave(file=paste("out/Onset glottal - n",cases,".png", sep=""), width=9,height=6)

