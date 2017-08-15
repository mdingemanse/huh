# Huh project - create data table
# First version of script by FT
# Additions, changes, and comments by MD
# --------------------------------------

setwd(indir)
d = read.delim('table.txt')

# Intonation
# ----------
# Start at 0 (= level)
# Add 1 for every "r" (a vote upwards)
# Substract 1 for every "f" (a vote downwards)
# Score "x" as NA

d$int = 0
d[d$iF=='r',]$int = d[d$iF=='r',]$int + 1
d[d$iM=='r',]$int = d[d$iM=='r',]$int + 1
d[d$iN=='r',]$int = d[d$iN=='r',]$int + 1

d[d$iF=='f',]$int = d[d$iF=='f',]$int - 1
d[d$iM=='f',]$int = d[d$iM=='f',]$int - 1
d[d$iN=='f',]$int = d[d$iN=='f',]$int - 1

d[d$iF=='x',]$int = NA
d[d$iM=='x',]$int = NA
d[d$iN=='x',]$int = NA

# Closure
# -------
# Start at 2 (= open)
# Subtract 2 for every "c" (a vote towards closure)
# Subtract 1 for every "i" (a vote for intermediate)
# Score "x" as NA

d$mouth = 2
d[d$mF=='c',]$mouth = d[d$mF=='c',]$mouth - 2
d[d$mM=='c',]$mouth = d[d$mM=='c',]$mouth - 2
d[d$mN=='c',]$mouth = d[d$mN=='c',]$mouth - 2

d[d$mF=='i',]$mouth = d[d$mF=='i',]$mouth - 1
d[d$mM=='i',]$mouth = d[d$mM=='i',]$mouth - 1
d[d$mN=='i',]$mouth = d[d$mN=='i',]$mouth - 1

d[d$mF=='x',]$mouth = NA
d[d$mM=='x',]$mouth = NA
d[d$mN=='x',]$mouth = NA

# Nasalisation
# ------------
# Start at 0 (= oral)
# Add 1 for every "n" (a vote towards nasalisation)
# Score "x" as NA

d$nasal = 0
d[d$nF=='n',]$nasal = d[d$nF=='n',]$nasal + 1
d[d$nM=='n',]$nasal = d[d$nM=='n',]$nasal + 1
d[d$nN=='n',]$nasal = d[d$nN=='n',]$nasal + 1

d[d$nF=='x',]$nasal = NA
d[d$nM=='x',]$nasal = NA
d[d$nN=='x',]$nasal = NA

# Vowel quality
# -------------
# The starting point is "a", defined as an low/open central vowel
# For height, start at 0 (=low/open)
# Add 1 for every "e" or "@" (schwa)
# Score "x" as NA

d$height = 0
d[d$vF%in%c('e', '@'),]$height = d[d$vF%in%c('e', '@'),]$height + 1
d[d$vM%in%c('e', '@'),]$height = d[d$vM%in%c('e', '@'),]$height + 1
d[d$vN%in%c('e', '@'),]$height = d[d$vN%in%c('e', '@'),]$height + 1

d[d$vF=='x',]$height = NA
d[d$vM=='x',]$height = NA
d[d$vN=='x',]$height = NA

# For frontness, start at 0 (=central)
# Add 1 for every "e" or "E" (BrEn /æ/), moving towards more fronting
# Score "x" as NA

d$front = 0
d[d$vF%in%c('e', 'E'),]$front = d[d$vF%in%c('e', 'E'),]$front + 1
d[d$vM%in%c('e', 'E'),]$front = d[d$vM%in%c('e', 'E'),]$front + 1
d[d$vN%in%c('e', 'E'),]$front = d[d$vN%in%c('e', 'E'),]$front + 1

d[d$vF=='x',]$front = NA
d[d$vM=='x',]$front = NA
d[d$vN=='x',]$front = NA

# Possibly:
# Add 2 to both front and height for every "o" (which is a token judged to be outside the e/E/a/@ space)
d[d$vF=='o',]$height = d[d$vF=='o',]$height #+ 2
d[d$vM=='o',]$height = d[d$vM=='o',]$height #+ 2
d[d$vN=='o',]$height = d[d$vN=='o',]$height #+ 2

d[d$vF=='o',]$front = d[d$vF=='o',]$front #+ 2
d[d$vM=='o',]$front = d[d$vM=='o',]$front #+ 2
d[d$vN=='o',]$front = d[d$vN=='o',]$front #+ 2




# Onset aspiration
# ----------------
# Start at 0 (zero, i.e. no onset heard)
# Add 1 for every "h" (aspiration heard)
# Score "g" (glottal) and "x" (not audible) as NA
# (Why? Because g and h are mutually exclusive.)

d$asp = 0
d[d$oF=='h',]$asp = d[d$oF=='h',]$asp + 1
d[d$oM=='h',]$asp = d[d$oM=='h',]$asp + 1
d[d$oN=='h',]$asp = d[d$oN=='h',]$asp + 1

d[d$oF=='g',]$asp = NA
d[d$oM=='g',]$asp = NA
d[d$oN=='g',]$asp = NA

d[d$oF=='x',]$asp = NA
d[d$oM=='x',]$asp = NA
d[d$oN=='x',]$asp = NA


# Onset glottalisation
# --------------------
# Start at 0 (zero, i.e. no onset heard)
# Add 1 for every "g" (glottal stop heard)
# Score "h" (aspiration) and "x" (not audible) as NA
# (Why? Because g and h are mutually exclusive.)

d$glot = 0
d[d$oF=='g',]$glot = d[d$oF=='g',]$glot + 1
d[d$oM=='g',]$glot = d[d$oM=='g',]$glot + 1
d[d$oN=='g',]$glot = d[d$oN=='g',]$glot + 1

d[d$oF=='h',]$glot = NA
d[d$oM=='h',]$glot = NA
d[d$oN=='h',]$glot = NA

d[d$oF=='x',]$glot = NA
d[d$oM=='x',]$glot = NA
d[d$oN=='x',]$glot = NA

# Onset combined
# --------------
# Start at 0 (zero, i.e. no onset heard)
# Add 1 for every "h"
# Substract 1 for every "g"
# Score "x" as NA

d$onset = 0
d[d$oF=='h',]$onset = d[d$oF=='h',]$onset + 1
d[d$oM=='h',]$onset = d[d$oM=='h',]$onset + 1
d[d$oN=='h',]$onset = d[d$oN=='h',]$onset + 1

d[d$oF=='g',]$onset = d[d$oF=='g',]$onset - 1
d[d$oM=='g',]$onset = d[d$oM=='g',]$onset - 1
d[d$oN=='g',]$onset = d[d$oN=='g',]$onset - 1

d[d$oF=='x',]$onset = NA
d[d$oM=='x',]$onset = NA
d[d$oN=='x',]$onset = NA


# Combined codes
# --------------
# in the order FT, MD, NJE

d$vC = paste(d$vF, d$vM, d$vN)
d$iC = paste(d$iF, d$iM, d$iN)
d$oC = paste(d$oF, d$oM, d$oN)
d$nC = paste(d$nF, d$nM, d$nN)
d$mC = paste(d$mF, d$mM, d$mN)

# Write the table
# ---------------
setwd(basedir)
write.table(d, file="data.txt")