#
#
# Filter to species somewhat attributed to cats.
#
#

library("data.table")

dat <- fread("data/iucn/assessments.csv")
dat

head(dat$threats)
head(dat$rationale)
head(dat$conservationActions)

dat.cat <- dat[grepl("cats", threats, ignore.case = T) |
      grepl("cats", rationale, ignore.case = T) |
      grepl("cats", conservationActions, ignore.case = T) |
      
      grepl("cattus", conservationActions, ignore.case = T) |
      grepl("cattus", conservationActions, ignore.case = T) |
      grepl("cattus", conservationActions, ignore.case = T) |
      
      grepl("feral cat", conservationActions, ignore.case = T) |
      grepl("feral cat", conservationActions, ignore.case = T) |
      grepl("feral cat", conservationActions, ignore.case = T) |
      
      grepl("Felis", threats, ignore.case = T) |
      grepl("Felis", rationale, ignore.case = T) |
      grepl("Felis", conservationActions, ignore.case = T)]
dat.cat


fwrite(dat.cat, "data/iucn/assessment_filtered.csv")








