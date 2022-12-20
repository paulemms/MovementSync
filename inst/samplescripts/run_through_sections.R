# Sources all the specification section scripts contained in the *installed* package
rm(list = ls())

section_dir <- system.file('samplescripts', package = 'movementsync')
spec_scripts <- list.files(section_dir, pattern = '^\\d.*?', full.names = TRUE)

lapply(spec_scripts, source)
