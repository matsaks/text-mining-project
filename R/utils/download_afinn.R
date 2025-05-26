# Download and save AFINN lexicon
library(tidytext)

# Create a temporary file to store the lexicon
temp_file <- tempfile()
download.file("https://raw.githubusercontent.com/fnielsen/afinn/master/afinn/data/AFINN-111.txt", temp_file)

# Read the lexicon
afinn <- read.delim(temp_file, header = FALSE, stringsAsFactors = FALSE)
names(afinn) <- c("word", "value")

# Save the lexicon
saveRDS(afinn, "data/raw/afinn.rds")
cat("AFINN lexicon saved to data/raw/afinn.rds\n")

# Clean up
unlink(temp_file)
