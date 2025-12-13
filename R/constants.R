# Filter Constants
# ----------------
# These values are used throughout the codebase to indicate article filter status.
# 0 = include (show by default)
# 1 = editorial/frontmatter (hide)
# 2 = AI classified as not relevant (hide)
# 3 = Science filter - short/missing abstract (hide)
# 4 = Nature filter - not research article (hide)
# -1 = error during processing

FILTER_INCLUDE <- 0
FILTER_EDITORIAL <- 1
FILTER_AI_EXCLUDED <- 2
FILTER_SCIENCE <- 3
FILTER_NATURE <- 4
FILTER_ERROR <- -1

# Disciplines
DISCIPLINES <- c("communication", "politics", "po", "psych", "sociology", "multidisciplinary")

# Preprint servers
PREPRINT_SERVERS <- c("PsyArxiv", "SocArxiv", "MetaArxiv", "MediArxiv")
