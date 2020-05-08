---
name: Question
about: Ask a question
title: ''
labels: question
assignees: ''

---

**What is your question?**
A clear and concise description of the question.

**To Reproduce**
If possible, narrow down the question to a `dataRetrieval` query:

```r
library(dataRetrieval)
questionable_query <- readNWISdv("a","b","c","d")
```

**Expected behavior**
A clear and concise description of what you expected to happen.

**Screenshots**
If applicable, add screenshots to help explain your problem.

**Session Info**
Please include your session info:
```r
sessionInfo()
#OR preferred:
devtools::session_info()
```

**Additional context**
Add any other context about the problem here.
