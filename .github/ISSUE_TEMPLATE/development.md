---
name: New Feature Development
about: Work plan for new features
title: 'Development Work'
labels: enhancement
assignees: ''

---

New features should include all of the following work:

* [ ] Create the function/code 
* [ ] Create tests in tests/testthat folder
* [ ] Create help file using roxygen2 above code.
* [ ] Create working examples in help file (via roxygen2)
* [ ] Add to appropriate vignette (or create new one)
* [ ] Add example README
* [ ] Check that `devtools::check()` produces no errors, warnings, or notes
* [ ] Check that `covr::package_coverage()` has not significantly decreased

Feel free to create independent issues for each of these subtasks. 
This task should not be considered completed until all of the above boxes are checked, or a reasonable argument is made for ignoring a section (ie...not appropriate for top-level README overview).
