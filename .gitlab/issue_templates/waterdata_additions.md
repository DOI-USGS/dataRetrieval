---
name: New waterdata function
about: Checklist for adding new USGS waterdata endpoint
---

New features should include all of the following work:

* [ ] Create the `read_waterdata_` file. File should:
  - [ ] Update endpoint name
  - [ ] Update parameter list in Roxygen section
  - [ ] Update arguments
  - [ ] Update examples
  - [ ] Think about if sorting looks right, if convertType should be used, etc.
* [ ] Add endpoint name to AAA.R `.onLoad`
* [ ] Add deprecate notice to corresponding NWIS function
* [ ] Comment out or delete deprecated NWIS examples
* [ ] Replace any NWIS tests with new waterdata function
* [ ] Create new tests in tests/testthat folder for unique situations
* [ ] Add example to read_waterdata_functions.Rmd vignette
* [ ] Update Status.Rmd vignette
* [ ] Update dataRetrieval.Rmd vignette
* [ ] Update tutorial.Rmd vignette
* [ ] Update NEWS
* [ ] Update _pkgdown.yml
* [ ] Check if README needs to be update
* [ ] Update version (for development, only bump up last 4 digits)

