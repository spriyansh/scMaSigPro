## scMaSigPro 0.1.0 (Upcoming)

* New 
    * functions for read/write operations of the 'ScMaSigPro' objects.
        * Support for read/write in H5 format, rds, rData.
    * New slot 'Misc' in the 'ScMaSigPro' class to store additional information.

* Dependency Changes
    * Removed hard dependency on shiny, RColorConesa, plotly.
    * Added dependency on 'RColorBrewer' and 'shiny'.

* Bug fixes
    * Storing count tables in `dgCMatrix` format for Sparse and Dense classes.

* General improvements 
    * Documentation (Examples and details).
    * Verbose text.


## scMaSigPro 0.0.3 (2024-03-3)

* Bug fixes
    * `eSparse()` and `eDense()` indexes
    * Removed the ComplexUpset error due to ggplot 3.5.0.

* Remove Package Dependence
    * SummarizedExperiment
    * ComplexUpset
    
* Package Suggestions Added
    * Patchwork
    * ComplexUpset
    * UpSetR

* Updated Test cases
    * Functionality of `eSparse()` and `eDense()`
    * Testing `sc.squeeze()` against manual pseudo-bulking

## scMaSigPro 0.0.1 (2023-12-20)

* Initial Github release.
