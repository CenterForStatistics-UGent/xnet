---
---

![center-aligned-image](static/images/xnet_logo.png){:.align-center}

The package `xnet` is a package in development for cross-network analysis
like protein-ligand or plant-pollinator interactions. Currently the
package provides a basic interface for two-step kernel ridge regression,
including quick cross-validation due to algebraic shortcuts.

The package is still in beta, so expect thorough changes and additions. A first stable version (0.1.10) is released on [CRAN]( https://CRAN.R-project.org/package=xnet). 

The current version can be installed at your own risk using following code:

    install.packages("xnet")
    
If you want to download the latest devel version, you can use the following code :

    devtools::install_github("CenterForStatistics-UGent/xnet", ref = "devel) 

It includes a few vignettes explaining how to use the package, how the example data was prepared and how the internal class inheritance works. After installing, you can open the introduction using following code :

    vignette("xnet_ShortIntroduction", package = "xnet")

The paper describing the method is published in Briefings in Bioinformatics:

Stock, M., Pahikkala, T., Airola, A., Waegeman, W. and De Baets, B. *Algebraic shortcuts for leave-one-out cross-validation in supervised network inference* Briefings in Bioinformatics, accepted sep 2018.

[https://doi.org/10.1093/bib/bby095](https://doi.org/10.1093/bib/bby095)

[Preprint](https://www.biorxiv.org/content/early/2018/01/03/242321.1)

Our algorithms are part of a larger kernel-based framework for pairwise learning. A paper relating the different methods and their learning properties was recently accepted for publication in Neural Computation:

Stock, M., Pahikkala, T., Airola, A., De Baets, B. and Waegeman, W. *A comparative study of pairwise learning methods based on kernel ridge regression* Neural Computation 30 (2018), 2245-2283. 

[https://doi.org/10.1162/neco_a_01096](https://doi.org/10.1162/neco_a_01096)

[Preprint](https://arxiv.org/abs/1803.01575)
