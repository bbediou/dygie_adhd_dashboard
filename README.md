# Dygie ADHD Dashboard

Visualisation of the Dygie mini-game dataset: movement trajectories, derived play metrics and their relation to age and ADHD score.

An R Shiny app compiled to WebAssembly with [shinylive](https://posit-dev.github.io/r-shinylive/)
and served as a static site from GitHub Pages. No Shiny server is involved: R runs
in the visitor's browser via webR.

- App source: `app/`
- Build and deploy: `.github/workflows/deploy-shinylive.yaml`

## Layout

```
app/
  app.R
  _Mini-jeu ile - Dataset_06-30-23.csv
```

## Local preview

```r
install.packages("shinylive")
shinylive::export("app", "site")
httpuv::runStaticServer("site")
```

## Packages

Everything the app needs is downloaded into the visitor's browser on first load,
so the dependency list is kept as small as possible:

shiny, shinydashboard, shinyWidgets, shinycssloaders, tidyverse, dplyr, ggplot2,
GGally, ggsci, ggExtra, ggpubr, wesanderson, RColorBrewer, plotly, cowplot, lme4

Adding a package here adds megabytes to every visitor's first load. Packages must
have a WebAssembly build on [repo.r-wasm.org](https://repo.r-wasm.org); pure-R and
most common packages do, but not all of CRAN.
