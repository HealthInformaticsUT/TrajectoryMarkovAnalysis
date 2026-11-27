# Maintaining the documentation site

The site uses pkgdown and the Bootstrap 5 theme shared with Cohort2Trajectory.
Its intended address is
https://healthinformaticsut.github.io/TrajectoryMarkovAnalysis/.

Edit these sources:

- `pkgdown/index.md` for the home page.
- `vignettes/articles/` for website-only task guides.
- `vignettes/trajectory-analysis.Rmd` for the installed worked vignette.
- R roxygen comments for function help, then regenerate `man/` with
  `roxygen2::roxygenise()`.
- `_pkgdown.yml` for navigation, article ordering and reference groups.

Build from the package root with pkgdown installed:

```r
pkgdown::build_site(preview = FALSE)
```

The generated site is written to `docs/site/`, which is ignored by Git. It does
not replace `docs/workflow.md`. Review rendered guides, executed examples and
local links before publishing. Website-only articles and site configuration
are excluded from the source package; the worked vignette remains included.

The `Documentation` workflow builds pull requests without deployment. Pushes
to `main` and manual runs on `main` upload an artifact and deploy it through
GitHub Pages. Before the first deployment, enable Pages in the repository
settings with **GitHub Actions** as the publishing source. No `gh-pages`
branch or committed generated HTML is needed.
