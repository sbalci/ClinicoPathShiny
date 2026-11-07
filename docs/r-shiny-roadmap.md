# ClinicoPathShiny: R implementation roadmap

Prepared 2026-11-07. Read with the [joint plan](shiny-development-plan.md) and
[earlier-project review](prior-shiny-projects.md). All tasks below are proposed.

## Intended structure

```text
ClinicoPathShiny/
  app.R                         # thin development/hosting entry point
  DESCRIPTION                   # app package and explicit dependencies
  R/
    run_app.R                   # one configurable public launcher function
    app_ui.R
    app_server.R
    mod_import.R
    mod_data_check.R
    mod_analysis_catalog.R
    mod_results.R
    mod_project.R
    engines/                    # logical boundary; use flat prefixed R files
                                # if package tooling requires conventional R/
  inst/
    app/www/                    # local CSS/JS/fonts/icons
    examples/                   # reviewed synthetic data only
    catalog/                    # analysis manifests and scope records
  contracts/                    # first shared contract home
  tests/testthat/
  tests/browser/
  renv.lock
  deployment/                   # container and host profiles
  packaging/                    # platform launcher/runtime build definitions
  docs/
```

Use a lightweight package-style app with Shiny modules and `bslib` for the new
shell. Recover useful golem conventions from histopathRshiny; adopt the full
framework only if it simplifies the implementation. Avoid a framework migration
that delays the first working analysis. R package code must be discoverable by
standard package tooling; the tree describes boundaries, not required nesting.

## R0. Inventory and preserve

- [ ] Record legacy app/modules, package uses, source provenance and existing tests.
- [ ] Compare histopathRshiny package functions, Docker recipe and lockfile with
  the current draft; selectively recover useful pieces without merging old trees.
- [ ] Classify each page: working candidate, disconnected draft, placeholder,
  unsupported inference, or obsolete deployment helper.
- [ ] Preserve original files in version history; introduce the new entry point
  through a reviewable transition. Do not delete drafts to make inventory pass.
- [ ] Review `mydata.xlsx` provenance separately and stop using it as automatic
  startup content in the new shell; provide documented synthetic examples.
- [ ] Replace the general expression-evaluating importer before any user deployment.
- [ ] Audit auto-loaded `R/` files for startup side effects. The observed
  `force_git_push.R` content was commented out; do not describe it as active behavior.

Exit: one dependency/source inventory, synthetic fixtures and an agreed Wave A scope.

## R1. Modern app shell and data boundary

- [ ] Add `run_app(host, port, launch_browser, ...)` with validated configuration.
- [ ] Explicitly load owned modules; use session-owned dataset and analysis stores.
- [ ] Replace old theme/landing-page extension dependencies as their pages migrate.
- [ ] Implement typed CSV and XLSX import, sheet selection, encoding/delimiter controls,
  role/type review and actionable errors. Never evaluate reader arguments as code.
- [ ] Give datasets revision IDs and columns stable IDs distinct from display names.
- [ ] Retain original row identities and an explicit missingness/level dictionary.
- [ ] Keep filters/recode actions reversible and visible in preparation history.
- [ ] Add searchable catalog, shared labels/help, result state and scope indicators.
- [ ] Establish minimal DESCRIPTION/renv environment with no user-session installation.

SPSS/SAV import follows the first stable data contract: preserve value labels and
user-missing semantics through explicit review. Arbitrary RData/RDS/script import
is not a supported default. Date parsing requires explicit formats/timezone policy.

## R2. Numerical adapters

For each analysis define a function with explicit data, options and seed, returning
tables, plot-ready data, warnings, inclusion counts and provenance. UI modules
must not be the only way to execute or test a calculation.

- [ ] First slice: data check → summaries → overall Table One.
- [ ] Use reviewed underlying packages or extracted pure functions; do not require
  an active jamovi results tree in a Shiny session.
- [ ] Keep stable option/result IDs where meanings match upstream. Record semantic
  differences and omitted options rather than translating them approximately.
- [ ] Freeze R package versions and method settings in result metadata.
- [ ] Add a narrow 2×2 calculator, then raw-data diagnostic accuracy and cutoff view.
- [ ] Qualify cross tables/correlation and age pyramid next.
- [ ] Treat agreement draft formulas, orientation and bootstrap support as new
  validation work; an existing R6 class does not constitute numerical approval.
- [ ] Add survival preparation/KM only after event, time origin and censoring rules
  are explicit. Cox/competing risks remain separate milestones.

Do not silently change tests, correction methods, CI methods or clinical thresholds
to match the Python implementation. Store differences in the shared feature matrix.

## R3. Reactivity and interaction

- [ ] Run/Apply captures an immutable data/options snapshot and creates a job ID.
- [ ] Dataset or option changes mark results stale; failed validation clears old outputs.
- [ ] Accept job results only if their data revision/options/job ID still match.
- [ ] Use bounded background computation for slow work, with cancel/deadline/cleanup.
- [ ] Keep diagnostic cutoff dragging as preview; Run/Apply commits reportable values.
- [ ] Use linked row IDs for plots. Selection alone never silently refits models.
- [ ] Namespace all module IDs and test two copies of one analysis in one session.

## R4. Reports and project files

- [ ] CSV table export, PNG/SVG figures and a self-contained static HTML report first.
- [ ] Report includes applied options, methods/references, cohort/exclusions and versions.
- [ ] Code export calls supported functions with safely quoted literals; never
  execute generated code automatically.
- [ ] Implement `.cpath` save/open against shared contract fixtures, including
  missingness, factor metadata and stale-result checks.
- [ ] R/Python common-subset round trips must be tested both directions.
- [ ] Add Word/PDF after checking bundled renderer/font/system-tool requirements.
  HTML reports should not require a separate authoring runtime on the user's machine.

## R5. Deploy and package

Use a pinned R version plus renv restore for developer/CI builds. Lockfiles alone
do not reproduce OS libraries; record native dependencies and pin container images.
The official [renv Docker guide](https://pkgs.rstudio.com/renv/articles/docker.html)
describes combining those layers.

- [ ] Build one hosted artifact with explicit environment configuration and WebSocket testing.
- [ ] Qualify an institution-hosted profile and, separately, a synthetic managed demo.
- [ ] Run a small R Shinylive calculator spike in M0/M1 using the earlier Quarto demo
  as a reference; measure package availability, download/startup and export behavior.
- [ ] Prototype Windows bundled R + default-browser launcher using
  Shiny_Desktop_App's structure, with dependencies installed at build time.
- [ ] Prototype macOS arm64 runtime distribution separately; verify relocation,
  native library paths, signing and absence of development-machine dependencies.
- [ ] Do not reuse old extraction scripts or pin R 4.2.2 just because an M1 demo did.
- [ ] Add installer/update/rollback and full launcher contract checks from the joint plan.

## R6. Required checks for each release

Pure-engine `testthat` cases; `shiny::testServer()` lifecycle cases; real browser
tests using shinytest2/Playwright where appropriate; independent statistical
fixtures; actual downloads/reopen; simultaneous-session isolation; fresh-machine
packaging and deployed-host checks. Existing printed PASS output is not a release gate.

For package changes run relevant package checks and regenerate owned documentation.
Do not run umbrella jamovi generation or change production menu groups as part of
app-only work. Record numerical, app, host and platform evidence separately.

First release target: small dependable descriptive/diagnostic workflows, browser
hosting, project persistence and explicit coverage documentation. Full R analysis
catalog and universal offline installers are later milestones.
