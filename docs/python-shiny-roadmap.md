# ClinicoPathShinyPython: implementation roadmap

Prepared 2026-11-07. Proposed sibling repository; not created by this planning task.
This document stays in ClinicoPathShiny until the sibling is initialized, then
travels with its source/provenance links. See the [joint plan](shiny-development-plan.md).

## Product and framework

Use **Shiny for Python Core**, with modular UI/server functions and normal Python
packaging. It fits the requested companion application and supports an analogous
reactive workflow. Keep ordinary Python functions as the numerical boundary so
future UI experiments do not force a rewrite of the statistics.

Do not add Streamlit, Dash, Electron, a custom React frontend, a separate REST
service or an R bridge to the MVP without a demonstrated requirement. Those are
independent architecture costs. The initial app needs one Python service.

## Intended structure

```text
ClinicoPathShinyPython/
  pyproject.toml
  uv.lock
  app.py                         # hosting/development entry point
  src/clinicopath_shiny/
    app.py
    config.py
    modules/                     # import, checks, catalog, results, projects
    adapters/                    # Pj/core input and result translation
    engines/                     # new pure numerical helpers where necessary
    jobs/                        # bounded workers and lifecycle management
    projects/                    # schema reading/writing/migration
    reports/
    assets/
  catalog/
  contracts/                     # pinned shared contract snapshot
  examples/
  tests/{unit,contracts,numerical,browser}/
  upstream/                      # source manifest/licenses; reviewed vendoring if used
  deployment/
  packaging/
  docs/
```

Use a supported Python version selected from dependency/platform testing. Pj's
qualified bundled interpreter is context, not a requirement imposed on this app.
Pin one qualified version for initial release; expand only with CI evidence.

## P0. Reuse boundary and inventory

- [ ] Initialize the sibling repo with README, license/attribution, contributor
  guidance, package metadata, dependency lock and synthetic-only examples.
- [ ] Map Pj registry entries to stable domain IDs and list exact supported subsets.
- [ ] First candidates: `cpj_descriptive.py`, `cpj_quality.py`, `cpj_tableone.py`,
  then `cpj_accuracy.py`, `cpj_cutoff.py` and the reviewed cutoff analysis adapter.
- [ ] Trace transitive imports and runtime assumptions for each selected core.
  For example `cpj_accuracy.py` imports `Factor`, `InputError`, and `fmt_num`
  from `cpj_runtime`; this needs a reviewed domain/adapter boundary.
- [ ] Pin upstream commit/files/hashes and retain tests, licenses and omission records.
- [ ] Avoid importing Pj from a sibling path or depending on a jamovi installation.
- [ ] Choose minimal reviewed vendoring/package artifact for the first slice;
  plan pure-core extraction only when consumers and boundaries are established.

Pj's registry and SPEC metadata inform the catalog, but do not blindly generate
the entire web UI. Ignore jamovi-only menu/result transport fields and reject
unknown uploaded options. Do not import a user-supplied Python module as a plugin.

## P1. Data and session model

- [ ] CSV/XLSX import with sheet/encoding/delimiter/type review, stable IDs and limits.
- [ ] Explicit missingness map, ordered category dictionary, units and source labels.
- [ ] Convert into Pj/core types through tested adapters; preserve literal `NA`,
  numeric factor values and source row positions.
- [ ] Keep immutable dataset revisions and session-owned analysis state.
- [ ] Preserve large integer IDs as strings or exact typed values across JSON/browser
  transport; JavaScript number coercion must not change identity.
- [ ] Later add SAV through an independently tested dependency, preserving label
  and user-missing semantics. Do not accept pickle or executable project content.

Use pandas/NumPy where their behavior is explicit and tested. Avoid changing the
core's category/missingness rules through dataframe defaults.

## P2. First complete workflow

- [ ] Match the R navigation: Start → Check data → Analysis → Results → Export/save.
- [ ] Implement data checks, numeric/category summaries and overall Table One.
- [ ] Show data roles, exclusions, scope and warnings in ordinary language.
- [ ] Render typed structured results as tables and static plots.
- [ ] Add HTML report, CSV table and PNG/SVG download with methods/provenance.
- [ ] Add `.cpath` save/reopen with shared fixtures and version/method checks.
- [ ] Exercise complete round trips in both Python and R; unsupported features
  must become explicit read-only results or actionable import errors.

Do not count missing Pj features as web-app capabilities. Each manifest records
partial scope even if the UI for the subset is polished.

## P3. Diagnostics and interactive extensions

- [ ] Diagnostic 2×2 calculator followed by raw-data accuracy and cutoff explorer.
- [ ] Require positive/reference levels and test direction explicitly.
- [ ] Preserve exact interval methods, zero-denominator statuses and tie behavior.
- [ ] Distinguish minimum errors from maximum Youden index.
- [ ] Keep preview changes separate from applied options/results/reports.
- [ ] Reuse reviewed local JS visualization patterns with escaped data and no CDN
  requirement; replace jamovi-specific `window.setOption` with a tested Shiny binding.
- [ ] Add cross tables/correlation and later IHC/sets/tree/alluvial by shared priority.
- [ ] Keep static counterparts for exported reports and accessible table alternatives.
- [ ] Keep survival preparation separate from fitted survival-model implementation.

Full ROC inference, pooling, clustered uncertainty and agreement methods require
their own numerical specification. Selecting a Python library is not validation.

## P4. Jobs and reliability

- [ ] Capture immutable options/data/seed with a job and dataset revision ID.
- [ ] Use bounded worker processes for CPU-bound calculations; configure thread
  counts to avoid multiplying BLAS threads across sessions.
- [ ] Use Shiny task facilities to keep the UI responsive; async alone does not
  make synchronous CPU work nonblocking.
- [ ] Cancel or discard superseded jobs, bound memory/time, and clean temporary artifacts.
- [ ] Never publish an old job's result after a new dataset or options were applied.
- [ ] Provide redacted support diagnostics with app/core/runtime versions.

See [official Python nonblocking guidance](https://shiny.posit.co/py/docs/nonblocking.html).

## P5. Deployment and local packaging

- [ ] Pinned environment and one deployable Python app for managed and institutional hosts.
- [ ] Export provider-specific requirements from the lockfile when necessary; do not
  hand-maintain conflicting dependency specifications.
- [ ] Build a loopback browser launcher and exercise its full lifecycle early.
- [ ] Pilot a PyInstaller **directory bundle** before a single-file executable:
  explicitly collect Shiny web assets, templates, numerical native libraries and fonts.
- [ ] Build on each supported OS/architecture and test with no Python installed.
  PyInstaller is not a cross-compiler. [Official manual](https://www.pyinstaller.org/en/stable/).
- [ ] Add signing, installer/updater/rollback and disconnected operation tests.
- [ ] Run a Shinylive feasibility spike on a small calculator in parallel with R's
  spike; compare package coverage, startup/download, memory and export behavior.
- [ ] Keep browser-only tools a declared subset, sharing fixtures and method versions
  with server execution; do not assume all SciPy/pandas/Pj paths work in Pyodide.

Docker is an administrator/developer distribution route. A Python wheel, uv command
or source checkout requiring terminal setup does not satisfy the end-user installer goal.

## P6. Verification and release

Use pytest for core/adapter/project tests; independent R oracles plus exact small
cases for numerical qualification; browser tests for actual Shiny input/Apply,
downloads and reopen. Test invalid/recovery, two users, stale jobs, cancellation,
hostile labels, ZIP traversal/expansion bounds, missingness and row identity.

CI should validate upstream hashes, catalog/contracts, numerical fixtures, package
installation, browser flows and artifact composition. Actual Windows/macOS builds
and deployed-host smoke tests are separate gates. Preserve the original Pj suite
when extracting shared code; cross-adapter agreement does not replace independent
oracles or jamovi regression checks.

The first supported Python release covers a narrow descriptive/diagnostic scope.
It does not claim full ClinicoPathPj, R app or original R jamovi parity.
