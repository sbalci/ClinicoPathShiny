# ClinicoPath Shiny: joint development plan

Prepared: 2026-11-07. Status: proposed implementation roadmap.

The eleven earlier repositories supplied during planning are assessed in the
[prior-project review](prior-shiny-projects.md). They add concrete reuse candidates
for package structure, browser launchers, static browser experiments and job control.

## 1. Product decision

Develop two sibling applications alongside the existing jamovi repositories:

| Product | Main responsibility | Numerical source |
| --- | --- | --- |
| ClinicoPathShiny | R Shiny application; broad R method coverage | Reviewed R analyses and their underlying packages |
| ClinicoPathShinyPython | Shiny for Python application; Python workflows and interactive exploration | Reviewed ClinicoPathPj cores, followed by separately validated extensions |
| ClinicoPathJamoviModule and domain siblings | Existing R jamovi products and method development | Existing R implementation and independent statistical references |
| ClinicoPathPj | Python jamovi product and Python method development | Existing Python registry, cores and independent references |

Use the same workflow, analysis identities, examples and interchange contracts
across the Shiny applications. Allow different feature coverage and independent
release schedules. Share numerical implementation within each language when
practical; do not maintain a second handwritten copy of every method.

Recommended delivery order:

1. A browser application with synthetic examples and guided CSV/Excel workflows.
2. An institution-hosted edition using the same application build.
3. A bundled local runtime opened by a double-click launcher in the user's browser.
4. Selected browser-only tools after dependency and performance qualification.

Run the first small R/Python Shinylive feasibility experiments during M0/M1;
full browser-only product delivery remains conditional on their results.

Electron is not a dependency of this architecture. A normal browser can display
both locally running and hosted applications. Local runtime distribution remains
a real packaging task and must have its own acceptance gates.

## 2. What was inspected

This is a source-based planning review, not a runtime or statistical audit.

- R Shiny working tree was clean before these documents; HEAD `c05e9b0`.
- `app.R` uses a sourced page/server architecture with older Shiny extensions.
  It contains data selection, summaries, correlation, decision, pivot and survival UI.
  Presence in a menu does not demonstrate functioning or validated analysis.
- `modules/brief_summary/`, `modules/age_pyramid/` and `modules/agreement/`
  provide newer modular starting points. Their connection to the main entry point
  and their numerical behavior need explicit qualification.
- `server/server0_data.R` reads `mydata.xlsx` at startup. No contents were inspected
  for this plan; its anonymity and redistribution rights have not been established.
- `server/server_general_upload.R:37` evaluates parsed user input; line 41 dynamically
  dispatches a selected reader. Replace this path before exposing the app to users.
- `install_packages.R` uses a broad dependency installation list; no DESCRIPTION,
  renv lockfile, or documented deployment contract appeared in the inspected root.
- `tests/test_data_loading.R` is a custom printed-status script with mocked reactive
  behavior. It is not evidence of real multi-session or browser correctness.
- The agreement draft acknowledges unimplemented bootstrap intervals. Disabled or
  unsupported options must not look like available inferential methods.
- ClinicoPathPj HEAD was `818ee4e`; README version was 0.3.56. Its current migration
  plan records 39 entry points: 27 partial baseline ports and 12 utilities, 34 baseline
  targets planned, zero full-parity releases. These are repository-recorded statuses,
  not validation newly executed here.
- Pj already has a versioned registry, literal SPECs, shared `cpj_*.py` engines,
  fixtures and static/interactive outputs. Some cores still import runtime types;
  they cannot all be treated as standalone Python packages without adaptation.
- `ClinicoPathShinyPython` was absent from the inspected sibling directory.

Preserve the existing drafts while introducing the new architecture in small
reviewable changes. Leave unrelated R jamovi work untouched.

## 3. Users and the shared workflow

Primary users are pathologists, clinicians, researchers and trainees who can use
a spreadsheet but should not need R, Python, terminal commands or package managers.

The application should guide them through:

1. **Start:** Try an example, open a project, or import a dataset.
2. **Check data:** Preview rows, inspect variable types and missingness, identify
   patient IDs, select date formats and review import problems.
3. **Choose an analysis:** Search by task, such as “compare two tests” or “summarize
   my cohort”; organize the catalog into Descriptives, Plots, Diagnostic tests,
   Survival, Agreement and Oncology.
4. **Assign variables:** Choose roles from suitable columns. Display units, observed
   levels, positive/event levels, exclusions and essential assumptions together.
5. **Run:** Explicit Run/Apply commits a computation. Long jobs show progress and
   permit cancellation. Lightweight visual inspection can remain immediate.
6. **Review:** Show cohort accounting, main table/plot, warnings, methods and
   references. Keep advanced options collapsed and unsupported features visible
   in an analysis scope panel.
7. **Save/share:** Download tables, static figures, an HTML report and a project.
   Reproducible code is available under an advanced control.

Keep analysis selection and data preparation distinct. Changing the dataset must
invalidate results and incompatible mappings immediately. Never leave a previous
successful result looking current after a failed run.

Use consistent wording and layout in R and Python. Match user workflows rather
than forcing identical widget implementation. Plan English first with translatable
strings and Turkish next. Provide keyboard navigation, visible focus, accessible
table alternatives and color-independent plot explanations.

## 4. Deployment strategy

| Route | User experience | Recommendation | Engineering/operating tradeoff |
| --- | --- | --- | --- |
| Managed browser hosting | Open a URL; no runtime installation | First public demonstration with synthetic data | Recurring hosting cost, compute/session limits, data residency and access policy |
| Institution-hosted browser app | Open an approved internal URL | Preferred shared route for institutional research data | IT owns authentication, backups, TLS, resources and operational approval |
| Bundled local app | Install/download, double-click, browser opens | Offline/private workstation target after core stability | Separate OS/architecture builds, signing, runtime licenses and update support |
| Shinylive subset | Open a static website; supported computations run in browser | Separate feasibility track for small calculators | Browser package availability, memory, download size and export limitations |
| Developer source or Docker | Run documented setup commands | Developer/admin route | Not the installation experience offered to nontechnical end users |

For a new managed deployment, evaluate Posit Connect Cloud first. Current R and
Python documentation recommends it and describes shinyapps.io as a legacy route
with migration planned in 2027. Verify current terms, private access, region,
resource limits and costs at the deployment decision; this plan quotes no prices.
[R hosting documentation](https://shiny.posit.co/r/articles/share/deployment-web/),
[Python hosting documentation](https://shiny.posit.co/py/get-started/deploy-cloud.html).

For institutional hosting, choose between Posit Connect if already available and
an IT-managed container/reverse-proxy deployment. Keep the applications deployable
without a proprietary host. Python hosting configuration must support its runtime
and WebSocket connections; use current official deployment guidance rather than
assuming an old Shiny Server configuration supports it.
[Python self-hosting](https://shiny.posit.co/py/get-started/deploy-on-prem.html).

Shinylive can run Python applications through Pyodide on static hosting, but not
all Python packages are supported and browser execution changes resource/network
constraints. Select a small tool only after testing its complete dependency tree.
Evaluate R/WebR separately; success for Python does not qualify the R suite.
Do not promise offline operation merely because a page is cached.
[Shinylive documentation](https://shiny.posit.co/py/get-started/shinylive.html).

### Local launcher acceptance contract

- End users install neither R/Python nor development tools. A script requiring an
  existing runtime is a developer convenience, not the finished local edition.
- Launcher starts an application-owned process, waits for readiness, opens the
  default browser and displays a small status/Stop interface.
- Bind to loopback only; use an unpredictable per-launch access token, validate
  origins/hosts, disable developer/reload modes and avoid permissive CORS.
- Handle occupied ports, duplicate launches, paths with spaces/non-ASCII text,
  read-only install locations and abrupt process termination.
- Browser closing alone need not terminate a job; provide explicit Stop and a
  documented idle-shutdown policy. Stop cleans up owned workers and temporary files.
- Bundle assets/fonts and approved dependencies; work with network disabled after
  installation. No package installation occurs during user analysis.
- Store projects only at user-selected paths. Redact values from diagnostics.
- Publish checksums and runtime manifests; use signed/notarized installers where
  applicable, versioned updates, rollback and a visible app version.
- Qualify macOS arm64 and Windows x64 separately. Add other platforms only with
  build and fresh-machine evidence. Development on one OS proves nothing about another.

## 5. Architecture and numerical reuse

Each application has five layers:

1. Import and dataset metadata: stable column/row identities, typed values and audit.
2. Analysis catalog and options: schema validation, defaults, supported scope.
3. Numerical engine: explicit data/options/seed in, structured results out.
4. Application adapter: job lifecycle, state, cache boundaries and error mapping.
5. Presentation/export: tables, plots, interaction, report and project persistence.

R uses Shiny modules and a package-style app structure. Python uses Shiny Core
with modules and a normal installable project. A single entry point in each repo
supports local and hosted execution. Keep scientific code free of UI session state.

Do not copy jamovi `.h.R` or generated UI files into Shiny. Do not execute arbitrary
SPEC content as Python from uploaded projects. Translate reviewed option/result
metadata into an application catalog; write task-focused UI where needed.

### Staged sharing, without creating a dependency tangle

**Initially:** pin the upstream analysis revision and explicitly list imported
files, hashes, licenses and dependencies. Use a reviewed versioned source snapshot
or a package artifact, never a runtime `../ClinicoPathPj` import or automatic pull
from the latest sibling working tree. Retain original tests and attribution.

**After two consumers are stable:** extract cohesive language-specific numerical
packages if that reduces duplication. Candidate names are `clinicopath-core` in
Python and an R core package; names and new repositories are proposals, not required
for the first application. Move a pure core behind the old adapter first, prove
unchanged jamovi behavior, then consume its versioned release from Shiny.

R `.b.R` methods frequently mix calculations and jmvcore output. For each analysis,
choose a reviewed existing callable API or extract a pure calculation function.
Loading the entire umbrella package is not the default dependency strategy.

For Python, adapt Pj runtime types through a narrow compatibility boundary before
deciding which types belong in a shared domain package. The Shiny app uses its own
Python runtime; it does not depend on the interpreter bundled inside jamovi.

An R fallback from Python would be a separately labeled hybrid feature. Do not use
it to claim a completed Python analysis. No R/Python bridge is needed for the MVP.

## 6. Shared contracts and projects

Start contracts in `ClinicoPathShiny/contracts/` during implementation. The R repo
owns their first version; Python consumes a pinned snapshot. Changes require review
of both readers. A separate contracts repository is justified only when publishing
these snapshots becomes cumbersome.

### Analysis manifest

Record stable domain ID; upstream IDs/aliases; family; source revision/hashes;
license; option types/defaults/constraints; role and level requirements; methods;
result IDs/types; supported and omitted features; dependency versions; fixture
locations; numerical, browser, export and deployment evidence; support status.

Keep **implementation status**, **statistical coverage** and **deployment support**
as separate dimensions. A working Shiny panel must not become “validated” through
catalog generation. R and Python may implement different subsets of one domain ID.

### Dataset and result envelopes

- Preserve source row IDs independently of display sorting and filtering.
- Preserve column identities, labels, factor order/codes, literal strings such as
  `NA`, true missingness, units and explicit date/timezone semantics.
- Do not silently coerce numeric factors, truncate identifiers, guess event levels
  or replace undefined quantities with zero.
- Results identify data revision, canonical option hash, engine/version, seed,
  included/excluded counts, method and stable table/plot IDs.
- JSON must represent missing, infinite and undefined quantities with explicit
  typed status fields; do not emit nonstandard JSON NaN/Infinity tokens.

### Proposed portable project: `.cpath`

Use a versioned ZIP container with JSON metadata, options, audit, results and
optional data. For MVP data, use UTF-8 CSV plus an explicit typed schema and
missingness map; never infer saved data types by reading CSV again. Preserve IDs
and large integers lexically. Validate round trips before choosing this encoding;
Arrow is a later alternative if its dependency/packaging cost is justified.

Allow settings/results-only export or explicitly include data. Data inclusion is
shown clearly before download. A data-free project requires reattachment and
fingerprint validation before recomputation; fingerprints are provenance, not
anonymization. Never place participant details in default filenames or URLs.

On opening: check schema version, member paths/counts/uncompressed sizes, hashes,
allowed types and supported options before use. Never deserialize pickle, execute
R code, or source uploaded scripts. Unsupported analyses open as read-only saved
results with a clear scope explanation. Engine switches require explicit selection
and a new run; record the engine that produced each result.

Cross-language project exchange covers shared data/options/results contracts only;
it does not promise equal methods or automatic `.omv` round trips. Schema migrations
need old-version fixtures and may never silently change statistical meaning.

## 7. Analysis waves

Prioritize useful complete workflows over replicating the full menu.

| Wave | R target | Python target | Scope and exit condition |
| --- | --- | --- | --- |
| A: vertical slice | Import, data check, numeric/category summaries, overall Table One | Same supported subset using Pj descriptive/quality/Table One cores | Import → run → report → save → reopen on both apps |
| B: diagnostics | 2×2 calculator, diagnostic accuracy, cutoff explorer | Pj accuracy/cutoff cores | Explicit positive level/direction, ties, undefined metrics and intervals verified |
| C: relationships/plots | Cross tables, correlation, age pyramid | Corresponding Pj partial cores | Shared fixtures and clearly listed differences in inference/formatting |
| D: richer exploration | Agreement and linked plots as individually qualified | IHC heatmap, sets/tree/alluvial; agreement only after an adequate core exists | Source-row identity, keyboard behavior, cohort changes and static exports |
| E: time-to-event | Explicit outcome/time preparation, KM then Cox | Existing preparation cores first; fitted models as new method milestones | Event/censoring policy and independent model oracles before release |
| F: specialist work | Oncology, diagnostic meta-analysis, complex decision models | Study review first, pooled/advanced models later | Each method separately scoped and validated |

Make Wave A and a narrow Wave B the first useful release. Agreement bootstrap,
full ROC inference, clustered uncertainty, pooled meta-analysis, competing risks,
Markov/PSA and full baseline parity are not implicit MVP commitments.

For cutoffs retain separate choices for minimum errors and maximum Youden index.
Exploratory cutoff selection must not imply externally validated performance.
For endpoint preparation retain current policies; administrative censoring remains
opt-in with explicit cutoff and events exactly at cutoff remain events.

## 8. Parallel repository development

Maintain a small compatibility matrix: app release → core revision → contract
version → fixtures → supported runtime/platforms. Each repository keeps its own
backlog and can release independently.

For every analysis increment:

1. Select the authoritative R method, Python core and exact supported options.
2. Freeze synthetic fixtures and independent expected results with method notes.
3. Add/adjust the core behind existing adapters; verify original jamovi behavior
   if shared code changed. Preserve unrelated work and generated-file policy.
4. Implement the R and Python Shiny adapters in separate reviewable changes.
5. Run contract/numerical/browser/export tests, then update both coverage matrices.
6. Bump pinned dependencies explicitly; leave omission records for unfinished work.

No app deployment automatically promotes a method into the production jamovi menu.
No sibling repository is rewritten by a Shiny release script. Existing umbrella
module-distribution tooling remains separate until a reviewed extension is needed.

Suggested maintenance lanes: shared contracts/fixtures; R app; Python app;
deployment/usability. These are workstreams, not a requirement for concurrent
agents or a new staffing commitment. With one maintainer, alternate small paired
increments rather than running two broad migrations simultaneously.

## 9. Validation and release gates

| Gate | Required evidence |
| --- | --- |
| Numerical | Independent reference or exact enumerated fixtures; documented tolerances and method differences |
| Data handling | Factors, missing vs literal NA, duplicate names, non-ASCII labels, dates, long IDs and row alignment |
| Lifecycle | Valid → invalid → valid; dataset replacement; stale-job rejection; cancellation and cleanup |
| Browser | Real browser workflow, keyboard use, downloads, fresh session and supported viewport/browser matrix |
| Persistence | Save/reopen after process restart; old project migration; results/options/data integrity |
| Exports | Static figures and tables match applied analysis; report includes methods, exclusions and provenance |
| Isolation | Two simultaneous users cannot see each other's data/results/files; disconnect cleanup |
| Deployment | Actual built artifact runs on target host/fresh supported OS; offline test where claimed |
| Usability | Five target users attempt import/run/export/save/reopen with no coding; record assistance and failures |

Use hand-calculated small cases and independent package/oracle implementations.
Agreement between two adapters using the same core proves adapter consistency,
not independent mathematical correctness. Existing Pj evidence is a starting
point; new Shiny runtime, persistence and hosting evidence must be collected.

Initial usability target: at least four of five pilot users complete the core
workflow without developer intervention; fix observed barriers before expanding
the catalog. Initial performance target: a declared synthetic 10,000-row,
20-column Wave A dataset completes within 10 seconds after startup on a recorded
reference machine. Benchmark import and computation separately; publish memory
and concurrency limits and revise targets based on measurements.

For long CPU-bound work, isolate computation in bounded workers with deadlines,
memory/job limits and cancellation. Async syntax alone does not provide CPU
parallelism. Use Shiny task mechanisms with immutable job inputs and stale-result
guards. [R nonblocking guidance](https://shiny.posit.co/r/articles/improve/nonblocking/),
[Python nonblocking guidance](https://shiny.posit.co/py/docs/nonblocking.html).

Public demonstrations ship only reviewed synthetic/open examples. Before research
hosting, specify authentication, retention, geographic hosting, data ownership,
session isolation and institutional requirements. Uploads are allowlisted and
size bounded; HTML/CSV output must escape hostile labels and spreadsheet formulas.
Clinical use requires its own evaluation; a software gate is not clinical clearance.

## 10. Milestones, effort and decision points

These are planning ranges in total engineering person-weeks across both apps,
not fixed calendar promises. Part-time parallel repository work takes longer.

| Milestone | Estimate | Deliverable / decision |
| --- | --- | --- |
| M0: inventory and contracts | 1–2 | Classify drafts, choose first cores, license review, synthetic fixtures, schemas |
| M1: paired vertical slices | 3–4 | Both apps complete Wave A workflow locally; dependency locks and CI |
| M2: user-ready browser MVP | 3–5 | Narrow diagnostics, HTML/static export, project reopen and pilot usability |
| M3: hosting qualification | 2–3 | Synthetic demo plus one selected institutional deployment profile |
| M4: local packaging pilot | 3–5 | R and Python launcher proof on macOS arm64 and Windows x64; go/no-go |
| M5: hardening and first supported release | 3–5 | Fix pilot problems, compatibility/rollback docs, published support matrix |

Total indicative effort: **15–24 person-weeks**, plus hosting procurement, signing
lead time, independent clinical/method review and later analysis waves. M4 is the
largest uncertainty. Timebox initial R and Python packaging experiments separately
before committing to a supported offline edition. A larger analysis inventory is
additional effort, not absorbed into this estimate.

At M0 choose initial hosting/data policy and confirm supported pilot platforms.
At M1 decide whether pure-core extraction is already warranted. At M3 measure
hosting resources and monthly cost for observed usage. At M4 accept a local edition
only if fresh-machine installation and no-network use pass; otherwise keep a
documented developer launcher and prioritize institution-hosted browser access.

## 11. First implementation sprint

- [ ] Freeze the legacy R draft baseline and inventory all entry points/dependencies.
- [ ] Compare histopathRshiny package work, Shiny_Desktop_App's launcher, and the
  earlier R Shinylive demo before choosing new packaging/build details.
- [ ] Review bundled data provenance and create a synthetic replacement example.
- [ ] Replace the general expression-evaluating importer with typed CSV/XLSX readers.
- [ ] Draft analysis, dataset, result and project schemas plus shared fixtures.
- [ ] Build one R summary module with a pure engine and real session tests.
- [ ] Create the Python sibling scaffold and pin a minimal Pj descriptive core.
- [ ] Implement identical import/check/summary flows and stable row/column IDs.
- [ ] Round-trip one synthetic project across apps; report unsupported options.
- [ ] Add HTML/CSV/static-figure export and browser smoke tests to both repositories.
- [ ] Run an early launcher spike to expose runtime packaging risk before expansion.

Implementation details and repository backlogs are in
[R roadmap](r-shiny-roadmap.md) and [Python roadmap](python-shiny-roadmap.md).
