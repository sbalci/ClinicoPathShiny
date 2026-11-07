# Earlier Shiny projects: reuse assessment

Reviewed 2026-11-07 using GitHub repository metadata, each README, and selected
source/build files. This is an architecture/source review; no historical app,
installer, workflow, linked website or release binary was executed. Fork history
does not establish the present maintenance status of the upstream project.

## Repository-by-repository decisions

| Repository | Observed role and evidence | Proposed use |
| --- | --- | --- |
| [clinicopathological](https://github.com/sbalci/clinicopathological) | Original pathology R/Electron product; README labels the template work in progress. `package.json` includes Forge and builder/webpack paths. | Recover product ideas and launch/error patterns; simplify packaging into one supported build pipeline. Do not restore this dependency tree wholesale. |
| [clinicopathological2](https://github.com/sbalci/clinicopathological2) | Fork of hinkelman's R/Electron template. README describes startup screen, separate Shiny process and limited retries. | Reuse readiness/error-recovery requirements in the browser launcher; retain as historical packaging reference. |
| [r-shinylive-demo](https://github.com/sbalci/r-shinylive-demo) | Fork of coatless-quarto's R/WebAssembly demonstration; Quarto examples and a GitHub Pages workflow. | First reference for an R calculator feasibility spike and educational documentation. Rebuild with current qualified tools. |
| [shiny-electron-template-m1-2023](https://github.com/sbalci/shiny-electron-template-m1-2023) | M1 Electron fork; runtime download script targets R 4.2.2 arm64 and patches extracted paths. | Document historical macOS relocation approach; test current runtime relocation/signing independently. Not the new application's base. |
| [histopathRshiny](https://github.com/sbalci/histopathRshiny) | Original pathology Shiny app with DESCRIPTION, golem conventions, Dockerfile and renv lockfile; root app resembles current drafts. | Main older application/package reference alongside ClinicoPathShiny. Compare workflows, recover useful modules/help and modernize dependencies. |
| [Shiny_Desktop_App](https://github.com/sbalci/Shiny_Desktop_App) | Windows portable-R launcher; `run.bat` resolves bundled R relative to the app, `run.R` opens the default browser and installs missing packages. | Closest existing reference for the desired local UX. Move dependency installation to build time and add lifecycle/security/update controls. |
| [ShinyElectron](https://github.com/sbalci/ShinyElectron) | Electron quick-start fork whose README combines Electron with R-Portable 3.4.0. | Historical wrapper/launch reference only; do not inherit its runtime assumptions. |
| [shinyproxy-desktop-app-demo](https://github.com/sbalci/shinyproxy-desktop-app-demo) | OpenAnalytics fork demonstrating remote desktop apps with Docker, X/VNC/noVNC and ShinyProxy. | Infrastructure reference only. Remote desktop streaming adds unnecessary components for an ordinary Shiny browser interface. |
| [targets-shiny](https://github.com/sbalci/targets-shiny) | Pipeline prototype with projects, cancellation, progress and transient/persistent modes, conditional on appropriate hosting. | Later reference for expensive reproducible research pipelines; borrow explicit job/project lifecycle concepts early. Avoid persistent jobs for the MVP. |
| [r-shiny-electron-app](https://github.com/sbalci/r-shiny-electron-app) | Walter's Electron setup-guide fork, documenting older Node/R/Electron setup and OS-specific manual steps. | Troubleshooting history and packaging failure checklist; do not impose its developer setup on end users. |
| [electricShine](https://github.com/sbalci/electricShine) | Package-oriented R/Electron builder fork; README describes Windows-only support in that snapshot and snapshot-based dependency installation. | Reuse the principle of an app package, single launch function and reproducible artifacts; reassess tools/platform support before any future use. |

The listed forks' last-push metadata mostly dates from 2020–2023. That explains
why their scripts need review against current runtimes; it does not show that
every technique failed or that their upstreams are now abandoned.

## Concrete source findings that change the plan

### 1. Local delivery already has a relevant non-Electron prototype

[Shiny_Desktop_App/run.bat](https://github.com/sbalci/Shiny_Desktop_App/blob/master/run.bat)
resolves a local R executable and passes the application directory to
[run.R](https://github.com/sbalci/Shiny_Desktop_App/blob/master/run.R), which opens
the default browser. This is the appropriate conceptual starting point for a
Windows local edition.

Its launch-time installation of missing packages must become build-time dependency
resolution for the new offline-capable app. Add readiness checks, bounded retries,
visible Stop, private loopback access, cleanup, diagnostics and fresh-machine tests.
Do not distribute a second browser by default; the system browser avoids another
runtime to bundle and maintain.

### 2. Recover package work already present in histopathRshiny

[DESCRIPTION](https://github.com/sbalci/histopathRshiny/blob/master/DESCRIPTION)
declares a golem-based app package and many dependencies. The repository root also
contains `renv.lock`, tests/build helpers and a
[Dockerfile](https://github.com/sbalci/histopathRshiny/blob/master/Dockerfile).
The Dockerfile uses R 3.6.0 and installs packages during the build; it is a useful
structure reference, not a current reproducible deployment specification.

Compare it with the newer ClinicoPathShiny modules before extracting engines.
Keep one future product identity, ClinicoPathShiny; preserve old names through
attribution/history and migration notes rather than maintaining another active app.

### 3. Treat R browser execution as an early experiment

The [R Shinylive demo README](https://github.com/sbalci/r-shinylive-demo/blob/main/README.md)
and [publishing workflow](https://github.com/sbalci/r-shinylive-demo/blob/main/.github/workflows/publish-demo.yml)
already connect Quarto/Shinylive to static hosting. Start with a synthetic 2×2
calculator or simple summary in both R and Python. Verify numerical equality with
the server engine, package availability, file input/download, startup payload,
browser compatibility and behavior after network disconnection.

A passing small calculator does not qualify the full survival/graphics stack.
Measure external requests and asset caching before describing any mode as local
data processing or offline. User data must never be built into published static assets.

### 4. Electron templates identify packaging risks rather than solve the new app

The [M1 runtime script](https://github.com/sbalci/shiny-electron-template-m1-2023/blob/main/get-r-mac.sh)
downloads a specific R arm64 package, extracts it and rewrites paths. Its
[Shiny startup script](https://github.com/sbalci/shiny-electron-template-m1-2023/blob/main/start-shiny.R)
already binds loopback and accepts launch configuration through environment variables.
Preserve those useful boundaries, while qualifying current runtime relocation,
native dependencies, signing and lifecycle behavior from scratch.

The [older product package.json](https://github.com/sbalci/clinicopathological/blob/master/package.json)
mixes several build approaches. Choose a single new release path rather than
upgrading every inherited bundler. Electron can remain an optional future shell
if a tested native integration need emerges; it should not block either app.

### 5. Pipeline persistence is a separate product capability

[targets-shiny](https://github.com/sbalci/targets-shiny/blob/main/README.md)
is relevant to long-running analysis, project progress and cancellation. Its
persistence depends on storage, authentication and process execution arrangements.
Do not copy a process-wide working-directory change or OS username-based ownership
into a multi-session app. Use per-user authorization, per-job directories and
worker processes with explicit working directories.

For MVP, jobs belong to a session and clean up on termination. Introduce durable
jobs later with a job store, queue, authenticated ownership, retention, worker
restart/reconciliation and deployment-specific tests. `targets` is optional for
expensive R workflows; there is no requirement to reproduce it in Python.

## How to maintain these repositories

- Active product development: ClinicoPathShiny and the proposed ClinicoPathShinyPython.
- Shared method work: existing R/jamovi repositories and ClinicoPathPj, with pinned
  reviewed reuse as described in the joint plan.
- Historical references: the eleven repositories above remain unchanged for now.
  Add migration pointers later if requested; no deletion or archival is required.
- Small technology experiments: isolate them from the product runtime and record
  scope/results before moving their code into either application.
- Before copying, inspect licenses and required notices at file/dependency level.
  Fork ownership and GitHub's license summary do not settle redistribution terms.

## Review limits and implementation follow-ups

All eleven README files and metadata were consulted, together with root listings
for the main application/deployment candidates. Selected source reads covered
histopathRshiny's DESCRIPTION/Dockerfile/app entry, the Windows launcher pair,
M1 runtime/startup scripts, clinicopathological's package metadata, the Shinylive
publishing workflow and electricShine's DESCRIPTION. Large files were read in
bounded excerpts where sufficient for architecture assessment.

This review does not establish installer success, modern package compatibility,
full numerical correctness, dependency vulnerabilities, data anonymity or current
upstream release support. Implementation must pin exact commits, inspect selected
transitive files/licenses, and run the relevant tests before importing code.
