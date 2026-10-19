# static-site

Static site generator built with **Haskell**, **Shake**, and **Slick**, using **esbuild** for asset minification.

## Architecture & Toolchain
- **Language**: Haskell (GHC 9.6.7, Cabal 3.14.2)
- **Engine**: [Shake](https://shakebuild.com/) build system with [Slick](https://github.com/ChrisPenner/slick)
- **Asset Pipeline**: [esbuild](https://esbuild.github.io/) with content-hash cache busting
- **Container**: Dev Containers specification (`.devcontainer`)
- **Task Runner**: [Taskfile](https://taskfile.dev/) (`Taskfile.yaml`)
- **CI/CD**: GitHub Actions deployment to GitHub Pages

## Development

### Dev Container
Run the container environment via CLI or VS Code:
```bash
npx @devcontainers/cli up --workspace-folder .
```

### Common Tasks
Commands defined in `Taskfile.yaml`:

| Command | Alias | Description |
| :--- | :--- | :--- |
| `task build:site` | `task bs` | Builds the static site output into `build/docs/` |
| `task serve:site` | `task ss` | Serves `build/docs/` locally on `http://localhost:8000` |
| `task build:app`  | `task ba` | Compiles the Haskell generator application |

## CI/CD
Pushes to `main` trigger `.github/workflows/deploy.yml`, which compiles the Haskell binary, generates `build/docs/`, and publishes directly to GitHub Pages.