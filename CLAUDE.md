# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

scape-agents is a Haskell library and microVM agent for the Scape agentic execution platform. It contains shared protocol types (used by both orchestrator and agent), the agent binary that runs inside Firecracker microVMs, and NixOS modules for building VM templates.

## Build & Test Commands

Enter the dev shell first (provides GHC 9.10, cabal, nats-server, HLS):

```bash
nix develop
```

Then:

```bash
cabal build all                                        # Build everything
cabal test all                                         # Run all tests
cabal test scape-agents-test -- --match "Protocol"     # Run tests matching a pattern
cabal test scape-agents-test -- --match "Integration"  # Integration tests (starts a NATS server)
```

Nix-level commands:

```bash
nix build .#scape-agents    # Build the package via nix
nix flake check             # Full nix evaluation check
```

## Architecture

### Protocol Layer (`src/Scape/Protocol/`)

Shared types serialized as JSON between orchestrator and agent. Two main message directions:

- **Command** (`Command.hs`): Orchestrator -> Agent. Sum type: `CmdExec`, `CmdWriteFile`, `CmdReadFile`, `CmdSecrets`, `CmdCancel`, `CmdPing`.
- **Observation** (`Observation.hs`): Agent -> Orchestrator. Sum type: `ObsReady`, `ObsOutput`, `ObsComplete`, `ObsError`, `ObsPong`.

Supporting types: `Types.hs` (newtypes for `SessionId`, `CommandId`, etc.), `MMDS.hs` (Firecracker metadata config), `Transport.hs` (length-prefixed JSON framing), `AISDK.hs` (OpenAI/Vercel compatibility).

### Agent Runtime (`src/Scape/Agent/`)

The agent runs inside a Firecracker microVM. Boot sequence (in `app/Agent.hs`):

1. Fetch config from Firecracker MMDS (`http://169.254.169.254/scape`) — provides instance ID, NATS URL, credentials
2. Connect to NATS for command/observation messaging
3. Start HTTP server (health checks on `/healthz`, plus WebSocket/SSE streaming)
4. Publish `ObsReady` and enter command loop

Key subsystems:
- **Executor** (`Executor.hs`): Runs shell commands with streaming output via callbacks. Uses `OutputCallback = OutputChannel -> ByteString -> IO ()`.
- **State** (`State.hs`): STM-based `AgentState` with `TVar`s for secrets, active commands, metrics. All concurrent access goes through `atomically`.
- **Server** (`Server.hs`): Servant-based HTTP/WS/SSE server.
- **NATS** (`Nats.hs`): Subscribes to command subjects, publishes observations. Uses natskell library.
- **Logging** (`Logging.hs`): Katip structured logging with namespaced contexts.

If MMDS is unavailable (e.g., running outside Firecracker), the agent falls back to HTTP-only mode unless `--nats-url` is provided via CLI.

### NixOS Modules (`modules/`)

- **`agent.nix`**: Systemd service definition for scape-agent with security hardening. Options under `services.scape.agent.*`.
- **`base-vm.nix`**: Firecracker microVM base config (256MB RAM, 1 vCPU, erofs+lz4, TAP networking, vsock CID 3). All templates import this.
- **`template.nix`**: Schema for template metadata (`scape.template.<name>.*`) — resources, egress policy, packages, mounts, secrets.

### VM Templates (`templates/`)

Each template is a NixOS config that imports `base-vm` and adds packages/services:

- **python-sandbox**: Python 3 + pip/numpy/pandas/requests, 512MB, deny-all egress
- **duckdb-analyst**: DuckDB + Python data stack, 1024MB, 2 vCPUs
- **debug**: SSH + debug logging for troubleshooting

Registered as flake outputs, used via: `scape-ctl template add --name <name> --flake-ref github:getmissionctrl/scape-agents#<name>`

## Haskell Conventions

- **GHC 9.10** with `GHC2021` language edition. Strict by default (`StrictData` extension).
- **`-Wall -Werror`** — all warnings are errors.
- Key extensions used everywhere: `OverloadedStrings`, `OverloadedRecordDot`, `DuplicateRecordFields`, `DerivingStrategies`, `RecordWildCards`, `LambdaCase`.
- All modules use **explicit export lists** with haddock section headers.
- Newtypes for domain identifiers (`CommandId`, `SessionId`, `Token`, etc.).
- Field updates via **optics-core**: `m & #commandsActive %~ (+ 1)`.
- External dependency **natskell** is fetched from `github:getmissionctrl/natskell` via the flake.

## Test Structure

Tests use **hspec** with the `*Spec.hs` naming convention. Test tree in `test/Main.hs`:

- `Agent.{StateSpec, ExecutorSpec, NatsSpec}` — unit tests for agent subsystems
- `Protocol.{CommandSpec, ObservationSpec, MMDSSpec}` — JSON round-trip property tests
- `Integration.AgentSpec` — full agent + NATS integration (uses `Test.Nats` harness that auto-starts nats-server on a free port)

Test fixtures (NATS auth configs) live in `test/fixtures/`.
