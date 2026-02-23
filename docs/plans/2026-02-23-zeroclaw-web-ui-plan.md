# ZeroClaw Web UI Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build a React/Vite/TypeScript web UI for the zeroclaw template with chat, voice, widgets, fullscreen desktop viewer, and fullscreen terminal.

**Architecture:** Npm workspace with two packages — `@scape/ui-components` (shared DesktopViewer + TerminalWidget) and `@scape/zeroclaw-ui` (Hono server + React SPA). Server on port 5000 serves static SPA, proxies chat WS to zeroclaw's WebChannel on port 5100, and provides TTS/message REST endpoints. Terminal/VNC WebSocket connections go directly through the platform proxy to agent:8080.

**Tech Stack:** React 18, Vite 6, TypeScript 5, Hono, Vitest, React Testing Library, Tailwind CSS, xterm.js, noVNC, npm workspaces.

**Design doc:** `docs/plans/2026-02-23-zeroclaw-web-ui-design.md`

**Reference implementations:**
- ClawTime (gitingest at `clawtimeingest.txt`) — chat, voice, widgets, store
- Scape console (`/home/ben/dev/scape/console/src/components/DesktopViewer.tsx`) — noVNC
- Scape console (`/home/ben/dev/scape/console/src/components/Terminal.tsx`) — xterm.js

---

## Status & Kickoff Brief for Next Session

### Completed Tasks (Tasks 1-12)

All work is on branch `feature/zeroclaw-web-ui` in a worktree at `.worktrees/zeroclaw-web-ui/`.

| Task | Description | Status | Tests |
|------|-------------|--------|-------|
| 1 | npm workspace scaffolding | DONE | n/a |
| 2 | DesktopViewer (ui-components) | DONE | 3 pass |
| 3 | TerminalWidget (ui-components) | DONE | 2 pass |
| 4 | Server config module | DONE | 2 pass |
| 5 | Message store | DONE | 4 pass |
| 6 | Gateway WebSocket proxy | DONE | 3 pass |
| 7 | TTS endpoint | DONE | 8 pass |
| 8 | Hono HTTP + WS server entry | DONE | n/a (wiring) |
| 9 | Client types, CSS, main.tsx | DONE | n/a |
| 10 | Widget parsing utility | DONE | 4 pass |
| 11 | useGateway WebSocket hook | DONE | 3 pass |
| 12 | useMessages hook | DONE | n/a (stateful hook) |

**Total tests passing:** 29 (5 ui-components + 24 zeroclaw-ui)

### Remaining Tasks (Tasks 13-22)

| Task | Description | Phase |
|------|-------------|-------|
| 13 | MessageBubble component | Phase 5: Chat UI |
| 14 | InputBar component | Phase 5: Chat UI |
| 15 | ChatPanel, App shell, Layout | Phase 5: Chat UI |
| 16 | WidgetRenderer + core widgets | Phase 6: Widgets |
| 17 | DesktopModal + TerminalModal | Phase 7: Modals |
| 18 | useVoice hook + VoiceButton | Phase 8: Voice |
| 19 | TaskPanel component | Phase 9: Tasks |
| 20 | Integration test | Phase 10: Integration |
| 21 | Update NixOS template | Phase 10: Integration |
| 22 | Final build verification | Phase 10: Integration |

### Critical Blocker (Resolved)

The zeroclaw gateway was discovered to be **HTTP-only** (`POST /webhook`), not WebSocket. This blocks the streaming chat UI. A separate plan has been written to add a `WebChannel` to the zeroclaw Rust codebase:

**Plan:** `~/dev/agents/zeroclaw/docs/plans/2026-02-23-web-channel.md`

The WebChannel implements the `Channel` trait with an axum WebSocket server on port 5100. It gets the full agent loop (tools, streaming, history, cancellation) for free through the existing channel dispatch pipeline.

**Dependency order:**
1. **First:** Implement WebChannel in `~/dev/agents/zeroclaw` (separate session/PR)
2. **Then:** Update gateway-proxy.ts here to do direct WS-to-WS forwarding to port 5100
3. **Then:** Continue with Tasks 13-22

### Changes Made vs Original Plan

1. **Gateway proxy reworked** — Task 6's proxy was rewritten from WS-to-WS forwarding to WS-to-HTTP bridging to work with the existing HTTP-only gateway. Once the WebChannel lands, this gets reverted to direct WS forwarding.
2. **Config default changed** — `GATEWAY_URL` default is `http://127.0.0.1:3000` (was `ws://`). Will change to `ws://127.0.0.1:5100` after WebChannel.
3. **`ws` package no longer needed** — The HTTP bridging approach uses `fetch()` instead of the `ws` npm package. After WebChannel lands and we revert to WS forwarding, we'll need to re-evaluate whether to use the `ws` package or browser-native WebSocket relay.
4. **`@types/ws` kept as devDependency** — Will be needed again after WS forwarding is restored.

### Worktree Location

```
/home/ben/dev/scape-agents/.worktrees/zeroclaw-web-ui/
```

All npm workspace code is under `packages/` in that worktree. Run tests with:
```bash
cd packages/ui-components && npx vitest run    # 5 tests
cd packages/zeroclaw-ui && npx vitest run      # 24 tests
```

---

## Phase 1: Project Scaffolding

### Task 1: Create workspace root and package configs

**Files:**
- Create: `packages/package.json`
- Create: `packages/ui-components/package.json`
- Create: `packages/ui-components/tsconfig.json`
- Create: `packages/ui-components/vitest.config.ts`
- Create: `packages/zeroclaw-ui/package.json`
- Create: `packages/zeroclaw-ui/tsconfig.json`
- Create: `packages/zeroclaw-ui/vite.config.ts`
- Create: `packages/zeroclaw-ui/vitest.config.ts`
- Create: `packages/zeroclaw-ui/tailwind.config.ts`
- Create: `packages/zeroclaw-ui/postcss.config.js`
- Create: `packages/zeroclaw-ui/index.html`

**Step 1: Create workspace root package.json**

```json
// packages/package.json
{
  "name": "scape-packages",
  "private": true,
  "workspaces": ["ui-components", "zeroclaw-ui"]
}
```

**Step 2: Create ui-components package.json**

```json
// packages/ui-components/package.json
{
  "name": "@scape/ui-components",
  "version": "0.1.0",
  "type": "module",
  "main": "src/index.ts",
  "types": "src/index.ts",
  "scripts": {
    "test": "vitest run",
    "test:watch": "vitest"
  },
  "peerDependencies": {
    "react": "^18.3.0",
    "react-dom": "^18.3.0"
  },
  "dependencies": {
    "@novnc/novnc": "1.4.0",
    "@xterm/xterm": "^6.0.0",
    "@xterm/addon-fit": "^0.11.0",
    "@xterm/addon-web-links": "^0.12.0",
    "@xterm/addon-webgl": "^0.19.0",
    "@xterm/addon-unicode11": "^0.9.0"
  },
  "devDependencies": {
    "@testing-library/react": "^16.0.0",
    "@testing-library/jest-dom": "^6.0.0",
    "@types/react": "^18.3.0",
    "@types/react-dom": "^18.3.0",
    "jsdom": "^25.0.0",
    "react": "^18.3.0",
    "react-dom": "^18.3.0",
    "typescript": "^5.3.0",
    "vitest": "^3.0.0"
  }
}
```

**Step 3: Create ui-components tsconfig.json**

```json
// packages/ui-components/tsconfig.json
{
  "compilerOptions": {
    "target": "ES2022",
    "module": "ESNext",
    "moduleResolution": "bundler",
    "jsx": "react-jsx",
    "strict": true,
    "esModuleInterop": true,
    "skipLibCheck": true,
    "outDir": "dist",
    "declaration": true,
    "declarationDir": "dist"
  },
  "include": ["src"]
}
```

**Step 4: Create ui-components vitest.config.ts**

```ts
// packages/ui-components/vitest.config.ts
import { defineConfig } from 'vitest/config'

export default defineConfig({
  test: {
    environment: 'jsdom',
    setupFiles: [],
  },
})
```

**Step 5: Create zeroclaw-ui package.json**

```json
// packages/zeroclaw-ui/package.json
{
  "name": "@scape/zeroclaw-ui",
  "version": "0.1.0",
  "type": "module",
  "scripts": {
    "dev": "vite",
    "dev:server": "node --import tsx src/server/index.ts",
    "build": "vite build",
    "start": "node dist/server/index.js",
    "test": "vitest run",
    "test:watch": "vitest"
  },
  "dependencies": {
    "@scape/ui-components": "*",
    "hono": "^4.0.0",
    "@hono/node-server": "^1.0.0",
    "@hono/node-ws": "^1.0.0",
    "react": "^18.3.0",
    "react-dom": "^18.3.0",
    "lucide-react": "^0.400.0"
  },
  "devDependencies": {
    "@testing-library/react": "^16.0.0",
    "@testing-library/jest-dom": "^6.0.0",
    "@testing-library/user-event": "^14.0.0",
    "@types/react": "^18.3.0",
    "@types/react-dom": "^18.3.0",
    "@vitejs/plugin-react": "^4.0.0",
    "autoprefixer": "^10.4.0",
    "jsdom": "^25.0.0",
    "postcss": "^8.4.0",
    "tailwindcss": "^3.4.0",
    "tsx": "^4.0.0",
    "typescript": "^5.3.0",
    "vite": "^6.0.0",
    "vitest": "^3.0.0"
  }
}
```

**Step 6: Create zeroclaw-ui tsconfig.json**

```json
// packages/zeroclaw-ui/tsconfig.json
{
  "compilerOptions": {
    "target": "ES2022",
    "module": "ESNext",
    "moduleResolution": "bundler",
    "jsx": "react-jsx",
    "strict": true,
    "esModuleInterop": true,
    "skipLibCheck": true,
    "paths": {
      "@/*": ["./src/*"]
    }
  },
  "include": ["src"]
}
```

**Step 7: Create vite.config.ts, tailwind.config.ts, postcss.config.js, index.html**

```ts
// packages/zeroclaw-ui/vite.config.ts
import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
import path from 'path'

export default defineConfig({
  plugins: [react()],
  root: '.',
  build: {
    outDir: 'dist/client',
  },
  resolve: {
    alias: {
      '@': path.resolve(__dirname, 'src'),
    },
  },
  server: {
    proxy: {
      '/api': 'http://localhost:5000',
    },
  },
})
```

```ts
// packages/zeroclaw-ui/tailwind.config.ts
import type { Config } from 'tailwindcss'

export default {
  content: ['./index.html', './src/**/*.{ts,tsx}'],
  theme: {
    extend: {},
  },
  plugins: [],
} satisfies Config
```

```js
// packages/zeroclaw-ui/postcss.config.js
export default {
  plugins: {
    tailwindcss: {},
    autoprefixer: {},
  },
}
```

```html
<!-- packages/zeroclaw-ui/index.html -->
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>ZeroClaw</title>
  </head>
  <body class="bg-gray-950 text-gray-100">
    <div id="root"></div>
    <script type="module" src="/src/client/main.tsx"></script>
  </body>
</html>
```

**Step 8: Create zeroclaw-ui vitest.config.ts**

```ts
// packages/zeroclaw-ui/vitest.config.ts
import { defineConfig } from 'vitest/config'
import path from 'path'

export default defineConfig({
  test: {
    environment: 'jsdom',
    setupFiles: [],
  },
  resolve: {
    alias: {
      '@': path.resolve(__dirname, 'src'),
    },
  },
})
```

**Step 9: Install dependencies**

Run: `cd /home/ben/dev/scape-agents/packages && npm install`
Expected: node_modules created, workspaces linked

**Step 10: Verify workspace setup**

Run: `cd /home/ben/dev/scape-agents/packages && npm ls --depth=0`
Expected: Both `@scape/ui-components` and `@scape/zeroclaw-ui` listed

**Step 11: Commit**

```bash
git add packages/
git commit -m "feat: scaffold npm workspace for zeroclaw web UI"
```

---

## Phase 2: Shared UI Components

### Task 2: DesktopViewer component

**Files:**
- Create: `packages/ui-components/src/types.ts`
- Create: `packages/ui-components/src/DesktopViewer.tsx`
- Create: `packages/ui-components/src/index.ts`
- Create: `packages/ui-components/test/DesktopViewer.test.tsx`

**Reference:** `/home/ben/dev/scape/console/src/components/DesktopViewer.tsx`

**Step 1: Create shared types**

```ts
// packages/ui-components/src/types.ts
export type ConnectionState = 'connecting' | 'connected' | 'disconnected' | 'failed'
```

**Step 2: Write the DesktopViewer test**

```tsx
// packages/ui-components/test/DesktopViewer.test.tsx
import { describe, it, expect, vi, beforeEach } from 'vitest'
import { render, screen } from '@testing-library/react'
import { DesktopViewer } from '../src/DesktopViewer'
import type { ConnectionState } from '../src/types'

// Mock noVNC RFB
vi.mock('@novnc/novnc/core/rfb', () => {
  return {
    default: vi.fn().mockImplementation(() => ({
      scaleViewport: false,
      resizeSession: false,
      clipViewport: false,
      showDotCursor: false,
      qualityLevel: 0,
      compressionLevel: 0,
      addEventListener: vi.fn(),
      disconnect: vi.fn(),
    })),
  }
})

describe('DesktopViewer', () => {
  it('renders the container div', () => {
    const { container } = render(
      <DesktopViewer wsUrl="wss://test.example.com/ws/vnc" />
    )
    expect(container.querySelector('[data-testid="desktop-container"]')).toBeTruthy()
  })

  it('calls onConnectionChange callback', async () => {
    const onChange = vi.fn()
    render(
      <DesktopViewer
        wsUrl="wss://test.example.com/ws/vnc"
        onConnectionChange={onChange}
      />
    )
    // The mock RFB constructor is called, which triggers connecting state
    expect(onChange).toHaveBeenCalledWith('connecting')
  })

  it('renders nothing meaningful when wsUrl is empty', () => {
    const { container } = render(<DesktopViewer wsUrl="" />)
    expect(container.querySelector('[data-testid="desktop-container"]')).toBeTruthy()
  })
})
```

**Step 3: Run test to verify it fails**

Run: `cd /home/ben/dev/scape-agents/packages/ui-components && npx vitest run`
Expected: FAIL — modules not found

**Step 4: Write DesktopViewer implementation**

Port from `/home/ben/dev/scape/console/src/components/DesktopViewer.tsx` with these changes:
- Accept `wsUrl: string` instead of `subdomain` + `baseDomain`
- Report state via `onConnectionChange` callback instead of internal rendering
- No internal status bar (consumer renders their own)
- Add `data-testid="desktop-container"` for testing

```tsx
// packages/ui-components/src/DesktopViewer.tsx
import { useEffect, useRef, useCallback } from 'react'
import RFB from '@novnc/novnc/core/rfb'
import type { ConnectionState } from './types'

export interface DesktopViewerProps {
  wsUrl: string
  visible?: boolean
  className?: string
  onConnectionChange?: (state: ConnectionState) => void
}

const MAX_RECONNECT_DELAY = 30_000
const INITIAL_RECONNECT_DELAY = 2_000

export function DesktopViewer({
  wsUrl,
  visible = true,
  className,
  onConnectionChange,
}: DesktopViewerProps) {
  const containerRef = useRef<HTMLDivElement>(null)
  const rfbRef = useRef<InstanceType<typeof RFB> | null>(null)
  const reconnectTimeoutRef = useRef<ReturnType<typeof setTimeout> | null>(null)
  const reconnectAttemptRef = useRef(0)
  const unmountedRef = useRef(false)

  const connect = useCallback(() => {
    if (!wsUrl || !containerRef.current || unmountedRef.current) return

    if (rfbRef.current) {
      rfbRef.current.disconnect()
      rfbRef.current = null
    }

    onConnectionChange?.('connecting')

    try {
      const rfb = new RFB(containerRef.current, wsUrl, {
        wsProtocols: ['binary'],
      })

      rfb.scaleViewport = true
      rfb.resizeSession = false
      rfb.clipViewport = false
      rfb.showDotCursor = true
      rfb.qualityLevel = 6
      rfb.compressionLevel = 2

      rfb.addEventListener('connect', () => {
        onConnectionChange?.('connected')
        reconnectAttemptRef.current = 0
      })

      rfb.addEventListener('disconnect', (e: CustomEvent) => {
        if (unmountedRef.current) return
        const clean = e.detail?.clean ?? false
        if (!clean) {
          onConnectionChange?.('disconnected')
          const attempt = reconnectAttemptRef.current
          const delay = Math.min(
            INITIAL_RECONNECT_DELAY * Math.pow(2, attempt),
            MAX_RECONNECT_DELAY,
          )
          reconnectAttemptRef.current = attempt + 1
          reconnectTimeoutRef.current = setTimeout(() => {
            if (!unmountedRef.current) connect()
          }, delay)
        } else {
          onConnectionChange?.('disconnected')
        }
      })

      rfb.addEventListener('securityfailure', () => {
        onConnectionChange?.('failed')
      })

      rfbRef.current = rfb
    } catch (err) {
      console.error('Failed to create RFB connection:', err)
      onConnectionChange?.('failed')
    }
  }, [wsUrl, onConnectionChange])

  useEffect(() => {
    if (!wsUrl) return
    unmountedRef.current = false
    connect()

    return () => {
      unmountedRef.current = true
      if (reconnectTimeoutRef.current) {
        clearTimeout(reconnectTimeoutRef.current)
      }
      if (rfbRef.current) {
        rfbRef.current.disconnect()
        rfbRef.current = null
      }
    }
  }, [wsUrl, connect])

  useEffect(() => {
    if (visible && rfbRef.current) {
      requestAnimationFrame(() => {
        if (rfbRef.current) {
          rfbRef.current.scaleViewport = true
        }
      })
    }
  }, [visible])

  return (
    <div
      ref={containerRef}
      data-testid="desktop-container"
      className={className}
      style={{ width: '100%', height: '100%', background: '#1A1B26' }}
    />
  )
}
```

**Step 5: Create index.ts exports**

```ts
// packages/ui-components/src/index.ts
export { DesktopViewer } from './DesktopViewer'
export type { DesktopViewerProps } from './DesktopViewer'
export type { ConnectionState } from './types'
```

**Step 6: Run tests**

Run: `cd /home/ben/dev/scape-agents/packages/ui-components && npx vitest run`
Expected: PASS

**Step 7: Commit**

```bash
git add packages/ui-components/
git commit -m "feat: add DesktopViewer shared component with noVNC"
```

### Task 3: TerminalWidget component

**Files:**
- Create: `packages/ui-components/src/TerminalWidget.tsx`
- Create: `packages/ui-components/test/TerminalWidget.test.tsx`
- Modify: `packages/ui-components/src/index.ts`

**Reference:** `/home/ben/dev/scape/console/src/components/Terminal.tsx`

**Step 1: Write TerminalWidget test**

```tsx
// packages/ui-components/test/TerminalWidget.test.tsx
import { describe, it, expect, vi } from 'vitest'
import { render } from '@testing-library/react'
import { TerminalWidget } from '../src/TerminalWidget'

// Mock xterm.js — it requires a real DOM for canvas
vi.mock('@xterm/xterm', () => ({
  Terminal: vi.fn().mockImplementation(() => ({
    loadAddon: vi.fn(),
    open: vi.fn(),
    onData: vi.fn(),
    onResize: vi.fn(),
    write: vi.fn(),
    dispose: vi.fn(),
    cols: 80,
    rows: 24,
    unicode: { activeVersion: '11' },
  })),
}))

vi.mock('@xterm/addon-fit', () => ({
  FitAddon: vi.fn().mockImplementation(() => ({
    fit: vi.fn(),
    dispose: vi.fn(),
  })),
}))

vi.mock('@xterm/addon-web-links', () => ({
  WebLinksAddon: vi.fn().mockImplementation(() => ({ dispose: vi.fn() })),
}))

vi.mock('@xterm/addon-webgl', () => ({
  WebglAddon: vi.fn().mockImplementation(() => ({
    onContextLoss: vi.fn(),
    dispose: vi.fn(),
  })),
}))

vi.mock('@xterm/addon-unicode11', () => ({
  Unicode11Addon: vi.fn().mockImplementation(() => ({ dispose: vi.fn() })),
}))

describe('TerminalWidget', () => {
  it('renders the terminal container', () => {
    const { container } = render(
      <TerminalWidget wsUrl="wss://test.example.com/ws/terminal" />
    )
    expect(container.querySelector('[data-testid="terminal-container"]')).toBeTruthy()
  })

  it('calls onConnectionChange', () => {
    const onChange = vi.fn()
    render(
      <TerminalWidget
        wsUrl="wss://test.example.com/ws/terminal"
        onConnectionChange={onChange}
      />
    )
    // WebSocket mock will be constructed, triggering connecting state
    expect(onChange).toHaveBeenCalled()
  })
})
```

**Step 2: Run test to verify it fails**

Run: `cd /home/ben/dev/scape-agents/packages/ui-components && npx vitest run`
Expected: FAIL — TerminalWidget not found

**Step 3: Write TerminalWidget implementation**

Port from `/home/ben/dev/scape/console/src/components/Terminal.tsx` with same changes as DesktopViewer (accept `wsUrl`, report via callback).

```tsx
// packages/ui-components/src/TerminalWidget.tsx
import { useEffect, useRef, useCallback } from 'react'
import { Terminal } from '@xterm/xterm'
import { FitAddon } from '@xterm/addon-fit'
import { WebLinksAddon } from '@xterm/addon-web-links'
import { WebglAddon } from '@xterm/addon-webgl'
import { Unicode11Addon } from '@xterm/addon-unicode11'
import type { ConnectionState } from './types'

export interface TerminalWidgetProps {
  wsUrl: string
  visible?: boolean
  className?: string
  onConnectionChange?: (state: ConnectionState) => void
}

const tokyoNightTheme = {
  background: '#1A1B26',
  foreground: '#A9B1D6',
  cursor: '#A9B1D6',
  cursorAccent: '#1A1B26',
  selectionBackground: '#2F3549',
  selectionForeground: '#1A1B26',
  black: '#1A1B26',
  red: '#C0CAF5',
  green: '#9ECE6A',
  yellow: '#0DB9D7',
  blue: '#2AC3DE',
  magenta: '#BB9AF7',
  cyan: '#B4F9F8',
  white: '#A9B1D6',
  brightBlack: '#444B6A',
  brightRed: '#C0CAF5',
  brightGreen: '#9ECE6A',
  brightYellow: '#0DB9D7',
  brightBlue: '#2AC3DE',
  brightMagenta: '#BB9AF7',
  brightCyan: '#B4F9F8',
  brightWhite: '#D5D6DB',
}

const MAX_RECONNECT_DELAY = 30_000
const INITIAL_RECONNECT_DELAY = 1_000

export function TerminalWidget({
  wsUrl,
  visible = true,
  className,
  onConnectionChange,
}: TerminalWidgetProps) {
  const termRef = useRef<HTMLDivElement>(null)
  const terminalRef = useRef<Terminal | null>(null)
  const wsRef = useRef<WebSocket | null>(null)
  const fitAddonRef = useRef<FitAddon | null>(null)
  const reconnectTimeoutRef = useRef<ReturnType<typeof setTimeout> | null>(null)
  const reconnectAttemptRef = useRef(0)
  const unmountedRef = useRef(false)

  const connectWs = useCallback(() => {
    if (!wsUrl || !terminalRef.current || unmountedRef.current) return

    const terminal = terminalRef.current

    if (wsRef.current) {
      wsRef.current.onopen = null
      wsRef.current.onmessage = null
      wsRef.current.onclose = null
      wsRef.current.onerror = null
      if (wsRef.current.readyState === WebSocket.OPEN || wsRef.current.readyState === WebSocket.CONNECTING) {
        wsRef.current.close()
      }
      wsRef.current = null
    }

    onConnectionChange?.('connecting')

    const ws = new WebSocket(wsUrl)
    ws.binaryType = 'arraybuffer'
    wsRef.current = ws

    ws.onopen = () => {
      reconnectAttemptRef.current = 0
      onConnectionChange?.('connected')
      const { cols, rows } = terminal
      ws.send(JSON.stringify({ type: 'resize', cols, rows }))
    }

    ws.onmessage = (event) => {
      if (event.data instanceof ArrayBuffer) {
        terminal.write(new Uint8Array(event.data))
      } else {
        terminal.write(event.data)
      }
    }

    ws.onclose = () => {
      if (unmountedRef.current) return
      onConnectionChange?.('disconnected')
      const attempt = reconnectAttemptRef.current
      const delay = Math.min(INITIAL_RECONNECT_DELAY * Math.pow(2, attempt), MAX_RECONNECT_DELAY)
      reconnectAttemptRef.current = attempt + 1
      terminal.write(`\r\n\x1b[33mConnection lost. Reconnecting in ${(delay / 1000).toFixed(0)}s...\x1b[0m\r\n`)
      reconnectTimeoutRef.current = setTimeout(() => {
        if (!unmountedRef.current) connectWs()
      }, delay)
    }

    ws.onerror = () => {
      // onclose fires after onerror
    }
  }, [wsUrl, onConnectionChange])

  useEffect(() => {
    if (!wsUrl || !termRef.current) return

    unmountedRef.current = false

    const terminal = new Terminal({
      cursorBlink: true,
      fontSize: 14,
      fontFamily: 'JetBrains Mono, Menlo, Monaco, monospace',
      scrollback: 5000,
      allowProposedApi: true,
      customGlyphs: true,
      rescaleOverlappingGlyphs: true,
      theme: tokyoNightTheme,
    })

    const fitAddon = new FitAddon()
    terminal.loadAddon(fitAddon)
    terminal.loadAddon(new WebLinksAddon())

    const unicode11 = new Unicode11Addon()
    terminal.loadAddon(unicode11)
    terminal.unicode.activeVersion = '11'

    terminal.open(termRef.current)

    try {
      const webgl = new WebglAddon()
      webgl.onContextLoss(() => webgl.dispose())
      terminal.loadAddon(webgl)
    } catch {
      // WebGL unavailable, DOM renderer used
    }

    fitAddon.fit()

    terminalRef.current = terminal
    fitAddonRef.current = fitAddon

    terminal.onData((data) => {
      const ws = wsRef.current
      if (ws && ws.readyState === WebSocket.OPEN) {
        ws.send(data)
      }
    })

    terminal.onResize(({ cols, rows }) => {
      const ws = wsRef.current
      if (ws && ws.readyState === WebSocket.OPEN) {
        ws.send(JSON.stringify({ type: 'resize', cols, rows }))
      }
    })

    connectWs()

    const handleResize = () => fitAddonRef.current?.fit()
    window.addEventListener('resize', handleResize)

    return () => {
      unmountedRef.current = true
      window.removeEventListener('resize', handleResize)
      if (reconnectTimeoutRef.current) {
        clearTimeout(reconnectTimeoutRef.current)
      }
      if (wsRef.current) {
        wsRef.current.onclose = null
        wsRef.current.close()
      }
      terminal.dispose()
      terminalRef.current = null
      fitAddonRef.current = null
    }
  }, [wsUrl, connectWs])

  useEffect(() => {
    if (visible && fitAddonRef.current) {
      requestAnimationFrame(() => fitAddonRef.current?.fit())
    }
  }, [visible])

  return (
    <div
      className={className}
      style={{ background: '#1A1B26', padding: 12, height: '100%' }}
    >
      <div ref={termRef} data-testid="terminal-container" />
    </div>
  )
}
```

**Step 4: Update index.ts exports**

```ts
// packages/ui-components/src/index.ts
export { DesktopViewer } from './DesktopViewer'
export type { DesktopViewerProps } from './DesktopViewer'
export { TerminalWidget } from './TerminalWidget'
export type { TerminalWidgetProps } from './TerminalWidget'
export type { ConnectionState } from './types'
```

**Step 5: Run tests**

Run: `cd /home/ben/dev/scape-agents/packages/ui-components && npx vitest run`
Expected: PASS (all tests)

**Step 6: Commit**

```bash
git add packages/ui-components/
git commit -m "feat: add TerminalWidget shared component with xterm.js"
```

---

## Phase 3: Server

### Task 4: Server config module

**Files:**
- Create: `packages/zeroclaw-ui/src/server/config.ts`
- Create: `packages/zeroclaw-ui/test/server/config.test.ts`

**Step 1: Write config test**

```ts
// packages/zeroclaw-ui/test/server/config.test.ts
import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'

describe('config', () => {
  const originalEnv = process.env

  beforeEach(() => {
    vi.resetModules()
    process.env = { ...originalEnv }
  })

  afterEach(() => {
    process.env = originalEnv
  })

  it('uses default values when no env vars set', async () => {
    const { config } = await import('../../src/server/config')
    expect(config.port).toBe(5000)
    expect(config.gatewayUrl).toBe('ws://127.0.0.1:3000')
    expect(config.botName).toBe('ZeroClaw')
    expect(config.enableVoice).toBe(true)
    expect(config.themeAccent).toBe('6366f1')
  })

  it('reads values from environment variables', async () => {
    process.env.PORT = '9000'
    process.env.GATEWAY_URL = 'ws://custom:4000'
    process.env.BOT_NAME = 'TestBot'
    process.env.ENABLE_VOICE = 'false'
    process.env.THEME_ACCENT = 'ff0000'
    const { config } = await import('../../src/server/config')
    expect(config.port).toBe(9000)
    expect(config.gatewayUrl).toBe('ws://custom:4000')
    expect(config.botName).toBe('TestBot')
    expect(config.enableVoice).toBe(false)
    expect(config.themeAccent).toBe('ff0000')
  })
})
```

**Step 2: Run test to verify it fails**

Run: `cd /home/ben/dev/scape-agents/packages/zeroclaw-ui && npx vitest run test/server/config.test.ts`
Expected: FAIL

**Step 3: Write config implementation**

```ts
// packages/zeroclaw-ui/src/server/config.ts
import path from 'path'
import os from 'os'

export const config = {
  port: parseInt(process.env.PORT || '5000', 10),
  gatewayUrl: process.env.GATEWAY_URL || 'ws://127.0.0.1:3000',
  botName: process.env.BOT_NAME || 'ZeroClaw',
  enableVoice: (process.env.ENABLE_VOICE || 'true').toLowerCase() === 'true',
  ttsCommand:
    process.env.TTS_COMMAND ||
    'edge-tts --text "{{TEXT}}" --write-media "{{OUTPUT}}"',
  dataDir: process.env.DATA_DIR || path.join(os.homedir(), '.zeroclaw-ui'),
  themeAccent: process.env.THEME_ACCENT || '6366f1',
} as const

/** Public config safe to send to the browser (no secrets). */
export const publicConfig = {
  botName: config.botName,
  enableVoice: config.enableVoice,
  themeAccent: config.themeAccent,
} as const
```

**Step 4: Run test to verify it passes**

Run: `cd /home/ben/dev/scape-agents/packages/zeroclaw-ui && npx vitest run test/server/config.test.ts`
Expected: PASS

**Step 5: Commit**

```bash
git add packages/zeroclaw-ui/src/server/config.ts packages/zeroclaw-ui/test/server/config.test.ts
git commit -m "feat: add server config module with env var parsing"
```

### Task 5: Message store

**Files:**
- Create: `packages/zeroclaw-ui/src/server/store.ts`
- Create: `packages/zeroclaw-ui/test/server/store.test.ts`

**Reference:** `clawtimeingest.txt` — `src/store.js` section

**Step 1: Write store test**

```ts
// packages/zeroclaw-ui/test/server/store.test.ts
import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'
import fs from 'fs'
import path from 'path'
import os from 'os'

describe('MessageStore', () => {
  let tmpDir: string
  let store: typeof import('../../src/server/store')

  beforeEach(async () => {
    vi.resetModules()
    tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), 'store-test-'))
    process.env.DATA_DIR = tmpDir
    store = await import('../../src/server/store')
  })

  afterEach(() => {
    fs.rmSync(tmpDir, { recursive: true, force: true })
  })

  it('returns empty array when no messages exist', () => {
    expect(store.getMessages()).toEqual([])
  })

  it('saves and retrieves a message', () => {
    const msg = store.saveMessage({ role: 'user', text: 'hello' })
    expect(msg.id).toBeDefined()
    expect(msg.role).toBe('user')
    expect(msg.text).toBe('hello')
    expect(msg.timestamp).toBeDefined()

    const messages = store.getMessages()
    expect(messages).toHaveLength(1)
    expect(messages[0].text).toBe('hello')
  })

  it('limits returned messages', () => {
    for (let i = 0; i < 10; i++) {
      store.saveMessage({ role: 'user', text: `msg ${i}` })
    }
    const messages = store.getMessages(5)
    expect(messages).toHaveLength(5)
    expect(messages[0].text).toBe('msg 5')
  })

  it('flushes to disk synchronously', () => {
    store.saveMessage({ role: 'user', text: 'persist me' })
    store.flushSync()
    const filePath = path.join(tmpDir, 'messages.json')
    expect(fs.existsSync(filePath)).toBe(true)
    const data = JSON.parse(fs.readFileSync(filePath, 'utf8'))
    expect(data).toHaveLength(1)
    expect(data[0].text).toBe('persist me')
  })
})
```

**Step 2: Run test to verify it fails**

Run: `cd /home/ben/dev/scape-agents/packages/zeroclaw-ui && npx vitest run test/server/store.test.ts`
Expected: FAIL

**Step 3: Write store implementation**

```ts
// packages/zeroclaw-ui/src/server/store.ts
import crypto from 'crypto'
import fs from 'fs'
import path from 'path'
import { config } from './config'

export interface StoredMessage {
  id: string
  role: 'user' | 'bot'
  text: string
  timestamp: string
  images?: string[]
  widget?: Record<string, unknown>
  streaming?: boolean
}

const STORE_PATH = path.join(config.dataDir, 'messages.json')
const WRITE_DEBOUNCE_MS = 200
const WRITE_MAX_DELAY_MS = 1000

let messages: StoredMessage[] | null = null
let writeTimer: ReturnType<typeof setTimeout> | null = null
let writeForceTimer: ReturnType<typeof setTimeout> | null = null
let pendingWrite = false

function ensureLoaded(): StoredMessage[] {
  if (messages === null) {
    try {
      if (fs.existsSync(STORE_PATH)) {
        const raw = fs.readFileSync(STORE_PATH, 'utf8')
        const parsed = JSON.parse(raw)
        messages = Array.isArray(parsed) ? parsed : []
      } else {
        messages = []
      }
    } catch {
      messages = []
    }
  }
  return messages
}

function schedulePersist(immediate = false): void {
  pendingWrite = true
  if (writeTimer) {
    clearTimeout(writeTimer)
    writeTimer = null
  }
  if (immediate) {
    doPersist()
    return
  }
  if (!writeForceTimer) {
    writeForceTimer = setTimeout(() => {
      writeForceTimer = null
      if (pendingWrite) doPersist()
    }, WRITE_MAX_DELAY_MS)
  }
  writeTimer = setTimeout(() => {
    writeTimer = null
    if (pendingWrite) doPersist()
  }, WRITE_DEBOUNCE_MS)
}

function doPersist(): void {
  if (!pendingWrite || messages === null) return
  pendingWrite = false
  if (writeTimer) { clearTimeout(writeTimer); writeTimer = null }
  if (writeForceTimer) { clearTimeout(writeForceTimer); writeForceTimer = null }

  const data = JSON.stringify(messages, null, 2)
  fs.writeFile(STORE_PATH, data, 'utf8', (err) => {
    if (err) console.error('[store] Failed to persist:', err.message)
  })
}

export function flushSync(): void {
  if (pendingWrite && messages !== null) {
    try {
      fs.mkdirSync(path.dirname(STORE_PATH), { recursive: true })
      fs.writeFileSync(STORE_PATH, JSON.stringify(messages, null, 2), 'utf8')
      pendingWrite = false
    } catch (err) {
      console.error('[store] Failed to flush:', err)
    }
  }
}

export function getMessages(limit = 200): StoredMessage[] {
  const msgs = ensureLoaded()
  if (msgs.length <= limit) return msgs
  return msgs.slice(-limit)
}

export function saveMessage(msg: {
  role: 'user' | 'bot'
  text: string
  images?: string[]
  widget?: Record<string, unknown>
}): StoredMessage {
  const msgs = ensureLoaded()
  const entry: StoredMessage = {
    id: crypto.randomUUID(),
    role: msg.role,
    text: msg.text || '',
    timestamp: new Date().toISOString(),
  }
  if (msg.images?.length) entry.images = msg.images
  if (msg.widget) entry.widget = msg.widget
  msgs.push(entry)
  schedulePersist(true)
  return entry
}
```

**Step 4: Run test to verify it passes**

Run: `cd /home/ben/dev/scape-agents/packages/zeroclaw-ui && npx vitest run test/server/store.test.ts`
Expected: PASS

**Step 5: Commit**

```bash
git add packages/zeroclaw-ui/src/server/store.ts packages/zeroclaw-ui/test/server/store.test.ts
git commit -m "feat: add message store with debounced persistence"
```

### Task 6: Gateway WebSocket proxy

**Files:**
- Create: `packages/zeroclaw-ui/src/server/gateway-proxy.ts`
- Create: `packages/zeroclaw-ui/test/server/gateway-proxy.test.ts`

**Step 1: Write gateway proxy test**

```ts
// packages/zeroclaw-ui/test/server/gateway-proxy.test.ts
import { describe, it, expect, vi } from 'vitest'
import { createGatewayProxy } from '../../src/server/gateway-proxy'

// We test the proxy function's interface — actual WS behavior
// is tested in integration tests.

describe('createGatewayProxy', () => {
  it('returns a handler function', () => {
    const handler = createGatewayProxy('ws://localhost:3000')
    expect(typeof handler).toBe('function')
  })

  it('accepts a valid gateway URL', () => {
    expect(() => createGatewayProxy('ws://localhost:3000')).not.toThrow()
    expect(() => createGatewayProxy('wss://gateway.example.com')).not.toThrow()
  })
})
```

**Step 2: Run test to verify it fails**

Run: `cd /home/ben/dev/scape-agents/packages/zeroclaw-ui && npx vitest run test/server/gateway-proxy.test.ts`
Expected: FAIL

**Step 3: Write gateway proxy implementation**

```ts
// packages/zeroclaw-ui/src/server/gateway-proxy.ts
import { WebSocket as NodeWebSocket } from 'ws'
import type { ServerWebSocket } from '@hono/node-ws'

/**
 * Creates a WebSocket proxy handler that forwards frames
 * between a client WebSocket and the zeroclaw gateway.
 */
export function createGatewayProxy(gatewayUrl: string) {
  return function handleUpgrade(clientWs: ServerWebSocket) {
    const upstream = new NodeWebSocket(gatewayUrl)
    const buffer: (string | Buffer)[] = []

    upstream.on('open', () => {
      // Send buffered messages
      for (const msg of buffer) {
        upstream.send(msg)
      }
      buffer.length = 0
    })

    upstream.on('message', (data) => {
      try {
        if (clientWs.readyState === 1) {
          clientWs.send(typeof data === 'string' ? data : data.toString())
        }
      } catch {
        // Client disconnected
      }
    })

    upstream.on('close', () => {
      try { clientWs.close() } catch { /* ignore */ }
    })

    upstream.on('error', (err) => {
      console.error('[gateway-proxy] upstream error:', err.message)
      try { clientWs.close() } catch { /* ignore */ }
    })

    // Client -> upstream
    clientWs.addEventListener('message', (event) => {
      const data = typeof event.data === 'string' ? event.data : event.data
      if (upstream.readyState === NodeWebSocket.OPEN) {
        upstream.send(data)
      } else {
        buffer.push(data as string | Buffer)
      }
    })

    clientWs.addEventListener('close', () => {
      if (upstream.readyState === NodeWebSocket.OPEN) {
        upstream.close()
      }
    })
  }
}
```

**Step 4: Run test to verify it passes**

Run: `cd /home/ben/dev/scape-agents/packages/zeroclaw-ui && npx vitest run test/server/gateway-proxy.test.ts`
Expected: PASS

**Step 5: Commit**

```bash
git add packages/zeroclaw-ui/src/server/gateway-proxy.ts packages/zeroclaw-ui/test/server/gateway-proxy.test.ts
git commit -m "feat: add WebSocket proxy for zeroclaw gateway"
```

### Task 7: TTS endpoint

**Files:**
- Create: `packages/zeroclaw-ui/src/server/tts.ts`
- Create: `packages/zeroclaw-ui/test/server/tts.test.ts`

**Step 1: Write TTS test**

```ts
// packages/zeroclaw-ui/test/server/tts.test.ts
import { describe, it, expect, vi, beforeEach } from 'vitest'
import { cleanTextForTTS, buildTTSCommand } from '../../src/server/tts'

describe('cleanTextForTTS', () => {
  it('strips code blocks', () => {
    expect(cleanTextForTTS('hello ```code``` world')).toBe('hello code block world')
  })

  it('strips inline code', () => {
    expect(cleanTextForTTS('use `npm install`')).toBe('use')
  })

  it('extracts link text', () => {
    expect(cleanTextForTTS('see [docs](https://example.com)')).toBe('see docs')
  })

  it('strips markdown formatting', () => {
    expect(cleanTextForTTS('**bold** and *italic*')).toBe('bold and italic')
  })

  it('strips URLs', () => {
    expect(cleanTextForTTS('visit https://example.com now')).toBe('visit link now')
  })

  it('normalizes whitespace', () => {
    expect(cleanTextForTTS('  hello   world  ')).toBe('hello world')
  })
})

describe('buildTTSCommand', () => {
  it('replaces TEXT and OUTPUT placeholders', () => {
    const template = 'edge-tts --text "{{TEXT}}" --write-media "{{OUTPUT}}"'
    const cmd = buildTTSCommand(template, 'hello world', '/tmp/out.mp3')
    expect(cmd).toContain("'hello world'")
    expect(cmd).toContain('/tmp/out.mp3')
  })

  it('escapes single quotes in text', () => {
    const template = 'edge-tts --text "{{TEXT}}" --write-media "{{OUTPUT}}"'
    const cmd = buildTTSCommand(template, "it's fine", '/tmp/out.mp3')
    expect(cmd).toContain("'it'\\''s fine'")
  })
})
```

**Step 2: Run test to verify it fails**

Run: `cd /home/ben/dev/scape-agents/packages/zeroclaw-ui && npx vitest run test/server/tts.test.ts`
Expected: FAIL

**Step 3: Write TTS implementation**

```ts
// packages/zeroclaw-ui/src/server/tts.ts
import { exec } from 'child_process'
import crypto from 'crypto'
import fs from 'fs'
import path from 'path'
import { promisify } from 'util'
import { config } from './config'

const execAsync = promisify(exec)
const TTS_DIR = '/tmp/zeroclaw-tts'

// Ensure TTS directory exists
try { fs.mkdirSync(TTS_DIR, { recursive: true }) } catch { /* ignore */ }

export function cleanTextForTTS(text: string): string {
  return text
    .replace(/```[\s\S]*?```/g, ' code block ')
    .replace(/`[^`]+`/g, '')
    .replace(/\[([^\]]+)\]\([^)]+\)/g, '$1')
    .replace(/[#*_~>]/g, '')
    .replace(/https?:\/\/\S+/g, ' link ')
    .replace(/\s+/g, ' ')
    .trim()
}

export function buildTTSCommand(
  template: string,
  text: string,
  outputPath: string,
): string {
  const escaped = text.replace(/'/g, "'\\''")
  return template
    .replace(/"?\{\{TEXT\}\}"?/g, "'" + escaped + "'")
    .replace(/\{\{OUTPUT\}\}/g, outputPath)
}

export async function generateTTS(text: string): Promise<Buffer> {
  const clean = cleanTextForTTS(text).slice(0, 4000)
  if (!clean || clean.length < 3) {
    throw new Error('Text too short for TTS')
  }

  const id = crypto.randomBytes(8).toString('hex')
  const outputPath = path.join(TTS_DIR, `tts-${id}.mp3`)

  try {
    const cmd = buildTTSCommand(config.ttsCommand, clean, outputPath)
    await execAsync(cmd, { timeout: 30000 })
    const audio = fs.readFileSync(outputPath)
    return audio
  } finally {
    try { fs.unlinkSync(outputPath) } catch { /* ignore */ }
  }
}
```

**Step 4: Run test to verify it passes**

Run: `cd /home/ben/dev/scape-agents/packages/zeroclaw-ui && npx vitest run test/server/tts.test.ts`
Expected: PASS

**Step 5: Commit**

```bash
git add packages/zeroclaw-ui/src/server/tts.ts packages/zeroclaw-ui/test/server/tts.test.ts
git commit -m "feat: add TTS endpoint with configurable command"
```

### Task 8: HTTP + WS server (Hono)

**Files:**
- Create: `packages/zeroclaw-ui/src/server/index.ts`

**Step 1: Write the server**

```ts
// packages/zeroclaw-ui/src/server/index.ts
import { Hono } from 'hono'
import { serve } from '@hono/node-server'
import { createNodeWebSocket } from '@hono/node-ws'
import { serveStatic } from '@hono/node-server/serve-static'
import path from 'path'
import fs from 'fs'
import { config, publicConfig } from './config'
import { getMessages, saveMessage, flushSync } from './store'
import { generateTTS } from './tts'
import { createGatewayProxy } from './gateway-proxy'

// Ensure data directory exists
fs.mkdirSync(config.dataDir, { recursive: true })

const app = new Hono()
const { injectWebSocket, upgradeWebSocket } = createNodeWebSocket({ app })

// --- REST API ---

app.get('/api/config', (c) => c.json(publicConfig))

app.get('/api/messages', (c) => {
  const limit = parseInt(c.req.query('limit') || '200', 10)
  return c.json(getMessages(limit))
})

app.post('/api/messages', async (c) => {
  const body = await c.req.json<{ role: 'user' | 'bot'; text: string }>()
  const msg = saveMessage(body)
  return c.json(msg, 201)
})

app.post('/api/tts', async (c) => {
  if (!config.enableVoice) {
    return c.json({ error: 'Voice disabled' }, 403)
  }
  const { text } = await c.req.json<{ text: string }>()
  try {
    const audio = await generateTTS(text)
    return new Response(audio, {
      headers: { 'Content-Type': 'audio/mpeg' },
    })
  } catch (err) {
    return c.json({ error: (err as Error).message }, 500)
  }
})

// --- WebSocket gateway proxy ---

const gatewayHandler = createGatewayProxy(config.gatewayUrl)

app.get(
  '/api/gateway',
  upgradeWebSocket(() => ({
    onOpen(_event, ws) {
      gatewayHandler(ws)
    },
  })),
)

// --- Static files (built SPA) ---

const distDir = path.join(import.meta.dirname, '../../dist/client')
if (fs.existsSync(distDir)) {
  app.use('/*', serveStatic({ root: distDir }))
  // SPA fallback
  app.get('*', (c) => {
    const indexPath = path.join(distDir, 'index.html')
    const html = fs.readFileSync(indexPath, 'utf8')
    return c.html(html)
  })
}

// --- Start ---

const server = serve({ fetch: app.fetch, port: config.port }, (info) => {
  console.log(`[zeroclaw-ui] Server running on :${info.port}`)
})

injectWebSocket(server)

// Graceful shutdown
process.on('SIGTERM', () => {
  console.log('[zeroclaw-ui] Shutting down...')
  flushSync()
  process.exit(0)
})

process.on('SIGINT', () => {
  console.log('[zeroclaw-ui] Shutting down...')
  flushSync()
  process.exit(0)
})
```

Note: This file is the server entry point. It depends on all previous server modules. We don't unit test it directly — it's tested via integration tests and manual verification.

**Step 2: Commit**

```bash
git add packages/zeroclaw-ui/src/server/index.ts
git commit -m "feat: add Hono HTTP + WS server entry point"
```

---

## Phase 4: Client Core

### Task 9: Types and CSS entry point

**Files:**
- Create: `packages/zeroclaw-ui/src/client/lib/types.ts`
- Create: `packages/zeroclaw-ui/src/client/index.css`
- Create: `packages/zeroclaw-ui/src/client/main.tsx`

**Step 1: Create shared types**

```ts
// packages/zeroclaw-ui/src/client/lib/types.ts

export interface ChatMessage {
  id: string
  role: 'user' | 'bot'
  text: string
  timestamp: string
  images?: string[]
  widget?: WidgetData
  streaming?: boolean
}

export interface WidgetData {
  widget: string
  id: string
  [key: string]: unknown
}

export interface WidgetResponse {
  id: string
  widget: string
  value: unknown
  action: string
}

export interface GatewayMessage {
  type: string
  text?: string
  state?: 'streaming' | 'final'
  runId?: string
  error?: string
  images?: string[]
}

export interface PublicConfig {
  botName: string
  enableVoice: boolean
  themeAccent: string
}
```

**Step 2: Create CSS entry**

```css
/* packages/zeroclaw-ui/src/client/index.css */
@tailwind base;
@tailwind components;
@tailwind utilities;
```

**Step 3: Create main.tsx**

```tsx
// packages/zeroclaw-ui/src/client/main.tsx
import { StrictMode } from 'react'
import { createRoot } from 'react-dom/client'
import { App } from './App'
import './index.css'

createRoot(document.getElementById('root')!).render(
  <StrictMode>
    <App />
  </StrictMode>,
)
```

**Step 4: Commit**

```bash
git add packages/zeroclaw-ui/src/client/
git commit -m "feat: add client entry point, types, and CSS"
```

### Task 10: Widget parsing utility

**Files:**
- Create: `packages/zeroclaw-ui/src/client/lib/widgets.ts`
- Create: `packages/zeroclaw-ui/test/client/widgets.test.ts`

**Step 1: Write widget parsing test**

```ts
// packages/zeroclaw-ui/test/client/widgets.test.ts
import { describe, it, expect } from 'vitest'
import { parseWidgets } from '../../src/client/lib/widgets'

describe('parseWidgets', () => {
  it('returns text unchanged when no widgets present', () => {
    const result = parseWidgets('Hello world')
    expect(result.text).toBe('Hello world')
    expect(result.widgets).toEqual([])
  })

  it('extracts a single widget', () => {
    const input = 'Choose: [[WIDGET:{"widget":"buttons","id":"c1","label":"Pick:","options":["A","B"]}]]'
    const result = parseWidgets(input)
    expect(result.text).toBe('Choose:')
    expect(result.widgets).toHaveLength(1)
    expect(result.widgets[0].widget).toBe('buttons')
    expect(result.widgets[0].id).toBe('c1')
  })

  it('extracts multiple widgets', () => {
    const input = 'First [[WIDGET:{"widget":"buttons","id":"a","options":["X"]}]] then [[WIDGET:{"widget":"confirm","id":"b","title":"Sure?"}]]'
    const result = parseWidgets(input)
    expect(result.widgets).toHaveLength(2)
    expect(result.widgets[0].id).toBe('a')
    expect(result.widgets[1].id).toBe('b')
  })

  it('handles malformed widget JSON gracefully', () => {
    const input = 'Bad [[WIDGET:{invalid json}]] data'
    const result = parseWidgets(input)
    expect(result.text).toBe('Bad  data')
    expect(result.widgets).toEqual([])
  })
})
```

**Step 2: Run test to verify it fails**

Run: `cd /home/ben/dev/scape-agents/packages/zeroclaw-ui && npx vitest run test/client/widgets.test.ts`
Expected: FAIL

**Step 3: Write widget parsing implementation**

```ts
// packages/zeroclaw-ui/src/client/lib/widgets.ts
import type { WidgetData } from './types'

const WIDGET_RE = /\[\[WIDGET:(.*?)\]\]/g

export function parseWidgets(text: string): {
  text: string
  widgets: WidgetData[]
} {
  const widgets: WidgetData[] = []
  const cleanText = text.replace(WIDGET_RE, (_match, json: string) => {
    try {
      const data = JSON.parse(json) as WidgetData
      if (data.widget && data.id) {
        widgets.push(data)
      }
    } catch {
      // Skip malformed widgets
    }
    return ''
  })

  return {
    text: cleanText.trim(),
    widgets,
  }
}
```

**Step 4: Run test to verify it passes**

Run: `cd /home/ben/dev/scape-agents/packages/zeroclaw-ui && npx vitest run test/client/widgets.test.ts`
Expected: PASS

**Step 5: Commit**

```bash
git add packages/zeroclaw-ui/src/client/lib/widgets.ts packages/zeroclaw-ui/test/client/widgets.test.ts
git commit -m "feat: add widget parsing utility"
```

### Task 11: useGateway WebSocket hook

**Files:**
- Create: `packages/zeroclaw-ui/src/client/hooks/useGateway.ts`
- Create: `packages/zeroclaw-ui/test/client/useGateway.test.ts`

**Step 1: Write useGateway test**

```ts
// packages/zeroclaw-ui/test/client/useGateway.test.ts
import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'
import { renderHook, act } from '@testing-library/react'
import { useGateway } from '../../src/client/hooks/useGateway'

// Mock WebSocket
class MockWebSocket {
  static OPEN = 1
  static instances: MockWebSocket[] = []
  readyState = 0
  onopen: (() => void) | null = null
  onmessage: ((e: { data: string }) => void) | null = null
  onclose: (() => void) | null = null
  onerror: (() => void) | null = null
  sent: string[] = []

  constructor(public url: string) {
    MockWebSocket.instances.push(this)
    setTimeout(() => {
      this.readyState = 1
      this.onopen?.()
    }, 0)
  }

  send(data: string) { this.sent.push(data) }
  close() { this.readyState = 3 }
}

beforeEach(() => {
  MockWebSocket.instances = []
  vi.stubGlobal('WebSocket', MockWebSocket)
})

afterEach(() => {
  vi.unstubAllGlobals()
})

describe('useGateway', () => {
  it('connects to the gateway URL', async () => {
    renderHook(() => useGateway())
    expect(MockWebSocket.instances).toHaveLength(1)
    expect(MockWebSocket.instances[0].url).toContain('/api/gateway')
  })

  it('sends chat messages', async () => {
    const { result } = renderHook(() => useGateway())

    // Wait for connection
    await act(async () => {
      await new Promise((r) => setTimeout(r, 10))
    })

    act(() => {
      result.current.sendMessage('hello')
    })

    const ws = MockWebSocket.instances[0]
    expect(ws.sent).toHaveLength(1)
    const parsed = JSON.parse(ws.sent[0])
    expect(parsed.type).toBe('chat')
    expect(parsed.text).toBe('hello')
  })

  it('tracks connection state', async () => {
    const { result } = renderHook(() => useGateway())
    expect(result.current.connected).toBe(false)

    await act(async () => {
      await new Promise((r) => setTimeout(r, 10))
    })

    expect(result.current.connected).toBe(true)
  })
})
```

**Step 2: Run test to verify it fails**

Run: `cd /home/ben/dev/scape-agents/packages/zeroclaw-ui && npx vitest run test/client/useGateway.test.ts`
Expected: FAIL

**Step 3: Write useGateway implementation**

```ts
// packages/zeroclaw-ui/src/client/hooks/useGateway.ts
import { useEffect, useRef, useState, useCallback } from 'react'
import type { ChatMessage, GatewayMessage } from '../lib/types'

export interface UseGatewayOptions {
  onMessage?: (msg: GatewayMessage) => void
}

export function useGateway(options?: UseGatewayOptions) {
  const [connected, setConnected] = useState(false)
  const wsRef = useRef<WebSocket | null>(null)
  const reconnectRef = useRef<ReturnType<typeof setTimeout> | null>(null)
  const attemptRef = useRef(0)
  const unmountedRef = useRef(false)
  const optionsRef = useRef(options)
  optionsRef.current = options

  const connect = useCallback(() => {
    if (unmountedRef.current) return

    const proto = location.protocol === 'https:' ? 'wss:' : 'ws:'
    const wsUrl = `${proto}//${location.host}/api/gateway`

    const ws = new WebSocket(wsUrl)
    wsRef.current = ws

    ws.onopen = () => {
      setConnected(true)
      attemptRef.current = 0
    }

    ws.onmessage = (event) => {
      try {
        const data = JSON.parse(event.data) as GatewayMessage
        optionsRef.current?.onMessage?.(data)
      } catch {
        // Non-JSON message, ignore
      }
    }

    ws.onclose = () => {
      setConnected(false)
      if (unmountedRef.current) return
      const delay = Math.min(1000 * Math.pow(2, attemptRef.current), 30000)
      attemptRef.current += 1
      reconnectRef.current = setTimeout(connect, delay)
    }

    ws.onerror = () => {
      // onclose will handle reconnection
    }
  }, [])

  useEffect(() => {
    unmountedRef.current = false
    connect()

    return () => {
      unmountedRef.current = true
      if (reconnectRef.current) clearTimeout(reconnectRef.current)
      if (wsRef.current) {
        wsRef.current.onclose = null
        wsRef.current.close()
      }
    }
  }, [connect])

  const sendMessage = useCallback((text: string, images?: string[]) => {
    const ws = wsRef.current
    if (ws && ws.readyState === WebSocket.OPEN) {
      ws.send(JSON.stringify({ type: 'chat', text, images: images || [] }))
    }
  }, [])

  const sendWidgetResponse = useCallback(
    (id: string, widget: string, value: unknown, action = 'submit') => {
      const ws = wsRef.current
      if (ws && ws.readyState === WebSocket.OPEN) {
        ws.send(JSON.stringify({ type: 'widget_response', id, widget, value, action }))
      }
    },
    [],
  )

  return { connected, sendMessage, sendWidgetResponse }
}
```

**Step 4: Run test to verify it passes**

Run: `cd /home/ben/dev/scape-agents/packages/zeroclaw-ui && npx vitest run test/client/useGateway.test.ts`
Expected: PASS

**Step 5: Commit**

```bash
git add packages/zeroclaw-ui/src/client/hooks/useGateway.ts packages/zeroclaw-ui/test/client/useGateway.test.ts
git commit -m "feat: add useGateway WebSocket hook"
```

### Task 12: useMessages hook

**Files:**
- Create: `packages/zeroclaw-ui/src/client/hooks/useMessages.ts`

**Step 1: Write useMessages implementation**

This hook manages the message list state, handles streaming updates from the gateway, and persists to the server.

```ts
// packages/zeroclaw-ui/src/client/hooks/useMessages.ts
import { useState, useCallback, useEffect } from 'react'
import type { ChatMessage, GatewayMessage } from '../lib/types'

export function useMessages() {
  const [messages, setMessages] = useState<ChatMessage[]>([])
  const [loading, setLoading] = useState(true)

  // Load history from server on mount
  useEffect(() => {
    fetch('/api/messages')
      .then((r) => r.json())
      .then((msgs: ChatMessage[]) => {
        setMessages(msgs)
        setLoading(false)
      })
      .catch(() => setLoading(false))
  }, [])

  const addUserMessage = useCallback((text: string) => {
    const msg: ChatMessage = {
      id: crypto.randomUUID(),
      role: 'user',
      text,
      timestamp: new Date().toISOString(),
    }
    setMessages((prev) => [...prev, msg])

    // Persist to server
    fetch('/api/messages', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ role: 'user', text }),
    }).catch(() => {})

    return msg
  }, [])

  const handleGatewayMessage = useCallback((data: GatewayMessage) => {
    if (data.type !== 'chat') return

    setMessages((prev) => {
      const existing = prev.findIndex(
        (m) => m.role === 'bot' && m.streaming && data.runId,
      )

      if (existing !== -1 && data.state === 'streaming') {
        // Update existing streaming message
        const updated = [...prev]
        updated[existing] = {
          ...updated[existing],
          text: data.text || '',
        }
        return updated
      }

      if (existing !== -1 && data.state === 'final') {
        // Finalize streaming message
        const updated = [...prev]
        updated[existing] = {
          ...updated[existing],
          text: data.text || '',
          streaming: false,
        }
        // Persist final message
        fetch('/api/messages', {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ role: 'bot', text: data.text || '' }),
        }).catch(() => {})
        return updated
      }

      if (data.state === 'streaming') {
        // New streaming message
        return [
          ...prev,
          {
            id: data.runId || crypto.randomUUID(),
            role: 'bot' as const,
            text: data.text || '',
            timestamp: new Date().toISOString(),
            streaming: true,
          },
        ]
      }

      if (data.state === 'final') {
        // Single-shot final message (no prior streaming)
        const msg: ChatMessage = {
          id: data.runId || crypto.randomUUID(),
          role: 'bot',
          text: data.text || '',
          timestamp: new Date().toISOString(),
        }
        fetch('/api/messages', {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ role: 'bot', text: data.text || '' }),
        }).catch(() => {})
        return [...prev, msg]
      }

      return prev
    })
  }, [])

  return { messages, loading, addUserMessage, handleGatewayMessage }
}
```

**Step 2: Commit**

```bash
git add packages/zeroclaw-ui/src/client/hooks/useMessages.ts
git commit -m "feat: add useMessages hook with streaming support"
```

---

## Phase 5: Chat UI Components

### Task 13: MessageBubble component

**Files:**
- Create: `packages/zeroclaw-ui/src/client/components/Chat/MessageBubble.tsx`
- Create: `packages/zeroclaw-ui/test/client/MessageBubble.test.tsx`

**Step 1: Write test**

```tsx
// packages/zeroclaw-ui/test/client/MessageBubble.test.tsx
import { describe, it, expect } from 'vitest'
import { render, screen } from '@testing-library/react'
import { MessageBubble } from '../../src/client/components/Chat/MessageBubble'

describe('MessageBubble', () => {
  it('renders user message with correct alignment', () => {
    render(
      <MessageBubble
        message={{ id: '1', role: 'user', text: 'Hello', timestamp: '' }}
      />
    )
    expect(screen.getByText('Hello')).toBeTruthy()
  })

  it('renders bot message', () => {
    render(
      <MessageBubble
        message={{ id: '2', role: 'bot', text: 'Hi there', timestamp: '' }}
      />
    )
    expect(screen.getByText('Hi there')).toBeTruthy()
  })

  it('shows streaming indicator when streaming', () => {
    const { container } = render(
      <MessageBubble
        message={{ id: '3', role: 'bot', text: 'typing...', timestamp: '', streaming: true }}
      />
    )
    expect(container.querySelector('[data-streaming="true"]')).toBeTruthy()
  })
})
```

**Step 2: Run test to verify it fails, then implement, then verify it passes**

The implementation should render a styled div with the message text, right-aligned for user, left-aligned for bot. Add a `data-streaming` attribute when streaming.

**Step 3: Commit**

```bash
git add packages/zeroclaw-ui/src/client/components/Chat/MessageBubble.tsx packages/zeroclaw-ui/test/client/MessageBubble.test.tsx
git commit -m "feat: add MessageBubble component"
```

### Task 14: InputBar component

**Files:**
- Create: `packages/zeroclaw-ui/src/client/components/Chat/InputBar.tsx`
- Create: `packages/zeroclaw-ui/test/client/InputBar.test.tsx`

**Step 1: Write test**

```tsx
// packages/zeroclaw-ui/test/client/InputBar.test.tsx
import { describe, it, expect, vi } from 'vitest'
import { render, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { InputBar } from '../../src/client/components/Chat/InputBar'

describe('InputBar', () => {
  it('renders a text input and send button', () => {
    render(<InputBar onSend={() => {}} />)
    expect(screen.getByPlaceholderText(/type a message/i)).toBeTruthy()
    expect(screen.getByRole('button', { name: /send/i })).toBeTruthy()
  })

  it('calls onSend with input text on submit', async () => {
    const onSend = vi.fn()
    const user = userEvent.setup()
    render(<InputBar onSend={onSend} />)

    const input = screen.getByPlaceholderText(/type a message/i)
    await user.type(input, 'hello world')
    await user.click(screen.getByRole('button', { name: /send/i }))

    expect(onSend).toHaveBeenCalledWith('hello world')
  })

  it('clears input after send', async () => {
    const user = userEvent.setup()
    render(<InputBar onSend={() => {}} />)

    const input = screen.getByPlaceholderText(/type a message/i) as HTMLTextAreaElement
    await user.type(input, 'hello')
    await user.click(screen.getByRole('button', { name: /send/i }))

    expect(input.value).toBe('')
  })

  it('does not send empty messages', async () => {
    const onSend = vi.fn()
    const user = userEvent.setup()
    render(<InputBar onSend={onSend} />)
    await user.click(screen.getByRole('button', { name: /send/i }))
    expect(onSend).not.toHaveBeenCalled()
  })
})
```

**Step 2: Implement InputBar** — textarea with send button. Enter sends (shift+enter for newline). Clear on send.

**Step 3: Run tests, verify pass, commit**

```bash
git add packages/zeroclaw-ui/src/client/components/Chat/ packages/zeroclaw-ui/test/client/InputBar.test.tsx
git commit -m "feat: add InputBar component"
```

### Task 15: ChatPanel, App shell, and Layout

**Files:**
- Create: `packages/zeroclaw-ui/src/client/components/Chat/ChatPanel.tsx`
- Create: `packages/zeroclaw-ui/src/client/components/Layout/AppShell.tsx`
- Create: `packages/zeroclaw-ui/src/client/components/Layout/Sidebar.tsx`
- Create: `packages/zeroclaw-ui/src/client/App.tsx`

**Step 1: Build ChatPanel** — renders message list with auto-scroll, contains InputBar at bottom.

**Step 2: Build AppShell** — flex layout with Sidebar on left, main content area.

**Step 3: Build Sidebar** — vertical nav with icons for Chat, Desktop, Terminal. Uses state to toggle active view.

**Step 4: Build App.tsx** — wires together useGateway, useMessages, AppShell, ChatPanel, and fullscreen modals.

**Step 5: Verify the SPA builds**

Run: `cd /home/ben/dev/scape-agents/packages/zeroclaw-ui && npx vite build`
Expected: Build succeeds, output in `dist/client/`

**Step 6: Commit**

```bash
git add packages/zeroclaw-ui/src/client/
git commit -m "feat: add ChatPanel, AppShell, Sidebar, and App"
```

---

## Phase 6: Widget Components

### Task 16: WidgetRenderer and core widgets

**Files:**
- Create: `packages/zeroclaw-ui/src/client/components/Widgets/WidgetRenderer.tsx`
- Create: `packages/zeroclaw-ui/src/client/components/Widgets/ButtonsWidget.tsx`
- Create: `packages/zeroclaw-ui/src/client/components/Widgets/ConfirmWidget.tsx`
- Create: `packages/zeroclaw-ui/src/client/components/Widgets/ProgressWidget.tsx`
- Create: `packages/zeroclaw-ui/src/client/components/Widgets/CodeWidget.tsx`
- Create: `packages/zeroclaw-ui/src/client/components/Widgets/FormWidget.tsx`
- Create: `packages/zeroclaw-ui/test/client/WidgetRenderer.test.tsx`

**Step 1: Write WidgetRenderer test**

```tsx
// packages/zeroclaw-ui/test/client/WidgetRenderer.test.tsx
import { describe, it, expect, vi } from 'vitest'
import { render, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { WidgetRenderer } from '../../src/client/components/Widgets/WidgetRenderer'

describe('WidgetRenderer', () => {
  it('renders buttons widget', () => {
    render(
      <WidgetRenderer
        widget={{ widget: 'buttons', id: 'b1', label: 'Pick:', options: ['A', 'B'] }}
        onRespond={() => {}}
      />
    )
    expect(screen.getByText('Pick:')).toBeTruthy()
    expect(screen.getByText('A')).toBeTruthy()
    expect(screen.getByText('B')).toBeTruthy()
  })

  it('calls onRespond when button clicked', async () => {
    const onRespond = vi.fn()
    const user = userEvent.setup()
    render(
      <WidgetRenderer
        widget={{ widget: 'buttons', id: 'b1', options: ['X', 'Y'] }}
        onRespond={onRespond}
      />
    )
    await user.click(screen.getByText('X'))
    expect(onRespond).toHaveBeenCalledWith('b1', 'buttons', 'X', 'submit')
  })

  it('renders confirm widget', () => {
    render(
      <WidgetRenderer
        widget={{ widget: 'confirm', id: 'c1', title: 'Delete?', message: 'Are you sure?' }}
        onRespond={() => {}}
      />
    )
    expect(screen.getByText('Delete?')).toBeTruthy()
    expect(screen.getByText('Are you sure?')).toBeTruthy()
  })

  it('renders progress widget', () => {
    render(
      <WidgetRenderer
        widget={{ widget: 'progress', id: 'p1', label: 'Loading...', value: 50 }}
        onRespond={() => {}}
      />
    )
    expect(screen.getByText('Loading...')).toBeTruthy()
  })

  it('renders code widget with copy button', () => {
    render(
      <WidgetRenderer
        widget={{ widget: 'code', id: 'cd1', code: 'console.log("hi")', language: 'js' }}
        onRespond={() => {}}
      />
    )
    expect(screen.getByText('console.log("hi")')).toBeTruthy()
  })

  it('returns null for unknown widget type', () => {
    const { container } = render(
      <WidgetRenderer
        widget={{ widget: 'unknown', id: 'u1' }}
        onRespond={() => {}}
      />
    )
    expect(container.innerHTML).toBe('')
  })
})
```

**Step 2: Implement each widget as a simple component, then WidgetRenderer as a switch.**

**Step 3: Run tests, verify pass, commit**

```bash
git add packages/zeroclaw-ui/src/client/components/Widgets/ packages/zeroclaw-ui/test/client/WidgetRenderer.test.tsx
git commit -m "feat: add widget renderer and core widget components"
```

---

## Phase 7: Fullscreen Desktop & Terminal Modals

### Task 17: DesktopModal and TerminalModal

**Files:**
- Create: `packages/zeroclaw-ui/src/client/components/Desktop/DesktopModal.tsx`
- Create: `packages/zeroclaw-ui/src/client/components/Terminal/TerminalModal.tsx`
- Create: `packages/zeroclaw-ui/test/client/DesktopModal.test.tsx`
- Create: `packages/zeroclaw-ui/test/client/TerminalModal.test.tsx`

**Step 1: Write DesktopModal test**

```tsx
// packages/zeroclaw-ui/test/client/DesktopModal.test.tsx
import { describe, it, expect, vi } from 'vitest'
import { render, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { DesktopModal } from '../../src/client/components/Desktop/DesktopModal'

// Mock the ui-components DesktopViewer
vi.mock('@scape/ui-components', () => ({
  DesktopViewer: ({ wsUrl }: { wsUrl: string }) => (
    <div data-testid="desktop-viewer">{wsUrl}</div>
  ),
}))

describe('DesktopModal', () => {
  it('renders nothing when not open', () => {
    const { container } = render(<DesktopModal open={false} onClose={() => {}} />)
    expect(container.querySelector('[data-testid="desktop-modal"]')).toBeNull()
  })

  it('renders fullscreen overlay when open', () => {
    render(<DesktopModal open={true} onClose={() => {}} />)
    expect(screen.getByTestId('desktop-modal')).toBeTruthy()
  })

  it('calls onClose when close button clicked', async () => {
    const onClose = vi.fn()
    const user = userEvent.setup()
    render(<DesktopModal open={true} onClose={onClose} />)
    await user.click(screen.getByRole('button', { name: /close/i }))
    expect(onClose).toHaveBeenCalled()
  })

  it('shows connection status', () => {
    render(<DesktopModal open={true} onClose={() => {}} />)
    expect(screen.getByTestId('desktop-viewer')).toBeTruthy()
  })
})
```

**Step 2: Implement DesktopModal** — fixed fullscreen overlay (`inset-0 z-50`), floating toolbar at top with close button and connection state indicator. Wraps `<DesktopViewer>` from `@scape/ui-components`. Constructs `wsUrl` from `window.location` → `wss://${host}/ws/vnc`. Escape key closes.

**Step 3: Write TerminalModal test** — same pattern as DesktopModal but uses `TerminalWidget` and `wsUrl` → `/ws/terminal`.

**Step 4: Implement TerminalModal** — same fullscreen overlay pattern.

**Step 5: Run tests, verify pass, commit**

```bash
git add packages/zeroclaw-ui/src/client/components/Desktop/ packages/zeroclaw-ui/src/client/components/Terminal/ packages/zeroclaw-ui/test/client/
git commit -m "feat: add fullscreen Desktop and Terminal modals"
```

---

## Phase 8: Voice Mode

### Task 18: useVoice hook

**Files:**
- Create: `packages/zeroclaw-ui/src/client/hooks/useVoice.ts`
- Create: `packages/zeroclaw-ui/src/client/components/Chat/VoiceButton.tsx`
- Create: `packages/zeroclaw-ui/test/client/useVoice.test.ts`

**Step 1: Write useVoice test**

```ts
// packages/zeroclaw-ui/test/client/useVoice.test.ts
import { describe, it, expect, vi, beforeEach } from 'vitest'
import { renderHook, act } from '@testing-library/react'
import { useVoice } from '../../src/client/hooks/useVoice'

// Mock SpeechRecognition
const mockRecognition = {
  start: vi.fn(),
  stop: vi.fn(),
  abort: vi.fn(),
  onresult: null as any,
  onend: null as any,
  onerror: null as any,
  continuous: false,
  interimResults: false,
  lang: '',
}

beforeEach(() => {
  vi.stubGlobal('SpeechRecognition', vi.fn(() => mockRecognition))
  vi.stubGlobal('webkitSpeechRecognition', vi.fn(() => mockRecognition))
})

describe('useVoice', () => {
  it('starts and stops listening', () => {
    const { result } = renderHook(() => useVoice({ onTranscript: () => {} }))

    act(() => result.current.startListening())
    expect(result.current.listening).toBe(true)
    expect(mockRecognition.start).toHaveBeenCalled()

    act(() => result.current.stopListening())
    expect(mockRecognition.stop).toHaveBeenCalled()
  })

  it('reports speech availability', () => {
    const { result } = renderHook(() => useVoice({ onTranscript: () => {} }))
    expect(result.current.available).toBe(true)
  })
})
```

**Step 2: Implement useVoice** — wraps browser SpeechRecognition. Provides `startListening`, `stopListening`, `listening`, `available`. Calls `onTranscript(text)` when speech result arrives. Also has `playTTS(text)` that calls `POST /api/tts` and plays the returned audio via Web Audio API.

**Step 3: Implement VoiceButton** — push-to-talk button with microphone icon. Shows recording state.

**Step 4: Run tests, verify pass, commit**

```bash
git add packages/zeroclaw-ui/src/client/hooks/useVoice.ts packages/zeroclaw-ui/src/client/components/Chat/VoiceButton.tsx packages/zeroclaw-ui/test/client/useVoice.test.ts
git commit -m "feat: add voice mode with browser STT and server TTS"
```

---

## Phase 9: Task Panel

### Task 19: TaskPanel component

**Files:**
- Create: `packages/zeroclaw-ui/src/client/components/Layout/TaskPanel.tsx`

**Step 1: Implement TaskPanel** — reads tasks from the gateway (or a REST endpoint). Displays markdown-based task list like ClawTime with Active/Blocked/Backlog/Done sections. Collapsible sidebar panel.

**Step 2: Commit**

```bash
git add packages/zeroclaw-ui/src/client/components/Layout/TaskPanel.tsx
git commit -m "feat: add task panel component"
```

---

## Phase 10: Integration & NixOS Template

### Task 20: Integration test

**Files:**
- Create: `packages/zeroclaw-ui/test/integration/chat-flow.test.ts`

**Step 1: Write integration test** — starts the Hono server on a random port, connects a WebSocket client, sends a chat message through the gateway proxy (against a mock gateway), verifies the message round-trip and persistence in the store.

**Step 2: Run all tests**

Run: `cd /home/ben/dev/scape-agents/packages/zeroclaw-ui && npx vitest run`
Expected: All tests PASS

Run: `cd /home/ben/dev/scape-agents/packages/ui-components && npx vitest run`
Expected: All tests PASS

**Step 3: Commit**

```bash
git add packages/zeroclaw-ui/test/integration/
git commit -m "test: add chat flow integration test"
```

### Task 21: Update zeroclaw NixOS template

**Files:**
- Modify: `templates/zeroclaw/default.nix`

**Step 1: Add systemd service for the UI server**

Add to `templates/zeroclaw/default.nix`:

```nix
# ZeroClaw Web UI — serves the React SPA and proxies chat WS to gateway
systemd.services.zeroclaw-ui = {
  description = "ZeroClaw Web UI";
  wantedBy = [ "multi-user.target" ];
  after = [ "network-online.target" "zeroclaw-daemon.service" ];
  wants = [ "zeroclaw-daemon.service" ];
  environment = {
    PORT = "5000";
    GATEWAY_URL = "ws://127.0.0.1:3000";
    NODE_ENV = "production";
  };
  serviceConfig = {
    User = "operator";
    Group = "operator";
    Restart = "always";
    RestartSec = "2s";
    WorkingDirectory = "/path/to/zeroclaw-ui";  # Will be a nix store path
    ExecStart = "${pkgs.nodejs-slim}/bin/node dist/server/index.js";
  };
};
```

**Step 2: Update services declaration**

Change the template services to expose the UI on port 5000:

```nix
services = [
  { name = "ui"; port = 5000; path = "/"; type = "http"; }
];
```

**Step 3: Commit**

```bash
git add templates/zeroclaw/default.nix
git commit -m "feat: add zeroclaw-ui systemd service to template"
```

### Task 22: Final build verification

**Step 1: Run all tests**

```bash
cd /home/ben/dev/scape-agents/packages && npm test --workspaces
```

**Step 2: Build the SPA**

```bash
cd /home/ben/dev/scape-agents/packages/zeroclaw-ui && npx vite build
```

**Step 3: Verify server starts**

```bash
cd /home/ben/dev/scape-agents/packages/zeroclaw-ui && node --import tsx src/server/index.ts
# Should output: [zeroclaw-ui] Server running on :5000
```

**Step 4: Final commit**

```bash
git add -A
git commit -m "feat: zeroclaw web UI complete — chat, voice, desktop, terminal"
```
