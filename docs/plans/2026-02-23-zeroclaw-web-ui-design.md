# ZeroClaw Web UI Design

A React/Vite/TypeScript web interface for the zeroclaw template, inspired by ClawTime. Runs inside the VM as a service on port 5000, exposed at `op-<instanceid>.scape.host.com`.

## Architecture

```
Browser (op-<instanceid>.scape.host.com)
    |
    | Platform proxy (scape console subdomain-proxy)
    |   /ws/* --> agent:8080  (terminal, VNC - direct)
    |   /*    --> service:5000 (UI server)
    v
┌─────────────────────────────────┐
│  UI Server (port 5000)          │
│  - Serves React SPA (static)   │
│  - REST: /api/tts, /api/history│
│  - WS proxy: /api/gateway -->  │──> ZeroClaw Gateway (port 3000)
│    localhost:3000               │
└─────────────────────────────────┘

Browser WS connections (via platform proxy, no VM-side proxy):
  wss://op-<id>.scape.host.com/ws/terminal --> agent:8080/ws/terminal
  wss://op-<id>.scape.host.com/ws/vnc      --> agent:8080/ws/vnc
```

## Repository Structure

```
packages/
  ui-components/                 # Shared package (npm workspace)
    package.json                 # @scape/ui-components
    tsconfig.json
    vitest.config.ts
    src/
      DesktopViewer.tsx          # noVNC wrapper (ported from scape console)
      TerminalWidget.tsx         # xterm.js wrapper (ported from scape console)
      index.ts                   # Public exports
    test/
      DesktopViewer.test.tsx
      TerminalWidget.test.tsx

  zeroclaw-ui/                   # The web UI app
    package.json                 # @scape/zeroclaw-ui
    vite.config.ts
    vitest.config.ts
    tsconfig.json
    index.html
    src/
      server/                    # Node.js backend (Hono)
        index.ts                 # HTTP + WS server, port 5000
        config.ts                # Env vars with defaults
        tts.ts                   # TTS via edge-tts (shell out)
        store.ts                 # Message persistence (JSON file)
        gateway-proxy.ts         # WS proxy to zeroclaw gateway

      client/                    # React SPA
        main.tsx                 # Entry point
        App.tsx                  # Router/layout
        components/
          Chat/
            ChatPanel.tsx        # Main chat view
            MessageBubble.tsx    # Individual message
            InputBar.tsx         # Text input + send + attachments
            VoiceButton.tsx      # Push-to-talk
          Widgets/
            WidgetRenderer.tsx   # Parses [[WIDGET:...]] and renders
            ButtonsWidget.tsx
            ConfirmWidget.tsx
            ProgressWidget.tsx
            CodeWidget.tsx
            FormWidget.tsx
          Desktop/
            DesktopModal.tsx     # Fullscreen overlay wrapping DesktopViewer
          Terminal/
            TerminalModal.tsx    # Fullscreen overlay wrapping TerminalWidget
          Layout/
            AppShell.tsx         # Main layout container
            Sidebar.tsx          # Nav: chat, desktop, terminal, tasks
            TaskPanel.tsx        # Task list panel
        hooks/
          useGateway.ts          # WebSocket to gateway
          useVoice.ts            # SpeechRecognition + TTS playback
          useMessages.ts         # Message history state
        lib/
          types.ts               # Shared TypeScript types
          widgets.ts             # Widget parsing utilities

      test/
        client/
          ChatPanel.test.tsx
          MessageBubble.test.tsx
          InputBar.test.tsx
          WidgetRenderer.test.tsx
          DesktopModal.test.tsx
          TerminalModal.test.tsx
          useGateway.test.ts
          useVoice.test.ts
        server/
          store.test.ts
          tts.test.ts
          gateway-proxy.test.ts
          config.test.ts
        integration/
          chat-flow.test.ts      # Full WS message round-trip
```

## Shared Package: `@scape/ui-components`

Extracted from the scape console so both projects can import them.

### DesktopViewer

Wraps noVNC `RFB` class. Props:
- `wsUrl: string` - Full WebSocket URL (e.g. `wss://op-xxx.scape.host.com/ws/vnc`)
- `visible?: boolean` - Triggers resize/repaint on visibility change
- `className?: string`
- `onConnectionChange?: (state: ConnectionState) => void`

Changes from console version:
- Accept `wsUrl` directly instead of constructing from subdomain/baseDomain (more reusable)
- Connection state reported via callback instead of internal rendering
- Container styling left to consumer

### TerminalWidget

Wraps xterm.js with addons (FitAddon, WebLinksAddon, WebglAddon, Unicode11Addon). Props:
- `wsUrl: string` - Full WebSocket URL
- `visible?: boolean`
- `className?: string`
- `onConnectionChange?: (state: ConnectionState) => void`

Same changes as DesktopViewer — accept `wsUrl` directly, report state via callback.

## UI Server (Hono)

Minimal Node.js server responsibilities:

1. **Static file serving** - Serve the built Vite SPA from `dist/client/`
2. **Message history REST API**
   - `GET /api/messages` - Load recent messages
   - `POST /api/messages` - Save a message (called by client after send)
3. **TTS endpoint**
   - `POST /api/tts` - Generate speech audio from text, return as audio/mpeg
4. **Gateway WS proxy**
   - `WS /api/gateway` - Proxy WebSocket frames to zeroclaw gateway on localhost:3000
   - Pure frame forwarding, no processing

## Chat System

### Gateway WebSocket Protocol

The client connects to `wss://host/api/gateway` (proxied to zeroclaw gateway on port 3000). The protocol follows the OpenClaw/ClawTime pattern:

**Client -> Gateway:**
```json
{ "type": "chat", "text": "Hello", "images": [] }
```

**Gateway -> Client:**
```json
{ "type": "chat", "state": "streaming", "runId": "abc123", "text": "partial response..." }
{ "type": "chat", "state": "final", "runId": "abc123", "text": "complete response" }
```

### Widget Protocol

Widgets are embedded in bot messages using the ClawTime format:
```
[[WIDGET:{"widget":"buttons","id":"choice1","label":"Pick:","options":["A","B"]}]]
```

The `WidgetRenderer` component:
1. Parses `[[WIDGET:...]]` markers from message text
2. Strips them from displayed text
3. Renders the appropriate widget component below the message
4. Sends widget responses back via the gateway WS

### Message Persistence

Server-side JSON file at `$DATA_DIR/messages.json`. Same approach as ClawTime:
- Debounced writes (200ms debounce, 1s max delay)
- Sync flush on shutdown
- Messages loaded into memory on first access
- Append-only with UUID IDs

## Voice Mode

- **STT**: Browser `SpeechRecognition` API (no server-side Whisper needed initially)
- **TTS**: Server-side via configurable `TTS_COMMAND` (defaults to edge-tts)
  - Client sends text to `POST /api/tts`
  - Server runs edge-tts, returns audio as base64 or streaming audio
  - Client plays via Web Audio API
- **Push-to-talk**: Tap button to start, release to stop
- **Barge-in**: New user input cancels ongoing TTS playback

## Fullscreen Desktop/Terminal

Both open as fullscreen modal overlays:
- Triggered by sidebar buttons or keyboard shortcuts
- Take full viewport with a floating top toolbar (close button, connection status)
- Underlying chat remains mounted but hidden (preserves state)
- Escape key or close button dismisses the overlay
- Connection is established on first open, maintained across close/reopen

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `PORT` | `5000` | UI server port |
| `GATEWAY_URL` | `ws://127.0.0.1:3000` | ZeroClaw gateway WebSocket URL |
| `BOT_NAME` | `ZeroClaw` | Display name in chat |
| `ENABLE_VOICE` | `true` | Enable voice mode |
| `TTS_COMMAND` | `edge-tts --text "{{TEXT}}" --write-media "{{OUTPUT}}"` | TTS command template |
| `DATA_DIR` | `/home/operator/.zeroclaw-ui` | Data directory for messages, config |
| `THEME_ACCENT` | `6366f1` | Primary accent color (hex) |

## Testing Strategy

### Unit Tests (Vitest + React Testing Library)
- Widget parsing/rendering
- Message bubble rendering (markdown, code blocks, images)
- Input bar behavior (send on enter, multiline with shift+enter)
- Voice hook (mock SpeechRecognition)
- Server store (read/write/debounce)
- Server config (env var parsing, defaults)
- TTS endpoint (mock child_process)

### Component Tests
- DesktopModal (fullscreen open/close, connection state display)
- TerminalModal (fullscreen open/close, connection state display)
- ChatPanel (message list rendering, auto-scroll)
- WidgetRenderer (all widget types)

### Integration Tests
- Chat WS flow: connect, send message, receive streaming response, verify persistence
- Gateway proxy: verify frame forwarding

### Not Testing (initially)
- Actual noVNC/xterm.js rendering (these are well-tested libraries)
- TTS audio playback (browser API mocking is fragile)
- Platform proxy routing (tested in scape console)

## NixOS Template Changes

Update `templates/zeroclaw/default.nix` to:
1. Add the built UI as a package
2. Add a systemd service for the UI server (Node.js on port 5000)
3. Update the declared services list to expose port 5000 as the primary service
4. Keep the zeroclaw gateway on port 3000 (internal only)

```nix
services = [
  { name = "ui"; port = 5000; path = "/"; type = "http"; }
];
```

## What's NOT Included

- **3D avatar** - Skip Three.js avatar from ClawTime
- **WebAuthn auth** - Platform handles auth
- **E2E encryption** - Platform proxy handles TLS
- **Cloudflare tunnel** - Platform provides the hostname
- **Server-side STT** - Use browser SpeechRecognition only
- **Image upload** - Can be added later
