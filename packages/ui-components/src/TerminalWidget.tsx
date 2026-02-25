import { useEffect, useRef, useCallback } from 'react'
import { Terminal } from '@xterm/xterm'
import { FitAddon } from '@xterm/addon-fit'
import { WebLinksAddon } from '@xterm/addon-web-links'
import { WebglAddon } from '@xterm/addon-webgl'
import { Unicode11Addon } from '@xterm/addon-unicode11'
import '@xterm/xterm/css/xterm.css'
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

    requestAnimationFrame(() => fitAddon.fit())

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
      style={{ background: '#1A1B26', padding: 12, height: '100%', display: 'flex', flexDirection: 'column' }}
    >
      <div ref={termRef} data-testid="terminal-container" style={{ flex: 1 }} />
    </div>
  )
}
