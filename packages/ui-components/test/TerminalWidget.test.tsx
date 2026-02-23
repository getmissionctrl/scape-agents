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
