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
