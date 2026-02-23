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
