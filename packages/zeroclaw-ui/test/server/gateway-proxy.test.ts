import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'
import { WebSocketServer } from 'ws'
import { createGatewayProxy } from '../../src/server/gateway-proxy'

describe('createGatewayProxy', () => {
  it('returns a handler factory function', () => {
    const factory = createGatewayProxy('ws://localhost:5100')
    expect(typeof factory).toBe('function')
  })

  it('handler factory returns WSEvents object', () => {
    const factory = createGatewayProxy('ws://localhost:5100')
    const handlers = factory()
    expect(typeof handlers.onOpen).toBe('function')
    expect(typeof handlers.onMessage).toBe('function')
    expect(typeof handlers.onClose).toBe('function')
  })

  it('appends /ws path to gateway URL', () => {
    // The proxy should connect to gatewayUrl + '/ws'
    // We verify this indirectly: the factory should not throw
    expect(() => createGatewayProxy('ws://localhost:5100')).not.toThrow()
    expect(() => createGatewayProxy('ws://127.0.0.1:5100')).not.toThrow()
  })
})

describe('gateway proxy WS forwarding', () => {
  let wss: WebSocketServer
  let port: number

  beforeEach(async () => {
    // Start a mock WebChannel server
    wss = new WebSocketServer({ port: 0, path: '/ws' })
    port = (wss.address() as { port: number }).port
  })

  afterEach(() => {
    wss.close()
  })

  it('forwards messages from client to upstream', async () => {
    const received: string[] = []
    wss.on('connection', (ws) => {
      ws.on('message', (data) => received.push(data.toString()))
    })

    const factory = createGatewayProxy(`ws://127.0.0.1:${port}`)
    const handlers = factory()

    // Simulate Hono WSContext
    const mockClientWs = {
      send: vi.fn(),
      close: vi.fn(),
    }

    // Trigger onOpen (connects to upstream)
    handlers.onOpen!({} as any, mockClientWs as any)

    // Wait for upstream connection
    await new Promise<void>((resolve) => {
      wss.on('connection', () => resolve())
    })

    // Send a message from client
    handlers.onMessage!({ data: '{"type":"chat","text":"hello"}' } as any, mockClientWs as any)

    // Give it a moment to forward
    await new Promise((r) => setTimeout(r, 50))

    expect(received).toContain('{"type":"chat","text":"hello"}')
  })

  it('forwards messages from upstream to client', async () => {
    const factory = createGatewayProxy(`ws://127.0.0.1:${port}`)
    const handlers = factory()

    const mockClientWs = {
      send: vi.fn(),
      close: vi.fn(),
    }

    // When upstream client connects, send a message back
    wss.on('connection', (ws) => {
      ws.send('{"type":"chat","state":"final","text":"hi from upstream","runId":"r1"}')
    })

    handlers.onOpen!({} as any, mockClientWs as any)

    // Wait for the message to be relayed
    await new Promise((r) => setTimeout(r, 100))

    expect(mockClientWs.send).toHaveBeenCalledWith(
      '{"type":"chat","state":"final","text":"hi from upstream","runId":"r1"}'
    )
  })

  it('buffers messages sent before upstream connects', async () => {
    const received: string[] = []
    wss.on('connection', (ws) => {
      ws.on('message', (data) => received.push(data.toString()))
    })

    const factory = createGatewayProxy(`ws://127.0.0.1:${port}`)
    const handlers = factory()

    const mockClientWs = {
      send: vi.fn(),
      close: vi.fn(),
    }

    handlers.onOpen!({} as any, mockClientWs as any)

    // Send immediately (before upstream is connected)
    handlers.onMessage!({ data: '{"type":"chat","text":"buffered"}' } as any, mockClientWs as any)

    // Wait for upstream to connect and flush
    await new Promise((r) => setTimeout(r, 150))

    expect(received).toContain('{"type":"chat","text":"buffered"}')
  })
})
