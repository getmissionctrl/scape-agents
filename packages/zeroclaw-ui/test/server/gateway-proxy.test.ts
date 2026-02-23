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
