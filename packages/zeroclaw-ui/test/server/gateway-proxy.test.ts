import { describe, it, expect } from 'vitest'
import { createGatewayProxy } from '../../src/server/gateway-proxy'

describe('createGatewayProxy', () => {
  it('returns a handler factory function', () => {
    const factory = createGatewayProxy('http://localhost:3000')
    expect(typeof factory).toBe('function')
  })

  it('handler factory returns WSEvents object', () => {
    const factory = createGatewayProxy('http://localhost:3000')
    const handlers = factory()
    expect(typeof handlers.onOpen).toBe('function')
    expect(typeof handlers.onMessage).toBe('function')
    expect(typeof handlers.onClose).toBe('function')
  })

  it('accepts various gateway URLs', () => {
    expect(() => createGatewayProxy('http://localhost:3000')).not.toThrow()
    expect(() => createGatewayProxy('https://gateway.example.com')).not.toThrow()
  })
})
