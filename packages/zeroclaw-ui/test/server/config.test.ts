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
