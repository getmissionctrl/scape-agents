import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'
import { Hono } from 'hono'
import fs from 'fs'
import path from 'path'
import os from 'os'

describe('Chat flow integration', () => {
  let tmpDir: string
  let app: Hono

  beforeEach(async () => {
    vi.resetModules()

    tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), 'chat-flow-'))
    process.env.DATA_DIR = tmpDir
    process.env.PORT = '0'
    process.env.ENABLE_VOICE = 'false'

    // Import modules fresh so they pick up new env
    const { config, publicConfig } = await import('../../src/server/config')
    const store = await import('../../src/server/store')

    app = new Hono()

    app.get('/api/config', (c) => c.json(publicConfig))

    app.get('/api/messages', (c) => {
      const limit = parseInt(c.req.query('limit') || '200', 10)
      return c.json(store.getMessages(limit))
    })

    app.post('/api/messages', async (c) => {
      const body = await c.req.json<{ role: 'user' | 'bot'; text: string }>()
      const msg = store.saveMessage(body)
      return c.json(msg, 201)
    })

    app.post('/api/tts', (c) => {
      if (!config.enableVoice) {
        return c.json({ error: 'Voice disabled' }, 403)
      }
      return c.json({ error: 'Not implemented' }, 500)
    })
  })

  afterEach(() => {
    fs.rmSync(tmpDir, { recursive: true, force: true })
  })

  it('returns public config', async () => {
    const res = await app.request('/api/config')
    expect(res.status).toBe(200)
    const data = await res.json()
    expect(data.botName).toBe('ZeroClaw')
    expect(data.enableVoice).toBe(false)
    expect(data.themeAccent).toBe('6366f1')
  })

  it('returns empty messages initially', async () => {
    const res = await app.request('/api/messages')
    expect(res.status).toBe(200)
    const data = await res.json()
    expect(data).toEqual([])
  })

  it('saves and retrieves user message', async () => {
    const postRes = await app.request('/api/messages', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ role: 'user', text: 'hello bot' }),
    })
    expect(postRes.status).toBe(201)
    const saved = await postRes.json()
    expect(saved.id).toBeDefined()
    expect(saved.role).toBe('user')
    expect(saved.text).toBe('hello bot')

    const getRes = await app.request('/api/messages')
    const messages = await getRes.json()
    expect(messages).toHaveLength(1)
    expect(messages[0].text).toBe('hello bot')
  })

  it('saves bot response and retrieves full conversation', async () => {
    await app.request('/api/messages', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ role: 'user', text: 'What time is it?' }),
    })

    await app.request('/api/messages', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ role: 'bot', text: 'I cannot tell the time.' }),
    })

    const res = await app.request('/api/messages')
    const messages = await res.json()
    expect(messages).toHaveLength(2)
    expect(messages[0].role).toBe('user')
    expect(messages[1].role).toBe('bot')
    expect(messages[1].text).toBe('I cannot tell the time.')
  })

  it('respects message limit', async () => {
    for (let i = 0; i < 10; i++) {
      await app.request('/api/messages', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ role: 'user', text: `message ${i}` }),
      })
    }

    const res = await app.request('/api/messages?limit=3')
    const messages = await res.json()
    expect(messages).toHaveLength(3)
    expect(messages[0].text).toBe('message 7')
  })

  it('returns 403 for TTS when voice disabled', async () => {
    const res = await app.request('/api/tts', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ text: 'hello' }),
    })
    expect(res.status).toBe(403)
  })
})
