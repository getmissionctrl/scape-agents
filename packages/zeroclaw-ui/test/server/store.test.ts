import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'
import fs from 'fs'
import path from 'path'
import os from 'os'

describe('MessageStore', () => {
  let tmpDir: string
  let store: typeof import('../../src/server/store')

  beforeEach(async () => {
    vi.resetModules()
    tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), 'store-test-'))
    process.env.DATA_DIR = tmpDir
    store = await import('../../src/server/store')
  })

  afterEach(() => {
    fs.rmSync(tmpDir, { recursive: true, force: true })
  })

  it('returns empty array when no messages exist', () => {
    expect(store.getMessages()).toEqual([])
  })

  it('saves and retrieves a message', () => {
    const msg = store.saveMessage({ role: 'user', text: 'hello' })
    expect(msg.id).toBeDefined()
    expect(msg.role).toBe('user')
    expect(msg.text).toBe('hello')
    expect(msg.timestamp).toBeDefined()

    const messages = store.getMessages()
    expect(messages).toHaveLength(1)
    expect(messages[0].text).toBe('hello')
  })

  it('limits returned messages', () => {
    for (let i = 0; i < 10; i++) {
      store.saveMessage({ role: 'user', text: `msg ${i}` })
    }
    const messages = store.getMessages(5)
    expect(messages).toHaveLength(5)
    expect(messages[0].text).toBe('msg 5')
  })

  it('flushes to disk synchronously', () => {
    store.saveMessage({ role: 'user', text: 'persist me' })
    store.flushSync()
    const filePath = path.join(tmpDir, 'messages.json')
    expect(fs.existsSync(filePath)).toBe(true)
    const data = JSON.parse(fs.readFileSync(filePath, 'utf8'))
    expect(data).toHaveLength(1)
    expect(data[0].text).toBe('persist me')
  })
})
