import crypto from 'crypto'
import fs from 'fs'
import path from 'path'
import { config } from './config'

export interface StoredMessage {
  id: string
  role: 'user' | 'bot'
  text: string
  timestamp: string
  images?: string[]
  widget?: Record<string, unknown>
  streaming?: boolean
}

const STORE_PATH = path.join(config.dataDir, 'messages.json')
const WRITE_DEBOUNCE_MS = 200
const WRITE_MAX_DELAY_MS = 1000

let messages: StoredMessage[] | null = null
let writeTimer: ReturnType<typeof setTimeout> | null = null
let writeForceTimer: ReturnType<typeof setTimeout> | null = null
let pendingWrite = false

function ensureLoaded(): StoredMessage[] {
  if (messages === null) {
    try {
      if (fs.existsSync(STORE_PATH)) {
        const raw = fs.readFileSync(STORE_PATH, 'utf8')
        const parsed = JSON.parse(raw)
        messages = Array.isArray(parsed) ? parsed : []
      } else {
        messages = []
      }
    } catch {
      messages = []
    }
  }
  return messages
}

function schedulePersist(immediate = false): void {
  pendingWrite = true
  if (writeTimer) {
    clearTimeout(writeTimer)
    writeTimer = null
  }
  if (immediate) {
    doPersist()
    return
  }
  if (!writeForceTimer) {
    writeForceTimer = setTimeout(() => {
      writeForceTimer = null
      if (pendingWrite) doPersist()
    }, WRITE_MAX_DELAY_MS)
  }
  writeTimer = setTimeout(() => {
    writeTimer = null
    if (pendingWrite) doPersist()
  }, WRITE_DEBOUNCE_MS)
}

function doPersist(): void {
  if (!pendingWrite || messages === null) return
  pendingWrite = false
  if (writeTimer) { clearTimeout(writeTimer); writeTimer = null }
  if (writeForceTimer) { clearTimeout(writeForceTimer); writeForceTimer = null }

  const data = JSON.stringify(messages, null, 2)
  fs.writeFile(STORE_PATH, data, 'utf8', (err) => {
    if (err) console.error('[store] Failed to persist:', err.message)
  })
}

export function flushSync(): void {
  if (messages !== null && messages.length > 0) {
    if (writeTimer) { clearTimeout(writeTimer); writeTimer = null }
    if (writeForceTimer) { clearTimeout(writeForceTimer); writeForceTimer = null }
    try {
      fs.mkdirSync(path.dirname(STORE_PATH), { recursive: true })
      fs.writeFileSync(STORE_PATH, JSON.stringify(messages, null, 2), 'utf8')
      pendingWrite = false
    } catch (err) {
      console.error('[store] Failed to flush:', err)
    }
  }
}

export function getMessages(limit = 200): StoredMessage[] {
  const msgs = ensureLoaded()
  if (msgs.length <= limit) return msgs
  return msgs.slice(-limit)
}

export function saveMessage(msg: {
  role: 'user' | 'bot'
  text: string
  images?: string[]
  widget?: Record<string, unknown>
}): StoredMessage {
  const msgs = ensureLoaded()
  const entry: StoredMessage = {
    id: crypto.randomUUID(),
    role: msg.role,
    text: msg.text || '',
    timestamp: new Date().toISOString(),
  }
  if (msg.images?.length) entry.images = msg.images
  if (msg.widget) entry.widget = msg.widget
  msgs.push(entry)
  schedulePersist(true)
  return entry
}
