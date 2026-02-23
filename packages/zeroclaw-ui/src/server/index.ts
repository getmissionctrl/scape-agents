import { Hono } from 'hono'
import { serve } from '@hono/node-server'
import { createNodeWebSocket } from '@hono/node-ws'
import { serveStatic } from '@hono/node-server/serve-static'
import path from 'path'
import fs from 'fs'
import { config, publicConfig } from './config'
import { getMessages, saveMessage, flushSync } from './store'
import { generateTTS } from './tts'
import { createGatewayProxy } from './gateway-proxy'

// Ensure data directory exists
fs.mkdirSync(config.dataDir, { recursive: true })

const app = new Hono()
const { injectWebSocket, upgradeWebSocket } = createNodeWebSocket({ app })

// --- REST API ---

app.get('/api/config', (c) => c.json(publicConfig))

app.get('/api/messages', (c) => {
  const limit = parseInt(c.req.query('limit') || '200', 10)
  return c.json(getMessages(limit))
})

app.post('/api/messages', async (c) => {
  const body = await c.req.json<{ role: 'user' | 'bot'; text: string }>()
  const msg = saveMessage(body)
  return c.json(msg, 201)
})

app.post('/api/tts', async (c) => {
  if (!config.enableVoice) {
    return c.json({ error: 'Voice disabled' }, 403)
  }
  const { text } = await c.req.json<{ text: string }>()
  try {
    const audio = await generateTTS(text)
    return new Response(new Uint8Array(audio), {
      headers: { 'Content-Type': 'audio/mpeg' },
    })
  } catch (err) {
    return c.json({ error: (err as Error).message }, 500)
  }
})

// --- WebSocket gateway proxy ---

const gatewayHandler = createGatewayProxy(config.gatewayUrl)

app.get(
  '/api/gateway',
  upgradeWebSocket(() => gatewayHandler()),
)

// --- Static files (built SPA) ---

const distDir = path.join(import.meta.dirname, '../../dist/client')
if (fs.existsSync(distDir)) {
  app.use('/*', serveStatic({ root: distDir }))
  // SPA fallback
  app.get('*', (c) => {
    const indexPath = path.join(distDir, 'index.html')
    const html = fs.readFileSync(indexPath, 'utf8')
    return c.html(html)
  })
}

// --- Start ---

const server = serve({ fetch: app.fetch, port: config.port }, (info) => {
  console.log(`[zeroclaw-ui] Server running on :${info.port}`)
})

injectWebSocket(server)

// Graceful shutdown
process.on('SIGTERM', () => {
  console.log('[zeroclaw-ui] Shutting down...')
  flushSync()
  process.exit(0)
})

process.on('SIGINT', () => {
  console.log('[zeroclaw-ui] Shutting down...')
  flushSync()
  process.exit(0)
})
