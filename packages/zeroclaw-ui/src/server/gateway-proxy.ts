import type { WSContext, WSEvents } from 'hono/ws'
import crypto from 'crypto'

/**
 * Bridges browser WebSocket to the zeroclaw HTTP gateway.
 *
 * The zeroclaw gateway exposes POST /webhook with { message: string }
 * and returns { response: string, model: string }.
 *
 * This handler converts WS chat messages into HTTP POST calls
 * and sends the response back over the WebSocket.
 */
export function createGatewayProxy(gatewayUrl: string) {
  // gatewayUrl is e.g. "http://127.0.0.1:3000" — we POST to /webhook
  const webhookUrl = gatewayUrl.replace(/^ws/, 'http') + '/webhook'

  return function createHandlers(): WSEvents {
    let clientWs: WSContext | null = null

    return {
      onOpen(_event, ws) {
        clientWs = ws
      },

      async onMessage(event) {
        if (!clientWs) return

        const raw = typeof event.data === 'string' ? event.data : String(event.data)
        let parsed: { type: string; text?: string; images?: string[] }
        try {
          parsed = JSON.parse(raw)
        } catch {
          return
        }

        if (parsed.type !== 'chat' || !parsed.text?.trim()) return

        const runId = crypto.randomUUID()

        // Send streaming indicator so UI shows typing state
        try {
          clientWs.send(JSON.stringify({
            type: 'chat',
            state: 'streaming',
            runId,
            text: '',
          }))
        } catch { /* client gone */ }

        try {
          const resp = await fetch(webhookUrl, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ message: parsed.text }),
          })

          if (!resp.ok) {
            const errBody = await resp.text()
            clientWs.send(JSON.stringify({
              type: 'chat',
              state: 'final',
              runId,
              text: `Error: gateway returned ${resp.status}`,
              error: errBody,
            }))
            return
          }

          const body = await resp.json() as { response: string; model?: string }

          clientWs.send(JSON.stringify({
            type: 'chat',
            state: 'final',
            runId,
            text: body.response,
          }))
        } catch (err) {
          try {
            clientWs.send(JSON.stringify({
              type: 'chat',
              state: 'final',
              runId,
              text: `Error: ${(err as Error).message}`,
              error: (err as Error).message,
            }))
          } catch { /* client gone */ }
        }
      },

      onClose() {
        clientWs = null
      },
    }
  }
}
