import type { WSContext, WSEvents } from 'hono/ws'
import WebSocket from 'ws'

/**
 * Proxies browser WebSocket frames to the zeroclaw WebChannel.
 *
 * The WebChannel listens on ws://127.0.0.1:5100/ws and speaks the same
 * JSON protocol the browser client uses:
 *
 *   Client → WebChannel: {"type":"chat","text":"...","images":[]}
 *   WebChannel → Client: {"type":"chat","state":"streaming"|"final","runId":"...","text":"..."}
 *
 * This proxy is a transparent WS-to-WS bridge — it forwards text frames
 * in both directions without transformation.
 */
export function createGatewayProxy(gatewayUrl: string) {
  // Ensure the URL has a /ws path for the WebChannel endpoint
  const wsUrl = gatewayUrl.replace(/\/$/, '') + '/ws'

  return function createHandlers(): WSEvents {
    let clientWs: WSContext | null = null
    let upstream: WebSocket | null = null
    const buffer: string[] = []

    return {
      onOpen(_event, ws) {
        clientWs = ws

        upstream = new WebSocket(wsUrl)

        upstream.on('open', () => {
          // Flush any messages buffered before upstream connected
          for (const msg of buffer) {
            upstream!.send(msg)
          }
          buffer.length = 0
        })

        upstream.on('message', (data) => {
          try {
            const text = typeof data === 'string' ? data : data.toString()
            clientWs?.send(text)
          } catch {
            // Client disconnected
          }
        })

        upstream.on('close', () => {
          try { clientWs?.close() } catch { /* ignore */ }
          upstream = null
        })

        upstream.on('error', (err) => {
          console.error('[gateway-proxy] upstream error:', err.message)
          try { clientWs?.close() } catch { /* ignore */ }
        })
      },

      onMessage(event) {
        const text = typeof event.data === 'string' ? event.data : String(event.data)

        if (upstream && upstream.readyState === WebSocket.OPEN) {
          upstream.send(text)
        } else {
          // Buffer until upstream connects
          buffer.push(text)
        }
      },

      onClose() {
        clientWs = null
        if (upstream && upstream.readyState === WebSocket.OPEN) {
          upstream.close()
        }
        upstream = null
      },
    }
  }
}
