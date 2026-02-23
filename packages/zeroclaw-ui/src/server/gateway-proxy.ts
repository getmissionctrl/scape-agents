import { WebSocket as NodeWebSocket } from 'ws'
import type { WSContext, WSEvents } from 'hono/ws'

/**
 * Creates a WebSocket proxy that returns hono WSEvents handlers
 * to forward frames between a client and the zeroclaw gateway.
 */
export function createGatewayProxy(gatewayUrl: string) {
  return function createHandlers(): WSEvents<NodeWebSocket> {
    let upstream: NodeWebSocket | null = null
    let clientWs: WSContext | null = null
    const buffer: string[] = []

    return {
      onOpen(_event, ws) {
        clientWs = ws
        upstream = new NodeWebSocket(gatewayUrl)

        upstream.on('open', () => {
          for (const msg of buffer) {
            upstream!.send(msg)
          }
          buffer.length = 0
        })

        upstream.on('message', (data) => {
          try {
            if (clientWs && clientWs.readyState === 1) {
              clientWs.send(typeof data === 'string' ? data : data.toString())
            }
          } catch {
            // Client disconnected
          }
        })

        upstream.on('close', () => {
          try { clientWs?.close() } catch { /* ignore */ }
        })

        upstream.on('error', (err) => {
          console.error('[gateway-proxy] upstream error:', err.message)
          try { clientWs?.close() } catch { /* ignore */ }
        })
      },

      onMessage(event) {
        const data = typeof event.data === 'string' ? event.data : String(event.data)
        if (upstream && upstream.readyState === NodeWebSocket.OPEN) {
          upstream.send(data)
        } else {
          buffer.push(data)
        }
      },

      onClose() {
        if (upstream && upstream.readyState === NodeWebSocket.OPEN) {
          upstream.close()
        }
        upstream = null
        clientWs = null
      },
    }
  }
}
