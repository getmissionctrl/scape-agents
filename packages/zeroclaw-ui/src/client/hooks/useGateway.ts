import { useEffect, useRef, useState, useCallback } from 'react'
import type { GatewayMessage } from '../lib/types'

export interface UseGatewayOptions {
  onMessage?: (msg: GatewayMessage) => void
}

export function useGateway(options?: UseGatewayOptions) {
  const [connected, setConnected] = useState(false)
  const wsRef = useRef<WebSocket | null>(null)
  const reconnectRef = useRef<ReturnType<typeof setTimeout> | null>(null)
  const attemptRef = useRef(0)
  const unmountedRef = useRef(false)
  const optionsRef = useRef(options)
  optionsRef.current = options

  const connect = useCallback(() => {
    if (unmountedRef.current) return

    const proto = location.protocol === 'https:' ? 'wss:' : 'ws:'
    const wsUrl = `${proto}//${location.host}/ui-api/gateway`

    const ws = new WebSocket(wsUrl)
    wsRef.current = ws

    ws.onopen = () => {
      setConnected(true)
      attemptRef.current = 0
    }

    ws.onmessage = (event) => {
      try {
        const data = JSON.parse(event.data) as GatewayMessage
        optionsRef.current?.onMessage?.(data)
      } catch {
        // Non-JSON message, ignore
      }
    }

    ws.onclose = () => {
      setConnected(false)
      if (unmountedRef.current) return
      const delay = Math.min(1000 * Math.pow(2, attemptRef.current), 30000)
      attemptRef.current += 1
      reconnectRef.current = setTimeout(connect, delay)
    }

    ws.onerror = () => {
      // onclose will handle reconnection
    }
  }, [])

  useEffect(() => {
    unmountedRef.current = false
    connect()

    return () => {
      unmountedRef.current = true
      if (reconnectRef.current) clearTimeout(reconnectRef.current)
      if (wsRef.current) {
        wsRef.current.onclose = null
        wsRef.current.close()
      }
    }
  }, [connect])

  const sendMessage = useCallback((text: string, images?: string[]) => {
    const ws = wsRef.current
    if (ws && ws.readyState === WebSocket.OPEN) {
      ws.send(JSON.stringify({ type: 'chat', text, images: images || [] }))
    }
  }, [])

  const sendWidgetResponse = useCallback(
    (id: string, widget: string, value: unknown, action = 'submit') => {
      const ws = wsRef.current
      if (ws && ws.readyState === WebSocket.OPEN) {
        ws.send(JSON.stringify({ type: 'widget_response', id, widget, value, action }))
      }
    },
    [],
  )

  return { connected, sendMessage, sendWidgetResponse }
}
