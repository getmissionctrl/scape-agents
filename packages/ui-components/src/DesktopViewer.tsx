import { useEffect, useRef, useCallback } from 'react'
import RFB from '@novnc/novnc/core/rfb'
import type { ConnectionState } from './types'

export interface DesktopViewerProps {
  wsUrl: string
  visible?: boolean
  className?: string
  onConnectionChange?: (state: ConnectionState) => void
}

const MAX_RECONNECT_DELAY = 30_000
const INITIAL_RECONNECT_DELAY = 2_000

export function DesktopViewer({
  wsUrl,
  visible = true,
  className,
  onConnectionChange,
}: DesktopViewerProps) {
  const containerRef = useRef<HTMLDivElement>(null)
  const rfbRef = useRef<InstanceType<typeof RFB> | null>(null)
  const reconnectTimeoutRef = useRef<ReturnType<typeof setTimeout> | null>(null)
  const reconnectAttemptRef = useRef(0)
  const unmountedRef = useRef(false)

  const connect = useCallback(() => {
    if (!wsUrl || !containerRef.current || unmountedRef.current) return

    if (rfbRef.current) {
      rfbRef.current.disconnect()
      rfbRef.current = null
    }

    onConnectionChange?.('connecting')

    try {
      const rfb = new RFB(containerRef.current, wsUrl, {
        wsProtocols: ['binary'],
      })

      rfb.scaleViewport = true
      rfb.resizeSession = false
      rfb.clipViewport = false
      rfb.showDotCursor = true
      rfb.qualityLevel = 6
      rfb.compressionLevel = 2

      rfb.addEventListener('connect', () => {
        onConnectionChange?.('connected')
        reconnectAttemptRef.current = 0
      })

      rfb.addEventListener('disconnect', (e: CustomEvent) => {
        if (unmountedRef.current) return
        const clean = e.detail?.clean ?? false
        if (!clean) {
          onConnectionChange?.('disconnected')
          const attempt = reconnectAttemptRef.current
          const delay = Math.min(
            INITIAL_RECONNECT_DELAY * Math.pow(2, attempt),
            MAX_RECONNECT_DELAY,
          )
          reconnectAttemptRef.current = attempt + 1
          reconnectTimeoutRef.current = setTimeout(() => {
            if (!unmountedRef.current) connect()
          }, delay)
        } else {
          onConnectionChange?.('disconnected')
        }
      })

      rfb.addEventListener('securityfailure', () => {
        onConnectionChange?.('failed')
      })

      rfbRef.current = rfb
    } catch (err) {
      console.error('Failed to create RFB connection:', err)
      onConnectionChange?.('failed')
    }
  }, [wsUrl, onConnectionChange])

  useEffect(() => {
    if (!wsUrl) return
    unmountedRef.current = false
    connect()

    return () => {
      unmountedRef.current = true
      if (reconnectTimeoutRef.current) {
        clearTimeout(reconnectTimeoutRef.current)
      }
      if (rfbRef.current) {
        rfbRef.current.disconnect()
        rfbRef.current = null
      }
    }
  }, [wsUrl, connect])

  useEffect(() => {
    if (visible && rfbRef.current) {
      requestAnimationFrame(() => {
        if (rfbRef.current) {
          rfbRef.current.scaleViewport = true
        }
      })
    }
  }, [visible])

  return (
    <div
      ref={containerRef}
      data-testid="desktop-container"
      className={className}
      style={{ width: '100%', height: '100%', background: '#1A1B26' }}
    />
  )
}
