import { useEffect, useState } from 'react'
import { DesktopViewer } from '@scape/ui-components'
import type { ConnectionState } from '@scape/ui-components'

export interface DesktopModalProps {
  open: boolean
  onClose: () => void
}

function buildWsUrl(): string {
  const proto = location.protocol === 'https:' ? 'wss:' : 'ws:'
  return `${proto}//${location.host}/ws/vnc`
}

export function DesktopModal({ open, onClose }: DesktopModalProps) {
  const [connectionState, setConnectionState] = useState<ConnectionState>('disconnected')

  useEffect(() => {
    if (!open) return

    const handleEscape = (e: KeyboardEvent) => {
      if (e.key === 'Escape') onClose()
    }
    window.addEventListener('keydown', handleEscape)
    return () => window.removeEventListener('keydown', handleEscape)
  }, [open, onClose])

  if (!open) return null

  return (
    <div
      data-testid="desktop-modal"
      className="fixed inset-0 z-50 bg-gray-950 flex flex-col"
    >
      <div className="flex items-center justify-between px-4 py-2 bg-gray-900 border-b border-gray-800">
        <div className="flex items-center gap-3">
          <span className="text-sm font-medium text-gray-200">Desktop</span>
          <span className={`text-xs px-2 py-0.5 rounded-full ${
            connectionState === 'connected'
              ? 'bg-green-900 text-green-300'
              : connectionState === 'connecting'
              ? 'bg-yellow-900 text-yellow-300'
              : 'bg-red-900 text-red-300'
          }`}>
            {connectionState}
          </span>
        </div>
        <button
          onClick={onClose}
          aria-label="Close"
          className="text-gray-400 hover:text-white transition-colors text-lg px-2"
        >
          ✕
        </button>
      </div>
      <div className="flex-1 min-h-0">
        <DesktopViewer
          wsUrl={buildWsUrl()}
          visible={open}
          onConnectionChange={setConnectionState}
          className="w-full h-full"
        />
      </div>
    </div>
  )
}
