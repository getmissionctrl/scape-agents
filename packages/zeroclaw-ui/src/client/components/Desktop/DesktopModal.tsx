import { useEffect, useState } from 'react'
import { X } from 'lucide-react'
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

const statusStyles: Record<ConnectionState, string> = {
  connected: 'bg-accent-green/15 text-accent-green border border-accent-green/30',
  connecting: 'bg-accent-yellow/15 text-accent-yellow border border-accent-yellow/30',
  disconnected: 'bg-accent-red/15 text-accent-red border border-accent-red/30',
  failed: 'bg-accent-red/15 text-accent-red border border-accent-red/30',
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
      className="fixed inset-0 z-50 bg-bg-base flex flex-col"
    >
      <div className="flex items-center justify-between px-4 py-2 bg-bg-raised border-b border-fg-subtle/20">
        <div className="flex items-center gap-3">
          <span className="text-sm font-medium text-fg-secondary">Desktop</span>
          <span className={`text-[10px] font-mono px-2 py-0.5 rounded-full ${statusStyles[connectionState]}`}>
            {connectionState}
          </span>
        </div>
        <button
          onClick={onClose}
          aria-label="Close"
          className="text-fg-muted hover:text-fg-primary transition-colors p-1 rounded hover:bg-bg-elevated"
        >
          <X size={16} />
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
