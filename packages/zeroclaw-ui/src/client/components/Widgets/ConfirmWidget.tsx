import type { WidgetData } from '../../lib/types'

export interface ConfirmWidgetProps {
  widget: WidgetData
  onRespond: (id: string, widget: string, value: unknown, action: string) => void
}

export function ConfirmWidget({ widget, onRespond }: ConfirmWidgetProps) {
  const title = widget.title as string | undefined
  const message = widget.message as string | undefined

  return (
    <div className="mt-2 rounded-lg bg-bg-raised p-3 border border-fg-subtle/20">
      {title && <div className="font-medium text-sm text-fg-primary mb-1">{title}</div>}
      {message && <div className="text-xs text-fg-muted mb-2">{message}</div>}
      <div className="flex gap-2">
        <button
          onClick={() => onRespond(widget.id, 'confirm', true, 'confirm')}
          className="px-3 py-1.5 text-sm rounded-lg bg-accent-cyan text-bg-base hover:bg-accent-green transition-colors"
        >
          Confirm
        </button>
        <button
          onClick={() => onRespond(widget.id, 'confirm', false, 'cancel')}
          className="px-3 py-1.5 text-sm rounded-lg bg-bg-elevated text-fg-secondary border border-fg-subtle/20 hover:bg-bg-surface transition-colors"
        >
          Cancel
        </button>
      </div>
    </div>
  )
}
