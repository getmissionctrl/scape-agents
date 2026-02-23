import type { WidgetData } from '../../lib/types'

export interface ConfirmWidgetProps {
  widget: WidgetData
  onRespond: (id: string, widget: string, value: unknown, action: string) => void
}

export function ConfirmWidget({ widget, onRespond }: ConfirmWidgetProps) {
  const title = widget.title as string | undefined
  const message = widget.message as string | undefined

  return (
    <div className="mt-2 rounded-lg bg-gray-800 p-3 border border-gray-700">
      {title && <div className="font-medium text-sm text-gray-100 mb-1">{title}</div>}
      {message && <div className="text-xs text-gray-400 mb-2">{message}</div>}
      <div className="flex gap-2">
        <button
          onClick={() => onRespond(widget.id, 'confirm', true, 'confirm')}
          className="px-3 py-1.5 text-sm rounded-lg bg-indigo-600 text-white hover:bg-indigo-500 transition-colors"
        >
          Confirm
        </button>
        <button
          onClick={() => onRespond(widget.id, 'confirm', false, 'cancel')}
          className="px-3 py-1.5 text-sm rounded-lg bg-gray-700 text-gray-200 hover:bg-gray-600 transition-colors"
        >
          Cancel
        </button>
      </div>
    </div>
  )
}
