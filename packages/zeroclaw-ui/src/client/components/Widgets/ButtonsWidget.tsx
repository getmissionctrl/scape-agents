import type { WidgetData } from '../../lib/types'

export interface ButtonsWidgetProps {
  widget: WidgetData
  onRespond: (id: string, widget: string, value: unknown, action: string) => void
}

export function ButtonsWidget({ widget, onRespond }: ButtonsWidgetProps) {
  const options = (widget.options as string[]) || []
  const label = widget.label as string | undefined

  return (
    <div className="mt-2">
      {label && <div className="text-xs text-gray-400 mb-1.5">{label}</div>}
      <div className="flex flex-wrap gap-2">
        {options.map((opt) => (
          <button
            key={opt}
            onClick={() => onRespond(widget.id, 'buttons', opt, 'submit')}
            className="px-3 py-1.5 text-sm rounded-lg bg-gray-700 text-gray-200 hover:bg-indigo-600 hover:text-white transition-colors"
          >
            {opt}
          </button>
        ))}
      </div>
    </div>
  )
}
