import type { WidgetData } from '../../lib/types'

export interface ProgressWidgetProps {
  widget: WidgetData
}

export function ProgressWidget({ widget }: ProgressWidgetProps) {
  const label = widget.label as string | undefined
  const value = (widget.value as number) || 0

  return (
    <div className="mt-2">
      {label && <div className="text-xs text-gray-400 mb-1">{label}</div>}
      <div className="w-full h-2 bg-gray-700 rounded-full overflow-hidden">
        <div
          className="h-full bg-indigo-500 rounded-full transition-all duration-300"
          style={{ width: `${Math.min(100, Math.max(0, value))}%` }}
        />
      </div>
      <div className="text-xs text-gray-500 mt-0.5 text-right">{value}%</div>
    </div>
  )
}
