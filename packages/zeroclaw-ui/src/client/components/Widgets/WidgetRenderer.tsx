import type { WidgetData } from '../../lib/types'
import { ButtonsWidget } from './ButtonsWidget'
import { ConfirmWidget } from './ConfirmWidget'
import { ProgressWidget } from './ProgressWidget'
import { CodeWidget } from './CodeWidget'
import { FormWidget } from './FormWidget'

export interface WidgetRendererProps {
  widget: WidgetData
  onRespond: (id: string, widget: string, value: unknown, action: string) => void
}

export function WidgetRenderer({ widget, onRespond }: WidgetRendererProps) {
  switch (widget.widget) {
    case 'buttons':
      return <ButtonsWidget widget={widget} onRespond={onRespond} />
    case 'confirm':
      return <ConfirmWidget widget={widget} onRespond={onRespond} />
    case 'progress':
      return <ProgressWidget widget={widget} />
    case 'code':
      return <CodeWidget widget={widget} />
    case 'form':
      return <FormWidget widget={widget} onRespond={onRespond} />
    default:
      return null
  }
}
