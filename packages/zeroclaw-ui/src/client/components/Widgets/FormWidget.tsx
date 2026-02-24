import { useState } from 'react'
import type { WidgetData } from '../../lib/types'

export interface FormWidgetProps {
  widget: WidgetData
  onRespond: (id: string, widget: string, value: unknown, action: string) => void
}

interface FormField {
  name: string
  label?: string
  type?: string
  placeholder?: string
  required?: boolean
}

export function FormWidget({ widget, onRespond }: FormWidgetProps) {
  const fields = (widget.fields as FormField[]) || []
  const title = widget.title as string | undefined
  const [values, setValues] = useState<Record<string, string>>({})

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault()
    onRespond(widget.id, 'form', values, 'submit')
  }

  return (
    <form onSubmit={handleSubmit} className="mt-2 rounded-lg bg-bg-raised p-3 border border-fg-subtle/20">
      {title && <div className="font-medium text-sm text-fg-primary mb-2">{title}</div>}
      {fields.map((field) => (
        <div key={field.name} className="mb-2">
          {field.label && (
            <label className="block text-xs text-fg-muted mb-0.5">{field.label}</label>
          )}
          <input
            type={field.type || 'text'}
            placeholder={field.placeholder}
            required={field.required}
            value={values[field.name] || ''}
            onChange={(e) =>
              setValues((prev) => ({ ...prev, [field.name]: e.target.value }))
            }
            className="w-full rounded-lg bg-bg-surface px-3 py-1.5 text-sm text-fg-primary border border-fg-subtle/20 outline-none focus:border-accent-cyan focus:ring-1 focus:ring-accent-cyan/20 transition-colors"
          />
        </div>
      ))}
      <button
        type="submit"
        className="mt-1 px-3 py-1.5 text-sm rounded-lg bg-accent-cyan text-bg-base hover:bg-accent-green transition-colors"
      >
        Submit
      </button>
    </form>
  )
}
