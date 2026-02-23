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
    <form onSubmit={handleSubmit} className="mt-2 rounded-lg bg-gray-800 p-3 border border-gray-700">
      {title && <div className="font-medium text-sm text-gray-100 mb-2">{title}</div>}
      {fields.map((field) => (
        <div key={field.name} className="mb-2">
          {field.label && (
            <label className="block text-xs text-gray-400 mb-0.5">{field.label}</label>
          )}
          <input
            type={field.type || 'text'}
            placeholder={field.placeholder}
            required={field.required}
            value={values[field.name] || ''}
            onChange={(e) =>
              setValues((prev) => ({ ...prev, [field.name]: e.target.value }))
            }
            className="w-full rounded-lg bg-gray-900 px-3 py-1.5 text-sm text-gray-200 border border-gray-700 outline-none focus:ring-1 focus:ring-indigo-500"
          />
        </div>
      ))}
      <button
        type="submit"
        className="mt-1 px-3 py-1.5 text-sm rounded-lg bg-indigo-600 text-white hover:bg-indigo-500 transition-colors"
      >
        Submit
      </button>
    </form>
  )
}
