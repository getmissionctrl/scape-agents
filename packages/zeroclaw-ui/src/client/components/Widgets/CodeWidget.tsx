import { useState } from 'react'
import type { WidgetData } from '../../lib/types'

export interface CodeWidgetProps {
  widget: WidgetData
}

export function CodeWidget({ widget }: CodeWidgetProps) {
  const [copied, setCopied] = useState(false)
  const code = (widget.code as string) || ''
  const language = (widget.language as string) || ''

  const handleCopy = async () => {
    try {
      await navigator.clipboard.writeText(code)
      setCopied(true)
      setTimeout(() => setCopied(false), 2000)
    } catch {
      // Clipboard API not available
    }
  }

  return (
    <div className="mt-2 rounded-lg bg-gray-900 border border-gray-700 overflow-hidden">
      <div className="flex items-center justify-between px-3 py-1.5 bg-gray-800 border-b border-gray-700">
        {language && <span className="text-xs text-gray-500">{language}</span>}
        <button
          onClick={handleCopy}
          className="text-xs text-gray-400 hover:text-gray-200 transition-colors"
        >
          {copied ? 'Copied!' : 'Copy'}
        </button>
      </div>
      <pre className="p-3 overflow-x-auto text-sm text-gray-200">
        <code>{code}</code>
      </pre>
    </div>
  )
}
