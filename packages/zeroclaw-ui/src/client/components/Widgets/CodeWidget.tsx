import { useState } from 'react'
import { Copy, Check } from 'lucide-react'
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
    <div className="mt-2 rounded-lg bg-bg-base border border-fg-subtle/20 overflow-hidden">
      <div className="flex items-center justify-between px-3 py-1.5 bg-bg-raised border-b border-fg-subtle/15">
        {language && <span className="text-[10px] font-mono text-fg-subtle uppercase">{language}</span>}
        <button
          onClick={handleCopy}
          className="text-fg-muted hover:text-accent-cyan transition-colors flex items-center gap-1"
        >
          {copied ? <Check size={12} /> : <Copy size={12} />}
          <span className="text-[10px] font-mono">{copied ? 'Copied' : 'Copy'}</span>
        </button>
      </div>
      <pre className="p-3 overflow-x-auto text-sm font-mono text-fg-secondary">
        <code>{code}</code>
      </pre>
    </div>
  )
}
