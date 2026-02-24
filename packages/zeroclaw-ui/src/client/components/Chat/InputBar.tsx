import { useState, useRef, type KeyboardEvent } from 'react'
import { Send } from 'lucide-react'

export interface InputBarProps {
  onSend: (text: string) => void
  disabled?: boolean
}

export function InputBar({ onSend, disabled }: InputBarProps) {
  const [text, setText] = useState('')
  const textareaRef = useRef<HTMLTextAreaElement>(null)

  const handleSend = () => {
    const trimmed = text.trim()
    if (!trimmed) return
    onSend(trimmed)
    setText('')
    textareaRef.current?.focus()
  }

  const handleKeyDown = (e: KeyboardEvent<HTMLTextAreaElement>) => {
    if (e.key === 'Enter' && !e.shiftKey) {
      e.preventDefault()
      handleSend()
    }
  }

  return (
    <div className="flex items-end gap-2 p-3 border-t border-fg-subtle/15 bg-bg-base">
      <textarea
        ref={textareaRef}
        value={text}
        onChange={(e) => setText(e.target.value)}
        onKeyDown={handleKeyDown}
        placeholder="Type a message..."
        disabled={disabled}
        rows={1}
        className="flex-1 resize-none rounded-lg bg-bg-surface px-4 py-2.5 text-sm text-fg-primary placeholder-fg-muted outline-none border border-fg-subtle/20 focus:border-accent-cyan focus:ring-1 focus:ring-accent-cyan/20 transition-colors"
      />
      <button
        onClick={handleSend}
        disabled={disabled}
        aria-label="Send"
        className="rounded-lg bg-accent-cyan px-3 py-2.5 text-bg-base hover:bg-accent-green disabled:opacity-50 disabled:cursor-not-allowed transition-colors shadow-glow-cyan"
      >
        <Send size={16} />
      </button>
    </div>
  )
}
