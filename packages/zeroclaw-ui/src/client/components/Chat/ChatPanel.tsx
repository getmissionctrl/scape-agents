import { useEffect, useRef } from 'react'
import { MessageBubble } from './MessageBubble'
import { InputBar } from './InputBar'
import type { ChatMessage } from '../../lib/types'

export interface ChatPanelProps {
  messages: ChatMessage[]
  loading: boolean
  onSend: (text: string) => void
  disabled?: boolean
}

export function ChatPanel({ messages, loading, onSend, disabled }: ChatPanelProps) {
  const scrollRef = useRef<HTMLDivElement>(null)

  useEffect(() => {
    const el = scrollRef.current
    if (el) {
      el.scrollTop = el.scrollHeight
    }
  }, [messages])

  return (
    <div className="flex flex-col h-full">
      <div ref={scrollRef} className="flex-1 overflow-y-auto px-4 py-3">
        {loading && (
          <div className="text-center text-fg-muted py-8 text-sm">Loading messages...</div>
        )}
        {!loading && messages.length === 0 && (
          <div className="flex flex-col items-center justify-center h-full text-fg-muted">
            <div className="w-3 h-3 rounded-full bg-accent-cyan shadow-glow-cyan mb-4" />
            <div className="text-sm">Start a conversation</div>
          </div>
        )}
        {messages.map((msg) => (
          <MessageBubble key={msg.id} message={msg} />
        ))}
      </div>
      <InputBar onSend={onSend} disabled={disabled} />
    </div>
  )
}
