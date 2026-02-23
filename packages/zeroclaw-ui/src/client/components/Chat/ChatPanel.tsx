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
          <div className="text-center text-gray-500 py-8">Loading messages...</div>
        )}
        {!loading && messages.length === 0 && (
          <div className="text-center text-gray-500 py-8">
            No messages yet. Start a conversation!
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
