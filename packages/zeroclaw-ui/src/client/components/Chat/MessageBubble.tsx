import type { ChatMessage } from '../../lib/types'

export interface MessageBubbleProps {
  message: ChatMessage
}

export function MessageBubble({ message }: MessageBubbleProps) {
  const isUser = message.role === 'user'

  return (
    <div
      className={`flex ${isUser ? 'justify-end' : 'justify-start'} mb-3`}
      data-streaming={message.streaming ? 'true' : undefined}
    >
      <div
        className={`max-w-[80%] rounded-2xl px-4 py-2.5 text-sm leading-relaxed ${
          isUser
            ? 'bg-indigo-600 text-white rounded-br-md'
            : 'bg-gray-800 text-gray-100 rounded-bl-md'
        }`}
      >
        <div className="whitespace-pre-wrap break-words">{message.text}</div>
        {message.streaming && (
          <div className="flex gap-1 mt-1.5">
            <span className="w-1.5 h-1.5 rounded-full bg-gray-400 animate-pulse" />
            <span className="w-1.5 h-1.5 rounded-full bg-gray-400 animate-pulse [animation-delay:150ms]" />
            <span className="w-1.5 h-1.5 rounded-full bg-gray-400 animate-pulse [animation-delay:300ms]" />
          </div>
        )}
      </div>
    </div>
  )
}
