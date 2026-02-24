import { useState, useCallback, useEffect } from 'react'
import { flushSync } from 'react-dom'
import { uuid } from '../lib/types'
import type { ChatMessage, GatewayMessage } from '../lib/types'

export function useMessages() {
  const [messages, setMessages] = useState<ChatMessage[]>([])
  const [loading, setLoading] = useState(true)
  // Load history from server on mount
  useEffect(() => {
    fetch('/api/messages')
      .then((r) => r.json())
      .then((msgs: ChatMessage[]) => {
        setMessages(msgs)
        setLoading(false)
      })
      .catch(() => setLoading(false))
  }, [])

  const addUserMessage = useCallback((text: string) => {
    const msg: ChatMessage = {
      id: uuid(),
      role: 'user',
      text,
      timestamp: new Date().toISOString(),
    }
    setMessages((prev) => [...prev, msg])

    // Persist to server
    fetch('/api/messages', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ role: 'user', text }),
    }).catch(() => {})

    return msg
  }, [])

  const handleGatewayMessage = useCallback((data: GatewayMessage) => {
    if (data.type !== 'chat') return

    const update = (prev: ChatMessage[]): ChatMessage[] => {
      const existing = data.runId
        ? prev.findIndex((m) => m.id === data.runId)
        : prev.findIndex((m) => m.role === 'bot' && m.streaming)

      if (existing !== -1 && data.state === 'streaming') {
        const updated = [...prev]
        updated[existing] = {
          ...updated[existing],
          text: data.text || '',
        }
        return updated
      }

      if (existing !== -1 && data.state === 'final') {
        const updated = [...prev]
        updated[existing] = {
          ...updated[existing],
          text: data.text || '',
          streaming: false,
        }
        fetch('/api/messages', {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ role: 'bot', text: data.text || '' }),
        }).catch(() => {})
        return updated
      }

      if (existing !== -1 && data.state === 'cancelled') {
        return prev.filter((_, i) => i !== existing)
      }

      if (data.state === 'streaming') {
        return [
          ...prev,
          {
            id: data.runId || uuid(),
            role: 'bot' as const,
            text: data.text || '',
            timestamp: new Date().toISOString(),
            streaming: true,
          },
        ]
      }

      if (data.state === 'final') {
        const msg: ChatMessage = {
          id: data.runId || uuid(),
          role: 'bot',
          text: data.text || '',
          timestamp: new Date().toISOString(),
        }
        fetch('/api/messages', {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ role: 'bot', text: data.text || '' }),
        }).catch(() => {})
        return [...prev, msg]
      }

      return prev
    }

    // Force synchronous render for streaming so each chunk paints immediately
    if (data.state === 'streaming' || data.state === 'final') {
      flushSync(() => setMessages(update))
    } else {
      setMessages(update)
    }
  }, [])

  return { messages, loading, addUserMessage, handleGatewayMessage }
}
