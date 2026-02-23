import { useState, useCallback, useEffect } from 'react'
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
      id: crypto.randomUUID(),
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

    setMessages((prev) => {
      const existing = prev.findIndex(
        (m) => m.role === 'bot' && m.streaming && data.runId,
      )

      if (existing !== -1 && data.state === 'streaming') {
        // Update existing streaming message
        const updated = [...prev]
        updated[existing] = {
          ...updated[existing],
          text: data.text || '',
        }
        return updated
      }

      if (existing !== -1 && data.state === 'final') {
        // Finalize streaming message
        const updated = [...prev]
        updated[existing] = {
          ...updated[existing],
          text: data.text || '',
          streaming: false,
        }
        // Persist final message
        fetch('/api/messages', {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ role: 'bot', text: data.text || '' }),
        }).catch(() => {})
        return updated
      }

      if (existing !== -1 && data.state === 'cancelled') {
        // Remove cancelled streaming message
        return prev.filter((_, i) => i !== existing)
      }

      if (data.state === 'streaming') {
        // New streaming message
        return [
          ...prev,
          {
            id: data.runId || crypto.randomUUID(),
            role: 'bot' as const,
            text: data.text || '',
            timestamp: new Date().toISOString(),
            streaming: true,
          },
        ]
      }

      if (data.state === 'final') {
        // Single-shot final message (no prior streaming)
        const msg: ChatMessage = {
          id: data.runId || crypto.randomUUID(),
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
    })
  }, [])

  return { messages, loading, addUserMessage, handleGatewayMessage }
}
