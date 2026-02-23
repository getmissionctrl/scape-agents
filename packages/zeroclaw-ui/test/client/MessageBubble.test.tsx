import { describe, it, expect } from 'vitest'
import { render, screen } from '@testing-library/react'
import { MessageBubble } from '../../src/client/components/Chat/MessageBubble'

describe('MessageBubble', () => {
  it('renders user message with correct alignment', () => {
    render(
      <MessageBubble
        message={{ id: '1', role: 'user', text: 'Hello', timestamp: '' }}
      />
    )
    expect(screen.getByText('Hello')).toBeTruthy()
  })

  it('renders bot message', () => {
    render(
      <MessageBubble
        message={{ id: '2', role: 'bot', text: 'Hi there', timestamp: '' }}
      />
    )
    expect(screen.getByText('Hi there')).toBeTruthy()
  })

  it('shows streaming indicator when streaming', () => {
    const { container } = render(
      <MessageBubble
        message={{ id: '3', role: 'bot', text: 'typing...', timestamp: '', streaming: true }}
      />
    )
    expect(container.querySelector('[data-streaming="true"]')).toBeTruthy()
  })
})
