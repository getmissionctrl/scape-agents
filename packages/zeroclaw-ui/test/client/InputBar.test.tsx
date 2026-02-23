import { describe, it, expect, vi } from 'vitest'
import { render, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { InputBar } from '../../src/client/components/Chat/InputBar'

describe('InputBar', () => {
  it('renders a text input and send button', () => {
    render(<InputBar onSend={() => {}} />)
    expect(screen.getByPlaceholderText(/type a message/i)).toBeTruthy()
    expect(screen.getByRole('button', { name: /send/i })).toBeTruthy()
  })

  it('calls onSend with input text on submit', async () => {
    const onSend = vi.fn()
    const user = userEvent.setup()
    render(<InputBar onSend={onSend} />)

    const input = screen.getByPlaceholderText(/type a message/i)
    await user.type(input, 'hello world')
    await user.click(screen.getByRole('button', { name: /send/i }))

    expect(onSend).toHaveBeenCalledWith('hello world')
  })

  it('clears input after send', async () => {
    const user = userEvent.setup()
    render(<InputBar onSend={() => {}} />)

    const input = screen.getByPlaceholderText(/type a message/i) as HTMLTextAreaElement
    await user.type(input, 'hello')
    await user.click(screen.getByRole('button', { name: /send/i }))

    expect(input.value).toBe('')
  })

  it('does not send empty messages', async () => {
    const onSend = vi.fn()
    const user = userEvent.setup()
    render(<InputBar onSend={onSend} />)
    await user.click(screen.getByRole('button', { name: /send/i }))
    expect(onSend).not.toHaveBeenCalled()
  })
})
