import { describe, it, expect, vi } from 'vitest'
import { render, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { TerminalModal } from '../../src/client/components/Terminal/TerminalModal'

vi.mock('@scape/ui-components', () => ({
  TerminalWidget: ({ wsUrl }: { wsUrl: string }) => (
    <div data-testid="terminal-widget">{wsUrl}</div>
  ),
}))

describe('TerminalModal', () => {
  it('renders nothing when not open', () => {
    const { container } = render(<TerminalModal open={false} onClose={() => {}} />)
    expect(container.querySelector('[data-testid="terminal-modal"]')).toBeNull()
  })

  it('renders fullscreen overlay when open', () => {
    render(<TerminalModal open={true} onClose={() => {}} />)
    expect(screen.getByTestId('terminal-modal')).toBeTruthy()
  })

  it('calls onClose when close button clicked', async () => {
    const onClose = vi.fn()
    const user = userEvent.setup()
    render(<TerminalModal open={true} onClose={onClose} />)
    await user.click(screen.getByRole('button', { name: /close/i }))
    expect(onClose).toHaveBeenCalled()
  })

  it('shows the terminal widget', () => {
    render(<TerminalModal open={true} onClose={() => {}} />)
    expect(screen.getByTestId('terminal-widget')).toBeTruthy()
  })
})
