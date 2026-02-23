import { describe, it, expect, vi } from 'vitest'
import { render, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { DesktopModal } from '../../src/client/components/Desktop/DesktopModal'

vi.mock('@scape/ui-components', () => ({
  DesktopViewer: ({ wsUrl }: { wsUrl: string }) => (
    <div data-testid="desktop-viewer">{wsUrl}</div>
  ),
}))

describe('DesktopModal', () => {
  it('renders nothing when not open', () => {
    const { container } = render(<DesktopModal open={false} onClose={() => {}} />)
    expect(container.querySelector('[data-testid="desktop-modal"]')).toBeNull()
  })

  it('renders fullscreen overlay when open', () => {
    render(<DesktopModal open={true} onClose={() => {}} />)
    expect(screen.getByTestId('desktop-modal')).toBeTruthy()
  })

  it('calls onClose when close button clicked', async () => {
    const onClose = vi.fn()
    const user = userEvent.setup()
    render(<DesktopModal open={true} onClose={onClose} />)
    await user.click(screen.getByRole('button', { name: /close/i }))
    expect(onClose).toHaveBeenCalled()
  })

  it('shows the desktop viewer', () => {
    render(<DesktopModal open={true} onClose={() => {}} />)
    expect(screen.getByTestId('desktop-viewer')).toBeTruthy()
  })
})
