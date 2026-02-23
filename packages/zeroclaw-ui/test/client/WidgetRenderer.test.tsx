import { describe, it, expect, vi } from 'vitest'
import { render, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { WidgetRenderer } from '../../src/client/components/Widgets/WidgetRenderer'

describe('WidgetRenderer', () => {
  it('renders buttons widget', () => {
    render(
      <WidgetRenderer
        widget={{ widget: 'buttons', id: 'b1', label: 'Pick:', options: ['A', 'B'] }}
        onRespond={() => {}}
      />
    )
    expect(screen.getByText('Pick:')).toBeTruthy()
    expect(screen.getByText('A')).toBeTruthy()
    expect(screen.getByText('B')).toBeTruthy()
  })

  it('calls onRespond when button clicked', async () => {
    const onRespond = vi.fn()
    const user = userEvent.setup()
    render(
      <WidgetRenderer
        widget={{ widget: 'buttons', id: 'b1', options: ['X', 'Y'] }}
        onRespond={onRespond}
      />
    )
    await user.click(screen.getByText('X'))
    expect(onRespond).toHaveBeenCalledWith('b1', 'buttons', 'X', 'submit')
  })

  it('renders confirm widget', () => {
    render(
      <WidgetRenderer
        widget={{ widget: 'confirm', id: 'c1', title: 'Delete?', message: 'Are you sure?' }}
        onRespond={() => {}}
      />
    )
    expect(screen.getByText('Delete?')).toBeTruthy()
    expect(screen.getByText('Are you sure?')).toBeTruthy()
  })

  it('renders progress widget', () => {
    render(
      <WidgetRenderer
        widget={{ widget: 'progress', id: 'p1', label: 'Loading...', value: 50 }}
        onRespond={() => {}}
      />
    )
    expect(screen.getByText('Loading...')).toBeTruthy()
  })

  it('renders code widget with copy button', () => {
    render(
      <WidgetRenderer
        widget={{ widget: 'code', id: 'cd1', code: 'console.log("hi")', language: 'js' }}
        onRespond={() => {}}
      />
    )
    expect(screen.getByText('console.log("hi")')).toBeTruthy()
  })

  it('returns null for unknown widget type', () => {
    const { container } = render(
      <WidgetRenderer
        widget={{ widget: 'unknown', id: 'u1' }}
        onRespond={() => {}}
      />
    )
    expect(container.innerHTML).toBe('')
  })
})
