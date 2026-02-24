import type { ReactNode } from 'react'
import { Sidebar, type ViewMode } from './Sidebar'

export interface AppShellProps {
  activeView: ViewMode
  onNavigate: (view: ViewMode) => void
  connected: boolean
  children: ReactNode
}

export function AppShell({ activeView, onNavigate, connected, children }: AppShellProps) {
  return (
    <div className="flex h-screen w-screen bg-bg-base text-fg-primary">
      <Sidebar active={activeView} onNavigate={onNavigate} connected={connected} />
      <main className="flex-1 min-w-0">{children}</main>
    </div>
  )
}
