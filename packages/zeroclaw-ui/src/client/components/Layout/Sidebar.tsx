import { MessageSquare, Monitor, TerminalSquare } from 'lucide-react'
import type { ReactNode } from 'react'

export type ViewMode = 'chat' | 'desktop' | 'terminal'

export interface SidebarProps {
  active: ViewMode
  onNavigate: (view: ViewMode) => void
  connected: boolean
}

const navItems: { id: ViewMode; label: string; icon: ReactNode }[] = [
  { id: 'chat', label: 'Chat', icon: <MessageSquare size={18} /> },
  { id: 'desktop', label: 'Desktop', icon: <Monitor size={18} /> },
  { id: 'terminal', label: 'Terminal', icon: <TerminalSquare size={18} /> },
]

export function Sidebar({ active, onNavigate, connected }: SidebarProps) {
  return (
    <nav className="flex flex-col w-14 bg-bg-raised border-r border-fg-subtle/20 items-center py-4 gap-1.5">
      <div className="mb-3 flex items-center gap-1.5" title="ZeroClaw">
        <span className="w-2 h-2 rounded-full bg-accent-cyan shadow-glow-cyan" />
        <span className="text-[10px] font-mono font-medium text-fg-secondary tracking-wider">ZC</span>
      </div>
      {navItems.map((item) => (
        <button
          key={item.id}
          onClick={() => onNavigate(item.id)}
          title={item.label}
          className={`w-10 h-10 rounded-lg flex items-center justify-center transition-colors ${
            active === item.id
              ? 'bg-accent-cyan/10 text-accent-cyan'
              : 'text-fg-muted hover:bg-bg-elevated hover:text-fg-primary'
          }`}
        >
          {item.icon}
        </button>
      ))}
      <div className="mt-auto">
        <div
          title={connected ? 'Connected' : 'Disconnected'}
          className={`w-2.5 h-2.5 rounded-full transition-colors ${
            connected
              ? 'bg-accent-green shadow-glow-green'
              : 'bg-accent-red'
          }`}
        />
      </div>
    </nav>
  )
}
