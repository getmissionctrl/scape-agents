export type ViewMode = 'chat' | 'desktop' | 'terminal'

export interface SidebarProps {
  active: ViewMode
  onNavigate: (view: ViewMode) => void
  connected: boolean
}

const navItems: { id: ViewMode; label: string; icon: string }[] = [
  { id: 'chat', label: 'Chat', icon: '💬' },
  { id: 'desktop', label: 'Desktop', icon: '🖥' },
  { id: 'terminal', label: 'Terminal', icon: '⌨' },
]

export function Sidebar({ active, onNavigate, connected }: SidebarProps) {
  return (
    <nav className="flex flex-col w-16 bg-gray-900 border-r border-gray-800 items-center py-4 gap-2">
      {navItems.map((item) => (
        <button
          key={item.id}
          onClick={() => onNavigate(item.id)}
          title={item.label}
          className={`w-11 h-11 rounded-xl flex items-center justify-center text-lg transition-colors ${
            active === item.id
              ? 'bg-indigo-600 text-white'
              : 'text-gray-400 hover:bg-gray-800 hover:text-gray-200'
          }`}
        >
          {item.icon}
        </button>
      ))}
      <div className="mt-auto">
        <div
          title={connected ? 'Connected' : 'Disconnected'}
          className={`w-3 h-3 rounded-full ${connected ? 'bg-green-500' : 'bg-red-500'}`}
        />
      </div>
    </nav>
  )
}
