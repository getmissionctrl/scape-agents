import { useState } from 'react'
import { ChevronRight, ChevronDown } from 'lucide-react'

export interface Task {
  id: string
  title: string
  status: 'active' | 'blocked' | 'backlog' | 'done'
  description?: string
}

export interface TaskPanelProps {
  tasks: Task[]
  open: boolean
  onToggle: () => void
}

const statusConfig: Record<Task['status'], { label: string; color: string }> = {
  active: { label: 'Active', color: 'text-accent-green' },
  blocked: { label: 'Blocked', color: 'text-accent-red' },
  backlog: { label: 'Backlog', color: 'text-fg-muted' },
  done: { label: 'Done', color: 'text-fg-subtle' },
}

const statusOrder: Task['status'][] = ['active', 'blocked', 'backlog', 'done']

export function TaskPanel({ tasks, open, onToggle }: TaskPanelProps) {
  const [collapsed, setCollapsed] = useState<Record<string, boolean>>({ done: true })

  const grouped = statusOrder.reduce(
    (acc, status) => {
      acc[status] = tasks.filter((t) => t.status === status)
      return acc
    },
    {} as Record<Task['status'], Task[]>,
  )

  return (
    <div
      className={`border-l border-fg-subtle/15 bg-bg-raised transition-all duration-200 overflow-hidden ${
        open ? 'w-72' : 'w-0'
      }`}
    >
      {open && (
        <div className="w-72 h-full flex flex-col">
          <div className="flex items-center justify-between px-4 py-3 border-b border-fg-subtle/15">
            <span className="text-[10px] font-mono font-medium text-fg-muted uppercase tracking-wider">Tasks</span>
            <button
              onClick={onToggle}
              className="text-fg-muted hover:text-fg-primary text-xs transition-colors"
            >
              Close
            </button>
          </div>
          <div className="flex-1 overflow-y-auto px-3 py-2">
            {tasks.length === 0 && (
              <div className="text-fg-subtle text-xs text-center py-4">No tasks</div>
            )}
            {statusOrder.map((status) => {
              const items = grouped[status]
              if (items.length === 0) return null
              const cfg = statusConfig[status]
              const isCollapsed = collapsed[status]

              return (
                <div key={status} className="mb-3">
                  <button
                    onClick={() =>
                      setCollapsed((prev) => ({ ...prev, [status]: !prev[status] }))
                    }
                    className="flex items-center gap-1.5 w-full text-left mb-1"
                  >
                    {isCollapsed
                      ? <ChevronRight size={12} className="text-fg-subtle" />
                      : <ChevronDown size={12} className="text-fg-subtle" />
                    }
                    <span className={`text-[10px] font-mono font-medium uppercase tracking-wider ${cfg.color}`}>
                      {cfg.label}
                    </span>
                    <span className="text-[10px] text-fg-subtle font-mono">{items.length}</span>
                  </button>
                  {!isCollapsed &&
                    items.map((task) => (
                      <div
                        key={task.id}
                        className={`pl-5 py-1 text-xs ${
                          task.status === 'done'
                            ? 'text-fg-subtle line-through'
                            : 'text-fg-secondary'
                        }`}
                      >
                        {task.title}
                      </div>
                    ))}
                </div>
              )
            })}
          </div>
        </div>
      )}
    </div>
  )
}
