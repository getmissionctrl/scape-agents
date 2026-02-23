import { useState } from 'react'

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
  active: { label: 'Active', color: 'text-green-400' },
  blocked: { label: 'Blocked', color: 'text-red-400' },
  backlog: { label: 'Backlog', color: 'text-gray-400' },
  done: { label: 'Done', color: 'text-gray-600' },
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
      className={`border-l border-gray-800 bg-gray-900 transition-all duration-200 overflow-hidden ${
        open ? 'w-72' : 'w-0'
      }`}
    >
      {open && (
        <div className="w-72 h-full flex flex-col">
          <div className="flex items-center justify-between px-4 py-3 border-b border-gray-800">
            <span className="text-sm font-medium text-gray-200">Tasks</span>
            <button
              onClick={onToggle}
              className="text-gray-400 hover:text-gray-200 text-xs"
            >
              Close
            </button>
          </div>
          <div className="flex-1 overflow-y-auto px-3 py-2">
            {tasks.length === 0 && (
              <div className="text-gray-500 text-xs text-center py-4">No tasks</div>
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
                    className="flex items-center gap-2 w-full text-left mb-1"
                  >
                    <span className="text-[10px] text-gray-500">
                      {isCollapsed ? '▸' : '▾'}
                    </span>
                    <span className={`text-xs font-medium ${cfg.color}`}>
                      {cfg.label}
                    </span>
                    <span className="text-[10px] text-gray-600">{items.length}</span>
                  </button>
                  {!isCollapsed &&
                    items.map((task) => (
                      <div
                        key={task.id}
                        className={`pl-4 py-1 text-xs ${
                          task.status === 'done'
                            ? 'text-gray-600 line-through'
                            : 'text-gray-300'
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
