import { useState, useCallback } from 'react'
import { useGateway } from './hooks/useGateway'
import { useMessages } from './hooks/useMessages'
import { AppShell } from './components/Layout/AppShell'
import { ChatPanel } from './components/Chat/ChatPanel'
import { DesktopModal } from './components/Desktop/DesktopModal'
import { TerminalModal } from './components/Terminal/TerminalModal'
import type { ViewMode } from './components/Layout/Sidebar'

export function App() {
  const [activeView, setActiveView] = useState<ViewMode>('chat')
  const [desktopOpen, setDesktopOpen] = useState(false)
  const [terminalOpen, setTerminalOpen] = useState(false)
  const { messages, loading, addUserMessage, handleGatewayMessage } = useMessages()

  const { connected, sendMessage } = useGateway({
    onMessage: handleGatewayMessage,
  })

  const handleSend = useCallback(
    (text: string) => {
      addUserMessage(text)
      sendMessage(text)
    },
    [addUserMessage, sendMessage],
  )

  const handleNavigate = useCallback((view: ViewMode) => {
    if (view === 'desktop') {
      setDesktopOpen(true)
    } else if (view === 'terminal') {
      setTerminalOpen(true)
    } else {
      setActiveView(view)
    }
  }, [])

  return (
    <>
      <AppShell
        activeView={activeView}
        onNavigate={handleNavigate}
        connected={connected}
      >
        <ChatPanel
          messages={messages}
          loading={loading}
          onSend={handleSend}
          disabled={!connected}
        />
      </AppShell>
      <DesktopModal open={desktopOpen} onClose={() => setDesktopOpen(false)} />
      <TerminalModal open={terminalOpen} onClose={() => setTerminalOpen(false)} />
    </>
  )
}
