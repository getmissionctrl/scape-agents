import type { WidgetData } from './types'

const WIDGET_RE = /\[\[WIDGET:(.*?)\]\]/g

export function parseWidgets(text: string): {
  text: string
  widgets: WidgetData[]
} {
  const widgets: WidgetData[] = []
  const cleanText = text.replace(WIDGET_RE, (_match, json: string) => {
    try {
      const data = JSON.parse(json) as WidgetData
      if (data.widget && data.id) {
        widgets.push(data)
      }
    } catch {
      // Skip malformed widgets
    }
    return ''
  })

  return {
    text: cleanText.trim(),
    widgets,
  }
}
