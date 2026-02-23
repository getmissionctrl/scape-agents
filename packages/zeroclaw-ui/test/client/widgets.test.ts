import { describe, it, expect } from 'vitest'
import { parseWidgets } from '../../src/client/lib/widgets'

describe('parseWidgets', () => {
  it('returns text unchanged when no widgets present', () => {
    const result = parseWidgets('Hello world')
    expect(result.text).toBe('Hello world')
    expect(result.widgets).toEqual([])
  })

  it('extracts a single widget', () => {
    const input = 'Choose: [[WIDGET:{"widget":"buttons","id":"c1","label":"Pick:","options":["A","B"]}]]'
    const result = parseWidgets(input)
    expect(result.text).toBe('Choose:')
    expect(result.widgets).toHaveLength(1)
    expect(result.widgets[0].widget).toBe('buttons')
    expect(result.widgets[0].id).toBe('c1')
  })

  it('extracts multiple widgets', () => {
    const input = 'First [[WIDGET:{"widget":"buttons","id":"a","options":["X"]}]] then [[WIDGET:{"widget":"confirm","id":"b","title":"Sure?"}]]'
    const result = parseWidgets(input)
    expect(result.widgets).toHaveLength(2)
    expect(result.widgets[0].id).toBe('a')
    expect(result.widgets[1].id).toBe('b')
  })

  it('handles malformed widget JSON gracefully', () => {
    const input = 'Bad [[WIDGET:{invalid json}]] data'
    const result = parseWidgets(input)
    expect(result.text).toBe('Bad  data')
    expect(result.widgets).toEqual([])
  })
})
