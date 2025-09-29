<script setup lang="ts">
import { computed } from 'vue'
import type { FileTab } from '../types'

const props = defineProps<{
  tabs: FileTab[]
  activeTabId: string | null
}>()

const emit = defineEmits<{
  'tab-select': [tabId: string]
  'tab-close': [tabId: string]
  'tab-new': []
}>()

const activeTabs = computed(() => props.tabs)

const selectTab = (tabId: string) => {
  emit('tab-select', tabId)
}

const closeTab = (event: Event, tabId: string) => {
  event.stopPropagation()
  emit('tab-close', tabId)
}

const createNewTab = () => {
  emit('tab-new')
}

const getTabDisplayName = (tab: FileTab) => {
  return tab.isModified ? `${tab.name} •` : tab.name
}
</script>

<template>
  <div class="tab-manager">
    <div class="tabs-container">
      <div 
        v-for="tab in activeTabs" 
        :key="tab.id"
        class="tab"
        :class="{ 
          active: tab.id === activeTabId,
          modified: tab.isModified 
        }"
        @click="selectTab(tab.id)"
        :title="tab.path || tab.name"
      >
        <span class="tab-icon">📄</span>
        <span class="tab-name">{{ getTabDisplayName(tab) }}</span>
        <button 
          class="tab-close"
          @click="closeTab($event, tab.id)"
          :title="`Закрыть ${tab.name}`"
        >
          ×
        </button>
      </div>
      <button 
        class="new-tab-btn"
        @click="createNewTab"
        title="Новый файл"
      >
        +
      </button>
    </div>
  </div>
</template>

<style scoped>
.tab-manager {
  background: var(--bg-secondary);
  border-bottom: 1px solid var(--border-color);
  overflow-x: auto;
  overflow-y: hidden;
}

.tabs-container {
  display: flex;
  align-items: stretch;
  min-height: 32px;
}

.tab {
  display: flex;
  align-items: center;
  gap: 6px;
  padding: 6px 8px;
  background: var(--bg-secondary);
  border-right: 1px solid var(--border-color);
  cursor: pointer;
  user-select: none;
  transition: background-color 0.2s;
  min-width: 120px;
  max-width: 200px;
  position: relative;
  font-size: 12px;
}

.tab:hover { background: var(--border-color); }

.tab.active { background: var(--bg-primary); color: var(--text-primary); }

.tab.modified .tab-name {
  font-style: italic;
}

.tab-icon {
  font-size: 12px;
  flex-shrink: 0;
}

.tab-name {
  flex: 1;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
  color: var(--text-primary);
}

.tab.active .tab-name { color: var(--text-primary); }

.tab-close {
  background: none;
  border: none;
  color: var(--text-muted);
  cursor: pointer;
  padding: 2px;
  border-radius: 2px;
  font-size: 16px;
  line-height: 1;
  width: 16px;
  height: 16px;
  display: flex;
  align-items: center;
  justify-content: center;
  flex-shrink: 0;
  opacity: 0;
  transition: all 0.2s;
}

.tab:hover .tab-close {
  opacity: 1;
}

.tab-close:hover { background: #f48771; color: white; }

.new-tab-btn {
  background: none;
  border: none;
  color: var(--text-muted);
  cursor: pointer;
  padding: 6px 12px;
  font-size: 16px;
  transition: all 0.2s;
  border-right: 1px solid var(--border-color);
}

.new-tab-btn:hover { background: var(--border-color); color: var(--text-primary); }

.tabs-container::-webkit-scrollbar {
  height: 3px;
}

.tabs-container::-webkit-scrollbar-track { background: var(--bg-secondary); }

.tabs-container::-webkit-scrollbar-thumb {
  background: #424242;
  border-radius: 3px;
}

.tabs-container::-webkit-scrollbar-thumb:hover {
  background: #4f4f4f;
}
</style>