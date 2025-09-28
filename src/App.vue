<script setup lang="ts">
import { ref } from 'vue'
import MenuBar from './components/MenuBar.vue'
import CodeEditor from './components/CodeEditor.vue'
import OutputPanel from './components/OutputPanel.vue'

const code = ref('// Добро пожаловать в редактор кода\nconsole.log("Hello, World!");')
const output = ref('')
const fontSize = ref(14)

const handleFileAction = (action: string) => {
  console.log('File action:', action)
  // Здесь будет интеграция с Tauri для работы с файлами
}

const handleTextAction = (action: string) => {
  console.log('Text action:', action)
  // Здесь будут действия для работы с текстом
}

const handleRunCode = () => {
  output.value = 'Программа запущена...\n' + code.value
  console.log('Running code')
  // Здесь будет логика запуска программы
}

const handleFontSizeChange = (change: number) => {
  fontSize.value = Math.max(10, Math.min(24, fontSize.value + change))
}
</script>

<template>
  <div class="app">
    <MenuBar 
      @file-action="handleFileAction"
      @text-action="handleTextAction"
      @run-code="handleRunCode"
      @font-size-change="handleFontSizeChange"
    />
    
    <div class="editor-container">
      <CodeEditor 
        v-model="code"
        :font-size="fontSize"
        class="code-section"
      />
      
      <OutputPanel 
        :output="output"
        :font-size="fontSize"
        class="output-section"
      />
    </div>
  </div>
</template>

<style scoped>
.app {
  height: 100vh;
  display: flex;
  flex-direction: column;
  background: #1e1e1e;
  color: #d4d4d4;
  font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
}

.editor-container {
  flex: 1;
  display: flex;
  overflow: hidden;
}

.code-section {
  flex: 1;
  border-right: 1px solid #3e3e3e;
}

.output-section {
  width: 400px;
  min-width: 300px;
  max-width: 50%;
}

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

body {
  overflow: hidden;
}
</style>