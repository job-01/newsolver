// Updated SelectRunouts.vue
<template>
  <div>
    <h2 class="text-xl font-semibold mb-4">Select Turn/River Cards</h2>
    
    <div class="flex mb-4 gap-3">
      <button class="button-base button-blue" @click="selectAll">
        Select All
      </button>
      <button class="button-base button-red" @click="deselectAll">
        Deselect All
      </button>
    </div>
    
    <div class="flex flex-wrap">
      <div v-for="suit in 4" :key="suit" class="flex mb-3">
        <BoardSelectorCard
          v-for="rank in 13"
          :key="rank"
          class="m-1"
          :card-id="56 - 4 * rank - suit"
          :is-selected="selectedRunouts.includes(56 - 4 * rank - suit)"
          @click="toggleCard(56 - 4 * rank - suit)"
        />
      </div>
    </div>
    
    <div class="flex mt-4 gap-3">
      <button class="button-base button-blue" @click="saveRunouts">
        Save Runouts
      </button>
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref } from "vue";
import { useStore, useConfigStore } from "../store";
import BoardSelectorCard from "./BoardSelectorCard.vue";
import { parseCardString, cardText } from "../utils";

const config = useConfigStore();
const store = useStore();

// Initialize with all cards selected by default
const selectedRunouts = ref<number[]>(
  Array.from({ length: 52 }, (_, i) => i)
);

const toggleCard = (cardId: number) => {
  if (selectedRunouts.value.includes(cardId)) {
    selectedRunouts.value = selectedRunouts.value.filter(id => id !== cardId);
  } else {
    selectedRunouts.value.push(cardId);
  }
};

const selectAll = () => {
  selectedRunouts.value = Array.from({ length: 52 }, (_, i) => i);
};

const deselectAll = () => {
  selectedRunouts.value = [];
};

const saveRunouts = () => {
  // Store the selected runouts in the config store
  config.allowedRunouts = [...selectedRunouts.value];
};
</script>