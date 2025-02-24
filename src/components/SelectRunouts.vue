<template>
  <div class="select-runouts mx-4">
    <h2 class="text-xl font-semibold mb-4">Select Turn/River Cards</h2>
    <div class="flex justify-between mb-4">
      <button
        class="px-4 py-2 bg-green-500 text-white rounded hover:bg-green-600"
        @click="selectAll"
      >
        Select All
      </button>
      <button
        class="px-4 py-2 bg-red-500 text-white rounded hover:bg-red-600"
        @click="deselectAll"
      >
        Deselect All
      </button>
    </div>
    <div class="card-grid">
      <div
        v-for="card in allCards"
        :key="card"
        :class="[
          'card',
          { 'selected': selectedCards.includes(card) },
          suitColor(card),
        ]"
        @click="toggleCard(card)"
      >
        {{ card }}
      </div>
    </div>
    <button
      class="mt-4 px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600"
      @click="saveRunouts"
    >
      Save Runouts
    </button>
  </div>
</template>

<script setup lang="ts">
import { ref } from "vue";
import { invoke } from "@tauri-apps/api/tauri";

const SUITS = ["♠", "♥", "♦", "♣"];
const RANKS = ["2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K", "A"];
const allCards = SUITS.flatMap((suit) => RANKS.map((rank) => `${rank}${suit}`));
const selectedCards = ref<string[]>([]);

const toggleCard = (card: string) => {
  if (selectedCards.value.includes(card)) {
    selectedCards.value = selectedCards.value.filter((c) => c !== card);
  } else {
    selectedCards.value.push(card);
  }
};

const suitColor = (card: string) => {
  const suit = card.slice(-1);
  return suit === "♥" || suit === "♦" ? "text-red-600" : "text-black";
};

const selectAll = () => {
  selectedCards.value = [...allCards]; // Select all 52 cards
};

const deselectAll = () => {
  selectedCards.value = []; // Deselect all cards
};

const saveRunouts = async () => {
  try {
    await invoke("set_allowed_runouts", { cards: selectedCards.value });
    alert("Runouts saved successfully!");
  } catch (error) {
    console.error("Failed to save runouts:", error);
    alert("Error saving runouts.");
  }
};
</script>

<style scoped>
.select-runouts {
  padding: 1rem;
}

.card-grid {
  display: grid;
  grid-template-columns: repeat(13, 2.5rem);
  gap: 0.25rem;
}

.card {
  width: 2.5rem;
  height: 3.5rem;
  border: 1px solid #ccc;
  border-radius: 0.25rem;
  display: flex;
  align-items: center;
  justify-content: center;
  cursor: pointer;
  user-select: none;
  transition: background-color 0.2s;
}

.card:hover {
  background-color: #f0f0f0;
}

.card.selected {
  background-color: #4caf50;
  color: white;
}
</style>