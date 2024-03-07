import { useCallback } from "react";

// Return the filter method.
export function useFilter(value, search) {
  return useCallback(
    (data) => {
      const isSearchMatch = !search ? true : data.title.toLowerCase().indexOf(search) > -1;
      const isFilterMatch = value === "all" ? true : data.color === value;
      return isSearchMatch && isFilterMatch;
    },
    [value, search],
  );
}

// Return one of the values of the array.
export function oneOf(array) {
  return array[Math.floor(Math.random() * Math.floor(array.length))];
}

let uuid = 3;
// Generate 3 items.
export function generateItems() {
  const items = [];
  for (let i = 0; i < 3; i++) {
    const color = oneOf(["red", "green", "blue"]);
    const width = oneOf([1, 2]);
    const height = oneOf([1, 2]);

    const alphabet = "abcdefghijklmnopqrstuvwxyz";
    const title = oneOf(alphabet) + oneOf(alphabet);
    const id = uuid++;

    items.push({ id, color, width, height, title });
  }

  return items;
}

// Grid static options.
export const options = {
  dragSortHeuristics: {
    sortInterval: 70,
  },
  layoutDuration: 400,
  dragRelease: {
    duration: 400,
    easing: "ease-out",
  },
  dragEnabled: true,
  dragContainer: document.body,
  // The placeholder of an item that is being dragged.
  dragPlaceholder: {
    enabled: true,
    createElement: (item) => {
      // The element will have the Css class ".muuri-item-placeholder".
      item.getElement().cloneNode(true);
    },
  },
};

export const itemData = [
  {
    id: "1",
    color: "red",
    url: "https://images.unsplash.com/photo-1551963831-b3b1ca40c98e",
    text: "Breakfast",
  },
  {
    id: "2",
    color: "red",
    url: "https://images.unsplash.com/photo-1551782450-a2132b4ba21d",
    text: "Burger",
  },
  {
    id: "3",
    color: "red",
    url: "https://images.unsplash.com/photo-1522770179533-24471fcdba45",
    text: "Camera",
  },
  {
    id: "4",
    color: "red",
    url: "https://images.unsplash.com/photo-1444418776041-9c7e33cc5a9c",
    text: "Coffee",
  },
  {
    id: "5",
    color: "red",
    url: "https://images.unsplash.com/photo-1533827432537-70133748f5c8",
    text: "Hats",
  },
  {
    id: "6",
    color: "red",
    url: "https://images.unsplash.com/photo-1558642452-9d2a7deb7f62",
    text: "Honey",
  },
  {
    id: "7",
    color: "red",
    url: "https://images.unsplash.com/photo-1516802273409-68526ee1bdd6",
    text: "Basketball",
  },
  {
    id: "8",
    color: "red",
    url: "https://images.unsplash.com/photo-1518756131217-31eb79b20e8f",
    text: "Fern",
  },
];
