"use strict";
import { Elm } from "./Main.elm";

// Run Elm on all elm Nodes
function initializeWidgets() {
  const elmNodes = document.querySelectorAll(".elm");
  elmNodes.forEach((node) => {
    console.log({ flags: node.dataset.flags });
    const app = Elm.Main.init({
      node,
      flags: getFlags(node.dataset.flags)
    });
    // Initialize ports below this line
  });
}

function getFlags(data) {
  return data ? JSON.parse(data) : null;
}

// Initialize Elm on page load
window.addEventListener("load", (event) => {
  initializeWidgets();
});

// Initialize Elm on Turbolinks transition
document.addEventListener("turbolinks:load", (e) => {
  initializeWidgets();
});
