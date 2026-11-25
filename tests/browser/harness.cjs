"use strict";

// Shared minimal DOM/event/timer helpers for the zero-dependency browser suites.
function createEventTarget(options = {}) {
  const listeners = new Map();
  const target = {
    addEventListener(type, fn) {
      if (!listeners.has(type)) listeners.set(type, new Set());
      listeners.get(type).add(fn);
    },
    removeEventListener(type, fn) {
      listeners.get(type)?.delete(fn);
    },
    dispatch(type, extra = {}) {
      const event = {
        type,
        target: this,
        preventDefault() {},
        stopPropagation() {},
        ...extra
      };
      if (options.inlineHandlers) this["on" + type]?.(event);
      for (const fn of [...(listeners.get(type) || [])]) fn(event);
    },
    listenerCount(type) {
      if (type) return (listeners.get(type) || new Set()).size;
      return [...listeners.values()].reduce((sum, group) => sum + group.size, 0);
    }
  };
  if (options.registry) options.registry.push(target);
  return target;
}

function createScheduler() {
  const timers = new Map();
  const frames = new Map();
  let nextId = 1;
  return {
    timers,
    frames,
    setTimeout(fn, delay) {
      const id = nextId++;
      timers.set(id, { fn, delay });
      return id;
    },
    clearTimeout(id) {
      timers.delete(id);
    },
    requestAnimationFrame(fn) {
      const id = nextId++;
      frames.set(id, fn);
      return id;
    },
    cancelAnimationFrame(id) {
      frames.delete(id);
    }
  };
}

module.exports = { createEventTarget, createScheduler };
