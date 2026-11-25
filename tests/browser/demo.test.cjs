"use strict";

// Exercise pkgdown/extra.js against the real generated popup assets.
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const test = require("node:test");
const vm = require("node:vm");
const { createEventTarget, createScheduler } = require("./harness.cjs");

const rootDir = path.resolve(__dirname, "../..");
const extraScriptPath = path.join(rootDir, "pkgdown/extra.js");
const docAssetsDir = path.join(rootDir, "pkgdown/assets/doc-assets");
const tick = () => new Promise(setImmediate);

function createDemoBrowser(options = {}) {
  const {
    scriptSrc = "https://updoc.leightonpayne.dev/extra.js",
    livePreview = false,
    initialBsTheme = "light",
    storedTheme = null,
    includeStateControls = true,
    includeThemeControls = true
  } = options;

  const { timers, frames, setTimeout, clearTimeout, requestAnimationFrame, cancelAnimationFrame } =
    createScheduler();
  const intervals = new Map();
  let nextIntervalId = 1;
  let now = 1_000_000;
  const loadedUrls = [];
  const nativeFetchCalls = [];
  const observers = [];
  let celebrated = 0;
  let dynamicLoading = false;

  const storageData = new Map();
  if (storedTheme !== null) storageData.set("__updoc_demo_theme", storedTheme);
  const sessionStorage = {
    getItem(key) { return storageData.has(key) ? storageData.get(key) : null; },
    setItem(key, value) { storageData.set(key, String(value)); },
    removeItem(key) { storageData.delete(key); }
  };

  const events = () => createEventTarget({ inlineHandlers: true });
  const document = { ...events(), hidden: false, readyState: "complete", activeElement: null };

  function matchesSelector(node, selector) {
    const tagMatch = selector.match(/^[a-zA-Z0-9_-]+/);
    const classMatches = [...selector.matchAll(/\.([a-zA-Z0-9_-]+)/g)].map(m => m[1]);
    const attrMatches = [...selector.matchAll(/\[([^=\]]+)(?:="([^"]*)")?\]/g)];
    if (tagMatch && node.tagName.toLowerCase() !== tagMatch[0].toLowerCase()) return false;
    for (const cls of classMatches) {
      if (!node.classList.contains(cls)) return false;
    }
    for (const [, name, expected] of attrMatches) {
      const actual = node.getAttribute(name);
      if (actual === null) return false;
      if (expected !== undefined && actual !== expected) return false;
    }
    return true;
  }

  function element(tagName) {
    const attributes = new Map();
    const styles = new Map();
    const dataName = name => name.slice(5).replace(/-([a-z])/g, (_, c) => c.toUpperCase());
    let ownText = "";
    const node = {
      ...events(),
      tagName,
      dataset: {},
      childNodes: [],
      parentNode: null,
      className: "",
      disabled: false,
      hidden: false,
      scrollTop: 0,
      scrollHeight: 100,
      clientHeight: 100,
      clientTop: 0,
      style: {
        setProperty(name, value) { styles.set(name, value); },
        removeProperty(name) { styles.delete(name); }
      },
      get textContent() { return ownText + this.childNodes.map(child => child.textContent).join(""); },
      set textContent(value) {
        this.childNodes.slice().forEach(child => child.remove());
        ownText = String(value);
      },
      setAttribute(name, value) {
        if (name.startsWith("data-")) this.dataset[dataName(name)] = String(value);
        else attributes.set(name, String(value));
      },
      getAttribute(name) {
        if (name.startsWith("data-")) {
          const key = dataName(name);
          return Object.prototype.hasOwnProperty.call(this.dataset, key) ? this.dataset[key] : null;
        }
        return attributes.has(name) ? attributes.get(name) : null;
      },
      removeAttribute(name) {
        if (name.startsWith("data-")) delete this.dataset[dataName(name)];
        else attributes.delete(name);
      },
      appendChild(child) {
        child.remove();
        child.parentNode = this;
        this.childNodes.push(child);
        if (dynamicLoading && child.tagName === "script" && child.src && this === document.head) {
          loadedUrls.push(child.src);
          const file = path.basename(new URL(child.src).pathname);
          const full = path.join(docAssetsDir, file);
          Promise.resolve().then(() => {
            if (file === "doc.js") {
              window.UpdocDoc = {
                create() {
                  const mascot = element("span");
                  return {
                    element: mascot,
                    setState(state) { mascot.dataset.state = state; },
                    stopCelebration() {},
                    celebrate() { celebrated += 1; },
                    refreshGaze() {}
                  };
                }
              };
            } else {
              const previousScript = document.currentScript;
              document.currentScript = child;
              try {
                vm.runInContext(fs.readFileSync(full, "utf8"), context, { filename: file });
              } finally {
                document.currentScript = previousScript;
              }
            }
            if (typeof child.onload === "function") child.onload();
          });
        }
        return child;
      },
      prepend(child) { child.remove(); child.parentNode = this; this.childNodes.unshift(child); },
      replaceChildren(...children) {
        this.childNodes.slice().forEach(child => child.remove());
        children.forEach(child => this.appendChild(child));
      },
      remove() {
        if (!this.parentNode) return;
        const siblings = this.parentNode.childNodes;
        siblings.splice(siblings.indexOf(this), 1);
        this.parentNode = null;
      },
      contains(child) { return child === this || this.childNodes.some(n => n.contains(child)); },
      getRootNode() { return this.parentNode ? this.parentNode.getRootNode() : this; },
      get isConnected() { return this === document.documentElement || !!(this.parentNode || this.host)?.isConnected; },
      getBoundingClientRect() { return { left: 0, top: 0, bottom: 28, width: 380, height: 28 }; },
      querySelectorAll(selector) {
        const nodes = [];
        function visit(parent) {
          for (const child of parent.childNodes) {
            if (matchesSelector(child, selector)) nodes.push(child);
            visit(child);
          }
        }
        visit(this);
        return nodes;
      },
      querySelector(selector) { return this.querySelectorAll(selector)[0] || null; },
      attachShadow() { this.shadowRoot = element("shadow-root"); this.shadowRoot.host = this; return this.shadowRoot; },
      focus() {
        this.getRootNode().activeElement = this;
        document.activeElement = this.getRootNode().host || this;
      },
      click() { if (!this.disabled) this.dispatch("click"); }
    };
    node.classList = {
      contains(name) { return node.className.split(/\s+/).includes(name); },
      add(name) { if (!this.contains(name)) node.className = (node.className + " " + name).trim(); },
      toggle(name, force) {
        if (force) this.add(name);
        else node.className = node.className.split(/\s+/).filter(v => v !== name).join(" ");
      }
    };
    return node;
  }

  document.createElement = element;
  document.createElementNS = (_, tag) => element(tag);
  document.createTextNode = text => { const node = element("#text"); node.textContent = text; return node; };
  document.documentElement = element("html");
  document.documentElement.dataset.bsTheme = initialBsTheme;
  document.head = element("head");
  document.body = element("body");
  document.documentElement.appendChild(document.head);
  document.documentElement.appendChild(document.body);
  document.querySelectorAll = selector => document.documentElement.querySelectorAll(selector);
  document.querySelector = selector => document.documentElement.querySelector(selector);
  document.getElementById = id => {
    const visit = node => node.id === id ? node : node.childNodes.map(visit).find(Boolean);
    return visit(document.documentElement) || null;
  };

  if (livePreview) {
    const previewTag = element("script");
    previewTag.setAttribute("data-updoc-preview", "true");
    document.head.appendChild(previewTag);
  }

  if (includeStateControls) {
    const group = element("div");
    group.setAttribute("data-updoc-demo", "states");
    for (const state of ["ready", "building", "warning", "error", "disconnected"]) {
      const btn = element("button");
      btn.setAttribute("data-updoc-demo-state", state);
      btn.setAttribute("aria-pressed", state === "ready" ? "true" : "false");
      btn.textContent = state;
      group.appendChild(btn);
    }
    document.body.appendChild(group);
  }

  if (includeThemeControls) {
    const group = element("div");
    group.setAttribute("data-updoc-demo", "themes");
    for (const theme of ["auto", "inverse", "light", "dark"]) {
      const btn = element("button");
      btn.setAttribute("data-updoc-demo-theme", theme);
      btn.setAttribute("aria-pressed", theme === "auto" ? "true" : "false");
      btn.textContent = theme;
      group.appendChild(btn);
    }
    document.body.appendChild(group);
  }

  const extraTag = element("script");
  extraTag.src = scriptSrc;
  extraTag.setAttribute("src", "../extra.js");
  document.head.appendChild(extraTag);
  document.currentScript = extraTag;
  dynamicLoading = true;

  class MockMutationObserver {
    constructor(callback) {
      this.callback = callback;
      this.targets = [];
      observers.push(this);
    }
    observe(target) { this.targets.push(target); }
    disconnect() { this.targets = []; }
  }

  const nativeFetch = (input, init) => {
    nativeFetchCalls.push({ input, init });
    return Promise.resolve({ ok: true, status: 200, json: async () => ({ native: true }) });
  };

  const window = {
    ...events(),
    console,
    sessionStorage,
    fetch: nativeFetch,
    setTimeout(fn, delay) { return setTimeout(fn, delay); },
    clearTimeout(id) { return clearTimeout(id); },
    setInterval(fn, delay) {
      const id = nextIntervalId++;
      intervals.set(id, { fn, delay });
      return id;
    },
    clearInterval(id) { intervals.delete(id); },
    requestAnimationFrame,
    cancelAnimationFrame,
    getComputedStyle() { return { transform: "none", maxHeight: "560px" }; },
    matchMedia() { return { matches: false, addEventListener() {} }; }
  };

  const FakeDate = { now: () => now };
  const context = vm.createContext({
    window,
    document,
    URL,
    AbortController,
    MutationObserver: MockMutationObserver,
    Date: FakeDate,
    fetch: (...args) => window.fetch(...args)
  });

  vm.runInContext(fs.readFileSync(extraScriptPath, "utf8"), context, { filename: "extra.js" });

  async function flushLoads() {
    for (let i = 0; i < 12; i += 1) await tick();
    settleFrames();
  }

  function settleFrames() {
    for (let i = 0; i < 10; i += 1) {
      const currentFrames = [...frames.values()];
      frames.clear();
      currentFrames.forEach(fn => fn());
      for (const [id, timer] of [...timers]) {
        if (timer.delay < 500) { timers.delete(id); timer.fn(); }
      }
      if (!frames.size && ![...timers.values()].some(timer => timer.delay < 500)) return;
    }
  }

  function advanceTimers(maxDelay) {
    const due = [...timers.entries()]
      .filter(([, timer]) => timer.delay <= maxDelay)
      .sort((a, b) => a[1].delay - b[1].delay);
    for (const [id, timer] of due) {
      if (!timers.has(id)) continue;
      timers.delete(id);
      now += timer.delay;
      timer.fn();
      settleFrames();
    }
  }

  function triggerBsTheme(theme) {
    document.documentElement.dataset.bsTheme = theme;
    for (const observer of observers) {
      if (observer.targets.includes(document.documentElement)) observer.callback([]);
    }
  }

  return {
    window,
    document,
    loadedUrls,
    nativeFetchCalls,
    storageData,
    intervals,
    timers,
    flushLoads,
    settleFrames,
    advanceTimers,
    triggerBsTheme,
    celebrated: () => celebrated,
    host: () => document.getElementById("__updoc_status"),
    shell: () => document.getElementById("__updoc_status")?.shadowRoot.querySelector(".updoc-shell"),
    stateButton: name => document.querySelector(`[data-updoc-demo-state="${name}"]`),
    themeButton: name => document.querySelector(`[data-updoc-demo-theme="${name}"]`),
    popupButton: label => document.getElementById("__updoc_status").shadowRoot
      .querySelectorAll("button")
      .find(node => node.textContent === label || node.getAttribute("aria-label") === label)
  };
}

test("demo popup stands down cleanly when a live updoc preview is active", async () => {
  const app = createDemoBrowser({ livePreview: true });
  await app.flushLoads();
  assert.deepEqual(app.loadedUrls, []);
  assert.equal(app.host(), null);
  assert.equal(app.document.documentElement.dataset.updocDemo, undefined);
  assert.equal(app.stateButton("error").disabled, true);
  assert.equal(app.themeButton("dark").disabled, true);
  assert.match(app.document.querySelector(".updoc-demo-note").textContent, /Live updoc preview is active/);

  await app.window.fetch("/__updoc__/rebuild", {
    method: "POST",
    headers: { "X-Updoc-Request": "rebuild" }
  });
  assert.equal(app.nativeFetchCalls.length, 1, "live preview fetch must not be intercepted");
});

test("static docs page mounts the real popup and resolves doc-assets from extra.js", async () => {
  const app = createDemoBrowser({
    scriptSrc: "https://updoc.leightonpayne.dev/extra.js"
  });
  await app.flushLoads();
  assert.deepEqual(app.loadedUrls, [
    "https://updoc.leightonpayne.dev/doc-assets/panel-css.js",
    "https://updoc.leightonpayne.dev/doc-assets/doc.js",
    "https://updoc.leightonpayne.dev/doc-assets/panel-primitives.js",
    "https://updoc.leightonpayne.dev/doc-assets/panel.js"
  ]);
  assert.equal(app.document.documentElement.dataset.updocDemo, "true");
  assert.equal(app.window.__updocDemoCSS, undefined);
  assert.equal(app.window.__updocPreviewBootstrap, undefined);
  assert.equal(app.window.__updocPreviewPanel, undefined);
  assert.equal(app.shell().dataset.updocState, "ready");
  assert.match(app.shell().textContent, /Watching for changes/);
  assert.equal(app.host().dataset.theme, "light");
  const styleNode = app.host().shadowRoot.querySelector("style");
  assert.ok(styleNode && styleNode.textContent.includes(".updoc-shell"));
});

test("state buttons switch every real popup state, recover minimized errors, and celebrate after building", async () => {
  const app = createDemoBrowser();
  await app.flushLoads();

  // Clicking Ready while already in collapsed ready opens the ready drawer.
  app.stateButton("ready").click();
  app.settleFrames();
  assert.equal(app.shell().dataset.updocState, "ready-full");
  assert.ok(app.popupButton("Rebuild"));

  // Error state renders structured diagnostic and re-expands if minimized.
  app.stateButton("error").click();
  app.settleFrames();
  assert.equal(app.shell().dataset.updocState, "error-full");
  assert.equal(app.stateButton("error").getAttribute("aria-pressed"), "true");
  assert.match(app.shell().textContent, /Could not render vignette/);
  assert.match(app.shell().textContent, /vignettes\/articles\/get-started\.Rmd:18-20/);
  app.popupButton("Minimize updoc message").click();
  app.settleFrames();
  assert.equal(app.shell().dataset.updocState, "error-compact");
  app.stateButton("error").click();
  app.settleFrames();
  assert.equal(app.shell().dataset.updocState, "error-full");

  // Warning state renders warning diagnostic.
  app.stateButton("warning").click();
  app.settleFrames();
  assert.equal(app.shell().dataset.updocState, "warning-full");
  assert.match(app.shell().textContent, /Missing title in YAML front matter/);

  // Disconnected state renders live retry countdown and clears it when leaving.
  app.stateButton("disconnected").click();
  app.settleFrames();
  assert.equal(app.shell().dataset.updocState, "disconnected");
  assert.match(app.shell().textContent, /Preview disconnected/);
  assert.ok(app.intervals.size > 0);

  // Building state steps through progress and returns to ready with celebration.
  app.stateButton("building").click();
  app.settleFrames();
  assert.equal(app.intervals.size, 0, "leaving disconnected clears countdown interval");
  assert.equal(app.shell().dataset.updocState, "building");
  assert.match(app.shell().textContent, /serve\.Rd/);
  app.advanceTimers(3000);
  assert.equal(app.shell().dataset.updocState, "ready");
  assert.equal(app.stateButton("ready").getAttribute("aria-pressed"), "true");
  assert.equal(app.celebrated(), 1);
});

test("clicking Rebuild inside the popup simulates a rebuild without intercepting other fetches", async () => {
  const app = createDemoBrowser();
  await app.flushLoads();

  // Regular fetches pass straight through to native fetch.
  const search = await app.window.fetch("/search.json");
  assert.deepEqual(await search.json(), { native: true });
  assert.equal(app.nativeFetchCalls.length, 1);

  // Open the real popup and click its Rebuild button.
  app.shell().click();
  app.settleFrames();
  app.popupButton("Rebuild").click();
  await tick();
  app.settleFrames();
  assert.equal(app.nativeFetchCalls.length, 1, "demo rebuild should not hit network");
  assert.equal(app.shell().dataset.updocState, "building");
  assert.equal(app.stateButton("building").getAttribute("aria-pressed"), "true");

  app.advanceTimers(3000);
  assert.equal(app.shell().dataset.updocState, "ready");
  assert.equal(app.stateButton("ready").getAttribute("aria-pressed"), "true");
  assert.equal(app.celebrated(), 1);
});

test("theme buttons override and track the site theme and persist across page loads", async () => {
  const app = createDemoBrowser({ initialBsTheme: "light" });
  await app.flushLoads();
  assert.equal(app.host().dataset.theme, "light");

  app.themeButton("inverse").click();
  assert.equal(app.host().dataset.theme, "dark");
  assert.equal(app.storageData.get("__updoc_demo_theme"), "inverse");
  assert.equal(app.themeButton("inverse").getAttribute("aria-pressed"), "true");

  // Toggling site light/dark switch keeps inverse inverted.
  app.triggerBsTheme("dark");
  assert.equal(app.host().dataset.theme, "light");
  app.triggerBsTheme("light");
  assert.equal(app.host().dataset.theme, "dark");

  // Explicit dark/light stays pinned across site theme switches.
  app.themeButton("light").click();
  app.triggerBsTheme("dark");
  assert.equal(app.host().dataset.theme, "light");

  // Returning to auto clears stored override and tracks the site theme again.
  app.themeButton("auto").click();
  assert.equal(app.storageData.has("__updoc_demo_theme"), false);
  assert.equal(app.host().dataset.theme, "dark");
  app.triggerBsTheme("light");
  assert.equal(app.host().dataset.theme, "light");

  // A subsequent page load restores a persisted theme choice.
  const next = createDemoBrowser({ initialBsTheme: "dark", storedTheme: "inverse" });
  await next.flushLoads();
  assert.equal(next.host().dataset.theme, "light");
  assert.equal(next.themeButton("inverse").getAttribute("aria-pressed"), "true");
});
