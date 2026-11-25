"use strict";

// Execute both the real controller and DOM primitives. Geometry is deterministic;
// actual layout and browser-native focus behavior still need a browser smoke test.
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const test = require("node:test");
const vm = require("node:vm");
const { createEventTarget, createScheduler } = require("./harness.cjs");
const assets = process.argv[2] || path.resolve(__dirname, "../../inst/preview");
const ready = { phase: "ready", warnings: [] };
const error = { phase: "error", error: { summary: "A transient build failure" }, warnings: [] };
const warning = { summary: "A build warning", location: "R/example.R:3" };
const building = { phase: "building", progress: { operation: "Building documentation" } };
const tick = () => new Promise(setImmediate);

function browser(status = ready, view = { readyExpanded: true }) {
  const { timers, frames, setTimeout, clearTimeout, requestAnimationFrame, cancelAnimationFrame } =
    createScheduler();
  const requests = [];
  const events = () => createEventTarget({ inlineHandlers: true });
  const document = { ...events(), hidden: false, activeElement: null };
  function element(tagName) {
    const attributes = new Map();
    const styles = new Map();
    const dataName = name => name.slice(5).replace(/-([a-z])/g, (_, c) => c.toUpperCase());
    let ownText = "";
    const node = {
      ...events(), tagName, dataset: {}, childNodes: [], parentNode: null,
      className: "", disabled: false, scrollTop: 0, scrollHeight: 100, clientHeight: 100, clientTop: 0,
      textUpdates: 0,
      style: {
        setProperty(name, value) { styles.set(name, value); },
        removeProperty(name) { styles.delete(name); }
      },
      get textContent() { return ownText + this.childNodes.map(child => child.textContent).join(""); },
      set textContent(value) {
        this.textUpdates++;
        this.childNodes.slice().forEach(child => child.remove());
        ownText = String(value);
      },
      setAttribute(name, value) {
        if (name.startsWith("data-")) this.dataset[dataName(name)] = String(value);
        else attributes.set(name, String(value));
      },
      getAttribute(name) {
        if (name.startsWith("data-")) return this.dataset[dataName(name)] ?? null;
        return attributes.get(name) ?? null;
      },
      removeAttribute(name) {
        if (name.startsWith("data-")) delete this.dataset[dataName(name)];
        else attributes.delete(name);
      },
      appendChild(child) { child.remove(); child.parentNode = this; this.childNodes.push(child); return child; },
      prepend(child) { child.remove(); child.parentNode = this; this.childNodes.unshift(child); },
      replaceChildren(...children) {
        this.childNodes.slice().forEach(child => child.remove());
        children.forEach(child => this.appendChild(child));
      },
      remove() {
        if (!this.parentNode) return;
        const root = this.getRootNode();
        if (this.contains(root.activeElement)) root.activeElement = null;
        const siblings = this.parentNode.childNodes;
        siblings.splice(siblings.indexOf(this), 1);
        this.parentNode = null;
      },
      contains(child) { return child === this || this.childNodes.some(node => node.contains(child)); },
      getRootNode() { return this.parentNode ? this.parentNode.getRootNode() : this; },
      get isConnected() { return this === document.documentElement || !!(this.parentNode || this.host)?.isConnected; },
      getBoundingClientRect() { return { left: 0, top: 0, bottom: 28, width: 380, height: 28 }; },
      querySelectorAll(selector) {
        const nodes = [];
        function visit(parent) {
          for (const child of parent.childNodes) {
            const base = selector.replace(/\[.*$/, "");
            const attr = selector.match(/\[([^=]+)="([^"]*)"\]/);
            const match = base.startsWith(".") ? child.classList.contains(base.slice(1)) : child.tagName === base;
            if (match && (!attr || child.getAttribute(attr[1]) === attr[2])) nodes.push(child);
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
      add(name) { if (!this.contains(name)) node.className += " " + name; },
      toggle(name, force) {
        if (force) this.add(name);
        else node.className = node.className.split(/\s+/).filter(value => value !== name).join(" ");
      }
    };
    return node;
  }
  document.createElement = element;
  document.createElementNS = (_, tag) => element(tag);
  document.createTextNode = text => { const node = element("#text"); node.textContent = text; return node; };
  document.documentElement = element("html");
  document.documentElement.activeElement = null;
  document.body = element("body");
  document.documentElement.appendChild(document.body);
  document.getElementById = id => {
    const visit = node => node.id === id ? node : node.childNodes.map(visit).find(Boolean);
    return visit(document.documentElement) || null;
  };
  const window = {
    ...events(), console,
    __updocPreviewBootstrap: { css: "/* preloaded panel styles */" },
    setTimeout, clearTimeout, requestAnimationFrame, cancelAnimationFrame,
    getComputedStyle() { return { transform: "none", maxHeight: "560px" }; },
    matchMedia() { return { matches: false, addEventListener() {} }; },
    UpdocDoc: {
      create() {
        const mascot = element("span");
        return { element: mascot, setState(state) { mascot.dataset.state = state; },
          stopCelebration() {}, celebrate() {}, refreshGaze() {} };
      }
    }
  };
  const context = vm.createContext({ window, document, AbortController,
    fetch(url, init) {
      return new Promise((resolve, reject) => {
        requests.push({ url, init, resolve, reject });
        init.signal.addEventListener("abort", () => reject(new Error("aborted")));
      });
    }
  });
  for (const filename of ["panel-primitives.js", "panel.js"]) {
    vm.runInContext(fs.readFileSync(path.join(assets, filename), "utf8"), context, { filename });
  }
  const api = window.__updocPreviewPanel;
  api.initialize(status, view);
  const root = document.getElementById("__updoc_status").shadowRoot;
  function settle() {
    for (let i = 0; i < 10; i++) {
      const currentFrames = [...frames.values()];
      frames.clear();
      currentFrames.forEach(fn => fn());
      for (const [id, timer] of [...timers]) {
        if (timer.delay < 1000) { timers.delete(id); timer.fn(); }
      }
      if (!frames.size && ![...timers.values()].some(timer => timer.delay < 1000)) return;
    }
    throw new Error("Panel animations did not settle");
  }
  settle();
  return {
    api, root, document, requests, timers, settle,
    shell: () => root.querySelector(".updoc-shell"),
    button: label => root.querySelectorAll("button").find(node =>
      node.textContent === label || node.getAttribute("aria-label") === label),
    fireTimer(delay) {
      const entry = [...timers].find(([, timer]) => timer.delay === delay);
      assert.ok(entry, `a ${delay} ms timer is pending`);
      timers.delete(entry[0]); entry[1].fn();
    }
  };
}

for (const [name, status] of [
  ["ready", ready], ["error", error],
  ["single warning", { phase: "ready", warnings: [warning] }],
  ["multiple warnings", { phase: "ready", warnings: [warning, { summary: "Another warning" }] }],
  ["error with warnings", { ...error, warnings: [warning] }]
]) {
  test(`${name} offers a working Rebuild action after minimizing and reopening`, async () => {
    const app = browser(status);
    assert.ok(app.button("Rebuild"));
    app.button(name === "ready" ? "Minimize preview controls" : "Minimize updoc message").click();
    app.settle();
    assert.equal(app.button("Rebuild"), undefined);
    app.shell().dispatch("keydown", { key: "Enter" });
    app.settle();
    const button = app.button("Rebuild");
    button.click();
    button.click();
    assert.equal(app.requests.length, 1, "disabled controls cannot submit duplicate requests");
    assert.equal(button.getAttribute("aria-busy"), "true");
    const request = app.requests[0];
    assert.equal(request.url, "/__updoc__/rebuild");
    assert.equal(request.init.method, "POST");
    assert.equal(request.init.headers["X-Updoc-Request"], "rebuild");
    request.resolve({ ok: true, json: async () => building });
    await tick(); app.settle();
    assert.equal(app.shell().dataset.updocState, "building");
    assert.equal([...app.timers.values()].some(timer => timer.delay === 10000), false);
  });
}

for (const failure of ["HTTP failure", "network failure", "invalid response", "request timeout"]) {
  test(`${failure} shows an announced message and allows retry`, async () => {
    const app = browser(error);
    const button = app.button("Rebuild");
    button.click();
    const request = app.requests[0];
    if (failure === "HTTP failure") request.resolve({ ok: false });
    else if (failure === "network failure") request.reject(new Error("offline"));
    else if (failure === "invalid response") request.resolve({ ok: true, json: async () => { throw new Error("invalid JSON"); } });
    else app.fireTimer(10000);
    await tick();
    assert.equal(button.disabled, false);
    assert.equal(button.getAttribute("aria-busy"), null);
    const message = app.root.querySelector(".updoc-ready-actions").querySelector(".updoc-value");
    assert.equal(message.hidden, false);
    assert.equal(message.getAttribute("role"), "alert");
    assert.equal(message.textContent, "Could not start rebuild. Try again.");
    button.click();
    assert.equal(app.requests.length, 2);
    assert.equal(message.hidden, true);
    app.requests[1].resolve({ ok: true, json: async () => building });
    await tick(); app.settle();
    assert.equal(app.shell().dataset.updocState, "building");
  });
}

test("a delayed rebuild response cannot replace a newer status", async () => {
  const app = browser();
  app.button("Rebuild").click();
  app.api.render(error); app.settle();
  app.requests[0].resolve({ ok: true, json: async () => building });
  await tick(); app.settle();
  assert.equal(app.shell().dataset.updocState, "error-full");
  assert.match(app.shell().textContent, /A transient build failure/);
});

test("keyboard focus follows expanded and minimized controls", () => {
  const app = browser(ready, { readyExpanded: false });
  app.shell().focus();
  app.shell().dispatch("keydown", { key: " " });
  app.settle();
  const minimize = app.button("Minimize preview controls");
  assert.equal(app.root.activeElement, minimize);
  minimize.click(); app.settle();
  assert.equal(app.root.activeElement, app.shell());
  assert.equal(app.shell().getAttribute("tabindex"), "0");
});

test("removing focused controls for a build retains focus in the drawer", () => {
  const app = browser(error);
  app.button("Rebuild").focus();
  app.api.render(building); app.settle();
  assert.equal(app.root.activeElement, app.shell());
  assert.equal(app.shell().getAttribute("tabindex"), "-1");
  app.api.render(ready); app.settle();
  assert.equal(app.root.activeElement, app.shell());
  assert.equal(app.shell().getAttribute("tabindex"), "0");
});

test("status updates do not steal focus from the documentation", () => {
  const app = browser();
  const outside = app.document.createElement("button");
  app.document.body.appendChild(outside);
  outside.focus();
  app.api.render(error); app.settle();
  assert.equal(app.document.activeElement, outside);
});

test("status announcements exclude progress ticks and drawer controls", () => {
  const app = browser(building);
  const host = app.document.getElementById("__updoc_status");
  const announcement = app.root.querySelector(".updoc-announcement");
  assert.equal(host.getAttribute("aria-live"), null);
  assert.equal(host.getAttribute("role"), null);
  assert.equal(announcement.getAttribute("role"), "status");
  assert.equal(announcement.getAttribute("aria-live"), "polite");
  assert.equal(announcement.getAttribute("aria-atomic"), "true");
  assert.equal(app.shell().contains(announcement), false);
  assert.equal(announcement.textContent, "Building documentation.");
  const initialUpdates = announcement.textUpdates;

  for (let current = 1; current <= 3; current++) {
    app.api.render({ phase: "building", queued: current === 3, progress: {
      operation: "Building reference", target: `topic-${current}`, current, total: 3
    } });
    app.settle();
    assert.match(app.shell().textContent, new RegExp(`topic-${current}`));
    assert.equal(announcement.textUpdates, initialUpdates);
  }

  app.api.render(error); app.settle();
  assert.equal(announcement.textContent, "Build failed. A transient build failure");
  assert.equal(announcement.textUpdates, initialUpdates + 1);
  assert.equal(app.root.querySelector(".ui-alert").getAttribute("role"), null);
  app.button("Minimize updoc message").click(); app.settle();
  app.shell().dispatch("keydown", { key: "Enter" }); app.settle();
  assert.equal(announcement.textUpdates, initialUpdates + 1);

  app.api.render({ ...error, error: { summary: "Another failure" } }); app.settle();
  assert.equal(announcement.textContent, "Build failed. Another failure");
  app.api.render({ phase: "ready", warnings: [warning] }); app.settle();
  assert.equal(announcement.textContent, "Built with 1 warning. A build warning");
  assert.equal(app.root.querySelector(".ui-alert").getAttribute("role"), null);
  app.api.render({ phase: "ready", warnings: [warning, { summary: "Another warning" }] }); app.settle();
  assert.equal(announcement.textContent, "Built with 2 warnings. A build warning; Another warning");
  app.api.render(ready); app.settle();
  assert.equal(announcement.textContent, "Watching for changes.");
});

test("disconnect and reconnect are announced without repeating retries", () => {
  const app = browser(ready);
  const announcement = app.root.querySelector(".updoc-announcement");
  app.api.renderDisconnected(); app.settle();
  assert.equal(announcement.textContent, "Preview disconnected. Retrying automatically.");
  const disconnectedUpdates = announcement.textUpdates;
  app.api.showRetrying();
  app.api.renderDisconnected(); app.settle();
  assert.equal(announcement.textUpdates, disconnectedUpdates);
  app.api.render(ready); app.settle();
  assert.equal(announcement.textContent, "Watching for changes.");
  assert.equal(announcement.textUpdates, disconnectedUpdates + 1);
});

test("warning text has sufficient contrast in both themes", () => {
  const css = fs.readFileSync(path.join(assets, "panel.css"), "utf8");
  function luminance(color) {
    const components = color.match(/^oklch\(([\d.]+) ([\d.]+) ([\d.]+)\)$/);
    assert.ok(components, `Expected an OKLCH color, got ${color}`);
    const [L, C, H] = components.slice(1).map(Number);
    const a = C * Math.cos(H * Math.PI / 180);
    const b = C * Math.sin(H * Math.PI / 180);
    const l = (L + .3963377774 * a + .2158037573 * b) ** 3;
    const m = (L - .1055613458 * a - .0638541728 * b) ** 3;
    const s = (L - .0894841775 * a - 1.291485548 * b) ** 3;
    const rgb = [
      4.0767416621 * l - 3.3077115913 * m + .2309699292 * s,
      -1.2684380046 * l + 2.6097574011 * m - .3413193965 * s,
      -.0041960863 * l - .7034186147 * m + 1.707614701 * s
    ].map(value => Math.max(0, Math.min(1, value)));
    return .2126 * rgb[0] + .7152 * rgb[1] + .0722 * rgb[2];
  }
  const palettes = [...css.matchAll(/[^{}]*:host[^{}]*\{([^{}]+)\}/g)];
  assert.equal(palettes.length, 2);
  for (const [, palette] of palettes) {
    const foreground = luminance(palette.match(/--warning:\s*([^;]+);/)[1]);
    const background = luminance(palette.match(/--card:\s*([^;]+);/)[1]);
    const contrast = (Math.max(foreground, background) + .05) /
      (Math.min(foreground, background) + .05);
    assert.ok(contrast >= 4.5, `Warning contrast ${contrast.toFixed(2)} is below 4.5:1`);
  }
  const description = css.match(/\.ui-alert--warning \.ui-alert-description\s*\{([^}]+)\}/)[1];
  assert.match(description, /color:\s*var\(--warning\);/);
});
