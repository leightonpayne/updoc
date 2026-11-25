"use strict";

// No browser dependencies: drive the real component with a small DOM/event
// harness. The optional asset directory also exercises installed package files.
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const test = require("node:test");
const vm = require("node:vm");
const { createEventTarget, createScheduler } = require("./harness.cjs");
const assets = process.argv[2] || path.resolve(__dirname, "../../inst/mascot");
const source = fs.readFileSync(path.join(assets, "doc.js"), "utf8");

function browser(options = {}) {
  const targets = [];
  const { timers, frames, setTimeout, clearTimeout, requestAnimationFrame, cancelAnimationFrame } =
    createScheduler();
  const operations = [];
  let mutations = 0;

  const eventTarget = () => createEventTarget({ registry: targets });

  const document = { ...eventTarget(), hidden: false };
  function element(tagName, namespaceURI = null) {
    const attributes = new Map();
    const properties = new Map();
    const data = {};
    const node = {
      ...eventTarget(), tagName, namespaceURI, ownerDocument: document,
      children: [], parentNode: null, textContent: "",
      dataset: new Proxy(data, {
        set(target, key, value) { mutations++; target[key] = String(value); return true; },
        deleteProperty(target, key) { mutations++; delete target[key]; return true; }
      }),
      style: {
        setProperty(name, value) {
          if (name.startsWith("--doc-look-") || name === "--doc-idle-weight") operations.push("write");
          mutations++; properties.set(name, String(value));
        },
        removeProperty(name) { mutations++; properties.delete(name); },
        getPropertyValue(name) { return properties.get(name) || ""; }
      },
      setAttribute(name, value) {
        mutations++;
        attributes.set(name, String(value));
        if (name.startsWith("data-")) {
          data[name.slice(5).replace(/-([a-z])/g, (_, c) => c.toUpperCase())] = String(value);
        }
      },
      getAttribute(name) {
        if (name.startsWith("data-")) {
          return data[name.slice(5).replace(/-([a-z])/g, (_, c) => c.toUpperCase())] ?? null;
        }
        return attributes.get(name) ?? null;
      },
      removeAttribute(name) {
        mutations++;
        attributes.delete(name);
        if (name.startsWith("data-")) {
          delete data[name.slice(5).replace(/-([a-z])/g, (_, c) => c.toUpperCase())];
        }
      },
      appendChild(child) {
        if (child.parentNode) child.remove();
        this.children.push(child);
        child.parentNode = this;
        mutations++;
        return child;
      },
      removeChild(child) {
        assert.equal(child.parentNode, this, "only an attached child can be removed");
        child.remove();
        return child;
      },
      remove() {
        if (!this.parentNode) return;
        const siblings = this.parentNode.children;
        siblings.splice(siblings.indexOf(this), 1);
        this.parentNode = null;
        mutations++;
      },
      get isConnected() {
        return this === document.body || !!this.parentNode?.isConnected;
      },
      get firstChild() { return this.children[0] || null; },
      getBoundingClientRect() {
        operations.push("read");
        return this.rect || { left: 100, top: 100, width: 48, height: 48 };
      },
      querySelector(selector) { return this.querySelectorAll(selector)[0] || null; },
      querySelectorAll(selector) {
        const nodes = [];
        function visit(parent) {
          for (const child of parent.children) {
            const matches = selector.startsWith(".") ?
              (child.className || child.getAttribute("class") || "").split(/\s+/).includes(selector.slice(1)) :
              child.tagName === selector;
            if (matches) nodes.push(child);
            visit(child);
          }
        }
        visit(this);
        return nodes;
      }
    };
    return node;
  }
  document.createElement = tag => element(tag);
  document.createElementNS = (namespace, tag) => element(tag, namespace);
  document.body = element("body");
  const media = { ...eventTarget(), matches: !!options.reducedMotion };
  if (options.legacyMedia) {
    const add = media.addEventListener.bind(media);
    const remove = media.removeEventListener.bind(media);
    media.addListener = callback => add("change", callback);
    media.removeListener = callback => remove("change", callback);
    delete media.addEventListener;
    delete media.removeEventListener;
  }
  const window = {
    ...eventTarget(), document,
    setTimeout, clearTimeout, requestAnimationFrame, cancelAnimationFrame,
    matchMedia: () => media
  };
  const context = { window, document, console, module: { exports: {} } };
  vm.runInNewContext(source, context, { filename: "doc.js" });
  const api = context.module.exports;
  return {
    api, window, document, media, timers, frames, operations,
    mutations: () => mutations,
    listenerCount: () => targets.reduce((sum, target) => sum + target.listenerCount(), 0),
    reloadAPI() {
      vm.runInNewContext(source, context, { filename: "doc.js" });
      return context.module.exports;
    },
    create(settings = {}) {
      const mascot = api.create(settings);
      document.body.appendChild(mascot.element);
      return mascot;
    },
    pointer(x, y, pointerType = "mouse") {
      window.dispatch("pointermove", { clientX: x, clientY: y, pointerType });
    },
    paint() {
      const callbacks = [...frames.values()];
      frames.clear();
      callbacks.forEach(callback => callback());
    },
    settleGaze() {
      for (let n = 0; frames.size && n < 240; n++) this.paint();
      assert.equal(frames.size, 0, "gaze settles and stops requesting frames");
    },
    finishTimers() {
      const callbacks = [...timers.values()];
      timers.clear();
      callbacks.forEach(timer => timer.fn());
    },
    reduceMotion(value) {
      media.matches = value;
      media.dispatch("change", { matches: value });
    }
  };
}

test("state updates preserve the same face and repeated progress does not restart it", () => {
  const app = browser();
  const doc = app.create({ state: "building" });
  const face = doc.element.querySelector("svg");
  const originalNodes = face.querySelectorAll("g");
  const before = app.mutations();
  for (let progress = 0; progress < 20; progress++) doc.setState("building");
  assert.equal(app.mutations(), before, "identical state causes no DOM/style mutation");
  for (const state of ["warning", "error", "disconnected", "ready", "building"]) {
    doc.setState(state);
    assert.equal(doc.element.dataset.state, state);
    assert.equal(doc.element.querySelector("svg"), face);
    assert.deepEqual(face.querySelectorAll("g"), originalNodes);
  }
  doc.destroy();
});

test("celebration is a single event that survives unchanged ready updates", () => {
  const app = browser();
  const doc = app.create();
  assert.equal(doc.celebrate(), true);
  const timer = [...app.timers.keys()][0];
  assert.equal(doc.element.dataset.celebrating, "true");
  doc.setState("ready");
  assert.equal(doc.celebrate(), false, "another event cannot restart an active celebration");
  assert.deepEqual([...app.timers.keys()], [timer]);
  app.finishTimers();
  assert.notEqual(doc.element.dataset.celebrating, "true");
  assert.equal(doc.element.dataset.state, "ready");
  assert.equal(doc.celebrate(), true, "a later completed build can celebrate again");
  doc.stopCelebration();
  assert.equal(app.timers.size, 0);
  assert.notEqual(doc.element.dataset.celebrating, "true");
  doc.destroy();
});

test("new status cancels celebration and non-ready states cannot celebrate", () => {
  const app = browser();
  const doc = app.create();
  for (const state of ["building", "warning", "error", "disconnected"]) {
    doc.setState("ready");
    doc.celebrate();
    doc.setState(state);
    assert.notEqual(doc.element.dataset.celebrating, "true");
    assert.equal(app.timers.size, 0);
    assert.equal(doc.celebrate(), false);
  }
  doc.destroy();
});

test("only the celebration animation ending clears its completion expression", () => {
  const app = browser();
  const doc = app.create();
  const layer = doc.element.querySelector(".doc-celebration");
  doc.celebrate();
  layer.dispatch("animationend", { animationName: "doc-blink" });
  layer.dispatch("animationend", { animationName: "doc-celebrate", target: doc.element });
  assert.equal(doc.element.dataset.celebrating, "true");
  layer.dispatch("animationend", { animationName: "doc-celebrate" });
  assert.notEqual(doc.element.dataset.celebrating, "true");
  assert.equal(app.timers.size, 0);
  doc.destroy();
});

test("pointer tracking coalesces events and stays inside the face at every size", () => {
  const app = browser();
  const doc = app.create();
  for (const size of [28, 44, 160]) {
    doc.element.rect = { left: 100, top: 100, width: size, height: size };
    for (const direction of [-1, 1]) {
      app.pointer(direction * 10000, direction * 10000);
      app.pointer(direction * 20000, direction * 20000);
      assert.equal(app.frames.size, 1, "pointer events share a pending paint");
      app.paint();
      app.settleGaze();
      const x = parseFloat(doc.element.style.getPropertyValue("--doc-look-x"));
      const y = parseFloat(doc.element.style.getPropertyValue("--doc-look-y"));
      assert.ok(x * direction > 3.5 && Math.abs(x) <= 5.5, `horizontal gaze remains pronounced and bounded: ${x}`);
      assert.ok(y * direction > 2.5 && Math.abs(y) <= 4, `vertical gaze remains pronounced and bounded: ${y}`);
      assert.ok(Math.hypot(x / 5.5, y / 4) <= 1.001, "diagonal gaze stays in the same ellipse");
      assert.equal(doc.element.dataset.tracking, "true");
    }
  }
  doc.destroy();
});

test("gaze refresh follows a moved mascot without another pointer event", () => {
  const app = browser();
  const doc = app.create();
  app.pointer(200, 124);
  app.paint();
  const initial = parseFloat(doc.element.style.getPropertyValue("--doc-look-x"));
  doc.element.rect = { left: 300, top: 100, width: 48, height: 48 };
  doc.refreshGaze();
  app.paint();
  const moved = parseFloat(doc.element.style.getPropertyValue("--doc-look-x"));
  assert.ok(initial > 0 && moved < 0, "gaze uses the new screen position");
  doc.destroy();
});

test("touch, leaving the window, hidden pages and gaze disabling settle back to idle", () => {
  const app = browser();
  const doc = app.create();
  for (const reset of [
    () => app.pointer(500, 500, "touch"),
    () => app.window.dispatch("pointerout", { relatedTarget: null }),
    () => app.window.dispatch("blur"),
    () => { app.document.hidden = true; app.document.dispatch("visibilitychange"); },
    () => doc.setGaze("none")
  ]) {
    app.document.hidden = false;
    doc.setGaze("pointer");
    app.pointer(500, 500);
    app.paint();
    assert.equal(doc.element.dataset.tracking, "true");
    app.pointer(600, 600);
    reset();
    app.settleGaze();
    assert.equal(app.frames.size, 0);
    assert.notEqual(doc.element.dataset.tracking, "true");
    assert.equal(doc.element.style.getPropertyValue("--doc-look-x"), "");
    assert.equal(doc.element.style.getPropertyValue("--doc-look-y"), "");
  }
  app.pointer(500, 500);
  assert.equal(app.frames.size, 0, "disabled gaze stays disabled");
  doc.destroy();
});

test("pointer attention eases in and hands back to the running idle motion", () => {
  const app = browser();
  const doc = app.create();
  app.pointer(500, 124);
  app.paint();
  const first = parseFloat(doc.element.style.getPropertyValue("--doc-look-x"));
  assert.ok(first > 0);
  assert.ok(parseFloat(doc.element.style.getPropertyValue("--doc-idle-weight")) > 0.9);
  app.settleGaze();
  const held = parseFloat(doc.element.style.getPropertyValue("--doc-look-x"));
  assert.ok(held > first);
  assert.equal(doc.element.style.getPropertyValue("--doc-idle-weight"), "0.0000");
  app.window.dispatch("pointerout", { relatedTarget: null });
  assert.equal(parseFloat(doc.element.style.getPropertyValue("--doc-look-x")), held, "leaving does not snap the eyes");
  app.paint();
  const returning = parseFloat(doc.element.style.getPropertyValue("--doc-look-x"));
  assert.ok(returning > 0 && returning < held);
  assert.ok(parseFloat(doc.element.style.getPropertyValue("--doc-idle-weight")) > 0);
  app.settleGaze();
  assert.notEqual(doc.element.dataset.tracking, "true");
  assert.equal(doc.element.style.getPropertyValue("--doc-idle-weight"), "");
  doc.destroy();
});

test("mascots share pointer listeners and batch all measurements before gaze writes", () => {
  const app = browser();
  const mascots = [app.create(), app.create(), app.create()];
  assert.equal(app.window.listenerCount("pointermove"), 1);
  app.pointer(500, 500);
  app.pointer(600, 500);
  assert.equal(app.frames.size, 1);
  app.operations.length = 0;
  app.paint();
  assert.deepEqual(app.operations.slice(0, 3), ["read", "read", "read"]);
  assert.ok(app.operations.slice(3).every(operation => operation === "write"));
  app.settleGaze();
  mascots[0].destroy();
  assert.equal(app.window.listenerCount("pointermove"), 1);
  mascots[1].destroy();
  mascots[2].destroy();
  assert.equal(app.window.listenerCount("pointermove"), 0);
  assert.equal(app.listenerCount(), 0);
});

test("explicit seeds reproduce motion phases and defaults separate neighboring mascots", () => {
  const app = browser();
  const first = app.create({ seed: "readme-ready" });
  const same = app.create({ seed: "readme-ready" });
  for (const key of Object.keys(app.api.motionSettings("readme-ready"))) {
    assert.equal(first.element.style.getPropertyValue(key), same.element.style.getPropertyValue(key));
  }
  const one = app.create(), two = app.create();
  assert.notEqual(one.element.style.getPropertyValue("--doc-blink-duration"), two.element.style.getPropertyValue("--doc-blink-duration"));
  for (const mascot of [first, same, one, two]) mascot.destroy();
});

test("homepage and injected preview copies share the same pointer stream", () => {
  const app = browser();
  const first = app.create();
  const second = app.reloadAPI().create();
  app.document.body.appendChild(second.element);
  assert.equal(app.window.listenerCount("pointermove"), 1);
  app.pointer(500, 500);
  assert.equal(app.frames.size, 1);
  app.settleGaze();
  assert.equal(first.element.dataset.tracking, "true");
  assert.equal(second.element.dataset.tracking, "true");
  first.destroy();
  assert.equal(app.window.listenerCount("pointermove"), 1);
  second.destroy();
  assert.equal(app.listenerCount(), 0);
  assert.equal(app.frames.size, 0);
});

for (const legacyMedia of [false, true]) {
  test(`reduced motion follows ${legacyMedia ? "legacy" : "current"} media preference changes`, () => {
    const app = browser({ legacyMedia });
    const doc = app.create();
    app.pointer(500, 500);
    app.paint();
    doc.celebrate();
    app.reduceMotion(true);
    assert.equal(doc.element.dataset.motion, "reduce");
    assert.notEqual(doc.element.dataset.tracking, "true");
    app.pointer(500, 500);
    assert.equal(app.frames.size, 0);
    app.reduceMotion(false);
    assert.equal(doc.element.dataset.motion, "auto");
    app.pointer(500, 500);
    assert.equal(app.frames.size, 1);
    doc.setMotion("reduce");
    assert.equal(app.frames.size, 0);
    app.reduceMotion(false);
    assert.equal(doc.element.dataset.motion, "reduce", "explicit reduction overrides system preference");
    doc.setMotion("auto");
    assert.equal(doc.element.dataset.motion, "auto");
    doc.destroy();
    assert.equal(app.listenerCount(), 0, "including media subscriptions");
  });
}

test("reduced motion still provides a temporary completion expression", () => {
  const app = browser({ reducedMotion: true });
  const doc = app.create();
  assert.equal(doc.element.dataset.motion, "reduce");
  assert.equal(doc.celebrate(), true);
  assert.equal(doc.element.dataset.celebrating, "true");
  app.finishTimers();
  assert.notEqual(doc.element.dataset.celebrating, "true");
  doc.destroy();
});

test("destroy removes the element, listeners and pending work, and is safe twice", () => {
  const app = browser();
  const doc = app.create();
  app.pointer(500, 500);
  doc.celebrate();
  assert.ok(app.listenerCount() > 0);
  assert.equal(app.timers.size, 1);
  assert.equal(app.frames.size, 1);
  doc.destroy();
  doc.destroy();
  assert.equal(doc.element.isConnected, false);
  assert.equal(app.listenerCount(), 0);
  assert.equal(app.timers.size, 0);
  assert.equal(app.frames.size, 0);
  const after = app.mutations();
  doc.setState("building");
  doc.setMotion("reduce");
  doc.setGaze("none");
  doc.celebrate();
  doc.stopCelebration();
  doc.refreshGaze();
  app.pointer(100, 100);
  app.reduceMotion(true);
  assert.equal(app.mutations(), after, "a disposed component cannot start work again");
  assert.equal(app.timers.size + app.frames.size, 0);
});

test("disposing one mascot preserves another mascot's tracking and celebration", () => {
  const app = browser();
  const first = app.create();
  const second = app.create();
  first.celebrate();
  second.celebrate();
  first.destroy();
  assert.equal(app.timers.size, 1);
  app.pointer(500, 500);
  app.paint();
  assert.equal(second.element.dataset.tracking, "true");
  assert.equal(second.element.dataset.celebrating, "true");
  second.destroy();
  assert.equal(app.listenerCount(), 0);
  assert.equal(app.timers.size + app.frames.size, 0);
});

test("invalid inputs fail before replacing the current expression", () => {
  const app = browser();
  const doc = app.create({ state: "warning" });
  for (const [method, value] of [["setState", "complete"], ["setMotion", "fast"], ["setGaze", "touch"]]) {
    assert.throws(() => doc[method](value), { name: "TypeError" });
  }
  assert.equal(doc.element.dataset.state, "warning");
  doc.destroy();
});

test("the component exports a browser API and can generate static SVG without a DOM", () => {
  const browserWindow = {};
  vm.runInNewContext(source, { window: browserWindow }, { filename: "doc.js" });
  assert.equal(typeof browserWindow.UpdocDoc.create, "function");
  assert.equal(typeof browserWindow.UpdocDoc.svg, "function");
  const commonjs = { module: { exports: {} } };
  vm.runInNewContext(source, commonjs, { filename: "doc.js" });
  const markup = commonjs.module.exports.svg({ state: "ready", theme: "light", label: "Doc" });
  assert.match(markup, /^<svg\b/);
  assert.match(markup, /xmlns="http:\/\/www\.w3\.org\/2000\/svg"/);
  assert.match(markup, /viewBox="0 0 48 48"/);
  assert.match(markup, /<\/svg>$/);
  assert.doesNotMatch(markup, /<script\b|@keyframes|<animate\b/);
});

test("static assets share the live face geometry and safely escape their accessible label", () => {
  const app = browser();
  const doc = app.create();
  const facePath = doc.element.querySelector(".doc-face").getAttribute("d");
  const label = 'Doc <mascot> & "friend"';
  const outputs = new Set();
  for (const state of ["ready", "building", "warning", "error", "disconnected"]) {
    for (const theme of ["light", "dark"]) {
      const markup = app.api.svg({ state, theme, label });
      assert.ok(markup.includes(`d="${facePath}"`), "static exports use the canonical face");
      assert.ok(markup.includes("&lt;mascot&gt;"));
      assert.ok(markup.includes("&amp;"));
      assert.ok(markup.includes("&quot;friend&quot;"));
      assert.doesNotMatch(markup, /<mascot>/);
      outputs.add(markup);
    }
  }
  assert.equal(outputs.size, 10, "each state and theme gets its own useful static expression");
  doc.destroy();
});
