"use strict";

// Run directly with Node; the optional directory lets R CMD check exercise the
// installed package's assets rather than depending on a source checkout.
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const test = require("node:test");
const vm = require("node:vm");
const { createEventTarget } = require("./harness.cjs");
const assets = process.argv[2] || path.resolve(__dirname, "../../inst/preview");
const source = fs.readFileSync(path.join(assets, "reload.js"), "utf8");
const refreshKey = "__updoc_refresh";
const now = 1_000_000;
const plain = value => JSON.parse(JSON.stringify(value));
const ready = (version = 1, extra = {}) => ({
  version, phase: "ready", warnings: [], queued: false, ...extra
});
const changed = (version, paths, extra = {}) => ready(version, {
  reload: { full: false, paths }, ...extra
});

function browser(options = {}) {
  const calls = [];
  const pending = [];
  const timers = [];
  const animationFrames = [];
  const refreshCallbacks = [];
  const storage = options.storage || new Map();
  const nodes = options.nodes || [];
  const entries = options.entries || [];
  const location = new URL(options.url || "http://localhost:4321/");
  location.reload = () => calls.push(["reload"]);
  const panel = {};
  for (const name of ["initialize", "render", "celebrateCompletion",
    "stopRetryCountdown", "showRetrying", "renderDisconnected", "showRetryCountdown"]) {
    panel[name] = (...args) => calls.push([name, ...args]);
  }
  panel.viewState = () => options.view || { expanded: true, dismissed: false };
  if (options.beforeRefresh) {
    panel.beforeRefresh = callback => {
      calls.push(["beforeRefresh"]);
      refreshCallbacks.push(callback);
    };
  }
  const sessionStorage = {};
  for (const name of ["getItem", "setItem", "removeItem"]) {
    sessionStorage[name] = (...args) => {
      if (options.storageBlocked) throw new Error("Storage blocked");
      if (name === "getItem") return storage.get(args[0]) || null;
      if (name === "setItem") storage.set(...args);
      if (name === "removeItem") storage.delete(args[0]);
    };
  }
  let observer;
  const eventTarget = () => createEventTarget();
  const window = {
    ...eventTarget(),
    __updocPreviewPanel: panel,
    location,
    console,
    sessionStorage,
    performance: { getEntriesByType: () => entries },
    setTimeout: (fn, delay) => { const timer = { fn, delay }; timers.push(timer); return timer; },
    clearTimeout: timer => { const index = timers.indexOf(timer); if (index !== -1) timers.splice(index, 1); }
  };
  if (options.animationFrames) {
    window.requestAnimationFrame = fn => { const request = { fn }; animationFrames.push(request); return request; };
    window.cancelAnimationFrame = request => {
      const index = animationFrames.indexOf(request);
      if (index !== -1) animationFrames.splice(index, 1);
    };
  }
  if (options.bootstrap !== false) {
    window.__updocPreviewBootstrap = { status: options.bootstrap || ready() };
  }
  const document = {
    ...eventTarget(),
    readyState: options.readyState || "complete",
    hidden: !!options.hidden,
    querySelectorAll: () => nodes
  };
  const context = {
    window, URL, Set, AbortController: options.abortUnsupported ? undefined : AbortController,
    Date: class extends Date { static now() { return now; } },
    document,
    fetch: (url, init) => {
      calls.push(["fetch", url, init]);
      return new Promise((resolve, reject) => pending.push({ resolve, reject }));
    },
    PerformanceObserver: class {
      constructor(callback) { observer = callback; }
      observe() {}
    }
  };
  vm.runInNewContext(source, context, { filename: "reload.js" });

  return {
    calls, timers, pending, animationFrames, refreshCallbacks, storage, nodes, entries, window, document,
    count: name => calls.filter(call => call[0] === name).length,
    of: name => calls.filter(call => call[0] === name),
    observe: (...urls) => observer({ getEntries: () => urls.map(name => ({ name })) }),
    paint() {
      assert.ok(animationFrames.length, "a browser animation frame is pending");
      // Callbacks requested from a frame belong to the next browser paint.
      animationFrames.splice(0).forEach(request => request.fn());
    },
    fireTimer(delay) {
      const index = timers.findIndex(timer => timer.delay === delay);
      assert.notEqual(index, -1, `a ${delay} ms timer is pending`);
      timers.splice(index, 1)[0].fn();
    },
    finishRefresh() {
      assert.equal(refreshCallbacks.length, 1, "one geometry gate is pending");
      refreshCallbacks.shift()();
    },
    async poll(value) {
      if (!pending.length) {
        assert.ok(timers.length, "the previous poll scheduled another request");
        timers.shift().fn();
      }
      const request = pending.shift();
      assert.ok(request, "a status request is pending");
      if (value instanceof Error) request.reject(value);
      else request.resolve({ ok: true, json: () => Promise.resolve(value) });
      await new Promise(setImmediate);
    }
  };
}

test("bootstrap paints synchronously while the first status request is pending", async () => {
  const app = browser({ bootstrap: ready(8) });
  assert.equal(app.count("initialize"), 1);
  assert.equal(app.calls[0][0], "initialize");
  assert.equal(app.of("initialize")[0][1].version, 8);
  assert.equal(app.count("fetch"), 1);
  assert.equal(app.count("reload"), 0);
  assert.equal(app.window.__updocPreviewBootstrap, undefined);
  await app.poll(ready(8));
  await app.poll(ready(8));
  assert.equal(app.count("initialize"), 1, "ordinary polls do not reinitialize the popup");
  assert.equal(app.count("render"), 2);
});

test("without bootstrap the first response initializes the panel once", async () => {
  const app = browser({ bootstrap: false });
  assert.equal(app.count("initialize"), 0);
  await app.poll(ready(4));
  await app.poll(ready(4));
  assert.equal(app.count("initialize"), 1);
  assert.equal(app.count("render"), 1);
});

for (const abortUnsupported of [false, true]) {
  test(`a stalled status request retries and ignores late replies (abort unavailable: ${abortUnsupported})`, async () => {
    const app = browser({ abortUnsupported });
    const stalled = app.pending.shift();
    app.fireTimer(10000);
    await new Promise(setImmediate);
    if (!abortUnsupported) assert.equal(app.of("fetch")[0][2].signal.aborted, true);
    assert.equal(app.count("render"), 0);
    assert.deepEqual(app.timers.map(timer => timer.delay), [750]);
    app.fireTimer(750);
    await app.poll(ready(1));
    assert.equal(app.count("render"), 1);
    stalled.resolve({ ok: true, json: () => Promise.resolve(changed(2, ["index.html"])) });
    await new Promise(setImmediate);
    assert.equal(app.count("reload"), 0, "an expired request cannot trigger a stale reload");
    assert.equal(app.count("render"), 1);
    assert.deepEqual(app.timers.map(timer => timer.delay), [750]);
  });
}

test("repeated stalled status requests show disconnection and recover", async () => {
  const app = browser();
  for (let failure = 0; failure < 3; failure++) {
    app.pending.shift();
    app.fireTimer(10000);
    await new Promise(setImmediate);
    if (failure < 2) app.fireTimer(750);
  }
  assert.equal(app.count("renderDisconnected"), 1);
  await app.poll(ready(1));
  assert.equal(app.count("render"), 1);
  assert.deepEqual(app.timers.map(timer => timer.delay), [750]);
});

test("the status deadline includes a stalled JSON body", async () => {
  const app = browser();
  let finishBody;
  app.pending.shift().resolve({ ok: true, json: () => new Promise(resolve => { finishBody = resolve; }) });
  await new Promise(setImmediate);
  app.fireTimer(10000);
  await new Promise(setImmediate);
  assert.deepEqual(app.timers.map(timer => timer.delay), [750]);
  await app.poll(ready(1));
  finishBody(changed(2, ["index.html"]));
  await new Promise(setImmediate);
  assert.equal(app.count("reload"), 0);
  assert.equal(app.count("render"), 1);
});

test("an initial missing article and a restored missing page reload when published", async () => {
  for (const phase of ["building", "ready"]) {
    const app = browser({
      url: "http://localhost:4321/articles/guide.html",
      bootstrap: ready(0, { phase })
    });
    await app.poll(changed(1, ["articles/guide.html"]));
    assert.equal(app.count("reload"), 1);
    assert.equal(app.timers.length, 0);
  }
});

test("unrelated article and unused search output preserve the current page", async () => {
  const app = browser();
  await app.poll(changed(2, ["articles/other.html", "search.json"]));
  await app.poll(changed(3, ["articles/another.html"]));
  assert.equal(app.count("reload"), 0, "each skipped reload still advances the observed version");
  assert.deepEqual(app.of("render").map(call => call[1].version), [2, 3]);
  assert.equal(app.storage.has(refreshKey), false);
});

const relevantCases = [
  ["current root document", {}, "index.html"],
  ["current nested document", { url: "http://localhost:4321/articles/example.html" }, "articles/example.html"],
  ["directory index and query string", { url: "http://localhost:4321/articles/?filter=one#example" }, "articles/index.html"],
  ["encoded current document", { url: "http://localhost:4321/articles/caf%C3%A9%20example.html?x=1" }, "articles/café example.html"],
  ["stylesheet", { nodes: [{ href: "/styles/site.css?v=2" }] }, "styles/site.css"],
  ["script", { nodes: [{ src: "/scripts/site.js?cache=4" }] }, "scripts/site.js"],
  ["responsive image", { nodes: [{ src: "/images/small.png", currentSrc: "http://localhost:4321/images/large.png" }] }, "images/large.png"],
  ["embedded object", { nodes: [{ data: "/images/chart.svg#view" }] }, "images/chart.svg"],
  ["CSS image from resource timing", { entries: [{ name: "http://localhost:4321/images/background.png" }] }, "images/background.png"],
  ["font from resource timing", { entries: [{ name: "http://localhost:4321/fonts/regular.woff2?v=1" }] }, "fonts/regular.woff2"],
  ["fetched widget data", { entries: [{ name: "http://localhost:4321/data/plot.json" }] }, "data/plot.json"],
  ["used search data", { entries: [{ name: "http://localhost:4321/search.json?q=range" }] }, "search.json"]
];
for (const [name, options, changedPath] of relevantCases) {
  test(`changes to ${name} refresh the document`, async () => {
    const app = browser(options);
    await app.poll(changed(2, [changedPath]));
    assert.equal(app.count("reload"), 1);
    assert.equal(app.count("render"), 0, "do not collapse or celebrate before navigation");
    assert.equal(app.timers.length, 0, "a committed reload does not launch another poll");
  });
}

test("observed resources survive timing-buffer eviction and ignore external origins", async () => {
  const app = browser();
  app.observe("http://localhost:4321/fonts/seen.woff2", "https://other.example/external.js");
  await app.poll(changed(2, ["external.js"]));
  assert.equal(app.count("reload"), 0);
  await app.poll(changed(3, ["fonts/seen.woff2"]));
  assert.equal(app.count("reload"), 1);
});

test("the preview status endpoint is not a page resource", async () => {
  const app = browser({ entries: [{ name: "http://localhost:4321/__updoc__/status" }] });
  await app.poll(changed(2, ["__updoc__/status"]));
  assert.equal(app.count("reload"), 0);
});

for (const [name, status] of [
  ["full build", ready(2, { reload: { full: true, paths: [] } })],
  ["missing change descriptor", ready(2)],
  ["missing path list", ready(2, { reload: { full: false } })],
  ["invalid path list", ready(2, { reload: { full: false, paths: "index.html" } })],
  ["missed build version", changed(3, ["articles/other.html"])],
  ["server version reset", changed(0, [])]
]) {
  test(`${name} conservatively refreshes the document`, async () => {
    const app = browser();
    await app.poll(status);
    assert.equal(app.count("reload"), 1);
  });
}

test("an empty targeted output delta does not refresh", async () => {
  const app = browser();
  await app.poll(changed(2, []));
  assert.equal(app.count("reload"), 0);
  assert.equal(app.count("render"), 1);
});

for (const [name, extra] of [["building", { phase: "building" }], ["queued", { queued: true }]]) {
  test(`${name} output waits for a settled build before refreshing`, async () => {
    const app = browser();
    await app.poll(changed(2, ["index.html"], extra));
    assert.equal(app.count("reload"), 0);
    assert.equal(app.count("render"), 1);
    await app.poll(changed(2, ["index.html"]));
    assert.equal(app.count("reload"), 1);
    assert.equal(app.count("render"), 1, "ready is not rendered immediately before the refresh");
  });
}

test("a necessary refresh saves the drawer and celebrates once after navigation", async () => {
  const storage = new Map();
  const view = { expanded: true, dismissed: false };
  const app = browser({ storage, view });
  await app.poll(changed(2, ["index.html"]));
  const marker = JSON.parse(storage.get(refreshKey));
  assert.deepEqual(marker, { path: "/", version: 2, time: now, view, celebrate: true });
  const nextPage = browser({ storage, bootstrap: ready(2) });
  assert.deepEqual(plain(nextPage.of("initialize")[0][2]), view);
  assert.equal(nextPage.count("celebrateCompletion"), 1);
  assert.equal(storage.has(refreshKey), false);
  await nextPage.poll(ready(2));
  assert.equal(nextPage.count("celebrateCompletion"), 1);
  const laterPage = browser({ storage, bootstrap: ready(2) });
  assert.equal(laterPage.count("celebrateCompletion"), 0);
  assert.equal(laterPage.of("initialize")[0][2], null);
});

test("a necessary refresh stops polling immediately but saves geometry only after the motion gate", async () => {
  const view = { readyExpanded: false, minimizedKey: null, value: ready(1, { phase: "building" }) };
  const app = browser({ beforeRefresh: true, view });
  await app.poll(changed(2, ["index.html"]));
  assert.equal(app.count("beforeRefresh"), 1);
  assert.equal(app.count("fetch"), 1);
  assert.equal(app.timers.length, 0, "no next poll races the pending refresh");
  assert.equal(app.count("reload"), 0);
  assert.equal(app.count("render"), 0, "completion cannot change the drawer while it settles");
  assert.equal(app.storage.has(refreshKey), false, "the old intermediate geometry is not persisted");

  // The real motion gate lets the drawer reach this settled state before the
  // callback captures it. A snapshot made when the response arrived is stale.
  view.readyExpanded = true;
  view.value = ready(1);
  app.finishRefresh();
  const marker = JSON.parse(app.storage.get(refreshKey));
  assert.deepEqual(marker.view, view);
  assert.equal(marker.version, 2);
  assert.equal(marker.celebrate, true);
  assert.equal(app.count("reload"), 1);
  assert.equal(app.count("beforeRefresh"), 1);
  assert.equal(app.refreshCallbacks.length, 0);
  assert.equal(app.timers.length, 0);
  assert.equal(app.count("fetch"), 1);
  assert.equal(app.count("render"), 0);
});

test("unrelated output never waits for the refresh motion gate", async () => {
  const app = browser({ beforeRefresh: true });
  await app.poll(changed(2, ["articles/elsewhere.html"]));
  assert.equal(app.count("beforeRefresh"), 0);
  assert.equal(app.refreshCallbacks.length, 0);
  assert.equal(app.count("render"), 1);
  assert.equal(app.timers.length, 1);
});

test("the motion gate still reloads once when session storage is unavailable", async () => {
  const app = browser({ beforeRefresh: true, storageBlocked: true });
  await app.poll(changed(2, ["index.html"]));
  assert.equal(app.count("reload"), 0);
  assert.equal(app.timers.length, 0);
  app.finishRefresh();
  assert.equal(app.count("reload"), 1);
  assert.equal(app.count("renderDisconnected"), 0);
});

function savedView(value, extra = {}) {
  const view = { readyExpanded: false, minimizedKey: null, value };
  return new Map([[refreshKey, JSON.stringify({
    path: "/", version: 2, time: now, view, celebrate: true, ...extra
  })]]);
}

test("a reloaded building drawer paints in its old position before the ready transition", async () => {
  const previous = ready(1, { phase: "building", changed: "vignettes/example.Rmd" });
  const target = changed(2, ["index.html"]);
  const storage = savedView(previous);
  const saved = JSON.parse(storage.get(refreshKey));
  const app = browser({ bootstrap: target, storage, animationFrames: true });

  assert.deepEqual(plain(app.of("initialize")[0][1]), previous);
  assert.deepEqual(plain(app.of("initialize")[0][2]), saved.view);
  assert.equal(app.count("fetch"), 0, "status polling cannot race the restored drawer");
  assert.equal(app.count("render"), 0);
  assert.equal(app.count("celebrateCompletion"), 0);
  assert.equal(app.animationFrames.length, 1);

  app.paint();
  assert.equal(app.count("render"), 0, "the previous geometry gets a full paint");
  assert.equal(app.count("fetch"), 0);
  assert.equal(app.animationFrames.length, 1);

  app.paint();
  assert.deepEqual(plain(app.of("render")[0][1]), target);
  assert.equal(app.count("fetch"), 1);
  assert.ok(app.calls.findIndex(call => call[0] === "render") <
    app.calls.findIndex(call => call[0] === "fetch"));
  assert.equal(app.animationFrames.length, 0);
  assert.equal(app.count("celebrateCompletion"), 0,
    "building-to-ready already celebrates in panel.render; reload must not trigger it again");
  assert.equal(storage.has(refreshKey), false);

  await app.poll(target);
  assert.equal(app.count("initialize"), 1);
  assert.equal(app.count("reload"), 0, "the retained snapshot does not roll back the current version");
  assert.equal(app.count("celebrateCompletion"), 0);
});

test("a restored error drawer transitions to ready and celebrates once after paint", async () => {
  const previous = ready(1, { phase: "error", error: { message: "Missing demo data" } });
  const target = ready(2);
  const storage = savedView(previous);
  const app = browser({ bootstrap: target, storage, animationFrames: true });
  assert.deepEqual(plain(app.of("initialize")[0][1]), previous);
  assert.equal(app.count("celebrateCompletion"), 0);
  app.paint();
  assert.equal(app.count("fetch"), 0);
  assert.equal(app.count("celebrateCompletion"), 0);
  app.paint();
  assert.deepEqual(app.calls.map(call => call[0]), [
    "initialize", "render", "celebrateCompletion", "fetch"
  ]);
  assert.deepEqual(plain(app.of("render")[0][1]), target);
  await app.poll(target);
  assert.equal(app.count("celebrateCompletion"), 1);
  const laterPage = browser({ bootstrap: target, storage, animationFrames: true });
  assert.equal(laterPage.count("celebrateCompletion"), 0);
  assert.equal(laterPage.animationFrames.length, 0);
  assert.equal(laterPage.count("fetch"), 1);
});

test("a disconnected drawer survives a changed-version reload until ready can animate in", async () => {
  const previous = { phase: "disconnected" };
  const view = { readyExpanded: true, minimizedKey: null, value: previous };
  const target = changed(2, ["index.html"]);
  const storage = savedView(previous, { view });
  const app = browser({ bootstrap: target, storage, animationFrames: true });
  assert.deepEqual(plain(app.of("initialize")[0].slice(1)), [previous, view]);
  assert.equal(app.count("fetch"), 0);
  app.paint();
  assert.equal(app.count("render"), 0);
  assert.equal(app.count("celebrateCompletion"), 0);
  app.paint();
  assert.deepEqual(plain(app.of("render")[0][1]), target);
  assert.deepEqual(app.calls.map(call => call[0]), [
    "initialize", "render", "celebrateCompletion", "fetch"
  ]);
  await app.poll(target);
  assert.equal(app.count("reload"), 0);
  assert.equal(app.count("initialize"), 1);
  assert.equal(app.count("celebrateCompletion"), 1);
});

for (const event of ["pageshow", "load"]) {
  test(`restored geometry waits for ${event} before spending its two paint frames`, () => {
    const previous = ready(1, { phase: "error", error: { summary: "Missing file" } });
    const target = ready(2);
    const app = browser({
      bootstrap: target, storage: savedView(previous), animationFrames: true, readyState: "loading"
    });
    assert.deepEqual(plain(app.of("initialize")[0][1]), previous);
    assert.equal(app.count("render"), 0);
    assert.equal(app.count("fetch"), 0);
    assert.equal(app.animationFrames.length, 0, "parsing-time frames cannot consume the visible transition");
    assert.equal(app.timers[0].delay, 2000);
    const staleFallback = app.timers[0].fn;

    app.window.dispatch(event);
    assert.equal(app.animationFrames.length, 1);
    app.paint();
    assert.equal(app.count("render"), 0);
    app.window.dispatch("load");
    app.window.dispatch("pageshow");
    assert.equal(app.animationFrames.length, 1, "duplicate lifecycle events do not add another handoff");
    app.paint();
    assert.deepEqual(app.calls.map(call => call[0]), [
      "initialize", "render", "celebrateCompletion", "fetch"
    ]);
    assert.deepEqual(app.timers.map(timer => timer.delay), [10000],
      "only the status request deadline remains after the handoff");
    assert.equal(app.animationFrames.length, 0);
    assert.equal(app.window.listenerCount() + app.document.listenerCount(), 0);

    staleFallback();
    app.window.dispatch("pageshow");
    app.document.hidden = true;
    app.document.dispatch("visibilitychange");
    assert.equal(app.count("render"), 1);
    assert.equal(app.count("celebrateCompletion"), 1);
    assert.equal(app.count("fetch"), 1);
  });
}

test("initially hidden pages hand off immediately so status polling cannot stall", () => {
  const app = browser({
    bootstrap: ready(2), storage: savedView({ phase: "disconnected" }),
    animationFrames: true, readyState: "loading", hidden: true
  });
  assert.deepEqual(app.calls.map(call => call[0]), [
    "initialize", "render", "celebrateCompletion", "fetch"
  ]);
  assert.equal(app.animationFrames.length, 0);
  assert.deepEqual(app.timers.map(timer => timer.delay), [10000],
      "only the status request deadline remains after the handoff");
  assert.equal(app.window.listenerCount() + app.document.listenerCount(), 0);
});

for (const state of ["before load", "between paint frames"]) {
  test(`hiding the page ${state} cancels the pending handoff and resumes polling once`, () => {
    const app = browser({
      bootstrap: ready(2), storage: savedView(ready(1, { phase: "building" })),
      animationFrames: true, readyState: "loading"
    });
    if (state === "between paint frames") {
      app.window.dispatch("load");
      app.paint();
      assert.equal(app.animationFrames.length, 1);
    }
    app.document.hidden = true;
    app.document.dispatch("visibilitychange");
    assert.equal(app.count("render"), 1);
    assert.equal(app.count("fetch"), 1);
    assert.equal(app.count("celebrateCompletion"), 0, "the building-to-ready panel transition owns celebration");
    assert.equal(app.animationFrames.length, 0);
    assert.deepEqual(app.timers.map(timer => timer.delay), [10000],
      "only the status request deadline remains after the handoff");
    assert.equal(app.window.listenerCount() + app.document.listenerCount(), 0);
  });
}

for (const state of ["loading stalled", "animation frames stalled"]) {
  test(`${state} falls back to one target render and resumes polling`, () => {
    const app = browser({
      bootstrap: ready(2), storage: savedView({ phase: "disconnected" }),
      animationFrames: true, readyState: "loading"
    });
    if (state === "animation frames stalled") {
      app.window.dispatch("pageshow");
      app.paint();
      assert.equal(app.animationFrames.length, 1);
    }
    app.fireTimer(2000);
    assert.equal(app.count("render"), 1);
    assert.equal(app.count("fetch"), 1);
    assert.equal(app.count("celebrateCompletion"), 1);
    assert.equal(app.animationFrames.length, 0);
    assert.deepEqual(app.timers.map(timer => timer.delay), [10000],
      "only the status request deadline remains after the handoff");
    assert.equal(app.window.listenerCount() + app.document.listenerCount(), 0);
    app.window.dispatch("load");
    assert.equal(app.count("render"), 1);
    assert.equal(app.count("fetch"), 1);
  });
}

test("the actual panel captures and restores its visible disconnected drawer", () => {
  const panelSource = fs.readFileSync(path.join(assets, "panel.js"), "utf8");
  function panelInstance() {
    const shown = [];
    const window = {
      console,
      __updocPanelPrimitives: {
        cleanText: value => value == null ? "" : String(value),
        announce() {},
        initialize: render => render(),
        // Content and animation belong to the primitives. Observe the actual
        // panel controller's selected state and size without constructing DOM.
        showPanel: (kind, state, expanded) => shown.push({ kind, state, expanded })
      }
    };
    vm.runInNewContext(panelSource, { window }, { filename: "panel.js" });
    return { api: window.__updocPreviewPanel, shown };
  }
  const previousPage = panelInstance();
  previousPage.api.initialize(ready(1), { readyExpanded: true });
  assert.deepEqual(previousPage.shown.at(-1), {
    kind: "ready", state: "ready-full", expanded: true
  });
  previousPage.api.renderDisconnected();
  const saved = plain(previousPage.api.viewState());
  assert.deepEqual(saved, {
    readyExpanded: true, minimizedKey: null, value: { phase: "disconnected" }
  });

  const reloadedPage = panelInstance();
  reloadedPage.api.initialize(saved.value, saved);
  assert.deepEqual(reloadedPage.shown, [{
    kind: "disconnected", state: "disconnected", expanded: true
  }]);
  assert.deepEqual(plain(reloadedPage.api.viewState()), saved);
  reloadedPage.api.render(ready(2));
  assert.deepEqual(reloadedPage.shown.at(-1), {
    kind: "ready", state: "ready-full", expanded: true
  });
  assert.deepEqual(plain(reloadedPage.api.viewState()).value, ready(2));
  reloadedPage.api.render(ready(2));
  assert.equal(reloadedPage.shown.length, 2, "unchanged connected polls remain deduplicated");
});

test("restored warning details stay visible until the next status can animate in", () => {
  const previous = ready(1, { warnings: ["Previous article title mismatch"] });
  const target = ready(2, { warnings: ["Updated article title mismatch"] });
  const view = { readyExpanded: true, minimizedKey: "previous-warning", value: previous };
  const storage = savedView(previous, { view, celebrate: false });
  const app = browser({ bootstrap: target, storage, animationFrames: true });
  assert.deepEqual(plain(app.of("initialize")[0].slice(1)), [previous, view]);
  app.paint();
  assert.equal(app.count("render"), 0);
  assert.equal(app.count("fetch"), 0);
  app.paint();
  assert.deepEqual(plain(app.of("render")[0][1]), target);
  assert.equal(app.count("celebrateCompletion"), 0);
  assert.equal(app.count("fetch"), 1);
});

test("restored nonbuilding state respects a missing celebration intent", () => {
  const storage = savedView(ready(1), { celebrate: false });
  const app = browser({ bootstrap: ready(2), storage, animationFrames: true });
  app.paint();
  app.paint();
  assert.equal(app.count("render"), 1);
  assert.equal(app.count("celebrateCompletion"), 0);
});

for (const [name, previous] of [
  ["unknown phase", { version: 1, phase: "unexpected" }],
  ["missing phase", { version: 1 }],
  ["missing snapshot", null]
]) {
  test(`${name} initializes current status without delaying polling`, () => {
    const app = browser({ bootstrap: ready(2), storage: savedView(previous), animationFrames: true });
    assert.equal(app.of("initialize")[0][1].version, 2);
    assert.equal(app.of("initialize")[0][1].phase, "ready");
    assert.equal(app.animationFrames.length, 0);
    assert.equal(app.count("fetch"), 1);
  });
}

test("expired saved geometry cannot delay current status or polling", () => {
  const storage = savedView(ready(1, { phase: "building" }), { time: now - 15000 });
  const app = browser({ bootstrap: ready(2), storage, animationFrames: true });
  assert.equal(app.of("initialize")[0][1].version, 2);
  assert.equal(app.of("initialize")[0][2], null);
  assert.equal(app.animationFrames.length, 0);
  assert.equal(app.count("fetch"), 1);
  assert.equal(app.count("celebrateCompletion"), 0);
});

for (const [name, change] of [
  ["another path", { path: "/articles/other.html" }],
  ["another version", { version: 9 }],
  ["expired", { time: now - 15000 }],
  ["future timestamp", { time: now + 1 }],
  ["nonnumeric timestamp", { time: String(now) }]
]) {
  test(`${name} refresh state is discarded`, () => {
    const marker = { path: "/", version: 2, time: now, view: { expanded: true }, celebrate: true, ...change };
    const storage = new Map([[refreshKey, JSON.stringify(marker)]]);
    const app = browser({ storage, bootstrap: ready(2) });
    assert.equal(app.of("initialize")[0][2], null);
    assert.equal(app.count("celebrateCompletion"), 0);
    assert.equal(storage.has(refreshKey), false);
  });
}

test("malformed refresh storage is discarded without interrupting initialization", () => {
  const storage = new Map([[refreshKey, "{broken json"]]);
  const app = browser({ storage });
  assert.equal(app.count("initialize"), 1);
  assert.equal(app.count("celebrateCompletion"), 0);
  assert.equal(storage.has(refreshKey), false);
});

for (const [name, status] of [
  ["warning", ready(2, { warnings: ["Article title does not match"] })],
  ["error", ready(2, { phase: "error", error: { message: "Build failed" } })],
  ["building", ready(2, { phase: "building" })]
]) {
  test(`restored ${name} state never celebrates`, () => {
    const marker = { path: "/", version: 2, time: now, view: { expanded: true }, celebrate: true };
    const app = browser({ storage: new Map([[refreshKey, JSON.stringify(marker)]]), bootstrap: status });
    assert.deepEqual(plain(app.of("initialize")[0][2]), marker.view);
    assert.equal(app.count("celebrateCompletion"), 0);
  });
}

test("blocked session storage still initializes, polls, and refreshes", async () => {
  const app = browser({ storageBlocked: true });
  assert.equal(app.count("initialize"), 1);
  await app.poll(changed(2, ["index.html"]));
  assert.equal(app.count("reload"), 1);
  assert.equal(app.count("renderDisconnected"), 0);
});

test("disconnect retries recover by rendering the same version without reinitialization", async () => {
  const app = browser();
  await app.poll(new Error("offline"));
  await app.poll(new Error("offline"));
  assert.equal(app.count("renderDisconnected"), 0);
  await app.poll(new Error("offline"));
  assert.equal(app.count("renderDisconnected"), 1);
  assert.equal(app.timers[0].delay, 750);
  await app.poll(new Error("offline"));
  assert.equal(app.count("renderDisconnected"), 1);
  assert.equal(app.timers[0].delay, 1500);
  await app.poll(ready(1));
  assert.equal(app.count("initialize"), 1);
  assert.equal(app.count("render"), 1);
  assert.equal(app.count("stopRetryCountdown"), 1);
  assert.equal(app.timers[0].delay, 750);
  assert.equal(app.count("reload"), 0);
});
