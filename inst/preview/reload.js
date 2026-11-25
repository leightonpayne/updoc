(function () {
  "use strict";

  var panel = window.__updocPreviewPanel;
  if (!panel) {
    window.console.warn("updoc: browser panel failed to initialize");
    return;
  }
  delete window.__updocPreviewPanel;

  var bootstrap = window.__updocPreviewBootstrap;
  var version = null;
  var failures = 0;
  var disconnected = false;
  var reloadPending = false;
  var basePollDelay = 750;
  var maxRetryDelay = 10000;
  var requestTimeout = 10000;
  var refreshKey = "__updoc_refresh";
  var resources = new Set();

  function cleanReady(value) {
    return value.phase === "ready" && !value.error &&
      (!value.warnings || (Array.isArray(value.warnings) && !value.warnings.length));
  }

  function outputPath(url) {
    try {
      var resolved = new URL(url, window.location.href);
      if (resolved.origin !== window.location.origin) return null;
      var path = decodeURIComponent(resolved.pathname).replace(/^\/+/, "");
      if (!path || path.endsWith("/")) path += "index.html";
      return path.startsWith("__updoc__/") ? null : path;
    } catch (_) { return null; }
  }

  function rememberResource(url) {
    var path = outputPath(url);
    if (path) resources.add(path);
  }

  function collectResources() {
    rememberResource(window.location.href);
    document.querySelectorAll(
      "script[src],link[rel~='stylesheet'][href],img,source[src],video[src],audio[src],object[data]"
    ).forEach(function (node) {
      var url = node.currentSrc || node.src || node.href || node.data;
      if (url) rememberResource(url);
    });
    if (window.performance && window.performance.getEntriesByType) {
      window.performance.getEntriesByType("resource").forEach(function (entry) {
        rememberResource(entry.name);
      });
    }
  }

  // Retain dynamically loaded resources after timing-buffer eviction.
  if (typeof PerformanceObserver === "function") {
    var observer = new PerformanceObserver(function (list) {
      list.getEntries().forEach(function (entry) { rememberResource(entry.name); });
    });
    observer.observe({ entryTypes: ["resource"] });
  }

  function needsRefresh(value) {
    var change = value.reload;
    // Missed versions have unknown output paths, so refresh conservatively.
    if (value.version !== version + 1 || !change || change.full ||
        !Array.isArray(change.paths)) return true;
    collectResources();
    return change.paths.some(function (path) { return resources.has(path); });
  }

  function rememberRefresh(value) {
    try {
      window.sessionStorage.setItem(refreshKey, JSON.stringify({
        path: window.location.pathname,
        version: value.version,
        time: Date.now(),
        view: panel.viewState(),
        celebrate: cleanReady(value)
      }));
    } catch (_) { /* Preview reloads also work when storage is unavailable. */ }
  }

  function takeRefresh(value) {
    try {
      var saved = window.sessionStorage.getItem(refreshKey);
      window.sessionStorage.removeItem(refreshKey);
      var refresh = saved && JSON.parse(saved);
      var age = refresh && Date.now() - refresh.time;
      if (refresh && refresh.path === window.location.pathname &&
          refresh.version === value.version && typeof refresh.time === "number" &&
          age >= 0 && age < 15000) return refresh;
    } catch (_) { /* Ignore expired or unavailable session state. */ }
    return null;
  }

  function afterPagePaint(callback) {
    var finished = false;
    var painting = false;
    var frame = null;
    var timeout = null;

    function finish() {
      if (finished) return;
      finished = true;
      window.clearTimeout(timeout);
      if (frame !== null && window.cancelAnimationFrame) {
        window.cancelAnimationFrame(frame);
      }
      window.removeEventListener("pageshow", paint);
      window.removeEventListener("load", paint);
      document.removeEventListener("visibilitychange", visibilityChanged);
      callback();
    }

    function paint() {
      if (finished || painting) return;
      if (document.hidden || !window.requestAnimationFrame) {
        finish();
        return;
      }
      painting = true;
      frame = window.requestAnimationFrame(function () {
        frame = window.requestAnimationFrame(finish);
      });
    }

    function visibilityChanged() {
      if (document.hidden) finish();
    }

    // Frames may precede navigation's first paint. Bound the wait so hidden
    // tabs and stalled resources cannot stop status polling.
    window.addEventListener("pageshow", paint);
    window.addEventListener("load", paint);
    document.addEventListener("visibilitychange", visibilityChanged);
    timeout = window.setTimeout(finish, 2000);
    if (document.hidden) finish();
    else if (document.readyState === "complete") paint();
  }

  function initialize(value, afterPaint) {
    var refresh = takeRefresh(value);
    version = value.version;
    var previous = refresh && refresh.view && refresh.view.value;
    if (afterPaint && previous && ["building", "ready", "error", "disconnected"].indexOf(previous.phase) !== -1 &&
        window.requestAnimationFrame) {
      // Restore the previous drawer before transitioning to the new status.
      panel.initialize(previous, refresh.view);
      afterPagePaint(function () {
        panel.render(value);
        if (refresh.celebrate && cleanReady(value) && previous.phase !== "building") {
          panel.celebrateCompletion();
        }
        if (afterPaint) afterPaint();
      });
      return;
    }
    panel.initialize(value, refresh && refresh.view);
    if (refresh && refresh.celebrate && cleanReady(value)) panel.celebrateCompletion();
    if (afterPaint) afterPaint();
  }

  function nextPollDelay() {
    if (failures <= 3) return basePollDelay;
    return Math.min(maxRetryDelay, basePollDelay * Math.pow(2, failures - 3));
  }

  function fetchStatus() {
    var controller = typeof AbortController === "function" ? new AbortController() : null;
    var timeout;
    var deadline = new Promise(function (_, reject) {
      timeout = window.setTimeout(function () {
        if (controller) controller.abort();
        reject(new Error("preview request timed out"));
      }, requestTimeout);
    });
    var options = { cache: "no-store" };
    if (controller) options.signal = controller.signal;
    var request = new Promise(function (resolve) {
      resolve(fetch("/__updoc__/status", options));
    })
      .then(function (response) {
        if (!response.ok) throw new Error("preview unavailable");
        return response.json();
      });
    // Include reading the response body in the deadline, and discard a late
    // response even when abort is unsupported or the connection ignores it.
    return Promise.race([request, deadline]).finally(function () {
      window.clearTimeout(timeout);
    });
  }

  function poll() {
    if (failures >= 3) panel.showRetrying();
    fetchStatus()
      .then(function (value) {
        panel.stopRetryCountdown();
        failures = 0;
        disconnected = false;
        if (version === null) {
          initialize(value);
        } else if (value.version !== version && value.phase !== "building" && !value.queued) {
          if (needsRefresh(value)) {
            // Keep the old popup stable until navigation commits. Rendering
            // completion first would start a collapse that the reload tears down.
            reloadPending = true;
            var refresh = function () {
              rememberRefresh(value);
              window.location.reload();
            };
            if (panel.beforeRefresh) panel.beforeRefresh(refresh);
            else refresh();
            return;
          }
          version = value.version;
          panel.render(value);
        } else {
          panel.render(value);
        }
      })
      .catch(function () {
        failures += 1;
        if (failures >= 3 && !disconnected) {
          disconnected = true;
          panel.renderDisconnected();
        }
      })
      .finally(function () {
        if (reloadPending) return;
        var delay = nextPollDelay();
        if (failures >= 3) panel.showRetryCountdown(delay);
        window.setTimeout(poll, delay);
      });
  }

  // Inline bootstrap avoids waiting for a request before the first paint.
  if (bootstrap && bootstrap.status) initialize(bootstrap.status, poll);
  else poll();
  delete window.__updocPreviewBootstrap;
}());
