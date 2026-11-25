(function () {
  "use strict";

  var THEME_STORAGE_KEY = "__updoc_demo_theme";
  var VALID_THEMES = ["auto", "inverse", "light", "dark"];

  var READY_STATUS = { phase: "ready", warnings: [] };
  var BUILD_STEPS = [
    {
      phase: "building",
      progress: {
        operation: "Building reference",
        target: "serve.Rd",
        current: 1,
        total: 4
      }
    },
    {
      phase: "building",
      progress: {
        operation: "Building reference",
        target: "preview.Rd",
        current: 2,
        total: 4
      }
    },
    {
      phase: "building",
      progress: {
        operation: "Building articles",
        target: "vignettes/articles/get-started.Rmd",
        current: 3,
        total: 4
      }
    },
    {
      phase: "building",
      queued: true,
      progress: {
        operation: "Updating site",
        target: "Finishing preview",
        current: 4,
        total: 4
      }
    }
  ];
  var WARNING_STATUS = {
    phase: "ready",
    warnings: [
      {
        summary: "Missing title in YAML front matter",
        location: "vignettes/articles/get-started.Rmd",
        cause: "Add a title field so pkgdown can list this article in navigation."
      }
    ]
  };
  var ERROR_STATUS = {
    phase: "error",
    warnings: [],
    error: {
      summary: "Could not render vignette",
      location: "vignettes/articles/get-started.Rmd:18-20",
      cause: "object 'my_data' not found",
      excerpt: "18 | summary(my_data)\n19 | plot(my_data$x, my_data$y)",
      hint: "Define or load my_data before this chunk, then save again."
    }
  };

  function resolveAssetsRoot() {
    var current = document.currentScript;
    var src = current && current.src;
    if (!src) {
      var scripts = document.querySelectorAll("script[src]");
      for (var i = scripts.length - 1; i >= 0; i -= 1) {
        if (/(^|\/)extra\.js(\?|#|$)/.test(scripts[i].getAttribute("src") || "")) {
          src = scripts[i].src;
          break;
        }
      }
    }
    if (!src) return "doc-assets/";
    try {
      return new URL("doc-assets/", src).href;
    } catch (_) {
      return "doc-assets/";
    }
  }

  var assetsRoot = resolveAssetsRoot();

  function livePreviewActive() {
    return !!(
      document.querySelector('script[data-updoc-preview="true"]') ||
      document.getElementById("__updoc_status")
    );
  }

  function disableDemoControls(reason) {
    document.querySelectorAll("[data-updoc-demo]").forEach(function (group) {
      group.querySelectorAll("button").forEach(function (button) {
        button.disabled = true;
      });
      if (!group.querySelector(".updoc-demo-note")) {
        var note = document.createElement("p");
        note.className = "updoc-demo-note";
        note.textContent = reason;
        group.appendChild(note);
      }
    });
  }

  function loadScript(url) {
    return new Promise(function (resolve, reject) {
      var script = document.createElement("script");
      script.src = url;
      script.async = false;
      script.onload = function () { resolve(); };
      script.onerror = function () { reject(new Error("Failed to load " + url)); };
      (document.head || document.documentElement).appendChild(script);
    });
  }

  function readStoredTheme() {
    try {
      var saved = window.sessionStorage.getItem(THEME_STORAGE_KEY);
      if (VALID_THEMES.indexOf(saved) !== -1) return saved;
    } catch (_) { /* Storage may be unavailable in restricted contexts. */ }
    return "auto";
  }

  function writeStoredTheme(theme) {
    try {
      if (theme === "auto") window.sessionStorage.removeItem(THEME_STORAGE_KEY);
      else window.sessionStorage.setItem(THEME_STORAGE_KEY, theme);
    } catch (_) { /* Ignore unavailable storage. */ }
  }

  function pageTheme() {
    var nodes = [document.documentElement, document.body];
    for (var i = 0; i < nodes.length; i += 1) {
      var node = nodes[i];
      if (!node) continue;
      var theme = node.getAttribute("data-bs-theme") ||
        node.getAttribute("data-theme");
      if (theme === "dark" || theme === "light") return theme;
      if (node.classList.contains("dark")) return "dark";
      if (node.classList.contains("light")) return "light";
    }
    return null;
  }

  function resolvePanelTheme(mode) {
    if (mode === "light" || mode === "dark") return mode;
    var base = pageTheme() || (window.matchMedia &&
      window.matchMedia("(prefers-color-scheme: dark)").matches ?
      "dark" : "light");
    if (mode === "inverse") return base === "dark" ? "light" : "dark";
    return base;
  }

  function isRebuildRequest(input, init) {
    var url = typeof input === "string" ? input : (input && input.url) || "";
    var method = ((init && init.method) || (input && input.method) || "GET").toUpperCase();
    var headers = (init && init.headers) || (input && input.headers);
    var hasHeader = false;
    if (headers) {
      if (typeof headers.get === "function") {
        hasHeader = headers.get("X-Updoc-Request") === "rebuild";
      } else {
        hasHeader = headers["X-Updoc-Request"] === "rebuild" ||
          headers["x-updoc-request"] === "rebuild";
      }
    }
    return url === "/__updoc__/rebuild" && method === "POST" && hasHeader;
  }

  function startDemo() {
    if (livePreviewActive()) {
      disableDemoControls("Live updoc preview is active on this page.");
      return;
    }

    var sequence = loadScript(assetsRoot + "panel-css.js")
      .then(function () {
        if (window.UpdocDoc) return;
        return loadScript(assetsRoot + "doc.js");
      })
      .then(function () {
        if (livePreviewActive()) return null;
        window.__updocPreviewBootstrap = { css: window.__updocDemoCSS || "" };
        delete window.__updocDemoCSS;
        return loadScript(assetsRoot + "panel-primitives.js")
          .then(function () { return loadScript(assetsRoot + "panel.js"); });
      })
      .then(function () {
        if (livePreviewActive()) {
          delete window.__updocPreviewBootstrap;
          delete window.__updocPreviewPanel;
          disableDemoControls("Live updoc preview is active on this page.");
          return;
        }
        var panel = window.__updocPreviewPanel;
        delete window.__updocPreviewPanel;
        if (!panel) {
          delete window.__updocPreviewBootstrap;
          return;
        }
        mountDemo(panel);
      })
      .catch(function (error) {
        delete window.__updocDemoCSS;
        delete window.__updocPreviewBootstrap;
        window.console.warn("updoc: demo popup failed to initialize", error);
      });

    return sequence;
  }

  function mountDemo(panel) {
    var timers = [];
    var activeState = "ready";
    var activeTheme = readStoredTheme();

    function schedule(fn, delay) {
      var id = window.setTimeout(function () {
        var index = timers.indexOf(id);
        if (index !== -1) timers.splice(index, 1);
        fn();
      }, delay);
      timers.push(id);
      return id;
    }

    function clearTimers() {
      while (timers.length) window.clearTimeout(timers.pop());
    }

    function panelShell() {
      var host = document.getElementById("__updoc_status");
      if (!host) return null;
      return (host.shadowRoot || host).querySelector(".updoc-shell");
    }

    function syncStateButtons(state) {
      activeState = state;
      document.querySelectorAll("[data-updoc-demo-state]").forEach(function (button) {
        button.setAttribute(
          "aria-pressed",
          button.dataset.updocDemoState === state ? "true" : "false"
        );
      });
    }

    function applyTheme() {
      var host = document.getElementById("__updoc_status");
      if (host) host.dataset.theme = resolvePanelTheme(activeTheme);
      document.querySelectorAll("[data-updoc-demo-theme]").forEach(function (button) {
        button.setAttribute(
          "aria-pressed",
          button.dataset.updocDemoTheme === activeTheme ? "true" : "false"
        );
      });
    }

    function setTheme(theme) {
      if (VALID_THEMES.indexOf(theme) === -1) return;
      activeTheme = theme;
      writeStoredTheme(theme);
      applyTheme();
    }

    function scheduleBuildProgress(fromRebuildButton) {
      clearTimers();
      panel.stopRetryCountdown();
      syncStateButtons("building");
      if (!fromRebuildButton) panel.render(BUILD_STEPS[0]);

      schedule(function () { panel.render(BUILD_STEPS[1]); }, 750);
      schedule(function () { panel.render(BUILD_STEPS[2]); }, 1500);
      schedule(function () { panel.render(BUILD_STEPS[3]); }, 2250);
      schedule(function () {
        panel.render(READY_STATUS);
        syncStateButtons("ready");
      }, 2950);
    }

    function startDisconnectedCycle() {
      panel.showRetryCountdown(5000);
      schedule(function () {
        if (activeState !== "disconnected") return;
        panel.showRetrying();
        schedule(function () {
          if (activeState !== "disconnected") return;
          startDisconnectedCycle();
        }, 500);
      }, 5000);
    }

    function showState(state) {
      clearTimers();
      panel.stopRetryCountdown();
      if (state === "building") {
        scheduleBuildProgress(false);
        return;
      }
      if (state === "disconnected") {
        syncStateButtons("disconnected");
        panel.renderDisconnected();
        startDisconnectedCycle();
        return;
      }
      if (state === "warning" || state === "error") {
        syncStateButtons(state);
        panel.render(state === "warning" ? WARNING_STATUS : ERROR_STATUS);
        var diagnosticShell = panelShell();
        if (diagnosticShell && diagnosticShell.dataset.updocState === state + "-compact") {
          diagnosticShell.click();
        }
        return;
      }
      if (state === "ready") {
        var readyShell = panelShell();
        if (activeState === "ready" && readyShell && readyShell.dataset.updocState === "ready") {
          readyShell.click();
          return;
        }
        syncStateButtons("ready");
        panel.render(READY_STATUS);
      }
    }

    if (typeof window.fetch === "function") {
      var nativeFetch = window.fetch.bind(window);
      window.fetch = function (input, init) {
        if (isRebuildRequest(input, init)) {
          scheduleBuildProgress(true);
          return Promise.resolve({
            ok: true,
            status: 202,
            json: function () { return Promise.resolve(BUILD_STEPS[0]); }
          });
        }
        return nativeFetch(input, init);
      };
    }

    panel.initialize(READY_STATUS);
    delete window.__updocPreviewBootstrap;
    document.documentElement.dataset.updocDemo = "true";
    applyTheme();
    syncStateButtons("ready");

    if (typeof MutationObserver === "function") {
      var themeObserver = new MutationObserver(applyTheme);
      var observerOptions = {
        attributes: true,
        attributeFilter: ["class", "data-bs-theme", "data-theme"]
      };
      themeObserver.observe(document.documentElement, observerOptions);
      if (document.body) themeObserver.observe(document.body, observerOptions);
    }
    if (window.matchMedia) {
      var preference = window.matchMedia("(prefers-color-scheme: dark)");
      if (typeof preference.addEventListener === "function") {
        preference.addEventListener("change", applyTheme);
      } else if (typeof preference.addListener === "function") {
        preference.addListener(applyTheme);
      }
    }

    document.querySelectorAll("[data-updoc-demo-state]").forEach(function (button) {
      button.addEventListener("click", function () {
        showState(button.dataset.updocDemoState);
      });
    });
    document.querySelectorAll("[data-updoc-demo-theme]").forEach(function (button) {
      button.addEventListener("click", function () {
        setTheme(button.dataset.updocDemoTheme);
      });
    });

    window.addEventListener("pagehide", clearTimers);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", startDemo, { once: true });
  } else {
    startDemo();
  }
}());
