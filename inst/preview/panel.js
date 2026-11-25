(function () {
  "use strict";

  var lastValue = null;
  var lastRenderKey = null;
  var minimizedKey = null;
  var retryCountdownTimer = null;
  var retryDeadline = 0;
  var readyExpanded = false;
  var showingDisconnected = false;

  var ui = window.__updocPanelPrimitives;
  if (!ui) {
    window.console.warn("updoc: panel components failed to initialize");
    return;
  }
  delete window.__updocPanelPrimitives;
  var cleanText = ui.cleanText;
  var element = ui.element;
  var showPanel = ui.showPanel;
  var badge = ui.badge;
  var progress = ui.progress;
  var updateProgress = ui.updateProgress;
  var setProgressVisible = ui.setProgressVisible;
  var actionButton = ui.actionButton;
  var defaultButton = ui.defaultButton;
  var disclosureIndicator = ui.disclosureIndicator;
  var cardHeader = ui.cardHeader;
  var collapsibleHeader = ui.collapsibleHeader;

  function setReadyExpanded(expanded) {
    readyExpanded = expanded;
    lastRenderKey = null;
    render(lastValue);
  }

  function requestPreviewRebuild(button, message) {
    if (button.disabled) return;
    button.disabled = true;
    button.setAttribute("aria-busy", "true");
    message.hidden = true;
    message.textContent = "";
    var controller = new AbortController();
    var timeout = window.setTimeout(function () { controller.abort(); }, 10000);
    fetch("/__updoc__/rebuild", {
      method: "POST",
      cache: "no-store",
      headers: { "X-Updoc-Request": "rebuild" },
      signal: controller.signal
    }).then(function (response) {
      if (!response.ok) throw new Error("rebuild request failed");
      return response.json();
    }).then(function (value) {
      // A newer status or a user action may already have replaced this drawer.
      if (!button.isConnected) return;
      readyExpanded = false;
      lastRenderKey = null;
      render(value);
    }).catch(function () {
      if (!button.isConnected) return;
      button.disabled = false;
      button.removeAttribute("aria-busy");
      message.hidden = false;
      message.textContent = "Could not start rebuild. Try again.";
    }).finally(function () {
      window.clearTimeout(timeout);
    });
  }

  function appendRebuildActions(shell) {
    var content = element("div", "ui-card-content updoc-ready-actions");
    content.dataset.slot = "card-content";
    var message = element("div", "updoc-value");
    message.hidden = true;
    message.setAttribute("role", "alert");
    content.appendChild(defaultButton("Rebuild", "refresh-cw", function (button) {
      requestPreviewRebuild(button, message);
    }));
    content.appendChild(message);
    shell.appendChild(content);
  }

  function renderReady() {
    if (!readyExpanded) {
      showPanel("ready", "ready", false, null, function (shell) {
        shell.title = "Watching for changes. Show preview controls.";
        shell.setAttribute(
          "aria-label", "Watching for changes. Show preview controls."
        );
        shell.setAttribute("role", "button");
        shell.setAttribute("tabindex", "0");
        shell.dataset.interactive = "true";
        shell.onclick = function () { setReadyExpanded(true); };
        shell.onkeydown = function (event) {
          if (event.key === "Enter" || event.key === " ") {
            event.preventDefault();
            setReadyExpanded(true);
          }
        };
        shell.appendChild(cardHeader(
          "Watching for changes", null, disclosureIndicator()
        ));
      });
      return;
    }

    showPanel("ready", "ready-full", true, null, function (shell) {
      var header = cardHeader(
        "Watching for changes",
        "Previewing documentation with {updoc}",
        actionButton("Minimize preview controls", "disclosure", function () {
          setReadyExpanded(false);
        })
      );
      shell.appendChild(collapsibleHeader(header, function () {
        setReadyExpanded(false);
      }));
      appendRebuildActions(shell);
    });
  }

  function renderBuilding(value) {
    var build = value.progress || {};
    var operation = cleanText(build.operation) || "Building documentation";
    var target = cleanText(build.target);
    var current = Number(build.current);
    var total = Number(build.total);
    var determinate = Number.isFinite(current) && Number.isFinite(total) && total > 1;
    var description = target || "Watching package changes";
    var action = value.queued ? badge("Queued", "secondary") : null;

    showPanel("building", "building", true, "updoc-building", function (shell) {
      shell.appendChild(cardHeader(operation, description, action));
      var content = element("div", "ui-card-content updoc-build-progress");
      content.dataset.slot = "card-content";
      var reveal = element("div", "updoc-progress-reveal");
      reveal.appendChild(progress(current, total));
      content.appendChild(reveal);
      setProgressVisible(content, determinate);
      shell.appendChild(content);
    }, function (shell) {
      var header = shell.querySelector(".ui-card-header");
      var title = header && header.querySelector(".ui-card-title-text");
      var subtitle = header && header.querySelector(".ui-card-description");
      var content = shell.querySelector(".ui-card-content");
      var meter = shell.querySelector(".ui-progress");
      if (!title || !subtitle || !content || !meter) return false;

      if (title.textContent !== operation) title.textContent = operation;
      if (subtitle.textContent !== description) subtitle.textContent = description;
      var actionNode = header.querySelector(".ui-card-action");
      if (action) {
        if (!actionNode) {
          actionNode = element("div", "ui-card-action");
          actionNode.dataset.slot = "card-action";
          header.appendChild(actionNode);
        }
        actionNode.replaceChildren(action);
      } else if (actionNode) {
        actionNode.remove();
      }
      updateProgress(meter, current, total);
      setProgressVisible(content, determinate);
      return true;
    });
  }

  function diagnosticField(container, label, value, code, source) {
    value = cleanText(value);
    if (!value) return;
    var field = element("div", "updoc-field");
    label = cleanText(label);
    if (label) field.appendChild(element("div", "updoc-label", label));
    var valueNode = element(source ? "pre" : "div",
      source ? "updoc-source updoc-code" : "updoc-value" + (code ? " updoc-code" : ""),
      value);
    field.appendChild(valueNode);
    container.appendChild(field);
  }

  function diagnosticKey(kind, diagnostic) {
    return kind + ":" + JSON.stringify(diagnostic || {});
  }

  function minimize(key) {
    minimizedKey = key;
    lastRenderKey = null;
    render(lastValue);
  }

  function restore() {
    minimizedKey = null;
    lastRenderKey = null;
    render(lastValue);
  }

  function diagnosticAlert(kind, diagnostic, includeLocation) {
    var alertClass = "ui-alert ui-alert--flat";
    if (kind === "error") alertClass += " ui-alert--destructive";
    if (kind === "warning") alertClass += " ui-alert--warning";
    var alert = element("div", alertClass);
    alert.dataset.slot = "alert";
    alert.dataset.variant = kind === "error" ? "destructive" : kind;

    var fallback = kind === "warning" ? "Unknown warning" : "Unknown error";
    var title = element(
      "div", "ui-alert-title", cleanText(diagnostic.summary) || fallback
    );
    title.dataset.slot = "alert-title";
    alert.appendChild(title);

    if (includeLocation && cleanText(diagnostic.location)) {
      var location = element(
        "div", "ui-alert-description updoc-warning-location",
        cleanText(diagnostic.location)
      );
      location.dataset.slot = "alert-description";
      alert.appendChild(location);
    }
    if (cleanText(diagnostic.cause)) {
      var cause = element(
        "div", "ui-alert-description", cleanText(diagnostic.cause)
      );
      cause.dataset.slot = "alert-description";
      alert.appendChild(cause);
    }
    return alert;
  }

  function diagnosticDetails(diagnostic) {
    var details = element("div", "updoc-details");
    diagnosticField(details, "Details", diagnostic.details, true, false);
    diagnosticField(details, null, diagnostic.excerpt, false, true);
    diagnosticField(details, null, diagnostic.hint, false, false);
    return details;
  }

  function cleanWarnings(warnings) {
    if (!Array.isArray(warnings)) return [];
    return warnings.map(function (warning) {
      if (warning && typeof warning === "object") {
        return {
          summary: cleanText(warning.summary) || "Build warning",
          location: cleanText(warning.location),
          cause: cleanText(warning.cause),
          details: cleanText(warning.details),
          hint: cleanText(warning.hint),
          excerpt: cleanText(warning.excerpt)
        };
      }
      var text = cleanText(warning);
      return text ? {
        summary: text, location: "", cause: "", details: "", hint: "",
        excerpt: ""
      } : null;
    }).filter(Boolean);
  }

  function warningEntry(warning, includeLocation) {
    var entry = element("div", "updoc-warning-entry");
    entry.setAttribute("role", "listitem");
    entry.appendChild(diagnosticAlert(
      "warning", warning, includeLocation
    ));
    var details = diagnosticDetails(warning);
    if (details.childNodes.length) entry.appendChild(details);
    return entry;
  }

  function appendWarningEntries(container, warnings, includeLocation) {
    warnings.forEach(function (warning, index) {
      if (index > 0) {
        var separator = element("div", "ui-separator");
        separator.dataset.slot = "separator";
        separator.setAttribute("aria-hidden", "true");
        container.appendChild(separator);
      }
      container.appendChild(warningEntry(warning, includeLocation));
    });
  }

  function appendRelatedWarnings(content, warnings) {
    if (!warnings.length) return;
    var separator = element("div", "ui-separator");
    separator.dataset.slot = "separator";
    separator.setAttribute("aria-hidden", "true");
    content.appendChild(separator);

    var section = element("section", "updoc-related-warnings");
    var description = warnings.length === 1 ? warnings[0].location :
      warnings.length + " warnings";
    var header = cardHeader(
      "Built with warnings", description
    );
    header.classList.add("updoc-related-header");
    section.appendChild(header);

    var list = element("div", "updoc-warning-list");
    list.setAttribute("role", "list");
    appendWarningEntries(list, warnings, warnings.length > 1);
    section.appendChild(list);
    content.appendChild(section);
  }

  function renderMinimized(kind, title, summary) {
    showPanel(kind, kind + "-compact", false, null, function (shell) {
      shell.title = title + ": " + cleanText(summary);
      shell.setAttribute("aria-label", title + ". Show details.");
      shell.setAttribute("role", "button");
      shell.setAttribute("tabindex", "0");
      shell.dataset.interactive = "true";
      shell.onclick = restore;
      shell.onkeydown = function (event) {
        if (event.key === "Enter" || event.key === " ") {
          event.preventDefault();
          restore();
        }
      };
      shell.appendChild(cardHeader(
        title, null, disclosureIndicator()
      ));
    });
  }

  function renderDiagnostic(kind, title, diagnostic, warnings) {
    diagnostic = diagnostic && typeof diagnostic === "object" ? diagnostic : {
      summary: diagnostic ||
        (kind === "warning" ? "Unknown warning" : "Unknown error")
    };
    warnings = cleanWarnings(warnings);
    var key = diagnosticKey(kind, {
      diagnostic: diagnostic,
      warnings: warnings
    });
    if (minimizedKey === key) {
      renderMinimized(kind, title, diagnostic.summary);
      return;
    }

    showPanel(kind, kind + "-full", true, null, function (shell) {
      var header = cardHeader(
        title,
        cleanText(diagnostic.location),
        actionButton("Minimize updoc message", "disclosure", function () {
          minimize(key);
        })
      );
      shell.appendChild(collapsibleHeader(header, function () {
        minimize(key);
      }));
      var content = element("div", "ui-card-content ui-card-content--diagnostic");
      content.dataset.slot = "card-content";
      content.appendChild(diagnosticAlert(kind, diagnostic, false));
      var details = diagnosticDetails(diagnostic);
      if (details.childNodes.length) content.appendChild(details);
      appendRelatedWarnings(content, warnings);
      shell.appendChild(content);
      appendRebuildActions(shell);
    });
  }

  function renderWarnings(warnings) {
    var cleaned = cleanWarnings(warnings);
    var diagnostic = {
      summary: cleaned[0] ? cleaned[0].summary : "Unknown warning",
      warnings: cleaned
    };
    var key = diagnosticKey("warning", diagnostic);
    var count = cleaned.length;
    var countLabel = count + (count === 1 ? " warning" : " warnings");
    if (count === 1) {
      renderDiagnostic("warning", "Built with warnings", cleaned[0]);
      return;
    }
    if (minimizedKey === key) {
      renderMinimized(
        "warning",
        "Built with warnings",
        diagnostic.summary
      );
      return;
    }

    showPanel("warning", "warning-full", true, null, function (shell) {
      var header = cardHeader(
        "Built with warnings",
        countLabel,
        actionButton("Minimize updoc message", "disclosure", function () {
          minimize(key);
        })
      );
      shell.appendChild(collapsibleHeader(header, function () {
        minimize(key);
      }));
      var content = element(
        "div", "ui-card-content ui-card-content--diagnostic updoc-warning-list"
      );
      content.dataset.slot = "card-content";
      content.setAttribute("role", "list");
      appendWarningEntries(content, cleaned, true);
      shell.appendChild(content);
      appendRebuildActions(shell);
    });
  }

  function renderDisconnected() {
    showingDisconnected = true;
    // Reconnecting must restore the status even when its payload is unchanged.
    lastRenderKey = null;
    ui.announce("Preview disconnected. Retrying automatically.");
    showPanel("disconnected", "disconnected", true, null, function (shell) {
      var header = cardHeader(
        "Preview disconnected", "Retrying in 1s"
      );
      var countdown = header.querySelector(".ui-card-description");
      countdown.classList.add("updoc-retry-countdown");
      countdown.setAttribute("aria-hidden", "true");
      header.appendChild(element(
        "span", "ui-sr-only", "Retrying automatically."
      ));
      shell.appendChild(header);
    });
  }

  function retryCountdownNode() {
    var host = document.getElementById("__updoc_status");
    if (!host) return null;
    return (host.shadowRoot || host).querySelector(".updoc-retry-countdown");
  }

  function stopRetryCountdown() {
    if (retryCountdownTimer !== null) {
      window.clearInterval(retryCountdownTimer);
      retryCountdownTimer = null;
    }
    retryDeadline = 0;
  }

  function showRetryCountdown(delay) {
    stopRetryCountdown();
    retryDeadline = Date.now() + delay;

    var update = function () {
      var countdown = retryCountdownNode();
      if (!countdown) {
        stopRetryCountdown();
        return;
      }
      var remaining = Math.max(0, retryDeadline - Date.now());
      var label = "Retrying in " + Math.ceil(remaining / 1000) + "s";
      if (countdown.textContent !== label) countdown.textContent = label;
      if (remaining <= 0) stopRetryCountdown();
    };

    update();
    retryCountdownTimer = window.setInterval(update, 200);
  }

  function showRetrying() {
    stopRetryCountdown();
    var countdown = retryCountdownNode();
    if (countdown) countdown.textContent = "Retrying in 0s";
  }

  function render(value) {
    showingDisconnected = false;
    lastValue = value;
    var key = JSON.stringify({
      phase: value.phase,
      progress: value.progress,
      changed: value.changed,
      queued: value.queued,
      warnings: value.warnings,
      error: value.error,
      minimized: minimizedKey,
      readyExpanded: readyExpanded
    });
    if (key === lastRenderKey) return;
    lastRenderKey = key;

    if (value.phase === "building") {
      ui.announce("Building documentation.");
      readyExpanded = false;
      minimizedKey = null;
      renderBuilding(value);
    } else if (value.phase === "error") {
      var diagnostic = value.error;
      var summary = diagnostic && typeof diagnostic === "object" ?
        diagnostic.summary : diagnostic;
      ui.announce("Build failed. " + (cleanText(summary) || "Unknown error"));
      renderDiagnostic("error", "Build failed", value.error, value.warnings);
    } else if (Array.isArray(value.warnings) && value.warnings.length) {
      var warnings = cleanWarnings(value.warnings);
      ui.announce("Built with " + warnings.length +
        (warnings.length === 1 ? " warning. " : " warnings. ") +
        warnings.map(function (warning) { return warning.summary; }).join("; "));
      renderWarnings(value.warnings);
    } else {
      ui.announce("Watching for changes.");
      minimizedKey = null;
      renderReady();
    }
  }

  window.__updocPreviewPanel = Object.freeze({
    beforeRefresh: ui.beforeRefresh,
    viewState: function () {
      return {
        readyExpanded: readyExpanded,
        minimizedKey: minimizedKey,
        value: showingDisconnected ? { phase: "disconnected" } : lastValue
      };
    },
    initialize: function (value, view) {
      readyExpanded = !!(view && view.readyExpanded);
      minimizedKey = view && typeof view.minimizedKey === "string" ?
        view.minimizedKey : null;
      ui.initialize(function () {
        if (value.phase === "disconnected") renderDisconnected();
        else render(value);
      });
    },
    celebrateCompletion: ui.celebrateCompletion,
    render: render,
    renderDisconnected: renderDisconnected,
    showRetrying: showRetrying,
    showRetryCountdown: showRetryCountdown,
    stopRetryCountdown: stopRetryCountdown
  });
}());
