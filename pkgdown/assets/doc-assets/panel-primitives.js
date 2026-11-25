(function () {
  "use strict";

  var previewScript = document.currentScript;
  var panelTheme = previewScript && previewScript.dataset.updocTheme || "auto";
  var panelTransition = 0;
  var panelAvatar = null;
  var panelDoc = null;
  var initializing = false;

  function cleanText(value) {
    return String(value || "")
      .replace(/\u001b\[[0-?]*[ -/]*[@-~]/g, "")
      .replace(/\r/g, "")
      .trim();
  }

  function element(tag, className, text) {
    var node = document.createElement(tag);
    if (className) node.className = className;
    if (text !== undefined && text !== null) node.textContent = text;
    return node;
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

  function preferredTheme() {
    if (panelTheme === "light" || panelTheme === "dark") return panelTheme;
    var theme = pageTheme() || (window.matchMedia &&
      window.matchMedia("(prefers-color-scheme: dark)").matches ?
      "dark" : "light");
    if (panelTheme === "inverse") {
      return theme === "dark" ? "light" : "dark";
    }
    return theme;
  }

  function watchTheme(host) {
    var update = function () {
      host.dataset.theme = preferredTheme();
    };
    var options = {
      attributes: true,
      attributeFilter: ["class", "data-bs-theme", "data-theme"]
    };

    update();
    if (panelTheme === "light" || panelTheme === "dark") return;
    if (typeof MutationObserver === "function") {
      var observer = new MutationObserver(update);
      observer.observe(document.documentElement, options);
      if (document.body) observer.observe(document.body, options);
    }

    if (window.matchMedia) {
      var preference = window.matchMedia("(prefers-color-scheme: dark)");
      if (typeof preference.addEventListener === "function") {
        preference.addEventListener("change", update);
      } else if (typeof preference.addListener === "function") {
        preference.addListener(update);
      }
    }
  }

  function interfaceRoot() {
    var host = document.getElementById("__updoc_status");
    if (host) {
      var existingRoot = host.shadowRoot || host;
      return existingRoot.querySelector(".updoc-stage");
    }

    host = document.createElement("div");
    host.id = "__updoc_status";
    var shadow = typeof host.attachShadow === "function" ?
      host.attachShadow({ mode: "open" }) : host;
    var bootstrap = window.__updocPreviewBootstrap;
    var inlineCSS = bootstrap && bootstrap.css;
    var style = document.createElement(inlineCSS ? "style" : "link");
    var stage = element("div", "updoc-stage");
    var announcement = element("div", "ui-sr-only updoc-announcement");
    announcement.setAttribute("role", "status");
    announcement.setAttribute("aria-live", "polite");
    announcement.setAttribute("aria-atomic", "true");
    stage.appendChild(announcement);
    stage.hidden = !inlineCSS;
    if (inlineCSS) {
      style.textContent = inlineCSS;
    } else {
      style.rel = "stylesheet";
      style.href = "/__updoc__/panel.css";
      style.addEventListener("load", function () { stage.hidden = false; });
      style.addEventListener("error", function () {
        window.console.warn("updoc: browser panel stylesheet failed to load");
      });
    }
    shadow.appendChild(style);
    shadow.appendChild(stage);
    document.body.appendChild(host);
    watchTheme(host);
    return stage;
  }

  function announce(text) {
    var announcement = interfaceRoot().querySelector(".updoc-announcement");
    if (announcement.textContent !== text) announcement.textContent = text;
  }

  function panelShell() {
    var stage = interfaceRoot();
    var shell = stage.querySelector(".updoc-shell");
    if (!shell) {
      shell = element("section", "ui-card updoc-shell");
      shell.dataset.slot = "card";
      shell.dataset.size = "default";
      shell.dataset.expanded = "false";
      shell.addEventListener("transitionend", function (event) {
        if (event.target === shell && panelAvatar) panelDoc.refreshGaze();
      });
      stage.appendChild(shell);
    }
    return shell;
  }

  function updateScrollArea(area) {
    if (!area || !area.isConnected) return;
    var viewport = area.querySelector(".ui-scroll-area-viewport");
    var scrollbar = area.querySelector(".ui-scroll-area-scrollbar");
    var thumb = area.querySelector(".ui-scroll-area-thumb");
    if (!viewport || !scrollbar || !thumb) return;

    var scrollRange = viewport.scrollHeight - viewport.clientHeight;
    var overflow = scrollRange > 1;
    area.dataset.overflow = overflow ? "true" : "false";
    if (overflow) {
      viewport.setAttribute("tabindex", "0");
    } else {
      viewport.removeAttribute("tabindex");
      viewport.scrollTop = 0;
    }
    area.parentNode.style.setProperty("--updoc-avatar-scroll", -viewport.scrollTop + "px");
    updateAvatarPlacement(area.parentNode, viewport.scrollTop);

    var trackHeight = Math.max(0, scrollbar.clientHeight - 2);
    var thumbHeight = overflow ? Math.max(
      18,
      trackHeight * viewport.clientHeight / viewport.scrollHeight
    ) : trackHeight;
    var thumbRange = Math.max(0, trackHeight - thumbHeight);
    var thumbTop = scrollRange > 0 ?
      thumbRange * viewport.scrollTop / scrollRange : 0;
    thumb.style.height = thumbHeight + "px";
    thumb.style.transform = "translateY(" + thumbTop + "px)";
  }

  function updateAvatarPlacement(shell, scrollTop) {
    var header = shell.querySelector(".ui-card-header--avatar");
    var title = header && header.querySelector(".ui-card-title");
    if (!title) return;
    var titleRect = title.getBoundingClientRect();
    if (!titleRect.height) return;
    var description = header.querySelector(".ui-card-description");
    var bottom = description ? description.getBoundingClientRect().bottom : titleRect.bottom;
    var offset = scrollTop - shell.getBoundingClientRect().top - shell.clientTop;
    // Compact titles start at the header top. Use that destination even when a
    // full one-line header has centered its title beside the larger face.
    shell.style.setProperty("--updoc-avatar-compact-y",
      (header.getBoundingClientRect().top + offset + titleRect.height / 2 - 14).toFixed(2) + "px");
    shell.style.setProperty("--updoc-avatar-expanded-y",
      ((titleRect.top + bottom) / 2 + offset - 22).toFixed(2) + "px");
  }

  function installScrollArea(shell) {
    var children = Array.prototype.filter.call(shell.childNodes, function (child) {
      return child !== panelAvatar;
    });
    var area = element("div", "ui-scroll-area");
    area.dataset.slot = "scroll-area";
    area.dataset.overflow = "false";
    var viewport = element("div", "ui-scroll-area-viewport");
    viewport.dataset.slot = "scroll-area-viewport";
    var content = element("div", "ui-scroll-area-content");
    content.dataset.slot = "scroll-area-content";
    children.forEach(function (child) { content.appendChild(child); });
    viewport.appendChild(content);

    var scrollbar = element("div", "ui-scroll-area-scrollbar");
    scrollbar.dataset.slot = "scroll-area-scrollbar";
    scrollbar.dataset.orientation = "vertical";
    scrollbar.setAttribute("aria-hidden", "true");
    var thumb = element("div", "ui-scroll-area-thumb");
    thumb.dataset.slot = "scroll-area-thumb";
    scrollbar.appendChild(thumb);
    area.appendChild(viewport);
    area.appendChild(scrollbar);
    shell.appendChild(area);
    updateScrollArea(area);

    var update = function () { updateScrollArea(area); };
    viewport.addEventListener("scroll", update, { passive: true });

    var drag = null;
    thumb.addEventListener("pointerdown", function (event) {
      event.preventDefault();
      event.stopPropagation();
      drag = { y: event.clientY, scrollTop: viewport.scrollTop };
      thumb.setPointerCapture(event.pointerId);
    });
    thumb.addEventListener("pointermove", function (event) {
      if (!drag) return;
      var trackHeight = Math.max(1, scrollbar.clientHeight - 2);
      var thumbRange = Math.max(1, trackHeight - thumb.offsetHeight);
      var scrollRange = Math.max(0, viewport.scrollHeight - viewport.clientHeight);
      viewport.scrollTop = drag.scrollTop +
        (event.clientY - drag.y) * scrollRange / thumbRange;
    });
    var stopDrag = function (event) {
      drag = null;
      if (thumb.hasPointerCapture(event.pointerId)) {
        thumb.releasePointerCapture(event.pointerId);
      }
    };
    thumb.addEventListener("pointerup", stopDrag);
    thumb.addEventListener("pointercancel", stopDrag);
    scrollbar.addEventListener("pointerdown", function (event) {
      if (event.target !== scrollbar) return;
      var rect = scrollbar.getBoundingClientRect();
      var ratio = (event.clientY - rect.top) / Math.max(1, rect.height);
      viewport.scrollTop = ratio * viewport.scrollHeight -
        viewport.clientHeight / 2;
    });

    var observer = typeof ResizeObserver === "function" ?
      new ResizeObserver(update) : null;
    if (observer) {
      observer.observe(viewport);
      observer.observe(content);
    } else {
      window.addEventListener("resize", update);
    }
    shell._updocScrollCleanup = function () {
      if (observer) observer.disconnect();
      else window.removeEventListener("resize", update);
    };
    window.requestAnimationFrame(update);
  }

  function showPanel(kind, state, expanded, className, buildContent, updateContent) {
    var shell = panelShell();
    if (initializing) shell.dataset.initializing = "true";
    var avatar = panelAvatar || (panelAvatar = botAvatar());
    if (avatar.parentNode !== shell) shell.appendChild(avatar);
    state = state || kind;
    // A completed build celebrates once, after the drawer settles. Retain that
    // intent across ready updates, and cancel it if another state interrupts.
    if (kind !== "ready") {
      shell._updocCelebrateOnReady = false;
      panelDoc.stopCelebration();
    } else if (shell._updocTargetKind === "building") {
      shell._updocCelebrateOnReady = true;
    }
    shell._updocTargetKind = kind;
    // Progress ticks must not detach animated nodes or restart their timelines.
    if (updateContent && shell.dataset.updocState === state &&
        shell._updocTargetState === state && shell._updocTargetExpanded === expanded &&
        updateContent(shell)) {
      updateScrollArea(shell.querySelector(".ui-scroll-area"));
      panelDoc.refreshGaze();
      return;
    }
    var wasExpanded = shell.dataset.expanded === "true";
    // A closing drawer already reports collapsed, although its tall contents
    // are still moving. Preserve that visible geometry when a build reopens it.
    var interruptedCollapse = expanded && !!shell._updocCollapseCleanup;
    var transformFrom = interruptedCollapse ?
      window.getComputedStyle(shell).transform : null;
    var leavingProgress = state !== "building" &&
      shell.querySelector('.updoc-build-progress[data-visible="true"]');
    var collapseHeight = !expanded && wasExpanded &&
      (shell.dataset.resizing === "true" || leavingProgress) ?
      shell.getBoundingClientRect().height : null;
    var resizeFrom = expanded && (wasExpanded || interruptedCollapse) ?
      shell.getBoundingClientRect().height : null;
    var transition = ++panelTransition;
    if (shell._updocResizeCleanup) shell._updocResizeCleanup();
    if (shell._updocCollapseCleanup) shell._updocCollapseCleanup();
    if (shell._updocProgressCleanup) shell._updocProgressCleanup();
    if (interruptedCollapse) shell.style.transform = transformFrom;
    shell._updocTargetState = state;
    shell._updocTargetExpanded = expanded;

    function applyContent() {
      if (transition !== panelTransition) return;
      var root = shell.getRootNode();
      var focused = root.activeElement;
      var hadFocus = focused && (focused === shell || shell.contains(focused));
      shell.style.height = "";
      delete shell.dataset.resizing;
      shell.className = "ui-card updoc-shell" + (className ? " " + className : "");
      shell.dataset.size = "sm";
      shell.dataset.updocState = state;
      shell.dataset.expanded = expanded &&
        (wasExpanded || interruptedCollapse || initializing) ? "true" : "false";
      shell.dataset.interactive = "false";
      shell.removeAttribute("title");
      shell.removeAttribute("aria-label");
      shell.removeAttribute("role");
      shell.removeAttribute("tabindex");
      shell.onclick = null;
      shell.onkeydown = null;
      if (shell._updocScrollCleanup) {
        shell._updocScrollCleanup();
        shell._updocScrollCleanup = null;
      }
      Array.prototype.slice.call(shell.childNodes).forEach(function (child) {
        if (child !== avatar) child.remove();
      });
      shell.style.setProperty("--updoc-avatar-scroll", "0px");
      buildContent(shell);
      panelDoc.setState(kind);
      var header = shell.querySelector(".ui-card-header");
      if (header) {
        header.classList.add("ui-card-header--avatar");
        header.classList.toggle("ui-card-header--avatar-full", expanded);
        var slot = element("span", "updoc-avatar-slot");
        slot.setAttribute("aria-hidden", "true");
        header.prepend(slot);
      }
      installScrollArea(shell);
      if (hadFocus) {
        // Keep keyboard users in the drawer when its focused control is replaced.
        var focusTarget = expanded ? shell.querySelector("button") : shell;
        if (!focusTarget) {
          shell.setAttribute("tabindex", "-1");
          focusTarget = shell;
        }
        focusTarget.focus({ preventScroll: true });
      }
      panelDoc.refreshGaze();
      if (shell._updocCelebrateOnReady && kind === "ready") {
        shell._updocCelebrateOnReady = false;
        panelDoc.celebrate();
      }
    }

    function animateResize() {
      if (resizeFrom === null || transition !== panelTransition) return;
      shell.style.height = "";
      var resizeTo = shell.getBoundingClientRect().height;
      var enteringProgress = shell.querySelector(".updoc-build-progress");
      if (enteringProgress && enteringProgress._updocProgressVisible) {
        // The reveal starts at zero height. Include its final height in the
        // drawer's destination so clearing the fixed height cannot add a jump.
        var meter = enteringProgress.querySelector(".ui-progress");
        resizeTo += Math.max(0, meter.getBoundingClientRect().height -
          enteringProgress.getBoundingClientRect().height);
        resizeTo = Math.min(resizeTo,
          parseFloat(window.getComputedStyle(shell).maxHeight) || resizeTo);
      }
      var resizingHeight = Math.abs(resizeFrom - resizeTo) >= 1;
      if (!resizingHeight && !interruptedCollapse) return;

      shell.style.height = resizeFrom + "px";
      shell.dataset.resizing = "true";
      shell.getBoundingClientRect();
      window.requestAnimationFrame(function () {
        if (transition !== panelTransition) return;
        shell.style.height = resizeTo + "px";
        if (interruptedCollapse) shell.style.transform = "translateY(-16px)";
      });

      var settled = false;
      var timeout;
      var onResizeEnd = function (event) {
        if (event.target === shell && event.propertyName ===
            (resizingHeight ? "height" : "transform")) finish();
      };
      var cleanup = function () {
        window.clearTimeout(timeout);
        shell.removeEventListener("transitionend", onResizeEnd);
        shell._updocResizeCleanup = null;
        if (interruptedCollapse) shell.style.transform = "";
      };
      var finish = function () {
        if (settled) return;
        settled = true;
        cleanup();
        shell.style.height = "";
        delete shell.dataset.resizing;
      };
      // A replacement transition has already sampled the visible height. Only
      // settling may release it; cancellation must not first snap to auto.
      shell._updocResizeCleanup = cleanup;
      shell.addEventListener("transitionend", onResizeEnd);
      timeout = window.setTimeout(finish, 340);
    }

    function commitPanel() {
      if (transition !== panelTransition) return;
      if (initializing) {
        applyContent();
        window.requestAnimationFrame(function () {
          window.requestAnimationFrame(function () {
            delete shell.dataset.initializing;
          });
        });
        return;
      }
      if (!expanded && wasExpanded) {
        if (collapseHeight !== null) {
          shell.style.height = collapseHeight + "px";
          shell.dataset.resizing = "true";
        }
        shell.dataset.expanded = "false";
        var settled = false;
        var timeout;
        var cleanup = function () {
          window.clearTimeout(timeout);
          shell.removeEventListener("transitionend", onCollapseEnd);
          shell._updocCollapseCleanup = null;
        };
        var finish = function () {
          if (settled || transition !== panelTransition) return;
          settled = true;
          cleanup();
          applyContent();
        };
        var onCollapseEnd = function (event) {
          if (event.target === shell && event.propertyName === "transform") finish();
        };
        shell._updocCollapseCleanup = cleanup;
        shell.addEventListener("transitionend", onCollapseEnd);
        timeout = window.setTimeout(finish, 340);
        return;
      }

      applyContent();
      animateResize();
      if (expanded && !wasExpanded && !interruptedCollapse) {
        shell.dataset.expanded = "false";
        window.requestAnimationFrame(function () {
          window.requestAnimationFrame(function () {
            if (transition === panelTransition) shell.dataset.expanded = "true";
          });
        });
      }
    }

    if (leavingProgress) {
      setProgressVisible(leavingProgress, false);
      if (expanded) {
        // Finish the bar's exit before replacing it with expanded diagnostics.
        var exitTimer = window.setTimeout(function () {
          shell._updocProgressCleanup = null;
          if (transition !== panelTransition) return;
          resizeFrom = wasExpanded ? shell.getBoundingClientRect().height : null;
          commitPanel();
        }, 220);
        shell._updocProgressCleanup = function () {
          window.clearTimeout(exitTimer);
          shell._updocProgressCleanup = null;
        };
        return;
      }
    }
    commitPanel();
  }

  // The popup owns placement; Doc owns the persistent face and its behavior.
  function botAvatar() {
    var avatar = element("span", "updoc-avatar");
    avatar.setAttribute("aria-hidden", "true");
    panelDoc = window.UpdocDoc.create();
    avatar.appendChild(panelDoc.element);
    return avatar;
  }

  function badge(text, variant) {
    var node = element("span", "ui-badge ui-badge--" + (variant || "outline"));
    node.dataset.slot = "badge";
    node.dataset.variant = variant || "outline";
    node.appendChild(document.createTextNode(text));
    return node;
  }

  function icon(name, className) {
    var node = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    node.setAttribute("viewBox", "0 0 24 24");
    node.setAttribute("fill", "none");
    node.setAttribute("stroke", "currentColor");
    node.setAttribute("stroke-width", "2");
    node.setAttribute("stroke-linecap", "round");
    node.setAttribute("stroke-linejoin", "round");
    if (className) node.setAttribute("class", className);
    if (name === "loader") {
      var path = document.createElementNS("http://www.w3.org/2000/svg", "path");
      path.setAttribute("d", "M21 12a9 9 0 1 1-6.219-8.56");
      node.appendChild(path);
    } else if (name === "circle-alert") {
      var circle = document.createElementNS("http://www.w3.org/2000/svg", "circle");
      circle.setAttribute("cx", "12");
      circle.setAttribute("cy", "12");
      circle.setAttribute("r", "10");
      node.appendChild(circle);
      var mark = document.createElementNS("http://www.w3.org/2000/svg", "path");
      mark.setAttribute("d", "M12 8v4m0 4h.01");
      node.appendChild(mark);
    } else if (name === "circle-check") {
      var circle = document.createElementNS("http://www.w3.org/2000/svg", "circle");
      circle.setAttribute("cx", "12");
      circle.setAttribute("cy", "12");
      circle.setAttribute("r", "10");
      node.appendChild(circle);
      var check = document.createElementNS("http://www.w3.org/2000/svg", "path");
      check.setAttribute("d", "m9 12 2 2 4-4");
      node.appendChild(check);
    } else if (name === "refresh-cw") {
      [
        "M21 12a9 9 0 0 0-15.17-6.55L3 8",
        "M3 3v5h5",
        "M3 12a9 9 0 0 0 15.17 6.55L21 16",
        "M16 16h5v5"
      ].forEach(function (shape) {
        var path = document.createElementNS("http://www.w3.org/2000/svg", "path");
        path.setAttribute("d", shape);
        node.appendChild(path);
      });
    } else if (name === "wifi-off") {
      [
        "M12 20h.01",
        "M8.5 16.429a5 5 0 0 1 7 0",
        "M5 12.859a10 10 0 0 1 5.17-2.69",
        "M19 12.859a10 10 0 0 0-2.007-1.523",
        "M2 8.82a15 15 0 0 1 4.177-2.643",
        "M22 8.82a15 15 0 0 0-11.288-3.764",
        "m2 2 20 20"
      ].forEach(function (shape) {
        var path = document.createElementNS("http://www.w3.org/2000/svg", "path");
        path.setAttribute("d", shape);
        node.appendChild(path);
      });
    } else if (name === "chevron-up") {
      var path = document.createElementNS("http://www.w3.org/2000/svg", "path");
      path.setAttribute("d", "m18 15-6-6-6 6");
      node.appendChild(path);
    } else {
      var path = document.createElementNS("http://www.w3.org/2000/svg", "path");
      path.setAttribute("d", "M5 12h14");
      node.appendChild(path);
    }
    return node;
  }

  function progress(current, total) {
    var node = element("div", "ui-progress");
    node.dataset.slot = "progress";
    node.setAttribute("role", "progressbar");
    node.setAttribute("aria-label", "Documentation build progress");
    var track = element("div", "ui-progress-track");
    track.dataset.slot = "progress-track";
    var indicator = element("span", "ui-progress-indicator");
    indicator.dataset.slot = "progress-indicator";
    track.appendChild(indicator);
    node.appendChild(track);
    updateProgress(node, current, total);
    return node;
  }

  function updateProgress(node, current, total) {
    var determinate = Number.isFinite(current) && Number.isFinite(total) && total > 0;
    var indicator = node.querySelector(".ui-progress-indicator");
    node.classList.toggle("ui-progress--indeterminate", !determinate);
    if (determinate) {
      var value = Math.min(100, Math.max(0, current / total * 100));
      node.setAttribute("aria-valuemin", "0");
      node.setAttribute("aria-valuemax", String(total));
      node.setAttribute("aria-valuenow", String(current));
      indicator.style.width = value + "%";
    } else {
      node.removeAttribute("aria-valuemin");
      node.removeAttribute("aria-valuemax");
      node.removeAttribute("aria-valuenow");
      // Retain the last fill while the progress container fades closed.
    }
  }

  function setProgressVisible(content, visible) {
    content._updocProgressVisible = visible;
    content.setAttribute("aria-hidden", String(!visible));
    if (initializing || content.isConnected) {
      content.dataset.visible = String(visible);
      return;
    }
    content.dataset.visible = "false";
    window.requestAnimationFrame(function () {
      window.requestAnimationFrame(function () {
        if (content.isConnected) {
          content.dataset.visible = String(content._updocProgressVisible);
        }
      });
    });
  }

  function actionButton(label, iconName, onClick) {
    var node = element("button", "ui-button ui-button--ghost");
    node.type = "button";
    node.dataset.slot = "button";
    node.dataset.variant = "ghost";
    node.dataset.size = "icon-sm";
    node.title = label;
    node.setAttribute("aria-label", label);
    node.appendChild(icon(
      iconName === "disclosure" ? "chevron-up" : iconName,
      iconName === "disclosure" ? "ui-disclosure-icon" : null
    ));
    node.addEventListener("click", function (event) {
      event.stopPropagation();
      onClick(event);
    });
    return node;
  }

  function defaultButton(label, iconName, onClick) {
    var node = element("button", "ui-button ui-button--default");
    node.type = "button";
    node.dataset.slot = "button";
    node.dataset.variant = "default";
    node.dataset.size = "sm";
    node.appendChild(icon(iconName));
    node.appendChild(element("span", null, label));
    node.addEventListener("click", function (event) {
      event.stopPropagation();
      onClick(node);
    });
    return node;
  }

  function disclosureIndicator() {
    var node = element("span", "ui-button ui-button--ghost ui-button--static");
    node.dataset.slot = "button";
    node.dataset.variant = "ghost";
    node.dataset.size = "icon-sm";
    node.setAttribute("aria-hidden", "true");
    node.appendChild(icon("chevron-up", "ui-disclosure-icon"));
    return node;
  }

  function cardHeader(title, description, action, leading) {
    var header = element("div", "ui-card-header");
    header.dataset.slot = "card-header";
    var titleNode = element("div", "ui-card-title");
    titleNode.dataset.slot = "card-title";
    if (leading) {
      var row = element("div", "ui-card-title-row");
      row.appendChild(leading);
      row.appendChild(element("span", "ui-card-title-text", title));
      titleNode.appendChild(row);
    } else {
      titleNode.appendChild(element("span", "ui-card-title-text", title));
    }
    header.appendChild(titleNode);
    if (description) {
      var descriptionNode = element("div", "ui-card-description", description);
      descriptionNode.dataset.slot = "card-description";
      header.appendChild(descriptionNode);
    }
    if (action) {
      var actionClass = "ui-card-action" +
        (action.dataset.align === "title" ? " ui-card-action--build" : "");
      var actionNode = element("div", actionClass);
      actionNode.dataset.slot = "card-action";
      actionNode.appendChild(action);
      header.appendChild(actionNode);
    }
    return header;
  }

  function collapsibleHeader(header, onCollapse) {
    header.classList.add("ui-card-header--interactive");
    header.addEventListener("click", onCollapse);
    return header;
  }

  function beforeRefresh(callback) {
    var shell = panelAvatar && panelAvatar.parentNode;
    if (!shell || document.hidden || !window.requestAnimationFrame ||
        (window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches)) {
      callback();
      return;
    }
    var frame = null;
    var timeout;
    var finished = false;
    function finish() {
      if (finished) return;
      finished = true;
      window.clearTimeout(timeout);
      if (frame !== null) window.cancelAnimationFrame(frame);
      callback();
    }
    function check() {
      if (finished) return;
      var pending = shell._updocResizeCleanup || shell._updocCollapseCleanup ||
        shell._updocProgressCleanup ||
        shell.dataset.expanded !== String(shell._updocTargetExpanded);
      var moving = shell.getAnimations &&
        shell.getAnimations({ subtree: true }).some(function (animation) {
          var target = animation.effect && animation.effect.target;
          return (target === shell || target === panelAvatar ||
            (target && target.classList.contains("updoc-build-progress"))) &&
            (animation.pending || animation.playState === "running");
        });
      if (document.hidden || (!pending && !moving)) finish();
      else frame = window.requestAnimationFrame(check);
    }
    // Save a settled drawer, rather than a logical destination halfway through
    // its motion. Also cover the two-frame delay before an opening starts.
    timeout = window.setTimeout(finish, 650);
    frame = window.requestAnimationFrame(check);
  }

  window.__updocPanelPrimitives = Object.freeze({
    beforeRefresh: beforeRefresh,
    initialize: function (render) {
      initializing = true;
      try { render(); } finally { initializing = false; }
    },
    celebrateCompletion: function () {
      if (panelAvatar && panelDoc.element.dataset.state === "ready") {
        panelDoc.celebrate();
      } else if (panelAvatar && panelAvatar.parentNode._updocTargetKind === "ready") {
        panelAvatar.parentNode._updocCelebrateOnReady = true;
      }
    },
    cleanText: cleanText,
    announce: announce,
    element: element,
    showPanel: showPanel,
    badge: badge,
    icon: icon,
    progress: progress,
    updateProgress: updateProgress,
    setProgressVisible: setProgressVisible,
    actionButton: actionButton,
    defaultButton: defaultButton,
    disclosureIndicator: disclosureIndicator,
    cardHeader: cardHeader,
    collapsibleHeader: collapsibleHeader
  });
}());
