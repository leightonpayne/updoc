/* Load with doc.css. create({ state, motion, gaze, label, seed }) returns a
 * persistent element and controller; call destroy() when removing it.
 * Without a label the mascot is decorative. svg(), frame(), palette(), and
 * motionSettings() also work without a browser for generated assets. */
(function (factory) {
  "use strict";
  var api = factory();
  if (typeof module === "object" && module.exports) module.exports = api;
  if (typeof window !== "undefined") window.UpdocDoc = api;
}(function () {
  "use strict";

  var SVG_NS = "http://www.w3.org/2000/svg";
  var STATES = ["ready", "building", "warning", "error", "disconnected"];
  var HEXAGON = "M20 3.8Q24 1.5 28 3.8L39.8 10.6Q43.8 12.9 43.8 17.5" +
    "V30.5Q43.8 35.1 39.8 37.4L28 44.2Q24 46.5 20 44.2" +
    "L8.2 37.4Q4.2 35.1 4.2 30.5V17.5Q4.2 12.9 8.2 10.6Z";
  var PALETTES = {
    light: { fill: "oklch(.145 0 0)", ink: "oklch(1 0 0)", warning: "oklch(.852 .199 91.936)", error: "oklch(.577 .245 27.325)" },
    dark: { fill: "oklch(.985 0 0)", ink: "oklch(.205 0 0)", warning: "oklch(.795 .184 86.047)", error: "oklch(.704 .191 22.216)" }
  };
  // CSS and SVG exports share these rest poses.
  var POSES = {
    ready: { body: [0, 0, 0], eyes: [[1, 1, 0], [1, 1, 0]], expression: "open", opacity: 1 },
    building: { body: [0, 0, 0], eyes: [[1.45, .36, 0], [1.45, .36, 0]], expression: "open", opacity: 1 },
    warning: { body: [0, -1, 10], eyes: [[1.25, .55, .75], [1.08, 1.2, -1.5]], expression: "open", opacity: 1 },
    error: { body: [0, 0, 0], eyes: [[1, 1, 0], [1, 1, 0]], expression: "cross", opacity: 1 },
    disconnected: { body: [0, 2, -10], eyes: [[1, 1, 2], [1, 1, 2]], expression: "sleep", opacity: .72 }
  };

  function choice(value, fallback, choices, name) {
    var result = value === undefined ? fallback : value;
    if (choices.indexOf(result) === -1) {
      throw new TypeError("Doc " + name + " must be one of: " + choices.join(", "));
    }
    return result;
  }

  function part(tag, attributes, children) {
    return { tag: tag, attributes: attributes || {}, children: children || [] };
  }

  function scaleEye(x, scaleX, scaleY, y) {
    var center = x + 2.25;
    return "translate(0 " + (y || 0) + ") translate(" + center + " 24) scale(" +
      scaleX + " " + scaleY + ") translate(" + (-center) + " -24)";
  }

  function palette(theme) {
    var colors = PALETTES[choice(theme, "light", ["light", "dark"], "theme")];
    return { fill: colors.fill, ink: colors.ink, warning: colors.warning, error: colors.error };
  }

  // Seeded phases keep exports repeatable and neighboring mascots independent.
  function motionSettings(seed) {
    var value = String(seed === undefined ? "doc" : seed);
    var hash = 2166136261;
    for (var i = 0; i < value.length; i++) {
      hash ^= value.charCodeAt(i);
      hash = Math.imul(hash, 16777619);
    }
    function fraction(shift) { return ((hash >>> shift) & 255) / 255; }
    return {
      "--doc-blink-duration": (12.5 + fraction(0) * 3.5).toFixed(3) + "s",
      "--doc-blink-delay": (-fraction(8) * 12).toFixed(3) + "s",
      "--doc-idle-duration": (10 + fraction(16) * 3).toFixed(3) + "s",
      "--doc-idle-delay": (-fraction(24) * 10).toFixed(3) + "s",
      "--doc-body-delay": (-fraction(8) * 4).toFixed(3) + "s"
    };
  }

  function frame(options) {
    options = options || {};
    var state = choice(options.state, "ready", STATES, "state");
    var theme = choice(options.theme, "light", ["light", "dark"], "theme");
    var colors = palette(theme);
    var pose = POSES[state];
    var body = pose.body;
    return {
      state: state,
      theme: theme,
      color: state === "warning" ? colors.warning : state === "error" ? colors.error : colors.fill,
      ink: colors.ink,
      expression: pose.expression,
      body: {
        transform: "translate(" + body[0] + " " + body[1] + ") rotate(" + body[2] + " 24 24)",
        cssTransform: "translate(" + body[0] + "px, " + body[1] + "px) rotate(" + body[2] + "deg)"
      },
      eyes: pose.eyes.map(function (eye, index) {
        return {
          transform: scaleEye(index ? 27.5 : 16, eye[0], eye[1], eye[2]),
          cssTransform: "translateY(" + eye[2] + "px) scale(" + eye[0] + ", " + eye[1] + ")",
          opacity: pose.opacity
        };
      }),
      motion: motionSettings(options.seed)
    };
  }

  function poseCSS() {
    return STATES.map(function (state) {
      var pose = frame({ state: state });
      var selector = '.doc-mascot[data-state="' + state + '"]';
      return selector + " { --doc-body-pose: " + pose.body.cssTransform + "; }\n" +
        pose.eyes.map(function (eye, index) {
          return selector + " .doc-eye--" + (index ? "right" : "left") +
            " { --doc-eye-pose: " + eye.cssTransform + "; --doc-eye-opacity: " + eye.opacity + "; }";
        }).join("\n");
    }).join("\n");
  }

  // One shape tree feeds the DOM renderer and static exporter. Static state
  // poses use SVG presentation attributes, so exports never depend on doc.css.
  function artwork(state, snapshot) {
    var staticMode = Boolean(snapshot);
    var fill = staticMode ? snapshot.color : undefined;
    var ink = staticMode ? snapshot.ink : undefined;
    var bodyAttributes = { "class": "doc-body" };
    if (staticMode) bodyAttributes.transform = snapshot.body.transform;

    var eyes = [16, 27.5].map(function (x, index) {
      var eyeAttributes = { "class": "doc-eye doc-eye--" + (index ? "right" : "left") };
      if (staticMode) {
        eyeAttributes.transform = snapshot.eyes[index].transform;
        eyeAttributes.opacity = snapshot.eyes[index].opacity;
      }

      function expression(kind, path) {
        var attributes = { "class": "doc-eye-" + kind, d: path };
        if (staticMode) {
          attributes.fill = "none";
          attributes.stroke = ink;
          attributes["stroke-width"] = "2.3";
          attributes["stroke-linecap"] = "round";
          attributes["stroke-linejoin"] = "round";
          attributes.opacity = kind === snapshot.expression ? "1" : "0";
        }
        return part("path", attributes);
      }

      return part("g", eyeAttributes, [
        part("rect", {
          "class": "doc-eye-open", x: x, y: "19.5", width: "4.5", height: "9", rx: "2.25",
          fill: ink,
          opacity: staticMode ? (snapshot.expression === "open" ? "1" : "0") : undefined
        }),
        expression("cross", "M" + (x - 1) + " 20.75l6.5 6.5m0-6.5-6.5 6.5"),
        expression("sleep", "M" + (x - 1) + " 24q3.25 2 6.5 0"),
        expression("happy", "M" + (x - 1) + " 25q3.25-5 6.5 0")
      ]);
    });

    return part("g", { "class": "doc-celebration" }, [
      part("g", bodyAttributes, [
        part("path", { "class": "doc-face", d: HEXAGON, fill: fill }),
        part("g", { "class": "doc-gaze" }, [
          part("g", { "class": "doc-idle" }, [part("g", { "class": "doc-eyes" }, eyes)])
        ])
      ])
    ]);
  }

  function eachAttribute(attributes, callback) {
    Object.keys(attributes).forEach(function (name) {
      if (attributes[name] !== undefined) callback(name, String(attributes[name]));
    });
  }

  function render(doc, tree) {
    var node = doc.createElementNS(SVG_NS, tree.tag);
    eachAttribute(tree.attributes, function (name, value) { node.setAttribute(name, value); });
    tree.children.forEach(function (child) { node.appendChild(render(doc, child)); });
    return node;
  }

  function escapeXML(value) {
    return String(value).replace(/[&<>"']/g, function (character) {
      return { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&apos;" }[character];
    });
  }

  function serialize(tree) {
    var attributes = "";
    eachAttribute(tree.attributes, function (name, value) {
      attributes += " " + name + '="' + escapeXML(value) + '"';
    });
    return "<" + tree.tag + attributes + ">" + tree.children.map(serialize).join("") + "</" + tree.tag + ">";
  }

  function svg(options) {
    options = options || {};
    var state = choice(options.state, "ready", STATES, "state");
    var theme = choice(options.theme, "light", ["light", "dark"], "theme");
    return serialize(part("svg", {
      xmlns: SVG_NS, viewBox: "0 0 48 48", width: "48", height: "48", fill: "none",
      role: "img", "aria-label": options.label === undefined ? "Doc, " + state : options.label,
      focusable: "false"
    }, [artwork(state, frame({ state: state, theme: theme, seed: options.seed }))]));
  }

  function create(options) {
    options = options || {};
    var doc = typeof document !== "undefined" ? document : null;
    var win = doc && (doc.defaultView || (typeof window !== "undefined" ? window : null));
    if (!doc || !win) throw new Error("Doc.create() requires a browser document");
    var state = choice(options.state, "ready", STATES, "state");
    var motion = choice(options.motion, "auto", ["auto", "reduce"], "motion");
    var gaze = choice(options.gaze, "pointer", ["pointer", "none"], "gaze");
    var mascot = doc.createElement("span");
    mascot.className = "doc-mascot";
    if (options.label === undefined || options.label === null || options.label === "") {
      mascot.setAttribute("aria-hidden", "true");
    } else {
      mascot.setAttribute("role", "img");
      mascot.setAttribute("aria-label", String(options.label));
    }
    var drawing = render(doc, part("svg", {
      viewBox: "0 0 48 48", fill: "none", focusable: "false", "aria-hidden": "true"
    }, [artwork(state)]));
    mascot.appendChild(drawing);
    var settings = motionSettings(options.seed === undefined ? "doc-" + nextMascot++ : options.seed);
    Object.keys(settings).forEach(function (key) { mascot.style.setProperty(key, settings[key]); });
    var celebration = drawing.firstChild;
    var preference = win.matchMedia ? win.matchMedia("(prefers-reduced-motion: reduce)") : null;
    var destroyed = false, reduced = false, timeout = null;
    var listeners = [], bridges = [];
    var look = { x: 0, y: 0, attention: 0 };
    var hub = pointerHub(win, doc), unsubscribe;
    var layers = [".doc-body", ".doc-idle", ".doc-eyes", ".doc-eye--left", ".doc-eye--right", ".doc-celebration"].map(function (selector) {
      return { node: drawing.querySelector(selector), property: "transform" };
    });
    ["open", "cross", "happy", "sleep"].forEach(function (expression) {
      Array.prototype.forEach.call(drawing.querySelectorAll(".doc-eye-" + expression), function (node) {
        layers.push({ node: node, property: "opacity" });
      });
    });
    function listen(target, event, callback, listenerOptions) {
      target.addEventListener(event, callback, listenerOptions);
      listeners.push(function () { target.removeEventListener(event, callback, listenerOptions); });
    }
    function cancelBridges() {
      var previous = bridges;
      bridges = [];
      previous.forEach(function (animation) { animation.cancel(); });
    }
    function capturePose() {
      if (reduced || doc.hidden || !win.getComputedStyle || !drawing.getAnimations || !layers[0].node.animate) return null;
      // Include any interrupted bridge in the starting pose; read before writes.
      return layers.map(function (layer) { return win.getComputedStyle(layer.node)[layer.property]; });
    }
    function bridgePose(before, changeExpression) {
      if (!before || destroyed || reduced) return;
      var duration = 320;
      // Seek CSS effects just long enough to sample the future destination, then
      // restore their clocks before paint. Their loops continue under the bridge.
      win.getComputedStyle(layers[0].node).transform;
      var running = drawing.getAnimations({ subtree: true }).filter(function (animation) {
        return (animation.animationName || animation.transitionProperty) && animation.playState === "running" &&
          typeof animation.currentTime === "number" && layers.some(function (layer) {
            return animation.effect && animation.effect.target === layer.node;
          });
      }).map(function (animation) { return { animation: animation, time: animation.currentTime }; });
      var after;
      try {
        running.forEach(function (item) { item.animation.currentTime = item.time + duration; });
        after = layers.map(function (layer) { return win.getComputedStyle(layer.node)[layer.property]; });
      } finally {
        running.forEach(function (item) { item.animation.currentTime = item.time; });
      }
      layers.forEach(function (layer, index) {
        // Let a fresh hop play its anticipation unchanged. If a hop was
        // interrupted, bridge the outer layer too so its displacement survives.
        if (layer.node === celebration && mascot.dataset.celebrating === "true" &&
            (before[index] === "none" || before[index] === "matrix(1, 0, 0, 1, 0, 0)")) return;
        var start = { offset: 0 }, end = { offset: 1 };
        start[layer.property] = before[index];
        end[layer.property] = after[index];
        var keyframes = [start];
        if (changeExpression && layer.node === drawing.querySelector(".doc-eyes")) {
          keyframes.push({ offset: 0.5, transform: "scaleY(.08)" });
        } else if (changeExpression && layer.property === "opacity") {
          keyframes.push({ offset: 0.45, opacity: before[index] }, { offset: 0.55, opacity: after[index] });
        }
        keyframes.push(end);
        var animation = layer.node.animate(keyframes, { duration: duration, easing: "ease-in-out" });
        bridges.push(animation);
        animation.onfinish = function () {
          var index = bridges.indexOf(animation);
          if (index !== -1) bridges.splice(index, 1);
        };
      });
    }
    function clearGaze() {
      if (look.attention === 0 && look.x === 0 && look.y === 0 && mascot.dataset.tracking !== "true") return;
      look = { x: 0, y: 0, attention: 0 };
      delete mascot.dataset.tracking;
      mascot.style.removeProperty("--doc-look-x");
      mascot.style.removeProperty("--doc-look-y");
      mascot.style.removeProperty("--doc-idle-weight");
    }
    function wantsGaze(pointer) {
      return !destroyed && !doc.hidden && !reduced && mascot.isConnected &&
        ((gaze === "pointer" && pointer !== null) || look.attention > 0);
    }
    function measureGaze(pointer) {
      if (destroyed || doc.hidden || reduced || !mascot.isConnected) return { clear: true };
      if (gaze === "none" || !pointer) return null;
      var rect = mascot.getBoundingClientRect();
      if (!rect.width || !rect.height) return null;
      var dx = pointer.x - (rect.left + rect.width / 2), dy = pointer.y - (rect.top + rect.height / 2);
      // Radially bounded in SVG units, with a reach adapted to the displayed size.
      var denominator = Math.hypot(dx, dy) + Math.max(100, rect.width * 1.8);
      return { x: dx / denominator * 5.5, y: dy / denominator * 4 };
    }
    function drawGaze(target, elapsed) {
      if (target && target.clear) { clearGaze(); return false; }
      if (!target && look.attention === 0) return false;
      var goal = target || { x: 0, y: 0 };
      var direction = 1 - Math.exp(-elapsed / (target ? 0.08 : 0.18));
      var attention = 1 - Math.exp(-elapsed / (target ? 0.16 : 0.24));
      look.x += (goal.x - look.x) * direction;
      look.y += (goal.y - look.y) * direction;
      look.attention += ((target ? 1 : 0) - look.attention) * attention;
      var moving = Math.abs(goal.x - look.x) > 0.005 || Math.abs(goal.y - look.y) > 0.005 ||
        Math.abs((target ? 1 : 0) - look.attention) > 0.002;
      if (!moving) {
        if (!target) { clearGaze(); return false; }
        look = { x: target.x, y: target.y, attention: 1 };
      }
      var weight = look.attention * look.attention * (3 - 2 * look.attention);
      mascot.style.setProperty("--doc-look-x", look.x.toFixed(3) + "px");
      mascot.style.setProperty("--doc-look-y", look.y.toFixed(3) + "px");
      mascot.style.setProperty("--doc-idle-weight", (1 - weight).toFixed(4));
      mascot.dataset.tracking = "true";
      return moving;
    }
    function refreshGaze() { if (!destroyed) hub.refresh(); }
    function updateMotion() {
      reduced = motion === "reduce" || Boolean(preference && preference.matches);
      mascot.dataset.motion = reduced ? "reduce" : "auto";
      if (reduced) { cancelBridges(); clearGaze(); }
      hub.refresh();
    }
    function stopCelebration(smooth) {
      if (destroyed) return;
      var active = mascot.dataset.celebrating === "true";
      var before = active && smooth !== false ? capturePose() : null;
      if (timeout !== null) win.clearTimeout(timeout);
      timeout = null;
      if (!active) return;
      cancelBridges();
      delete mascot.dataset.celebrating;
      bridgePose(before, true);
    }
    function celebrate() {
      if (destroyed || state !== "ready" || mascot.dataset.celebrating === "true") return false;
      var before = capturePose();
      cancelBridges();
      mascot.dataset.celebrating = "true";
      bridgePose(before, true);
      timeout = win.setTimeout(stopCelebration, 1200);
      return true;
    }
    function setState(next) {
      if (destroyed) return;
      next = choice(next, "ready", STATES, "state");
      if (state === next) return;
      var before = capturePose();
      var changeExpression = state === "error" || state === "disconnected" || next === "error" ||
        next === "disconnected" || mascot.dataset.celebrating === "true";
      cancelBridges();
      state = next;
      if (state !== "ready") stopCelebration(false);
      mascot.dataset.state = state;
      bridgePose(before, changeExpression);
    }
    function setMotion(next) {
      if (destroyed) return;
      next = choice(next, "auto", ["auto", "reduce"], "motion");
      if (motion === next) return;
      motion = next;
      updateMotion();
    }
    function setGaze(next) {
      if (destroyed) return;
      next = choice(next, "pointer", ["pointer", "none"], "gaze");
      if (gaze === next) return;
      gaze = next;
      mascot.dataset.gaze = gaze;
      hub.refresh();
    }
    function destroy() {
      if (destroyed) return;
      stopCelebration(false);
      cancelBridges();
      clearGaze();
      destroyed = true;
      unsubscribe();
      listeners.forEach(function (remove) { remove(); });
      listeners = [];
      if (mascot.parentNode) mascot.parentNode.removeChild(mascot);
    }
    unsubscribe = hub.subscribe({ wants: wantsGaze, measure: measureGaze, draw: drawGaze, clear: clearGaze });
    listen(celebration, "animationend", function (event) {
      if (event.target === celebration && event.animationName === "doc-celebrate") stopCelebration();
    });
    if (preference) {
      if (typeof preference.addEventListener === "function") listen(preference, "change", updateMotion);
      else if (typeof preference.addListener === "function") {
        preference.addListener(updateMotion);
        listeners.push(function () { preference.removeListener(updateMotion); });
      }
    }
    mascot.dataset.state = state;
    mascot.dataset.gaze = gaze;
    updateMotion();
    return {
      element: mascot, setState: setState, setMotion: setMotion, setGaze: setGaze,
      celebrate: celebrate, stopCelebration: stopCelebration, refreshGaze: refreshGaze, destroy: destroy
    };
  }

  var nextMascot = 0;
  // Homepage and injected-preview copies share one pointer stream.
  var pointerHubKey = typeof Symbol === "function" ? Symbol.for("updoc.doc.pointerHub") : "__updocDocPointerHub";
  // Batch reads before writes and stop scheduling when every gaze settles.
  function pointerHub(win, doc) {
    if (win[pointerHubKey]) return win[pointerHubKey];
    var members = [], pointer = null, frame = null, last = null, remove = [];
    function refresh() {
      var busy = members.some(function (member) { return member.wants(pointer); });
      if (!busy) {
        if (frame !== null) win.cancelAnimationFrame(frame);
        frame = null;
        last = null;
      } else if (frame === null) frame = win.requestAnimationFrame(tick);
    }
    function tick(now) {
      frame = null;
      now = typeof now === "number" ? now : (last === null ? 0 : last + 1000 / 60);
      var elapsed = last === null ? 1 / 60 : Math.max(0.001, Math.min((now - last) / 1000, 0.05));
      last = now;
      var ready = members.map(function (member) { return { member: member, target: member.measure(pointer) }; });
      var moving = false;
      ready.forEach(function (item) { moving = item.member.draw(item.target, elapsed) || moving; });
      if (moving) frame = win.requestAnimationFrame(tick);
      else last = null;
    }
    function listen(target, name, callback, options) {
      target.addEventListener(name, callback, options);
      remove.push(function () { target.removeEventListener(name, callback, options); });
    }
    function leave(immediate) {
      pointer = null;
      if (immediate === true) members.forEach(function (member) { member.clear(); });
      refresh();
    }
    function attach() {
      listen(win, "pointermove", function (event) {
        if (event.pointerType === "touch") { leave(true); return; }
        pointer = { x: event.clientX, y: event.clientY };
        refresh();
      }, { passive: true });
      listen(win, "pointerout", function (event) { if (!event.relatedTarget) leave(false); }, { passive: true });
      listen(win, "pointercancel", function () { leave(false); }, { passive: true });
      listen(win, "pointerup", function (event) { if (event.pointerType !== "mouse") leave(false); }, { passive: true });
      listen(win, "blur", function () { leave(false); });
      listen(win, "resize", refresh, { passive: true });
      listen(doc, "scroll", refresh, { passive: true, capture: true });
      listen(doc, "visibilitychange", function () { if (doc.hidden) leave(true); });
    }
    var hub = {
      refresh: refresh,
      subscribe: function (member) {
        if (!members.length) attach();
        members.push(member);
        return function () {
          var index = members.indexOf(member);
          if (index === -1) return;
          members.splice(index, 1);
          if (!members.length) {
            remove.forEach(function (detach) { detach(); });
            remove = [];
            pointer = null;
            delete win[pointerHubKey];
          }
          refresh();
        };
      }
    };
    win[pointerHubKey] = hub;
    return hub;
  }

  return { create: create, svg: svg, palette: palette, frame: frame, motionSettings: motionSettings, poseCSS: poseCSS };
}));
