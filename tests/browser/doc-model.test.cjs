"use strict";

const assert = require("node:assert/strict");
const path = require("node:path");
const test = require("node:test");
const assets = process.argv[2] || path.resolve(__dirname, "../../inst/mascot");
const Doc = require(path.join(assets, "doc.js"));
const states = ["ready", "building", "warning", "error", "disconnected"];

test("palette and frame consumers cannot mutate later exports", () => {
  const original = Doc.svg({ state: "warning", theme: "dark" });
  const colors = Doc.palette("dark");
  colors.ink = "pink";
  const snapshot = Doc.frame({ state: "warning", theme: "dark" });
  snapshot.body.transform = "scale(0)";
  snapshot.eyes[0].opacity = 0;
  snapshot.motion["--doc-blink-delay"] = "bad";
  assert.equal(Doc.svg({ state: "warning", theme: "dark" }), original);
  assert.notEqual(Doc.palette("dark").ink, "pink");
});

test("every exported state uses the shared rest pose and theme eye color", () => {
  for (const theme of ["light", "dark"]) {
    const ink = Doc.palette(theme).ink;
    for (const state of states) {
      const snapshot = Doc.frame({ state, theme });
      const svg = Doc.svg({ state, theme });
      assert.equal(snapshot.ink, ink);
      assert.ok(svg.includes(`transform="${snapshot.body.transform}"`));
      assert.ok(svg.includes(`fill="${snapshot.color}"`));
      for (const eye of snapshot.eyes) assert.ok(svg.includes(`transform="${eye.transform}"`));
      for (const [mark] of svg.matchAll(/<(?:rect|path)\b[^>]*class="doc-eye-[^"]+"[^>]*>/g)) {
        assert.ok(mark.includes(`fill="${ink}"`) || mark.includes(`stroke="${ink}"`));
      }
      assert.equal(snapshot.expression, state === "error" ? "cross" : state === "disconnected" ? "sleep" : "open");
    }
  }
  assert.throws(() => Doc.palette("sepia"), TypeError);
  assert.throws(() => Doc.frame({ state: "unknown" }), TypeError);
});

test("motion seeds give bounded repeatable timings and varied phases", () => {
  const phases = new Set();
  for (let seed = 0; seed < 24; seed++) {
    const first = Doc.motionSettings(`mascot-${seed}`);
    assert.deepEqual(Doc.motionSettings(`mascot-${seed}`), first);
    phases.add(JSON.stringify(first));
    for (const [name, value] of Object.entries(first)) {
      assert.match(value, /^-?\d+\.\d+s$/);
      const seconds = parseFloat(value);
      assert.ok(Number.isFinite(seconds));
      if (name.endsWith("duration")) assert.ok(seconds >= 10 && seconds <= 16);
      else assert.ok(seconds >= -12 && seconds <= 0);
    }
  }
  assert.equal(phases.size, 24);
});
