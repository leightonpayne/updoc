"use strict";

const fs = require("node:fs");
const path = require("node:path");
const Doc = require("../inst/mascot/doc.js");
const destination = path.resolve(__dirname, "../pkgdown/assets/doc-assets");
const figures = path.resolve(__dirname, "../man/figures");
const check = process.argv.includes("--check");
let stale = false;
function output(name, expected, directory = destination) {
  const file = path.join(directory, name);
  const actual = fs.existsSync(file) ? fs.readFileSync(file, "utf8") : null;
  if (check) {
    if (actual !== expected) {
      console.error(`Regenerate ${path.relative(process.cwd(), file)}`);
      stale = true;
    }
  } else if (actual !== expected) {
    fs.mkdirSync(directory, { recursive: true });
    fs.writeFileSync(file, expected);
    console.log(path.relative(process.cwd(), file));
  }
}

const generatedSources = new Map();
function generated(file, label, content) {
  const absolute = path.resolve(__dirname, "..", file);
  const source = generatedSources.get(absolute) || fs.readFileSync(absolute, "utf8");
  const begin = `/* BEGIN GENERATED ${label} */`;
  const end = `/* END GENERATED ${label} */`;
  const start = source.indexOf(begin), finish = source.indexOf(end, start);
  if (start < 0 || finish < 0) throw new Error(`Missing generated block ${label} in ${file}`);
  const indent = source.slice(source.lastIndexOf("\n", start) + 1, start);
  const expected = source.slice(0, start + begin.length) + "\n" + content + "\n" + indent + source.slice(finish);
  generatedSources.set(absolute, expected);
  return expected;
}

function paletteProperties(theme, prefix, indent = "  ") {
  return Object.entries(Doc.palette(theme)).map(([key, value]) => `${indent}${prefix}${key}: ${value};`).join("\n");
}

generated("inst/mascot/doc.css", "DOC DEFAULT PALETTE", `.doc-mascot {\n${paletteProperties("light", "--doc-default-")}\n}`);
const stylesheet = generated("inst/mascot/doc.css", "DOC POSES", Doc.poseCSS());
generated("pkgdown/extra.css", "DOC PALETTE", `.doc-intro {\n${paletteProperties("light", "--doc-")}\n}\n` +
  `[data-bs-theme="dark"] .doc-intro {\n${paletteProperties("dark", "--doc-")}\n}`);
for (const theme of ["light", "dark"]) {
  generated("inst/preview/panel.css", `DOC ${theme.toUpperCase()} PALETTE`, paletteProperties(theme, "--updoc-avatar-"));
}
for (const [file, content] of generatedSources) output(path.basename(file), content, path.dirname(file));

// README images cannot run the browser controller. Embed its CSS animations
// around the canonical SVG exports, keeping their state-specific colors.
function readmeMascots(theme, animated = true) {
  const mascots = ["ready", "warning", "error"].map((state, index) => {
    const frame = Doc.frame({ state, theme, seed: `readme-${state}` });
    const svg = Doc.svg({ state, theme }).replace('width="48" height="48"', 'width="56" height="56"');
    const position = `data-state="${state}" x="${8 + index * 76}" y="12"`;
    if (!animated) return svg.replace("<svg ", `<svg ${position} `);
    const motion = Object.entries(frame.motion).map(([name, value]) => `${name}:${value}`).join(";");
    return svg.replace("<svg ", `<svg class="doc-mascot doc-mascot--decorative" ` +
      `${position} ` +
      `style="--doc-size:56px;--doc-color:${frame.color};--doc-eye-color:${frame.ink};${motion}" `);
  });
  const animation = animated ? `<style><![CDATA[
${stylesheet}
svg.doc-mascot { overflow: visible; }
@media (prefers-reduced-motion: reduce) {
  .doc-mascot, .doc-mascot * { animation: none !important; transition: none !important; }
  .doc-mascot .doc-celebration, .doc-mascot .doc-gaze,
  .doc-mascot .doc-idle, .doc-mascot .doc-eyes { transform: none !important; }
}
]]></style>` : "";
  return `<svg xmlns="http://www.w3.org/2000/svg" width="224" height="80" viewBox="0 0 224 80" role="img" aria-label="Doc in ready, warning, and error states">
${animation}
${mascots.join("\n")}
</svg>\n`;
}

function readmeStillMascots() {
  // GitHub rewrites source media queries that select its site theme. Keep the
  // motion source independent, and let its SVG inherit the page's color scheme.
  const darkColors = ["ready", "warning", "error"].map(state => {
    const frame = Doc.frame({ state, theme: "dark" });
    return `[data-state="${state}"] .doc-face { fill: ${frame.color}; }
[data-state="${state}"] .doc-eye-open { fill: ${frame.ink}; }
[data-state="${state}"] .doc-eye path { stroke: ${frame.ink}; }`;
  }).join("\n");
  return readmeMascots("light", false).replace("\n", `\n<style>
@media (prefers-color-scheme: dark) {
${darkColors}
}
</style>\n`);
}

for (const theme of ["light", "dark"]) {
  output(`doc-${theme}.svg`, Doc.svg({ state: "ready", theme, label: "Doc, updoc’s mascot" }) + "\n");
  output(`readme-mascots-${theme}.svg`, readmeMascots(theme), figures);
}
output("readme-mascots-still.svg", readmeStillMascots(), figures);
for (const name of ["doc.js", "doc.css"]) {
  output(name, fs.readFileSync(path.resolve(__dirname, "../inst/mascot", name), "utf8"));
}
if (stale) process.exitCode = 1;
