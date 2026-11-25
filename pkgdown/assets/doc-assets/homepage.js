(function () {
  "use strict";
  var intro = document.querySelector(".doc-intro");
  if (!intro || !window.UpdocDoc || intro.dataset.initialized) return;
  var examples = [];
  intro.querySelectorAll("[data-doc-intro-state]").forEach(function (mount) {
    var doc = window.UpdocDoc.create({ state: mount.dataset.docIntroState });
    doc.element.classList.add("doc-mascot--decorative");
    mount.replaceChildren(doc.element);
    examples.push(doc);
  });
  intro.dataset.initialized = "true";
  window.addEventListener("pagehide", function (event) {
    if (event.persisted) return;
    examples.forEach(function (doc) { doc.destroy(); });
    delete intro.dataset.initialized;
  }, { once: false });
}());
