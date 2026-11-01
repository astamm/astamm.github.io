// Zoom with viewpoint
// Instead of moving elements on screen, the viewpoint (camera) zooms and
// pans between elements as you step through fragments.
//
// The camera follows the optimal path from van Wijk & Nuij, "Smooth and
// efficient zooming and panning" (IEEE InfoVis 2003), so that the perceived
// velocity of the image stays constant throughout the move. This is the same
// model d3-zoom's interpolateZoom implements.

(function () {
  const PADDING = 40; // px of breathing room around element zoom targets

  // rho: zoom/pan trade-off. Theory gives 6^(1/4) ~ 1.565; the paper's user
  // study preferred 1.42. speed: screen widths of optical flow per second.
  // The same study preferred 0.90, but that was interactive map browsing;
  // 1.7 keeps a typical slide zoom near the 0.8s this extension used before,
  // which suits stepping through a deck. duration (s) overrides speed.
  const DEFAULTS = { rho: 1.42, speed: 1.7, duration: null };

  const EPS = 1e-6;
  const MAX_FRAME = 0.1; // clamp dt, so a backgrounded tab doesn't teleport

  const reducedMotion = window.matchMedia
    ? window.matchMedia("(prefers-reduced-motion: reduce)")
    : { matches: false };

  const state = new WeakMap(); // slide -> { view, raf, remaining }

  function deckConfig() {
    const cfg = window.zoomVp || {};
    return {
      rho: num(cfg.rho, DEFAULTS.rho),
      speed: num(cfg.speed, DEFAULTS.speed),
      duration: num(cfg.duration, DEFAULTS.duration),
    };
  }

  function num(value, fallback) {
    const n = parseFloat(value);
    return isFinite(n) ? n : fallback;
  }

  // per-step overrides, e.g. []{.zoom-vp zoom-duration="2"}
  function optionsFor(fragment) {
    const opts = deckConfig();
    if (!fragment) return opts;
    ["rho", "speed", "duration"].forEach((key) => {
      const attr = fragment.getAttribute("data-zoom-" + key);
      if (attr !== null) opts[key] = num(attr, opts[key]);
    });
    // an explicit speed on the step overrides an inherited fixed duration
    if (fragment.getAttribute("data-zoom-speed") !== null) opts.duration = null;
    return opts;
  }

  // ---------------------------------------------------------------- geodesic

  // Optimal path between two views, each { cx, cy, w } in slide coordinates:
  // center of the visible window plus its width. Returns the total path
  // length S under the metric, and at(s) for s in [0, S].
  function interpolateView(v0, v1, rho) {
    const dx = v1.cx - v0.cx;
    const dy = v1.cy - v0.cy;
    const d2 = dx * dx + dy * dy;
    const w0 = v0.w;
    const w1 = v1.w;
    const rho2 = rho * rho;

    if (d2 > EPS) {
      // equation (9): the geodesic, an arc of an ellipse in (u, w) space
      const u1 = Math.sqrt(d2);
      const dw = w1 * w1 - w0 * w0;
      const rho4d2 = rho2 * rho2 * d2;
      const b0 = (dw + rho4d2) / (2 * w0 * rho2 * u1);
      const b1 = (dw - rho4d2) / (2 * w1 * rho2 * u1);
      const r0 = Math.log(-b0 + Math.sqrt(b0 * b0 + 1));
      const r1 = Math.log(-b1 + Math.sqrt(b1 * b1 + 1));
      const S = (r1 - r0) / rho;

      if (isFinite(S) && S > EPS) {
        const ux = dx / u1;
        const uy = dy / u1;
        const coshr0 = Math.cosh(r0);
        const sinhr0 = Math.sinh(r0);
        return {
          S: S,
          at: function (s) {
            const t = rho * s + r0;
            const u = ((w0 * coshr0) / rho2) * Math.tanh(t) - (w0 / rho2) * sinhr0;
            return {
              cx: v0.cx + ux * u,
              cy: v0.cy + uy * u,
              w: (w0 * coshr0) / Math.cosh(t),
            };
          },
        };
      }
    }

    // degenerate case: no pan, so the path is a pure logarithmic zoom
    const ratio = Math.log(w1 / w0);
    const S = Math.abs(ratio) / rho;
    if (!isFinite(S) || S <= EPS) {
      return { S: 0, at: () => ({ cx: v1.cx, cy: v1.cy, w: v1.w }) };
    }
    const k = w1 < w0 ? -1 : 1;
    return {
      S: S,
      at: function (s) {
        return { cx: v0.cx, cy: v0.cy, w: w0 * Math.exp(k * rho * s) };
      },
    };
  }

  // ------------------------------------------------------------------ camera

  function fullView(slide) {
    return {
      cx: slide.offsetWidth / 2,
      cy: slide.offsetHeight / 2,
      w: slide.offsetWidth,
    };
  }

  function stateFor(slide) {
    let st = state.get(slide);
    if (!st) {
      st = { view: fullView(slide), raf: 0, remaining: 0 };
      state.set(slide, st);
    }
    return st;
  }

  function renderView(slide, view) {
    const scale = slide.offsetWidth / view.w;
    const tx = slide.offsetWidth / 2 / scale - view.cx;
    const ty = slide.offsetHeight / 2 / scale - view.cy;
    slide.style.transformOrigin = "0 0";
    slide.style.transition = "none"; // the frame loop owns the timing
    slide.style.transform = `scale(${scale}) translate(${tx}px, ${ty}px)`;
    stateFor(slide).view = view;
  }

  // back to the untransformed slide, leaving no inline transform behind
  function clearView(slide) {
    slide.style.transition = "none";
    slide.style.transform = "";
    stateFor(slide).view = fullView(slide);
  }

  function stop(slide) {
    const st = stateFor(slide);
    if (st.raf) cancelAnimationFrame(st.raf);
    st.raf = 0;
  }

  function animateTo(slide, target, opts) {
    stop(slide);
    const st = stateFor(slide);

    if (reducedMotion.matches) {
      renderView(slide, target);
      return;
    }

    st.remaining = opts.duration;
    let last = performance.now();

    function frame(now) {
      const dt = Math.min((now - last) / 1000, MAX_FRAME);
      last = now;

      // recompute the path from wherever the camera is now: geodesics are
      // unique, so this is identical to following one precomputed path, but
      // a target change mid-flight redirects without a discontinuity
      const path = interpolateView(st.view, target, opts.rho);
      let s;
      if (st.remaining !== null) {
        s = st.remaining > dt ? path.S * (dt / st.remaining) : path.S;
        st.remaining = Math.max(st.remaining - dt, 0);
      } else {
        s = opts.speed * dt;
      }

      if (!path.S || s >= path.S) {
        st.raf = 0;
        renderView(slide, target);
        return;
      }
      renderView(slide, path.at(s));
      st.raf = requestAnimationFrame(frame);
    }

    st.raf = requestAnimationFrame(frame);
  }

  // ------------------------------------------------------------------- steps

  function currentZoomFragment(slide) {
    // the visible zoom fragment with the highest fragment index wins
    // (DOM order and fragment order can differ)
    const visible = [...slide.querySelectorAll(".fragment.visible.zoom-vp")];
    visible.sort(
      (a, b) =>
        (+a.getAttribute("data-fragment-index") || 0) -
        (+b.getAttribute("data-fragment-index") || 0)
    );
    return visible.length ? visible[visible.length - 1] : null;
  }

  function resolveTarget(slide, fragment) {
    const sel = fragment.getAttribute("data-zoom-to");
    if (sel) return slide.querySelector(sel);
    return fragment;
  }

  // "x, y, w, h" in slide coordinates (e.g. 1280x720 space)
  function parseZoomRect(fragment) {
    const spec = fragment.getAttribute("data-zoom-rect");
    if (!spec) return null;
    const nums = spec.split(/[,\s]+/).filter(Boolean).map(Number);
    if (nums.length !== 4 || nums.some(isNaN)) return null;
    const [x, y, rw, rh] = nums;
    return { x, y, rw, rh };
  }

  // the view that frames a zoom step, or null if the step can't be resolved
  function targetView(slide, fragment) {
    if (!fragment || fragment.classList.contains("zoom-reset")) {
      return fullView(slide);
    }

    const w = slide.offsetWidth;
    const h = slide.offsetHeight;

    let rx, ry, rw, rh, padding;
    const zoomRect = parseZoomRect(fragment);
    if (zoomRect) {
      // explicit rect in slide coordinates, used as-is
      ({ x: rx, y: ry, rw, rh } = zoomRect);
      padding = 0;
    } else {
      const target = resolveTarget(slide, fragment);
      if (!target) return null;

      // measure with the current transform in place: the zoom is a uniform
      // scale + translate, so normalizing by the slide's own on-screen rect
      // cancels out both reveal's scaling and any active zoom/transition
      const slideRect = slide.getBoundingClientRect();
      const rect = target.getBoundingClientRect();
      const revealScale = slideRect.width / w;

      rx = (rect.left - slideRect.left) / revealScale;
      ry = (rect.top - slideRect.top) / revealScale;
      rw = rect.width / revealScale;
      rh = rect.height / revealScale;
      padding = PADDING;
    }

    // the camera is described by width alone, so express "fit the padded rect
    // in both dimensions" as the width that does it
    const pw = rw + padding * 2;
    const ph = rh + padding * 2;
    return { cx: rx + rw / 2, cy: ry + rh / 2, w: Math.max(pw, (ph * w) / h) };
  }

  function applyZoom(slide, instant) {
    const fragment = currentZoomFragment(slide);
    const view = targetView(slide, fragment);
    if (!view) return;

    if (instant) {
      stop(slide);
      renderView(slide, view);
      return;
    }
    animateTo(slide, view, optionsFor(fragment));
  }

  function maybeZoom() {
    const slide = Reveal.getCurrentSlide();
    if (!slide) return;
    // only react if the slide contains zoom fragments
    if (!slide.querySelector(".zoom-vp")) return;
    applyZoom(slide, false);
  }

  function setup() {
    // a bare .zoom-vp (no .fragment) is a pure camera step: promote it to a
    // reveal "custom effect" fragment, which stays visible and gets no fade
    document
      .querySelectorAll(".reveal .slides section .zoom-vp:not(.fragment)")
      .forEach((el) => el.classList.add("fragment", "custom"));

    // reveal numbers pre-existing fragments before the promoted ones exist,
    // so on slides using zoom-vp, drop the assigned indices and renumber all
    // fragments in document order (explicit fragment-index is not supported
    // on these slides)
    document.querySelectorAll(".reveal .slides section").forEach((sec) => {
      if (!sec.querySelector(".zoom-vp")) return;
      sec
        .querySelectorAll(".fragment")
        .forEach((f) => f.removeAttribute("data-fragment-index"));
    });
    Reveal.sync();

    Reveal.on("fragmentshown", maybeZoom);
    Reveal.on("fragmenthidden", maybeZoom);

    Reveal.on("slidechanged", (event) => {
      // reset any slide we left, and sync the incoming one (handles jumps)
      if (event.previousSlide) {
        stop(event.previousSlide);
        clearView(event.previousSlide);
      }
      if (event.currentSlide && event.currentSlide.querySelector(".zoom-vp")) {
        applyZoom(event.currentSlide, true);
      }
    });

    // sync the initial slide (handles loading mid-deck via URL hash)
    const slide = Reveal.getCurrentSlide();
    if (slide && slide.querySelector(".zoom-vp")) applyZoom(slide, true);
  }

  function waitForReveal() {
    if (typeof Reveal !== "undefined" && typeof Reveal.on === "function") {
      if (Reveal.isReady()) setup();
      else Reveal.on("ready", setup);
    } else {
      window.addEventListener("load", waitForReveal);
    }
  }
  waitForReveal();
})();
