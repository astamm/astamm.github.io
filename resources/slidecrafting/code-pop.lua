-- code-pop.lua
-- RevealJS filter for Quarto: appends a code-pop trigger to every content
-- slide. The trigger is an icon button (a native <details>/<summary> toggle)
-- anchored in the slide footer; clicking it opens a persistent pop-up card
-- that nearly fills the slide and shows the raw markdown source that generated
-- that slide, with an icon-only copy-to-clipboard button and syntax
-- highlighting. The card is keyboard-accessible: pressing "c" (or clicking
-- the trigger, or clicking anywhere outside the pane) closes it, the trigger
-- exposes aria-expanded, and focus is moved into the card while it is open.
--
-- The source is read directly from the on-disk .qmd (via quarto.doc.input_file),
-- so executable R chunk bodies appear in full — they are NOT recoverable from
-- the Pandoc AST (after Quarto executes a deck, those chunks become empty
-- "cell" Divs whose source lives in the execution pipeline).
--
-- Usage (YAML front-matter):
--   format:
--     revealjs:
--       filters:
--         - code-pop.lua
--
-- Opt out of a single slide by adding the class to its heading:
--   ## My slide {.no-code-pop}
--
-- The emitted footer is a <div class="code-pop">; position it (e.g. bottom-
-- right, above any deck footer) and give the trigger a dark background via
-- SCSS (styles.scss).
--
-- NOTE: the class is deliberately not named "footer" — Quarto/Reveal reserve
-- `.footer` for the deck's own footer bar and paint it full-width with
-- !important rules, which would fight our absolute positioning.

local function meta_get(path)
  local m = quarto.doc.metadata or {}
  local node = m
  for _, key in ipairs(path) do
    if type(node) ~= "table" or node[key] == nil then return nil end
    node = node[key]
  end
  return node
end

-- Slide level: revealjs splits on the top heading level(s). Defaults to 2.
local function slide_level()
  local lvl = meta_get({"slide-level"})
  if lvl == nil then return 2 end
  lvl = tonumber(lvl) or 0
  return math.max(0, math.min(6, lvl))
end

-- A slide starts at a heading at or above (<=) the slide level in the AST.
local function starts_slide(bl)
  return bl.tag == "Header" and bl.level <= slide_level()
end

-- Read the raw markdown source of the document being rendered.
local function read_source()
  local path = quarto.doc.input_file
  if not path then return nil end
  local f = io.open(path, "r")
  if not f then return nil end
  local content = f:read("*a")
  f:close()
  return content
end

-- Escape text for embedding inside an HTML <code> block.
local function escape_html(s)
  s = s:gsub("&", "&amp;")
  s = s:gsub("<", "&lt;")
  s = s:gsub(">", "&gt;")
  return s
end

-- Split the raw source into per-slide markdown strings. A slide starts at an
-- ATX heading line (#{1,slide_level} at column 0) — fenced code blocks are
-- tracked so a "```{r}" chunk's "## ..." comment lines are not mistaken for
-- headings. Returns { {src = string, no_pop = boolean}, ... } covering only
-- the content slides (the title-slide prologue before the first heading is
-- skipped). Slices keep the exact authored text including the heading line.
local function split_slides(source, slvl)
  if not source then return {} end
  local slides = {}
  local pending = nil  -- table for the slide currently being collected
  local in_fence = false
  local fence_char = nil -- "`" or "~"
  for line in source:gmatch("([^\n]*)\n?") do
    local first = line:match("^%s*(.)")
    if in_fence then
      local c = line:match("^%s*(`+)") or line:match("^%s*(~+)")
      if c and #c >= 3 and (first == fence_char) then
        in_fence = false
      end
    else
      local b = line:match("^%s*(`+)") or line:match("^%s*(~+)")
      if b and #b >= 3 then
        in_fence = true
        fence_char = first
      end
    end

    local hsh = not in_fence and line:match("^(#+)%s") or nil
    local is_heading = hsh ~= nil and #hsh >= 1 and #hsh <= slvl

    if is_heading then
      local no_pop = line:find("%{%.no%-code%-pop%}", 1, true) ~= nil
      pending = { src = line, no_pop = no_pop }
      slides[#slides + 1] = pending
    elseif pending then
      pending.src = pending.src .. "\n" .. line
    end
  end
  return slides
end

-- Inline SVG: the classic "</>" code-brackets glyph.
local CODE_ICON = '<svg viewBox="0 0 24 24" aria-hidden="true">'
  .. '<path fill="none" stroke="currentColor" stroke-width="2" '
  .. 'stroke-linecap="round" stroke-linejoin="round" '
  .. 'd="M8 7 3 12l5 5M16 7l5 5-5 5"/></svg>'

-- Inline SVG: a small copy icon (two overlapping squares).
local COPY_ICON = '<svg viewBox="0 0 24 24" aria-hidden="true">'
  .. '<rect x="9" y="9" width="11" height="11" rx="1.5" fill="none" '
  .. 'stroke="currentColor" stroke-width="2"/>'
  .. '<path d="M5 15V5a2 2 0 0 1 2-2h10" fill="none" stroke="currentColor" '
  .. 'stroke-width="2" stroke-linecap="round"/></svg>'

-- Build the per-slide card: a <details> whose card nearly fills the slide, with
-- a toolbar (language label + icon-only copy button) and a scrollable,
-- syntax-highlighted <pre>. The opening <summary> trigger is NOT emitted here:
-- it is hoisted into a single screen-fixed trigger at the .reveal root (see
-- the SCRIPT below), because a trigger nested inside the transformed `.slides`
-- wrapper would scale and drift with reveal's zoom. Keeping the <details> per
-- slide lets the toggle/open state and the card live where they always did. A
-- native Div keeps the "code-pop" class through to the HTML (a lone raw
-- <aside> lost its classes).
local function make_code_pop(source)
  local card = '<div class="code-pop-card" role="dialog" '
    .. 'aria-label="Source markdown de cette diapositive" aria-live="polite">'
    .. '<div class="code-pop-toolbar">'
    .. '<span class="code-pop-lang">markdown</span>'
    .. '<button type="button" class="code-pop-copy" '
    .. 'aria-label="Copier le code source">' .. COPY_ICON .. "</button>"
    .. "</div>"
    .. '<pre><code class="language-markdown">' .. escape_html(source)
    .. "</code></pre></div>"

  -- An in-slide <details> with no <summary> child makes the browser insert its
  -- own default summary labelled "Details" (with a disclosure marker), which
  -- would paint at the slide's top-left where the deck logo sits. Emit an empty
  -- hidden <summary> so the real (hoisted) trigger stays the only visible one
  -- and nothing renders behind the logo.
  local details = '<details><summary class="code-pop-slot" aria-hidden="true"></summary>'
    .. card .. "</details>"

  return pandoc.Div(
    { pandoc.RawBlock("html", details) },
    pandoc.Attr("", { "code-pop" })
  )
end

-- One global <script> (emitted once, after the slides) wiring:
--   * a copy-to-clipboard button per card (icon-only, with a non-secure
--     context fallback and a brief visual + aria feedback state)
--   * keyboard accessibility: a card opens only via its trigger or "c", and
--     closes only via the trigger again, an outside click, or "c"; while a card
--     is open only the scroll keys and Tab are intercepted, so every other
--     reveal shortcut still works; Esc keeps its slide-overview role; the
--     trigger's aria-expanded is kept in sync; focus moves into the card on
--     open
--   * an Enter-to-enter path in reveal's slide overview (reveal itself only
--     lets you click a slide there), working whether or not a pane is open
--   * auto-closing any open popover when the presenter navigates slides
--   * (re)running highlight.js on a card's code when it is first opened
local SCRIPT = [[
<script>
(function () {
  "use strict";
  var COPIED_LABEL = "Copi\u00e9";
  document.addEventListener("click", function (e) {
    var btn = e.target.closest ? e.target.closest(".code-pop-copy") : null;
    if (!btn) return;
    var card = btn.closest(".code-pop-card");
    var code = card && card.querySelector("pre code");
    if (!code) return;
    // mono-whitespace.html turns each trailing space in the pane into a "·"
    // middle-dot span (.mono-tt); map those back to real spaces so copying the
    // source yields the true markdown (dots would corrupt it otherwise).
    var text = code.innerText;
    var s = code.querySelectorAll(".mono-tt");
    if (s && s.length) {
      var cl = code.cloneNode(true);
      cl.querySelectorAll(".mono-tt").forEach(function (m) {
        m.textContent = m.textContent.replace(/\u00B7/g, " ");
      });
      text = cl.innerText;
    }
    btn.classList.add("is-loading");
    function done(ok) {
      btn.classList.remove("is-loading");
      btn.classList.toggle("is-copied", ok);
      btn.setAttribute("aria-label", ok ? COPIED_LABEL : "Copier le code source");
      setTimeout(function () {
        btn.classList.remove("is-copied");
        btn.setAttribute("aria-label", "Copier le code source");
      }, 1500);
    }
    if (navigator.clipboard && window.isSecureContext) {
      navigator.clipboard.writeText(text).then(
        function () { done(true); },
        function () { copyFallback(text, done); });
    } else {
      copyFallback(text, done);
    }
  });
  function copyFallback(text, done) {
    var ta = document.createElement("textarea");
    ta.value = text;
    ta.setAttribute("readonly", "");
    ta.style.position = "fixed"; ta.style.left = "-9999px";
    document.body.appendChild(ta);
    ta.select();
    var ok = false;
    try { ok = document.execCommand("copy"); } catch (err) { ok = false; }
    document.body.removeChild(ta);
    done(ok);
  }
  // The single trigger button is hoisted out of the transformed `.slides`
  // wrapper into a root `.code-pop-trigger` wrapper (a sibling of `.slides`
  // inside `.reveal`, mirroring the logo/footer). Because `.slides` carries a
  // CSS `transform`, any `position: fixed` trigger nested inside it would
  // scale and drift with reveal's zoom; hoisting it to `.reveal` root lets its
  // `position: fixed; right: .5em; bottom: 46px` sit in true screen space and
  // stay a constant distance above the footer at every zoom level. Each slide
  // keeps its own in-slide `<details>` (holding the card + open state) but no
  // longer a `<summary>`; the one root trigger drives whichever slide is
  // current. The hoisted trigger uses the same code-brackets SVG as the old
  // in-slide summary.
  function codepopSvg() {
    return '<svg viewBox="0 0 24 24" aria-hidden="true">'
      + '<path fill="none" stroke="currentColor" stroke-width="2" '
      + 'stroke-linecap="round" stroke-linejoin="round" '
      + 'd="M8 7 3 12l5 5M16 7l5 5-5 5"/></svg>';
  }
  var codepopBtn = null;
  // The CURRENT (visible) slide's code-pop <details>. For a vertical stack,
  // reveal keeps several sections present/hidden at once and querySelector
  // returns the first in document order — often a hidden section-title card —
  // so "c" opened an invisible pane. Reveal.getCurrentSlide() is the one the
  // viewer is actually looking at.
  function currentDetails() {
    var cur = (window.Reveal && Reveal.getCurrentSlide) ? Reveal.getCurrentSlide() : null;
    var det = cur ? cur.querySelector(":scope > .code-pop details") : null;
    if (!det) det = document.querySelector(".reveal section.present > .code-pop details");
    return det;
  }
  // Reflect the current slide's details open/closed state onto the hoisted
  // trigger's aria-expanded.
  function syncExpand() {
    if (!codepopBtn) return;
    var det = currentDetails();
    codepopBtn.setAttribute("aria-expanded", det && det.open ? "true" : "false");
  }
  // Build the root trigger once and append it to .reveal (which exists in the
  // static HTML, sibling to .slides). Take explicit control of its click:
  // toggle details.open via the property and suppress the native summary
  // default. Chromium's internal summary toggle state falls out of sync when
  // the card is closed (e.g. via Esc or reveal), which silently swallows the
  // next native click; owning the toggle keeps the open attribute authoritative
  // and click/reopen reliable.
  (function ensureRootTrigger() {
    var reveal = document.querySelector(".reveal");
    if (!reveal) return;
    var wrap = document.createElement("div");
    wrap.className = "code-pop-trigger";
    var sum = document.createElement("summary");
    sum.className = "code-pop-btn";
    sum.setAttribute("aria-expanded", "false");
    sum.innerHTML = codepopSvg();
    sum.addEventListener("click", function (e) {
      var det = currentDetails();
      if (det) det.open = !det.open;
      e.preventDefault();
    });
    wrap.appendChild(sum);
    reveal.appendChild(wrap);
    codepopBtn = sum;
  })();
  // Keep the trigger's aria-expanded in sync, move focus into the card on
  // open, and (re)highlight the code the first time it is shown.
  // Capture phase: something on the page (Quarto/reveal) calls
  // stopPropagation() on the bubble-phase toggle event, so register here to
  // fire regardless of that interference.
  document.addEventListener("toggle", function (e) {
    var t = e.target;
    if (!t || t.tagName !== "DETAILS") return;
    syncExpand(); // keep the hoisted root trigger's aria-expanded in step
    if (t.open) {
      // Start each open with a fresh scroll target so a leftover accumulator
      // from a previous visit cannot pin the pane to an old offset.
      var pc = t.querySelector(".code-pop-card pre");
      var pcard = t.querySelector(".code-pop-card");
      if (pc) { pc.__codePopTop = undefined; pc.__codePopPending = null; pc.__codePopRaf = false; }
      if (pcard) { pcard.__codePopTop = undefined; pcard.__codePopPending = null; pcard.__codePopRaf = false; }
      var copy = t.querySelector(".code-pop-copy");
      var code = t.querySelector(".code-pop-card pre code");
      // The card is revealed with a ~0.18s opacity/visibility transition;
      // focus() on its controls is dropped while the reveal is mid-transition,
      // so focus the copy button once it has settled.
      function focusCopy() { if (copy) copy.focus(); }
      if (copy) {
        var card = t.querySelector(".code-pop-card");
        var done = false;
        var finish = function () { if (done) return; done = true; focusCopy(); };
        if (card) card.addEventListener("transitionend", finish, { once: true });
        setTimeout(finish, 250); // fallback if the transition never fires
      }
      if (code && window.hljs && !code.classList.contains("hljs")) {
        hljs.highlightElement(code);
      }
    }
  }, true);
  // Keyboard handling. Registered in the capture phase so it runs ahead of
  // reveal's bubble-phase handler (reveal binds its own keydown on document in
  // the bubble phase). While a card is open the ONLY keys we intercept are the
  // scroll keys, Tab (trap), and "c" (toggle); Esc is left to reveal (slide
  // overview) and EVERY OTHER KEY falls straight through to reveal, so all of
  // reveal's shortcuts (Left/Right navigation, F fullscreen, "?", ...) keep
  // working even while the pane is open — navigating slides then auto-closes
  // the pane via the slidechanged handler. With no open card, every key passes
  // through untouched.
  // The ONLY ways to open a card are clicking its trigger or pressing "c"; the
  // ONLY ways to close it are clicking the trigger again, clicking anywhere
  // outside the pane, or pressing "c" again.
  // Keyboard shortcut (default "c") toggles the current slide's code pane, as
  // an alternative to clicking the mouse trigger. Handled here (not via
  // reveal's API) because Reveal.registerKeyboardShortcut() only adds an entry
  // to the help overlay and does not bind a callback. Skip while typing in an
  // editable field or with a modifier held, so it never eats real input.
  function toggleCodePop() {
    // Target the CURRENT (visible) slide's code-pop, not just any
    // `section.present`: see currentDetails() above for why
    // Reveal.getCurrentSlide() is the one the viewer is actually looking at.
    var det = currentDetails();
    if (!det) return;
    det.open = !det.open; // the toggle event moves focus into the card
  }
  // Clicking anywhere on the slide outside the pane closes it. Clicks inside
  // the open pane (including its trigger button) are intentionally left alone:
  // the trigger toggles via its own summary handler, so an outside-click is
  // only ever an action on the pane's surroundings.
  document.addEventListener("click", function (e) {
    var open = document.querySelector(".code-pop details[open]");
    if (!open) return;
    // Clicks on the hoisted root trigger are handled by its own listener (which
    // toggles the current slide's details) and land outside the pane's <details>
    // subtree, so exclude them here or this outside-click would immediately
    // re-close a card the trigger just opened.
    if (e.target.closest && e.target.closest(".code-pop-trigger")) return;
    if (open.contains(e.target)) return; // inside the pane: let it handle clicks
    open.open = false; // close via the property so the trigger stays in sync
  });
  document.addEventListener("keydown", function (e) {
    var t = e.target;
    var editable = (t && t.isContentEditable) ||
      (t && t.tagName && /input|textarea|select/i.test(t.tagName));
    var withMod = !!(e.altKey || e.ctrlKey || e.metaKey || e.shiftKey);
    var overviewActive = !!(window.Reveal && Reveal.isOverview && Reveal.isOverview());
    // "c" toggles the pane (open or close). Skip while typing in an editable
    // field or with a modifier held so real input is never eaten.
    if (e.key === "c" || e.key === "C" || e.keyCode === 67) {
      if (!editable && !withMod) {
        toggleCodePop();
        e.preventDefault();
        return;
      }
    }
    // Reveal's overview has no Enter-to-enter binding (only clicking a slide
    // enters it), so add a keyboard path: while the overview is open, Enter
    // jumps to the currently highlighted slide and closes the overview. This
    // must work regardless of whether a code pane is open.
    if (overviewActive && (e.key === "Enter" || e.keyCode === 13)) {
      if (!editable && !withMod) {
        Reveal.toggleOverview(false); // deactivates and slides to the selection
        e.preventDefault();
        return;
      }
    }
    var card = document.querySelector(".code-pop details[open] .code-pop-card");
    // With no open card, every key falls through to reveal's normal handling.
    if (!card) return;
    // While a card is open we intercept ONLY the scroll keys and Tab; every
    // other key (including Esc/O for overview, F for fullscreen, Left/Right
    // and arrows for navigation, "?", ...) passes through to reveal so all of
    // its shortcuts keep working. Navigating slides auto-closes the pane.
    if (e.key === "Escape") return; // let reveal open/close the slide overview
    // When reveal's overview is active, hand all navigation keys back to
    // reveal so it can move between the thumbnails / exit overview.
    if (overviewActive) return;
    // Trap Tab within the card's controls (leave the pane via "c", the trigger
    // button, or an outside click).
    if (e.key === "Tab") {
      var focusables = card.querySelectorAll("button:not([disabled])");
      if (focusables.length) {
        var first = focusables[0], last = focusables[focusables.length - 1];
        var active = document.activeElement;
        if (e.shiftKey) {
          if (active === first || !card.contains(active)) { e.preventDefault(); last.focus(); }
        } else {
          if (active === last || !card.contains(active)) { e.preventDefault(); first.focus(); }
        }
      }
      e.preventDefault();
      e.stopImmediatePropagation();
      return;
    }
    // Up/Down (and Page) arrows scroll the pane so they don't move slides.
    // Scroll the element that actually overflows (the <pre> in practice, but
    // fall back to the card). The target offset is tracked in a JS field on
    // the scroller rather than read back from scrollTop: WebKit/Gecko can
    // defer committing a scripted scrollTop write during fast OS key
    // auto-repeat, so re-reading it each keystroke returns a stale value and
    // accumulation stalls short of the bottom. Clamping a tracked target to
    // [0, scrollHeight - clientHeight] deterministically reaches the exact
    // bottom regardless of that commit lag. We only fall back to the DOM when
    // it has clearly been scrolled natively (wheel/drag) ahead of our target.
    var scrollMap = {
      ArrowDown:  [0,  48], ArrowUp: [0, -48],
      PageDown:   [0, 320], PageUp:  [0, -320]
    };
    var mv = scrollMap[e.key];
    if (mv) {
      e.preventDefault();
      e.stopImmediatePropagation();
      var pre = card.querySelector("pre");
      var scroller = (pre && pre.scrollHeight > pre.clientHeight) ? pre
        : (card.scrollHeight > card.clientHeight ? card : pre);
      if (scroller) {
        // max is re-read on every keystroke so it tracks any mid-scroll reflow
        // (e.g. webfonts settling, highlight reflow). The target offset is
        // tracked in a JS field rather than read back from scrollTop, because
        // some engines defer committing a scripted scrollTop write during fast
        // OS auto-repeat, so read-back can return a stale value and stall short
        // of the bottom. We only fall back to the DOM when it has clearly been
        // scrolled natively (wheel/drag) ahead of our target.
        var max = Math.max(scroller.scrollHeight - scroller.clientHeight, 0);
        var acc = scroller.__codePopTop;
        if (acc === undefined) acc = scroller.scrollTop;
        else if (scroller.scrollTop > acc + 2) acc = scroller.scrollTop; // native wheel/drag
        acc = Math.max(0, Math.min(acc + mv[1], max));
        scroller.__codePopTop = acc;
        if (mv[1] > 0 && acc >= max) {
          // Reached the bottom: snap authoritatively NOW to the exact bottom,
          // and cancel any coalesced write so it cannot fight the snap.
          scroller.__codePopPending = null;
          scroller.scrollTop = scroller.scrollHeight;
        } else {
          // Defer the actual scrollTop write to the next animation frame.
          // Setting scrollTop forces a synchronous reflow of the whole pane,
          // and on slides whose source reflows into one very long pre-wrap line
          // that reflow is expensive; doing it on every keystroke during fast
          // OS auto-repeat can starve the event loop and the browser then
          // drops/skips repeat keydowns, so the target stalls short of the
          // bottom (seen in the real browser on long-line slides, while the
          // many-short-lines repro page still worked). Coalescing N keystrokes
          // into a single write per animation frame keeps auto-repeat flowing
          // so the target reliably reaches max and the bottom snap above fires.
          scroller.__codePopPending = acc;
          if (!scroller.__codePopRaf) {
            scroller.__codePopRaf = true;
            requestAnimationFrame(function () {
              scroller.__codePopRaf = false;
              if (scroller.__codePopPending == null) return;
              var target = scroller.__codePopPending;
              scroller.__codePopPending = null;
              // Re-clamp at commit time in case layout settled meanwhile (e.g.
              // fonts landing slightly changed scrollHeight).
              var m = Math.max(scroller.scrollHeight - scroller.clientHeight, 0);
              scroller.scrollTop = Math.max(0, Math.min(target, m));
              // Keep the tracked target aligned so the next keystroke continues
              // from where we actually committed, not from the stale acc.
              scroller.__codePopTop = scroller.scrollTop;
            });
          }
        }
      }
      return;
    }
    // Any other key while the pane is open: pass through to reveal untouched.
  }, true);
  // Auto-close the popover when the presenter moves to another slide. This
  // script runs before reveal.js loads (it is injected ahead of the reveal
  // <script>), so window.Reveal may not exist yet; poll until it is ready.
  var codePopReady = false;
  var codePopRevealTimer = setInterval(function () {
    if (codePopReady) { clearInterval(codePopRevealTimer); return; }
    if (window.Reveal && Reveal.on) {
      codePopReady = true;
      clearInterval(codePopRevealTimer);
      Reveal.on("slidechanged", function () {
        var open = document.querySelectorAll(".code-pop details[open]");
        for (var i = 0; i < open.length; i++) {
          open[i].removeAttribute("open");
        }
        syncExpand(); // reflect the now-closed state on the hoisted trigger
      });
      // Document the "c" toggle in reveal's help overlay (the "?" / F1
      // shortcut table). Reveal.registerKeyboardShortcut() only adds a row to
      // that table — the actual toggle is wired in this script's own keydown
      // handler, so there is no double-binding. The description also flags
      // that Up/Down (and Page) scroll the pane row-by-row while it is open
      // instead of navigating slides.
      try {
        Reveal.registerKeyboardShortcut("C", "Toggle code pane (Up/Down/Page scroll it while open)");
      } catch (err) { /* help overlay may be unavailable */ }
    }
  }, 100);
  // Optional self-diagnostic: open the deck with ?codepopdiag=1 to get an
  // on-screen readout of what happens on every scroll keystroke (which element
  // scrolls, the committed scrollTop, the tracked target, and the max). Use
  // this to localize a real-browser "Down doesn't reach the bottom" report:
  // whether the handler runs, whether target/scrollTop diverge, or whether
  // something else re-scrolls. This is diagnostic only and is off by default.
  if (/(^|[?&])codepopdiag=1(\&|$)/.test(window.location.search)) {
    var dbg = document.createElement("div");
    dbg.style.cssText = "position:fixed;left:10px;bottom:10px;z-index:9999;"
      + "background:rgba(0,0,0,.85);color:#8ce0c2;font:11px/1.5 ui-monospace,Menlo,monospace;"
      + "padding:8px 10px;border-radius:6px;white-space:pre-wrap;max-width:90vw";
    document.body.appendChild(dbg);
    // Listen on window (capture) rather than document: the main scroll handler
    // above is a capture-phase listener on document that calls
    // stopImmediatePropagation() for arrow keys, which would swallow a same-node
    // document-capture listener and leave this readout blank. window sits before
    // document on the capture path, so this fires first and its setTimeout(0)
    // reads the committed state after the handler has run.
    window.addEventListener("keydown", function (e) {
      if (!/^(ArrowUp|ArrowDown|PageUp|PageDown)$/.test(e.key)) return;
      setTimeout(function () {
        var det = document.querySelector(".code-pop details[open]");
        if (!det) { dbg.textContent = "no open pane"; return; }
        var card = det.querySelector(".code-pop-card");
        var pre = card && card.querySelector("pre");
        var scroller = (pre && pre.scrollHeight > pre.clientHeight) ? pre : card;
        var max = scroller ? Math.max(scroller.scrollHeight - scroller.clientHeight, 0) : -1;
        dbg.textContent = e.key
          + " | el=" + (scroller === pre ? "pre" : "card")
          + " | scrollTop=" + (scroller ? scroller.scrollTop : "-")
          + " | acc=" + (scroller ? scroller.__codePopTop : "-")
          + " | max=" + max
          + (scroller && max > 0 && scroller.scrollTop >= max - 1
              ? "  => BOTTOM" : "  => NOT bottom");
      }, 0);
    }, true);
  }
})();
</script>
]]

function Pandoc(doc)
  local pp = read_source()
  local slides = split_slides(pp, slide_level())
  local slide_i = 0

  local out = {}
  local slide = {}      -- blocks of the current slide (its heading included)
  local is_title = true -- first run of blocks = title/preamble slide

  local function flush()
    if #slide == 0 then return end
    if not is_title then
      slide_i = slide_i + 1
      local meta_slide = slides[slide_i]
      local skip = (meta_slide and meta_slide.no_pop)
        or (slide[1].tag == "Header" and slide[1].classes:includes("no-code-pop"))
        -- Level-1 headings are section-title divider cards (e.g. "## What is
        -- R?" becomes a centered card under the '# ...' section). They carry
        -- no teaching code and their own (vertically-centred, short) card makes
        -- the bottom-right trigger land mid-slide; skip them so no misplaced
        -- or useless button is emitted.
        or (slide[1].tag == "Header" and slide[1].level == 1)
      if not skip and meta_slide then
        slide[#slide + 1] = make_code_pop(meta_slide.src)
      end
    end
    for _, b in ipairs(slide) do out[#out + 1] = b end
    slide = {}
    is_title = false
  end

  for _, bl in ipairs(doc.blocks) do
    if starts_slide(bl) then
      flush()
      slide[#slide + 1] = bl
      is_title = false
    else
      slide[#slide + 1] = bl
    end
  end
  flush()

  -- Emit the shared wiring script once, outside the slide sections.
  out[#out + 1] = pandoc.RawBlock("html", SCRIPT)

  doc.blocks = out
  return doc
end
