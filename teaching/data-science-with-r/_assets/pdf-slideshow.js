/* Shared PDF slideshow loader for the Data Science with R slide viewers.
 *
 * Turns a PDF into a paged, arrow-key navigable slideshow rendered to a
 * <canvas>, so the browser's native (continuous-scroll) PDF viewer is never
 * used. Relies on the vendored pdf.js UMD build (window.pdfjsLib).
 *
 * The wrapper's markdown only carries an empty container:
 *   <div id="pdf-frame"></div>
 * This script builds the whole viewer (canvas area + a toolbar strip that sits
 * BELOW the slide, never overlapping it) so we avoid authoring fragile raw HTML
 * in markdown (pandoc hijacks <div>/<span> with class/id attributes).
 *
 * Usage (from any unit's -view.qmd):
 *   <script src="../_assets/pdfjs/pdf.min.js"></script>
 *   <script src="../_assets/pdf-slideshow.js"></script>
 *   <script>
 *     window.addEventListener('DOMContentLoaded', function () {
 *       window.initPDFSlideshow({ src: '01-Introduction-Slides.pdf',
 *                                 workerSrc: '../_assets/pdfjs/pdf.worker.min.js' });
 *     });
 *   </script>
 *
 * opts:
 *   src       - the PDF url (same-origin), relative to the page.
 *   workerSrc - url of pdf.worker.min.js, relative to the page.
 *   frameId   - the container <div> (default 'pdf-frame').
 */
window.initPDFSlideshow = function (opts) {
  'use strict';
  opts = opts || {};
  var frame = document.getElementById(opts.frameId || 'pdf-frame');
  if (!frame || !window.pdfjsLib) return;

  window.pdfjsLib.GlobalWorkerOptions.workerSrc = opts.workerSrc;

  // --- Build the viewer DOM inside the empty container ---------------------
  frame.classList.add('pdf-frame');

  var stage = document.createElement('div');
  stage.className = 'pdf-stage';
  frame.appendChild(stage);

  var canvas = document.createElement('canvas');
  canvas.className = 'pdf-canvas';
  stage.appendChild(canvas);

  var toolbar = document.createElement('div');
  toolbar.className = 'pdf-toolbar';
  frame.appendChild(toolbar);

  var fsBtn = document.createElement('button');
  fsBtn.type = 'button';
  fsBtn.className = 'pdf-fs';
  fsBtn.textContent = 'Fullscreen';
  toolbar.appendChild(fsBtn);

  var nav = document.createElement('div');
  nav.className = 'pdf-nav';
  toolbar.appendChild(nav);

  var prevBtn = document.createElement('button');
  prevBtn.type = 'button';
  prevBtn.className = 'pdf-nav-btn';
  prevBtn.title = 'Previous slide';
  prevBtn.textContent = '\u2039'; // ‹
  prevBtn.disabled = true;
  nav.appendChild(prevBtn);

  var pageNo = document.createElement('span');
  pageNo.className = 'pdf-pageno';
  pageNo.textContent = 'Loading';
  nav.appendChild(pageNo);

  var nextBtn = document.createElement('button');
  nextBtn.type = 'button';
  nextBtn.className = 'pdf-nav-btn';
  nextBtn.title = 'Next slide';
  nextBtn.textContent = '\u203A'; // ›
  nextBtn.disabled = true;
  nav.appendChild(nextBtn);

  // --- State ----------------------------------------------------------------
  var pdfDoc = null;
  var pageNum = 1;
  var renderTask = null;
  var renderQueue = null;
  var pageDim = null;

  function fitScale() {
    // The stage is the flex item holding the slide; its client box is the
    // available area (the toolbar is a sibling strip below, so it never
    // overlaps the slide). Force a reflow so flex values are current.
    void frame.offsetHeight;
    var availW = stage.clientWidth;
    var availH = stage.clientHeight;
    if (!pageDim || availW <= 0 || availH <= 0) return 1;
    var s = Math.min(availW / pageDim.width, availH / pageDim.height);
    // The slide must never overflow the stage (a too-high floor would force a
    // zoom that spills past the toolbar). Allow zooming out until it fits.
    return Math.max(0.25, Math.min(2.0, s));
  }

  function renderPage() {
    if (!pdfDoc) return;
    if (renderQueue) clearTimeout(renderQueue);
    renderQueue = setTimeout(function () {
      renderQueue = null;
      pdfDoc.getPage(pageNum).then(function (page) {
        if (!pageDim) pageDim = { width: page.view[2], height: page.view[3] };
        var viewport = page.getViewport({ scale: fitScale() });
        canvas.width = viewport.width;
        canvas.height = viewport.height;
        if (renderTask) { renderTask.cancel(); }
        renderTask = page.render({ canvasContext: canvas.getContext('2d'), viewport: viewport });
        renderTask.promise.catch(function (err) {
          if (err && err.name !== 'RenderingCancelledException') throw err;
        });
      });
    }, 40);
  }

  function updateNav() {
    if (!pdfDoc) return;
    pageNo.textContent = pageNum + ' / ' + pdfDoc.numPages;
    prevBtn.disabled = pageNum <= 1;
    nextBtn.disabled = pageNum >= pdfDoc.numPages;
  }

  function goTo(n) {
    if (!pdfDoc) return;
    n = Math.max(1, Math.min(pdfDoc.numPages, n));
    if (n === pageNum) return;
    pageNum = n;
    updateNav();
    renderPage();
  }
  function next() { goTo(pageNum + 1); }
  function prev() { goTo(pageNum - 1); }

  function toggleFullscreen() {
    if (!document.fullscreenElement) {
      var p = frame.requestFullscreen ? frame.requestFullscreen() :
              frame.webkitRequestFullscreen ? frame.webkitRequestFullscreen() : null;
      if (p && p.catch) p.catch(function () {});
      fsBtn.textContent = 'Exit fullscreen (Esc)';
    } else {
      var d = document.exitFullscreen ? document.exitFullscreen() :
              document.webkitExitFullscreen ? document.webkitExitFullscreen() : null;
      if (d && d.catch) d.catch(function () {});
      fsBtn.textContent = 'Fullscreen';
    }
  }

  // Expose for testing / debugging.
  frame._pdfSlideshow = {
    next: next, prev: prev, goTo: goTo,
    get pageNum() { return pageNum; },
    get numPages() { return pdfDoc ? pdfDoc.numPages : 0; }
  };

  prevBtn.addEventListener('click', prev);
  nextBtn.addEventListener('click', next);
  fsBtn.addEventListener('click', toggleFullscreen);

  document.addEventListener('fullscreenchange', function () {
    fsBtn.textContent =
      document.fullscreenElement ? 'Exit fullscreen (Esc)' : 'Fullscreen';
  });

  // Arrow keys / PageUp / PageDown navigate only while the viewer is in view.
  document.addEventListener('keydown', function (e) {
    if (!pdfDoc) return;
    var r = frame.getBoundingClientRect();
    var inView = r.top < window.innerHeight && r.bottom > 0;
    if (!inView) return;
    if (e.key === 'ArrowRight' || e.key === 'PageDown' || e.key === ' ') { e.preventDefault(); next(); }
    else if (e.key === 'ArrowLeft' || e.key === 'PageUp') { e.preventDefault(); prev(); }
  });

  window.addEventListener('resize', renderPage);

  window.pdfjsLib.getDocument(opts.src).promise.then(function (doc) {
    pdfDoc = doc;
    updateNav();
    renderPage();
    // The flex layout may not be settled when the first page renders, so
    // re-fit once the browser has had a chance to lay out the frame.
    requestAnimationFrame(function () { renderPage(); });
    setTimeout(function () { renderPage(); }, 250);
  });
};
