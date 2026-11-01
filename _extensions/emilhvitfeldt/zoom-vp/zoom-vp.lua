-- zoom-vp.lua
-- Quarto RevealJS extension: zoom with viewpoint.
--
-- Instead of moving elements on screen, the viewpoint (camera) zooms and
-- pans between elements as you step through fragments. All behavior is
-- driven by classes/attributes handled in zoom.js:
--
--   [text]{.zoom-vp}                          zoom to element (pure camera step)
--   [text]{.fragment .zoom-vp}                reveal and zoom in one step
--   []{.zoom-vp .zoom-reset}                  zoom back out to the full slide
--   []{.zoom-vp zoom-to="#id"}                zoom to another element by selector
--   []{.zoom-vp zoom-rect="x, y, w, h"}       zoom to a rect in slide coordinates
--
-- The animation is tuned deck-wide from YAML:
--
--   zoom-vp:
--     rho: 1.42       zoom/pan trade-off
--     speed: 0.9      world widths of optical flow per second
--     duration: 0.8   seconds; overrides speed with a fixed duration
--
-- and per step with zoom-rho / zoom-speed / zoom-duration attributes.

local OPTIONS = { "rho", "speed", "duration" }

-- collect the numeric zoom-vp options into a JS object literal, or nil if
-- none were given (in which case the defaults in zoom-vp.js stand)
local function config_literal(meta)
  local opts = meta["zoom-vp"]
  if type(opts) ~= "table" then
    return nil
  end

  local parts = {}
  for _, key in ipairs(OPTIONS) do
    local value = opts[key]
    if value ~= nil then
      local number = tonumber(pandoc.utils.stringify(value))
      if number then
        table.insert(parts, string.format("%s: %s", key, tostring(number)))
      else
        quarto.log.warning("zoom-vp: ignoring non-numeric '" .. key .. "'")
      end
    end
  end

  if #parts == 0 then
    return nil
  end
  return "{" .. table.concat(parts, ", ") .. "}"
end

function Meta(meta)
  if quarto.doc.is_format("revealjs") then
    local config = config_literal(meta)
    if config then
      quarto.doc.include_text("in-header", "<script>window.zoomVp = " .. config .. ";</script>")
    end

    quarto.doc.add_html_dependency({
      name    = "revealjs-zoom-vp",
      version = "0.2.0",
      scripts = { "zoom-vp.js" },
    })
  end
  return meta
end
