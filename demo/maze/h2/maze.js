// -*- Mode: javascript -*-
//
// Maze Race client: plain Canvas 2D, no dependencies.
//
// Wire protocol (all binary frames, first byte is the command):
//   server -> client
//     'M' w h <2-bit cells...>  new maze (bit0 = left wall, bit1 = top wall, big-endian in each byte)
//     'g' x y                   new goal position
//     'm' id x y                another player moved
//     'q' id                    another player left (optional; ignored by older servers)
//     'f' x y                   force-move this player
//     'N' <utf8>                this player's name ("player-<id>/<peer>")
//     'n' <utf8>                notice text
//   client -> server
//     'm' x y                   I moved
//     '+'                       level up
//     '!'                       reset

"use strict";

(() => {
  // ---------------------------------------------------------------------------
  // tunables

  const STEP_DELAY  = 170;   // ms before a held key starts repeating
  const STEP_REPEAT = 65;    // ms between repeated steps while held
  const TWEEN_MS    = 70;    // movement animation length
  const SWIPE_DEAD  = 18;    // px of finger travel before a swipe registers
  const MAX_LOG     = 12;

  // Wall bits after decode/fill-out: DRUL
  const L = 1, U = 2, R = 4, D = 8;

  // The server grows the maze by w/4 each level starting at 16, so the width
  // tells us the level without an extra message.
  const LEVEL_WIDTHS = (() => {
    const out = [];
    for (let w = 16; w <= 255; w += Math.floor(w / 4)) out.push(w);
    return out;
  })();

  // ---------------------------------------------------------------------------
  // DOM

  const $ = (id) => document.getElementById(id);
  const cv = $("cv");
  const ctx = cv.getContext("2d");
  const board = $("board");
  const ui = {
    status: $("status"), statusText: $("status-text"),
    level: $("level"), goals: $("goals"), size: $("size"), players: $("players"),
    meName: $("me-name"), meSwatch: $("me-swatch"),
    log: $("log"), help: $("help"), helpBtn: $("help-btn"),
  };

  const utf8 = new TextDecoder("utf-8");
  const darkQuery = matchMedia("(prefers-color-scheme: dark)");

  // ---------------------------------------------------------------------------
  // game state

  let maze = null;            // { w, h, cells: Uint8Array(w*h) }
  let trail = null;           // Uint8Array(w*h), 1 = I've been here
  const goal = { x: -1, y: -1 };
  const me = makeSprite(0, 0);
  me.id = 0;
  const others = new Map();   // id -> sprite
  let level = 0, goalsHit = 0;

  function makeSprite(x, y) {
    return { x, y, fromX: x, fromY: y, t0: 0 };
  }

  function moveSprite(s, x, y, animate) {
    const now = performance.now();
    if (animate) {
      // start from wherever it currently appears, so rapid steps chain smoothly
      const [cx, cy] = spritePos(s, now);
      s.fromX = cx; s.fromY = cy; s.t0 = now;
    } else {
      s.fromX = x; s.fromY = y; s.t0 = 0;
    }
    s.x = x; s.y = y;
  }

  function spritePos(s, now) {
    const t = s.t0 ? Math.min(1, (now - s.t0) / TWEEN_MS) : 1;
    const e = 1 - (1 - t) * (1 - t);           // ease-out
    return [s.fromX + (s.x - s.fromX) * e, s.fromY + (s.y - s.fromY) * e];
  }

  // ---------------------------------------------------------------------------
  // maze decoding

  function decodeMaze(u8) {
    const w = u8[1], h = u8[2], n = w * h;
    const cells = new Uint8Array(n);
    let k = 0;
    for (let i = 3; i < u8.length && k < n; i++) {
      const b = u8[i];
      for (let j = 3; j >= 0 && k < n; j--) {
        cells[k++] = (b >> (2 * j)) & 3;
      }
    }
    // outer border
    for (let x = 0; x < w; x++) { cells[x] |= U; cells[(h - 1) * w + x] |= D; }
    for (let y = 0; y < h; y++) { cells[y * w] |= L; cells[y * w + w - 1] |= R; }
    // derive right/down walls from neighbours' left/top walls
    for (let y = 0; y < h; y++) {
      for (let x = 0; x < w; x++) {
        const i = y * w + x;
        if (x < w - 1 && (cells[i + 1] & L)) cells[i] |= R;
        if (y < h - 1 && (cells[i + w] & U)) cells[i] |= D;
      }
    }
    return { w, h, cells };
  }

  function legal(x, y, dx, dy) {
    const c = maze.cells[y * maze.w + x];
    if (dx ===  1) return !(c & R);
    if (dx === -1) return !(c & L);
    if (dy ===  1) return !(c & D);
    if (dy === -1) return !(c & U);
    return false;
  }

  // ---------------------------------------------------------------------------
  // rendering
  //
  // Everything is drawn in device pixels (no ctx.scale), which keeps wall
  // lines crisp on any display. The walls and the trail each live on their
  // own offscreen canvas and are only redrawn when they change; a frame is
  // just two blits plus a handful of small shapes.

  let dpr = 1, cell = 16, pad = 0;            // cell & pad in device px
  const wallLayer = document.createElement("canvas");
  const trailLayer = document.createElement("canvas");
  let colors = {};

  function readColors() {
    const cs = getComputedStyle(document.documentElement);
    const v = (name) => cs.getPropertyValue(name).trim();
    colors = { wall: v("--wall"), goal: v("--goal"), board: v("--board"), lightness: v("--player-l") };
  }

  function playerColor(id, alpha = 1) {
    const hue = (id * 137.508 + 210) % 360;     // golden-angle spread
    return `hsl(${hue.toFixed(1)} 80% ${colors.lightness} / ${alpha})`;
  }

  function layout() {
    if (!maze) return;
    dpr = window.devicePixelRatio || 1;
    const availW = board.clientWidth - 4;
    const availH = board.clientHeight - 4;
    // CSS px per cell: fit the window, but stay usable for huge mazes
    const cssCell = Math.max(4, Math.min(36,
      Math.floor(Math.min(availW / (maze.w + 1), availH / (maze.h + 1)))));
    const cssPad = Math.max(6, Math.round(cssCell / 2));
    const cssW = maze.w * cssCell + 2 * cssPad;
    const cssH = maze.h * cssCell + 2 * cssPad;

    cell = Math.round(cssCell * dpr);
    pad = Math.round(cssPad * dpr);
    for (const c of [cv, wallLayer, trailLayer]) {
      c.width = maze.w * cell + 2 * pad;
      c.height = maze.h * cell + 2 * pad;
    }
    cv.style.width = cssW + "px";
    cv.style.height = cssH + "px";

    readColors();
    drawWalls();
    drawTrail();
  }

  function drawWalls() {
    const g = wallLayer.getContext("2d");
    const { w, h, cells } = maze;
    const lw = Math.max(1, Math.round(cell / 7));
    const o = pad + (lw % 2 ? 0.5 : 0);       // half-pixel offset for odd widths
    g.clearRect(0, 0, wallLayer.width, wallLayer.height);

    // One path for the whole maze; merge horizontal and vertical runs so
    // a long wall is one segment instead of many.
    const path = new Path2D();
    for (let y = 0; y <= h; y++) {             // horizontal walls (top edges, plus bottom border)
      let run = -1;
      for (let x = 0; x <= w; x++) {
        const wall = x < w && (y < h ? cells[y * w + x] & U : cells[(h - 1) * w + x] & D);
        if (wall && run < 0) run = x;
        if (!wall && run >= 0) {
          path.moveTo(o + run * cell, o + y * cell);
          path.lineTo(o + x * cell, o + y * cell);
          run = -1;
        }
      }
    }
    for (let x = 0; x <= w; x++) {             // vertical walls (left edges, plus right border)
      let run = -1;
      for (let y = 0; y <= h; y++) {
        const wall = y < h && (x < w ? cells[y * w + x] & L : cells[y * w + w - 1] & R);
        if (wall && run < 0) run = y;
        if (!wall && run >= 0) {
          path.moveTo(o + x * cell, o + run * cell);
          path.lineTo(o + x * cell, o + y * cell);
          run = -1;
        }
      }
    }
    g.strokeStyle = colors.wall;
    g.lineWidth = lw;
    g.lineCap = "round";
    g.lineJoin = "round";
    g.stroke(path);
  }

  function drawTrail() {
    const g = trailLayer.getContext("2d");
    g.clearRect(0, 0, trailLayer.width, trailLayer.height);
    if (!trail) return;
    g.fillStyle = playerColor(me.id, 0.18);
    for (let i = 0; i < trail.length; i++) {
      if (trail[i]) paintTrailCell(g, i % maze.w, (i / maze.w) | 0);
    }
  }

  function paintTrailCell(g, x, y) {
    const inset = Math.max(1, Math.round(cell * 0.18));
    g.fillRect(pad + x * cell + inset, pad + y * cell + inset, cell - 2 * inset, cell - 2 * inset);
  }

  function markTrail(x, y) {
    const i = y * maze.w + x;
    if (trail[i]) return;
    trail[i] = 1;
    const g = trailLayer.getContext("2d");
    g.fillStyle = playerColor(me.id, 0.18);
    paintTrailCell(g, x, y);
  }

  // Markers never shrink below MIN_MARKER css px, so you can still find
  // yourself (and the goal) on a 200-wide maze with 4px cells.
  const MIN_MARKER = 7;
  function drawBlock(px, py, fill, inset, radius) {
    const s = Math.max(cell - 2 * inset, MIN_MARKER * dpr);
    const x = pad + (px + 0.5) * cell - s / 2, y = pad + (py + 0.5) * cell - s / 2;
    radius = Math.max(radius, s * 0.25);
    ctx.fillStyle = fill;
    ctx.beginPath();
    if (ctx.roundRect) ctx.roundRect(x, y, s, s, radius); else ctx.rect(x, y, s, s);
    ctx.fill();
  }

  function frame(now) {
    tickInput(now);
    if (maze) {
      ctx.clearRect(0, 0, cv.width, cv.height);
      ctx.drawImage(trailLayer, 0, 0);
      ctx.drawImage(wallLayer, 0, 0);

      const inset = Math.max(1, Math.round(cell * 0.16));
      const r = Math.max(1, cell * 0.22);

      // goal: soft pulsing halo behind a solid core
      if (goal.x >= 0) {
        const pulse = 0.5 + 0.5 * Math.sin(now / 280);
        const cx = pad + (goal.x + 0.5) * cell, cy = pad + (goal.y + 0.5) * cell;
        ctx.globalAlpha = 0.18 + 0.22 * pulse;
        ctx.fillStyle = colors.goal;
        ctx.beginPath();
        ctx.arc(cx, cy, Math.max(cell, 14 * dpr) * (0.55 + 0.25 * pulse), 0, Math.PI * 2);
        ctx.fill();
        ctx.globalAlpha = 1;
        drawBlock(goal.x, goal.y, colors.goal, inset, r);
      }

      for (const [id, s] of others) {
        const [x, y] = spritePos(s, now);
        drawBlock(x, y, playerColor(id, 0.75), inset + 1, r);
      }

      const [mx, my] = spritePos(me, now);
      ctx.shadowColor = playerColor(me.id, 0.8);
      ctx.shadowBlur = Math.max(cell, 10 * dpr) * 0.6;
      drawBlock(mx, my, playerColor(me.id), inset, r);
      ctx.shadowBlur = 0;
    }
    requestAnimationFrame(frame);
  }

  // ---------------------------------------------------------------------------
  // HUD

  function updateHud() {
    ui.level.textContent = level ? String(level) : "?";
    ui.goals.textContent = "●".repeat(goalsHit) + "○".repeat(Math.max(0, 3 - goalsHit));
    ui.size.textContent = maze ? `${maze.w} × ${maze.h}` : "–";
    ui.players.textContent = String(others.size + 1);
  }

  function logNotice(text) {
    const empty = ui.log.querySelector(".empty");
    if (empty) empty.remove();

    const li = document.createElement("li");
    const sw = document.createElement("span");
    sw.className = "swatch";
    const span = document.createElement("span");
    span.textContent = text;                         // never innerHTML for server text
    const who = /player (\d+)/.exec(text);
    if (who) {
      const id = +who[1];
      sw.style.background = playerColor(id);
      if (id === me.id) li.classList.add("mine");
    }
    li.append(sw, span);
    ui.log.prepend(li);
    while (ui.log.children.length > MAX_LOG) ui.log.lastChild.remove();

    // "player 3 captured level 2 goal #1 in 14s" -> keep the goal dots in sync
    const cap = /level (\d+) goal #(\d+)/.exec(text);
    if (cap) {
      goalsHit = Math.min(3, +cap[2]);
      updateHud();
    }
  }

  // ---------------------------------------------------------------------------
  // networking

  let ws = null, backoff = 500;

  function wsUrl() {
    const override = new URLSearchParams(location.search).get("ws");
    if (override) return override;
    const proto = location.protocol === "https:" ? "wss://" : "ws://";
    return proto + location.hostname + ":4434/";
  }

  function setStatus(state, text) {
    ui.status.dataset.state = state;
    ui.statusText.textContent = text;
  }

  function connect() {
    setStatus("connecting", "connecting…");
    ws = new WebSocket(wsUrl());
    ws.binaryType = "arraybuffer";
    ws.onopen = () => { backoff = 500; setStatus("live", "live"); };
    ws.onmessage = (ev) => {
      if (ev.data instanceof ArrayBuffer) onMessage(new Uint8Array(ev.data));
      else console.warn("unexpected text frame", ev.data);
    };
    ws.onclose = () => {
      others.clear();
      updateHud();
      setStatus("offline", `offline, retrying in ${Math.round(backoff / 1000)}s`);
      setTimeout(connect, backoff);
      backoff = Math.min(backoff * 2, 10000);
    };
    ws.onerror = () => {};   // onclose follows and handles it
  }

  function send(...bytes) {
    if (ws && ws.readyState === WebSocket.OPEN) ws.send(new Uint8Array(bytes));
  }

  const CMD = (c) => c.charCodeAt(0);

  function onMessage(u8) {
    switch (u8[0]) {
      case CMD("M"): {
        maze = decodeMaze(u8);
        trail = new Uint8Array(maze.w * maze.h);
        level = LEVEL_WIDTHS.indexOf(maze.w) + 1;
        goalsHit = 0;
        // keep my position if it still fits (level-up grows the maze)
        moveSprite(me, Math.min(me.x, maze.w - 1), Math.min(me.y, maze.h - 1), false);
        layout();
        updateHud();
        break;
      }
      case CMD("g"):
        goal.x = u8[1]; goal.y = u8[2];
        break;
      case CMD("m"): {
        const id = u8[1], s = others.get(id);
        if (s) moveSprite(s, u8[2], u8[3], true);
        else { others.set(id, makeSprite(u8[2], u8[3])); updateHud(); }
        break;
      }
      case CMD("q"):
        others.delete(u8[1]);
        updateHud();
        break;
      case CMD("f"):
        moveSprite(me, u8[1], u8[2], false);
        break;
      case CMD("N"): {
        const name = utf8.decode(u8.subarray(1));
        const m = /player-(\d+)/.exec(name);
        if (m) me.id = +m[1];
        ui.meName.textContent = name;
        ui.meSwatch.style.background = playerColor(me.id);
        if (maze) drawTrail();
        break;
      }
      case CMD("n"):
        logNotice(utf8.decode(u8.subarray(1)));
        break;
      default:
        console.warn("unknown command", u8[0]);
    }
  }

  // ---------------------------------------------------------------------------
  // input
  //
  // Key presses step immediately (so a quick tap is never lost), then a held
  // key repeats on a timer. The most recently pressed direction wins.

  const DIRS = {
    ArrowLeft: [-1, 0], ArrowRight: [1, 0], ArrowUp: [0, -1], ArrowDown: [0, 1],
    KeyA: [-1, 0],      KeyD: [1, 0],       KeyW: [0, -1],    KeyS: [0, 1],
  };
  const held = [];      // stack of active direction sources (key codes or "touch")
  const heldDir = {};   // source -> [dx, dy]
  let nextStep = 0;

  function step([dx, dy]) {
    if (!maze || !legal(me.x, me.y, dx, dy)) return;
    markTrail(me.x, me.y);
    moveSprite(me, me.x + dx, me.y + dy, true);
    send(CMD("m"), me.x, me.y);
  }

  function press(src, dir) {
    const i = held.indexOf(src);
    if (i >= 0) held.splice(i, 1);
    held.push(src);
    heldDir[src] = dir;
    step(dir);
    nextStep = performance.now() + STEP_DELAY;
  }

  function release(src) {
    const i = held.indexOf(src);
    if (i >= 0) held.splice(i, 1);
    delete heldDir[src];
  }

  function tickInput(now) {
    if (held.length && now >= nextStep) {
      step(heldDir[held[held.length - 1]]);
      nextStep = now + STEP_REPEAT;
    }
  }

  addEventListener("keydown", (e) => {
    if (ui.help.open && e.key !== "?" && e.key !== "Escape") return;
    if (e.metaKey || e.ctrlKey || e.altKey) return;
    const dir = DIRS[e.code];
    if (dir) {
      e.preventDefault();                 // don't scroll the page
      if (!e.repeat) press(e.code, dir);  // we do our own repeat
      return;
    }
    if (e.key === "?") { toggleHelp(); return; }
    if (e.key === "=" || e.key === "+") send(CMD("+"));
    else if (e.key === "R" && e.shiftKey) send(CMD("!"));
  });
  addEventListener("keyup", (e) => { if (DIRS[e.code]) release(e.code); });
  addEventListener("blur", () => { held.length = 0; });

  // touch: put a finger down and drag in a direction; hold to keep moving
  let touchOrigin = null;
  cv.addEventListener("pointerdown", (e) => {
    if (e.pointerType === "mouse") return;
    touchOrigin = [e.clientX, e.clientY];
    cv.setPointerCapture(e.pointerId);
  });
  cv.addEventListener("pointermove", (e) => {
    if (!touchOrigin) return;
    const dx = e.clientX - touchOrigin[0], dy = e.clientY - touchOrigin[1];
    if (Math.hypot(dx, dy) < SWIPE_DEAD) return;
    const dir = Math.abs(dx) > Math.abs(dy) ? [Math.sign(dx), 0] : [0, Math.sign(dy)];
    const cur = heldDir.touch;
    if (!cur || cur[0] !== dir[0] || cur[1] !== dir[1]) press("touch", dir);
    // re-centre so changing direction mid-swipe only needs a short drag
    touchOrigin = [e.clientX, e.clientY];
  });
  const endTouch = () => { touchOrigin = null; release("touch"); };
  cv.addEventListener("pointerup", endTouch);
  cv.addEventListener("pointercancel", endTouch);

  function toggleHelp() {
    if (ui.help.open) ui.help.close(); else ui.help.showModal();
  }
  ui.helpBtn.addEventListener("click", toggleHelp);
  ui.help.addEventListener("click", (e) => { if (e.target === ui.help) ui.help.close(); });

  // ---------------------------------------------------------------------------
  // startup

  new ResizeObserver(() => layout()).observe(board);
  darkQuery.addEventListener("change", () => {
    readColors();
    if (maze) { drawWalls(); drawTrail(); }
    ui.meSwatch.style.background = playerColor(me.id);
  });
  readColors();
  updateHud();
  connect();
  requestAnimationFrame(frame);
})();
