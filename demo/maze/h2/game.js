// -*- Mode: javascript -*-

var m;
var p;
var scl = 15;
var bgm;
var wss;

var others = new Object();
var cnv;

function init_maze (edata) {
  m = new Maze (edata);
  var gw = (m.w+2) * scl;
  var gh = (m.h+2) * scl;
  cnv = createCanvas (gw, gh);
  cnv.parent ('maze-sketch'); // place the canvas in its div
  document.getElementById ("mazeinfo").innerHTML = "new maze (" + m.w + " x " + m.h + ")";
  // we pre-render the maze into a separate image,
  //   rather than drawing it continuously.
  bgm = createGraphics (gw, gh);
  if (bgm.pixelDensity() != 1) {
    bgm.scale (1/bgm.pixelDensity()); // scale for retina
  }
  m.draw_maze (bgm);
  frameRate(10);
}

function set_div (divid, s) {
  var enc = new TextDecoder ('utf-8');
  document.getElementById (divid).innerHTML = enc.decode(s);
}

function on_message (event) {
  if (event.data instanceof ArrayBuffer) {
    var edata = new Uint8Array (event.data);
    var cmd = edata[0];
    switch (String.fromCharCode(cmd)) {
    case 'M': // new maze
      init_maze (event.data);
      break;
    case 'g': // new goal
      m.new_goal (edata[1], edata[2]);
      break;
    case 'm': // other player movement
      other_player_move (edata);
      break;
    case 'f': // force move player
      force_move_player (edata[1], edata[2]);
      break;
    case 'N': // player name
      set_div ('player', edata.slice(1));
      break;
    case 'n': // notices
      set_div ('notices', edata.slice(1));
      break;
    default:
      console.log ('unknown command', cmd);
    }
  } else {
    console.log ('unexpected message type', event);
  }
}

function setup() {
  p = new Player();
  // we can't do normal setup here, we need to wait for the websocket connection.
  wss = new WebSocket (((window.location.protocol === "https:") ? "wss://" : "ws://") + window.location.hostname + ":4434/");
  wss.binaryType = 'arraybuffer';
  wss.onclose = function (event) {
    console.log('*** why did it close??? ***', event.data);
  };
  wss.onerror = function (event) {
    console.log('*** wss error ***', event.data);
  };
  wss.onmessage = on_message;
}

function draw() {
  if (m) {
    image (bgm, 0, 0, (m.w+2)*scl, (m.h+2)*scl);
    maybe_move();
    m.draw_meta();
    p.show();
  }
}

function maybe_move() {
  if (keyIsDown (LEFT_ARROW))  {p.move (-1, 0);}
  if (keyIsDown (RIGHT_ARROW)) {p.move (+1, 0);}
  if (keyIsDown (UP_ARROW))    {p.move (0, -1);}
  if (keyIsDown (DOWN_ARROW))  {p.move (0, +1);}
}

function keyPressed() {
  console.log ('key', key);
  if (key === 'N') {
    wss.send ('n');
  } else if (key === 'R') {
    wss.send ('!');
  } else if (key === '=') {
    wss.send ('+');
  }
}

function other_player_move (data) {
  var pid = data[1];
  var px = data[2];
  var py = data[3];
  others[pid] = [px, py];
}

function force_move_player (x, y) {
  p.x = x;
  p.y = y;
}

// ----------------------------------------------------------------------------

function Player() {
  this.x = 0;
  this.y = 0;

  this.move = function (dx, dy) {
    if (m.legal (this.x, this.y, dx, dy)) {
      m.visited (this.x, this.y);
      // we don't need to range-check here, the walls do that.
      this.x = this.x + dx;
      this.y = this.y + dy;
      wss.send ('m' + String.fromCharCode (this.x, this.y));
    }
  }

  this.show = function() {
    fill(0, 0, 255);
    rect ((1+this.x) * scl+2, (1+this.y) * scl+2, scl-4, scl-4);
  }
}

// ----------------------------------------------------------------------------

function Maze (edata) {

  // we translate the 2-bits-per-cell encoded maze into
  // an 8-bits-per cell array with redundant information about
  // the walls. [i.e., a R wall is also represented by a L wall
  // in the adjoining cell].
  // this leaves an additional 4 bits for other uses (like a trail,
  // marking other players, in/out, etc). If 4 bits is not enough,
  // a bit could be designated as 'look in the other place for more
  // info'.

  // format: bits 0-3 mark out the presence of walls.
  // DRUL: D is bit 3, R is bit 2, ...

  this.decode = function() {
    this.w = this.encoded[1];
    this.h = this.encoded[2];
    this.data = Array (this.h);
    for (y=0; y < this.h; y++) {
      this.data[y] = new Uint8Array (this.w);
    }
    var x = 0;
    var y = 0;
    for (i=3; i < this.encoded.length; i++) {
      var b = this.encoded[i];
      for (j=3; j >=0; j--) {
        if (y === this.h) {
          // this can happen when there are extra bits.
          break;
        } else {
          // big-endian encoding is clumsy here.
          this.data[y][x] = (b & (3<<(2*j))) >> (2*j);
          x++;
          if (x === this.w) {
            x = 0;
            y++;
          }
        }
      }
    }
  }

  this.fill_out = function() {
    // add walls around the whole maze
    for (x=0; x < this.w; x++) {
      this.data[0][x] |= 2;
      this.data[this.h-1][x] |= 8;
    }
    for (y=0; y < this.h; y++) {
      this.data[y][0] |= 1;
      this.data[y][this.w-1] |= 4
    }
    // compute walls from neighbors
    for (y=0; y < this.h; y++) {
      for (x=0; x < this.w; x++) {
        if ((x < this.w-1) && (this.data[y][x+1] & 1)) {
          this.data[y][x] |= 4;
        }
        if ((y < this.h-1) && (this.data[y+1][x] & 2)) {
          this.data[y][x] |= 8;
        }
      }
    }
  }

  // draw the maze into its own separate graphics object.
  this.draw_maze = function (bgm) {
    bgm.background (240);
    for (y=0; y < this.h; y++) {
      for (x=0; x < this.w; x++) {
        // bits: 3210, DRUL
        var sx = (x+1) * scl;
        var sy = (y+1) * scl;
        if (this.data[y][x] & 1) {
          bgm.line (sx, sy, sx, sy+scl);
        }
        if (this.data[y][x] & 2) {
          bgm.line (sx, sy, sx+scl, sy);
        }
      }
    }
    // draw right side & bottom
    bgm.line ((this.w+1) * scl, scl, (this.w+1) * scl, (this.h+1) * scl);
    bgm.line (scl, (this.h+1) * scl, (this.w+1) * scl, (this.h+1) * scl);
  }

  this.draw_meta = function() {
    // not sure if this is the best place for this.
    // note: not drawing on the background image
    fill (255, 0, 0);
    rect (((1+this.goal_x) * scl)+2, ((1+this.goal_y) * scl)+2, scl-4, scl-4);
    for (const [k, v] of Object.entries (others)) {
      fill (220);
      px = v[0]; py = v[1];
      rect (((1+px) * scl)+2, ((1+py) * scl)+2, scl-4, scl-4);
    }
  }

  // is this move legal, given the wall layout in this cell?
  this.legal = function (x, y, dx, dy) {
    var walls = this.data[y][x];
    if (dx === 1 && (walls & 4)) {
      return false;
    } else if (dx === -1 && (walls & 1)) {
      return false;
    } else if (dy === 1 && (walls & 8)) {
      return false;
    } else if (dy === -1 && (walls & 2)) {
      return false;
    } else {
      return true;
    }
  }

  // modify the background image.
  this.visited = function (x, y) {
    var sx = ((x+1) * scl) + scl/2;
    var sy = ((y+1) * scl) + scl/2;
    bgm.fill (200);
    bgm.noStroke();
    bgm.ellipse (sx, sy, scl/3);
  }

  // --------------------------------------------------------------------------------
  // next steps:
  // do more stuff over the websocket.
  // 1) deliver maze data
  // 2) set goal & player position
  //
  // for all this we need a real handler over in irken-land.
  // --------------------------------------------------------------------------------

  this.new_goal = function (x, y) {
    this.goal_x = x;
    this.goal_y = y;
  }

  // initialize the maze object.
  this.encoded = new Uint8Array (edata);
  this.decode();
  this.fill_out();
  this.goal_x = -2;
  this.goal_y = -2;
}
