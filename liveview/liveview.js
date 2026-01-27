var liveview_socket = null;
const liveview_queue = [];

function liveview_patch(id, html) {
  // TODO I think it's feasible to put the id into a custom element,
  // like data-liveview-component=<id>, to free the id attribute for
  // use by the API consumer. A manually maintained WeakMap index should
  // do the trick
  const component = document.getElementById(id);
  if (!component) {
    console.log('error: patch component: component id not found:', id);
    return;
  }
  Idiomorph.morph(component, html, {
    // this prevents truncation of text input when input effect is delayed
    // TODO some more thought required to preserve input values on navigation
    // and soft refresh, where browsers restore the input without liveview
    // noticing it.
    ignoreActiveValue: true,
    callbacks: {
      beforeNodeMorphed: (oldNode, newNode) => {
        if (newNode.hasAttribute && newNode.hasAttribute('data-morph-skip')) {
          return false; // skip updating this element
        } else {
          console.log("liveview: morph", newNode);
          return true;
        };
      },
    },
  });
};

function liveview_boot(crsf_token) {
  console.log('liveview: start liveview');
  const loc = window.location;
  const tok = crsf_token;
  var ws_url;
  if (loc.search) {
    ws_url = `//${loc.host}${loc.pathname}${loc.search}&csrf_token=${tok}`;
  } else {
    ws_url = `//${loc.host}${loc.pathname}?csrf_token=${tok}`;
  }
  console.log('liveview: open websocket');
  liveview_socket = new WebSocket(ws_url);

  liveview_socket.onopen = function () {
    console.log('liveview: ready');
    while (liveview_queue.length > 0) {
      liveview_socket.send(liveview_queue.shift());
    };
  };

  liveview_socket.onmessage = function (e) {
    var obj = false;
    try {
      obj = JSON.parse(e.data);
    } catch (err) {
      console.log('liveview: rcv (parse error)', e.data);
      return;
    }
    console.log('liveview: rcv', obj);
    if (obj && obj[0] === 'Updates') {
      obj[1].forEach(upd => {
        liveview_patch(upd[0], upd[1]);
      });
    };
  };
}

function liveview_send(obj) {
  console.log('liveview: snd', obj);
  var msg = JSON.stringify(obj);
  if (liveview_socket && liveview_socket.readyState === 1) {
    liveview_socket.send(msg);
  } else {
    liveview_queue.push(msg);
  }
};

function liveview_send_info(text) {
  liveview_send(['Info', text])
};

function liveview_send_event() {
  liveview_send(['Event', obj])
};

function liveview_handler(name, arg1) {
  return ((id, event) => {
    event.preventDefault();
    event.stopPropagation();

    var obj = [name];
    if (arg1) {
      obj.push(arg1(event));
    }
    liveview_send(['Event', id, obj]);
  });
};

const liveview_onclick = liveview_handler('OnClick');
const liveview_oninput = liveview_handler('OnInput', e => e.target.value);
