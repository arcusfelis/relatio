
sigma.publicPrototype.showAllNodes = function() {
   this.iterNodes(function(n) {
      if (!n.attr.alwaysHidden)
          n.hidden = false;
   });
}

sigma.publicPrototype.showNodes = function(node_ids) {
   this.iterNodes(function(n) {
      n.hidden = false;
   }, node_ids);
}

sigma.publicPrototype.showEdges = function(edge_ids, group_name) {
   // If the group name is specified.
   if (group_name)
   {
     this._index = this._index || [];
     var idx = this._index;
    
     // Hide edges that were previously registered under the name.
     if (idx[group_name])
     {
         var old_edge_ids = idx[group_name];
         this.iterEdges(function(e) {
            e.hidden = true;
         }, old_edge_ids);
     }
    
     idx[group_name] = edge_ids;
  }

   this.iterEdges(function(e) {
      e.hidden = false;
   }, edge_ids);
}

sigma.publicPrototype.hideEdges = function(edge_ids) {
   this.iterEdges(function(e) {
      e.hidden = false;
   }, edge_ids);
}

sigma.publicPrototype.borderNodes = function(onlyVisibleNodes) {
   var topNode, bottomNode, leftNode, rightNode;
   this.iterNodes(function(n) {
     if (onlyVisibleNodes && n.hidden)
       return;

     if (!topNode)
     {
       topNode = bottomNode = leftNode = rightNode = n;
       return;
     }

     if (n.y > topNode.y)         topNode    = n;
     if (n.y < bottomNode.y)      bottomNode = n;
     if (n.x < leftNode.x)        leftNode   = n;
     if (n.x > rightNode.x)       rightNode  = n;
   });

   return {"top": topNode, "bottom": bottomNode, "left": leftNode, "right": rightNode};
}

sigma.publicPrototype.getNodeById = function(nid) {
    var nodes = this.getNodes([nid]);
    return nodes[0];
}

sigma.publicPrototype.forAllNodes = function(fun, nids) {
  var graph = this._core.graph;
  return nids.every(function(nid) {
    return fun(graph.nodesIndex[nid]);
  });
}

sigma.publicPrototype.function2moduleNode = function(functionNode) {
  var moduleNode;
  var fid = functionNode.id;
  var sig = this;
  this.iterEdges(function(e) {
      if ((e.attr.edge_type == "mf") && (e.source == fid))
      {
          // var s = sig.getNodeById(e.source);
          // s.type == "module"
          moduleNode = sig.getNodeById(e.target);
      }
  });
  return moduleNode;
}

sigma.publicPrototype.module2applicationName = function(moduleNode) {
  var appNode;
  var fid = moduleNode.id;
  var sig = this;
  this.iterEdges(function(e) {
      if ((e.attr.edge_type == "am") && (e.source == fid))
      {
          // var s = sig.getNodeById(e.source);
          // s.type == "module"
          appNode = sig.getNodeById(e.target);
      }
  });
  return appNode;
}

sigma.publicPrototype.hideAllNodes = function() {
   this.iterNodes(function(n) {
      n.hidden = true;
   });
}

sigma.publicPrototype.setSourceEdgeColor = function() {
    var si = this;
    this.iterEdges(function(e) {
      e.color = si.getNodeById(e.source).color;
    });
}

sigma.publicPrototype.setTargetEdgeColor = function() {
    var si = this;
    this.iterEdges(function(e) {
      e.color = si.getNodeById(e.target).color;
    });
}


sigma.publicPrototype.zoomToNode = function(nid, ratio) {
  var nodes = this.getNodes([nid]);
  var node = nodes[0];
  if (!node) return;
  ratio = ratio ? ratio : 16;
  switch (node.attr.node_type)
  {
      case "module":
          ratio = 10;
  }

//console.log("node {" + node.displayX + ", " + node.displayY + "}");
  this.zoomToCoordinates(node.displayX, node.displayY, ratio);
}

sigma.publicPrototype.zoomToPoint = function(point, ratio) {
  this.zoomToCoordinates(point.x, point.y, ratio);
}

sigma.publicPrototype.zoomToCoordinates = function(displayX, displayY, ratio) {
  var m = this._core.mousecaptor;
  var s = this._core;
  if (!ratio) ratio = m.ratio;
//console.log("Zoom to: ", displayX, ", ", displayY, " with ", ratio);
  // Different formulas for diffenent cases
  if (ratio == m.ratio)
  {
      // TODO: dirty
      displayX = s.width - displayX; 
      displayY = s.height - displayY;
  }
  this.zoomTo(displayX, displayY, ratio);
}

sigma.publicPrototype.collectNoiseStatistics = function() {
    var s2c = [], t2c = [];
    this.iterEdges(function(e) {
        switch (e.attr.edge_type) {
            case undefined:
                if (!s2c[e.source]) s2c[e.source] = 1; else s2c[e.source]++;
                if (!t2c[e.target]) t2c[e.target] = 1; else t2c[e.target]++;
        }
    });
    this.sourceNodeId2callCount = s2c;
    this.targetNodeId2callCount = t2c;
}

sigma.publicPrototype.noiseFilter = function(enabled) {
    var s2c = this.sourceNodeId2callCount;
    var t2c = this.targetNodeId2callCount;
    var color = enabled ? "#000" : undefined;
    this.iterEdges(function(e) {
      if (s2c[e.source] > 15) e.hidden = enabled;
      if (t2c[e.target] > 15) e.hidden = enabled;
    });
    this.iterNodes(function(n) {
      if (s2c[n.id] > 15) n.rightSideColor = color;
      if (t2c[n.id] > 15) n.leftSideColor = color;
    });
}

sigma.publicPrototype.pickNode = function(nid) {
  switch (typeof(nid))
  {
    case "number":
    case "string":
      this.hoverNode(nid);
      break;

    case "object":
      this.hoverNode(nid);
      break;
    
    default:
      throw "Unknown type";
  }
}

sigma.publicPrototype.unpickNode = function(nid) {
  this.refresh();
  // Hide border nodes.
  $("#border-node, #top-border-node, #bottom-border-node," +
    "#left-border-node, #right-border-node").css("z-index", -10);
}

sigma.publicPrototype.hoverNode = function(nid) {
  var s = this._core,
      node = this.getNodeById(nid),

      canvasWidth = this.calculateCanvasWidth(),
      canvasHeight = this.calculateCanvasHeight(),

      top = Math.min(canvasHeight-20, Math.max(0, node.displayY)),
      left = Math.min(canvasWidth-20, Math.max(0, node.displayX)),
      right = s.width - canvasWidth,

  // stores a pointer on a div block, that represent this node.
      border,

  // Is the node out?
      isLeft = node.displayX < 0,
      isRight = node.displayX > canvasWidth,
      isTop = node.displayY < 0,
      isBottom = node.displayY > canvasHeight,

      bgColor = node.color;


  // This code adds a small triangle in the corner of the canvas.
  //
  // Creating Triangles in CSS
  // http://jonrohan.me/guide/css/creating-triangles-in-css/
  if (isLeft && isTop)
  {
      var bColor = bgColor + " transparent transparent " + bgColor;
      border = $("#border-node").css({"top": 0, "left": 0,
                                      "border-color": bColor});
      bgColor = "transparent";
  }
  else if (isLeft && isBottom)
  {
      var bColor = "transparent transparent " + bgColor + " " + bgColor;
      border = $("#border-node").css({"top": top, "left": 0, 
                                      "border-color": bColor});
      bgColor = "transparent";
  }
  else if (isRight && isTop)
  {
      var bColor = bgColor + " " + bgColor + " transparent transparent";
      border = $("#border-node").css({"top": 0, "left": left,
                                      "border-color": bColor});
      bgColor = "transparent";
  }
  else if (isRight && isBottom)
  {
      var bColor = "transparent " + bgColor + " " + bgColor + " transparent";
      border = $("#border-node").css({"top": top, "left": left, 
                                      "border-color": bColor});
      bgColor = "transparent";
  }
  // This code adds a small rectangle on the border of the canvas
  else if (isLeft)
  {
      border = $("#left-border-node").css("top", top);
  }
  else if (isRight)
  {
      border = $("#right-border-node").css("top", top)
                                      .css("right", right);
  }
  else if (isTop)
  {
      border = $("#top-border-node").css("left", left);
  }
  else if (isBottom)
  {
      border = $("#bottom-border-node").css("left", left);
  }
  // This code draws the label on background
  else s.plotter.drawHoverNode(node);

  if (border)
      border.css("z-index", 9999).css("background", bgColor);
}

/**
 * overwritten
 */
sigma.publicPrototype.calculateCanvasWidth = function() {
    return this._core.width;
}

sigma.publicPrototype.calculateCanvasHeight = function() {
    return this._core.height;
}

sigma.publicPrototype.centerPoint = function (coordinates)
{
  var minX = coordinates.left.displayX;
  var maxX = coordinates.right.displayX;
  var minY = coordinates.bottom.displayY;
  var maxY = coordinates.top.displayY;
  return {"x": this.centerBeetween2Points(minX, maxX),
          "y": this.centerBeetween2Points(minY, maxY)};
}

sigma.publicPrototype.centerBeetween2Points = function (a, b)
{
  return (a + b) / 2;
}

sigma.publicPrototype.borderDimentions = function(borders)
{
  var height = borders.top.y - borders.bottom.y;
  var width  = borders.right.x - borders.left.x;
  return {"height": height, "width": width};
};

sigma.publicPrototype.optimalScale = function(rightPanelSize) {
  var si = this,
      s = si._core,
      m = si._core.mousecaptor;

//console.log("rightPanelSize = ", rightPanelSize);
  // Borders of box for visible nodes only
  var vizBorders = si.borderNodes(true);
  // Borders of box for all nodes
  var allBorders = si.borderNodes();
  var vizDims = si.borderDimentions(vizBorders);
  var allDims = si.borderDimentions(allBorders);
  // Calculate the center of visible nodes.
  var vizCP = si.centerPoint(vizBorders);

  var allRatioX = allDims.width / s.width;
  var allRatioY = allDims.height / s.height;
  // Is there free space horizontically with default ratio?
  var isVerticalAlign = allRatioX < allRatioY;
//console.log("isVerticalAlign ", isVerticalAlign);

  // horGap = 0 .. 1
  var horGap = rightPanelSize / s.width;

  var ratioX = (1 - horGap) * allDims.width / vizDims.width;
  var ratioY = allDims.height / vizDims.height;

  /* When window height is too small, then vertical align is active.
     Otherwise, horizontal align is active.
     A factor is used to calculate ratioX with vertical align and ratioY with 
     horizontal align.

     A box is a border rectangle.

     Factor = WindowDimentionSize * BoxDimentionSizeForRatio1
     BoxDimentionSizeForRatio1 = DisplayDimentionSize * CurrentRatio
   */
  if (isVerticalAlign)
      ratioX *= s.width / (allBorders.right.displayX - allBorders.left.displayX) * m.ratio;
  else
      ratioY *= s.height / (allBorders.top.displayY - allBorders.bottom.displayY) * m.ratio;

  // Select the sharpest ratio.
  var ratio  = Math.min(ratioX, ratioY);

  /*
     If rightPanelSize = 300, than result center will be moved to 
     150 pixels to the left.

     Factor = m.ratio / ratio is used to scale the shift size.
  */
  var scaledRightPanelSize = rightPanelSize / 2 * m.ratio / ratio;
//console.log("scaledRightPanelSize = ", scaledRightPanelSize);
  si.zoomToCoordinates(vizCP.x + scaledRightPanelSize, vizCP.y, ratio);
};



sigma.publicPrototype.saveCurrentPosition = function() {
    var si = this;
    var n0 = si.getNodeById('zero');
    var n1 = si.getNodeById('one');
    var m = si._core.mousecaptor;
    var s = si._core;
    var ratio = m.ratio,
        centerX = s.width / 2,
        centerY = s.height / 2,

    // How many pixels from x = 0 to x = 1.
        pixelCountX = n1.displayX - n0.displayX,
        pixelCountY = n1.displayY - n0.displayY;

    var centerx = (centerX - n0.displayX) / pixelCountX;
    var centery = (centerY - n0.displayY) / pixelCountY;

    return {"x": centerx, "y": centery, "ratio": m.ratio};
};


sigma.publicPrototype.setPosition = function(pos) {
  var si = this;
  n0 = si.getNodeById('zero');
  n1 = si.getNodeById('one');

  var pixelCountX = n1.displayX - n0.displayX,
      pixelCountY = n1.displayY - n0.displayY;

  si.zoomToCoordinates(n0.displayX + pixelCountX * pos.x, 
                       n0.displayY + pixelCountY * pos.y, 
                       pos.ratio);
};



var Ring = function(size)
{
    var a = new Array(size),
        oldestN = 0,
        newestN = 0,
        currentN = -1;

    this.reset = function() {
        oldestN = 0;
        newestN = 0;
        currentN = -1;
    }

    /**
     * Get Nth element.
     */
    var pos = function(N) { 
        return (N + size) % size; 
    }

    this.next = function() {
        return a[pos(currentN+1)];
    }

    this.current = function() {
      if (currentN < oldestN)
        // We are out of the ring.
        return;

      return a[pos(currentN)];
    }

    var show = function() {
        console.log("oldestN = ", oldestN, ", currentN = ", currentN, ", newestN = ", newestN);
    }

    /** 
     * Add an element at the end.
     */
    this.add = function(e) {
//    console.log("add " + e);
      // Move the pointer on the next cell.
      currentN++;

      // Save value in the circular buffer.
      a[pos(currentN)] = e;

      newestN = currentN;

      if (size < currentN && currentN == newestN)
      {
          // The value in the old cell will be rewritten by `e'.
          oldestN++;
      }

//    console.log(oldestN);

//    console.dir(a);
    }

    /**
     * Add or replace
     */
    this.check = function(e) {
      // If e is a next element, than save history.
//    console.log("check " + e + " " + this.next());
      show();

      if ((newestN != currentN) && (e == this.next()))
        currentN++;
      else
        this.add(e);

//    console.dir(a);
    }

    this.go = function(stepCount) {
//    console.log("GO TO " + stepCount);
      show();
      currentN += stepCount;

      if (currentN < oldestN)
      {
        // We are out of the ring.
        // Put the cursor on a position before the first element.
        currentN = oldestN - 1;
        return;
      }

      if (currentN > newestN)
      {
        // The forward history is empty.
        // Put the curson on the last position.
        currentN = newestN;
      }

      return this.current();
    }
}


var Window = function(hidden) {
    this.hidden = hidden;
    this.keyHandler = function(e) {};
    this.activationHandler = function(e) {};
    this.deactivationHandler = function(e) {};
}

var WindowManager = function() {
    var ws = [],
        pos = 0;

    // Add the new window
    var register = function(win) {
        ws.push(win);
    }

    var activate = function(win) {
        var old_pos = pos;
        for (var i = 0; i < ws.length; i++)
        {
          if (ws[i] != win) continue;
          win.hidden = false;
          pos = i;
          ws[old_pos].deactivationHandler();
          ws[pos].activationHandler();
          return;
        }

        throw "Win is not registered.";
    }

    var deactivate = function(win) {
        win.hidden = true;
        return current();
    }

    var allHidden = function() {
      return ws.every(function(win) { return win.hidden; });
    }

    // Activate the next visible window
    var next = function(cnt) {
        if (allHidden()) return;

        cnt = cnt || 1;
        var step = cnt > 0 ? 1 : -1;
        var old_pos = pos;

        while(cnt)
        {
          pos += step;

          // Check borders
          if (pos == ws.length) pos = 0;
          else
          if (pos == -1) pos = ws.length - 1;

          if (!ws[pos].hidden)
            cnt -= step;
        }

        if (old_pos != pos) {
            ws[old_pos].deactivationHandler();
            ws[pos].activationHandler();
        }

        return ws[pos];
    }

    // If the current window is visible, then return it, otherwise return
    // the next window and declate it as a current one.
    var maybeCurrent = function() {
      if (!ws.length)
        return;

      var cur = ws[pos];
      if (cur.hidden)
        return next();

      return ws[pos];
    }

    var current = function() {
        var win = maybeCurrent();
        if (!win) throw "Bad window.";
        return win;
    }

    this.register = register;
    this.next = next;
    this.current = current;
    this.activate = activate;
    this.deactivate = deactivate;
}

var Tip = function(si) 
{
  var view = $("#info-tip");
  this.show = function(e) {
      var nodeIds = e.content;
      if (!nodeIds.length)
          return;
      var node = si.getNodeById(nodeIds[0]);
      if (!node.attr.node_title)
          return;
      view.html(node.attr.node_title);

      var canvasWidth = si.calculateCanvasWidth(),
          canvasHeight = si.calculateCanvasHeight(),
          tipWidth = view.outerWidth(),
          tipHeight = view.outerHeight();

      // Calculate popup position.
      // Set X and Y, if the selected node is not on the screen.
      var props = {};

      if (node.displayX < 0)
      {
          // left border
          props.left = 10;
          props.top = node.displayY - tipHeight / 2;
          props.top = Math.min(Math.max(10, props.top),
                               canvasHeight - tipHeight - 10); 
      }
      else if (node.displayX > canvasWidth)
      {
          // right border
          props.left = canvasWidth - tipWidth - 10;
          props.top = node.displayY - tipHeight / 2;
          props.top = Math.min(Math.max(10, props.top),
                               canvasHeight - tipHeight - 10); 
      }
      else if (node.displayY < 0)
      {
          // top border
          props.top = 10;
          props.left = node.displayX - tipWidth / 2;
          props.left = Math.min(Math.max(10, props.left),
                                canvasWidth - tipWidth - 10); 
      }
      else if (node.displayY > canvasHeight)
      {
          // bottom border
          props.top = canvasHeight - tipHeight - 10;
          props.left = node.displayX - tipWidth / 2;
          props.left = Math.min(Math.max(10, props.left),
                                canvasWidth - tipWidth - 10); 
      }
      else
      {
        // The node is on the screen
        props.left = Math.min(node.displayX,
                              canvasWidth - tipWidth - 15); // 15 is a margin.
       
        var centerY = si._core.height / 2;
       
        // 5 is an additional margin.
        var popupY = Math.min(node.displayY,
                              canvasHeight + 5);
       
        if (popupY < centerY)
            // The node is on top-side of the screen.
            props.top = popupY + 15;
        else
            props.top = popupY - tipHeight - 15;
      }

      view.css(props);

      $("body").addClass("info-tip-active");
  };
  this.hide = function(e) {
      $("body").removeClass("info-tip-active");
  };
};

$.fn.dataArray = function(key) {
    var arr = [];
    for (var i = 0; i < this.length; i++) {
      var val = $(this[i]).data(key);
      if (val)
        arr.push(val);
    }
    return arr;
}

/* JSP (scrollbars) EXTENSIONS FOR jQuery */
$.fn.activateAutoResizeMonitor = function() {
  if (!this.length)
      throw "Bad area";

  var area = this;
  var win = $(window);
  var jsp = area.data("jsp");
  var pane = $(".jspPane", area);

  var windowHeight = win.height();
  var containerHeight = pane.height();
  var resized;

  var intId;
  intId = area.data("activateAutoResizeMonitorId");
  if (intId)
      clearInterval(intId);

  jsp.reinitialise();
  var reinit = function() {
    jsp.reinitialise();
    // Return context to initial position, if the horizontal scrollbar was removed.
    pane.css({left: 0});
      
    // Prevent propagation
    $("div.jspHorizontalBar, div.jspVerticalBar", area).on("click.preventProp", function(e) {
      return false;
    });
  };
  reinit();

  intId = setInterval(function() {

    if(windowHeight!=win.height() || containerHeight!=pane.height())
    {
      clearTimeout(resized);
      resized = setTimeout(reinit, 100);
    }

    windowHeight = win.height();
    containerHeight = pane.height();
  }, 75);

  area.data("activateAutoResizeMonitorId", intId);
};

$.fn.updateScrolling = function() {
  var area = this;
  if (area.jspUpdateTimeout)
      clearTimeout(area.timeout);

  var updateScrollingNow = function(area) {
    var jsp = area.data("jsp");
    var pane = $(".jspPane", area);
    if (jsp)
      jsp.reinitialise();
  };

  var f = function() {
      updateScrollingNow(area);
      area.jspUpdateTimeout = undefined;
  }

  area.jspUpdateTimeout = setTimeout(f, 700);
};


var relatio = {};
relatio.keyBoard = {};
relatio.utils = {};
relatio.keyBoard.keyCodes = {
    ESCAPE:     27,
    ENTER:      13,
    SHIFT:      16,
    TAB:        9,
    PAGEDOWN:   34,
    PAGEUP:     33
};
relatio.keyBoard.charCodes = {
    H:        72,
    J:        74,
    K:        75,
    L:        76,
    M:        77,
    N:        78,

    R:        82,
    U:        85,

    h:        104,
    j:        106,
    k:        107,
    l:        108,
    m:        109,
    n:        110,

    r:        114,
    u:        117,

    w:        119,

    ONE:      49,
    NINE:     57,
    ZERO:     48,
    PLUS:     43,
    EQUALS:   61,
    MINUS:    45,
    SLASH:    47,
    TILDA:    126,
    GRAVE:    96,
    RIGHT_PARENTHESIS: 41,
    WHITESPACE: 32
};

// Move from id on `offset` nodes forward using ids as an index,
relatio.utils.nextNode = function(offset, id, ids)
{
  var currect_index = ids.indexOf(id),
      len = ids.length;
  // circled
  var new_index = (currect_index + offset) % len;
  // Fix negative index of the array.
  if (new_index < 0)
      new_index = len + new_index;
  return ids[new_index];
};


(function($) {
  $.expr[":"].onScreen = function(elem) {
    var $window = $(window),
        viewport_top = $window.scrollTop(),
        viewport_height = $window.height(),
        viewport_bottom = viewport_top + viewport_height,
        $elem = $(elem),
        top = $elem.offset().top,
        height = $elem.height(),
        bottom = top + height;

    return (top >= viewport_top && top < viewport_bottom) &&
           (bottom > viewport_top && bottom <= viewport_bottom); 
}
})(jQuery);

