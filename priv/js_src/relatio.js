sigma.publicPrototype.showAllNodes = function() {
   this.iterNodes(function(n) {
      n.hidden = false;
   });
}

sigma.publicPrototype.showNodes = function(node_ids) {
   this.iterNodes(function(n) {
      n.hidden = false;
   }, node_ids);
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

  console.log("node {" + node.displayX + ", " + node.displayY + "}");
  this.zoomToCoordinates(node.displayX, node.displayY, ratio);
}

sigma.publicPrototype.zoomToPoint = function(point, ratio) {
  this.zoomToCoordinates(point.x, point.y, ratio);
}

sigma.publicPrototype.zoomToCoordinates = function(displayX, displayY, ratio) {
  var m = this._core.mousecaptor;
  var s = this._core;
  if (!ratio) ratio = m.ratio;
  console.log("Zoom to: ", displayX, ", ", displayY, " with ", ratio);
  // Different formulas for diffenent cases
  if (ratio == m.ratio)
  {
      // TODO: dirty
      displayX = s.width - displayX; 
      displayY = s.height - displayY;
  }
  this.zoomTo(displayX, displayY, ratio);
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
      console.error("Unknown type");
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


  // This code adds a small triangle in the corner of the canvas
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

var Ring = function(size)
{
    var a = new Array(size),
        oldestN = 0,
        newestN = 0,
        currentN = -1;

    this.reset = function() {
        oldestN = 0;
        newestN = 0;
        nextN = -1;
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
        return a[pos(currentN)];
    }

    var resetCurrent = function() {
        a[pos(currentN)] = undefined;
    }

    var show = function() {
        console.log("oldestN = ", oldestN, ", currentN = ", currentN, ", newestN = ", newestN);
    }

    /** 
     * Add an element at the end.
     */
    this.add = function(e) {
      console.log("add " + e);
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
      console.log("check " + e + " " + this.next());
      show();

      if ((newestN != currentN) && (e == this.next()))
        currentN++;
      else
        this.add(e);

      console.dir(a);
    }

    this.goBackward = function() {
      console.log("goBackward");
      show();

      if (currentN < oldestN)
          // We are out of the ring.
          return;

      currentN--;

      if (currentN < oldestN)
      {
          // The backward history is empty.
          // Set on -1 position.
          resetCurrent();
          return;
      }


      return this.current();
    }

    this.goForward = function() {
      if (currentN == newestN)
          // The forward history is empty.
          return undefined;

      currentN++;
      return this.current();
    }
}

var relatio = {};

relatio.init = function() {

  self = this;
  var keyCodes = {
      ESCAPE:   27,
      ENTER:    13
  };
  var charCodes = {
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



      ONE:      49,
      NINE:     57,
      ZERO:     48,
      PLUS:     43,
      EQUALS:   61,
      MINUS:    45,
      SLASH:    47,
      TILDA:    126,
      GRAVE:    96,
      RIGHT_PARENTHESIS: 41
  };
  var MIN_IMPORTANT_SIZE = 9;


  var current_node_id, current_node_ids, showPopup;

  var active_node_history = new Ring(10);

  function centerPoint(coordinates)
  {
    var minX = coordinates.left.displayX;
    var maxX = coordinates.right.displayX;
    var minY = coordinates.bottom.displayY;
    var maxY = coordinates.top.displayY;
    return {"x": centerBeetween2Points(minX, maxX),
            "y": centerBeetween2Points(minY, maxY)};
  }

  function centerBeetween2Points(a, b)
  {
    return (a + b) / 2;
  }


  var overlayNodeContainer = $("#overlay-nodes");
  // Add overlay node.
  function addOverlayNode(node)
  {
    var o = $("<div>").addClass("overlay-node")
        .css("left", node["displayX"])
        .css("top", node["displayY"])
        .css("background-color", node.color);
    overlayNodeContainer.append(o);
    return o;
  }

  // Instanciate sigma.js and customize rendering :
  var si = sigma.init(document.getElementById('graph-main')).drawingProperties({
    defaultLabelColor: '#fff',
    defaultLabelSize: 14,
    defaultLabelBGColor: '#fff',
    defaultLabelHoverColor: '#000',
    labelThreshold: 6,
    defaultEdgeType: 'curve',
    font: 'DejaVu Sans Mono, Arial'
  }).graphProperties({
    minNodeSize: 1,
    maxNodeSize: 5,
    minEdgeSize: 1,
    maxEdgeSize: 2
  }).mouseProperties({
    maxRatio: 32,
    minRatio: 0.5
  });

  si.addNode('zero', { 'x': 0, 'y': 0, 'hidden': true });
  si.addNode('one',  { 'x': 1, 'y': 1, 'hidden': true });

  // Parse a GEXF encoded file to fill the graph
  // (requires "sigma.parseGexf.js" to be included)
  si.parseGexf('data/v-e-m.gexf');
  si.iterEdges(function(e) {
    if (e.weight)
      e.size = e.weight;
  });

  si.draw(2, 0, 0);


  // elem is a DOM element.
  // node is a sigma node.
  function nodeToHtmlLink(si, node, elem)
  {

    var nid = node.id;
    var a = $("<a tabindex='0'>").addClass("node-" + nid).text(node.label);

    // Add a class to element (not to "<a>")
    if (node.attr.is_exported)
        elem.addClass("is-exported");
    else
        elem.removeClass("is-exported");

    a.off(".show_node_label");
    a.on("click.show_node_label", function(e) {
      si.unpickNode(nid);
      hidePopup({"content": [nid]});

      // Capture a Shift and Click event with jQuery
      if (e.shiftKey) {
        activateNode({'target': si, 'content': [nid]});
        return false;
      }
      si.zoomToNode(nid);
//    si.pickNode(nid);
      return false;
    });
    // Show hovered label fore the node.
    // Show popup window with description.
    a.on("mouseleave.show_node_label", function(){ 
        si.unpickNode(nid);
        hidePopup({"content": [nid]});
    });
    a.on("mouseenter.show_node_label", function(){ 
        si.pickNode(nid);
        showPopup({"content": [nid]});
    });
    elem.append(a);
    return elem;
  }

  // Copy color from a node or an edge to DOM element.
  function colorize(nodeOrEdge, elem)
  {
    elem.css("color", nodeOrEdge.color)
    return elem;
  }

  var nodeIdsToHtml = function(si, nodes) {
    var ul = $("<ul>");
    si.iterNodes(function(node) {
      var elem = colorize(node, $("<li>"));
      var li = nodeToHtmlLink(si, node, elem);
      ul.append(li);
    }, nodes);
    return ul;
  }

  var moduleIds2functionsIds = function(si, module_ids) {
    var function_node_ids = [];
    si.iterEdges(function(e) {
        if (e.attr.edge_type == "mf" && ~module_ids.indexOf(e.target)) {
            function_node_ids.push(e.source);
        }
    });
    return function_node_ids;
  }

  var functionIds2functionIds = function(si, ids) {
    var function_node_ids = [];
    si.iterEdges(function(e) {
        if (!e.attr.edge_type) // call edge
        {
          if (~ids.indexOf(e.target))
            function_node_ids.push(e.source);
          else if (~ids.indexOf(e.source))
            function_node_ids.push(e.target);
        }
    });
    return function_node_ids;
  }

  var functionIds2modulesIds = function(si, function_ids) {
    var module_ids = [];
    si.iterEdges(function(e) {
        if (e.attr.edge_type == "mf" && ~function_ids.indexOf(e.source)) 
          module_ids.push(e.target);
    });
    return module_ids;
  }

  // This function will be called, if a node was clicked.
  var activateNode = function(event) { 
    var si = event.target; 
    var ids = event.content;
    var rcpnt_node_ids      = [];
    var donor_node_ids      = [];
    var module_node_ids     = [];
    var function_node_ids   = [];
    var visible_node_ids    = [];

    var node_id = ids[0];
    var node = si.getNodeById(node_id);

    active_node_history.check(node_id);

    $(".selected-elem-info").hide();
    var active_header_block;
    var focused_elem;

    // Change the header
    switch (node.attr.node_type)
    {
      case "module":
        var activeHeaderBlock = $("#selected-module-elem-info").show();
        var m_elem = $(".module-name", active_header_block).empty();
        nodeToHtmlLink(si, node, m_elem);
        focused_elem = $("a", m_elem);

        // Change a current set of nodes
        current_node_id = node.id;
        current_node_ids = module_ids;
        break;

      // MFA
      default:
        var parent_node = si.function2moduleNode(node);
        $("#selected-function-elem-info").show();
        var fn_elem = $(".function-name", active_header_block).empty();
        var m_elem = $(".module-name", active_header_block).empty();
        nodeToHtmlLink(si, node, fn_elem);
        nodeToHtmlLink(si, parent_node, m_elem);
        focused_elem = $("a", fn_elem);

        // Change a current set of nodes
        current_node_id = node.id;
        // Get brothers of the function node.
        current_node_ids = moduleIds2functionsIds(si, [parent_node.id]);
    }


    switch (node.attr.node_type)
    {
      case "module":
        si.iterEdges(function(e) {
            switch (e.attr.edge_type)
            {
            case "mf":
              if (~ids.indexOf(e.target)) {
                  function_node_ids.push(e.source);
              }
              break;
            case "mm":
              if (~ids.indexOf(e.source)) {
                  // This edge is out.
                  // The selected node calls the iterated node.
                  donor_node_ids.push(e.target);
              }
              else if (~ids.indexOf(e.target)) {
                  // This edge is in.
                  // The selected node is called by the iterated node.
                  rcpnt_node_ids.push(e.source);
              }
            }
        });
        var module_node_ids = ids.concat(donor_node_ids).concat(rcpnt_node_ids);
        visible_node_ids = function_node_ids.concat(module_node_ids)
//          .concat(moduleIds2functionsIds(si, module_node_ids));
            .concat(functionIds2functionIds(si, function_node_ids));
        break;


      // function
      default:
        si.iterEdges(function(e) {
          if (~ids.indexOf(e.source) && e.attr.edge_type != "mf") {
            // This edge is out.
            // The selected node calls the iterated node.
            donor_node_ids.push(e.target);
          }
          else if (~ids.indexOf(e.target)) {
            // This edge is in.
            // The selected node is called by the iterated node.
            rcpnt_node_ids.push(e.source);
          }
        });
        var function_ids = ids.concat(donor_node_ids).concat(rcpnt_node_ids);
        visible_node_ids = function_ids
                         .concat(functionIds2modulesIds(si, function_ids));
    }

    si.hideAllNodes();
    si.showNodes(visible_node_ids);
    si.draw(2, 1);

    if (donor_node_ids.length > 0)
    {
      var ul_out = nodeIdsToHtml(si, donor_node_ids);
      $("#directions-list-out").empty().append(ul_out);
      $("#directions-out").show();
    }
    else
    {
      $("#directions-out").hide();
    }

    if (rcpnt_node_ids.length > 0)
    {
      var ul_in  = nodeIdsToHtml(si, rcpnt_node_ids);
      $("#directions-list-in").empty().append(ul_in);
      $("#directions-in").show();
    }
    else
    {
      $("#directions-in").hide();
    }

    if (function_node_ids.length > 0)
    {
      var ul_in  = nodeIdsToHtml(si, function_node_ids);
      $("#functions-list").empty().append(ul_in);
      $("#functions").show();
    }
    else
    {
      $("#functions").hide();
    }
    openDirectionSidebar();

    // Set focus
    focused_elem.focus();
  }; // end of activateNode


  // The node was clicked.
  si.bind('downnodes', activateNode);


  var tip = $("#info-tip");
  showPopup = function(e) {
      var nodeIds = e.content;
      if (!nodeIds.length)
          return;
      var node = si.getNodeById(nodeIds[0]);
      if (!node.attr.node_title)
          return;
      tip.html(node.attr.node_title);

      var canvasWidth = si.calculateCanvasWidth(),
          canvasHeight = si.calculateCanvasHeight(),
          tipWidth = tip.outerWidth(),
          tipHeight = tip.outerHeight();

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

      tip.css(props);

      $("body").addClass("info-tip-active");
  };
  var hidePopup = function(e) {
      $("body").removeClass("info-tip-active");
  };
  si.bind('overnodes', showPopup);
  si.bind('outnodes', hidePopup);
  si._core.mousecaptor.bind('startdrag', hidePopup);

  /*
  var stopInterpolate = function(e) {
      console.log("stopInterpolate");
  }
  si._core.mousecaptor.bind('stopinterpolate', stopInterpolate);
  */




  var saveCurrentPosition = function() {
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
  }


  var setPosition = function(pos) {
    n0 = si.getNodeById('zero');
    n1 = si.getNodeById('one');

    var pixelCountX = n1.displayX - n0.displayX,
        pixelCountY = n1.displayY - n0.displayY;

    si.zoomToCoordinates(n0.displayX + pixelCountX * pos.x, 
                         n0.displayY + pixelCountY * pos.y, 
                         pos.ratio);
  }


  var openDirectionSidebar = function() {
    $("body").addClass("directions-active"); 
    activateAutoResizeMonitor($("#graph-directions"));
  };

  var closeDirectionSidebar = function(saveHistory) {
    si.showAllNodes();
    si.draw(2, 1);
    $("body").removeClass("directions-active"); 
    $(".searching-active #search-field").focus();
    if (!saveHistory)
        active_node_history.reset();
  }

  var closeSearchSidebar = function(save_socus) {
    if (!save_socus)
      $("#graph-modules-link a:visible").focus();
    $("body").removeClass("searching-active"); 
  };


  var isDirectionSidebarOpen = function() {
    return $("body").hasClass("directions-active");
  };

  var resetScale = function(){ 
    si.position(0, 0, 1);
    si.draw(2, 1);
    si.goTo(0, 0, 1);
  };

  var calculateRightPanelSize = function() {
    if ($("body").hasClass("directions-active"))
        return $("#graph-directions").width();

    if ($("body").hasClass("searching-active"))
        return $("#search-pane").width();

    return 0;
  };


  var borderDimentions = function(borders)
  {
    var height = borders.top.y - borders.bottom.y;
    var width  = borders.right.x - borders.left.x;
    return {"height": height, "width": width};
  };

  si.calculateCanvasWidth = function() {
    return si._core.width - calculateRightPanelSize();
  }

  var optimalScale = function() {
    var s = si._core;
    var m = si._core.mousecaptor;
    var rightPanelSize = calculateRightPanelSize();
//  console.log("rightPanelSize = ", rightPanelSize);
    // Borders of box for visible nodes only
    var vizBorders = si.borderNodes(true);
    // Borders of box for all nodes
    var allBorders = si.borderNodes();
    var vizDims = borderDimentions(vizBorders);
    var allDims = borderDimentions(allBorders);
    // Calculate the center of visible nodes.
    var vizCP = centerPoint(vizBorders);

    var allRatioX = allDims.width / s.width;
    var allRatioY = allDims.height / s.height;
    // Is there free space horizontically with default ratio?
    var isVerticalAlign = allRatioX < allRatioY;
//  console.log("isVerticalAlign ", isVerticalAlign);

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
//  console.log("scaledRightPanelSize = ", scaledRightPanelSize);
    si.zoomToCoordinates(vizCP.x + scaledRightPanelSize, vizCP.y, ratio);
  };

  // Handler for closing the sidebar
  $("#graph-directions").click(function(e) {
    closeDirectionSidebar();
  });

  $("#search-pane").click(function(e) {
    closeSearchSidebar();
  });

  /////////////////////// Key Handlers ///////////////////////////////

  var repeatCount = 0;
  var nextKeyHandler;
  var marks = [];

  var canvasKeyPressHandler = function(e) {
    var m = si._core.mousecaptor;
    var s = si._core;
    var kc = e.keyCode;
    var cc = e.charCode;
    if (!!nextKeyHandler)
      return nextKeyHandler(e);

    switch (kc) {
      case keyCodes.ESCAPE:
        resetScale();
        break;
    }

    
    switch (cc) {
      case charCodes.ZERO:
        if (repeatCount == 0) {
          resetScale();
          break;
        } else return "default";

      case charCodes.RIGHT_PARENTHESIS:
        optimalScale(); 
        break;

      case charCodes.h:
        // right
        var step = s.width / 150;
        si.goTo(-step * (repeatCount || 1) + m.stageX, m.stageY);
        repeatCount = 0;
        break;

      case charCodes.l:
        // left
        var step = s.width / 150;
        si.goTo(step * (repeatCount || 1) + m.stageX, m.stageY);
        repeatCount = 0;
        break;

      case charCodes.k:
        // up
        var step = s.height / 100;
        si.goTo(m.stageX, -step * (repeatCount || 1) + m.stageY);
        repeatCount = 0;
        break;

      case charCodes.j:
        // down
        var step = s.height / 100;
        si.goTo(m.stageX, step * (repeatCount || 1) + m.stageY);
        repeatCount = 0;
        break;

      case charCodes.N:
      case charCodes.n:
        // next
        var offset = (e.shiftKey ? -1 : 1) * (repeatCount || 1);
        var next_node_id = nextNode(offset, current_node_id, current_node_ids);
        activateNode({'target': si, 'content': [next_node_id]});
        repeatCount = 0;
        break;

      case charCodes.m:
        // put a mark
        repeatCount = 0;
        nextKeyHandler = function(e) {
          nextKeyHandler = undefined;

          marks[e.charCode] = saveCurrentPosition();
        }
        break;

      case charCodes.GRAVE:
        // go to the mark
        repeatCount = 0;
        nextKeyHandler = function(e) {
          nextKeyHandler = undefined;

          var pos = marks[e.charCode];
          if (pos) setPosition(pos);
          else noty({text: "A mark is not available for this key.",
                     type: "warning",
                     layout: "bottomCenter",
                     timeout: 3000});
        }
        break;

      case charCodes.PLUS:
      case charCodes.EQUALS:
        var delta = 0.1 * m.ratio * (repeatCount || 1);
        si.zoomTo(s.width / 2, s.height / 2, m.ratio + delta);
        repeatCount = 0;
        break;

      case charCodes.MINUS:
        var delta = 0.1 * m.ratio * (repeatCount || 1);
        si.zoomTo(s.width / 2, s.height / 2, m.ratio - delta);
        repeatCount = 0;
        break;

      case charCodes.SLASH:
        $("#search-field").focus();
        // TODO: select old text.
        // Don't let this char be entrered in the search field.
        return false;
        break;

      default:
        return "default";
    }
  } // End of the `canvasKeyPressHandler` function.


  var panelKeyPressHandler = function(e) {
    var m = si._core.mousecaptor;
    var s = si._core;
    var kc = e.keyCode;
    var cc = e.charCode;
    if (!!nextKeyHandler)
      return nextKeyHandler(e);

    switch (kc) {
      case keyCodes.ESCAPE:
        if (isDirectionSidebarOpen())
          closeDirectionSidebar();
        else
          resetScale();
        break;

      case keyCodes.ENTER:
        // Thare are different behaviour for focused links without the `href`
        // attribute.
        //
        // Firefox handles pressing the ENTER key and calls `click`,
        // while Chromium does not call it.
        //
        // Emulate Shift+Click event.
        var ee = $.Event("click");
        ee.shiftKey = e.shiftKey;
        $(":focus").trigger(ee);
        return false;
        break;
    }

    
    switch (cc) {

      case charCodes.h:
        // the right key, the previous pane
        
        // Tho steps back, one forward.
        var currentNodeId = active_node_history.current();
        var prevNodeId = active_node_history.goBackward();

        console.log(currentNodeId + " " + prevNodeId);

        if (!currentNodeId)
            break;


        if (!prevNodeId) {
            // It is already the oldest node on the direction pane.
            // Close this pane with savind the history of movements.
            // Activate search panel.
            closeDirectionSidebar(true);
            // Focus the node.
            $(".node-" + currentNodeId).focus();
        } else {
            active_node_history.goBackward();
            console.log("cur: " + active_node_history.current());
            // Activate previous node
            activateNode({'target': si, 'content': [prevNodeId]});
            $(".node-" + currentNodeId).focus();
        }
   //   repeatCount = 0;
        break;


      case charCodes.l:
        // the left key, the next pane

        // If the current node is already latest, then nextNodeId is undefined.
        var nextNodeId = active_node_history.next();
        console.log("nextNodeId = " + nextNodeId);
        if (!!nextNodeId) {
            activateNode({'target': si, 'content': [nextNodeId]});
        }
        break;

      case charCodes.k:
        // up
        break;

      case charCodes.j:
        // down
        break;


      default:
        return "default";
    }
  } // End of the `canvasKeyPressHandler` function.


  var selectedKeyHandler = canvasKeyPressHandler;

  $(document).keypress(function(e) {

    var resultFromKeyHandler = selectedKeyHandler(e);

    switch (resultFromKeyHandler)
    {
        case "default":
          return defaultKeyHandler(e);
          break;

        // This key was handled.
        default:
          return resultFromKeyHandler;
    }
  });


  var defaultKeyHandler = function(e)
  {
    var kc = e.keyCode,
        cc = e.charCode;

    // common handler
    switch (kc) {
      case keyCodes.ENTER:
        // Thare are different behaviour for focused links without the `href`
        // attribute.
        //
        // Firefox handles pressing the ENTER key and calls `click`,
        // while Chromium does not call it.
        //
        // Emulate Shift+Click event.
        var ee = $.Event("click");
        ee.shiftKey = e.shiftKey;
        $(":focus").trigger(ee);
        return false;
        break;
    }


    switch (cc) {
      default:
        if (cc >= charCodes.ZERO && cc <= charCodes.NINE)
            repeatCount = repeatCount * 10 + cc - charCodes.ZERO;
      console.log("Key pressed: " + e.keyCode, " Char entered: " + e.charCode);
    }
  };
            


  // The other canvas for a list of modules
  var msi = sigma.init(document.getElementById('graph-modules')).drawingProperties({
    defaultLabelColor: '#fff',
    defaultLabelSize: 14,
    defaultLabelBGColor: '#fff',
    defaultLabelHoverColor: '#000',
    labelThreshold: 10,
    defaultEdgeType: 'curve'
  }).graphProperties({
    minNodeSize: 0.5,
    maxNodeSize: 5,
    minEdgeSize: 1,
    maxEdgeSize: 1
  }).mouseProperties({
    mouseEnabled: false
  });

  // Add border edges
  msi.addNode(1, {
        'x': -1,
        'y': -2,
        'label' : "Left-Top",
        hidden: true,
        color: "#F00"
      });
  msi.addNode(2, {
        'x': 40,
        'y': 25,
        'label' : "Right-Bottom",
        hidden: true,
        color: "#F00"
      });

  // All other nodes will have X and Y beetween -1 .. 1
  var nextModNodeId = 3;
  var firstModNodeId = 3;

  // Add large nodes (modules).
  si.iterNodes(function(node) {
//  if (node.size < MIN_IMPORTANT_SIZE)
//    return; // too small
    if (node.attr.node_type != "module")
      return;

    var countX = 10;
    var countY = 20;
    var num = nextModNodeId - firstModNodeId;
    var numX = parseInt(num / countY); // 0 .. 10
    var numY = num % countY;           // 0 .. 10
    msi.addNode(nextModNodeId,{
      'x':  numX,
      'y': numY,
      'size': node.size,
      'hidden': true,
      'label': node.label,
      'realNodeId': node.id,
      'color': node.color
    });
    nextModNodeId++;
  });

  // Hide module-function edges.
  si.iterEdges(function(edge) {
    if (edge.attr.edge_type == "mf") edge.hidden = true;
  });

  msi.draw();

  // By default, the events on the main node are not passed down.
  // Add for all nodes on modules' canvas a special block above the main canvas.
  msi.iterNodes(function(node) {
    var nid = node.id;
    var realNid = node.attr["realNodeId"];
    // Skip border nodes
    if (nid < firstModNodeId)
      return;
    var o = addOverlayNode(node);
    o.mouseenter(function(){ 
        $("body").addClass("active-hover-modules");
        msi.pickNode(nid); 
        si.pickNode(realNid); 
    });
    o.mouseleave(function(){ 
        $("body").removeClass("active-hover-modules");
        msi.unpickNode(nid); 
        si.unpickNode(realNid); 
    });

    o.click(function() {
        si.zoomToNode(realNid, 5);
    });
  });

  // Add a handler for "Modules" label.
  $("#graph-modules-link a").click(function(e) { 
      $("body").toggleClass("active-modules");
  });

  $("#node-tip-switch a").click(function(e) { 
      $("body").toggleClass("active-tip-switch");
  });

  $("#ff-edge-switch a").click(function(e) { 
      var is_disabled = $(this).parent().hasClass("active-block");
      si.iterEdges(function(e) {
        if (!e.attr.edge_type) // ff
          e.hidden = is_disabled;
      });
      si.draw(2, 1);
  });

  $("#mm-edge-switch a").click(function(e) { 
      var is_disabled = $(this).parent().hasClass("active-block");
      si.iterEdges(function(e) {
        if (e.attr.edge_type == "mm")
          e.hidden = is_disabled;
      });
      si.draw(2, 1);
  });

  $("#graph-main canvas").each(function() {
      this.addEventListener('dblclick', resetScale);
  });


  $(".header-closed").click(function(e) { 
      var t = $(e.target);
      var p = t.parent().addClass("active-block"); 
      $("a:visible:first", p).focus();
      return false;
  });

  $(".header-opened").click(function(e) { 
      var t = $(e.target);
      var p = t.parent().removeClass("active-block"); 
      $("a:visible:first", p).focus();
      return false;
   });
  
  var tryToSearch = function(e) {
    var needle = e.target.value;
    var sidebar = $("#search-results");

    // skip, if it is too short
    if (needle.length < 4) {
      closeSearchSidebar(true);
      return;
    }

    var matched_module_node_ids = [];
    var matched_function_node_ids = [];
    si.iterNodes(function(n) {
      // Is the needle a substring of the label?
      // indexOf returns -1, if not found.
      // ~-1 is 0.
      if (~n.label.indexOf(needle))
      {
        switch (n.attr.node_type)
        {
          case "module":
            matched_module_node_ids.push(n.id);
            break;
          default:
            matched_function_node_ids.push(n.id);
        }
      }
    });


    // Nothing was found
    if (!matched_module_node_ids.length && !matched_function_node_ids.length) {
        closeSearchSidebar(true);
        return;
    }

    if (matched_module_node_ids.length > 0)
    {
        var ul_mods = nodeIdsToHtml(si, matched_module_node_ids);
        $("#modules-list", sidebar).empty().append(ul_mods);
        $("#modules", sidebar).show();
    }
    else
    {
        $("#modules", sidebar).hide();
    }

    if (matched_function_node_ids.length > 0)
    {
        var ul_funs = nodeIdsToHtml(si, matched_function_node_ids);
        $("#functions-list", sidebar).empty().append(ul_funs);
        $("#functions", sidebar).show();
    }
    else
    {
        $("#functions", sidebar).hide();
    }

    $("body").addClass("searching-active"); 

    activateAutoResizeMonitor($("#search-results"));
  }; // end of tryToSearch

  $("#search-field").keydown(function(e) {
    e.stopPropagation();
    switch (e.keyCode)
    {
      case keyCodes.ESCAPE:
      closeSearchSidebar();
    }
  });
  $("#search-field").keyup(tryToSearch);
  $("#search-results, #graph-directions").jScrollPane({trackClickRepeatFreq: 20});

    
  var activateAutoResizeMonitor = function(area) {
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



  var module_ids = [];
  // Add nodes index for n and N keys
  si.iterNodes(function(node) {
//    activateNode({target: this, context: [node.id]});
      switch (node.attr.node_type)
      {
        case "module":
          module_ids.push(node.id);
      }
     node.active = false;
  });

  // Set modules as current nodes
  current_node_id = module_ids[0];
  current_node_ids = module_ids;


  // Move from id on `offset` nodes forward using ids as an index,
  function nextNode(offset, id, ids)
  {
    var currect_index = ids.indexOf(id);
    // circled
    var new_index = (currect_index + offset) % ids.length;
    return ids[new_index];
  }

  $("#edge-direction-selector a").click(function(e) {
    if ($("body").hasClass("source-color"))
    {
        // Set the edge color of the target.
        $("body").removeClass("source-color");
        $("body").addClass("target-color");
        si.setTargetEdgeColor();
    } else {
        // Set the edge color of the source.
        $("body").removeClass("target-color");
        $("body").addClass("source-color");
        si.setSourceEdgeColor();
    }

    var t = $(e.target),
        p = t.parent().removeClass("active-block"); 
    $("a:visible:first", p).focus();

    // Ignore nodes and labels (-1). Draw edges.
    si.draw(-1, 1, -1);
  });

  /* FIXME: It is hack (a hot fix of a bug). 
     It forces to draw hovered labels, when initial loading is done.
     Otherwise, hovered labels don't work as expercted in Firefox.
   */
  si._core.plotter.drawHoverNode(si._core.graph.nodes[0]);

  // Draw the graph :
  // - directly redraw labels (2)
  si.draw(-1, 1, 2);
}
