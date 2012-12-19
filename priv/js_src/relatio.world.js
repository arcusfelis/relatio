relatio.initWorld = function() {
  var keyCodes = relatio.keyBoard.keyCodes,
      charCodes = relatio.keyBoard.charCodes;

  var self = this, u = this.utils;
  var MIN_IMPORTANT_SIZE = 9;

  var wm = new WindowManager();
  var mainWindow = new Window(false); // visible
  wm.register(mainWindow);

  var searchingWindow = new Window(true);
  wm.register(searchingWindow);

  var dirWindow = new Window(true);
  wm.register(dirWindow);


  dirWindow.activationHandler = function() { 
      $("body").addClass("directions-selected"); 
  }

  dirWindow.deactivationHandler = function() { 
      $("body").removeClass("directions-selected"); 
  }

  mainWindow.activationHandler = function() { 
      $("body").addClass("main-selected"); 
  }

  mainWindow.deactivationHandler = function() { 
      $("body").removeClass("main-selected"); 
  }

  searchingWindow.activationHandler = function() { 
      $("body").addClass("searching-selected"); 
  }

  searchingWindow.deactivationHandler = function() { 
      $("body").removeClass("searching-selected"); 
  }

  dirWindow.pane = $("#graph-directions");
  searchingWindow.pane = $("#search-pane");


  var current_node_id, current_node_ids, current_hovered_node;

  var active_node_history = new Ring(10);



  // Instanciate sigma.js and customize rendering :
  var si = sigma.init(document.getElementById('graph-main')).drawingProperties({
    defaultLabelColor: '#fff',
    defaultLabelSize: 14,
    defaultLabelBGColor: '#fff',
    defaultLabelHoverColor: '#000',
    labelThreshold: 7,
    defaultEdgeType: 'curve',
    font: 'DejaVu Sans Mono, Arial'
  }).graphProperties({
    minNodeSize: 1,
    maxNodeSize: 5,
    minEdgeSize: 1,
    maxEdgeSize: 3
  }).mouseProperties({
    maxRatio: 16,
    minRatio: 0.8,
    zoomDelta: 0.05
  });

  si.addNode('zero', { 'x': 0, 'y': 0, 'hidden': true, 'alwaysHidden': true });
  si.addNode('one',  { 'x': 1, 'y': 1, 'hidden': true, 'alwaysHidden': true });

  // Parse a GEXF encoded file to fill the graph
  // (requires "sigma.parseGexf.js" to be included)
  si.parseGexf('data/world.gexf');
  si.iterEdges(function(e) {
    if (e.weight)
      e.size = e.weight;
  });

  si.draw(2, 0, 0);
  si.collectNoiseStatistics();
  si.noiseFilter(true);

  var tip = new Tip(si);

  // elem is a DOM element.
  // node is a sigma node.
  function nodeToHtmlLink(si, node, elem, add_cb)
  {
    var nid = node.id;
    var a = $("<a tabindex='0'>").addClass("node-" + nid)
                                 .addClass("node-link").text(node.label);

    // Add a class to element (not to "<a>")
    if (node.attr.is_exported)
        elem.addClass("is-exported");
    else
        elem.removeClass("is-exported");

    a.off(".show_node_label");
    a.on("click.show_node_label", function(e) {
      si.unpickNode(nid);
      tip.hide({"content": [nid]});

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
        current_hovered_node = undefined;
        si.unpickNode(nid);
        tip.hide({"content": [nid]});
    });
    a.on("mouseenter.show_node_label", function(){ 
        current_hovered_node = nid;
        si.pickNode(nid);
        tip.show({"content": [nid]});
    });
    if (add_cb)
    {
        var cb = $("<span class='checkbox'></span>");
        elem.append(cb);
    }
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
      var li = nodeToHtmlLink(si, node, elem, true);
      ul.append(li);
    }, nodes);

    var cbs = $(".checkbox", ul);
    // last1 and last2 are neaded if we press on the same checkbox more than
    // once.
    var last1 = 0; // older
    var last2 = 0; // newer

    cbs.click(function(e) {
      // This fucuses are neaded to make chromium happy!
      // This browser tries to select the next.
      $(cbs[0]).next().focus();
      $(this).next().focus();
      var cur = cbs.index(this);
      if (last2 != cur) {
        last1 = last2;
        last2 = cur;
      }
      if (e.shiftKey) {
        var min = Math.min(last1, cur);
        var max = Math.max(last1, cur);
        var sel = cbs.slice(min, max+1);
        if ($(this).hasClass("selected"))
          sel.removeClass("selected");
        else
          sel.addClass("selected");
      }
      else
      {
        $(this).toggleClass("selected");
      }
      return false;
    });

    cbs.on("mouseover", function(e) {
      var cur = cbs.index(this);
      var last = last2 == cur ? last1 : last2;
      var min = Math.min(last, cur);
      var max = Math.max(last, cur);
      var sel = cbs.slice(min, max+1);
      cbs.removeClass("will-be-selected");
      sel.addClass("will-be-selected");
    });

    cbs.on("mouseout", function(e) {
        cbs.removeClass("will-be-selected");
    });

    return ul;
  }

  var applicationIds2modulesIds = function(si, app_ids) {
    var mod_node_ids = [];
    si.iterEdges(function(e) {
        if (e.attr.edge_type == "am" && ~app_ids.indexOf(e.target)) {
            mod_node_ids.push(e.source);
        }
    });
    return mod_node_ids;
  }

  var moduleIds2moduleIds = function(si, ids) {
    var module_node_ids = [];
    si.iterEdges(function(e) {
        if (!e.attr.edge_type) // call edge
        {
          if (~ids.indexOf(e.target))
            module_node_ids.push(e.source);
          else if (~ids.indexOf(e.source))
            module_node_ids.push(e.target);
        }
    });
    return module_node_ids;
  }

  var moduleIds2applicationIds = function(si, module_ids) {
    var app_ids = [];
    si.iterEdges(function(e) {
        if (e.attr.edge_type == "am" && ~module_ids.indexOf(e.source)) 
          app_ids.push(e.target);
    });
    return app_ids;
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

    if (node.id == current_node_id && isDirectionSidebarOpen())
        return;

    active_node_history.check(node_id);

    $(".selected-elem-info").hide();
    var active_header_block;
    var focused_elem;

    // Change the header
    switch (node.attr.node_type)
    {
      case "module":
        var parent_node = si.module2applicationName(node);
        $("#selected-module-elem-info").show();
        var m_elem = $(".module-name", active_header_block).empty();
        var a_elem = $(".application-name", active_header_block).empty();
        nodeToHtmlLink(si, node, m_elem);
        nodeToHtmlLink(si, parent_node, a_elem);
        focused_elem = $("a", m_elem);

        // Change a current set of nodes
        current_node_id = node.id;
        // Get brothers of the function node.
        current_node_ids = applicationIds2modulesIds(si, [parent_node.id]);
        break;

      case "app":
        var activeHeaderBlock = $("#selected-application-elem-info").show();
        var a_elem = $(".application-name", active_header_block).empty();
        nodeToHtmlLink(si, node, a_elem);
        focused_elem = $("a", a_elem);

        // Change a current set of nodes
        current_node_id = node.id;
        current_node_ids = application_ids;
    }


    switch (node.attr.node_type)
    {
      case "app":
        si.iterEdges(function(e) {
            switch (e.attr.edge_type)
            {
            case "am":
              if (~ids.indexOf(e.target)) {
                  module_node_ids.push(e.source);
              }
              break;
            case "aa":
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
        var application_node_ids = ids.concat(donor_node_ids)
                                      .concat(rcpnt_node_ids);
        visible_node_ids = module_node_ids.concat(application_node_ids)
            .concat(moduleIds2moduleIds(si, module_node_ids));
        break;


      case "module":
        si.iterEdges(function(e) {
          if (~ids.indexOf(e.source) && e.attr.edge_type != "am") {
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
        var module_ids = ids.concat(donor_node_ids).concat(rcpnt_node_ids);
        visible_node_ids = module_ids
                         .concat(moduleIds2applicationIds(si, module_ids));
    }

    // Pane root element
    var pane = $("#graph-directions");

    si.hideAllNodes();
    si.showNodes(visible_node_ids);
    si.draw(2, 1);

    if (donor_node_ids.length > 0)
    {
      var ul_out = nodeIdsToHtml(si, donor_node_ids);
      $("#directions-list-out", pane).empty().append(ul_out);
      $(".direction-out-count", pane).text(donor_node_ids.length);
      $("#directions-out", pane).show();
    }
    else
    {
      $("#directions-out", pane).hide();
    }

    if (rcpnt_node_ids.length > 0)
    {
      var ul_in  = nodeIdsToHtml(si, rcpnt_node_ids);
      $("#directions-list-in", pane).empty().append(ul_in);
      $(".direction-in-count", pane).text(rcpnt_node_ids.length);
      $("#directions-in", pane).show();
    }
    else
    {
      $("#directions-in", pane).hide();
    }

    if (module_node_ids.length > 0)
    {
      var ul_in  = nodeIdsToHtml(si, module_node_ids);
      $("#module-list", pane).empty().append(ul_in);
      $(".module-count", pane).text(module_node_ids.length);
      $("#modules", pane).show();
    }
    else
    {
      $("#modules", pane).hide();
    }
    openDirectionSidebar();

    // Set focus
    focused_elem.focus();
  }; // end of activateNode


  // Activate the focused on the pane node.
  var activateFocusedNode = function() {
    // Emulate `shift+CLICK`.
    var ee = $.Event("click");
    ee.shiftKey = true;
    // Classes: .node-link or .header-closed or .header-opened
    $("a:focus").trigger(ee);
  }

  // The node was clicked.
  si.bind('downnodes', activateNode);

  si.bind('overnodes', tip.show);
  si.bind('outnodes', tip.hide);
  si._core.mousecaptor.bind('startdrag', tip.hide);


  /* Show info popup again, when the graphic stops moving. */
  var stopInterpolate = function(e) {
    if (!current_hovered_node)
        return;

    si.pickNode(current_hovered_node);
    tip.show({"content": [current_hovered_node]});
  }
  si._core.mousecaptor.bind('stopinterpolate', stopInterpolate);



  var openDirectionSidebar = function() {
    if (isDirectionSidebarOpen())
        return;

    wm.activate(dirWindow);
    $("body").addClass("directions-active"); 
    $("#graph-directions").activateAutoResizeMonitor();
  };

  var closeDirectionSidebar = function(saveHistory) {
    wm.deactivate(dirWindow);
    si.showAllNodes();
    si.draw(2, 1);
    $("body").removeClass("directions-active"); 
    
    // "reanimate" the searching field
    var searchFieldSel = $(".searching-active #search-field").focus();
    var isSearchActive = searchFieldSel.length;
    if (isSearchActive)
      wm.activate(searchingWindow);

    if (!saveHistory)
        active_node_history.reset();

    current_node_id = undefined;
  }


  var openSearchSidebar = function() {
    if (isSearchingSidebarOpen())
        return;

    wm.activate(searchingWindow);
    $("body").addClass("searching-active"); 
    $("#search-results").activateAutoResizeMonitor();
  };

  var closeSearchSidebar = function(save_socus) {
    wm.deactivate(searchingWindow);
    if (!save_socus)
      $("#graph-modules-link a:visible").focus();
    $("body").removeClass("searching-active"); 
  };


  var isDirectionSidebarOpen = function() {
    return $("body").hasClass("directions-active");
  };

  var isSearchingSidebarOpen = function() {
    return $("body").hasClass("searching-active");
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



  si.calculateCanvasWidth = function() {
    return si._core.width - calculateRightPanelSize();
  }

  // Handler for closing the pane
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
        si.optimalScale(calculateRightPanelSize()); 
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
        var next_node_id = u.nextNode(offset, current_node_id, current_node_ids);
        activateNode({'target': si, 'content': [next_node_id]});
        repeatCount = 0;
        break;

      case charCodes.m:
        // put a mark
        //
        // The next char will be handled by the special key handler.
        repeatCount = 0;
        nextKeyHandler = function(e) {
          nextKeyHandler = undefined;

          marks[e.charCode] = si.saveCurrentPosition();
        }
        break;

      case charCodes.GRAVE:
        // go to the mark
        repeatCount = 0;
        nextKeyHandler = function(e) {
          nextKeyHandler = undefined;

          var pos = marks[e.charCode];
          if (pos) si.setPosition(pos);
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

      default:
        return "default";
    }
  } // End of the `canvasKeyPressHandler` function.


  var panelKeyPressHandler = function(e) {
    var m = si._core.mousecaptor;
    var s = si._core;
    var kc = e.keyCode;
    var cc = e.charCode;

    switch (kc) {
      case keyCodes.ESCAPE:
        if (isDirectionSidebarOpen())
          closeDirectionSidebar();
        else if (isSearchingSidebarOpen())
          closeSearchSidebar();
        break;

      
      case keyCodes.ENTER:
        if ($(":focus").attr("id") == "search-field")
        {
          $("#search-pane a:visible:first").focus();
          return false;
        }
        break;
    }
    if ($("input:focus").length)
      return true;

    
    switch (cc) {
      case charCodes.h:
      // the left key, the prev pane
      case charCodes.l:
      // the right key, the next pane
      
        var goFocusedAndStop = false;
        repeatCount = repeatCount || 1;
        switch (cc) {
          case charCodes.h:
            // change a sign.
            repeatCount *= -1;
            break;

          // Check, that a cursor is on the link to the next node.
          case charCodes.l:
            var nextNID = active_node_history.next();
            if (!nextNID) {
              goFocusedAndStop = true;
              break;
            }

            var isNextNodeLinkFocused = $(":focus").hasClass("node-" + nextNID);
            if (!isNextNodeLinkFocused) {
              goFocusedAndStop = true;
              break;
            }
        }

        if (goFocusedAndStop) {
          activateFocusedNode();
          break;
        }

        var oldNID = active_node_history.current();
        var curNID = active_node_history.go(repeatCount);
        repeatCount = 0;

        // Warning: currentN can be -1, than we will do "nothing", if searcing
        // is not active.
        if (oldNID == curNID) break;

        if (!curNID)
        {
            // cur is before the first element.

            // Here is black magic.
            var searchingActive = $("body").hasClass("searching-active");
            var dirActive = $("body").hasClass("directions-active");

            if (searchingActive)
            {
                // Activate searching.
                if (dirActive)
                    closeDirectionSidebar(true);

                openSearchSidebar();

                var nextNID = active_node_history.next();
                $(".node-" + nextNID).focus();
                break; // exit from `switch`.
            } else {
                // Show the first element in the list.
                curNID = active_node_history.go(1);
            }
        }

        var nextNID = active_node_history.next();

        if (oldNID == curNID) break;

        // The call of the function `check` from `activateNode` will move the
        // cursor forward.
        active_node_history.go(-1);

        activateNode({'target': si, 'content': [curNID]});

        if (nextNID)
          $(".node-" + nextNID).focus();

        break;

      // These two cases emulate TAB pressing, but only visible `a` tags can be
      // passed.
      case charCodes.k:
        // up
        repeatCount = repeatCount || 1;
        repeatCount *= -1;

      case charCodes.j:
        // down
        repeatCount = repeatCount || 1;

        var cur = $(":focus"),
            set = $(".pane a:visible"),
            pos = 0,
            len = set.length;

        for (var i = 0; i < len; i++)
          if (set[i] == cur[0]) {
            pos = i;
            break;
          }

        pos += repeatCount;
        pos = Math.max(0, pos);
        pos = Math.min(pos, len - 1);

        $(set[pos]).focus();
        this.pane.updateScrolling();
        repeatCount = 0;
        break;


      default:
        return "default";
    }
  } // End of the `canvasKeyPressHandler` function.

  searchingWindow.keyHandler = panelKeyPressHandler;
  dirWindow.keyHandler = panelKeyPressHandler;
  mainWindow.keyHandler = canvasKeyPressHandler;



  $(document).keypress(function(e) {

    var win = wm.current();

    var resultFromKeyHandler = win.keyHandler(e);

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
        // There are different behaviour for focused links without the `href`
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
      case charCodes.w:
        wm.next(repeatCount);
        repeatCount = 0;
        break;

      case charCodes.SLASH:
        $("#search-field").select().focus();
        // TODO: select old text.
        // Don't let this char be entrered in the search field.
        return false;
        break;

      default:
        if (cc >= charCodes.ZERO && cc <= charCodes.NINE)
            repeatCount = repeatCount * 10 + cc - charCodes.ZERO;
      console.log("Key pressed: " + e.keyCode, " Char entered: " + e.charCode);
    }
  };
            


  // All other nodes will have X and Y beetween -1 .. 1
  var nextModNodeId = 3;
  var firstModNodeId = 3;

  // Hide module-function edges.
  si.iterEdges(function(edge) {
    if (edge.attr.edge_type == "am") edge.hidden = true;
  });


  $("#node-tip-switch a").click(function(e) { 
      $("body").toggleClass("active-tip-switch");
  });

  $("#noise-switch a").click(function(e) { 
      var is_disabled = $(this).parent().hasClass("active-block");
      si.noiseFilter(!is_disabled);
      si.draw(2, 1);
  });

  $("#aa-edge-switch a").click(function(e) { 
      var is_disabled = $(this).parent().hasClass("active-block");
      si.iterEdges(function(e) {
        if (e.attr.edge_type == "aa")
          e.hidden = is_disabled;
      });
      si.draw(2, 1);
  });

  $("#mm-edge-switch a").click(function(e) { 
      var is_disabled = $(this).parent().hasClass("active-block");
      si.iterEdges(function(e) {
        if (!e.attr.edge_type) // type == "mm"
          e.hidden = is_disabled;
      });
      si.draw(2, 1);
  });

  $("#graph-main canvas").on('dblclick', resetScale);


  $(".header-closed").click(function(e) { 
      var t = $(e.target);
      var p = t.parents(".sub-block:first").addClass("active-block"); 
      $("a:visible:first", p).focus();
      return false;
  });

  $(".header-opened").click(function(e) { 
      var t = $(e.target);
      var p = t.parents(".sub-block:first").removeClass("active-block"); 
      $("a:visible:first", p).focus();
      return false;
   });
  


  /**
   * This function matches nodes and forms a list of them.
   * It controls of visibility of the search panel.
   */
  var tryToSearch = function(e) {
    var needle = e.target.value;
    var pane = $("#search-results");

    // skip, if it is too short
    if (needle.length < 4) {
      closeSearchSidebar(true);
      return;
    }

    var matched_module_node_ids = [];
    var matched_app_node_ids = [];
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
          case "app":
            matched_app_node_ids.push(n.id);
        }
      }
    });


    // Nothing was found
    if (!matched_module_node_ids.length && !matched_app_node_ids.length) {
        closeSearchSidebar(true);
        return;
    }

    if (matched_module_node_ids.length > 0)
    {
        var ul_mods = nodeIdsToHtml(si, matched_module_node_ids);
        $("#module-list", pane).empty().append(ul_mods);
        $(".module-count", pane).text(matched_module_node_ids.length);
        $("#modules", pane).show();
    }
    else
    {
        $("#modules", pane).hide();
    }

    if (matched_app_node_ids.length > 0)
    {
        var ul_apps = nodeIdsToHtml(si, matched_app_node_ids);
        $("#application-list", pane).empty().append(ul_apps);
        $(".application-count", pane).text(matched_app_node_ids.length);
        $("#applications", pane).show();
    }
    else
    {
        $("#applications", pane).hide();
    }

    openSearchSidebar();

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




  var application_ids = [];
  // Add nodes index for n and N keys
  si.iterNodes(function(node) {
//    activateNode({target: this, context: [node.id]});
      switch (node.attr.node_type)
      {
        case "module":
          application_ids.push(node.id);
      }
     node.active = false;
  });

  // Set modules as current nodes
  current_node_id = application_ids[0];
  current_node_ids = application_ids;

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

  $(document).on("keydown keyup", function(e) {
    if (e.keyCode == keyCodes.SHIFT) {
      switch (e.type) {
        case "keydown":
          $("body").addClass("shift-pressed");
          break;
        case "keyup":
          $("body").removeClass("shift-pressed");
      }
    }
  });
}

