Relatio
=======

Web UI for Erlang Xref.
This program display interaction beetween modules from different applications.

**License**: MIT

**Author**: Uvarov Michael (arcusfelis@gmail.com)


Installation and building
-------------------------

.. code-block::

    git clone git://github.com/arcusfelis/relatio.git
    rebar get-deps compile
    cp relatio.config.example relatio.config

Edit ``relatio.config``.

This file contain a list of applications, that we want to analyse.
The format is: ``{application, AppName, DirName}``.
For example,  ``{application, stdlib, code:lib_dir(stdlib)}`` or
``{application, proper, "/home/user/erlang/proper"}``.

.. code-block::

    ./start-dev.sh
    relatio_db:up().

Open http://127.0.0.1:2080


The next commands are used to run this program after initial installation.

.. code-block::

    ./start-dev.sh

Open http://127.0.0.1:2080


Applications' graph (or world graph)
------------------------------------

This diagram shows connections beetween applications. Modules and applications
are represented as nodes and calls beetween them are edges.
Each application has a set of modules around it.
Only nodes with connections are shown.

Detalization
------------

Button ``Detalize...`` opens a window for building a module graph.
The idea of this operation is building a detail graph for a *part* of the
application graph. 

*Caution:* If you select too many nodes, than the result graph will be
useless and nodes will overlap.


Module graph (or detail graph)
------------------------------

Functions and modules are represented as nodes and calls beetween them are edges.
Module nodes from different applications have different border colors.
Only nodes with connections are shown.


Mouse Actions
-------------

There are actions applicable to canvas elements:

- Click on a node to open the sidebar and to select the node;
- Use a wheel to scale canvas.


There are actions applicable to sidebar elements:

- Click on a node link to focus on a node;
- Shift+Click on a node link to select the node and to activate information
  about the node on the sidebar;
- Click on a sidebar will close the sidebar.


Keyboard Shortcuts
------------------

Keys h, j, k, l are used for navigation as in Vim:

- ``h`` - left;
- ``j`` - up;
- ``k`` - down;
- ``l`` - right;

- ``n`` - select the next module or function node;
- ``N`` - select the previous module or function node.

Canvas scaling:

- ``+`` - zoom in;
- ``-`` - zoom out;
- ``0`` or ``Esc`` - reset scaling;
- ``)`` or ``Shift+0`` - scale to the visible nodes.

Typing a number before a motion repeats it that many times.

- ``10h`` - repeat the "move to left" command ten times;
- ``10+`` - repeat zooming ten times.


Widgets:

- ``Esc`` - close sidebars (or just click on them);
- ``/`` - focus on the search field.


Windows:

- ``ctrl+w`` in vim, *but* ``w`` here. This key combination is reserved by few browsers.
- ``w`` - switch beetween windows (``ctrl+w ctrl+w`` in vim)


Search field:

- ``Esc`` - stop searching, close the search side bar;
- ``Enter`` - move to the founded element list.

Sidebars:

- ``h`` - previous sidebar;
- ``j`` - up;
- ``k`` - down;
- ``l`` and ``Shift+Enter`` - open the focused element.


Noise
-----

If some node has too many edges, the noise filter hides these edges.

