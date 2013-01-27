Relatio
=======

Web UI for Erlang Xref.

**License**: MIT

**Author**: Uvarov Michael (arcusfelis@gmail.com)


Installation and building
-------------------------

.. code-block::

    git clone git://github.com/arcusfelis/relatio.git
    rebar get-deps compile
    cp relatio.config.example relatio.config

Edit ``relatio.config``.

.. code-block::

    ./start-dev.sh
    relatio_db:up().

Open http://127.0.0.1:2080


The next commands are used to run this program after initial installation.

.. code-block::

    ./start-dev.sh

Open http://127.0.0.1:2080


Keyboard Shortcuts
------------------

Keys h, j, k, l are used for navigation as in Vim:

- ``h`` - left;
- ``j`` - up;
- ``k`` - down;
- ``l`` - right.

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

