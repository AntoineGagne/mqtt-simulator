==============
mqtt-simulator
==============

.. image:: https://travis-ci.org/AntoineGagne/mqtt-simulator.svg?branch=master
    :target: https://travis-ci.org/AntoineGagne/mqtt-simulator

.. image:: https://ci.appveyor.com/api/projects/status/glyeekdu4vum33ht/branch/master?svg=true
    :target: https://ci.appveyor.com/api/projects/status/glyeekdu4vum33ht/branch/master

:Author: `Antoine Gagné <gagnantoine@gmail.com>`_

.. contents::
    :backlinks: none

.. sectnum::

Installation
============

Local Build
-----------

To build the runnable release, you need to have Erlang with OTP 21 and above.
You also need ``rebar3``. Then, you can run the following command:

.. code-block:: sh

    rebar3 as prod release

Docker Image
------------

To build this image, you can use the following command:

.. code-block:: sh

    docker build -f Dockerfile -t "${name_of_the_image}" .

Usage
=====

From Local Build
----------------

If you built the release, you can run it with:

.. code-block:: sh

    ./_build/prod/rel/mqtt_simulator/bin/mqtt_simulator foreground


Docker
------

After building the image, you can run the image by using the following command:

.. code-block:: sh

    docker run \
        --detach \
        --name "${name_of_the_running_container}" \
        --publish "${port_on_host}:${port_of_simulator:-8000}" \
        "${name_of_the_image}"

Development
===========

Running all the tests and linters
---------------------------------

You can run all the tests and linters with the ``rebar3`` alias:

.. code-block:: sh

    rebar3 check
