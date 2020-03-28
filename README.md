# Hello, GUI

The two goals behind the project are:

1. To prove that we can build a high-performance massively parallel graphical
   user system
2. To provide a reflective, self-hosted environment in which to build Erlang
   systems in the spirit of Smalltalk

## Architecture

Currently the main project is split into 2 layers, from the top down:

1. **Chalk**, a high-level graphics pipeline that leverages Erlang's SMP.

2. A set of NIFs and Ports written in Rust to interact with native libraries
   that provide hardware accelerated graphics.

On top of Chalk, we can build libraries for:

1. Writing any kind of GUI applications

2. Writing games, interactive media, and digital art

On top of which we can build an interactive environment for building Erlang
systems, that runs within Erlang, in Erlang, and allows you to visualize, edit,
and modify the code of the running system as it runs.

## Motivation

As part of the StageVM project, Hello GUI aims to explore the required
abstractions to efficiently build and run GUI applications that continue to
enjoy [The Free Lunch](the free lunch is over link).

In addition, the process will help evaluate the difficulties in extending an
actor runtime system to access native resources with minimum overhead.
