# Directed graph for UI components
[![Build Status](https://travis-ci.org/flarebyte/bubblegum-graph.svg?branch=master)](https://travis-ci.org/flarebyte/bubblegum-graph)

> This library provides a directed acyclic graph for representing relationships between UI components.

This is an **experimental** implementation which attempts to expose a graph as a list of paths, similar in concept to xpath.
In other words, the graph is converted to a flat list to make processing easier.

This library has been implemented with the idea of of been used for the Bubblegum UI library. However, it should be possible to use it for other purposes.

If you are looking for a general purpose Graph library, I would recommend [the elm-community/graph one](https://github.com/elm-community/graph) instead.

Limits:
 * No more than 1000 nodes or edges.
 * Only one edge with the same source and destination.
 * The graph should not have any cycles (loops).

## Getting Started

### Installing Elm packages

There is no dependency.

```
elm-app package install flarebyte/bubblegum-graph
```


## Contributing

Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

Managed automatically by [Elm version rules](https://github.com/elm-lang/elm-package#version-rules).

## Authors

* **Olivier Huin** - *Initial work* - [olih](https://github.com/olih)

See also the list of [contributors](https://github.com/flarebyte/bubblegum-graph/graphs/contributors) who participated in this project.

## License

This project is licensed under the BSD 3-Clause License - see the [LICENSE.md](LICENSE) file for details
