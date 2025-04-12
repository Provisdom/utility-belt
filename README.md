# Utility Belt

A collection of small, focused utility functions for Clojure/ClojureScript development. This library provides a variety of helper functions that make common Clojure development tasks easier.

## Features

* **Map utilities** - Functions for working with maps, sorted maps, priority maps, and monotonic maps
* **String manipulation** - String handling utilities that complement clojure.string
* **Nil handling** - Various strategies for dealing with nil values
* **Anomalies** - Error handling compatible with Cognitect's anomalies pattern
* **Sorted collections** - Utilities for working with sorted sequences and sets
* **Async helpers** - Extensions for core.async
* **Debugging tools** - Helpers for debugging and development
* **Spec utilities** - Generators and helpers for working with clojure.spec

## Installation

Add the following dependency to your project:

### deps.edn

```clojure
provisdom/utility-belt {:mvn/version "0.1.0"} ;; Replace with the actual version
```

## Usage Examples

### Maps

```clojure
(require '[provisdom.utility-belt.maps :as maps])

;; Check if a map is a priority map
(maps/priority-map? my-map)

;; Create a priority map with a custom key function and comparator
(maps/priority-map identity < 1 2 3 4)

;; Filter a map based on key-value predicate
(maps/filter-map (fn [k v] (> v 10)) {:a 5 :b 15 :c 20})
;; => {:b 15, :c 20}
```

### Nil Handling

```clojure
(require '[provisdom.utility-belt.nils :as nils])

;; Apply a function ignoring nil arguments
(nils/ignore-nils + 1 nil 2 nil 3)
;; => 6

;; Replace nil values in a collection
(nils/replace-nils [1 nil 3 nil 5] [:a :b])
;; => [1 :a 3 :b 5]
```

### Strings

```clojure
(require '[provisdom.utility-belt.strings :as strings])

;; Abbreviate a long string with ellipses
(strings/abbreviate "This is a very long string that needs to be truncated" 20)
;; => "This is a very l..."

;; Insert a substring at a specific index
(strings/insert "Hello world" 5 ", beautiful")
;; => "Hello, beautiful world"
```

## Contributing

Pull requests and issues are welcome. Please include tests for new functionality.

## License

Copyright © 2016-2025 Provisdom

Distributed under the GNU Lesser General Public License version 3.0.
