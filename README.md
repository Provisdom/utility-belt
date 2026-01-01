# utility-belt

A Clojure utility library providing foundational functions for the Provisdom ecosystem. Offers error handling as data (anomalies), extended collection operations, map utilities, string manipulation, debugging helpers, and spec extensions.

## Installation

Add to your `deps.edn`:

```clojure
provisdom/utility-belt {:git/url "https://github.com/Provisdom/utility-belt.git"
                        :git/sha "..."}
```

## Namespaces

### `provisdom.utility-belt.anomalies`
Error handling as data, based on Cognitect's anomaly approach. Treats errors as maps with categories rather than exceptions.

- `anomaly?` - Test if value is an anomaly
- `anomalous-let` - Let binding that short-circuits on anomalies
- `ex` / `ex!` / `ex?!` - Convert anomalies to/throw exceptions
- `anomaly-try` - Catch exceptions and convert to anomalies
- `chain` - Monadic chaining for anomaly-returning functions
- `recover` - Provide fallback values for anomalies

### `provisdom.utility-belt.arities`
Function introspection using reflection to analyze function signatures at runtime.

- `arities` - Returns vector of arity maps with `::parameters` (count) and `::variadic?` (boolean)
- `min-arity` - Minimum number of arguments the function accepts
- `max-fixed-arity` - Maximum fixed (non-variadic) arity, or nil if only variadic
- `variadic?` - Returns true if function accepts variable arguments
- `accepts-arity?` - Check if function can be called with n arguments

### `provisdom.utility-belt.async`
Parallel processing with `core.async`.

- `thread` - Execute functions in parallel with coordination modes and options:
  - Threading types: `:all`, `:and`, `:or`, `:first-success!!`, `:race!!`, `:any-ordered`
  - Options: `:timeout-ms` (max wait time), `:allow-nil?` (permit nil returns)
- `thread-select` - Parallel execution with custom result selection:
  - Options: `:timeout-ms`, `:min-successes` (short-circuit after N successes), `:progress-fn`, `:parallel?`, `:allow-nil?`
- `catch-error-or-exception` - Wrap function to convert exceptions to anomalies (allows nil)
- `catch-error-or-exception-or-nil` - Wrap function to convert exceptions and nil to anomalies

### `provisdom.utility-belt.debug`
Debugging utilities for REPL-driven development. All print-based macros respect `*debug-enabled*`.

- `*debug-enabled*` - Dynamic var to globally enable/disable print-based debug output
- `dbg` - Print and return expression value
- `dbg-pp` - Like `dbg` but uses pprint for nested data
- `dbg-when` - Conditional debug; only prints when predicate is true
- `dbg-fn` - Wrap function to trace its calls and returns
- `spy` - Print labeled value and return it
- `spy->` - Debug version of `->` that prints each step
- `spy->>` - Debug version of `->>` that prints each step
- `timed` - Print execution time and return value
- `tap-dbg` - Send value to `tap>` and return it
- `tap-spy` - Send labeled value to `tap>` and return it
- `tap-timed` - Send timing info to `tap>` and return value

### `provisdom.utility-belt.extensions`
Extended Clojure core functionality for collections and bindings.

- `if-all-let` / `when-all-let` - Multi-binding conditionals requiring all truthy
- `if-some-let` / `when-some-let` - Multi-binding conditionals requiring all non-nil (allows false)
- `update-in-with-not-found` - `update-in` with custom default for missing keys
- `interleave-all` - Interleave that consumes all elements from all collections
- `reduce-kv-ext` - Reduce with indices over multiple collections
- `deep-merge` - Recursive map merging
- `index-by` - Group collection by unique key into map
- `frequencies-by` - Count occurrences after applying function
- `partition-map` - Split collection by predicate into [matches, non-matches]
- `find-first` - Find first element matching predicate (eager, stops early)
- `dissoc-in` - Dissociate a key at a nested path, removing empty parents
- `assoc-some` - Like `assoc` but skips nil values
- `distinct-by` - Remove duplicates using a key function
- `index-of` - Find index of first element matching predicate
- `safe-nth` - Like `nth` but returns nil/default for out-of-bounds
- `keep-kv` - Like `keep-indexed` with (index, value) argument order
- `take-until` / `drop-until` - Like take-while/drop-while but inclusive of terminating element

### `provisdom.utility-belt.maps`
Utilities for priority maps, sorted maps, and map manipulation.

Note: For basic map-over-keys/values operations, use Clojure 1.11+ core functions:
- `update-vals` - map function over values
- `update-keys` - map function over keys

Functions provided:
- `priority-map` / `priority-map?` - Priority map creation and testing
- `sorted-map?` / `sorted-map-by?` - Sorted map predicates
- `sorted-map-monotonic?` - Test for monotonically increasing values
- `filter-map` / `remove-map` - Filter/remove map entries preserving map type
- `submap?` - Test if one map contains all entries of another
- `select-keys-by` / `remove-keys-by` - Select/remove keys matching a predicate
- `invert-map` - Swap keys and values
- `map-kv` - Transform both keys and values simultaneously
- `update-some` - Like `update` but only if key exists

### `provisdom.utility-belt.nils`
Nil-safe function composition and nil handling utilities.

- `ignore-nils` / `ignore-nils-fn` - Remove nil args before calling function
- `anomaly-nils` / `anomaly-nils-fn` - Return anomaly if any arg is nil
- `nil-nils` / `nil-nils-fn` - Return nil if any arg is nil
- `coalesce` - Return first non-nil value (nil-safe alternative to `or`)
- `default-nil` - Return value or default if nil (nil-safe alternative to `or`)
- `remove-nil-vals` - Remove entries with nil values from a map
- `replace-nil-vals` - Replace nil values in a map with a default
- `replace-nils` - Replace nils in sequence with values from another

### `provisdom.utility-belt.sorted-seq`
Specs, predicates, and utilities for point-in-time sorted sequences. All predicates use O(n) algorithms.

Generic predicates (work on any seqable):
- `sorted?` / `sorted-by?` - Test if sorted ascending (allows duplicates)
- `strictly-sorted?` / `strictly-sorted-by?` - Test if sorted ascending (no duplicates)
- `sorted-desc?` / `sorted-desc-by?` - Test if sorted descending (allows duplicates)
- `strictly-sorted-desc?` / `strictly-sorted-desc-by?` - Test if sorted descending (no duplicates)

Type-specific predicates:
- `seq-sorted?` / `seq-sorted-by?` - Test if sequential is sorted
- `list-sorted?` / `list-sorted-by?` - Test if list is sorted
- `vector-sorted?` / `vector-sorted-by?` - Test if vector is sorted

Spec macros:
- `list-sorted-of` / `list-sorted-by-of` - Specs for sorted lists
- `vector-sorted-of` / `vector-sorted-by-of` - Specs for sorted vectors

Binary search (O(log n) on indexed collections):
- `binary-search` / `binary-search-by` - Find index of element, or nil
- `binary-search-insertion-point` / `binary-search-insertion-point-by` - Find insertion index

Utilities:
- `insert-sorted` / `insert-sorted-by` - Insert into sorted vector maintaining order
- `merge-sorted` / `merge-sorted-by` - Merge two sorted sequences into sorted vector

### `provisdom.utility-belt.sorted-sets`
Specs, predicates, and utilities for self-sorting sets.

Predicates:
- `sorted-set?` / `sorted-set-by?` - Test if set is sorted

Spec macros:
- `sorted-set-of` / `sorted-set-by-of` - Create specs for sorted sets (accepts `:min-count`, `:max-count`, `:gen-max`, etc.)

NavigableSet-style operations (like Java's NavigableSet):
- `floor` - Greatest element <= value, or nil
- `ceiling` - Least element >= value, or nil
- `lower` - Greatest element < value, or nil
- `higher` - Least element > value, or nil
- `subset` - Elements in range [from, to)
- `subset-inclusive` - Elements in range [from, to]

### `provisdom.utility-belt.strings`
String manipulation utilities complementing `clojure.string`.

- `substring` - Safe substring with optional end index
- `trim-start` / `trim-end` - Recursively remove prefix/suffix
- `ensure-prefix` / `ensure-suffix` - Add prefix/suffix only if not already present
- `insert` - Insert substring at index
- `abbreviate` - Truncate with ellipsis
- `truncate-words` - Truncate at word boundary
- `truncate-middle` - Truncate keeping start and end (useful for paths)
- `blank->nil` - Convert blank strings to nil
- `normalize-whitespace` - Collapse multiple whitespace to single space
- `word-count` - Count words in a string
- `capitalize-words` - Capitalize first letter of each word
- `pad-left` / `pad-right` - Pad string to length with character
- `kebab->camel` / `camel->kebab` - Convert between kebab-case and camelCase
- `kebab->snake` / `snake->kebab` - Convert between kebab-case and snake_case

### `provisdom.utility-belt.spec-ext`
Extensions to `clojure.spec.alpha` for enhanced spec definitions.

Note: This namespace shadows `clojure.core/def`, so use `:refer-clojure :exclude [def]` or alias it.

Enhanced spec definition:
- `def` - Like `s/def` but supports metadata maps and docstrings
- `fdef` - Like `s/fdef` but supports `:throws` clause for declaring exceptions
- `get-meta` - Retrieve metadata map for a spec
- `specs-with-meta` - List all specs that have metadata
- `clear-meta!` - Remove metadata for a spec
- `throwing-fdefs` - Find all fdefs that declare `:throws`

```clojure
(require '[provisdom.utility-belt.spec-ext :as spec-ext])

;; def with metadata
(spec-ext/def ::my-spec {:doc "A positive integer"} pos-int?)

;; fdef with :throws - declares which exceptions the function may throw
(spec-ext/fdef my-fn
  :args (s/cat :x int?)
  :ret string?
  :throws [IllegalArgumentException ArithmeticException])

;; Use predicates for exception matching (e.g., anomaly exceptions)
(spec-ext/fdef risky-fn
  :args (s/cat :x any?)
  :ret map?
  :throws [anomalies/anomaly-ex?])

;; Query metadata
(spec-ext/get-meta ::my-spec)  ; => {:doc "A positive integer"}
(spec-ext/throwing-fdefs)       ; => {my-fn [IllegalArgumentException ...]}
```

The `:throws` clause integrates with `provisdom.test.core/is-spec-check` - during generative testing, exceptions matching the `:throws` spec are treated as valid behavior rather than test failures.

## License

Copyright © 2016-2026 Provisdom Corp.

Distributed under the GNU Lesser General Public License version 3.0.
