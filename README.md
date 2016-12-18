# epicea-optional

Adds *optional expressions* to Clojure. Using optional expressions and the control flow utilities that goes along with them, we can write robust functions that don't break so easily for different kinds of data.

Summary of three most important commands.
* Use ```(either expr*)``` to choose the first defined value from a set of optional expressions.
* Use ```(expect fun? expr)``` to define an optional expression ```expr``` whenever ```(fun? expr)``` is true.
* Use ```(optionally pred expr)``` to define an optional expression ```expr``` whenever ```pred``` is true.

## Usage

Example of BMI calculator:
```clojure
(defn compute-bmi-sub [weight height]
  (/ weight (* height height)))

(defn compute-bmi [person]
  (either (compute-bmi-sub (expect number? (:weight person))
                           (expect number? (:height person)))
          nil))

(compute-bmi {:weight 74 :height 1.73}) 
;; 24.725182932941294
(compute-bmi {:height 1.73})
;;nil
```
*See the unittests* for a rich set of examples of how this library can be used.
## Overview
* Use ```(either expr*)``` to choose the first defined value among a set of optional expressions.
* Use ```(expect fun? expr)``` to define an optional value if a function when applied to an expression returns true.
* Use ```(optionally pred expr)``` to define an optional value if another value is true.
* Use ```(defined? x)``` to check if the optional ```x``` is defined.
* Use ```(undefined? x)``` to check if the optional ```x``` is undefined.
* Use ```(but x)``` to make an optional that is defined to be true only if ```x``` is undefined.
* To return an optional value from a function, use ```(epicea.tag/wrap x)```. That value can then be imported again using ```(epicea.tag/unwrap x)```.

In the above listing, ```either``` and ```optionally``` are the fundamental macros on which all the others can be expressed.

## Discussion

Clojure functions frequently return nil as a result to signal the absence of a value. Checking the return value of every computation before it is being used in the next computation is a lot of work. This library aims at taking away some of that pain by providing control flow utilites and the concept of *optional expressions* that are only true if a condition holds. And consequently, any computation that depends on an optional expression becomes itself an optional expression. The ```either``` macro can then be used to choose the first defined value from a set of optional expressions.

The concept of optionals is also present in other languages, such as Swift, which also provides some control flow constructs to make programming with optionals easier, e.g. optional chaining. With this library, optionals can appear anywhere in an expression and is not limited to just a few special cases.

It may also make it easier to use only return values to communicate the result of a computation, where exceptions otherwise would be used. Because the exceptional flow resulting from exceptions can be very hard to follow, I try to avoid them and only use return values. But if we use return values to communicate different kinds of results, we need to consider those different outcomes and take action depending on the type of result. This library can help with that. Exceptions can still be useful though for indicating bugs, e.g. through assertions.

## License

Copyright © 2016 Jonas Östlund

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
