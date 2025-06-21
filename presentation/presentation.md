---
marp: true
theme: uncover
---

# Bunny
<bunny logo here>

---

is an

- immutable
- deterministic
- functional
- interpreted
- lazy
- statically-typed
- strongly-typed

language for creating graphs.

---

# Show me the code.

---
# Yes, it's a LISP.

_(Without homoiconicity)_

```lisp
(def fib (n) 
    (foldl
        (\ (acc num) (
            (def l (len acc))
            (def f1 (get acc (- l 1)))
            (def f2 (get acc (- l 2)))
            (append acc (+ f1 f2))
        ))
        [0 1]
        (range 0 n)
    )
)

(fib 20)
```

---

# Quick Technical Rundown

---

# Lexer

---

# Parser

---

# Typechecker

Implements Hindley-Milner's _Algorithm J_

```lisp
; Can infer generics
(def arr [[1 2] [3 4] [5 6]]) ; array[array[int]]

; Does not require type annotations, even for complex types
<TODO EXAMPLE HERE>

; Supports let-polymorphism
(def id (x) x) 
(get (id [1 2 3]) (id 0)) ; => 1
```

---

# Runner

---

