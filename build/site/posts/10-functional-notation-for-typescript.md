---
title: "Designing a Functional Notation for TypeScript"
tags: ["2026", "FP", "TypeScript"]
date: Oct 27, 2026
---

I really enjoy building applications with **Effect**, **ts-pattern**, and **Mutative** in TypeScript (as in my [Same App, Different Languages](/posts/5-same-app-diffent-languages.html) experiment). Together they give TypeScript a real functional effect system, typed errors, dependency injection via `Context` and `Layer`, runtime-validated schemas (`Schema`), structural equality (`Data`), exhaustive pattern matching, and fast immutable updates.

The problem isn't what those libraries do — it's **how much syntax ceremony it takes to say it**.

What if we kept the exact same TypeScript type checker (`tsc`), the exact same npm ecosystem, and **Effect** as the standard library, but stripped away thirty years of JavaScript syntax baggage?

**FunScript** is a language idea and specification I've been designing: a small, **16-keyword**, indentation-based functional notation that compiles 1-to-1 to readable TypeScript for the Effect + ts-pattern + Mutative stack.

---

## The Itch: Intent vs. Ceremony

A small, idiomatic Effect program in TypeScript looks like this:

<pre>
<code class="language-js">
import { Context, Data, Effect, Layer, Schema } from "effect";
import { match } from "ts-pattern";

export const User = Schema.Struct({
  id: Schema.UUID,
  name: Schema.NonEmptyString,
});
export type User = typeof User.Type;

export class NotFound extends Data.TaggedError("NotFound")&lt;{
  readonly id: string;
}&gt; {}

export class UserRepo extends Context.Service&lt;UserRepo, {
  readonly find: (id: string) =&gt; Effect.Effect&lt;User, NotFound&gt;;
}&gt;()("UserRepo") {}

export const UserRepoLive = Layer.succeed(UserRepo, {
  find: (id) =&gt;
    id === "1"
      ? Effect.succeed({ id: "1" as Schema.UUID, name: "Ada" as Schema.NonEmptyString })
      : Effect.fail(new NotFound({ id })),
});

export const main = Effect.gen(function* () {
  const repo = yield* UserRepo;
  const user = yield* repo.find("1");
  return user.name;
}).pipe(Effect.provide(UserRepoLive));
</code>
</pre>

And here is the exact same program in **FunScript**:

<pre>
<code class="language-js">
import { Effect, Layer } from "effect"

export type User = {
  id: UUID
  name: NonEmptyString
}

export error NotFound(id: string)

export service UserRepo "UserRepo"
  find: (id: string) =&gt; Effect&lt;User, NotFound&gt;

export UserRepoLive = Layer.succeed(UserRepo, {
  find: (id) -&gt;
    match
      | id == "1" -&gt; Effect.succeed({ id: "1" as UUID, name: "Ada" as NonEmptyString })
      | _ -&gt; Effect.fail(NotFound(id))
})

export main =
  effect
    repo &lt;- UserRepo
    user &lt;- repo.find("1")
    user.name
  |&gt; Effect.provide(UserRepoLive)
</code>
</pre>

The goal is not to invent a new virtual machine, a custom type checker, or a parallel standard library:

- **Effect TS is the standard library.**
- **`tsc` is the type checker.**
- **Zero custom runtime.** The compiler emits clean, formatted `.ts` files that import `effect`, `ts-pattern`, and `mutative` directly. If you ever stop using it, you run `funscript emit`, keep the generated TypeScript, and walk away.

---

## Core Design Philosophy: One Way Per Job

JavaScript and TypeScript have accumulated decades of competing ways to express the same idea (`let`/`const`/`var`, `function`/arrows/methods, `if`/ternary/`switch`, `for`/`while`/`forEach`, `null`/`undefined`, `interface`/`type`/`class`/`enum`, `try`/`catch` vs. `Promise` vs. `Effect`).

This design takes the opposite stance — **one way per job**:

- **16 keywords total:** `and`, `as`, `effect`, `error`, `export`, `false`, `from`, `import`, `match`, `mut`, `not`, `or`, `service`, `true`, `type`, `when`.
- **No `if`, `then`, `else`, `switch`, or `? :`** — `match` handles both structural pattern matching and boolean branching.
- **No `return`** — every function, `match` arm, and `effect` block is an expression that evaluates to its final line.
- **No `null`, `undefined`, or `any`** — absence is `Option<A>`, and external boundaries use `unknown` + `Schema.decodeUnknown`.
- **No `class`, `new`, `this`, `interface`, `enum`, or `data`** — one `type` keyword handles schemas, tagged unions, and type aliases.
- **No `for` or `while` loops** — pure iteration uses Effect `Array`/`Record` pipelines; effectful iteration uses `Effect.forEach`.
- **No `async`/`await` or `try`/`catch`/`throw`** — `effect` blocks (`<-`) and typed `error` declarations are the only concurrency and error model.

---

## Language Tour & Syntax Specs

### 1. Unified `type`: Runtime `Schema` + Tagged Unions + TypeScript Types

In TypeScript, static types and runtime validators often drift apart, or require declaring both a `Schema` value and `export type X = typeof X.Type`. Here, a single `type` keyword handles all three jobs:

1. **Data records and scalars** compile to **both** an Effect `Schema` runtime validator and a static TypeScript `type`.
2. **Tagged unions (`| Variant(...)`)** compile to Effect's `Data.TaggedEnum`, positional constructors, and a companion `Schema.Union`.
3. **Function/Effect signatures** compile to zero-cost TypeScript `type` aliases.

<pre>
<code class="language-js">
# 1. Branded &amp; refined scalar Schema + static type
export type Username =
  string
  |&gt; Schema.trimmed
  |&gt; Schema.minLength(3)
  |&gt; Schema.brand("Username")

# 2. Struct Schema with optional (?) and default (=) fields
export type User = {
  id: UUID
  name: Username
  age: Int |&gt; Schema.between(0, 150)
  role: "admin" | "member" = "member"
  bio?: NonEmptyString
}

# 3. Tagged union (Data.TaggedEnum + constructors + Schema.Union)
export type Shape =
  | Circle(radius: number)
  | Rect(width: number, height: number)

# 4. Compile-time TypeScript alias (no runtime output)
export type Lookup = (id: UUID) =&gt; Effect&lt;User, NotFound&gt;
</code>
</pre>

Because `User` and `Shape` exist at both the type level and value level, you can annotate parameters with `: User` and decode untrusted JSON with `Schema.decodeUnknown(User)(raw)` from a single declaration.

---

### 2. Only `match`: Exhaustive Pattern Matching & Boolean Branching

There is no `if`/`else`, `switch`, or ternary `? :`. Instead, `match` has two forms:

#### Structural value matching (compiles to `ts-pattern` `.exhaustive()`)

<pre>
<code class="language-js">
export describe = (shape: Shape) -&gt;
  match shape
    | Circle(0) -&gt; "point"
    | Circle(r) when r &gt; 100 -&gt; "huge circle ({r})"
    | Circle(r) -&gt; "circle ({r})"
    | Rect(w, h) -&gt; "{w} by {h}"

# Or-patterns (`| A | B -&gt;`) and pipeline `|&gt; match`
export findName = (users: Array&lt;User&gt;, id: string) -&gt;
  users
  |&gt; Array.findFirst((u) -&gt; u.id == id)
  |&gt; match
    | Some({ name }) -&gt; name
    | None -&gt; "anonymous"
</code>
</pre>

#### Subjectless `match` for boolean conditions (compiles to native JS `? :` or `if / else`)

When `match` has no scrutinee expression, each arm is a boolean condition that compiles directly to native JavaScript `? :` or `if / else if / else` with zero `ts-pattern` runtime overhead:

<pre>
<code class="language-js">
# Inline two-branch expression
sign = match | n &gt; 0 -&gt; "positive" | _ -&gt; "non-positive"

# Multi-branch block
tier =
  match
    | score &gt;= 90 and verified -&gt; "platinum"
    | score &gt;= 75 -&gt; "gold"
    | score &gt;= 50 -&gt; "silver"
    | _ -&gt; "bronze"
</code>
</pre>

When used as a value expression, a trailing `| _ ->` fallback is required so a conditional can never silently evaluate to `undefined`.

---

### 3. Pipelines (`|>`) with `_` Argument Placeholders

Data flows left-to-right using F#-style pipelines that compile to Effect's `pipe`:

- `x |> f` calls `f(x)`
- `x |> f(a)` calls curried data-last `f(a)(x)` (Effect's standard convention)
- `x |> f(a, _)` plugs `x` into the `_` slot of an ordinary uncurried function `f(a, x)`

<pre>
<code class="language-js">
score =
  rawInput
  |&gt; parseScore
  |&gt; clamp(0, 100, _)
  |&gt; formatScore(_, "en-US")
</code>
</pre>

Point-free composition (`>>`) is intentionally omitted because `flow(f, g)` degrades TypeScript's generic type inference; writing `(x) -> x |> f |> g` is nearly as short and preserves full contextual inference.

---

### 4. Immutable Copy-on-Write Updates: `{ ...spread }` and `mut` (Mutative)

All bindings (`x = 1`), records, and arrays are deeply `readonly` by default. To update data ergonomically without mutating shared references, declarative spreads are paired with imperative `mut` drafts powered by **Mutative** (`create`):

#### Declarative updates with `{ ...base, ... }` (90% of updates)

Shallow overrides emit a plain JS spread (`{ ...todo, completed: true }`). When you use a nested dot-path (`address.city: "Paris"`), the compiler automatically lowers it to Mutative's `create`:

<pre>
<code class="language-js">
# Shallow -&gt; `{ ...todo, completed: true }`
done = { ...todo, completed: true }

# Deep dot-paths -&gt; `create(user, draft =&gt; { ... })`
moved = {
  ...user
  age: user.age + 1
  address.city: "Paris"
  address.zip: "75001"
}
</code>
</pre>

#### Imperative copy-on-write with `mut` + `=` (10% of updates)

When you need step-by-step updates, array methods (`.push`, `.splice`, `.sort`), or conditional mutations, declare a local `mut` binding or pipe into `|> mut (draft) ->`:

<pre>
<code class="language-js">
recordLogin = (user: User) -&gt;
  mut draft = user
  draft.visits = draft.visits + 1
  draft.tags.push("active")
  match
    | draft.visits &gt; 10 -&gt; draft.tier = "gold"
  draft

# Or inline in a pipeline:
updated =
  user
  |&gt; mut (d) -&gt; d.tags.push("onboarded")
</code>
</pre>

Reassigning with `=` is only valid on a `mut` binding (never on an immutable binding), and `mut` cannot cross closure boundaries — shared or concurrent state across fibers always uses `Effect.Ref`.

---

### 5. `effect` Blocks, `error`, and `service`

`effect` blocks compile to `Effect.gen(function* () { ... })`, where `<-` is `yield*` and `_ <-` runs an effect for its side effects:

<pre>
<code class="language-js">
import { Effect, Schedule } from "effect"
import { FetchHttpClient, HttpClient, HttpClientResponse } from "effect/http"

export type Todo = {
  id: Int
  title: NonEmptyString
  completed: boolean
}

export fetchTodo = (id: number) -&gt;
  effect
    response &lt;- HttpClient.get("https://jsonplaceholder.typicode.com/todos/{id}")
    todo &lt;- response |&gt; HttpClientResponse.schemaBodyJson(Todo)
    { ...todo, completed: true }
  |&gt; Effect.retry(Schedule.exponential("100 millis"))
  |&gt; Effect.provide(FetchHttpClient.layer)
</code>
</pre>

A key guardrail in `effect` blocks: **a non-final bare expression is a syntax error**. In raw `Effect.gen`, writing `Console.log("hi")` without `yield*` silently creates an unused `Effect` and never runs it. In FunScript, every non-final line must bind (`=` or `<-`) or discard (`_ <- Console.log("hi")`).

---

### 6. Type-Driven Development with Typed Holes (`???`)

While sketching an implementation, you can drop `???` (or `??? "label"`) into any expression position:

<pre>
<code class="language-js">
export checkout = (user: User, cart: Cart): Effect&lt;Receipt, PaymentError&gt; -&gt;
  effect
    total = calculateTotal(cart)
    ??? "charge payment provider"
</code>
</pre>

The compiler lowers `???` to a `never`-typed throw and queries TypeScript's `checker.getContextualType(node)` and `checker.getSymbolsInScope(node)` to print the exact expected type and local bindings in scope:

<pre>
<code class="language-bash">
warning[funscript/typed-hole]: Unfinished hole `???` ("charge payment provider")
  --&gt; src/checkout.fun:4:5
  | Expected type: Effect&lt;Receipt, PaymentError, never&gt;
  | In scope:
  |   user  : User
  |   cart  : Cart
  |   total : number
</code>
</pre>

Typed holes emit warnings during `funscript dev` and `funscript test` (so you can run other branches while iterating) and are a hard error in `funscript build`.

---

## Summary Cheat Sheet

| Construct | Syntax | Compiles to |
| --- | --- | --- |
| Immutable binding | `x = 1` | `const x = 1;` |
| Local mutable / draft | `mut x = user` / `x.a = 1` | `let` / Mutative `create(user, ...)` |
| Shallow / deep copy | `{ ...u, a.b: 1 }` | `{ ...u, ... }` / Mutative `create(...)` |
| Function | `(a: number, b: number) -&gt; a + b` | `(a: number, b: number) =&gt; a + b` |
| Pipeline &amp; placeholder | `x \|&gt; f(a) \|&gt; g(1, _)` | `pipe(x, f(a), (__p) =&gt; g(1, __p))` |
| Pattern match | `match x \| A(v) -&gt; v` | `match(x).with({ _tag: "A" }, ...).exhaustive()` |
| Boolean match | `match \| x &gt; 0 -&gt; "pos" \| _ -&gt; "neg"` | `x &gt; 0 ? "pos" : "neg"` |
| Schema &amp; type | `type User = { id: UUID }` | `Schema.Struct(...)` + `typeof User.Type` |
| Tagged union | `type Shape = \| Circle(r: number)` | `Data.TaggedEnum` + `Schema.Union` |
| Typed error | `error NotFound(id: string)` | `class NotFound extends Data.TaggedError...` |
| Service | `service Repo ...` | `class Repo extends Context.Service...` |
| Effect block | `effect` + `x &lt;- eff` | `Effect.gen(function* () { const x = yield* eff; })` |
| Typed hole | `??? "todo"` | `never` throw + contextual type diagnostic |
