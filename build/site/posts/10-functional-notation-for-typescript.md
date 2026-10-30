---
title: "FunScript: Reinventing Functional CoffeeScript"
tags: ["2026", "Programming"]
date: Oct 30, 2026
---

*Disclaimer: FunScript is currently an idea and design exploration — there is no compiler implementation yet. It is a thought experiment exploring what the ideal functional notation for the TypeScript and Effect ecosystem could look like.*

In 2010, **CoffeeScript** captured the developer world with a simple, seductive promise: *"It's just JavaScript."* By replacing curly braces, semicolons, and verbose function declarations with clean indentation and expression-oriented syntax, it proved that writing code for the web could be elegant, joyful, and concise. But CoffeeScript was untyped, dynamic, and preceded modern functional paradigms. When ES6 arrived and brought native arrow functions, destructuring, and classes, CoffeeScript's original job was done.

Fast forward fifteen years. In 2026, building applications with **Effect**, **ts-pattern**, and **Mutative** in TypeScript (as in my [Same App, Different Languages](/posts/5-same-app-diffent-languages.html) experiment) provides what functional programmers always dreamed of: pure typed effects, structured concurrency, typed errors, dependency injection, runtime schema validation, exhaustive matching, and copy-on-write immutability.

The problem isn't what those libraries do — it's **how much syntax ceremony it takes to say it**. To write idiomatic functional code today, we drown in `Effect.gen(function* () { const x = yield* ... })`, `match(x).with(...).exhaustive()`, duplicate `Schema.Struct` and `type` declarations, tagged error classes, and nested callbacks.

What if CoffeeScript were reborn today, not for imperative JavaScript, but for purely functional TypeScript?

**FunScript is that idea**: CoffeeScript's calm, indentation-based elegance combined with F#'s expression-oriented power, compiled 1-to-1 to readable TypeScript for the Effect + ts-pattern + Mutative stack. A small, **15-keyword**, zero-runtime notation that keeps the TypeScript type checker (`tsc`), uses Effect TS as its standard library, and makes writing correct programs the path of least resistance.

---

## The Itch: Intent vs. Ceremony

A small, idiomatic Effect program in TypeScript looks like this:

<pre>
<code class="language-js">
import { Context, Effect, Layer, Schema } from "effect";
import { match } from "ts-pattern";

export const User = Schema.Struct({
  id: Schema.UUID,
  name: Schema.NonEmptyString,
});
export type User = typeof User.Type;

export class NotFound extends Schema.TaggedError&lt;NotFound>()("NotFound", {
  id: Schema.String,
}) {}

export class UserRepo extends Context.Service&lt;UserRepo, {
  readonly find: (id: string) => Effect.Effect&lt;User, NotFound>;
}>()("UserRepo") {}

export const UserRepoLive = Layer.succeed(UserRepo, {
  find: (id) =>
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

export schema User = {
  id: UUID
  name: NonEmptyString
}

export error NotFound(id: string)

export service UserRepo
  find: (id: string) -> Effect&lt;User, NotFound>

export UserRepoLive = Layer.succeed(UserRepo, {
  find: (id) ->
    match
      | id == "1" -> Effect.succeed({ id: UUID("1"), name: NonEmptyString("Ada") })
      | _ -> Effect.fail(NotFound(id))
})

export main =
  effect
    user &lt;- UserRepo.find("1")
    user.name
  |> Effect.provide(UserRepoLive)
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

- **15 keywords total:** `and`, `effect`, `error`, `export`, `false`, `from`, `import`, `match`, `mut`, `not`, `or`, `schema`, `service`, `true`, `when`.
- **No `if`, `then`, `else`, `switch`, or `? :`** — `match` handles both structural pattern matching and boolean branching.
- **No `return`** — every function, `match` arm, and `effect` block is an expression that evaluates to its final line.
- **No `null`, `undefined`, or `any`** — absence is `Option<A>`, and untrusted boundaries use `raw |> Schema.decode(Type)` (there is no `as` keyword).
- **Effect TS data types as primitives** — `Int`, `Float`, `Decimal`, `DateTimeUtc` replace fragile `number` and `Date`; comparisons (`<, <=, >, >=`) are pure data-last functions (`Number.greaterThan`).
- **No `class`, `new`, `this`, `interface`, `enum`, `type`, or `data`** — one `schema` keyword handles schemas, tagged unions, and type aliases.
- **No `for` or `while` loops** — pure iteration uses Effect `Array`/`Record` pipelines; effectful iteration uses `Effect.forEach`.
- **No `async`/`await` or `try`/`catch`/`throw`** — `effect` blocks (`<-`) and typed `error` declarations are the only concurrency and error model.

---

## Language Tour & Syntax Specs

### 1. Unified `schema`: Runtime `Schema` + Tagged Unions + TypeScript Types

In TypeScript, static types and runtime validators often drift apart, or require declaring both a `Schema` value and `export type X = typeof X.Type`. Here, a single `schema` keyword handles all three jobs:

1. **Data records and scalars** compile to **both** an Effect `Schema` runtime validator and a static TypeScript `type`.
2. **Tagged unions (`| Variant(...)`)** compile to Effect's `Data.TaggedEnum`, positional constructors, and a companion `Schema.Union`.
3. **Function/Effect signatures** compile to zero-cost TypeScript `type` aliases.

<pre>
<code class="language-js">
# 1. Branded & refined scalar Schema + static type
export schema Username =
  string
  |> Schema.trimmed
  |> Schema.minLength(3)
  |> Schema.brand("Username")

# 2. Struct Schema with default (=) fields and explicit Option (no ?)
export schema User = {
  id: UUID
  name: Username
  age: Int |> Schema.between(0, 150)
  role: "admin" | "member" = "member"
  bio: Option&lt;NonEmptyString>
}

# 3. Tagged union (Data.TaggedEnum + constructors + Schema.Union)
export schema Shape =
  | Circle(radius: number)
  | Rect(width: number, height: number)

# 4. Compile-time TypeScript alias (no runtime output)
export schema Lookup = (id: UUID) -> Effect&lt;User, NotFound>
</code>
</pre>

Because `User` and `Shape` exist at both the type level and value level, you can annotate parameters with `: User` and decode untrusted JSON with `raw |> Schema.decode(User)` from a single declaration.

Every data `schema` is also a **validating constructor**, which replaces type casts entirely (Option C: no `as` keyword in the language). `UUID("1")` and `User({ ... })` lower to `Schema.make`, and when the argument is a literal the compiler runs the schema at compile time — so `Age(200)` is a compile error, and a branded type can never be forged with `as`:

<pre>
<code class="language-js">
ada = User({ id: UUID("0b9e6c1e-5f6a-4c6e-9a43-0c2f3b1d9e7a"), name: Username("ada"), age: 36, bio: None })

# x as User   -> parser error: use raw |> Schema.decode(User)
# Age(200)    -> compile error: Expected a number between 0 and 150, actual 200
</code>
</pre>

---

### 2. Only `match`: Exhaustive Pattern Matching & Boolean Branching

There is no `if`/`else`, `switch`, or ternary `? :`. Instead, `match` has two forms:

#### Structural value matching (compiles to `ts-pattern` `.exhaustive()`)

<pre>
<code class="language-js">
export describe = (shape: Shape) ->
  match shape
    | Circle(0) -> "point"
    | Circle(r) when r |> Number.greaterThan(100) -> "huge circle ({r})"
    | Circle(r) -> "circle ({r})"
    | Rect(w, h) -> "{w} by {h}"

# Or-patterns (`| A | B ->`) and pipeline `|> match`
export findName = (users: Array&lt;User>, id: string) ->
  users
  |> Array.findFirst((u) -> u.id == id)
  |> match
    | Some({ name }) -> name
    | None -> "anonymous"
</code>
</pre>

#### Subjectless `match` for boolean conditions (compiles to native JS `? :` or `if / else`)

When `match` has no scrutinee expression, each arm is a boolean condition that compiles directly to native JavaScript `? :` or `if / else if / else` with zero `ts-pattern` runtime overhead:

<pre>
<code class="language-js">
# Inline two-branch expression (comparisons are pure data-last functions)
sign = match | n |> Number.greaterThan(0) -> "positive" | _ -> "non-positive"

# Multi-branch block
tier =
  match
    | score |> Number.greaterThanOrEqualTo(90) and verified -> "platinum"
    | score |> Number.greaterThanOrEqualTo(75) -> "gold"
    | score |> Number.greaterThanOrEqualTo(50) -> "silver"
    | _ -> "bronze"
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
  |> parseScore
  |> clamp(0, 100, _)
  |> formatScore(_, "en-US")
</code>
</pre>

Point-free composition (`>>`) is intentionally omitted because `flow(f, g)` degrades TypeScript's generic type inference; writing `(x) -> x |> f |> g` is nearly as short and preserves full contextual inference.

#### Type-directed pipeline resolution (auto-namespace)

To eliminate repetitive module prefixes (`Array.`, `Option.`, `Effect.`), bare functions in pipelines automatically resolve against the companion module of the piped type:

<pre>
<code class="language-js">
# Clean, readable pipelines without repetitive "Array." prefixes:
topActive =
  users
  |> filter((u) -> u.active)
  |> sortBy((u) -> u.name)
  |> take(10)
  |> map((u) -> u.name)
</code>
</pre>

The compiler inspects the static type of `users` (`Array<User>`) and lowers to `pipe(users, Array.filter(...), Array.sortBy(...), ...)`, auto-injecting the required `effect/Array` import.

---

### 4. Immutable Copy-on-Write Updates: `{ ...spread }` and `mut` (Mutative)

All bindings (`x = 1`), records, and arrays are deeply `readonly` by default. To update data ergonomically without mutating shared references, declarative spreads are paired with imperative `mut` drafts powered by **Mutative** (`create`):

#### Declarative updates with `{ ...base, ... }` (90% of updates)

Shallow overrides emit a plain JS spread (`{ ...todo, completed: true }`). When you use a nested dot-path (`address.city: "Paris"`), the compiler automatically lowers it to Mutative's `create`:

<pre>
<code class="language-js">
# Shallow -> `{ ...todo, completed: true }`
done = { ...todo, completed: true }

# Deep dot-paths -> `create(user, draft => { ... })`
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
recordLogin = (user: User) ->
  mut draft = user
  draft.visits = draft.visits + 1
  draft.tags.push("active")
  match
    | draft.visits > 10 -> draft.tier = "gold"
  draft

# Or inline in a pipeline:
updated =
  user
  |> mut (d) -> d.tags.push("onboarded")
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

export schema Todo = {
  id: Int
  title: NonEmptyString
  completed: boolean
}

export fetchTodo = (id: number) ->
  effect
    response &lt;- HttpClient.get("https://jsonplaceholder.typicode.com/todos/{id}")
    todo &lt;- response |> HttpClientResponse.schemaBodyJson(Todo)
    { ...todo, completed: true }
  |> Effect.retry(Schedule.exponential("100 millis"))
  |> Effect.provide(FetchHttpClient.layer)
</code>
</pre>

A key guardrail in `effect` blocks: **a non-final bare expression is a syntax error**. In raw `Effect.gen`, writing `Console.log("hi")` without `yield*` silently creates an unused `Effect` and never runs it. In FunScript, every non-final line must bind (`=` or `<-`) or discard (`_ <- Console.log("hi")`).

#### Direct service calls without context ceremony

Instead of manually fetching a service tag before calling its methods (`repo <- UserRepo; user <- repo.find(id)`), calling the service method directly with `<-` automatically resolves the dependency from context and tracks it in the `R` channel:

<pre>
<code class="language-js">
export getUser = (id: string) ->
  effect
    user <- UserRepo.find(id)
    org <- OrgRepo.find(user.orgId)
    { user, org }
</code>
</pre>

#### Automatic error union tracking and `orDie` defect demotion

When sequencing operations that fail with different typed errors, Effect automatically tracks the union of errors in the `E` channel (`NotFound | PaymentDeclined | OutOfStock`), and ts-pattern exhaustiveness ensures callers cannot ignore any variant.

When an error represents an invariant violation or unrecoverable defect, `|> orDie` strips it from the `E` channel and demotes it to an Effect defect, auto-resolving to `Effect.orDie`:

<pre>
<code class="language-js">
export loadDefaultConfig =
  effect
    config <- ConfigRepo.loadDefault |> orDie
    config
</code>
</pre>

---

### 6. Type-Driven Development with Typed Holes (`???`)

While sketching an implementation, you can drop `???` (or `??? "label"`) into any expression position:

<pre>
<code class="language-js">
export checkout = (user: User, cart: Cart): Effect&lt;Receipt, PaymentError> ->
  effect
    total = calculateTotal(cart)
    ??? "charge payment provider"
</code>
</pre>

The compiler lowers `???` to a `never`-typed throw and queries TypeScript's `checker.getContextualType(node)` and `checker.getSymbolsInScope(node)` to print the exact expected type and local bindings in scope:

<pre>
<code class="language-bash">
warning[funscript/typed-hole]: Unfinished hole `???` ("charge payment provider")
  --> src/checkout.fun:4:5
  | Expected type: Effect&lt;Receipt, PaymentError, never>
  | In scope:
  |   user  : User
  |   cart  : Cart
  |   total : number
</code>
</pre>

Typed holes emit warnings during `funscript dev` and `funscript test` (so you can run other branches while iterating) and are a hard error in `funscript build`.

---

### 7. The Pit of Success: Guardrails Without Keywords

The goal is that **the shortest way to write something is also the safe way**. Beyond the syntax above, `funscript check` enforces a few rules that cost zero keywords, each backed by something Effect already ships:

<table>
<thead>
<tr>
<th>You cannot...</th>
<th>Rejected</th>
<th>Write instead</th>
</tr>
</thead>
<tbody>
<tr>
<td>Hide time or randomness</td>
<td><code>Date.now()</code>, <code>Math.random()</code>, <code>performance.now()</code></td>
<td><code>DateTime.now</code>, <code>Clock.currentTimeMillis</code>, <code>Random.next</code> — controlled by <code>TestClock</code> in tests</td>
</tr>
<tr>
<td>Read config from globals</td>
<td><code>process.env.PORT</code></td>
<td><code>Schema.Config("PORT", Port)</code>, <code>Config.redacted("DATABASE_URL")</code></td>
</tr>
<tr>
<td>Run an effect mid-program</td>
<td><code>Effect.runPromise(...)</code> in a helper</td>
<td>Return the <code>Effect</code>; <code>export main</code> in an entry module is run once with <code>runMain</code></td>
</tr>
<tr>
<td>Cast past a schema</td>
<td><code>"1" as UUID</code>, <code>input as User</code></td>
<td><code>UUID("1")</code> (validated at compile time) or <code>raw |> Schema.decode(User)</code></td>
</tr>
</tbody>
</table>

Time becomes testable without real waiting:

<pre>
<code class="language-js">
export isExpired = (token: Token) ->
  effect
    now &lt;- DateTime.now
    DateTime.greaterThan(now, token.expiresAt)

it.effect("expires after one hour", ->
  effect
    token &lt;- issueToken(user)
    _ &lt;- TestClock.adjust("61 minutes")
    expired &lt;- isExpired(token)
    expect(expired).toBe(true)
)
</code>
</pre>

And every place that crosses from Effect into Promise-land is listed in one reviewed config file (`"entry"` in `funscript.json`) instead of being scattered through the codebase:

<pre>
<code class="language-bash">
src/users.fun:18:5  error  `Effect.runPromise` outside an entry module
  running here drops typed errors, interruption, and the caller's layers
  try: return the Effect and let the caller compose it
</code>
</pre>

---

### 8. Concurrency: Goroutines & Channels for Free

FunScript doesn't need a `go`, `chan`, or `select` keyword. In **Effect v4**, **fibers are green threads (goroutines)**, `Queue` is a channel, `Effect.race` is `select`, and worker pools are structured:

<table>
<thead>
<tr>
<th>Go</th>
<th>Effect v4</th>
<th>FunScript</th>
</tr>
</thead>
<tbody>
<tr>
<td><code>go worker()</code></td>
<td><code>Effect.forkScoped</code></td>
<td><code>_ &lt;- Effect.forkScoped(worker)</code></td>
</tr>
<tr>
<td><code>chan T</code> (buffered)</td>
<td><code>Queue.bounded&lt;T>(n)</code></td>
<td><code>jobs &lt;- Queue.bounded&lt;Job>(100)</code></td>
</tr>
<tr>
<td><code>ch &lt;- v</code> / <code>&lt;-ch</code></td>
<td><code>Queue.offer</code> / <code>Queue.take</code></td>
<td><code>_ &lt;- Queue.offer(jobs, j)</code> / <code>j &lt;- Queue.take(jobs)</code></td>
</tr>
<tr>
<td><code>select</code></td>
<td><code>Effect.race</code></td>
<td><code>data &lt;- Effect.race(primary, replica)</code></td>
</tr>
<tr>
<td><code>errgroup</code> / pool</td>
<td><code>Effect.forEach(..., { concurrency })</code></td>
<td><code>users &lt;- ids |> Effect.forEach(fetch, { concurrency: 8 })</code></td>
</tr>
</tbody>
</table>

Unlike Go goroutines, Effect fibers are **structured**: tied to an enclosing `Scope` or parent fiber. When a scope finishes or a task fails, child fibers are interrupted automatically — no goroutine leaks, and no manual `context.Context` plumbing:

<pre>
<code class="language-js">
# Producer / consumer pipeline with 4 background workers
export pipeline =
  effect
    jobs &lt;- Queue.bounded&lt;Job>(100)
    _ &lt;- Effect.forkScoped(producer(jobs))
    _ &lt;- Array.range(1, 4) |> Effect.forEach((id) -> Effect.forkScoped(worker(jobs, id)))
    Effect.never
  |> Effect.scoped   # closing the scope cleans up all workers automatically
</code>
</pre>

---

## Summary Cheat Sheet

<table>
<thead>
<tr>
<th>Construct</th>
<th>Syntax</th>
<th>Compiles to</th>
</tr>
</thead>
<tbody>
<tr>
<td>Immutable binding</td>
<td><code>x = 1</code></td>
<td><code>const x = 1;</code></td>
</tr>
<tr>
<td>Local mutable / draft</td>
<td><code>mut x = user</code> / <code>x.a = 1</code></td>
<td><code>let</code> / Mutative <code>create(user, ...)</code></td>
</tr>
<tr>
<td>Shallow / deep copy</td>
<td><code>{ ...u, a.b: 1 }</code></td>
<td><code>{ ...u, ... }</code> / Mutative <code>create(...)</code></td>
</tr>
<tr>
<td>Function</td>
<td><code>(a: number, b: number) -> a + b</code></td>
<td><code>(a: number, b: number) => a + b</code></td>
</tr>
<tr>
<td>Pipeline & placeholder</td>
<td><code>x |> f(a) |> g(1, _)</code></td>
<td><code>pipe(x, f(a), (__p) => g(1, __p))</code></td>
</tr>
<tr>
<td>Pattern match</td>
<td><code>match x | A(v) -> v</code></td>
<td><code>match(x).with({ _tag: "A" }, ...).exhaustive()</code></td>
</tr>
<tr>
<td>Boolean match</td>
<td><code>match | x |> Number.greaterThan(0) -> "pos" | _ -> "neg"</code></td>
<td><code>Number.greaterThan(x, 0) ? "pos" : "neg"</code></td>
</tr>
<tr>
<td>Schema & type</td>
<td><code>schema User = { id: UUID }</code></td>
<td><code>Schema.Struct(...)</code> + <code>typeof User.Type</code></td>
</tr>
<tr>
<td>Schema constructor</td>
<td><code>UUID("1")</code> / <code>User({ ... })</code></td>
<td><code>Schema.UUID.make("1")</code> (literals checked at compile time)</td>
</tr>
<tr>
<td>Tagged union</td>
<td><code>schema Shape = | Circle(r: number)</code></td>
<td><code>Data.TaggedEnum</code> + <code>Schema.Union</code></td>
</tr>
<tr>
<td>Typed error</td>
<td><code>error NotFound(id: UUID)</code></td>
<td><code>class NotFound extends Schema.TaggedError...</code></td>
</tr>
<tr>
<td>Service</td>
<td><code>service Repo ...</code></td>
<td><code>class Repo extends Context.Service...</code></td>
</tr>
<tr>
<td>Direct service call</td>
<td><code>x &lt;- Repo.find(id)</code></td>
<td><code>const x = yield* (yield* Repo).find(id);</code></td>
</tr>
<tr>
<td>Effect block</td>
<td><code>effect</code> + <code>x &lt;- eff</code></td>
<td><code>Effect.gen(function* () { const x = yield* eff; })</code></td>
</tr>
<tr>
<td>Defect demotion</td>
<td><code>eff |> orDie</code></td>
<td><code>pipe(eff, Effect.orDie)</code></td>
</tr>
<tr>
<td>Fork / channel</td>
<td><code>forkScoped(eff)</code> / <code>Queue.bounded(n)</code></td>
<td>Structured green thread / buffered channel</td>
</tr>
<tr>
<td>Typed hole</td>
<td><code>??? "todo"</code></td>
<td><code>never</code> throw + contextual type diagnostic</td>
</tr>
</tbody>
</table>
