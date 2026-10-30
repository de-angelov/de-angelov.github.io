---
title: "Same App, Different Languages"
tags: ["2026", "Programming"]
date: Feb 27, 2026
---

## Overview
This multi‑language project implements the **same simple web application** in different ecosystems to compare approaches in functional programming.  
The app includes:

- Server‑Side Rendered (SSR) HTML  
- Protected routes  
- User registration and login  
- Database integration  

---

## Goal
The main goal is to **compare programming styles and frameworks** across languages.

---

## Implemented

### Scala 3 + ZIO
**Repository:** https://github.com/de-angelov/scala-user-login

**Tech Stack & Libraries:**

- **Scala 3** — modern, strongly typed JVM language with functional programming features.
- **ZIO** — a purely functional effect system & runtime for Scala, offering type‑safe, composable concurrency and resource management (handles effects, asynchronous programming, error handling, and more). 

---

### Haskell + Servant + RIO
**Repository:** https://github.com/de-angelov/servant-user-login

**Tech Stack & Libraries:**

- **Haskell** — a purely functional programming language with a strong static type system.
- **Servant** — a Haskell web framework / type‑level DSL for describing APIs. Routes are defined as types which are then interpreted by the framework to provide servers, client stubs, documentation, etc. 
- **RIO** — a batteries‑included, opinionated Haskell standard library and Prelude replacement, providing common tools (logging, environment management, safe defaults) to make production Haskell code more ergonomic. 


---

### TypeScript + Effect
**Repository:** https://github.com/de-angelov/effect-user-login

**Tech Stack & Libraries:**

- **TypeScript** — a typed superset of JavaScript with a mature tooling ecosystem.
- **Effect** — a functional effects system for TypeScript, providing typed errors, composable concurrency, resource management, `Schema`, and dependency injection through services and layers.
- **@effect/platform** — the HTTP server and router used to serve server‑rendered HTML and handle form submissions.
- **@kitajs/html** — type‑safe JSX templates that render to HTML strings.
- **@effect/sql** — typed SQL queries with `Model` and `SqlSchema`.
- **@effect/sql-pg** — the PostgreSQL driver for Effect SQL, used for users and sessions.

The example keeps the same feature set as the other implementations. It uses Effect's `Schema` module to validate forms and brand usernames, passwords, and session tokens, and `@effect/sql`'s `Model` and `SqlSchema` for typed queries. The database, password hashing, and authentication logic are self-contained `Effect.Service` layers, and domain errors are mapped to HTTP responses with `Effect.catchTags`.

