---
title: "Same App, Different Languages"
tags: ["2026", "FP", "Haskell"]
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
The main goal is to **compare Programming styles and frameworks** across languages.

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

## Planned

- **TypeScript + Effect‑TS** — using a functional effects system in the TypeScript ecosystem.

