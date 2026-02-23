---
title: "IIEF in modern JS"
tags: ["2025", "JavaScript", "FP"]
date: Feb 13, 2025
---
In modern JavaScript there is one rarely seen feature [IIFE](https://developer.mozilla.org/en-US/docs/Glossary/IIFEs).
Once before ES6, it was heavily used for module encapsulation.

In my opinion there is actually one nice use for the trusty old IIFE even today when functional style has become popular - converting imperative statements to an expressions! 

A statement is a complete instruction that tells the computer to perform an action. It does not necessarily return a value.

<pre>
<code class="language-js">
    if (x > 5) {  // Conditional statement
        console.log("x is greater than 5");  // Print statement
    }    
</code>
</pre>

An expression is any valid combination of values, variables, operators, and function calls that produces a value.

<pre>
<code class="language-js">
   const result = x * y + z;   
</code>
</pre>

In functional programming statements  are less common because this style emphasizes pure functions and immutability.
It is all about evaluating expressions to produce values.

One cool trick that Javascript allows, is to use IIFEs to skip mutable variables.

<pre>
<code class="language-js">
//using imperative style 
    let result;
    if (x < 0) result = "small";
    if (x === 0) result = "mid";
    if (x > 0) result = "big";

//using functional style 
   const result = (() => {
    if (x < 0) return "small";
    if (x === 0) return "mid";
    if (x > 0) return "big";
   })() 
</code>
</pre>
