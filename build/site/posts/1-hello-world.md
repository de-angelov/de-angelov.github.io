---
title: "Hello World"
tags: ["2025", "Haskell"]
date: Jan 18, 2025
---

Arguably, it might be a bit of overengineering for a simple blog to use Haskell.But at least I can get some programmer cred :)

Some details:

- Uses Haskell site generator [Slick](https://github.com/ChrisPenner/slick)
- [Task](https://taskfile.dev/) simple task runner

- [Mermaid diagrams](https://mermaid.js.org/) markdown extensions

- [Prism](https://mermaid.js.org/) syntax highlighter

- The site background comes from [Hero Patterns](https://heropatterns.com/)


Some examples

<pre>
    <code class="language-js">
    () => { console.log(10);}
    </code>

    <p class="mermaid">
    graph TD;
        A[Square Rect] -- Link text --> B((Circle))
        A --> C(Round Rect)
        B --> D{Rhombus}
        C --> D
    </p>
</pre>