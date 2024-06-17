function highlightCurrentPage() {
        var currentPath = window.location.pathname.split('/').pop();
        var selector = "#blog"

        if(currentPath == "cv.html") {
            selector = "#cv"
        }

        if(currentPath == "" || currentPath == "index.html") {
            selector = "#home"
        }

        document
            .querySelector(selector)
            .classList
            .add("header__nav--active")
}

function initMermaidCharts(){
    mermaid && mermaid.initialize({ startOnLoad: true });
}

document
    .addEventListener(
    "DOMContentLoaded", 
    () => { 
        highlightCurrentPage();
        initMermaidCharts();
    });
