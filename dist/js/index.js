function highlightCurrentPage() {
        // Get the current URL path
        var currentPath = window.location.pathname.split('/').pop();
        console.log("currentPath", currentPath);

        var selector = "#blog"

        if(currentPath == "cv.html") {
            selector = "#cv"
        }

        if(currentPath == "index.html") {
            selector = "#home"
        }

        document
        .querySelector(selector)
        .classList
        .add("header__nav--active")
}

document.addEventListener("DOMContentLoaded", highlightCurrentPage)
