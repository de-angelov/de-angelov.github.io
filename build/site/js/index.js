(function () {

	var filterParamName = "filter";
	var filterDataAttr = "data-filter";
	var tagButtons = [];
	var blogLinks = [];

	function markTagAsSelected(b){ b.classList.add('tag-list__tag--selected') };
	function markTagAsDeselected(b) { b.classList.remove('tag-list__tag--selected'); };
	function getCurrentActiveFilters() { 
		var currentURL = new URL(window.location.href);
		return  (currentURL.searchParams.get(filterParamName) || "")
		.split(',')
		.filter(x => x !== "");
	}

	function highlightCurrentPage() {
		var currentPath = window.location.pathname;
		var selector = "#home";

		if (currentPath.includes("cv.html")) {
			selector = "#cv";
		}	

		if (currentPath.includes("blog.html") || currentPath.includes("posts/")) {
			selector = "#blog";
		}

		var activeElement = document.querySelector(selector);
	
		if (activeElement) {
			activeElement.classList.add("header__nav--active");
		
			activeElement.setAttribute("aria-current", "page");
		}
	}

	function initMermaidCharts() {
		mermaid && mermaid.initialize({ startOnLoad: true });
	}

	
	function handleFilterClick(e) {
		var currentURL = new URL(window.location.href);
		
		var filterValue = e.currentTarget.getAttribute(filterDataAttr);
		
		var currentFilters = getCurrentActiveFilters();

		var updatedFilters = currentFilters.includes(filterValue)
			? currentFilters.filter(function (x) { return x !== filterValue })
			: [...currentFilters, filterValue];
		
		if(filterValue === 'all' || updatedFilters.length === 0){
			currentURL.searchParams.delete(filterParamName);
		} else {
			currentURL.searchParams.set(filterParamName, updatedFilters.join(','));
		}

		window.history.pushState({}, '', currentURL.toString());
		hideShowFilters();
	}

	function hideShowFilters() {
		var currentFilters = getCurrentActiveFilters();

		blogLinks.forEach(function (a) {
			var tags = a.getAttribute('data-tags').split(',')
			var hasSelectedFilterTag = 
				currentFilters.length === 0 ||
				currentFilters.some(x => tags.includes(x));

			a.style.display = hasSelectedFilterTag ? 'inherit' : 'none';
		});

		if(currentFilters.length === 0) {
			tagButtons
				.forEach(markTagAsDeselected);

			tagButtons
				.filter(b => b.getAttribute(filterDataAttr) === 'all')
				.forEach(markTagAsSelected);
		} else {
			tagButtons
				.forEach(markTagAsDeselected);

			tagButtons
				.filter(b => currentFilters.some(function(f) { return f == b.getAttribute(filterDataAttr); }))
				.map(x => { console.log("currentfilters", x); return x })
				.forEach(markTagAsSelected);
		}

	}

	function initFiltering() {
		tagButtons = Array.from(document.querySelectorAll('.tag-list__tag'));
		blogLinks = Array.from(document.querySelectorAll('.post__list_link'));
		tagButtons.forEach(function (b) { b.addEventListener('click', handleFilterClick) });
	}

	document
		.addEventListener(
			"DOMContentLoaded",
			function () {
				highlightCurrentPage();
				initMermaidCharts();
				initFiltering();
				hideShowFilters();
				window.addEventListener('popstate', () => { 
					highlightCurrentPage();
					hideShowFilters();
				});
			});
})();
