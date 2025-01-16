(function () {
	function highlightCurrentPage() {
		var currentPath = window.location.pathname.split('/').pop();
		var selector = "#blog"

		if (currentPath == "cv.html") {
			selector = "#cv"
		}

		if (currentPath == "" || currentPath == "index.html") {
			selector = "#home"
		}

		document
			.querySelector(selector)
			.classList
			.add("header__nav--active")
	}

	function initMermaidCharts() {
		mermaid && mermaid.initialize({ startOnLoad: true });
	}

	var filterParam = "filter";
	var currentURL = new URL(window.location.href);

	function handleFilterClick(e) {
		
		var filterValue = e.target.getAttribute('data-filter');
		
		console.log('click', e.target, filterValue);

		var currentFilters = (currentURL.searchParams.get(filterParam) || "").split(',');
		var updatedFilter = currentFilters.includes(filterValue)
			? currentFilters.filter(function (x) { x !== filterValue })
			: [...currentFilters, filterValue];

		currentURL.searchParams.set('filter', updatedFilter.join(','));
		hideShowFilters();
	}

	function hideShowFilters() {
		var currentFilters = (currentURL.searchParams.get(filterParam) || "").split(',');
		var currentBlogLinks = Array.from(document.querySelectorAll('.blog__article')) || [];

		currentBlogLinks.forEach(function (a) {
			var tags = a.attributes.getNamedItem('data-tags').split(',')
			var hasFilter = currentFilters.some(x => tags.includes(x));

			a.style.display = hasFilter ? 'none' : 'inherit';

		});

	}

	function initFiltering() {
		// init button filtering 

		const buttons = Array.from(document.querySelectorAll('.tag-list__tag'));

		buttons.forEach(function (b) { b.addEventListener('click', handleFilterClick) });

	}

	document
		.addEventListener(
			"DOMContentLoaded",
			function () {
				highlightCurrentPage();
				initMermaidCharts();
				initFiltering();
				hideShowFilters();
			});

})();