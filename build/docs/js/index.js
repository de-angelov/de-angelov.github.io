(function () {

	var filterParamName = "filter";
	var filterDataAttr = "data-filter";

	function getTagsButtons(){ return Array.from(document.querySelectorAll('.tag-list__tag')); };
	function markTagAsSelected(b){ b.classList.add('tag-list__tag--selected') };
	function markTagAsDeselected(b) { b.classList.remove('tag-list__tag--selected'); };
	function getCurrentActiveFilters() { 
		var currentURL = new URL(window.location.href);
		return  (currentURL.searchParams.get(filterParamName) || "")
		.split(',')
		.filter(x => x !== "");
	}

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

	
	function handleFilterClick(e) {
		var currentURL = new URL(window.location.href);
		
		var filterValue = e.target.getAttribute(filterDataAttr);
		
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
		
		var currentBlogLinks = Array.from(document.querySelectorAll('.post__list_link')) || [];
		var tagButtons = getTagsButtons();

		currentBlogLinks.forEach(function (a) {
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
				.filter(b => b.getAttribute(filterDataAttr) === 'all')
				.map(markTagAsDeselected);

			tagButtons
				.filter(b => currentFilters.some(function(f) { return f == b.getAttribute(filterDataAttr); }))
				.forEach(markTagAsSelected);
		}

	}

	function initFiltering() {
		const buttons = getTagsButtons();
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