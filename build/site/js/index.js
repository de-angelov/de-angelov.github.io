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

	function initHeaderIconAnimation() {
		var headerIcon = document.querySelector('.header__icon');
		var header = document.querySelector('.header__nav');

		headerIcon.addEventListener('click',function(){
			headerIcon.classList.remove('spring-effect');
			void headerIcon.offsetWidth; // force reflow to restart animation
			headerIcon.classList.add('spring-effect');

			header.classList.remove('shine-effect');
			void header.offsetWidth; // force reflow to restart animation
			header.classList.add('shine-effect');
		})
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

			// a.style.display = hasSelectedFilterTag ? 'inherit' : 'none';

			if(hasSelectedFilterTag) {
				a.classList.remove('fade-out-effect');
				a.classList.add('fade-in-effect');
			} else {
				a.classList.remove('fade-in-effect');
				a.classList.add('fade-out-effect');
			}
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
				initHeaderIconAnimation();
				window.addEventListener('popstate', () => { 
					highlightCurrentPage();
					hideShowFilters();
				});
			});
})();
