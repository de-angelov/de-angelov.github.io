(function () {

	var filterParamName = "filter";
	var filterDataAttr = "data-filter";
	var tagButtons = [];
	var blogLinks = [];

	function markTagAsSelected(b) { b.classList.add('tag-list__tag--selected') };
	function markTagAsDeselected(b) { b.classList.remove('tag-list__tag--selected'); };

	function getCurrentActiveFilters() {
		var currentURL = new URL(window.location.href);
		return (currentURL.searchParams.get(filterParamName) || "")
			.split(',')
			.filter(x => x !== "");
	}

	function initHeaderIconAnimation() {
		var headerIcon = document.querySelector('.header__icon');
		var header = document.querySelector('.header__nav');
		if (!headerIcon) return;

		headerIcon.addEventListener('click', function () {
			headerIcon.classList.remove('spring-effect');
			void headerIcon.offsetWidth;
			headerIcon.classList.add('spring-effect');

			header.classList.remove('shine-effect');
			void header.offsetWidth;
			header.classList.add('shine-effect');
		})
	}

	function highlightCurrentPage() {
		var currentPath = window.location.pathname;
		var selector = "#home";
		if (currentPath.includes("cv.html")) selector = "#cv";
		if (currentPath.includes("blog.html") || currentPath.includes("posts/")) selector = "#blog";

		var activeElement = document.querySelector(selector);
		if (activeElement) {
			activeElement.classList.add("header__nav--active");
			activeElement.setAttribute("aria-current", "page");
		}
	}

	function initMermaidCharts() {
		if (typeof mermaid !== 'undefined') {
			mermaid.initialize({ startOnLoad: true });
		}
	}

	function handleFilterClick(e) {
		var currentURL = new URL(window.location.href);
		var filterValue = e.currentTarget.getAttribute(filterDataAttr);
		var currentFilters = getCurrentActiveFilters();

		var updatedFilters = currentFilters.includes(filterValue)
			? currentFilters.filter(function (x) { return x !== filterValue })
			: [...currentFilters, filterValue];

		if (filterValue === 'all' || updatedFilters.length === 0) {
			currentURL.searchParams.delete(filterParamName);
		} else {
			currentURL.searchParams.set(filterParamName, updatedFilters.join(','));
		}

		window.history.pushState({}, '', currentURL.toString());
		hideShowFilters({ initialFade: true });
	}


	async function hideShowFilters({ initialFade = true } = {}) {
		const currentFilters = getCurrentActiveFilters();
		const toShow = [];
		const toHide = [];

		const onDone = (e) => {
			if (e.animationName === 'fadeScaleDown') {
				el.style.display = 'none';
				el.removeEventListener('animationend', onDone);
				resolve();
			}
		};

		const fadeOut = (el) => new Promise(resolve => {
			el.classList.remove('fade-in-effect');
			el.classList.add('fade-out-effect');


			el.addEventListener('animationend', onDone);
			setTimeout(() => { el.style.display = 'none'; resolve(); }, 500); // Fallback
		});

		if (!initialFade) {
			blogLinks.forEach(a => {
				const tags = a.getAttribute('data-tags').split(',');
				const hasMatch = currentFilters.length === 0 || currentFilters.some(x => tags.includes(x));
				a.style.display = hasMatch ? 'inherit' : 'none';
				a.classList.remove('fade-out-effect', 'fade-in-effect');
			});
			updateTagButtonUI(currentFilters);
			return;
		}

		blogLinks.forEach(a => {
			const tags = a.getAttribute('data-tags').split(',');
			const hasMatch = currentFilters.length === 0 || currentFilters.some(x => tags.includes(x));
			const isVisible = a.style.display !== 'none';

			if (hasMatch && !isVisible) toShow.push(a);
			if (!hasMatch && isVisible) toHide.push(a);
		});

		if (toHide.length > 0) {
			await Promise.all(toHide.map(fadeOut));
		}

		toShow.forEach(a => {
			a.style.display = 'inherit';
			void a.offsetWidth;
			a.classList.remove('fade-out-effect');
			a.classList.add('fade-in-effect');
		});

		updateTagButtonUI(currentFilters);
	}

	function updateTagButtonUI(currentFilters) {
		const activeTags = currentFilters.length === 0 ? ['all'] : currentFilters;
		tagButtons.forEach(b => {
			const val = b.getAttribute(filterDataAttr);
			activeTags.includes(val) ? markTagAsSelected(b) : markTagAsDeselected(b);
		});
	}
	function initFiltering() {
		tagButtons = Array.from(document.querySelectorAll('.tag-list__tag'));
		blogLinks = Array.from(document.querySelectorAll('.post__list_link'));
		tagButtons.forEach(function (b) { b.addEventListener('click', handleFilterClick) });
	}

	document.addEventListener("DOMContentLoaded", function () {
		highlightCurrentPage();
		initMermaidCharts();
		initFiltering();
		hideShowFilters({ initialFade: false });
		initHeaderIconAnimation();
		window.addEventListener('popstate', () => {
			highlightCurrentPage();
			hideShowFilters({ initialFade: false });
		});
	});
})();