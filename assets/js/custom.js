function getContainerIds() {
    const containerElements = document.getElementsByClassName('container');
    const containerIds = [];

    for (let i = 0; i < containerElements.length; i++) {
        const containerId = containerElements[i].id;
        if (containerId) {
            containerIds.push(containerId);
        }
    }

    return containerIds;
}

function mapArrayToLookup(array,lookup) {
	return array.map(item => {
	    if( lookup.hasOwnProperty(item)) {
	      return(lookup[item])
	    } else {
	      return(item)
	    };
	}).flat();
	};

// Search functionality
function searchArticles(query) {
    const searchTerm = query.toLowerCase().trim();
    const cards = document.querySelectorAll('#article-card');
    const journalContainers = document.querySelectorAll('.container[id]');

    if (searchTerm === '') {
        // Show all articles and journals
        cards.forEach(card => {
            card.style.display = '';
        });
        journalContainers.forEach(container => {
            if (container.id && container.id !== 'search-container') {
                container.style.display = '';
            }
        });
        updateSearchResults(null);
        return;
    }

    let matchCount = 0;
    const matchedJournals = new Set();

    cards.forEach(card => {
        const title = card.querySelector('.card-title')?.textContent?.toLowerCase() || '';
        const authors = card.querySelector('.card-subtitle')?.textContent?.toLowerCase() || '';
        const abstract = card.querySelector('.card-text')?.textContent?.toLowerCase() || '';

        if (title.includes(searchTerm) || authors.includes(searchTerm) || abstract.includes(searchTerm)) {
            card.style.display = '';
            matchCount++;
            // Find parent journal container
            const journalContainer = card.closest('.container[id]');
            if (journalContainer) {
                matchedJournals.add(journalContainer.id);
            }
        } else {
            card.style.display = 'none';
        }
    });

    // Hide journals with no matching articles
    journalContainers.forEach(container => {
        if (container.id && container.id !== 'search-container') {
            if (matchedJournals.has(container.id)) {
                container.style.display = '';
            } else {
                container.style.display = 'none';
            }
        }
    });

    updateSearchResults(matchCount);
}

function updateSearchResults(count) {
    const resultsEl = document.getElementById('search-results');
    if (resultsEl) {
        if (count === null) {
            resultsEl.textContent = '';
        } else {
            resultsEl.textContent = `Found ${count} article${count !== 1 ? 's' : ''}`;
        }
    }
}

// Citation export functions
function generateBibTeX(article) {
    const doi = article.doi || '';
    const doiKey = doi.replace(/[^a-zA-Z0-9]/g, '_') || 'unknown';
    const authors = article.authors || 'Unknown';
    const title = article.title || 'Untitled';
    const journal = article.journal || '';
    const year = new Date().getFullYear();
    const url = article.url || '';

    return `@article{${doiKey},
  author = {${authors.replace(/, /g, ' and ')}},
  title = {${title}},
  journal = {${journal}},
  year = {${year}},
  doi = {${doi}},
  url = {${url}}
}`;
}

function generateRIS(article) {
    const authors = (article.authors || 'Unknown').split(/, |; /);
    const authorLines = authors.map(a => `AU  - ${a}`).join('\n');

    return `TY  - JOUR
${authorLines}
TI  - ${article.title || 'Untitled'}
JO  - ${article.journal || ''}
PY  - ${new Date().getFullYear()}
DO  - ${article.doi || ''}
UR  - ${article.url || ''}
ER  - `;
}

function copyToClipboard(text, button) {
    navigator.clipboard.writeText(text).then(() => {
        const originalText = button.textContent;
        button.textContent = 'Copied!';
        setTimeout(() => {
            button.textContent = originalText;
        }, 1500);
    });
}

function exportCitation(format, cardElement) {
    const title = cardElement.querySelector('.card-title')?.textContent || '';
    const authors = cardElement.querySelector('.card-subtitle')?.textContent || '';
    const abstract = cardElement.querySelector('.card-text')?.textContent || '';
    const fullTextLink = cardElement.querySelector('a[href*="doi.org"], a.btn')?.href || '';
    const doiMatch = fullTextLink.match(/10\.[^/]+\/[^\s]+/);
    const doi = doiMatch ? doiMatch[0] : '';
    const journalContainer = cardElement.closest('.container[id]');
    const journal = journalContainer?.querySelector('h2')?.textContent || '';

    const article = { title, authors, abstract, doi, url: fullTextLink, journal };

    let citation;
    if (format === 'bibtex') {
        citation = generateBibTeX(article);
    } else if (format === 'ris') {
        citation = generateRIS(article);
    }

    return citation;
}

$(document).ready(function () {
    const urlParams = new URLSearchParams(window.location.search);

    const hideIds = urlParams.get('hide');
    const showIds = urlParams.get('show');

    const hideArray = hideIds ? hideIds.split(',') : [];
    const showArray = showIds ? showIds.split(',') : [];

    console.log(hideArray);

    if (hideArray.length > 0) {

        const hideArrayFull = mapArrayToLookup(hideArray,bundles);

        hideArrayFull.forEach(id => {
            const div = document.getElementById(id);
            if (div) {
                div.style.display = 'none';
            }
        });
    }

    if (showArray.length > 0) {

        const showArrayFull = mapArrayToLookup(showArray,bundles);

        const allArray = getContainerIds();
        const combArray = allArray.filter(x => !showArrayFull.includes(x));

        combArray.forEach(id => {
            const div = document.getElementById(id);
            if (div) {
                div.style.display = 'none';
            }
        });
    }

    const tooltipTriggerList = document.querySelectorAll('[data-bs-toggle="tooltip"]')
    const tooltipList = [...tooltipTriggerList].map(tooltipTriggerEl => new bootstrap.Tooltip(tooltipTriggerEl))

    // Initialize search with debounce
    const searchInput = document.getElementById('article-search');
    if (searchInput) {
        let debounceTimer;
        searchInput.addEventListener('input', function() {
            clearTimeout(debounceTimer);
            debounceTimer = setTimeout(() => {
                searchArticles(this.value);
            }, 300);
        });
    }

    // Handle citation export button clicks
    $(document).on('click', '.cite-btn', function() {
        const format = $(this).data('format');
        const card = $(this).closest('#article-card')[0];
        const citation = exportCitation(format, card);
        copyToClipboard(citation, this);
    });

});