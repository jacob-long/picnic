---
title: "Home"
layout: home
---

# Motivation #

Keeping up with newly published research in communication science can be challenging. Email alerts from publishers clutter inboxes and arrive at irregular intervals. Setting up alerts or finding RSS feeds for multiple publishers is time-consuming. Social media platforms like Twitter have their own limitations for academic purposes.

Inspired by [Moritz Marbach's](https://www.moritz-marbach.com/) 
[Paper Picnic project](https://paper-picnic.com) for
Political Science, this adaptation of his project focuses on communication 
science research with some love for politics and related social science areas 
that are helpful to my research. 

All journal data is sourced from the Crossref
[Crossref](https://www.crossref.org/community/) API. Preprints are retrieved
from the [Open Science Foundation](https://osf.io) API.

<br>
<hr>

# Backend #

The backend consists of a crawler written in R, hosted in a GitHub repository. 
Every morning, GitHub Actions executes the crawler. The resulting data is stored 
in a JSON file and rendered into an HTML file using GitHub Pages.

For each journal, the crawler retrieves articles added in the previous week. It
requests all articles with "created" or "published" fields in the Crossref 
database within the last seven days.

The crawler collects title, authors, full-text link, and abstract information. 
However, some publishers, like Elsevier or Taylor & Francis, do not include 
abstracts in their Crossref metadata (see [this](https://www.crossref.org/blog/i4oa-hall-of-fame-2023-edition/) Crossref Blog for details). Unfortunately,
a large portion of communication journals are partnered with publishers who 
do not allow any automated retrieval of their abstracts.

As journals typically have two ISSN numbers (print and electronic, see 
[here](https://en.wikipedia.org/wiki/ISSN)), the crawler retrieves articles for
both and deduplicates the results (usually). ISSN numbers are obtained from the
Crossref lookup [tool](https://www.crossref.org/titleList/).

For generic titles (e.g., "Errata", "Frontmatter", "Backmatter"), the crawler 
adds a filter tag. For articles from multidisciplinary journals and preprint,
GPT-4 is prompted to determine if the content is relevant to communication 
science (intending to err on the side of inclusiveness). Filtered content is 
hidden by default but can be displayed by clicking the +N button at the top left
of each journal section.

<br>

<hr>

# Contribute #

1. Find and fix bugs or add new features to the crawler/web page. Help out
Moritz on [his original version](https://github.com/sumtxt/picnic) or make 
suggestions [in mine](https://github.com/jacob-long/picnic).

2. Use the crawled data for your own tool: <button type="button" class="align-items-center btn btn-primary btn-sm rounded-pill" data-bs-toggle="modal" data-bs-target="#jsonlist">All JSON Files</button>

3. Build an improved (and equally open source) version of this page.

4. Support [The Initiative for Open Abstracts](https://i4oa.org/).
