---
title: "Home"
layout: home
---

# Motivation #

Keeping up with newly published research in communication science can be challenging. Email alerts from publishers clutter inboxes, arrive at irregular intervals, and often lack abstracts. Publisher RSS feeds can be equally frustrating, with available RSS readers being either clunky or expensive. Setting up alerts or finding RSS feeds for multiple publishers is time-consuming. Social media platforms like Twitter/X have their own limitations for academic purposes.

Inspired by [Moritz Marbach's](https://www.moritz-marbach.com/) 
[Paper Picnic project](https://paper-picnic.com) for
Political Science, this adaptation focuses on communication science research with 
some love for politics and related social science areas that are helpful to my
research.

All data is sourced from the Crossref API. 
[Crossref](https://www.crossref.org/community/).

<br>
<hr>

# Backend #

*Text below comes from Moritz but is true for this version also*:

The backend consists of a crawler written in R, hosted in a GitHub repository. 
Every Friday, GitHub Actions executes the crawler. The resulting data is stored 
in a JSON file and rendered into an HTML file using GitHub Pages.

For each journal, the crawler retrieves articles added in the previous week. It
requests all articles with "created" or "published" fields in the Crossref 
database within the last seven days.

The crawler collects title, authors, full-text link, and abstract information. 
However, some publishers, like Elsevier or Taylor & Francis, do not include 
abstracts in their Crossref metadata (see [this](https://www.crossref.org/blog/i4oa-hall-of-fame-2023-edition/) Crossref Blog for details).

As journals typically have two ISSN numbers (print and electronic, see 
[here](https://en.wikipedia.org/wiki/ISSN)), the crawler retrieves articles for
both and deduplicates the results. ISSN numbers are obtained from the Crossref
lookup [tool](https://www.crossref.org/titleList/).

To avoid duplicates, the crawler maintains a list of previously crawled article 
DOIs. Only new articles are included in each update, ensuring that articles 
appearing first online and later in print are only listed once.

For generic titles (e.g., "Errata", "Frontmatter", "Backmatter"), the crawler 
adds a filter tag. For articles from multidisciplinary journals, GPT-4 is 
prompted to determine if the content is relevant to communication science. 
Filtered content is hidden by default but can be displayed by clicking the +N 
button at the top left of each journal section.

<br>

<hr>

# Contribute #

1. Find and fix bugs or add new features to the crawler/web page. Help out
Moritz on [his original version](https://github.com/sumtxt/picnic) or make 
suggestions [in mine](https://github.com/jacob-long/picnic).

2. Use the crawled data for your own tool: <button type="button" class="align-items-center btn btn-primary btn-sm rounded-pill" data-bs-toggle="modal" data-bs-target="#jsonlist">All JSON Files</button>

3. Build an improved (and equally open source) version of this page.

4. Support [The Initiative for Open Abstracts](https://i4oa.org/).

5. <script type="text/javascript" src="https://cdnjs.buymeacoffee.com/1.0.0/button.prod.min.js" data-name="bmc-button" data-slug="mmarbach" data-color="#FFDD00" data-emoji="☕"  data-font="Cookie" data-text="Buy Moritz a coffee for doing the legwork." data-outline-color="#000000" data-font-color="#000000" data-coffee-color="#ffffff" ></script>

