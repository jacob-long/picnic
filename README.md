# Communication Paper Picnic

A basket with the latest published research in communication and a
grabbag from related fields. This is an adaptation of 
[a project](https://paper-picnic.com) by 
[Moritz Marbach](http://moritz-marbach.com/).

The crawler lives in the `main` branch of the backend while the website is rendered from the `gh-pages` branch.

After forking the repository, you need to make some changes to the repository settings for it to function properly.

1. Go to Settings > Actions > General. Scroll down to Workflow permissions and allow workflows to read and write in the repository.

2. Go to Security > Secrets and Variables > Actions. Set `CROSSREF_EMAIL` and `OPENAI_APIKEY` in as a repository secret.  The latter is used to query the OpenAI API while the former is to politely identify yourself to the Crossref API.
