# Communication Paper Picnic

A basket with the latest published research in communication and a
grabbag from related fields. This is an adaptation of
[a project](https://paper-picnic.com) by
[Moritz Marbach](http://moritz-marbach.com/).

The crawler lives in the `main` branch of the backend while the website is rendered from the `gh-pages` branch.

## Setup

After forking the repository, you need to make some changes to the repository settings for it to function properly.

1. Go to Settings > Actions > General. Scroll down to Workflow permissions and allow workflows to read and write in the repository.

2. Go to Security > Secrets and Variables > Actions. Set the following repository secrets:
   - `CROSSREF_EMAIL` - Your email for the Crossref API (polite pool)
   - `OPENAI_APIKEY` - OpenAI API key for article classification
   - `GEMINI_APIKEY` - Google Gemini API key for must-read selection
   - `BUTTONDOWN_API_KEY` - (Optional) Buttondown API key for email digests

## Email Digests (Optional)

The project supports sending email digests via [Buttondown](https://buttondown.com/). Subscribers can choose their preferred frequency (daily, weekly, or monthly).

### Setting up Buttondown

1. Create a Buttondown account and get your API key from Settings > API
2. Add `BUTTONDOWN_API_KEY` to your GitHub repository secrets
3. Enable tags in Buttondown: Settings > Basics > Tags
4. Enable subscriber autonomy: Settings > Subscribing > Autonomy

### Subscriber Tags

Create the following tags in Buttondown for subscriber preferences:

**Frequency tags** (subscribers choose one):
- `freq-daily` - Daily digest
- `freq-weekly` - Weekly digest (recommended default)
- `freq-monthly` - Monthly digest

**Discipline tags** (subscribers can choose multiple):
- `disc-communication` - Communication journals
- `disc-politics` - Political science journals
- `disc-po` - Public opinion journals
- `disc-psych` - Psychology journals
- `disc-sociology` - Sociology journals
- `disc-multidisciplinary` - Multidisciplinary journals (Nature, Science, PNAS, etc.)
- `disc-preprints` - Preprints from OSF servers
- `disc-mustread` - AI-curated must-read picks

### Signup Form

You can configure your Buttondown signup form to let subscribers self-select their preferences using the Portal feature. Add form inputs for frequency and disciplines.

Example signup URL with default tags:
```
https://buttondown.com/your-newsletter?tag=freq-weekly&tag=disc-communication&tag=disc-mustread
```

### Manual Trigger

You can manually trigger email sends from the Actions tab:
1. Go to Actions > send_email
2. Click "Run workflow"
3. Select frequency tier and optionally enable dry run mode

### Schedule

The email workflow runs daily at 3 AM UTC (after the 2 AM crawl):
- **Daily subscribers**: Receive emails every day
- **Weekly subscribers**: Receive emails on Mondays
- **Monthly subscribers**: Receive emails on the 1st of each month
