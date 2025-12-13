---
title: "Subscribe"
layout: main
---

<div class="container my-5">
  <div class="row justify-content-center">
    <div class="col-lg-8">
      <div class="text-center mb-4">
        <h1>Subscribe to Paper Picnic</h1>
        <p class="lead text-muted">Get the latest communication research delivered to your inbox.</p>
      </div>

      <div class="card shadow-sm">
        <div class="card-body p-4">
          <form id="subscribe-form" class="needs-validation" novalidate>

            <!-- Email -->
            <div class="mb-4">
              <label for="email" class="form-label fw-bold">Email address</label>
              <input type="email" class="form-control form-control-lg" id="email" name="email" required>
              <div class="invalid-feedback">Please enter a valid email address.</div>
            </div>

            <!-- Frequency -->
            <div class="mb-4">
              <label class="form-label fw-bold">How often would you like to receive updates?</label>
              <div class="form-check">
                <input class="form-check-input" type="radio" name="frequency" id="freq-daily" value="freq-daily">
                <label class="form-check-label" for="freq-daily">
                  <strong>Daily</strong> <span class="text-muted">- Every morning</span>
                </label>
              </div>
              <div class="form-check">
                <input class="form-check-input" type="radio" name="frequency" id="freq-weekly" value="freq-weekly" checked>
                <label class="form-check-label" for="freq-weekly">
                  <strong>Weekly</strong> <span class="text-muted">- Every Monday (recommended)</span>
                </label>
              </div>
              <div class="form-check">
                <input class="form-check-input" type="radio" name="frequency" id="freq-monthly" value="freq-monthly">
                <label class="form-check-label" for="freq-monthly">
                  <strong>Monthly</strong> <span class="text-muted">- First of each month</span>
                </label>
              </div>
            </div>

            <!-- Disciplines -->
            <div class="mb-4">
              <label class="form-label fw-bold">Which disciplines interest you?</label>
              <p class="text-muted small mb-2">Select all that apply. You'll only receive articles from your selected disciplines.</p>

              <div class="row">
                <div class="col-md-6">
                  <div class="form-check">
                    <input class="form-check-input disc-check" type="checkbox" id="disc-communication" value="disc-communication" checked>
                    <label class="form-check-label" for="disc-communication">Communication</label>
                  </div>
                  <div class="form-check">
                    <input class="form-check-input disc-check" type="checkbox" id="disc-politics" value="disc-politics">
                    <label class="form-check-label" for="disc-politics">Political Science</label>
                  </div>
                  <div class="form-check">
                    <input class="form-check-input disc-check" type="checkbox" id="disc-po" value="disc-po">
                    <label class="form-check-label" for="disc-po">Public Opinion</label>
                  </div>
                  <div class="form-check">
                    <input class="form-check-input disc-check" type="checkbox" id="disc-psych" value="disc-psych">
                    <label class="form-check-label" for="disc-psych">Psychology</label>
                  </div>
                </div>
                <div class="col-md-6">
                  <div class="form-check">
                    <input class="form-check-input disc-check" type="checkbox" id="disc-sociology" value="disc-sociology">
                    <label class="form-check-label" for="disc-sociology">Sociology</label>
                  </div>
                  <div class="form-check">
                    <input class="form-check-input disc-check" type="checkbox" id="disc-multidisciplinary" value="disc-multidisciplinary">
                    <label class="form-check-label" for="disc-multidisciplinary">Multidisciplinary</label>
                  </div>
                  <div class="form-check">
                    <input class="form-check-input disc-check" type="checkbox" id="disc-preprints" value="disc-preprints">
                    <label class="form-check-label" for="disc-preprints">Preprints</label>
                  </div>
                </div>
              </div>

              <hr class="my-3">

              <div class="form-check">
                <input class="form-check-input disc-check" type="checkbox" id="disc-mustread" value="disc-mustread" checked>
                <label class="form-check-label" for="disc-mustread">
                  <strong>Must-Read Picks</strong> <span class="text-muted">- AI-curated highlights</span>
                </label>
              </div>
            </div>

            <!-- Quick select buttons -->
            <div class="mb-4">
              <button type="button" class="btn btn-outline-secondary btn-sm me-2" id="select-all">Select all disciplines</button>
              <button type="button" class="btn btn-outline-secondary btn-sm" id="select-none">Clear selection</button>
            </div>

            <!-- Submit -->
            <div class="d-grid">
              <button type="submit" class="btn btn-dark btn-lg">Subscribe</button>
            </div>

            <p class="text-muted small mt-3 mb-0 text-center">
              You can update your preferences or unsubscribe at any time.
            </p>
          </form>

          <!-- Success message (hidden by default) -->
          <div id="success-message" class="text-center py-4" style="display: none;">
            <svg xmlns="http://www.w3.org/2000/svg" width="64" height="64" fill="currentColor" class="bi bi-check-circle text-success mb-3" viewBox="0 0 16 16">
              <path d="M8 15A7 7 0 1 1 8 1a7 7 0 0 1 0 14m0 1A8 8 0 1 0 8 0a8 8 0 0 0 0 16"/>
              <path d="m10.97 4.97-.02.022-3.473 4.425-2.093-2.094a.75.75 0 0 0-1.06 1.06L6.97 11.03a.75.75 0 0 0 1.079-.02l3.992-4.99a.75.75 0 0 0-1.071-1.05"/>
            </svg>
            <h3>You're subscribed!</h3>
            <p class="text-muted">Check your email to confirm your subscription.</p>
            <a href="./" class="btn btn-outline-dark">Back to Paper Picnic</a>
          </div>
        </div>
      </div>

      <div class="text-center mt-4">
        <p class="text-muted small">
          Powered by <a href="https://buttondown.com" target="_blank" rel="noopener">Buttondown</a>
        </p>
      </div>
    </div>
  </div>
</div>

<script>
// Buttondown newsletter ID
const BUTTONDOWN_USERNAME = 'jacoblong';

document.addEventListener('DOMContentLoaded', function() {
    const form = document.getElementById('subscribe-form');
    const successMessage = document.getElementById('success-message');
    const selectAll = document.getElementById('select-all');
    const selectNone = document.getElementById('select-none');
    const discChecks = document.querySelectorAll('.disc-check');

    // Select all disciplines
    selectAll.addEventListener('click', function() {
        discChecks.forEach(check => check.checked = true);
    });

    // Clear all disciplines
    selectNone.addEventListener('click', function() {
        discChecks.forEach(check => check.checked = false);
    });

    // Form submission
    form.addEventListener('submit', function(e) {
        e.preventDefault();

        if (!form.checkValidity()) {
            form.classList.add('was-validated');
            return;
        }

        const email = document.getElementById('email').value;
        const frequency = document.querySelector('input[name="frequency"]:checked').value;

        // Collect selected disciplines
        const disciplines = Array.from(discChecks)
            .filter(check => check.checked)
            .map(check => check.value);

        // Build tags array
        const tags = [frequency, ...disciplines];

        // Build Buttondown subscription URL
        const tagParams = tags.map(tag => `tag=${encodeURIComponent(tag)}`).join('&');
        const subscribeUrl = `https://buttondown.com/api/emails/${BUTTONDOWN_USERNAME}`;

        // Submit to Buttondown
        // Using their form action endpoint
        const formData = new FormData();
        formData.append('email', email);
        tags.forEach(tag => formData.append('tag', tag));

        fetch(`https://buttondown.com/api/emails/${BUTTONDOWN_USERNAME}`, {
            method: 'POST',
            body: formData,
            mode: 'no-cors' // Buttondown doesn't support CORS for this endpoint
        }).then(() => {
            // Since we can't read the response due to no-cors,
            // redirect to Buttondown's hosted form as fallback
            // Actually, let's use the redirect method which is more reliable
        });

        // More reliable: redirect to Buttondown's subscribe page with tags
        const redirectUrl = `https://buttondown.com/${BUTTONDOWN_USERNAME}?${tagParams}&email=${encodeURIComponent(email)}`;
        window.location.href = redirectUrl;
    });
});
</script>
